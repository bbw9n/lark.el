;;; lark-core.el --- Core process management for lark.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; This module provides the engine that all lark.el domain modules use
;; to interact with the lark-cli process.  It handles async/sync
;; execution, JSON parsing, error handling, pagination, and argument
;; formatting.

;;; Code:

(require 'json)

;;;; Forward declarations for customization variables defined in lark.el
;; These provide defaults when lark-core is loaded without lark.el.

(defvar lark-cli-executable "lark-cli"
  "Path to the lark-cli executable.")
(defvar lark-default-format "json"
  "Default output format for lark-cli commands.")
(defvar lark-default-identity nil
  "Default identity (\"user\" or \"bot\") for lark-cli commands.")
(defvar lark-dry-run nil
  "When non-nil, pass --dry-run to destructive commands.")

;;;; Variables

(defvar lark--debug nil
  "When non-nil, log lark-cli invocations to *lark-log* buffer.")

(defvar lark--process-output-alist nil
  "Alist mapping process objects to accumulated output strings.")

;;;; Executable resolution

(defun lark--executable ()
  "Return the lark-cli executable path.
Signals an error if the executable is not found."
  (let ((exe (or lark-cli-executable "lark-cli")))
    (or (executable-find exe)
        (user-error "Cannot find `%s' executable; install lark-cli or set `lark-cli-executable'" exe))))

;;;; Argument formatting

(defun lark--format-args (plist)
  "Convert PLIST to a flat list of CLI arguments.
Keys are converted from :keyword to --keyword form.
Nil values are omitted.  t values produce a flag with no argument.
List values produce repeated flags.

Example:
  (lark--format-args \\='(:page-size 20 :verbose t :fields (\"a\" \"b\")))
  => (\"--page-size\" \"20\" \"--verbose\" \"--fields\" \"a\" \"--fields\" \"b\")"
  (let (args)
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (flag (concat "--" (substring (symbol-name key) 1))))
        (cond
         ((null val))                   ; skip nil values
         ((eq val t)                    ; boolean flag
          (push flag args))
         ((listp val)                   ; repeated flag
          (dolist (v val)
            (push flag args)
            (push (format "%s" v) args)))
         (t                             ; key-value pair
          (push flag args)
          (push (format "%s" val) args)))))
    (nreverse args)))

(defun lark--build-command (cmd-args &optional extra-args format)
  "Build the full argument list for a lark-cli invocation.
CMD-ARGS is a list of positional arguments (e.g., (\"calendar\" \"+agenda\")).
EXTRA-ARGS is a plist of keyword arguments (converted via `lark--format-args').
FORMAT overrides `lark-default-format'."
  (let ((fmt (or format lark-default-format "json"))
        (identity lark-default-identity)
        (dry-run lark-dry-run))
    (append cmd-args
            (lark--format-args extra-args)
            (list "--format" fmt)
            (when identity (list "--as" identity))
            (when dry-run (list "--dry-run")))))

;;;; Logging

(defun lark--log (format-string &rest args)
  "Log FORMAT-STRING with ARGS to the *lark-log* buffer when debugging."
  (when lark--debug
    (with-current-buffer (get-buffer-create "*lark-log*")
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S] ")
              (apply #'format format-string args)
              "\n"))))

;;;; JSON parsing

(defun lark--parse-json (string)
  "Parse STRING as JSON, returning an alist.
Returns nil if STRING is empty or not valid JSON."
  (when (and string (not (string-empty-p (string-trim string))))
    (condition-case err
        (json-parse-string string :object-type 'alist :array-type 'list)
      (json-parse-error
       (lark--log "JSON parse error: %s\nInput: %s" (error-message-string err) string)
       nil))))

(defun lark--parse-ndjson (string)
  "Parse STRING as newline-delimited JSON, returning a list of alists."
  (when (and string (not (string-empty-p (string-trim string))))
    (let ((lines (split-string (string-trim string) "\n" t)))
      (delq nil (mapcar #'lark--parse-json lines)))))

;;;; Error handling

(defun lark--handle-error (response)
  "Check RESPONSE for errors and signal `user-error' if found.
RESPONSE should be an alist parsed from lark-cli JSON output.
Returns RESPONSE if no error is detected."
  (when (and response (listp response))
    (let ((code (alist-get 'code response))
          (msg (alist-get 'msg response)))
      (when (and code (not (eq code 0)))
        (user-error "Lark API error (code %s): %s" code (or msg "unknown error")))))
  response)

;;;; Async process execution

(defun lark--run-command (cmd-args callback &optional extra-args &rest keys)
  "Run a lark-cli command asynchronously.

CMD-ARGS is a list of positional arguments (e.g., (\"calendar\" \"+agenda\")).
CALLBACK is called with the parsed JSON result on success.
EXTRA-ARGS is a plist of keyword arguments.
KEYS are keyword arguments:
  :format   - output format (default: `lark-default-format')
  :raw      - if non-nil, pass raw string to callback instead of parsed JSON
  :no-error - if non-nil, don't signal errors from the response
  :literal  - if non-nil, use CMD-ARGS as-is (skip --format/--as/--dry-run)

Returns the process object."
  (let* ((format (plist-get keys :format))
         (raw (plist-get keys :raw))
         (no-error (plist-get keys :no-error))
         (literal (plist-get keys :literal))
         (full-args (if literal cmd-args
                      (lark--build-command cmd-args extra-args format)))
         (exe (lark--executable))
         (proc-name (format "lark-%s" (string-join cmd-args "-")))
         proc)
    (lark--log "Running: %s %s" exe (string-join full-args " "))
    (setq proc
          (make-process
           :name proc-name
           :command (cons exe full-args)
           :connection-type 'pipe
           :noquery t
           :filter (lambda (proc output)
                     (let ((existing (alist-get proc lark--process-output-alist)))
                       (setf (alist-get proc lark--process-output-alist)
                             (concat (or existing "") output))))
           :sentinel (lambda (proc event)
                       (unwind-protect
                           (let ((output (alist-get proc lark--process-output-alist))
                                 (exit-code (process-exit-status proc)))
                             (lark--log "Process %s exited (%s): %s"
                                        (process-name proc) exit-code
                                        (string-trim event))
                             (cond
                              ((not (zerop exit-code))
                               (let ((msg (or (and output (string-trim output)) event)))
                                 (unless no-error
                                   (message "Lark CLI error (exit %d): %s" exit-code msg))))
                              (callback
                               (let ((result (if raw output (lark--parse-json output))))
                                 (unless (or raw no-error)
                                   (lark--handle-error result))
                                 (funcall callback result)))))
                         (setf (alist-get proc lark--process-output-alist nil t) nil)))))
    proc))

;;;; Sync process execution

(defun lark--run-command-sync (cmd-args &optional extra-args &rest keys)
  "Run a lark-cli command synchronously and return the parsed result.

CMD-ARGS is a list of positional arguments.
EXTRA-ARGS is a plist of keyword arguments.
KEYS are keyword arguments:
  :format   - output format (default: `lark-default-format')
  :raw      - if non-nil, return raw string instead of parsed JSON
  :no-error - if non-nil, don't signal errors from the response
  :literal  - if non-nil, use CMD-ARGS as-is (skip --format/--as/--dry-run)"
  (let* ((format (plist-get keys :format))
         (raw (plist-get keys :raw))
         (no-error (plist-get keys :no-error))
         (literal (plist-get keys :literal))
         (full-args (if literal cmd-args
                      (lark--build-command cmd-args extra-args format)))
         (exe (lark--executable))
         (output-buf (generate-new-buffer " *lark-sync*"))
         exit-code output)
    (lark--log "Running (sync): %s %s" exe (string-join full-args " "))
    (unwind-protect
        (progn
          (setq exit-code
                (apply #'call-process exe nil output-buf nil full-args))
          (setq output (with-current-buffer output-buf (buffer-string)))
          (lark--log "Sync exit %d: %s" exit-code (truncate-string-to-width output 200))
          (if (not (zerop exit-code))
              (unless no-error
                (user-error "Lark CLI error (exit %d): %s"
                            exit-code (string-trim output)))
            (if raw
                output
              (let ((result (lark--parse-json output)))
                (unless no-error
                  (lark--handle-error result))
                result))))
      (kill-buffer output-buf))))

;;;; Pagination

(defun lark--paginate (cmd-args callback &optional extra-args &rest keys)
  "Run a lark-cli command with --page-all and collect all results.

CMD-ARGS, EXTRA-ARGS, and KEYS are as in `lark--run-command'.
CALLBACK receives the full parsed result (lark-cli handles pagination
internally with --page-all)."
  (let ((paginate-args (append extra-args '(:page-all t))))
    (apply #'lark--run-command cmd-args callback paginate-args keys)))

;;;; Utility functions

(defun lark--get-nested (alist &rest keys)
  "Access a nested value in ALIST by following KEYS.
Example: (lark--get-nested data \\='items 0 \\='name)"
  (let ((current alist))
    (dolist (key keys)
      (setq current
            (cond
             ((and (integerp key) (listp current))
              (nth key current))
             ((listp current)
              (alist-get key current))
             (t nil))))
    current))

(defun lark--format-timestamp (timestamp)
  "Format a Unix TIMESTAMP (string or number) to a human-readable string."
  (when timestamp
    (let ((ts (if (stringp timestamp) (string-to-number timestamp) timestamp)))
      (when (> ts 0)
        (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time ts))))))

(provide 'lark-core)
;;; lark-core.el ends here
