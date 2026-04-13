;;; lark-auth.el --- Authentication for lark.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Handles the OAuth device-code authentication flow for lark-cli,
;; auth status checking, and identity switching (user/bot).
;;
;; Note: lark-cli auth commands use their own flags (e.g., --json)
;; rather than the global --format/--as flags.  All auth commands use
;; :literal t to bypass lark--build-command.

;;; Code:

(require 'lark-core)
(require 'browse-url)

;;;; Variables

(defvar lark-auth--authenticated-p nil
  "Cached authentication status.  nil means unknown.")

(defvar lark-auth--polling-timer nil
  "Timer for polling device-code authentication.")

;;;; Auth status

;;;###autoload
(defun lark-auth-status ()
  "Display lark-cli authentication status."
  (interactive)
  ;; `auth status' outputs JSON by default, no --json flag needed.
  (lark--run-command
   '("auth" "status")
   (lambda (result)
     (if (null result)
         (progn
           (setq lark-auth--authenticated-p nil)
           (message "Lark: not authenticated (no config)"))
       (let ((user (or (alist-get 'userName result)
                       (alist-get 'user_name result)
                       (alist-get 'name result)))
             (identity (alist-get 'identity result))
             (status (alist-get 'tokenStatus result)))
         (if user
             (progn
               (setq lark-auth--authenticated-p t)
               (message "Lark: authenticated as %s (identity: %s, token: %s)"
                        user (or identity "?") (or status "?")))
           (setq lark-auth--authenticated-p nil)
           (message "Lark: not authenticated")))))
   nil
   :literal t :no-error t))

(defun lark-auth--check-sync ()
  "Check auth status synchronously.  Return non-nil if authenticated."
  (condition-case nil
      (let ((result (lark--run-command-sync
                     '("auth" "status") nil :literal t :no-error t)))
        (setq lark-auth--authenticated-p
              (and result (alist-get 'userName result) t)))
    (error
     (setq lark-auth--authenticated-p nil))))

;;;; Auth ensure

(defun lark-auth-ensure ()
  "Ensure the user is authenticated, prompting login if needed.
Returns non-nil if authenticated."
  (unless lark-auth--authenticated-p
    (lark-auth--check-sync))
  (unless lark-auth--authenticated-p
    (if (yes-or-no-p "Not authenticated with Lark.  Login now? ")
        (lark-auth-login)
      (user-error "Lark authentication required")))
  lark-auth--authenticated-p)

;;;; Login (device-code flow)

;;;###autoload
(defun lark-auth-login (&optional domains)
  "Initiate lark-cli OAuth login via the device-code flow.
DOMAINS is a comma-separated string of scopes to request
\(e.g., \"calendar,task,im\").  Defaults to \"all\".

Opens the authorization URL in a browser and polls for completion."
  (interactive
   (list (read-string "Domains (comma-separated, default all): " nil nil "all")))
  (let ((domain (or domains "all")))
    (message "Lark: initiating login (domains: %s)..." domain)
    (lark--run-command
     (list "auth" "login" "--no-wait" "--json" "--domain" domain)
     (lambda (result)
       (let ((ok (alist-get 'ok result))
             (url (alist-get 'verification_url result))
             (device-code (alist-get 'device_code result))
             (err (alist-get 'error result)))
         (cond
          ;; Error response: {ok: false, error: {message: ...}}
          ((and err (not (eq ok :false)))
           (user-error "Lark login error: %s"
                       (or (alist-get 'message err) (format "%S" err))))
          ((eq ok :false)
           (user-error "Lark login error: %s"
                       (or (and err (alist-get 'message err)) "unknown")))
          ;; Missing expected fields
          ((not (and url device-code))
           (user-error "Lark: unexpected login response: %S" result))
          ;; Success — open browser and poll
          (t
           (message "Lark: opening browser for authorization...")
           (browse-url url)
           (lark-auth--start-polling device-code)))))
     nil
     :literal t :no-error t)))

(defun lark-auth--start-polling (device-code)
  "Poll for device-code auth completion.
DEVICE-CODE is the code to check."
  (when lark-auth--polling-timer
    (cancel-timer lark-auth--polling-timer))
  (let ((attempts 0)
        (max-attempts 120)              ; 10 minutes at 5s intervals
        (interval 5))
    (setq lark-auth--polling-timer
          (run-with-timer
           interval interval
           (lambda ()
             (setq attempts (1+ attempts))
             (if (>= attempts max-attempts)
                 (progn
                   (cancel-timer lark-auth--polling-timer)
                   (setq lark-auth--polling-timer nil)
                   (message "Lark: login timed out"))
               (lark--run-command
                (list "auth" "login"
                      "--device-code" device-code
                      "--json")
                (lambda (result)
                  ;; Success: {ok: true, ...} or has userName
                  (let ((ok (alist-get 'ok result)))
                    (when (eq ok t)
                      (cancel-timer lark-auth--polling-timer)
                      (setq lark-auth--polling-timer nil
                            lark-auth--authenticated-p t)
                      (message "Lark: login successful!"))))
                nil
                :literal t :no-error t)))))))

;;;; Logout

;;;###autoload
(defun lark-auth-logout ()
  "Log out from lark-cli."
  (interactive)
  (when (yes-or-no-p "Log out from Lark? ")
    (lark--run-command
     '("auth" "logout")
     (lambda (_result)
       (setq lark-auth--authenticated-p nil)
       (message "Lark: logged out"))
     nil
     :literal t :no-error t)))

;;;; Identity switching

;;;###autoload
(defun lark-auth-switch-identity ()
  "Switch between user and bot identity for lark-cli commands."
  (interactive)
  (let ((identity (completing-read "Identity: " '("user" "bot") nil t)))
    (customize-set-variable 'lark-default-identity identity)
    (message "Lark: identity set to %s" identity)))

;;;; List logged-in users

;;;###autoload
(defun lark-auth-list ()
  "List all logged-in lark-cli users."
  (interactive)
  (lark--run-command
   '("auth" "list")
   (lambda (result)
     (let ((buf (get-buffer-create "*Lark Auth*")))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (propertize "Lark Authenticated Users\n" 'face 'bold)
                   (make-string 60 ?─) "\n\n"
                   (if result (pp-to-string result) "(none)\n")))
         (special-mode)
         (goto-char (point-min)))
       (pop-to-buffer buf)))
   nil
   :literal t :no-error t))

(provide 'lark-auth)
;;; lark-auth.el ends here
