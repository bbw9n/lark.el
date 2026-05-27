;;; lark-ui-test.el --- Tests for lark-ui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; ERT tests for lark.el shared UI helpers (the loading spinner).

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add project directories to load-path
(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'lark-ui)

;;;; Spinner tests

(ert-deftest lark-test-spinner-frames-nonempty ()
  "The default spinner frame set is a non-empty vector of strings."
  (should (vectorp lark-spinner-frames))
  (should (> (length lark-spinner-frames) 0))
  (should (cl-every #'stringp (append lark-spinner-frames nil))))

(ert-deftest lark-test-spinner-renders-first-frame-synchronously ()
  "RENDER-FN is called immediately with the first frame on start."
  (let ((seen nil))
    (let ((sp (lark-spinner-start
               (lambda (frame) (push frame seen) t))))
      (lark-spinner-stop sp))
    (should (equal seen (list (aref lark-spinner-frames 0))))))

(ert-deftest lark-test-spinner-no-timer-when-first-render-nil ()
  "When RENDER-FN returns nil immediately, no live timer is created."
  (let ((sp (lark-spinner-start (lambda (_frame) nil))))
    (should-not (and (timerp sp) (memq sp timer-list)))))

(ert-deftest lark-test-spinner-on-stop-fires-on-immediate-stop ()
  "ON-STOP runs when RENDER-FN signals stop on the first frame."
  (let ((stopped nil))
    (lark-spinner-start (lambda (_frame) nil) nil nil nil
                        (lambda () (setq stopped t)))
    (should stopped)))

(ert-deftest lark-test-spinner-stop-cancels-timer ()
  "`lark-spinner-stop' removes the timer from `timer-list'."
  (let ((sp (lark-spinner-start (lambda (_frame) t))))
    (should (memq sp timer-list))
    (lark-spinner-stop sp)
    (should-not (memq sp timer-list))))

(ert-deftest lark-test-spinner-stop-tolerates-nil ()
  "`lark-spinner-stop' is a no-op on nil."
  (should-not (lark-spinner-stop nil)))

;;;; Progress bar

(ert-deftest lark-test-bar-frames ()
  "`lark-ui-bar-frames' is a faced left-to-right fill that loops."
  (should (vectorp lark-ui-bar-frames))
  (should (= 10 (length lark-ui-bar-frames)))
  (should (equal "█░░░░░░░░░" (substring-no-properties (aref lark-ui-bar-frames 0))))
  (should (equal "██████████" (substring-no-properties (aref lark-ui-bar-frames 9))))
  ;; Filled cells carry the bar face; the track is shadowed.
  (should (eq 'lark-ui-bar (get-text-property 0 'face (aref lark-ui-bar-frames 4))))
  (should (eq 'shadow (get-text-property 9 'face (aref lark-ui-bar-frames 4)))))

;;;; Field rendering tests

(defun lark-test-ui--render (thunk)
  "Return the buffer text produced by calling THUNK in a temp buffer."
  (with-temp-buffer
    (funcall thunk)
    (buffer-string)))

(ert-deftest lark-test-ui-insert-field-basic ()
  "A non-empty value renders an aligned, faced \"label: value\" line."
  (let ((text (lark-test-ui--render
               (lambda () (lark-ui-insert-field "Start" "10:00")))))
    (should (equal text (concat "  " (format "%-14s" "Start:") "10:00\n")))
    ;; The label (through the colon) carries `font-lock-keyword-face'.
    (should (eq (get-text-property 0 'face text) 'font-lock-keyword-face))))

(ert-deftest lark-test-ui-insert-field-skips-empty-and-nil ()
  "Nil, empty, and non-string values insert nothing."
  (dolist (v (list nil "" 42))
    (should (equal "" (lark-test-ui--render
                       (lambda () (lark-ui-insert-field "X" v)))))))

(ert-deftest lark-test-ui-insert-field-width-and-indent ()
  "WIDTH controls label padding and INDENT the leading whitespace."
  (should (equal (concat (format "%-10s" "From:") "a@b\n")
                 (lark-test-ui--render
                  (lambda () (lark-ui-insert-field "From" "a@b" 10 ""))))))

(ert-deftest lark-test-ui-insert-field-multiline-aligned ()
  "Continuation lines are indented under the value column."
  (let ((prefix (concat "  " (format "%-4s" "K:"))))
    (should (equal (concat prefix "one\n" (make-string (length prefix) ?\s) "two\n")
                   (lark-test-ui--render
                    (lambda () (lark-ui-insert-field "K" "one\ntwo" 4)))))))

;;;; Separator / title tests

(ert-deftest lark-test-ui-separator ()
  "`lark-ui-separator' returns WIDTH rule chars (default 60)."
  (should (equal (make-string 60 lark-ui-rule-char) (lark-ui-separator)))
  (should (equal (make-string 7 lark-ui-rule-char) (lark-ui-separator 7)))
  (should (= 7 (length (lark-ui-separator 7)))))

(ert-deftest lark-test-ui-insert-title-scaled-rule ()
  "Title is bold; the rule scales to title length clamped to [20, max]."
  (let ((text (lark-test-ui--render
               (lambda () (lark-ui-insert-title "Hi")))))
    ;; "Hi" is shorter than 20, so the rule is clamped up to 20.
    (should (equal text (concat "Hi\n" (make-string 20 lark-ui-rule-char) "\n\n")))
    (should (eq (get-text-property 0 'face text) 'bold)))
  ;; A long title is clamped down to the max width.
  (let* ((long (make-string 80 ?x))
         (text (lark-test-ui--render
                (lambda () (lark-ui-insert-title long 1 72)))))
    (should (equal text (concat long "\n" (make-string 72 lark-ui-rule-char) "\n")))))

;;;; Section navigation tests

(defun lark-test-ui--sections-buffer ()
  "Create a buffer with two `lark-id'-tagged sections separated by a gap."
  (let ((buf (generate-new-buffer " *lark-nav-test*")))
    (with-current-buffer buf
      (let ((a (point)))
        (insert "Alpha\nfields\n")
        (put-text-property a (point) 'lark-id "a"))
      (insert "\n")                    ; gap with no property
      (let ((b (point)))
        (insert "Beta\nfields\n")
        (put-text-property b (point) 'lark-id "b")))
    buf))

(ert-deftest lark-test-ui-next-section ()
  "`lark-ui-next-section' jumps from one section to the next's start."
  (let ((buf (lark-test-ui--sections-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (lark-ui-next-section 'lark-id)
          (should (equal "b" (get-text-property (point) 'lark-id)))
          (should (looking-at-p "Beta"))
          ;; No further section: point stays put.
          (let ((here (point)))
            (lark-ui-next-section 'lark-id)
            (should (= here (point)))))
      (kill-buffer buf))))

(ert-deftest lark-test-ui-prev-section ()
  "`lark-ui-prev-section' jumps back to the previous section's start."
  (let ((buf (lark-test-ui--sections-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-max))
          (lark-ui-prev-section 'lark-id)
          (should (equal "b" (get-text-property (point) 'lark-id)))
          (should (looking-at-p "Beta"))
          (lark-ui-prev-section 'lark-id)
          (should (equal "a" (get-text-property (point) 'lark-id)))
          (should (looking-at-p "Alpha")))
      (kill-buffer buf))))

(provide 'lark-ui-test)
;;; lark-ui-test.el ends here
