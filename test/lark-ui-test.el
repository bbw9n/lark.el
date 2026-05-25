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

(provide 'lark-ui-test)
;;; lark-ui-test.el ends here
