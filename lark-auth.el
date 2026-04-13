;;; lark-auth.el --- Authentication for lark.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Handles the OAuth device-code authentication flow for lark-cli,
;; auth status checking, and identity switching (user/bot).

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
  (lark--run-command
   '("auth" "status")
   (lambda (result)
     (let* ((status (or (alist-get 'status result)
                        (alist-get 'login_status result)))
            (user (or (alist-get 'user result)
                      (alist-get 'name result)
                      (alist-get 'user_name result)))
            (domain (or (alist-get 'domain result)
                        (alist-get 'tenant result))))
       (if (and status (member status '("authenticated" "logged_in" "ok")))
           (progn
             (setq lark-auth--authenticated-p t)
             (message "Lark: authenticated as %s%s"
                      (or user "unknown")
                      (if domain (format " (%s)" domain) "")))
         (setq lark-auth--authenticated-p nil)
         (message "Lark: not authenticated"))))
   nil
   :no-error t))

(defun lark-auth--check-sync ()
  "Check auth status synchronously.  Return non-nil if authenticated."
  (condition-case nil
      (let ((result (lark--run-command-sync
                     '("auth" "status") nil :no-error t)))
        (let ((status (or (alist-get 'status result)
                          (alist-get 'login_status result))))
          (setq lark-auth--authenticated-p
                (and status (member status '("authenticated" "logged_in" "ok"))
                     t))))
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
(defun lark-auth-login ()
  "Initiate lark-cli OAuth login via the device-code flow.
Opens the authorization URL in a browser and polls for completion."
  (interactive)
  (message "Lark: initiating login...")
  (lark--run-command
   '("auth" "login" "--no-wait")
   (lambda (result)
     (let ((url (or (alist-get 'verification_url result)
                    (alist-get 'verification_uri result)
                    (alist-get 'url result)))
           (device-code (or (alist-get 'device_code result)
                            (alist-get 'code result)))
           (user-code (alist-get 'user_code result))
           (interval (or (alist-get 'interval result) 5)))
       (unless (and url device-code)
         (user-error "Lark: unexpected login response: %S" result))
       (message "Lark: opening browser for authorization (code: %s)..."
                (or user-code device-code))
       (browse-url url)
       ;; Start polling for completion
       (lark-auth--start-polling device-code interval)))
   nil
   :no-error t))

(defun lark-auth--start-polling (device-code interval)
  "Poll for device-code auth completion.
DEVICE-CODE is the code to check.  INTERVAL is seconds between polls."
  (when lark-auth--polling-timer
    (cancel-timer lark-auth--polling-timer))
  (let ((attempts 0)
        (max-attempts 60))                ; 5 minutes at 5s intervals
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
                '("auth" "login" "--device-code")
                (lambda (result)
                  (let ((status (or (alist-get 'status result)
                                    (alist-get 'login_status result))))
                    (when (and status (member status '("authenticated" "logged_in" "ok" "success")))
                      (cancel-timer lark-auth--polling-timer)
                      (setq lark-auth--polling-timer nil
                            lark-auth--authenticated-p t)
                      (message "Lark: login successful!"))))
                (list :device-code device-code)
                :no-error t)))))))

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
     :no-error t)))

;;;; Identity switching

;;;###autoload
(defun lark-auth-switch-identity ()
  "Switch between user and bot identity for lark-cli commands."
  (interactive)
  (let ((identity (completing-read "Identity: " '("user" "bot") nil t)))
    (customize-set-variable 'lark-default-identity identity)
    (message "Lark: identity set to %s" identity)))

(provide 'lark-auth)
;;; lark-auth.el ends here
