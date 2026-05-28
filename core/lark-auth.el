;;; lark-auth.el --- Authentication for lark.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

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

(defun lark-auth--user-from-alist (obj)
  "Return the first user-name-like field in alist OBJ, or nil."
  (when (listp obj)
    (or (alist-get 'userName obj)
        (alist-get 'user_name obj)
        (alist-get 'name obj)
        (alist-get 'displayName obj)
        (alist-get 'display_name obj))))

(defun lark-auth--extract-user (result)
  "Return the authenticated user name from RESULT, or nil.
Tolerates several response shapes: a user-name field at the top level,
or nested one level deep under `data' or `user', or two levels deep at
`data.user' — the `{ok, identity, data: {...}}' envelope lark-cli uses
for many endpoints means we can't assume the user field is at the top."
  (or (lark-auth--user-from-alist result)
      (lark-auth--user-from-alist (alist-get 'data result))
      (lark-auth--user-from-alist (alist-get 'user result))
      (lark-auth--user-from-alist
       (alist-get 'user (alist-get 'data result)))))

(defun lark-auth--extract-field (result key)
  "Look up KEY in RESULT and one level deep under `data'."
  (or (alist-get key result)
      (and (listp (alist-get 'data result))
           (alist-get key (alist-get 'data result)))))

;;;###autoload
(defun lark-auth-status ()
  "Display lark-cli authentication status."
  (interactive)
  ;; `auth status' outputs JSON by default, no --json flag needed.
  (lark--run-command
   '("auth" "status")
   (lambda (result)
     (lark-auth--log "status response: %S" result)
     (let ((user (lark-auth--extract-user result)))
       (cond
        ((null result)
         (setq lark-auth--authenticated-p nil)
         (message "Lark: not authenticated (no config — see *lark-log*)"))
        (user
         (setq lark-auth--authenticated-p t)
         (message "Lark: authenticated as %s (identity: %s, token: %s)"
                  user
                  (or (lark-auth--extract-field result 'identity) "?")
                  (or (lark-auth--extract-field result 'tokenStatus)
                      (lark-auth--extract-field result 'token_status) "?")))
        (t
         (setq lark-auth--authenticated-p nil)
         (message
          "Lark: not authenticated (response shape unrecognised — see *lark-log*)")))))
   nil :literal t :no-error t
   :on-error
   (lambda (exit-code msg)
     (lark-auth--log "status error: exit %s: %s" exit-code msg)
     (setq lark-auth--authenticated-p nil)
     (message "Lark: auth status failed (exit %s) — see *lark-log*" exit-code))))

(defun lark-auth--check-sync ()
  "Check auth status synchronously.  Return non-nil if authenticated."
  (condition-case nil
      (let ((result (lark--run-command-sync
                     '("auth" "status") nil :literal t :no-error t)))
        (lark-auth--log "status (sync) response: %S" result)
        (setq lark-auth--authenticated-p
              (and (lark-auth--extract-user result) t)))
    (error
     (setq lark-auth--authenticated-p nil))))

;;;; Auth ensure

(defun lark-auth-ensure ()
  "Ensure the user is authenticated, prompting login if needed.
Returns t when authentication is already in place.  Signals a
`user-error' otherwise — both when the user declines the login
prompt and when login was just kicked off (the device-code flow
completes asynchronously, so the in-flight command can't proceed
yet).  Callers therefore never need to check the return value:
either control flows past the call (authed) or unwinds with a
clear message."
  (unless lark-auth--authenticated-p
    (lark-auth--check-sync))
  (unless lark-auth--authenticated-p
    (if (yes-or-no-p "Not authenticated with Lark.  Login now? ")
        (progn
          (lark-auth-login)
          (user-error "Lark: complete the login flow in your browser, then re-run the command"))
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

(defun lark-auth--log (fmt &rest args)
  "Append a timestamped auth entry to *lark-log*.
Bypasses `lark--debug' so login traces are always recorded — auth
runs rarely and silent failures (see `lark-auth--start-polling') are
hard to diagnose without them."
  (let ((lark--debug t))
    (apply #'lark--log (concat "auth: " fmt) args)))

(defun lark-auth--login-success-p (result)
  "Return non-nil when RESULT from a device-code poll indicates success.
Accepts any of: explicit `ok: t' at top level, a recognised user-name
field via `lark-auth--extract-user', or an access token at the top
level or under `data' — the polling endpoint's success shape isn't
fully pinned down, so this errs on the side of recognising more."
  (when (listp result)
    (or (eq (alist-get 'ok result) t)
        (lark-auth--extract-user result)
        (lark-auth--extract-field result 'access_token)
        (lark-auth--extract-field result 'accessToken))))

(defun lark-auth--stop-polling ()
  "Cancel the device-code polling timer if one is running."
  (when (timerp lark-auth--polling-timer)
    (cancel-timer lark-auth--polling-timer))
  (setq lark-auth--polling-timer nil))

(defun lark-auth--finish-login (result)
  "Mark login complete, stop polling, and notify the user.
Idempotent — extra polling responses arriving after success are no-ops."
  (when lark-auth--polling-timer
    (lark-auth--stop-polling)
    (setq lark-auth--authenticated-p t)
    (let ((user (lark-auth--extract-user result)))
      (lark-auth--log "login complete; user=%S" user)
      (message "Lark: login successful%s"
               (if user (format " as %s" user) "!")))))

(defun lark-auth--check-status-async ()
  "Query `auth status' asynchronously; finish login if it reports a user.
Used as a fallback when the device-code poll's response shape doesn't
match `lark-auth--login-success-p' — auth status is the authoritative
source, so consulting it covers any CLI response we don't recognise."
  (lark--run-command
   '("auth" "status")
   (lambda (result)
     (lark-auth--log "status check: %S" result)
     (when (lark-auth--extract-user result)
       (lark-auth--finish-login result)))
   nil :literal t :no-error t))

(defun lark-auth--poll-once (device-code)
  "Run one device-code poll iteration."
  (lark--run-command
   (list "auth" "login" "--device-code" device-code "--json")
   (lambda (result)
     (lark-auth--log "poll response: %S" result)
     (if (lark-auth--login-success-p result)
         (lark-auth--finish-login result)
       ;; Response didn't look like success — cross-check with
       ;; `auth status' in case the CLI returned a shape we don't
       ;; recognise but did actually authenticate.
       (lark-auth--check-status-async)))
   nil :literal t :no-error t
   ;; Without :on-error, non-zero exits during the pending phase are
   ;; silently dropped (the success callback never fires), so failures
   ;; would be invisible.  Log and still consult `auth status'.
   :on-error
   (lambda (exit-code msg)
     (lark-auth--log "poll error: exit %s: %s" exit-code msg)
     (lark-auth--check-status-async))))

(defun lark-auth--start-polling (device-code)
  "Poll for device-code auth completion.
DEVICE-CODE is the code returned by the initial `auth login --no-wait'.
Each tick fires `lark-auth--poll-once', which both drives the CLI's
device-code exchange and cross-checks `auth status' so we don't miss
completion if the polling response shape changes."
  (lark-auth--stop-polling)
  (let ((attempts 0)
        (max-attempts 120)              ; 10 minutes at 5s intervals
        (interval 5))
    (lark-auth--log "polling started (device-code=%s)" device-code)
    (setq lark-auth--polling-timer
          (run-with-timer
           interval interval
           (lambda ()
             (setq attempts (1+ attempts))
             (cond
              ((null lark-auth--polling-timer) nil)   ; finished elsewhere
              ((>= attempts max-attempts)
               (lark-auth--stop-polling)
               (lark-auth--log "polling timed out after %d attempts" attempts)
               (message "Lark: login timed out — run `M-x lark-auth-login' to retry"))
              (t (lark-auth--poll-once device-code))))))))

;;;###autoload
(defun lark-auth-show-log ()
  "Pop up the *lark-log* buffer to inspect auth traces.
Use this after a failing `lark-auth-status' or login attempt to see the
raw response shapes recorded by `lark-auth--log'."
  (interactive)
  (let ((buf (get-buffer "*lark-log*")))
    (if buf
        (pop-to-buffer buf)
      (user-error "No *lark-log* buffer yet — run an auth command first"))))

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
