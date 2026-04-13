;;; lark-transient.el --- Transient dispatch menus for lark.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Provides the top-level transient dispatch menu and shared transient
;; infrastructure for all lark.el domain modules.

;;; Code:

(require 'transient)
(require 'lark-auth)

;; Autoloads for domain modules
(autoload 'lark-calendar-dispatch "lark-calendar" nil t)
(autoload 'lark-tasks-dispatch "lark-tasks" nil t)
(autoload 'lark-im-dispatch "lark-im" nil t)
(autoload 'lark-mail-dispatch "lark-mail" nil t)
(autoload 'lark-contacts-dispatch "lark-contact" nil t)
(autoload 'lark-docs-dispatch "lark-docs" nil t)

;;;; Top-level dispatch

;;;###autoload (autoload 'lark-dispatch "lark-transient" nil t)
(transient-define-prefix lark-dispatch ()
  "Lark/Feishu command dispatch."
  ["Navigation"
   ("c" "Calendar"   lark-calendar-dispatch)
   ("m" "Messenger"  lark-im-dispatch)
   ("d" "Docs"       lark-docs-dispatch)
   ("f" "Drive"      lark-drive-dispatch)
   ("b" "Base"       lark-base-dispatch)
   ("s" "Sheets"     lark-sheets-dispatch)
   ("t" "Tasks"      lark-tasks-dispatch)
   ("e" "Mail"       lark-mail-dispatch)
   ("w" "Wiki"       lark-wiki-dispatch)
   ("k" "Contacts"   lark-contacts-dispatch)
   ("v" "Meetings"   lark-meetings-dispatch)]
  ["Auth & Config"
   ("a" "Auth status"  lark-auth-status)
   ("L" "Login"        lark-auth-login)
   ("O" "Logout"       lark-auth-logout)
   ("I" "Identity"     lark-auth-switch-identity)
   ("S" "Schema"       lark-schema-lookup)]
  ["Options"
   ("-d" "Dry run"  "--dry-run")
   ("-f" "Format"   "--format=" :choices ("json" "table" "csv" "pretty"))])

;;;; Schema lookup

;;;###autoload
(defun lark-schema-lookup (method)
  "Look up the schema for a lark-cli METHOD."
  (interactive "sMethod (e.g., calendar.events.get): ")
  (lark--run-command
   (list "schema" method)
   (lambda (result)
     (let ((buf (get-buffer-create (format "*Lark Schema: %s*" method))))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (format "Schema: %s\n" method)
                   (make-string 60 ?─)
                   "\n\n"
                   (pp-to-string result)))
         (special-mode)
         (goto-char (point-min)))
       (pop-to-buffer buf)))
   nil
   :format "json"))

;;;; Placeholder dispatchers for domains not yet implemented
;; These will be replaced as each domain module is built.

(defmacro lark--define-stub-dispatch (name domain)
  "Define a stub transient dispatch for DOMAIN with NAME."
  `(progn
     ;;;###autoload (autoload ',name ,(format "lark-transient") nil t)
     (transient-define-prefix ,name ()
       ,(format "Lark %s commands (not yet implemented)." domain)
       [,(format "%s (coming soon)" domain)
        ("q" "Quit" transient-quit-all)])))

;; lark-docs-dispatch is now provided by lark-docs.el
(lark--define-stub-dispatch lark-drive-dispatch "Drive")
(lark--define-stub-dispatch lark-base-dispatch "Base")
(lark--define-stub-dispatch lark-sheets-dispatch "Sheets")
(lark--define-stub-dispatch lark-wiki-dispatch "Wiki")
;; lark-contacts-dispatch is now provided by lark-contact.el
(lark--define-stub-dispatch lark-meetings-dispatch "Meetings")

(provide 'lark-transient)
;;; lark-transient.el ends here
