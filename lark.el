;;; lark.el --- Emacs interface to Lark/Feishu via lark-cli -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; URL: https://github.com/bbw9n/lark.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: comm, tools, agent

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; lark.el provides an Emacs interface to the Lark/Feishu platform by
;; wrapping the lark-cli command-line tool.  It covers 11 business
;; domains including Calendar, Messenger, Docs, Drive, Base, Sheets,
;; Tasks, Mail, Contacts, Wiki, and Meetings.
;;
;; The package uses transient menus for command dispatch.
;; All CLI interactions are async by default to keep Emacs responsive.
;;
;; Quick start:
;;   M-x lark-dispatch   or   C-c l l
;;
;; Prerequisites:
;;   - lark-cli installed and on PATH (https://github.com/larksuite/cli)
;;   - Emacs 28.1+ (for built-in transient)

;;; Code:

;;;; Load path setup for subdirectories

(defconst lark--directory
  (file-name-directory
   (or load-file-name
       (bound-and-true-p byte-compile-current-file)
       buffer-file-name))
  "Directory where lark.el is installed.")

(dolist (sub '("core" "domain" "ai"))
  (add-to-list 'load-path (expand-file-name sub lark--directory)))

(require 'lark-core)
(require 'lark-auth)
(require 'lark-transient)

;;;; Customization

(defgroup lark nil
  "Emacs interface to Lark/Feishu via lark-cli."
  :group 'comm
  :prefix "lark-")

(defcustom lark-cli-executable "lark-cli"
  "Path to the lark-cli executable."
  :type 'string
  :group 'lark)

(defcustom lark-default-format "json"
  "Default output format for lark-cli commands.
One of \"json\", \"table\", \"csv\", \"ndjson\", or \"pretty\"."
  :type '(choice (const "json")
          (const "table")
          (const "csv")
          (const "ndjson")
          (const "pretty"))
  :group 'lark)

(defcustom lark-default-identity "user"
  "Default identity for lark-cli commands.
Either \"user\" or \"bot\"."
  :type '(choice (const "user")
          (const "bot"))
  :group 'lark)

(defcustom lark-dry-run nil
  "When non-nil, pass --dry-run to all destructive lark-cli commands."
  :type 'boolean
  :group 'lark)

;;;; Prefix keymap

(autoload 'lark-calendar-agenda "lark-calendar" nil t)
(autoload 'lark-tasks-list "lark-tasks" nil t)
(autoload 'lark-im-dispatch "lark-im" nil t)
(autoload 'lark-mail-inbox "lark-mail" nil t)
(autoload 'lark-contacts-dispatch "lark-contact" nil t)
(autoload 'lark-docs-dispatch "lark-docs" nil t)
(autoload 'lark-docs-search "lark-docs" nil t)
(autoload 'lark-drive-dispatch "lark-drive" nil t)
(autoload 'lark-drive-list "lark-drive" nil t)
(autoload 'lark-ai-ask "lark-ai" nil t)
(autoload 'lark-ai-chat "lark-ai" nil t)
(autoload 'lark-ai-act "lark-ai" nil t)

(defvar lark-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'lark-dispatch)
    (define-key map (kbd "a") #'lark-calendar-agenda)
    (define-key map (kbd "m") #'lark-im-dispatch)
    (define-key map (kbd "t") #'lark-tasks-list)
    (define-key map (kbd "e") #'lark-mail-inbox)
    (define-key map (kbd "d") #'lark-docs-dispatch)
    (define-key map (kbd "f") #'lark-drive-list)
    (define-key map (kbd "k") #'lark-contacts-dispatch)
    (define-key map (kbd "s") #'lark-auth-status)
    (define-key map (kbd "i") #'lark-ai-ask)
    (define-key map (kbd "I") #'lark-ai-chat)
    (define-key map (kbd ".") #'lark-ai-act)
    map)
  "Keymap for Lark commands, intended to be bound under a prefix like C-c l.")

;;;###autoload
(defun lark-setup-prefix-key (&optional key)
  "Bind `lark-prefix-map' to KEY (default \"C-c l\") in `global-map'."
  (interactive)
  (global-set-key (kbd (or key "C-c l")) lark-prefix-map))

(provide 'lark)
;;; lark.el ends here
