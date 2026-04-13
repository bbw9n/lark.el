;;; lark-mail.el --- Lark Mail integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Provides Mail domain commands for lark.el: inbox listing, mail
;; reading, compose/send, reply, forward, draft management, and
;; search.
;;
;; CLI command mapping:
;;   Inbox/search: mail +triage [--query X] [--filter JSON] [--max N]
;;   Read message: mail +message --message-id X
;;   Send:         mail +send --to X --subject X --body X [--confirm-send]  (NO --format)
;;   Reply:        mail +reply --message-id X --body X [--confirm-send]     (NO --format)
;;   Forward:      mail +forward --message-id X --to X [--body X]           (NO --format)
;;   Drafts:       mail +draft-create --to X --subject X --body X
;;   List drafts:  mail user_mailbox.drafts list

;;; Code:

(require 'lark-core)
(require 'json)
(require 'transient)

;;;; Customization

(defgroup lark-mail nil
  "Lark Mail settings."
  :group 'lark
  :prefix "lark-mail-")

(defcustom lark-mail-page-size 20
  "Number of emails to fetch per page."
  :type 'integer
  :group 'lark-mail)

;;;; Buffer-local variables

(defvar-local lark-mail--items nil
  "Cached mail items for the current buffer.")

(defvar-local lark-mail--folder nil
  "Current folder/label for the mail buffer.")

(defvar-local lark-mail--mail-id nil
  "Mail ID for the current detail buffer.")

;;;; Mail parsing

(defun lark-mail--mail-id (mail)
  "Extract the mail ID from MAIL."
  (or (alist-get 'mail_id mail)
      (alist-get 'message_id mail)
      (alist-get 'id mail)))

(defun lark-mail--mail-subject (mail)
  "Extract the subject from MAIL."
  (or (alist-get 'subject mail)
      (alist-get 'title mail)
      "(no subject)"))

(defun lark-mail--mail-from (mail)
  "Extract the sender from MAIL."
  (let ((from (or (alist-get 'from mail)
                  (alist-get 'sender mail))))
    (cond
     ((stringp from) from)
     ((listp from) (or (alist-get 'name from)
                       (alist-get 'address from)
                       ""))
     (t ""))))

(defun lark-mail--mail-date (mail)
  "Extract the date from MAIL."
  (let ((date (or (alist-get 'date mail)
                  (alist-get 'send_time mail)
                  (alist-get 'timestamp mail))))
    (cond
     ((numberp date) (or (lark--format-timestamp date) ""))
     ((stringp date) (substring date 0 (min 16 (length date))))
     (t ""))))

(defun lark-mail--mail-read-p (mail)
  "Return whether MAIL has been read."
  (let ((read (or (alist-get 'is_read mail)
                  (alist-get 'read mail))))
    (and read (not (eq read :false)) (not (equal read 0)))))

(defun lark-mail--mail-has-attachment-p (mail)
  "Return whether MAIL has attachments."
  (let ((att (or (alist-get 'has_attachment mail)
                 (alist-get 'attachments mail))))
    (cond
     ((eq att t) t)
     ((and (listp att) att) t)
     (t nil))))

(defun lark-mail--extract-mails (data)
  "Extract mail list from lark-cli response DATA."
  (cond
   ((and (listp data) (alist-get 'items data))
    (alist-get 'items data))
   ((and (listp data) (alist-get 'mails data))
    (alist-get 'mails data))
   ((and (listp data) (alist-get 'messages data))
    (alist-get 'messages data))
   ((and (listp data) (alist-get 'data data))
    (let ((inner (alist-get 'data data)))
      (or (alist-get 'items inner)
          (alist-get 'mails inner)
          (alist-get 'messages inner)
          (and (listp inner) inner))))
   ((and (listp data) (listp (car data))
         (or (alist-get 'mail_id (car data))
             (alist-get 'message_id (car data))
             (alist-get 'subject (car data))))
    data)
   (t nil)))

(defun lark-mail--make-entries (mails)
  "Convert MAILS to `tabulated-list-entries' format."
  (mapcar
   (lambda (mail)
     (let ((id (lark-mail--mail-id mail))
           (read (if (lark-mail--mail-read-p mail) " " "*"))
           (from (lark-mail--mail-from mail))
           (subject (lark-mail--mail-subject mail))
           (date (lark-mail--mail-date mail))
           (att (if (lark-mail--mail-has-attachment-p mail) "@" " ")))
       (list id (vector
                 (propertize read 'face (if (equal read "*")
                                            'bold 'default))
                 from subject date att))))
   mails))

;;;; Tabulated list mode

(defvar lark-mail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-mail-read)
    (define-key map (kbd "g")   #'lark-mail-refresh)
    (define-key map (kbd "c")   #'lark-mail-compose)
    (define-key map (kbd "r")   #'lark-mail-reply)
    (define-key map (kbd "f")   #'lark-mail-forward)
    (define-key map (kbd "d")   #'lark-mail-delete)
    (define-key map (kbd "/")   #'lark-mail-search)
    (define-key map (kbd "y")   #'lark-mail-copy-id)
    (define-key map (kbd "?")   #'lark-mail-dispatch)
    map)
  "Keymap for `lark-mail-mode'.")

(define-derived-mode lark-mail-mode tabulated-list-mode
  "Lark Mail"
  "Major mode for browsing Lark mail."
  (setq tabulated-list-format
        [("" 2 nil)                     ; unread marker
         ("From" 25 t)
         ("Subject" 45 t)
         ("Date" 16 t)
         ("" 2 nil)])                   ; attachment marker
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

;;;; Inbox
;; CLI: mail +triage [--query X] [--max N] [--format json]


;;;###autoload
(defun lark-mail-inbox ()
  "Show the Lark mail inbox."
  (interactive)
  (message "Lark: fetching inbox...")
  (lark--run-command
   (list "mail" "+triage"
         "--max" (number-to-string lark-mail-page-size))
   (lambda (data) (lark-mail--display-list data "Inbox"))
   nil
   :format "json"))

(defun lark-mail-refresh ()
  "Refresh the current mail list buffer."
  (interactive)
  (lark-mail-inbox))

(defun lark-mail--display-list (data folder)
  "Display mail list DATA for FOLDER."
  (let* ((mails (lark-mail--extract-mails data))
         (buf (get-buffer-create (format "*Lark Mail: %s*" folder))))
    (with-current-buffer buf
      (lark-mail-mode)
      (setq lark-mail--items mails
            lark-mail--folder folder
            tabulated-list-entries (lark-mail--make-entries mails))
      (tabulated-list-print t)
      (setq header-line-format
            (format " Lark Mail: %s — %d message(s)" folder (length mails))))
    (pop-to-buffer buf)))

;;;; Read mail
;; CLI: mail +message --message-id X


(defun lark-mail-read ()
  "Read the mail at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No mail at point"))
    (message "Lark: fetching mail...")
    (lark--run-command
     (list "mail" "+message" "--message-id" id)
     (lambda (data)
       (lark-mail--display-detail data id)))))

(defun lark-mail--display-detail (data mail-id)
  "Display mail detail DATA for MAIL-ID."
  (let* ((mail (or (alist-get 'data data) data))
         (subject (lark-mail--mail-subject mail))
         (buf (get-buffer-create (format "*Lark Mail: %s*" subject))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq lark-mail--mail-id mail-id)
        ;; Headers
        (insert (propertize subject 'face 'bold) "\n"
                (make-string (min 72 (max 20 (length subject))) ?─) "\n\n")
        (let ((from (lark-mail--mail-from mail))
              (to (lark-mail--format-recipients
                   (or (alist-get 'to mail) nil)))
              (cc (lark-mail--format-recipients
                   (or (alist-get 'cc mail) nil)))
              (date (lark-mail--mail-date mail)))
          (insert (propertize "From: " 'face 'bold) from "\n")
          (unless (string-empty-p to)
            (insert (propertize "To: " 'face 'bold) to "\n"))
          (unless (string-empty-p cc)
            (insert (propertize "Cc: " 'face 'bold) cc "\n"))
          (unless (string-empty-p date)
            (insert (propertize "Date: " 'face 'bold) date "\n")))
        ;; Attachments
        (let ((attachments (alist-get 'attachments mail)))
          (when attachments
            (insert "\n" (propertize "Attachments:" 'face 'bold) "\n")
            (dolist (att attachments)
              (let ((name (or (alist-get 'file_name att)
                              (alist-get 'name att) "unknown")))
                (insert "  " name "\n")))))
        ;; Body
        (insert "\n" (make-string 72 ?─) "\n\n")
        (let ((body (or (alist-get 'body mail)
                        (alist-get 'text_body mail)
                        (alist-get 'content mail) "")))
          (if (stringp body)
              (insert body)
            (insert (or (alist-get 'text body)
                        (alist-get 'content body)
                        (pp-to-string body)))))
        (insert "\n\n"
                (propertize "Mail ID: " 'face 'font-lock-comment-face)
                (or mail-id "") "\n"))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun lark-mail--format-recipients (recipients)
  "Format RECIPIENTS list to a display string."
  (cond
   ((null recipients) "")
   ((stringp recipients) recipients)
   ((listp recipients)
    (mapconcat
     (lambda (r)
       (if (stringp r) r
         (or (alist-get 'name r)
             (alist-get 'address r)
             (format "%s" r))))
     recipients ", "))
   (t (format "%s" recipients))))

;;;; Compose / Send
;; CLI: mail +send --to X --subject X --body X [--cc X] [--confirm-send]


;;;###autoload (autoload 'lark-mail-compose "lark-mail" nil t)
(transient-define-prefix lark-mail-compose ()
  "Compose a new Lark mail."
  ["Headers"
   ("t" "To"      "--to=" :prompt "To (email addresses, comma-separated): ")
   ("c" "Cc"      "--cc=" :prompt "Cc: ")
   ("s" "Subject" "--subject=" :prompt "Subject: ")]
  ["Body"
   ("b" "Body"    "--body=" :prompt "Body text: ")]
  ["Actions"
   ("RET" "Send"        lark-mail--do-send)
   ("d"   "Save draft"  lark-mail--do-save-draft)
   ("q"   "Cancel"      transient-quit-all)])

(defun lark-mail--do-send (&rest _args)
  "Execute mail send with transient arguments."
  (interactive)
  (let ((args (transient-args 'lark-mail-compose)))
    (unless args (user-error "No mail details provided"))
    (when (yes-or-no-p "Send this email? ")
      (message "Lark: sending mail...")
      (lark--run-command
       (append '("mail" "+send" "--confirm-send") args)
       (lambda (_data)
         (message "Lark: mail sent"))))))

(defun lark-mail--do-save-draft (&rest _args)
  "Save mail as draft with transient arguments."
  (interactive)
  (let ((args (transient-args 'lark-mail-compose)))
    (unless args (user-error "No mail details provided"))
    (message "Lark: saving draft...")
    (lark--run-command
     (append '("mail" "+draft-create") args)
     (lambda (_data)
       (message "Lark: draft saved")))))

;;;; Reply
;; CLI: mail +reply --message-id X --body X [--confirm-send]

(defun lark-mail-reply ()
  "Reply to the mail at point or in the current detail buffer."
  (interactive)
  (let ((id (or (tabulated-list-get-id) lark-mail--mail-id)))
    (unless id (user-error "No mail selected"))
    (let ((body (read-string "Reply: ")))
      (when (string-empty-p body)
        (user-error "Empty reply"))
      (when (yes-or-no-p "Send this reply? ")
        (message "Lark: sending reply...")
        (lark--run-command
         (list "mail" "+reply" "--message-id" id
               "--body" body "--confirm-send")
         (lambda (_data)
           (message "Lark: reply sent")))))))

;;;; Forward
;; CLI: mail +forward --message-id X --to X [--body X] [--confirm-send]

(defun lark-mail-forward ()
  "Forward the mail at point."
  (interactive)
  (let ((id (or (tabulated-list-get-id) lark-mail--mail-id)))
    (unless id (user-error "No mail selected"))
    (let ((to (read-string "Forward to (email): ")))
      (when (string-empty-p to)
        (user-error "No recipient specified"))
      (when (yes-or-no-p (format "Forward to %s? " to))
        (message "Lark: forwarding mail...")
        (lark--run-command
         (list "mail" "+forward" "--message-id" id
               "--to" to "--confirm-send")
         (lambda (_data)
           (message "Lark: mail forwarded")))))))

;;;; Delete
;; CLI: mail user_mailbox.messages delete --params '{"user_mailbox_id":"me","message_id":"X"}'


(defun lark-mail-delete ()
  "Delete the mail at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No mail at point"))
    (when (yes-or-no-p (format "Delete mail %s? " id))
      (message "Lark: deleting mail...")
      (let ((params (json-encode `((user_mailbox_id . "me")
                                   (message_id . ,id)))))
        (lark--run-command
         (list "mail" "user_mailbox.messages" "delete" "--params" params)
         (lambda (_data)
           (message "Lark: mail deleted")
           (lark-mail-refresh)))))))

;;;; Search
;; CLI: mail +triage --query X [--max N] [--format json]


;;;###autoload
(defun lark-mail-search (query)
  "Search Lark mail for QUERY."
  (interactive "sSearch mail: ")
  (message "Lark: searching mail...")
  (lark--run-command
   (list "mail" "+triage" "--query" query
         "--max" (number-to-string lark-mail-page-size))
   (lambda (data)
     (lark-mail--display-list data (format "Search: %s" query)))
   nil
   :format "json"))

;;;; Copy mail ID

(defun lark-mail-copy-id ()
  "Copy the mail ID at point to the kill ring."
  (interactive)
  (let ((id (or (tabulated-list-get-id) lark-mail--mail-id)))
    (unless id (user-error "No mail ID"))
    (kill-new id)
    (message "Copied: %s" id)))

;;;; Drafts
;; CLI: mail user_mailbox.drafts list


;;;###autoload
(defun lark-mail-drafts ()
  "List Lark mail drafts."
  (interactive)
  (message "Lark: fetching drafts...")
  (lark--run-command
   '("mail" "user_mailbox.drafts" "list")
   (lambda (data)
     (lark-mail--display-list data "Drafts"))))

;;;; Transient dispatch

;;;###autoload (autoload 'lark-mail-dispatch "lark-mail" nil t)
(transient-define-prefix lark-mail-dispatch ()
  "Lark Mail commands."
  ["Browse"
   ("i" "Inbox"       lark-mail-inbox)
   ("d" "Drafts"      lark-mail-drafts)
   ("/" "Search"      lark-mail-search)]
  ["Compose"
   ("c" "Compose"     lark-mail-compose)
   ("r" "Reply"       lark-mail-reply)
   ("f" "Forward"     lark-mail-forward)]
  ["At Point"
   ("RET" "Read"      lark-mail-read)
   ("x"   "Delete"    lark-mail-delete)])

(provide 'lark-mail)
;;; lark-mail.el ends here
