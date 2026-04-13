;;; lark-mail.el --- Lark Mail integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

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
(require 'shr)

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
  (let ((from (or (alist-get 'head_from mail)
                  (alist-get 'from mail)
                  (alist-get 'sender mail))))
    (cond
     ((stringp from) from)
     ((listp from)
      (let ((name (or (alist-get 'name from) ""))
            (addr (or (alist-get 'mail_address from)
                      (alist-get 'address from) "")))
        (cond
         ((and (not (string-empty-p name)) (not (string-empty-p addr)))
          (format "%s <%s>" name addr))
         ((not (string-empty-p name)) name)
         ((not (string-empty-p addr)) addr)
         (t ""))))
     (t ""))))

(defun lark-mail--mail-date (mail)
  "Extract the date from MAIL."
  (let ((date (or (alist-get 'date_formatted mail)
                  (alist-get 'date mail)
                  (alist-get 'send_time mail)
                  (alist-get 'internal_date mail)
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

;;;; Mail list mode (mu4e-like)

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
    (define-key map (kbd "n")   #'lark-mail--next)
    (define-key map (kbd "p")   #'lark-mail--prev)
    (define-key map (kbd "?")   #'lark-mail-dispatch)
    map)
  "Keymap for `lark-mail-mode'.")

(define-derived-mode lark-mail-mode special-mode
  "Lark Mail"
  "Major mode for browsing Lark mail, styled after mu4e.

\\{lark-mail-mode-map}")

(defun lark-mail--id-at-point ()
  "Return the mail ID at point, or nil."
  (get-text-property (point) 'lark-mail-id))

(defun lark-mail--next ()
  "Move to the next mail entry."
  (interactive)
  (let ((current (lark-mail--id-at-point))
        (pos (point)))
    (when current
      (while (and (not (eobp))
                  (equal (lark-mail--id-at-point) current))
        (forward-char)))
    (while (and (not (eobp))
                (not (lark-mail--id-at-point)))
      (forward-char))
    (when (eobp) (goto-char pos))
    (beginning-of-line)))

(defun lark-mail--prev ()
  "Move to the previous mail entry."
  (interactive)
  (let ((current (lark-mail--id-at-point))
        (pos (point)))
    (when current
      (while (and (not (bobp))
                  (equal (lark-mail--id-at-point) current))
        (backward-char)))
    (while (and (not (bobp))
                (not (lark-mail--id-at-point)))
      (backward-char))
    (if (lark-mail--id-at-point)
        (beginning-of-line)
      (goto-char pos))))

(defun lark-mail--insert-entry (mail)
  "Insert a single-line mu4e-style entry for MAIL."
  (let* ((id (lark-mail--mail-id mail))
         (unread (not (lark-mail--mail-read-p mail)))
         (flags (concat (if unread "N" " ")
                        (if (lark-mail--mail-has-attachment-p mail) "a" " ")))
         (date (lark-mail--mail-date mail))
         (from (lark-mail--mail-from mail))
         (subject (lark-mail--mail-subject mail))
         (face (if unread 'bold 'default))
         (beg (point)))
    (insert (propertize flags 'face 'font-lock-type-face)
            "  "
            (propertize (format "%-16s" date) 'face 'font-lock-comment-face)
            "  "
            (propertize (format "%-20s"
                                (truncate-string-to-width from 20 nil nil t))
                        'face face)
            "  "
            (propertize subject 'face face)
            "\n")
    (put-text-property beg (point) 'lark-mail-id id)))

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
  "Display mail list DATA for FOLDER in mu4e-like format."
  (let* ((mails (lark-mail--extract-mails data))
         (buf (get-buffer-create (format "*Lark Mail: %s*" folder))))
    (with-current-buffer buf
      (lark-mail-mode)
      (setq lark-mail--items mails
            lark-mail--folder folder)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null mails)
            (insert "  (no messages)\n")
          (dolist (mail mails)
            (lark-mail--insert-entry mail))))
      (goto-char (point-min))
      (setq header-line-format
            (format " Lark Mail: %s — %d message(s)  [N]=new [a]=attach"
                    folder (length mails))))
    (pop-to-buffer buf)))

;;;; Read mail
;; CLI: mail +message --message-id X


(defun lark-mail-read ()
  "Read the mail at point."
  (interactive)
  (let ((id (lark-mail--id-at-point)))
    (unless id (user-error "No mail at point"))
    (message "Lark: fetching mail...")
    (lark--run-command
     (list "mail" "+message" "--message-id" id)
     (lambda (data)
       (lark-mail--display-detail data id)))))

(defun lark-mail--header-field (label value)
  "Insert a mail header LABEL: VALUE line if VALUE is non-empty."
  (when (and value (stringp value) (not (string-empty-p value)))
    (insert (propertize (format "%-10s" (concat label ":")) 'face 'font-lock-keyword-face)
            value "\n")))

(defun lark-mail--format-address (addr)
  "Format a single address ADDR (string or alist) for display."
  (cond
   ((stringp addr) addr)
   ((listp addr)
    (let ((name (or (alist-get 'name addr) ""))
          (address (or (alist-get 'address addr)
                       (alist-get 'mail_address addr) "")))
      (cond
       ((and (not (string-empty-p name)) (not (string-empty-p address)))
        (format "%s <%s>" name address))
       ((not (string-empty-p name)) name)
       ((not (string-empty-p address)) address)
       (t ""))))
   (t (format "%s" addr))))

(defun lark-mail--format-address-list (addresses)
  "Format ADDRESSES (string, alist, or list) for display."
  (cond
   ((null addresses) "")
   ((stringp addresses) addresses)
   ;; Single address alist (not a list of addresses)
   ((and (listp addresses)
         (or (alist-get 'address addresses)
             (alist-get 'mail_address addresses)
             (alist-get 'name addresses)))
    (lark-mail--format-address addresses))
   ((listp addresses)
    (mapconcat #'lark-mail--format-address addresses ", "))
   (t (format "%s" addresses))))

(defun lark-mail--extract-body (mail)
  "Extract the best body from MAIL as (TYPE . CONTENT).
TYPE is `plain' or `html'.  Prefers HTML (rendered via shr); falls back
to plain text."
  (let ((html (alist-get 'body_html mail))
        (plain (alist-get 'body_plain_text mail))
        (body (or (alist-get 'body mail)
                  (alist-get 'text_body mail)
                  (alist-get 'content mail)
                  (alist-get 'plain_text mail)))
        (preview (alist-get 'body_preview mail)))
    (cond
     ((and (stringp html) (not (string-empty-p html)))
      (cons 'html html))
     ((and (stringp plain) (not (string-empty-p plain)))
      (cons 'plain plain))
     ((and (stringp body) (not (string-empty-p body)))
      (cons 'plain body))
     ((and (listp body)
           (let ((text (or (alist-get 'plain_text body)
                           (alist-get 'text body)
                           (alist-get 'content body))))
             (and (stringp text) (not (string-empty-p text))
                  (cons 'plain text)))))
     ((and (stringp preview) (not (string-empty-p preview)))
      (cons 'plain preview))
     (t (cons 'plain "")))))

(defun lark-mail--display-detail (data mail-id)
  "Display mail detail DATA for MAIL-ID."
  (let* ((mail (or (alist-get 'data data) data))
         (subject (lark-mail--mail-subject mail))
         (buf (get-buffer-create (format "*Lark Mail: %s*" subject))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq lark-mail--mail-id mail-id)
        ;; Subject line
        (insert (propertize subject 'face 'bold) "\n"
                (make-string (min 72 (max 20 (length subject))) ?─) "\n")
        ;; Headers
        (lark-mail--header-field "From"
          (lark-mail--format-address-list (or (alist-get 'head_from mail)
                                              (alist-get 'from mail)
                                              (alist-get 'sender mail))))
        (lark-mail--header-field "To"
          (lark-mail--format-address-list (alist-get 'to mail)))
        (lark-mail--header-field "Cc"
          (lark-mail--format-address-list (alist-get 'cc mail)))
        (lark-mail--header-field "Bcc"
          (lark-mail--format-address-list (alist-get 'bcc mail)))
        (lark-mail--header-field "Reply-To"
          (lark-mail--format-address-list (alist-get 'reply_to mail)))
        (lark-mail--header-field "Date" (lark-mail--mail-date mail))
        (lark-mail--header-field "Mail-ID" (or mail-id ""))
        ;; Folder / labels / state
        (lark-mail--header-field "Folder"
          (or (alist-get 'folder_id mail) ""))
        (let ((labels (or (alist-get 'labels mail)
                          (alist-get 'label_ids mail))))
          (when labels
            (lark-mail--header-field "Labels"
              (cond
               ((stringp labels) labels)
               ((listp labels) (mapconcat (lambda (l) (format "%s" l)) labels ", "))
               (t "")))))
        (lark-mail--header-field "State"
          (or (alist-get 'message_state_text mail) ""))
        (lark-mail--header-field "Thread"
          (or (alist-get 'thread_id mail) ""))
        ;; Attachments
        (let ((attachments (or (alist-get 'attachments mail)
                               (alist-get 'files mail))))
          (when (and (listp attachments) attachments)
            (insert "\n" (propertize "Attachments" 'face 'bold) "\n")
            (dolist (att attachments)
              (let* ((name (or (alist-get 'file_name att)
                               (alist-get 'name att)
                               (alist-get 'filename att) "unknown"))
                     (size (or (alist-get 'size att)
                               (alist-get 'file_size att)))
                     (mime (or (alist-get 'mime_type att)
                               (alist-get 'content_type att) "")))
                (insert "  " (propertize name 'face 'link))
                (when size
                  (insert (propertize (format "  (%s)" (lark-mail--format-size size))
                                      'face 'font-lock-comment-face)))
                (when (and mime (not (string-empty-p mime)))
                  (insert (propertize (format "  [%s]" mime)
                                      'face 'font-lock-comment-face)))
                (insert "\n")))))
        ;; Body
        (insert "\n" (make-string 72 ?─) "\n\n")
        (let* ((body-pair (lark-mail--extract-body mail))
               (body-type (car body-pair))
               (body-content (cdr body-pair)))
          (cond
           ((string-empty-p body-content)
            (insert (propertize "(no body)" 'face 'font-lock-comment-face) "\n"))
           ((eq body-type 'html)
            (let ((shr-use-fonts nil)
                  (shr-width (min 72 (window-width))))
              (shr-insert-document
               (with-temp-buffer
                 (insert body-content)
                 (libxml-parse-html-region (point-min) (point-max))))))
           (t
            (insert body-content "\n")))))
      (special-mode)
      (visual-line-mode 1)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun lark-mail--format-size (size)
  "Format byte SIZE as a human-readable string."
  (cond
   ((not (numberp size)) (format "%s" size))
   ((< size 1024) (format "%d B" size))
   ((< size (* 1024 1024)) (format "%.1f KB" (/ size 1024.0)))
   (t (format "%.1f MB" (/ size (* 1024.0 1024.0))))))

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
  (let ((id (or (lark-mail--id-at-point) lark-mail--mail-id)))
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
  (let ((id (or (lark-mail--id-at-point) lark-mail--mail-id)))
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
  (let ((id (lark-mail--id-at-point)))
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
  (let ((id (or (lark-mail--id-at-point) lark-mail--mail-id)))
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
