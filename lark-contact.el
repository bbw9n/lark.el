;;; lark-contact.el --- Lark Contact integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Provides Contact domain commands for lark.el: user lookup, user
;; search, and a shared user-name resolution cache used by other
;; modules (IM, Calendar, Tasks, etc.).
;;
;; CLI command mapping:
;;   Get user:    contact +get-user [--user-id X] [--user-id-type Y]
;;   Search user: contact +search-user --query X

;;; Code:

(require 'lark-core)
(require 'transient)

;;;; Customization

(defgroup lark-contact nil
  "Lark Contact settings."
  :group 'lark
  :prefix "lark-contact-")

;;;; User cache

(defvar lark-contact--user-cache (make-hash-table :test 'equal)
  "Hash table mapping \"user_id:id_type\" to display name strings.
Populated lazily by `lark-contact-get-user' and
`lark-contact-resolve-name'.")

(defun lark-contact--cache-key (user-id id-type)
  "Build a cache key from USER-ID and ID-TYPE."
  (format "%s:%s" user-id (or id-type "open_id")))

(defun lark-contact--cache-get (user-id id-type)
  "Return cached display name for USER-ID / ID-TYPE, or nil."
  (gethash (lark-contact--cache-key user-id id-type)
           lark-contact--user-cache))

(defun lark-contact--cache-put (user-id id-type name)
  "Store NAME in cache for USER-ID / ID-TYPE."
  (puthash (lark-contact--cache-key user-id id-type) name
           lark-contact--user-cache))

;;;; Core: get-user (sync, used as fundamental utility)

(defun lark-contact--extract-user (data)
  "Extract the user alist from lark-cli response DATA."
  (or (lark--get-nested data 'data 'user)
      (alist-get 'user data)
      (alist-get 'data data)
      data))

(defun lark-contact--user-display-name (user)
  "Extract a display name from USER alist."
  (or (alist-get 'name user)
      (alist-get 'display_name user)
      (alist-get 'en_name user)
      ""))

(defun lark-contact-get-user-sync (user-id &optional id-type)
  "Fetch user info for USER-ID synchronously, return the user alist.
ID-TYPE defaults to \"open_id\".  Also accepts \"user_id\", \"union_id\"."
  (let ((id-type (or id-type "open_id")))
    (lark--run-command-sync
     (list "contact" "+get-user"
           "--user-id" user-id
           "--user-id-type" id-type))))

(defun lark-contact-resolve-name (user-id &optional id-type)
  "Return the display name for USER-ID (sync, cached).
ID-TYPE defaults to \"open_id\".  Returns USER-ID as fallback."
  (let ((id-type (or id-type "open_id")))
    (or (lark-contact--cache-get user-id id-type)
        (condition-case nil
            (let* ((data (lark-contact-get-user-sync user-id id-type))
                   (user (lark-contact--extract-user data))
                   (name (lark-contact--user-display-name user)))
              (when (and name (not (string-empty-p name)))
                (lark-contact--cache-put user-id id-type name)
                name))
          (error nil))
        user-id)))

;;;; Async get-user

;;;###autoload
(defun lark-contact-get-user (user-id &optional id-type callback)
  "Fetch user info for USER-ID asynchronously.
ID-TYPE defaults to \"open_id\".
CALLBACK, if non-nil, is called with the user alist.
When called interactively, displays the result in a detail buffer."
  (interactive
   (list (read-string "User ID: ")
         (completing-read "ID type: "
                          '("open_id" "user_id" "union_id") nil t nil nil "open_id")))
  (let ((id-type (or id-type "open_id")))
    (lark--run-command
     (list "contact" "+get-user"
           "--user-id" user-id
           "--user-id-type" id-type)
     (lambda (data)
       (let* ((user (lark-contact--extract-user data))
              (name (lark-contact--user-display-name user)))
         (when (and name (not (string-empty-p name)))
           (lark-contact--cache-put user-id id-type name))
         (if callback
             (funcall callback user)
           (lark-contact--display-user user user-id)))))))

;;;; User search

;;;###autoload
(defun lark-contact-search (query)
  "Search Lark contacts by QUERY."
  (interactive "sSearch contacts: ")
  (when (string-empty-p query)
    (user-error "Search query is required"))
  (message "Lark: searching contacts...")
  (lark--run-command
   (list "contact" "+search-user" "--query" query)
   #'lark-contact--display-search-results))

;;;; Display: user detail

(defun lark-contact--display-user (user user-id)
  "Display USER detail in a buffer.  USER-ID is used for the buffer name."
  (let* ((name (lark-contact--user-display-name user))
         (buf-name (format "*Lark User: %s*" (if (string-empty-p name) user-id name)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (if (string-empty-p name) user-id name) 'face 'bold) "\n"
                (make-string 60 ?─) "\n\n")
        (lark-contact--insert-field "Name" name)
        (lark-contact--insert-field "EN Name" (or (alist-get 'en_name user) ""))
        (lark-contact--insert-field "Email" (or (alist-get 'email user) ""))
        (lark-contact--insert-field "Mobile" (or (alist-get 'mobile user) ""))
        (lark-contact--insert-field "Open ID" (or (alist-get 'open_id user) ""))
        (lark-contact--insert-field "User ID" (or (alist-get 'user_id user) ""))
        (lark-contact--insert-field "Union ID" (or (alist-get 'union_id user) ""))
        (lark-contact--insert-field "Status"
                                    (let ((s (alist-get 'status user)))
                                      (cond
                                       ((stringp s) s)
                                       ((listp s) (if (eq (alist-get 'is_activated s) t)
                                                      "activated" "inactive"))
                                       (t ""))))
        (lark-contact--insert-field "Department"
                                    (let ((ids (alist-get 'department_ids user)))
                                      (if (and ids (listp ids))
                                          (mapconcat (lambda (x) (format "%s" x)) ids ", ")
                                        ""))))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun lark-contact--insert-field (label value)
  "Insert a LABEL: VALUE line if VALUE is non-empty."
  (when (and value (not (string-empty-p value)))
    (insert (propertize (format "  %-14s" (concat label ":")) 'face 'font-lock-keyword-face)
            value "\n")))

;;;; Display: search results

(defvar lark-contact-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-contact-open-at-point)
    (define-key map (kbd "y")   #'lark-contact-copy-id)
    (define-key map (kbd "?")   #'lark-contacts-dispatch)
    map)
  "Keymap for `lark-contact-search-mode'.")

(define-derived-mode lark-contact-search-mode tabulated-list-mode
  "Lark Contacts"
  "Major mode for browsing Lark contact search results."
  (setq tabulated-list-format
        [("Name" 24 t)
         ("EN Name" 20 t)
         ("Email" 30 t)
         ("Open ID" 28 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defvar-local lark-contact--users nil
  "Cached user list for the current buffer.")

(defun lark-contact--extract-users (data)
  "Extract user list from lark-cli response DATA."
  (or (lark--get-nested data 'data 'items)
      (lark--get-nested data 'data 'users)
      (alist-get 'items data)
      (alist-get 'users data)
      (and (listp data) (listp (car data))
           (alist-get 'open_id (car data))
           data)))

(defun lark-contact--make-user-entries (users)
  "Convert USERS to `tabulated-list-entries' format."
  (mapcar
   (lambda (user)
     (let ((id (or (alist-get 'open_id user) (alist-get 'user_id user) ""))
           (name (or (alist-get 'name user) (alist-get 'display_name user) ""))
           (en-name (or (alist-get 'en_name user) ""))
           (email (or (alist-get 'email user) "")))
       (list id (vector name en-name email id))))
   users))

(defun lark-contact--display-search-results (data)
  "Display contact search results DATA."
  (let* ((users (lark-contact--extract-users data))
         (buf (get-buffer-create "*Lark Contacts*")))
    (with-current-buffer buf
      (lark-contact-search-mode)
      (setq lark-contact--users users
            tabulated-list-entries (lark-contact--make-user-entries users))
      (tabulated-list-print t)
      (setq header-line-format
            (format " Lark Contacts — %d result(s)" (length users))))
    (pop-to-buffer buf)))

(defun lark-contact-open-at-point ()
  "Open user detail for the entry at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No contact at point"))
    (lark-contact-get-user id "open_id")))

(defun lark-contact-copy-id ()
  "Copy the open_id of the contact at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No contact at point"))
    (kill-new id)
    (message "Copied: %s" id)))

;;;; Cache management

(defun lark-contact-clear-cache ()
  "Clear the user name cache."
  (interactive)
  (clrhash lark-contact--user-cache)
  (message "Lark: contact cache cleared"))

;;;; Transient dispatch

;;;###autoload (autoload 'lark-contacts-dispatch "lark-contact" nil t)
(transient-define-prefix lark-contacts-dispatch ()
  "Lark Contacts commands."
  ["Contacts"
   ("g" "Get user"      lark-contact-get-user)
   ("s" "Search"        lark-contact-search)
   ("C" "Clear cache"   lark-contact-clear-cache)])

(provide 'lark-contact)
;;; lark-contact.el ends here
