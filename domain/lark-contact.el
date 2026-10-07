;;; lark-contact.el --- Lark Contact integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5

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
(require 'lark-ui)
(require 'transient)

;;;; Customization

(defgroup lark-contact nil
  "Lark Contact settings."
  :group 'lark
  :prefix "lark-contact-")

;;;; User cache
;;
;; Resolved names (open_id → display name) are kept in memory AND
;; persisted to disk, so a fresh Emacs session doesn't re-resolve the
;; same people over the network.  Entries carry a timestamp; stale
;; ones (people do get renamed) are dropped at load time and
;; re-resolve transparently on next use.

(defcustom lark-contact-cache-file
  (expand-file-name "lark.el/contact-names.eld"
                    (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "File persisting resolved contact names across sessions.
Set to nil to keep names in memory only."
  :type '(choice (const :tag "In-memory only" nil) file)
  :group 'lark-contact)

(defcustom lark-contact-cache-ttl (* 30 24 60 60)
  "Seconds before a persisted contact name is considered stale.
Stale entries are dropped when the cache file is loaded, so the
name re-resolves on next use.  nil = entries never expire."
  :type '(choice (const :tag "Never expires" nil) integer)
  :group 'lark-contact)

(defvar lark-contact--user-cache (make-hash-table :test 'equal)
  "Hash table mapping \"user_id:id_type\" to (NAME . TIMESTAMP).
Populated lazily by resolution and from `lark-contact-cache-file'.")

(defvar lark-contact--cache-loaded nil
  "Non-nil once the persisted cache has been read this session.")

(defvar lark-contact--cache-save-timer nil
  "Pending idle-timer that flushes the cache to disk, or nil.")

(defun lark-contact--cache-key (user-id id-type)
  "Build a cache key from USER-ID and ID-TYPE."
  (format "%s:%s" user-id (or id-type "open_id")))

(defun lark-contact--cache-load ()
  "Populate the in-memory cache from disk, once per session.
Entries older than `lark-contact-cache-ttl' are skipped."
  (unless lark-contact--cache-loaded
    (setq lark-contact--cache-loaded t)
    (when (and lark-contact-cache-file
               (file-readable-p lark-contact-cache-file))
      (condition-case nil
          (let ((now (float-time)))
            (dolist (entry (with-temp-buffer
                             (insert-file-contents lark-contact-cache-file)
                             (read (current-buffer))))
              (pcase entry
                (`(,key ,name . ,time)
                 (when (and (stringp key) (stringp name) (numberp time)
                            (or (null lark-contact-cache-ttl)
                                (< (- now time) lark-contact-cache-ttl))
                            ;; In-session resolutions win over disk.
                            (not (gethash key lark-contact--user-cache)))
                   (puthash key (cons name time)
                            lark-contact--user-cache))))))
        (error nil)))))

(defun lark-contact--cache-get (user-id id-type)
  "Return cached display name for USER-ID / ID-TYPE, or nil."
  (lark-contact--cache-load)
  (let ((v (gethash (lark-contact--cache-key user-id id-type)
                    lark-contact--user-cache)))
    (if (consp v) (car v) v)))

(defun lark-contact--cache-put (user-id id-type name)
  "Store NAME in cache for USER-ID / ID-TYPE and schedule a disk flush."
  (lark-contact--cache-load)
  (puthash (lark-contact--cache-key user-id id-type)
           (cons name (float-time))
           lark-contact--user-cache)
  (lark-contact--cache-schedule-save)
  name)

(defun lark-contact--cache-schedule-save ()
  "Flush the cache to disk on the next idle moment (debounced)."
  (when (and lark-contact-cache-file
             (null lark-contact--cache-save-timer))
    (setq lark-contact--cache-save-timer
          (run-with-idle-timer 2 nil #'lark-contact--cache-save))))

(defun lark-contact--cache-save ()
  "Write the in-memory name cache to `lark-contact-cache-file'."
  (setq lark-contact--cache-save-timer nil)
  (when lark-contact-cache-file
    (condition-case nil
        (let (entries)
          (maphash (lambda (k v)
                     (push (cons k (if (consp v) v (cons v (float-time))))
                           entries))
                   lark-contact--user-cache)
          (make-directory (file-name-directory lark-contact-cache-file) t)
          (with-temp-file lark-contact-cache-file
            (insert ";; lark.el contact name cache — safe to delete.\n")
            (prin1 entries (current-buffer))
            (insert "\n")))
      (error nil))))

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

;;;; Non-blocking name annotation
;;
;; Rendering a listing must never run a synchronous network RPC per
;; user id (that froze Emacs for seconds).  The pattern: insert the
;; cached name, or the raw id tagged with a `lark-contact-ref' text
;; property; after the buffer is displayed, call
;; `lark-contact-resolve-buffer-async' — it looks up each distinct
;; unresolved id once and patches every occurrence in place.

(defun lark-contact-annotate (user-id &optional id-type)
  "Return a display string for USER-ID without blocking.
The cached display name when known; otherwise USER-ID itself,
propertized with `lark-contact-ref' so a later
`lark-contact-resolve-buffer-async' pass can patch it in place."
  (let ((id-type (or id-type "open_id")))
    (or (lark-contact--cache-get user-id id-type)
        (propertize user-id 'lark-contact-ref (cons user-id id-type)))))

(defun lark-contact--unresolved-refs (buf)
  "Collect distinct `lark-contact-ref' values still shown as raw ids in BUF."
  (let (refs)
    (with-current-buffer buf
      (save-excursion
        (let ((pos (point-min)))
          (while (setq pos (text-property-not-all
                            pos (point-max) 'lark-contact-ref nil))
            (let ((end (or (next-single-property-change pos 'lark-contact-ref)
                           (point-max)))
                  (ref (get-text-property pos 'lark-contact-ref)))
              (when (equal (buffer-substring-no-properties pos end) (car ref))
                (push ref refs))
              (setq pos end))))))
    (delete-dups (nreverse refs))))

(defun lark-contact-resolve-buffer-async (buf)
  "Resolve user ids displayed raw in BUF and patch them in place.
One async lookup per distinct id; results land in the name cache
so later renders resolve synchronously.  Ids whose lookup returns
no name (restricted profiles, bots) are left as-is."
  (dolist (ref (lark-contact--unresolved-refs buf))
    (let ((ref ref))
      (lark--run-command
       (list "contact" "+get-user"
             "--user-id" (car ref)
             "--user-id-type" (cdr ref))
       (lambda (data)
         (let ((name (lark-contact--user-display-name
                      (lark-contact--extract-user data))))
           (when (and name (not (string-empty-p name)))
             (lark-contact--cache-put (car ref) (cdr ref) name)
             (lark-contact--patch-ref buf ref name))))
       nil :no-error t))))

(defun lark-contact--patch-ref (buf ref name)
  "Replace every raw display of REF in BUF with NAME.
The replaced text's properties are carried over so section-wide
properties (item ids, faces) stay intact."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (save-excursion
        (let ((inhibit-read-only t)
              (pos (point-min)))
          (while (setq pos (text-property-not-all
                            pos (point-max) 'lark-contact-ref nil))
            (let ((end (or (next-single-property-change pos 'lark-contact-ref)
                           (point-max)))
                  (this (get-text-property pos 'lark-contact-ref)))
              (if (and (equal this ref)
                       (equal (buffer-substring-no-properties pos end)
                              (car ref)))
                  (let ((props (text-properties-at pos)))
                    (goto-char pos)
                    (delete-region pos end)
                    (insert (apply #'propertize name props))
                    (setq pos (point)))
                (setq pos end)))))))))

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
                (lark-ui-separator 60) "\n\n")
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
  (lark-ui-insert-field label value))

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

;;;; AI context provider

(defun lark-contact--ai-context ()
  "Return the AI context plist for a contact search buffer."
  (let ((users lark-contact--users))
    (list :domain "contacts"
          :buffer-type "contact-search"
          :item nil
          :summary (format "Contact search results: %d users"
                           (length users)))))

(put 'lark-contact-search-mode 'lark-ai-context-provider
     #'lark-contact--ai-context)

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
