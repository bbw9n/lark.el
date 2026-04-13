;;; lark-drive.el --- Lark Drive integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; Provides a dired-like file browser for Lark Drive, plus upload,
;; download, copy, move, import, and export operations.
;;
;; CLI command mapping:
;;   List folder:    drive files list --params '{"folder_token":"X"}' --page-all
;;   Create folder:  drive files create_folder --data '{"name":"X","folder_token":"Y"}'
;;   Copy file:      drive files copy --params '{"file_token":"X"}' --data '{"name":"Y","type":"Z","folder_token":"W"}'
;;   Upload:         drive +upload --file X [--folder-token Y] [--name Z]
;;   Download:       drive +download --file-token X [--output Y]
;;   Move:           drive +move --file-token X --folder-token Y [--type Z]
;;   Import:         drive +import --file X [--folder-token Y] [--type Z]
;;   Export:         drive +export --token X --doc-type Y --file-extension Z [--output-dir W]

;;; Code:

(require 'lark-core)
(require 'lark-contact)
(require 'json)
(require 'transient)

;;;; Customization

(defgroup lark-drive nil
  "Lark Drive settings."
  :group 'lark
  :prefix "lark-drive-")

;;;; Buffer-local variables

(defvar-local lark-drive--folder-token nil
  "Current folder token.  Empty string or nil means root.")

(defvar-local lark-drive--folder-name nil
  "Current folder display name.")

(defvar-local lark-drive--parent-token nil
  "Parent folder token for \"go up\" navigation.")

(defvar-local lark-drive--files nil
  "Cached file list for the current folder.")

(defvar-local lark-drive--folder-history nil
  "Stack of (token . name) for breadcrumb navigation.")

;;;; File field extractors

(defun lark-drive--file-token (file)
  "Extract the token from FILE."
  (or (alist-get 'token file) (alist-get 'file_token file) ""))

(defun lark-drive--file-name (file)
  "Extract the name from FILE."
  (or (alist-get 'name file) (alist-get 'title file) "(unnamed)"))

(defun lark-drive--file-type (file)
  "Extract the type from FILE."
  (or (alist-get 'type file) "file"))

(defun lark-drive--file-url (file)
  "Extract the URL from FILE."
  (or (alist-get 'url file) ""))

(defun lark-drive--file-owner-id (file)
  "Extract the owner ID from FILE."
  (or (alist-get 'owner_id file) ""))

(defun lark-drive--file-modified-time (file)
  "Extract and format the modified time from FILE."
  (let ((ts (alist-get 'modified_time file)))
    (cond
     ((and (stringp ts) (not (string-empty-p ts)))
      (or (lark--format-timestamp ts) ts))
     ((numberp ts) (or (lark--format-timestamp ts) ""))
     (t ""))))

(defun lark-drive--file-created-time (file)
  "Extract and format the created time from FILE."
  (let ((ts (alist-get 'created_time file)))
    (cond
     ((and (stringp ts) (not (string-empty-p ts)))
      (or (lark--format-timestamp ts) ts))
     ((numberp ts) (or (lark--format-timestamp ts) ""))
     (t ""))))

(defun lark-drive--file-parent (file)
  "Extract the parent token from FILE."
  (or (alist-get 'parent_token file) ""))

(defun lark-drive--folder-p (file)
  "Return non-nil if FILE is a folder."
  (equal (lark-drive--file-type file) "folder"))

(defun lark-drive--type-indicator (type)
  "Return a display indicator for file TYPE."
  (pcase type
    ("folder"   "/")
    ("doc"      " [doc]")
    ("docx"     " [docx]")
    ("sheet"    " [sheet]")
    ("bitable"  " [bitable]")
    ("mindnote" " [mindnote]")
    ("slides"   " [slides]")
    (_          "")))

;;;; Response extraction

(defun lark-drive--extract-files (data)
  "Extract the file list from lark-cli response DATA."
  (or (lark--get-nested data 'data 'files)
      (alist-get 'files data)
      (lark--get-nested data 'data 'items)
      (alist-get 'items data)))

;;;; Dired-like mode

(defvar lark-drive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-drive-open)
    (define-key map (kbd "^")   #'lark-drive-up)
    (define-key map (kbd "g")   #'lark-drive-refresh)
    (define-key map (kbd "+")   #'lark-drive-mkdir)
    (define-key map (kbd "C")   #'lark-drive-copy)
    (define-key map (kbd "R")   #'lark-drive-move)
    (define-key map (kbd "U")   #'lark-drive-upload)
    (define-key map (kbd "D")   #'lark-drive-download)
    (define-key map (kbd "I")   #'lark-drive-import)
    (define-key map (kbd "E")   #'lark-drive-export)
    (define-key map (kbd "w")   #'lark-drive-copy-token)
    (define-key map (kbd "W")   #'lark-drive-copy-url)
    (define-key map (kbd "v")   #'lark-drive-view-content)
    (define-key map (kbd "n")   #'lark-drive--next-file)
    (define-key map (kbd "p")   #'lark-drive--prev-file)
    (define-key map (kbd "?")   #'lark-drive-dispatch)
    map)
  "Keymap for `lark-drive-mode'.
Keybindings follow dired conventions where possible.")

(define-derived-mode lark-drive-mode special-mode
  "Lark Drive"
  "Major mode for browsing Lark Drive, modelled after dired.

\\{lark-drive-mode-map}")

;;;; Text properties & navigation

(defun lark-drive--file-at-point ()
  "Return the file alist at point, or nil."
  (get-text-property (point) 'lark-drive-file))

(defun lark-drive--token-at-point ()
  "Return the file token at point, or nil."
  (let ((f (lark-drive--file-at-point)))
    (when f (lark-drive--file-token f))))

(defun lark-drive--next-file ()
  "Move to the next file entry."
  (interactive)
  (let ((current (lark-drive--token-at-point))
        (pos (point)))
    (when current
      (while (and (not (eobp))
                  (equal (lark-drive--token-at-point) current))
        (forward-char)))
    (while (and (not (eobp))
                (not (lark-drive--file-at-point)))
      (forward-char))
    (when (eobp) (goto-char pos))))

(defun lark-drive--prev-file ()
  "Move to the previous file entry."
  (interactive)
  (let ((current (lark-drive--token-at-point))
        (pos (point)))
    (when current
      (while (and (not (bobp))
                  (equal (lark-drive--token-at-point) current))
        (backward-char)))
    (while (and (not (bobp))
                (not (lark-drive--file-at-point)))
      (backward-char))
    (let ((target (lark-drive--token-at-point)))
      (if target
          (while (and (not (bobp))
                      (equal (lark-drive--token-at-point) target))
            (backward-char))
        (goto-char pos)))
    ;; Move forward onto the entry start
    (unless (lark-drive--file-at-point)
      (forward-char))))

;;;; Rendering

(defun lark-drive--insert-file-entry (file)
  "Insert a single-line entry for FILE, like a dired listing."
  (let* ((name (lark-drive--file-name file))
         (type (lark-drive--file-type file))
         (modified (lark-drive--file-modified-time file))
         (owner-id (lark-drive--file-owner-id file))
         (owner-name (if (string-empty-p owner-id) ""
                       (lark-contact-resolve-name owner-id "open_id")))
         (folder-p (lark-drive--folder-p file))
         (indicator (lark-drive--type-indicator type))
         (beg (point)))
    (insert "  "
            (propertize (format "%-8s" type)
                        'face 'font-lock-type-face)
            "  "
            (propertize (format "%-16s" modified)
                        'face 'font-lock-comment-face)
            "  "
            (propertize (format "%-12s" owner-name)
                        'face 'font-lock-string-face)
            "  "
            (propertize (concat name indicator)
                        'face (if folder-p 'bold 'default))
            "\n")
    (put-text-property beg (point) 'lark-drive-file file)))

(defun lark-drive--render (files folder-token folder-name)
  "Render FILES into the drive buffer for FOLDER-TOKEN / FOLDER-NAME."
  (let ((buf (get-buffer-create "*Lark Drive*"))
        ;; Sort: folders first, then by name
        (sorted (sort (copy-sequence files)
                      (lambda (a b)
                        (let ((fa (lark-drive--folder-p a))
                              (fb (lark-drive--folder-p b)))
                          (cond
                           ((and fa (not fb)) t)
                           ((and (not fa) fb) nil)
                           (t (string< (downcase (lark-drive--file-name a))
                                       (downcase (lark-drive--file-name b))))))))))
    (with-current-buffer buf
      (lark-drive-mode)
      (setq lark-drive--files files
            lark-drive--folder-token folder-token
            lark-drive--folder-name (or folder-name "My Drive"))
      ;; Derive parent from first file's parent if we're not at root
      (setq lark-drive--parent-token
            (when (and files (not (string-empty-p (or folder-token ""))))
              (lark-drive--file-parent (car files))))
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Header / breadcrumb
        (insert (propertize (format "  %s" (or folder-name "My Drive")) 'face 'bold))
        (when (and folder-token (not (string-empty-p folder-token)))
          (insert (propertize (format "  [%s]" folder-token) 'face 'font-lock-comment-face)))
        (insert "\n"
                (make-string 60 ?─) "\n")
        ;; ".." entry for going up
        (when (and folder-token (not (string-empty-p folder-token)))
          (let ((beg (point)))
            (insert "  "
                    (propertize "folder  " 'face 'font-lock-type-face)
                    "  "
                    (propertize (format "%-16s" "") 'face 'font-lock-comment-face)
                    "  "
                    (propertize ".." 'face 'bold)
                    "\n")
            (put-text-property beg (point) 'lark-drive-file '((name . "..") (type . "parent")))))
        ;; File entries
        (if (null sorted)
            (insert "  (empty folder)\n")
          (dolist (file sorted)
            (lark-drive--insert-file-entry file))))
      (goto-char (point-min))
      ;; Move to first file entry
      (while (and (not (eobp)) (not (lark-drive--file-at-point)))
        (forward-line 1))
      (setq header-line-format
            (format " Lark Drive: %s — %d item(s)"
                    (or folder-name "My Drive") (length files))))
    (pop-to-buffer buf)))

;;;; List folder / browse

;;;###autoload
(defun lark-drive-list (&optional folder-token folder-name)
  "List files in FOLDER-TOKEN (nil/empty for root).
FOLDER-NAME is used for display."
  (interactive)
  (message "Lark: listing drive folder...")
  (let ((token (or folder-token ""))
        (name (or folder-name "My Drive")))
    (lark--run-command
     (list "drive" "files" "list"
           "--params" (json-encode `((folder_token . ,token))))
     (lambda (data)
       (let ((files (lark-drive--extract-files data)))
         (lark-drive--render files token name))))))

(defun lark-drive-refresh ()
  "Refresh the current folder listing."
  (interactive)
  (lark-drive-list lark-drive--folder-token lark-drive--folder-name))

(defun lark-drive-open ()
  "Open the item at point.
Folders descend into them; files show metadata."
  (interactive)
  (let ((file (lark-drive--file-at-point)))
    (unless file (user-error "No file at point"))
    (let ((type (lark-drive--file-type file))
          (token (lark-drive--file-token file))
          (name (lark-drive--file-name file)))
      (cond
       ;; ".." — go up
       ((equal type "parent")
        (lark-drive-up))
       ;; Folder — descend
       ((equal type "folder")
        ;; Push current location to history
        (push (cons lark-drive--folder-token lark-drive--folder-name)
              lark-drive--folder-history)
        (lark-drive-list token name))
       ;; File — show detail
       (t
        (lark-drive--show-detail file))))))

(defun lark-drive-up ()
  "Go up to the parent folder."
  (interactive)
  (if lark-drive--folder-history
      ;; Use history stack for accurate back-navigation
      (let ((prev (pop lark-drive--folder-history)))
        (lark-drive-list (car prev) (cdr prev)))
    ;; Fallback: go to root
    (lark-drive-list "" "My Drive")))

;;;; File detail view

(defvar-local lark-drive--detail-file nil
  "The file alist displayed in this detail buffer.")

(defvar lark-drive-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-drive-detail-view-content)
    (define-key map (kbd "?")   #'lark-drive-dispatch)
    map)
  "Keymap for `lark-drive-detail-mode'.")

(define-derived-mode lark-drive-detail-mode special-mode
  "Lark Drive Detail"
  "Major mode for viewing Lark Drive file detail.
Press RET to view document content (doc/docx/sheet).

\\{lark-drive-detail-mode-map}")

(defun lark-drive-detail-view-content ()
  "View the content of the file shown in this detail buffer."
  (interactive)
  (unless lark-drive--detail-file
    (user-error "No file in this buffer"))
  (let ((type (lark-drive--file-type lark-drive--detail-file))
        (url (lark-drive--file-url lark-drive--detail-file))
        (token (lark-drive--file-token lark-drive--detail-file)))
    (let ((ref (if (and url (not (string-empty-p url))) url token)))
      (when (string-empty-p ref)
        (user-error "No URL or token available for this file"))
      (pcase type
        ((or "doc" "docx")
         (require 'lark-docs)
         (lark-docs-fetch ref))
        ("sheet"
         (require 'lark-sheets)
         (lark-sheets-info ref))
        (_
         (user-error "Viewing content is not supported for type \"%s\"" type))))))

(defun lark-drive--show-detail (file)
  "Display detail for FILE in a buffer."
  (let* ((name (lark-drive--file-name file))
         (type (lark-drive--file-type file))
         (owner-id (lark-drive--file-owner-id file))
         (owner-name (if (string-empty-p owner-id) ""
                       (lark-contact-resolve-name owner-id "open_id")))
         (viewable (member type '("doc" "docx" "sheet")))
         (buf (get-buffer-create (format "*Lark Drive: %s*" name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize name 'face 'bold) "\n"
                (make-string (min 60 (max 20 (length name))) ?─) "\n\n")
        (lark-drive--detail-field "Type" type)
        (lark-drive--detail-field "Token" (lark-drive--file-token file))
        (lark-drive--detail-field "Modified" (lark-drive--file-modified-time file))
        (lark-drive--detail-field "Created" (lark-drive--file-created-time file))
        (lark-drive--detail-field "Owner" owner-name)
        (lark-drive--detail-field "URL" (lark-drive--file-url file))
        (when viewable
          (insert "\n" (propertize "Press RET to view content" 'face 'font-lock-comment-face) "\n")))
      (lark-drive-detail-mode)
      (setq lark-drive--detail-file file)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun lark-drive--detail-field (label value)
  "Insert LABEL: VALUE if VALUE is non-empty."
  (when (and value (not (string-empty-p value)))
    (insert (propertize (format "  %-12s" (concat label ":")) 'face 'font-lock-keyword-face)
            value "\n")))

;;;; View document content

(defun lark-drive-view-content ()
  "Fetch and view the content of the doc/docx/sheet at point.
Uses the file URL to dispatch to `lark-docs-fetch' or `lark-sheets-info'."
  (interactive)
  (let ((file (lark-drive--file-at-point)))
    (unless file (user-error "No file at point"))
    (let ((type (lark-drive--file-type file))
          (url (lark-drive--file-url file))
          (token (lark-drive--file-token file)))
      (when (lark-drive--folder-p file)
        (user-error "Cannot view content of a folder"))
      (let ((ref (if (and url (not (string-empty-p url))) url token)))
        (when (string-empty-p ref)
          (user-error "No URL or token available for this file"))
        (pcase type
          ((or "doc" "docx")
           (require 'lark-docs)
           (lark-docs-fetch ref))
          ("sheet"
           (require 'lark-sheets)
           (lark-sheets-info ref))
          (_
           (user-error "Viewing content is not supported for type \"%s\"" type)))))))

;;;; Create folder (dired "+")

(defun lark-drive-mkdir (name)
  "Create a new folder named NAME in the current folder."
  (interactive "sNew folder name: ")
  (when (string-empty-p name)
    (user-error "Folder name is required"))
  (message "Lark: creating folder...")
  (let ((folder-token (or lark-drive--folder-token "")))
    (lark--run-command
     (list "drive" "files" "create_folder"
           "--data" (json-encode `((name . ,name)
                                   (folder_token . ,folder-token))))
     (lambda (_data)
       (message "Lark: folder \"%s\" created" name)
       (lark-drive-refresh)))))

;;;; Copy file (dired "C")

(defun lark-drive-copy ()
  "Copy the file at point to the current folder with a new name."
  (interactive)
  (let ((file (lark-drive--file-at-point)))
    (unless file (user-error "No file at point"))
    (let* ((token (lark-drive--file-token file))
           (type (lark-drive--file-type file))
           (old-name (lark-drive--file-name file))
           (new-name (read-string (format "Copy \"%s\" as: " old-name) old-name))
           (target-folder (or lark-drive--folder-token "")))
      (when (string-empty-p new-name)
        (user-error "Name is required"))
      (message "Lark: copying file...")
      (lark--run-command
       (list "drive" "files" "copy"
             "--params" (json-encode `((file_token . ,token)))
             "--data" (json-encode `((name . ,new-name)
                                     (type . ,type)
                                     (folder_token . ,target-folder))))
       (lambda (_data)
         (message "Lark: file copied")
         (lark-drive-refresh))))))

;;;; Move file (dired "R" for rename/move)

(defun lark-drive-move ()
  "Move the file at point to another folder."
  (interactive)
  (let ((file (lark-drive--file-at-point)))
    (unless file (user-error "No file at point"))
    (let* ((token (lark-drive--file-token file))
           (type (lark-drive--file-type file))
           (name (lark-drive--file-name file))
           (target (read-string (format "Move \"%s\" to folder token: " name))))
      (when (string-empty-p target)
        (user-error "Target folder token is required"))
      (message "Lark: moving file...")
      (lark--run-command
       (list "drive" "+move"
             "--file-token" token
             "--folder-token" target
             "--type" type)
       (lambda (_data)
         (message "Lark: file moved")
         (lark-drive-refresh))))))

;;;; Upload (dired-like)

(defun lark-drive-upload (file)
  "Upload local FILE to the current folder."
  (interactive "fUpload file: ")
  (message "Lark: uploading %s..." (file-name-nondirectory file))
  (let ((args (list "drive" "+upload"
                    "--file" (expand-file-name file))))
    (when (and lark-drive--folder-token
               (not (string-empty-p lark-drive--folder-token)))
      (setq args (append args (list "--folder-token" lark-drive--folder-token))))
    (lark--run-command
     args
     (lambda (data)
       (let ((token (or (lark--get-nested data 'data 'file_token)
                        (alist-get 'file_token data))))
         (message "Lark: uploaded%s" (if token (format " (token: %s)" token) ""))
         (when (eq major-mode 'lark-drive-mode)
           (lark-drive-refresh)))))))

;;;; Download

(defun lark-drive-download ()
  "Download the file at point to a local directory."
  (interactive)
  (let ((file (lark-drive--file-at-point)))
    (unless file (user-error "No file at point"))
    (when (lark-drive--folder-p file)
      (user-error "Cannot download a folder"))
    (let* ((token (lark-drive--file-token file))
           (name (lark-drive--file-name file))
           (dir (read-directory-name (format "Save \"%s\" to: " name)))
           (output (expand-file-name name dir)))
      (message "Lark: downloading %s..." name)
      (lark--run-command
       (list "drive" "+download"
             "--file-token" token
             "--output" output
             "--overwrite")
       (lambda (_data)
         (message "Lark: downloaded to %s" output))))))

;;;; Import local file to Drive

(defun lark-drive-import (file type)
  "Import local FILE to Drive as cloud document TYPE (docx, sheet, bitable)."
  (interactive
   (list (read-file-name "Import file: ")
         (completing-read "Import as: " '("docx" "sheet" "bitable") nil t)))
  (message "Lark: importing %s..." (file-name-nondirectory file))
  (let ((args (list "drive" "+import"
                    "--file" (expand-file-name file)
                    "--type" type)))
    (when (and lark-drive--folder-token
               (not (string-empty-p lark-drive--folder-token)))
      (setq args (append args (list "--folder-token" lark-drive--folder-token))))
    (lark--run-command
     args
     (lambda (_data)
       (message "Lark: import complete")
       (when (eq major-mode 'lark-drive-mode)
         (lark-drive-refresh))))))

;;;; Export cloud doc to local

(defun lark-drive-export ()
  "Export the file at point to a local directory."
  (interactive)
  (let ((file (lark-drive--file-at-point)))
    (unless file (user-error "No file at point"))
    (when (lark-drive--folder-p file)
      (user-error "Cannot export a folder"))
    (let* ((token (lark-drive--file-token file))
           (type (lark-drive--file-type file))
           (name (lark-drive--file-name file))
           (ext (completing-read
                 (format "Export \"%s\" as: " name)
                 '("docx" "pdf" "xlsx" "csv" "markdown") nil t))
           (dir (read-directory-name "Save to: "))
           (default-directory (expand-file-name dir)))
      (message "Lark: exporting %s as %s..." name ext)
      (lark--run-command
       (list "drive" "+export"
             "--token" token
             "--doc-type" type
             "--file-extension" ext
             "--output-dir" "."
             "--overwrite")
       (lambda (data)
         (let ((path (or (alist-get 'path data)
                         (lark--get-nested data 'data 'path))))
           (message "Lark: exported%s"
                    (if path (format " → %s" (expand-file-name path dir)) ""))))))))

;;;; Copy token / URL

(defun lark-drive-copy-token ()
  "Copy the file token at point to the kill ring."
  (interactive)
  (let ((token (lark-drive--token-at-point)))
    (unless token (user-error "No file at point"))
    (kill-new token)
    (message "Copied token: %s" token)))

(defun lark-drive-copy-url ()
  "Copy the file URL at point to the kill ring."
  (interactive)
  (let ((file (lark-drive--file-at-point)))
    (unless file (user-error "No file at point"))
    (let ((url (lark-drive--file-url file)))
      (if (string-empty-p url)
          (user-error "No URL for this file")
        (kill-new url)
        (message "Copied URL: %s" url)))))

;;;; Transient dispatch

;;;###autoload (autoload 'lark-drive-dispatch "lark-drive" nil t)
(transient-define-prefix lark-drive-dispatch ()
  "Lark Drive commands."
  ["Browse"
   ("l" "List root"       lark-drive-list)
   ("g" "Refresh"         lark-drive-refresh)
   ("v" "View content"    lark-drive-view-content)]
  ["File Operations"
   ("U" "Upload"          lark-drive-upload)
   ("D" "Download"        lark-drive-download)
   ("+" "Create folder"   lark-drive-mkdir)
   ("C" "Copy"            lark-drive-copy)
   ("R" "Move/Rename"     lark-drive-move)]
  ["Import / Export"
   ("I" "Import"          lark-drive-import)
   ("E" "Export"           lark-drive-export)])

(provide 'lark-drive)
;;; lark-drive.el ends here
