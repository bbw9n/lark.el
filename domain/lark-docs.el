;;; lark-docs.el --- Lark Docs integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; Provides Docs domain commands for lark.el: document creation,
;; fetching, updating, searching, and media operations.
;;
;; CLI command mapping:
;;   Create:           docs +create --title X [--markdown X] [--folder-token X]
;;                                  [--wiki-space X] [--wiki-node X]
;;   Fetch:            docs +fetch --doc X
;;   Update:           docs +update --doc X --markdown X --mode X
;;                                  [--new-title X] [--selection-by-title X]
;;                                  [--selection-with-ellipsis X]
;;   Search:           docs +search --query X [--page-size N]
;;   Media download:   docs +media-download --token X [--output X] [--type X]
;;   Media insert:     docs +media-insert --doc X --file X [--type X]
;;                                        [--align X] [--caption X]
;;   Media preview:    docs +media-preview --token X [--output X]
;;   Whiteboard:       docs +whiteboard-update --whiteboard-token X --source X
;;                                            [--input_format X] [--overwrite]

;;; Code:

(require 'lark-core)
(require 'org-lark)
(require 'json)
(require 'transient)

;;;; Customization

(defgroup lark-docs nil
  "Lark Docs settings."
  :group 'lark
  :prefix "lark-docs-")

(defcustom lark-docs-search-page-size "15"
  "Default page size for document search."
  :type 'string
  :group 'lark-docs)

(defcustom lark-docs-render-mode 'org
  "How to render fetched document content.
`org' renders via org-lark (Lark tags, code blocks, tables converted).
`markdown' displays raw markdown in markdown-mode or special-mode."
  :type '(choice (const :tag "Org (via org-lark)" org)
                 (const :tag "Markdown" markdown))
  :group 'lark-docs)

;;;; Buffer-local variables

(defvar-local lark-docs--doc-token nil
  "Document token for the current buffer.")

(defvar-local lark-docs--search-results nil
  "Cached search results for the current buffer.")

(defvar-local lark-docs--search-query nil
  "Last search query used.")

;;;; Result extraction

(defun lark-docs--extract-doc (data)
  "Extract document info from lark-cli response DATA."
  (or (alist-get 'data data)
      data))

(defun lark-docs--extract-search-results (data)
  "Extract search result list from lark-cli response DATA.
Handles the response shape: {data: {results: [...], has_more, total}}."
  (or (lark--get-nested data 'data 'results)
      (alist-get 'results data)
      (lark--get-nested data 'data 'items)
      (alist-get 'items data)))

(defun lark-docs--result-meta (doc)
  "Return the result_meta sub-alist from DOC, or DOC itself.
Search results nest fields under result_meta; other responses don't."
  (or (alist-get 'result_meta doc) doc))

(defun lark-docs--strip-highlight (str)
  "Remove <h>...</h> highlight tags from STR."
  (if (and str (stringp str))
      (replace-regexp-in-string "</?h>" "" str)
    str))

(defun lark-docs--doc-title (doc)
  "Extract the title from DOC."
  (or (lark-docs--strip-highlight (alist-get 'title_highlighted doc))
      (alist-get 'title doc)
      (alist-get 'title (lark-docs--result-meta doc))
      (alist-get 'name doc)
      "(untitled)"))

(defun lark-docs--doc-token (doc)
  "Extract the document token from DOC."
  (let ((meta (lark-docs--result-meta doc)))
    (or (alist-get 'document_id doc)
        (alist-get 'doc_token doc)
        (alist-get 'token meta)
        (alist-get 'obj_token doc)
        (alist-get 'id doc))))

(defun lark-docs--doc-type (doc)
  "Extract the document type from DOC."
  (let ((meta (lark-docs--result-meta doc)))
    (or (alist-get 'entity_type doc)
        (alist-get 'doc_types meta)
        (alist-get 'doc_type doc)
        (alist-get 'type doc)
        "")))

(defun lark-docs--doc-url (doc)
  "Extract the URL from DOC."
  (let ((meta (lark-docs--result-meta doc)))
    (or (alist-get 'url meta)
        (alist-get 'url doc)
        (alist-get 'doc_url doc)
        "")))

(defun lark-docs--doc-owner (doc)
  "Extract the owner from DOC."
  (let ((meta (lark-docs--result-meta doc)))
    (or (alist-get 'owner_name meta)
        (alist-get 'owner_name doc)
        (lark--get-nested doc 'owner 'name)
        "")))

(defun lark-docs--doc-create-time (doc)
  "Extract create time from DOC."
  (let* ((meta (lark-docs--result-meta doc))
         (iso (or (alist-get 'create_time_iso meta) ""))
         (ts (or (alist-get 'create_time meta)
                 (alist-get 'create_time doc)
                 (alist-get 'created_time doc))))
    (cond
     ((and (stringp iso) (not (string-empty-p iso)))
      (substring iso 0 (min 16 (length iso))))
     ((and (stringp ts) (not (string-empty-p ts)))
      (substring ts 0 (min 16 (length ts))))
     ((numberp ts) (or (lark--format-timestamp ts) ""))
     (t ""))))

(defun lark-docs--doc-update-time (doc)
  "Extract update time from DOC."
  (let* ((meta (lark-docs--result-meta doc))
         (iso (or (alist-get 'update_time_iso meta) ""))
         (ts (or (alist-get 'update_time meta)
                 (alist-get 'update_time doc)
                 (alist-get 'edit_time doc)
                 (alist-get 'updated_time doc))))
    (cond
     ((and (stringp iso) (not (string-empty-p iso)))
      (substring iso 0 (min 16 (length iso))))
     ((and (stringp ts) (not (string-empty-p ts)))
      (substring ts 0 (min 16 (length ts))))
     ((numberp ts) (or (lark--format-timestamp ts) ""))
     (t ""))))

;;;; Search
;; CLI: docs +search --query X [--page-size N]

(defvar lark-docs-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-docs-open-at-point)
    (define-key map (kbd "O")   #'lark-docs-open-as-org-at-point)
    (define-key map (kbd "E")   #'lark-docs-export-org-at-point)
    (define-key map (kbd "g")   #'lark-docs-search-refresh)
    (define-key map (kbd "n")   #'lark-docs--next-result)
    (define-key map (kbd "p")   #'lark-docs--prev-result)
    (define-key map (kbd "y")   #'lark-docs-copy-token)
    (define-key map (kbd "?")   #'lark-docs-dispatch)
    map)
  "Keymap for `lark-docs-search-mode'.")

(define-derived-mode lark-docs-search-mode special-mode
  "Lark Docs"
  "Major mode for browsing Lark document search results.
Each result is displayed as a multi-line section.")

(defun lark-docs--doc-token-at-point ()
  "Return the document token at point, or nil."
  (get-text-property (point) 'lark-doc-token))

(defun lark-docs--doc-title-at-point ()
  "Return the document title at point, or nil."
  (get-text-property (point) 'lark-doc-title))

(defun lark-docs--insert-field (label value)
  "Insert a LABEL: VALUE line if VALUE is non-empty."
  (when (and value (not (string-empty-p value)))
    (insert (propertize (format "  %-14s" (concat label ":")) 'face 'font-lock-keyword-face)
            value "\n")))

(defun lark-docs--doc-summary (doc)
  "Extract and clean the summary from search result DOC."
  (lark-docs--strip-highlight
   (or (alist-get 'summary_highlighted doc)
       (alist-get 'summary doc)
       "")))

(defun lark-docs--insert-result (doc)
  "Insert a multi-line section for search result DOC."
  (let ((token (lark-docs--doc-token doc))
        (title (lark-docs--doc-title doc))
        (type (lark-docs--doc-type doc))
        (url (lark-docs--doc-url doc))
        (owner (lark-docs--doc-owner doc))
        (created (lark-docs--doc-create-time doc))
        (updated (lark-docs--doc-update-time doc))
        (summary (lark-docs--doc-summary doc))
        (beg (point)))
    (insert (propertize title 'face 'bold) "\n")
    (lark-docs--insert-field "Type" type)
    (lark-docs--insert-field "Token" token)
    (lark-docs--insert-field "Owner" owner)
    (lark-docs--insert-field "Created" created)
    (lark-docs--insert-field "Updated" updated)
    (lark-docs--insert-field "URL" url)
    (when (and summary (not (string-empty-p summary)))
      (insert (propertize (format "  %s" summary) 'face 'font-lock-comment-face) "\n"))
    (insert "\n")
    (put-text-property beg (point) 'lark-doc-token token)
    (put-text-property beg (point) 'lark-doc-title title)))

(defun lark-docs--next-result ()
  "Move to the next result section."
  (interactive)
  (let ((current (lark-docs--doc-token-at-point))
        (pos (point)))
    (when current
      (while (and (not (eobp))
                  (equal (get-text-property (point) 'lark-doc-token) current))
        (forward-char)))
    (while (and (not (eobp))
                (not (get-text-property (point) 'lark-doc-token)))
      (forward-char))
    (when (eobp) (goto-char pos))))

(defun lark-docs--prev-result ()
  "Move to the previous result section."
  (interactive)
  (let ((current (lark-docs--doc-token-at-point))
        (pos (point)))
    (when current
      (while (and (not (bobp))
                  (equal (get-text-property (point) 'lark-doc-token) current))
        (backward-char)))
    (while (and (not (bobp))
                (not (get-text-property (point) 'lark-doc-token)))
      (backward-char))
    (let ((target (get-text-property (point) 'lark-doc-token)))
      (if target
          (while (and (not (bobp))
                      (equal (get-text-property (1- (point)) 'lark-doc-token) target))
            (backward-char))
        (goto-char pos)))))

;;;###autoload
(defun lark-docs-search (query)
  "Search Lark documents by QUERY."
  (interactive "sSearch docs: ")
  (when (string-empty-p query)
    (user-error "Search query is required"))
  (message "Lark: searching docs...")
  (let ((q query))
    (lark--run-command
     (list "docs" "+search" "--query" query
           "--page-size" lark-docs-search-page-size)
     (lambda (data) (lark-docs--display-search data q)))))

(defun lark-docs-search-refresh ()
  "Refresh the search results with the previous query."
  (interactive)
  (lark-docs-search (or lark-docs--search-query
                        (read-string "Search docs: "))))

(defun lark-docs--display-search (data &optional query)
  "Display search results DATA.  QUERY is stored for refresh."
  (let* ((results (lark-docs--extract-search-results data))
         (buf (get-buffer-create "*Lark Docs*")))
    (with-current-buffer buf
      (lark-docs-search-mode)
      (setq lark-docs--search-results results
            lark-docs--search-query query)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null results)
            (insert "(no results)\n")
          (dolist (doc results)
            (lark-docs--insert-result doc))))
      (goto-char (point-min))
      (setq header-line-format
            (format " Lark Docs — %d result(s)" (length results))))
    (pop-to-buffer buf)))

(defun lark-docs-open-at-point ()
  "Fetch and display the document at point."
  (interactive)
  (let ((token (lark-docs--doc-token-at-point)))
    (unless token (user-error "No document at point"))
    (lark-docs-fetch token)))

(defun lark-docs-open-as-org-at-point ()
  "Fetch the document at point and display as Org."
  (interactive)
  (let ((token (lark-docs--doc-token-at-point)))
    (unless token (user-error "No document at point"))
    (lark-docs-fetch-as-org token)))

(defun lark-docs-export-org-at-point ()
  "Export the document at point to an Org file."
  (interactive)
  (let ((token (lark-docs--doc-token-at-point))
        (title (lark-docs--doc-title-at-point)))
    (unless token (user-error "No document at point"))
    (let ((output (read-file-name "Write Org file: " nil nil nil
                                  (concat (or title "lark-export") ".org"))))
      (lark-docs-export-org token output))))

(defun lark-docs-copy-token ()
  "Copy the document token at point to the kill ring."
  (interactive)
  (let ((token (or (lark-docs--doc-token-at-point) lark-docs--doc-token)))
    (unless token (user-error "No document token"))
    (kill-new token)
    (message "Copied: %s" token)))

;;;; Fetch
;; CLI: docs +fetch --doc X

;;;###autoload
(defun lark-docs-fetch (doc)
  "Fetch and display a Lark document by DOC (URL or token)."
  (interactive "sDocument URL or token: ")
  (when (string-empty-p doc)
    (user-error "Document URL or token is required"))
  (message "Lark: fetching document...")
  (lark--run-command
   (list "docs" "+fetch" "--doc" doc)
   (lambda (data) (lark-docs--display-document data doc))))

(defun lark-docs--display-document (data doc-ref)
  "Display fetched document DATA.  DOC-REF is the URL or token used."
  (let* ((doc (lark-docs--extract-doc data))
         (title (or (lark-docs--doc-title doc) doc-ref))
         (token (or (lark-docs--doc-token doc) doc-ref))
         (content (or (alist-get 'content doc)
                      (alist-get 'markdown doc)
                      (alist-get 'body doc)
                      (lark--get-nested doc 'document 'content)
                      ""))
         (doc-url (lark-docs--doc-url doc)))
    ;; Try org rendering if configured and content is available
    (if (and (not (string-empty-p content))
             (lark-docs--use-org-p))
        (condition-case err
            (let ((org-content (lark-docs--markdown-to-org
                                content title
                                (or (alist-get 'doc_id doc) token)
                                (or doc-url doc-ref))))
              (lark-docs--display-org-buffer org-content title token))
          (error
           (message "Lark: org-lark conversion failed (%s), falling back to markdown"
                    (error-message-string err))
           (lark-docs--display-document-markdown title token doc content)))
      (lark-docs--display-document-markdown title token doc content))))

(defun lark-docs--display-document-markdown (title token doc content)
  "Display document as markdown.  TITLE, TOKEN, DOC, CONTENT as expected."
  (let ((buf (get-buffer-create (format "*Lark Doc: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize title 'face 'bold) "\n"
                (make-string (min 60 (max 20 (length title))) ?─) "\n\n")
        (lark-docs--insert-field "Token" token)
        (lark-docs--insert-field "Type" (lark-docs--doc-type doc))
        (lark-docs--insert-field "URL" (lark-docs--doc-url doc))
        (insert "\n")
        (if (string-empty-p content)
            (insert (propertize "(no content)" 'face 'font-lock-comment-face) "\n")
          (insert content "\n")))
      (if (fboundp 'markdown-mode) (markdown-mode) (special-mode))
      (setq lark-docs--doc-token token)
      (goto-char (point-min))
      (setq header-line-format (format " Lark Doc: %s" title)))
    (pop-to-buffer buf)))

;;;; Create
;; CLI: docs +create --title X [--markdown X] [--folder-token X]
;;                   [--wiki-space X] [--wiki-node X]

;;;###autoload (autoload 'lark-docs-create "lark-docs" nil t)
(transient-define-prefix lark-docs-create ()
  "Create a new Lark document."
  ["Document"
   ("t" "Title"        "--title=" :prompt "Title: ")
   ("m" "Markdown"     "--markdown=" :prompt "Markdown content (or @file): ")]
  ["Location"
   ("f" "Folder token" "--folder-token=" :prompt "Folder token: ")
   ("w" "Wiki space"   "--wiki-space=" :prompt "Wiki space ID: ")
   ("n" "Wiki node"    "--wiki-node=" :prompt "Wiki node token: ")]
  ["Actions"
   ("RET" "Create" lark-docs--do-create)
   ("q"   "Cancel" transient-quit-all)])

(defun lark-docs--do-create (&rest _args)
  "Execute document creation with transient arguments."
  (interactive)
  (let ((args (transient-args 'lark-docs-create)))
    (unless args
      (user-error "No document details provided"))
    (message "Lark: creating document...")
    (lark--run-command
     (append '("docs" "+create") args)
     (lambda (data)
       (let* ((doc (lark-docs--extract-doc data))
              (token (lark-docs--doc-token doc))
              (url (lark-docs--doc-url doc)))
         (message "Lark: document created%s%s"
                  (if token (format " (token: %s)" token) "")
                  (if (and url (not (string-empty-p url)))
                      (format " — %s" url) "")))))))

;;;; Update
;; CLI: docs +update --doc X --markdown X --mode X [--new-title X]
;;                   [--selection-by-title X] [--selection-with-ellipsis X]

;;;###autoload (autoload 'lark-docs-update "lark-docs" nil t)
(transient-define-prefix lark-docs-update ()
  "Update a Lark document."
  ["Target"
   ("d" "Document"     "--doc=" :prompt "Document URL or token: ")
   ("T" "New title"    "--new-title=" :prompt "New title: ")]
  ["Content"
   ("m" "Markdown"     "--markdown=" :prompt "Markdown content (or @file): ")
   ("M" "Mode"         "--mode=" :prompt "Mode: "
    :choices ("append" "overwrite" "replace_range" "replace_all"
              "insert_before" "insert_after" "delete_range"))]
  ["Selection"
   ("s" "By title"     "--selection-by-title=" :prompt "Title locator (e.g. ## Section): ")
   ("e" "By ellipsis"  "--selection-with-ellipsis=" :prompt "Content locator (e.g. start...end): ")]
  ["Actions"
   ("RET" "Update" lark-docs--do-update)
   ("q"   "Cancel" transient-quit-all)])

(defun lark-docs--do-update (&rest _args)
  "Execute document update with transient arguments."
  (interactive)
  (let ((args (transient-args 'lark-docs-update)))
    (unless args
      (user-error "No update details provided"))
    (message "Lark: updating document...")
    (lark--run-command
     (append '("docs" "+update") args)
     (lambda (_data)
       (message "Lark: document updated")))))

;;;; Media download
;; CLI: docs +media-download --token X [--output X] [--type X]

;;;###autoload
(defun lark-docs-media-download (token &optional output type)
  "Download media by TOKEN.
Optional OUTPUT is the local save path.
Optional TYPE is \"media\" (default) or \"whiteboard\"."
  (interactive
   (list (read-string "Media token: ")
         (read-file-name "Save to (empty for auto): " nil nil nil)
         (completing-read "Type: " '("media" "whiteboard") nil t nil nil "media")))
  (when (string-empty-p token)
    (user-error "Media token is required"))
  (message "Lark: downloading media...")
  (let ((args (list "docs" "+media-download" "--token" token)))
    (when (and output (not (string-empty-p output)))
      (setq args (append args (list "--output" (expand-file-name output)))))
    (when (and type (not (string-empty-p type)) (not (equal type "media")))
      (setq args (append args (list "--type" type))))
    (lark--run-command
     args
     (lambda (data)
       (let ((path (or (alist-get 'path data)
                       (alist-get 'output data)
                       (lark--get-nested data 'data 'path))))
         (message "Lark: media downloaded%s"
                  (if path (format " → %s" path) "")))))))

;;;; Media insert
;; CLI: docs +media-insert --doc X --file X [--type X] [--align X] [--caption X]

;;;###autoload
(defun lark-docs-media-insert (doc file &optional type align caption)
  "Insert media FILE into document DOC.
TYPE is \"image\" (default) or \"file\".
ALIGN is \"left\", \"center\", or \"right\".
CAPTION is an optional image caption."
  (interactive
   (list (read-string "Document URL or token: "
                      (when lark-docs--doc-token lark-docs--doc-token))
         (read-file-name "File: ")
         (completing-read "Type: " '("image" "file") nil t nil nil "image")
         (completing-read "Align: " '("" "left" "center" "right") nil nil)
         (read-string "Caption (optional): ")))
  (when (string-empty-p doc)
    (user-error "Document URL or token is required"))
  (when (string-empty-p file)
    (user-error "File path is required"))
  (message "Lark: inserting media...")
  (let ((args (list "docs" "+media-insert"
                    "--doc" doc
                    "--file" (expand-file-name file))))
    (when (and type (not (string-empty-p type)))
      (setq args (append args (list "--type" type))))
    (when (and align (not (string-empty-p align)))
      (setq args (append args (list "--align" align))))
    (when (and caption (not (string-empty-p caption)))
      (setq args (append args (list "--caption" caption))))
    (lark--run-command
     args
     (lambda (_data)
       (message "Lark: media inserted into document")))))

;;;; Media preview
;; CLI: docs +media-preview --token X [--output X]

;;;###autoload
(defun lark-docs-media-preview (token &optional output)
  "Preview media file by TOKEN.
Optional OUTPUT is the local save path."
  (interactive
   (list (read-string "Media token: ")
         (read-file-name "Save to (empty for auto): " nil nil nil)))
  (when (string-empty-p token)
    (user-error "Media token is required"))
  (message "Lark: previewing media...")
  (let ((args (list "docs" "+media-preview" "--token" token)))
    (when (and output (not (string-empty-p output)))
      (setq args (append args (list "--output" (expand-file-name output)))))
    (lark--run-command
     args
     (lambda (data)
       (let ((path (or (alist-get 'path data)
                       (alist-get 'output data)
                       (lark--get-nested data 'data 'path))))
         (if path
             (progn
               (message "Lark: media saved to %s" path)
               (find-file path))
           (message "Lark: media preview complete")))))))

;;;; Whiteboard update
;; CLI: docs +whiteboard-update --whiteboard-token X --source X
;;                              [--input_format X] [--overwrite]

;;;###autoload (autoload 'lark-docs-whiteboard-update "lark-docs" nil t)
(transient-define-prefix lark-docs-whiteboard-update ()
  "Update a whiteboard in a Lark document."
  ["Whiteboard"
   ("w" "Whiteboard token" "--whiteboard-token=" :prompt "Whiteboard token: ")
   ("s" "Source"           "--source=" :prompt "Source data (or @file): ")
   ("f" "Input format"    "--input_format=" :prompt "Format: "
    :choices ("raw" "plantuml" "mermaid"))
   ("o" "Overwrite"       "--overwrite")]
  ["Actions"
   ("RET" "Update" lark-docs--do-whiteboard-update)
   ("q"   "Cancel" transient-quit-all)])

(defun lark-docs--do-whiteboard-update (&rest _args)
  "Execute whiteboard update with transient arguments."
  (interactive)
  (let ((args (transient-args 'lark-docs-whiteboard-update)))
    (unless args
      (user-error "No whiteboard details provided"))
    (message "Lark: updating whiteboard...")
    (lark--run-command
     (append '("docs" "+whiteboard-update") args)
     (lambda (_data)
       (message "Lark: whiteboard updated")))))

;;;; org-lark integration
;; org-lark is bundled with lark.el and provides Lark Markdown → Org
;; conversion (Lark tags, code blocks, tables, media download).

(defun lark-docs--use-org-p ()
  "Return non-nil if documents should be rendered as Org."
  (eq lark-docs-render-mode 'org))

(defun lark-docs--sync-org-lark-config ()
  "Sync lark.el config into org-lark variables."
  (setq org-lark-cli-program lark-cli-executable))

(defun lark-docs--markdown-to-org (markdown &optional title doc-id source)
  "Convert Lark MARKDOWN to an Org string using org-lark's pipeline.
TITLE, DOC-ID, and SOURCE are metadata for the header.
Media is not downloaded (buffer-only display)."
  (lark-docs--sync-org-lark-config)
  (let* ((org-lark-download-media nil)
         (fetched `((markdown . ,markdown)
                    (title . ,title)
                    (doc_id . ,doc-id)))
         (st (make-org-lark--state
              :output-file (expand-file-name "lark-doc.org" temporary-file-directory)
              :asset-dir (expand-file-name "lark-assets/" temporary-file-directory))))
    (org-lark--pipeline markdown fetched (or source "") st)))

;;;###autoload
(defun lark-docs-fetch-as-org (doc)
  "Fetch a Lark document DOC and display it in org-mode."
  (interactive "sDocument URL or token: ")
  (when (string-empty-p doc)
    (user-error "Document URL or token is required"))
  (message "Lark: fetching document as org...")
  (lark--run-command
   (list "docs" "+fetch" "--doc" doc)
   (lambda (data)
     (let* ((inner (lark-docs--extract-doc data))
            (title (or (lark-docs--doc-title inner) doc))
            (token (or (lark-docs--doc-token inner) doc))
            (content (or (alist-get 'content inner)
                         (alist-get 'markdown inner)
                         (alist-get 'body inner)
                         (lark--get-nested inner 'document 'content)
                         ""))
            (doc-id (or (alist-get 'doc_id inner) token))
            (url (lark-docs--doc-url inner)))
       (if (string-empty-p content)
           (message "Lark: document has no content")
         (let ((org-content (lark-docs--markdown-to-org
                             content title doc-id (or url doc))))
           (lark-docs--display-org-buffer org-content title token)))))))

(defun lark-docs--display-org-buffer (org-content title token)
  "Display ORG-CONTENT in an org-mode buffer named after TITLE.
TOKEN is stored as the doc token."
  (let ((buf (get-buffer-create (format "*Lark Doc: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert org-content))
      (org-mode)
      (setq-local lark-docs--doc-token token)
      (goto-char (point-min))
      (setq header-line-format (format " Lark Doc (org): %s" title)))
    (pop-to-buffer buf)))

;;;###autoload
(defun lark-docs-export-org (doc output-file)
  "Export Lark document DOC to OUTPUT-FILE as Org.
Delegates to `org-lark-export' which handles media download."
  (interactive
   (list (read-string "Document URL or token: "
                      (when lark-docs--doc-token lark-docs--doc-token))
         (read-file-name "Write Org file: " nil nil nil "lark-export.org")))
  (lark-docs--sync-org-lark-config)
  (org-lark-export doc output-file))

(defun lark-docs-toggle-render-mode ()
  "Toggle between org and markdown render modes."
  (interactive)
  (setq lark-docs-render-mode
        (if (eq lark-docs-render-mode 'markdown) 'org 'markdown))
  (message "Lark docs render mode: %s" lark-docs-render-mode))

;;;; Transient dispatch

;;;###autoload (autoload 'lark-docs-dispatch "lark-docs" nil t)
(transient-define-prefix lark-docs-dispatch ()
  "Lark Docs commands."
  ["Read"
   ("s" "Search"           lark-docs-search)
   ("f" "Fetch document"   lark-docs-fetch)
   ("O" "Fetch as Org"     lark-docs-fetch-as-org)]
  ["Write"
   ("c" "Create document"  lark-docs-create)
   ("u" "Update document"  lark-docs-update)]
  ["Org Export"
   ("E" "Export to Org file" lark-docs-export-org)
   ("M" "Toggle render mode" lark-docs-toggle-render-mode)]
  ["Media"
   ("d" "Download media"   lark-docs-media-download)
   ("i" "Insert media"     lark-docs-media-insert)
   ("p" "Preview media"    lark-docs-media-preview)]
  ["Whiteboard"
   ("w" "Update whiteboard" lark-docs-whiteboard-update)])

(provide 'lark-docs)
;;; lark-docs.el ends here
