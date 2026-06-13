;;; lark-wiki.el --- Lark Wiki (knowledge base) integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; Provides Wiki domain commands for lark.el: browsing knowledge
;; spaces and their node trees, viewing node content, and node
;; create/delete/copy/move operations.
;;
;; Wiki is two nested trees: spaces contain nodes, and nodes nest into
;; sub-nodes.  Navigation mirrors `lark-drive': descend into a node
;; with RET, go up with `^', refresh with `g'.  A leaf node is backed
;; by a cloud object (doc/sheet/...); opening it hands off to
;; `lark-docs-fetch' or `lark-sheets-info'.
;;
;; CLI command mapping:
;;   Spaces:   wiki +space-list
;;   Nodes:    wiki +node-list --space-id X [--parent-node-token Y]
;;   Detail:   wiki +node-get --node-token X
;;   Create:   wiki +node-create --space-id X [--parent-node-token Y]
;;                               --title T [--obj-type docx]
;;   Delete:   wiki +node-delete --node-token X --yes
;;   Copy:     wiki +node-copy --node-token X --space-id S
;;                             (--target-space-id | --target-parent-node-token)
;;   Move:     wiki +move --node-token X [--source-space-id S]
;;                        (--target-space-id | --target-parent-token)

;;; Code:

(require 'lark-core)
(require 'lark-ui)
(require 'lark-contact)
(require 'json)
(require 'transient)

;; Leaf-node content handoff loads these on demand.
(declare-function lark-docs-fetch "lark-docs" (doc))
(declare-function lark-sheets-info "lark-sheets" (ref))

;;;; Customization

(defgroup lark-wiki nil
  "Lark Wiki settings."
  :group 'lark
  :prefix "lark-wiki-")

(defcustom lark-wiki-default-obj-type "docx"
  "Default object type for newly created wiki nodes."
  :type '(choice (const "docx") (const "sheet") (const "bitable")
                 (const "mindnote") (const "slides"))
  :group 'lark-wiki)

;;;; Buffer-local variables

(defvar-local lark-wiki--space-id nil
  "Wiki space ID for the current node-browser buffer.")

(defvar-local lark-wiki--space-name nil
  "Display name of the current wiki space.")

(defvar-local lark-wiki--parent-token nil
  "Parent node token for the current level.  Nil means the space root.")

(defvar-local lark-wiki--level-name nil
  "Display name of the current level (space or parent node title).")

(defvar-local lark-wiki--nodes nil
  "Cached node list for the current level.")

(defvar-local lark-wiki--spaces nil
  "Cached space list for a spaces buffer.")

(defvar-local lark-wiki--history nil
  "Stack of (PARENT-TOKEN . LEVEL-NAME) for breadcrumb navigation.")

(defvar-local lark-wiki--detail-node nil
  "The node alist displayed in a detail buffer.")

;;;; Space field extractors

(defun lark-wiki--space-id-of (space)
  "Extract the space ID from SPACE."
  (or (alist-get 'space_id space) ""))

(defun lark-wiki--space-name (space)
  "Extract the display name from SPACE."
  (or (alist-get 'name space) "(unnamed space)"))

(defun lark-wiki--space-description (space)
  "Extract the description from SPACE."
  (or (alist-get 'description space) ""))

(defun lark-wiki--space-type (space)
  "Extract the space type (team/person) from SPACE."
  (or (alist-get 'space_type space) ""))

;;;; Node field extractors

(defun lark-wiki--node-token (node)
  "Extract the wiki node_token from NODE."
  (or (alist-get 'node_token node) ""))

(defun lark-wiki--obj-token (node)
  "Extract the backing object token from NODE."
  (or (alist-get 'obj_token node) ""))

(defun lark-wiki--obj-type (node)
  "Extract the backing object type from NODE."
  (or (alist-get 'obj_type node) ""))

(defun lark-wiki--node-title (node)
  "Extract the title from NODE."
  (let ((title (alist-get 'title node)))
    (if (and title (stringp title) (not (string-empty-p title)))
        title
      "(untitled)")))

(defun lark-wiki--node-space-id (node)
  "Extract the space ID from NODE."
  (or (alist-get 'space_id node) ""))

(defun lark-wiki--node-parent-token (node)
  "Extract the parent node token from NODE."
  (or (alist-get 'parent_node_token node) ""))

(defun lark-wiki--node-type (node)
  "Extract the node type (origin/shortcut) from NODE."
  (or (alist-get 'node_type node) "origin"))

(defun lark-wiki--node-url (node)
  "Extract the node URL from NODE, or empty string."
  (or (alist-get 'url node) ""))

(defun lark-wiki--has-child-p (node)
  "Return non-nil when NODE has child nodes."
  (let ((hc (alist-get 'has_child node)))
    (and hc (not (eq hc :false)) (not (eq hc :json-false)))))

(defun lark-wiki--node-time (node key)
  "Extract and format timestamp KEY from NODE."
  (let ((ts (alist-get key node)))
    (cond
     ((and (stringp ts) (not (string-empty-p ts)))
      (or (lark--format-timestamp ts) ts))
     ((numberp ts) (or (lark--format-timestamp ts) ""))
     (t ""))))

;;;; Response extraction

(defun lark-wiki--extract-spaces (data)
  "Extract the space list from a `+space-list' response DATA.
The CLI returns the list under `data.spaces' (both single-page and
`--page-all' aggregated)."
  (or (lark--get-nested data 'data 'spaces)
      (alist-get 'spaces data)
      ;; Defensive fallbacks for alternate shapes.
      (lark--get-nested data 'data 'items)
      (alist-get 'items data)))

(defun lark-wiki--extract-nodes (data)
  "Extract the node list from a `+node-list' response DATA.
The CLI returns the list under `data.nodes'."
  (or (lark--get-nested data 'data 'nodes)
      (alist-get 'nodes data)
      ;; Defensive fallbacks for alternate shapes.
      (lark--get-nested data 'data 'items)
      (alist-get 'items data)))

;;;; Spaces list mode

(defvar lark-wiki-spaces-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-wiki-space-open)
    (define-key map (kbd "g")   #'lark-wiki-spaces-refresh)
    (define-key map (kbd "w")   #'lark-wiki-copy-space-id)
    (define-key map (kbd "n")   #'lark-wiki--next-space)
    (define-key map (kbd "p")   #'lark-wiki--prev-space)
    (define-key map (kbd "?")   #'lark-wiki-dispatch)
    map)
  "Keymap for `lark-wiki-spaces-mode'.")

(define-derived-mode lark-wiki-spaces-mode special-mode
  "Lark Wiki Spaces"
  "Major mode for browsing Lark wiki spaces.

\\{lark-wiki-spaces-mode-map}")

(defun lark-wiki--spaces-ai-context ()
  "Return the AI context plist for a wiki spaces buffer."
  (let ((spaces lark-wiki--spaces)
        (space-id (get-text-property (point) 'lark-wiki-space-id)))
    (list :domain "wiki"
          :buffer-type "space-list"
          :item (when space-id (list :space-id space-id))
          :summary (format "Wiki space list, %d space(s)" (length spaces)))))

(put 'lark-wiki-spaces-mode 'lark-ai-context-provider
     #'lark-wiki--spaces-ai-context)

;;;; Node browser mode

(defvar lark-wiki-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-wiki-open)
    (define-key map (kbd "^")   #'lark-wiki-up)
    (define-key map (kbd "g")   #'lark-wiki-refresh)
    (define-key map (kbd "v")   #'lark-wiki-view-content)
    (define-key map (kbd "d")   #'lark-wiki-node-detail)
    (define-key map (kbd "+")   #'lark-wiki-node-create)
    (define-key map (kbd "C")   #'lark-wiki-node-copy)
    (define-key map (kbd "R")   #'lark-wiki-move)
    (define-key map (kbd "D")   #'lark-wiki-node-delete)
    (define-key map (kbd "w")   #'lark-wiki-copy-token)
    (define-key map (kbd "W")   #'lark-wiki-copy-url)
    (define-key map (kbd "n")   #'lark-wiki--next-node)
    (define-key map (kbd "p")   #'lark-wiki--prev-node)
    (define-key map (kbd "?")   #'lark-wiki-dispatch)
    map)
  "Keymap for `lark-wiki-mode'.")

(define-derived-mode lark-wiki-mode special-mode
  "Lark Wiki"
  "Major mode for browsing a Lark wiki space's node tree.

\\{lark-wiki-mode-map}")

(defun lark-wiki--ai-context ()
  "Return the AI context plist for a wiki node-browser buffer."
  (let ((nodes lark-wiki--nodes)
        (node-token (get-text-property (point) 'lark-wiki-node-token)))
    (list :domain "wiki"
          :buffer-type "node-list"
          :item (when node-token (list :node-token node-token
                                       :space-id lark-wiki--space-id))
          :summary (format "Wiki space %s%s, %d node(s)"
                           (or lark-wiki--space-name lark-wiki--space-id "?")
                           (if lark-wiki--parent-token
                               (format " / %s" (or lark-wiki--level-name "subtree"))
                             "")
                           (length nodes)))))

(put 'lark-wiki-mode 'lark-ai-context-provider #'lark-wiki--ai-context)

;;;; Navigation helpers

(defun lark-wiki--space-at-point ()
  "Return the space alist at point, or nil."
  (get-text-property (point) 'lark-wiki-space))

(defun lark-wiki--node-at-point ()
  "Return the node alist at point, or nil."
  (get-text-property (point) 'lark-wiki-node))

(defun lark-wiki--next-space ()
  "Move to the next space entry."
  (interactive)
  (lark-ui-next-section 'lark-wiki-space-id))

(defun lark-wiki--prev-space ()
  "Move to the previous space entry."
  (interactive)
  (lark-ui-prev-section 'lark-wiki-space-id))

(defun lark-wiki--next-node ()
  "Move to the next node entry."
  (interactive)
  (lark-ui-next-section 'lark-wiki-node-token))

(defun lark-wiki--prev-node ()
  "Move to the previous node entry."
  (interactive)
  (lark-ui-prev-section 'lark-wiki-node-token))

;;;; Spaces: list / open
;; CLI: wiki +space-list

;;;###autoload
(defun lark-wiki-spaces ()
  "List the wiki spaces accessible to the current user.
The personal document library (my_library) is prepended, since the
API never returns it in the space list."
  (interactive)
  (message "Lark: listing wiki spaces...")
  (lark--run-command
   (list "wiki" "+space-list" "--as" "user" "--page-all")
   (lambda (data)
     (lark-wiki--render-spaces (lark-wiki--extract-spaces data)))
   nil :no-error t))

(defun lark-wiki-spaces-refresh ()
  "Refresh the wiki spaces list."
  (interactive)
  (lark-wiki-spaces))

(defun lark-wiki--my-library-space ()
  "Return a synthetic space entry for the personal document library."
  '((space_id . "my_library")
    (name . "My Document Library")
    (space_type . "person")
    (description . "Personal wiki library")))

(defun lark-wiki--render-spaces (spaces)
  "Render SPACES into the wiki spaces buffer."
  (let ((buf (get-buffer-create "*Lark Wiki Spaces*"))
        (all (cons (lark-wiki--my-library-space) (or spaces '()))))
    (with-current-buffer buf
      (lark-wiki-spaces-mode)
      (setq lark-wiki--spaces all)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Wiki Spaces" 'face 'bold) "\n"
                (lark-ui-separator 60) "\n\n")
        (dolist (space all)
          (lark-wiki--insert-space space)))
      (goto-char (point-min))
      (lark-ui-next-section 'lark-wiki-space-id)
      (setq header-line-format
            (format " Lark Wiki — %d space(s)" (length all))))
    (pop-to-buffer buf)))

(defun lark-wiki--insert-space (space)
  "Insert a one-line entry for SPACE."
  (let ((id (lark-wiki--space-id-of space))
        (name (lark-wiki--space-name space))
        (type (lark-wiki--space-type space))
        (desc (lark-wiki--space-description space))
        (beg (point)))
    (insert "  "
            (propertize (format "%-8s" (if (string-empty-p type) "space" type))
                        'face 'font-lock-type-face)
            "  "
            (propertize name 'face 'bold))
    (unless (string-empty-p desc)
      (insert (propertize (format "  — %s" desc) 'face 'font-lock-comment-face)))
    (insert "\n")
    (put-text-property beg (point) 'lark-wiki-space-id id)
    (put-text-property beg (point) 'lark-wiki-space space)))

(defun lark-wiki-space-open ()
  "Open the wiki space at point, listing its root nodes."
  (interactive)
  (let ((space (lark-wiki--space-at-point)))
    (unless space (user-error "No space at point"))
    (lark-wiki-nodes (lark-wiki--space-id-of space)
                     (lark-wiki--space-name space))))

;;;; Nodes: list / navigate
;; CLI: wiki +node-list --space-id X [--parent-node-token Y]

;;;###autoload
(defun lark-wiki-nodes (space-id &optional space-name parent-token level-name reset-history)
  "List wiki nodes in SPACE-ID, optionally under PARENT-TOKEN.
SPACE-NAME and LEVEL-NAME are display labels.  When RESET-HISTORY is
non-nil, the breadcrumb history is cleared (a fresh space entry)."
  (interactive "sSpace ID (or my_library): ")
  (when (string-empty-p space-id)
    (user-error "Space ID is required"))
  (message "Lark: listing wiki nodes...")
  (let ((args (list "wiki" "+node-list" "--as" "user"
                    "--space-id" space-id "--page-all")))
    (when (and parent-token (not (string-empty-p parent-token)))
      (setq args (append args (list "--parent-node-token" parent-token))))
    (let ((history (unless reset-history
                     (when (get-buffer "*Lark Wiki*")
                       (buffer-local-value 'lark-wiki--history
                                           (get-buffer "*Lark Wiki*"))))))
      (lark--run-command
       args
       (lambda (data)
         (lark-wiki--render-nodes (lark-wiki--extract-nodes data)
                                  space-id space-name parent-token level-name
                                  (if reset-history nil history)))
       nil :no-error t))))

(defun lark-wiki--render-nodes (nodes space-id space-name parent-token level-name history)
  "Render NODES for SPACE-ID under PARENT-TOKEN into the wiki buffer.
SPACE-NAME, LEVEL-NAME label the header; HISTORY is the breadcrumb stack."
  (let ((buf (get-buffer-create "*Lark Wiki*")))
    (with-current-buffer buf
      (lark-wiki-mode)
      (setq lark-wiki--nodes nodes
            lark-wiki--space-id space-id
            lark-wiki--space-name (or space-name space-id)
            lark-wiki--parent-token parent-token
            lark-wiki--level-name level-name
            lark-wiki--history history)
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Breadcrumb header
        (insert (propertize (format "  %s" (or space-name space-id)) 'face 'bold))
        (when (and parent-token (not (string-empty-p parent-token)))
          (insert (propertize (format "  / %s" (or level-name "subtree"))
                              'face 'font-lock-function-name-face)))
        (insert "\n" (lark-ui-separator 60) "\n")
        ;; ".." up-entry when not at the space root
        (when (and parent-token (not (string-empty-p parent-token)))
          (let ((beg (point)))
            (insert "  "
                    (propertize "up      " 'face 'font-lock-type-face)
                    "  "
                    (propertize ".." 'face 'bold)
                    "\n")
            (put-text-property beg (point) 'lark-wiki-node
                               '((node_token . "..") (obj_type . "parent")))
            (put-text-property beg (point) 'lark-wiki-node-token "..")))
        (if (null nodes)
            (insert "  (no nodes)\n")
          (dolist (node nodes)
            (lark-wiki--insert-node node))))
      (goto-char (point-min))
      (lark-ui-next-section 'lark-wiki-node-token)
      (setq header-line-format
            (format " Lark Wiki: %s — %d node(s)"
                    (or space-name space-id) (length nodes))))
    (pop-to-buffer buf)))

(defun lark-wiki--insert-node (node)
  "Insert a one-line entry for NODE."
  (let* ((token (lark-wiki--node-token node))
         (title (lark-wiki--node-title node))
         (obj-type (lark-wiki--obj-type node))
         (has-child (lark-wiki--has-child-p node))
         (shortcut (equal (lark-wiki--node-type node) "shortcut"))
         (beg (point)))
    (insert "  "
            (propertize (format "%-8s" (if (string-empty-p obj-type) "node" obj-type))
                        'face 'font-lock-type-face)
            "  "
            (propertize title 'face (if has-child 'bold 'default)))
    (when has-child
      (insert (propertize " ▸" 'face 'font-lock-keyword-face)))
    (when shortcut
      (insert (propertize " ↗" 'face 'font-lock-comment-face)))
    (insert "\n")
    (put-text-property beg (point) 'lark-wiki-node-token token)
    (put-text-property beg (point) 'lark-wiki-node node)))

(defun lark-wiki-open ()
  "Open the node at point.
Nodes with children are descended into; leaf nodes hand off to the
backing document viewer (see `lark-wiki-view-content')."
  (interactive)
  (let ((node (lark-wiki--node-at-point)))
    (unless node (user-error "No node at point"))
    (cond
     ;; ".." up-entry
     ((equal (lark-wiki--obj-type node) "parent")
      (lark-wiki-up))
     ;; Has children — descend, pushing the current level onto history
     ((lark-wiki--has-child-p node)
      (push (cons lark-wiki--parent-token lark-wiki--level-name)
            lark-wiki--history)
      (let ((hist lark-wiki--history)
            (sid lark-wiki--space-id)
            (sname lark-wiki--space-name))
        (lark-wiki-nodes sid sname
                         (lark-wiki--node-token node)
                         (lark-wiki--node-title node))
        ;; Preserve the pushed history across the async re-render.
        (with-current-buffer (get-buffer "*Lark Wiki*")
          (setq lark-wiki--history hist))))
     ;; Leaf — view the backing object
     (t (lark-wiki--view-node node)))))

(defun lark-wiki-up ()
  "Go up one level in the node tree."
  (interactive)
  (if lark-wiki--history
      (let ((prev (pop lark-wiki--history))
            (hist lark-wiki--history)
            (sid lark-wiki--space-id)
            (sname lark-wiki--space-name))
        (lark-wiki-nodes sid sname (car prev) (cdr prev))
        (with-current-buffer (get-buffer "*Lark Wiki*")
          (setq lark-wiki--history hist)))
    ;; At the space root — go back to the space list.
    (lark-wiki-spaces)))

(defun lark-wiki-refresh ()
  "Refresh the current node level."
  (interactive)
  (unless lark-wiki--space-id
    (user-error "Not in a wiki node buffer"))
  (let ((sid lark-wiki--space-id)
        (sname lark-wiki--space-name)
        (ptok lark-wiki--parent-token)
        (lname lark-wiki--level-name)
        (hist lark-wiki--history))
    (lark-wiki-nodes sid sname ptok lname)
    (with-current-buffer (get-buffer "*Lark Wiki*")
      (setq lark-wiki--history hist))))

;;;; View node content (leaf handoff)

(defun lark-wiki--view-node (node)
  "Hand off NODE's backing object to the appropriate content viewer."
  (let* ((obj-type (lark-wiki--obj-type node))
         (obj-token (lark-wiki--obj-token node))
         (url (lark-wiki--node-url node))
         (ref (if (and url (not (string-empty-p url))) url obj-token)))
    (when (string-empty-p ref)
      (user-error "No object token or URL for this node"))
    (pcase obj-type
      ((or "doc" "docx")
       (require 'lark-docs)
       (lark-docs-fetch ref))
      ("sheet"
       (require 'lark-sheets)
       (lark-sheets-info ref))
      (_
       (message "Lark: node type \"%s\" is not viewable; showing details" obj-type)
       (lark-wiki--show-node-detail node)))))

(defun lark-wiki-view-content ()
  "View the backing document of the node at point."
  (interactive)
  (let ((node (lark-wiki--node-at-point)))
    (unless node (user-error "No node at point"))
    (when (equal (lark-wiki--obj-type node) "parent")
      (user-error "Not a content node"))
    (lark-wiki--view-node node)))

;;;; Node detail
;; CLI: wiki +node-get --node-token X

(defvar lark-wiki-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-wiki-detail-view-content)
    (define-key map (kbd "w")   #'lark-wiki-copy-token)
    (define-key map (kbd "?")   #'lark-wiki-dispatch)
    map)
  "Keymap for `lark-wiki-detail-mode'.")

(define-derived-mode lark-wiki-detail-mode special-mode
  "Lark Wiki Node"
  "Major mode for viewing a Lark wiki node's details.

\\{lark-wiki-detail-mode-map}")

(defun lark-wiki--detail-ai-context ()
  "Return the AI context plist for a wiki node detail buffer."
  (let* ((node lark-wiki--detail-node)
         (token (and node (lark-wiki--node-token node)))
         (title (and node (lark-wiki--node-title node))))
    (list :domain "wiki"
          :buffer-type "node-detail"
          :item (list :node-token token
                      :obj-token (and node (lark-wiki--obj-token node))
                      :obj-type (and node (lark-wiki--obj-type node)))
          :summary (format "Wiki node detail: %s (%s)" title token))))

(put 'lark-wiki-detail-mode 'lark-ai-context-provider
     #'lark-wiki--detail-ai-context)

(defun lark-wiki-node-detail ()
  "Show full details for the node at point.
Renders the node alist already obtained from `+node-list', which
carries every field the API exposes for a node.  (The `+node-get'
shortcut is not used: its `--node-token' heuristic only recognises
`wik'-prefixed tokens and misresolves the node tokens that
`+node-list' returns.)"
  (interactive)
  (let ((node (lark-wiki--node-at-point)))
    (unless node (user-error "No node at point"))
    (when (equal (lark-wiki--obj-type node) "parent")
      (user-error "Not a content node"))
    (lark-wiki--show-node-detail node)))

(defun lark-wiki--detail-field (label value)
  "Insert LABEL: VALUE if VALUE is non-empty."
  (lark-ui-insert-field label value 14))

(defun lark-wiki--show-node-detail (node)
  "Display NODE details in a detail buffer."
  (let* ((title (lark-wiki--node-title node))
         (obj-type (lark-wiki--obj-type node))
         (viewable (member obj-type '("doc" "docx" "sheet")))
         (buf (get-buffer-create (format "*Lark Wiki Node: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (lark-ui-insert-title title)
        (lark-wiki--detail-field "Node token" (lark-wiki--node-token node))
        (lark-wiki--detail-field "Object token" (lark-wiki--obj-token node))
        (lark-wiki--detail-field "Object type" obj-type)
        (lark-wiki--detail-field "Node type" (lark-wiki--node-type node))
        (lark-wiki--detail-field "Space ID" (lark-wiki--node-space-id node))
        (lark-wiki--detail-field "Parent" (lark-wiki--node-parent-token node))
        (lark-wiki--detail-field "Created" (lark-wiki--node-time node 'obj_create_time))
        (lark-wiki--detail-field "Edited" (lark-wiki--node-time node 'obj_edit_time))
        (lark-wiki--detail-field "URL" (lark-wiki--node-url node))
        (when viewable
          (insert "\n" (propertize "Press RET to view content"
                                   'face 'font-lock-comment-face) "\n")))
      (lark-wiki-detail-mode)
      (setq lark-wiki--detail-node node)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun lark-wiki-detail-view-content ()
  "View the backing document of the node in this detail buffer."
  (interactive)
  (unless lark-wiki--detail-node
    (user-error "No node in this buffer"))
  (lark-wiki--view-node lark-wiki--detail-node))

;;;; Create node
;; CLI: wiki +node-create --space-id X [--parent-node-token Y] --title T --obj-type Z

(defun lark-wiki-node-create (title obj-type)
  "Create a new wiki node titled TITLE of OBJ-TYPE at the current level.
Created in the current buffer's space, under the current parent node."
  (interactive
   (list (read-string "New node title: ")
         (completing-read "Object type: "
                          '("docx" "sheet" "bitable" "mindnote" "slides")
                          nil t nil nil lark-wiki-default-obj-type)))
  (unless lark-wiki--space-id
    (user-error "Not in a wiki node buffer"))
  (when (string-empty-p title)
    (user-error "Title is required"))
  (message "Lark: creating wiki node...")
  (let ((args (list "wiki" "+node-create" "--as" "user"
                    "--space-id" lark-wiki--space-id
                    "--title" title
                    "--obj-type" obj-type)))
    (when (and lark-wiki--parent-token
               (not (string-empty-p lark-wiki--parent-token)))
      (setq args (append args (list "--parent-node-token"
                                    lark-wiki--parent-token))))
    (lark--run-command
     args
     (lambda (_data)
       (message "Lark: node \"%s\" created" title)
       (lark-wiki-refresh)))))

;;;; Delete node
;; CLI: wiki +node-delete --node-token X --yes

(defun lark-wiki-node-delete ()
  "Delete the wiki node at point after confirmation."
  (interactive)
  (let ((node (lark-wiki--node-at-point)))
    (unless node (user-error "No node at point"))
    (when (equal (lark-wiki--obj-type node) "parent")
      (user-error "Not a deletable node"))
    (let ((token (lark-wiki--node-token node))
          (title (lark-wiki--node-title node)))
      (unless (yes-or-no-p
               (format "Delete wiki node \"%s\" and its subtree? " title))
        (user-error "Aborted"))
      (message "Lark: deleting wiki node...")
      (lark--run-command
       ;; --obj-type wiki declares that --node-token is a wiki node
       ;; token; the CLI requires it because these tokens are not
       ;; `wik'-prefixed and would otherwise be read as obj_tokens.
       (list "wiki" "+node-delete" "--as" "user"
             "--node-token" token
             "--space-id" lark-wiki--space-id
             "--obj-type" "wiki"
             "--yes")
       (lambda (_data)
         (message "Lark: node \"%s\" deleted" title)
         (lark-wiki-refresh))
       nil :no-error t))))

;;;; Copy node
;; CLI: wiki +node-copy --node-token X --space-id S (--target-space-id | --target-parent-node-token)

(defun lark-wiki-node-copy (target-parent new-title)
  "Copy the node at point under TARGET-PARENT, optionally renaming to NEW-TITLE.
TARGET-PARENT is a node token in the same space; empty copies to the
space root."
  (interactive
   (list (read-string "Target parent node token (empty = space root): ")
         (read-string "New title (empty = keep original): ")))
  (let ((node (lark-wiki--node-at-point)))
    (unless node (user-error "No node at point"))
    (when (equal (lark-wiki--obj-type node) "parent")
      (user-error "Not a copyable node"))
    (let ((args (list "wiki" "+node-copy" "--as" "user"
                      "--node-token" (lark-wiki--node-token node)
                      "--space-id" lark-wiki--space-id
                      "--yes")))
      (if (and target-parent (not (string-empty-p target-parent)))
          (setq args (append args (list "--target-parent-node-token" target-parent)))
        (setq args (append args (list "--target-space-id" lark-wiki--space-id))))
      (when (and new-title (not (string-empty-p new-title)))
        (setq args (append args (list "--title" new-title))))
      (message "Lark: copying wiki node...")
      (lark--run-command
       args
       (lambda (_data)
         (message "Lark: node copied")
         (lark-wiki-refresh))
       nil :no-error t))))

;;;; Move node
;; CLI: wiki +move --node-token X [--source-space-id S] (--target-space-id | --target-parent-token)

(defun lark-wiki-move (target-parent target-space)
  "Move the node at point under TARGET-PARENT, or into TARGET-SPACE root.
Provide TARGET-PARENT (a node token) or TARGET-SPACE (a space ID).
If both are empty, the move is aborted."
  (interactive
   (list (read-string "Target parent node token (empty to use space): ")
         (read-string "Target space ID (empty = current space): ")))
  (let ((node (lark-wiki--node-at-point)))
    (unless node (user-error "No node at point"))
    (when (equal (lark-wiki--obj-type node) "parent")
      (user-error "Not a movable node"))
    (when (and (string-empty-p target-parent) (string-empty-p target-space))
      (setq target-space lark-wiki--space-id))
    (let ((args (list "wiki" "+move" "--as" "user"
                      "--node-token" (lark-wiki--node-token node)
                      "--source-space-id" lark-wiki--space-id)))
      (when (and target-space (not (string-empty-p target-space)))
        (setq args (append args (list "--target-space-id" target-space))))
      (when (and target-parent (not (string-empty-p target-parent)))
        (setq args (append args (list "--target-parent-token" target-parent))))
      (message "Lark: moving wiki node...")
      (lark--run-command
       args
       (lambda (_data)
         (message "Lark: node moved")
         (lark-wiki-refresh))
       nil :no-error t))))

;;;; Copy helpers

(defun lark-wiki-copy-token ()
  "Copy the node token at point (or in a detail buffer) to the kill ring."
  (interactive)
  (let ((token (or (get-text-property (point) 'lark-wiki-node-token)
                   (and lark-wiki--detail-node
                        (lark-wiki--node-token lark-wiki--detail-node)))))
    (when (or (null token) (string-empty-p token) (equal token ".."))
      (user-error "No node at point"))
    (kill-new token)
    (message "Copied token: %s" token)))

(defun lark-wiki-copy-url ()
  "Copy the node URL at point to the kill ring."
  (interactive)
  (let ((node (lark-wiki--node-at-point)))
    (unless node (user-error "No node at point"))
    (let ((url (lark-wiki--node-url node)))
      (if (string-empty-p url)
          (user-error "No URL for this node (use w to copy the token)")
        (kill-new url)
        (message "Copied URL: %s" url)))))

(defun lark-wiki-copy-space-id ()
  "Copy the space ID at point to the kill ring."
  (interactive)
  (let ((id (get-text-property (point) 'lark-wiki-space-id)))
    (when (or (null id) (string-empty-p id))
      (user-error "No space at point"))
    (kill-new id)
    (message "Copied space ID: %s" id)))

;;;; Transient dispatch

;;;###autoload (autoload 'lark-wiki-dispatch "lark-wiki" nil t)
(transient-define-prefix lark-wiki-dispatch ()
  "Lark Wiki commands."
  ["Browse"
   ("l" "List spaces"   lark-wiki-spaces)
   ("g" "Refresh"       lark-wiki-refresh)
   ("^" "Up"            lark-wiki-up)
   ("v" "View content"  lark-wiki-view-content)]
  ["At Point"
   ("RET" "Open node"   lark-wiki-open)
   ("d"   "Node detail" lark-wiki-node-detail)
   ("w"   "Copy token"  lark-wiki-copy-token)
   ("W"   "Copy URL"    lark-wiki-copy-url)]
  ["Write"
   ("+" "Create node"   lark-wiki-node-create)
   ("C" "Copy node"     lark-wiki-node-copy)
   ("R" "Move node"     lark-wiki-move)
   ("D" "Delete node"   lark-wiki-node-delete)])

(provide 'lark-wiki)
;;; lark-wiki.el ends here
