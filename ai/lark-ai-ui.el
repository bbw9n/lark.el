;;; lark-ai-ui.el --- Fragment-based UI for the Lark AI buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; Provides a fragment-based UI model for the *Lark AI* buffer.
;; Inspired by agent-shell-ui.el, each section (prompt, skills, log,
;; plan, output) is a named fragment that can be inserted, updated,
;; or appended to independently — no full-buffer rerenders.
;;
;; Key concepts:
;;   - Fragment: a named region in the buffer with a type and body
;;   - Each fragment is identified by a `lark-ai-ui-id' text property
;;   - Bodies live in a buffer-local hash table (not text properties)
;;   - Markdown highlighting is applied incrementally on append

;;; Code:

(require 'cl-lib)

;;;; Fragment data model

(defvar-local lark-ai-ui--content-store nil
  "Hash table mapping fragment-id to body string.")

(defun lark-ai-ui--ensure-store ()
  "Ensure the content store exists."
  (unless lark-ai-ui--content-store
    (setq lark-ai-ui--content-store
          (make-hash-table :test 'equal))))

;;;; Fragment types
;;
;; Each fragment has:
;;   :id    — unique string identifier
;;   :type  — symbol: header, prompt, skills, log, plan, output
;;   :label — display label (optional)

;;;; Buffer setup

(defvar lark-ai-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'lark-ai-ui-toggle-section)
    map)
  "Keymap for `lark-ai-ui-mode'.")

(define-derived-mode lark-ai-ui-mode fundamental-mode "Lark AI"
  "Major mode for the Lark AI fragment-based UI buffer.
Read-only protection is handled per-fragment via text properties,
not `buffer-read-only', so the input area remains editable."
  (setq-local lark-ai-ui--content-store
              (make-hash-table :test 'equal))
  (setq buffer-read-only nil)
  (setq truncate-lines nil)
  (setq word-wrap t))

;;;; Low-level fragment operations

(defun lark-ai-ui--find-fragment (id)
  "Find the start and end positions of fragment ID.
Returns (BEG . END) or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((match (text-property-search-forward
                  'lark-ai-ui-id id #'equal)))
      (when match
        (let ((beg (prop-match-beginning match)))
          ;; Find end: next fragment or end of buffer
          (goto-char (prop-match-end match))
          (let ((next (text-property-search-forward
                       'lark-ai-ui-id nil
                       (lambda (val _) (and val (not (equal val id)))))))
            (cons beg (if next
                         (prop-match-beginning next)
                       (point-max)))))))))

(defun lark-ai-ui-insert-fragment (id type label body)
  "Insert a new fragment at point.
ID is a unique string, TYPE is a symbol, LABEL is the
header text, BODY is the content string."
  (lark-ai-ui--ensure-store)
  (puthash id (or body "") lark-ai-ui--content-store)
  (let ((inhibit-read-only t)
        (frag-beg (point)))
    ;; Label line
    (when label
      (insert (propertize label
                          'face (lark-ai-ui--label-face type)
                          'lark-ai-ui-id id
                          'lark-ai-ui-type type
                          'lark-ai-ui-section 'label)
              "\n"))
    ;; Body
    (when (and body (not (string-empty-p body)))
      (let ((body-beg (point)))
        (lark-ai-ui--insert-body id type body)
        (put-text-property body-beg (point)
                           'lark-ai-ui-id id)
        (put-text-property body-beg (point)
                           'lark-ai-ui-section 'body)))
    ;; Spacing
    (insert "\n")
    ;; Mark entire fragment as read-only
    (put-text-property frag-beg (point) 'read-only t)
    (put-text-property frag-beg (point)
                       'rear-nonsticky '(read-only))))

(defun lark-ai-ui--insert-body (_id type body)
  "Insert BODY for fragment of TYPE with appropriate formatting."
  (pcase type
    ('output (lark-ai-ui--insert-markdown body))
    ('plan   (insert body))
    ('log    (insert (propertize body 'face 'font-lock-comment-face)))
    (_       (insert body))))

(defun lark-ai-ui-update-fragment (id &optional label body)
  "Replace the content of fragment ID.
If LABEL is non-nil, update the label.  BODY replaces the body."
  (lark-ai-ui--ensure-store)
  (let ((region (lark-ai-ui--find-fragment id)))
    (if region
        (let ((inhibit-read-only t))
          (when body
            (puthash id body lark-ai-ui--content-store))
          (delete-region (car region) (cdr region))
          (goto-char (car region))
          (lark-ai-ui-insert-fragment
           id
           (or (get-text-property (car region) 'lark-ai-ui-type)
               'default)
           label
           (or body (gethash id lark-ai-ui--content-store ""))))
      ;; Fragment doesn't exist yet — insert at end
      (goto-char (point-max))
      (when body
        (puthash id (or body "") lark-ai-ui--content-store))
      (lark-ai-ui-insert-fragment
       id 'default label
       (or body "")))))

(defun lark-ai-ui-append-fragment (id text)
  "Append TEXT to the body of fragment ID.
Applies incremental markdown highlighting to the new text."
  (lark-ai-ui--ensure-store)
  (let* ((old (gethash id lark-ai-ui--content-store ""))
         (new (concat old text)))
    (puthash id new lark-ai-ui--content-store)
    ;; Find the fragment's body end and append there
    (let ((region (lark-ai-ui--find-fragment id)))
      (when region
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (cdr region))
            ;; Back up past the trailing newline
            (when (and (> (point) (point-min))
                       (eq (char-before) ?\n))
              (backward-char))
            (let ((ins-start (point)))
              (insert (propertize text 'lark-ai-ui-id id
                                  'lark-ai-ui-section 'body))
              ;; Incremental markdown highlighting
              (lark-ai-ui--highlight-region ins-start (point))
              ;; Match other fragment insertions: appended text is
              ;; read-only too, with the read-only stickiness limited
              ;; so typing right after it (e.g. in the input area
              ;; below) is still allowed.
              (put-text-property ins-start (point) 'read-only t)
              (put-text-property ins-start (point)
                                 'rear-nonsticky '(read-only)))))))))

(defun lark-ai-ui-clear ()
  "Clear all fragments from the buffer."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq lark-ai-ui--content-store
        (make-hash-table :test 'equal)))

;;;; Collapse / expand

(defun lark-ai-ui-toggle-section ()
  "Toggle collapse of the fragment at point."
  (interactive)
  (let ((id (get-text-property (point) 'lark-ai-ui-id)))
    (when id
      (let ((region (lark-ai-ui--find-fragment id)))
        (when region
          (save-excursion
            (goto-char (car region))
            ;; Find where label ends and body begins
            (let ((label-end
                   (next-single-property-change
                    (car region) 'lark-ai-ui-section
                    nil (cdr region))))
              (when (and label-end (< label-end (cdr region)))
                (let ((inhibit-read-only t)
                      (currently-invisible
                       (get-text-property label-end 'invisible)))
                  (put-text-property label-end (cdr region)
                                     'invisible
                                     (not currently-invisible)))))))))))

;;;; Markdown highlighting

(defun lark-ai-ui--insert-markdown (text)
  "Insert TEXT with markdown highlighting applied."
  (let ((start (point)))
    (insert text)
    (lark-ai-ui--highlight-region start (point))))

(defun lark-ai-ui--highlight-region (beg end)
  "Apply markdown highlighting to the region BEG..END."
  (save-excursion
    ;; Headers: # ## ### etc.
    (goto-char beg)
    (while (re-search-forward
            "^\\(#{1,4}\\) +\\(.+\\)$" end t)
      (put-text-property
       (match-beginning 0) (match-end 0) 'face
       (pcase (length (match-string 1))
         (1 'info-title-1) (2 'info-title-2)
         (3 'info-title-3) (_ 'info-title-4))))
    ;; Bold: **text**
    (goto-char beg)
    (while (re-search-forward
            "\\*\\*\\([^*]+\\)\\*\\*" end t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'bold))
    ;; Inline code: `text`
    (goto-char beg)
    (while (re-search-forward "`\\([^`\n]+\\)`" end t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'font-lock-constant-face))
    ;; Code blocks: ```...```
    (goto-char beg)
    (while (re-search-forward
            "^```[a-z]*\n\\(\\(?:.\\|\n\\)*?\\)\n```$" end t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'font-lock-string-face))
    ;; List items: - item or * item
    (goto-char beg)
    (while (re-search-forward "^\\([ ]*\\)[-*] " end t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'font-lock-keyword-face))))

;;;; Face helpers

(defun lark-ai-ui--label-face (type)
  "Return the face for a fragment label of TYPE."
  (pcase type
    ('header 'bold)
    ('prompt 'font-lock-keyword-face)
    ('skills 'font-lock-comment-face)
    ('plan   'font-lock-keyword-face)
    ('output 'bold)
    ('log    'font-lock-comment-face)
    ('separator 'font-lock-comment-face)
    (_       'default)))

;;;; Separator helper

(defun lark-ai-ui-insert-separator (id)
  "Insert a visual separator with ID."
  (let ((inhibit-read-only t)
        (beg (point)))
    (insert (propertize (make-string 50 ?─)
                        'face 'font-lock-comment-face
                        'lark-ai-ui-id id
                        'lark-ai-ui-type 'separator)
            "\n")
    (put-text-property beg (point) 'read-only t)
    (put-text-property beg (point)
                       'rear-nonsticky '(read-only))))

(provide 'lark-ai-ui)
;;; lark-ai-ui.el ends here
