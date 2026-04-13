;;; lark-ai-context.el --- Buffer context extraction for lark.el AI layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; Extracts structured context from the current Emacs buffer to inform
;; AI actions.  Each lark.el major mode has a context extractor that
;; returns a plist describing the domain, buffer type, and item at
;; point.  This lets `lark-ai-act' be context-aware without the user
;; having to re-state what they're looking at.

;;; Code:

(require 'lark-core)

;;;; Forward declarations for buffer-local variables from domain modules

(defvar lark-calendar--events)
(defvar lark-calendar--calendar-id)
(defvar lark-im--chat-id)
(defvar lark-im--chat-name)
(defvar lark-im--messages)
(defvar lark-tasks--items)
(defvar lark-tasks--tasklist-id)
(defvar lark-mail--items)
(defvar lark-mail--folder)
(defvar lark-mail--mail-id)
(defvar lark-docs--doc-token)
(defvar lark-docs--search-results)
(defvar lark-drive--folder-token)
(defvar lark-drive--files)
(defvar lark-meetings--items)
(defvar lark-sheets--token)
(defvar lark-sheets--sheet-id)
(defvar lark-contact--users)

;;;; Generic context extractor

(defun lark-ai-context ()
  "Extract structured context from the current buffer.
Returns a plist with:
  :domain      - string like \"calendar\", \"im\", \"tasks\", etc.
  :buffer-type - string like \"agenda\", \"chat\", \"inbox\", etc.
  :item        - plist describing the item at point (if any)
  :summary     - brief text summary for the LLM"
  (cond
   ((derived-mode-p 'lark-calendar-events-mode)
    (lark-ai-context--calendar))
   ((derived-mode-p 'lark-im-chat-mode)
    (lark-ai-context--im-chat))
   ((derived-mode-p 'lark-im-chats-mode)
    (lark-ai-context--im-chats))
   ((derived-mode-p 'lark-tasks-mode)
    (lark-ai-context--tasks))
   ((derived-mode-p 'lark-tasks-agenda-mode)
    (lark-ai-context--tasks))
   ((derived-mode-p 'lark-mail-mode)
    (lark-ai-context--mail))
   ((derived-mode-p 'lark-docs-search-mode)
    (lark-ai-context--docs))
   ((derived-mode-p 'lark-drive-mode)
    (lark-ai-context--drive))
   ((derived-mode-p 'lark-meetings-mode)
    (lark-ai-context--meetings))
   ((derived-mode-p 'lark-sheets-mode 'lark-sheets-info-mode)
    (lark-ai-context--sheets))
   ((derived-mode-p 'lark-contact-search-mode)
    (lark-ai-context--contacts))
   (t
    (list :domain nil
          :buffer-type "other"
          :item nil
          :summary (format "Buffer: %s (mode: %s)" (buffer-name) major-mode)))))

;;;; Domain-specific extractors

(defun lark-ai-context--calendar ()
  "Extract context from a calendar events buffer."
  (let ((event-id (get-text-property (point) 'lark-event-id))
        (events (and (boundp 'lark-calendar--events) lark-calendar--events)))
    (list :domain "calendar"
          :buffer-type "agenda"
          :item (when event-id
                  (list :event-id event-id
                        :event (seq-find (lambda (e)
                                           (equal (or (alist-get 'event_id e)
                                                      (alist-get 'id e))
                                                  event-id))
                                         events)))
          :summary (format "Calendar agenda with %d events%s"
                           (length events)
                           (if event-id (format ", cursor on event %s" event-id) "")))))

(defun lark-ai-context--im-chat ()
  "Extract context from a chat message buffer."
  (let ((chat-id (and (boundp 'lark-im--chat-id) lark-im--chat-id))
        (chat-name (and (boundp 'lark-im--chat-name) lark-im--chat-name))
        (msg-id (get-text-property (point) 'lark-message-id)))
    (list :domain "im"
          :buffer-type "chat"
          :item (list :chat-id chat-id
                      :chat-name chat-name
                      :message-id msg-id)
          :summary (format "Chat: %s%s"
                           (or chat-name chat-id "unknown")
                           (if msg-id (format ", cursor on message %s" msg-id) "")))))

(defun lark-ai-context--im-chats ()
  "Extract context from a chat list buffer."
  (let ((chats (and (boundp 'lark-im--chats) lark-im--chats))
        (chat-id (get-text-property (point) 'lark-chat-id)))
    (list :domain "im"
          :buffer-type "chat-list"
          :item (when chat-id (list :chat-id chat-id))
          :summary (format "Chat list with %d chats" (length chats)))))

(defun lark-ai-context--tasks ()
  "Extract context from a tasks buffer."
  (let ((items (and (boundp 'lark-tasks--items) lark-tasks--items))
        (task-id (get-text-property (point) 'lark-task-id)))
    (list :domain "tasks"
          :buffer-type "task-list"
          :item (when task-id
                  (list :task-id task-id
                        :task (seq-find (lambda (t)
                                          (equal (alist-get 'guid t) task-id))
                                        items)))
          :summary (format "Task list with %d tasks%s"
                           (length items)
                           (if task-id (format ", cursor on task %s" task-id) "")))))

(defun lark-ai-context--mail ()
  "Extract context from a mail buffer."
  (let ((items (and (boundp 'lark-mail--items) lark-mail--items))
        (mail-id (and (boundp 'lark-mail--mail-id) lark-mail--mail-id))
        (folder (and (boundp 'lark-mail--folder) lark-mail--folder)))
    (list :domain "mail"
          :buffer-type (if mail-id "mail-detail" "inbox")
          :item (when mail-id (list :mail-id mail-id))
          :summary (format "Mail %s%s"
                           (or folder "inbox")
                           (if mail-id (format ", viewing mail %s" mail-id)
                             (format " with %d items" (length items)))))))

(defun lark-ai-context--docs ()
  "Extract context from a docs buffer."
  (let ((doc-token (and (boundp 'lark-docs--doc-token) lark-docs--doc-token))
        (results (and (boundp 'lark-docs--search-results) lark-docs--search-results)))
    (list :domain "docs"
          :buffer-type (if doc-token "doc-detail" "search-results")
          :item (when doc-token (list :doc-token doc-token))
          :summary (if doc-token
                       (format "Viewing document %s" doc-token)
                     (format "Doc search results: %d items" (length results))))))

(defun lark-ai-context--drive ()
  "Extract context from a drive buffer."
  (let ((folder (and (boundp 'lark-drive--folder-token) lark-drive--folder-token))
        (files (and (boundp 'lark-drive--files) lark-drive--files))
        (file-token (get-text-property (point) 'lark-file-token)))
    (list :domain "drive"
          :buffer-type "file-list"
          :item (when file-token (list :file-token file-token))
          :summary (format "Drive folder %s, %d files"
                           (or folder "root") (length files)))))

(defun lark-ai-context--meetings ()
  "Extract context from a meetings buffer."
  (let ((items (and (boundp 'lark-meetings--items) lark-meetings--items))
        (meeting-id (get-text-property (point) 'lark-meeting-id)))
    (list :domain "meetings"
          :buffer-type "meeting-list"
          :item (when meeting-id (list :meeting-id meeting-id))
          :summary (format "Meeting list with %d meetings" (length items)))))

(defun lark-ai-context--sheets ()
  "Extract context from a sheets buffer."
  (let ((token (and (boundp 'lark-sheets--token) lark-sheets--token))
        (sheet-id (and (boundp 'lark-sheets--sheet-id) lark-sheets--sheet-id)))
    (list :domain "sheets"
          :buffer-type "spreadsheet"
          :item (list :token token :sheet-id sheet-id)
          :summary (format "Spreadsheet %s" (or token "unknown")))))

(defun lark-ai-context--contacts ()
  "Extract context from a contacts buffer."
  (let ((users (and (boundp 'lark-contact--users) lark-contact--users)))
    (list :domain "contacts"
          :buffer-type "contact-search"
          :item nil
          :summary (format "Contact search results: %d users" (length users)))))

;;;; Context formatting for LLM

(defun lark-ai-context-format ()
  "Format current buffer context as a string for the LLM prompt."
  (let ((ctx (lark-ai-context)))
    (if (plist-get ctx :domain)
        (format "Current context: %s (%s)\n%s"
                (plist-get ctx :domain)
                (plist-get ctx :buffer-type)
                (plist-get ctx :summary))
      "")))

(provide 'lark-ai-context)
;;; lark-ai-context.el ends here
