;;; lark-im.el --- Lark Messenger integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Provides Messenger domain commands for lark.el: chat listing,
;; message history, send/reply messages, reactions, group chat
;; management, and real-time event subscription.
;;
;; CLI command mapping:
;;   Chat search:     im +chat-search [--query X]
;;   Chat messages:   im +chat-messages-list --chat-id X
;;   Send message:    im +messages-send --chat-id X --text X  (NO --format)
;;   Reply:           im +messages-reply --message-id X --text X  (NO --format)
;;   Search messages: im +messages-search --query X
;;   Create group:    im +chat-create --name X [--users X]
;;   Reactions:       im reactions create --params JSON --data JSON

;;; Code:

(require 'lark-core)
(require 'lark-contact)
(require 'json)
(require 'transient)

;;;; Customization

(defgroup lark-im nil
  "Lark Messenger settings."
  :group 'lark
  :prefix "lark-im-")

(defcustom lark-im-message-limit "50"
  "Default number of messages to fetch in history."
  :type 'string
  :group 'lark-im)

;;;; Buffer-local variables

(defvar-local lark-im--chat-id nil
  "Chat ID for the current buffer.")

(defvar-local lark-im--chat-name nil
  "Chat name for the current buffer.")

(defvar-local lark-im--messages nil
  "Cached messages for the current buffer.")

(defvar-local lark-im--chats nil
  "Cached chat list for the current buffer.")

(defvar-local lark-im--chat-query nil
  "Last search query used to fetch the chat list.")

;;;; Event subscription process

(defvar lark-im--event-process nil
  "Long-running process for real-time event subscription.")

;;;; Chat parsing

(defun lark-im--chat-id-of (chat)
  "Extract the chat ID from CHAT."
  (or (alist-get 'chat_id chat)
      (alist-get 'id chat)))

(defun lark-im--chat-name-of (chat)
  "Extract the chat name from CHAT."
  (or (alist-get 'name chat)
      (alist-get 'chat_name chat)
      (alist-get 'title chat)
      "(unnamed)"))

(defun lark-im--chat-type (chat)
  "Extract the chat type from CHAT."
  (or (alist-get 'chat_type chat)
      (alist-get 'type chat)
      ""))

(defun lark-im--chat-member-count (chat)
  "Extract member count from CHAT."
  (let ((count (or (alist-get 'member_count chat)
                   (alist-get 'user_count chat))))
    (if count (format "%s" count) "")))

(defun lark-im--chat-owner-name (chat)
  "Resolve the owner display name from CHAT.
Uses owner_id and owner_id_type via `lark-contact-resolve-name'."
  (let ((owner-id (alist-get 'owner_id chat))
        (id-type (or (alist-get 'owner_id_type chat) "open_id")))
    (if (and owner-id (not (string-empty-p owner-id)))
        (lark-contact-resolve-name owner-id id-type)
      "")))

(defun lark-im--extract-chats (data)
  "Extract chat list from lark-cli response DATA."
  (cond
   ((and (listp data) (alist-get 'items data))
    (alist-get 'items data))
   ((and (listp data) (alist-get 'chats data))
    (alist-get 'chats data))
   ((and (listp data) (alist-get 'data data))
    (let ((inner (alist-get 'data data)))
      (or (alist-get 'items inner)
          (alist-get 'chats inner)
          (and (listp inner) inner))))
   ((and (listp data) (listp (car data))
         (or (alist-get 'chat_id (car data))
             (alist-get 'name (car data))))
    data)
   (t nil)))

(defun lark-im--make-chat-entries (chats)
  "Convert CHATS to `tabulated-list-entries' format."
  (mapcar
   (lambda (chat)
     (let ((id (lark-im--chat-id-of chat))
           (name (lark-im--chat-name-of chat))
           (type (lark-im--chat-type chat))
           (owner (lark-im--chat-owner-name chat))
           (members (lark-im--chat-member-count chat)))
       (list id (vector name type owner members))))
   chats))

;;;; Message parsing

(defun lark-im--msg-id (msg)
  "Extract message ID from MSG."
  (or (alist-get 'message_id msg)
      (alist-get 'id msg)))

(defun lark-im--msg-sender (msg)
  "Extract sender name from MSG."
  (or (alist-get 'sender_name msg)
      (lark--get-nested msg 'sender 'name)
      (lark--get-nested msg 'sender 'id)
      "unknown"))

(defun lark-im--msg-time (msg)
  "Extract and format message time from MSG."
  (let ((ts (or (alist-get 'create_time msg)
                (alist-get 'send_time msg)
                (alist-get 'timestamp msg))))
    (cond
     ((numberp ts) (or (lark--format-timestamp ts) ""))
     ((and (stringp ts) (string-match-p "^[0-9]+$" ts))
      (or (lark--format-timestamp ts) ""))
     ((stringp ts) ts)
     (t ""))))

(defun lark-im--msg-content (msg)
  "Extract message text content from MSG."
  (or (alist-get 'text msg)
      (alist-get 'content msg)
      (let ((body (alist-get 'body msg)))
        (when body
          (if (stringp body) body
            (or (alist-get 'text body)
                (alist-get 'content body)))))
      (lark--get-nested msg 'msg 'content)
      ""))

(defun lark-im--msg-type (msg)
  "Extract message type from MSG."
  (or (alist-get 'msg_type msg)
      (alist-get 'message_type msg)
      (alist-get 'type msg)
      "text"))

(defun lark-im--extract-messages (data)
  "Extract message list from lark-cli response DATA."
  (cond
   ((and (listp data) (alist-get 'items data))
    (alist-get 'items data))
   ((and (listp data) (alist-get 'messages data))
    (alist-get 'messages data))
   ((and (listp data) (alist-get 'data data))
    (let ((inner (alist-get 'data data)))
      (or (alist-get 'items inner)
          (alist-get 'messages inner)
          (and (listp inner) inner))))
   ((and (listp data) (listp (car data))
         (or (alist-get 'message_id (car data))
             (alist-get 'sender_name (car data))))
    data)
   (t nil)))

;;;; Chat list mode

(defvar lark-im-chats-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-im-chat-open)
    (define-key map (kbd "g")   #'lark-im-chats-refresh)
    (define-key map (kbd "s")   #'lark-im-send-to-chat)
    (define-key map (kbd "c")   #'lark-im-group-create)
    (define-key map (kbd "y")   #'lark-im-chat-copy-id)
    (define-key map (kbd "?")   #'lark-im-dispatch)
    map)
  "Keymap for `lark-im-chats-mode'.")

(define-derived-mode lark-im-chats-mode tabulated-list-mode
  "Lark Chats"
  "Major mode for browsing Lark chats."
  (setq tabulated-list-format
        [("Name" 36 t)
         ("Type" 10 t)
         ("Owner" 20 t)
         ("Members" 8 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;; Chat message mode

(defvar lark-im-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")   #'lark-im-chat-refresh)
    (define-key map (kbd "r")   #'lark-im-reply)
    (define-key map (kbd "s")   #'lark-im-send)
    (define-key map (kbd "e")   #'lark-im-react)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "?")   #'lark-im-dispatch)
    map)
  "Keymap for `lark-im-chat-mode'.")

(define-derived-mode lark-im-chat-mode special-mode
  "Lark Chat"
  "Major mode for viewing a Lark chat thread.")

;;;; Chat listing
;; CLI: im +chat-search [--query X]


;;;###autoload
(defun lark-im-chats (&optional query)
  "List Lark chats. With QUERY, search by keyword.
When called interactively, prompt for a search query."
  (interactive
   (list (read-string "Search chats (keyword): ")))
  (when (or (null query) (string-empty-p query))
    (user-error "A search query is required (e.g. a chat name or keyword)"))
  (message "Lark: fetching chats...")
  (let ((q query))
    (lark--run-command
     (list "im" "+chat-search" "--query" query)
     (lambda (data) (lark-im--display-chats data q)))))

(defun lark-im-chats-refresh ()
  "Refresh the chat list buffer with the previous query."
  (interactive)
  (lark-im-chats (or lark-im--chat-query
                      (read-string "Search chats (keyword): "))))

(defun lark-im--display-chats (data &optional query)
  "Display chat list DATA.  QUERY is stored for refresh."
  (let* ((chats (lark-im--extract-chats data))
         (buf (get-buffer-create "*Lark Chats*")))
    (with-current-buffer buf
      (lark-im-chats-mode)
      (setq lark-im--chats chats
            lark-im--chat-query query
            tabulated-list-entries (lark-im--make-chat-entries chats))
      (tabulated-list-print t)
      (setq header-line-format
            (format " Lark Chats — %d chat(s)" (length chats))))
    (pop-to-buffer buf)))

;;;; Chat messages
;; CLI: im +chat-messages-list --chat-id X [--page-size N] [--sort asc|desc]


(defun lark-im-chat-open ()
  "Open the chat at point and show message history."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No chat at point"))
    (let ((name (aref (tabulated-list-get-entry) 0)))
      (lark-im-messages id name))))

;;;###autoload
(defun lark-im-messages (chat-id &optional chat-name)
  "Show message history for CHAT-ID (optionally with CHAT-NAME)."
  (interactive "sChat ID: ")
  (message "Lark: fetching messages...")
  (lark--run-command
   (list "im" "+chat-messages-list"
         "--chat-id" chat-id
         "--page-size" lark-im-message-limit
         "--sort" "asc")
   (lambda (data)
     (lark-im--display-messages data chat-id (or chat-name chat-id)))))

(defun lark-im-chat-refresh ()
  "Refresh the current chat message buffer."
  (interactive)
  (when lark-im--chat-id
    (lark-im-messages lark-im--chat-id lark-im--chat-name)))

(defun lark-im--display-messages (data chat-id chat-name)
  "Display message history DATA for CHAT-ID with CHAT-NAME."
  (let* ((messages (lark-im--extract-messages data))
         (buf (get-buffer-create (format "*Lark Chat: %s*" chat-name))))
    (with-current-buffer buf
      (lark-im-chat-mode)
      (setq lark-im--chat-id chat-id
            lark-im--chat-name chat-name
            lark-im--messages messages)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize chat-name 'face 'bold) "\n"
                (make-string (min 60 (max 20 (length chat-name))) ?─) "\n\n")
        (if (null messages)
            (insert "(no messages)\n")
          (dolist (msg messages)
            (lark-im--insert-message msg)))
        (goto-char (point-max)))
      (setq header-line-format
            (format " Lark Chat: %s — %d message(s)" chat-name (length messages))))
    (pop-to-buffer buf)))

(defun lark-im--insert-message (msg)
  "Insert a formatted MSG into the current buffer."
  (let ((sender (lark-im--msg-sender msg))
        (time (lark-im--msg-time msg))
        (content (lark-im--msg-content msg))
        (type (lark-im--msg-type msg))
        (id (lark-im--msg-id msg))
        (beg (point)))
    (insert (propertize sender 'face 'bold)
            "  "
            (propertize time 'face 'font-lock-comment-face)
            "\n")
    (if (equal type "text")
        (insert content "\n")
      (insert (propertize (format "[%s message]" type)
                          'face 'font-lock-type-face)
              (if (string-empty-p content) "" (format ": %s" content))
              "\n"))
    (insert "\n")
    (put-text-property beg (point) 'lark-message-id id)))

;;;; Send message
;; CLI: im +messages-send --chat-id X --text X


;;;###autoload
(defun lark-im-send (&optional chat-id)
  "Send a message to CHAT-ID (or the current chat buffer)."
  (interactive)
  (let ((id (or chat-id lark-im--chat-id
                (read-string "Chat ID: "))))
    (when (string-empty-p id)
      (user-error "No chat ID specified"))
    (let ((text (read-string "Message: ")))
      (when (string-empty-p text)
        (user-error "Empty message"))
      (message "Lark: sending message...")
      (lark--run-command
       (list "im" "+messages-send" "--chat-id" id "--text" text)
       (lambda (_data)
         (message "Lark: message sent")
         (when lark-im--chat-id
           (lark-im-chat-refresh)))))))

(defun lark-im-send-to-chat ()
  "Send a message to the chat at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No chat at point"))
    (lark-im-send id)))

;;;; Reply
;; CLI: im +messages-reply --message-id X --text X

(defun lark-im-reply ()
  "Reply to a message in the current chat.
Prompts for message ID if not determinable from point."
  (interactive)
  (unless lark-im--chat-id
    (user-error "Not in a chat buffer"))
  (let* ((msg-id (or (get-text-property (point) 'lark-message-id)
                     (read-string "Reply to message ID: ")))
         (text (read-string "Reply: ")))
    (when (string-empty-p text)
      (user-error "Empty reply"))
    (message "Lark: sending reply...")
    (lark--run-command
     (list "im" "+messages-reply" "--message-id" msg-id "--text" text)
     (lambda (_data)
       (message "Lark: reply sent")
       (lark-im-chat-refresh)))))

;;;; Reactions
;; CLI: im reactions create --params '{"message_id":"X"}' --data '{"reaction_type":{"emoji_type":"X"}}'


(defun lark-im-react ()
  "Add a reaction to a message in the current chat."
  (interactive)
  (unless lark-im--chat-id
    (user-error "Not in a chat buffer"))
  (let* ((msg-id (or (get-text-property (point) 'lark-message-id)
                     (read-string "Message ID: ")))
         (emoji (read-string "Emoji type (e.g., THUMBSUP): ")))
    (when (string-empty-p emoji)
      (user-error "No emoji specified"))
    (let ((params (json-encode `((message_id . ,msg-id))))
          (body (json-encode `((reaction_type . ((emoji_type . ,emoji)))))))
      (lark--run-command
       (list "im" "reactions" "create" "--params" params "--data" body)
       (lambda (_data)
         (message "Lark: reaction added"))))))

;;;; Chat copy ID

(defun lark-im-chat-copy-id ()
  "Copy the chat ID at point to the kill ring."
  (interactive)
  (let ((id (or (tabulated-list-get-id) lark-im--chat-id)))
    (unless id (user-error "No chat ID"))
    (kill-new id)
    (message "Copied: %s" id)))

;;;; Group chat management
;; CLI: im +chat-create --name X [--description X] [--users X] [--type private|public]


;;;###autoload (autoload 'lark-im-group-create "lark-im" nil t)
(transient-define-prefix lark-im-group-create ()
  "Create a new Lark group chat."
  ["Group Details"
   ("n" "Name"        "--name=" :prompt "Group name: ")
   ("d" "Description" "--description=" :prompt "Description: ")]
  ["Members"
   ("m" "Users"       "--users=" :prompt "User open_ids (comma-separated ou_): ")]
  ["Actions"
   ("RET" "Create"    lark-im--do-group-create)
   ("q"   "Cancel"    transient-quit-all)])

(defun lark-im--do-group-create (&rest _args)
  "Execute group chat creation with transient arguments."
  (interactive)
  (let ((args (transient-args 'lark-im-group-create)))
    (unless args (user-error "No group details provided"))
    (message "Lark: creating group chat...")
    (lark--run-command
     (append '("im" "+chat-create") args)
     (lambda (data)
       (let ((id (or (lark--get-nested data 'data 'chat_id)
                     (alist-get 'chat_id data))))
         (message "Lark: group created%s"
                  (if id (format " (ID: %s)" id) "")))))))

;;;; Search messages
;; CLI: im +messages-search --query X


;;;###autoload
(defun lark-im-search (query)
  "Search Lark messages for QUERY."
  (interactive "sSearch messages: ")
  (message "Lark: searching messages...")
  (lark--run-command
   (list "im" "+messages-search" "--query" query)
   (lambda (data)
     (let* ((messages (lark-im--extract-messages data))
            (buf (get-buffer-create (format "*Lark Search: %s*" query))))
       (with-current-buffer buf
         (lark-im-chat-mode)
         (setq lark-im--messages messages)
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (propertize (format "Search: %s\n" query) 'face 'bold)
                   (make-string 60 ?─) "\n\n")
           (if (null messages)
               (insert "(no results)\n")
             (dolist (msg messages)
               (lark-im--insert-message msg))))
         (goto-char (point-min))
         (setq header-line-format
               (format " Lark Search: \"%s\" — %d result(s)"
                       query (length messages))))
       (pop-to-buffer buf)))))

;;;; Real-time event subscription

;;;###autoload
(defun lark-im-subscribe ()
  "Subscribe to real-time Lark messenger events.
Starts a long-running process that receives NDJSON events."
  (interactive)
  (when (and lark-im--event-process
             (process-live-p lark-im--event-process))
    (if (yes-or-no-p "Event subscription already active.  Restart? ")
        (lark-im-unsubscribe)
      (user-error "Subscription already active")))
  (message "Lark: subscribing to events...")
  (let ((exe (lark--executable)))
    (setq lark-im--event-process
          (make-process
           :name "lark-events"
           :command (list exe "event" "subscribe"
                         "--format" "ndjson" "--compact")
           :connection-type 'pipe
           :noquery t
           :filter #'lark-im--event-filter
           :sentinel (lambda (_proc event)
                       (message "Lark events: %s" (string-trim event))))))
  (message "Lark: event subscription started"))

(defun lark-im--event-filter (_proc output)
  "Process filter for real-time events.  Parses NDJSON OUTPUT lines."
  (dolist (event (lark--parse-ndjson output))
    (lark-im--handle-event event)))

(defun lark-im--handle-event (event)
  "Handle a single real-time EVENT.
Dispatches to appropriate handler based on event type."
  (let ((type (or (alist-get 'type event)
                  (alist-get 'event_type event))))
    (lark--log "Event received: %s" type)
    (cond
     ((and type (string-match-p "message" type))
      (lark-im--handle-new-message event))
     (t nil))))

(defun lark-im--handle-new-message (event)
  "Handle a new message EVENT — refresh chat buffer if open."
  (let* ((msg (or (alist-get 'message event)
                  (alist-get 'data event)
                  event))
         (chat-id (or (alist-get 'chat_id msg)
                      (lark--get-nested msg 'message 'chat_id))))
    (when chat-id
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and (eq major-mode 'lark-im-chat-mode)
                     (equal lark-im--chat-id chat-id))
            (lark-im-chat-refresh)))))))

;;;###autoload
(defun lark-im-unsubscribe ()
  "Stop the real-time event subscription."
  (interactive)
  (when (and lark-im--event-process
             (process-live-p lark-im--event-process))
    (delete-process lark-im--event-process))
  (setq lark-im--event-process nil)
  (message "Lark: event subscription stopped"))

;;;; Transient dispatch

;;;###autoload (autoload 'lark-im-dispatch "lark-im" nil t)
(transient-define-prefix lark-im-dispatch ()
  "Lark Messenger commands."
  ["Quick Actions"
   ("l" "Chat list"     lark-im-chats)
   ("s" "Send message"  lark-im-send)
   ("/" "Search"        lark-im-search)]
  ["Group"
   ("c" "Create group"  lark-im-group-create)]
  ["Real-time"
   ("S" "Subscribe"     lark-im-subscribe)
   ("U" "Unsubscribe"   lark-im-unsubscribe)]
  ["In Chat"
   ("r" "Reply"         lark-im-reply)
   ("e" "React"         lark-im-react)
   ("g" "Refresh"       lark-im-chat-refresh)])

(provide 'lark-im)
;;; lark-im.el ends here
