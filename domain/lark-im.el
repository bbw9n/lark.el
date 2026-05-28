;;; lark-im.el --- Lark Messenger integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

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
(require 'lark-ui)
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

(defvar-local lark-im--page-token nil
  "Next-page token for fetching older messages in the current chat buffer.
nil means either no older page is known yet, or no more older messages.")

(defvar-local lark-im--has-more nil
  "Non-nil if more older messages can be fetched in the current chat buffer.")

(defvar-local lark-im--loading-older nil
  "Non-nil while an older-messages fetch is in flight.")

(defvar-local lark-im--spinner-timer nil
  "Active loading-spinner handle for the current chat buffer, or nil.")

(defvar-local lark-im--spinner-frame nil
  "Current spinner frame string while older messages are loading.")

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

(defun lark-im--chat-description (chat)
  "Extract description from CHAT."
  (or (alist-get 'description chat) ""))

(defun lark-im--chat-create-time (chat)
  "Extract and format create time from CHAT.
Expects an ISO 8601 string like \"2026-01-04T09:08:05Z\".
Truncates to \"YYYY-MM-DD HH:MM\" for display."
  (let ((ts (alist-get 'create_time chat)))
    (if (and (stringp ts) (not (string-empty-p ts)))
        (substring ts 0 (min 16 (length ts)))
      "")))

(defun lark-im--insert-chat-field (label value)
  "Insert a LABEL: VALUE line if VALUE is non-empty."
  (lark-ui-insert-field label value))

(defun lark-im--insert-chat (chat)
  "Insert a multi-line section for CHAT into the current buffer."
  (let ((id (lark-im--chat-id-of chat))
        (name (lark-im--chat-name-of chat))
        (type (lark-im--chat-type chat))
        (owner (lark-im--chat-owner-name chat))
        (desc (lark-im--chat-description chat))
        (created (lark-im--chat-create-time chat))
        (members (lark-im--chat-member-count chat))
        (beg (point)))
    (insert (propertize name 'face 'bold) "\n")
    (lark-im--insert-chat-field "Type" type)
    (lark-im--insert-chat-field "Owner" owner)
    (lark-im--insert-chat-field "Description" desc)
    (lark-im--insert-chat-field "Created" created)
    (lark-im--insert-chat-field "Members" members)
    (insert "\n")
    (put-text-property beg (point) 'lark-chat-id id)
    (put-text-property beg (point) 'lark-chat-name name)))

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

(defun lark-im--messages-container (data)
  "Return the alist that holds messages plus pagination metadata in DATA.
Falls back to nil when DATA is not a recognized response shape."
  (cond
   ((and (listp data) (or (alist-get 'items data)
                          (alist-get 'messages data)
                          (assq 'has_more data)
                          (assq 'page_token data)))
    data)
   ((and (listp data) (alist-get 'data data))
    (let ((inner (alist-get 'data data)))
      (and (listp inner) inner)))
   (t nil)))

(defun lark-im--extract-messages (data)
  "Extract message list from lark-cli response DATA."
  (let ((c (lark-im--messages-container data)))
    (cond
     ((and c (alist-get 'items c)) (alist-get 'items c))
     ((and c (alist-get 'messages c)) (alist-get 'messages c))
     ((and (listp data) (listp (car-safe data))
           (or (alist-get 'message_id (car data))
               (alist-get 'sender_name (car data))))
      data)
     ((and c (listp c) (not (assq 'has_more c)) (not (assq 'page_token c)))
      c)
     (t nil))))

(defun lark-im--truthy-p (v)
  "Return non-nil if V is a truthy JSON value (t, \"true\", non-zero)."
  (cond
   ((eq v t) t)
   ((or (null v) (eq v :false) (eq v :null)) nil)
   ((stringp v) (not (member v '("" "false" "False" "FALSE" "0"))))
   ((numberp v) (not (zerop v)))
   (t t)))

(defun lark-im--extract-has-more (data)
  "Return non-nil if DATA indicates more pages are available."
  (let ((c (lark-im--messages-container data)))
    (and c (lark-im--truthy-p (alist-get 'has_more c)))))

(defun lark-im--extract-page-token (data)
  "Return the next-page token from DATA, or nil if absent."
  (let* ((c (lark-im--messages-container data))
         (v (and c (alist-get 'page_token c))))
    (and (stringp v) (not (string-empty-p v)) v)))

;;;; Chat list mode

(defvar lark-im-chats-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-im-chat-open)
    (define-key map (kbd "g")   #'lark-im-chats-refresh)
    (define-key map (kbd "s")   #'lark-im-send-to-chat)
    (define-key map (kbd "c")   #'lark-im-group-create)
    (define-key map (kbd "y")   #'lark-im-chat-copy-id)
    (define-key map (kbd "n")   #'lark-im--next-chat)
    (define-key map (kbd "p")   #'lark-im--prev-chat)
    (define-key map (kbd "?")   #'lark-im-dispatch)
    map)
  "Keymap for `lark-im-chats-mode'.")

(define-derived-mode lark-im-chats-mode special-mode
  "Lark Chats"
  "Major mode for browsing Lark chats.
Each chat is displayed as a multi-line section.")

(defun lark-im--chat-id-at-point ()
  "Return the chat ID at point, or nil."
  (get-text-property (point) 'lark-chat-id))

(defun lark-im--chat-name-at-point ()
  "Return the chat name at point, or nil."
  (get-text-property (point) 'lark-chat-name))

(defun lark-im--next-chat ()
  "Move to the next chat section."
  (interactive)
  (lark-ui-next-section 'lark-chat-id))

(defun lark-im--prev-chat ()
  "Move to the previous chat section."
  (interactive)
  (lark-ui-prev-section 'lark-chat-id))

;;;; Chat message mode

(defvar lark-im-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")   #'lark-im-chat-refresh)
    (define-key map (kbd "r")   #'lark-im-reply)
    (define-key map (kbd "R")   #'lark-ai-workflow-reply)
    (define-key map (kbd "s")   #'lark-im-send)
    (define-key map (kbd "e")   #'lark-im-react)
    (define-key map (kbd "o")   #'lark-im-load-older)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "?")   #'lark-im-dispatch)
    map)
  "Keymap for `lark-im-chat-mode'.")

(defun lark-im--maybe-autoload-older (window _start)
  "Auto-fetch older messages when WINDOW has scrolled to the top.
Hook attached to `window-scroll-functions' in chat buffers."
  (when (and lark-im--chat-id
             lark-im--has-more
             (not lark-im--loading-older)
             lark-im--page-token
             (= (window-start window) (point-min)))
    (lark-im-load-older)))

(define-derived-mode lark-im-chat-mode special-mode
  "Lark Chat"
  "Major mode for viewing a Lark chat thread."
  (add-hook 'window-scroll-functions
            #'lark-im--maybe-autoload-older nil t))

;;;; AI context providers

(defun lark-im--ai-context-chat ()
  "Return the AI context plist for a chat thread buffer.
Includes the rendered chat transcript as :content so the LLM can
work from what's already on screen instead of re-fetching via
the CLI."
  (let* ((chat-id lark-im--chat-id)
         (chat-name lark-im--chat-name)
         (msg-id (get-text-property (point) 'lark-message-id))
         (msg-count (length lark-im--messages))
         (has-more lark-im--has-more)
         (content (buffer-substring-no-properties (point-min) (point-max))))
    (list :domain "im"
          :buffer-type "chat"
          :item (list :chat-id chat-id
                      :chat-name chat-name
                      :message-id msg-id
                      :message-count msg-count
                      :has-older has-more)
          :content content
          ;; Messages render oldest→newest top→bottom, so if the content
          ;; is ever clipped keep the recent end (the bottom), not the top.
          :content-keep 'tail
          :summary (format "Chat: %s — %d message(s) loaded%s%s"
                           (or chat-name chat-id "unknown")
                           msg-count
                           (if has-more
                               " (older messages exist but are not loaded)"
                             "")
                           (if msg-id
                               (format "; cursor on message %s" msg-id)
                             "")))))

(defun lark-im--ai-context-chats ()
  "Return the AI context plist for a chat list buffer."
  (let ((chats lark-im--chats)
        (chat-id (get-text-property (point) 'lark-chat-id)))
    (list :domain "im"
          :buffer-type "chat-list"
          :item (when chat-id (list :chat-id chat-id))
          :summary (format "Chat list with %d chats" (length chats)))))

(put 'lark-im-chat-mode  'lark-ai-context-provider #'lark-im--ai-context-chat)
(put 'lark-im-chats-mode 'lark-ai-context-provider #'lark-im--ai-context-chats)

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
            lark-im--chat-query query)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null chats)
            (insert "(no chats)\n")
          (dolist (chat chats)
            (lark-im--insert-chat chat))))
      (goto-char (point-min))
      (setq header-line-format
            (format " Lark Chats — %d chat(s)" (length chats))))
    (pop-to-buffer buf)))

;;;; Chat messages
;; CLI: im +chat-messages-list --chat-id X [--page-size N] [--sort asc|desc]


(defun lark-im-chat-open ()
  "Open the chat at point and show message history."
  (interactive)
  (let ((id (lark-im--chat-id-at-point)))
    (unless id (user-error "No chat at point"))
    (let ((name (or (lark-im--chat-name-at-point) "")))
      (lark-im-messages id name))))

;;;###autoload
(defun lark-im-messages (chat-id &optional chat-name)
  "Show the latest message history for CHAT-ID (optionally with CHAT-NAME).
Fetches the most recent `lark-im-message-limit' messages and displays
them in chronological order (oldest at top, newest at bottom).  Older
messages can be loaded one page at a time with `lark-im-load-older'."
  (interactive "sChat ID: ")
  (message "Lark: fetching messages...")
  (lark--run-command
   (list "im" "+chat-messages-list"
         "--chat-id" chat-id
         "--page-size" lark-im-message-limit
         "--sort" "desc")
   (lambda (data)
     (lark-im--display-messages data chat-id (or chat-name chat-id)))))

(defun lark-im-chat-refresh ()
  "Refresh the current chat message buffer."
  (interactive)
  (when lark-im--chat-id
    (lark-im-messages lark-im--chat-id lark-im--chat-name)))

(defun lark-im--header-line ()
  "Build the header-line string for a chat buffer from buffer-local state.
While older messages load, the spinner animates here — the header line
is its own display region, so the glyph's `lark-spinner' face is never
clobbered by buffer text faces.  Otherwise it shows the \"press o\"
affordance when more history is available."
  (concat
   (format " Lark Chat: %s — %d message(s)"
           (or lark-im--chat-name "")
           (length lark-im--messages))
   (cond
    (lark-im--loading-older
     (concat "  "
             (or lark-im--spinner-frame (aref lark-spinner-frames 0))
             " loading older…"))
    (lark-im--has-more " (older available, press o)")
    (t ""))))

(defun lark-im--render-header (chat-name)
  "Insert the chat title block for CHAT-NAME at point.
The older-messages affordance and the loading spinner live on the
header line (see `lark-im--header-line'), not in the buffer body."
  (lark-ui-insert-title chat-name 1)
  (insert "\n"))

(defun lark-im--spinner-stop ()
  "Stop the loading spinner in the current chat buffer."
  (lark-spinner-stop lark-im--spinner-timer)
  (setq lark-im--spinner-timer nil))

(defun lark-im--spinner-start ()
  "Start the older-messages loading spinner on the chat buffer's header line.
Built on the shared `lark-spinner-start' engine: it animates the header
line while loading and self-cleans if loading finishes, the buffer
dies, or the safety timeout elapses (e.g. when the fetch errors without
ever invoking its callback).  Each tick stores the frame and forces a
header-line redisplay; `lark-im--header-line' renders it."
  (lark-im--spinner-stop)
  (let ((buf (current-buffer)))
    (setq lark-im--spinner-timer
          (lark-spinner-start
           (lambda (frame)
             (when (and (buffer-live-p buf)
                        (buffer-local-value 'lark-im--loading-older buf))
               (with-current-buffer buf
                 (setq lark-im--spinner-frame frame)
                 (force-mode-line-update))
               t))
           nil nil nil
           ;; On auto-stop (timeout / buffer death): clear stuck state.
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq lark-im--loading-older nil
                       lark-im--spinner-timer nil)
                 (force-mode-line-update))))))))

(defun lark-im--display-messages (data chat-id chat-name)
  "Display the latest message history DATA for CHAT-ID with CHAT-NAME.
DATA is expected to be desc-sorted (newest first); messages are reversed
so they render chronologically with the newest at the bottom."
  (let* ((raw (lark-im--extract-messages data))
         (messages (reverse raw))
         (has-more (lark-im--extract-has-more data))
         (page-token (lark-im--extract-page-token data))
         (buf (get-buffer-create (format "*Lark Chat: %s*" chat-name))))
    (with-current-buffer buf
      (lark-im-chat-mode)
      (setq lark-im--chat-id chat-id
            lark-im--chat-name chat-name
            lark-im--messages messages
            lark-im--has-more has-more
            lark-im--page-token page-token
            lark-im--loading-older nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (lark-im--render-header chat-name)
        (if (null messages)
            (insert "(no messages)\n")
          (dolist (msg messages)
            (lark-im--insert-message msg)))
        (goto-char (point-max)))
      (setq header-line-format '(:eval (lark-im--header-line))))
    (pop-to-buffer buf)))

(defun lark-im--header-end ()
  "Return buffer position just after the chat header block."
  (save-excursion
    (goto-char (point-min))
    (let ((p (point-min)))
      (while (and (not (eobp))
                  (not (get-text-property (point) 'lark-message-id)))
        (forward-line 1)
        (setq p (point)))
      p)))

(defun lark-im--prepend-older (data)
  "Insert older messages from DATA at the top of the current chat buffer."
  (lark-im--spinner-stop)
  (let* ((raw (lark-im--extract-messages data))
         (older (reverse raw))
         (has-more (lark-im--extract-has-more data))
         (page-token (lark-im--extract-page-token data))
         (anchor (or (get-text-property (point) 'lark-message-id)
                     (save-excursion
                       (goto-char (lark-im--header-end))
                       (get-text-property (point) 'lark-message-id))))
         (window-line (and anchor
                           (count-screen-lines (window-start) (point)))))
    (setq lark-im--has-more has-more
          lark-im--page-token page-token
          lark-im--loading-older nil
          lark-im--messages (append older lark-im--messages))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (let ((hend (lark-im--header-end)))
          (delete-region (point-min) hend))
        (goto-char (point-min))
        (lark-im--render-header lark-im--chat-name)
        (dolist (msg older)
          (lark-im--insert-message msg))))
    (setq header-line-format '(:eval (lark-im--header-line)))
    (when anchor
      (let (found)
        (save-excursion
          (goto-char (point-min))
          (while (and (not found) (not (eobp)))
            (if (equal (get-text-property (point) 'lark-message-id) anchor)
                (setq found (point))
              (goto-char (or (next-single-property-change
                              (point) 'lark-message-id)
                             (point-max))))))
        (when found
          (goto-char found)
          (when window-line
            (recenter window-line)))))
    (message "Lark: loaded %d older message(s)%s"
             (length older)
             (if has-more "" "; no more older messages"))))

(defun lark-im-load-older ()
  "Fetch one more page of older messages and prepend them to the buffer."
  (interactive)
  (unless lark-im--chat-id
    (user-error "Not in a chat buffer"))
  (cond
   (lark-im--loading-older
    (message "Lark: already loading older messages…"))
   ((not lark-im--has-more)
    (user-error "No more older messages"))
   ((not lark-im--page-token)
    (user-error "No pagination token available; try refreshing with g"))
   (t
    (setq lark-im--loading-older t)
    (lark-im--spinner-start)
    (message "Lark: loading older messages…")
    (let ((buf (current-buffer)))
      (lark--run-command
       (list "im" "+chat-messages-list"
             "--chat-id" lark-im--chat-id
             "--page-size" lark-im-message-limit
             "--sort" "desc"
             "--page-token" lark-im--page-token)
       (lambda (data)
         (if (buffer-live-p buf)
             (with-current-buffer buf
               (lark-im--prepend-older data))
           (message "Lark: chat buffer was closed"))))))))

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
  (let ((id (lark-im--chat-id-at-point)))
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
  (let ((id (or (lark-im--chat-id-at-point) lark-im--chat-id)))
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
                   (lark-ui-separator 60) "\n\n")
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

(autoload 'lark-ai-workflow-reply "lark-ai" nil t)

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
   ("R" "AI smart reply" lark-ai-workflow-reply)
   ("e" "React"         lark-im-react)
   ("o" "Load older"    lark-im-load-older)
   ("g" "Refresh"       lark-im-chat-refresh)])

(provide 'lark-im)
;;; lark-im.el ends here
