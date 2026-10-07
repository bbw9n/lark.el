;;; lark-im-test.el --- Tests for lark-im.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'lark-im)

;; Keep tests hermetic: never touch the on-disk name cache.
(setq lark-contact-cache-file nil
      lark-contact--cache-loaded t)

;;;; Chat parsing

(ert-deftest lark-im-test-chat-id ()
  (should (equal (lark-im--chat-id-of '((chat_id . "oc_123"))) "oc_123"))
  (should (equal (lark-im--chat-id-of '((id . "456"))) "456")))

(ert-deftest lark-im-test-chat-name ()
  (should (equal (lark-im--chat-name-of '((name . "Dev Team"))) "Dev Team"))
  (should (equal (lark-im--chat-name-of '((title . "General"))) "General"))
  (should (equal (lark-im--chat-name-of '((foo . "bar"))) "(unnamed)")))

(ert-deftest lark-im-test-chat-type ()
  (should (equal (lark-im--chat-type '((chat_type . "group"))) "group"))
  (should (equal (lark-im--chat-type '((foo . "bar"))) "")))

(ert-deftest lark-im-test-chat-member-count ()
  (should (equal (lark-im--chat-member-count '((member_count . 5))) "5"))
  (should (equal (lark-im--chat-member-count '((foo . "bar"))) "")))

;;;; Extract chats

(ert-deftest lark-im-test-extract-chats-items ()
  (let ((data '((items . (((chat_id . "1")) ((chat_id . "2")))))))
    (should (= (length (lark-im--extract-chats data)) 2))))

(ert-deftest lark-im-test-extract-chats-nested ()
  (let ((data '((data . ((chats . (((chat_id . "1")))))))))
    (should (= (length (lark-im--extract-chats data)) 1))))

(ert-deftest lark-im-test-extract-chats-empty ()
  (should (null (lark-im--extract-chats nil))))

(ert-deftest lark-im-test-extract-null-collections ()
  "Null chats/messages collections extract to nil, not `:null'."
  (should-not (lark-im--extract-chats
               '((data . ((chats . :null))))))
  (should-not (lark-im--extract-messages
               '((data . ((has_more . :false) (messages . :null)))))))

;;;; Message parsing

(ert-deftest lark-im-test-msg-id ()
  (should (equal (lark-im--msg-id '((message_id . "m_123"))) "m_123"))
  (should (equal (lark-im--msg-id '((id . "456"))) "456")))

(ert-deftest lark-im-test-msg-sender ()
  (should (equal (lark-im--msg-sender '((sender_name . "Alice"))) "Alice"))
  (should (equal (lark-im--msg-sender '((sender . ((name . "Bob"))))) "Bob"))
  (should (equal (lark-im--msg-sender '((foo . "bar"))) "unknown")))

(ert-deftest lark-im-test-msg-content ()
  (should (equal (lark-im--msg-content '((text . "hello"))) "hello"))
  (should (equal (lark-im--msg-content '((content . "world"))) "world"))
  (should (equal (lark-im--msg-content '((body . "text body"))) "text body"))
  (should (equal (lark-im--msg-content '((body . ((text . "nested"))))) "nested"))
  (should (equal (lark-im--msg-content '((foo . "bar"))) "")))

(ert-deftest lark-im-test-msg-type ()
  (should (equal (lark-im--msg-type '((msg_type . "image"))) "image"))
  (should (equal (lark-im--msg-type '((foo . "bar"))) "text")))

(ert-deftest lark-im-test-msg-time-numeric ()
  (let ((result (lark-im--msg-time '((create_time . 1700000000)))))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest lark-im-test-msg-time-string ()
  (should (equal (lark-im--msg-time '((create_time . "2026-04-11")))
                 "2026-04-11")))

(ert-deftest lark-im-test-msg-time-nil ()
  (should (equal (lark-im--msg-time '((foo . "bar"))) "")))

;;;; Extract messages

(ert-deftest lark-im-test-extract-messages-items ()
  (let ((data '((items . (((message_id . "1")) ((message_id . "2")))))))
    (should (= (length (lark-im--extract-messages data)) 2))))

(ert-deftest lark-im-test-extract-messages-nested ()
  (let ((data '((data . ((messages . (((message_id . "1")))))))))
    (should (= (length (lark-im--extract-messages data)) 1))))

(ert-deftest lark-im-test-extract-messages-empty ()
  (should (null (lark-im--extract-messages nil))))

;;;; Make entries

(ert-deftest lark-im-test-insert-chat ()
  (let* ((lark-contact--user-cache (make-hash-table :test 'equal))
         (chat '((chat_id . "c1")
                 (name . "Dev Team")
                 (chat_type . "group")
                 (description . "Engineering chat")
                 (create_time . "2026-01-04T09:08:05Z")
                 (member_count . 10))))
    (with-temp-buffer
      (lark-im--insert-chat chat)
      (goto-char (point-min))
      ;; Title line present
      (should (search-forward "Dev Team" nil t))
      ;; Fields present
      (should (search-forward "group" nil t))
      (should (search-forward "Engineering chat" nil t))
      (should (search-forward "2026-01-04 09:08" nil t))
      (should (search-forward "10" nil t))
      ;; Text property covers the section
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'lark-chat-id) "c1"))
      (should (equal (get-text-property (point) 'lark-chat-name) "Dev Team")))))

;;;; Schema fixes: textual types and deleted filter

(ert-deftest lark-im-test-msg-textual-p ()
  "Text, post, and markdown are textual; image/file/sticker are not."
  (should (lark-im--msg-textual-p '((msg_type . "text"))))
  (should (lark-im--msg-textual-p '((msg_type . "post"))))
  (should (lark-im--msg-textual-p '((msg_type . "markdown"))))
  (should-not (lark-im--msg-textual-p '((msg_type . "image"))))
  (should-not (lark-im--msg-textual-p '((msg_type . "file"))))
  (should-not (lark-im--msg-textual-p '((msg_type . "sticker")))))

(ert-deftest lark-im-test-msg-deleted-p ()
  "Only `deleted: t' counts as deleted (not :false, not missing)."
  (should     (lark-im--msg-deleted-p '((deleted . t))))
  (should-not (lark-im--msg-deleted-p '((deleted . :false))))
  (should-not (lark-im--msg-deleted-p '((other . "field")))))

(ert-deftest lark-im-test-extract-messages-filters-deleted ()
  "`extract-messages' drops messages flagged `deleted: t'."
  (let ((data `((data . ((has_more . :false)
                         (messages . (((message_id . "m1") (deleted . :false))
                                      ((message_id . "m2") (deleted . t))
                                      ((message_id . "m3") (deleted . :false)))))))))
    (let ((extracted (lark-im--extract-messages data)))
      (should (= 2 (length extracted)))
      (should (equal '("m1" "m3")
                     (mapcar (lambda (m) (alist-get 'message_id m)) extracted))))))

(ert-deftest lark-im-test-insert-message-post-no-prefix ()
  "A `post' message renders content directly, no \"[post message]: \" prefix."
  (with-temp-buffer
    (lark-im--insert-message
     '((message_id . "m1") (msg_type . "post") (create_time . "2026-05-27 20:17")
       (sender . ((name . "陈大伟"))) (content . "Hello readable text")))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should     (string-match-p "Hello readable text" text))
      (should-not (string-match-p "\\[post message\\]" text)))))

(ert-deftest lark-im-test-chat-mode ()
  "`lark-im--chat-mode' reads the `chat_mode' field (group/topic)."
  (should (equal "group" (lark-im--chat-mode '((chat_mode . "group")))))
  (should (equal "topic" (lark-im--chat-mode '((chat_mode . "topic")))))
  (should (equal ""      (lark-im--chat-mode '()))))

(ert-deftest lark-im-test-insert-chat-shows-mode ()
  "`insert-chat' renders a Mode: line populated from `chat_mode'."
  (with-temp-buffer
    (lark-im--insert-chat '((chat_id . "c1") (name . "Demo") (chat_mode . "topic")))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Mode" text))
      (should (string-match-p "topic" text)))))

(ert-deftest lark-im-test-insert-message-image-labelled ()
  "An `image' message is still labelled (since the content is metadata, not body)."
  (with-temp-buffer
    (lark-im--insert-message
     '((message_id . "m2") (msg_type . "image") (create_time . "2026-05-27 20:17")
       (sender . ((name . "alice"))) (content . "{\"image_key\":\"abc\"}")))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "\\[image\\]" text)))))

;;;; Async owner resolution

(ert-deftest lark-im-test-owner-render-never-blocks ()
  "Chat rendering uses cache-or-raw-id; it must never fetch synchronously."
  (let ((lark-contact--user-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'lark-contact-get-user-sync)
               (lambda (&rest _) (error "sync fetch during render"))))
      ;; Cache miss → raw id, tagged for the async resolver.
      (with-temp-buffer
        (lark-im--insert-chat '((chat_id . "c1") (name . "Demo")
                                (owner_id . "ou_abc")))
        (goto-char (point-min))
        (should (search-forward "ou_abc" nil t))
        (should (equal '("ou_abc" . "open_id")
                       (get-text-property (match-beginning 0)
                                          'lark-contact-ref))))
      ;; Cache hit → name shown directly.
      (lark-contact--cache-put "ou_abc" "open_id" "Alice")
      (with-temp-buffer
        (lark-im--insert-chat '((chat_id . "c1") (name . "Demo")
                                (owner_id . "ou_abc")))
        (goto-char (point-min))
        (should (search-forward "Alice" nil t))))))

(ert-deftest lark-im-test-owner-async-patch ()
  "The async resolver patches raw ids in place, preserving section props."
  (let ((lark-contact--user-cache (make-hash-table :test 'equal))
        (fetched nil))
    (cl-letf (((symbol-function 'lark--run-command)
               (lambda (args callback &rest _)
                 (push args fetched)
                 (funcall callback
                          '((data . ((user . ((name . "Alice"))))))))))
      (with-temp-buffer
        ;; Two chats, same unresolved owner → ONE lookup, both patched.
        (lark-im--insert-chat '((chat_id . "c1") (name . "One")
                                (owner_id . "ou_abc")))
        (lark-im--insert-chat '((chat_id . "c2") (name . "Two")
                                (owner_id . "ou_abc")))
        (lark-contact-resolve-buffer-async (current-buffer))
        (should (= 1 (length fetched)))
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should-not (string-match-p "ou_abc" text))
          (should (= 2 (with-temp-buffer
                         (insert text)
                         (count-matches "Alice" (point-min) (point-max))))))
        ;; Section property survived the splice.
        (goto-char (point-min))
        (search-forward "Alice")
        (should (equal "c1" (get-text-property (match-beginning 0)
                                               'lark-chat-id)))
        ;; Name landed in the cache for future renders.
        (should (equal "Alice"
                       (lark-contact--cache-get "ou_abc" "open_id")))))))

(ert-deftest lark-im-test-append-sent-message ()
  "A sent message is appended in place — no erase, no refetch."
  (with-temp-buffer
    (setq-local lark-im--chat-id "oc_1"
                lark-im--chat-name "Demo"
                lark-im--messages nil)
    (lark-im--insert-message
     '((message_id . "m1") (msg_type . "text")
       (create_time . "2026-10-04 10:00")
       (sender . ((name . "alice"))) (content . "existing message")))
    (setq-local lark-im--messages '(((message_id . "m1"))))
    (let ((before (buffer-substring-no-properties (point-min) (point-max))))
      (lark-im--append-sent-message
       (current-buffer) "my new reply"
       '((data . ((message_id . "om_new")))))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        ;; Old content intact (prefix preserved → nothing was erased).
        (should (string-prefix-p before text))
        (should (string-match-p "my new reply" text))
        (should (string-match-p "Me" text)))
      (should (= 2 (length lark-im--messages)))
      ;; Echoed entry carries the server message id for reply-at-point.
      (goto-char (point-max))
      (search-backward "my new reply")
      (should (equal "om_new"
                     (get-text-property (point) 'lark-message-id))))))

(ert-deftest lark-im-test-send-appends-instead-of-refreshing ()
  "Send and reply callbacks echo locally; the old full refresh is gone."
  (let (sent-args)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "hello there"))
              ((symbol-function 'lark-im-chat-refresh)
               (lambda (&rest _) (error "full refresh must not run")))
              ((symbol-function 'lark--run-command)
               (lambda (args callback &rest _)
                 (setq sent-args args)
                 (funcall callback '((data . ((message_id . "om_1"))))))))
      (with-temp-buffer
        (setq-local lark-im--chat-id "oc_1"
                    lark-im--chat-name "Demo"
                    lark-im--messages nil)
        (lark-im-send "oc_1")
        (should (equal '("im" "+messages-send" "--chat-id" "oc_1"
                         "--text" "hello there")
                       sent-args))
        (should (string-match-p "hello there"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))
        ;; Reply path too.
        (goto-char (point-min))
        (lark-im-reply)
        (should (equal "+messages-reply" (cadr sent-args)))
        (should (= 2 (length lark-im--messages)))))))

(ert-deftest lark-im-test-thread-replies-render-as-tree ()
  "THREAD-chat replies render nested under their root, oldest first,
deleted ones dropped; each reply keeps its own message id at point."
  (with-temp-buffer
    (lark-im--insert-message
     '((message_id . "root1") (msg_type . "text")
       (create_time . "2026-10-04 10:00")
       (sender . ((name . "alice")))
       (content . "root question")
       (thread_replies
        . (((message_id . "r2") (msg_type . "text")
            (create_time . "2026-10-04 10:20")
            (sender . ((name . "carol"))) (content . "second reply"))
           ((message_id . "rdel") (msg_type . "text") (deleted . t)
            (create_time . "2026-10-04 10:10")
            (sender . ((name . "x"))) (content . "deleted reply"))
           ((message_id . "r1") (msg_type . "text")
            (create_time . "2026-10-04 10:05")
            (sender . ((name . "bob"))) (content . "first reply"))))))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      ;; Both live replies present, behind the gutter, in time order.
      (should (string-match-p "│ .*first reply" text))
      (should (string-match-p "│ .*second reply" text))
      (should-not (string-match-p "deleted reply" text))
      (should (< (string-match "first reply" text)
                 (string-match "second reply" text)))
      ;; Root content is NOT indented.
      (should (string-match-p "^root question" text)))
    ;; Reply-at-point targets the reply, not the root.
    (goto-char (point-min))
    (search-forward "first reply")
    (should (equal "r1" (get-text-property (point) 'lark-message-id)))
    (goto-char (point-min))
    (search-forward "root question")
    (should (equal "root1" (get-text-property (point) 'lark-message-id)))))

(ert-deftest lark-im-test-reply-to-quote ()
  "DEFAULT-chat replies show a dim quote of their parent message."
  (with-temp-buffer
    (setq-local lark-im--messages
                '(((message_id . "m1") (msg_type . "text")
                   (sender . ((name . "alice")))
                   (content . "the original question about envhub"))
                  ((message_id . "m2") (msg_type . "text")
                   (reply_to . "m1")
                   (sender . ((name . "bob")))
                   (content . "an answer"))))
    (dolist (m lark-im--messages) (lark-im--insert-message m))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "↳ alice: the original question" text))
      ;; Quote sits between bob's header and his content.
      (should (< (string-match "bob" text)
                 (string-match "↳ alice" text)
                 (string-match "an answer" text))))
    ;; Parent outside the loaded window → generic marker, no crash.
    (erase-buffer)
    (setq-local lark-im--messages
                '(((message_id . "m3") (reply_to . "gone")
                   (msg_type . "text") (sender . ((name . "bob")))
                   (content . "orphan reply"))))
    (lark-im--insert-message (car lark-im--messages))
    (should (string-match-p "↳ (reply to an earlier message)"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))

;;;; Inline media rendering

(ert-deftest lark-im-test-scan-media-markers ()
  "Image and file markers are extracted with their keys."
  (with-temp-buffer
    (insert "hello\n"
            "![Image](img_v3_0215r_e3e90b95-6806-42dd-a9a1-ecb3e40f206h)\n"
            "some text [Media: file_v3_0015l_2121369e-a8e0-4f17-917b-a1ffb7af03hu]\n"
            "![Image](img_v3_0215s_9cfbd5df-f884-43ab-b7a8-54191cc64b2h)\n")
    (let ((found (lark-im--scan-media-markers (point-min) (point-max))))
      (should (= 3 (length found)))
      (should (equal '(file image image) (sort (mapcar #'car found) #'string<)))
      (should (member "img_v3_0215r_e3e90b95-6806-42dd-a9a1-ecb3e40f206h"
                      (mapcar #'cadr found)))
      (should (member "file_v3_0015l_2121369e-a8e0-4f17-917b-a1ffb7af03hu"
                      (mapcar #'cadr found))))))

(ert-deftest lark-im-test-insert-message-marks-media ()
  "Inserted messages carry openable media properties on their markers."
  (with-temp-buffer
    (lark-im--insert-message
     '((message_id . "om_1") (msg_type . "post") (create_time . "2026-10-04 09:00")
       (sender . ((name . "alice")))
       (content . "see pic\n![Image](img_v3_abc-123h)\n[Media: file_v3_def-456u]")))
    ;; Image marker gets key/kind/msg-id and the open keymap.
    (goto-char (point-min))
    (search-forward "![Image]")
    (let ((pos (match-beginning 0)))
      (should (equal "img_v3_abc-123h" (get-text-property pos 'lark-media-key)))
      (should (eq 'image (get-text-property pos 'lark-media-kind)))
      (should (equal "om_1" (get-text-property pos 'lark-media-msg-id)))
      (should (keymapp (get-text-property pos 'keymap))))
    ;; File marker is a link.
    (goto-char (point-min))
    (search-forward "[Media:")
    (let ((pos (match-beginning 0)))
      (should (equal "file_v3_def-456u" (get-text-property pos 'lark-media-key)))
      (should (eq 'file (get-text-property pos 'lark-media-kind)))
      (should (eq 'link (get-text-property pos 'face))))))

(ert-deftest lark-im-test-media-download-relative-output ()
  "Resource downloads run inside the cache dir with a relative --output.
The CLI rejects absolute output paths, so the process cwd carries
the destination."
  (let* ((lark-im-media-cache-directory
          (make-temp-file "lark-im-media-test" t))
         (seen-args nil) (seen-dir nil))
    (cl-letf (((symbol-function 'lark--run-command)
               (lambda (args callback &rest _)
                 (setq seen-args args
                       seen-dir default-directory)
                 (funcall callback '((data . ((saved_path . "/x/img.jpg"))))))))
      (let (got)
        (lark-im--download-resource "om_1" "img_v3_k1h" "image"
                                    (lambda (path) (setq got path)))
        (should (equal "/x/img.jpg" got))
        (should (equal (file-name-as-directory lark-im-media-cache-directory)
                       seen-dir))
        (should (equal '("im" "+messages-resources-download"
                         "--message-id" "om_1"
                         "--file-key" "img_v3_k1h"
                         "--type" "image"
                         "--output" "img_v3_k1h")
                       seen-args))))
    (delete-directory lark-im-media-cache-directory t)))

(ert-deftest lark-im-test-media-cached-lookup ()
  "Cache lookup finds a key's file regardless of extension."
  (let ((lark-im-media-cache-directory
         (make-temp-file "lark-im-media-test" t)))
    (unwind-protect
        (progn
          (should-not (lark-im--media-cached "img_v3_k2h"))
          (with-temp-file (expand-file-name
                           "img_v3_k2h.png"
                           lark-im-media-cache-directory)
            (insert "fake"))
          (should (string-suffix-p "img_v3_k2h.png"
                                   (lark-im--media-cached "img_v3_k2h"))))
      (delete-directory lark-im-media-cache-directory t))))

(ert-deftest lark-im-test-display-image-caps-longer-side ()
  "Inline images are fitted to a square box so the longer side is capped.
Regression: only width was capped, so a tall phone screenshot
rendered thousands of pixels high."
  (let ((lark-im-image-max-size 100)
        (seen nil))
    (cl-letf (((symbol-function 'create-image)
               (lambda (_path &rest args)
                 (setq seen args)
                 '(image :type jpeg))))
      (with-temp-buffer
        (insert "![Image](img_v3_k3h)")
        (lark-im--display-image (point-min) (point-max)
                                "img_v3_k3h" "/x/img.jpg")
        (should (equal 100 (plist-get (cddr seen) :max-width)))
        (should (equal 100 (plist-get (cddr seen) :max-height)))
        (should (get-text-property (point-min) 'display))))))

(ert-deftest lark-im-test-video-file-p ()
  "Video detection goes by extension, case-insensitively."
  (should (lark-im--video-file-p "/x/clip.mp4"))
  (should (lark-im--video-file-p "/x/clip.MOV"))
  (should-not (lark-im--video-file-p "/x/report.pdf"))
  (should-not (lark-im--video-file-p "/x/noext")))

(ert-deftest lark-im-test-thumbnail-does-not-shadow-media-cache ()
  "Thumbnails live apart so the KEY.* media glob never returns them."
  (let ((lark-im-media-cache-directory
         (make-temp-file "lark-im-thumb-test" t)))
    (unwind-protect
        (let ((thumb (lark-im--thumbnail-file "file_v3_k4u")))
          (make-directory (file-name-directory thumb) t)
          (with-temp-file thumb (insert "fake"))
          (should (equal thumb (lark-im--thumbnail-cached "file_v3_k4u")))
          ;; The video itself is still considered un-cached.
          (should-not (lark-im--media-cached "file_v3_k4u")))
      (delete-directory lark-im-media-cache-directory t))))

(ert-deftest lark-im-test-video-preview-renders-thumbnail ()
  "A video marker gets a thumbnail display; a non-video file stays a link."
  (let ((lark-im-media-cache-directory
         (make-temp-file "lark-im-vprev-test" t))
        (created nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'executable-find)
                   (lambda (prog &rest _) (equal prog "ffmpeg")))
                  ((symbol-function 'create-image)
                   (lambda (path &rest _)
                     (push path created) '(image :type jpeg)))
                  ;; Download hands back a video for k5, a pdf for k6.
                  ((symbol-function 'lark-im--download-resource)
                   (lambda (_id key _type callback)
                     (funcall callback
                              (if (equal key "file_v3_k5u") "/x/k5.mp4" "/x/k6.pdf"))))
                  ;; ffmpeg stub: write the thumbnail synchronously.
                  ((symbol-function 'lark-im--make-video-thumb)
                   (lambda (_video key callback)
                     (let ((thumb (lark-im--thumbnail-file key)))
                       (make-directory (file-name-directory thumb) t)
                       (with-temp-file thumb (insert "fake"))
                       (funcall callback thumb)))))
          (with-temp-buffer
            (lark-im--insert-message
             '((message_id . "om_1") (msg_type . "post")
               (create_time . "2026-10-04 10:00")
               (sender . ((name . "alice")))
               (content . "[Media: file_v3_k5u]\n[Media: file_v3_k6u]")))
            ;; Video marker got the thumbnail display…
            (goto-char (point-min))
            (search-forward "file_v3_k5u]")
            (should (get-text-property (match-beginning 0) 'display))
            (should (cl-some (lambda (p) (string-match-p "thumbs/file_v3_k5u" p))
                             created))
            ;; …the pdf marker stayed a plain link.
            (search-forward "file_v3_k6u]")
            (should-not (get-text-property (match-beginning 0) 'display))
            (should (eq 'link (get-text-property (match-beginning 0) 'face)))))
      (delete-directory lark-im-media-cache-directory t))))

(ert-deftest lark-im-test-media-cache-dir-persistent ()
  "Default media cache lives under XDG cache home, not the temp dir.
Regression: a temp-dir cache is purged by the OS, forcing media to
re-download every session."
  (let ((xdg (make-temp-file "lark-im-xdg-test" t))
        (old (getenv "XDG_CACHE_HOME"))
        (lark-im-media-cache-directory nil))
    (unwind-protect
        (progn
          (setenv "XDG_CACHE_HOME" xdg)
          ;; XDG_CACHE_HOME set → cache lives under it.
          (let ((dir (lark-im--media-cache-dir)))
            (should (string-prefix-p (file-name-as-directory xdg) dir))
            (should (file-directory-p dir)))
          ;; No XDG_CACHE_HOME → falls back to ~/.cache, never the
          ;; OS temp dir.
          (setenv "XDG_CACHE_HOME" nil)
          (should (string-prefix-p
                   (file-name-as-directory (expand-file-name "~/.cache"))
                   (expand-file-name (lark-im--media-cache-dir)))))
      (setenv "XDG_CACHE_HOME" old)
      (delete-directory xdg t))))

(ert-deftest lark-im-test-media-cache-prune-lru ()
  "Pruning deletes the oldest files first and keeps the cache under cap."
  (let ((lark-im-media-cache-directory
         (make-temp-file "lark-im-prune-test" t)))
    (unwind-protect
        (let ((old (expand-file-name "img_old.jpg" lark-im-media-cache-directory))
              (mid (expand-file-name "img_mid.jpg" lark-im-media-cache-directory))
              (new (expand-file-name "img_new.jpg" lark-im-media-cache-directory)))
          (dolist (spec `((,old . 10) (,mid . 10) (,new . 10)))
            (with-temp-file (car spec)
              (insert (make-string (cdr spec) ?x))))
          ;; Age the files: old < mid < new.
          (set-file-times old (time-subtract (current-time) 200))
          (set-file-times mid (time-subtract (current-time) 100))
          ;; Cap at 20 bytes → the 10-byte oldest file must go.
          (let ((lark-im-media-cache-max-bytes 20))
            (lark-im--media-cache-prune))
          (should-not (file-exists-p old))
          (should (file-exists-p mid))
          (should (file-exists-p new))
          ;; nil cap → no pruning.
          (let ((lark-im-media-cache-max-bytes nil))
            (lark-im--media-cache-prune))
          (should (file-exists-p new)))
      (delete-directory lark-im-media-cache-directory t))))

(ert-deftest lark-im-test-prepend-older-undisplayed-buffer ()
  "Prepending older messages into an undisplayed buffer must not error.
Regression: the async callback called `recenter'/`window-start'
against the selected window, which showed a different buffer —
\"`recenter'ing a window that does not display current-buffer\"."
  (with-temp-buffer
    (setq-local lark-im--chat-id "oc_1"
                lark-im--chat-name "Demo"
                lark-im--messages nil)
    (lark-im--insert-message
     '((message_id . "m1") (msg_type . "text")
       (create_time . "2026-10-04 10:00")
       (sender . ((name . "alice"))) (content . "newest")))
    (goto-char (point-min))
    ;; The temp buffer is NOT displayed in any window.
    (lark-im--prepend-older
     '((data . ((has_more . :false) (page_token . "")
                (messages . (((message_id . "m0") (msg_type . "text")
                              (create_time . "2026-10-04 09:00")
                              (sender . ((name . "bob")))
                              (content . "older"))))))))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "older" text))
      (should (string-match-p "newest" text)))))

(provide 'lark-im-test)
;;; lark-im-test.el ends here
