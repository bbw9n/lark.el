;;; lark-im-test.el --- Tests for lark-im.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'lark-im)

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
      (should (search-forward "2026-01-04T09:08" nil t))
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

(ert-deftest lark-im-test-insert-message-image-labelled ()
  "An `image' message is still labelled (since the content is metadata, not body)."
  (with-temp-buffer
    (lark-im--insert-message
     '((message_id . "m2") (msg_type . "image") (create_time . "2026-05-27 20:17")
       (sender . ((name . "alice"))) (content . "{\"image_key\":\"abc\"}")))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "\\[image\\]" text)))))

(provide 'lark-im-test)
;;; lark-im-test.el ends here
