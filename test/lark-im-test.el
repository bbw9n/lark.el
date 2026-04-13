;;; lark-im-test.el --- Tests for lark-im.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "domain" "ai"))
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

(provide 'lark-im-test)
;;; lark-im-test.el ends here
