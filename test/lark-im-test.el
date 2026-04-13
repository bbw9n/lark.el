;;; lark-im-test.el --- Tests for lark-im.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

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

(ert-deftest lark-im-test-make-chat-entries ()
  (let* ((lark-contact--user-cache (make-hash-table :test 'equal))
         (chats '(((chat_id . "c1")
                    (name . "Dev Team")
                    (chat_type . "group")
                    (member_count . 10))))
         (entries (lark-im--make-chat-entries chats)))
    (should (= (length entries) 1))
    (should (equal (car (car entries)) "c1"))
    (let ((vec (cadr (car entries))))
      (should (equal (aref vec 0) "Dev Team"))
      (should (equal (aref vec 1) "group"))
      ;; vec[2] is owner (empty when no owner_id)
      (should (equal (aref vec 2) ""))
      (should (equal (aref vec 3) "10")))))

(provide 'lark-im-test)
;;; lark-im-test.el ends here
