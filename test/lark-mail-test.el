;;; lark-mail-test.el --- Tests for lark-mail.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'lark-mail)

;;;; Mail parsing

(ert-deftest lark-mail-test-mail-id ()
  (should (equal (lark-mail--mail-id '((mail_id . "m_123"))) "m_123"))
  (should (equal (lark-mail--mail-id '((message_id . "456"))) "456"))
  (should (equal (lark-mail--mail-id '((id . "789"))) "789")))

(ert-deftest lark-mail-test-mail-subject ()
  (should (equal (lark-mail--mail-subject '((subject . "Hello"))) "Hello"))
  (should (equal (lark-mail--mail-subject '((title . "Update"))) "Update"))
  (should (equal (lark-mail--mail-subject '((foo . "bar"))) "(no subject)")))

(ert-deftest lark-mail-test-mail-from-string ()
  (should (equal (lark-mail--mail-from '((from . "alice@example.com")))
                 "alice@example.com")))

(ert-deftest lark-mail-test-mail-from-alist ()
  (should (equal (lark-mail--mail-from '((from . ((name . "Alice")))))
                 "Alice"))
  (should (equal (lark-mail--mail-from '((sender . ((address . "bob@x.com")))))
                 "bob@x.com")))

(ert-deftest lark-mail-test-mail-from-nil ()
  (should (equal (lark-mail--mail-from '((foo . "bar"))) "")))

(ert-deftest lark-mail-test-mail-date-timestamp ()
  (let ((result (lark-mail--mail-date '((send_time . 1700000000)))))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest lark-mail-test-mail-date-string ()
  (should (equal (lark-mail--mail-date '((date . "2026-04-11 10:00")))
                 "2026-04-11 10:00")))

(ert-deftest lark-mail-test-mail-date-nil ()
  (should (equal (lark-mail--mail-date '((foo . "bar"))) "")))

(ert-deftest lark-mail-test-mail-read-p ()
  (should (lark-mail--mail-read-p '((is_read . t))))
  (should-not (lark-mail--mail-read-p '((is_read . :false))))
  (should-not (lark-mail--mail-read-p '((foo . "bar")))))

(ert-deftest lark-mail-test-mail-has-attachment-p ()
  (should (lark-mail--mail-has-attachment-p '((has_attachment . t))))
  (should (lark-mail--mail-has-attachment-p '((attachments . (((name . "f.pdf")))))))
  (should-not (lark-mail--mail-has-attachment-p '((foo . "bar")))))

;;;; Extract mails

(ert-deftest lark-mail-test-extract-mails-items ()
  (let ((data '((items . (((mail_id . "1")) ((mail_id . "2")))))))
    (should (= (length (lark-mail--extract-mails data)) 2))))

(ert-deftest lark-mail-test-extract-mails-nested ()
  (let ((data '((data . ((mails . (((mail_id . "1")))))))))
    (should (= (length (lark-mail--extract-mails data)) 1))))

(ert-deftest lark-mail-test-extract-mails-empty ()
  (should (null (lark-mail--extract-mails nil))))

;;;; Insert entry

(ert-deftest lark-mail-test-insert-entry ()
  "Insert a read mail entry with text properties."
  (let ((mail '((mail_id . "m1")
                (subject . "Hello")
                (from . "alice@x.com")
                (date . "2026-04-11")
                (is_read . t))))
    (with-temp-buffer
      (lark-mail--insert-entry mail)
      (goto-char (point-min))
      ;; Read flag should be space, not N
      (should (looking-at "  "))
      ;; Subject and from present
      (should (search-forward "alice@x.com" nil t))
      (goto-char (point-min))
      (should (search-forward "Hello" nil t))
      ;; Text property
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'lark-mail-id) "m1")))))

(ert-deftest lark-mail-test-insert-entry-unread ()
  "Unread mail entry should have N flag."
  (let ((mail '((mail_id . "m2")
                (subject . "Urgent")
                (from . "bob@x.com")
                (date . "2026-04-11"))))
    (with-temp-buffer
      (lark-mail--insert-entry mail)
      (goto-char (point-min))
      ;; Unread flag N
      (should (looking-at "N")))))

;;;; Format recipients

(ert-deftest lark-mail-test-format-recipients-nil ()
  (should (equal (lark-mail--format-recipients nil) "")))

(ert-deftest lark-mail-test-format-recipients-string ()
  (should (equal (lark-mail--format-recipients "alice@x.com") "alice@x.com")))

(ert-deftest lark-mail-test-format-recipients-list ()
  (should (equal (lark-mail--format-recipients
                  '(((name . "Alice")) ((address . "bob@x.com"))))
                 "Alice, bob@x.com")))

(provide 'lark-mail-test)
;;; lark-mail-test.el ends here
