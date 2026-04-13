;;; lark-contact-test.el --- Tests for lark-contact.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Code:

(require 'ert)

(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'lark-contact)

;;;; User extraction

(ert-deftest lark-contact-test-extract-user-nested ()
  (let ((data '((data . ((user . ((name . "Alice") (open_id . "ou_1"))))))))
    (should (equal (alist-get 'name (lark-contact--extract-user data)) "Alice"))))

(ert-deftest lark-contact-test-extract-user-flat ()
  (let ((data '((user . ((name . "Bob"))))))
    (should (equal (alist-get 'name (lark-contact--extract-user data)) "Bob"))))

(ert-deftest lark-contact-test-extract-user-data-only ()
  (let ((data '((data . ((name . "Carol") (open_id . "ou_3"))))))
    (should (equal (alist-get 'name (lark-contact--extract-user data)) "Carol"))))

;;;; Display name extraction

(ert-deftest lark-contact-test-display-name ()
  (should (equal (lark-contact--user-display-name '((name . "Alice"))) "Alice"))
  (should (equal (lark-contact--user-display-name '((display_name . "Bob"))) "Bob"))
  (should (equal (lark-contact--user-display-name '((en_name . "Carol"))) "Carol"))
  (should (equal (lark-contact--user-display-name '((foo . "bar"))) "")))

;;;; Cache

(ert-deftest lark-contact-test-cache-roundtrip ()
  (let ((lark-contact--user-cache (make-hash-table :test 'equal)))
    (should-not (lark-contact--cache-get "ou_1" "open_id"))
    (lark-contact--cache-put "ou_1" "open_id" "Alice")
    (should (equal (lark-contact--cache-get "ou_1" "open_id") "Alice"))))

(ert-deftest lark-contact-test-cache-key-includes-type ()
  (let ((lark-contact--user-cache (make-hash-table :test 'equal)))
    (lark-contact--cache-put "id_1" "open_id" "Alice")
    (lark-contact--cache-put "id_1" "user_id" "Bob")
    (should (equal (lark-contact--cache-get "id_1" "open_id") "Alice"))
    (should (equal (lark-contact--cache-get "id_1" "user_id") "Bob"))))

(ert-deftest lark-contact-test-clear-cache ()
  (let ((lark-contact--user-cache (make-hash-table :test 'equal)))
    (lark-contact--cache-put "ou_1" "open_id" "Alice")
    (clrhash lark-contact--user-cache)
    (should-not (lark-contact--cache-get "ou_1" "open_id"))))

;;;; User entries

(ert-deftest lark-contact-test-extract-users-items ()
  (let ((data '((data . ((items . (((open_id . "ou_1")) ((open_id . "ou_2")))))))))
    (should (= (length (lark-contact--extract-users data)) 2))))

(ert-deftest lark-contact-test-extract-users-flat ()
  (let ((data '((items . (((open_id . "ou_1")))))))
    (should (= (length (lark-contact--extract-users data)) 1))))

(ert-deftest lark-contact-test-make-user-entries ()
  (let* ((users '(((open_id . "ou_1")
                    (name . "Alice")
                    (en_name . "Alice L")
                    (email . "alice@example.com"))))
         (entries (lark-contact--make-user-entries users)))
    (should (= (length entries) 1))
    (should (equal (car (car entries)) "ou_1"))
    (let ((vec (cadr (car entries))))
      (should (equal (aref vec 0) "Alice"))
      (should (equal (aref vec 1) "Alice L"))
      (should (equal (aref vec 2) "alice@example.com"))
      (should (equal (aref vec 3) "ou_1")))))

(provide 'lark-contact-test)
;;; lark-contact-test.el ends here
