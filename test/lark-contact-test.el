;;; lark-contact-test.el --- Tests for lark-contact.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'lark-contact)

;; Keep tests hermetic: never touch the on-disk name cache.
(setq lark-contact-cache-file nil
      lark-contact--cache-loaded t)

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

(ert-deftest lark-contact-test-annotate ()
  "Annotate returns the cached name, or a tagged raw id — never fetches."
  (let ((lark-contact--user-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'lark-contact-get-user-sync)
               (lambda (&rest _) (error "sync fetch from annotate"))))
      ;; Miss: raw id carrying the ref property.
      (let ((s (lark-contact-annotate "ou_1")))
        (should (equal "ou_1" (substring-no-properties s)))
        (should (equal '("ou_1" . "open_id")
                       (get-text-property 0 'lark-contact-ref s))))
      ;; Hit: plain cached name.
      (lark-contact--cache-put "ou_1" "open_id" "Alice")
      (should (equal "Alice" (lark-contact-annotate "ou_1"))))))

(ert-deftest lark-contact-test-resolve-buffer-async ()
  "Buffer pass fetches each distinct raw id once and patches in place."
  (let ((lark-contact--user-cache (make-hash-table :test 'equal))
        (calls nil))
    (cl-letf (((symbol-function 'lark--run-command)
               (lambda (args callback &rest _)
                 (push args calls)
                 (funcall callback
                          '((data . ((user . ((name . "Bob"))))))))))
      (with-temp-buffer
        (insert "Owner: " (lark-contact-annotate "ou_2") "\n"
                "Owner: " (lark-contact-annotate "ou_2") "\n")
        (lark-contact-resolve-buffer-async (current-buffer))
        (should (= 1 (length calls)))
        (should-not (string-match-p
                     "ou_2" (buffer-substring-no-properties
                             (point-min) (point-max))))
        (should (equal "Bob" (lark-contact--cache-get "ou_2" "open_id")))))))

(ert-deftest lark-contact-test-cache-persistence-roundtrip ()
  "Names persist to disk and come back in a fresh session."
  (let* ((file (make-temp-file "lark-contact-cache" nil ".eld"))
         (lark-contact-cache-file file))
    (unwind-protect
        (progn
          ;; Session 1: resolve and flush.
          (let ((lark-contact--user-cache (make-hash-table :test 'equal))
                (lark-contact--cache-loaded t)
                (lark-contact--cache-save-timer nil))
            (lark-contact--cache-put "ou_p1" "open_id" "Alice")
            (lark-contact--cache-save))
          ;; Session 2: fresh memory, loaded lazily from disk.
          (let ((lark-contact--user-cache (make-hash-table :test 'equal))
                (lark-contact--cache-loaded nil))
            (should (equal "Alice"
                           (lark-contact--cache-get "ou_p1" "open_id")))))
      (delete-file file))))

(ert-deftest lark-contact-test-cache-persistence-ttl ()
  "Stale persisted names are dropped at load so they re-resolve."
  (let* ((file (make-temp-file "lark-contact-cache" nil ".eld"))
         (lark-contact-cache-file file))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";; test\n")
            (prin1 (list (cons "ou_old:open_id"
                               (cons "Oldname" (- (float-time) 1000)))
                         (cons "ou_new:open_id"
                               (cons "Newname" (float-time))))
                   (current-buffer)))
          (let ((lark-contact--user-cache (make-hash-table :test 'equal))
                (lark-contact--cache-loaded nil)
                (lark-contact-cache-ttl 500))
            (should-not (lark-contact--cache-get "ou_old" "open_id"))
            (should (equal "Newname"
                           (lark-contact--cache-get "ou_new" "open_id")))))
      (delete-file file))))

(provide 'lark-contact-test)
;;; lark-contact-test.el ends here
