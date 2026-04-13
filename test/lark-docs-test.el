;;; lark-docs-test.el --- Tests for lark-docs.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'lark-docs)

;;;; Field extractors

(ert-deftest lark-docs-test-doc-title ()
  (should (equal (lark-docs--doc-title '((title . "My Doc"))) "My Doc"))
  (should (equal (lark-docs--doc-title '((name . "Alt"))) "Alt"))
  (should (equal (lark-docs--doc-title '((foo . "bar"))) "(untitled)")))

(ert-deftest lark-docs-test-doc-token ()
  (should (equal (lark-docs--doc-token '((document_id . "d1"))) "d1"))
  (should (equal (lark-docs--doc-token '((doc_token . "d2"))) "d2"))
  (should (equal (lark-docs--doc-token '((token . "d3"))) "d3"))
  (should (equal (lark-docs--doc-token '((obj_token . "d4"))) "d4")))

(ert-deftest lark-docs-test-doc-type ()
  (should (equal (lark-docs--doc-type '((doc_type . "docx"))) "docx"))
  (should (equal (lark-docs--doc-type '((type . "sheet"))) "sheet"))
  (should (equal (lark-docs--doc-type '((foo . "bar"))) "")))

(ert-deftest lark-docs-test-doc-url ()
  (should (equal (lark-docs--doc-url '((url . "https://example.com"))) "https://example.com"))
  (should (equal (lark-docs--doc-url '((foo . "bar"))) "")))

(ert-deftest lark-docs-test-doc-owner ()
  (should (equal (lark-docs--doc-owner '((owner_name . "Alice"))) "Alice"))
  (should (equal (lark-docs--doc-owner '((owner . ((name . "Bob"))))) "Bob"))
  (should (equal (lark-docs--doc-owner '((owner_id . "ou_1"))) "ou_1"))
  (should (equal (lark-docs--doc-owner '((foo . "bar"))) "")))

(ert-deftest lark-docs-test-doc-create-time-string ()
  (should (equal (lark-docs--doc-create-time '((create_time . "2026-01-04T09:08:05Z")))
                 "2026-01-04T09:08")))

(ert-deftest lark-docs-test-doc-create-time-number ()
  (let ((result (lark-docs--doc-create-time '((create_time . 1700000000)))))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest lark-docs-test-doc-create-time-empty ()
  (should (equal (lark-docs--doc-create-time '((foo . "bar"))) "")))

(ert-deftest lark-docs-test-doc-update-time ()
  (should (equal (lark-docs--doc-update-time '((update_time . "2026-03-15T14:30:00Z")))
                 "2026-03-15T14:30")))

;;;; Extraction

(ert-deftest lark-docs-test-extract-doc ()
  (let ((data '((data . ((title . "Hello") (document_id . "d1"))))))
    (should (equal (alist-get 'title (lark-docs--extract-doc data)) "Hello"))))

(ert-deftest lark-docs-test-extract-search-results-nested ()
  (let ((data '((data . ((items . (((title . "A")) ((title . "B")))))))))
    (should (= (length (lark-docs--extract-search-results data)) 2))))

(ert-deftest lark-docs-test-extract-search-results-flat ()
  (let ((data '((items . (((title . "A")))))))
    (should (= (length (lark-docs--extract-search-results data)) 1))))

;;;; Insert result

(ert-deftest lark-docs-test-insert-result ()
  (let ((doc '((title . "My Document")
               (doc_type . "docx")
               (document_id . "doxbc12345")
               (url . "https://lark.example.com/doc/doxbc12345")
               (create_time . "2026-01-04T09:08:05Z"))))
    (with-temp-buffer
      (lark-docs--insert-result doc)
      (goto-char (point-min))
      (should (search-forward "My Document" nil t))
      (should (search-forward "docx" nil t))
      (should (search-forward "doxbc12345" nil t))
      (should (search-forward "2026-01-04T09:08" nil t))
      ;; Text properties
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'lark-doc-token) "doxbc12345"))
      (should (equal (get-text-property (point) 'lark-doc-title) "My Document")))))

(provide 'lark-docs-test)
;;; lark-docs-test.el ends here
