;;; lark-docs-test.el --- Tests for lark-docs.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'lark-docs)

;;;; Field extractors

(ert-deftest lark-docs-test-doc-title ()
  (should (equal (lark-docs--doc-title '((title . "My Doc"))) "My Doc"))
  (should (equal (lark-docs--doc-title '((name . "Alt"))) "Alt"))
  (should (equal (lark-docs--doc-title '((foo . "bar"))) "(untitled)"))
  ;; search result with highlight tags
  (should (equal (lark-docs--doc-title '((title_highlighted . "My <h>Doc</h>")))
                 "My Doc")))

(ert-deftest lark-docs-test-doc-token ()
  (should (equal (lark-docs--doc-token '((document_id . "d1"))) "d1"))
  (should (equal (lark-docs--doc-token '((doc_token . "d2"))) "d2"))
  ;; search result with token in result_meta
  (should (equal (lark-docs--doc-token '((result_meta . ((token . "d3"))))) "d3"))
  (should (equal (lark-docs--doc-token '((obj_token . "d4"))) "d4")))

(ert-deftest lark-docs-test-doc-type ()
  ;; search result: entity_type at top, doc_types in meta
  (should (equal (lark-docs--doc-type '((entity_type . "WIKI"))) "WIKI"))
  (should (equal (lark-docs--doc-type
                  '((result_meta . ((doc_types . "DOCX")))))
                 "DOCX"))
  (should (equal (lark-docs--doc-type '((foo . "bar"))) "")))

(ert-deftest lark-docs-test-doc-url ()
  ;; search result: url in result_meta
  (should (equal (lark-docs--doc-url
                  '((result_meta . ((url . "https://example.com")))))
                 "https://example.com"))
  (should (equal (lark-docs--doc-url '((url . "https://direct.com")))
                 "https://direct.com"))
  (should (equal (lark-docs--doc-url '((foo . "bar"))) "")))

(ert-deftest lark-docs-test-doc-owner ()
  ;; search result: owner_name in result_meta
  (should (equal (lark-docs--doc-owner
                  '((result_meta . ((owner_name . "Alice")))))
                 "Alice"))
  (should (equal (lark-docs--doc-owner '((owner_name . "Bob"))) "Bob"))
  (should (equal (lark-docs--doc-owner '((foo . "bar"))) "")))

(ert-deftest lark-docs-test-doc-create-time-iso ()
  ;; search result: create_time_iso in result_meta
  (should (equal (lark-docs--doc-create-time
                  '((result_meta . ((create_time_iso . "2025-10-28T19:29:32+08:00")))))
                 "2025-10-28T19:29")))

(ert-deftest lark-docs-test-doc-create-time-number ()
  (let ((result (lark-docs--doc-create-time '((create_time . 1700000000)))))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest lark-docs-test-doc-create-time-empty ()
  (should (equal (lark-docs--doc-create-time '((foo . "bar"))) "")))

(ert-deftest lark-docs-test-doc-update-time-iso ()
  (should (equal (lark-docs--doc-update-time
                  '((result_meta . ((update_time_iso . "2026-04-08T14:28:42+08:00")))))
                 "2026-04-08T14:28")))

;;;; Extraction

(ert-deftest lark-docs-test-extract-doc ()
  (let ((data '((data . ((title . "Hello") (document_id . "d1"))))))
    (should (equal (alist-get 'title (lark-docs--extract-doc data)) "Hello"))))

(ert-deftest lark-docs-test-extract-search-results-nested ()
  (let ((data '((data . ((results . (((title_highlighted . "A"))
                                      ((title_highlighted . "B"))))
                          (has_more . t)
                          (total . 2))))))
    (should (= (length (lark-docs--extract-search-results data)) 2))))

(ert-deftest lark-docs-test-extract-search-results-flat ()
  (let ((data '((results . (((title_highlighted . "A")))))))
    (should (= (length (lark-docs--extract-search-results data)) 1))))

(ert-deftest lark-docs-test-extract-search-results-items-fallback ()
  (let ((data '((data . ((items . (((title . "A")))))))))
    (should (= (length (lark-docs--extract-search-results data)) 1))))

;;;; Insert result

(ert-deftest lark-docs-test-insert-result ()
  "Test insertion of a search result with result_meta nesting."
  (let ((doc '((entity_type . "WIKI")
               (result_meta . ((token . "SrhtwtsCYi")
                               (owner_name . "Alice")
                               (create_time_iso . "2025-10-28T19:29:32+08:00")
                               (update_time_iso . "2026-04-08T14:28:42+08:00")
                               (url . "https://lark.example.com/wiki/SrhtwtsCYi")))
               (title_highlighted . "My <h>Document</h>")
               (summary_highlighted . "some <h>summary</h> text"))))
    (with-temp-buffer
      (lark-docs--insert-result doc)
      (goto-char (point-min))
      (should (search-forward "My Document" nil t))
      (should (search-forward "WIKI" nil t))
      (should (search-forward "SrhtwtsCYi" nil t))
      (should (search-forward "Alice" nil t))
      (should (search-forward "2025-10-28T19:29" nil t))
      (should (search-forward "some summary text" nil t))
      ;; Text properties
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'lark-doc-token) "SrhtwtsCYi"))
      (should (equal (get-text-property (point) 'lark-doc-title) "My Document")))))

(provide 'lark-docs-test)
;;; lark-docs-test.el ends here
