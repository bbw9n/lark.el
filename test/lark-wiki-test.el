;;; lark-wiki-test.el --- Tests for lark-wiki.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'lark-wiki)

;;;; Space field extractors

(ert-deftest lark-wiki-test-space-id-of ()
  (should (equal (lark-wiki--space-id-of '((space_id . "sp1"))) "sp1"))
  (should (equal (lark-wiki--space-id-of '((foo . "bar"))) "")))

(ert-deftest lark-wiki-test-space-name ()
  (should (equal (lark-wiki--space-name '((name . "Eng KB"))) "Eng KB"))
  (should (equal (lark-wiki--space-name '((foo . "bar"))) "(unnamed space)")))

(ert-deftest lark-wiki-test-space-type ()
  (should (equal (lark-wiki--space-type '((space_type . "team"))) "team"))
  (should (equal (lark-wiki--space-type '((foo . "bar"))) "")))

;;;; Node field extractors

(ert-deftest lark-wiki-test-node-token ()
  (should (equal (lark-wiki--node-token '((node_token . "wikabc"))) "wikabc"))
  (should (equal (lark-wiki--node-token '((foo . "bar"))) "")))

(ert-deftest lark-wiki-test-obj-token-and-type ()
  (should (equal (lark-wiki--obj-token '((obj_token . "docxAAA"))) "docxAAA"))
  (should (equal (lark-wiki--obj-token '((foo . "bar"))) ""))
  (should (equal (lark-wiki--obj-type '((obj_type . "docx"))) "docx"))
  (should (equal (lark-wiki--obj-type '((foo . "bar"))) "")))

(ert-deftest lark-wiki-test-node-title ()
  (should (equal (lark-wiki--node-title '((title . "Design Doc"))) "Design Doc"))
  (should (equal (lark-wiki--node-title '((title . ""))) "(untitled)"))
  (should (equal (lark-wiki--node-title '((foo . "bar"))) "(untitled)")))

(ert-deftest lark-wiki-test-node-type ()
  (should (equal (lark-wiki--node-type '((node_type . "shortcut"))) "shortcut"))
  ;; Defaults to origin when absent.
  (should (equal (lark-wiki--node-type '((foo . "bar"))) "origin")))

(ert-deftest lark-wiki-test-has-child-p ()
  (should (lark-wiki--has-child-p '((has_child . t))))
  (should-not (lark-wiki--has-child-p '((has_child . :false))))
  (should-not (lark-wiki--has-child-p '((has_child . :json-false))))
  (should-not (lark-wiki--has-child-p '((foo . "bar")))))

(ert-deftest lark-wiki-test-node-url ()
  (should (equal (lark-wiki--node-url '((url . "https://x/wiki/abc"))) "https://x/wiki/abc"))
  (should (equal (lark-wiki--node-url '((foo . "bar"))) "")))

;;;; Response extraction

(ert-deftest lark-wiki-test-extract-spaces ()
  (should (equal (lark-wiki--extract-spaces
                  '((data . ((items . (((space_id . "s1")) ((space_id . "s2"))))))))
                 '(((space_id . "s1")) ((space_id . "s2")))))
  ;; Top-level items fallback.
  (should (equal (lark-wiki--extract-spaces '((items . (((space_id . "s3"))))))
                 '(((space_id . "s3"))))))

(ert-deftest lark-wiki-test-extract-nodes ()
  (should (equal (lark-wiki--extract-nodes
                  '((data . ((items . (((node_token . "n1"))))))))
                 '(((node_token . "n1"))))))

(ert-deftest lark-wiki-test-extract-node ()
  (should (equal (lark-wiki--extract-node
                  '((data . ((node . ((node_token . "n1") (title . "T")))))))
                 '((node_token . "n1") (title . "T")))))

;;;; Synthetic personal library

(ert-deftest lark-wiki-test-my-library-space ()
  (let ((lib (lark-wiki--my-library-space)))
    (should (equal (lark-wiki--space-id-of lib) "my_library"))
    (should (equal (lark-wiki--space-type lib) "person"))))

;;;; Dispatch is a real command

(ert-deftest lark-wiki-test-dispatch-is-command ()
  (should (commandp 'lark-wiki-dispatch))
  (should (commandp 'lark-wiki-spaces))
  (should (commandp 'lark-wiki-open)))

(provide 'lark-wiki-test)
;;; lark-wiki-test.el ends here
