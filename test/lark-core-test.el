;;; lark-core-test.el --- Tests for lark-core.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; ERT tests for lark.el core functions.

;;; Code:

(require 'ert)

;; Add project directories to load-path
(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'lark-core)

;;;; lark--format-args tests

(ert-deftest lark-test-format-args-empty ()
  "Empty plist produces empty args."
  (should (equal (lark--format-args nil) nil)))

(ert-deftest lark-test-format-args-key-value ()
  "Key-value pair produces --key value."
  (should (equal (lark--format-args '(:page-size 20))
                 '("--page-size" "20"))))

(ert-deftest lark-test-format-args-boolean ()
  "Boolean t produces a flag with no value."
  (should (equal (lark--format-args '(:verbose t))
                 '("--verbose"))))

(ert-deftest lark-test-format-args-nil-skipped ()
  "Nil values are omitted entirely."
  (should (equal (lark--format-args '(:foo nil :bar "baz"))
                 '("--bar" "baz"))))

(ert-deftest lark-test-format-args-list ()
  "List values produce repeated flags."
  (should (equal (lark--format-args '(:fields ("a" "b")))
                 '("--fields" "a" "--fields" "b"))))

(ert-deftest lark-test-format-args-string-value ()
  "String values are passed through."
  (should (equal (lark--format-args '(:title "My Event"))
                 '("--title" "My Event"))))

(ert-deftest lark-test-format-args-mixed ()
  "Mixed argument types work together."
  (should (equal (lark--format-args '(:page-size 20 :verbose t :skip nil))
                 '("--page-size" "20" "--verbose"))))

;;;; lark--parse-json tests

(ert-deftest lark-test-parse-json-object ()
  "Parse a JSON object to alist."
  (let ((result (lark--parse-json "{\"name\": \"test\", \"count\": 42}")))
    (should (equal (alist-get 'name result) "test"))
    (should (equal (alist-get 'count result) 42))))

(ert-deftest lark-test-parse-json-array ()
  "Parse a JSON array."
  (let ((result (lark--parse-json "[1, 2, 3]")))
    (should (equal result '(1 2 3)))))

(ert-deftest lark-test-parse-json-empty-string ()
  "Empty string returns nil."
  (should (null (lark--parse-json "")))
  (should (null (lark--parse-json "  ")))
  (should (null (lark--parse-json nil))))

(ert-deftest lark-test-parse-json-invalid ()
  "Invalid JSON returns nil without error."
  (should (null (lark--parse-json "not json"))))

;;;; lark--strip-debug-lines tests

(ert-deftest lark-test-strip-debug-lines-removes-prefixed ()
  "Strip lines with [cmd +sub] debug prefixes."
  (let ((input "[vc +recording] fetching recording...\n[vc +recording] done\n{\"data\": 1}"))
    (should (equal (string-trim (lark--strip-debug-lines input))
                   "{\"data\": 1}"))))

(ert-deftest lark-test-strip-debug-lines-preserves-json-array ()
  "JSON arrays starting with [ are not stripped."
  (let ((input "[1, 2, 3]"))
    (should (equal (lark--strip-debug-lines input) input))))

(ert-deftest lark-test-strip-debug-lines-no-debug ()
  "Plain JSON passes through unchanged."
  (let ((input "{\"key\": \"value\"}"))
    (should (equal (lark--strip-debug-lines input) input))))

(ert-deftest lark-test-parse-json-with-debug-lines ()
  "parse-json strips debug lines and parses the JSON."
  (let ((result (lark--parse-json
                 "[vc +recording] querying...\n{\"data\": {\"id\": \"abc\"}}")))
    (should result)
    (should (equal (alist-get 'id (alist-get 'data result)) "abc"))))

(ert-deftest lark-test-parse-json-only-debug-lines ()
  "parse-json returns nil when input is only debug lines."
  (should (null (lark--parse-json
                 "[vc +recording] querying...\n[vc +recording] done\n"))))

(ert-deftest lark-test-strip-debug-lines-removes-tip ()
  "Strip tip lines from CLI output."
  (let ((input "tip: use --verbose for more details\n{\"data\": 1}"))
    (should (equal (string-trim (lark--strip-debug-lines input))
                   "{\"data\": 1}"))))

(ert-deftest lark-test-strip-debug-lines-tip-no-colon ()
  "Strip tip lines without colon separator."
  (let ((input "tip something helpful\n{\"count\": 5}"))
    (should (equal (string-trim (lark--strip-debug-lines input))
                   "{\"count\": 5}"))))

(ert-deftest lark-test-parse-json-with-tip-line ()
  "parse-json strips tip lines and parses the JSON."
  (let ((result (lark--parse-json
                 "tip: login required\n{\"items\": [1, 2]}")))
    (should result)
    (should (equal (alist-get 'items result) '(1 2)))))

(ert-deftest lark-test-strip-debug-lines-preserves-title-field ()
  "JSON containing a key like `title' is not stripped."
  (let ((input "{\"title\": \"tips for success\"}"))
    (should (equal (lark--strip-debug-lines input) input))))

;;;; lark--parse-ndjson tests

(ert-deftest lark-test-parse-ndjson ()
  "Parse newline-delimited JSON."
  (let ((result (lark--parse-ndjson "{\"a\":1}\n{\"b\":2}\n")))
    (should (= (length result) 2))
    (should (equal (alist-get 'a (car result)) 1))
    (should (equal (alist-get 'b (cadr result)) 2))))

(ert-deftest lark-test-parse-ndjson-empty ()
  "Empty NDJSON returns nil."
  (should (null (lark--parse-ndjson ""))))

;;;; lark--handle-error tests

(ert-deftest lark-test-handle-error-success ()
  "Response with code 0 passes through."
  (let ((resp '((code . 0) (data . "ok"))))
    (should (equal (lark--handle-error resp) resp))))

(ert-deftest lark-test-handle-error-nil ()
  "Nil response passes through."
  (should (null (lark--handle-error nil))))

(ert-deftest lark-test-handle-error-signals ()
  "Non-zero code signals user-error."
  (should-error
   (lark--handle-error '((code . 403) (msg . "forbidden")))
   :type 'user-error))

;;;; lark--get-nested tests

(ert-deftest lark-test-get-nested ()
  "Navigate nested alist structure."
  (let ((data '((items . (((name . "first")) ((name . "second")))))))
    (should (equal (lark--get-nested data 'items 0 'name) "first"))
    (should (equal (lark--get-nested data 'items 1 'name) "second"))))

(ert-deftest lark-test-get-nested-missing ()
  "Missing keys return nil."
  (let ((data '((foo . "bar"))))
    (should (null (lark--get-nested data 'missing 'key)))))

;;;; lark--format-timestamp tests

(ert-deftest lark-test-format-timestamp-number ()
  "Format a numeric timestamp."
  (let ((result (lark--format-timestamp 1700000000)))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" result))))

(ert-deftest lark-test-format-timestamp-string ()
  "Format a string timestamp."
  (let ((result (lark--format-timestamp "1700000000")))
    (should (stringp result))))

(ert-deftest lark-test-format-timestamp-nil ()
  "Nil timestamp returns nil."
  (should (null (lark--format-timestamp nil))))

(ert-deftest lark-test-format-timestamp-zero ()
  "Zero timestamp returns nil."
  (should (null (lark--format-timestamp 0))))

;;;; lark--build-command tests

(ert-deftest lark-test-build-command-basic ()
  "Basic command building — no --format or --as added by default."
  (let ((lark-dry-run nil))
    (let ((result (lark--build-command '("calendar" "+agenda") nil)))
      (should (equal (car result) "calendar"))
      (should (equal (cadr result) "+agenda"))
      (should-not (member "--format" result))
      (should-not (member "--as" result)))))

(ert-deftest lark-test-build-command-with-dry-run ()
  "Dry run flag is included when enabled."
  (let ((lark-dry-run t))
    (let ((result (lark--build-command '("tasks" "create") nil)))
      (should (member "--dry-run" result)))))

(ert-deftest lark-test-build-command-format-override ()
  "Explicit format adds --format flag."
  (let ((lark-dry-run nil))
    (let ((result (lark--build-command '("mail" "+triage") nil "json")))
      (should (member "--format" result))
      (should (member "json" result)))))

(ert-deftest lark-test-build-command-no-format-without-override ()
  "No format arg means no --format flag."
  (let ((lark-dry-run nil))
    (let ((result (lark--build-command '("calendar" "+create") nil)))
      (should-not (member "--format" result)))))

;;;; lark--parse-time-input tests

(ert-deftest lark-test-parse-time-hhmm ()
  "HH:MM expands to today's date in ISO 8601."
  (let ((result (lark--parse-time-input "10:00")))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T10:00:00" result))
    ;; Should include timezone offset
    (should (string-match-p "[+-][0-9]\\{2\\}:[0-9]\\{2\\}$" result))))

(ert-deftest lark-test-parse-time-mmdd-hhmm ()
  "MM-DD HH:MM expands to this year in ISO 8601."
  (let ((result (lark--parse-time-input "04-15 14:30")))
    (should (string-match-p "^[0-9]\\{4\\}-04-15T14:30:00" result))))

(ert-deftest lark-test-parse-time-full ()
  "YYYY-MM-DD HH:MM expands to ISO 8601."
  (let ((result (lark--parse-time-input "2026-04-15 14:30")))
    (should (string-match-p "^2026-04-15T14:30:00" result))))

(ert-deftest lark-test-parse-time-iso8601-passthrough ()
  "Already ISO 8601 is passed through."
  (should (equal (lark--parse-time-input "2026-04-15T14:30:00+08:00")
                 "2026-04-15T14:30:00+08:00")))

(ert-deftest lark-test-parse-time-unknown-passthrough ()
  "Unrecognized input is passed through."
  (should (equal (lark--parse-time-input "tomorrow") "tomorrow")))

(provide 'lark-core-test)
;;; lark-core-test.el ends here
