;;; lark-drive-test.el --- Tests for lark-drive.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'lark-drive)

;;;; Field extractors

(ert-deftest lark-drive-test-file-token ()
  (should (equal (lark-drive--file-token '((token . "tok1"))) "tok1"))
  (should (equal (lark-drive--file-token '((file_token . "tok2"))) "tok2"))
  (should (equal (lark-drive--file-token '((foo . "bar"))) "")))

(ert-deftest lark-drive-test-file-name ()
  (should (equal (lark-drive--file-name '((name . "report.pdf"))) "report.pdf"))
  (should (equal (lark-drive--file-name '((title . "Alt"))) "Alt"))
  (should (equal (lark-drive--file-name '((foo . "bar"))) "(unnamed)")))

(ert-deftest lark-drive-test-file-type ()
  (should (equal (lark-drive--file-type '((type . "folder"))) "folder"))
  (should (equal (lark-drive--file-type '((foo . "bar"))) "file")))

(ert-deftest lark-drive-test-folder-p ()
  (should (lark-drive--folder-p '((type . "folder"))))
  (should-not (lark-drive--folder-p '((type . "docx")))))

(ert-deftest lark-drive-test-file-url ()
  (should (equal (lark-drive--file-url '((url . "https://example.com"))) "https://example.com"))
  (should (equal (lark-drive--file-url '((foo . "bar"))) "")))

(ert-deftest lark-drive-test-modified-time-numeric-string ()
  "Modified time from a numeric string timestamp."
  (let ((result (lark-drive--file-modified-time '((modified_time . "1775742859")))))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest lark-drive-test-modified-time-number ()
  (let ((result (lark-drive--file-modified-time '((modified_time . 1775742859)))))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest lark-drive-test-modified-time-empty ()
  (should (equal (lark-drive--file-modified-time '((foo . "bar"))) "")))

(ert-deftest lark-drive-test-type-indicator ()
  (should (equal (lark-drive--type-indicator "folder") "/"))
  (should (equal (lark-drive--type-indicator "docx") " [docx]"))
  (should (equal (lark-drive--type-indicator "sheet") " [sheet]"))
  (should (equal (lark-drive--type-indicator "file") "")))

;;;; Extract files

(ert-deftest lark-drive-test-extract-files-nested ()
  (let ((data '((data . ((files . (((token . "t1") (name . "A"))
                                    ((token . "t2") (name . "B")))))))))
    (should (= (length (lark-drive--extract-files data)) 2))))

(ert-deftest lark-drive-test-extract-files-flat ()
  (let ((data '((files . (((token . "t1")))))))
    (should (= (length (lark-drive--extract-files data)) 1))))

;;;; Rendering

(ert-deftest lark-drive-test-insert-file-entry ()
  "Insert a file entry with text properties."
  (let ((file '((token . "tok1")
                (name . "report.pdf")
                (type . "file")
                (modified_time . "1775742859"))))
    (with-temp-buffer
      (lark-drive--insert-file-entry file)
      (goto-char (point-min))
      (should (search-forward "report.pdf" nil t))
      ;; Text property covers the line
      (goto-char (point-min))
      (let ((f (get-text-property (point) 'lark-drive-file)))
        (should f)
        (should (equal (alist-get 'token f) "tok1"))
        (should (equal (alist-get 'type f) "file"))))))

(ert-deftest lark-drive-test-insert-folder-entry ()
  "Folder entries get a trailing / indicator."
  (let ((file '((token . "ftok")
                (name . "Documents")
                (type . "folder")
                (modified_time . "1775742859"))))
    (with-temp-buffer
      (lark-drive--insert-file-entry file)
      (goto-char (point-min))
      (should (search-forward "Documents/" nil t)))))

(ert-deftest lark-drive-test-render-sorts-folders-first ()
  "Render sorts folders before files."
  (let ((files '(((token . "f1") (name . "Zebra.pdf") (type . "file") (modified_time . "0"))
                 ((token . "f2") (name . "Alpha") (type . "folder") (modified_time . "0") (parent_token . "root"))
                 ((token . "f3") (name . "data.csv") (type . "file") (modified_time . "0")))))
    (lark-drive--render files "" "Test")
    (let ((buf (get-buffer "*Lark Drive*")))
      (should buf)
      (with-current-buffer buf
        (goto-char (point-min))
        ;; Folder should appear before files
        (let ((folder-pos (search-forward "Alpha/" nil t))
              (file-pos (progn (goto-char (point-min))
                               (search-forward "Zebra.pdf" nil t))))
          (should folder-pos)
          (should file-pos)
          (should (< folder-pos file-pos))))
      (kill-buffer buf))))

(provide 'lark-drive-test)
;;; lark-drive-test.el ends here
