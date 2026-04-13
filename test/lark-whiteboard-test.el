;;; lark-whiteboard-test.el --- Tests for lark-whiteboard.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'lark-whiteboard)

;;;; Display query — raw string content

(ert-deftest lark-whiteboard-test-display-query-string ()
  (let ((data '((data . "graph LR; A-->B"))))
    (with-temp-buffer
      (lark-whiteboard--display-query data "wb_token_1" "code")
      ;; Verify buffer was created and has content
      (let ((buf (get-buffer "*Lark Whiteboard: wb_token_1*")))
        (should buf)
        (with-current-buffer buf
          (goto-char (point-min))
          (should (search-forward "wb_token_1" nil t))
          (should (search-forward "code" nil t))
          (should (search-forward "graph LR; A-->B" nil t)))
        (kill-buffer buf)))))

;;;; Display query — alist content

(ert-deftest lark-whiteboard-test-display-query-alist ()
  (let ((data '((data . ((nodes . (((id . "n1") (type . "rect")))))))))
    (with-temp-buffer
      (lark-whiteboard--display-query data "wb_token_2" "raw")
      (let ((buf (get-buffer "*Lark Whiteboard: wb_token_2*")))
        (should buf)
        (with-current-buffer buf
          (goto-char (point-min))
          (should (search-forward "wb_token_2" nil t))
          (should (search-forward "raw" nil t)))
        (kill-buffer buf)))))

;;;; Display query — nil content

(ert-deftest lark-whiteboard-test-display-query-empty ()
  (let ((data nil))
    (with-temp-buffer
      (lark-whiteboard--display-query data "wb_empty" "raw")
      (let ((buf (get-buffer "*Lark Whiteboard: wb_empty*")))
        (should buf)
        (with-current-buffer buf
          (goto-char (point-min))
          (should (search-forward "(no content)" nil t)))
        (kill-buffer buf)))))

(provide 'lark-whiteboard-test)
;;; lark-whiteboard-test.el ends here
