;;; lark-meetings-test.el --- Tests for lark-meetings.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'lark-meetings)

;;;; Field extractors

(ert-deftest lark-meetings-test-meeting-id ()
  (should (equal (lark-meetings--meeting-id '((id . "123"))) "123"))
  (should (equal (lark-meetings--meeting-id '((meeting_id . "456"))) "456"))
  (should (equal (lark-meetings--meeting-id '((foo . "bar"))) "")))

(ert-deftest lark-meetings-test-meeting-title ()
  "Title is first line of display_info."
  (should (equal (lark-meetings--meeting-title
                  '((display_info . "Team Standup\n\nToday 10:00")))
                 "Team Standup"))
  ;; Fallback to meta_data.description
  (should (equal (lark-meetings--meeting-title
                  '((meta_data . ((description . "Daily sync")))))
                 "Daily sync"))
  ;; Fallback to topic
  (should (equal (lark-meetings--meeting-title '((topic . "Retro")))
                 "Retro"))
  (should (equal (lark-meetings--meeting-title '((foo . "bar")))
                 "(untitled meeting)")))

(ert-deftest lark-meetings-test-meeting-description ()
  (should (equal (lark-meetings--meeting-description
                  '((meta_data . ((description . "10:00 | Org: Alice")))))
                 "10:00 | Org: Alice"))
  (should (equal (lark-meetings--meeting-description '((foo . "bar"))) "")))

(ert-deftest lark-meetings-test-meeting-app-link ()
  (should (equal (lark-meetings--meeting-app-link
                  '((meta_data . ((app_link . "https://example.com/meeting")))))
                 "https://example.com/meeting"))
  (should (equal (lark-meetings--meeting-app-link '((foo . "bar"))) "")))

;;;; Extraction

(ert-deftest lark-meetings-test-extract-meetings ()
  (let ((data '((data . ((items . (((id . "1")) ((id . "2")))))))))
    (should (= (length (lark-meetings--extract-meetings data)) 2))))

(ert-deftest lark-meetings-test-extract-meetings-flat ()
  (let ((data '((items . (((id . "1")))))))
    (should (= (length (lark-meetings--extract-meetings data)) 1))))

(ert-deftest lark-meetings-test-extract-meetings-nil ()
  (should (null (lark-meetings--extract-meetings nil))))

(ert-deftest lark-meetings-test-extract-notes ()
  (let ((data '((data . ((notes . (((meeting_id . "1") (summary . "hi")))))))))
    (should (= (length (lark-meetings--extract-notes data)) 1))))

;;;; Insert meeting

(ert-deftest lark-meetings-test-insert-meeting ()
  (let ((meeting '((id . "7627741315669528286")
                   (display_info . "Team Standup\n\nToday 14:24 | Org: Alice")
                   (meta_data . ((description . "Today 14:24 | Org: Alice")
                                 (app_link . "https://example.com/m/123"))))))
    (with-temp-buffer
      (lark-meetings--insert-meeting meeting)
      (goto-char (point-min))
      (should (search-forward "Team Standup" nil t))
      (should (search-forward "Today 14:24" nil t))
      (should (search-forward "7627741315669528286" nil t))
      ;; Text properties
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'lark-meeting-id) "7627741315669528286"))
      (should (lark-meetings--meeting-at-point)))))

;;;; Time helpers

(ert-deftest lark-meetings-test-default-times ()
  "Default start/end are formatted dates."
  (let ((start (lark-meetings--default-start))
        (end (lark-meetings--default-end)))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" start))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" end))
    ;; end should be after start
    (should (string> end start))))

(provide 'lark-meetings-test)
;;; lark-meetings-test.el ends here
