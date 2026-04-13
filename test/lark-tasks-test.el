;;; lark-tasks-test.el --- Tests for lark-tasks.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'lark-tasks)

;;;; Task parsing tests

(ert-deftest lark-tasks-test-task-id ()
  "Extract task ID from various formats."
  (should (equal (lark-tasks--task-id '((task_id . "t_123")))
                 "t_123"))
  (should (equal (lark-tasks--task-id '((id . "456")))
                 "456"))
  (should (equal (lark-tasks--task-id '((guid . "g789")))
                 "g789")))

(ert-deftest lark-tasks-test-task-title ()
  "Extract task title."
  (should (equal (lark-tasks--task-title '((summary . "Fix bug")))
                 "Fix bug"))
  (should (equal (lark-tasks--task-title '((title . "Review PR")))
                 "Review PR"))
  (should (equal (lark-tasks--task-title '((name . "Deploy")))
                 "Deploy"))
  (should (equal (lark-tasks--task-title '((foo . "bar")))
                 "(no title)")))

(ert-deftest lark-tasks-test-task-status-todo ()
  "Default status is todo."
  (should (equal (lark-tasks--task-status '((summary . "x")))
                 "todo")))

(ert-deftest lark-tasks-test-task-status-done ()
  "Completed tasks return done."
  (should (equal (lark-tasks--task-status '((completed_at . "2026-01-01")))
                 "done"))
  (should (equal (lark-tasks--task-status '((is_completed . t)))
                 "done")))

(ert-deftest lark-tasks-test-task-status-explicit ()
  "Explicit status field is used."
  (should (equal (lark-tasks--task-status '((status . "in_progress")))
                 "in_progress")))

(ert-deftest lark-tasks-test-task-due-timestamp ()
  "Format numeric due timestamp."
  (let ((result (lark-tasks--task-due '((due . 1700000000)))))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest lark-tasks-test-task-due-string ()
  "Format string due date."
  (should (equal (lark-tasks--task-due '((due . "2026-04-15")))
                 "2026-04-15")))

(ert-deftest lark-tasks-test-task-due-nil ()
  "Missing due returns empty string."
  (should (equal (lark-tasks--task-due '((summary . "x")))
                 "")))

(ert-deftest lark-tasks-test-task-creator ()
  "Extract creator name."
  (should (equal (lark-tasks--task-creator '((creator_name . "Alice")))
                 "Alice"))
  (should (equal (lark-tasks--task-creator
                  '((creator . ((display_name . "Bob")))))
                 "Bob"))
  (should (equal (lark-tasks--task-creator '((foo . "bar")))
                 "")))

;;;; Extract tasks from various response shapes

(ert-deftest lark-tasks-test-extract-tasks-items ()
  "Extract tasks from {items: [...]}."
  (let ((data '((items . (((task_id . "1")) ((task_id . "2")))))))
    (should (= (length (lark-tasks--extract-tasks data)) 2))))

(ert-deftest lark-tasks-test-extract-tasks-nested ()
  "Extract tasks from {data: {tasks: [...]}}."
  (let ((data '((data . ((tasks . (((task_id . "1")))))))))
    (should (= (length (lark-tasks--extract-tasks data)) 1))))

(ert-deftest lark-tasks-test-extract-tasks-flat ()
  "Extract from flat list."
  (let ((data '(((task_id . "1") (summary . "A")))))
    (should (= (length (lark-tasks--extract-tasks data)) 1))))

(ert-deftest lark-tasks-test-extract-tasks-empty ()
  "Nil data returns nil."
  (should (null (lark-tasks--extract-tasks nil))))

;;;; Make entries

(ert-deftest lark-tasks-test-make-entries ()
  "Convert tasks to tabulated-list entries."
  (let* ((tasks '(((task_id . "t1")
                    (summary . "Fix bug")
                    (due . "2026-04-15")
                    (status . "todo")
                    (creator_name . "Alice"))))
         (entries (lark-tasks--make-entries tasks)))
    (should (= (length entries) 1))
    (should (equal (car (car entries)) "t1"))
    (let ((vec (cadr (car entries))))
      (should (equal (substring-no-properties (aref vec 0)) "todo"))
      (should (equal (aref vec 1) "Fix bug"))
      (should (equal (aref vec 2) "2026-04-15"))
      (should (equal (aref vec 3) "Alice")))))

;;;; Format date

(ert-deftest lark-tasks-test-format-date ()
  "Format date strings."
  (should (equal (lark-tasks--format-date "2026-04-15T10:00:00")
                 "2026-04-15"))
  (should (equal (lark-tasks--format-date "2026-04-15")
                 "2026-04-15"))
  (should (equal (lark-tasks--format-date nil) ""))
  (should (equal (lark-tasks--format-date "not a date") "not a date")))

(provide 'lark-tasks-test)
;;; lark-tasks-test.el ends here
