;;; lark-tasks.el --- Lark Tasks integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Provides Task domain commands for lark.el: task listing, detail
;; view, create/update/complete tasks, subtask management, and task
;; list (tasklist) management.
;;
;; CLI command mapping:
;;   My tasks:     task +get-my-tasks [--query X] [--complete]
;;   Create:       task +create --summary X [--description X] [--due X] [--assignee X]
;;   Update:       task +update --task-id X [--summary X] [--description X] [--due X]
;;   Complete:     task +complete --task-id X
;;   Get detail:   task tasks get --params '{"task_id":"X"}'
;;   Delete:       task tasks delete --params '{"task_id":"X"}'
;;   Tasklists:    task tasklists list

;;; Code:

(require 'lark-core)
(require 'json)
(require 'transient)

;;;; Customization

(defgroup lark-tasks nil
  "Lark Tasks settings."
  :group 'lark
  :prefix "lark-tasks-")

;;;; Buffer-local variables

(defvar-local lark-tasks--items nil
  "Cached task data for the current buffer.")

(defvar-local lark-tasks--tasklist-id nil
  "Tasklist ID for the current buffer, or nil for all tasks.")

;;;; Task parsing

(defun lark-tasks--task-id (task)
  "Extract the task ID from TASK."
  (or (alist-get 'task_id task)
      (alist-get 'id task)
      (alist-get 'guid task)))

(defun lark-tasks--task-title (task)
  "Extract the title from TASK."
  (or (alist-get 'summary task)
      (alist-get 'title task)
      (alist-get 'name task)
      "(no title)"))

(defun lark-tasks--task-due (task)
  "Extract the due date from TASK as a display string."
  (let ((due (or (alist-get 'due task)
                 (lark--get-nested task 'due 'timestamp)
                 (lark--get-nested task 'due 'date))))
    (cond
     ((numberp due) (or (lark--format-timestamp due) ""))
     ((stringp due) (lark-tasks--format-date due))
     (t ""))))

(defun lark-tasks--format-date (str)
  "Format a date STR for display."
  (if (and str (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" str))
      (substring str 0 (min 10 (length str)))
    (or str "")))

(defun lark-tasks--task-status (task)
  "Extract the completion status from TASK."
  (let ((completed (or (alist-get 'completed_at task)
                       (alist-get 'completed task)
                       (alist-get 'is_completed task))))
    (cond
     ((and completed (not (equal completed :false)) (not (eq completed 0))
           (not (equal completed "")))
      "done")
     (t
      (or (alist-get 'status task) "todo")))))

(defun lark-tasks--task-creator (task)
  "Extract the creator name from TASK."
  (or (alist-get 'creator_name task)
      (lark--get-nested task 'creator 'display_name)
      (lark--get-nested task 'creator 'name)
      ""))

(defun lark-tasks--extract-tasks (data)
  "Extract the task list from lark-cli response DATA."
  (cond
   ((and (listp data) (alist-get 'items data))
    (alist-get 'items data))
   ((and (listp data) (alist-get 'tasks data))
    (alist-get 'tasks data))
   ((and (listp data) (alist-get 'data data))
    (let ((inner (alist-get 'data data)))
      (or (alist-get 'items inner)
          (alist-get 'tasks inner)
          (and (listp inner) inner))))
   ((and (listp data) (listp (car data))
         (or (alist-get 'task_id (car data))
             (alist-get 'summary (car data))))
    data)
   (t nil)))

(defun lark-tasks--make-entries (tasks)
  "Convert TASKS to `tabulated-list-entries' format."
  (mapcar
   (lambda (task)
     (let ((id (lark-tasks--task-id task))
           (title (lark-tasks--task-title task))
           (due (lark-tasks--task-due task))
           (status (lark-tasks--task-status task))
           (creator (lark-tasks--task-creator task)))
       (list id (vector
                 (propertize status 'face
                             (if (equal status "done")
                                 'success
                               'default))
                 title due creator))))
   tasks))

;;;; Tabulated list mode

(defvar lark-tasks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-tasks-open)
    (define-key map (kbd "g")   #'lark-tasks-refresh)
    (define-key map (kbd "c")   #'lark-tasks-create)
    (define-key map (kbd "e")   #'lark-tasks-edit)
    (define-key map (kbd "x")   #'lark-tasks-complete)
    (define-key map (kbd "d")   #'lark-tasks-delete)
    (define-key map (kbd "y")   #'lark-tasks-copy-id)
    (define-key map (kbd "s")   #'lark-tasks-subtask-create)
    (define-key map (kbd "?")   #'lark-tasks-dispatch)
    map)
  "Keymap for `lark-tasks-mode'.")

(define-derived-mode lark-tasks-mode tabulated-list-mode
  "Lark Tasks"
  "Major mode for browsing Lark tasks."
  (setq tabulated-list-format
        [("Status" 8 t)
         ("Title" 45 t)
         ("Due" 12 t)
         ("Creator" 15 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;; Task listing
;; CLI: task +get-my-tasks [--query X] [--complete]


;;;###autoload
(defun lark-tasks-list ()
  "List Lark tasks assigned to me."
  (interactive)
  (message "Lark: fetching tasks...")
  (lark--run-command
   '("task" "+get-my-tasks")
   (lambda (data)
     (lark-tasks--display-list data nil))))

(defun lark-tasks-refresh ()
  "Refresh the current task list buffer."
  (interactive)
  (lark-tasks-list))

(defun lark-tasks--display-list (data &optional tasklist-id)
  "Display task list DATA for TASKLIST-ID."
  (let* ((tasks (lark-tasks--extract-tasks data))
         (buf (get-buffer-create (if tasklist-id
                                     (format "*Lark Tasks: %s*" tasklist-id)
                                   "*Lark Tasks*"))))
    (with-current-buffer buf
      (lark-tasks-mode)
      (setq lark-tasks--items tasks
            lark-tasks--tasklist-id tasklist-id
            tabulated-list-entries (lark-tasks--make-entries tasks))
      (tabulated-list-print t)
      (setq header-line-format
            (format " Lark Tasks — %d task(s)" (length tasks))))
    (pop-to-buffer buf)))

;;;; Task detail
;; CLI: task tasks get --params '{"task_id":"X"}'


(defun lark-tasks-open ()
  "Open the task at point in a detail view."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No task at point"))
    (message "Lark: fetching task details...")
    (let ((params (json-encode `((task_id . ,id)))))
      (lark--run-command
       (list "task" "tasks" "get" "--params" params)
       (lambda (data)
         (lark-tasks--display-detail data id))))))

(defun lark-tasks--display-detail (data task-id)
  "Display task detail DATA for TASK-ID."
  (let* ((task (or (alist-get 'data data) data))
         (title (lark-tasks--task-title task))
         (buf (get-buffer-create (format "*Lark Task: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize title 'face 'bold) "\n"
                (make-string (min 60 (max 20 (length title))) ?─) "\n\n")
        ;; Status
        (insert (propertize "Status: " 'face 'bold)
                (lark-tasks--task-status task) "\n")
        ;; Due date
        (let ((due (lark-tasks--task-due task)))
          (unless (string-empty-p due)
            (insert (propertize "Due: " 'face 'bold) due "\n")))
        ;; Creator
        (let ((creator (lark-tasks--task-creator task)))
          (unless (string-empty-p creator)
            (insert (propertize "Creator: " 'face 'bold) creator "\n")))
        ;; Assignees
        (let ((members (or (alist-get 'members task)
                           (alist-get 'assignees task))))
          (when members
            (insert (propertize "Assignees: " 'face 'bold)
                    (mapconcat
                     (lambda (m)
                       (or (alist-get 'display_name m)
                           (alist-get 'name m) "?"))
                     members ", ")
                    "\n")))
        ;; Description
        (let ((desc (or (alist-get 'description task)
                        (alist-get 'notes task) "")))
          (unless (string-empty-p desc)
            (insert "\n" (propertize "Description" 'face 'bold) "\n"
                    (make-string 40 ?─) "\n"
                    desc "\n")))
        ;; Subtasks
        (let ((subtasks (or (alist-get 'subtasks task)
                            (alist-get 'children task))))
          (when subtasks
            (insert "\n" (propertize "Subtasks" 'face 'bold) "\n"
                    (make-string 40 ?─) "\n")
            (dolist (st subtasks)
              (let ((st-title (lark-tasks--task-title st))
                    (st-status (lark-tasks--task-status st)))
                (insert (if (equal st-status "done") "  [x] " "  [ ] ")
                        st-title "\n")))))
        ;; Task ID
        (insert "\n" (propertize "Task ID: " 'face 'font-lock-comment-face)
                (or task-id "") "\n"))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;; Task creation
;; CLI: task +create --summary X [--description X] [--due X] [--assignee X]
;;                   [--tasklist-id X]


;;;###autoload (autoload 'lark-tasks-create "lark-tasks" nil t)
(transient-define-prefix lark-tasks-create ()
  "Create a new Lark task."
  ["Task Details"
   ("t" "Title"       "--summary=" :prompt "Task title: ")
   ("d" "Due date"    "--due=" :prompt "Due (YYYY-MM-DD or ISO 8601): ")
   ("n" "Description" "--description=" :prompt "Description: ")]
  ["Assignment"
   ("a" "Assignee"    "--assignee=" :prompt "Assignee open_id (ou_): ")
   ("l" "Tasklist"    "--tasklist-id=" :prompt "Tasklist ID: ")]
  ["Actions"
   ("RET" "Create"    lark-tasks--do-create)
   ("q"   "Cancel"    transient-quit-all)])

(defun lark-tasks--do-create (&rest _args)
  "Execute task creation with transient arguments."
  (interactive)
  (let ((args (transient-args 'lark-tasks-create)))
    (unless args
      (user-error "No task details provided"))
    (message "Lark: creating task...")
    (lark--run-command
     (append '("task" "+create") args)
     (lambda (data)
       (let ((id (or (lark--get-nested data 'data 'task_id)
                     (alist-get 'task_id data)
                     (alist-get 'id data))))
         (message "Lark: task created%s"
                  (if id (format " (ID: %s)" id) "")))))))

;;;; Task update
;; CLI: task +update --task-id X [--summary X] [--description X] [--due X]


(defun lark-tasks-edit ()
  "Edit the task at point (update title)."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No task at point"))
    (let ((new-title (read-string "New title: ")))
      (when (string-empty-p new-title)
        (user-error "Title cannot be empty"))
      (message "Lark: updating task...")
      (lark--run-command
       (list "task" "+update" "--task-id" id "--summary" new-title)
       (lambda (_data)
         (message "Lark: task updated")
         (lark-tasks-refresh))))))

;;;; Task completion
;; CLI: task +complete --task-id X


(defun lark-tasks-complete ()
  "Mark the task at point as complete."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No task at point"))
    (message "Lark: completing task...")
    (lark--run-command
     (list "task" "+complete" "--task-id" id)
     (lambda (_data)
       (message "Lark: task completed")
       (lark-tasks-refresh)))))

;;;; Task deletion
;; CLI: task tasks delete --params '{"task_id":"X"}'


(defun lark-tasks-delete ()
  "Delete the task at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No task at point"))
    (when (yes-or-no-p (format "Delete task %s? " id))
      (message "Lark: deleting task...")
      (let ((params (json-encode `((task_id . ,id)))))
        (lark--run-command
         (list "task" "tasks" "delete" "--params" params)
         (lambda (_data)
           (message "Lark: task deleted")
           (lark-tasks-refresh)))))))

;;;; Copy task ID

(defun lark-tasks-copy-id ()
  "Copy the task ID at point to the kill ring."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No task at point"))
    (kill-new id)
    (message "Copied: %s" id)))

;;;; Subtask creation
;; Subtasks are created via the raw API:
;; task subtasks create --params '{"task_id":"PARENT"}' --data '{"summary":"X"}'

(defun lark-tasks-subtask-create ()
  "Create a subtask under the task at point."
  (interactive)
  (let ((parent-id (tabulated-list-get-id)))
    (unless parent-id (user-error "No task at point"))
    (let ((title (read-string "Subtask title: ")))
      (when (string-empty-p title)
        (user-error "Title cannot be empty"))
      (message "Lark: creating subtask...")
      (let ((params (json-encode `((task_id . ,parent-id))))
            (body (json-encode `((summary . ,title)))))
        (lark--run-command
         (list "task" "subtasks" "create"
               "--params" params "--data" body)
         (lambda (data)
           (let ((id (or (lark--get-nested data 'data 'task_id)
                         (alist-get 'task_id data))))
             (message "Lark: subtask created%s"
                      (if id (format " (ID: %s)" id) "")))))))))

;;;; Tasklist management
;; CLI: task tasklists list


;;;###autoload
(defun lark-tasks-tasklists ()
  "List available task lists."
  (interactive)
  (message "Lark: fetching task lists...")
  (lark--run-command
   '("task" "tasklists" "list")
   #'lark-tasks--display-tasklists))

(defun lark-tasks--display-tasklists (data)
  "Display tasklist DATA."
  (let* ((lists (or (alist-get 'items data)
                    (lark--get-nested data 'data 'items)
                    (alist-get 'tasklists data)
                    (and (listp data) data)))
         (buf (get-buffer-create "*Lark Task Lists*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Lark Task Lists\n" 'face 'bold)
                (make-string 60 ?─) "\n\n")
        (if (null lists)
            (insert "(no task lists found)\n")
          (dolist (tl lists)
            (let ((id (or (alist-get 'tasklist_id tl) (alist-get 'id tl)))
                  (name (or (alist-get 'name tl) (alist-get 'title tl) "")))
              (insert (propertize name 'face 'bold)
                      (format "  (ID: %s)\n" (or id "")))))))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;; Transient dispatch

;;;###autoload (autoload 'lark-tasks-dispatch "lark-tasks" nil t)
(transient-define-prefix lark-tasks-dispatch ()
  "Lark Tasks commands."
  ["Quick Actions"
   ("l" "My tasks"       lark-tasks-list)
   ("c" "Create task"    lark-tasks-create)
   ("x" "Complete task"  lark-tasks-complete)]
  ["Browse"
   ("t" "Task lists"     lark-tasks-tasklists)]
  ["At Point"
   ("RET" "Open"         lark-tasks-open)
   ("e"   "Edit"         lark-tasks-edit)
   ("s"   "Add subtask"  lark-tasks-subtask-create)
   ("d"   "Delete"       lark-tasks-delete)])

(provide 'lark-tasks)
;;; lark-tasks.el ends here
