;;; lark-tasks.el --- Lark Tasks integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; Provides Task domain commands for lark.el: task listing, detail
;; view, create/update/complete tasks, subtask management, task list
;; (tasklist) management, and an org-agenda–style view.
;;
;; CLI command mapping:
;;   My tasks:     task +get-my-tasks [--query X] [--complete] [--due-start X]
;;                                    [--due-end X] [--created_at X]
;;   Create:       task +create --summary X [--description X] [--due X] [--assignee X]
;;   Update:       task +update --task-id X [--summary X] [--description X] [--due X]
;;   Complete:     task +complete --task-id X
;;   Reopen:       task +reopen --task-id X
;;   Get detail:   task tasks get --params '{"task_guid":"X"}'
;;   Delete:       task tasks delete --params '{"task_id":"X"}'
;;   Tasklists:    task tasklists list

;;; Code:

(require 'lark-core)
(require 'lark-contact)
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
  "Extract the task ID from TASK.
Prefers guid (from +get-my-tasks), falls back to task_id / id."
  (or (alist-get 'guid task)
      (alist-get 'task_id task)
      (alist-get 'id task)))

(defun lark-tasks--task-title (task)
  "Extract the title from TASK."
  (or (alist-get 'summary task)
      (alist-get 'title task)
      (alist-get 'name task)
      "(no title)"))

(defun lark-tasks--task-due (task)
  "Extract the due date from TASK as a display string."
  (let ((due (or (alist-get 'due_at task)
                 (alist-get 'due task)
                 (lark--get-nested task 'due 'timestamp)
                 (lark--get-nested task 'due 'date))))
    (cond
     ((numberp due) (or (lark--format-timestamp due) ""))
     ((stringp due) (lark-tasks--format-date due))
     (t ""))))

(defun lark-tasks--task-due-raw (task)
  "Extract the raw due date string from TASK (for sorting)."
  (or (alist-get 'due_at task)
      (alist-get 'due task)
      (let ((ts (or (lark--get-nested task 'due 'timestamp)
                     (lark--get-nested task 'due 'date))))
        (cond
         ((numberp ts) (or (lark--format-timestamp ts) ""))
         ((stringp ts) ts)
         (t "")))))

(defun lark-tasks--format-date (str)
  "Format a date STR for display (truncate to YYYY-MM-DD)."
  (if (and str (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" str))
      (substring str 0 (min 10 (length str)))
    (or str "")))

(defun lark-tasks--format-datetime (str)
  "Format a datetime STR for display (truncate to YYYY-MM-DD HH:MM)."
  (if (and str (stringp str) (not (string-empty-p str)))
      (substring str 0 (min 16 (length str)))
    ""))

(defun lark-tasks--task-status (task)
  "Extract the completion status from TASK."
  (let ((completed (or (alist-get 'completed_at task)
                       (alist-get 'completed task)
                       (alist-get 'is_completed task))))
    (cond
     ((and completed (not (equal completed :false)) (not (eq completed 0))
           (not (equal completed "")) (not (equal completed "0")))
      "done")
     (t
      (or (alist-get 'status task) "todo")))))

(defun lark-tasks--task-creator (task)
  "Extract the creator name from TASK."
  (or (alist-get 'creator_name task)
      (lark--get-nested task 'creator 'display_name)
      (lark--get-nested task 'creator 'name)
      (let ((id (lark--get-nested task 'creator 'id)))
        (when (and id (stringp id) (not (string-empty-p id)))
          (lark-contact-resolve-name id "open_id")))
      ""))

(defun lark-tasks--task-url (task)
  "Extract the URL from TASK."
  (or (alist-get 'url task) ""))

(defun lark-tasks--task-created-at (task)
  "Extract the created_at time from TASK."
  (let ((ts (alist-get 'created_at task)))
    (cond
     ((and (stringp ts) (not (string-empty-p ts)))
      (lark-tasks--format-datetime ts))
     ((numberp ts) (or (lark--format-timestamp ts) ""))
     (t ""))))

(defun lark-tasks--task-description (task)
  "Extract the description from TASK."
  (or (alist-get 'description task)
      (alist-get 'notes task)
      ""))

(defun lark-tasks--task-subtask-count (task)
  "Extract the subtask count from TASK."
  (let ((n (alist-get 'subtask_count task)))
    (if (and n (numberp n) (> n 0)) (format "%d" n) "")))

(defun lark-tasks--task-origin (task)
  "Extract the origin/source info from TASK."
  (or (lark--get-nested task 'origin 'href 'title) ""))

(defun lark-tasks--task-members-string (task)
  "Extract a string of assignee names from TASK members."
  (let ((members (or (alist-get 'members task)
                     (alist-get 'assignees task))))
    (if members
        (mapconcat
         (lambda (m)
           (let ((role (or (alist-get 'role m) ""))
                 (name (or (alist-get 'display_name m)
                           (alist-get 'name m)
                           (let ((id (alist-get 'id m)))
                             (when id (lark-contact-resolve-name id "open_id"))))))
             (if (equal role "assignee")
                 (or name "?")
               nil)))
         members ", ")
      "")))

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
          (and (listp inner) (listp (car inner))
               (or (alist-get 'task_id (car inner))
                   (alist-get 'guid (car inner))
                   (alist-get 'summary (car inner)))
               inner))))
   ((and (listp data) (listp (car data))
         (or (alist-get 'task_id (car data))
             (alist-get 'guid (car data))
             (alist-get 'summary (car data))))
    data)
   (t nil)))

;;;; Multi-line task list view

(defvar lark-tasks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-tasks-open)
    (define-key map (kbd "g")   #'lark-tasks-refresh)
    (define-key map (kbd "c")   #'lark-tasks-create)
    (define-key map (kbd "e")   #'lark-tasks-edit)
    (define-key map (kbd "x")   #'lark-tasks-complete)
    (define-key map (kbd "X")   #'lark-tasks-reopen)
    (define-key map (kbd "d")   #'lark-tasks-delete)
    (define-key map (kbd "y")   #'lark-tasks-copy-id)
    (define-key map (kbd "s")   #'lark-tasks-subtask-create)
    (define-key map (kbd "n")   #'lark-tasks--next-task)
    (define-key map (kbd "p")   #'lark-tasks--prev-task)
    (define-key map (kbd "A")   #'lark-tasks-agenda)
    (define-key map (kbd "?")   #'lark-tasks-dispatch)
    map)
  "Keymap for `lark-tasks-mode'.")

(define-derived-mode lark-tasks-mode special-mode
  "Lark Tasks"
  "Major mode for browsing Lark tasks.
Each task is displayed as a multi-line section.")

;;;; AI context provider — shared by both task list and agenda modes

(defun lark-tasks--ai-context ()
  "Return the AI context plist for a tasks buffer."
  (let ((items lark-tasks--items)
        (task-id (get-text-property (point) 'lark-task-id)))
    (list :domain "tasks"
          :buffer-type "task-list"
          :item (when task-id
                  (list :task-id task-id
                        :task (seq-find (lambda (tk)
                                          (equal (alist-get 'guid tk) task-id))
                                        items)))
          :summary (format "Task list with %d tasks%s"
                           (length items)
                           (if task-id
                               (format ", cursor on task %s" task-id)
                             "")))))

(put 'lark-tasks-mode 'lark-ai-context-provider #'lark-tasks--ai-context)

(defun lark-tasks--task-id-at-point ()
  "Return the task ID at point, or nil."
  (get-text-property (point) 'lark-task-id))

(defun lark-tasks--insert-field (label value)
  "Insert a LABEL: VALUE line if VALUE is non-empty."
  (when (and value (not (string-empty-p value)))
    (insert (propertize (format "  %-14s" (concat label ":")) 'face 'font-lock-keyword-face)
            value "\n")))

(defun lark-tasks--status-face (status)
  "Return a face for task STATUS."
  (pcase status
    ("done" 'success)
    ("todo" 'warning)
    ("in_progress" 'font-lock-constant-face)
    (_ 'default)))

(defun lark-tasks--insert-task (task)
  "Insert a multi-line section for TASK into the current buffer."
  (let ((id (lark-tasks--task-id task))
        (title (lark-tasks--task-title task))
        (status (lark-tasks--task-status task))
        (due (lark-tasks--task-due task))
        (created (lark-tasks--task-created-at task))
        (url (lark-tasks--task-url task))
        (origin (lark-tasks--task-origin task))
        (beg (point)))
    (insert (propertize (if (equal status "done") "[x] " "[ ] ")
                        'face (lark-tasks--status-face status))
            (propertize title 'face 'bold) "\n")
    (lark-tasks--insert-field "Status" (propertize status 'face (lark-tasks--status-face status)))
    (lark-tasks--insert-field "Due" due)
    (lark-tasks--insert-field "Created" created)
    (lark-tasks--insert-field "Origin" origin)
    (lark-tasks--insert-field "URL" url)
    (insert "\n")
    (put-text-property beg (point) 'lark-task-id id)))

(defun lark-tasks--next-task ()
  "Move to the next task section."
  (interactive)
  (let ((current (lark-tasks--task-id-at-point))
        (pos (point)))
    (when current
      (while (and (not (eobp))
                  (equal (get-text-property (point) 'lark-task-id) current))
        (forward-char)))
    (while (and (not (eobp))
                (not (get-text-property (point) 'lark-task-id)))
      (forward-char))
    (when (eobp) (goto-char pos))))

(defun lark-tasks--prev-task ()
  "Move to the previous task section."
  (interactive)
  (let ((current (lark-tasks--task-id-at-point))
        (pos (point)))
    (when current
      (while (and (not (bobp))
                  (equal (get-text-property (point) 'lark-task-id) current))
        (backward-char)))
    (while (and (not (bobp))
                (not (get-text-property (point) 'lark-task-id)))
      (backward-char))
    (let ((target (get-text-property (point) 'lark-task-id)))
      (if target
          (while (and (not (bobp))
                      (equal (get-text-property (1- (point)) 'lark-task-id) target))
            (backward-char))
        (goto-char pos)))))

;;;; Task listing
;; CLI: task +get-my-tasks [--query X] [--complete]

;;;###autoload
(defun lark-tasks-list (&optional show-completed)
  "List Lark tasks assigned to me.
With prefix argument SHOW-COMPLETED, include completed tasks."
  (interactive "P")
  (message "Lark: fetching tasks...")
  (let ((args (list "task" "+get-my-tasks" "--page-all")))
    (unless show-completed
      (setq args (append args (list "--complete=false"))))
    (lark--run-command
     args
     (lambda (data)
       (lark-tasks--display-list data nil)))))

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
            lark-tasks--tasklist-id tasklist-id)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null tasks)
            (insert "(no tasks)\n")
          (dolist (task tasks)
            (lark-tasks--insert-task task))))
      (goto-char (point-min))
      (setq header-line-format
            (format " Lark Tasks — %d task(s)" (length tasks))))
    (pop-to-buffer buf)))

;;;; Task detail
;; CLI: task tasks get --params '{"task_guid":"X"}'

(defun lark-tasks-open ()
  "Open the task at point in a detail view."
  (interactive)
  (let ((id (lark-tasks--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (message "Lark: fetching task details...")
    (let ((params (json-encode `((task_guid . ,id)))))
      (lark--run-command
       (list "task" "tasks" "get" "--params" params)
       (lambda (data)
         (lark-tasks--display-detail data id))))))

(defun lark-tasks--display-detail (data task-id)
  "Display task detail DATA for TASK-ID."
  (let* ((raw (or (alist-get 'data data) data))
         (task (or (alist-get 'task raw) raw))
         (title (lark-tasks--task-title task))
         (buf (get-buffer-create (format "*Lark Task: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize title 'face 'bold) "\n"
                (make-string (min 60 (max 20 (length title))) ?─) "\n\n")
        ;; Status
        (let ((status (lark-tasks--task-status task)))
          (insert (propertize "Status: " 'face 'bold)
                  (propertize status 'face (lark-tasks--status-face status)) "\n"))
        ;; Due date
        (let ((due (lark-tasks--task-due task)))
          (unless (string-empty-p due)
            (insert (propertize "Due: " 'face 'bold) due "\n")))
        ;; Created
        (let ((created (lark-tasks--task-created-at task)))
          (unless (string-empty-p created)
            (insert (propertize "Created: " 'face 'bold) created "\n")))
        ;; Creator
        (let ((creator (lark-tasks--task-creator task)))
          (unless (string-empty-p creator)
            (insert (propertize "Creator: " 'face 'bold) creator "\n")))
        ;; Assignees
        (let ((assignees (lark-tasks--task-members-string task)))
          (unless (string-empty-p assignees)
            (insert (propertize "Assignees: " 'face 'bold) assignees "\n")))
        ;; Subtask count
        (let ((sc (lark-tasks--task-subtask-count task)))
          (unless (string-empty-p sc)
            (insert (propertize "Subtasks: " 'face 'bold) sc "\n")))
        ;; Origin
        (let ((origin (lark-tasks--task-origin task)))
          (unless (string-empty-p origin)
            (insert (propertize "Origin: " 'face 'bold) origin "\n")))
        ;; URL
        (let ((url (lark-tasks--task-url task)))
          (unless (string-empty-p url)
            (insert (propertize "URL: " 'face 'bold) url "\n")))
        ;; Description
        (let ((desc (lark-tasks--task-description task)))
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
  (let ((id (lark-tasks--task-id-at-point)))
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

;;;; Task completion / reopen
;; CLI: task +complete --task-id X
;; CLI: task +reopen --task-id X

(defun lark-tasks-complete ()
  "Mark the task at point as complete."
  (interactive)
  (let ((id (lark-tasks--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (message "Lark: completing task...")
    (lark--run-command
     (list "task" "+complete" "--task-id" id)
     (lambda (_data)
       (message "Lark: task completed")
       (lark-tasks-refresh)))))

(defun lark-tasks-reopen ()
  "Reopen the completed task at point."
  (interactive)
  (let ((id (lark-tasks--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (message "Lark: reopening task...")
    (lark--run-command
     (list "task" "+reopen" "--task-id" id)
     (lambda (_data)
       (message "Lark: task reopened")
       (lark-tasks-refresh)))))

;;;; Task deletion
;; CLI: task tasks delete --params '{"task_id":"X"}'

(defun lark-tasks-delete ()
  "Delete the task at point."
  (interactive)
  (let ((id (lark-tasks--task-id-at-point)))
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
  (let ((id (lark-tasks--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (kill-new id)
    (message "Copied: %s" id)))

;;;; Subtask creation
;; task subtasks create --params '{"task_id":"PARENT"}' --data '{"summary":"X"}'

(defun lark-tasks-subtask-create ()
  "Create a subtask under the task at point."
  (interactive)
  (let ((parent-id (lark-tasks--task-id-at-point)))
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

;;; ================================================================
;;;; Org-agenda–style view
;;; ================================================================

(defvar lark-tasks-agenda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-tasks-agenda-open)
    (define-key map (kbd "g")   #'lark-tasks-agenda-refresh)
    (define-key map (kbd "x")   #'lark-tasks-agenda-complete)
    (define-key map (kbd "X")   #'lark-tasks-agenda-reopen)
    (define-key map (kbd "n")   #'lark-tasks-agenda--next)
    (define-key map (kbd "p")   #'lark-tasks-agenda--prev)
    (define-key map (kbd "?")   #'lark-tasks-dispatch)
    map)
  "Keymap for `lark-tasks-agenda-mode'.")

(define-derived-mode lark-tasks-agenda-mode special-mode
  "Lark Agenda"
  "Major mode for the Lark Tasks org-agenda–style view.")

(put 'lark-tasks-agenda-mode 'lark-ai-context-provider
     #'lark-tasks--ai-context)

(defvar-local lark-tasks-agenda--items nil
  "Cached tasks for the agenda buffer.")

;;;###autoload
(defun lark-tasks-agenda ()
  "Display Lark tasks in an org-agenda–style view.
Shows incomplete tasks sorted by due date."
  (interactive)
  (message "Lark: building task agenda...")
  (lark--run-command
   (list "task" "+get-my-tasks" "--page-all" "--complete=false")
   #'lark-tasks-agenda--render))

(defun lark-tasks-agenda-refresh ()
  "Refresh the task agenda."
  (interactive)
  (lark-tasks-agenda))

(defun lark-tasks-agenda--render (data)
  "Render the org-agenda–style view from DATA."
  (let* ((tasks (lark-tasks--extract-tasks data))
         (today (format-time-string "%Y-%m-%d"))
         ;; Partition into overdue, today, upcoming (has due), and no-due
         (with-due (seq-filter (lambda (tk) (not (string-empty-p (lark-tasks--task-due-raw tk)))) tasks))
         (no-due (seq-filter (lambda (tk) (string-empty-p (lark-tasks--task-due-raw tk))) tasks))
         ;; Sort by due date
         (sorted (sort with-due
                       (lambda (a b)
                         (string< (lark-tasks--task-due-raw a)
                                  (lark-tasks--task-due-raw b)))))
         (overdue (seq-filter (lambda (tk) (string< (lark-tasks--format-date (lark-tasks--task-due-raw tk)) today)) sorted))
         (due-today (seq-filter (lambda (tk) (equal (lark-tasks--format-date (lark-tasks--task-due-raw tk)) today)) sorted))
         (upcoming (seq-filter (lambda (tk) (string> (lark-tasks--format-date (lark-tasks--task-due-raw tk)) today)) sorted))
         (buf (get-buffer-create "*Lark Task Agenda*")))
    (with-current-buffer buf
      (lark-tasks-agenda-mode)
      (setq lark-tasks-agenda--items tasks)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Lark Task Agenda  %s\n" today) 'face 'bold)
                (make-string 60 ?─) "\n")
        ;; Overdue
        (when overdue
          (insert "\n" (propertize "Overdue" 'face '(:foreground "red" :weight bold)) "\n")
          (dolist (task overdue)
            (lark-tasks-agenda--insert-entry task 'overdue)))
        ;; Today
        (insert "\n" (propertize "Today" 'face '(:foreground "blue" :weight bold)) "\n")
        (if due-today
            (dolist (task due-today)
              (lark-tasks-agenda--insert-entry task 'today))
          (insert (propertize "  (nothing due today)\n" 'face 'font-lock-comment-face)))
        ;; Upcoming
        (when upcoming
          (insert "\n" (propertize "Upcoming" 'face '(:weight bold)) "\n")
          (dolist (task upcoming)
            (lark-tasks-agenda--insert-entry task 'upcoming)))
        ;; No due date
        (when no-due
          (insert "\n" (propertize "No Due Date" 'face 'font-lock-comment-face) "\n")
          (dolist (task no-due)
            (lark-tasks-agenda--insert-entry task 'no-due))))
      (goto-char (point-min))
      (setq header-line-format
            (format " Lark Task Agenda — %d task(s)" (length tasks))))
    (pop-to-buffer buf)))

(defun lark-tasks-agenda--insert-entry (task category)
  "Insert a single-line agenda entry for TASK.
CATEGORY is one of overdue, today, upcoming, no-due."
  (let* ((id (lark-tasks--task-id task))
         (title (lark-tasks--task-title task))
         (status (lark-tasks--task-status task))
         (due (lark-tasks--task-due task))
         (keyword (if (equal status "done") "DONE" "TODO"))
         (due-face (pcase category
                     ('overdue 'error)
                     ('today 'warning)
                     ('upcoming 'default)
                     ('no-due 'font-lock-comment-face)))
         (beg (point)))
    (insert "  "
            (propertize (format "%-5s" keyword) 'face
                        (if (equal keyword "DONE") 'success 'org-todo))
            " "
            (propertize title 'face (if (equal keyword "DONE")
                                        '(:strike-through t)
                                      'default)))
    (when (and due (not (string-empty-p due)))
      (insert "  "
              (propertize (format "<%s>" due) 'face due-face)))
    (insert "\n")
    (put-text-property beg (point) 'lark-task-id id)))

(defun lark-tasks-agenda--task-id-at-point ()
  "Return the task ID at point in the agenda buffer."
  (get-text-property (point) 'lark-task-id))

(defun lark-tasks-agenda-open ()
  "Open the task at point in the agenda view."
  (interactive)
  (let ((id (lark-tasks-agenda--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (message "Lark: fetching task details...")
    (let ((params (json-encode `((task_guid . ,id)))))
      (lark--run-command
       (list "task" "tasks" "get" "--params" params)
       (lambda (data)
         (lark-tasks--display-detail data id))))))

(defun lark-tasks-agenda-complete ()
  "Complete the task at point in the agenda view."
  (interactive)
  (let ((id (lark-tasks-agenda--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (message "Lark: completing task...")
    (lark--run-command
     (list "task" "+complete" "--task-id" id)
     (lambda (_data)
       (message "Lark: task completed")
       (lark-tasks-agenda-refresh)))))

(defun lark-tasks-agenda-reopen ()
  "Reopen the task at point in the agenda view."
  (interactive)
  (let ((id (lark-tasks-agenda--task-id-at-point)))
    (unless id (user-error "No task at point"))
    (message "Lark: reopening task...")
    (lark--run-command
     (list "task" "+reopen" "--task-id" id)
     (lambda (_data)
       (message "Lark: task reopened")
       (lark-tasks-agenda-refresh)))))

(defun lark-tasks-agenda--next ()
  "Move to the next task entry in the agenda."
  (interactive)
  (let ((pos (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (get-text-property (point) 'lark-task-id)))
      (forward-line 1))
    (when (eobp) (goto-char pos))))

(defun lark-tasks-agenda--prev ()
  "Move to the previous task entry in the agenda."
  (interactive)
  (let ((pos (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (get-text-property (point) 'lark-task-id)))
      (forward-line -1))
    (unless (get-text-property (point) 'lark-task-id)
      (goto-char pos))))

;;;; Transient dispatch

;;;###autoload (autoload 'lark-tasks-dispatch "lark-tasks" nil t)
(transient-define-prefix lark-tasks-dispatch ()
  "Lark Tasks commands."
  ["Quick Actions"
   ("l" "My tasks"       lark-tasks-list)
   ("A" "Agenda view"    lark-tasks-agenda)
   ("c" "Create task"    lark-tasks-create)
   ("x" "Complete task"  lark-tasks-complete)
   ("X" "Reopen task"    lark-tasks-reopen)]
  ["Browse"
   ("t" "Task lists"     lark-tasks-tasklists)]
  ["At Point"
   ("RET" "Open"         lark-tasks-open)
   ("e"   "Edit"         lark-tasks-edit)
   ("s"   "Add subtask"  lark-tasks-subtask-create)
   ("d"   "Delete"       lark-tasks-delete)])

(provide 'lark-tasks)
;;; lark-tasks.el ends here
