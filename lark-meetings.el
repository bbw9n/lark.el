;;; lark-meetings.el --- Lark Meetings (VC) integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Provides Meeting / Video Conference domain commands for lark.el:
;; meeting search, meeting detail, meeting notes, and recordings.
;;
;; The default search window is last week to tomorrow.
;;
;; CLI command mapping:
;;   Search:    vc +search --start X --end X [--query X] [--organizer-ids X]
;;                         [--participant-ids X] [--room-ids X]
;;   Notes:     vc +notes --meeting-ids X | --minute-tokens X | --calendar-event-ids X
;;   Recording: vc +recording --meeting-ids X | --calendar-event-ids X
;;   Detail:    vc meeting get --params '{"meeting_id":"X"}'

;;; Code:

(require 'lark-core)
(require 'json)
(require 'transient)

;;;; Customization

(defgroup lark-meetings nil
  "Lark Meetings settings."
  :group 'lark
  :prefix "lark-meetings-")

(defcustom lark-meetings-search-page-size "15"
  "Default page size for meeting search."
  :type 'string
  :group 'lark-meetings)

;;;; Buffer-local variables

(defvar-local lark-meetings--items nil
  "Cached meeting list for the current buffer.")

(defvar-local lark-meetings--search-start nil
  "Last search start time.")

(defvar-local lark-meetings--search-end nil
  "Last search end time.")

(defvar-local lark-meetings--search-query nil
  "Last search query keyword.")

;;;; Time helpers

(defun lark-meetings--default-start ()
  "Return the default search start: 7 days ago as YYYY-MM-DD."
  (format-time-string "%Y-%m-%d" (time-subtract nil (* 7 86400))))

(defun lark-meetings--default-end ()
  "Return the default search end: tomorrow as YYYY-MM-DD."
  (format-time-string "%Y-%m-%d" (time-add nil 86400)))

;;;; Meeting field extractors

(defun lark-meetings--meeting-id (meeting)
  "Extract the meeting ID from MEETING."
  (or (alist-get 'id meeting)
      (alist-get 'meeting_id meeting)
      ""))

(defun lark-meetings--meeting-title (meeting)
  "Extract a display title from MEETING.
Uses the first line of display_info, falling back to description."
  (let ((info (alist-get 'display_info meeting)))
    (if (and info (stringp info) (not (string-empty-p info)))
        (car (split-string info "\n" t))
      (or (lark--get-nested meeting 'meta_data 'description)
          (alist-get 'topic meeting)
          "(untitled meeting)"))))

(defun lark-meetings--meeting-description (meeting)
  "Extract the description line from MEETING."
  (or (lark--get-nested meeting 'meta_data 'description)
      ""))

(defun lark-meetings--meeting-app-link (meeting)
  "Extract the app link from MEETING."
  (or (lark--get-nested meeting 'meta_data 'app_link)
      ""))

(defun lark-meetings--meeting-display-info (meeting)
  "Extract the full display_info from MEETING."
  (or (alist-get 'display_info meeting) ""))

;;;; Response extraction

(defun lark-meetings--extract-meetings (data)
  "Extract meeting list from lark-cli search response DATA."
  (or (lark--get-nested data 'data 'items)
      (alist-get 'items data)
      (lark--get-nested data 'data 'meetings)))

(defun lark-meetings--extract-notes (data)
  "Extract notes list from lark-cli notes response DATA."
  (or (lark--get-nested data 'data 'notes)
      (alist-get 'notes data)))

;;;; Meeting list mode (multi-line sections)

(defvar lark-meetings-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-meetings-open)
    (define-key map (kbd "g")   #'lark-meetings-search-refresh)
    (define-key map (kbd "N")   #'lark-meetings-notes-at-point)
    (define-key map (kbd "y")   #'lark-meetings-copy-id)
    (define-key map (kbd "w")   #'lark-meetings-copy-link)
    (define-key map (kbd "n")   #'lark-meetings--next)
    (define-key map (kbd "p")   #'lark-meetings--prev)
    (define-key map (kbd "?")   #'lark-meetings-dispatch)
    map)
  "Keymap for `lark-meetings-mode'.")

(define-derived-mode lark-meetings-mode special-mode
  "Lark Meetings"
  "Major mode for browsing Lark meeting search results.
Each meeting is displayed as a multi-line section.")

(defun lark-meetings--meeting-id-at-point ()
  "Return the meeting ID at point, or nil."
  (get-text-property (point) 'lark-meeting-id))

(defun lark-meetings--meeting-at-point ()
  "Return the meeting alist at point, or nil."
  (get-text-property (point) 'lark-meeting))

(defun lark-meetings--insert-field (label value)
  "Insert LABEL: VALUE if VALUE is non-empty."
  (when (and value (not (string-empty-p value)))
    (insert (propertize (format "  %-18s" (concat label ":")) 'face 'font-lock-keyword-face)
            value "\n")))

(defun lark-meetings--insert-meeting (meeting)
  "Insert a multi-line section for MEETING."
  (let ((id (lark-meetings--meeting-id meeting))
        (title (lark-meetings--meeting-title meeting))
        (desc (lark-meetings--meeting-description meeting))
        (link (lark-meetings--meeting-app-link meeting))
        (beg (point)))
    (insert (propertize title 'face 'bold) "\n")
    (lark-meetings--insert-field "Description" desc)
    (lark-meetings--insert-field "Meeting ID" id)
    (lark-meetings--insert-field "Link" link)
    (insert "\n")
    (put-text-property beg (point) 'lark-meeting-id id)
    (put-text-property beg (point) 'lark-meeting meeting)))

(defun lark-meetings--next ()
  "Move to the next meeting section."
  (interactive)
  (let ((current (lark-meetings--meeting-id-at-point))
        (pos (point)))
    (when current
      (while (and (not (eobp))
                  (equal (get-text-property (point) 'lark-meeting-id) current))
        (forward-char)))
    (while (and (not (eobp))
                (not (get-text-property (point) 'lark-meeting-id)))
      (forward-char))
    (when (eobp) (goto-char pos))))

(defun lark-meetings--prev ()
  "Move to the previous meeting section."
  (interactive)
  (let ((current (lark-meetings--meeting-id-at-point))
        (pos (point)))
    (when current
      (while (and (not (bobp))
                  (equal (get-text-property (point) 'lark-meeting-id) current))
        (backward-char)))
    (while (and (not (bobp))
                (not (get-text-property (point) 'lark-meeting-id)))
      (backward-char))
    (let ((target (get-text-property (point) 'lark-meeting-id)))
      (if target
          (while (and (not (bobp))
                      (equal (get-text-property (1- (point)) 'lark-meeting-id) target))
            (backward-char))
        (goto-char pos)))))

;;;; Search
;; CLI: vc +search --start X --end X [--query X] [--page-size N]

;;;###autoload
(defun lark-meetings-search (&optional query start end)
  "Search meeting records.
QUERY is an optional keyword.  START and END default to last week through tomorrow."
  (interactive
   (list (read-string "Search meetings (keyword, optional): ")
         (read-string (format "Start date [%s]: " (lark-meetings--default-start)))
         (read-string (format "End date [%s]: " (lark-meetings--default-end)))))
  (let ((start (if (or (null start) (string-empty-p start))
                   (lark-meetings--default-start) start))
        (end (if (or (null end) (string-empty-p end))
                 (lark-meetings--default-end) end)))
    (message "Lark: searching meetings (%s to %s)..." start end)
    (let ((args (list "vc" "+search"
                      "--start" start
                      "--end" end
                      "--page-size" lark-meetings-search-page-size))
          (q (unless (string-empty-p (or query "")) query)))
      (when q
        (setq args (append args (list "--query" q))))
      (lark--run-command
       args
       (lambda (data)
         (lark-meetings--display-search data start end q))))))

(defun lark-meetings-search-refresh ()
  "Refresh meeting search with previous parameters."
  (interactive)
  (lark-meetings-search
   lark-meetings--search-query
   lark-meetings--search-start
   lark-meetings--search-end))

(defun lark-meetings--display-search (data start end &optional query)
  "Display search results DATA.  Store START, END, QUERY for refresh."
  (let* ((meetings (lark-meetings--extract-meetings data))
         (buf (get-buffer-create "*Lark Meetings*")))
    (with-current-buffer buf
      (lark-meetings-mode)
      (setq lark-meetings--items meetings
            lark-meetings--search-start start
            lark-meetings--search-end end
            lark-meetings--search-query query)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Meetings: %s → %s" start end) 'face 'bold)
                (if query (format "  [%s]" query) "")
                "\n"
                (make-string 60 ?─) "\n\n")
        (if (null meetings)
            (insert "(no meetings found)\n")
          (dolist (m meetings)
            (lark-meetings--insert-meeting m))))
      (goto-char (point-min))
      (setq header-line-format
            (format " Lark Meetings — %d result(s)" (length meetings))))
    (pop-to-buffer buf)))

;;;; Meeting detail
;; CLI: vc meeting get --params '{"meeting_id":"X"}'

(defun lark-meetings-open ()
  "Open the meeting at point — show full display info."
  (interactive)
  (let ((meeting (lark-meetings--meeting-at-point)))
    (unless meeting (user-error "No meeting at point"))
    (let* ((id (lark-meetings--meeting-id meeting))
           (title (lark-meetings--meeting-title meeting))
           (info (lark-meetings--meeting-display-info meeting))
           (desc (lark-meetings--meeting-description meeting))
           (link (lark-meetings--meeting-app-link meeting))
           (buf (get-buffer-create (format "*Lark Meeting: %s*" title))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize title 'face 'bold) "\n"
                  (make-string (min 60 (max 20 (length title))) ?─) "\n\n")
          (lark-meetings--insert-field "Meeting ID" id)
          (lark-meetings--insert-field "Description" desc)
          (lark-meetings--insert-field "Link" link)
          (when (and info (not (string-empty-p info)))
            (insert "\n" (propertize "Full Info" 'face 'bold) "\n"
                    (make-string 40 ?─) "\n"
                    info "\n")))
        (special-mode)
        (goto-char (point-min)))
      (pop-to-buffer buf))))

;;;; Meeting notes
;; CLI: vc +notes --meeting-ids X

(defun lark-meetings-notes-at-point ()
  "Fetch notes for the meeting at point."
  (interactive)
  (let ((id (lark-meetings--meeting-id-at-point)))
    (unless id (user-error "No meeting at point"))
    (lark-meetings-notes id)))

;;;###autoload
(defun lark-meetings-notes (meeting-id)
  "Fetch and display notes for MEETING-ID."
  (interactive "sMeeting ID: ")
  (when (string-empty-p meeting-id)
    (user-error "Meeting ID is required"))
  (message "Lark: fetching meeting notes...")
  (lark--run-command
   (list "vc" "+notes" "--meeting-ids" meeting-id)
   (lambda (data)
     (lark-meetings--display-notes data meeting-id))
   nil :no-error t))

(defun lark-meetings--display-notes (data meeting-id)
  "Display meeting notes DATA for MEETING-ID."
  (let* ((notes (lark-meetings--extract-notes data))
         (buf (get-buffer-create (format "*Lark Meeting Notes: %s*" meeting-id))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Meeting Notes: %s" meeting-id) 'face 'bold) "\n"
                (make-string 60 ?─) "\n\n")
        (if (null notes)
            (insert "(no notes available)\n")
          (dolist (note notes)
            (let ((err (alist-get 'error note))
                  (mid (or (alist-get 'meeting_id note) ""))
                  (summary (or (alist-get 'summary note) ""))
                  (todos (alist-get 'todos note))
                  (chapters (alist-get 'chapters note))
                  (transcript (or (alist-get 'transcript note) "")))
              (if err
                  (insert (propertize (format "  %s: %s\n" mid err) 'face 'error))
                ;; Summary
                (when (not (string-empty-p summary))
                  (insert (propertize "Summary" 'face 'bold) "\n"
                          summary "\n\n"))
                ;; Todos
                (when todos
                  (insert (propertize "Action Items" 'face 'bold) "\n")
                  (dolist (todo todos)
                    (let ((text (or (alist-get 'text todo)
                                    (alist-get 'content todo) "")))
                      (insert "  - " text "\n")))
                  (insert "\n"))
                ;; Chapters
                (when chapters
                  (insert (propertize "Chapters" 'face 'bold) "\n")
                  (dolist (ch chapters)
                    (let ((ch-title (or (alist-get 'title ch) ""))
                          (ch-content (or (alist-get 'content ch) "")))
                      (insert "  " (propertize ch-title 'face 'bold) "\n")
                      (unless (string-empty-p ch-content)
                        (insert "    " ch-content "\n"))))
                  (insert "\n"))
                ;; Transcript
                (when (not (string-empty-p transcript))
                  (insert (propertize "Transcript" 'face 'bold) "\n"
                          transcript "\n\n")))))))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;; Recording
;; CLI: vc +recording --meeting-ids X

(defun lark-meetings--format-value (value)
  "Format VALUE as a display string."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((eq value t) "true")
   ((eq value :false) "false")
   (t (format "%s" value))))

(defun lark-meetings--insert-download-link (minute-token)
  "Insert a clickable download link for MINUTE-TOKEN.
Uses the nf-fa-download icon ()."
  (insert "  "
          (propertize " Download Recording"
                      'face 'link
                      'mouse-face 'highlight
                      'lark-minute-token minute-token
                      'keymap lark-meetings--download-keymap)
          "\n"))

(defvar lark-meetings--download-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-meetings--download-at-point)
    (define-key map [mouse-1]   #'lark-meetings--download-at-point)
    map)
  "Keymap for download links in recording buffers.")

(defun lark-meetings--download-at-point ()
  "Download the recording at point."
  (interactive)
  (let ((token (get-text-property (point) 'lark-minute-token)))
    (unless token (user-error "No download link at point"))
    (lark-meetings-download-recording token)))

(defun lark-meetings-download-recording (minute-token &optional output-dir)
  "Download recording for MINUTE-TOKEN.
Optional OUTPUT-DIR specifies where to save; prompts if interactive."
  (interactive
   (list (read-string "Minute token: ")
         (read-directory-name "Save to: ")))
  (when (string-empty-p minute-token)
    (user-error "Minute token is required"))
  (message "Lark: downloading recording...")
  (let ((args (list "minutes" "+download" "--minute-tokens" minute-token)))
    (when (and output-dir (not (string-empty-p output-dir)))
      (setq args (append args (list "--output" (expand-file-name output-dir)))))
    (lark--run-command
     args
     (lambda (data)
       (let ((path (or (alist-get 'path data)
                       (alist-get 'output data)
                       (lark--get-nested data 'data 'path))))
         (message "Lark: recording downloaded%s"
                  (if path (format " → %s" path) "")))))))

(defun lark-meetings--insert-recording (recording)
  "Insert a structured section for a single RECORDING alist."
  (let ((known-keys '(meeting_id minute_token url recording_url
                      download_url status duration file_size
                      title topic start_time end_time error)))
    ;; Display known fields first in order
    (dolist (key known-keys)
      (let ((val (alist-get key recording)))
        (when val
          (let ((label (capitalize (replace-regexp-in-string "_" " " (symbol-name key))))
                (str (lark-meetings--format-value val)))
            (unless (string-empty-p str)
              (lark-meetings--insert-field label str))))))
    ;; Display remaining unknown fields
    (dolist (pair recording)
      (unless (memq (car pair) known-keys)
        (let ((label (capitalize (replace-regexp-in-string "_" " " (symbol-name (car pair)))))
              (val (cdr pair)))
          (cond
           ((and (listp val) (consp (car val)))
            ;; Nested alist — render as indented sub-fields
            (insert (propertize (format "  %-18s" (concat label ":")) 'face 'font-lock-keyword-face) "\n")
            (dolist (sub-pair val)
              (let ((sub-label (capitalize (replace-regexp-in-string "_" " " (symbol-name (car sub-pair)))))
                    (sub-val (lark-meetings--format-value (cdr sub-pair))))
                (unless (string-empty-p sub-val)
                  (insert (propertize (format "    %-16s" (concat sub-label ":")) 'face 'font-lock-comment-face)
                          sub-val "\n")))))
           (t
            (let ((str (lark-meetings--format-value val)))
              (unless (string-empty-p str)
                (lark-meetings--insert-field label str))))))))
    ;; Download link when minute_token is available
    (let ((mt (lark-meetings--format-value (alist-get 'minute_token recording))))
      (when (and mt (not (string-empty-p mt)))
        (insert "\n")
        (lark-meetings--insert-download-link mt)))))

;;;###autoload
(defun lark-meetings-recording (meeting-id)
  "Query recording / minute_token for MEETING-ID."
  (interactive "sMeeting ID: ")
  (when (string-empty-p meeting-id)
    (user-error "Meeting ID is required"))
  (message "Lark: querying recording...")
  (lark--run-command
   (list "vc" "+recording" "--meeting-ids" meeting-id)
   (lambda (data)
     (let* ((items (or (lark--get-nested data 'data 'items)
                       (lark--get-nested data 'data 'recordings)
                       (alist-get 'data data)))
            ;; Normalize: if items is a single alist (not a list of alists), wrap it
            (items (if (and items (listp items) (not (listp (car items))))
                       (list items)
                     items))
            (buf (get-buffer-create (format "*Lark Recording: %s*" meeting-id))))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (propertize (format "Recording: %s" meeting-id) 'face 'bold) "\n"
                   (make-string 60 ?─) "\n\n")
           (if (null items)
               (insert "(no recording found)\n")
             (let ((idx 0))
               (dolist (rec items)
                 (when (> idx 0)
                   (insert "\n" (make-string 40 ?─) "\n\n"))
                 (lark-meetings--insert-recording rec)
                 (setq idx (1+ idx))))))
         (special-mode)
         (goto-char (point-min)))
       (pop-to-buffer buf)))))

;;;; Copy helpers

(defun lark-meetings-copy-id ()
  "Copy the meeting ID at point."
  (interactive)
  (let ((id (lark-meetings--meeting-id-at-point)))
    (unless id (user-error "No meeting at point"))
    (kill-new id)
    (message "Copied: %s" id)))

(defun lark-meetings-copy-link ()
  "Copy the meeting app link at point."
  (interactive)
  (let ((meeting (lark-meetings--meeting-at-point)))
    (unless meeting (user-error "No meeting at point"))
    (let ((link (lark-meetings--meeting-app-link meeting)))
      (if (string-empty-p link)
          (user-error "No link for this meeting")
        (kill-new link)
        (message "Copied: %s" link)))))

;;;; Transient dispatch

;;;###autoload (autoload 'lark-meetings-dispatch "lark-meetings" nil t)
(transient-define-prefix lark-meetings-dispatch ()
  "Lark Meetings commands."
  ["Search"
   ("s" "Search meetings"   lark-meetings-search)]
  ["At Point"
   ("RET" "Open detail"     lark-meetings-open)
   ("N"   "Meeting notes"   lark-meetings-notes-at-point)]
  ["By ID"
   ("n" "Notes by ID"       lark-meetings-notes)
   ("r" "Recording by ID"   lark-meetings-recording)])

(provide 'lark-meetings)
;;; lark-meetings.el ends here
