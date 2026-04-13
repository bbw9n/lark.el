;;; lark-calendar.el --- Lark Calendar integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Provides Calendar domain commands for lark.el: agenda view, event
;; listing, event detail, event creation, free/busy checks, and time
;; suggestions.  All data is fetched from lark-cli asynchronously and
;; displayed in tabulated-list buffers.

;;; Code:

(require 'lark-core)
(require 'transient)

;;;; Customization

(defgroup lark-calendar nil
  "Lark Calendar settings."
  :group 'lark
  :prefix "lark-calendar-")

(defcustom lark-calendar-default-days 1
  "Number of days to show in the agenda view."
  :type 'integer
  :group 'lark-calendar)

;;;; Buffer-local variables

(defvar-local lark-calendar--events nil
  "Cached event data for the current buffer.")

(defvar-local lark-calendar--calendar-id nil
  "Calendar ID for the current buffer, or nil for primary.")

;;;; Event parsing

(defun lark-calendar--parse-event-time (event)
  "Extract a display time string from EVENT."
  (let* ((start (or (alist-get 'start_time event)
                    (lark--get-nested event 'start 'timestamp)
                    (lark--get-nested event 'start 'date_time)
                    (lark--get-nested event 'start 'date)))
         (end (or (alist-get 'end_time event)
                  (lark--get-nested event 'end 'timestamp)
                  (lark--get-nested event 'end 'date_time)
                  (lark--get-nested event 'end 'date)))
         (start-str (if (numberp start)
                        (lark--format-timestamp start)
                      (if (stringp start)
                          (lark-calendar--format-datetime start)
                        "")))
         (end-str (if (numberp end)
                      (lark--format-timestamp end)
                    (if (stringp end)
                        (lark-calendar--format-datetime end)
                      ""))))
    (if (and start-str end-str
             (string-prefix-p (substring start-str 0 (min 10 (length start-str)))
                              end-str))
        ;; Same day: show "YYYY-MM-DD HH:MM - HH:MM"
        (format "%s - %s" start-str (substring end-str (min 11 (length end-str))))
      (format "%s - %s" start-str end-str))))

(defun lark-calendar--format-datetime (str)
  "Format a datetime STR for display (pass through if already readable)."
  (if (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" str)
      (substring str 0 (min 16 (length str)))
    str))

(defun lark-calendar--event-id (event)
  "Extract the event ID from EVENT."
  (or (alist-get 'event_id event)
      (alist-get 'id event)))

(defun lark-calendar--event-title (event)
  "Extract the title from EVENT."
  (or (alist-get 'summary event)
      (alist-get 'title event)
      "(no title)"))

(defun lark-calendar--event-location (event)
  "Extract the location from EVENT."
  (let ((loc (alist-get 'location event)))
    (cond
     ((stringp loc) loc)
     ((listp loc) (or (alist-get 'name loc) ""))
     (t ""))))

(defun lark-calendar--event-status (event)
  "Extract the status/RSVP from EVENT."
  (or (alist-get 'status event)
      (alist-get 'self_attendee_status event)
      ""))

(defun lark-calendar--extract-events (data)
  "Extract the event list from lark-cli response DATA."
  (cond
   ((and (listp data) (alist-get 'items data))
    (alist-get 'items data))
   ((and (listp data) (alist-get 'events data))
    (alist-get 'events data))
   ((and (listp data) (alist-get 'data data))
    (let ((inner (alist-get 'data data)))
      (or (alist-get 'items inner)
          (alist-get 'events inner)
          (and (listp inner) inner))))
   ((and (listp data) (listp (car data))
         (or (alist-get 'event_id (car data))
             (alist-get 'summary (car data))))
    data)
   (t nil)))

(defun lark-calendar--make-entries (events)
  "Convert EVENTS to `tabulated-list-entries' format."
  (mapcar
   (lambda (event)
     (let ((id (lark-calendar--event-id event))
           (time (lark-calendar--parse-event-time event))
           (title (lark-calendar--event-title event))
           (location (lark-calendar--event-location event))
           (status (lark-calendar--event-status event)))
       (list id (vector time title location status))))
   events))

;;;; Tabulated list mode

(defvar lark-calendar-events-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-calendar-event-open)
    (define-key map (kbd "g")   #'lark-calendar-agenda-refresh)
    (define-key map (kbd "c")   #'lark-calendar-create-event)
    (define-key map (kbd "d")   #'lark-calendar-event-delete)
    (define-key map (kbd "y")   #'lark-calendar-event-copy-id)
    (define-key map (kbd "?")   #'lark-calendar-dispatch)
    map)
  "Keymap for `lark-calendar-events-mode'.")

(define-derived-mode lark-calendar-events-mode tabulated-list-mode
  "Lark Events"
  "Major mode for browsing Lark calendar events."
  (setq tabulated-list-format
        [("Time" 24 t)
         ("Title" 40 t)
         ("Location" 20 t)
         ("Status" 10 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;; Agenda

;;;###autoload
(defun lark-calendar-agenda ()
  "Show today's Lark calendar agenda."
  (interactive)
  (message "Lark: fetching agenda...")
  (lark--run-command
   '("calendar" "+agenda")
   #'lark-calendar--display-agenda))

(defun lark-calendar-agenda-refresh ()
  "Refresh the current agenda buffer."
  (interactive)
  (if lark-calendar--calendar-id
      (lark-calendar-events lark-calendar--calendar-id)
    (lark-calendar-agenda)))

(defun lark-calendar--display-agenda (data)
  "Display agenda DATA in a tabulated list buffer."
  (let* ((events (lark-calendar--extract-events data))
         (buf (get-buffer-create "*Lark Agenda*")))
    (with-current-buffer buf
      (lark-calendar-events-mode)
      (setq lark-calendar--events events
            lark-calendar--calendar-id nil
            tabulated-list-entries (lark-calendar--make-entries events))
      (tabulated-list-print t)
      (setq header-line-format
            (format " Lark Agenda — %d event(s)" (length events))))
    (pop-to-buffer buf)))

;;;; Event listing

;;;###autoload
(defun lark-calendar-events (&optional calendar-id)
  "List events for CALENDAR-ID (or primary calendar)."
  (interactive)
  (message "Lark: fetching events...")
  (let ((args (if calendar-id
                  (list "calendar" "events" "list" "--calendar-id" calendar-id)
                '("calendar" "events" "list"))))
    (lark--run-command
     args
     (lambda (data)
       (lark-calendar--display-events data calendar-id)))))

(defun lark-calendar--display-events (data &optional calendar-id)
  "Display event list DATA for CALENDAR-ID."
  (let* ((events (lark-calendar--extract-events data))
         (buf (get-buffer-create (if calendar-id
                                     (format "*Lark Events: %s*" calendar-id)
                                   "*Lark Events*"))))
    (with-current-buffer buf
      (lark-calendar-events-mode)
      (setq lark-calendar--events events
            lark-calendar--calendar-id calendar-id
            tabulated-list-entries (lark-calendar--make-entries events))
      (tabulated-list-print t)
      (setq header-line-format
            (format " Lark Events — %d event(s)" (length events))))
    (pop-to-buffer buf)))

;;;; Calendar listing

;;;###autoload
(defun lark-calendar-list ()
  "List available Lark calendars."
  (interactive)
  (message "Lark: fetching calendars...")
  (lark--run-command
   '("calendar" "list")
   #'lark-calendar--display-calendars))

(defun lark-calendar--display-calendars (data)
  "Display calendar list DATA."
  (let* ((calendars (or (alist-get 'items data)
                        (lark--get-nested data 'data 'items)
                        (and (listp data) data)))
         (buf (get-buffer-create "*Lark Calendars*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Lark Calendars\n" 'face 'bold)
                (make-string 60 ?─) "\n\n")
        (if (null calendars)
            (insert "(no calendars found)\n")
          (dolist (cal calendars)
            (let ((id (or (alist-get 'calendar_id cal) (alist-get 'id cal)))
                  (name (or (alist-get 'summary cal) (alist-get 'name cal) ""))
                  (desc (or (alist-get 'description cal) ""))
                  (role (or (alist-get 'role cal) "")))
              (insert (propertize name 'face 'bold) "\n"
                      "  ID: " (or id "") "\n"
                      (if (string-empty-p desc) "" (format "  Description: %s\n" desc))
                      (if (string-empty-p role) "" (format "  Role: %s\n" role))
                      "\n")))))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;; Event detail

(defun lark-calendar-event-open ()
  "Open the event at point in a detail view."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No event at point"))
    (message "Lark: fetching event details...")
    (lark--run-command
     (list "calendar" "events" "get" "--event-id" id)
     (lambda (data)
       (lark-calendar--display-event-detail data id)))))

(defun lark-calendar--display-event-detail (data event-id)
  "Display event detail DATA for EVENT-ID."
  (let* ((event (or (alist-get 'data data) data))
         (title (lark-calendar--event-title event))
         (buf (get-buffer-create (format "*Lark Event: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize title 'face 'bold) "\n"
                (make-string (min 60 (max 20 (length title))) ?─) "\n\n")
        ;; Time
        (let ((time-str (lark-calendar--parse-event-time event)))
          (unless (string-empty-p time-str)
            (insert (propertize "Time: " 'face 'bold) time-str "\n")))
        ;; Location
        (let ((loc (lark-calendar--event-location event)))
          (unless (string-empty-p loc)
            (insert (propertize "Location: " 'face 'bold) loc "\n")))
        ;; Organizer
        (let ((org (or (alist-get 'organizer_name event)
                       (lark--get-nested event 'organizer 'display_name)
                       (lark--get-nested event 'organizer 'name))))
          (when org
            (insert (propertize "Organizer: " 'face 'bold) org "\n")))
        ;; Status
        (let ((status (lark-calendar--event-status event)))
          (unless (string-empty-p status)
            (insert (propertize "Status: " 'face 'bold) status "\n")))
        ;; Description
        (let ((desc (or (alist-get 'description event) "")))
          (unless (string-empty-p desc)
            (insert "\n" (propertize "Description" 'face 'bold) "\n"
                    (make-string 40 ?─) "\n"
                    desc "\n")))
        ;; Attendees
        (let ((attendees (or (alist-get 'attendees event) nil)))
          (when attendees
            (insert "\n" (propertize "Attendees" 'face 'bold) "\n"
                    (make-string 40 ?─) "\n")
            (dolist (att attendees)
              (let ((name (or (alist-get 'display_name att)
                              (alist-get 'name att) ""))
                    (rsvp (or (alist-get 'rsvp_status att)
                              (alist-get 'status att) "")))
                (insert "  " name
                        (if (string-empty-p rsvp) "" (format " (%s)" rsvp))
                        "\n")))))
        ;; Event ID
        (insert "\n" (propertize "Event ID: " 'face 'font-lock-comment-face)
                (or event-id "") "\n"))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;; Event creation

;;;###autoload (autoload 'lark-calendar-create-event "lark-calendar" nil t)
(transient-define-prefix lark-calendar-create-event ()
  "Create a new Lark calendar event."
  ["Event Details"
   ("t" "Title"       "--title=" :prompt "Event title: ")
   ("s" "Start time"  "--start-time=" :prompt "Start (YYYY-MM-DD HH:MM): ")
   ("e" "End time"    "--end-time=" :prompt "End (YYYY-MM-DD HH:MM): ")
   ("l" "Location"    "--location=" :prompt "Location: ")
   ("d" "Description" "--description=" :prompt "Description: ")]
  ["Attendees"
   ("a" "Attendees"   "--attendees=" :prompt "Attendees (comma-separated): ")]
  ["Actions"
   ("RET" "Create"    lark-calendar--do-create-event)
   ("q"   "Cancel"    transient-quit-all)])

(defun lark-calendar--do-create-event (&rest _args)
  "Execute event creation with transient arguments."
  (interactive)
  (let* ((args (transient-args 'lark-calendar-create-event))
         (cmd-args '("calendar" "+create")))
    (unless args
      (user-error "No event details provided"))
    (message "Lark: creating event...")
    (lark--run-command
     (append cmd-args args)
     (lambda (data)
       (let ((id (or (lark--get-nested data 'data 'event_id)
                     (alist-get 'event_id data)
                     (alist-get 'id data))))
         (message "Lark: event created%s"
                  (if id (format " (ID: %s)" id) ""))))
     nil
     :format "json")))

;;;; Event deletion

(defun lark-calendar-event-delete ()
  "Delete the event at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No event at point"))
    (when (yes-or-no-p (format "Delete event %s? " id))
      (message "Lark: deleting event...")
      (lark--run-command
       (list "calendar" "events" "delete" "--event-id" id)
       (lambda (_data)
         (message "Lark: event deleted")
         (lark-calendar-agenda-refresh))))))

;;;; Copy event ID

(defun lark-calendar-event-copy-id ()
  "Copy the event ID at point to the kill ring."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No event at point"))
    (kill-new id)
    (message "Copied: %s" id)))

;;;; Free/busy

;;;###autoload
(defun lark-calendar-freebusy (user-id)
  "Check free/busy status for USER-ID."
  (interactive "sUser ID (or email): ")
  (message "Lark: checking free/busy...")
  (lark--run-command
   (list "calendar" "+freebusy" "--user-id" user-id)
   #'lark-calendar--display-freebusy))

(defun lark-calendar--display-freebusy (data)
  "Display free/busy DATA."
  (let ((buf (get-buffer-create "*Lark Free/Busy*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Free/Busy\n" 'face 'bold)
                (make-string 60 ?─) "\n\n"
                (pp-to-string data)))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;; Time suggestion

;;;###autoload
(defun lark-calendar-suggest-time ()
  "Get time suggestions for a meeting."
  (interactive)
  (message "Lark: fetching time suggestions...")
  (lark--run-command
   '("calendar" "+suggestion")
   (lambda (data)
     (let ((buf (get-buffer-create "*Lark Time Suggestions*")))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (propertize "Time Suggestions\n" 'face 'bold)
                   (make-string 60 ?─) "\n\n")
           (let ((suggestions (or (alist-get 'suggestions data)
                                  (lark--get-nested data 'data 'suggestions)
                                  (and (listp data) data))))
             (if (null suggestions)
                 (insert "(no suggestions available)\n")
               (dolist (s suggestions)
                 (let ((start (or (alist-get 'start_time s)
                                  (lark--get-nested s 'start 'timestamp)))
                       (end (or (alist-get 'end_time s)
                                (lark--get-nested s 'end 'timestamp))))
                   (insert (format "  %s  -  %s\n"
                                   (or (lark--format-timestamp start) (format "%s" start))
                                   (or (lark--format-timestamp end) (format "%s" end)))))))))
         (special-mode)
         (goto-char (point-min)))
       (pop-to-buffer buf)))))

;;;; RSVP

;;;###autoload
(defun lark-calendar-rsvp (event-id status)
  "RSVP to EVENT-ID with STATUS (accept/decline/tentative)."
  (interactive
   (list (or (tabulated-list-get-id)
             (read-string "Event ID: "))
         (completing-read "RSVP: " '("accept" "decline" "tentative") nil t)))
  (lark--run-command
   (list "calendar" "+rsvp" "--event-id" event-id "--status" status)
   (lambda (_data)
     (message "Lark: RSVP'd %s to event %s" status event-id))))

;;;; Transient dispatch

;;;###autoload (autoload 'lark-calendar-dispatch "lark-calendar" nil t)
(transient-define-prefix lark-calendar-dispatch ()
  "Lark Calendar commands."
  ["Quick Actions"
   ("a" "Agenda (today)" lark-calendar-agenda)
   ("n" "New event"      lark-calendar-create-event)
   ("f" "Free/busy"      lark-calendar-freebusy)
   ("s" "Suggest time"   lark-calendar-suggest-time)
   ("r" "RSVP"           lark-calendar-rsvp)]
  ["Browse"
   ("l" "List calendars" lark-calendar-list)
   ("e" "List events"    lark-calendar-events)])

(provide 'lark-calendar)
;;; lark-calendar.el ends here
