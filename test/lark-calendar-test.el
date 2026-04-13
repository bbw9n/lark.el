;;; lark-calendar-test.el --- Tests for lark-calendar.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'lark-calendar)

;;;; Event parsing tests

(ert-deftest lark-calendar-test-event-id ()
  "Extract event ID from various formats."
  (should (equal (lark-calendar--event-id '((event_id . "ev_123")))
                 "ev_123"))
  (should (equal (lark-calendar--event-id '((id . "456")))
                 "456")))

(ert-deftest lark-calendar-test-event-title ()
  "Extract event title."
  (should (equal (lark-calendar--event-title '((summary . "Meeting")))
                 "Meeting"))
  (should (equal (lark-calendar--event-title '((title . "Standup")))
                 "Standup"))
  (should (equal (lark-calendar--event-title '((foo . "bar")))
                 "(no title)")))

(ert-deftest lark-calendar-test-event-location ()
  "Extract event location."
  (should (equal (lark-calendar--event-location '((location . "Room A")))
                 "Room A"))
  (should (equal (lark-calendar--event-location
                  '((location . ((name . "Room B")))))
                 "Room B"))
  (should (equal (lark-calendar--event-location '((foo . "bar")))
                 "")))

(ert-deftest lark-calendar-test-event-status ()
  "Extract event status."
  (should (equal (lark-calendar--event-status '((status . "accepted")))
                 "accepted"))
  (should (equal (lark-calendar--event-status '((foo . "bar")))
                 "")))

;;;; Extract events from actual response shapes

(ert-deftest lark-calendar-test-extract-events-agenda ()
  "Extract events from +agenda shape: {ok: t, data: [<events>]}."
  (let ((data '((ok . t)
                (data . (((event_id . "1") (summary . "A"))
                          ((event_id . "2") (summary . "B")))))))
    (should (= (length (lark-calendar--extract-events data)) 2))))

(ert-deftest lark-calendar-test-extract-events-raw-api ()
  "Extract events from raw API shape: {code: 0, data: {items: [...]}}."
  (let ((data '((code . 0)
                (data . ((items . (((event_id . "1") (summary . "A")))))))))
    (should (= (length (lark-calendar--extract-events data)) 1))))

(ert-deftest lark-calendar-test-extract-events-empty-data ()
  "Empty data array returns nil."
  (let ((data '((ok . t) (data))))
    (should (null (lark-calendar--extract-events data)))))

(ert-deftest lark-calendar-test-extract-events-nil ()
  "Nil data returns nil."
  (should (null (lark-calendar--extract-events nil))))

;;;; Field extractors

(ert-deftest lark-calendar-test-event-organizer ()
  "Extract organizer from event."
  ;; Prefer event_organizer.display_name
  (should (equal (lark-calendar--event-organizer
                  '((event_organizer . ((display_name . "Alice")))))
                 "Alice"))
  ;; Fall back to organizer_calendar_id
  (should (equal (lark-calendar--event-organizer
                  '((organizer_calendar_id . "cal@group")))
                 "cal@group"))
  ;; Empty when neither present
  (should (equal (lark-calendar--event-organizer '((foo . "bar"))) "")))

(ert-deftest lark-calendar-test-event-meeting-url ()
  "Extract meeting URL from event."
  (should (equal (lark-calendar--event-meeting-url
                  '((vchat . ((meeting_url . "https://vc.example.com/123")))))
                 "https://vc.example.com/123"))
  (should (equal (lark-calendar--event-meeting-url '((foo . "bar"))) "")))

(ert-deftest lark-calendar-test-event-start-end ()
  "Extract start/end times from event."
  (let ((event '((start_time . ((datetime . "2026-04-11T09:00:00+08:00")))
                 (end_time . ((datetime . "2026-04-11T09:30:00+08:00"))))))
    (should (equal (lark-calendar--event-start event) "2026-04-11T09:00"))
    (should (equal (lark-calendar--event-end event) "2026-04-11T09:30"))))

;;;; Section rendering

(ert-deftest lark-calendar-test-insert-event ()
  "Insert event renders all fields with text property."
  (let ((event '((event_id . "ev1")
                 (summary . "Standup")
                 (start_time . ((datetime . "2026-04-11T09:00:00+08:00")))
                 (end_time . ((datetime . "2026-04-11T09:30:00+08:00")))
                 (location . "Room 1")
                 (self_rsvp_status . "accept")
                 (vchat . ((meeting_url . "https://vc.example.com/1"))))))
    (with-temp-buffer
      (lark-calendar--insert-event event)
      (let ((text (buffer-string)))
        (should (string-match-p "Standup" text))
        (should (string-match-p "2026-04-11T09:00" text))
        (should (string-match-p "Room 1" text))
        (should (string-match-p "accept" text))
        (should (string-match-p "vc.example.com" text)))
      ;; Check text property
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'lark-event-id) "ev1")))))

;;;; Time resolution

(ert-deftest lark-calendar-test-resolve-time-datetime-object ()
  "Resolve {datetime: ...} object."
  (should (equal (lark-calendar--resolve-time
                  '((datetime . "2026-04-11T09:00:00+08:00")))
                 "2026-04-11T09:00")))

(ert-deftest lark-calendar-test-resolve-time-string ()
  "Resolve plain datetime string."
  (should (equal (lark-calendar--resolve-time "2026-04-11T10:00:00")
                 "2026-04-11T10:00")))

(ert-deftest lark-calendar-test-resolve-time-number ()
  "Resolve numeric timestamp."
  (let ((result (lark-calendar--resolve-time 1700000000)))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest lark-calendar-test-resolve-time-nil ()
  "Resolve nil returns empty string."
  (should (equal (lark-calendar--resolve-time nil) "")))

;;;; Format datetime

(ert-deftest lark-calendar-test-format-datetime ()
  "Format datetime strings."
  (should (equal (lark-calendar--format-datetime "2026-04-11T10:00:00")
                 "2026-04-11T10:00"))
  (should (equal (lark-calendar--format-datetime "2026-04-11")
                 "2026-04-11"))
  (should (equal (lark-calendar--format-datetime "not a date")
                 "not a date")))

(provide 'lark-calendar-test)
;;; lark-calendar-test.el ends here
