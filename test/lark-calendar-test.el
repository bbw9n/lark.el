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

;;;; Make entries

(ert-deftest lark-calendar-test-make-entries ()
  "Convert events to tabulated-list entries."
  (let* ((events '(((event_id . "ev1")
                     (summary . "Standup")
                     (start_time . ((datetime . "2026-04-11T09:00:00+08:00")))
                     (end_time . ((datetime . "2026-04-11T09:30:00+08:00")))
                     (location . "Room 1")
                     (self_rsvp_status . "accepted"))))
         (entries (lark-calendar--make-entries events)))
    (should (= (length entries) 1))
    (should (equal (car (car entries)) "ev1"))
    (let ((vec (cadr (car entries))))
      (should (stringp (aref vec 0)))      ; time
      (should (equal (aref vec 1) "Standup"))
      (should (equal (aref vec 2) "Room 1"))
      (should (equal (aref vec 3) "accepted")))))

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
