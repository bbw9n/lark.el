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

;;;; Extract events from various response shapes

(ert-deftest lark-calendar-test-extract-events-items ()
  "Extract events from {items: [...]} shape."
  (let ((data '((items . (((event_id . "1") (summary . "A"))
                           ((event_id . "2") (summary . "B")))))))
    (should (= (length (lark-calendar--extract-events data)) 2))))

(ert-deftest lark-calendar-test-extract-events-nested ()
  "Extract events from {data: {items: [...]}} shape."
  (let ((data '((data . ((items . (((event_id . "1")))))))))
    (should (= (length (lark-calendar--extract-events data)) 1))))

(ert-deftest lark-calendar-test-extract-events-flat ()
  "Extract events from a flat list."
  (let ((data '(((event_id . "1") (summary . "A")))))
    (should (= (length (lark-calendar--extract-events data)) 1))))

(ert-deftest lark-calendar-test-extract-events-empty ()
  "Nil data returns nil."
  (should (null (lark-calendar--extract-events nil))))

;;;; Make entries

(ert-deftest lark-calendar-test-make-entries ()
  "Convert events to tabulated-list entries."
  (let* ((events '(((event_id . "ev1")
                     (summary . "Standup")
                     (start_time . 1700000000)
                     (end_time . 1700003600)
                     (location . "Room 1")
                     (status . "accepted"))))
         (entries (lark-calendar--make-entries events)))
    (should (= (length entries) 1))
    (should (equal (car (car entries)) "ev1"))
    (let ((vec (cadr (car entries))))
      (should (stringp (aref vec 0)))      ; time
      (should (equal (aref vec 1) "Standup"))
      (should (equal (aref vec 2) "Room 1"))
      (should (equal (aref vec 3) "accepted")))))

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
