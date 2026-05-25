;;; lark-ui.el --- Shared UI helpers for lark.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; Presentation-layer helpers shared across lark.el domains, kept
;; separate from the process/IO engine in `lark-core'.
;;
;; Currently this provides a small, render-target-agnostic loading
;; spinner.  `lark-spinner-start' cycles through frames on a timer and
;; calls a render function with each frame string; the render function
;; decides what to do with it — update an in-buffer line, a header-line,
;; the echo area, etc.  Domains build their own spinners on top of this;
;; `lark-spinner-message' is a ready-made echo-area spinner for the
;; common "fetching…" case.

;;; Code:

;;;; Loading spinner

(defconst lark-spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Default braille frames cycled by lark.el loading spinners.")

(defun lark-spinner-start (render-fn &optional interval frames max-ticks on-stop)
  "Animate a loading spinner by repeatedly calling RENDER-FN.

RENDER-FN is called with one argument, the current frame string (an
element of FRAMES).  It is invoked immediately with the first frame and
then once per tick.  If RENDER-FN returns nil (or signals), the spinner
stops — callers use this to signal completion (loading finished, the
target buffer died, etc.).

INTERVAL   seconds between frames (default 0.1).
FRAMES     vector of frame strings (default `lark-spinner-frames').
MAX-TICKS  safety cap on total frames before auto-stopping (default 300,
           ~30s at the default interval) — guards against runaway timers
           when an async operation errors without ever signalling done.
ON-STOP    optional 0-arg function run when the spinner stops on its own
           (RENDER-FN returned nil or MAX-TICKS was hit).  It is NOT run
           when the spinner is stopped explicitly via `lark-spinner-stop'.

Returns a spinner handle; pass it to `lark-spinner-stop'."
  (let* ((frames (or frames lark-spinner-frames))
         (count (length frames))
         (interval (or interval 0.1))
         (max-ticks (or max-ticks 300))
         (index 0)
         timer)
    (if (not (ignore-errors (funcall render-fn (aref frames 0))))
        (when on-stop (ignore-errors (funcall on-stop)))
      (setq timer
            (run-with-timer
             interval interval
             (lambda ()
               (setq index (1+ index))
               (if (and (< index max-ticks)
                        (ignore-errors
                          (funcall render-fn (aref frames (mod index count)))))
                   nil
                 (when (timerp timer) (cancel-timer timer))
                 (when on-stop (ignore-errors (funcall on-stop))))))))
    timer))

(defun lark-spinner-stop (spinner)
  "Stop SPINNER, a handle returned by `lark-spinner-start'.
Does not invoke the spinner's ON-STOP callback (an explicit stop is the
caller's own action).  Safe to call with nil or an already-stopped
spinner."
  (when (timerp spinner)
    (cancel-timer spinner)))

(defun lark-spinner-message (format-string &rest args)
  "Show an echo-area loading spinner prefixed to a message.
FORMAT-STRING and ARGS are formatted like `message'; the current
spinner frame is prepended, e.g. \"⠋ Fetching messages…\".

The spinner refreshes the echo area every tick but logs nothing to
*Messages*, and stops itself if the echo area is taken over by an
unrelated message or while the minibuffer is active, so it never
clobbers other output.  Returns a spinner handle; call
`lark-spinner-stop' on it when the operation finishes."
  (let ((text (apply #'format format-string args))
        (last nil))
    (lark-spinner-start
     (lambda (frame)
       (cond
        ((active-minibuffer-window) nil)
        ((and last (current-message) (not (equal (current-message) last)))
         nil)
        (t
         (setq last (format "%s %s" frame text))
         (let ((message-log-max nil))
           (message "%s" last))
         t))))))

(provide 'lark-ui)
;;; lark-ui.el ends here
