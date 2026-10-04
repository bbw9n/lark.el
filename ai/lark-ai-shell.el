;;; lark-ai-shell.el --- shell-maker front-end for the lark AI session -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; A comint-style conversational front-end for the lark AI engine,
;; built on `shell-maker' (the engine under chatgpt-shell/agent-shell).
;; You get native shell ergonomics for free: prompt, input history
;; (M-p/M-n), history search, transcript saving, busy state.
;;
;; The engine is untouched: skills routing, the agent loop, the ACP
;; backend and session/history semantics are exactly those of
;; `lark-ai-ask'.  The loop's UI callbacks are redirected into the
;; shell via the `lark-ai--frontend' dispatch seam in lark-ai.el:
;; progress lines render dimmed, finished tool calls render as
;; one-line ✓/✗ entries, and the final answer is written as the
;; command output.
;;
;; shell-maker is a soft dependency: this file loads without it;
;; `lark-ai-shell' errors with instructions when it is missing.
;;
;; One turn may be in flight at a time (across shell and classic UI
;; alike — the engine is a singleton today).  C-c C-k aborts.

;;; Code:

(require 'map)
(require 'lark-ai)   ; engine + `lark-ai--frontend' dispatch seam

;; shell-maker forward declarations — soft-required in `lark-ai-shell'.
(declare-function shell-maker-start "ext:shell-maker")
(declare-function shell-maker-submit "ext:shell-maker")
(declare-function shell-maker-interrupt "ext:shell-maker")
(declare-function make-shell-maker-config "ext:shell-maker")

(defconst lark-ai-shell--buffer-name "*Lark AI Shell*")

(defvar-local lark-ai-shell--session nil
  "The `lark-ai-session' for this shell buffer.")

(defvar-local lark-ai-shell--context nil
  "Originating-buffer context captured when the shell was opened.")

;;;; Entry point

;;;###autoload
(defun lark-ai-shell ()
  "Open the Lark AI session as a comint-style shell.
Context is captured from the buffer this command is invoked in,
exactly like `lark-ai-ask' — open the shell from a doc or chat
buffer and the turn sees that content."
  (interactive)
  (unless (require 'shell-maker nil t)
    (user-error "lark-ai-shell needs the `shell-maker' package (install from MELPA)"))
  (let* ((context (lark-ai-context-format))
         (buf (shell-maker-start
               (make-shell-maker-config
                :name "lark-ai"
                :prompt "Lark AI> "
                :prompt-regexp "^Lark AI> "
                :execute-command #'lark-ai-shell--execute)
               nil
               (lambda (_config)
                 (propertize
                  (format "Lark AI shell — backend: %s.  C-c C-k aborts a turn.\n"
                          lark-ai-backend)
                  'font-lock-face 'shadow))
               nil lark-ai-shell--buffer-name "Lark AI")))
    (with-current-buffer buf
      ;; Refresh context on every (re)entry so "open shell from a doc"
      ;; always reflects the doc the user just came from.
      (when (and context (not (string-empty-p context)))
        (setq-local lark-ai-shell--context context))
      (unless lark-ai-shell--session
        (setq-local lark-ai-shell--session (make-lark-ai-session)))
      (local-set-key (kbd "C-c C-k") #'lark-ai-shell-abort))
    buf))

;;;###autoload
(defun lark-ai-shell-ask (prompt)
  "Open the Lark AI shell and submit PROMPT as a turn.
Context is captured from the invoking buffer, exactly like
`lark-ai-ask' — this is what `lark-ai-ask' routes to when
`lark-ai-interface' is `shell'."
  (interactive "sLark AI: ")
  (let ((buf (lark-ai-shell)))
    (with-current-buffer buf
      (shell-maker-submit :input prompt))
    buf))

;;;; Turn execution

(defun lark-ai-shell--execute (input shell)
  "shell-maker executor: run INPUT through the lark agent loop.
SHELL is the callback alist shell-maker hands to executors."
  (let* ((buf (map-elt shell :buffer))
         (session (with-current-buffer buf
                    (or lark-ai-shell--session
                        (setq-local lark-ai-shell--session
                                    (make-lark-ai-session)))))
         (context (or (buffer-local-value 'lark-ai-shell--context buf) ""))
         ;; Snapshot BEFORE pushing the current prompt, mirroring
         ;; `lark-ai-ask' — the user message is built from prior turns.
         (history (lark-ai-session-history session)))
    (if lark-ai--frontend
        (progn
          (funcall (map-elt shell :write-output)
                   "A turn is already in flight (C-c C-k aborts it).\n")
          (funcall (map-elt shell :finish-output) nil))
      (push (cons "user" input) (lark-ai-session-history session))
      (setq lark-ai--frontend (lark-ai-shell--make-frontend shell session))
      (lark-ai--select-skills
       input (or (lark-ai-skills-routing-context context) "")
       (lambda (skills)
         (lark-ai-agent--run input context history session skills))))))

(defun lark-ai-shell--make-frontend (shell session)
  "Build the `lark-ai--frontend' callback plist writing into SHELL.
SESSION receives the history bookkeeping the classic UI would do."
  (let ((write (map-elt shell :write-output))
        (finish (map-elt shell :finish-output)))
    (list
     :progress-log
     (lambda (fmt &rest args)
       (funcall write
                (propertize (format "· %s\n" (apply #'format fmt args))
                            'font-lock-face 'shadow)))
     :render-tool-call
     (lambda (_iter cmd status)
       ;; One line per finished action; the running state is already
       ;; narrated by the progress log.
       (unless (eq status 'running)
         (funcall write
                  (concat
                   (propertize (pcase status ('done "✓") ('error "✗") (_ "–"))
                               'font-lock-face
                               (pcase status ('done 'success)
                                      ('error 'error) (_ 'shadow)))
                   " "
                   (propertize
                    (concat "lark-cli "
                            (string-join (lark-ai-agent-abbreviate-cmd cmd) " "))
                    'font-lock-face 'font-lock-function-name-face)
                   "\n"))))
     ;; The raw action JSON stream is noise in a shell; the busy state
     ;; plus progress lines carry liveness.  Must be non-nil — a nil
     ;; handler would make the LLM layer stream into the classic buffer.
     :stream-preview (lambda () #'ignore)
     :clear-waiting #'ignore
     :present
     (lambda (content skip-history)
       (unless skip-history
         (push (cons "assistant" content)
               (lark-ai-session-history session)))
       (setf (lark-ai-session-phase session) 'done)
       (setq lark-ai--frontend nil)
       (funcall write (concat "\n" content "\n"))
       (funcall finish t)))))

;;;; Abort

(defun lark-ai-shell-abort ()
  "Abort the in-flight shell turn and release the engine."
  (interactive)
  (when (fboundp 'lark-ai-acp-abort)
    (ignore-errors (lark-ai-acp-abort)))
  (when lark-ai-shell--session
    (setf (lark-ai-session-phase lark-ai-shell--session) 'idle))
  (setq lark-ai--frontend nil)
  (when (fboundp 'shell-maker-interrupt)
    (call-interactively #'shell-maker-interrupt))
  (message "Lark AI shell: aborted"))

(provide 'lark-ai-shell)
;;; lark-ai-shell.el ends here
