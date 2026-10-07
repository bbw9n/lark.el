;;; lark-ai-shell.el --- shell-maker front-end for the lark AI session -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; Assisted-by: Claude:claude-opus-5
;; SPDX-License-Identifier: GPL-3.0-or-later

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
(declare-function shell-maker-define-major-mode "ext:shell-maker")
(declare-function shell-maker-start "ext:shell-maker")
(declare-function shell-maker-submit "ext:shell-maker")
(declare-function shell-maker-interrupt "ext:shell-maker")
(declare-function make-shell-maker-config "ext:shell-maker")
(declare-function markdown-overlays-put "ext:markdown-overlays")

(defconst lark-ai-shell--buffer-name "*Lark AI Shell*")

(defcustom lark-ai-shell-display-action
  '((display-buffer-reuse-window display-buffer-in-side-window)
    (side . right)
    (window-width . 0.42))
  "`display-buffer' action for showing the Lark AI shell.
The default opens it as a vertical side panel on the right at 42%
of the frame width, reusing an existing window showing it.
shell-maker's own display (`shell-maker-display-function', which
defaults to taking over the current window) is bypassed so this
stays local to the lark shell.  Set to nil to fall back to plain
`pop-to-buffer' behavior."
  :type 'sexp
  :group 'lark-ai)

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
    (user-error "The lark-ai-shell command needs the `shell-maker' package (install from MELPA)"))
  (let* ((context (lark-ai-context-format))
         (config (make-shell-maker-config
                  :name "lark-ai"
                  :prompt "Lark AI> "
                  :prompt-regexp "^Lark AI> "
                  :execute-command #'lark-ai-shell--execute))
         ;; NO-FOCUS: skip shell-maker's own display (it takes over the
         ;; current window); we place the buffer ourselves below.
         (buf (shell-maker-start
               ;; Defining the mode first gives it a keymap under
               ;; `shell-maker-mode-map', where RET is remapped to
               ;; `shell-maker-submit'; without it RET never sends.
               (progn (shell-maker-define-major-mode config) config)
               t
               (lambda (_config)
                 (propertize
                  (format "Lark AI shell — backend: %s.  /brief <topic> builds a topic brief.  C-c C-k aborts a turn.\n"
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
    ;; Display as a side panel (see `lark-ai-shell-display-action').
    ;; Batch sessions (tests) just make it current.
    (if noninteractive
        (set-buffer buf)
      (pop-to-buffer buf lark-ai-shell-display-action))
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

;;;###autoload
(defun lark-ai-shell-brief (topic)
  "Open the Lark AI shell and build a cross-domain brief on TOPIC.
Shell counterpart of `lark-ai-brief-on' (which routes here when
`lark-ai-interface' is `shell'); the turn appears in the
transcript as \"/brief TOPIC\"."
  (interactive "sBrief on topic: ")
  (let ((topic (string-trim (or topic ""))))
    (when (string-empty-p topic)
      (user-error "Empty topic"))
    (let ((buf (lark-ai-shell)))
      (with-current-buffer buf
        (shell-maker-submit :input (concat "/brief " topic)))
      buf)))

;;;; Turn execution

(defun lark-ai-shell--execute (input shell)
  "Run INPUT through the lark agent loop (shell-maker executor).
SHELL is the callback alist shell-maker hands to executors.
An input of \"/brief <topic>\" runs the cross-domain topic brief
\(`lark-ai-brief-on') instead of the agent loop."
  (let* ((buf (map-elt shell :buffer))
         (session (with-current-buffer buf
                    (or lark-ai-shell--session
                        (setq-local lark-ai-shell--session
                                    (make-lark-ai-session)))))
         (context (or (buffer-local-value 'lark-ai-shell--context buf) ""))
         ;; Snapshot BEFORE pushing the current prompt, mirroring
         ;; `lark-ai-ask' — the user message is built from prior turns.
         (history (lark-ai-session-history session)))
    (cond
     (lark-ai--frontend
      (funcall (map-elt shell :write-output)
               "A turn is already in flight (C-c C-k aborts it).\n")
      (funcall (map-elt shell :finish-output) nil))
     ;; /brief <topic> — cross-domain topic brief.
     ((string-match "\\`/brief\\(?:[ \t]+\\(.*\\)\\)?\\'"
                    (string-trim input))
      (let ((topic (string-trim (or (match-string 1 (string-trim input)) ""))))
        (if (string-empty-p topic)
            (progn
              (funcall (map-elt shell :write-output)
                       "Usage: /brief <topic>\n")
              (funcall (map-elt shell :finish-output) nil))
          (lark-ai-shell--execute-brief topic shell session))))
     (t
      (push (cons "user" input) (lark-ai-session-history session))
      (setq lark-ai--frontend (lark-ai-shell--make-frontend shell session))
      (lark-ai--select-skills
       input (or (lark-ai-skills-routing-context context) "")
       (lambda (skills)
         (lark-ai-agent--run input context history session skills)))))))

(defun lark-ai-shell--execute-brief (topic shell session)
  "Run the cross-domain TOPIC brief as a turn in SHELL.
Mirrors `lark-ai--brief-on-classic': context-graph gather, then
one synthesis call — whose chunks stream live into the shell.
The gathered brief lands in SESSION history, so follow-up turns
\(\"draft a status update from this\") reuse it."
  (let ((write (map-elt shell :write-output))
        (finish (map-elt shell :finish-output)))
    (push (cons "user" (format "Brief on: %s" topic))
          (lark-ai-session-history session))
    (setf (lark-ai-session-phase session) 'executing)
    (setq lark-ai--frontend (lark-ai-shell--make-frontend shell session))
    (lark-ai--progress-log "Gathering context for \"%s\"…" topic)
    (lark-ai-context-graph-gather
     topic
     (lambda (context-text)
       (if (not (eq (lark-ai-session-phase session) 'executing))
           ;; Aborted between hops — release the turn quietly.
           (progn (setq lark-ai--frontend nil)
                  (funcall finish nil))
         (lark-ai--progress-log "Gathered %d chars; calling LLM…"
                                (length (or context-text "")))
         (funcall write "\n")
         (lark-ai--call-llm-stream
          lark-ai-brief-on-prompt
          (lark-ai--brief-user-message topic context-text)
          (lambda (text)
            (push (cons "assistant" text)
                  (lark-ai-session-history session))
            (setf (lark-ai-session-phase session) 'done)
            (setq lark-ai--frontend nil)
            (funcall write "\n")
            (funcall finish t)
            (lark-ai-shell--fontify (map-elt shell :buffer)))
          ;; Stream synthesis chunks straight into the shell.
          (lambda (chunk) (funcall write chunk))))))))

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
       (funcall finish t)
       (lark-ai-shell--fontify (map-elt shell :buffer))))))

(defface lark-ai-shell-table-zebra
  '((t :inherit hl-line :extend t))
  "Face for alternating markdown-table rows in the Lark AI shell.
Inherits the theme's subtle current-line background instead of
markdown-overlays' default `lazy-highlight', which is loud in
most themes."
  :group 'lark-ai)

(defvar markdown-overlays--table-zebra-face)

(defun lark-ai-shell--fontify (buf)
  "Render markdown markup in BUF via overlays, when available.
`markdown-overlays' ships with shell-maker (the same renderer
agent-shell/chatgpt-shell use): headers, bold, code fences and
tables display styled, with the raw markup hidden.  Silently a
no-op when the library is missing.
The table zebra face is softened buffer-locally (see
`lark-ai-shell-table-zebra') so other shell-maker shells keep
their own styling."
  (when (and (buffer-live-p buf)
             (require 'markdown-overlays nil t))
    (with-current-buffer buf
      (when (boundp 'markdown-overlays--table-zebra-face)
        (setq-local markdown-overlays--table-zebra-face
                    'lark-ai-shell-table-zebra))
      (markdown-overlays-put))))

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
