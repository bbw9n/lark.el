;;; lark-ai.el --- AI-native orchestration layer for lark.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; Provides the AI-native layer for lark.el: natural-language prompts
;; are turned into multi-step lark-cli execution plans by an LLM,
;; reviewed by the user, executed, and the results are synthesized
;; into readable output.
;;
;; The LLM's knowledge comes from lark-cli SKILL.md files (loaded by
;; lark-ai-skills.el).  Buffer context (from lark-ai-context.el)
;; makes actions aware of what the user is looking at.
;;
;; Two entry points:
;;   lark-ai-ask   — natural-language prompt (minibuffer)
;;   lark-ai-act   — context-aware action at point
;;
;; LLM backends: gptel (preferred), or raw HTTP fallback.

;;; Code:

(require 'cl-lib)
(require 'lark-core)
(require 'lark-ui)            ; shared spinner engine + `lark-spinner' face
(require 'lark-ai-skills)
(require 'lark-ai-context)
(require 'lark-ai-protocol)   ; debug log + plan parse + interpolate + user-message
(require 'lark-ai-llm)        ; backend dispatch (gptel/http) + streaming
(require 'lark-ai-runner)     ; plan executor
(require 'lark-ai-agent)      ; agentic loop executor (default; see `lark-ai-strategy')
(require 'lark-ai-context-graph) ; cross-domain topic gatherer (lark-ai-brief-on)

;; Forward declarations for IM module
(defvar lark-im--chat-id)
(defvar lark-im--chat-name)
(defvar lark-im--messages)
(declare-function lark-im-messages "lark-im")

;;;; Session state
;;
;; A single buffer-local struct holds all conversation state for the
;; AI buffer.  Replaces the previous ~9 separate buffer-locals so that
;; (a) the state machine is inspectable in one place, (b) `lark-ai-reset'
;; can clear it atomically, and (c) abort can find the in-flight gptel
;; request handle.  The `cl-defstruct' itself lives in
;; `lark-ai-protocol' so the runner can use the auto-generated
;; accessors at byte-compile time without depending on this module.

(defvar-local lark-ai--session nil
  "The `lark-ai-session' for this AI buffer.")

(defun lark-ai--session ()
  "Return the session for the AI buffer, creating it on first use.
Always operates against the AI buffer, not the current buffer, so
callers in user buffers (e.g. `lark-ai-ask' invoked from a doc)
don't have to switch first."
  (with-current-buffer (lark-ai--get-buffer)
    (or lark-ai--session
        (setq lark-ai--session (make-lark-ai-session)))))

;;;; Fragment-based AI buffer
;;
;; Uses lark-ai-ui.el for a fragment-based rendering model.
;; Each conversation turn gets its own set of fragments with a
;; turn-number prefix.  Follow-up questions append new turns.
;; An editable input area at the bottom allows inline follow-ups.

(require 'lark-ai-ui)

(defconst lark-ai--buf-name "*Lark AI*"
  "Name of the AI buffer.")

(defvar lark-ai-plan-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lark-ai-ui-mode-map)
    (define-key map (kbd "C-c C-c") #'lark-ai-send-followup)
    (define-key map (kbd "C-c C-k") #'lark-ai-abort)
    (define-key map (kbd "C-c C-d") #'lark-ai-toggle-debug)
    (define-key map (kbd "C-c C-l") #'lark-ai-reset)
    map)
  "Keymap for the Lark AI buffer.
Intentionally minimal — only the C-c prefix bindings live here
so that plain typing in the follow-up input area below isn't
shadowed.  TAB → toggle-section is on `lark-ai-ui-fragment-map'
(applied as a text property to each fragment), and the plan-review
keys (RET/q/x) are on `lark-ai-plan-keys-map' (applied to the plan
fragment only).")

(defvar lark-ai-plan-keys-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lark-ai-ui-fragment-map)
    (define-key map (kbd "RET") #'lark-ai-plan-execute)
    (define-key map (kbd "q")   #'lark-ai-plan-cancel)
    (define-key map (kbd "x")   #'lark-ai-plan-remove-step)
    map)
  "Plan-review bindings, attached as a `keymap' text property to
the plan fragment by `lark-ai--refresh-plan-fragment'.  Inherits
TAB from `lark-ai-ui-fragment-map' so toggling still works on the
plan, and adds RET/q/x for review actions.  Scoped to the plan so
plain typing in the follow-up input area below isn't intercepted.")

(define-derived-mode lark-ai-plan-mode lark-ai-ui-mode "Lark AI"
  "Major mode for the Lark AI buffer.")

(defun lark-ai--get-buffer ()
  "Get or create the AI buffer."
  (let ((buf (get-buffer-create lark-ai--buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'lark-ai-plan-mode)
        (lark-ai-plan-mode)))
    buf))

;;; Turn-scoped fragment IDs

(defun lark-ai--frag (suffix)
  "Return fragment ID for SUFFIX in the current turn."
  (format "t%d-%s" (lark-ai-session-turn (lark-ai--session)) suffix))

;;; Progress log — appends to the current turn's log fragment

(defun lark-ai--progress-log (fmt &rest args)
  "Add a timestamped entry to the log fragment."
  (let ((entry (format "[%s] %s\n"
                       (format-time-string "%H:%M:%S")
                       (apply #'format fmt args))))
    (when-let ((buf (get-buffer lark-ai--buf-name)))
      (with-current-buffer buf
        (lark-ai-ui-append-fragment
         (lark-ai--frag "log") entry)))))

;;; Header progress bar
;;
;; An indeterminate "working…" bar shown in the AI buffer's header line
;; while a turn is loading or executing.  Unlike the in-buffer streaming
;; preview, the header line is pinned to the top of the window, so the
;; user can tell the turn is running even when the loading zone has
;; scrolled out of view (e.g. on follow-ups).

(defvar-local lark-ai--bar-frame nil
  "Current header progress-bar frame string, or nil when idle.")
(defvar-local lark-ai--bar-timer nil
  "Timer animating the header progress bar, or nil.")

(defun lark-ai--ai-header-line ()
  "Return the AI buffer's header-line string while a turn is busy.
The bar itself is the shared `lark-ui-bar-frames' component."
  (concat " Lark AI  "
          (or lark-ai--bar-frame (aref lark-ui-bar-frames 0))
          "  working…"))

(defun lark-ai--stop-bar ()
  "Stop the header progress bar and remove the header line."
  (when-let ((buf (get-buffer lark-ai--buf-name)))
    (with-current-buffer buf
      (when lark-ai--bar-timer
        (lark-spinner-stop lark-ai--bar-timer))
      (setq lark-ai--bar-timer nil
            lark-ai--bar-frame nil
            header-line-format nil)
      (force-mode-line-update))))

(defun lark-ai--start-bar ()
  "Show and animate the header progress bar while the AI buffer is busy.
Self-stops once the session phase is no longer `loading' or `executing'
\(the render function then returns nil and ON-STOP clears the bar)."
  (when-let ((buf (get-buffer lark-ai--buf-name)))
    (with-current-buffer buf
      (when lark-ai--bar-timer
        (lark-spinner-stop lark-ai--bar-timer))
      (setq header-line-format '(:eval (lark-ai--ai-header-line)))
      (setq lark-ai--bar-timer
            (lark-spinner-start
             (lambda (frame)
               (when (and (buffer-live-p buf)
                          (memq (and (buffer-local-value 'lark-ai--session buf)
                                     (lark-ai-session-phase
                                      (buffer-local-value 'lark-ai--session buf)))
                                '(loading executing)))
                 (with-current-buffer buf
                   (setq lark-ai--bar-frame frame)
                   (force-mode-line-update))
                 t))
             0.12 lark-ui-bar-frames 36000
             (lambda () (lark-ai--stop-bar)))))))

;;; Session lifecycle commands

(declare-function gptel-abort "gptel" (&optional buf))

;;;###autoload
(defun lark-ai-abort ()
  "Abort any in-flight LLM request and reset the session phase.
Marks any running step as skipped so the plan fragment reflects
reality.  Bound to \\[lark-ai-abort] in `lark-ai-plan-mode-map'."
  (interactive)
  (when (fboundp 'gptel-abort)
    (ignore-errors (gptel-abort (lark-ai--get-buffer))))
  (lark-ai--stop-bar)
  (let ((session (lark-ai--session)))
    (setf (lark-ai-session-phase session) 'idle
          (lark-ai-session-callback session) nil)
    ;; Mark anything still 'running as 'skipped so the plan view
    ;; doesn't sit on a stale spinner.
    (setf (lark-ai-session-step-status session)
          (mapcar (lambda (kv)
                    (cons (car kv)
                          (if (eq (cdr kv) 'running) 'skipped (cdr kv))))
                  (lark-ai-session-step-status session))))
  (lark-ai--progress-log "Aborted by user.")
  (lark-ai--refresh-plan-fragment)
  (lark-ai--ensure-input-area)
  (message "Lark AI: aborted"))

;;;###autoload
(defun lark-ai-reset ()
  "Clear the AI buffer and start a fresh conversation.
Aborts any in-flight request and discards all session state
(history, context, skills, plan)."
  (interactive)
  (when-let ((buf (get-buffer lark-ai--buf-name)))
    (when (fboundp 'gptel-abort)
      (ignore-errors (gptel-abort buf)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq lark-ai--session (make-lark-ai-session)))
    (message "Lark AI: session reset")))

;;; Input area — editable region at the bottom
;;
;; Boundary tracking is done with two markers stored on the session
;; (`input-region-start' and `input-start').  This replaces the older
;; approach of identifying the input via `lark-ai-ui-id' text properties
;; on a body-sentinel "\n", which required a delicate front-sticky /
;; rear-nonsticky dance to make typed characters inherit the right
;; properties.  Markers are unambiguous and shrink/grow with edits.

(defun lark-ai--clear-input-area ()
  "Delete the existing input region (separator + label + body)."
  (let* ((session (lark-ai--session))
         (m (lark-ai-session-input-region-start session)))
    (when (and m (marker-position m))
      (let ((inhibit-read-only t))
        (delete-region m (point-max))))
    (setf (lark-ai-session-input-region-start session) nil
          (lark-ai-session-input-start session) nil)))

(defun lark-ai--ensure-input-area ()
  "Insert the editable follow-up area at the bottom of the AI buffer.
Records two markers on the session: `input-region-start' (start of
the separator, used to delete the whole region) and `input-start'
(start of editable text, used by `get-input-text')."
  (let* ((buf (lark-ai--get-buffer))
         (session (lark-ai--session)))
    (with-current-buffer buf
      (lark-ai--clear-input-area)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n")
        ;; Mark where the input region begins (everything from here
        ;; to point-max is recreated each turn).
        (setf (lark-ai-session-input-region-start session)
              (copy-marker (point) nil))
        (lark-ai-ui-insert-separator "ai-input-sep")
        ;; Read-only label.
        (let ((label-beg (point)))
          (insert (propertize
                   "Follow-up (C-c C-c send · C-c C-k abort · C-c C-l reset · C-c C-d debug):\n"
                   'face 'font-lock-keyword-face))
          (put-text-property label-beg (point) 'read-only t)
          ;; The label is rear-nonsticky for `read-only' only — so the
          ;; typed text just below isn't read-only, but other label
          ;; properties (face/keymap) don't leak either way because we
          ;; don't depend on inheritance for input identification.
          (put-text-property label-beg (point)
                             'rear-nonsticky '(read-only)))
        ;; Mark where typed text starts.  Insertion-type nil keeps the
        ;; marker at the same position when text is inserted at it,
        ;; so it always points at the *start* of the user's input.
        (setf (lark-ai-session-input-start session)
              (copy-marker (point) nil))
        (goto-char (lark-ai-session-input-start session))))))

(defun lark-ai--get-input-text ()
  "Return the text typed in the input area, trimmed."
  (when-let* ((session (lark-ai--session))
              (m (lark-ai-session-input-start session))
              (pos (marker-position m)))
    (string-trim
     (buffer-substring-no-properties pos (point-max)))))

(defun lark-ai-send-followup ()
  "Send the text in the input area as a follow-up question."
  (interactive)
  (let ((input (lark-ai--get-input-text)))
    (when (or (null input) (string-empty-p input))
      (user-error "Empty input"))
    (lark-ai--clear-input-area)
    (lark-ai-ask input)))

;;; Build a new turn's layout

(defun lark-ai--show-loading (prompt skill-names)
  "Start a new turn in the AI buffer."
  (let* ((buf (lark-ai--get-buffer))
         (session (lark-ai--session)))
    (with-current-buffer buf
      ;; Drop the previous input region, advance turn, reset per-turn
      ;; plan state on the session.
      (lark-ai--clear-input-area)
      (cl-incf (lark-ai-session-turn session))
      (setf (lark-ai-session-steps session) nil
            (lark-ai-session-callback session) nil
            (lark-ai-session-phase session) 'loading
            (lark-ai-session-step-status session) nil)
      ;; On first turn, add header
      (when (= (lark-ai-session-turn session) 1)
        (lark-ai-ui-clear)
        (lark-ai-ui-insert-fragment "ai-header" 'header "Lark AI" nil)
        (lark-ai-ui-insert-separator "ai-sep-header"))
      ;; Turn separator (after first turn)
      (when (> (lark-ai-session-turn session) 1)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "\n")
          (lark-ai-ui-insert-separator
           (lark-ai--frag "turn-sep"))))
      ;; Prompt
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (lark-ai-ui-insert-fragment
         (lark-ai--frag "prompt") 'prompt
         (concat "▶ " prompt) nil)
        ;; Skills
        (lark-ai-ui-insert-fragment
         (lark-ai--frag "skills") 'skills
         (concat "Skills: " (string-join skill-names " · "))
         nil)
        ;; Log
        (lark-ai-ui-insert-fragment
         (lark-ai--frag "log") 'log nil "")
        ;; Plan placeholder
        (lark-ai-ui-insert-fragment
         (lark-ai--frag "plan") 'plan nil
         (propertize "Waiting for LLM response...\n"
                     'face 'font-lock-comment-face)))
      ;; Track conversation
      (push (cons "user" prompt) (lark-ai-session-history session)))
    ;; Make the new turn's loading zone visible.  Follow-up turns are
    ;; appended at the bottom, so without scrolling there the
    ;; "Waiting for LLM response…" preview would be off-screen.
    (let ((win (display-buffer buf)))
      (when (window-live-p win)
        (when-let ((region (with-current-buffer buf
                             (lark-ai-ui-find-fragment (lark-ai--frag "prompt")))))
          (with-selected-window win
            (goto-char (car region))
            (recenter 0)))))
    ;; Animate the header "working…" bar for the duration of the turn.
    (lark-ai--start-bar)))

;;; Live stream preview — a small rolling tail under "Waiting for LLM
;;; response…" so a slow call visibly progresses instead of looking hung.

(defcustom lark-ai-stream-tail-lines 4
  "How many recent lines of the live LLM stream to preview while waiting.
Shown under \"Waiting for LLM response…\" in the plan fragment as a
small rolling tail, so the user can see the request is in progress
rather than hung.  Set to 0 to disable the preview."
  :type 'integer
  :group 'lark-ai)

(defun lark-ai--stream-tail (text n)
  "Return a bounded tail of TEXT for the loading preview.
Roughly the last N lines; newline-poor streams (e.g. one-line JSON) are
clipped to the last N*80 characters so the preview stays a few lines
tall rather than one ever-growing line."
  (when (and text (> n 0) (not (string-empty-p text)))
    (let* ((maxchars (* n 80))
           (clip (if (> (length text) maxchars)
                     (concat "…" (substring text (- (length text) maxchars)))
                   text))
           (lines (seq-remove #'string-empty-p (split-string clip "\n"))))
      (string-join (last lines n) "\n"))))

(defun lark-ai--stream-preview-handler ()
  "Return a streaming chunk-handler that previews the live LLM output.
The returned closure accumulates chunks and updates the plan fragment
to \"Waiting for LLM response…\" plus a rolling tail of the last
`lark-ai-stream-tail-lines' lines.  Use it as the CHUNK-HANDLER arg of
`lark-ai--call-llm-stream'."
  (let ((acc ""))
    (lambda (chunk)
      (setq acc (concat acc chunk))
      (when-let ((buf (get-buffer lark-ai--buf-name)))
        (with-current-buffer buf
          (when (lark-ai-ui-find-fragment (lark-ai--frag "plan"))
            (let ((tail (lark-ai--stream-tail acc lark-ai-stream-tail-lines)))
              (lark-ai-ui-update-fragment
               (lark-ai--frag "plan") nil
               (concat
                (propertize "Waiting for LLM response…\n"
                            'face 'font-lock-comment-face)
                (and tail (propertize tail 'face 'shadow)))))))))))

(defun lark-ai--clear-waiting ()
  "Clear the loading preview in the current turn's plan fragment."
  (when-let ((buf (get-buffer lark-ai--buf-name)))
    (with-current-buffer buf
      (when (lark-ai-ui-find-fragment (lark-ai--frag "plan"))
        (lark-ai-ui-update-fragment (lark-ai--frag "plan") nil "")))))

;;; Tool-call cards — per-action panes in the agent loop.
;;
;; Each dispatched lark-cli command gets its own collapsible fragment
;; sh-fontified and tinted with the theme's code-block background
;; (`lark-ui-block-bg-face').  The card is inserted in the running
;; state and its label is then updated as the command completes —
;; preserving body fold state across the status transition.

(defun lark-ai--tool-call-label (cmd status)
  "Build the label line for a tool-call card.
CMD is the lark-cli argument list; STATUS is `running', `done',
`error', or `skipped'."
  (let* ((head (mapconcat #'identity
                          (seq-take (or cmd nil) 2)
                          " "))
         (icon (pcase status
                 ('running (propertize "⟳" 'face 'warning))
                 ('done    (propertize "✓" 'face 'success))
                 ('error   (propertize "✗" 'face 'error))
                 ('skipped (propertize "⊘" 'face 'shadow))
                 (_        "·"))))
    (concat "  " icon " "
            (propertize (concat "lark-cli " head)
                        'face 'font-lock-function-name-face))))

(defun lark-ai--shell-quote (arg)
  "Return ARG with minimal display-time shell quoting.
Args that consist only of typical lark-cli token characters
\(alphanumerics plus . / + - _ : = , @ % # *) pass through verbatim
— so flags like `+chats-list' or `--page-size=20' read naturally —
and only arguments containing whitespace or shell metacharacters get
the heavier `shell-quote-argument' treatment.  This is purely cosmetic;
the rendered string is never re-executed by a shell.

ARG values that carry the `lark-ai-preview' text property (produced
by `lark-ai-agent--preview-string') are wrapped in display quotes
without escaping — the ellipsis and `[N chars]' tail would otherwise
get mangled by shell-quoting."
  (cond
   ((and (> (length arg) 0)
         (get-text-property 0 'lark-ai-preview arg))
    (concat "\"" (substring-no-properties arg) "\""))
   ((string-match-p "\\`[A-Za-z0-9_./+:=,@%#*-]+\\'" arg)
    arg)
   (t (shell-quote-argument arg))))

(defun lark-ai--format-cmd-body (cmd)
  "Pretty-print CMD onto multiple lines for a tool-call card body.
Leading positional args stay on the first line; each --flag and its
value follow on continuation lines with a two-space indent and a
trailing backslash — the same shape a person would type at a shell.

Content-flag values (--content, --markdown, --body, --text, --message)
are clipped to a one-line preview via `lark-ai-agent-abbreviate-cmd'
so a multi-KB doc body doesn't flood the card.  The CLI invocation
still receives the original, unabbreviated command."
  (let ((cmd (lark-ai-agent-abbreviate-cmd cmd))
        (head nil) (groups nil) (cur nil))
    (dolist (arg cmd)
      (if (string-prefix-p "--" arg)
          (progn
            (cond ((null head) (setq head (nreverse cur)))
                  (cur         (push (nreverse cur) groups)))
            (setq cur (list arg)))
        (push arg cur)))
    (cond ((null head) (setq head (nreverse cur)))
          (cur         (push (nreverse cur) groups)))
    (let ((head-line (concat "lark-cli "
                             (mapconcat #'lark-ai--shell-quote head " ")))
          (group-lines (mapcar (lambda (g)
                                 (concat "  "
                                         (mapconcat #'lark-ai--shell-quote
                                                    g " ")))
                               (nreverse groups))))
      (if group-lines
          (concat head-line " \\\n"
                  (mapconcat #'identity group-lines " \\\n"))
        head-line))))

(defun lark-ai--render-tool-call (iter cmd status)
  "Insert or update the tool-call card for agent iteration ITER.
On first call (STATUS=`running'), inserts a fragment just above the
plan fragment containing the sh-fontified command body.  On subsequent
calls, updates only the label — so a user-folded body stays folded
across the running → done/error transition."
  (when-let ((buf (get-buffer lark-ai--buf-name)))
    (with-current-buffer buf
      (let ((id (lark-ai--frag (format "tool-%d" iter)))
            (label (lark-ai--tool-call-label cmd status)))
        (cond
         ((lark-ai-ui-find-fragment id)
          (lark-ai-ui-update-label id label))
         (t
          (let ((plan-region (lark-ai-ui-find-fragment
                              (lark-ai--frag "plan"))))
            (save-excursion
              (goto-char (if plan-region (car plan-region) (point-max)))
              (lark-ai-ui-insert-fragment
               id 'tool-call label
               (lark-ai--format-cmd-body cmd))))))))))

;;; Plan display — update the plan fragment

(defun lark-ai--display-plan (steps callback)
  "Show STEPS for review.  CALLBACK called with confirmed steps."
  (let* ((buf (lark-ai--get-buffer))
         (session (lark-ai--session)))
    (with-current-buffer buf
      (setf (lark-ai-session-steps session) steps
            (lark-ai-session-callback session) callback
            (lark-ai-session-phase session) 'review
            (lark-ai-session-step-status session)
            (mapcar (lambda (s) (cons (plist-get s :index) 'pending))
                    steps))
      (lark-ai--refresh-plan-fragment))
    (pop-to-buffer buf)))

(defun lark-ai--refresh-plan-fragment ()
  "Rebuild the plan fragment from current state."
  (let ((body (lark-ai--format-plan-body)))
    (lark-ai-ui-update-fragment
     (lark-ai--frag "plan")
     (lark-ai--plan-label)
     body)
    ;; Scope plan-review keys (RET/q/x) to the plan fragment via a
    ;; `keymap' text property — keeps them off the buffer mode-map
    ;; so plain typing in the input area isn't intercepted.
    (let ((region (lark-ai-ui-find-fragment (lark-ai--frag "plan"))))
      (when region
        (let ((inhibit-read-only t))
          (put-text-property (car region) (cdr region)
                             'keymap lark-ai-plan-keys-map))))))

(defun lark-ai--plan-label ()
  "Return the plan section label for the current phase."
  (pcase (lark-ai-session-phase (lark-ai--session))
    ('review    "Plan — confirm to execute")
    ('executing "Executing")
    ('done      "Complete")
    (_          "Plan")))

(defun lark-ai--format-plan-body ()
  "Format the plan steps as a string for the plan fragment.
Each step's line carries a `lark-ai-step-index' text property so
`lark-ai-plan-remove-step' can pick the step under point without
relying on line-number arithmetic."
  (let ((session (lark-ai--session)))
    (with-temp-buffer
      (dolist (step (lark-ai-session-steps session))
        (let* ((idx (plist-get step :index))
               (desc (plist-get step :description))
               (cmd (plist-get step :command))
               (side-effect (plist-get step :side-effect))
               (synthesize (plist-get step :synthesize))
               (pg (plist-get step :parallel-group))
               (status (alist-get idx (lark-ai-session-step-status session)))
               (indicator
                (pcase status
                  ('done    (propertize "✓" 'face 'success))
                  ('running (propertize "⟳" 'face 'warning))
                  ('error   (propertize "✗" 'face 'error))
                  ('skipped (propertize "✗" 'face
                                        'font-lock-comment-face))
                  (_        (propertize "○" 'face
                                        'font-lock-comment-face)))))
          (let ((line-beg (point)))
            (insert "  " indicator " "
                    (propertize desc 'face
                                (pcase status
                                  ('done 'font-lock-comment-face)
                                  (_ (if side-effect 'warning
                                       'default))))
                    (if (and side-effect (not (eq status 'done)))
                        (propertize " ⚠ writes" 'face 'error)
                      "")
                    (if synthesize
                        (propertize " (synthesis)"
                                    'face 'font-lock-comment-face)
                      "")
                    (if pg (format " [group %d]" pg) "")
                    "\n")
            (when cmd
              (insert "    "
                      (propertize (string-join cmd " ")
                                  'face 'font-lock-string-face)
                      "\n"))
            ;; Tag both lines (description + optional command) with
            ;; the step's index so remove-step can dispatch by point.
            (put-text-property line-beg (point)
                               'lark-ai-step-index idx))))
      ;; Footer
      (pcase (lark-ai-session-phase session)
        ('review
         (insert "\n"
                 (propertize "RET" 'face 'bold) " Execute  "
                 (propertize "x" 'face 'bold) " Remove step  "
                 (propertize "q" 'face 'bold) " Cancel\n"))
        ('executing
         (insert "\n"
                 (propertize "Running..."
                             'face 'font-lock-comment-face)
                 "\n")))
      (buffer-string))))

;;; Step status updates — only refresh the plan fragment

(defun lark-ai--update-step-status (index status)
  "Update step INDEX to STATUS and refresh plan."
  (when-let ((buf (get-buffer lark-ai--buf-name)))
    (with-current-buffer buf
      (setf (alist-get index (lark-ai-session-step-status (lark-ai--session)))
            status)
      (lark-ai--refresh-plan-fragment))))

(defun lark-ai--mark-all-done ()
  "Mark execution as done and refresh plan."
  (when-let ((buf (get-buffer lark-ai--buf-name)))
    (with-current-buffer buf
      (setf (lark-ai-session-phase (lark-ai--session)) 'done)
      (lark-ai--refresh-plan-fragment))))


;;; Plan actions

(defun lark-ai-plan-execute ()
  "Execute the plan."
  (interactive)
  (let ((session (lark-ai--session)))
    (unless (eq (lark-ai-session-phase session) 'review)
      (user-error "No plan to execute"))
    (let ((steps (lark-ai-session-steps session))
          (callback (lark-ai-session-callback session)))
      (setf (lark-ai-session-phase session) 'executing
            (lark-ai-session-callback session) nil)
      (lark-ai--refresh-plan-fragment)
      ;; Re-arm the header bar (it stopped while the plan sat in review).
      (lark-ai--start-bar)
      (when callback
        (funcall callback steps)))))

(defun lark-ai-plan-cancel ()
  "Cancel the plan."
  (interactive)
  (let ((session (lark-ai--session)))
    (setf (lark-ai-session-phase session) 'done
          (lark-ai-session-callback session) nil))
  (lark-ai--refresh-plan-fragment)
  (lark-ai--progress-log "Cancelled."))

(defun lark-ai-plan-remove-step ()
  "Remove the plan step at point.
Reads the `lark-ai-step-index' text property attached by
`lark-ai--format-plan-body', so the cursor can be anywhere on the
step's description or command line."
  (interactive)
  (let ((session (lark-ai--session)))
    (unless (eq (lark-ai-session-phase session) 'review)
      (user-error "Can only remove steps during review"))
    (let ((idx (get-text-property (point) 'lark-ai-step-index)))
      (unless idx
        (user-error "Place point on a plan step"))
      (setf (lark-ai-session-steps session)
            (seq-remove (lambda (s) (= (plist-get s :index) idx))
                        (lark-ai-session-steps session)))
      (setf (lark-ai-session-step-status session)
            (mapcar (lambda (s) (cons (plist-get s :index) 'pending))
                    (lark-ai-session-steps session)))
      (lark-ai--refresh-plan-fragment))))

;; Plan execution, $step-N interpolation, LLM dispatch and streaming
;; have moved to `lark-ai-runner.el', `lark-ai-protocol.el', and
;; `lark-ai-llm.el' respectively (required at the top of this file).

;;;; Synthesis (second LLM pass)

(defun lark-ai--results-text (results plan)
  "Format step RESULTS as a labelled text block for an LLM prompt.
PLAN supplies each step's description.  The `:synthesize' sentinel — an
unproduced (deferred) synthesis step — is skipped.  String results
\(e.g. an inline producer's output) are inserted verbatim; other results
are JSON-encoded.  Steps are ordered by index."
  (mapconcat
   (lambda (pair)
     (let ((idx (car pair))
           (data (cdr pair)))
       (unless (eq data :synthesize)
         (let ((step (seq-find (lambda (s) (= (plist-get s :index) idx)) plan)))
           (format "### Step %d: %s\n```json\n%s\n```\n"
                   idx
                   (or (and step (plist-get step :description)) "")
                   (cond ((null data) "(no data)")
                         ((stringp data) data)
                         (t (json-encode data))))))))
   (sort (copy-sequence results)
         (lambda (a b) (< (car a) (car b))))
   "\n"))

(defun lark-ai--synthesis-instruction (results plan)
  "Choose the instruction for the terminal (final-answer) synthesis.
Prefer a synthesis step that is still deferred — its result in RESULTS
is the `:synthesize' sentinel.  A synthesis step already consumed inline
as a producer (its result is real text) should NOT drive the final
answer with its content-generation instruction, so it is ignored here.
Falls back to a generic summary instruction."
  (let ((deferred (seq-find
                   (lambda (s)
                     (and (plist-get s :synthesize)
                          (eq (alist-get (plist-get s :index) results)
                              :synthesize)))
                   plan)))
    (or (and deferred (plist-get deferred :synthesis-instruction))
        "Summarize the results clearly and concisely.")))

(defun lark-ai--synthesize (results plan system-prompt callback)
  "Send execution RESULTS back to the LLM for synthesis.
PLAN is the original plan.  SYSTEM-PROMPT must be the synthesis
prompt (see `lark-ai-skills-build-synthesis-prompt') — passing
the planning prompt here makes the model emit another JSON plan
instead of the prose answer.  CALLBACK receives the synthesis text."
  (let* ((instruction (lark-ai--synthesis-instruction results plan))
         (results-text (lark-ai--results-text results plan))
         (user-msg (format "Here are the execution results:\n\n%s\n\nInstruction: %s\n\nRespond with plain text (not JSON). Use markdown formatting."
                           results-text instruction)))
    (lark-ai--call-llm system-prompt user-msg callback)))

(defun lark-ai--synthesize-stream (results plan system-prompt)
  "Like `lark-ai--synthesize' but streams the output.
SYSTEM-PROMPT must be the synthesis prompt (no JSON mandate)."
  (let* ((instruction (lark-ai--synthesis-instruction results plan))
         (results-text (lark-ai--results-text results plan))
         (user-msg (format "Here are the execution results:\n\n%s\n\nInstruction: %s\n\nRespond with plain text (not JSON). Use markdown formatting."
                           results-text instruction)))
    (lark-ai--call-llm-stream
     system-prompt user-msg
     (lambda (text)
       (when-let ((buf (get-buffer lark-ai--buf-name)))
         (with-current-buffer buf
           (push (cons "assistant" text)
                 (lark-ai-session-history (lark-ai--session)))
           (lark-ai--ensure-input-area)))))))

(defun lark-ai--synthesize-inline (step done-fn)
  "Run a producer synthesis STEP now and pass its text to DONE-FN.
Called by the runner when a later step interpolates this step's output
via $step-N.  Unlike the terminal synthesis pass this is non-streaming
\(the full text must exist before the dependent step runs) and uses the
producer system prompt (bare artifact, no user-facing framing).  The
source material is the originating-buffer context plus any prior step
results — the chat/document content the plan was told to work from lives
in the context, not in a CLI result."
  (let* ((session (lark-ai--session))
         (instruction (or (plist-get step :synthesis-instruction)
                          "Produce the requested content."))
         (context (lark-ai-session-context session))
         (results (lark-ai-session-step-results session))
         (plan (lark-ai-session-steps session))
         (system-prompt (lark-ai-skills-build-producer-prompt
                         (lark-ai-session-skills session)))
         (results-text (lark-ai--results-text results plan))
         (user-msg
          (concat
           (when (and context (not (string-empty-p context)))
             (concat "## Source context\n" context "\n\n"))
           (unless (string-empty-p results-text)
             (concat "## Prior step results\n" results-text "\n\n"))
           "## Instruction\n" instruction
           "\n\nOutput only the content described above, ready to be used"
           " verbatim as a command argument.")))
    (lark-ai--call-llm system-prompt user-msg done-fn)))

;;;; Output rendering

(defun lark-ai--ensure-output-fragment ()
  "Ensure the output fragment exists for the current turn."
  (when-let ((buf (get-buffer lark-ai--buf-name)))
    (with-current-buffer buf
      (let ((out-id (lark-ai--frag "output")))
        (unless (lark-ai-ui-find-fragment out-id)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (lark-ai-ui-insert-separator (lark-ai--frag "sep"))
            (lark-ai-ui-insert-fragment
             out-id 'output "Output" "")))))))

(defun lark-ai--present (content &optional skip-history)
  "Display CONTENT in the output fragment, then add input area.
When SKIP-HISTORY is non-nil, do not push CONTENT into the
conversation history — used for error fallbacks like
parse-plan failure where the raw text would otherwise pollute
future turns."
  (let ((buf (lark-ai--get-buffer)))
    (with-current-buffer buf
      (lark-ai--ensure-output-fragment)
      (lark-ai-ui-update-fragment
       (lark-ai--frag "output") "Output" content)
      (unless skip-history
        (push (cons "assistant" content)
              (lark-ai-session-history (lark-ai--session))))
      (lark-ai--ensure-input-area))
    (pop-to-buffer buf)
    (lark-ai--scroll-to-output)))

(defun lark-ai--scroll-to-output ()
  "Scroll the AI buffer to the output fragment."
  (when-let ((buf (get-buffer lark-ai--buf-name)))
    (with-current-buffer buf
      (let ((region (lark-ai-ui-find-fragment
                     (lark-ai--frag "output"))))
        (when region
          (goto-char (car region))
          (forward-line 2)
          (recenter 0))))))

;;;; Entry points
;;
;; `lark-ai-history-truncate-chars' and `lark-ai--build-user-message'
;; live in `lark-ai-protocol.el'.

;;;###autoload
(defun lark-ai-ask (prompt)
  "Ask the Lark AI assistant to execute a natural-language PROMPT.
Identifies relevant lark-cli skills (via the LLM by default — see
`lark-ai-skill-routing'), sends to LLM, reviews the plan, executes
it, and presents results."
  (interactive "sLark AI: ")
  (let* ((ai-buf (lark-ai--get-buffer))
         (in-ai-buf (eq (current-buffer) ai-buf))
         (session (lark-ai--session))
         ;; On a follow-up (we are in the AI buffer), reuse the
         ;; originating context — the AI buffer itself has no domain,
         ;; so re-extracting it would lose what the user was viewing
         ;; when they started the conversation.  On a fresh ask from
         ;; a real buffer, capture context from there.
         (context (if in-ai-buf
                      (or (lark-ai-session-context session) "")
                    (lark-ai-context-format)))
         (history (lark-ai-session-history session))
         (prev-skills (lark-ai-session-skills session))
         ;; Build the skill match-text from the originating buffer
         ;; context plus the last plan's command heads + descriptions.
         ;; This lets a non-specific follow-up (\"tell me more\") still
         ;; match domain keywords carried over from the prior turn.
         (last-plan-text
          (when-let ((plan (lark-ai-session-last-plan session)))
            (mapconcat
             (lambda (s)
               (concat (or (when-let ((cmd (plist-get s :command)))
                             (string-join cmd " "))
                           "")
                       " "
                       (or (plist-get s :description) "")))
             plan "\n")))
         (match-text (mapconcat #'identity
                                (delq nil (list (and (not (string-empty-p context))
                                                     context)
                                                last-plan-text))
                                "\n")))
    ;; Persist originating context now (fresh ask only) so a follow-up
    ;; doesn't overwrite it with the empty AI-buffer context.
    (unless in-ai-buf
      (setf (lark-ai-session-context session) context))
    ;; Open the turn UI immediately; the skills line is filled in once
    ;; routing resolves (it may require an async LLM call).
    (lark-ai--show-loading prompt nil)
    ;; Step 1: identify which skills the request needs, then plan.
    (lark-ai--select-skills
     prompt match-text
     (lambda (matched-skills)
       ;; Reuse the previous turn's skills as the base set on follow-ups
       ;; so domain continuity is preserved even when this turn picks
       ;; nothing new.  Fresh asks (from a real buffer) start clean.
       (let ((skill-names (delete-dups
                           (if (and in-ai-buf prev-skills)
                               (append prev-skills matched-skills)
                             matched-skills))))
         (lark-ai--debug-log
          "SKILLS" "prev=%S matched=%S → final=%S"
          prev-skills matched-skills skill-names)
         (lark-ai-ui-update-fragment
          (lark-ai--frag "skills")
          (concat "Skills: " (string-join skill-names " · ")))
         (lark-ai--run-planning prompt context history session skill-names))))))

(defun lark-ai--select-skills (prompt context callback)
  "Identify relevant skills for PROMPT, then call CALLBACK with the list.
CONTEXT is extra match text (originating buffer + last plan).  When
`lark-ai-skill-routing' is `llm', ask the LLM to pick skills from the
catalogue and fall back to keyword routing if it returns nothing
usable; otherwise use keyword routing directly.  CALLBACK always
receives a deduplicated list that includes lark-shared."
  (if (eq lark-ai-skill-routing 'keyword)
      (funcall callback (lark-ai-skills-select prompt context))
    (lark-ai--progress-log "Identifying relevant skills…")
    (let ((router-prompt (lark-ai-skills-build-router-prompt))
          (user-msg (concat "User request:\n" prompt
                            (when (and context (not (string-empty-p context)))
                              (concat "\n\nContext:\n" context)))))
      (lark-ai--call-llm
       router-prompt user-msg
       (lambda (response)
         (let* ((data (ignore-errors (lark-ai--extract-json response)))
                (raw (and (listp data) (alist-get 'skills data)))
                (names (cond ((listp raw) raw)
                             ((vectorp raw) (append raw nil))
                             (t nil)))
                (valid (lark-ai-skills--validate-selection names)))
           (lark-ai--debug-log "SKILL ROUTER" "llm=%S → valid=%S" names valid)
           (funcall callback
                    (if valid
                        (delete-dups (cons "lark-shared" valid))
                      ;; LLM gave nothing usable — keyword fallback.
                      (lark-ai-skills-select prompt context)))))))))

(defun lark-ai--run-planning (prompt context history session skill-names)
  "Build the planning prompt from SKILL-NAMES and run plan→execute.
PROMPT, CONTEXT, HISTORY and SESSION carry the turn state.  This is
the second half of `lark-ai-ask', invoked once skills are selected.

When `lark-ai-strategy' is `agent', delegate to the iterative agent
loop instead of the upfront-plan executor."
  (if (eq lark-ai-strategy 'agent)
      (lark-ai-agent--run prompt context history session skill-names)
    (lark-ai--run-planning-1 prompt context history session skill-names)))

(defun lark-ai--run-planning-1 (prompt context history session skill-names)
  "Upfront-plan strategy: plan via the LLM, review, execute, synthesize."
  (let* ((system-prompt (lark-ai-skills-build-system-prompt skill-names))
         ;; Synthesis pass uses a different system prompt (no JSON
         ;; mandate) so the model produces prose.  Built once here
         ;; from the same skill set so both passes share context.
         (synth-prompt (lark-ai-skills-build-synthesis-prompt skill-names))
         (user-msg (lark-ai--build-user-message prompt context history)))
    (setf (lark-ai-session-system-prompt session) system-prompt
          (lark-ai-session-skills session) skill-names)
    ;; Stream the planning call into the *log* fragment so the user
    ;; sees tokens land instead of staring at "Waiting for LLM
    ;; response...".  HTTP backend silently falls through to a
    ;; non-streaming call inside `lark-ai--call-llm-stream'.
    (lark-ai--progress-log "← LLM planning (streaming)…")
    (lark-ai--call-llm-stream
     system-prompt user-msg
     (lambda (response)
       (lark-ai--progress-log "← LLM planning done (%d chars)"
                              (length response))
       (let ((plan (lark-ai--parse-plan response)))
         (lark-ai--debug-log
          "ASK BRANCH"
          "plan=%s steps=%d all-synthesize=%s"
          (if plan "parsed" "null")
          (length plan)
          (and plan (seq-every-p (lambda (s) (plist-get s :synthesize)) plan)))
         (if (null plan)
             (progn
               ;; Plan parse failed.  Surface a clear error to the
               ;; user (raw response is still in *Lark AI Debug* if
               ;; debug is on) and DO NOT push the raw text into
               ;; history — partial JSON pollutes future turns.
               (lark-ai--progress-log
                "LLM did not return a parseable plan; see *Lark AI Debug*.")
               (lark-ai--mark-all-done)
               (lark-ai--present
                (concat
                 (propertize "Could not parse a plan from the LLM response.\n"
                             'face 'error)
                 "Toggle `M-x lark-ai-toggle-debug' and re-ask to see the raw\
 response, or rephrase the request.")
                'skip-history))
           (setf (lark-ai-session-last-plan session) plan)
           (if (seq-every-p (lambda (s) (plist-get s :synthesize)) plan)
               (progn
                 (lark-ai--progress-log "Pure synthesis — streaming")
                 (lark-ai--mark-all-done)
                 ;; Synth-prompt (no JSON mandate) — otherwise the
                 ;; model would emit another plan JSON instead of the
                 ;; user-facing prose answer.
                 (lark-ai--call-llm-stream
                  synth-prompt
                  (lark-ai--build-user-message
                   (concat prompt
                           "\nRespond with plain text using markdown.")
                   context history)
                  (lambda (text)
                    (when-let ((buf (get-buffer lark-ai--buf-name)))
                      (with-current-buffer buf
                        (push (cons "assistant" text)
                              (lark-ai-session-history (lark-ai--session)))
                        (lark-ai--ensure-input-area))))))
             (progn
               (lark-ai--display-plan
                plan
                (lambda (confirmed-steps)
                  (lark-ai--progress-log "Executing plan...")
                  (lark-ai--execute-plan
                   confirmed-steps
                   (lambda (results)
                     (if (seq-find (lambda (s) (plist-get s :synthesize))
                                   confirmed-steps)
                         (progn
                           (lark-ai--progress-log "Synthesizing results...")
                           (lark-ai--mark-all-done)
                           ;; Synth-prompt here too — same reason.
                           (lark-ai--synthesize-stream
                            results confirmed-steps synth-prompt))
                       (lark-ai--mark-all-done)
                       (lark-ai--present
                        (mapconcat
                         (lambda (pair)
                           (format "## Step %d\n```\n%s\n```"
                                   (car pair)
                                   (if (cdr pair) (pp-to-string (cdr pair)) "(no data)")))
                         (sort (copy-sequence results)
                               (lambda (a b) (< (car a) (car b))))
                         "\n\n")))))))
               ;; Auto-run past the review gate unless the user wants to
               ;; review the plan first.
               (unless (eq lark-ai-execute-mode 'review)
                 (lark-ai-plan-execute)))))))
     ;; Chunk handler — preview a rolling tail of the streamed plan JSON
     ;; under "Waiting for LLM response…" so the user sees progress.
     (lark-ai--stream-preview-handler))))

;;;; Topic brief — cross-domain context provider
;;
;; `lark-ai-brief-on' is the first workflow built on the context-graph
;; substrate.  Given a TOPIC, it asks every registered provider (v1:
;; docs + messages) for matching snippets, then streams a synthesised
;; brief into the AI chat — so a follow-up like "draft a status update
;; from this" reuses the same gathered context for free.

(defcustom lark-ai-brief-on-prompt
  (concat
   "You are a productivity assistant building a topic brief from"
   " retrieved snippets across the user's docs and chat history.\n\n"
   "Rules:\n"
   "- Be terse — the reader is busy.\n"
   "- Keep the existing section headings (`## Docs', `## Messages', …)"
   " and write a short synthesis under each one.\n"
   "- Always preserve source links from the snippets so the reader"
   " can click through.\n"
   "- Highlight what's actionable, recent, or decided; downplay"
   " incidental matches.\n"
   "- If a section has no useful signal, omit it.\n"
   "- Output markdown only.")
  "System prompt for `lark-ai-brief-on'.
The synthesised brief is rendered straight into the AI buffer, so
any structural conventions (headings, link style) the prompt asks
for shape what the user sees."
  :type 'string
  :group 'lark-ai)

;;;###autoload
(defun lark-ai-brief-on (topic)
  "Build a cross-domain context brief for TOPIC and synthesise it.
Fans out to every provider in `lark-ai-context-graph-providers' (v1:
docs + messages), aggregates their results, and streams a structured
brief into the AI chat buffer.  Because the gathered context is then
in the chat's conversation history, a follow-up question (\"draft a
status update from this\", \"who owns this work?\") reuses it without
re-fetching."
  (interactive "sBrief on topic: ")
  (let ((topic (string-trim (or topic ""))))
    (when (string-empty-p topic)
      (user-error "Empty topic"))
    (let ((buf (lark-ai--get-buffer))
          (session (lark-ai--session)))
      (lark-ai--show-loading
       (format "Brief on: %s" topic) '("lark-shared"))
      (setf (lark-ai-session-phase session) 'executing)
      (lark-ai--progress-log "Gathering context for \"%s\"…" topic)
      (lark-ai-context-graph-gather
       topic
       (lambda (context-text)
         ;; Cancellation guard: the user may have aborted between hops.
         (let* ((alive (buffer-live-p buf))
                (s (and alive
                        (buffer-local-value 'lark-ai--session buf))))
           (when (and s (eq (lark-ai-session-phase s) 'executing))
             (lark-ai--brief-on-synthesize topic context-text))))))))

(defun lark-ai--brief-on-synthesize (topic context-text)
  "Stream a brief for TOPIC built from CONTEXT-TEXT.
Empty CONTEXT-TEXT yields a short \"no results\" reply rather than
firing the LLM at nothing; otherwise we hand the synthesis off to the
streaming LLM path so chunks land directly in the output fragment."
  (lark-ai--progress-log
   "Gathered %d chars; calling LLM…" (length (or context-text "")))
  (lark-ai--clear-waiting)
  (let* ((empty-p (string-empty-p (string-trim (or context-text ""))))
         (user-msg
          (if empty-p
              (format
               "## Topic\n%s\n\nNo results were retrieved across the\
 enabled providers. Tell the user no relevant docs or messages were\
 found, in one sentence, and suggest broadening the topic." topic)
            (format
             "## Topic\n%s\n\n## Retrieved snippets\n%s\n\nWrite the\
 brief now."
             topic context-text))))
    (lark-ai--call-llm-stream
     lark-ai-brief-on-prompt user-msg
     (lambda (text)
       (when-let ((buf (get-buffer lark-ai--buf-name)))
         (with-current-buffer buf
           (push (cons "assistant" text)
                 (lark-ai-session-history (lark-ai--session)))
           (lark-ai--mark-all-done)
           (lark-ai--ensure-input-area)))))))

;;;###autoload
(defun lark-ai-act ()
  "Perform a context-aware AI action on the item at point.
Offers a menu of actions appropriate to the current domain and item,
then executes the chosen action via `lark-ai-ask'."
  (interactive)
  (let* ((ctx (lark-ai-context))
         (domain (plist-get ctx :domain))
         (item (plist-get ctx :item))
         (summary (plist-get ctx :summary)))
    (unless domain
      (user-error "Not in a Lark buffer"))
    (let* ((actions (lark-ai-act--actions-for domain item))
           (choice (completing-read
                    (format "Action (%s): " summary)
                    (mapcar #'car actions)
                    nil nil nil nil (caar actions)))
           (template (alist-get choice actions nil nil #'equal))
           (prompt (if template
                       (lark-ai-act--expand-template template ctx)
                     choice)))
      (lark-ai-ask prompt))))

(defun lark-ai-act--actions-for (domain item)
  "Return alist of (LABEL . PROMPT-TEMPLATE) for DOMAIN and ITEM.
Templates can use {domain}, {summary}, {id}, and {custom} prompt."
  (append
     ;; Domain-specific actions
     (pcase domain
       ("calendar"
        (append
         '(("Show today's agenda" . "Show my agenda for today")
           ("Check free slots" .
            "Check my free time slots for today")
           ("Suggest meeting time" .
            "Suggest available meeting times this week"))
         (when (plist-get item :event-id)
           `(("Reschedule this event" .
              ,(format "Reschedule event %s. Ask me for the new time."
                       (plist-get item :event-id)))
             ("Add attendee" .
              ,(format "Add an attendee to event %s. Ask me who to add."
                       (plist-get item :event-id)))
             ("Cancel this event" .
              ,(format "Cancel event %s."
                       (plist-get item :event-id)))))))
       ("im"
        (append
         '(("Search messages" .
            "Search messages. Ask me for the keyword."))
         (when (plist-get item :chat-id)
           `(("Summarize this chat" .
              ,(format "Summarize the recent messages in chat %s."
                       (or (plist-get item :chat-name)
                           (plist-get item :chat-id))))
             ("Send a message" .
              ,(format "Send a message to chat %s. Ask me what to say."
                       (or (plist-get item :chat-name)
                           (plist-get item :chat-id))))))
         (when (plist-get item :message-id)
           `(("Reply to this message" .
              ,(format "Draft a reply to message %s in chat %s."
                       (plist-get item :message-id)
                       (or (plist-get item :chat-name)
                           (plist-get item :chat-id))))))))
       ("tasks"
        (append
         '(("Show my tasks" . "List my open tasks")
           ("Create a task" .
            "Create a new task. Ask me for the details."))
         (when (plist-get item :task-id)
           `(("Complete this task" .
              ,(format "Mark task %s as completed."
                       (plist-get item :task-id)))
             ("Set due date" .
              ,(format "Set a due date for task %s. Ask me for the date."
                       (plist-get item :task-id)))
             ("Add subtask" .
              ,(format "Add a subtask to task %s. Ask me for the title."
                       (plist-get item :task-id)))))))
       ("mail"
        (append
         '(("Check inbox" . "Show my recent emails"))
         (when (plist-get item :mail-id)
           `(("Reply to this email" .
              ,(format "Draft a reply to email %s."
                       (plist-get item :mail-id)))
             ("Forward this email" .
              ,(format "Forward email %s. Ask me who to forward to."
                       (plist-get item :mail-id)))))))
       ("docs"
        (append
         (when (plist-get item :doc-token)
           `(("Summarize this document" .
              ,(format "Summarize document %s."
                       (plist-get item :doc-token)))
             ("Extract action items" .
              ,(format "Extract all action items and TODOs from document %s."
                       (plist-get item :doc-token)))
             ("Find related docs" .
              ,(format "Search for documents related to %s."
                       (plist-get item :doc-token)))))
         '(("Search docs" .
            "Search documents. Ask me for the query."))))
       ("drive"
        (when (plist-get item :file-token)
          `(("Download this file" .
             ,(format "Download file %s."
                      (plist-get item :file-token)))
            ("Share this file" .
             ,(format "Share file %s. Ask me who to share with."
                      (plist-get item :file-token))))))
       ("meetings"
        (when (plist-get item :meeting-id)
          `(("Get meeting notes" .
             ,(format "Fetch meeting notes for meeting %s."
                      (plist-get item :meeting-id)))
            ("Summarize this meeting" .
             ,(format "Summarize meeting %s."
                      (plist-get item :meeting-id))))))
       ("sheets"
        `(("Read sheet data" .
           ,(format "Read data from spreadsheet %s."
                    (or (plist-get item :token) "this")))
          ("Analyze sheet" .
           ,(format "Analyze the data in spreadsheet %s."
                    (or (plist-get item :token) "this")))))
       (_ nil))
     ;; Always available: free-form input
     '(("Other..." . nil))))

(defun lark-ai-act--expand-template (template ctx)
  "Expand TEMPLATE string with context CTX.
If TEMPLATE is nil, prompt for free-form input."
  (if (null template)
      (read-string (format "Action (%s): "
                           (plist-get ctx :summary)))
    template))

;;;; Smart reply compose buffer

(defvar lark-ai-reply-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'lark-ai-reply-send)
    (define-key map (kbd "C-c C-k") #'lark-ai-reply-cancel)
    map)
  "Keymap for `lark-ai-reply-mode'.")

(define-derived-mode lark-ai-reply-mode text-mode "Lark Reply"
  "Major mode for editing an AI-drafted reply before sending.
\\[lark-ai-reply-send] to send, \\[lark-ai-reply-cancel] to cancel."
  (setq header-line-format
        " Lark AI Reply — C-c C-c send, C-c C-k cancel"))

(defvar-local lark-ai-reply--message-id nil
  "Message ID to reply to.")

(defvar-local lark-ai-reply--chat-id nil
  "Chat ID the reply belongs to.")

(defvar-local lark-ai-reply--chat-name nil
  "Chat name for refreshing after send.")

(defun lark-ai-reply-send ()
  "Send the drafted reply in the current compose buffer."
  (interactive)
  (let ((text (string-trim (buffer-string)))
        (msg-id lark-ai-reply--message-id)
        (chat-id lark-ai-reply--chat-id)
        (chat-name lark-ai-reply--chat-name))
    (when (string-empty-p text)
      (user-error "Empty reply"))
    (unless msg-id
      (user-error "No message ID — cannot send reply"))
    (quit-window t)
    (message "Lark AI: sending reply...")
    (lark--run-command
     (list "im" "+messages-reply" "--message-id" msg-id "--text" text)
     (lambda (_data)
       (message "Lark AI: reply sent")
       ;; Refresh the chat buffer if it exists
       (when chat-id
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (when (and (derived-mode-p 'lark-im-chat-mode)
                        (boundp 'lark-im--chat-id)
                        (equal lark-im--chat-id chat-id))
               (lark-im-messages chat-id chat-name)))))))))

(defun lark-ai-reply-cancel ()
  "Cancel the reply draft."
  (interactive)
  (quit-window t)
  (message "Lark AI: reply cancelled."))

(defun lark-ai--open-compose-buffer (draft message-id chat-id chat-name)
  "Open a compose buffer pre-filled with DRAFT.
MESSAGE-ID is the message being replied to.
CHAT-ID and CHAT-NAME identify the chat for refresh after send."
  (let ((buf (get-buffer-create "*Lark AI Reply*")))
    (with-current-buffer buf
      (lark-ai-reply-mode)
      (erase-buffer)
      (insert draft)
      (setq lark-ai-reply--message-id message-id
            lark-ai-reply--chat-id chat-id
            lark-ai-reply--chat-name chat-name)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;; Thread context extraction for smart reply

(defun lark-ai--collect-thread-context ()
  "Collect thread context from the current chat buffer.
Returns a plist (:chat-id ID :chat-name NAME :message-id ID
:thread-text TEXT) or nil if not in a chat buffer."
  (unless (derived-mode-p 'lark-im-chat-mode)
    (user-error "Not in a Lark chat buffer"))
  (let ((chat-id (and (boundp 'lark-im--chat-id) lark-im--chat-id))
        (chat-name (and (boundp 'lark-im--chat-name) lark-im--chat-name))
        (messages (and (boundp 'lark-im--messages) lark-im--messages))
        (msg-id (get-text-property (point) 'lark-message-id)))
    (unless chat-id
      (user-error "No chat ID in current buffer"))
    (unless msg-id
      (user-error "No message at point — place cursor on a message"))
    ;; Build thread text from recent messages for LLM context
    (let ((thread-text
           (mapconcat
            (lambda (msg)
              (let ((sender (or (alist-get 'sender_name msg)
                                (lark--get-nested msg 'sender 'name)
                                "unknown"))
                    (content (or (alist-get 'text msg)
                                 (alist-get 'content msg)
                                 (let ((body (alist-get 'body msg)))
                                   (when body
                                     (if (stringp body) body
                                       (or (alist-get 'text body)
                                           (alist-get 'content body)))))
                                 ""))
                    (id (or (alist-get 'message_id msg)
                            (alist-get 'id msg))))
                (format "%s%s: %s"
                        (if (equal id msg-id) ">>> " "")
                        sender content)))
            (seq-take (reverse (reverse messages)) 20)
            "\n")))
      (list :chat-id chat-id
            :chat-name chat-name
            :message-id msg-id
            :thread-text thread-text))))

;;;; Built-in workflow commands

;;;###autoload
(defun lark-ai-workflow-reply ()
  "Draft an AI-powered reply to the message at point.
Reads thread context from the current chat buffer, asks the LLM
to draft a reply, and opens a compose buffer for editing before send."
  (interactive)
  (let* ((ctx (lark-ai--collect-thread-context))
         (chat-id (plist-get ctx :chat-id))
         (chat-name (plist-get ctx :chat-name))
         (msg-id (plist-get ctx :message-id))
         (thread-text (plist-get ctx :thread-text))
         (skill-names (lark-ai-skills-select "reply message chat"))
         ;; Prose draft, not a plan — use the synthesis prompt so the
         ;; system message doesn't fight the user message's "no JSON".
         (system-prompt (lark-ai-skills-build-synthesis-prompt skill-names))
         (user-msg (format "Draft a concise, natural reply to the message marked with >>> in this chat thread. Output ONLY the reply text, nothing else — no JSON, no plan, no explanation.\n\nChat: %s\n\n%s"
                           (or chat-name chat-id)
                           thread-text)))
    (message "Lark AI: drafting reply...")
    (lark-ai--call-llm
     system-prompt user-msg
     (lambda (draft)
       (lark-ai--open-compose-buffer
        (string-trim draft) msg-id chat-id chat-name)))))

;;;###autoload
(defun lark-ai-workflow-standup ()
  "Generate a standup report: today's agenda + open tasks."
  (interactive)
  (lark-ai-ask "Generate a standup report for today: show my calendar agenda and open tasks, detect any scheduling conflicts, and list free time slots."))

;;;###autoload
(defun lark-ai-workflow-meeting-summary (&optional days)
  "Summarise meetings from the past DAYS days (default 7)."
  (interactive "P")
  (let ((n (or days 7)))
    (lark-ai-ask (format "Summarise my meetings from the past %d days. For each meeting, include the title, time, and any meeting notes available." n))))

;;;###autoload
(defun lark-ai-workflow-schedule (description)
  "Schedule a meeting described by DESCRIPTION using AI."
  (interactive "sDescribe the meeting: ")
  (lark-ai-ask (format "Schedule a meeting: %s. Find suitable times, check room availability, and create the event." description)))

(provide 'lark-ai)
;;; lark-ai.el ends here
