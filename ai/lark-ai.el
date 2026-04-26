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
(require 'lark-ai-skills)
(require 'lark-ai-context)
(require 'json)

(defvar url-http-end-of-headers)
(defvar url-request-method)
(defvar url-request-extra-headers)
(defvar url-request-data)

;; Forward declarations for IM module
(defvar lark-im--chat-id)
(defvar lark-im--chat-name)
(defvar lark-im--messages)
(declare-function lark-im-messages "lark-im")

;;;; Customization

(defcustom lark-ai-backend 'gptel
  "LLM backend for AI features.
`gptel' uses the gptel package (supports many providers).
`http' uses `url-retrieve' against an OpenAI-compatible endpoint."
  :type '(choice (const gptel) (const http))
  :group 'lark-ai)

(defcustom lark-ai-model nil
  "Model identifier for the LLM backend.
When nil, uses the backend's default (e.g., gptel's `gptel-model')."
  :type '(choice (const nil) string)
  :group 'lark-ai)

(defcustom lark-ai-http-endpoint nil
  "HTTP endpoint for the `http' backend.
An OpenAI-compatible chat completions URL, e.g.,
\"https://api.openai.com/v1/chat/completions\"."
  :type '(choice (const nil) string)
  :group 'lark-ai)

(defcustom lark-ai-http-api-key nil
  "API key for the `http' backend.
If nil, reads from environment variable LARK_AI_API_KEY."
  :type '(choice (const nil) string)
  :group 'lark-ai)

(defcustom lark-ai-confirm-side-effects t
  "When non-nil, pause for confirmation before executing write operations."
  :type 'boolean
  :group 'lark-ai)

(defcustom lark-ai-debug nil
  "When non-nil, log LLM requests, responses, and plan parsing to
the `*Lark AI Debug*' buffer.  Toggle interactively with
`lark-ai-toggle-debug'."
  :type 'boolean
  :group 'lark-ai)

;;;; Debug log

(defconst lark-ai--debug-buf-name "*Lark AI Debug*"
  "Name of the debug buffer.")

(define-derived-mode lark-ai-debug-mode special-mode "Lark AI Debug"
  "Major mode for the Lark AI debug log.")

(defun lark-ai--debug-buffer ()
  "Get or create the debug buffer."
  (let ((buf (get-buffer-create lark-ai--debug-buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'lark-ai-debug-mode)
        (lark-ai-debug-mode)))
    buf))

(defun lark-ai--debug-log (label fmt &rest args)
  "Append a labelled entry to the debug buffer.
LABEL is a short tag (e.g. \"REQUEST\", \"RESPONSE\").  FMT and
ARGS are passed to `format'.  No-op when `lark-ai-debug' is nil."
  (when lark-ai-debug
    (let ((entry (apply #'format fmt args)))
      (with-current-buffer (lark-ai--debug-buffer)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "\n=== [%s] %s ===\n%s\n"
                          (format-time-string "%H:%M:%S.%3N")
                          label entry)))))))

;;;###autoload
(defun lark-ai-toggle-debug ()
  "Toggle Lark AI debug logging.
When on, every LLM request/response and plan parse is logged to
`*Lark AI Debug*'."
  (interactive)
  (setq lark-ai-debug (not lark-ai-debug))
  (message "Lark AI debug: %s" (if lark-ai-debug "ON" "OFF"))
  (when lark-ai-debug
    (display-buffer (lark-ai--debug-buffer))))

;;;###autoload
(defun lark-ai-show-debug ()
  "Pop up the Lark AI debug buffer."
  (interactive)
  (display-buffer (lark-ai--debug-buffer)))

;;;; Session state
;;
;; A single buffer-local struct holds all conversation state for the
;; AI buffer.  Replaces the previous ~9 separate buffer-locals so that
;; (a) the state machine is inspectable in one place, (b) `lark-ai-reset'
;; can clear it atomically, and (c) abort can find the in-flight gptel
;; request handle.

(cl-defstruct lark-ai-session
  (turn 0)                ; current conversation turn (1-indexed when active)
  (phase 'idle)           ; idle | loading | review | executing | done
  (history nil)           ; ((ROLE . CONTENT) ...) — newest first
  (context "")            ; originating-buffer context string
  (skills nil)            ; skill names selected for this conversation
  (system-prompt nil)     ; full system prompt last sent
  (last-plan nil)         ; last parsed plan (for skill match-text)
  (steps nil)             ; current plan steps
  (callback nil)          ; pending callback for plan-execute
  (step-status nil)       ; alist (INDEX . STATUS) — pending/running/done/skipped
  (step-results nil)      ; alist (INDEX . RESULT) from execution
  (input-region-start nil) ; marker at start of the input region (separator)
  (input-start nil))      ; marker at start of editable input body

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

;;;; Plan data structure

;; A plan is a list of steps.  Each step is a plist:
;;   :command       - list of strings (lark-cli args), or nil for synthesis
;;   :description   - string
;;   :side-effect   - boolean
;;   :parallel-group - integer or nil
;;   :synthesize    - boolean
;;   :synthesis-instruction - string or nil

(defun lark-ai--parse-plan (json-string)
  "Parse a plan from the LLM's JSON-STRING response.
Returns a list of step plists, or nil on parse failure."
  (lark-ai--debug-log "PARSE-PLAN INPUT" "%s"
                      (if (stringp json-string) json-string
                        (format "%S" json-string)))
  (condition-case err
      (let* ((response (if (stringp json-string)
                           (lark-ai--extract-json json-string)
                         json-string))
             (plan-data (alist-get 'plan response))
             (steps
              (when (and plan-data (listp plan-data))
                (seq-map-indexed
                 (lambda (step idx)
                   (list :index idx
                         :command (let ((cmd (alist-get 'command step)))
                                    (when (and cmd (not (eq cmd :null)))
                                      (if (listp cmd) cmd (list cmd))))
                         :description (or (alist-get 'description step) "")
                         :side-effect (eq (alist-get 'side_effect step) t)
                         :parallel-group (let ((pg (alist-get 'parallel_group step)))
                                           (when (numberp pg) pg))
                         :synthesize (eq (alist-get 'synthesize step) t)
                         :synthesis-instruction (alist-get 'synthesis_instruction step)))
                 plan-data))))
        (lark-ai--debug-log
         "PARSE-PLAN RESULT"
         "extracted-keys=%S plan-key-type=%s steps=%d"
         (and (listp response) (mapcar #'car response))
         (cond ((null plan-data) "missing")
               ((listp plan-data) "list")
               (t (format "%s" (type-of plan-data))))
         (length steps))
        steps)
    (error
     (lark-ai--debug-log "PARSE-PLAN ERROR" "%s" (error-message-string err))
     (lark-ai--progress-log "Plan parse error: %s" (error-message-string err))
     nil)))

(defun lark-ai--extract-json (text)
  "Extract a JSON object from TEXT which may contain markdown fences."
  (let ((json-str text))
    ;; Strip markdown code fences (handle no newline after lang tag)
    (when (string-match "```[a-z]*[\n ]?\\(\\(?:.\\|\n\\)*?\\)[\n ]?```" json-str)
      (setq json-str (match-string 1 json-str)))
    ;; Find the outermost { ... }
    (when (string-match "\\({\\(?:.\\|\n\\)*}\\)" json-str)
      (setq json-str (match-string 1 json-str)))
    (json-parse-string json-str :object-type 'alist :array-type 'list)))

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
    (display-buffer buf)))

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

;;;; $step-N interpolation

(defun lark-ai--interpolate-cmd (cmd-args results)
  "Replace $step-N references in CMD-ARGS using RESULTS.
RESULTS is an alist of (index . parsed-json).
Supports:
  $step-0           — full result as JSON string
  $step-0.field     — top-level field
  $step-0.a.b       — nested field access
  $step-0.items[0].id — array index + field
  $step-0.items[*].id — collect from all elements"
  (mapcar #'lark-ai--interpolate-arg
          (mapcar (lambda (arg) (cons arg results)) cmd-args)))

(defun lark-ai--interpolate-arg (arg-and-results)
  "Interpolate a single ARG-AND-RESULTS (ARG . RESULTS) pair."
  (let ((arg (car arg-and-results))
        (results (cdr arg-and-results)))
    (if (not (string-match-p "\\$step-[0-9]" arg))
        arg
      ;; Process all $step-N references in this arg
      (let ((pos 0)
            (out ""))
        (while (string-match
                "\\$step-\\([0-9]+\\)\\(\\(?:[.[][^[:space:]]*\\)?\\)"
                arg pos)
          (let* ((match-beg (match-beginning 0))
                 (match-end (match-end 0))
                 (idx (string-to-number (match-string 1 arg)))
                 (path (match-string 2 arg))
                 (data (alist-get idx results))
                 (replacement
                  (if (null data)
                      (substring arg match-beg match-end)
                    (if (or (null path) (string-empty-p path))
                        (if (stringp data) data
                          (json-encode data))
                      (let ((val (lark-ai--resolve-path
                                  data (substring path 1))))
                        (cond
                         ((null val) "")
                         ((stringp val) val)
                         ((numberp val) (number-to-string val))
                         (t (json-encode val))))))))
            (setq out (concat out
                              (substring arg pos match-beg)
                              replacement)
                  pos match-end)))
        (concat out (substring arg pos))))))

(defun lark-ai--resolve-path (data path)
  "Resolve a dot/bracket PATH against DATA.
PATH examples: \"field\", \"a.b\", \"items[0].id\",
\"items[*].id\"."
  (let ((segments (lark-ai--parse-path path))
        (current data))
    (catch 'done
      (dolist (seg segments)
        (when (null current) (throw 'done nil))
        (pcase seg
          (`(index ,n)
           (if (and (listp current) (<= 0 n) (< n (length current)))
               (setq current (nth n current))
             (throw 'done nil)))
          (`(wildcard ,field)
           ;; Collect field from every element in the list
           (throw 'done
                  (if (listp current)
                      (mapconcat
                       (lambda (item)
                         (let ((v (lark-ai--resolve-path item field)))
                           (cond
                            ((null v) "")
                            ((stringp v) v)
                            ((numberp v) (number-to-string v))
                            (t (json-encode v)))))
                       current ",")
                    nil)))
          (`(field ,name)
           (setq current
                 (if (listp current)
                     (alist-get (intern name) current)
                   nil)))))
      current)))

(defun lark-ai--parse-path (path)
  "Parse PATH into a list of segments.
Returns list of (field NAME), (index N), or (wildcard REST)."
  (let ((segments nil)
        (remaining path))
    (while (and remaining (not (string-empty-p remaining)))
      (cond
       ;; [*].rest — wildcard collect
       ((string-match "^\\[\\*\\]\\.?" remaining)
        (let ((rest (substring remaining (match-end 0))))
          (push (list 'wildcard (if (string-empty-p rest) nil rest))
                segments)
          (setq remaining nil)))
       ;; [N] — array index
       ((string-match "^\\[\\([0-9]+\\)\\]\\.?" remaining)
        (push (list 'index (string-to-number (match-string 1 remaining)))
              segments)
        (setq remaining (substring remaining (match-end 0))))
       ;; field name (up to next . or [)
       ((string-match "^\\([^.[]+\\)\\.?" remaining)
        (push (list 'field (match-string 1 remaining)) segments)
        (setq remaining (substring remaining (match-end 0))))
       (t (setq remaining nil))))
    (nreverse segments)))

;;;; Plan execution

(defun lark-ai--push-result (idx result)
  "Push (IDX . RESULT) onto the session's step-results."
  (push (cons idx result)
        (lark-ai-session-step-results (lark-ai--session))))

(defun lark-ai--execute-plan (steps callback)
  "Execute STEPS sequentially/in-parallel, then call CALLBACK with all results.
Results is an alist of (index . parsed-json-or-string)."
  (setf (lark-ai-session-step-results (lark-ai--session)) nil)
  (let ((remaining (copy-sequence steps)))
    (lark-ai--execute-next remaining callback)))

(defun lark-ai--execute-next (remaining callback)
  "Execute the next batch of REMAINING steps, then call CALLBACK."
  (if (null remaining)
      ;; All done
      (funcall callback
               (lark-ai-session-step-results (lark-ai--session)))
    ;; Find the next parallel group
    (let* ((first (car remaining))
           (group (plist-get first :parallel-group))
           batch rest)
      (if group
          ;; Collect all steps in the same parallel group
          (dolist (step remaining)
            (if (equal (plist-get step :parallel-group) group)
                (push step batch)
              (push step rest)))
        ;; No group: just take the first step
        (setq batch (list first)
              rest (cdr remaining)))
      (setq batch (nreverse batch)
            rest (nreverse rest))
      ;; Execute batch
      (let ((pending (length batch)))
        (dolist (step batch)
          (let ((idx (plist-get step :index))
                (cmd (plist-get step :command))
                (synthesize (plist-get step :synthesize))
                (side-effect (plist-get step :side-effect)))
            (cond
             ;; Synthesis step: defer to after we have results
             (synthesize
              (lark-ai--push-result idx :synthesize)
              (setq pending (1- pending))
              (when (zerop pending)
                (lark-ai--execute-next rest callback)))
             ;; Side-effect step: confirm first
             ((and side-effect lark-ai-confirm-side-effects)
              (if (yes-or-no-p (format "Execute: lark-cli %s? "
                                       (string-join cmd " ")))
                  (lark-ai--run-step idx cmd
                                     (lambda ()
                                       (setq pending (1- pending))
                                       (when (zerop pending)
                                         (lark-ai--execute-next rest callback))))
                (lark-ai--push-result idx '((skipped . t)))
                (lark-ai--update-step-status idx 'skipped)
                (lark-ai--progress-log "Step %d: skipped by user" idx)
                (setq pending (1- pending))
                (when (zerop pending)
                  (lark-ai--execute-next rest callback))))
             ;; Normal read step
             (cmd
              (lark-ai--run-step idx cmd
                                 (lambda ()
                                   (setq pending (1- pending))
                                   (when (zerop pending)
                                     (lark-ai--execute-next rest callback)))))
             ;; No command and not synthesize: skip
             (t
              (lark-ai--push-result idx nil)
              (setq pending (1- pending))
              (when (zerop pending)
                (lark-ai--execute-next rest callback))))))))))

(defun lark-ai--run-step (index cmd-args done-fn)
  "Run lark-cli with CMD-ARGS, store result at INDEX, then call DONE-FN.
$step-N references in CMD-ARGS are resolved from prior results."
  (let* ((session (lark-ai--session))
         (resolved (lark-ai--interpolate-cmd
                    cmd-args
                    (lark-ai-session-step-results session))))
    (lark-ai--update-step-status index 'running)
    (lark-ai--progress-log "Step %d: lark-cli %s" index (string-join resolved " "))
    (lark--run-command
     resolved
     (lambda (result)
       (lark-ai--push-result index result)
       (lark-ai--update-step-status index 'done)
       (lark-ai--progress-log "Step %d: done" index)
       (funcall done-fn))
     nil
     :no-error t)))

;;;; LLM communication

(defun lark-ai--call-llm (system-prompt user-message callback)
  "Send SYSTEM-PROMPT and USER-MESSAGE to the LLM.
Call CALLBACK with the response text."
  (lark-ai--debug-log
   "REQUEST" "backend=%s\n--- system ---\n%s\n--- user ---\n%s"
   lark-ai-backend system-prompt user-message)
  (pcase lark-ai-backend
    ('gptel (lark-ai--call-gptel system-prompt user-message callback))
    ('http  (lark-ai--call-http system-prompt user-message callback))
    (_      (user-error "Unknown lark-ai-backend: %s" lark-ai-backend))))

;; gptel backend
(declare-function gptel-request "gptel")
(declare-function gptel--parse-response "gptel")
(defvar gptel-model)
(defvar gptel-backend)

(defvar gptel-log-level)

(defun lark-ai--call-gptel (system-prompt user-message callback)
  "Call the LLM via gptel.
Runs `gptel-request' inside the AI buffer so `gptel-abort'
against that buffer can find and cancel the in-flight request."
  (unless (require 'gptel nil t)
    (user-error "gptel is not installed; install it or set `lark-ai-backend' to `http'"))
  (with-current-buffer (lark-ai--get-buffer)
    (let ((gptel-log-level nil)
          (inhibit-message t))
      (gptel-request user-message
                     :system system-prompt
                     :callback (lambda (response info)
                                 (cond
                                  ((stringp response)
                                   (lark-ai--debug-log
                                    "RESPONSE (gptel)" "%s" response)
                                   (funcall callback response))
                                  (t
                                   (lark-ai--debug-log
                                    "ERROR (gptel)"
                                    "response=%S info=%S" response info)
                                   (lark-ai--progress-log
                                    "LLM error: %S" info))))))))

;; HTTP backend (OpenAI-compatible)
(defun lark-ai--call-http (system-prompt user-message callback)
  "Call the LLM via raw HTTP to an OpenAI-compatible endpoint."
  (let* ((url (or lark-ai-http-endpoint
                  (user-error "Set `lark-ai-http-endpoint' for the http backend")))
         (key (or lark-ai-http-api-key
                  (getenv "LARK_AI_API_KEY")
                  (user-error "Set `lark-ai-http-api-key' or LARK_AI_API_KEY env var")))
         (model (or lark-ai-model "claude-sonnet-4-20250514"))
         (payload (json-encode
                   `((model . ,model)
                     (messages . [((role . "system") (content . ,system-prompt))
                                  ((role . "user") (content . ,user-message))])
                     (max_tokens . 4096))))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" key))))
         (url-request-data payload))
    (url-retrieve
     url
     (lambda (status)
       (if (plist-get status :error)
           (progn
             (lark-ai--debug-log
              "ERROR (http)" "%S" (plist-get status :error))
             (lark-ai--progress-log
              "HTTP error: %S" (plist-get status :error)))
         (goto-char url-http-end-of-headers)
         (let* ((json-response (json-parse-buffer :object-type 'alist))
                (content (lark--get-nested json-response
                                           'choices 0 'message 'content)))
           (lark-ai--debug-log "RESPONSE (http)" "%s" (or content ""))
           (funcall callback (or content "")))))
     nil t t)))

;;; Streaming LLM calls

(defun lark-ai--call-llm-stream (system-prompt user-message callback
                                                &optional chunk-handler)
  "Send to LLM with streaming, calling CALLBACK with full text when done.
If CHUNK-HANDLER is non-nil, it is called as (CHUNK-HANDLER CHUNK)
on each streamed chunk and the default behaviour of appending
chunks to the output fragment is skipped — used by the planning
call to stream raw JSON into the log fragment instead.
HTTP backend has no streaming, so the fallback is a single
non-streaming call (CHUNK-HANDLER is not invoked)."
  (pcase lark-ai-backend
    ('gptel
     (lark-ai--call-gptel-stream system-prompt user-message
                                 callback chunk-handler))
    (_
     ;; Fallback: non-streaming
     (lark-ai--call-llm system-prompt user-message callback))))

(defun lark-ai--call-gptel-stream (system-prompt user-message callback
                                                  &optional chunk-handler)
  "Call LLM via gptel with streaming.
When CHUNK-HANDLER is nil, chunks are appended to the output
fragment (synthesis path).  When non-nil, CHUNK-HANDLER is
invoked with each chunk and the output fragment is left
untouched — the planning path uses this to stream raw JSON into
the log fragment.
Runs `gptel-request' inside the AI buffer so `gptel-abort' can
find and cancel the in-flight stream."
  (unless (require 'gptel nil t)
    (user-error "gptel is not installed"))
  ;; Only create the output fragment for the default (synthesis) path.
  (unless chunk-handler
    (lark-ai--ensure-output-fragment))
  (lark-ai--debug-log
   "STREAM REQUEST" "--- system ---\n%s\n--- user ---\n%s"
   system-prompt user-message)
  (let ((gptel-log-level nil)
        (inhibit-message t)
        (accumulated "")
        (chunks 0))
    (with-current-buffer (lark-ai--get-buffer)
      (gptel-request user-message
                   :system system-prompt
                   :stream t
                   :callback
                   (lambda (response info)
                     (cond
                      ((stringp response)
                       (setq accumulated
                             (concat accumulated response)
                             chunks (1+ chunks))
                       (lark-ai--debug-log
                        "STREAM CHUNK"
                        "#%d (%d chars): %s"
                        chunks (length response) response)
                       (when-let ((buf (get-buffer lark-ai--buf-name)))
                         (with-current-buffer buf
                           (if chunk-handler
                               (funcall chunk-handler response)
                             (lark-ai-ui-append-fragment
                              (lark-ai--frag "output") response)))))
                      ((eq response t)
                       (lark-ai--debug-log
                        "STREAM DONE"
                        "%d chunks, %d total chars\n--- accumulated ---\n%s"
                        chunks (length accumulated) accumulated)
                       (funcall callback accumulated))
                      ((and (consp response)
                            (eq (car response) 'reasoning))
                       nil)
                      (t
                       (lark-ai--debug-log
                        "STREAM ERROR" "response=%S info=%S" response info)
                       (lark-ai--progress-log
                        "LLM stream error"))))))))

;;;; Synthesis (second LLM pass)

(defun lark-ai--synthesize (results plan system-prompt callback)
  "Send execution RESULTS back to the LLM for synthesis.
PLAN is the original plan.  SYSTEM-PROMPT must be the synthesis
prompt (see `lark-ai-skills-build-synthesis-prompt') — passing
the planning prompt here makes the model emit another JSON plan
instead of the prose answer.  CALLBACK receives the synthesis text."
  ;; Find the synthesis step
  (let* ((synth-step (seq-find (lambda (s) (plist-get s :synthesize)) plan))
         (instruction (or (and synth-step
                               (plist-get synth-step :synthesis-instruction))
                          "Summarize the results clearly and concisely."))
         ;; Build results text
         (results-text
          (mapconcat
           (lambda (pair)
             (let ((idx (car pair))
                   (data (cdr pair)))
               (unless (eq data :synthesize)
                 (let ((step (seq-find (lambda (s) (= (plist-get s :index) idx)) plan)))
                   (format "### Step %d: %s\n```json\n%s\n```\n"
                           idx
                           (or (and step (plist-get step :description)) "")
                           (if data
                               (json-encode data)
                             "(no data)"))))))
           (sort (copy-sequence results)
                 (lambda (a b) (< (car a) (car b))))
           "\n"))
         (user-msg (format "Here are the execution results:\n\n%s\n\nInstruction: %s\n\nRespond with plain text (not JSON). Use markdown formatting."
                           results-text instruction)))
    (lark-ai--call-llm system-prompt user-msg callback)))

(defun lark-ai--synthesize-stream (results plan system-prompt)
  "Like `lark-ai--synthesize' but streams the output.
SYSTEM-PROMPT must be the synthesis prompt (no JSON mandate)."
  (let* ((synth-step (seq-find (lambda (s) (plist-get s :synthesize)) plan))
         (instruction (or (and synth-step
                               (plist-get synth-step :synthesis-instruction))
                          "Summarize the results clearly and concisely."))
         (results-text
          (mapconcat
           (lambda (pair)
             (let ((idx (car pair))
                   (data (cdr pair)))
               (unless (eq data :synthesize)
                 (let ((step (seq-find (lambda (s) (= (plist-get s :index) idx)) plan)))
                   (format "### Step %d: %s\n```json\n%s\n```\n"
                           idx
                           (or (and step (plist-get step :description)) "")
                           (if data (json-encode data) "(no data)"))))))
           (sort (copy-sequence results)
                 (lambda (a b) (< (car a) (car b))))
           "\n"))
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

(defcustom lark-ai-history-truncate-chars 800
  "Maximum characters retained per prior assistant message in the
follow-up prompt.  Long plan JSON or full document dumps from
earlier turns are clipped to this length so they don't flood the
LLM and bias it into re-running the previous plan."
  :type 'integer
  :group 'lark-ai)

(defun lark-ai--build-user-message (prompt context history)
  "Build the LLM user message for PROMPT.
Uses a labelled, sectioned structure so the new question is the
focus — prior turns and originating-buffer context are framed as
background.  HISTORY is the session history (newest first); long
assistant entries are truncated per `lark-ai-history-truncate-chars'
to keep the previous plan JSON from biasing the LLM into repeating
itself."
  (let* ((clip
          (lambda (text)
            (if (> (length text) lark-ai-history-truncate-chars)
                (concat (substring text 0 lark-ai-history-truncate-chars)
                        "\n…[truncated]")
              text)))
         (history-block
          (when history
            (mapconcat
             (lambda (entry)
               (let ((role (car entry))
                     (text (cdr entry)))
                 (format "- %s: %s"
                         (capitalize role)
                         ;; User prompts are typically short — only
                         ;; clip assistant content.
                         (if (equal role "assistant")
                             (funcall clip text)
                           text))))
             (reverse history)
             "\n")))
         (sections nil))
    (when history-block
      (push (concat "## Prior turns (background only)\n" history-block)
            sections))
    (when (and context (not (string-empty-p context)))
      (push (concat "## Originating buffer context\n" context)
            sections))
    (push (concat "## Current request\n" prompt
                  (when history
                    "\n\nAddress only the request above.  Do not\
 repeat prior plan steps unless the user explicitly asks; if the\
 question is a clarification or refinement of earlier results,\
 answer it directly with a synthesis step instead of re-fetching."))
          sections)
    (mapconcat #'identity (nreverse sections) "\n\n")))

;;;###autoload
(defun lark-ai-ask (prompt)
  "Ask the Lark AI assistant to execute a natural-language PROMPT.
Selects relevant lark-cli skills, sends to LLM, reviews the plan,
executes it, and presents results."
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
                                "\n"))
         (matched-skills (lark-ai-skills-select prompt match-text))
         ;; Default behaviour: reuse the previous turn's skills as the
         ;; base set on follow-ups so domain continuity is preserved
         ;; even when the new prompt + match-text picks nothing new.
         ;; Fresh asks (from a real buffer) start clean.
         (skill-names (delete-dups
                       (if (and in-ai-buf prev-skills)
                           (append prev-skills matched-skills)
                         matched-skills)))
         (system-prompt (lark-ai-skills-build-system-prompt skill-names))
         ;; Synthesis pass uses a different system prompt (no JSON
         ;; mandate) so the model produces prose.  Built once here
         ;; from the same skill set so both passes share context.
         (synth-prompt (lark-ai-skills-build-synthesis-prompt skill-names))
         (user-msg (lark-ai--build-user-message prompt context history)))
    (lark-ai--debug-log
     "SKILLS" "prev=%S matched=%S → final=%S"
     prev-skills matched-skills skill-names)
    (lark-ai--show-loading prompt skill-names)
    ;; Persist for follow-ups.  Originating context is set only on a
    ;; fresh ask (from a real buffer) so a follow-up doesn't overwrite
    ;; the captured context with the empty AI-buffer context.
    (setf (lark-ai-session-system-prompt session) system-prompt
          (lark-ai-session-skills session) skill-names)
    (unless in-ai-buf
      (setf (lark-ai-session-context session) context))
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
                       "\n\n")))))))))))
     ;; Chunk handler — stream raw planning JSON into the log
     ;; fragment so the user sees the model generating.  Wrapped
     ;; in the comment face to blend with progress lines.
     (lambda (chunk)
       (lark-ai-ui-append-fragment
        (lark-ai--frag "log")
        (propertize chunk 'face 'font-lock-comment-face))))))

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
