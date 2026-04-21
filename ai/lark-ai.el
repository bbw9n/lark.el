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

(require 'lark-core)
(require 'lark-ai-skills)
(require 'lark-ai-context)
(require 'json)

(defvar url-http-end-of-headers)

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

;;;; Internal state

(defvar lark-ai--last-plan nil
  "Last executed plan for debugging.")

(defvar lark-ai--step-results nil
  "Alist of (step-index . result) from plan execution.")

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
  (condition-case err
      (let* ((response (if (stringp json-string)
                           (lark-ai--extract-json json-string)
                         json-string))
             (plan-data (alist-get 'plan response)))
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
           plan-data)))
    (error
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
;; Each section (prompt, skills, log, plan, output) is a named
;; fragment that can be updated independently.

(require 'lark-ai-ui)

(defvar-local lark-ai-plan--steps nil
  "The plan steps in the current buffer.")

(defvar-local lark-ai-plan--callback nil
  "Callback to invoke when the plan is confirmed.")

(defvar-local lark-ai-plan--phase nil
  "Current phase: `loading', `review', `executing', or `done'.")

(defvar-local lark-ai-plan--step-status nil
  "Alist of (index . status).
Status is `pending', `running', `done', or `skipped'.")

(defconst lark-ai--buf-name "*Lark AI*"
  "Name of the AI buffer.")

(defvar lark-ai-plan-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lark-ai-ui-mode-map)
    (define-key map (kbd "RET") #'lark-ai-plan-execute)
    (define-key map (kbd "q")   #'lark-ai-plan-cancel)
    (define-key map (kbd "x")   #'lark-ai-plan-remove-step)
    map)
  "Keymap for the Lark AI buffer.")

(define-derived-mode lark-ai-plan-mode lark-ai-ui-mode "Lark AI"
  "Major mode for the Lark AI buffer.")

(defun lark-ai--get-buffer ()
  "Get or create the AI buffer."
  (let ((buf (get-buffer-create lark-ai--buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'lark-ai-plan-mode)
        (lark-ai-plan-mode)))
    buf))

;;; Fragment IDs

(defconst lark-ai--frag-header  "ai-header")
(defconst lark-ai--frag-prompt  "ai-prompt")
(defconst lark-ai--frag-skills  "ai-skills")
(defconst lark-ai--frag-log     "ai-log")
(defconst lark-ai--frag-plan    "ai-plan")
(defconst lark-ai--frag-sep     "ai-sep")
(defconst lark-ai--frag-output  "ai-output")

;;; Progress log — appends to the log fragment

(defun lark-ai--progress-log (fmt &rest args)
  "Add a timestamped entry to the log fragment."
  (let ((entry (format "[%s] %s\n"
                       (format-time-string "%H:%M:%S")
                       (apply #'format fmt args))))
    (when-let ((buf (get-buffer lark-ai--buf-name)))
      (with-current-buffer buf
        (lark-ai-ui-append-fragment lark-ai--frag-log entry)))))

;;; Build the initial buffer layout

(defun lark-ai--show-loading (prompt skill-names)
  "Initialize the AI buffer for a new session."
  (let ((buf (lark-ai--get-buffer)))
    (with-current-buffer buf
      (setq lark-ai-plan--steps nil
            lark-ai-plan--callback nil
            lark-ai-plan--phase 'loading
            lark-ai-plan--step-status nil)
      (lark-ai-ui-clear)
      ;; Header
      (lark-ai-ui-insert-fragment
       lark-ai--frag-header 'header "Lark AI" nil)
      (lark-ai-ui-insert-separator "ai-sep-header")
      ;; Prompt
      (lark-ai-ui-insert-fragment
       lark-ai--frag-prompt 'prompt
       (concat "▶ " prompt) nil)
      ;; Skills
      (lark-ai-ui-insert-fragment
       lark-ai--frag-skills 'skills
       (concat "Skills: " (string-join skill-names " · "))
       nil)
      ;; Log (starts empty, entries appended)
      (lark-ai-ui-insert-fragment
       lark-ai--frag-log 'log nil "")
      ;; Plan placeholder
      (lark-ai-ui-insert-fragment
       lark-ai--frag-plan 'plan nil
       (propertize "Waiting for LLM response...\n"
                   'face 'font-lock-comment-face)))
    (display-buffer buf)))

;;; Plan display — update the plan fragment

(defun lark-ai--display-plan (steps callback)
  "Show STEPS for review.  CALLBACK called with confirmed steps."
  (let ((buf (lark-ai--get-buffer)))
    (with-current-buffer buf
      (setq lark-ai-plan--steps steps
            lark-ai-plan--callback callback
            lark-ai-plan--phase 'review
            lark-ai-plan--step-status
            (mapcar (lambda (s) (cons (plist-get s :index) 'pending))
                    steps))
      (lark-ai--refresh-plan-fragment))
    (pop-to-buffer buf)))

(defun lark-ai--refresh-plan-fragment ()
  "Rebuild the plan fragment from current state."
  (let ((body (lark-ai--format-plan-body)))
    (lark-ai-ui-update-fragment
     lark-ai--frag-plan
     (lark-ai--plan-label)
     body)))

(defun lark-ai--plan-label ()
  "Return the plan section label for the current phase."
  (pcase lark-ai-plan--phase
    ('review    "Plan — confirm to execute")
    ('executing "Executing")
    ('done      "Complete")
    (_          "Plan")))

(defun lark-ai--format-plan-body ()
  "Format the plan steps as a string for the plan fragment."
  (with-temp-buffer
    (dolist (step lark-ai-plan--steps)
      (let* ((idx (plist-get step :index))
             (desc (plist-get step :description))
             (cmd (plist-get step :command))
             (side-effect (plist-get step :side-effect))
             (synthesize (plist-get step :synthesize))
             (pg (plist-get step :parallel-group))
             (status (alist-get idx lark-ai-plan--step-status))
             (indicator
              (pcase status
                ('done    (propertize "✓" 'face 'success))
                ('running (propertize "⟳" 'face 'warning))
                ('skipped (propertize "✗" 'face
                                      'font-lock-comment-face))
                (_        (propertize "○" 'face
                                      'font-lock-comment-face)))))
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
                  "\n"))))
    ;; Footer
    (pcase lark-ai-plan--phase
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
    (buffer-string)))

;;; Step status updates — only refresh the plan fragment

(defun lark-ai--update-step-status (index status)
  "Update step INDEX to STATUS and refresh plan."
  (when-let ((buf (get-buffer lark-ai--buf-name)))
    (with-current-buffer buf
      (setf (alist-get index lark-ai-plan--step-status) status)
      (lark-ai--refresh-plan-fragment))))

(defun lark-ai--mark-all-done ()
  "Mark execution as done and refresh plan."
  (when-let ((buf (get-buffer lark-ai--buf-name)))
    (with-current-buffer buf
      (setq lark-ai-plan--phase 'done)
      (lark-ai--refresh-plan-fragment))))


;;; Plan actions

(defun lark-ai-plan-execute ()
  "Execute the plan."
  (interactive)
  (unless (eq lark-ai-plan--phase 'review)
    (user-error "No plan to execute"))
  (let ((steps lark-ai-plan--steps)
        (callback lark-ai-plan--callback))
    (setq lark-ai-plan--phase 'executing
          lark-ai-plan--callback nil)
    (lark-ai--render)
    (when callback
      (funcall callback steps))))

(defun lark-ai-plan-cancel ()
  "Cancel the plan."
  (interactive)
  (setq lark-ai-plan--phase 'done
        lark-ai-plan--callback nil)
  (lark-ai--render)
  (lark-ai--progress-log "Cancelled."))

(defun lark-ai-plan-remove-step ()
  "Remove the step closest to point."
  (interactive)
  (unless (eq lark-ai-plan--phase 'review)
    (user-error "Can only remove steps during review"))
  (let* ((line (line-number-at-pos))
         (idx (max 0 (/ (- line 8) 2))))
    (when (and lark-ai-plan--steps (< idx (length lark-ai-plan--steps)))
      (setq lark-ai-plan--steps
            (append (seq-take lark-ai-plan--steps idx)
                    (seq-drop lark-ai-plan--steps (1+ idx))))
      (setq lark-ai-plan--step-status
            (mapcar (lambda (s) (cons (plist-get s :index) 'pending))
                    lark-ai-plan--steps))
      (lark-ai--render))))

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

(defun lark-ai--execute-plan (steps callback)
  "Execute STEPS sequentially/in-parallel, then call CALLBACK with all results.
Results is an alist of (index . parsed-json-or-string)."
  (setq lark-ai--step-results nil)
  (let ((remaining (copy-sequence steps)))
    (lark-ai--execute-next remaining callback)))

(defun lark-ai--execute-next (remaining callback)
  "Execute the next batch of REMAINING steps, then call CALLBACK."
  (if (null remaining)
      ;; All done
      (funcall callback lark-ai--step-results)
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
              (push (cons idx :synthesize) lark-ai--step-results)
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
                (push (cons idx '((skipped . t))) lark-ai--step-results)
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
              (push (cons idx nil) lark-ai--step-results)
              (setq pending (1- pending))
              (when (zerop pending)
                (lark-ai--execute-next rest callback))))))))))

(defun lark-ai--run-step (index cmd-args done-fn)
  "Run lark-cli with CMD-ARGS, store result at INDEX, then call DONE-FN.
$step-N references in CMD-ARGS are resolved from prior results."
  (let ((resolved (lark-ai--interpolate-cmd cmd-args lark-ai--step-results)))
    (lark-ai--update-step-status index 'running)
    (lark-ai--progress-log "Step %d: lark-cli %s" index (string-join resolved " "))
    (lark--run-command
     resolved
     (lambda (result)
       (push (cons index result) lark-ai--step-results)
       (lark-ai--update-step-status index 'done)
       (lark-ai--progress-log "Step %d: done" index)
       (funcall done-fn))
     nil
     :no-error t)))

;;;; LLM communication

(defun lark-ai--call-llm (system-prompt user-message callback)
  "Send SYSTEM-PROMPT and USER-MESSAGE to the LLM.
Call CALLBACK with the response text."
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
  "Call the LLM via gptel."
  (unless (require 'gptel nil t)
    (user-error "gptel is not installed; install it or set `lark-ai-backend' to `http'"))
  (let ((gptel-log-level nil)
        (inhibit-message t))
    (gptel-request user-message
                   :system system-prompt
                   :callback (lambda (response info)
                               (if (stringp response)
                                   (funcall callback response)
                                 (lark-ai--progress-log "LLM error: %S" info))))))

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
           (lark-ai--progress-log "HTTP error: %S" (plist-get status :error))
         (goto-char url-http-end-of-headers)
         (let* ((json-response (json-parse-buffer :object-type 'alist))
                (content (lark--get-nested json-response
                                           'choices 0 'message 'content)))
           (funcall callback (or content "")))))
     nil t t)))

;;; Streaming LLM calls

(defun lark-ai--call-llm-stream (system-prompt user-message callback)
  "Send to LLM with streaming, calling CALLBACK with full text when done.
Chunks are streamed into the *Lark AI* output section in real-time."
  (pcase lark-ai-backend
    ('gptel
     (lark-ai--call-gptel-stream system-prompt user-message callback))
    (_
     ;; Fallback: non-streaming
     (lark-ai--call-llm system-prompt user-message callback))))

(defun lark-ai--call-gptel-stream (system-prompt user-message callback)
  "Call LLM via gptel with streaming into the output fragment."
  (unless (require 'gptel nil t)
    (user-error "gptel is not installed"))
  ;; Create the output fragment for streaming
  (lark-ai--ensure-output-fragment)
  (let ((gptel-log-level nil)
        (inhibit-message t)
        (accumulated ""))
    (gptel-request user-message
                   :system system-prompt
                   :stream t
                   :callback
                   (lambda (response _info)
                     (cond
                      ((stringp response)
                       (setq accumulated
                             (concat accumulated response))
                       ;; Append chunk with incremental highlighting
                       (when-let ((buf (get-buffer lark-ai--buf-name)))
                         (with-current-buffer buf
                           (lark-ai-ui-append-fragment
                            lark-ai--frag-output response))))
                      ((eq response t)
                       (funcall callback accumulated))
                      ((and (consp response)
                            (eq (car response) 'reasoning))
                       nil)
                      (t
                       (lark-ai--progress-log
                        "LLM stream error")))))))

;;;; Synthesis (second LLM pass)

(defun lark-ai--synthesize (results plan system-prompt callback)
  "Send execution RESULTS back to the LLM for synthesis.
PLAN is the original plan, SYSTEM-PROMPT the skill context.
CALLBACK receives the synthesis text."
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
  "Like `lark-ai--synthesize' but streams the output."
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
    (lark-ai--call-llm-stream system-prompt user-msg #'ignore)))

;;;; Output rendering

(defun lark-ai--ensure-output-fragment ()
  "Ensure the output fragment exists in the AI buffer."
  (when-let ((buf (get-buffer lark-ai--buf-name)))
    (with-current-buffer buf
      (unless (lark-ai-ui--find-fragment lark-ai--frag-output)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (lark-ai-ui-insert-separator lark-ai--frag-sep)
          (lark-ai-ui-insert-fragment
           lark-ai--frag-output 'output "Output" ""))))))

(defun lark-ai--present (content &optional _buffer-name)
  "Display CONTENT in the output fragment."
  (let ((buf (lark-ai--get-buffer)))
    (with-current-buffer buf
      (lark-ai--ensure-output-fragment)
      (lark-ai-ui-update-fragment
       lark-ai--frag-output "Output" content))
    (pop-to-buffer buf)
    (lark-ai--scroll-to-output)))

(defun lark-ai--scroll-to-output ()
  "Scroll the AI buffer to the output fragment."
  (when-let ((buf (get-buffer lark-ai--buf-name)))
    (with-current-buffer buf
      (let ((region (lark-ai-ui--find-fragment
                     lark-ai--frag-output)))
        (when region
          (goto-char (car region))
          (forward-line 2)
          (recenter 0))))))

;;;; Entry points

;;;###autoload
(defun lark-ai-ask (prompt)
  "Ask the Lark AI assistant to execute a natural-language PROMPT.
Selects relevant lark-cli skills, sends to LLM, reviews the plan,
executes it, and presents results."
  (interactive "sLark AI: ")
  (let* ((context (lark-ai-context-format))
         (skill-names (lark-ai-skills-select prompt))
         (system-prompt (lark-ai-skills-build-system-prompt skill-names))
         (user-msg (if (string-empty-p context)
                       prompt
                     (format "%s\n\n%s" prompt context))))
    (lark-ai--show-loading prompt skill-names)
    (lark-ai--call-llm
     system-prompt user-msg
     (lambda (response)
       (let ((plan (lark-ai--parse-plan response)))
         (if (null plan)
             (progn
               (lark-ai--progress-log "No structured plan — showing raw response")
               (lark-ai--mark-all-done)
               (lark-ai--present response))
           (setq lark-ai--last-plan plan)
           (if (seq-every-p (lambda (s) (plist-get s :synthesize)) plan)
               (progn
                 (lark-ai--progress-log "Pure synthesis — streaming")
                 (lark-ai--mark-all-done)
                 (lark-ai--call-llm-stream
                  system-prompt
                  (format "%s\n\n%s\nRespond with plain text using markdown."
                          prompt context)
                  #'ignore))
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
                         (lark-ai--synthesize-stream
                          results confirmed-steps system-prompt))
                     (lark-ai--mark-all-done)
                     (lark-ai--present
                      (mapconcat
                       (lambda (pair)
                         (format "## Step %d\n```\n%s\n```"
                                 (car pair)
                                 (if (cdr pair) (pp-to-string (cdr pair)) "(no data)")))
                       (sort (copy-sequence results)
                             (lambda (a b) (< (car a) (car b))))
                       "\n\n"))))))))))))))

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
  (let ((has-item (and item
                       (cl-some (lambda (k) (plist-get item k))
                                '(:event-id :message-id :task-id
                                  :mail-id :doc-token :file-token
                                  :chat-id :meeting-id :token)))))
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
     '(("Other..." . nil)))))

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
         (system-prompt (lark-ai-skills-build-system-prompt skill-names))
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
