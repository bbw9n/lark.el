;;; lark-ai-protocol.el --- Pure helpers for the lark.el AI layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; Pure (UI-free, session-free) helpers used by the AI layer:
;;
;;   - debug logging          — `lark-ai-debug', `lark-ai--debug-log',
;;                              `lark-ai-toggle-debug', `lark-ai-show-debug'
;;   - plan JSON parsing      — `lark-ai--parse-plan', `lark-ai--extract-json'
;;   - $step-N interpolation  — `lark-ai--interpolate-cmd' and helpers
;;   - user-message assembly  — `lark-ai--build-user-message'
;;
;; This module has no UI dependency and no knowledge of the AI
;; session struct.  It is the testable foundation under
;; `lark-ai-llm.el', `lark-ai-runner.el', and `lark-ai.el'.

;;; Code:

(require 'cl-lib)
(require 'json)

;;;; Debug logging

(defcustom lark-ai-debug nil
  "When non-nil, log LLM requests, responses, and plan parsing to
the `*Lark AI Debug*' buffer.  Toggle interactively with
`lark-ai-toggle-debug'."
  :type 'boolean
  :group 'lark-ai)

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

;;;; Plan data structure
;;
;; A plan is a list of step plists with these keys:
;;   :index                - integer (0-based, assigned during parse)
;;   :command              - list of strings (lark-cli args), or nil for synthesis
;;   :description          - string
;;   :side-effect          - boolean
;;   :parallel-group       - integer or nil
;;   :synthesize           - boolean
;;   :synthesis-instruction - string or nil

(defun lark-ai--parse-plan (json-string)
  "Parse a plan from the LLM's JSON-STRING response.
Returns a list of step plists, or nil on parse failure.  Pure:
emits debug entries via `lark-ai--debug-log' but does not touch
any UI buffer."
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

;;;; Session struct
;;
;; Defined here (rather than in `lark-ai.el') so that
;; `lark-ai-runner.el' can use the auto-generated accessors and
;; `setf' forms at byte-compile time without depending on the UI
;; module.  The struct is the union of conversation state, plan
;; state and UI state — fields are buffer-local-friendly (e.g.
;; markers) but the definition itself is pure data.

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

;;;; User message assembly

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

(provide 'lark-ai-protocol)
;;; lark-ai-protocol.el ends here
