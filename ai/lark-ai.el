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
;; Three entry points:
;;   lark-ai-ask   — natural-language prompt (minibuffer)
;;   lark-ai-chat  — conversational AI buffer
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

(defvar lark-ai--conversation nil
  "Current conversation history as list of (:role ROLE :content TEXT).")

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
     (lark--log "Plan parse error: %s" (error-message-string err))
     nil)))

(defun lark-ai--extract-json (text)
  "Extract a JSON object from TEXT which may contain markdown fences."
  (let ((json-str text))
    ;; Strip markdown code fences
    (when (string-match "```\\(?:json\\)?\n?\\(\\(?:.\\|\n\\)*?\\)\n?```" json-str)
      (setq json-str (match-string 1 json-str)))
    ;; Find the outermost { ... }
    (when (string-match "\\({\\(?:.\\|\n\\)*}\\)" json-str)
      (setq json-str (match-string 1 json-str)))
    (json-parse-string json-str :object-type 'alist :array-type 'list)))

;;;; Plan review buffer

(defvar lark-ai-plan-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lark-ai-plan-execute)
    (define-key map (kbd "q")   #'lark-ai-plan-cancel)
    (define-key map (kbd "x")   #'lark-ai-plan-remove-step)
    map)
  "Keymap for `lark-ai-plan-mode'.")

(define-derived-mode lark-ai-plan-mode special-mode "Lark AI Plan"
  "Major mode for reviewing an AI execution plan.")

(defvar-local lark-ai-plan--steps nil
  "The plan steps in the current plan review buffer.")

(defvar-local lark-ai-plan--callback nil
  "Callback to invoke when the plan is confirmed.")

(defun lark-ai--display-plan (steps callback)
  "Display STEPS in a review buffer.  CALLBACK is called with confirmed steps."
  (let ((buf (get-buffer-create "*Lark AI Plan*")))
    (with-current-buffer buf
      (lark-ai-plan-mode)
      (setq lark-ai-plan--steps steps
            lark-ai-plan--callback callback)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Lark AI — Execution Plan\n" 'face 'bold)
                (make-string 40 ?─) "\n\n")
        (let ((idx 0))
          (dolist (step steps)
            (let ((desc (plist-get step :description))
                  (cmd (plist-get step :command))
                  (side-effect (plist-get step :side-effect))
                  (synthesize (plist-get step :synthesize))
                  (pg (plist-get step :parallel-group)))
              (insert (propertize (format "  [%d] " idx) 'face 'font-lock-constant-face)
                      (propertize desc 'face (if side-effect 'warning 'default))
                      (if side-effect
                          (propertize "  ⚠ writes" 'face 'error)
                        "")
                      (if synthesize
                          (propertize "  (AI synthesis)" 'face 'font-lock-comment-face)
                        "")
                      (if pg (format "  [group %d]" pg) "")
                      "\n")
              (when cmd
                (insert "      "
                        (propertize (format "lark-cli %s" (string-join cmd " "))
                                    'face 'font-lock-string-face)
                        "\n"))
              (insert "\n")
              (setq idx (1+ idx)))))
        (insert (make-string 40 ?─) "\n"
                "  " (propertize "RET" 'face 'bold) " Execute  "
                (propertize "x" 'face 'bold) " Remove step  "
                (propertize "q" 'face 'bold) " Cancel\n"))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun lark-ai-plan-execute ()
  "Execute the plan in the current plan buffer."
  (interactive)
  (let ((steps lark-ai-plan--steps)
        (callback lark-ai-plan--callback))
    (quit-window t)
    (when callback
      (funcall callback steps))))

(defun lark-ai-plan-cancel ()
  "Cancel the plan."
  (interactive)
  (quit-window t)
  (message "Lark AI: plan cancelled."))

(defun lark-ai-plan-remove-step ()
  "Remove the step closest to point."
  (interactive)
  (let* ((line (line-number-at-pos))
         ;; Rough: each step is ~3 lines, header is 3 lines
         (idx (max 0 (/ (- line 4) 3))))
    (when (and lark-ai-plan--steps (< idx (length lark-ai-plan--steps)))
      (setq lark-ai-plan--steps
            (append (seq-take lark-ai-plan--steps idx)
                    (seq-drop lark-ai-plan--steps (1+ idx))))
      (lark-ai--display-plan lark-ai-plan--steps lark-ai-plan--callback))))

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
  "Run lark-cli with CMD-ARGS, store result at INDEX, then call DONE-FN."
  (message "Lark AI: executing step %d..." index)
  (lark--run-command
   cmd-args
   (lambda (result)
     (push (cons index result) lark-ai--step-results)
     (funcall done-fn))
   nil
   :no-error t))

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

(defun lark-ai--call-gptel (system-prompt user-message callback)
  "Call the LLM via gptel."
  (unless (require 'gptel nil t)
    (user-error "gptel is not installed; install it or set `lark-ai-backend' to `http'"))
  (gptel-request user-message
    :system system-prompt
    :callback (lambda (response info)
                (if (stringp response)
                    (funcall callback response)
                  (message "Lark AI: LLM error: %S" info)))))

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
           (message "Lark AI HTTP error: %S" (plist-get status :error))
         (goto-char url-http-end-of-headers)
         (let* ((json-response (json-parse-buffer :object-type 'alist))
                (content (lark--get-nested json-response
                                           'choices 0 'message 'content)))
           (funcall callback (or content "")))))
     nil t t)))

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

;;;; Output rendering

(defun lark-ai--present (content &optional buffer-name)
  "Render CONTENT string in a dedicated output buffer."
  (let ((buf (get-buffer-create (or buffer-name "*Lark AI*"))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content "\n"))
      (goto-char (point-min))
      (if (fboundp 'markdown-mode)
          (markdown-mode)
        (special-mode))
      (setq header-line-format " Lark AI"))
    (pop-to-buffer buf)))

;;;; Entry points

;;;###autoload
(defun lark-ai-ask (prompt)
  "Ask the Lark AI assistant to execute a natural-language PROMPT.
Selects relevant lark-cli skills, sends to LLM, reviews the plan,
executes it, and presents results."
  (interactive "sLark AI: ")
  (message "Lark AI: thinking...")
  (let* ((context (lark-ai-context-format))
         (skill-names (lark-ai-skills-select prompt))
         (system-prompt (lark-ai-skills-build-system-prompt skill-names))
         (user-msg (if (string-empty-p context)
                       prompt
                     (format "%s\n\n%s" prompt context))))
    (lark--log "AI skills selected: %s" skill-names)
    (lark-ai--call-llm
     system-prompt user-msg
     (lambda (response)
       (let ((plan (lark-ai--parse-plan response)))
         (if (null plan)
             ;; No parseable plan — show raw response
             (lark-ai--present response)
           (setq lark-ai--last-plan plan)
           ;; Check if plan has only synthesis (no CLI calls)
           (if (seq-every-p (lambda (s) (plist-get s :synthesize)) plan)
               ;; Pure synthesis — extract instruction and respond
               (lark-ai--call-llm
                system-prompt
                (format "%s\n\n%s\nRespond with plain text using markdown."
                        prompt context)
                #'lark-ai--present)
             ;; Has CLI steps — show plan for review
             (lark-ai--display-plan
              plan
              (lambda (confirmed-steps)
                (message "Lark AI: executing plan...")
                (lark-ai--execute-plan
                 confirmed-steps
                 (lambda (results)
                   ;; Check if there's a synthesis step
                   (if (seq-find (lambda (s) (plist-get s :synthesize))
                                 confirmed-steps)
                       (lark-ai--synthesize results confirmed-steps
                                            system-prompt #'lark-ai--present)
                     ;; No synthesis: show raw results
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
Reads the current buffer context and prompts for an action."
  (interactive)
  (let* ((ctx (lark-ai-context))
         (domain (plist-get ctx :domain))
         (summary (plist-get ctx :summary)))
    (unless domain
      (user-error "Not in a Lark buffer"))
    (let ((action (read-string
                   (format "Lark AI action (%s): " summary))))
      (lark-ai-ask (format "%s\n\nContext: %s" action summary)))))

;;;###autoload
(defun lark-ai-chat ()
  "Open a conversational Lark AI chat buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Lark AI Chat*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'lark-ai-chat-mode)
        (lark-ai-chat-mode)))
    (pop-to-buffer buf)))

;;;; Chat mode

(defvar lark-ai-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'lark-ai-chat-send)
    map)
  "Keymap for `lark-ai-chat-mode'.")

(define-derived-mode lark-ai-chat-mode text-mode "Lark AI Chat"
  "Major mode for conversational Lark AI interaction.
Type your message and press \\[lark-ai-chat-send] to send."
  (setq-local lark-ai--conversation nil)
  (setq header-line-format
        " Lark AI Chat — C-c C-c to send"))

(defun lark-ai-chat-send ()
  "Send the current input in the chat buffer to the LLM."
  (interactive)
  (let* ((input (string-trim (buffer-substring-no-properties
                               (lark-ai-chat--input-start) (point-max))))
         (skill-names (lark-ai-skills-select input))
         (system-prompt (lark-ai-skills-build-system-prompt skill-names)))
    (when (string-empty-p input)
      (user-error "Empty message"))
    ;; Mark input as sent
    (goto-char (point-max))
    (insert "\n\n")
    (progn
      (insert (propertize (format "You: %s\n" input) 'face 'bold)
              (make-string 40 ?─) "\n")
      (insert (propertize "AI: " 'face 'bold) "thinking...\n")
      ;; Add to conversation
      (push (list :role "user" :content input) lark-ai--conversation)
      ;; Send
      (let ((response-start (- (point) (length "thinking...\n"))))
        (lark-ai--call-llm
         system-prompt input
         (lambda (response)
           (with-current-buffer (get-buffer "*Lark AI Chat*")
             (let ((inhibit-read-only t))
               ;; Try to parse as plan
               (let ((plan (lark-ai--parse-plan response)))
                 (if plan
                     (progn
                       (delete-region response-start (point-max))
                       (goto-char (point-max))
                       (insert (propertize "AI: " 'face 'bold)
                               "Generated a plan. Review it in the plan buffer.\n\n")
                       (lark-ai--display-plan
                        plan
                        (lambda (steps)
                          (lark-ai--execute-plan
                           steps
                           (lambda (results)
                             (if (seq-find (lambda (s) (plist-get s :synthesize)) steps)
                                 (lark-ai--synthesize
                                  results steps system-prompt
                                  (lambda (text)
                                    (lark-ai-chat--append-response text)))
                               (lark-ai-chat--append-response
                                (pp-to-string results))))))))
                   ;; Plain text response
                   (delete-region response-start (point-max))
                   (goto-char (point-max))
                   (insert (propertize "AI: " 'face 'bold) response "\n\n")))
               (push (list :role "assistant" :content response)
                     lark-ai--conversation)))))))))

(defun lark-ai-chat--input-start ()
  "Return the start position of the current input area."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward "^─\\{10,\\}" nil t)
        (progn (forward-line 1) (point))
      (point-min))))

(defun lark-ai-chat--append-response (text)
  "Append TEXT as an AI response in the chat buffer."
  (when-let ((buf (get-buffer "*Lark AI Chat*")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (propertize "AI: " 'face 'bold) text "\n\n"))))

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
