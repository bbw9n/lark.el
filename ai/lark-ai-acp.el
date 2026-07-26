;;; lark-ai-acp.el --- ACP agent backend for the lark.el AI layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; The `acp' backend for `lark-ai-backend' — drives a locally
;; installed ACP (Agent Client Protocol) agent such as Claude Code
;; (via the claude-code-acp adapter) or Gemini CLI as the text engine
;; behind the `lark-ai--call-llm' / `lark-ai--call-llm-stream' seams.
;; This is the agent-shell approach (acp.el under the hood), but
;; headless: no shell UI, and no API key — the agent's own login is
;; reused.
;;
;; One agent process (ACP client) is kept alive across calls; each
;; LLM call opens a FRESH ACP session so the stateless "system + user
;; in, text out" contract of the other backends holds.  Lark's agent
;; loop re-sends its own transcript every iteration, so a persistent
;; session would duplicate context on the agent side.
;;
;; The agent's own agentic abilities are suppressed: file-system
;; capabilities are not advertised at initialize time and any
;; session/request_permission is auto-declined, so the agent answers
;; in text rather than running its own tools.  (A future strategy
;; that delegates the whole loop to the agent — mapping ACP tool
;; calls onto lark's tool-call cards and permission requests onto the
;; confirm-writes gate — can reuse this client plumbing.)
;;
;; In-flight requests are tracked per session id, so a background
;; brief and an interactive turn can safely overlap on the one
;; client.
;;
;; acp.el is a soft dependency, mirroring how the `gptel' backend
;; treats gptel: required at call time with a `user-error' fallback.

;;; Code:

(require 'map)
(require 'seq)
(require 'lark-ai-protocol)   ; `lark-ai--debug-log'

;; acp.el forward declarations — soft-required in
;; `lark-ai-acp--ensure-client'.
(declare-function acp-make-client "ext:acp")
(declare-function acp-send-request "ext:acp")
(declare-function acp-send-response "ext:acp")
(declare-function acp-send-notification "ext:acp")
(declare-function acp-shutdown "ext:acp")
(declare-function acp-subscribe-to-notifications "ext:acp")
(declare-function acp-subscribe-to-requests "ext:acp")
(declare-function acp-subscribe-to-errors "ext:acp")
(declare-function acp-make-initialize-request "ext:acp")
(declare-function acp-make-session-new-request "ext:acp")
(declare-function acp-make-session-prompt-request "ext:acp")
(declare-function acp-make-session-cancel-notification "ext:acp")
(declare-function acp-make-session-request-permission-response "ext:acp")
(declare-function acp-make-error "ext:acp")

;; Forward declaration — lives in `lark-ai.el' (UI wiring).
(declare-function lark-ai--progress-log "lark-ai" (fmt &rest args))

;;;; Customization

(defcustom lark-ai-acp-command "claude-code-acp"
  "Program that speaks ACP on stdio for the `acp' backend.
Any Agent Client Protocol agent works, e.g.:
- \"claude-code-acp\" — Claude Code (npm i -g @zed-industries/claude-code-acp)
- \"gemini\"          — Gemini CLI (set `lark-ai-acp-command-params'
                        to \\='(\"--experimental-acp\"))"
  :type 'string
  :group 'lark-ai)

(defcustom lark-ai-acp-command-params nil
  "Extra command-line arguments passed to `lark-ai-acp-command'."
  :type '(repeat string)
  :group 'lark-ai)

(defcustom lark-ai-acp-environment-variables nil
  "Environment entries (\"VAR=value\" strings) for the agent process."
  :type '(repeat string)
  :group 'lark-ai)

(defcustom lark-ai-acp-cwd "~"
  "Working directory handed to each new ACP session."
  :type 'directory
  :group 'lark-ai)

;;;; State

(defvar lark-ai-acp--client nil
  "The live ACP client (agent process), shared across calls.")

(defvar lark-ai-acp--initialized nil
  "Non-nil once the current client answered the initialize request.")

(defvar lark-ai-acp--requests nil
  "Alist of (SESSION-ID . PLIST) for in-flight prompt requests.
PLIST keys: `:accumulated' (streamed text so far), `:on-chunk'
\(per-chunk callback or nil), `:callback' (final-text callback).")

;;;; Pure message helpers (unit-tested without acp.el)

(defun lark-ai-acp--notification-chunk (notification)
  "Return (SESSION-ID . TEXT) when NOTIFICATION is an agent message chunk.
NOTIFICATION is a decoded JSON-RPC object.  Returns nil for every
other notification kind (thought chunks, tool-call updates, …)."
  (when (equal (map-elt notification 'method) "session/update")
    (let ((params (map-elt notification 'params)))
      (when (equal (map-nested-elt params '(update sessionUpdate))
                   "agent_message_chunk")
        (cons (map-elt params 'sessionId)
              (or (map-nested-elt params '(update content text)) ""))))))

(defun lark-ai-acp--reject-option-id (options)
  "Return the optionId of a rejecting entry in OPTIONS, or nil.
OPTIONS is the sequence from a session/request_permission request;
prefers `reject_once' over `reject_always'."
  (let ((by-kind (lambda (kind)
                   (map-elt (seq-find (lambda (opt)
                                        (equal (map-elt opt 'kind) kind))
                                      options)
                            'optionId))))
    (or (funcall by-kind "reject_once")
        (funcall by-kind "reject_always"))))

(defun lark-ai-acp--prompt-text (system-prompt user-message)
  "Combine SYSTEM-PROMPT and USER-MESSAGE into one prompt string.
ACP prompts have no system-message slot, so the system prompt is
prepended in a tagged block the agent treats as instructions."
  (if (and system-prompt (not (string-empty-p system-prompt)))
      (concat "<system-instructions>\n" system-prompt
              "\n</system-instructions>\n\n" user-message)
    user-message))

;;;; Client lifecycle

(defun lark-ai-acp--reset-state ()
  "Forget the client and all in-flight bookkeeping."
  (setq lark-ai-acp--client nil
        lark-ai-acp--initialized nil
        lark-ai-acp--requests nil))

(defun lark-ai-acp--ensure-client ()
  "Return a live ACP client, creating and subscribing it if needed."
  (unless (require 'acp nil t)
    (user-error "acp.el is not installed; install it or change `lark-ai-backend'"))
  ;; A dead agent process invalidates the whole client state.
  (when-let ((proc (and lark-ai-acp--client
                        (map-elt lark-ai-acp--client :process))))
    (unless (process-live-p proc)
      (lark-ai-acp--reset-state)))
  (unless lark-ai-acp--client
    (setq lark-ai-acp--client
          (acp-make-client
           :command lark-ai-acp-command
           :command-params lark-ai-acp-command-params
           :environment-variables lark-ai-acp-environment-variables))
    (acp-subscribe-to-notifications
     :client lark-ai-acp--client
     :on-notification #'lark-ai-acp--on-notification)
    (acp-subscribe-to-requests
     :client lark-ai-acp--client
     :on-request #'lark-ai-acp--on-request)
    (acp-subscribe-to-errors
     :client lark-ai-acp--client
     :on-error #'lark-ai-acp--on-error))
  lark-ai-acp--client)

(defun lark-ai-acp--with-initialized (client then)
  "Run THEN once CLIENT has completed the ACP initialize handshake.
File-system capabilities are deliberately not advertised so the
agent doesn't route file access through us."
  (if lark-ai-acp--initialized
      (funcall then)
    (acp-send-request
     :client client
     :request (acp-make-initialize-request
               :protocol-version 1
               :client-info '((name . "lark.el")
                              (title . "Lark AI")
                              (version . "0.1.0")))
     :on-success (lambda (_response)
                   (setq lark-ai-acp--initialized t)
                   (funcall then))
     :on-failure #'lark-ai-acp--report-failure)))

;;;; Incoming traffic

(defun lark-ai-acp--on-notification (notification)
  "Accumulate agent message chunks from NOTIFICATION into their request."
  (pcase (lark-ai-acp--notification-chunk notification)
    (`(,session-id . ,text)
     (when-let ((req (assoc session-id lark-ai-acp--requests)))
       (setcdr req (plist-put (cdr req) :accumulated
                              (concat (plist-get (cdr req) :accumulated) text)))
       (when-let ((on-chunk (plist-get (cdr req) :on-chunk)))
         (funcall on-chunk text))))))

(defun lark-ai-acp--on-request (request)
  "Decline agent-initiated REQUEST — this backend wants text only.
Permission requests pick the reject option (or cancel); anything
else gets a method-not-found error so the agent falls back to
answering directly."
  (let ((method (map-elt request 'method))
        (id (map-elt request 'id)))
    (pcase method
      ("session/request_permission"
       (lark-ai--debug-log
        "ACP PERMISSION" "auto-declining: %s"
        (or (map-nested-elt request '(params toolCall title)) method))
       (let ((option-id (lark-ai-acp--reject-option-id
                         (map-nested-elt request '(params options)))))
         (acp-send-response
          :client lark-ai-acp--client
          :response (if option-id
                        (acp-make-session-request-permission-response
                         :request-id id :option-id option-id)
                      (acp-make-session-request-permission-response
                       :request-id id :cancelled t)))))
      (_
       (lark-ai--debug-log "ACP REQUEST" "declining unsupported: %s" method)
       (acp-send-response
        :client lark-ai-acp--client
        :response (list (cons :request-id id)
                        (cons :error (acp-make-error
                                      :code -32601
                                      :message (format "%s is not available"
                                                       method)))))))))

(defun lark-ai-acp--on-error (error)
  "Log agent process ERROR output (stderr noise included) to the debug log."
  (lark-ai--debug-log "ACP STDERR" "%S" error))

(defun lark-ai-acp--report-failure (error)
  "Surface a failed ACP request ERROR in the progress log."
  (let ((msg (or (map-elt error 'message) (format "%S" error))))
    (lark-ai--debug-log "ERROR (acp)" "%S" error)
    (lark-ai--progress-log "ACP error: %s" msg)
    (when (string-match-p "auth" (downcase msg))
      (lark-ai--progress-log
       "Hint: the agent may need a login — run its CLI once (e.g. `claude') and retry."))))

;;;; Prompt round-trip

(defun lark-ai-acp-call (system-prompt user-message callback &optional on-chunk)
  "Send SYSTEM-PROMPT and USER-MESSAGE to the ACP agent.
Call CALLBACK with the full response text when the turn ends.
When ON-CHUNK is non-nil, it also receives each streamed text
chunk as it arrives.  Opens a fresh ACP session per call (see
Commentary)."
  (let ((client (lark-ai-acp--ensure-client)))
    (lark-ai-acp--with-initialized
     client
     (lambda ()
       (acp-send-request
        :client client
        :request (acp-make-session-new-request :cwd lark-ai-acp-cwd)
        :on-success
        (lambda (response)
          (let ((session-id (map-elt response 'sessionId)))
            (push (cons session-id (list :accumulated ""
                                         :on-chunk on-chunk
                                         :callback callback))
                  lark-ai-acp--requests)
            (lark-ai-acp--prompt client session-id system-prompt user-message)))
        :on-failure #'lark-ai-acp--report-failure)))))

(defun lark-ai-acp--prompt (client session-id system-prompt user-message)
  "Send the session/prompt for SESSION-ID on CLIENT.
SYSTEM-PROMPT and USER-MESSAGE are combined by
`lark-ai-acp--prompt-text'; the response text arrives via
notifications and is finalized in `lark-ai-acp--finish'."
  (acp-send-request
   :client client
   :request (acp-make-session-prompt-request
             :session-id session-id
             :prompt `(((type . "text")
                        (text . ,(lark-ai-acp--prompt-text
                                  system-prompt user-message)))))
   :on-success (lambda (response)
                 (lark-ai-acp--finish session-id
                                      (map-elt response 'stopReason)))
   :on-failure (lambda (error)
                 (setq lark-ai-acp--requests
                       (assoc-delete-all session-id lark-ai-acp--requests))
                 (lark-ai-acp--report-failure error))))

(defun lark-ai-acp--finish (session-id stop-reason)
  "Complete the request for SESSION-ID that ended with STOP-REASON.
Invokes the stored callback with the accumulated text — except on
\"cancelled\", where the abort path already reset the session and
a late callback would resurrect a dead turn."
  (when-let ((req (assoc session-id lark-ai-acp--requests)))
    (setq lark-ai-acp--requests
          (assoc-delete-all session-id lark-ai-acp--requests))
    (let ((text (plist-get (cdr req) :accumulated)))
      (lark-ai--debug-log "RESPONSE (acp)" "stop=%s\n%s" stop-reason text)
      (unless (equal stop-reason "cancelled")
        (funcall (plist-get (cdr req) :callback) text)))))

;;;; Abort / shutdown

(defun lark-ai-acp-abort ()
  "Cancel every in-flight ACP prompt and drop its callback."
  (when (and lark-ai-acp--client lark-ai-acp--requests)
    (dolist (req lark-ai-acp--requests)
      (ignore-errors
        (acp-send-notification
         :client lark-ai-acp--client
         :notification (acp-make-session-cancel-notification
                        :session-id (car req)))))
    (setq lark-ai-acp--requests nil)))

(defun lark-ai-acp-shutdown ()
  "Kill the ACP agent process and reset the backend state.
The next `acp' backend call starts a fresh agent."
  (interactive)
  (when lark-ai-acp--client
    (ignore-errors (acp-shutdown :client lark-ai-acp--client)))
  (lark-ai-acp--reset-state))

(provide 'lark-ai-acp)
;;; lark-ai-acp.el ends here
