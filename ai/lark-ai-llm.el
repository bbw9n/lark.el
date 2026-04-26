;;; lark-ai-llm.el --- LLM backend dispatch for the lark.el AI layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; Backend dispatch for LLM calls.  Two backends are supported:
;;
;;   `gptel' — uses the gptel package (multi-provider, streaming).
;;   `http'  — raw `url-retrieve' against an OpenAI-compatible endpoint.
;;
;; Public entry points (used by lark-ai-runner / lark-ai):
;;
;;   `lark-ai--call-llm'        — non-streaming request.
;;   `lark-ai--call-llm-stream' — streaming request with optional
;;                                chunk handler.
;;
;; Tests rebind these functions via `cl-letf' to inject fake responses
;; without involving the UI or any real network call.
;;
;; Streaming destination: when invoked without a CHUNK-HANDLER, the
;; default streams chunks into the AI buffer's output fragment (used
;; by the synthesis pass).  When invoked with a CHUNK-HANDLER, the
;; handler receives each chunk and the output fragment is left
;; untouched (used by the planning pass to stream raw JSON into the
;; log fragment).  Both paths invoke CALLBACK with the full
;; accumulated text on stream end.

;;; Code:

(require 'lark-ai-protocol)
(require 'lark-core)
(require 'json)

;; Forward declarations — the LLM module emits chunks into the AI
;; buffer's fragments via these helpers, which live in `lark-ai.el'
;; and `lark-ai-ui.el'.  Declared here so byte-compile is clean even
;; when this file loads first; resolved at call time.
(declare-function lark-ai--ensure-output-fragment "lark-ai")
(declare-function lark-ai--frag "lark-ai" (suffix))
(declare-function lark-ai--get-buffer "lark-ai")
(declare-function lark-ai--progress-log "lark-ai" (fmt &rest args))
(declare-function lark-ai-ui-append-fragment "lark-ai-ui" (id text))
(defvar lark-ai--buf-name)

;; gptel forward declarations
(declare-function gptel-request "gptel")
(declare-function gptel--parse-response "gptel")
(defvar gptel-model)
(defvar gptel-backend)
(defvar gptel-log-level)

;; url forward declarations
(defvar url-http-end-of-headers)
(defvar url-request-method)
(defvar url-request-extra-headers)
(defvar url-request-data)

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

;;;; Non-streaming

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

;;;; Streaming

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

(provide 'lark-ai-llm)
;;; lark-ai-llm.el ends here
