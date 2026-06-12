;;; lark-ai-runner.el --- Plan executor for the lark.el AI layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; Runs an AI plan against `lark--run-command'.  Handles parallel
;; groups, $step-N interpolation (delegated to lark-ai-protocol),
;; side-effect confirmation, and result accumulation into the
;; session.
;;
;; The runner reads/writes the per-buffer `lark-ai-session' via the
;; `lark-ai--session' accessor (provided by `lark-ai.el', forward
;; declared here).  UI updates are not the runner's concern — it
;; calls thin notifier helpers (`lark-ai--update-step-status',
;; `lark-ai--progress-log') which the wiring module supplies.
;;
;; Public entry point:
;;   `lark-ai--execute-plan' STEPS CALLBACK
;; — execute each step, accumulate (idx . result) pairs into
;; session.step-results, then call CALLBACK with the final results
;; alist.

;;; Code:

(require 'lark-ai-protocol)
(require 'lark-core)

;; Forward declarations — these live in `lark-ai.el' (UI wiring).
(declare-function lark-ai--session "lark-ai")
(declare-function lark-ai--update-step-status "lark-ai" (index status))
(declare-function lark-ai--progress-log "lark-ai" (fmt &rest args))
(declare-function lark-ai--synthesize-inline "lark-ai" (step done-fn))

;;;; Customization

(defcustom lark-ai-execute-mode 'confirm-writes
  "How a generated plan is executed — a single mutually-exclusive choice.
- `auto'           run the whole plan immediately, no confirmation
                   (create/update/delete/send steps run on their own).
- `confirm-writes' run immediately but prompt before each write
                   (side-effect) step, except commands listed in
                   `lark-ai-auto-execute-commands'; read-only steps run
                   freely.
- `review'         show the plan and wait for the user to confirm
                   (press RET) before anything runs."
  :type '(choice (const :tag "Auto — run everything, no prompts" auto)
                 (const :tag "Confirm writes — prompt before each write" confirm-writes)
                 (const :tag "Review — confirm the whole plan first" review))
  :group 'lark-ai)

(defcustom lark-ai-auto-execute-commands '("docs +create" "docs +update")
  "Write commands that run WITHOUT confirmation even in `confirm-writes' mode.
Each entry is a space-separated command prefix matched against the
leading tokens of a step's lark-cli command — e.g. \"docs +create\"
matches the step `(\"docs\" \"+create\" \"--api-version\" \"v2\" …)'.
Consulted by the plan runner when `lark-ai-execute-mode' is
`confirm-writes', and by the agent loop when
`lark-ai-agent-confirm-writes' is non-nil."
  :type '(repeat string)
  :group 'lark-ai)

(defcustom lark-ai-step-timeout 1800
  "Seconds to wait for a single plan step's lark-cli call before giving up.
A step that exceeds this is killed and marked failed so the plan can
continue instead of hanging.  Set to nil to disable the timeout."
  :type '(choice (const :tag "No timeout" nil)
                 (integer :tag "Seconds"))
  :group 'lark-ai)

;;;; Result accumulation

(defun lark-ai--push-result (idx result)
  "Push (IDX . RESULT) onto the session's step-results."
  (push (cons idx result)
        (lark-ai-session-step-results (lark-ai--session))))

;;;; Confirmation policy

(defun lark-ai--command-auto-p (cmd)
  "Return non-nil when CMD's leading tokens match an auto-execute entry.
CMD is a list of lark-cli argument strings.  Matched against
`lark-ai-auto-execute-commands' so whitelisted writes skip the prompt."
  (seq-some
   (lambda (entry)
     (let ((prefix (split-string entry " " t)))
       (and prefix (equal prefix (seq-take cmd (length prefix))))))
   lark-ai-auto-execute-commands))

;;;; Execution

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
             ;; Synthesis step.  If a later step interpolates this
             ;; step's output via $step-N, produce the text NOW so the
             ;; reference resolves to real content — otherwise $step-N
             ;; would resolve to the `:synthesize' sentinel (the
             ;; empty-document bug).  An unreferenced synthesis step is
             ;; still deferred to the terminal answer pass.
             (synthesize
              (if (lark-ai--step-referenced-p
                   idx (lark-ai-session-steps (lark-ai--session)))
                  (progn
                    (lark-ai--update-step-status idx 'running)
                    (lark-ai--synthesize-inline
                     step
                     (lambda (text)
                       (lark-ai--push-result idx (or text ""))
                       (lark-ai--update-step-status idx 'done)
                       (lark-ai--progress-log
                        "Step %d: synthesized %d chars"
                        idx (length (or text "")))
                       (setq pending (1- pending))
                       (when (zerop pending)
                         (lark-ai--execute-next rest callback)))))
                (lark-ai--push-result idx :synthesize)
                (setq pending (1- pending))
                (when (zerop pending)
                  (lark-ai--execute-next rest callback))))
             ;; Side-effect step: in `confirm-writes' mode, prompt unless the
             ;; command is whitelisted in `lark-ai-auto-execute-commands'.
             ((and side-effect (eq lark-ai-execute-mode 'confirm-writes)
                   (not (lark-ai--command-auto-p cmd)))
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
                    (lark-ai-session-step-results session)))
         ;; The actual argument vector lark-cli receives — mirrors the
         ;; `lark--run-command' call below (no :format, no extra-args),
         ;; so framework-injected flags like --dry-run are surfaced too.
         (fired (lark--build-command resolved nil nil)))
    (lark-ai--update-step-status index 'running)
    (lark-ai--progress-log "Step %d: lark-cli %s" index (string-join resolved " "))
    (lark-ai--debug-log
     (format "CLI REQUEST (step %d)" index)
     "%s"
     (propertize (concat "lark-cli " (string-join fired " "))
                 'face 'font-lock-function-name-face))
    (lark--run-command
     resolved
     (lambda (result)
       (lark-ai--push-result index result)
       (lark-ai--update-step-status index 'done)
       (lark-ai--progress-log "Step %d: done" index)
       (lark-ai--debug-log
        (format "CLI RESPONSE (step %d)" index)
        "%s" (lark-ai--format-cli-result result))
       (funcall done-fn))
     nil
     :no-error t
     :timeout lark-ai-step-timeout
     ;; Without this, a non-zero exit (or a timeout kill) means CALLBACK
     ;; never fires, DONE-FN never runs, and the whole plan stalls
     ;; silently.  Record the failure and advance so the user sees it.
     :on-error
     (lambda (exit-code msg)
       (lark-ai--push-result
        index (list (cons 'error (if (and msg (not (string-empty-p msg)))
                                     msg "command failed"))
                    (cons 'exit_code exit-code)))
       (lark-ai--update-step-status index 'error)
       (lark-ai--progress-log "Step %d: failed (exit %s)" index exit-code)
       (lark-ai--debug-log
        (format "CLI ERROR (step %d)" index)
        "exit %s\n%s" exit-code (or msg ""))
       (funcall done-fn)))))

(provide 'lark-ai-runner)
;;; lark-ai-runner.el ends here
