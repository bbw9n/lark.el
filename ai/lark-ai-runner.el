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

;;;; Customization

(defcustom lark-ai-confirm-side-effects t
  "When non-nil, pause for confirmation before executing write operations."
  :type 'boolean
  :group 'lark-ai)

;;;; Result accumulation

(defun lark-ai--push-result (idx result)
  "Push (IDX . RESULT) onto the session's step-results."
  (push (cons idx result)
        (lark-ai-session-step-results (lark-ai--session))))

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

(provide 'lark-ai-runner)
;;; lark-ai-runner.el ends here
