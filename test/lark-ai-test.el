;;; lark-ai-test.el --- Tests for lark.el AI layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'lark-ai-skills)
(require 'lark-ai-context)
(require 'lark-ai)
(require 'lark-im)

;;;; Skill frontmatter parsing

(ert-deftest lark-ai-test-parse-frontmatter ()
  "Parse YAML frontmatter from SKILL.md content."
  (let ((text "---\nname: lark-calendar\nversion: 1.0.0\ndescription: \"Calendar management\"\n---\n# calendar\n"))
    (let ((meta (lark-ai-skills--parse-frontmatter text)))
      (should (equal (cdr (assoc "name" meta)) "lark-calendar"))
      (should (equal (cdr (assoc "version" meta)) "1.0.0"))
      (should (equal (cdr (assoc "description" meta)) "Calendar management")))))

(ert-deftest lark-ai-test-parse-frontmatter-no-block ()
  "Return nil when no frontmatter block present."
  (should (null (lark-ai-skills--parse-frontmatter "# Just a heading\nSome text."))))

;;;; Keyword extraction

(ert-deftest lark-ai-test-extract-keywords ()
  "Extract keywords from a description string."
  (let ((kw (lark-ai-skills--extract-keywords "Calendar management and scheduling")))
    (should (member "calendar" kw))
    (should (member "management" kw))
    (should (member "scheduling" kw))))

(ert-deftest lark-ai-test-extract-keywords-nil ()
  "Nil description returns nil."
  (should (null (lark-ai-skills--extract-keywords nil))))

;;;; Skill selection

(ert-deftest lark-ai-test-select-skills-calendar ()
  "Calendar-related prompt selects lark-calendar."
  ;; Install a minimal index for testing
  (let ((lark-ai-skills--index
         '(("lark-shared" . (:description "shared" :dir "/tmp" :keywords ("shared")))
           ("lark-calendar" . (:description "calendar" :dir "/tmp" :keywords ("calendar")))
           ("lark-im" . (:description "messaging" :dir "/tmp" :keywords ("messaging")))
           ("lark-task" . (:description "tasks" :dir "/tmp" :keywords ("tasks"))))))
    (let ((selected (lark-ai-skills-select "show my calendar agenda")))
      (should (member "lark-shared" selected))
      (should (member "lark-calendar" selected))
      (should-not (member "lark-im" selected)))))

(ert-deftest lark-ai-test-select-skills-standup ()
  "Standup prompt selects calendar, task, and workflow."
  (let ((lark-ai-skills--index
         '(("lark-shared" . (:description "shared" :dir "/tmp" :keywords ("shared")))
           ("lark-calendar" . (:description "calendar" :dir "/tmp" :keywords ("calendar")))
           ("lark-task" . (:description "tasks" :dir "/tmp" :keywords ("tasks")))
           ("lark-workflow-standup-report" . (:description "standup" :dir "/tmp" :keywords ("standup"))))))
    (let ((selected (lark-ai-skills-select "what's on my plate today standup")))
      (should (member "lark-shared" selected))
      (should (member "lark-task" selected))
      (should (member "lark-workflow-standup-report" selected)))))

(ert-deftest lark-ai-test-select-skills-fallback ()
  "Ambiguous prompt loads shared only."
  (let ((lark-ai-skills--index
         '(("lark-shared" . (:description "shared" :dir "/tmp" :keywords ("shared")))
           ("lark-calendar" . (:description "calendar" :dir "/tmp" :keywords ("calendar")))
           ("lark-im" . (:description "messaging" :dir "/tmp" :keywords ("messaging"))))))
    (let ((selected (lark-ai-skills-select "help me with something")))
      (should (member "lark-shared" selected)))))

;;;; Plan parsing

(ert-deftest lark-ai-test-parse-plan-basic ()
  "Parse a basic plan from JSON."
  (let ((json "{\"plan\": [{\"command\": [\"calendar\", \"+agenda\"], \"description\": \"Fetch agenda\", \"side_effect\": false}, {\"command\": null, \"description\": \"Summarize\", \"synthesize\": true, \"synthesis_instruction\": \"Make a summary\"}]}"))
    (let ((plan (lark-ai--parse-plan json)))
      (should (= (length plan) 2))
      ;; First step
      (should (equal (plist-get (car plan) :command) '("calendar" "+agenda")))
      (should (equal (plist-get (car plan) :description) "Fetch agenda"))
      (should-not (plist-get (car plan) :side-effect))
      ;; Second step
      (should (plist-get (cadr plan) :synthesize))
      (should (null (plist-get (cadr plan) :command))))))

(ert-deftest lark-ai-test-parse-plan-with-fences ()
  "Parse a plan wrapped in markdown code fences."
  (let ((json "```json\n{\"plan\": [{\"command\": [\"task\", \"+get-my-tasks\"], \"description\": \"Get tasks\", \"side_effect\": false}]}\n```"))
    (let ((plan (lark-ai--parse-plan json)))
      (should (= (length plan) 1))
      (should (equal (plist-get (car plan) :command) '("task" "+get-my-tasks"))))))

(ert-deftest lark-ai-test-parse-plan-parallel-groups ()
  "Parse plan with parallel groups."
  (let ((json "{\"plan\": [{\"command\": [\"calendar\", \"+agenda\"], \"description\": \"A\", \"side_effect\": false, \"parallel_group\": 1}, {\"command\": [\"task\", \"+get-my-tasks\"], \"description\": \"B\", \"side_effect\": false, \"parallel_group\": 1}]}"))
    (let ((plan (lark-ai--parse-plan json)))
      (should (= (length plan) 2))
      (should (= (plist-get (car plan) :parallel-group) 1))
      (should (= (plist-get (cadr plan) :parallel-group) 1)))))

(ert-deftest lark-ai-test-parse-plan-invalid ()
  "Invalid JSON returns nil."
  (should (null (lark-ai--parse-plan "not json at all"))))

(ert-deftest lark-ai-test-parse-plan-side-effect ()
  "Side effect flag is correctly parsed."
  (let ((json "{\"plan\": [{\"command\": [\"calendar\", \"+create\", \"--summary\", \"Meeting\"], \"description\": \"Create event\", \"side_effect\": true}]}"))
    (let ((plan (lark-ai--parse-plan json)))
      (should (= (length plan) 1))
      (should (plist-get (car plan) :side-effect)))))

;;;; $step-N interpolation

(ert-deftest lark-ai-test-interpolate-no-refs ()
  "Args without $step-N are passed through unchanged."
  (let ((result (lark-ai--interpolate-cmd '("calendar" "+agenda") nil)))
    (should (equal result '("calendar" "+agenda")))))

(ert-deftest lark-ai-test-interpolate-full-result ()
  "$step-0 is replaced with the full JSON result."
  (let* ((results '((0 . ((items . [])))))
         (result (lark-ai--interpolate-cmd '("--data" "$step-0") results)))
    (should (stringp (cadr result)))
    (should (string-match-p "items" (cadr result)))))

(ert-deftest lark-ai-test-interpolate-field ()
  "$step-0.name resolves a top-level field."
  (let* ((results '((0 . ((name . "Alice") (id . "u1")))))
         (result (lark-ai--interpolate-cmd '("--name" "$step-0.name") results)))
    (should (equal (cadr result) "Alice"))))

(ert-deftest lark-ai-test-interpolate-nested-field ()
  "$step-0.data.user.id resolves nested fields."
  (let* ((results '((0 . ((data . ((user . ((id . "u42")))))))))
         (result (lark-ai--interpolate-cmd '("--id" "$step-0.data.user.id") results)))
    (should (equal (cadr result) "u42"))))

(ert-deftest lark-ai-test-interpolate-array-index ()
  "$step-0.items[0].id resolves array index."
  (let* ((results '((0 . ((items . (((id . "a1")) ((id . "a2"))))))))
         (result (lark-ai--interpolate-cmd '("--id" "$step-0.items[0].id") results)))
    (should (equal (cadr result) "a1"))))

(ert-deftest lark-ai-test-interpolate-wildcard ()
  "$step-0.items[*].id collects from all elements."
  (let* ((results '((0 . ((items . (((id . "a1")) ((id . "a2")) ((id . "a3"))))))))
         (result (lark-ai--interpolate-cmd '("--ids" "$step-0.items[*].id") results)))
    (should (equal (cadr result) "a1,a2,a3"))))

(ert-deftest lark-ai-test-interpolate-missing-step ()
  "$step-9 with no result keeps the placeholder."
  (let* ((results '((0 . ((x . 1)))))
         (result (lark-ai--interpolate-cmd '("--x" "$step-9.y") results)))
    (should (equal (cadr result) "$step-9.y"))))

(ert-deftest lark-ai-test-interpolate-multiple-refs ()
  "Multiple $step refs in different args."
  (let* ((results '((0 . ((id . "u1"))) (1 . ((id . "u2")))))
         (result (lark-ai--interpolate-cmd
                  '("--a" "$step-0.id" "--b" "$step-1.id") results)))
    (should (equal (nth 1 result) "u1"))
    (should (equal (nth 3 result) "u2"))))

(ert-deftest lark-ai-test-interpolate-numeric-value ()
  "Numeric values are converted to strings."
  (let* ((results '((0 . ((count . 42)))))
         (result (lark-ai--interpolate-cmd '("--n" "$step-0.count") results)))
    (should (equal (cadr result) "42"))))

;;;; JSON extraction

(ert-deftest lark-ai-test-extract-json-plain ()
  "Extract JSON from plain text."
  (let ((result (lark-ai--extract-json "{\"plan\": []}")))
    (should (listp result))
    (should (equal (alist-get 'plan result) nil))))

(ert-deftest lark-ai-test-extract-json-fenced ()
  "Extract JSON from markdown-fenced response."
  (let ((result (lark-ai--extract-json "Here is the plan:\n```json\n{\"plan\": [{\"command\": [\"test\"], \"description\": \"t\"}]}\n```\n")))
    (should (= (length (alist-get 'plan result)) 1))))

(ert-deftest lark-ai-test-extract-json-fenced-no-newline ()
  "Extract JSON when no newline between lang tag and content."
  (let ((result (lark-ai--extract-json "```json{\"plan\": [{\"command\": null, \"description\": \"test\", \"synthesize\": true}]}```")))
    (should (= (length (alist-get 'plan result)) 1))))

;;;; Context extraction

(ert-deftest lark-ai-test-context-non-lark-buffer ()
  "Non-lark buffer returns nil domain."
  (with-temp-buffer
    (let ((ctx (lark-ai-context)))
      (should (null (plist-get ctx :domain)))
      (should (equal (plist-get ctx :buffer-type) "other")))))

(ert-deftest lark-ai-test-context-doc-detail-org ()
  "Doc detail buffer in org-mode is recognized by buffer name."
  (let ((buf (get-buffer-create "*Lark Doc: Test Doc*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local lark-docs--doc-token "tok_abc")
          (let ((ctx (lark-ai-context)))
            (should (equal (plist-get ctx :domain) "docs"))
            (should (equal (plist-get ctx :buffer-type) "doc-detail"))
            (should (equal (plist-get (plist-get ctx :item) :doc-token) "tok_abc"))))
      (kill-buffer buf))))

(ert-deftest lark-ai-test-context-doc-detail-markdown ()
  "Doc detail buffer in special-mode is recognized by buffer name."
  (let ((buf (get-buffer-create "*Lark Doc: Another*")))
    (unwind-protect
        (with-current-buffer buf
          (special-mode)
          (setq-local lark-docs--doc-token "tok_xyz")
          (let ((ctx (lark-ai-context)))
            (should (equal (plist-get ctx :domain) "docs"))
            (should (equal (plist-get ctx :buffer-type) "doc-detail"))))
      (kill-buffer buf))))

;;;; Smart reply — thread context extraction

(ert-deftest lark-ai-test-collect-thread-context-not-chat ()
  "Errors when not in a chat buffer."
  (with-temp-buffer
    (should-error (lark-ai--collect-thread-context) :type 'user-error)))

(ert-deftest lark-ai-test-collect-thread-context-no-message ()
  "Errors when no message at point."
  (with-temp-buffer
    (lark-im-chat-mode)
    (setq-local lark-im--chat-id "oc_test123")
    (setq-local lark-im--chat-name "Test Chat")
    (setq-local lark-im--messages nil)
    (let ((inhibit-read-only t))
      (insert "no message here"))
    (should-error (lark-ai--collect-thread-context) :type 'user-error)))

(ert-deftest lark-ai-test-collect-thread-context-success ()
  "Extracts thread context from a chat buffer."
  (with-temp-buffer
    (lark-im-chat-mode)
    (setq-local lark-im--chat-id "oc_test123")
    (setq-local lark-im--chat-name "Test Chat")
    (setq-local lark-im--messages
                '(((message_id . "msg_1") (sender_name . "Alice") (text . "Hello"))
                  ((message_id . "msg_2") (sender_name . "Bob") (text . "Hi there"))))
    ;; Insert text with message-id property
    (let ((inhibit-read-only t)
          (beg (point)))
      (insert "Bob: Hi there")
      (put-text-property beg (point) 'lark-message-id "msg_2"))
    (goto-char (point-min))
    (let ((ctx (lark-ai--collect-thread-context)))
      (should (equal (plist-get ctx :chat-id) "oc_test123"))
      (should (equal (plist-get ctx :chat-name) "Test Chat"))
      (should (equal (plist-get ctx :message-id) "msg_2"))
      ;; Thread text should contain both messages
      (should (string-match-p "Alice" (plist-get ctx :thread-text)))
      (should (string-match-p "Bob" (plist-get ctx :thread-text)))
      ;; Target message should be marked with >>>
      (should (string-match-p ">>>" (plist-get ctx :thread-text))))))

;;;; Smart reply — compose buffer

(ert-deftest lark-ai-test-compose-buffer-setup ()
  "Compose buffer is set up with draft, message-id, and chat-id."
  (lark-ai--open-compose-buffer "Draft reply" "msg_42" "oc_123" "Dev Chat")
  (unwind-protect
      (let ((buf (get-buffer "*Lark AI Reply*")))
        (should buf)
        (with-current-buffer buf
          (should (derived-mode-p 'lark-ai-reply-mode))
          (should (equal (string-trim (buffer-string)) "Draft reply"))
          (should (equal lark-ai-reply--message-id "msg_42"))
          (should (equal lark-ai-reply--chat-id "oc_123"))
          (should (equal lark-ai-reply--chat-name "Dev Chat"))))
    (when-let ((buf (get-buffer "*Lark AI Reply*")))
      (kill-buffer buf))))

(ert-deftest lark-ai-test-reply-cancel ()
  "Cancel closes the compose buffer."
  (lark-ai--open-compose-buffer "Draft" "msg_1" "oc_1" "Chat")
  (unwind-protect
      (with-current-buffer "*Lark AI Reply*"
        (lark-ai-reply-cancel)
        (should (null (get-buffer "*Lark AI Reply*"))))
    (when-let ((buf (get-buffer "*Lark AI Reply*")))
      (kill-buffer buf))))

(ert-deftest lark-ai-test-reply-send-empty-errors ()
  "Sending an empty reply signals an error."
  (lark-ai--open-compose-buffer "" "msg_1" "oc_1" "Chat")
  (unwind-protect
      (with-current-buffer "*Lark AI Reply*"
        (should-error (lark-ai-reply-send) :type 'user-error))
    (when-let ((buf (get-buffer "*Lark AI Reply*")))
      (kill-buffer buf))))

;;;; System prompt assembly

(ert-deftest lark-ai-test-preamble-contains-date ()
  "Preamble includes current date."
  (let ((preamble (lark-ai-skills--preamble)))
    (should (string-match-p (format-time-string "%Y-%m-%d") preamble))
    (should (string-match-p "Response Format" preamble))))

(ert-deftest lark-ai-test-planning-prompt-mandates-json ()
  "Planning prompt keeps the JSON-plan mandate."
  (let* ((lark-ai-skills--index nil)
         (lark-ai-skills--cache (make-hash-table :test 'equal))
         (prompt (lark-ai-skills-build-system-prompt nil)))
    (should (string-match-p "MUST respond with a JSON" prompt))
    (should (string-match-p "synthesis_instruction" prompt))))

(ert-deftest lark-ai-test-synthesis-prompt-no-json-mandate ()
  "Synthesis prompt strips the JSON-plan mandate so the model
emits prose instead of another plan."
  (let* ((lark-ai-skills--index nil)
         (lark-ai-skills--cache (make-hash-table :test 'equal))
         (prompt (lark-ai-skills-build-synthesis-prompt nil)))
    ;; No JSON-plan rules
    (should-not (string-match-p "MUST respond with a JSON" prompt))
    (should-not (string-match-p "parallel_group" prompt))
    (should-not (string-match-p "synthesis_instruction" prompt))
    ;; But still identity + an explicit prose directive
    (should (string-match-p "Lark/Feishu assistant" prompt))
    (should (string-match-p "markdown prose" prompt))))

(ert-deftest lark-ai-test-synthesis-prompt-includes-skills ()
  "Synthesis prompt still appends selected skill bodies."
  (let* ((lark-ai-skills--index
          '(("lark-shared" . (:description "shared" :dir "/tmp"
                              :keywords ("shared")))))
         (lark-ai-skills--cache (make-hash-table :test 'equal)))
    (puthash "lark-shared" "SKILL_BODY_MARKER" lark-ai-skills--cache)
    (let ((prompt (lark-ai-skills-build-synthesis-prompt '("lark-shared"))))
      (should (string-match-p "## Skill: lark-shared" prompt))
      (should (string-match-p "SKILL_BODY_MARKER" prompt)))))

;;;; Skill selection — context-aware + no-fallback

(ert-deftest lark-ai-test-select-skills-context-match ()
  "Match via the optional CONTEXT arg when the prompt itself is generic."
  (let ((lark-ai-skills--index
         '(("lark-shared" . (:description "shared" :dir "/tmp" :keywords ("shared")))
           ("lark-calendar" . (:description "calendar" :dir "/tmp" :keywords ("calendar")))
           ("lark-task" . (:description "tasks" :dir "/tmp" :keywords ("tasks"))))))
    (let ((selected (lark-ai-skills-select "tell me more"
                                           "calendar +agenda Fetch agenda")))
      (should (member "lark-shared" selected))
      (should (member "lark-calendar" selected))
      (should-not (member "lark-task" selected)))))

(ert-deftest lark-ai-test-select-skills-no-fallback ()
  "No regex match yields only lark-shared (no all-skills dump)."
  (let ((lark-ai-skills--index
         '(("lark-shared" . (:description "shared" :dir "/tmp" :keywords ("shared")))
           ("lark-calendar" . (:description "calendar" :dir "/tmp" :keywords ("calendar")))
           ("lark-im" . (:description "messaging" :dir "/tmp" :keywords ("messaging")))
           ("lark-task" . (:description "tasks" :dir "/tmp" :keywords ("tasks"))))))
    (let ((selected (lark-ai-skills-select "tell me more about that")))
      (should (equal selected '("lark-shared")))
      (should-not (member "lark-calendar" selected))
      (should-not (member "lark-task" selected)))))

;;;; build-user-message — structure, history truncation

(ert-deftest lark-ai-test-build-user-message-no-history ()
  "First-turn message: just `## Current request' + prompt."
  (let ((msg (lark-ai--build-user-message "do the thing" "" nil)))
    (should (string-match-p "## Current request" msg))
    (should (string-match-p "do the thing" msg))
    (should-not (string-match-p "## Prior turns" msg))
    (should-not (string-match-p "## Originating buffer context" msg))
    ;; No history means no anti-repeat trailer.
    (should-not (string-match-p "Address only the request" msg))))

(ert-deftest lark-ai-test-build-user-message-with-context ()
  "Originating-buffer context appears in its own labelled section."
  (let ((msg (lark-ai--build-user-message "x" "Viewing doc tok_42" nil)))
    (should (string-match-p "## Originating buffer context" msg))
    (should (string-match-p "Viewing doc tok_42" msg))))

(ert-deftest lark-ai-test-build-user-message-with-history ()
  "Prior turns rendered as a list with `## Prior turns' header."
  (let* ((history '(("assistant" . "Here's the agenda")
                    ("user" . "show agenda")))
         (msg (lark-ai--build-user-message "tell me more" "" history)))
    (should (string-match-p "## Prior turns" msg))
    (should (string-match-p "User: show agenda" msg))
    (should (string-match-p "Assistant: Here's the agenda" msg))
    ;; Trailing anti-repeat instruction only when history is present.
    (should (string-match-p "Address only the request" msg))))

(ert-deftest lark-ai-test-build-user-message-truncates-assistant ()
  "Long assistant text is clipped to `lark-ai-history-truncate-chars'."
  (let* ((lark-ai-history-truncate-chars 50)
         (long-text (make-string 200 ?a))
         (history `(("assistant" . ,long-text)
                    ("user" . "q")))
         (msg (lark-ai--build-user-message "next" "" history)))
    (should (string-match-p "…\\[truncated\\]" msg))
    ;; Original 200-char string should not appear verbatim.
    (should-not (string-match-p (regexp-quote long-text) msg))))

(ert-deftest lark-ai-test-build-user-message-keeps-user-prompts ()
  "User prompts are kept un-truncated even if very long."
  (let* ((lark-ai-history-truncate-chars 20)
         (long-user (make-string 100 ?u))
         (history `(("assistant" . "ok")
                    ("user" . ,long-user)))
         (msg (lark-ai--build-user-message "x" "" history)))
    (should (string-match-p (regexp-quote long-user) msg))))

;;;; Session struct

(ert-deftest lark-ai-test-session-defaults ()
  "Fresh session has sensible defaults."
  (let ((s (make-lark-ai-session)))
    (should (= (lark-ai-session-turn s) 0))
    (should (eq (lark-ai-session-phase s) 'idle))
    (should (null (lark-ai-session-history s)))
    (should (equal (lark-ai-session-context s) ""))
    (should (null (lark-ai-session-skills s)))
    (should (null (lark-ai-session-steps s)))
    (should (null (lark-ai-session-input-start s)))
    (should (null (lark-ai-session-input-region-start s)))))

(ert-deftest lark-ai-test-session-setf ()
  "Session slots are setf-able."
  (let ((s (make-lark-ai-session)))
    (setf (lark-ai-session-turn s) 3
          (lark-ai-session-phase s) 'review
          (lark-ai-session-skills s) '("lark-calendar"))
    (should (= (lark-ai-session-turn s) 3))
    (should (eq (lark-ai-session-phase s) 'review))
    (should (equal (lark-ai-session-skills s) '("lark-calendar")))))

;;;; Plan body — step-index text property for at-point removal

(ert-deftest lark-ai-test-format-plan-body-step-index ()
  "Each rendered step line carries its `lark-ai-step-index' index."
  (let ((buf (get-buffer-create "*lark-ai-test-plan*")))
    (unwind-protect
        (with-current-buffer buf
          (lark-ai-plan-mode)
          (let ((session (lark-ai--session)))
            (setf (lark-ai-session-steps session)
                  '((:index 0 :description "first"  :command ("a" "b"))
                    (:index 1 :description "second" :command nil :synthesize t))
                  (lark-ai-session-step-status session)
                  '((0 . pending) (1 . pending))
                  (lark-ai-session-phase session) 'review))
          (let ((body (lark-ai--format-plan-body)))
            ;; Body should mention both descriptions.
            (should (string-match-p "first" body))
            (should (string-match-p "second" body))
            ;; Step-index property should be set on each step's chars.
            (let ((found-0 nil) (found-1 nil))
              (dotimes (i (length body))
                (pcase (get-text-property i 'lark-ai-step-index body)
                  (0 (setq found-0 t))
                  (1 (setq found-1 t))))
              (should found-0)
              (should found-1))))
      (kill-buffer buf))))

(provide 'lark-ai-test)
;;; lark-ai-test.el ends here
