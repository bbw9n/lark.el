;;; lark-ai-skills.el --- Skill registry and loader for lark.el AI layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Commentary:

;; Manages the lark-cli SKILL.md corpus that powers the LLM's
;; knowledge.  Parses SKILL.md frontmatter, builds a keyword index,
;; selects relevant skills for a given user prompt, and assembles the
;; system prompt sent to the LLM backend.
;;
;; Skills are loaded from `lark-ai-skills-directory', which defaults
;; to ~/.agents/skills/ (where `npx skills add' installs them).

;;; Code:

(require 'lark-core)

;;;; Customization

(defgroup lark-ai nil
  "Lark AI integration settings."
  :group 'lark
  :prefix "lark-ai-")

(defcustom lark-ai-skills-directory
  (expand-file-name "~/.agents/skills/")
  "Directory containing lark-cli SKILL.md files.
Each skill lives in a subdirectory (e.g., lark-calendar/SKILL.md)."
  :type 'directory
  :group 'lark-ai)

;;;; Internal state

(defvar lark-ai-skills--index nil
  "Alist of (NAME . (:description DESC :dir DIR :keywords (WORD ...))).")

(defvar lark-ai-skills--cache (make-hash-table :test 'equal)
  "Hash table mapping skill name → cached SKILL.md content string.")

;;;; Frontmatter parsing

(defun lark-ai-skills--parse-frontmatter (text)
  "Parse YAML frontmatter from TEXT.
Returns an alist of (key . value) pairs from the --- delimited block."
  (when (string-match "\\`---\n\\(\\(?:.*\n\\)*?\\)---" text)
    (let ((yaml (match-string 1 text))
          result)
      (dolist (line (split-string yaml "\n" t))
        (when (string-match "^\\([a-z_]+\\):[[:space:]]*\"?\\([^\"]*?\\)\"?[[:space:]]*$" line)
          (push (cons (match-string 1 line) (match-string 2 line)) result)))
      (nreverse result))))

(defun lark-ai-skills--extract-keywords (description)
  "Extract keyword tokens from a skill DESCRIPTION string.
Returns a list of downcased words useful for matching."
  (when description
    (let ((words (split-string (downcase description) "[^a-z0-9\u4e00-\u9fff]+" t)))
      (seq-remove (lambda (w) (< (length w) 2)) words))))

;;;; Index building

(defun lark-ai-skills-refresh ()
  "Scan `lark-ai-skills-directory' and rebuild the skill index."
  (interactive)
  (setq lark-ai-skills--index nil)
  (clrhash lark-ai-skills--cache)
  (let ((dir (file-name-as-directory lark-ai-skills-directory)))
    (when (file-directory-p dir)
      (dolist (entry (directory-files dir t "^lark-"))
        (when (file-directory-p entry)
          (let ((skill-file (expand-file-name "SKILL.md" entry)))
            (when (file-exists-p skill-file)
              (let* ((text (with-temp-buffer
                             (insert-file-contents skill-file)
                             (buffer-string)))
                     (meta (lark-ai-skills--parse-frontmatter text))
                     (name (or (cdr (assoc "name" meta))
                               (file-name-nondirectory entry)))
                     (desc (or (cdr (assoc "description" meta)) "")))
                (push (cons name (list :description desc
                                       :dir entry
                                       :keywords (lark-ai-skills--extract-keywords desc)))
                      lark-ai-skills--index))))))))
  (lark--log "AI skills: indexed %d skills" (length lark-ai-skills--index))
  lark-ai-skills--index)

(defun lark-ai-skills--ensure-index ()
  "Ensure the skill index is built; build it on first use."
  (unless lark-ai-skills--index
    (lark-ai-skills-refresh)))

;;;; Skill loading

(defun lark-ai-skills-load (name)
  "Load and return the SKILL.md content for skill NAME as a string.
Results are cached in `lark-ai-skills--cache'."
  (lark-ai-skills--ensure-index)
  (or (gethash name lark-ai-skills--cache)
      (let ((entry (cdr (assoc name lark-ai-skills--index))))
        (when entry
          (let* ((dir (plist-get entry :dir))
                 (file (expand-file-name "SKILL.md" dir))
                 (text (when (file-exists-p file)
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))
            (when text
              (puthash name text lark-ai-skills--cache))
            text)))))

(defun lark-ai-skills-list ()
  "Return list of all available skill names."
  (lark-ai-skills--ensure-index)
  (mapcar #'car lark-ai-skills--index))

;;;; Skill selection

(defconst lark-ai-skills--routing-table
  '(("calendar\\|agenda\\|meeting\\|schedule\\|event\\|freebusy\\|rsvp\\|日[历程]"
     "lark-calendar")
    ("message\\|chat\\|send\\|im\\|群\\|消息\\|聊天"
     "lark-im")
    ("task\\|todo\\|待办\\|任务"
     "lark-task")
    ("standup\\|daily\\|开工\\|今天.*安排\\|what.*plate"
     "lark-task" "lark-calendar" "lark-workflow-standup-report")
    ("meeting.*\\(note\\|summar\\|minute\\|纪要\\|总结\\)"
     "lark-vc" "lark-workflow-meeting-summary" "lark-doc")
    ("doc\\|document\\|文档"
     "lark-doc" "lark-drive")
    ("mail\\|email\\|inbox\\|邮件\\|邮箱"
     "lark-mail")
    ("sheet\\|spreadsheet\\|表格"
     "lark-sheets")
    ("base\\|bitable\\|多维表"
     "lark-base")
    ("drive\\|file\\|upload\\|download\\|云空间"
     "lark-drive")
    ("wiki\\|知识库"
     "lark-wiki")
    ("contact\\|user\\|search.*people\\|通讯录\\|联系人"
     "lark-contact")
    ("whiteboard\\|画板"
     "lark-whiteboard")
    ("approval\\|审批"
     "lark-approval")
    ("video.*conference\\|vc\\|录制\\|recording"
     "lark-vc"))
  "Routing table: (REGEXP . SKILL-NAMES).
Used to select relevant skills based on user prompt keywords.")

(defun lark-ai-skills-select (prompt)
  "Select relevant skill names for the user PROMPT.
Always includes lark-shared.  Returns a deduplicated list of skill names."
  (lark-ai-skills--ensure-index)
  (let ((prompt-down (downcase prompt))
        (selected (list "lark-shared"))
        (available (mapcar #'car lark-ai-skills--index)))
    ;; Match against routing table
    (dolist (rule lark-ai-skills--routing-table)
      (when (string-match-p (car rule) prompt-down)
        (dolist (skill (cdr rule))
          (when (member skill available)
            (push skill selected)))))
    ;; If nothing matched (besides lark-shared), load all domain skills
    (when (= (length selected) 1)
      (dolist (name available)
        (unless (string-match-p "workflow\\|shared\\|skill-maker\\|openapi-explorer" name)
          (push name selected))))
    (delete-dups (nreverse selected))))

;;;; System prompt assembly

(defun lark-ai-skills--preamble ()
  "Build the preamble section of the system prompt.
Includes current date/time, identity, and instructions."
  (format "You are a Lark/Feishu assistant integrated into Emacs.
You help the user by planning and executing lark-cli commands.

Current date/time: %s
Timezone: %s
Default identity: %s

## Response Format

You MUST respond with a JSON object containing a plan — a list of steps to execute.
Each step is an object with these fields:
- \"command\": array of strings (lark-cli arguments), or null for synthesis steps
- \"description\": human-readable description of what this step does
- \"side_effect\": boolean, true if this step writes/modifies data
- \"parallel_group\": optional integer, steps with the same group run in parallel
- \"synthesize\": boolean, true if this step is an AI synthesis/summary step
- \"synthesis_instruction\": string, instruction for the synthesis (only when synthesize=true)

Example response:
{
  \"plan\": [
    {\"command\": [\"calendar\", \"+agenda\"], \"description\": \"Fetch today's agenda\", \"side_effect\": false, \"parallel_group\": 1},
    {\"command\": [\"task\", \"+get-my-tasks\"], \"description\": \"Fetch open tasks\", \"side_effect\": false, \"parallel_group\": 1},
    {\"command\": null, \"description\": \"Summarize schedule and tasks\", \"side_effect\": false, \"synthesize\": true, \"synthesis_instruction\": \"Combine the agenda and tasks into a standup summary. Detect time conflicts. List free slots.\"}
  ]
}

Rules:
- Only use lark-cli commands documented in the skills below.
- Mark any create/update/delete/send operation as side_effect: true.
- Use parallel_group to indicate steps that can run concurrently.
- For multi-step workflows, later steps can reference earlier results — the executor handles this.
- If the user's request is a simple question that needs no CLI calls, return a plan with a single synthesize step.
"
          (format-time-string "%Y-%m-%dT%H:%M:%S%z")
          (format-time-string "%Z")
          (or lark-default-identity "user")))

(defun lark-ai-skills-build-system-prompt (skill-names)
  "Build the full system prompt from SKILL-NAMES.
Concatenates the preamble with the content of each selected skill."
  (let ((parts (list (lark-ai-skills--preamble))))
    (dolist (name skill-names)
      (let ((content (lark-ai-skills-load name)))
        (when content
          (push (format "\n---\n## Skill: %s\n\n%s" name content) parts))))
    (mapconcat #'identity (nreverse parts) "\n")))

(provide 'lark-ai-skills)
;;; lark-ai-skills.el ends here
