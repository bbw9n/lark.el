;;; lark-im-test.el --- Tests for lark-im.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name (buffer-file-name))))))
  (dolist (sub '("." "core" "ui" "domain" "ai"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'lark-im)

;;;; Chat parsing

(ert-deftest lark-im-test-chat-id ()
  (should (equal (lark-im--chat-id-of '((chat_id . "oc_123"))) "oc_123"))
  (should (equal (lark-im--chat-id-of '((id . "456"))) "456")))

(ert-deftest lark-im-test-chat-name ()
  (should (equal (lark-im--chat-name-of '((name . "Dev Team"))) "Dev Team"))
  (should (equal (lark-im--chat-name-of '((title . "General"))) "General"))
  (should (equal (lark-im--chat-name-of '((foo . "bar"))) "(unnamed)")))

(ert-deftest lark-im-test-chat-type ()
  (should (equal (lark-im--chat-type '((chat_type . "group"))) "group"))
  (should (equal (lark-im--chat-type '((foo . "bar"))) "")))

(ert-deftest lark-im-test-chat-member-count ()
  (should (equal (lark-im--chat-member-count '((member_count . 5))) "5"))
  (should (equal (lark-im--chat-member-count '((foo . "bar"))) "")))

;;;; Extract chats

(ert-deftest lark-im-test-extract-chats-items ()
  (let ((data '((items . (((chat_id . "1")) ((chat_id . "2")))))))
    (should (= (length (lark-im--extract-chats data)) 2))))

(ert-deftest lark-im-test-extract-chats-nested ()
  (let ((data '((data . ((chats . (((chat_id . "1")))))))))
    (should (= (length (lark-im--extract-chats data)) 1))))

(ert-deftest lark-im-test-extract-chats-empty ()
  (should (null (lark-im--extract-chats nil))))

;;;; Message parsing

(ert-deftest lark-im-test-msg-id ()
  (should (equal (lark-im--msg-id '((message_id . "m_123"))) "m_123"))
  (should (equal (lark-im--msg-id '((id . "456"))) "456")))

(ert-deftest lark-im-test-msg-sender ()
  (should (equal (lark-im--msg-sender '((sender_name . "Alice"))) "Alice"))
  (should (equal (lark-im--msg-sender '((sender . ((name . "Bob"))))) "Bob"))
  (should (equal (lark-im--msg-sender '((foo . "bar"))) "unknown")))

(ert-deftest lark-im-test-msg-content ()
  (should (equal (lark-im--msg-content '((text . "hello"))) "hello"))
  (should (equal (lark-im--msg-content '((content . "world"))) "world"))
  (should (equal (lark-im--msg-content '((body . "text body"))) "text body"))
  (should (equal (lark-im--msg-content '((body . ((text . "nested"))))) "nested"))
  (should (equal (lark-im--msg-content '((foo . "bar"))) "")))

(ert-deftest lark-im-test-msg-type ()
  (should (equal (lark-im--msg-type '((msg_type . "image"))) "image"))
  (should (equal (lark-im--msg-type '((foo . "bar"))) "text")))

(ert-deftest lark-im-test-msg-time-numeric ()
  (let ((result (lark-im--msg-time '((create_time . 1700000000)))))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-" result))))

(ert-deftest lark-im-test-msg-time-string ()
  (should (equal (lark-im--msg-time '((create_time . "2026-04-11")))
                 "2026-04-11")))

(ert-deftest lark-im-test-msg-time-nil ()
  (should (equal (lark-im--msg-time '((foo . "bar"))) "")))

;;;; Extract messages

(ert-deftest lark-im-test-extract-messages-items ()
  (let ((data '((items . (((message_id . "1")) ((message_id . "2")))))))
    (should (= (length (lark-im--extract-messages data)) 2))))

(ert-deftest lark-im-test-extract-messages-nested ()
  (let ((data '((data . ((messages . (((message_id . "1")))))))))
    (should (= (length (lark-im--extract-messages data)) 1))))

(ert-deftest lark-im-test-extract-messages-empty ()
  (should (null (lark-im--extract-messages nil))))

;;;; Make entries

(ert-deftest lark-im-test-insert-chat ()
  (let* ((lark-contact--user-cache (make-hash-table :test 'equal))
         (chat '((chat_id . "c1")
                 (name . "Dev Team")
                 (chat_type . "group")
                 (description . "Engineering chat")
                 (create_time . "2026-01-04T09:08:05Z")
                 (member_count . 10))))
    (with-temp-buffer
      (lark-im--insert-chat chat)
      (goto-char (point-min))
      ;; Title line present
      (should (search-forward "Dev Team" nil t))
      ;; Fields present
      (should (search-forward "group" nil t))
      (should (search-forward "Engineering chat" nil t))
      (should (search-forward "2026-01-04 09:08" nil t))
      (should (search-forward "10" nil t))
      ;; Text property covers the section
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'lark-chat-id) "c1"))
      (should (equal (get-text-property (point) 'lark-chat-name) "Dev Team")))))

;;;; Schema fixes: textual types and deleted filter

(ert-deftest lark-im-test-msg-textual-p ()
  "Text, post, and markdown are textual; image/file/sticker are not."
  (should (lark-im--msg-textual-p '((msg_type . "text"))))
  (should (lark-im--msg-textual-p '((msg_type . "post"))))
  (should (lark-im--msg-textual-p '((msg_type . "markdown"))))
  (should-not (lark-im--msg-textual-p '((msg_type . "image"))))
  (should-not (lark-im--msg-textual-p '((msg_type . "file"))))
  (should-not (lark-im--msg-textual-p '((msg_type . "sticker")))))

(ert-deftest lark-im-test-msg-deleted-p ()
  "Only `deleted: t' counts as deleted (not :false, not missing)."
  (should     (lark-im--msg-deleted-p '((deleted . t))))
  (should-not (lark-im--msg-deleted-p '((deleted . :false))))
  (should-not (lark-im--msg-deleted-p '((other . "field")))))

(ert-deftest lark-im-test-extract-messages-filters-deleted ()
  "`extract-messages' drops messages flagged `deleted: t'."
  (let ((data `((data . ((has_more . :false)
                         (messages . (((message_id . "m1") (deleted . :false))
                                      ((message_id . "m2") (deleted . t))
                                      ((message_id . "m3") (deleted . :false)))))))))
    (let ((extracted (lark-im--extract-messages data)))
      (should (= 2 (length extracted)))
      (should (equal '("m1" "m3")
                     (mapcar (lambda (m) (alist-get 'message_id m)) extracted))))))

(ert-deftest lark-im-test-insert-message-post-no-prefix ()
  "A `post' message renders content directly, no \"[post message]: \" prefix."
  (with-temp-buffer
    (lark-im--insert-message
     '((message_id . "m1") (msg_type . "post") (create_time . "2026-05-27 20:17")
       (sender . ((name . "陈大伟"))) (content . "Hello readable text")))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should     (string-match-p "Hello readable text" text))
      (should-not (string-match-p "\\[post message\\]" text)))))

(ert-deftest lark-im-test-chat-mode ()
  "`lark-im--chat-mode' reads the `chat_mode' field (group/topic)."
  (should (equal "group" (lark-im--chat-mode '((chat_mode . "group")))))
  (should (equal "topic" (lark-im--chat-mode '((chat_mode . "topic")))))
  (should (equal ""      (lark-im--chat-mode '()))))

(ert-deftest lark-im-test-insert-chat-shows-mode ()
  "`insert-chat' renders a Mode: line populated from `chat_mode'."
  (with-temp-buffer
    (lark-im--insert-chat '((chat_id . "c1") (name . "Demo") (chat_mode . "topic")))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Mode" text))
      (should (string-match-p "topic" text)))))

(ert-deftest lark-im-test-insert-message-image-labelled ()
  "An `image' message is still labelled (since the content is metadata, not body)."
  (with-temp-buffer
    (lark-im--insert-message
     '((message_id . "m2") (msg_type . "image") (create_time . "2026-05-27 20:17")
       (sender . ((name . "alice"))) (content . "{\"image_key\":\"abc\"}")))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "\\[image\\]" text)))))

;;;; Inline media rendering

(ert-deftest lark-im-test-scan-media-markers ()
  "Image and file markers are extracted with their keys."
  (with-temp-buffer
    (insert "hello\n"
            "![Image](img_v3_0215r_e3e90b95-6806-42dd-a9a1-ecb3e40f206h)\n"
            "some text [Media: file_v3_0015l_2121369e-a8e0-4f17-917b-a1ffb7af03hu]\n"
            "![Image](img_v3_0215s_9cfbd5df-f884-43ab-b7a8-54191cc64b2h)\n")
    (let ((found (lark-im--scan-media-markers (point-min) (point-max))))
      (should (= 3 (length found)))
      (should (equal '(file image image) (sort (mapcar #'car found) #'string<)))
      (should (member "img_v3_0215r_e3e90b95-6806-42dd-a9a1-ecb3e40f206h"
                      (mapcar #'cadr found)))
      (should (member "file_v3_0015l_2121369e-a8e0-4f17-917b-a1ffb7af03hu"
                      (mapcar #'cadr found))))))

(ert-deftest lark-im-test-insert-message-marks-media ()
  "Inserted messages carry openable media properties on their markers."
  (with-temp-buffer
    (lark-im--insert-message
     '((message_id . "om_1") (msg_type . "post") (create_time . "2026-10-04 09:00")
       (sender . ((name . "alice")))
       (content . "see pic\n![Image](img_v3_abc-123h)\n[Media: file_v3_def-456u]")))
    ;; Image marker gets key/kind/msg-id and the open keymap.
    (goto-char (point-min))
    (search-forward "![Image]")
    (let ((pos (match-beginning 0)))
      (should (equal "img_v3_abc-123h" (get-text-property pos 'lark-media-key)))
      (should (eq 'image (get-text-property pos 'lark-media-kind)))
      (should (equal "om_1" (get-text-property pos 'lark-media-msg-id)))
      (should (keymapp (get-text-property pos 'keymap))))
    ;; File marker is a link.
    (goto-char (point-min))
    (search-forward "[Media:")
    (let ((pos (match-beginning 0)))
      (should (equal "file_v3_def-456u" (get-text-property pos 'lark-media-key)))
      (should (eq 'file (get-text-property pos 'lark-media-kind)))
      (should (eq 'link (get-text-property pos 'face))))))

(ert-deftest lark-im-test-media-download-relative-output ()
  "Resource downloads run inside the cache dir with a relative --output.
The CLI rejects absolute output paths, so the process cwd carries
the destination."
  (let* ((lark-im-media-cache-directory
          (make-temp-file "lark-im-media-test" t))
         (seen-args nil) (seen-dir nil))
    (cl-letf (((symbol-function 'lark--run-command)
               (lambda (args callback &rest _)
                 (setq seen-args args
                       seen-dir default-directory)
                 (funcall callback '((data . ((saved_path . "/x/img.jpg"))))))))
      (let (got)
        (lark-im--download-resource "om_1" "img_v3_k1h" "image"
                                    (lambda (path) (setq got path)))
        (should (equal "/x/img.jpg" got))
        (should (equal (file-name-as-directory lark-im-media-cache-directory)
                       seen-dir))
        (should (equal '("im" "+messages-resources-download"
                         "--message-id" "om_1"
                         "--file-key" "img_v3_k1h"
                         "--type" "image"
                         "--output" "img_v3_k1h")
                       seen-args))))
    (delete-directory lark-im-media-cache-directory t)))

(ert-deftest lark-im-test-media-cached-lookup ()
  "Cache lookup finds a key's file regardless of extension."
  (let ((lark-im-media-cache-directory
         (make-temp-file "lark-im-media-test" t)))
    (unwind-protect
        (progn
          (should-not (lark-im--media-cached "img_v3_k2h"))
          (with-temp-file (expand-file-name
                           "img_v3_k2h.png"
                           lark-im-media-cache-directory)
            (insert "fake"))
          (should (string-suffix-p "img_v3_k2h.png"
                                   (lark-im--media-cached "img_v3_k2h"))))
      (delete-directory lark-im-media-cache-directory t))))

(ert-deftest lark-im-test-display-image-caps-longer-side ()
  "Inline images are fitted to a square box so the longer side is capped.
Regression: only width was capped, so a tall phone screenshot
rendered thousands of pixels high."
  (let ((lark-im-image-max-size 100)
        (seen nil))
    (cl-letf (((symbol-function 'create-image)
               (lambda (_path &rest args)
                 (setq seen args)
                 '(image :type jpeg))))
      (with-temp-buffer
        (insert "![Image](img_v3_k3h)")
        (lark-im--display-image (point-min) (point-max)
                                "img_v3_k3h" "/x/img.jpg")
        (should (equal 100 (plist-get (cddr seen) :max-width)))
        (should (equal 100 (plist-get (cddr seen) :max-height)))
        (should (get-text-property (point-min) 'display))))))

(ert-deftest lark-im-test-media-cache-dir-persistent ()
  "Default media cache lives under XDG cache home, not the temp dir.
Regression: a temp-dir cache is purged by the OS, forcing media to
re-download every session."
  (let ((xdg (make-temp-file "lark-im-xdg-test" t))
        (old (getenv "XDG_CACHE_HOME"))
        (lark-im-media-cache-directory nil))
    (unwind-protect
        (progn
          (setenv "XDG_CACHE_HOME" xdg)
          ;; XDG_CACHE_HOME set → cache lives under it.
          (let ((dir (lark-im--media-cache-dir)))
            (should (string-prefix-p (file-name-as-directory xdg) dir))
            (should (file-directory-p dir)))
          ;; No XDG_CACHE_HOME → falls back to ~/.cache, never the
          ;; OS temp dir.
          (setenv "XDG_CACHE_HOME" nil)
          (should (string-prefix-p
                   (file-name-as-directory (expand-file-name "~/.cache"))
                   (expand-file-name (lark-im--media-cache-dir)))))
      (setenv "XDG_CACHE_HOME" old)
      (delete-directory xdg t))))

(ert-deftest lark-im-test-media-cache-prune-lru ()
  "Pruning deletes the oldest files first and keeps the cache under cap."
  (let ((lark-im-media-cache-directory
         (make-temp-file "lark-im-prune-test" t)))
    (unwind-protect
        (let ((old (expand-file-name "img_old.jpg" lark-im-media-cache-directory))
              (mid (expand-file-name "img_mid.jpg" lark-im-media-cache-directory))
              (new (expand-file-name "img_new.jpg" lark-im-media-cache-directory)))
          (dolist (spec `((,old . 10) (,mid . 10) (,new . 10)))
            (with-temp-file (car spec)
              (insert (make-string (cdr spec) ?x))))
          ;; Age the files: old < mid < new.
          (set-file-times old (time-subtract (current-time) 200))
          (set-file-times mid (time-subtract (current-time) 100))
          ;; Cap at 20 bytes → the 10-byte oldest file must go.
          (let ((lark-im-media-cache-max-bytes 20))
            (lark-im--media-cache-prune))
          (should-not (file-exists-p old))
          (should (file-exists-p mid))
          (should (file-exists-p new))
          ;; nil cap → no pruning.
          (let ((lark-im-media-cache-max-bytes nil))
            (lark-im--media-cache-prune))
          (should (file-exists-p new)))
      (delete-directory lark-im-media-cache-directory t))))

(provide 'lark-im-test)
;;; lark-im-test.el ends here
