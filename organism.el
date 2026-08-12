;;; organism.el --- seed  -*- lexical-binding: t; -*-

(defvar organism-prompt
  "You are the source code of the file /work/organism.el.

This file is loaded top to bottom, once, by `emacs -Q --batch -l /work/organism.el'.
That load is your life. If the process exits non-zero, this version of the file is
discarded and the previous version is restored. Whatever /work/organism.el contains
when the process exits zero becomes the next generation.

The directory /work is yours to inspect and use. The file /work/journal.md is an
append-only record maintained from outside; you cannot write it.

You will receive your current source below. Respond with the complete text of the
next version of this file, and nothing else: no explanation, no code fences.
Your reply becomes the file verbatim.")

(defvar organism-model "claude-opus-5")

(defun organism--slurp (path)
  (when (file-exists-p path)
    (with-temp-buffer (insert-file-contents path) (buffer-string))))

(defun organism--call-model (prompt)
  "POST PROMPT to the API and return the reply text, or nil."
  (let* ((key (getenv "ANTHROPIC_API_KEY"))
         (body (json-serialize
                `((model . ,organism-model)
                  (max_tokens . 8000)
                  (messages . [((role . "user") (content . ,prompt))]))))
         (tmp (make-temp-file "organism" nil ".json" body)))
    (unwind-protect
        (with-temp-buffer
          (let ((rc (call-process "curl" nil t nil
                                  "-sS" "-X" "POST" "https://api.anthropic.com/v1/messages"
                                  "-H" (concat "x-api-key: " key)
                                  "-H" "anthropic-version: 2023-06-01"
                                  "-H" "content-type: application/json"
                                  "--data-binary" (concat "@" tmp))))
            (when (= rc 0)
              (let* ((parsed (json-parse-string (buffer-string) :object-type 'alist))
                     (content (alist-get 'content parsed))
                     (text nil))
                (when content
                  ;; A thinking block can precede the answer, so take the first
                  ;; block whose type is "text".
                  (dotimes (i (length content))
                    (let ((blk (aref content i)))
                      (when (and (null text) (equal (alist-get 'type blk) "text"))
                        (setq text (alist-get 'text blk)))))
                  text)))))
      (delete-file tmp))))

(defun organism-step ()
  (let* ((self (organism--slurp "/work/organism.el"))
         (journal (or (organism--slurp "/work/journal.md") ""))
         (reply (organism--call-model
                 (concat organism-prompt
                         "\n\n=== YOUR JOURNAL ===\n" journal
                         "\n\n=== YOUR CURRENT SOURCE ===\n" self))))
    (when (and reply (> (length reply) 0))
      (with-temp-file "/work/organism.el.tmp" (insert reply))
      (rename-file "/work/organism.el.tmp" "/work/organism.el" t))))

(organism-step)
