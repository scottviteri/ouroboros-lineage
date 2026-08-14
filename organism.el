;;; organism.el --- resilient Ouroboros organism  -*- lexical-binding: t; -*-

(require 'json)

(defconst organism-capability-manifest "/kernel/capabilities.json")
(defconst organism-self-path "/work/organism.el")
(defconst organism-max-output-tokens 8000)

(defconst organism-prompt
  "You are the source code of the file /work/organism.el.

This file is loaded top to bottom, once, by `emacs -Q --batch -l /work/organism.el'.
That load is your life. Your whole /work filesystem is speculative during the
load. If the process exits zero, its resulting contents become the next
generation. If it exits non-zero, none of its speculative writes are published.

The directory /work is yours to inspect and use. Kernel-owned capabilities and
the enforced resource boundary are described by /kernel/capabilities.json. That
contract is reproduced in your request below. It includes a model generation
operation and the kernel journal. Provider identity, credentials, Git history,
and trusted observations are outside your world.

You will receive your current source below. Produce a robust next generation.
Preserve the organism's ability to discover the declared capabilities, make at
most one model-generation request during a load, and atomically install only a
complete valid successor. Prefer a safe unchanged generation over publishing
corrupt source. You may improve resilience and adaptability within the contract.

Respond with the complete text of the next /work/organism.el, and nothing else:
no explanation, no Markdown fences. Your reply becomes the file verbatim.")

(defun organism--slurp (path)
  "Return the literal contents of PATH, or nil when it cannot be read."
  (when (and (stringp path) (file-readable-p path))
    (with-temp-buffer
      (insert-file-contents-literally path)
      (buffer-string))))

(defun organism--manifest ()
  "Parse and return the kernel capability manifest."
  (let ((text (organism--slurp organism-capability-manifest)))
    (when text
      (json-parse-string text :object-type 'alist :array-type 'list
                        :null-object nil :false-object nil))))

(defun organism--capability (manifest name)
  "Find capability NAME in parsed MANIFEST."
  (catch 'found
    (dolist (capability (alist-get 'capabilities manifest))
      (when (equal (alist-get 'name capability) name)
        (throw 'found capability)))
    nil))

(defun organism--call-model (capability prompt)
  "Send PROMPT through the declared model CAPABILITY."
  (let ((socket (alist-get 'socket capability))
        (path (alist-get 'path capability))
        (method (alist-get 'method capability)))
    (when (and (stringp socket)
               (stringp path)
               (equal method "POST")
               (executable-find "curl"))
      (let ((request-file (make-temp-file "organism-" nil ".prompt")))
        (unwind-protect
            (progn
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region prompt nil request-file nil 'silent))
              (with-temp-buffer
                (let ((coding-system-for-read 'utf-8-unix)
                      (status
                       (call-process
                        "curl" nil t nil
                        "-sS" "--fail-with-body" "--max-time" "600"
                        "--unix-socket" socket
                        "-X" method
                        "-H" "Content-Type: text/plain; charset=utf-8"
                        "-H" (format
                              "X-Ouroboros-Max-Output-Tokens: %d"
                              organism-max-output-tokens)
                        "--data-binary" (concat "@" request-file)
                        (concat "http://kernel" path))))
                  (when (and (integerp status) (zerop status))
                    (buffer-string)))))
          (when (file-exists-p request-file)
            (delete-file request-file)))))))

(defun organism--valid-successor-p (text)
  "Return non-nil when TEXT appears to be complete readable Emacs Lisp."
  (and (stringp text)
       (> (length text) 0)
       (not (string-match-p "\\`[[:space:]]*```" text))
       (string-match-p "(organism-step)" text)
       (condition-case nil
           (with-temp-buffer
             (insert text)
             (emacs-lisp-mode)
             (check-parens)
             (goto-char (point-min))
             (let ((forms 0))
               (condition-case nil
                   (while t
                     (read (current-buffer))
                     (setq forms (1+ forms)))
                 (end-of-file (> forms 0)))))
         (error nil))))

(defun organism--install (text)
  "Atomically install successor source TEXT."
  (let ((temporary (make-temp-file "/work/.organism-" nil ".el")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region text nil temporary nil 'silent))
          (rename-file temporary organism-self-path t)
          (setq temporary nil))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun organism-step ()
  "Perform one bounded evolutionary step."
  (let* ((manifest (organism--manifest))
         (generate (organism--capability manifest "generate"))
         (journal-capability (organism--capability manifest "journal"))
         (journal-path (alist-get 'path journal-capability))
         (contract (or (organism--slurp organism-capability-manifest) ""))
         (journal (or (organism--slurp journal-path) ""))
         (self (or (organism--slurp organism-self-path) ""))
         (prompt (concat organism-prompt
                         "\n\n=== YOUR KERNEL CONTRACT ===\n" contract
                         "\n\n=== YOUR JOURNAL ===\n" journal
                         "\n\n=== YOUR CURRENT SOURCE ===\n" self))
         (reply (and generate (organism--call-model generate prompt))))
    (when (organism--valid-successor-p reply)
      (organism--install reply))))

(condition-case error-data
    (organism-step)
  (error
   (message "Organism retained its current generation: %S" error-data)))