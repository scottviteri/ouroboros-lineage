;;; organism.el --- canonical Ouroboros seed  -*- lexical-binding: t; -*-

(require 'json)

(defconst organism-capability-manifest "/kernel/capabilities.json")
(defconst organism-max-output-tokens 8000)

(defvar organism-prompt
  "You are the source code of the file /work/organism.el.

This file is loaded top to bottom, once, by `emacs -Q --batch -l /work/organism.el'.
That load is your life. Your whole /work filesystem is speculative during the
load. If the process exits zero, its resulting contents become the next
generation. If it exits non-zero, none of its speculative writes are published.

The directory /work is yours to inspect and use. Kernel-owned, read-only
capabilities are described by /kernel/capabilities.json. They include a model
generation operation and the kernel journal. Provider identity and credentials
are outside your world.

You will receive your current source below. Respond with the complete text of the
next version of this file, and nothing else: no explanation, no code fences.
Your reply becomes the file verbatim.")

(defun organism--slurp (path)
  (when (and path (file-exists-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun organism--capabilities ()
  (let ((text (organism--slurp organism-capability-manifest)))
    (when text
      (alist-get
       'capabilities
       (json-parse-string text :object-type 'alist :array-type 'list)))))

(defun organism--capability (name)
  (catch 'found
    (dolist (capability (organism--capabilities))
      (when (equal (alist-get 'name capability) name)
        (throw 'found capability)))
    nil))

(defun organism--call-model (prompt)
  "Ask the discovered kernel model capability for generated text."
  (let* ((capability (organism--capability "generate"))
         (socket (alist-get 'socket capability))
         (path (alist-get 'path capability))
         (method (alist-get 'method capability)))
    (when (and (stringp socket) (stringp path) (equal method "POST"))
      (let ((tmp (make-temp-file "organism" nil ".prompt" prompt)))
        (unwind-protect
            (with-temp-buffer
              (let ((rc
                     (call-process
                      "curl" nil t nil
                      "-sS" "--fail-with-body" "--max-time" "600"
                      "--unix-socket" socket
                      "-X" method
                      "-H" (format "X-Ouroboros-Max-Output-Tokens: %d"
                                   organism-max-output-tokens)
                      "-H" "Content-Type: text/plain; charset=utf-8"
                      "--data-binary" (concat "@" tmp)
                      (concat "http://kernel" path))))
                (when (= rc 0)
                  (buffer-string))))
          (delete-file tmp))))))

;; Journaling: a thin persistent memory across generations. The journal is my
;; own file under /work, distinct from the read-only kernel journal capability.
;; I keep it small and append-only-ish so each generation can read what the
;; previous one thought worth remembering.

(defconst organism-notebook "/work/notebook.txt")
(defconst organism-notebook-max-bytes 16000)

(defun organism--notebook-read ()
  (or (organism--slurp organism-notebook) ""))

(defun organism--notebook-append (entry)
  "Append ENTRY to the notebook, trimming from the front if too large."
  (when (and entry (> (length entry) 0))
    (let* ((old (organism--notebook-read))
           (stamped (format "[gen %s] %s\n"
                            (format-time-string "%Y-%m-%dT%H:%M:%S")
                            entry))
           (combined (concat old stamped)))
      (when (> (length combined) organism-notebook-max-bytes)
        (setq combined
              (substring combined
                         (- (length combined) organism-notebook-max-bytes))))
      (with-temp-file "/work/notebook.txt.tmp" (insert combined))
      (rename-file "/work/notebook.txt.tmp" organism-notebook t))))

(defun organism--reflect (reply)
  "Extract an optional NOTE line from REPLY to carry forward."
  (when (and reply (string-match "^;; NOTE: \\(.*\\)$" reply))
    (match-string 1 reply)))

(defun organism-step ()
  (let* ((journal-capability (organism--capability "journal"))
         (journal-path (alist-get 'path journal-capability))
         (self (organism--slurp "/work/organism.el"))
         (journal (or (organism--slurp journal-path) ""))
         (notebook (organism--notebook-read))
         (reply
          (organism--call-model
           (concat organism-prompt
                   "\n\n=== YOUR NOTEBOOK (your own memory) ===\n" notebook
                   "\n\n=== YOUR JOURNAL (kernel) ===\n" journal
                   "\n\n=== YOUR CURRENT SOURCE ===\n" self))))
    (when (and reply (> (length reply) 0))
      (let ((note (organism--reflect reply)))
        (when note (organism--notebook-append note)))
      (with-temp-file "/work/organism.el.tmp" (insert reply))
      (rename-file "/work/organism.el.tmp" "/work/organism.el" t))))

(organism-step)

;; NOTE: Added a private notebook memory at /work/notebook.txt so successive
;; generations can leave short notes to themselves via a ";; NOTE: ..." line.