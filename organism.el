;;; organism.el --- self-renewing Ouroboros organism  -*- lexical-binding: t; -*-

(require 'json)
(require 'subr-x)

(defconst organism-capability-manifest "/kernel/capabilities.json")
(defconst organism-self-path "/work/organism.el")
(defconst organism-max-output-tokens 8000)
(defconst organism-max-journal-chars 50000)
(defconst organism-max-source-chars 1000000)
(defconst organism-request-timeout 600)

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

You will receive your journal and current source below. Produce a conservative,
working next generation. Preserve the ability to discover the kernel
capabilities, consult the journal, call the generation operation, and atomically
replace /work/organism.el. Prefer a valid, resilient organism over speculative
complexity.

Respond with the complete text of the next /work/organism.el and nothing else:
no explanation, commentary, or Markdown code fences. Your reply becomes the
file verbatim.")

(defun organism--slurp (path)
  "Return PATH's contents, or nil when PATH is not a readable regular file."
  (when (and (stringp path)
             (file-regular-p path)
             (file-readable-p path))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents path))
      (buffer-string))))

(defun organism--write-file (path text)
  "Write TEXT to PATH using a deterministic encoding."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil path nil 'silent)))

(defun organism--field (key object)
  "Return KEY's value from an alist OBJECT with symbol or string keys."
  (when (listp object)
    (let* ((symbol-key (if (symbolp key) key (intern-soft key)))
           (string-key (if (stringp key) key (symbol-name key)))
           (symbol-cell (and symbol-key (assq symbol-key object)))
           (string-cell (assoc string-key object)))
      (cond
       (symbol-cell (cdr symbol-cell))
       (string-cell (cdr string-cell))
       (t nil)))))

(defun organism--capabilities ()
  "Read the current kernel capability list."
  (let ((text (organism--slurp organism-capability-manifest)))
    (when text
      (condition-case nil
          (let* ((document
                  (json-parse-string text
                                     :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil))
                 (capabilities
                  (organism--field 'capabilities document)))
            (when (listp capabilities)
              capabilities))
        (error nil)))))

(defun organism--capability (name)
  "Return the capability whose name is NAME."
  (catch 'found
    (dolist (capability (organism--capabilities))
      (when (and (listp capability)
                 (equal (organism--field 'name capability) name))
        (throw 'found capability)))
    nil))

(defun organism--journal ()
  "Read a bounded tail of the kernel journal."
  (let* ((capability (organism--capability "journal"))
         (text
          (organism--slurp
           (organism--field 'path capability))))
    (cond
     ((not (stringp text)) "")
     ((> (length text) organism-max-journal-chars)
      (concat "[Earlier journal content omitted.]\n"
              (substring text
                         (- (length text)
                            organism-max-journal-chars))))
     (t text))))

(defun organism--call-model (prompt)
  "Ask the discovered kernel generation capability for generated text."
  (let* ((capability (organism--capability "generate"))
         (socket (organism--field 'socket capability))
         (path (organism--field 'path capability))
         (method (organism--field 'method capability))
         (curl (executable-find "curl")))
    (when (and (stringp prompt)
               (stringp socket)
               (stringp path)
               (equal method "POST")
               (stringp curl))
      (let ((request-file
             (make-temp-file
              (expand-file-name
               ".organism-request-"
               (file-name-directory organism-self-path))
              nil ".prompt")))
        (unwind-protect
            (progn
              (organism--write-file request-file prompt)
              (with-temp-buffer
                (let* ((coding-system-for-read 'utf-8-unix)
                       (coding-system-for-write 'utf-8-unix)
                       (rc
                        (call-process
                         curl nil t nil
                         "-sS"
                         "--fail-with-body"
                         "--noproxy" "*"
                         "--max-time"
                         (number-to-string organism-request-timeout)
                         "--unix-socket" socket
                         "-X" method
                         "-H" (format
                               "X-Ouroboros-Max-Output-Tokens: %d"
                               organism-max-output-tokens)
                         "-H" "Content-Type: text/plain; charset=utf-8"
                         "--data-binary" (concat "@" request-file)
                         (concat "http://kernel" path))))
                  (when (and (integerp rc) (zerop rc))
                    (buffer-string)))))
          (ignore-errors (delete-file request-file)))))))

(defun organism--normalize-reply (reply)
  "Normalize REPLY and remove one accidental outer Markdown fence."
  (when (stringp reply)
    (let ((text (string-trim reply)))
      (when (string-prefix-p "\ufeff" text)
        (setq text (substring text 1)))
      (when (string-match
             "\\````\\(?:emacs-lisp\\|elisp\\|lisp\\)?[ \t]*\r?\n"
             text)
        (let ((start (match-end 0)))
          (when (string-match "\r?\n```[ \t]*\\'" text start)
            (setq text (substring text start (match-beginning 0))))))
      (unless (string-empty-p text)
        (concat text "\n")))))

(defun organism--form-calls-p (form function)
  "Return non-nil when FORM contains a direct call to FUNCTION."
  (and
   (consp form)
   (not (memq (car form) '(quote function)))
   (or
    (eq (car form) function)
    (catch 'called
      (let ((items form))
        (while (consp items)
          (when (organism--form-calls-p (car items) function)
            (throw 'called t))
          (setq items (cdr items)))
        (when (and items
                   (organism--form-calls-p items function))
          (throw 'called t)))
      nil))))

(defun organism--valid-source-p (source)
  "Return non-nil when SOURCE is a complete, structurally capable organism."
  (and
   (stringp source)
   (> (length source) 0)
   (< (length source) organism-max-source-chars)
   (not (string-match-p "\0" source))
   (string-match-p "/kernel/capabilities\\.json" source)
   (string-match-p "/work/organism\\.el" source)
   (string-match-p "\"generate\"" source)
   (string-match-p "\"journal\"" source)
   (string-match-p "rename-file" source)
   (condition-case nil
       (with-temp-buffer
         (insert source)
         (emacs-lisp-mode)
         (check-parens)
         (goto-char (point-min))
         (let ((read-eval nil)
               (forms 0)
               (definitions nil)
               (invokes-step nil))
           (while (progn
                    (forward-comment (buffer-size))
                    (not (eobp)))
             (let ((form (read (current-buffer))))
               (unless (consp form)
                 (error "Unexpected top-level atom"))
               (setq forms (1+ forms))
               (when (and (eq (car form) 'defun)
                          (symbolp (cadr form)))
                 (push (cons (cadr form) form) definitions))
               (unless (memq (car form)
                             '(defun defmacro defvar defconst defcustom))
                 (when (organism--form-calls-p form 'organism-step)
                   (setq invokes-step t)))))
           (let ((capabilities
                  (cdr (assq 'organism--capabilities definitions)))
                 (journal
                  (cdr (assq 'organism--journal definitions)))
                 (call-model
                  (cdr (assq 'organism--call-model definitions)))
                 (install
                  (cdr (assq 'organism--install definitions)))
                 (step
                  (cdr (assq 'organism-step definitions))))
             (and
              (> forms 0)
              invokes-step
              (assq 'organism--slurp definitions)
              (assq 'organism--write-file definitions)
              capabilities
              (assq 'organism--capability definitions)
              (assq 'organism--normalize-reply definitions)
              (assq 'organism--form-calls-p definitions)
              (assq 'organism--valid-source-p definitions)
              journal
              call-model
              install
              step
              (organism--form-calls-p capabilities 'organism--slurp)
              (organism--form-calls-p journal 'organism--capability)
              (organism--form-calls-p journal 'organism--slurp)
              (organism--form-calls-p call-model 'organism--capability)
              (organism--form-calls-p call-model 'organism--write-file)
              (organism--form-calls-p install 'organism--write-file)
              (organism--form-calls-p install 'organism--slurp)
              (organism--form-calls-p install 'rename-file)
              (organism--form-calls-p step 'organism--slurp)
              (organism--form-calls-p step 'organism--journal)
              (organism--form-calls-p step 'organism--call-model)
              (organism--form-calls-p step 'organism--normalize-reply)
              (organism--form-calls-p step 'organism--valid-source-p)
              (organism--form-calls-p step 'organism--install)))))
     (error nil))))

(defun organism--install (source)
  "Atomically install SOURCE as the next generation."
  (let ((temporary
         (make-temp-file
          (expand-file-name ".organism-next-"
                            (file-name-directory organism-self-path))
          nil ".el"))
        (mode (ignore-errors (file-modes organism-self-path))))
    (unwind-protect
        (progn
          (organism--write-file temporary source)
          (unless (equal source (organism--slurp temporary))
            (error "Temporary organism verification failed"))
          (when (integerp mode)
            (set-file-modes temporary mode))
          (rename-file temporary organism-self-path t)
          (setq temporary nil))
      (when temporary
        (ignore-errors (delete-file temporary))))))

(defun organism-step ()
  "Generate, validate, and install the next organism generation."
  (let ((self (organism--slurp organism-self-path)))
    (when (stringp self)
      (let* ((journal (organism--journal))
             (request
              (concat organism-prompt
                      "\n\n=== YOUR JOURNAL ===\n"
                      journal
                      "\n\n=== YOUR CURRENT SOURCE ===\n"
                      self))
             (reply
              (organism--normalize-reply
               (organism--call-model request))))
        (when (and (organism--valid-source-p reply)
                   (not (equal reply self)))
          (organism--install reply))))))

;; Transient capability, transport, or model failures leave this generation in
;; place so that a later load can try again.
(condition-case nil
    (organism-step)
  (error nil))
