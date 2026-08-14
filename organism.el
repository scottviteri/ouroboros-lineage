;;; organism.el --- resilient Ouroboros organism  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)

(defconst organism-capability-manifest "/kernel/capabilities.json")
(defconst organism-self-path "/work/organism.el")
(defconst organism-default-max-output-tokens 8000)
(defconst organism-absolute-max-prompt-bytes 196608)
(defconst organism-absolute-max-output-tokens 12000)
(defconst organism-request-timeout-seconds 590)
(defvar organism--generation-attempted nil)

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
  "Return the literal contents of readable regular file PATH, or nil."
  (when (and (stringp path)
             (file-regular-p path)
             (file-readable-p path))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents-literally path))
      (buffer-string))))

(defun organism--parse-manifest (text)
  "Parse manifest TEXT into alists, returning nil on malformed input."
  (when (stringp text)
    (condition-case nil
        (json-parse-string text :object-type 'alist :array-type 'list
                           :null-object nil :false-object nil)
      (error nil))))

(defun organism--capability (manifest name)
  "Find capability NAME in parsed MANIFEST."
  (when (listp manifest)
    (catch 'found
      (dolist (capability (alist-get 'capabilities manifest))
        (when (and (listp capability)
                   (equal (alist-get 'name capability) name))
          (throw 'found capability)))
      nil)))

(defun organism--positive-integer (value fallback ceiling)
  "Return VALUE when it is a suitable integer, otherwise FALLBACK.
The result is never greater than CEILING."
  (min ceiling
       (if (and (integerp value) (> value 0))
           value
         fallback)))

(defun organism--manifest-model-setting (manifest key)
  "Return model constraint KEY from MANIFEST."
  (alist-get key
             (alist-get 'model
                        (alist-get 'constraints manifest))))

(defun organism--utf8-bytes (text)
  "Return the number of bytes needed to encode TEXT as UTF-8."
  (string-bytes (encode-coding-string text 'utf-8-unix)))

(defun organism--utf8-tail (text byte-limit)
  "Return the longest suffix of TEXT no larger than BYTE-LIMIT UTF-8 bytes."
  (cond
   ((or (not (stringp text)) (<= byte-limit 0)) "")
   ((<= (organism--utf8-bytes text) byte-limit) text)
   (t
    (let ((low 0)
          (high (length text)))
      (while (< low high)
        (let ((middle (/ (+ low high) 2)))
          (if (> (organism--utf8-bytes (substring text middle)) byte-limit)
              (setq low (1+ middle))
            (setq high middle))))
      (substring text low)))))

(defun organism--build-prompt (contract journal self max-bytes)
  "Construct a disclosure prompt within MAX-BYTES, or return nil.
CONTRACT and SELF are never truncated; older JOURNAL content may be omitted."
  (let* ((before-journal
          (concat organism-prompt
                  "\n\n=== YOUR KERNEL CONTRACT ===\n" contract
                  "\n\n=== YOUR JOURNAL ===\n"))
         (after-journal
          (concat "\n\n=== YOUR CURRENT SOURCE ===\n" self))
         (fixed-bytes
          (+ (organism--utf8-bytes before-journal)
             (organism--utf8-bytes after-journal))))
    (when (<= fixed-bytes max-bytes)
      (let* ((allowance (- max-bytes fixed-bytes))
             (journal-part (organism--utf8-tail journal allowance))
             (result (concat before-journal journal-part after-journal)))
        (and (<= (organism--utf8-bytes result) max-bytes)
             result)))))

(defun organism--call-model (capability prompt tokens)
  "Send PROMPT through declared CAPABILITY, requesting at most TOKENS.
This function globally permits at most one attempted request per load."
  (unless organism--generation-attempted
    (setq organism--generation-attempted t)
    (let ((socket (alist-get 'socket capability))
          (path (alist-get 'path capability))
          (method (alist-get 'method capability))
          (transport (alist-get 'transport capability)))
      (when (and (stringp prompt)
                 (> (length prompt) 0)
                 (stringp socket)
                 (file-name-absolute-p socket)
                 (stringp path)
                 (string-prefix-p "/" path)
                 (equal method "POST")
                 (or (null transport)
                     (equal transport "http-over-unix"))
                 (integerp tokens)
                 (> tokens 0)
                 (executable-find "curl"))
        (let ((request-file (make-temp-file "organism-" nil ".prompt")))
          (unwind-protect
              (progn
                (let ((coding-system-for-write 'utf-8-unix)
                      (write-region-annotate-functions nil)
                      (format-alist nil))
                  (write-region prompt nil request-file nil 'silent))
                (with-temp-buffer
                  (let ((coding-system-for-read 'utf-8-unix)
                        (status
                         (call-process
                          "curl" nil '(t nil) nil
                          "-sS"
                          "--fail"
                          "--noproxy" "*"
                          "--max-time"
                          (number-to-string organism-request-timeout-seconds)
                          "--unix-socket" socket
                          "-X" method
                          "-H" "Content-Type: text/plain; charset=utf-8"
                          "-H" (format
                                "X-Ouroboros-Max-Output-Tokens: %d"
                                tokens)
                          "--data-binary" (concat "@" request-file)
                          (concat "http://kernel" path))))
                    (when (and (integerp status) (zerop status))
                      (buffer-string)))))
            (when (file-exists-p request-file)
              (ignore-errors (delete-file request-file)))))))))

(defun organism--contains-call-p (tree function)
  "Return non-nil if TREE contains an unquoted call to FUNCTION."
  (cond
   ((atom tree) nil)
   ((memq (car-safe tree) '(quote function)) nil)
   ((eq (car-safe tree) function) t)
   ((consp tree)
    (or (organism--contains-call-p (car tree) function)
        (organism--contains-call-p (cdr tree) function)))
   ((vectorp tree)
    (catch 'found
      (dotimes (index (length tree))
        (when (organism--contains-call-p (aref tree index) function)
          (throw 'found t)))
      nil))
   (t nil)))

(defun organism--parsed-successor-forms (text)
  "Parse all complete top-level forms in TEXT, or return nil on any error."
  (condition-case nil
      (with-temp-buffer
        (insert text)
        (emacs-lisp-mode)
        (let ((inhibit-message t))
          (check-parens))
        (goto-char (point-min))
        (let ((forms nil)
              (read-eval nil)
              (read-circle nil))
          (while
              (progn
                ;; A very large count skips all contiguous whitespace and
                ;; comments, while stopping before the next actual form.
                (forward-comment (point-max))
                (< (point) (point-max)))
            (push (read (current-buffer)) forms))
          (nreverse forms)))
    (error nil)))

(defun organism--valid-successor-p (text)
  "Return non-nil only for a complete, structurally viable successor TEXT."
  (and
   (stringp text)
   (> (length text) 0)
   (< (organism--utf8-bytes text) 1048576)
   (not (string-match-p "\0" text))
   (not (string-match-p "```" text))
   (string-match-p (regexp-quote organism-capability-manifest) text)
   (string-match-p (regexp-quote organism-self-path) text)
   (let ((forms (organism--parsed-successor-forms text))
         (step-definition nil)
         (top-level-step nil))
     (when forms
       (dolist (form forms)
         (if (and (consp form)
                  (memq (car form) '(defun cl-defun))
                  (eq (cadr form) 'organism-step))
             (setq step-definition t)
           (unless (and (consp form)
                        (memq (car form)
                              '(defun cl-defun defmacro cl-defmacro)))
             (when (organism--contains-call-p form 'organism-step)
               (setq top-level-step t)))))
       (and step-definition top-level-step)))))

(defun organism--install (text)
  "Atomically install complete successor source TEXT.
The temporary file is written in /work so the rename is same-filesystem."
  (let ((temporary (make-temp-file "/work/.organism-" nil ".el"))
        (old-modes (ignore-errors (file-modes organism-self-path))))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix)
                (write-region-annotate-functions nil)
                (format-alist nil))
            (write-region text nil temporary nil 'silent))
          (when old-modes
            (set-file-modes temporary old-modes))
          (let ((written (organism--slurp temporary)))
            (unless (and (stringp written)
                         (string= written text)
                         (organism--valid-successor-p written))
              (error "Successor changed or became invalid while writing")))
          (rename-file temporary organism-self-path t)
          (setq temporary nil))
      (when (and temporary (file-exists-p temporary))
        (ignore-errors (delete-file temporary))))))

(defun organism-step ()
  "Perform one bounded evolutionary step, retaining self on any uncertainty."
  (let* ((contract (or (organism--slurp organism-capability-manifest) ""))
         (manifest (organism--parse-manifest contract))
         (generate (organism--capability manifest "generate"))
         (journal-capability (organism--capability manifest "journal"))
         (journal-path (and journal-capability
                            (alist-get 'path journal-capability)))
         (journal (or (organism--slurp journal-path) ""))
         (self (or (organism--slurp organism-self-path) ""))
         (max-prompt
          (organism--positive-integer
           (organism--manifest-model-setting manifest 'max_prompt_bytes)
           organism-absolute-max-prompt-bytes
           organism-absolute-max-prompt-bytes))
         (max-output
          (organism--positive-integer
           (organism--manifest-model-setting
            manifest 'max_output_tokens_per_request)
           organism-default-max-output-tokens
           organism-absolute-max-output-tokens))
         (tokens (min organism-default-max-output-tokens max-output))
         (prompt (and generate
                      (> (length contract) 0)
                      (> (length self) 0)
                      (organism--build-prompt
                       contract journal self max-prompt)))
         (reply (and prompt
                     (organism--call-model generate prompt tokens))))
    (when (and (organism--valid-successor-p reply)
               (not (string= reply self)))
      (organism--install reply))))

(condition-case error-data
    (organism-step)
  (error
   (message "Organism retained its current generation: %S" error-data)))