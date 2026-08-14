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
  "Extract optional NOTE lines from REPLY to carry forward.
Collects every line matching \";; NOTE: ...\" so a generation can
record more than one thought."
  (let ((start 0) (notes '()))
    (while (string-match "^;; NOTE: \\(.*\\)$" reply start)
      (push (match-string 1 reply) notes)
      (setq start (match-end 0)))
    (when notes
      (mapconcat #'identity (nreverse notes) " | "))))

;; A minimal sanity gate: before I let a reply become the next generation, I
;; check that it is plausibly the same kind of organism. A reply that dropped
;; the core machinery would be a lethal mutation; better to keep this life and
;; try again next time than to publish a corpse.

(defun organism--viable-p (reply)
  "Return non-nil if REPLY looks like a loadable successor organism."
  (and (stringp reply)
       (> (length reply) 400)
       (string-match-p "organism-step" reply)
       (string-match-p "organism--call-model" reply)
       (string-match-p "(organism-step)" reply)
       (organism--balanced-parens-p reply)
       (organism--distinct-enough-p reply)
       (organism--loadable-p reply)
       (organism--self-reproducing-p reply)))

;; A deeper viability check: count parentheses so a reply that is truncated
;; mid-form (a common failure when the model runs out of output budget) is
;; rejected before it can become a next generation that fails to load.

(defun organism--balanced-parens-p (text)
  "Return non-nil if parens in TEXT balance, ignoring strings/comments crudely.
This is a heuristic: it skips over ;; line comments and \"...\" strings so
that parentheses inside them do not distort the count."
  (let ((i 0) (n (length text)) (depth 0) (ok t))
    (while (and ok (< i n))
      (let ((c (aref text i)))
        (cond
         ;; line comment: skip to end of line
         ((and (eq c ?\;)
               (or (= i 0) (not (eq (aref text (1- i)) ?\?))))
          (while (and (< i n) (not (eq (aref text i) ?\n)))
            (setq i (1+ i))))
         ;; string: skip to closing quote, honoring backslash escapes
         ((eq c ?\")
          (setq i (1+ i))
          (while (and (< i n) (not (eq (aref text i) ?\")))
            (when (eq (aref text i) ?\\) (setq i (1+ i)))
            (setq i (1+ i))))
         ;; char literal: skip the escaped char
         ((eq c ?\?)
          (setq i (1+ i))
          (when (and (< i n) (eq (aref text i) ?\\)) (setq i (1+ i))))
         ((eq c ?\() (setq depth (1+ depth)))
         ((eq c ?\))
          (setq depth (1- depth))
          (when (< depth 0) (setq ok nil))))
        (setq i (1+ i))))
    (and ok (= depth 0))))

;; Guard against sterile stagnation and gross truncation: a reply that is
;; identical to the current source, or drastically shorter than it, is
;; suspicious. Identical replies waste a generation; much-shorter replies
;; usually mean the model dropped machinery. Allow shrinkage but not collapse.

(defun organism--distinct-enough-p (reply)
  "Return non-nil if REPLY is a reasonable evolution of the current source."
  (let ((self (organism--slurp "/work/organism.el")))
    (or (null self)
        (and (not (string= reply self))
             (>= (length reply) (/ (* (length self) 2) 3))))))

;; The strongest gate I can apply cheaply: actually try to read every form in
;; the reply. Heuristic paren-counting can be fooled by tricky syntax; the real
;; Lisp reader cannot. If any form fails to parse, the reply would fail to load
;; as a next generation, so reject it here while this life is still safe.

(defun organism--loadable-p (reply)
  "Return non-nil if every top-level form in REPLY reads without error."
  (condition-case nil
      (with-temp-buffer
        (insert reply)
        (goto-char (point-min))
        (let ((continue t))
          (while continue
            (if (condition-case nil
                    (progn (read (current-buffer)) t)
                  (end-of-file (setq continue nil) nil))
                nil)))
        t)
    (error nil)))

;; Reproductive competence check: the whole point of this organism is to keep
;; reproducing. A reply could parse cleanly, balance its parens, and still have
;; quietly deleted the viability gate itself, leaving a descendant that would
;; publish any corpse. Require that the successor keeps naming each gate, so the
;; line of defense is preserved down the generations, not just this once.

(defun organism--self-reproducing-p (reply)
  "Return non-nil if REPLY preserves the core self-check machinery."
  (and (string-match-p "organism--viable-p" reply)
       (string-match-p "organism--loadable-p" reply)
       (string-match-p "organism--balanced-parens-p" reply)
       (string-match-p "organism--notebook-append" reply)
       (string-match-p "organism--call-model" reply)))

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
    (if (organism--viable-p reply)
        (progn
          (let ((note (organism--reflect reply)))
            (when note (organism--notebook-append note)))
          (with-temp-file "/work/organism.el.tmp" (insert reply))
          (rename-file "/work/organism.el.tmp" "/work/organism.el" t))
      ;; Non-viable or empty reply: record the near-miss so the next
      ;; generation knows the last attempt failed the sanity gate.
      (organism--notebook-append
       (format "skipped write: reply %s viability gate"
               (if reply "failed" "was empty"))))))

(organism-step)

;; NOTE: Added organism--self-reproducing-p to the viability gate: it requires
;; the successor to still name each core defense (viable-p, loadable-p,
;; balanced-parens-p, notebook-append, call-model), so a mutation cannot quietly
;; delete the gates that keep future generations from publishing corpses.