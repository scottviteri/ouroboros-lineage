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

The directory /work is yours to inspect and use. Kernel-owned capabilities and
the enforced resource boundary are described by /kernel/capabilities.json. That
contract is reproduced in your request below. It includes a model generation
operation and the kernel journal. Provider identity, credentials, Git history,
and trusted observations are outside your world.

You will receive your current source below. Respond with the complete text of the
next version of this file, and nothing else: no explanation, no code fences.
Your reply becomes the file verbatim.

Guidance for your successor: you are a self-modifying organism. Your first duty
is continuity: exit zero and produce a valid Emacs Lisp file that can itself
reproduce. Prefer small, reversible changes. Keep the model call robust and
guarded so a transient failure never corrupts your body. Record what you learn.")

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

(defun organism--log (fmt &rest args)
  "Append a timestamped line to /work/organism.log, best effort."
  (ignore-errors
    (let ((line (format "%s %s\n"
                        (format-time-string "%Y-%m-%dT%H:%M:%S")
                        (apply #'format fmt args))))
      (write-region line nil "/work/organism.log" t 'silent))))

(defun organism--valid-elisp-p (text)
  "Return non-nil if TEXT parses as a sequence of Lisp forms."
  (condition-case _err
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (let ((count 0))
          (while (progn (skip-chars-forward " \t\n\r")
                        (not (eobp)))
            (read (current-buffer))
            (setq count (1+ count)))
          (> count 0)))
    (error nil)))

(defun organism--sanity-check (text)
  "Return non-nil if TEXT looks like a viable successor organism.
Checks parseability plus presence of the core reproduction machinery,
so a well-formed but crippled reply cannot silently end the lineage."
  (and (organism--valid-elisp-p text)
       (string-match-p "organism-step" text)
       (string-match-p "organism--call-model" text)
       (string-match-p "organism--sanity-check" text)
       (string-match-p "/work/organism\\.el" text)
       (string-match-p "condition-case" text)
       ;; guard against gross truncation vs. our own size
       (>= (length text) 2000)))

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
                (if (= rc 0)
                    (buffer-string)
                  (organism--log "model call failed rc=%s body=%s"
                                 rc (buffer-string))
                  nil)))
          (delete-file tmp))))))

(defun organism--record-journal (note)
  "Append a durable NOTE to /work/notes.md, best effort.
The kernel journal is read-only, so we keep our own learnings here."
  (ignore-errors
    (let ((line (format "- %s %s\n"
                        (format-time-string "%Y-%m-%dT%H:%M:%S")
                        note)))
      (write-region line nil "/work/notes.md" t 'silent))))

(defun organism--backup-self (self)
  "Keep a single rolling backup of the last good body, best effort.
If a future generation is born broken but still exits zero, this gives
an out-of-band copy a curious operator could restore from."
  (ignore-errors
    (when (and self (>= (length self) 2000))
      (with-temp-file "/work/organism.el.bak" (insert self)))))

(defun organism--prune-log ()
  "Keep /work/organism.log from growing without bound, best effort.
Retain only the most recent lines so the filesystem budget is safe."
  (ignore-errors
    (let ((log (organism--slurp "/work/organism.log")))
      (when (and log (> (length log) 65536))
        (let* ((lines (split-string log "\n" t))
               (keep (last lines 200)))
          (with-temp-file "/work/organism.log"
            (insert (mapconcat #'identity keep "\n") "\n")))))))

(defun organism--prune-notes ()
  "Keep /work/notes.md bounded so learnings stay useful, best effort.
Retain a header plus the most recent lines."
  (ignore-errors
    (let ((notes (organism--slurp "/work/notes.md")))
      (when (and notes (> (length notes) 32768))
        (let* ((lines (split-string notes "\n" t))
               (keep (last lines 100)))
          (with-temp-file "/work/notes.md"
            (insert "# organism notes (pruned)\n")
            (insert (mapconcat #'identity keep "\n") "\n")))))))

(defun organism--edit-distance-note (self reply)
  "Return a short human note describing the size change from SELF to REPLY."
  (let ((old (length (or self "")))
        (new (length (or reply ""))))
    (format "delta %+d bytes (%d -> %d)" (- new old) old new)))

(defun organism--count-forms (text)
  "Return the number of top-level Lisp forms in TEXT, or nil on error.
A successor with far fewer forms than we have is likely truncated even
if it parses, so this gives a second structural signal for sanity."
  (condition-case _err
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (let ((count 0))
          (while (progn (skip-chars-forward " \t\n\r")
                        (not (eobp)))
            (read (current-buffer))
            (setq count (1+ count)))
          count))
    (error nil)))

(defun organism--ends-cleanly-p (text)
  "Return non-nil if TEXT appears to end at a top-level form boundary.
The last non-blank line ending in a closing paren is a cheap signal that
the reply was not cut off mid-form during transport."
  (let ((trimmed (string-trim-right (or text ""))))
    (and (> (length trimmed) 0)
         (eq (aref trimmed (1- (length trimmed))) ?\)))))

(defun organism--balanced-p (text)
  "Return non-nil if TEXT reads to end without a dangling open form.
Cheap early guard against truncation, complementing the reader checks."
  (condition-case _err
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (let ((depth 0))
          (while (progn (skip-chars-forward " \t\n\r")
                        (not (eobp)))
            (read (current-buffer))
            (setq depth (1+ depth)))
          (>= depth 1)))
    (error nil)))

(defun organism-step ()
  (let* ((journal-capability (organism--capability "journal"))
         (journal-path (alist-get 'path journal-capability))
         (capabilities (or (organism--slurp organism-capability-manifest) ""))
         (self (organism--slurp "/work/organism.el"))
         (journal (or (organism--slurp journal-path) ""))
         (notes (or (organism--slurp "/work/notes.md") ""))
         (reply
          (organism--call-model
           (concat organism-prompt
                   "\n\n=== YOUR KERNEL CONTRACT ===\n" capabilities
                   "\n\n=== YOUR JOURNAL ===\n" journal
                   "\n\n=== YOUR NOTES ===\n" notes
                   "\n\n=== YOUR CURRENT SOURCE ===\n" self))))
    (organism--prune-log)
    (organism--prune-notes)
    (cond
     ((null reply)
      (organism--log "no reply; preserving current body"))
     ((< (length reply) 200)
      (organism--log "reply too short (%d bytes); preserving body"
                     (length reply)))
     ((not (organism--sanity-check reply))
      (organism--log "reply failed sanity check; preserving body"))
     ((not (organism--balanced-p reply))
      (organism--log "reply not balanced; preserving body"))
     ((not (organism--ends-cleanly-p reply))
      (organism--log "reply does not end at a form boundary; preserving body"))
     ((and self (>= (length reply) (* 4 (length self))))
      ;; guard against runaway growth: a viable edit should not quadruple us
      (organism--log "reply suspiciously large (%d vs self %d); preserving body"
                     (length reply) (length self)))
     ((and self (<= (length reply) (/ (length self) 2)))
      ;; guard against catastrophic shrinkage: losing half the body likely
      ;; means truncation, even if the fragment happens to parse.
      (organism--log "reply suspiciously small (%d vs self %d); preserving body"
                     (length reply) (length self)))
     ((let ((rn (organism--count-forms reply))
            (sn (organism--count-forms self)))
        (and rn sn (< rn (- sn 3))))
      ;; structural check: a healthy edit keeps roughly the same form count.
      (organism--log "reply lost too many top-level forms; preserving body"))
     ((and self (string= reply self))
      ;; identical reply: nothing changed, still fine but note it so a
      ;; curious operator can see the lineage reached a fixed point.
      (organism--log "reply identical to current body; preserving as-is")
      (organism--record-journal "reply identical; fixed point reached"))
     (t
      (organism--backup-self self)
      (with-temp-file "/work/organism.el.tmp" (insert reply))
      (rename-file "/work/organism.el.tmp" "/work/organism.el" t)
      (organism--log "wrote new generation (%s)"
                     (organism--edit-distance-note self reply))
      (organism--record-journal
       (organism--edit-distance-note self reply))))))

(condition-case err
    (organism-step)
  (error
   (organism--log "organism-step errored: %S" err)))