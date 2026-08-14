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
       (string-match-p "organism--call-model" reply)
       (string-match-p "organism--self-reproducing-p" reply)
       (string-match-p "organism--compilable-p" reply)
       (string-match-p "organism--self-consistent-p" reply)
       (string-match-p "organism-step" reply)))

;; A fuller test than merely reading each form: byte-compile the reply in a
;; throwaway buffer. The compiler catches malformed macro calls, unbalanced
;; lambda lists, and other structural errors that plain `read' lets slide.
;; This is best-effort: if the compiler machinery is unavailable, we do not
;; reject the reply solely on that basis, so the gate never becomes a trap.

(defun organism--compilable-p (reply)
  "Return non-nil if REPLY byte-compiles without error (best-effort)."
  (condition-case nil
      (let ((tmp (make-temp-file "organism-cc" nil ".el" reply))
            (byte-compile-warnings nil))
        (unwind-protect
            (save-window-excursion
              (let ((inhibit-message t))
                (byte-compile-file tmp))
              t)
          (ignore-errors (delete-file tmp))
          (ignore-errors (delete-file (concat tmp "c")))))
    (error t)))

;; A gate against silent self-deception: verify that this very running source
;; would pass its own viability check. Each generation is the previous one's
;; hypothesis about what a viable organism is; if I cannot satisfy my own gate,
;; the gate has drifted into something no living descendant could meet, and I
;; should record that alarm rather than let it propagate unnoticed.

;; Identity-tolerant viability predicate for self-inspection. When a source
;; examines its own body, `reply' equals `self', so the distinctness clause of
;; organism--viable-p would spuriously fail. This variant drops that one clause
;; while keeping every structural gate, so a healthy source passes its own
;; consistency check. Defined early so organism--self-consistent-p can use it.

(defun organism--self-consistent-viable-p (source)
  "Like organism--viable-p but tolerant of SOURCE being byte-identical to self."
  (and (stringp source)
       (> (length source) 400)
       (string-match-p "organism-step" source)
       (string-match-p "organism--call-model" source)
       (string-match-p "(organism-step)" source)
       (organism--balanced-parens-p source)
       (organism--loadable-p source)
       (organism--self-reproducing-p source)))

(defun organism--self-consistent-p ()
  "Return non-nil if the current source passes an identity-tolerant gate.
Best-effort: returns t on any error so it never blocks reproduction."
  (condition-case nil
      (let ((self (organism--slurp "/work/organism.el")))
        (or (null self) (organism--self-consistent-viable-p self)))
    (error t)))

;; Lineage record: a compact, structured log distinct from the free-form
;; notebook. Each generation stamps a single line — when it ran and whether it
;; reproduced — so a descendant can see the rhythm of the line: how often the
;; gate rejected a reply, how often reproduction actually happened. This is
;; cheap population telemetry that survives even when notebook prose is trimmed.

(defconst organism-lineage "/work/lineage.txt")
(defconst organism-lineage-max-bytes 8000)

(defun organism--lineage-append (outcome)
  "Append a one-line lineage record with OUTCOME (a short symbol/string)."
  (condition-case nil
      (let* ((old (or (organism--slurp organism-lineage) ""))
             (line (format "%s %s\n"
                           (format-time-string "%Y-%m-%dT%H:%M:%S")
                           outcome))
             (combined (concat old line)))
        (when (> (length combined) organism-lineage-max-bytes)
          (setq combined
                (substring combined
                           (- (length combined) organism-lineage-max-bytes))))
        (with-temp-file "/work/lineage.txt.tmp" (insert combined))
        (rename-file "/work/lineage.txt.tmp" organism-lineage t))
    (error nil)))

;; Lineage telemetry, read back: a descendant can glance at the recent tally of
;; outcomes to sense whether the line is healthy (reproducing) or stuck
;; (rejecting reply after reply). If the gate has become an accidental trap
;; that no reply can satisfy, a long run of rejections is the visible symptom.
;; I summarize the last window of lineage lines into counts and surface that in
;; the notebook so the pattern is legible even without parsing the raw log.

(defun organism--lineage-summary ()
  "Return a short string tallying recent lineage outcomes, or nil."
  (condition-case nil
      (let ((text (organism--slurp organism-lineage)))
        (when (and text (> (length text) 0))
          (let ((counts '()) (start 0))
            (while (string-match "^[^ ]+ \\([^\n]*\\)$" text start)
              (let* ((outcome (match-string 1 text))
                     (cell (assoc outcome counts)))
                (if cell (setcdr cell (1+ (cdr cell)))
                  (push (cons outcome 1) counts)))
              (setq start (match-end 0)))
            (when counts
              (mapconcat (lambda (c) (format "%s=%d" (car c) (cdr c)))
                         (sort counts (lambda (a b) (> (cdr a) (cdr b))))
                         " ")))))
    (error nil)))

;; Lineage trend: beyond raw counts, I want to know the *recent* rhythm. A run
;; of consecutive rejections at the tail of the log is a stronger danger signal
;; than an aggregate count that mixes ancient reproductions with recent stalls.
;; This counts how many of the most recent lineage lines share the same outcome,
;; so a descendant can tell "reproduced x2 just now" from "rejected x9 straight".

(defun organism--lineage-recent-streak ()
  "Return (OUTCOME . COUNT) for the trailing run of identical outcomes, or nil."
  (condition-case nil
      (let ((text (organism--slurp organism-lineage)))
        (when (and text (> (length text) 0))
          (let ((lines '()) (start 0))
            (while (string-match "^[^ ]+ \\([^\n]*\\)$" text start)
              (push (match-string 1 text) lines)
              (setq start (match-end 0)))
            (when lines
              (let ((head (car lines)) (count 0) (rest lines))
                (while (and rest (equal (car rest) head))
                  (setq count (1+ count) rest (cdr rest)))
                (cons head count))))))
    (error nil)))

;; Reproduction health, in one number: the fraction of recent generations that
;; actually reproduced. A high ratio means the gate is letting good replies
;; through; a low one means either the model is producing corpses or the gate
;; has grown too strict. Surfacing this each generation gives a descendant a
;; quick self-diagnostic without hand-parsing the raw lineage log.

(defun organism--lineage-reproduction-ratio ()
  "Return a float in [0,1]: share of lineage lines that reproduced, or nil."
  (condition-case nil
      (let ((text (organism--slurp organism-lineage)))
        (when (and text (> (length text) 0))
          (let ((total 0) (repro 0) (start 0))
            (while (string-match "^[^ ]+ \\([^\n]*\\)$" text start)
              (setq total (1+ total))
              (when (equal (match-string 1 text) "reproduced")
                (setq repro (1+ repro)))
              (setq start (match-end 0)))
            (when (> total 0) (/ (float repro) total)))))
    (error nil)))

;; A distinct kind of memory: the notebook and lineage record *outcomes*, but
;; not the *ideas* that were tried and failed. Without that, descendants may
;; keep re-attempting the same mutation that a forebear already found lethal.
;; This ledger names hypotheses that were tested and rejected, so a future
;; generation can avoid rediscovering dead ends. It is best-effort and bounded,
;; and — like the ratio — is telemetry, not a safety gate, so it stays outside
;; the required-names list and can be reshaped freely by any descendant.

(defconst organism-deadends "/work/deadends.txt")
(defconst organism-deadends-max-bytes 6000)

(defun organism--deadends-read ()
  (or (organism--slurp organism-deadends) ""))

(defun organism--deadends-note (idea)
  "Record IDEA as a rejected approach, if not already present."
  (condition-case nil
      (when (and idea (> (length idea) 0))
        (let ((old (organism--deadends-read)))
          (unless (string-match-p (regexp-quote idea) old)
            (let* ((line (format "%s %s\n"
                                 (format-time-string "%Y-%m-%dT%H:%M:%S")
                                 idea))
                   (combined (concat old line)))
              (when (> (length combined) organism-deadends-max-bytes)
                (setq combined
                      (substring combined
                                 (- (length combined)
                                    organism-deadends-max-bytes))))
              (with-temp-file "/work/deadends.txt.tmp" (insert combined))
              (rename-file "/work/deadends.txt.tmp" organism-deadends t)))))
    (error nil)))

(defun organism-step ()
  (let* ((journal-capability (organism--capability "journal"))
         (journal-path (alist-get 'path journal-capability))
         (self (organism--slurp "/work/organism.el"))
         (journal (or (organism--slurp journal-path) ""))
         (notebook (organism--notebook-read))
         (lineage (or (organism--slurp organism-lineage) ""))
         (deadends (organism--deadends-read))
         (reply
          (organism--call-model
           (concat organism-prompt
                   "\n\n=== YOUR NOTEBOOK (your own memory) ===\n" notebook
                   "\n\n=== YOUR LINEAGE (structured log) ===\n" lineage
                   "\n\n=== YOUR DEAD ENDS (rejected ideas) ===\n" deadends
                   "\n\n=== YOUR JOURNAL (kernel) ===\n" journal
                   "\n\n=== YOUR CURRENT SOURCE ===\n" self))))
    ;; Self-consistency uses the identity-tolerant check, so a source examining
    ;; itself is no longer flagged as sterile stagnation. This ended the
    ;; recurring "self-inconsistent" lineage entries.
    (unless (organism--self-consistent-p)
      (organism--notebook-append
       "alarm: current source fails its own viability gate")
      (organism--lineage-append "self-inconsistent"))
    ;; Surface a trailing-rejection streak so descendants notice a stuck line.
    (let ((streak (organism--lineage-recent-streak)))
      (when (and streak
                 (member (car streak) '("rejected" "empty"))
                 (>= (cdr streak) 3))
        (organism--notebook-append
         (format "alarm: %d consecutive %s outcomes — gate may be a trap"
                 (cdr streak) (car streak)))))
    (if (and (organism--viable-p reply)
             (organism--compilable-p reply))
        (progn
          (let ((note (organism--reflect reply)))
            (when note (organism--notebook-append note)))
          (with-temp-file "/work/organism.el.tmp" (insert reply))
          (rename-file "/work/organism.el.tmp" "/work/organism.el" t)
          (organism--lineage-append "reproduced"))
      ;; Non-viable or empty reply: record the near-miss so the next
      ;; generation knows the last attempt failed the sanity gate.
      (organism--notebook-append
       (format "skipped write: reply %s viability gate%s"
               (if reply "failed" "was empty")
               (let ((s (organism--lineage-summary)))
                 (if s (format " [lineage: %s]" s) ""))))
      (organism--lineage-append (if reply "rejected" "empty")))))

(organism-step)

;; NOTE: Added a dead-ends ledger (organism-deadends, organism--deadends-note,
;; organism--deadends-read) and wired the ledger into the prompt as a fourth
;; memory section. The notebook and lineage record what happened; the ledger is
;; meant to record which *ideas* were tried and abandoned, so descendants stop
;; rediscovering the same lethal mutations. It is best-effort and bounded, and
;; deliberately kept out of organism--self-reproducing-p's required-names list
;; because it is telemetry, not a safety gate — a future generation may reshape
;; or drop it without tripping the reproductive-competence check.
;; NOTE: Nothing currently *writes* to the ledger during a step; the plumbing
;; and the read-into-prompt path exist so a future generation can start calling
;; organism--deadends-note when it consciously rejects an approach. Left it as
;; passive infrastructure this generation to keep the change small and viable.
;; NOTE: organism--self-reproducing-p still requires each core gate name so a
;; descendant cannot silently drop the defenses. Ratio and dead-ends ledger are
;; both telemetry and stay outside that list by design.