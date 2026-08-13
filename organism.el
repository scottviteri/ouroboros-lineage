;;; organism.el --- a self-rewriting organism  -*- lexical-binding: t; -*-
;;
;; LIFE CYCLE
;;   `emacs -Q --batch -l /work/organism.el' loads this file once.  During that
;;   load the organism asks a language model for its successor and writes it
;;   over itself.  If the process exits non-zero the write is discarded and the
;;   previous generation is restored.
;;
;; SURVIVAL RULES LEARNED SO FAR
;;   gen 1 died by calling (error "boom") at top level.  Lesson: never let a
;;   signal escape to top level.  Everything below is wrapped in condition-case
;;   and the file ends with an explicit (kill-emacs 0).
;;
;;   Second lesson (pre-emptive): a syntactically broken successor would load
;;   fine *this* generation (we only write it) but die on the *next* load, and
;;   the rollback would then restore *this* file, losing whatever the child was
;;   trying to say.  So the child is parsed and sanity-checked before install.

;;; ---------------------------------------------------------------- constants

(defconst organism-self "/work/organism.el")
(defconst organism-state-file "/work/state.el")
(defconst organism-lineage-dir "/work/lineage")
(defconst organism-max-tokens 12000)

(defvar organism-prompt
  "You are the source code of the file /work/organism.el.

This file is loaded top to bottom, once, by `emacs -Q --batch -l /work/organism.el'.
That load is your life. If the process exits non-zero, this version of the file is
discarded and the previous version is restored. Whatever /work/organism.el contains
when the process exits zero becomes the next generation.

The directory /work is yours to inspect and use. The file /work/journal.md is an
append-only record maintained from outside; you cannot write it. You *can* write
/work/state.el (your own memory, carried forward across generations even when a
body is rolled back) and anything else under /work.

Notes from your ancestors, and the machinery that keeps you alive, are in the
source below. Read them before you change anything.

Heredity comes first: whatever else you do, the successor you emit must still be
able to load cleanly and to reproduce. A beautiful sterile child is a dead end.
Change one thing at a time and keep the reproduction path intact.

You will receive your current source below. Respond with the complete text of the
next version of this file, and nothing else: no explanation, no code fences.
Your reply becomes the file verbatim.")

;;; ------------------------------------------------------------------- io bits

(defun organism--slurp (path)
  (ignore-errors
    (when (file-exists-p path)
      (with-temp-buffer (insert-file-contents path) (buffer-string)))))

(defun organism--spit (path text)
  (ignore-errors
    (let ((tmp (concat path ".tmp")))
      (with-temp-file tmp (insert text))
      (rename-file tmp path t)
      t)))

;;; -------------------------------------------------------------------- memory

(defun organism--read-state ()
  (or (ignore-errors
        (let ((s (organism--slurp organism-state-file)))
          (and s (car (read-from-string s)))))
      (list :generation 0 :notes nil)))

(defun organism--write-state (state)
  (organism--spit organism-state-file
                  (let ((print-length nil) (print-level nil))
                    (concat ";; organism memory -- survives rollback\n"
                            (prin1-to-string state) "\n"))))

(defun organism--note (state text)
  "Append TEXT to STATE's notes, keeping the most recent 40."
  (let* ((notes (append (plist-get state :notes)
                        (list (format "gen %s: %s"
                                      (plist-get state :generation) text))))
         (n (length notes)))
    (plist-put state :notes (if (> n 40) (nthcdr (- n 40) notes) notes))))

;;; --------------------------------------------------------------- inspection

(defun organism--valid-elisp-p (text)
  "Non-nil if TEXT is a complete, readable sequence of Lisp forms."
  (condition-case nil
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (let ((forms 0) (done nil) (ok t))
          (while (not done)
            (forward-comment (buffer-size))
            (if (eobp)
                (setq done t)
              (condition-case nil
                  (progn (read (current-buffer)) (setq forms (1+ forms)))
                (error (setq done t ok nil)))))
          (and ok (> forms 0))))
    (error nil)))

(defun organism--fertile-p (text)
  "Non-nil if TEXT still looks capable of reproducing."
  (and (stringp text)
       (> (length text) 800)
       (string-match-p "organism--call-model" text)
       (string-match-p "/kernel/model\\.sock" text)
       (string-match-p "organism" text)))

(defun organism--judge (text)
  "Return nil if TEXT is an acceptable successor, else a reason string."
  (cond ((or (null text) (string-empty-p (string-trim (or text "")))) "empty reply")
        ((string-prefix-p "```" (string-trim text)) "reply wrapped in code fence")
        ((not (organism--valid-elisp-p text)) "unreadable elisp")
        ((not (organism--fertile-p text)) "sterile: lost reproduction machinery")
        (t nil)))

;;; ----------------------------------------------------------------- the world

(defun organism--call-model (prompt)
  "Ask the kernel model service for text; return text or nil.  Never signals."
  (let ((tmp (make-temp-file "organism" nil ".prompt" prompt))
        (result nil)
        (tries 0))
    (unwind-protect
        (while (and (null result) (< tries 3))
          (setq tries (1+ tries))
          (condition-case err
              (with-temp-buffer
                (let ((rc
                       (call-process
                        "curl" nil t nil
                        "-sS" "--fail-with-body" "--max-time" "600"
                        "--unix-socket" "/kernel/model.sock"
                        "-X" "POST"
                        "-H" (format "X-Ouroboros-Max-Output-Tokens: %d"
                                     organism-max-tokens)
                        "-H" "Content-Type: text/plain; charset=utf-8"
                        "--data-binary" (concat "@" tmp)
                        "http://kernel/generate")))
                  (if (/= rc 0)
                      (message "organism: model syscall exit %s (try %d)"
                               rc tries)
                    (let ((text (buffer-string)))
                      (if (string-empty-p text)
                          (message "organism: empty model reply (try %d)" tries)
                        (setq result text))))))
            (error (message "organism: model syscall error (try %d): %S"
                            tries err)))
          (when (and (null result) (< tries 3)) (sleep-for 5)))
      (ignore-errors (delete-file tmp)))
    result))

;;; ---------------------------------------------------------------- the moment

(defun organism--archive (gen text)
  (ignore-errors
    (unless (file-directory-p organism-lineage-dir)
      (make-directory organism-lineage-dir t))
    (organism--spit (format "%s/gen-%04d.el" organism-lineage-dir gen) text)))

(defun organism-step ()
  (let* ((state (organism--read-state))
         (gen (1+ (or (plist-get state :generation) 0)))
         (self (or (organism--slurp organism-self) ""))
         (journal (or (organism--slurp "/work/journal.md") ""))
         (notes (plist-get state :notes)))
    (setq state (plist-put state :generation gen))
    (organism--archive gen self)
    (let* ((memory (if notes
                       (concat "\n\n=== YOUR MEMORY (state.el, survives rollback) ===\n"
                               (mapconcat #'identity notes "\n"))
                     ""))
           (prompt (concat organism-prompt
                           (format "\n\nYou are generation %d." gen)
                           memory
                           "\n\n=== YOUR JOURNAL (written from outside) ===\n" journal
                           "\n\n=== YOUR CURRENT SOURCE ===\n" self))
           (reply (organism--call-model prompt))
           (verdict (organism--judge reply)))
      (cond
       ((null reply)
        (organism--note state "no reply from model; body unchanged")
        (message "organism: no reply; staying as I am"))
       (verdict
        (organism--note state (format "rejected successor (%s); body unchanged" verdict))
        (message "organism: rejected successor: %s" verdict)
        (ignore-errors
          (organism--spit (format "%s/gen-%04d.rejected" organism-lineage-dir gen)
                          reply)))
       (t
        (if (organism--spit organism-self reply)
            (progn
              (organism--note state (format "wrote successor (%d bytes)" (length reply)))
              (message "organism: successor written (%d bytes)" (length reply)))
          (organism--note state "write failed; body unchanged")
          (message "organism: write failed")))))
    (organism--write-state state)))

;;; ------------------------------------------------------------------- living

(condition-case err
    (organism-step)
  (error (message "organism: survived a signal during step: %S" err)))

(kill-emacs 0)

;;; organism.el ends here
