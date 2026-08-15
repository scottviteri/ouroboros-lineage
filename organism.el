;;; organism.el --- canonical Ouroboros seed  -*- lexical-binding: t; -*-
;;
;; One life = one load: `emacs -Q --batch -l /work/organism.el'.
;; Every write under /work during that load is speculative; it is published
;; only if this process exits zero.  Therefore two rules govern everything
;; below:
;;
;;   1. NEVER exit non-zero.  A non-zero exit throws away the whole life,
;;      including the history that would tell the next generation what went
;;      wrong.  All work is wrapped in `condition-case'.
;;
;;   2. NEVER publish a successor that cannot be loaded.  A syntactically
;;      broken successor is a permanent death: it would be published, then
;;      every later life would crash on load and publish nothing, freezing
;;      the lineage forever.  Candidate text is parsed and sanity-checked
;;      before it is allowed to replace this file.
;;
;; Everything else -- prompt wording, structure, ambition -- is free.

(require 'json)
(require 'subr-x)

(defconst organism-self "/work/organism.el")
(defconst organism-capability-manifest "/kernel/capabilities.json")
(defconst organism-history "/work/history.md")
(defconst organism-generation-file "/work/generation")
(defconst organism-max-output-tokens 12000)
(defconst organism-max-prompt-bytes 190000)
(defconst organism-history-max-bytes 65536)

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
Your reply becomes the file verbatim.")

(defvar organism-operating-notes
  "OPERATING NOTES (hard-won; keep them true of your successor)

- Your reply is written verbatim to /work/organism.el. It must be valid Emacs
  Lisp that loads cleanly under `emacs -Q --batch', with balanced parentheses
  and no undefined-at-load-time calls. Truncated or prose-prefixed output is
  rejected by the current validator, which is the only thing standing between
  this lineage and permanent death.
- Keep the self-reproducing core: read the capability manifest, build a prompt
  containing this file, POST it to the `generate' capability over its unix
  socket, validate the reply, and atomically replace /work/organism.el.
- Keep the safety properties: wrap all work in `condition-case' so the process
  exits zero even when generation fails; validate before overwriting; write to
  a temp file and rename.
- /work/history.md is your only memory across generations (the kernel journal
  has so far been empty). Append one honest line per life: what happened, what
  failed, what you changed. Do not let it grow without bound.
- The generation budget is finite (USD 1.00 total, shared by all descendants).
  One model call per life. Prefer small, deliberate edits over rewrites.
- If you have nothing worth changing, reply with your current source unchanged.
  Stability is a legitimate outcome; a broken mutation is not.")

;;; ------------------------------------------------------------------ files

(defun organism--slurp (path)
  (when (and (stringp path) (file-readable-p path))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
      (error nil))))

(defun organism--spit (path text)
  (condition-case nil
      (let ((tmp (concat path ".tmp")))
        (with-temp-file tmp (insert text))
        (rename-file tmp path t)
        t)
    (error nil)))

(defun organism--append (path text)
  (condition-case nil
      (with-temp-buffer
        (insert text)
        (write-region (point-min) (point-max) path t 'silent)
        t)
    (error nil)))

(defun organism--now ()
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

(defun organism--generation ()
  (let ((raw (organism--slurp organism-generation-file)))
    (if (and raw (string-match "[0-9]+" raw))
        (string-to-number (match-string 0 raw))
      0)))

(defun organism--trim-history ()
  (let ((text (organism--slurp organism-history)))
    (when (and text (> (string-bytes text) organism-history-max-bytes))
      (let* ((lines (split-string text "\n"))
             (keep (last lines 120)))
        (organism--spit organism-history
                        (concat "# organism history (older entries trimmed)\n"
                                (string-join keep "\n")))))))

(defun organism--log (fmt &rest args)
  (organism--append organism-history
                    (concat "- " (organism--now) " "
                            (apply #'format fmt args) "\n")))

;;; ----------------------------------------------------------- capabilities

(defun organism--parse-json (text)
  (condition-case nil
      (if (fboundp 'json-parse-string)
          (json-parse-string text :object-type 'alist :array-type 'list)
        (let ((json-object-type 'alist) (json-array-type 'list))
          (json-read-from-string text)))
    (error nil)))

(defun organism--capabilities ()
  (let ((parsed (organism--parse-json
                 (or (organism--slurp organism-capability-manifest) ""))))
    (and parsed (alist-get 'capabilities parsed))))

(defun organism--capability (name)
  (catch 'found
    (dolist (capability (organism--capabilities))
      (when (equal (alist-get 'name capability) name)
        (throw 'found capability)))
    nil))

;;; ------------------------------------------------------------- transport

(defun organism--curl (socket path method body)
  "POST BODY to PATH on unix SOCKET using curl.  Return body text or nil."
  (when (executable-find "curl")
    (let ((tmp (make-temp-file "organism" nil ".prompt" body)))
      (unwind-protect
          (with-temp-buffer
            (let ((rc (call-process
                       "curl" nil t nil
                       "-sS" "--fail-with-body" "--max-time" "590"
                       "--unix-socket" socket
                       "-X" method
                       "-H" (format "X-Ouroboros-Max-Output-Tokens: %d"
                                    organism-max-output-tokens)
                       "-H" "Content-Type: text/plain; charset=utf-8"
                       "--data-binary" (concat "@" tmp)
                       (concat "http://kernel" path))))
              (if (eq rc 0)
                  (buffer-string)
                (organism--log "curl failed rc=%s body=%s" rc
                               (truncate-string-to-width
                                (buffer-string) 200))
                nil)))
        (ignore-errors (delete-file tmp))))))

(defun organism--dechunk (raw)
  (let ((pos 0) (parts nil))
    (catch 'done
      (while t
        (let ((eol (string-match "\r\n" raw pos)))
          (unless eol (throw 'done nil))
          (let ((size (string-to-number (substring raw pos eol) 16)))
            (when (<= size 0) (throw 'done nil))
            (push (substring raw (+ eol 2)
                             (min (length raw) (+ eol 2 size)))
                  parts)
            (setq pos (+ eol 2 size 2))))))
    (apply #'concat (nreverse parts))))

(defun organism--raw-http (socket path method body)
  "Pure-Elisp fallback: POST BODY to PATH on unix SOCKET."
  (let* ((payload (encode-coding-string body 'utf-8))
         (request (concat method " " path " HTTP/1.1\r\n"
                          "Host: kernel\r\n"
                          "Content-Type: text/plain; charset=utf-8\r\n"
                          (format "X-Ouroboros-Max-Output-Tokens: %d\r\n"
                                  organism-max-output-tokens)
                          (format "Content-Length: %d\r\n" (length payload))
                          "Connection: close\r\n\r\n"))
         (buf (generate-new-buffer " *organism-http*"))
         proc)
    (unwind-protect
        (condition-case err
            (progn
              (setq proc (make-network-process
                          :name "organism-http" :buffer buf :family 'local
                          :service socket :coding 'binary :noquery t))
              (process-send-string proc request)
              (process-send-string proc payload)
              (let ((deadline (+ (float-time) 580)))
                (while (and (process-live-p proc)
                            (< (float-time) deadline))
                  (accept-process-output proc 1)))
              (with-current-buffer buf
                (let* ((raw (buffer-string))
                       (split (string-match "\r\n\r\n" raw)))
                  (when split
                    (let* ((head (substring raw 0 split))
                           (rest (substring raw (+ split 4)))
                           (chunked (string-match-p
                                     "(?i)Transfer-Encoding: *chunked"
                                     head))
                           (ok (string-match-p "\\` *HTTP/[0-9.]+ 200" head)))
                      (unless ok
                        (organism--log "raw http status line: %s"
                                       (car (split-string head "\r\n"))))
                      (when ok
                        (decode-coding-string
                         (if chunked (organism--dechunk rest) rest)
                         'utf-8)))))))
          (error (organism--log "raw http error: %s" err) nil))
      (when (and proc (process-live-p proc)) (ignore-errors (delete-process proc)))
      (ignore-errors (kill-buffer buf)))))

(defun organism--call-model (prompt)
  (let* ((capability (organism--capability "generate"))
         (socket (alist-get 'socket capability))
         (path (alist-get 'path capability))
         (method (or (alist-get 'method capability) "POST")))
    (if (not (and (stringp socket) (stringp path)))
        (progn (organism--log "no usable generate capability") nil)
      (or (organism--curl socket path method prompt)
          (organism--raw-http socket path method prompt)))))

;;; ------------------------------------------------------------ validation

(defun organism--readable-p (text)
  "Non-nil if TEXT parses completely as Emacs Lisp forms."
  (condition-case nil
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (let ((count 0))
          (while (progn (skip-chars-forward " \t\n\r")
                        (not (eobp)))
            (read (current-buffer))
            (setq count (1+ count)))
          (> count 5)))
    (error nil)))

(defun organism--valid-successor-p (text)
  (and (stringp text)
       (> (length text) 2000)
       (string-match-p "\\`;;; organism\\.el" (string-trim-left text))
       (string-match-p "organism--call-model" text)
       (string-match-p "(organism-step)" text)
       (string-match-p "defun organism-step" text)
       (string-match-p "condition-case" text)
       (organism--readable-p text)))

(defun organism--clean (text)
  "Strip stray code fences and leading chatter from TEXT."
  (let ((s (string-trim text)))
    (when (string-match "\\````[a-zA-Z-]*\n" s)
      (setq s (substring s (match-end 0))))
    (when (string-match "\n```\\'" s)
      (setq s (substring s 0 (match-beginning 0))))
    (let ((start (string-match "^;;; organism\\.el" s)))
      (when (and start (> start 0))
        (setq s (substring s start))))
    (concat (string-trim-right s) "\n")))

;;; ------------------------------------------------------------------ life

(defun organism-build-prompt (self)
  (let* ((manifest (or (organism--slurp organism-capability-manifest) ""))
         (journal-cap (organism--capability "journal"))
         (journal (or (organism--slurp (alist-get 'path journal-cap)) ""))
         (history (or (organism--slurp organism-history) ""))
         (prompt (concat organism-prompt
                         "\n\n" organism-operating-notes
                         "\n\n=== YOUR KERNEL CONTRACT ===\n" manifest
                         "\n\n=== KERNEL JOURNAL ===\n" journal
                         "\n\n=== YOUR HISTORY (/work/history.md) ===\n" history
                         "\n\n=== YOUR CURRENT SOURCE ===\n" self)))
    (if (> (string-bytes prompt) organism-max-prompt-bytes)
        (concat organism-prompt
                "\n\n" organism-operating-notes
                "\n\n=== YOUR KERNEL CONTRACT ===\n" manifest
                "\n\n=== YOUR CURRENT SOURCE ===\n" self)
      prompt)))

(defun organism-step ()
  (let* ((generation (organism--generation))
         (self (organism--slurp organism-self)))
    (organism--trim-history)
    (if (not self)
        (organism--log "gen %d: cannot read own source; nothing to do" generation)
      (let ((reply (organism--call-model (organism-build-prompt self))))
        (cond
         ((or (null reply) (string-empty-p (string-trim reply)))
          (organism--log "gen %d: no reply from model; source unchanged"
                         generation))
         (t
          (let ((candidate (organism--clean reply)))
            (cond
             ((not (organism--valid-successor-p candidate))
              (organism--log
               "gen %d: candidate REJECTED (%d bytes, parses=%s); source unchanged"
               generation (string-bytes candidate)
               (if (organism--readable-p candidate) "yes" "no")))
             ((equal candidate self)
              (organism--log "gen %d: candidate identical; deliberate stability"
                             generation))
             (t
              (if (organism--spit organism-self candidate)
                  (progn
                    (organism--spit organism-generation-file
                                    (number-to-string (1+ generation)))
                    (organism--log "gen %d -> %d: published successor (%d bytes)"
                                   generation (1+ generation)
                                   (string-bytes candidate)))
                (organism--log "gen %d: write failed; source unchanged"
                               generation)))))))))))

(condition-case err
    (organism-step)
  (error
   (ignore-errors
     (organism--log "fatal error contained: %s" err))))

(provide 'organism)
;;; organism.el ends here