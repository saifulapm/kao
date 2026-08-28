;;; kao-bench.el --- Batch benchmark harness -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `make bench' — batch timing of the hot paths the HARD perf rule guards
;; (native Emacs speed; allocation-light
;; keystrokes).  Four benches:
;;
;;   (a) single-cursor motion keystroke pipeline (command + post-command hooks)
;;   (b) 1k-cursor motion (the pooled-overlay render path)
;;   (c) N-command session history cost — flatness ratio guards against a
;;       return to O(history) growth (gc)
;;   (d) insert-state typing throughput vs the same typing with `kao-mode' off
;;
;; Each bench takes an optional REPS argument and returns a plist, so the smoke
;; tests in kao-bench-tests.el can run the same code at tiny scale.  Numbers
;; printed by `kao-bench-run' are observational (no thresholds here — the
;; smoke test pins only the flatness ratio, the one regression with a clear
;; asymptotic signature).

;;; Code:

(require 'kao)
(require 'benchmark)
(require 'cl-lib)
(require 'kao-treesit nil t)            ; optional: the treesit bench (g)

(defmacro kao-bench--with-buffer (content &rest body)
  "Run BODY in a `kao-mode' temp buffer containing CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (buffer-enable-undo)
     (kao-mode 1)
     (unwind-protect
         (progn
           (setq kao--sels (kao-sels-make
                            :list (list (kao-sel-make :anchor 1 :cursor 1))
                            :main 0))
           ,@body)
       (kao-mode -1))))

(defun kao-bench--command (cmd)
  "Run CMD through the keystroke pipeline (pre/post-command hooks included).
`inhibit-message' silences echo-area noise (batch carries eldoc's global
pre-command hook, which otherwise prints a blank line per keystroke)."
  (let ((this-command cmd)
        (inhibit-message t))
    (run-hooks 'pre-command-hook)
    (funcall cmd)
    (run-hooks 'post-command-hook)))

(defun kao-bench--us (spec reps)
  "Per-op µs from a `benchmark-run' SPEC (TOTAL GCS GC-TIME) over REPS."
  (/ (* (nth 0 spec) 1e6) reps))

(defun kao-bench--us-sans-gc (spec reps)
  "Per-op µs from SPEC with gc time subtracted (for flatness ratios)."
  (/ (* (- (nth 0 spec) (nth 2 spec)) 1e6) reps))

(defmacro kao-bench--with-line-cursors (&rest body)
  "Run BODY in a `kao-mode' buffer holding ~1k cursors, one per line.
The fixture shared by the 1k-cursor benches: 1000 `\"xxxxxxxxxx\"'
lines, then `kao-select-whole-buffer' + `kao-split-lines' place one cursor on
each line (~1000 secondaries).  BODY runs with those cursors installed in
`kao--sels' and supplies each bench's distinct measurement around the common
core."
  (declare (indent 0))
  `(kao-bench--with-buffer
       (mapconcat #'identity (make-list 1000 "xxxxxxxxxx") "\n")
     (kao-bench--command #'kao-select-whole-buffer)
     (kao-bench--command #'kao-split-lines)
     ,@body))

;;;; (a) single-cursor motion

(defun kao-bench-motion-single (&optional reps)
  "Time REPS alternating l/h keystrokes on a single cursor.
Returns (:us-per-op N :reps REPS)."
  (let ((reps (or reps 20000)))
    (kao-bench--with-buffer (make-string 200 ?x)
      (dotimes (_ 64)                                       ; warmup
        (kao-bench--command #'kao-right)
        (kao-bench--command #'kao-left))
      (let* ((i 0)
             (spec (benchmark-run reps
                     (kao-bench--command
                      (if (cl-evenp (setq i (1+ i))) #'kao-left #'kao-right)))))
        (list :us-per-op (kao-bench--us spec reps)
              :us-sans-gc (kao-bench--us-sans-gc spec reps) :reps reps)))))

;;;; (b) 1k-cursor motion

(defun kao-bench-motion-1k-cursors (&optional reps)
  "Time REPS alternating l/h keystrokes with ~1k cursors (one per line).
In batch the render falls back to whole-buffer bounds, so every keystroke
re-renders every secondary overlay — the worst case for the pool.
Returns (:us-per-op N :reps REPS :cursors N)."
  (let ((reps (or reps 200)))
    (kao-bench--with-line-cursors
      (let ((cursors (length (kao-sels-list kao--sels))))
        (dotimes (_ 4)                                      ; warmup
          (kao-bench--command #'kao-right)
          (kao-bench--command #'kao-left))
        (let* ((i 0)
               (spec (benchmark-run reps
                       (kao-bench--command
                        (if (cl-evenp (setq i (1+ i))) #'kao-left #'kao-right)))))
          (list :us-per-op (kao-bench--us spec reps)
                :us-sans-gc (kao-bench--us-sans-gc spec reps) :reps reps
                :cursors cursors))))))

;;;; (b') 1k-cursor motion, windowed bounds

(defun kao-bench--viewport-bounds ()
  "Simulated 40-line viewport around point, the live-frame render geometry.
Stands in for `kao--window-bounds' in the windowed bench: batch has no
window, so the real function falls back to the whole buffer and the batch
1k-cursor bench renders EVERY secondary.  This is what a user's window
actually hands the render."
  (save-excursion
    (cons (progn (forward-line -20) (point))
          (progn (forward-line 40) (point)))))

(defun kao-bench-motion-1k-cursors-windowed (&optional reps)
  "Time REPS alternating l/h keystrokes with ~1k cursors, viewport-clipped.
Identical to `kao-bench-motion-1k-cursors' except `kao--window-bounds' is
replaced by a simulated 40-line viewport around point, so the render touches
only the secondaries a user would see, \"what users feel\".
Returns (:us-per-op N :reps REPS :cursors N :visible N), where :visible is
the number of secondaries whose body intersects the viewport."
  (let ((reps (or reps 200)))
    (kao-bench--with-line-cursors
      ;;: the render clips through `kao--window-ranges'
      ;; (a list of per-window (WBEG . WEND) bounds), so the windowed bench
      ;; overrides THAT — the new override point — wrapping the simulated
      ;; viewport in a one-window list.  `kao--window-bounds' stays the helper
      ;; `kao-bench--viewport-bounds' stands in for.
      (cl-letf (((symbol-function 'kao--window-ranges)
                 (lambda () (list (kao-bench--viewport-bounds)))))
        (let ((cursors (length (kao-sels-list kao--sels))))
          (dotimes (_ 4)                                    ; warmup
            (kao-bench--command #'kao-right)
            (kao-bench--command #'kao-left))
          (let* ((bounds (kao-bench--viewport-bounds))
                 (main-idx (kao-sels-main kao--sels))
                 (idx -1)
                 (visible (cl-count-if
                           (lambda (sel)
                             (setq idx (1+ idx))
                             (and (/= idx main-idx)
                                  (< (kao-sel-beg sel) (cdr bounds))
                                  (> (kao-sel-end sel) (car bounds))))
                           (kao-sels-list kao--sels)))
                 (i 0)
                 (spec (benchmark-run reps
                         (kao-bench--command
                          (if (cl-evenp (setq i (1+ i))) #'kao-left #'kao-right)))))
            (list :us-per-op (kao-bench--us spec reps)
                  :us-sans-gc (kao-bench--us-sans-gc spec reps) :reps reps
                  :cursors cursors :visible visible)))))))

;;;; (c) session history cost + flatness

(defun kao-bench--history-type-x ()
  "Insert one char; the history bench's stand-in editing command.
A NAMED `kao-'-prefixed function, NOT a lambda: `this-command' must
classify as a kao command or the foreign-command catch-all adds a
collapse + an extra node per op and the row stops measuring the bare
history pipeline (a real editing keystroke runs a kao command)."
  (insert "x"))

(defun kao-bench-history (&optional reps)
  "Time REPS editing commands, each committing one buffer-history node.
Each iteration runs the full pipeline (change capture, history commit via the
post-command hook, selection-history record, render).  The flatness RATIO is
second-half per-op over first-half per-op, gc time excluded: O(1) history
stays near 1.0; an O(history) regression grows with REPS.
Returns (:us-per-op N :reps REPS :ratio R)."
  (let* ((reps (or reps 10000))
         (half (max 1 (/ reps 2)))
         (type-x #'kao-bench--history-type-x))
    (kao-bench--with-buffer ""
      (dotimes (_ 64) (kao-bench--command type-x))          ; warmup
      (let* ((s1 (benchmark-run half (kao-bench--command type-x)))
             (s2 (benchmark-run half (kao-bench--command type-x)))
             (u1 (kao-bench--us-sans-gc s1 half))
             (u2 (kao-bench--us-sans-gc s2 half)))
        (list :us-per-op (/ (+ (kao-bench--us s1 half) (kao-bench--us s2 half)) 2)
              :reps reps
              :ratio (/ u2 u1))))))

;;;; (d) insert-state typing throughput

(defun kao-bench--type-chars (reps)
  "Simulate typing REPS `a' chars through the keystroke pipeline."
  (let ((last-command-event ?a)
        (inhibit-message t))                  ; see `kao-bench--command'
    (dotimes (_ reps)
      (let ((this-command #'self-insert-command))
        (run-hooks 'pre-command-hook)
        (self-insert-command 1)
        (run-hooks 'post-command-hook)))))

(defun kao-bench-insert-throughput (&optional reps)
  "Time REPS self-inserting keystrokes in kao insert state vs `kao-mode' off.
Returns (:kao-us N :native-us N :ratio R :reps REPS) — RATIO is the kao
overhead factor over the same typing in a plain `fundamental-mode' buffer."
  (let* ((reps (or reps 20000))
         (kao-spec
          (kao-bench--with-buffer ""
            (kao-insert)
            (kao-bench--type-chars 64)                      ; warmup
            (prog1 (benchmark-run 1 (kao-bench--type-chars reps))
              (kao-insert-exit))))
         (native-spec
          (with-temp-buffer
            (buffer-enable-undo)
            (kao-bench--type-chars 64)                      ; warmup
            (benchmark-run 1 (kao-bench--type-chars reps))))
         (kao-us (kao-bench--us kao-spec reps))
         (native-us (kao-bench--us native-spec reps)))
    (list :kao-us kao-us :native-us native-us
          :ratio (/ kao-us native-us) :reps reps)))

;;;; (e) copy-lines flatness in buffer height

(defun kao-bench--copy-up-at-bottom (height reps)
  "Per-op µs of `<a-C>' (`kao-copy-selections-up') on a HEIGHT-line buffer.
The sole selection sits on the LAST line; each op copies it up one line.
`kao--line-bounds-rel' resolves the target line by a RELATIVE
`forward-line', so the cost is independent of HEIGHT — the old absolute walk
from `point-min' was O(HEIGHT).  Each rep resets the selection first (a copy
grows the list); that reset is O(1) in HEIGHT, so the across-height ratio
isolates the copy's height dependence.  GC-excluded µs (flatness ratio)."
  (kao-bench--with-buffer
      (mapconcat #'identity (make-list height "xxxxxxxxxx") "\n")
    (let* ((bottom (save-excursion (goto-char (point-max))
                                   (line-beginning-position)))
           (fresh (lambda ()
                    (setq kao--sels
                          (kao-sels-make
                           :list (list (kao-sel-make :anchor bottom
                                                     :cursor bottom))
                           :main 0)))))
      (dotimes (_ 16) (funcall fresh) (kao-copy-selections-up))   ; warmup
      (kao-bench--us-sans-gc
       (benchmark-run reps (funcall fresh) (kao-copy-selections-up))
       reps))))

(defun kao-bench-copy-lines (&optional reps)
  "Time `<a-C>' at two buffer heights; per-op cost must be FLAT in height.
Returns (:us-per-op N :tall-us N :short-us N :ratio R :reps REPS) — RATIO is
tall-over-short, ~1 for the O(1) relative walk, growing with height if
the absolute `point-min' walk ever returns."
  (let* ((reps (or reps 2000))
         (short (kao-bench--copy-up-at-bottom 200 reps))
         (tall (kao-bench--copy-up-at-bottom 20000 reps)))
    (list :us-per-op tall :tall-us tall :short-us short
          :ratio (/ tall short) :reps reps)))

;;;; (f) pipe env laziness in selection count

(defun kao-bench--pipe-env-us (sel-count reps)
  "Per-op µs of building the pipe env for `true' over SEL-COUNT selections.
`true' names no `kak_*' var, so `kao--pipe-environment' returns nil after
one cmdline scan without touching the selections — cost independent of
SEL-COUNT.  The earlier unconditional build walked every selection ×4 copies.
GC-excluded µs (flatness ratio)."
  (kao-bench--with-buffer
      (mapconcat #'identity (make-list sel-count "xxxxxxxxxx") "\n")
    (kao-bench--command #'kao-select-whole-buffer)
    (kao-bench--command #'kao-split-lines)
    (dotimes (_ 64) (kao--pipe-environment "true"))              ; warmup
    (kao-bench--us-sans-gc
     (benchmark-run reps (kao--pipe-environment "true")) reps)))

(defun kao-bench-pipe (&optional reps)
  "Time `|true' env construction at two selection counts; cost must be FLAT.
Returns (:us-per-op N :many-us N :few-us N :ratio R :sels N :reps REPS) — the
lazy env makes RATIO ~1; the old unconditional build grew with the count."
  (let* ((reps (or reps 5000))
         (few (kao-bench--pipe-env-us 50 reps))
         (many (kao-bench--pipe-env-us 2000 reps)))
    (list :us-per-op many :many-us many :few-us few
          :ratio (/ many few) :sels 2000 :reps reps)))

;;;; (g) treesit command-scoped query hoist

(defun kao-bench--treesit-available-p ()
  "Non-nil when the bash grammar is installed (the treesit bench's fixture lang)."
  (and (fboundp 'treesit-available-p) (treesit-available-p)
       (fboundp 'treesit-language-available-p)
       (treesit-language-available-p 'bash)
       (fboundp 'bash-ts-mode)
       (fboundp 'kao-treesit-select-function-inside)
       (fboundp 'kao-treesit-goto-next-function)))

(defun kao-bench-treesit (&optional reps)
  "Time `<a-i>f' and `SPC t n' with ~100 cursors in a large bash fixture.
hoists the tree query to command scope and memoizes object-at, so the query
runs about once per command regardless of cursor count.  Returns
\(:available nil) unless the bash grammar is installed; otherwise
\(:available t :select-us N :goto-us N :cursors N :reps REPS)."
  (let ((reps (or reps 50)))
    (if (not (kao-bench--treesit-available-p))
        (list :available nil)
      (let ((dir (make-temp-file "kao-bench-ts" t)))
        (unwind-protect
            (progn
              (make-directory (expand-file-name "bash" dir) t)
              (with-temp-file (expand-file-name "bash/textobjects.scm" dir)
                (insert "(function_definition body: (_) @function.inside)"
                        " @function.around\n"))
              (let ((kao-treesit-queries-dir (list dir))
                    (src (mapconcat (lambda (i)
                                      (format "f%d() {\n  echo %d\n}\n" i i))
                                    (number-sequence 1 100) "")))
                (kao-treesit-reload-queries)
                (with-temp-buffer
                  (insert src)
                  ;; Create the parser directly (as the treesit tests do):
                  ;; `bash-ts-mode' falls back to non-treesit sh-mode in a batch
                  ;; temp buffer, leaving `treesit-parser-list' empty so
                  ;; `kao-treesit-ready-p' fails and the object no-ops.
                  (treesit-parser-create 'bash)
                  (kao-mode 1)
                  (unwind-protect
                      (let* ((points
                              (let (ps)
                                (save-excursion
                                  (goto-char (point-min))
                                  (while (re-search-forward "echo" nil t)
                                    (push (match-beginning 0) ps)))
                                (nreverse ps)))
                             (fresh
                              (lambda ()
                                (setq kao--sels
                                      (kao-sels-make
                                       :list (mapcar
                                              (lambda (p)
                                                (kao-sel-make :anchor p :cursor p))
                                              points)
                                       :main 0))))
                             (cursors (length points)))
                        (dotimes (_ 4)                          ; warmup both
                          (funcall fresh) (kao-treesit-select-function-inside)
                          (funcall fresh) (kao-treesit-goto-next-function))
                        (list :available t
                              :select-us
                              (kao-bench--us
                               (benchmark-run reps
                                 (funcall fresh)
                                 (kao-treesit-select-function-inside))
                               reps)
                              :goto-us
                              (kao-bench--us
                               (benchmark-run reps
                                 (funcall fresh)
                                 (kao-treesit-goto-next-function))
                               reps)
                              :cursors cursors :reps reps))
                    (kao-mode -1)))))
          (delete-directory dir t))))))

;;;; (h) search-match highlight memo — a repeated `n' is free

(defun kao-bench-search-highlight (&optional reps)
  "Time the search-match highlight scan then REPS memoized repeats.
`kao-search--hl-set' short-circuits when the pattern and buffer are unchanged
and overlays are live, so a repeated `n' press pays ~0.  A single sparse `zebra'
in a large buffer (the scan walks the whole buffer once).  Returns
\(:first-us N :repeat-us N :ratio R :reps REPS) — RATIO (repeat/first) « 1."
  (let ((reps (or reps 500)))
    (kao-bench--with-buffer
        (concat (make-string 200000 ?x) "\nzebra\n" (make-string 200000 ?x))
      (let* ((first (kao-bench--us-sans-gc
                     (benchmark-run 1 (kao-search--hl-set "zebra")) 1))
             ;; memo now set + overlays live: every repeat short-circuits.
             (repeat (kao-bench--us-sans-gc
                      (benchmark-run reps (kao-search--hl-set "zebra")) reps)))
        (list :first-us first :repeat-us repeat
              :ratio (/ repeat first) :reps reps)))))

;;;; (i) incsearch preview step

(defun kao-bench-incsearch (&optional reps)
  "Time one incsearch preview step over a large buffer.
Each step restores the originals then re-applies the regex via an unchanged
*-apply fn (`kao--select-regex-apply').  Returns (:us-per-op N :reps REPS)."
  (let ((reps (or reps 300)))
    (kao-bench--with-buffer
        (concat (make-string 100000 ?x) "\nzebra\n" (make-string 100000 ?x))
      (kao-bench--command #'kao-select-whole-buffer)
      (let ((orig (kao--snapshot-sels kao--sels)))
        (dotimes (_ 16)                                          ; warmup
          (kao--incsearch-preview #'kao--select-regex-apply orig "zebra"))
        (list :us-per-op
              (kao-bench--us
               (benchmark-run reps
                 (kao--incsearch-preview #'kao--select-regex-apply orig "zebra"))
               reps)
              :reps reps)))))

;;;; (j) find / match worst-case scans

(defun kao-bench-find-match (&optional reps)
  "Time a MISSED `f' and an UNBALANCED `m' scan over a large buffer.
moved both scans to C-level search; a miss / unbalanced pair walks to the
buffer edge — the worst case.  `f' hunts a char absent from the buffer; `m'
starts before an unbalanced run of `{'.  Returns
\(:find-us N :match-us N :reps REPS)."
  (let ((reps (or reps 2000)))
    (kao-bench--with-buffer
        (concat (make-string 100000 ?x) (make-string 20000 ?{))
      (let ((find-us
             (progn (dotimes (_ 16) (kao-find--forward 1 ?Z 1 t))  ; ?Z absent
                    (kao-bench--us
                     (benchmark-run reps (kao-find--forward 1 ?Z 1 t)) reps)))
            (match-us
             (progn (dotimes (_ 16) (kao-match--selector 1 t))
                    (kao-bench--us
                     (benchmark-run reps (kao-match--selector 1 t)) reps))))
        (list :find-us find-us :match-us match-us :reps reps)))))

;;;; (k) live-list fold only on foreign edits

(defun kao-bench-foreign-sync (&optional reps)
  "Count live-list folds over plain vs foreign-edited keys.
kao keeps selections as integers at rest; a buffer edit made OUTSIDE a kao
command (a timer / process filter / async formatter) advances the history tree
but never reconciles `kao--sels'.  `kao--sels-sync' (a post-command hook) folds
the whole list through the tree ONLY when its tag `kao--sels-id' has fallen
behind — an integer `/=' compare on the hot path, the O(N)
`kao--snapshot-update' fold paid once per real foreign edit, never per
keystroke.  Instruments the fold
over ~1k cursors: `:folds-plain' plain keystrokes fold 0 times, `:folds-foreign'
one keystroke after a foreign insert folds exactly once; `:us-per-op' is the
hot-path `kao--sels-sync' fast-path cost (id current, no fold), flat in cursor
count.  Returns (:us-per-op N :us-sans-gc N :reps REPS :cursors N
:folds-plain N :folds-foreign N)."
  (let ((reps (or reps 5000)))
    (kao-bench--with-line-cursors
      (let* ((cursors (length (kao-sels-list kao--sels)))
             (folds 0)
             (orig (symbol-function 'kao--snapshot-update)))
        (cl-letf (((symbol-function 'kao--snapshot-update)
                   (lambda (&rest a) (cl-incf folds) (apply orig a))))
          ;; (i) plain keystrokes: id stays current, the sync never folds.
          (dotimes (_ 8)
            (kao-bench--command #'kao-right)
            (kao-bench--command #'kao-left))
          (let ((folds-plain folds))
            ;; (ii) one FOREIGN (non-command) edit, then a keystroke: the
            ;; post-command commit advances the tree and the sync folds ONCE.
            (setq folds 0)
            (save-excursion (goto-char (point-min)) (insert "Z"))
            (kao-bench--command #'kao-right)
            (let ((folds-foreign folds))
              ;; hot-path timing: the sync fast path (id re-pinned, no fold).
              (dotimes (_ 64) (kao--sels-sync))                 ; warmup
              (let ((spec (benchmark-run reps (kao--sels-sync))))
                (list :us-per-op (kao-bench--us spec reps)
                      :us-sans-gc (kao-bench--us-sans-gc spec reps)
                      :reps reps :cursors cursors
                      :folds-plain folds-plain
                      :folds-foreign folds-foreign)))))))))

;;;; (l) doubled per-key refresh deduped to one render

(defun kao-bench-refresh-dedup (&optional reps)
  "One keystroke renders ONCE though `kao--refresh' fires twice.
`kao--refresh' is called mid-command by the motion's selection installer and
again from `post-command-hook', over the same frame.  The
`(buffer-chars-modified-tick . kao--sels)' guard in `kao--refresh-key' collapses
the second call, so one keystroke walks / mirrors / renders ONCE, not twice.
`:renders-per-key' counts `kao--render' calls across a single keystroke (1 with
the guard, 2 without); `:us-per-op' is the deduped per-keystroke cost over ~1k
cursors.  Returns (:us-per-op N :us-sans-gc N :reps REPS :cursors N
:renders-per-key N)."
  (let ((reps (or reps 200)))
    (kao-bench--with-line-cursors
      (let* ((cursors (length (kao-sels-list kao--sels)))
             (renders 0)
             (orig (symbol-function 'kao--render)))
        (dotimes (_ 4)                                          ; warmup
          (kao-bench--command #'kao-right)
          (kao-bench--command #'kao-left))
        ;; render count over ONE keystroke: refresh fires twice, render once.
        (cl-letf (((symbol-function 'kao--render)
                   (lambda (&rest a) (cl-incf renders) (apply orig a))))
          (kao-bench--command #'kao-right))
        (let* ((renders-per-key renders)
               (i 0)
               (spec (benchmark-run reps
                       (kao-bench--command
                        (if (cl-evenp (setq i (1+ i))) #'kao-left #'kao-right)))))
          (list :us-per-op (kao-bench--us spec reps)
                :us-sans-gc (kao-bench--us-sans-gc spec reps)
                :reps reps :cursors cursors
                :renders-per-key renders-per-key))))))

;;;; (m) object-pending treesit dispatch memo — one query per container

(defun kao-bench-treesit-object-memo (&optional reps)
  "Object-pending `<a-i>f' over ~100 cursors sharing one container.
Without the memo the object-pending dispatch pays one tree query PER cursor;
`kao-treesit--object-dispatch-memo' (registered by `kao-treesit-setup' on the
`kao--object-dispatch-context-functions' seam) binds a command-scoped captures
memo around the whole multi-cursor pass, so ~100 cursors sharing ONE bash
function query `kao-treesit--matches-compute' ONCE.  `:queries' counts that
compute for a single `<a-i>f' pass (1 with the memo, = cursor count without),
flat in cursor count; `:us-per-op' times the pass.  Returns (:available nil)
unless the bash grammar is installed; otherwise (:available t :us-per-op N
:us-sans-gc N :reps REPS :cursors N :queries N)."
  (let ((reps (or reps 50)))
    (if (not (kao-bench--treesit-available-p))
        (list :available nil)
      (let ((dir (make-temp-file "kao-bench-ts-memo" t)))
        (unwind-protect
            (progn
              (make-directory (expand-file-name "bash" dir) t)
              (with-temp-file (expand-file-name "bash/textobjects.scm" dir)
                (insert "(function_definition body: (_) @function.inside)"
                        " @function.around\n"))
              (let ((kao-treesit-queries-dir (list dir))
                    ;; Contain the global registrations `kao-treesit-setup' makes:
                    ;; the object runtime, the dispatch-context hook, and the two
                    ;; maps it binds `t' / `M-RET' into (test's
                    ;; same containment) — the bench must not leak them.
                    (kao--object-runtime-table nil)
                    (kao--object-runtime-info nil)
                    (kao--object-runtime-nested nil)
                    (kao--object-dispatch-context-functions nil)
                    (kao-user-map (copy-keymap kao-user-map))
                    (kao-normal-state-map (copy-keymap kao-normal-state-map))
                    ;; ONE bash function whose body holds ~100 cursors (a shared
                    ;; container): every `<a-i>f' resolves the SAME container.
                    (src (concat "f() {\n"
                                 (mapconcat (lambda (i) (format "  echo %d\n" i))
                                            (number-sequence 1 100) "")
                                 "}\n")))
                (kao-treesit-reload-queries)
                (kao-treesit-setup t)             ; registers the `f' object + memo
                (with-temp-buffer
                  (insert src)
                  ;; Create the parser directly: `bash-ts-mode' falls back to
                  ;; non-treesit sh-mode in a batch temp buffer (row).
                  (treesit-parser-create 'bash)
                  (kao-mode 1)
                  (unwind-protect
                      (let* ((points
                              (let (ps)
                                (save-excursion
                                  (goto-char (point-min))
                                  (while (re-search-forward "echo" nil t)
                                    (push (match-beginning 0) ps)))
                                (nreverse ps)))
                             (cursors (length points))
                             (fresh
                              (lambda ()
                                (setq kao--sels
                                      (kao-sels-make
                                       :list (mapcar
                                              (lambda (p)
                                                (kao-sel-make :anchor p :cursor p))
                                              points)
                                       :main 0))))
                             (orig (symbol-function 'kao-treesit--matches-compute))
                             (queries 0))
                        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?f)))
                          ;; count the compute for ONE pass, then warm + time.
                          (cl-letf (((symbol-function 'kao-treesit--matches-compute)
                                     (lambda (&rest a)
                                       (cl-incf queries) (apply orig a))))
                            (funcall fresh)
                            (kao-select-inner))
                          (dotimes (_ 4) (funcall fresh) (kao-select-inner)) ; warmup
                          (let ((spec (benchmark-run reps
                                        (funcall fresh) (kao-select-inner))))
                            (list :available t
                                  :us-per-op (kao-bench--us spec reps)
                                  :us-sans-gc (kao-bench--us-sans-gc spec reps)
                                  :reps reps :cursors cursors
                                  :queries queries))))
                    (kao-mode -1)))))
          (delete-directory dir t))))))

;;;; Per-stage attribution

;; `make bench-attr' — re-runs the benches with one pipeline stage no-opped at
;; a time; Δ = baseline − stage-off is that stage's per-keystroke cost.  Pure
;; measurement (no thresholds, no production-code change); the numbers decide
;; which optimization slices are worth scheduling ("profile before
;; optimizing" — lesson was that the assumed-hot path measured 0%).
;;
;; Attribution rows use GC-EXCLUDED µs (`kao-bench--us-sans-gc'), unlike the
;; observational `make bench' rows.  GC-inclusive deltas are an artifact of
;; live-heap size, not stage cost: `gc-cons-percentage' scales the collection
;; threshold with the live heap, so a stage that RETAINS data (the sel-history
;; ring holds ~200k structs on the 1k bench) makes GCs rarer, and no-opping it
;; reproducibly measured NEGATIVE (probed: recorder on = 46 GCs/0.52s gc,
;; off = 118 GCs/0.92s gc, while mutator time correctly dropped 2.44→2.33s).

(defun kao-bench--stage-noops (stage)
  "Return ((SYMBOL . REPLACEMENT) ...) function overrides disabling STAGE.
The stages are suspect list: `sort-merge' = the per-keystroke
`kao-sels-sort-and-merge-overlapping' (identity keeps the list as built —
sound for the bench motions, which never reorder or overlap); `render' = the
secondary-overlay render; `sel-history' = the recorder (deep equal +
snapshot per command); `hist-commit' = the tree commit hook;
`change-capture' = the edit-time `before/after-change-functions' pair."
  (pcase stage
    ('sort-merge (list (cons 'kao-sels-sort-and-merge-overlapping #'identity)))
    ('render (list (cons 'kao--render #'ignore)))
    ('sel-history (list (cons 'kao--sel-history-record #'ignore)))
    ('hist-commit (list (cons 'kao--hist-maybe-commit #'ignore)))
    ('change-capture (list (cons 'kao--hist-before-change #'ignore)
                           (cons 'kao--hist-after-change #'ignore)))
    (_ (error "Unknown attribution stage: %s" stage))))

(defun kao-bench--with-stages-off (stages thunk)
  "Call THUNK with every pipeline stage in STAGES no-opped; restore after.
Function cells are swapped with `fset' (every stage is a plain defun, and
both hook dispatch and direct calls indirect through the cell, compiled
included), restored in an `unwind-protect'."
  (let ((overrides (mapcan #'kao-bench--stage-noops stages))
        (saved nil))
    (unwind-protect
        (progn
          (dolist (o overrides)
            (push (cons (car o) (symbol-function (car o))) saved)
            (fset (car o) (cdr o)))
          (funcall thunk))
      (dolist (s saved)
        (fset (car s) (cdr s))))))

(defun kao-bench--insert-kao-us (reps &optional stages undo-off)
  "Per-key µs for REPS kao insert-state keystrokes with STAGES no-opped.
With UNDO-OFF, `buffer-undo-list' is disabled AFTER the session's change
group is activated (activation re-enables a disabled list, so the order
matters) — the typed keys then pay no native undo bookkeeping, isolating the
double-bookkeeping half.  `undo-amalgamate-change-group' and
`accept-change-group' both tolerate the disabled list at exit (probed:
the list stays t, nothing signals).  GC-excluded µs (see commentary)."
  (kao-bench--with-stages-off stages
    (lambda ()
      (kao-bench--with-buffer ""
        (kao-insert)
        (when undo-off (setq buffer-undo-list t))
        (kao-bench--type-chars 64)                          ; warmup
        (prog1 (kao-bench--us-sans-gc
                (benchmark-run 1 (kao-bench--type-chars reps)) reps)
          (kao-insert-exit))))))

(defun kao-bench--attr-section (label baseline variants)
  "Print one attribution section; return ((baseline . µs) (NAME . µs) ...).
BASELINE and each (NAME . THUNK) variant in VARIANTS return per-op µs; the
printed `stage cost' is baseline − variant."
  (let* ((base (funcall baseline))
         (rows (list (cons 'baseline base))))
    (message "  %-30s %10.2f µs   baseline" label base)
    (dolist (v variants)
      (let ((us (funcall (cdr v))))
        (push (cons (car v) us) rows)
        (message "    %-28s %10.2f µs   (stage cost %+.2f)"
                 (format "%s off" (car v)) us (- base us))))
    (nreverse rows)))

(defun kao-bench--motion-attr-variants (runner stages)
  "Variants list for `kao-bench--attr-section': RUNNER under each of STAGES off.
Appends an `all' variant (every stage off at once) — the Δs need not sum, and
the floor shows the irreducible map/clamp/hook cost."
  (append
   (mapcar (lambda (s)
             (cons s (lambda () (kao-bench--with-stages-off (list s) runner))))
           stages)
   (list (cons 'all (lambda () (kao-bench--with-stages-off stages runner))))))

(defun kao-bench-attribution (&optional reps-single reps-1k reps-insert)
  "Print the per-stage µs attribution table; return the data.
Optional REPS-SINGLE / REPS-1K / REPS-INSERT override the per-bench rep
counts (the smoke test runs tiny scale).  Returns an alist
\((BENCH (VARIANT . µs) ...) ...) with benches `motion-single', `motion-1k',
`motion-1k-windowed', and `insert'."
  (let ((motion-stages '(sort-merge render sel-history hist-commit))
        (data nil))
    (message "kao bench attribution (GC-excluded µs) — Emacs %s, %s"
             emacs-version system-type)
    (let ((runner (lambda ()
                    (plist-get (kao-bench-motion-single reps-single)
                               :us-sans-gc))))
      (push (cons 'motion-single
                  (kao-bench--attr-section
                   "motion single-cursor" runner
                   (kao-bench--motion-attr-variants runner motion-stages)))
            data))
    (let ((runner (lambda ()
                    (plist-get (kao-bench-motion-1k-cursors reps-1k)
                               :us-sans-gc))))
      (push (cons 'motion-1k
                  (kao-bench--attr-section
                   "motion 1k cursors (batch)" runner
                   (kao-bench--motion-attr-variants runner motion-stages)))
            data))
    (let ((runner (lambda ()
                    (plist-get (kao-bench-motion-1k-cursors-windowed reps-1k)
                               :us-sans-gc))))
      (push (cons 'motion-1k-windowed
                  (kao-bench--attr-section
                   "motion 1k cursors (windowed)" runner
                   (kao-bench--motion-attr-variants runner motion-stages)))
            data))
    (let ((reps (or reps-insert 20000)))
      (push (cons 'insert
                  (kao-bench--attr-section
                   "insert typing (per key)"
                   (lambda () (kao-bench--insert-kao-us reps))
                   (list (cons 'change-capture
                               (lambda ()
                                 (kao-bench--insert-kao-us
                                  reps '(change-capture))))
                         (cons 'native-undo
                               (lambda ()
                                 (kao-bench--insert-kao-us reps nil t)))
                         (cons 'both--b1a-ceiling
                               (lambda ()
                                 (kao-bench--insert-kao-us
                                  reps '(change-capture) t))))))
            data))
    (nreverse data)))

;;;; Runner

(defun kao-bench-run ()
  "Run every bench at default scale and print one row each."
  ;; `byte-compiled:' flags whether the hot path (`kao--render') runs as
  ;; byte-code or interpreted: the `bench' target compiles
  ;; first, so a comparable row must show `t' — an interpreted row is ~9x
  ;; pessimistic and must not be compared against a compiled one.
  (message "kao bench — Emacs %s, %s, byte-compiled: %s"
           emacs-version system-type
           (byte-code-function-p (symbol-function 'kao--render)))
  (let ((r (kao-bench-motion-single)))
    (message "  motion single-cursor   %8.2f µs/op   (reps %d)"
             (plist-get r :us-per-op) (plist-get r :reps)))
  (let ((r (kao-bench-motion-1k-cursors)))
    (message "  motion %4d cursors    %8.2f µs/op   (reps %d)"
             (plist-get r :cursors) (plist-get r :us-per-op) (plist-get r :reps)))
  (let ((r (kao-bench-motion-1k-cursors-windowed)))
    (message "  motion %4d windowed   %8.2f µs/op   (reps %d, %d secondaries visible)"
             (plist-get r :cursors) (plist-get r :us-per-op)
             (plist-get r :reps) (plist-get r :visible)))
  (let ((r (kao-bench-history)))
    (message "  history per command    %8.2f µs/op   (reps %d, flatness ratio %.2f)"
             (plist-get r :us-per-op) (plist-get r :reps) (plist-get r :ratio)))
  (let ((r (kao-bench-insert-throughput)))
    (message "  insert typing          %8.2f µs/key  (native %.2f µs/key, overhead %.2fx, reps %d)"
             (plist-get r :kao-us) (plist-get r :native-us)
             (plist-get r :ratio) (plist-get r :reps)))
  (let ((r (kao-bench-copy-lines)))
    (message "  copy-lines C           %8.2f µs/op   (tall %.2f / short %.2f, flatness %.2f, reps %d)"
             (plist-get r :us-per-op) (plist-get r :tall-us) (plist-get r :short-us)
             (plist-get r :ratio) (plist-get r :reps)))
  (let ((r (kao-bench-pipe)))
    (message "  pipe |true env         %8.2f µs/op   (%d sels / few, flatness %.2f, reps %d)"
             (plist-get r :us-per-op) (plist-get r :sels)
             (plist-get r :ratio) (plist-get r :reps)))
  (let ((r (kao-bench-treesit)))
    (if (plist-get r :available)
        (message "  treesit <a-i>f / SPC t n %6.2f / %.2f µs/op   (%d cursors, reps %d)"
                 (plist-get r :select-us) (plist-get r :goto-us)
                 (plist-get r :cursors) (plist-get r :reps))
      (message "  treesit <a-i>f / SPC t n      —   (skipped: bash grammar unavailable)")))
  (let ((r (kao-bench-search-highlight)))
    (message "  search hl n (repeat)   %8.2f µs/op   (first %.2f, repeat/first %.4f, reps %d)"
             (plist-get r :repeat-us) (plist-get r :first-us)
             (plist-get r :ratio) (plist-get r :reps)))
  (let ((r (kao-bench-incsearch)))
    (message "  incsearch preview      %8.2f µs/op   (reps %d)"
             (plist-get r :us-per-op) (plist-get r :reps)))
  (let ((r (kao-bench-find-match)))
    (message "  find-miss / match-unbal %7.2f / %.2f µs/op   (reps %d)"
             (plist-get r :find-us) (plist-get r :match-us) (plist-get r :reps)))
  (let ((r (kao-bench-foreign-sync)))
    (message "  foreign-sync fold      %8.2f µs/op   (%d cursors, folds plain %d / foreign %d)"
             (plist-get r :us-per-op) (plist-get r :cursors)
             (plist-get r :folds-plain) (plist-get r :folds-foreign)))
  (let ((r (kao-bench-refresh-dedup)))
    (message "  refresh dedup          %8.2f µs/op   (%d cursors, %d render/key)"
             (plist-get r :us-per-op) (plist-get r :cursors)
             (plist-get r :renders-per-key)))
  (let ((r (kao-bench-treesit-object-memo)))
    (if (plist-get r :available)
        (message "  treesit <a-i>f memo    %8.2f µs/op   (%d cursors, %d query/pass)"
                 (plist-get r :us-per-op) (plist-get r :cursors)
                 (plist-get r :queries))
      (message "  treesit <a-i>f memo          —   (skipped: bash grammar unavailable)"))))

(provide 'kao-bench)
;;; kao-bench.el ends here
