;;; kao-motion.el --- Motions for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 3.  Motions are *list transforms*: each takes one
;; `kao-sel' and returns a new one, applied to every selection in a single pass
;; through `kao--map-selections' — there is no per-cursor command replay.
;;
;; The word selectors mirror references/kakoune/src/selectors.cc (`Word' class)
;; and its skip helpers in utils.hh:
;;   `skip_while'         returns "did not hit the end".
;;   `skip_while_reverse' returns "the predicate still holds at the stop"
;;                        (i.e. it hit the begin boundary while matching).
;; The WORD class (<a-w>/<a-b>/<a-e> + extends) reuses the same selectors with
;; `kao-motion--big-word' bound, folding punctuation into word (categorize<WORD>,
;; unicode.hh:118).  Categorisation ports `categorize' (unicode.hh:117-128):
;; ASCII alnum inline + `kao-extra-word-chars' (the `extra_word_chars' option
;; port, default underscore); codepoints >= 128 via the lazily-filled
;; `kao-motion--unicode-table' (Unicode letters+numbers = word, the faithful
;; `is_horizontal_blank' set = blank — see the table docstring for the
;; documented iswalnum/locale deviation).

;;; Code:

(require 'kao-selection)
(require 'kao-state)

;;;; Character categorisation (Kakoune Word class, ASCII)

(defun kao-motion--char (pos)
  "Return the char at buffer position POS, or nil at/after `point-max'."
  (when (< pos (point-max)) (char-after pos)))

(defcustom kao-extra-word-chars '(?_)
  "Extra characters categorised as word characters (beyond ASCII alnum).
The faithful port of Kakoune's `extra_word_chars' option, whose default is
also underscore-only (`is_word', unicode.hh:75; the option declaration,
main.cc:534-535).  Consulted by `kao-motion--category', so it shapes the
word motions (w/b/e and extends) and the word objects.  Can be set
buffer-locally, like the Kakoune option."
  :type '(repeat character)
  :group 'kao)

(defcustom kao-motion-skip-invisible nil
  "When non-nil, hop the CURSOR past invisible text runs after a motion.
Off by default, matching Kakoune: selections operate on buffer text regardless
of visibility (buffer-text stance), so a motion into a folded org-link
or org-fold span lands inside it.  When on, the char motions (h/l), the j/k
goal column landing, and the f/t find-char paths hop the cursor to the far edge
of an invisible run (the `adjust_point_for_property' discipline), so it lands on
visible text.  CURSOR-only: what edits, objects, and x operate on is never
changed, only where the cursor comes to rest."
  :type 'boolean
  :group 'kao)

(defvar kao-motion--big-word nil
  "When non-nil, categorise for WORD: any non-blank, non-eol char is `word'.
Bound dynamically by the `<a-w>'/`<a-b>'/`<a-e>' WORD motions so the shared word
selectors reproduce Kakoune `categorize<WORD>' (unicode.hh:118-127), where the
Punctuation category folds into Word.")

(defconst kao-motion--unicode-blanks
  '(#x00A0 #xFEFF #x1680 #x2028 #x2029 #x202F #x205F #x3000)
  "Non-ASCII horizontal blanks (Kakoune `is_horizontal_blank', unicode.hh:20-47).
U+2000..U+200A are preset as a range in `kao-motion--unicode-table' instead.")

(defvar kao-motion--unicode-table
  (let ((tbl (make-char-table 'kao-word-category nil)))
    (dolist (c kao-motion--unicode-blanks) (aset tbl c 'blank))
    (set-char-table-range tbl '(#x2000 . #x200A) 'blank)
    tbl)
  "Char-table caching the category of codepoints >= 128: `word', `punct', `blank'.
Blanks are preset (the faithful Kakoune `is_horizontal_blank' set); alnum-ness
is computed once per codepoint on first lookup (`kao-motion--unicode-category')
and cached — no eager Unicode scan at load, O(1) C-level `aref' thereafter.
Deviation (native-mechanism family, documented): Kakoune classifies
non-ASCII via the locale-dependent POSIX `iswalnum' (unicode.hh:77); kao uses
Emacs's locale-INdependent Unicode general-category tables (letters, numbers,
and — refining it — combining marks Mn/Mc/Me, which are Alphabetic under the
Other_Alphabetic property and so alnum on glibc, the platform Kakoune is
developed on).  This keeps Indic, Devanagari-with-matras, and decomposed Latin
words whole; on macOS libc Kakoune still fragments them, a documented
platform-dependent match, not a divergence from the reference build.")

(defun kao-motion--unicode-category (char)
  "Category of CHAR (a codepoint >= 128): `word', `punct' or `blank'.
O(1) `aref' after the first lookup of a given codepoint."
  (or (aref kao-motion--unicode-table char)
      (aset kao-motion--unicode-table char
            (if (memq (get-char-code-property char 'general-category)
                      '(Lu Ll Lt Lm Lo Nd Nl No Mn Mc Me))
                'word 'punct))))

(defun kao-motion--category (char)
  "Classify CHAR as `word', `punct', `blank', `eol', or nil (Kakoune Word).
With `kao-motion--big-word' non-nil, every non-blank/eol char is `word' (WORD).
Ports `categorize' (unicode.hh:117-128): eol, then horizontal blank (so a
Unicode blank stays `blank' even for WORD), then word-or-WORD, else punct.
\\f is blank per `is_horizontal_blank'; \\r/\\v are not (only `is_eol'/
`is_horizontal_blank' are tested there, so they fall through to punct)."
  (cond ((null char) nil)
        ((= char ?\n) 'eol)
        ((or (= char ?\s) (= char ?\t) (= char ?\f)) 'blank)
        ((>= char 128)
         (let ((cat (kao-motion--unicode-category char)))
           (cond ((eq cat 'blank) 'blank)
                 (kao-motion--big-word 'word)
                 ((eq cat 'word) 'word)
                 ((memq char kao-extra-word-chars) 'word)
                 (t 'punct))))
        (kao-motion--big-word 'word)        ; WORD: every non-blank/eol char is word
        ((or (and (>= char ?a) (<= char ?z))
             (and (>= char ?A) (<= char ?Z))
             (and (>= char ?0) (<= char ?9))
             (memq char kao-extra-word-chars))
         'word)
        (t 'punct)))

(defun kao-motion--cat-at (pos)
  "Category of the char at POS."
  (kao-motion--category (kao-motion--char pos)))

;;;; hjkl — reduce to a single char and move

(defun kao-motion--skip-invisible (pos dir)
  "Hop POS past an invisible run in direction DIR when `kao-motion-skip-invisible'.
DIR > 0 returns the first VISIBLE position at or past the run's forward edge
\(via `next-single-char-property-change'); DIR < 0 returns the first visible
position before the run (via `previous-single-char-property-change'), floored at
`point-min'.  A CURSOR-only adjustment — the buffer-text stance holds, so
what edits and objects operate on is unchanged.  Returns POS unchanged when the
knob is nil or POS is already visible."
  (if (and kao-motion-skip-invisible (invisible-p pos))
      (if (> dir 0)
          (kao--clamp-pos (next-single-char-property-change pos 'invisible))
        (max (point-min)
             (1- (previous-single-char-property-change (1+ pos) 'invisible))))
    pos))

(defun kao-motion-left (sel)
  "Kakoune `h': SEL's cursor one char left, reduced to one.
Buffer-bounded, not line-bounded: `move_cursor<CharCount>' resolves through
`utf8::advance' toward `begin()' with no line awareness (buffer.cc:165-168;
utf8.hh:182), so `h' from column 0 lands on the previous line's newline and
stops only at `point-min'."
  (let* ((cur (kao-sel-cursor sel))
         (p (kao-motion--skip-invisible (max (point-min) (1- cur)) -1)))
    (kao-sel-make :anchor p :cursor p)))

(defun kao-motion-right (sel)
  "Kakoune `l': SEL's cursor one char right, reduced to one.
Buffer-bounded, not line-bounded: `move_cursor<CharCount>' resolves through
`utf8::advance' toward `end()-1' with no line awareness (buffer.cc:165-168;
utf8.hh:182), so `l' from a line's newline lands on the next line's first char
and stops only at the last buffer char.  `kao--clamp-pos' is exactly the
`end()-1' limit (and keeps position 0 out of an empty buffer, unlike a bare
`(min (1- (point-max)) ...)')."
  (let* ((cur (kao-sel-cursor sel))
         (p (kao-motion--skip-invisible (kao--clamp-pos (1+ cur)) 1)))
    (kao-sel-make :anchor p :cursor p)))

(defun kao-motion--resolve-goal-column (col avoid-eol)
  "Park point at goal column COL on the current line, reduced to one.
Applies Kakoune's two display corrections after `move-to-column': the
break-before-wide-char rule (land ON a straddled wide char, not past it) and,
unless AVOID-EOL is nil, the `avoid_eol' rule (back off the trailing newline).
A point-motion in the current buffer — the caller reads `point' afterward."
  (move-to-column col)
  ;; break-before-wide-char (buffer_utils.cc:65-88): `get_byte_to_column'
  ;; stops BEFORE the char that would overshoot the goal column, so a goal
  ;; column landing mid-wide-char resolves ON that char.  Emacs's
  ;; `move-to-column' instead moves PAST a straddled wide char, overshooting
  ;; it by one; back off onto it.  `move-to-column' already stops before an
  ;; unenterable tab, so this fires only on a straddled wide (CJK) char.
  (when (> (current-column) col)
    (backward-char))
  ;; avoid_eol (buffer.cc:173): when the goal column overshoots the line,
  ;; `move-to-column' parks on the newline — back off one CHARACTER (not one
  ;; display column: a column-based `move-to-column (1- col)' re-straddles a
  ;; trailing wide char and parks on the eol again — the break-before rule
  ;; above) unless the `eol' sentinel wants the newline or the line is empty.
  ;; The common in-bounds press skips this entirely.  Guard on
  ;; `current-column' not point>bol (A-22a): an invisible line prefix leaves
  ;; point past bol at column 0, and `backward-char' there would cross onto
  ;; the previous line.
  (when (and avoid-eol (eolp) (> (current-column) 0))
    (backward-char)))

(defun kao-motion--step-lines (dir)
  "Step point DIR lines, skipping invisible lines and clamping the phantom line.
A point-motion in the current buffer; the caller reads `point' afterward within
its own `save-excursion'.  Steps `forward-line' by DIR, then applies two Kakoune
line-level corrections before the goal column resolves: skip lines hidden by
invisibility in the motion direction (A-22b), and clamp a downward overshoot
onto the phantom trailing line back to the last real line."
  (forward-line dir)
  ;; A-22b: skip lines hidden by invisibility (org/outline folds,
  ;; magit sections) in the motion direction — the native
  ;; `line-move-ignore-invisible' discipline — so j/k land on a VISIBLE
  ;; line instead of inside the fold.  Bounded by the buffer edges: a
  ;; fold reaching `point-min'/`point-max' stops there.  The common
  ;; no-fold press pays a single `invisible-p' check.
  (while (and (invisible-p (point))
              (if (> dir 0) (not (eobp)) (not (bobp))))
    (forward-line dir))
  ;; Emacs's phantom trailing line (the empty line after the
  ;; final newline) is not-a-line.  Kakoune buffers always end in `\n' and
  ;; have no phantom line, so `Buffer::offset_coord' clamps `line' to
  ;; `line_count()-1' (buffer.cc:170-179), making `j'/`J' on the last real
  ;; line resolve to the SAME line at the goal column.  The `bobp' guard
  ;; excludes empty buffers; a newline-less final line never hits
  ;; `eobp && bolp' while its content is under point, so it is unaffected.
  (when (and (> dir 0) (eobp) (bolp) (not (bobp)))
    (forward-line -1)))

(defun kao-motion--vertical (sel dir)
  "Move SEL's cursor DIR lines, resolving the sticky goal column, reduced to one.
Resolves the goal column per `Buffer::offset_coord' (buffer.cc:170-179); the
line step uses `forward-line' (the buffer-edge line clamp is delegated to the
universal `kao--clamp-sel', the family).  A downward step onto Emacs's
phantom trailing line (empty line after the final newline) is clamped back to
the last real line — Kakoune's `line_count()-1' clamp (buffer.cc:170-179), which
has no phantom line.  The goal column is resolved by
`kao-motion--resolve-goal-column'.  SEL's `target' slot is the
sticky goal column.  nil = no sticky column (the real column, Kakoune's -1);
an integer = a desired display column; `eol' = the `max_column' sentinel (stick
to the newline, no eol-avoid); `before-eol' = `max_non_eol_column' (stick to the
last non-eol char).  Lands at most one column before the newline unless the
`eol' sentinel is set (the C++ `avoid_eol = target < max_column').  The result
carries the resolved target so a following `j'/`k' restores the column through a
short line."
  (let* ((cur (kao-sel-cursor sel))
         (tgt (kao-sel-target sel))
         (avoid-eol (not (eq tgt 'eol))))
    (save-excursion
      (goto-char cur)
      (let ((col (cond ((null tgt) (current-column))         ; -1 -> real column
                       ((integerp tgt) tgt)                  ; sticky desired col
                       (t most-positive-fixnum))))           ; eol / before-eol
        ;; Step the line(s), skipping invisible lines and clamping the phantom
        ;; trailing line — `col' is captured above at the ORIGIN line first.
        (kao-motion--step-lines dir)
        (kao-motion--resolve-goal-column col avoid-eol)
        ;; when the goal column resolves onto an
        ;; invisible char (the eol-backoff tail of a folded run), opt into hopping
        ;; the cursor off it, in the motion direction.  CURSOR-only; the sticky
        ;; goal `target' is unchanged so a following j/k returns to the column.
        (let ((p (kao-motion--skip-invisible (point) dir)))
          (kao-sel-make :anchor p :cursor p
                        :target (if (null tgt) col tgt)))))))

(defun kao-motion-down (sel) "Kakoune `j': SEL one line down." (kao-motion--vertical sel 1))
(defun kao-motion-up (sel)   "Kakoune `k': SEL one line up."   (kao-motion--vertical sel -1))

;;;; Word motions (mirror selectors.cc)

(defun kao-motion--word-begin (cur)
  "Shared `w'/`e' begin setup from CUR: category-adjust then skip eol.
Return the begin position, or nil when the cursor cannot advance to a word."
  (let ((begin cur) (end (point-max)))
    (if (>= (1+ begin) end)                       ; cursor is the last char
        nil
      (unless (eq (kao-motion--cat-at begin) (kao-motion--cat-at (1+ begin)))
        (setq begin (1+ begin)))
      (while (and (< begin end) (eq (kao-motion--cat-at begin) 'eol))
        (setq begin (1+ begin)))
      (if (>= begin end) nil begin))))

(defun kao-motion-word-forward (sel)
  "Kakoune `w': select SEL to the next word start plus trailing blanks.
Returns nil when the cursor cannot advance (exhausted at the buffer edge) — the
nullopt the dispatch drops, not a stuck copy."
  (let ((begin (kao-motion--word-begin (kao-sel-cursor sel)))
        (end (point-max)))
    (if (null begin)
        nil
      (let ((e (1+ begin)) (cat (kao-motion--cat-at begin)))
        (cond
         ((eq cat 'word)
          (while (and (< e end) (eq (kao-motion--cat-at e) 'word)) (setq e (1+ e))))
         ((eq cat 'punct)
          (while (and (< e end) (eq (kao-motion--cat-at e) 'punct)) (setq e (1+ e)))))
        (while (and (< e end) (eq (kao-motion--cat-at e) 'blank)) (setq e (1+ e)))
        (kao-sel-make :anchor begin :cursor (1- e))))))

(defun kao-motion-word-end (sel)
  "Kakoune `e': select SEL to the next word end, skipping leading blanks.
Returns nil when the cursor cannot advance (exhausted at the buffer edge) — the
nullopt the dispatch drops, not a stuck copy."
  (let ((begin (kao-motion--word-begin (kao-sel-cursor sel)))
        (end (point-max)))
    (if (null begin)
        nil
      (let ((e begin))
        (while (and (< e end) (eq (kao-motion--cat-at e) 'blank)) (setq e (1+ e)))
        (let ((cat (kao-motion--cat-at e)))
          (cond
           ((eq cat 'word)
            (while (and (< e end) (eq (kao-motion--cat-at e) 'word)) (setq e (1+ e))))
           ((eq cat 'punct)
            (while (and (< e end) (eq (kao-motion--cat-at e) 'punct)) (setq e (1+ e))))))
        (kao-sel-make :anchor begin :cursor (1- e))))))

(defun kao-motion--skip-reverse (pos bob pred)
  "Move POS left while PRED holds on the category at POS, floored at BOB.
Return (NEWPOS . STILL-MATCHES), where STILL-MATCHES mirrors Kakoune
`skip_while_reverse': it is non-nil when POS stopped at BOB with PRED still
true."
  (while (and (> pos bob) (funcall pred (kao-motion--cat-at pos)))
    (setq pos (1- pos)))
  (cons pos (funcall pred (kao-motion--cat-at pos))))

(defun kao-motion-word-backward (sel)
  "Kakoune `b': select SEL back to the previous word start (backward).
Returns nil when the cursor is already at the buffer start (exhausted) — the
nullopt the dispatch drops, not a stuck copy."
  (let ((begin (kao-sel-cursor sel)) (bob (point-min)))
    (if (<= begin bob)
        nil
      (unless (eq (kao-motion--cat-at begin) (kao-motion--cat-at (1- begin)))
        (setq begin (1- begin)))
      (setq begin (car (kao-motion--skip-reverse begin bob
                                                 (lambda (c) (eq c 'eol)))))
      (let* ((br (kao-motion--skip-reverse begin bob (lambda (c) (eq c 'blank))))
             (e (car br)) (still (cdr br))
             (cat (kao-motion--cat-at e)))
        (cond
         ((eq cat 'word)
          (let ((r (kao-motion--skip-reverse e bob (lambda (c) (eq c 'word)))))
            (setq e (car r) still (cdr r))))
         ((eq cat 'punct)
          (let ((r (kao-motion--skip-reverse e bob (lambda (c) (eq c 'punct)))))
            (setq e (car r) still (cdr r)))))
        ;; with_end ? end : end+1  (still-matching means the word reaches bob)
        (kao-sel-make :anchor begin :cursor (if still e (1+ e)))))))

;;;; x — expand selections to whole lines

(defun kao-motion-line (sel)
  "Kakoune `x': expand SEL to contain its full lines (`select_lines').
Faithful to selectors.cc:1043: the min end moves to its line's beginning,
the max end onto its line's newline (the line's last char,
`buffer[line].length()-1'), direction preserved — and therefore idempotent;
the old select-line-then-extend-down was pre-2019 Kakoune, removed upstream
\(the pinned reference binds `x' to bare `select_lines', normal.cc:2544).
On a newline-less final line the max end lands at the `line-end-position'
\(= `point-max'), the no-forced-trailing-newline family.
`x' also sets the cursor's `max_column' sticky-eol TARGET (selectors.cc:1054,
`{cursor, max_column}'): a following `j'/`k' sticks to the newline."
  (let ((forward (<= (kao-sel-anchor sel) (kao-sel-cursor sel)))
        (mn (save-excursion (goto-char (kao-sel-min sel))
                            (line-beginning-position)))
        (mx (save-excursion (goto-char (kao-sel-max sel))
                            (line-end-position))))
    (if forward
        (kao-sel-make :anchor mn :cursor mx :target 'eol)
      (kao-sel-make :anchor mx :cursor mn :target 'eol))))

;;;; <a-x> — crop selections to the whole lines they cover

(defun kao-motion--on-line-terminator-p (pos)
  "Non-nil when POS sits on its line's terminator char.
The newline (Kakoune's line-last char, `buffer[line].length-1'), or
family — the last buffer char of a final line lacking a trailing newline
\(Kakoune's forced trailing `\\n' cannot express such a buffer)."
  (let ((eol (save-excursion (goto-char pos) (line-end-position))))
    (or (= pos eol)                       ; on the newline
        (and (= eol (point-max))          ; newline-less final line
             (= pos (1- eol))))))

(defun kao-motion--trim-partial-lines (sel)
  "Crop SEL to the whole lines it covers, or nil (Kakoune `trim_partial_lines').
Faithful to selectors.cc:1058: the min end moves forward to the next line's
begin unless already at a line begin; the max end moves back to the previous
line's newline unless on its line's terminator
\(`kao-motion--on-line-terminator-p') — a partial max end on the FIRST line
drops the selection (`to_line_end.line == 0' -> nullopt; in kao the guard is
structural — the min > max comparison would catch it anyway, since the min end
can never precede `point-min'), as does min > max after adjustment.  Direction
is preserved (the C++ adjusts anchor/cursor through min/max references).  Like
`select_lines', the cursor gets the `max_column' sticky-eol TARGET
\(selectors.cc:1080, `{cursor, max_column}'): a following `j'/`k' sticks to the
newline."
  (let ((mn (kao-sel-min sel))
        (mx (kao-sel-max sel))
        (forward (<= (kao-sel-anchor sel) (kao-sel-cursor sel))))
    (unless (= mn (save-excursion (goto-char mn) (line-beginning-position)))
      (setq mn (save-excursion (goto-char mn) (forward-line 1) (point))))
    (catch 'kao--drop
      (unless (kao-motion--on-line-terminator-p mx)
        (let ((bol (save-excursion (goto-char mx) (line-beginning-position))))
          (when (= bol (point-min))
            (throw 'kao--drop nil))
          (setq mx (1- bol))))            ; the previous line's newline
      (when (> mn mx)
        (throw 'kao--drop nil))
      (if forward
          (kao-sel-make :anchor mn :cursor mx :target 'eol)
        (kao-sel-make :anchor mx :cursor mn :target 'eol)))))

(defun kao-crop-lines ()
  "Kakoune `<a-x>': crop each selection to the whole lines it covers.
`select<SelectMode::Replace, trim_partial_lines>' (normal.cc:2545 — bare, NO
`repeated<>', so the count is ignored): selections not covering a full line
are dropped (`kao--map-filter-selections'), survivors sort-and-merge
\.  The faithful `<a-x>' key is Emacs's `M-x' (the command palette,
deliberately kept), so kao leaves this unbound by default (user decision
2026-06-13); reachable via `M-x' or a user binding in `kao-user-map'."
  (interactive)
  (kao--assert-mode)
  (kao--map-filter-selections #'kao-motion--trim-partial-lines))

;;;; Interactive commands (default keys live in kao-keys.el)

(defmacro kao-motion--defcmd (name fn &optional big drop)
  "Define interactive NAME applying FN to every selection.
FN repeats `kao--repeat-count' times per selection (Kakoune `repeated<>').
With BIG non-nil, `kao-motion--big-word' is bound around the pass so FN
categorises by WORD (`<a-w>'/`<a-b>'/`<a-e>').  With DROP non-nil FN may return
nil at an exhausted buffer edge and that selection is dropped through the
`kao--map-filter-selections' path (the w/b/e/WORD motions);
otherwise the 1:1 `kao--map-selections' is used (FN never returns nil — h/j/k/l,
x — keeping that hot path off the filter machinery)."
  (let ((mapper (if drop 'kao--map-filter-selections 'kao--map-selections)))
    `(defun ,name ()
       ,(format "Apply `%s' to every selection, repeated by the count.%s" fn
                (if big "\nCategorises by WORD (punctuation folds into word)." ""))
       (interactive)
       (kao--assert-mode)               ; clear error with the mode off
       ,(if big
            `(let ((kao-motion--big-word t))
               (,mapper (kao--repeat #',fn)))
          `(,mapper (kao--repeat #',fn))))))

(kao-motion--defcmd kao-left          kao-motion-left)
(kao-motion--defcmd kao-right         kao-motion-right)
(kao-motion--defcmd kao-down          kao-motion-down)
(kao-motion--defcmd kao-up            kao-motion-up)
(kao-motion--defcmd kao-word-forward  kao-motion-word-forward  nil t)
(kao-motion--defcmd kao-word-backward kao-motion-word-backward nil t)
(kao-motion--defcmd kao-word-end      kao-motion-word-end      nil t)
(kao-motion--defcmd kao-line          kao-motion-line)

;;;; Extend variants — keep the anchor, move the cursor (Kakoune Extend mode)

(defmacro kao-motion--defextend (name fn &optional big)
  "Define interactive NAME extending every selection toward FN.
With BIG non-nil, `kao-motion--big-word' is bound so FN categorises by WORD.
NAME reuses the SAME Replace motion FN; `kao--map-selections-extend' merges each
result onto its selection (`merge_selections'), so the anchor is preserved and
only the cursor follows FN.  The count is folded inside the mapping, so FN is
passed raw — NOT wrapped with `kao--repeat'.  A zero-width FN result (H/J/K/L,
bound in Kakoune to `move_cursor<...,Extend>') yields the keep-anchor rule; a
region result (W/B/E, bound to `repeated<select<Extend, …>>') the merge rule.
For both the per-step fold reproduces Kakoune's end position."
  `(defun ,name ()
     ,(format "Extend every selection toward `%s' (Kakoune Extend mode).%s" fn
              (if big "\nCategorises by WORD." ""))
     (interactive)
     (kao--assert-mode)                 ; clear error with the mode off
     ,(if big
          `(let ((kao-motion--big-word t)) (kao--map-selections-extend #',fn))
        `(kao--map-selections-extend #',fn))))

(kao-motion--defextend kao-extend-left          kao-motion-left)
(kao-motion--defextend kao-extend-right         kao-motion-right)
(kao-motion--defextend kao-extend-down          kao-motion-down)
(kao-motion--defextend kao-extend-up            kao-motion-up)
(kao-motion--defextend kao-extend-word-forward  kao-motion-word-forward)
(kao-motion--defextend kao-extend-word-backward kao-motion-word-backward)
(kao-motion--defextend kao-extend-word-end      kao-motion-word-end)

;;;; WORD motions — same selectors, punctuation folded into word (<a-[wbeWBE]>)
;; Reuse the Word selectors with `kao-motion--big-word' bound (categorize<WORD>,
;; unicode.hh:118); keys/binding from normal.cc:2532-2537.

(kao-motion--defcmd kao-WORD-forward  kao-motion-word-forward  t t)
(kao-motion--defcmd kao-WORD-backward kao-motion-word-backward t t)
(kao-motion--defcmd kao-WORD-end      kao-motion-word-end      t t)

(kao-motion--defextend kao-extend-WORD-forward  kao-motion-word-forward  t)
(kao-motion--defextend kao-extend-WORD-backward kao-motion-word-backward t)
(kao-motion--defextend kao-extend-WORD-end      kao-motion-word-end      t)

;;;; Find char (f/t/<a-f>/<a-t>) — select to a typed character

(defun kao-find--forward (cur c count inclusive)
  "Cursor position for a forward find of the COUNT-th C after CUR.
INCLUSIVE lands on C; else just before it.  nil when not found
\(Kakoune `select_to', selectors.cc:401: `++end' then skip to C, COUNT times).
A C-level `search-forward' from the char after CUR (single-char matches cannot
overlap, so the COUNT arg is the loop's count-th occurrence exactly); the old
per-char `char-after' scan was O(buffer) on a miss."
  (save-excursion
    (save-match-data
      (let ((case-fold-search nil))            ; buffer-local t in most buffers
        (goto-char (min (1+ cur) (point-max)))  ; `++end': never match CUR itself
        (when (search-forward (char-to-string c) nil t count)
          (let ((found (match-beginning 0)))
            (kao-motion--skip-invisible (if inclusive found (1- found)) 1)))))))

(defun kao-find--backward (cur c count inclusive)
  "Cursor position for a backward find of the COUNT-th C before CUR.
INCLUSIVE lands on C; else just after it.  nil when not found, when CUR is at
bob, or when the COUNT-th occurrence runs off the buffer start (Kakoune
`select_to_reverse', selectors.cc:419: `--end' then reverse-skip to C; a
`skip_while_reverse' that hits the begin boundary still matching returns nil).
A C-level `search-backward' from CUR (`--end': the match must end at or before
CUR, i.e. start strictly before it); COUNT-th occurrence and the run-off-start
nil follow from `search-backward's own NOERROR/COUNT contract."
  (if (<= cur (point-min))
      nil
    (save-excursion
      (save-match-data
        (let ((case-fold-search nil))
          (goto-char cur)
          (when (search-backward (char-to-string c) nil t count)
            (let ((found (match-beginning 0)))
              (kao-motion--skip-invisible (if inclusive found (1+ found)) -1))))))))

(defun kao--select-to-char-apply (ch count reverse inclusive &optional extend)
  "Apply a find-char selection for CH to every selection, COUNT occurrences out.
REVERSE searches backward; INCLUSIVE lands on CH, else just before it.  Per
selection the new selection is [cursor, target] with the anchor at the old
cursor; a selection whose target is not found is dropped (then sort-and-merged).
With EXTEND non-nil the result is merged onto the selection (anchor kept)."
  (kao--map-filter-selections
   (lambda (sel)
     (let* ((cur (kao-sel-cursor sel))
            (target (if reverse
                        (kao-find--backward cur ch count inclusive)
                      (kao-find--forward cur ch count inclusive))))
       (when target (kao-sel-make :anchor cur :cursor target))))
   extend))

(defun kao--select-to-char (reverse inclusive &optional extend)
  "Read one target char and run a find-char selection (`select_to_next_char').
REVERSE searches backward; INCLUSIVE lands on the char, else just before it.
Escape or a non-character event cancels with no change; a leading count selects
the count-th occurrence.  With EXTEND non-nil the selections are extended.

The char is read via `kao--read-key' so Q-macro recording stays in lockstep
\(bare `read-char' never reached the recorder), and normalized through
`kao--key-codepoint' so f/t with Return target the newline like Kakoune's
Key::codepoint (keys.cc:46-53) instead of a `\\r' that never exists in a
decoded buffer.  Both happen before the thunk capture, so `<a-.>' repeat
inherits the translated char.  The cancel guard is idiom unchanged."
  (let ((count (kao--repeat-count))
        (ch (kao--key-codepoint
             (kao--read-key (format "%s %s char: "
                                    (if extend (if inclusive "extend to" "extend till")
                                      (if inclusive "to" "till"))
                                    (if reverse "previous" "next"))))))
    (when (and (characterp ch) (/= ch ?\e))
      ;; Record the find so `<a-.>' can repeat it (the find-char
      ;; `select_and_set_last' call, normal.cc:1721): the recorded thunk IS the
      ;; live find, so the repeat is exact.  `kao--select-to-char-apply' stays
      ;; pure (batch-testable).
      (setq kao--last-select
            (lambda ()
              (kao--select-to-char-apply ch count reverse inclusive extend)))
      (funcall kao--last-select))))

(defun kao-find-to-char ()
  "Kakoune `f': select to the next occurrence of a typed char (inclusive)."
  (interactive)
  (kao--assert-mode)
  (kao--select-to-char nil t))

(defun kao-find-until-char ()
  "Kakoune \\=`t\\=': select until the next occurrence of a typed char."
  (interactive)
  (kao--assert-mode)
  (kao--select-to-char nil nil))

(defun kao-find-to-char-reverse ()
  "Kakoune `<a-f>': select to the previous occurrence of a char (inclusive)."
  (interactive)
  (kao--assert-mode)
  (kao--select-to-char t t))

(defun kao-find-until-char-reverse ()
  "Kakoune `<a-t>': select until the previous occurrence of a char (exclusive)."
  (interactive)
  (kao--assert-mode)
  (kao--select-to-char t nil))

(defun kao-extend-to-char ()
  "Kakoune `F': extend to the next occurrence of a typed char (inclusive)."
  (interactive)
  (kao--assert-mode)
  (kao--select-to-char nil t t))

(defun kao-extend-until-char ()
  "Kakoune `T': extend until the next occurrence of a typed char (exclusive)."
  (interactive)
  (kao--assert-mode)
  (kao--select-to-char nil nil t))

(defun kao-extend-to-char-reverse ()
  "Kakoune `<a-F>': extend to the previous occurrence of a char (inclusive)."
  (interactive)
  (kao--assert-mode)
  (kao--select-to-char t t t))

(defun kao-extend-until-char-reverse ()
  "Kakoune `<a-T>': extend until the previous occurrence of a char (exclusive)."
  (interactive)
  (kao--assert-mode)
  (kao--select-to-char t nil t))

;;;; Repeat last object-select / find-char (<a-.>)

(defun kao-repeat-select ()
  "Kakoune `<a-.>': repeat the last object-select or find-char command.
Re-runs `kao--last-select' — the closure recorded by the object
\(`kao--object-dispatch') and find-char (`kao--select-to-char') dispatch seams —
on the current selections (`repeat_last_select', normal.cc:177).  Only object
selects and `f'/\\=`t\\=' record one (Kakoune `set_last_select', :144);
motions, match
\(`m'/`M'), regex (`s'/`S') and goto do not.  No-op with a message when nothing
has been recorded yet."
  (interactive)
  (kao--assert-mode)
  (if kao--last-select
      (funcall kao--last-select)
    (message "kao: no selection to repeat")))

;;;; Selection-direction ops (in-place, no sort/merge) — ; <a-;> <a-:>
;; These bypass `select' (and its sort-and-merge), directly mutating each
;; selection like Kakoune `clear_selections'/`flip_selections'/`ensure_forward'
;; (normal.cc:2346-2375).  Order and the main index are preserved.

(defun kao-reduce ()
  "Kakoune `;': reduce every selection to its cursor (`clear_selections')."
  (interactive)
  (kao--assert-mode)
  (kao--map-selections-in-place #'kao-sel-collapse))

(defun kao-flip-selections ()
  "Kakoune `<a-;>': swap anchor and cursor of every selection (`flip_selections')."
  (interactive)
  (kao--assert-mode)
  (kao--map-selections-in-place #'kao-sel-flip))

(defun kao-ensure-forward ()
  "Kakoune `<a-:>': make every selection forward, cursor after anchor."
  (interactive)
  (kao--assert-mode)
  (kao--map-selections-in-place #'kao-sel-ensure-forward))

;;;; Match (m/<a-m>/M/<a-M>) — select to the matching delimiter
;; Port of Kakoune `select_matching<forward>' (selectors.cc:211).  From the
;; cursor we scan for a delimiter (forward for `m'/`M', backward for `<a-m>'/
;; `<a-M>'); the delimiter becomes the new anchor; from an opening we scan
;; forward to its balanced closing, from a closing we scan backward to its
;; balanced opening.  `forward' selects only the DELIMITER-FINDING direction —
;; the balance scan direction is fixed by whether that delimiter is an opening
;; or a closing.  All four route through `kao--map-filter-selections' (Replace,
;; or the `&optional extend' arm = `select<Extend>'), which already drops a
;; nullopt result, adjusts main, and sort-and-merges (normal.cc:81-133).  No
;; `repeated<>' wrapper in Kakoune (normal.cc:2547-2550) → count is ignored.

(defcustom kao-matching-pairs '(?\( ?\) ?{ ?} ?\[ ?\] ?< ?>)
  "Ordered list of matching delimiter characters (Kakoune `matching_pairs').
Even indices are opening characters, each followed at the next odd index by its
closing partner.  Used by the match commands `m'/`M'/`<a-m>'/`<a-M>' (main.cc
default `( ) { } [ ] < >')."
  :type '(repeat character)
  :group 'kao)

(defun kao-match--pair (ch)
  "Return (OPENING CLOSING ROLE) for a delimiter CH, or nil.
CH is matched against `kao-matching-pairs'.  ROLE is `open' when CH is an
opening character, `close' when a closing one.  Steps the list two at a time,
so an even-index hit is an opening and the following odd index its closing
\(Kakoune's `(match - begin) % 2' test, selectors.cc:239)."
  (let ((p kao-matching-pairs) (result nil))
    (while (and p (not result))
      (let ((o (car p)) (c (cadr p)))
        (cond ((eq ch o) (setq result (list o c 'open)))
              ((eq ch c) (setq result (list o c 'close))))
        (setq p (cddr p))))
    result))

(defun kao-match--selector (cur forward)
  "Select the matching-pair span at/from CUR (Kakoune `select_matching').
Scan for a delimiter starting at CUR — forward when FORWARD, else backward.
From an opening delimiter scan forward for the balanced closing; from a closing
delimiter scan backward for the balanced opening.  Return a `kao-sel' whose
anchor is the found delimiter and whose cursor is its match
\(`utf8_range(begin, it)', selectors.cc:211), or nil when no delimiter is found
or the pair is unbalanced (drop)."
  (save-excursion
    (save-match-data
      (let ((case-fold-search nil)
            (delims (regexp-opt (mapcar #'char-to-string kao-matching-pairs)))
            (begin nil) (info nil))
        ;; 1. locate a delimiter at/from CUR (the new anchor) — a C-level
        ;; `re-search' rather than the old per-char `char-after' scan.
        (if forward
            (progn (goto-char cur)
                   (when (re-search-forward delims nil t)
                     (setq begin (match-beginning 0))))
          ;; backward starts one past CUR so the char AT CUR can still match
          ;; (`re-search-backward' matches strictly before point).
          (goto-char (min (1+ cur) (point-max)))
          (when (re-search-backward delims nil t)
            (setq begin (match-beginning 0))))
        (when begin
          (setq info (kao-match--pair (char-after begin))))
        (when info
          (let* ((opening (nth 0 info))
                 (closing (nth 1 info))
                 (role (nth 2 info))
                 ;; balance scan steps only THIS pair, match-by-match.
                 (pair (regexp-opt (list (char-to-string opening)
                                         (char-to-string closing))))
                 (level 0) (res nil))
            (if (eq role 'open)
                ;; opening: scan forward to the balanced closing (begin's own
                ;; char is the first match, level -> 1)
                (progn
                  (goto-char begin)
                  (catch 'done
                    (while (re-search-forward pair nil t)
                      (let ((ch (char-after (match-beginning 0))))
                        (cond ((eq ch opening) (setq level (1+ level)))
                              ((eq ch closing)
                               (setq level (1- level))
                               (when (= level 0)
                                 (setq res (match-beginning 0))
                                 (throw 'done nil))))))))
              ;; closing: scan backward to the balanced opening (begin's own
              ;; char is the first match; start one past it so it matches)
              (goto-char (min (1+ begin) (point-max)))
              (catch 'done
                (while (re-search-backward pair nil t)
                  (let ((ch (char-after (match-beginning 0))))
                    (cond ((eq ch closing) (setq level (1+ level)))
                          ((eq ch opening)
                           (setq level (1- level))
                           (when (= level 0)
                             (setq res (match-beginning 0))
                             (throw 'done nil))))))))
            (when res (kao-sel-make :anchor begin :cursor res))))))))

(defun kao--select-matching-apply (forward &optional extend)
  "Run `kao-match--selector' over every selection (Replace, or EXTEND).
FORWARD chooses the delimiter-finding direction.  A selection with no match is
dropped (then sort-and-merged); with EXTEND non-nil each result is merged onto
its selection (anchor kept, Kakoune `select<Extend>')."
  (kao--map-filter-selections
   (lambda (sel) (kao-match--selector (kao-sel-cursor sel) forward))
   extend))

(defun kao-select-matching ()
  "Kakoune `m': select to the matching character (forward search, Replace)."
  (interactive)
  (kao--assert-mode)
  (kao--select-matching-apply t))

(defun kao-select-matching-backward ()
  "Kakoune `<a-m>': backward select to the matching character (Replace)."
  (interactive)
  (kao--assert-mode)
  (kao--select-matching-apply nil))

(defun kao-extend-matching ()
  "Kakoune `M': extend to the matching character (forward search, Extend)."
  (interactive)
  (kao--assert-mode)
  (kao--select-matching-apply t t))

(defun kao-extend-matching-backward ()
  "Kakoune `<a-M>': backward extend to the matching character (Extend)."
  (interactive)
  (kao--assert-mode)
  (kao--select-matching-apply nil t))

(provide 'kao-motion)
;;; kao-motion.el ends here
