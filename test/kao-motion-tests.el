;;; kao-motion-tests.el --- Tests for kao-motion -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the kao motions.  Per-sel transforms are checked against
;; Kakoune's selectors.cc semantics; a final integration test drives the live
;; minor mode through `kao-word-forward' / `kao-right'.

;;; Code:

(require 'ert)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-state)
(require 'kao-motion)
(require 'kao-keys)                    ; default bindings
(require 'outline)                     ; fold-skip tests

(defmacro kao-motion-tests--in (content &rest body)
  "Run BODY in a temp buffer containing CONTENT."
  (declare (indent 1))
  `(with-temp-buffer (insert ,content) ,@body))

(defun kao-motion-tests--apply (content cursor fn)
  "In CONTENT, apply FN to a single-char selection at CURSOR; return the sel."
  (with-temp-buffer
    (insert content)
    (funcall fn (kao-sel-make :anchor cursor :cursor cursor))))

;;;; hjkl

(ert-deftest kao-motion-right-crosses-newline ()
  "`l' is buffer-bounded, not line-bounded: Kakoune `move_cursor<CharCount>' is
`utf8::advance' toward `end()-1' (buffer.cc:165-168; utf8.hh:182), with no line
awareness — so from a line's newline it steps onto the next line's first char.
At the last buffer char it stays (the `end()-1' limit)."
  ;; "ab\ncd\n": a1 b2 \n3 c4 d5 \n6 ; point-max 7, last on-char pos 6
  (let ((s (kao-motion-tests--apply "ab\ncd\n" 3 #'kao-motion-right)))
    (should (= (kao-sel-cursor s) 4)))            ; on newline -> next line's 'c'
  (let ((s (kao-motion-tests--apply "ab\ncd\n" 6 #'kao-motion-right)))
    (should (= (kao-sel-cursor s) 6))))           ; last char -> stays (end()-1)

(ert-deftest kao-motion-right-counted-crosses-lines ()
  "A counted `l' walks freely across lines: `kao--repeat' folds the motion COUNT
times (kao-state.el:925), converging on Kakoune's single count-folded offset
(buffer.cc:165-168).  4l from pos 1 of \"ab\\ncd\\n\" lands on pos 5."
  (with-temp-buffer
    (insert "ab\ncd\n")                           ; a1 b2 \n3 c4 d5 \n6
    (let ((s (kao-sel-make :anchor 1 :cursor 1)))
      (dotimes (_ 4) (setq s (kao-motion-right s)))
      (should (= (kao-sel-cursor s) 5)))))        ; a->b->\n->c->d

(ert-deftest kao-motion-left-crosses-bol ()
  "`h' is buffer-bounded (buffer.cc:165-168; utf8.hh:182): from column 0 it steps
onto the previous line's newline.  At `point-min' it stays."
  (let ((s (kao-motion-tests--apply "ab\ncd\n" 4 #'kao-motion-left)))
    (should (= (kao-sel-cursor s) 3)))            ; bol of line 2 -> prev newline
  (let ((s (kao-motion-tests--apply "ab\ncd\n" 1 #'kao-motion-left)))
    (should (= (kao-sel-cursor s) 1))))           ; point-min -> stays

(ert-deftest kao-motion-vertical-preserves-column ()
  "`j'/`k' move a line, keeping the column, reduced to one char."
  (let ((s (kao-motion-tests--apply "abc\ndef\n" 2 #'kao-motion-down)))
    (should (= (kao-sel-cursor s) 6)))            ; col 1 on line 2 = 'e'
  (let ((s (kao-motion-tests--apply "abc\ndef\n" 6 #'kao-motion-up)))
    (should (= (kao-sel-cursor s) 2))))           ; back to 'b'

(ert-deftest kao-motion-vertical-goal-column-sticky ()
  "`j' onto a short line lands on the last non-eol char (avoid_eol,
buffer.cc:177), stores the goal column, and a following `k' restores it.
\"0123456789abcd\\nxyz\\n\": col 10 = pos 11 ('a'); short line \"xyz\" = pos 16-18."
  (kao-motion-tests--in "0123456789abcd\nxyz\n"
    (let* ((start (kao-sel-make :anchor 11 :cursor 11))   ; col 10 on the long line
           (down (kao-motion-down start)))
      (should (= (kao-sel-cursor down) 18))              ; 'z', last non-eol (col 2)
      (should (eql (kao-sel-target down) 10))            ; goal column stored
      (let ((up (kao-motion-up down)))
        (should (= (kao-sel-cursor up) 11))              ; restored to col 10 = 'a'
        (should (eql (kao-sel-target up) 10))))))

(ert-deftest kao-motion-vertical-goal-column-survives-short-intermediate ()
  "The per-step fold converges to the single-offset C++ result — `2j'
through a SHORT intermediate line restores col 10 on the next long line.
\"0123456789abcd\\nxy\\n0123456789abcd\\n\": L3 col 10 = pos 29."
  (kao-motion-tests--in "0123456789abcd\nxy\n0123456789abcd\n"
    (let* ((start (kao-sel-make :anchor 11 :cursor 11))   ; col 10 on L1
           (j2 (kao-motion-down (kao-motion-down start)))); 2j -> L3
      (should (= (kao-sel-cursor j2) 29))                 ; col 10 on L3 = 'a'
      (should (eql (kao-sel-target j2) 10)))))

(ert-deftest kao-motion-vertical-extend-carries-target ()
  "Extend `J' carries the sticky goal column — `kao-sel-extend-to' takes the
moved cursor's target so a following `K' restores the column."
  (kao-motion-tests--in "0123456789abcd\nxy\n"
    (let* ((start (kao-sel-make :anchor 11 :cursor 11))   ; col 10 on the long line
           (j (kao-sel-extend-to start (kao-motion-down start))))
      (should (eql (kao-sel-target j) 10)))))

(ert-deftest kao-motion-eol-sentinel-sticks-to-newline ()
  "With the `eol' (max_column) target, `j' onto a short line lands ON the
newline (avoid_eol false, selectors.cc:1054 + buffer.cc:173); sentinel kept."
  (kao-motion-tests--in "0123456789abcd\nxyz\n"
    (let ((down (kao-motion-down (kao-sel-make :anchor 11 :cursor 11
                                               :target 'eol))))
      (should (= (kao-sel-cursor down) 19))         ; the newline after "xyz"
      (should (eq (kao-sel-target down) 'eol)))))

(ert-deftest kao-motion-before-eol-sentinel-sticks-before-newline ()
  "With the `before-eol' (max_non_eol_column) target, `j' lands on the last
non-eol char (avoid_eol true, selectors.cc:186); sentinel kept."
  (kao-motion-tests--in "0123456789abcd\nxyz\n"
    (let ((down (kao-motion-down (kao-sel-make :anchor 11 :cursor 11
                                               :target 'before-eol))))
      (should (= (kao-sel-cursor down) 18))         ; 'z', last non-eol char
      (should (eq (kao-sel-target down) 'before-eol)))))

(ert-deftest kao-motion-line-sets-eol-target ()
  "`x' (`select_lines') sets the cursor's `eol' (max_column) sticky target,
both directions (selectors.cc:1054)."
  (kao-motion-tests--in "abc\ndef\n"
    (should (eq (kao-sel-target (kao-motion-line (kao-sel-make :anchor 1 :cursor 2)))
                'eol))                              ; forward
    (should (eq (kao-sel-target (kao-motion-line (kao-sel-make :anchor 6 :cursor 5)))
                'eol))))                            ; backward

(ert-deftest kao-motion-trim-partial-lines-sets-eol-target ()
  "`<a-x>' crop (`trim_partial_lines') sets the cursor's `eol' sticky target
(selectors.cc:1080)."
  (kao-motion-tests--in "abc\ndef\n"
    (let ((r (kao-motion--trim-partial-lines (kao-sel-make :anchor 1 :cursor 8))))
      (should (eq (kao-sel-target r) 'eol)))))

(ert-deftest kao-motion-column-motions-reset-target ()
  "A column-moving motion that advances rebuilds a fresh selection
with target nil (the implicit C++ BufferCoord->target=-1 path), so a starting
sticky target 7 does NOT survive `h'/`l'/`w'/`e'."
  (kao-motion-tests--in "foo bar\nbaz qux\n"
    (let ((s (kao-sel-make :anchor 2 :cursor 2 :target 7)))   ; col 1, sticky 7
      (should (null (kao-sel-target (kao-motion-left s))))
      (should (null (kao-sel-target (kao-motion-right s))))
      (should (null (kao-sel-target (kao-motion-word-forward s))))
      (should (null (kao-sel-target (kao-motion-word-end s)))))))

;;;; Invisible/folded text: j/k skip folds; no column-0 crash

(ert-deftest kao-motion-vertical-skips-outline-fold ()
  "`j'/`k' skip an `outline-hide-subtree' fold (ellipsis invisibility), landing
on a visible line — the native `line-move-ignore-invisible' discipline (A-22b).
Without the skip the cursor lands INSIDE the fold on a hidden body line."
  (with-temp-buffer
    (insert "* A\nb1\nb2\n* B\n")            ; *A@1 body b1@5 b2@8 *B@11 (both hidden)
    (outline-mode)
    (goto-char (point-min))
    (outline-hide-subtree)                    ; hides b1,b2 under "* A"
    (let ((down (kao-motion-down (kao-sel-make :anchor 1 :cursor 1))))
      (should (= (kao-sel-cursor down) 11)))  ; "* A" -> "* B", skipping b1/b2
    (let ((up (kao-motion-up (kao-sel-make :anchor 11 :cursor 11))))
      (should (= (kao-sel-cursor up) 1)))))   ; "* B" -> "* A", skipping b2/b1

(ert-deftest kao-motion-vertical-invisible-overlay-no-crash ()
  "`j' across a plain `invisible t' overlay (no ellipsis — the magit-section
shape) must not signal `(wrong-type-argument wholenump -1)': the avoid-eol
backoff guarded `(> (point) (line-beginning-position))', but an invisible line
prefix leaves point > bol at column 0, so it computed `(move-to-column -1)'
\(A-22a).  With the fix `j' skips the hidden line onto the visible one."
  (with-temp-buffer
    (insert "aaa\nhidden\nbbb\n")            ; aaa@1-3  hidden@5-10  bbb@12-14
    (let ((ov (make-overlay 5 11)))           ; "hidden" invisible, its \n visible
      (overlay-put ov 'invisible t))
    (let ((down (kao-motion-down (kao-sel-make :anchor 2 :cursor 2))))  ; col 1 on "aaa"
      (should (= (kao-sel-cursor down) 13)))))  ; skips "hidden" -> col 1 on "bbb"

;;;; Phantom trailing-line clamp
;; Kakoune buffers always end in `\n' and have no phantom trailing line, so
;; `Buffer::offset_coord' clamps `line = clamp(line, 0, line_count()-1)'
;; (buffer.cc:170-179): `j'/`J' on the last real line of a trailing-`\n' buffer
;; resolve to the SAME line at the goal column.  Emacs's phantom trailing line
;; (the empty line after the final newline) would otherwise catch j/k.

(ert-deftest kao-motion-vertical-j-clamps-phantom-last-line ()
  "`j' on the last real line of a trailing-`\\n' buffer is a same-line no-op —
the phantom trailing line is not-a-line, `LineCount offset_coord' clamps to
`line_count()-1' (buffer.cc:170-179).  Without the clamp `j' lands on the empty
phantom line at `point-max'."
  ;; "ab\ncd\n": a1 b2 \n3 c4 d5 \n6 ; point-max 7, phantom line at 7
  (with-temp-buffer
    (insert "ab\ncd\n")
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 4 :cursor 4)) ; 'c'
                           :main 0))
          (kao-down)
          (should (= (kao-sel-cursor (kao-motion-tests--main kao--sels)) 4)))
      (kao-mode -1))))

(ert-deftest kao-motion-vertical-extend-J-clamps-phantom-last-line ()
  "`J' (extend down) on the last real line stays zero-width: the clamped `j'
result keeps the cursor, so the extend merge keeps anchor=cursor (buffer.cc:170-179).
A following `d' then eats nothing past the last char."
  (with-temp-buffer
    (insert "ab\ncd\n")                          ; a1 b2 \n3 c4 d5 \n6
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 4 :cursor 4)) ; 'c'
                           :main 0))
          (kao-extend-down)                       ; J
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 4))
            (should (= (kao-sel-cursor s) 4))))    ; zero-width
      (kao-mode -1))))

(ert-deftest kao-motion-vertical-j-newlineless-last-line-unaffected ()
  "Control: a newline-less final line never hits `eobp && bolp' while its content
is under point, so the clamp is skipped and `j' stays a same-line no-op there too
\(already correct pre-clamp) — the clamp does not over-fire (buffer.cc:170-179)."
  ;; "ab\ncd" (no trailing newline): a1 b2 \n3 c4 d5 ; point-max 6
  (with-temp-buffer
    (insert "ab\ncd")
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 4 :cursor 4)) ; 'c'
                           :main 0))
          (kao-down)
          (should (= (kao-sel-cursor (kao-motion-tests--main kao--sels)) 4)))
      (kao-mode -1))))

(ert-deftest kao-motion-vertical-3j-overshoot-clamps-to-last-real-line ()
  "`3j' from two lines above the last real line lands ON the last real line at the
goal column, NOT the phantom line: each fold step clamps (buffer.cc:170-179), so
the third step's overshoot resolves back to the last real line."
  ;; "abcd\nefgh\nijkl\n": a1 b2 c3 d4 \n5 e6 f7 g8 h9 \n10 i11 j12 k13 l14 \n15
  ;; point-max 16, phantom line at 16.  Start col 1 on L1 (pos 2).
  (with-temp-buffer
    (insert "abcd\nefgh\nijkl\n")
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 2 :cursor 2)) ; col 1 on L1
                           :main 0))
          (setq kao--count 3)                     ; 3j
          (kao-down)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-cursor s) 12))    ; col 1 on L3 = 'j', NOT phantom (16)
            (should (eql (kao-sel-target s) 1)))) ; goal column preserved
      (kao-mode -1))))

;;;; Line-step extraction characterization (kao-motion--step-lines, #11)
;; These pin the FULL selection result (anchor + cursor + target, not cursor
;; alone) that `kao-motion--vertical' produces across the invisible-line skip and
;; the phantom-line clamp — the two behaviours the `kao-motion--step-lines (dir)'
;; helper owns.  They pass byte-identically before and after the extraction, so a
;; drift in the extracted line-step (ordering, a dropped guard) fails them.

(ert-deftest kao-motion-vertical-step-invisible-skip-preserves-full-sel ()
  "`j' across a plain `invisible t' overlay lands zero-width on the next visible
line at the goal column, carrying the resolved target — anchor, cursor, AND target
all pinned so the `kao-motion--step-lines' extraction stays byte-identical."
  (with-temp-buffer
    (insert "aaa\nhidden\nbbb\n")            ; aaa@1-3 hidden@5-10 bbb@12-14
    (let ((ov (make-overlay 5 11)))           ; "hidden" invisible, its \n visible
      (overlay-put ov 'invisible t))
    (let ((down (kao-motion-down (kao-sel-make :anchor 2 :cursor 2)))) ; col 1 on "aaa"
      (should (= (kao-sel-anchor down) 13))   ; zero-width on "bbb" col 1
      (should (= (kao-sel-cursor down) 13))
      (should (eql (kao-sel-target down) 1))))) ; resolved goal column carried

(ert-deftest kao-motion-vertical-step-phantom-clamp-preserves-full-sel ()
  "`j' on the last real line of a trailing-`\\n' buffer clamps to the same line (the
phantom line is not-a-line), staying zero-width at the goal column with the target
carried — anchor, cursor, AND target pinned so the `kao-motion--step-lines'
extraction stays byte-identical (buffer.cc:170-179)."
  (with-temp-buffer
    (insert "ab\ncd\n")                        ; a1 b2 \n3 c4 d5 \n6, phantom @7
    (let ((down (kao-motion-down (kao-sel-make :anchor 4 :cursor 4)))) ; col 0 on "cd"
      (should (= (kao-sel-anchor down) 4))      ; same-line no-op
      (should (= (kao-sel-cursor down) 4))
      (should (eql (kao-sel-target down) 0))))) ; resolved goal column carried

;;;; CJK vertical-motion break-before + avoid-eol backoff
;; Kakoune's `get_byte_to_column' stops BEFORE the char that would overshoot the
;; goal column (buffer_utils.cc:65-88): a goal column landing mid-wide-char
;; resolves ON that char, never one past it.  Emacs's `move-to-column' instead
;; moves PAST a straddled wide char, so `j'/`k' overshoots; the break-before rule
;; backs off.  The avoid-eol backoff likewise steps one CHARACTER (not one
;; display column) so a CJK-terminated line lands on the last real char, not the
;; newline (a column-based `move-to-column (1- col)' re-straddles the wide char
;; and parks on the eol again).

(ert-deftest kao-motion-vertical-cjk-break-before-wide-char ()
  "`j' with a goal column mid-wide-char lands ON the wide char, not one past it:
`get_byte_to_column' stops before the overshooting char (buffer_utils.cc:65-88).
Real Kakoune `ggljcX' on \"ab\\n漢字x\" yields \"X字x\" — the cursor sat on 漢."
  ;; "ab\n漢字x": a1 b2 \n3 漢4 字5 x6.  漢/字 are display-width 2.
  (kao-motion-tests--in "ab\n漢字x"
    (let ((down (kao-motion-down (kao-sel-make :anchor 2 :cursor 2)))) ; col 1 on "ab"
      (should (= (kao-sel-cursor down) 4)))))          ; ON 漢, NOT 字 (5)

(ert-deftest kao-motion-vertical-cjk-avoid-eol-backs-off-to-char ()
  "The avoid-eol backoff steps one CHARACTER: `j' onto a line ending in a wide
char lands on that last real char with `eolp' nil, never the newline.  A
column-based `move-to-column (1- col)' would re-straddle 字 and park on the eol
again (buffer_utils.cc:65-88); real Kakoune clamps to `max_column-1'."
  ;; "abcde\n漢字\nxyz\n": a1 b2 c3 d4 e5 \n6 漢7 字8 \n9 x10 y11 z12 \n13.
  (kao-motion-tests--in "abcde\n漢字\nxyz\n"
    (let ((down (kao-motion-down (kao-sel-make :anchor 5 :cursor 5)))) ; col 4 on "abcde"
      (should (= (kao-sel-cursor down) 8))             ; ON 字, last real char
      (goto-char (kao-sel-cursor down))
      (should-not (eolp)))))                            ; not parked on the newline

;;;; Empty-buffer motion safety
;; Defense-in-depth: every motion command is a safe no-op in an empty buffer
;; (`(= (point-min) (point-max))'), signalling nothing — no `args-out-of-range'
;; / `wholenump' — and leaving the single selection intact at position 1.  The
;; phantom-line clamp's `(not (bobp))' guard and the
;; all-dropped-leaves-the-list-unchanged rule are what keep these safe; this pin
;; locks that in so a future motion edit can't regress empty-buffer handling.

(ert-deftest kao-motion-empty-buffer-commands-are-safe-noops ()
  "Each motion command (h/l/j/k, w/b/e, x) is a safe no-op in an empty buffer:
no signal, single selection preserved at position 1.  A raised error would abort
the `funcall' and fail the test; the surviving-selection asserts confirm the
no-op.  The w/b/e drop path returns nil at the exhausted edge and the
single-selection all-dropped rule leaves the list unchanged (normal.cc:1299)."
  (dolist (cmd '(kao-left kao-right kao-down kao-up
                 kao-word-forward kao-word-backward kao-word-end kao-line))
    (with-temp-buffer
      (should (= (point-min) (point-max)))            ; empty buffer
      (kao-mode 1)
      (unwind-protect
          (progn
            (setq kao--sels (kao-sels-make
                             :list (list (kao-sel-make :anchor 1 :cursor 1))
                             :main 0))
            (funcall cmd)                              ; must not signal
            (let ((lst (kao-sels-list kao--sels)))
              (should (= 1 (length lst)))             ; single selection survives
              (should (= (kao-sel-cursor (nth 0 lst)) 1))
              (should (= (kao-sel-anchor (nth 0 lst)) 1))))
        (kao-mode -1)))))

;;;; Word motions ("foo bar baz": f1 o2 o3 _4 b5 a6 r7 _8 b9 a10 z11)

(ert-deftest kao-motion-word-forward-selects-word-and-trailing-blank ()
  "`w' selects the word plus trailing whitespace (forward)."
  (let ((s (kao-motion-tests--apply "foo bar baz" 1 #'kao-motion-word-forward)))
    (should (= (kao-sel-anchor s) 1))
    (should (= (kao-sel-cursor s) 4))             ; covers "foo "
    (should (kao-sel-forward-p s))))

(ert-deftest kao-motion-word-end-selects-to-word-end ()
  "`e' selects to the end of the word (forward), no trailing blank."
  (let ((s (kao-motion-tests--apply "foo bar baz" 1 #'kao-motion-word-end)))
    (should (= (kao-sel-anchor s) 1))
    (should (= (kao-sel-cursor s) 3))))           ; covers "foo"

(ert-deftest kao-motion-word-backward-is-backward-to-prev-word ()
  "`b' selects backward, landing the cursor on the previous word start."
  (let ((s (kao-motion-tests--apply "foo bar baz" 9 #'kao-motion-word-backward)))
    (should (= (kao-sel-cursor s) 5))             ; cursor on 'b' of "bar"
    (should (= (kao-sel-anchor s) 8))
    (should-not (kao-sel-forward-p s))
    (should (= (kao-sel-min s) 5))
    (should (= (kao-sel-max s) 8))))

(ert-deftest kao-motion-word-selectors-return-nil-at-edge ()
  "The word selectors return nil at an exhausted buffer edge —
the nullopt the dispatch turns into a dropped selection, Kakoune's `select'
returning nothing.  Previously they returned a `kao-sel-copy' (a stuck no-op)."
  (kao-motion-tests--in "foo bar baz"                  ; 11 chars, last = pos 11
    (should (null (kao-motion-word-forward (kao-sel-make :anchor 11 :cursor 11))))
    (should (null (kao-motion-word-end     (kao-sel-make :anchor 11 :cursor 11)))))
  (kao-motion-tests--in "foo"
    (should (null (kao-motion-word-backward (kao-sel-make :anchor 1 :cursor 1))))))

(ert-deftest kao-motion-word-forward-last-char-single-noop ()
  "`w' on the last char with a SINGLE selection is a no-op: the selector drops
it and all-dropped leaves the list unchanged (no_selections_remaining), so
single-cursor behaviour is observably unchanged from the old copy fallback."
  (with-temp-buffer
    (insert "foo bar baz")                             ; f1..z11
    (goto-char 11)
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-word-forward)
          (let ((lst (kao-sels-list kao--sels)))
            (should (= 1 (length lst)))
            (should (= (kao-sel-cursor (nth 0 lst)) 11))
            (should (= (kao-sel-anchor (nth 0 lst)) 11))))
      (kao-mode -1))))

(ert-deftest kao-motion-word-forward-counted-overshoot-noop ()
  "counted-overshoot corollary: a counted `w' that overshoots
the buffer is a total NO-OP for a single cursor, not Kakoune's partial-progress-
plus-error.  The fold in `kao--repeat' short-circuits on the first nullopt
\ and returns nil, so the single selection drops and all-dropped
leaves the list unchanged (`no_selections_remaining') — observably cleaner than
advancing partway and signalling, and identical to Kakoune whenever the count
is satisfiable."
  (with-temp-buffer
    (insert "one two three\n")                    ; o1 .. e13, nl14
    (goto-char 1)
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make          ; single cursor at buffer start
                           :list (list (kao-sel-make :anchor 1 :cursor 1))
                           :main 0))
          (let ((kao--count 9)) (kao-word-forward))  ; 9 ≫ the ~3 words present
          (let ((lst (kao-sels-list kao--sels)))
            (should (= 1 (length lst)))
            (should (= (kao-sel-cursor (nth 0 lst)) 1))    ; cursor unchanged
            (should (= (kao-sel-anchor (nth 0 lst)) 1))))  ; anchor unchanged
      (kao-mode -1))))

(ert-deftest kao-motion-word-forward-drops-exhausted-multi ()
  "With two cursors, `w' drops the one stuck on the buffer's final char and
adjusts the main index; the other advances."
  (with-temp-buffer
    (insert "foo bar baz")                             ; f1..z11
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)     ; advances
                                       (kao-sel-make :anchor 11 :cursor 11))  ; drops
                           :main 1))                    ; main = the doomed cursor
          (kao-word-forward)
          (let ((lst (kao-sels-list kao--sels)))
            (should (= 1 (length lst)))
            (should (= (kao-sel-anchor (nth 0 lst)) 1))
            (should (= (kao-sel-cursor (nth 0 lst)) 4)) ; "foo " incl. trailing blank
            (should (= 0 (kao-sels-main kao--sels)))))  ; main 1 -> 0
      (kao-mode -1))))

(ert-deftest kao-motion-extend-word-forward-drops-exhausted-multi ()
  "`W' (extend) also drops a cursor whose word motion is exhausted mid-fold
(the count-folding extend path returns nil on a nullopt), keeping
w/W symmetry; the survivor keeps its anchor (extend)."
  (with-temp-buffer
    (insert "foo bar baz")
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)
                                       (kao-sel-make :anchor 11 :cursor 11))
                           :main 1))
          (kao-extend-word-forward)
          (let ((lst (kao-sels-list kao--sels)))
            (should (= 1 (length lst)))
            (should (= (kao-sel-anchor (nth 0 lst)) 1))  ; anchor kept (extend)
            (should (= (kao-sel-cursor (nth 0 lst)) 4))
            (should (= 0 (kao-sels-main kao--sels)))))
      (kao-mode -1))))

;;;; x — expand selections to whole lines (select_lines)

(ert-deftest kao-motion-line-selects-whole-line ()
  "`x' selects bol..newline of the current line."
  ;; "ab\ncd\n": a1 b2 \n3 c4 d5 \n6
  (let ((s (kao-motion-tests--apply "ab\ncd\n" 1 #'kao-motion-line)))
    (should (= (kao-sel-anchor s) 1))
    (should (= (kao-sel-cursor s) 3))))           ; a,b,\n

(ert-deftest kao-motion-line-is-idempotent ()
  "A second `x' on a full-line selection is a no-op.
The reference keymap binds `x' to bare `select_lines' (normal.cc:2544) —
the pre-2019 select-line-then-extend-down is gone upstream."
  (kao-motion-tests--in "ab\ncd\n"
    (let* ((first (kao-motion-line (kao-sel-make :anchor 1 :cursor 1)))
           (second (kao-motion-line first)))
      (should (= (kao-sel-anchor first) 1))
      (should (= (kao-sel-cursor first) 3))
      (should (= (kao-sel-anchor second) (kao-sel-anchor first)))
      (should (= (kao-sel-cursor second) (kao-sel-cursor first))))))

(ert-deftest kao-motion-line-expands-forward-multiline ()
  "`x' on a forward selection spanning two lines covers BOTH full lines."
  ;; "ab\ncd\nef\n": a1 b2 \n3 c4 d5 \n6 e7 f8 \n9
  (kao-motion-tests--in "ab\ncd\nef\n"
    (let ((s (kao-motion-line (kao-sel-make :anchor 2 :cursor 4))))
      (should (= (kao-sel-anchor s) 1))           ; bol of line 1
      (should (= (kao-sel-cursor s) 6)))))        ; newline of line 2

(ert-deftest kao-motion-line-expands-backward-multiline ()
  "`x' on a BACKWARD selection (cursor above anchor) covers all touched
lines, direction preserved — the max end expands to ITS line's newline, not
the cursor's (selectors.cc:1043 mutates through min/max references).
Regression: the old code took eol(cursor), collapsing a backward selection
to the cursor's single line (user-reported 2026-06-13)."
  (kao-motion-tests--in "ab\ncd\nef\n"
    (let ((s (kao-motion-line (kao-sel-make :anchor 5 :cursor 1))))
      (should (= (kao-sel-anchor s) 6))           ; newline of line 2 (max side)
      (should (= (kao-sel-cursor s) 1)))))        ; bol of line 1 (still backward)

;;;; Integration — live minor mode driven by the commands

(ert-deftest kao-motion-integration-live-buffer ()
  "Through the real mode: `w' selects \"foo \", then `l' steps onto 'b'."
  (with-temp-buffer
    (insert "foo bar")                            ; f1 o2 o3 _4 b5 a6 r7
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-word-forward)
          (let ((s (nth (kao-sels-main kao--sels) (kao-sels-list kao--sels))))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 4)))
          (should (= (point) 4))
          (should (= (char-after (point)) ?\s))   ; cursor on the space (char 4)
          (should (= (region-beginning) 1))
          (should (= (region-end) 4))             ; region "foo"; cursor shows space
          (kao-right)
          (let ((s (nth (kao-sels-main kao--sels) (kao-sels-list kao--sels))))
            (should (= (kao-sel-cursor s) 5)))
          (should (= (point) 5))
          (should (= (char-after (point)) ?b)))
      (kao-mode -1))))

;;;; Count-aware motions (kao--count drives repeats)

(ert-deftest kao-motion-count-repeats-right ()
  "`3l' moves the cursor three chars right (one map pass)."
  (with-temp-buffer
    (insert "abcdef")                             ; a1 b2 c3 d4 e5 f6
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--count 3)
          (kao-right)
          (let ((s (nth (kao-sels-main kao--sels) (kao-sels-list kao--sels))))
            (should (= (kao-sel-cursor s) 4))))   ; 1 -> 4 ('d')
      (kao-mode -1))))

(ert-deftest kao-motion-count-repeats-word ()
  "`2w' selects through the second word, leaving the cursor on its trailing blank."
  (with-temp-buffer
    (insert "foo bar baz")                        ; f1..o3 _4 b5..r7 _8 b9..z11
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--count 2)
          (kao-word-forward)
          (let ((s (nth (kao-sels-main kao--sels) (kao-sels-list kao--sels))))
            (should (= (kao-sel-anchor s) 5))     ; second word starts at 'b'
            (should (= (kao-sel-cursor s) 8))))   ; trailing blank after "bar"
      (kao-mode -1))))

(ert-deftest kao-motion-count-zero-acts-as-one ()
  "With no count, a motion runs exactly once."
  (with-temp-buffer
    (insert "abcdef")
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (progn
          (should (= kao--count 0))
          (kao-right)
          (let ((s (nth (kao-sels-main kao--sels) (kao-sels-list kao--sels))))
            (should (= (kao-sel-cursor s) 2))))   ; 1 -> 2, single step
      (kao-mode -1))))

;;;; Extend motions (H/J/K/L move_cursor<Extend>, W/B/E select<Extend>)

(defun kao-motion-tests--main (sels)
  "Return the main `kao-sel' of SELS."
  (nth (kao-sels-main sels) (kao-sels-list sels)))

(ert-deftest kao-motion-extend-right-keeps-anchor ()
  "`L' moves the cursor right but keeps the anchor (the selection grows)."
  (with-temp-buffer
    (insert "abcdef")                             ; a1 b2 c3 d4 e5 f6
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-extend-right)                      ; from [1,1] -> [1,2]
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))     ; anchor pinned
            (should (= (kao-sel-cursor s) 2)))    ; cursor advanced
          (kao-extend-right)                      ; -> [1,3]
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 3))))
      (kao-mode -1))))

(ert-deftest kao-motion-extend-left-into-backward-keeps-anchor ()
  "`H' from a single char makes a backward selection, anchor fixed."
  (with-temp-buffer
    (insert "abcdef")
    (goto-char 4)                                 ; on 'd'
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-extend-left)                       ; [4,4] -> backward [4,3]
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 4))     ; anchor pinned
            (should (= (kao-sel-cursor s) 3))
            (should-not (kao-sel-forward-p s))))  ; now backward
      (kao-mode -1))))

(ert-deftest kao-motion-extend-right-count-folds ()
  "`3L' extends three chars in one command (count fold)."
  (with-temp-buffer
    (insert "abcdef")
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--count 3)
          (kao-extend-right)                      ; [1,1] -> [1,4]
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 4))))
      (kao-mode -1))))

(ert-deftest kao-motion-extend-word-forward-merges ()
  "`W' extends to the next word end, anchor pulled to the running min (merge)."
  (with-temp-buffer
    (insert "foo bar baz")                        ; f1 o2 o3 _4 b5 a6 r7 _8 b9 a10 z11
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (progn
          ;; Replace w from pos 1 selects "foo " ([1,4]); W from the same cursor
          ;; merges that region onto [1,1] -> anchor min(1,1)=1, cursor 4.
          (kao-extend-word-forward)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 4)))
          (kao-extend-word-forward)               ; extend over "bar " -> cursor 8
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))     ; anchor still the min
            (should (= (kao-sel-cursor s) 8))))
      (kao-mode -1))))

(ert-deftest kao-motion-extend-multi-selection ()
  "Extend maps over every selection (H/J/K/L are list transforms)."
  (with-temp-buffer
    (insert "abcdefghij")                         ; positions 1..10
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 2 :cursor 2)
                                       (kao-sel-make :anchor 6 :cursor 6))
                           :main 0))
          (kao-extend-right)                      ; [2,3] and [6,7]
          (let ((lst (kao-sels-list kao--sels)))
            (should (= 2 (length lst)))
            (should (equal (mapcar #'kao-sel-anchor lst) '(2 6)))
            (should (equal (mapcar #'kao-sel-cursor lst) '(3 7)))))
      (kao-mode -1))))

(ert-deftest kao-motion-extend-bindings-present ()
  "The uppercase extend keys are bound in normal state."
  (dolist (k '("H" "J" "K" "L" "W" "B" "E"))
    (should (commandp (lookup-key kao-normal-state-map k)))))

;;;; Selection-direction ops (; reduce, <a-;> flip, <a-:> ensure-forward)

(ert-deftest kao-motion-reduce-collapses-to-cursor ()
  "`;' reduces every selection to [cursor,cursor], keeping count/order/main, no merge."
  (with-temp-buffer
    (insert "abcdefghij")                           ; positions 1..10
    (kao-mode 1)
    (unwind-protect
        (progn
          ;; two forward sels [1,3] and [5,7]; main is the second
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 3)
                                       (kao-sel-make :anchor 5 :cursor 7))
                           :main 1))
          (kao-reduce)
          (let ((lst (kao-sels-list kao--sels)))
            (should (= 2 (length lst)))             ; not merged
            (should (= 1 (kao-sels-main kao--sels)))
            (should (equal (mapcar #'kao-sel-anchor lst) '(3 7)))
            (should (equal (mapcar #'kao-sel-cursor lst) '(3 7)))))
      (kao-mode -1))))

(ert-deftest kao-motion-flip-swaps-anchor-cursor ()
  "`<a-;>' swaps anchor and cursor; a forward sel becomes backward, min/max fixed."
  (with-temp-buffer
    (insert "abcdef")
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 2 :cursor 5))
                           :main 0))
          (kao-flip-selections)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 5))
            (should (= (kao-sel-cursor s) 2))
            (should-not (kao-sel-forward-p s))
            (should (= (kao-sel-min s) 2))          ; min/max unchanged
            (should (= (kao-sel-max s) 5))))
      (kao-mode -1))))

(ert-deftest kao-motion-ensure-forward-orients-selection ()
  "`<a-:>' makes a backward selection forward (anchor=min, cursor=max)."
  (with-temp-buffer
    (insert "abcdef")
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 5 :cursor 2)   ; backward
                                       (kao-sel-make :anchor 1 :cursor 3))  ; already fwd
                           :main 0))
          (kao-ensure-forward)
          (let ((lst (kao-sels-list kao--sels)))
            (should (equal (mapcar #'kao-sel-anchor lst) '(2 1)))
            (should (equal (mapcar #'kao-sel-cursor lst) '(5 3)))
            (should (cl-every #'kao-sel-forward-p lst))))
      (kao-mode -1))))

(ert-deftest kao-motion-select-dir-bindings-present ()
  "`;' `<a-;>' `<a-:>' are bound to commands in normal state."
  (should (eq (lookup-key kao-normal-state-map ";") #'kao-reduce))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-;")) #'kao-flip-selections))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-:")) #'kao-ensure-forward)))

;;;; WORD motions — punctuation folds into word (categorize<WORD>)
;; "foo.bar baz": f1 o2 o3 .4 b5 a6 r7 _8 b9 a10 z11

(ert-deftest kao-motion-WORD-forward-spans-punctuation ()
  "<a-w> treats \"foo.bar\" as one WORD; plain `w' stops at the punctuation."
  ;; plain word: from 1 selects "foo" [1,3]
  (let ((s (kao-motion-tests--apply "foo.bar baz" 1 #'kao-motion-word-forward)))
    (should (= (kao-sel-cursor s) 3)))
  ;; WORD: from 1 selects "foo.bar " [1,8] (cursor on the trailing space)
  (let ((kao-motion--big-word t))
    (let ((s (kao-motion-tests--apply "foo.bar baz" 1 #'kao-motion-word-forward)))
      (should (= (kao-sel-anchor s) 1))
      (should (= (kao-sel-cursor s) 8)))))

(ert-deftest kao-motion-WORD-end-spans-punctuation ()
  "<a-e> selects to the end of the WORD \"foo.bar\" (no trailing blank)."
  (let ((kao-motion--big-word t))
    (let ((s (kao-motion-tests--apply "foo.bar baz" 1 #'kao-motion-word-end)))
      (should (= (kao-sel-anchor s) 1))
      (should (= (kao-sel-cursor s) 7)))))            ; covers "foo.bar"

(ert-deftest kao-motion-WORD-backward-spans-punctuation ()
  "<a-b> from the trailing word selects back over the whole previous WORD."
  ;; from pos 9 ('b' of "baz"): previous WORD start is 'f' at 1, backward sel.
  (let ((kao-motion--big-word t))
    (let ((s (kao-motion-tests--apply "foo.bar baz" 9 #'kao-motion-word-backward)))
      (should (= (kao-sel-cursor s) 1))               ; cursor on 'f'
      (should-not (kao-sel-forward-p s)))))

(ert-deftest kao-motion-WORD-forward-live ()
  "Through the real mode, `<a-w>' selects the whole WORD \"foo.bar \"."
  (with-temp-buffer
    (insert "foo.bar baz")
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-WORD-forward)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 8)))
          (should (= (point) 8))
          ;; the dynamic WORD flag must not leak past the command
          (should-not kao-motion--big-word))
      (kao-mode -1))))

(ert-deftest kao-motion-WORD-bindings-present ()
  "The six WORD keys are bound to commands in normal state."
  (dolist (kc (list (cons (kbd "M-w") #'kao-WORD-forward)
                    (cons (kbd "M-b") #'kao-WORD-backward)
                    (cons (kbd "M-e") #'kao-WORD-end)
                    (cons (kbd "M-W") #'kao-extend-WORD-forward)
                    (cons (kbd "M-B") #'kao-extend-WORD-backward)
                    (cons (kbd "M-E") #'kao-extend-WORD-end)))
    (should (eq (lookup-key kao-normal-state-map (car kc)) (cdr kc)))))

;;;; Find char forward (f/t) — select_to

(ert-deftest kao-motion-find-forward-selector ()
  "`kao-find--forward' returns the next-char cursor position (incl/excl/count)."
  (kao-motion-tests--in "hello world"        ; h1 e2 l3 l4 o5 _6 w7 o8 r9 l10 d11
    (should (= (kao-find--forward 1 ?o 1 t) 5))      ; f o (inclusive)
    (should (= (kao-find--forward 1 ?o 1 nil) 4))    ; t o (exclusive)
    (should (null (kao-find--forward 1 ?z 1 t))))    ; not found
  (kao-motion-tests--in "aXaXaX"             ; a1 X2 a3 X4 a5 X6
    (should (= (kao-find--forward 1 ?a 2 t) 5))))    ; 2f a -> second 'a'

(ert-deftest kao-motion-find-to-char-apply-live ()
  "`f' over the live mode anchors at the old cursor and lands on the char."
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--select-to-char-apply ?o 1 nil t)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 5))))
      (kao-mode -1))))

(ert-deftest kao-motion-find-to-char-multi ()
  "`f' maps over every selection (each finds its own next char)."
  (with-temp-buffer
    (insert "axbxcx")                         ; a1 x2 b3 x4 c5 x6
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)   ; 'a'
                                       (kao-sel-make :anchor 3 :cursor 3))  ; 'b'
                           :main 0))
          (kao--select-to-char-apply ?x 1 nil t)
          (let ((lst (kao-sels-list kao--sels)))
            (should (= 2 (length lst)))
            (should (equal (mapcar #'kao-sel-cursor lst) '(2 4)))))
      (kao-mode -1))))

(ert-deftest kao-motion-find-to-char-drops-not-found ()
  "A selection whose char is not found is dropped; main is adjusted."
  (with-temp-buffer
    (insert "axbc")                           ; a1 x2 b3 c4
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)   ; 'a' finds x
                                       (kao-sel-make :anchor 3 :cursor 3))  ; 'b': no x after
                           :main 1))
          (kao--select-to-char-apply ?x 1 nil t)
          (let ((lst (kao-sels-list kao--sels)))
            (should (= 1 (length lst)))
            (should (= (kao-sel-cursor (nth 0 lst)) 2))
            (should (= 0 (kao-sels-main kao--sels)))))   ; 1 -> 0
      (kao-mode -1))))

(ert-deftest kao-motion-find-to-char-single-not-found-unchanged ()
  "A single selection whose char is not found is left unchanged (no_selections)."
  (with-temp-buffer
    (insert "abc")
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--select-to-char-apply ?z 1 nil t)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 1))))
      (kao-mode -1))))

(ert-deftest kao-motion-find-to-char-command-reads-char ()
  "`kao-find-to-char' reads one target char and applies the find.
The char is read via `kao--read-key' (macro-safe) not bare `read-char', so
the seam stubs `read-key'."
  (with-temp-buffer
    (insert "hello")                          ; h1 e2 l3 l4 o5
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?l)))
          (kao-find-to-char)                  ; f l -> first 'l' at 3
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 3))))
      (kao-mode -1))))

;;;; F/t macro-recording + Return normalization (f/t-site)
;;
;; kao--select-to-char reads its target char through
;; `(kao--key-codepoint (kao--read-key ...))': kao--read-key
;; keeps Q-macro recording in lockstep (the target char lands on the recorded
;; stream), and kao--key-codepoint maps Return to newline (keys.cc:46-53) so
;; `f<ret>'/`t<ret>' target the newline instead of a `\r' that never exists in a
;; decoded buffer.  Macro pins live HERE (test/kao-macro-tests.el is frozen).

(ert-deftest kao-motion-find-to-char-records-target-char ()
  "`f' records its target char onto the Q-recording stream (desync fix).
The recording hook captures only `this-command-keys-vector' (the `f'); the char
`f' consumes mid-command must be pushed by `kao--read-key', else replay eats the
next macro key.  `kao--macro-recorded-keys' is a list of one-key vectors."
  (with-temp-buffer
    (insert "a b")                            ; a1 _2 b3 ; 'b' is the target
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (let ((kao--macro-recording-reg ?@)
              (kao--macro-recorded-keys nil)
              (kao--macro-replaying nil))
          (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?b)))
            (kao-find-to-char))
          (should (equal kao--macro-recorded-keys (list [?b]))))
      (kao-mode -1))))

(ert-deftest kao-motion-find-to-char-records-cancel-key ()
  "A cancelled `f' still records its cancel key (input_handler.cc:1712).
GUI Escape arrives as the symbol `escape' (non-`characterp'): the cancel guard
drops the find with no selection change, but `kao--read-key' has already pushed
it so replay re-eats it."
  (with-temp-buffer
    (insert "abc")
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (let ((kao--macro-recording-reg ?@)
              (kao--macro-recorded-keys nil)
              (kao--macro-replaying nil))
          (cl-letf (((symbol-function 'read-key) (lambda (&rest _) 'escape)))
            (kao-find-to-char))
          (should (equal kao--macro-recorded-keys (list [escape])))
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))   ; selection unchanged (cancel)
            (should (= (kao-sel-cursor s) 1))))
      (kao-mode -1))))

(ert-deftest kao-motion-find-to-char-maps-return-to-newline ()
  "`f<ret>' targets the newline: Key::codepoint maps Return to `\\n' (keys.cc:46-53,
select_to_next_char normal.cc:1705).  \"ab\\ncd\": from pos 1, `f RET' lands on
the newline at pos 3 (inclusive)."
  (with-temp-buffer
    (insert "ab\ncd")                          ; a1 b2 \n3 c4 d5
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\r)))
          (kao-find-to-char)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 3))   ; on the newline (inclusive)
            (should-not (string-search "\r" (buffer-string)))))  ; no ^M
      (kao-mode -1))))

(ert-deftest kao-motion-find-until-char-maps-return-to-newline ()
  "`t<ret>' stops just before the newline (exclusive `select_to').
\"ab\\ncd\": from pos 1, `t RET' lands on pos 2 (the char before the newline)."
  (with-temp-buffer
    (insert "ab\ncd")                          ; a1 b2 \n3 c4 d5
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\r)))
          (kao-find-until-char)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 2))))  ; just before the newline
      (kao-mode -1))))

(ert-deftest kao-motion-find-forward-bindings-present ()
  "`f'/`t' are bound in normal state."
  (should (eq (lookup-key kao-normal-state-map "f") #'kao-find-to-char))
  (should (eq (lookup-key kao-normal-state-map "t") #'kao-find-until-char)))

;;;; Extend find char forward (F/T) — select_to in Extend mode

(ert-deftest kao-motion-extend-to-char-keeps-anchor ()
  "`F' extends the cursor to the char but keeps the anchor (vs `f' which resets it)."
  (with-temp-buffer
    (insert "hello world")                    ; o at 5
    (kao-mode 1)
    (unwind-protect
        (progn
          ;; F o over [1,3]: extend -> [1,5] (anchor 1 kept, cursor on 'o')
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 3))
                           :main 0))
          (kao--select-to-char-apply ?o 1 nil t t)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 5)))
          ;; contrast: f o over [1,3] replaces -> [3,5]
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 3))
                           :main 0))
          (kao--select-to-char-apply ?o 1 nil t nil)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 3))
            (should (= (kao-sel-cursor s) 5))))
      (kao-mode -1))))

(ert-deftest kao-motion-extend-until-char-exclusive ()
  "`T' extends to just before the char (exclusive), anchor kept."
  (with-temp-buffer
    (insert "hello world")
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 3))
                           :main 0))
          (kao--select-to-char-apply ?o 1 nil nil t)   ; T o -> [1,4]
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 4))))
      (kao-mode -1))))

(ert-deftest kao-motion-extend-to-char-not-found-unchanged ()
  "`F' on a single selection whose char is absent leaves it unchanged."
  (with-temp-buffer
    (insert "abc")
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--select-to-char-apply ?z 1 nil t t)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 1))))
      (kao-mode -1))))

(ert-deftest kao-motion-extend-forward-bindings-present ()
  "`F'/`T' are bound in normal state."
  (should (eq (lookup-key kao-normal-state-map "F") #'kao-extend-to-char))
  (should (eq (lookup-key kao-normal-state-map "T") #'kao-extend-until-char)))

;;;; Extend find char backward (<a-F>/<a-T>) — select_to_reverse in Extend mode

(ert-deftest kao-motion-extend-to-char-reverse-keeps-anchor ()
  "`<a-F>' extends the cursor back onto the char, keeping the anchor (backward)."
  (with-temp-buffer
    (insert "hello")                          ; h1 e2 l3 l4 o5
    (kao-mode 1)
    (unwind-protect
        (progn
          ;; forward [3,5] (anchor 3, cursor on 'o'); <a-F> h -> extend cursor to 'h'
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 3 :cursor 5))
                           :main 0))
          (kao--select-to-char-apply ?h 1 t t t)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 3))     ; anchor kept
            (should (= (kao-sel-cursor s) 1))     ; cursor on 'h'
            (should-not (kao-sel-forward-p s))))  ; now backward
      (kao-mode -1))))

(ert-deftest kao-motion-extend-reverse-count-near-bob-no-error ()
  "`<a-F>' with a count past the occurrences does not error near bob."
  (with-temp-buffer
    (insert "aXaXaX")
    (goto-char 6)
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--select-to-char-apply ?a 4 t t t)   ; single sel, all-not-found
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-cursor s) 6))))     ; unchanged, no error
      (kao-mode -1))))

(ert-deftest kao-motion-extend-backward-bindings-present ()
  "`<a-F>'/`<a-T>' are bound in normal state."
  (should (eq (lookup-key kao-normal-state-map (kbd "M-F")) #'kao-extend-to-char-reverse))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-T")) #'kao-extend-until-char-reverse)))

;;;; Find char backward (<a-f>/<a-t>) — select_to_reverse

(ert-deftest kao-motion-find-backward-selector ()
  "`kao-find--backward' returns the previous-char cursor position (incl/excl/bob)."
  (kao-motion-tests--in "hello"               ; h1 e2 l3 l4 o5
    (should (= (kao-find--backward 5 ?l 1 t) 4))     ; <a-f> l (inclusive)
    (should (= (kao-find--backward 5 ?l 1 nil) 5))   ; <a-t> l (exclusive, just after)
    (should (= (kao-find--backward 5 ?h 1 t) 1))     ; back to 'h'
    (should (null (kao-find--backward 5 ?z 1 t)))    ; not found
    (should (null (kao-find--backward 1 ?x 1 t)))))  ; at bob -> nil

(ert-deftest kao-motion-find-backward-count ()
  "A count selects the count-th previous occurrence."
  (kao-motion-tests--in "aXaXaX"              ; a1 X2 a3 X4 a5 X6
    (should (= (kao-find--backward 6 ?a 2 t) 3))))   ; from X6, 2nd 'a' back

(ert-deftest kao-motion-find-backward-count-overruns-bob ()
  "A backward count past the last occurrence returns nil (no crash at bob).
Regression: the COUNT-th `--end' must not step below `point-min' and call
`char-after' on an out-of-range position."
  (kao-motion-tests--in "aXaXaX"              ; only three 'a' (1,3,5)
    (should (null (kao-find--backward 6 ?a 4 t)))    ; 4th 'a' back runs off start
    (should (null (kao-find--backward 6 ?a 9 t))))
  (kao-motion-tests--in "abc"
    (should (null (kao-find--backward 3 ?a 2 t)))))  ; only one 'a', at point-min

(ert-deftest kao-motion-find-reverse-count-near-bob-no-error ()
  "Live `<a-f>' with a count past the occurrences drops/no-ops without erroring."
  (with-temp-buffer
    (insert "aXaXaX")
    (goto-char 6)                             ; on the last 'X'
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--count 4)
          (kao--select-to-char-apply ?a 4 t t)   ; single sel, all-not-found
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-cursor s) 6))     ; unchanged, no error
            (should (= (kao-sel-anchor s) 6))))
      (kao-mode -1))))

(ert-deftest kao-motion-find-to-char-reverse-apply-live ()
  "`<a-f>' produces a backward selection anchored at the old cursor."
  (with-temp-buffer
    (insert "hello")
    (goto-char 5)                             ; on 'o'
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--select-to-char-apply ?h 1 t t)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 5))
            (should (= (kao-sel-cursor s) 1))
            (should-not (kao-sel-forward-p s))))
      (kao-mode -1))))

(ert-deftest kao-motion-find-backward-bindings-present ()
  "`<a-f>'/`<a-t>' are bound in normal state."
  (should (eq (lookup-key kao-normal-state-map (kbd "M-f")) #'kao-find-to-char-reverse))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-t")) #'kao-find-until-char-reverse)))

;;;; Match (m/<a-m>/M/<a-M>)

(ert-deftest kao-motion-match-selector-on-opening ()
  "On an opening delimiter, `m' scans forward to the balanced closing."
  (kao-motion-tests--in "a(b)c"               ; a1 (2 b3 )4 c5
    (let ((s (kao-match--selector 2 t)))      ; cursor on '('
      (should (= (kao-sel-anchor s) 2))
      (should (= (kao-sel-cursor s) 4))
      (should (kao-sel-forward-p s)))))        ; forward selection

(ert-deftest kao-motion-match-selector-on-closing ()
  "On a closing delimiter, `m' scans backward to the balanced opening."
  (kao-motion-tests--in "a(b)c"               ; a1 (2 b3 )4 c5
    (let ((s (kao-match--selector 4 t)))      ; cursor on ')'
      (should (= (kao-sel-anchor s) 4))
      (should (= (kao-sel-cursor s) 2))
      (should-not (kao-sel-forward-p s)))))    ; backward selection

(ert-deftest kao-motion-match-selector-scans-to-delimiter ()
  "Off a delimiter, forward `m' scans forward to the first pair char.
From inside `(b)' with the cursor on `b', the first pair char forward is the
CLOSING `)', so the result is the backward span to the opening (Kakoune quirk)."
  (kao-motion-tests--in "a(b)c"               ; a1 (2 b3 )4 c5
    (let ((s (kao-match--selector 1 t)))      ; on 'a' -> finds '(' at 2
      (should (= (kao-sel-anchor s) 2))
      (should (= (kao-sel-cursor s) 4)))
    (let ((s (kao-match--selector 3 t)))      ; on 'b' -> finds ')' at 4 first
      (should (= (kao-sel-anchor s) 4))
      (should (= (kao-sel-cursor s) 2)))))

(ert-deftest kao-motion-match-selector-backward-find ()
  "`<a-m>' (forward=nil) scans BACKWARD for the delimiter.
From `b' the backward scan finds the opening `(' first, giving the forward span
to its closing — the mirror of `m' from the same spot."
  (kao-motion-tests--in "a(b)c"               ; a1 (2 b3 )4 c5
    (let ((s (kao-match--selector 3 nil)))    ; on 'b' -> backward finds '(' at 2
      (should (= (kao-sel-anchor s) 2))
      (should (= (kao-sel-cursor s) 4)))))

(ert-deftest kao-motion-match-selector-nesting ()
  "The balance scan respects nesting (skips inner pairs)."
  (kao-motion-tests--in "(a(b)c)"             ; (1 a2 (3 b4 )5 c6 )7
    (let ((s (kao-match--selector 1 t)))      ; outer '(' -> outer ')' at 7
      (should (= (kao-sel-anchor s) 1))
      (should (= (kao-sel-cursor s) 7)))
    (let ((s (kao-match--selector 3 t)))      ; inner '(' -> inner ')' at 5
      (should (= (kao-sel-anchor s) 3))
      (should (= (kao-sel-cursor s) 5)))))

(ert-deftest kao-motion-match-selector-unbalanced-nil ()
  "An unbalanced delimiter (no partner) returns nil (drop)."
  (kao-motion-tests--in "(ab"                 ; '(' never closed
    (should (null (kao-match--selector 1 t))))
  (kao-motion-tests--in "ab)"                 ; ')' never opened
    (should (null (kao-match--selector 3 t)))))

(ert-deftest kao-motion-match-selector-no-delimiter-nil ()
  "No matching-pair char at/after (or before) the cursor returns nil."
  (kao-motion-tests--in "abc"
    (should (null (kao-match--selector 1 t)))    ; forward find: none
    (should (null (kao-match--selector 3 nil))))) ; backward find: none

(ert-deftest kao-motion-match-selector-honours-custom-pairs ()
  "`kao-matching-pairs' drives which characters count as delimiters."
  (kao-motion-tests--in "a/b\\c"              ; a1 /2 b3 \4 c5
    (let ((kao-matching-pairs '(?/ ?\\)))     ; '/' opens, '\' closes
      (let ((s (kao-match--selector 2 t)))    ; on '/'
        (should (= (kao-sel-anchor s) 2))
        (should (= (kao-sel-cursor s) 4))))
    (should (null (kao-match--selector 2 t))))) ; default pairs: '/' not a delimiter

(ert-deftest kao-motion-match-apply-live ()
  "`m' over the live mode selects the matching span from the main cursor."
  (with-temp-buffer
    (insert "a(b)c")
    (goto-char (point-min))                   ; on 'a'
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--select-matching-apply t)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 2))
            (should (= (kao-sel-cursor s) 4))))
      (kao-mode -1))))

(ert-deftest kao-motion-match-multi ()
  "`m' maps over every selection (each finds its own pair)."
  (with-temp-buffer
    (insert "(a)(b)")                          ; (1 a2 )3 (4 b5 )6
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)   ; '('
                                       (kao-sel-make :anchor 4 :cursor 4))  ; '('
                           :main 0))
          (kao--select-matching-apply t)
          (let ((lst (kao-sels-list kao--sels)))
            (should (= 2 (length lst)))
            (should (equal (mapcar #'kao-sel-anchor lst) '(1 4)))
            (should (equal (mapcar #'kao-sel-cursor lst) '(3 6)))))
      (kao-mode -1))))

(ert-deftest kao-motion-match-drops-unmatched ()
  "A selection with no match is dropped; main is adjusted."
  (with-temp-buffer
    (insert "(a)xy")                           ; (1 a2 )3 x4 y5
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)   ; '(' matches
                                       (kao-sel-make :anchor 4 :cursor 4))  ; 'x' no delimiter
                           :main 1))
          (kao--select-matching-apply t)
          (let ((lst (kao-sels-list kao--sels)))
            (should (= 1 (length lst)))
            (should (= (kao-sel-anchor (nth 0 lst)) 1))
            (should (= (kao-sel-cursor (nth 0 lst)) 3))
            (should (= 0 (kao-sels-main kao--sels)))))   ; 1 -> 0
      (kao-mode -1))))

(ert-deftest kao-motion-match-single-unmatched-unchanged ()
  "A single selection with no match is left unchanged (no_selections_remaining)."
  (with-temp-buffer
    (insert "abc")
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--select-matching-apply t)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 1))))
      (kao-mode -1))))

(ert-deftest kao-motion-match-replace-bindings-present ()
  "`m'/`<a-m>' are bound in normal state."
  (should (eq (lookup-key kao-normal-state-map "m") #'kao-select-matching))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-m")) #'kao-select-matching-backward)))

(ert-deftest kao-motion-match-extend-keeps-anchor ()
  "`M' merges the matching span onto the selection, keeping the anchor (vs `m').
`m' from [1,1] would reset the anchor to the `(' at 2; `M' keeps it at 1."
  (with-temp-buffer
    (insert "a(b)c")                          ; a1 (2 b3 )4 c5
    (goto-char (point-min))                   ; sel [1,1] on 'a'
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-extend-matching)               ; M: extend to ')' at 4
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1)) ; anchor kept
            (should (= (kao-sel-cursor s) 4))))
      (kao-mode -1))))

(ert-deftest kao-motion-match-extend-backward ()
  "`<a-M>' extends backward to the matching opening, keeping the anchor."
  (with-temp-buffer
    (insert "a(b)c")                          ; a1 (2 b3 )4 c5
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 5 :cursor 5)) ; on 'c'
                           :main 0))
          (kao-extend-matching-backward)      ; <a-M>: back to ')' at 4 then '(' at 2
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 5)) ; anchor kept
            (should (= (kao-sel-cursor s) 2)) ; extended back to '('
            (should-not (kao-sel-forward-p s))))
      (kao-mode -1))))

(ert-deftest kao-motion-match-extend-drops-unmatched ()
  "Extend still drops a selection whose cursor has no match (drop precedes Extend)."
  (with-temp-buffer
    (insert "(a)xy")                           ; (1 a2 )3 x4 y5
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)   ; '(' matches
                                       (kao-sel-make :anchor 4 :cursor 4))  ; 'x' no delimiter
                           :main 1))
          (kao-extend-matching)               ; M
          (let ((lst (kao-sels-list kao--sels)))
            (should (= 1 (length lst)))
            (should (= (kao-sel-anchor (nth 0 lst)) 1))
            (should (= (kao-sel-cursor (nth 0 lst)) 3))
            (should (= 0 (kao-sels-main kao--sels)))))   ; 1 -> 0
      (kao-mode -1))))

(ert-deftest kao-motion-match-extend-bindings-present ()
  "`M'/`<a-M>' are bound in normal state."
  (should (eq (lookup-key kao-normal-state-map "M") #'kao-extend-matching))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-M")) #'kao-extend-matching-backward)))

;;;; Repeat last select (<a-.>) — repeat_last_select

(ert-deftest kao-motion-repeat-select-find-char ()
  "`<a-.>' repeats a find-char at the current cursor (records `f x', re-finds)."
  (with-temp-buffer
    (insert "axbxcx")                          ; a1 x2 b3 x4 c5 x6
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?x)))  ; seam
          (kao-find-to-char)                   ; f x: a@1 -> x@2, records last-select
          (should (= (kao-sel-cursor (kao-motion-tests--main kao--sels)) 2))
          (kao-repeat-select)                  ; <a-.>: from x@2 -> next x@4
          (should (= (kao-sel-cursor (kao-motion-tests--main kao--sels)) 4))
          (kao-repeat-select)                  ; again -> x@6
          (should (= (kao-sel-cursor (kao-motion-tests--main kao--sels)) 6)))
      (kao-mode -1))))

(ert-deftest kao-motion-repeat-select-extend-find ()
  "`<a-.>' after `F' repeats the EXTEND find (anchor kept, cursor advances)."
  (with-temp-buffer
    (insert "oaoao")                           ; o1 a2 o3 a4 o5
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?o)))  ; seam
          (kao-extend-to-char)                 ; F o: [1] -> [1,3] (extend, anchor 1)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 3)))
          (kao-repeat-select)                  ; <a-.>: extend on -> next o@5
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))  ; anchor still kept
            (should (= (kao-sel-cursor s) 5))))
      (kao-mode -1))))

(ert-deftest kao-motion-repeat-select-no-record-noop ()
  "`<a-.>' with nothing recorded is a no-op (message), selections unchanged."
  (with-temp-buffer
    (insert "abc")
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--last-select nil)
          (kao-repeat-select)
          (let ((s (kao-motion-tests--main kao--sels)))
            (should (= (kao-sel-anchor s) 1))
            (should (= (kao-sel-cursor s) 1))))
      (kao-mode -1))))

(ert-deftest kao-motion-repeat-select-binding-present ()
  "`<a-.>' is bound to `kao-repeat-select' in normal state."
  (should (eq (lookup-key kao-normal-state-map (kbd "M-.")) #'kao-repeat-select)))

(ert-deftest kao-motion-repeat-select-crosses-buffers ()
  "`<a-.>' repeats the last object-select across a buffer switch.
`kao--last-select' is client-global — a plain `defvar', the single-client
stance, same locality as the jump list and registers — not buffer-local, so the
inner-word closure recorded in buffer A re-selects the inner word at the cursor
in buffer B instead of failing with \"no selection to repeat\".  The recorded
closure is already buffer-agnostic; only the variable's locality changes."
  (let ((buf-a (generate-new-buffer " *kao-a*"))
        (buf-b (generate-new-buffer " *kao-b*")))
    (unwind-protect
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
          (with-current-buffer buf-a
            (insert "foo bar baz")               ; record <a-i>w here
            (kao-mode 1)
            (setq kao--sels (kao-sels-make
                             :list (list (kao-sel-make :anchor 1 :cursor 1))
                             :main 0))
            (kao-select-inner))                  ; sets the client-global thunk
          (with-current-buffer buf-b
            (insert "qux quux")                  ; q1 u2 x3 sp4 q5 u6 u7 x8
            (kao-mode 1)
            (setq kao--sels (kao-sels-make
                             :list (list (kao-sel-make :anchor 5 :cursor 5))
                             :main 0))
            (kao-repeat-select)                  ; <a-.>: A's thunk runs in B
            (should (= (kao-sel-min (kao--main-sel)) 5))    ; inner word "quux"
            (should (= (kao-sel-max (kao--main-sel)) 8))))
      (when (buffer-live-p buf-a) (with-current-buffer buf-a (kao-mode -1)))
      (when (buffer-live-p buf-b) (with-current-buffer buf-b (kao-mode -1)))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

;;;; <a-x> crop to whole lines (trim_partial_lines, selectors.cc:1058)
;; "aa\nbb\ncc\n": a1 a2 \n3 b4 b5 \n6 c7 c8 \n9

(defun kao-motion-tests--trim (content anchor cursor)
  "Apply `kao-motion--trim-partial-lines' in CONTENT to (ANCHOR . CURSOR).
Return (ANCHOR . CURSOR) of the result, or nil when dropped."
  (with-temp-buffer
    (insert content)
    (let ((r (kao-motion--trim-partial-lines
              (kao-sel-make :anchor anchor :cursor cursor))))
      (and r (cons (kao-sel-anchor r) (kao-sel-cursor r))))))

(ert-deftest kao-motion-trim-snaps-both-ends ()
  "A mid-line-to-mid-line span crops to the whole lines strictly inside."
  ;; (2..8) spans aa partially, bb fully, cc partially -> bb's line (4..6).
  (should (equal '(4 . 6) (kao-motion-tests--trim "aa\nbb\ncc\n" 2 8))))

(ert-deftest kao-motion-trim-whole-line-unchanged ()
  "An exact whole-line selection (x result, bol..newline) is unchanged."
  (should (equal '(4 . 6) (kao-motion-tests--trim "aa\nbb\ncc\n" 4 6))))

(ert-deftest kao-motion-trim-preserves-direction ()
  "A backward selection stays backward (the C++ adjusts via min/max refs)."
  (should (equal '(6 . 4) (kao-motion-tests--trim "aa\nbb\ncc\n" 8 2))))

(ert-deftest kao-motion-trim-drops-inside-one-line ()
  "A selection not covering any full line drops (min > max after adjust)."
  (should-not (kao-motion-tests--trim "aa\nbb\ncc\n" 4 5)))

(ert-deftest kao-motion-trim-drops-partial-on-first-line ()
  "A partial max end on the FIRST line drops (to_line_end.line == 0)."
  (should-not (kao-motion-tests--trim "aa\nbb\ncc\n" 1 2)))

(ert-deftest kao-motion-trim-first-line-full-kept ()
  "A full first line (1..newline) is kept — the line-0 guard only fires
when the max end must step back."
  (should (equal '(1 . 3) (kao-motion-tests--trim "aa\nbb\ncc\n" 1 3))))

(ert-deftest kao-motion-trim-eob-no-trailing-newline ()
  "On a newline-less final line, the last char counts as the line end."
  ;; "aa\nbb": a1 a2 \n3 b4 b5 — (4..5) is the whole final line.
  (should (equal '(4 . 5) (kao-motion-tests--trim "aa\nbb" 4 5)))
  ;; Partial final line still drops (min moves past eob).
  (should-not (kao-motion-tests--trim "aa\nbb" 5 5)))

(ert-deftest kao-motion-crop-lines-filters-list ()
  "`kao-crop-lines' drops non-covering selections and keeps survivors."
  (kao-motion-tests--in "aa\nbb\ncc\n"
    (setq-local kao-mode t)             ; guarded command : satisfy `kao--assert-mode'
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)   ; drop
                                 (kao-sel-make :anchor 4 :cursor 6))  ; keep
                     :main 1))
    (kao-crop-lines)
    (should (equal '((4 . 6))
                   (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                           (kao-sels-list kao--sels))))))

(ert-deftest kao-motion-crop-lines-all-drop-unchanged ()
  "When every selection drops, the list is left unchanged."
  (kao-motion-tests--in "aa\nbb\ncc\n"
    (setq-local kao-mode t)             ; guarded command : satisfy `kao--assert-mode'
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 4 :cursor 5)) :main 0))
    (kao-crop-lines)
    (should (equal '((4 . 5))
                   (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                           (kao-sels-list kao--sels))))))

(ert-deftest kao-motion-crop-lines-user-map-binding ()
  "The `SPC x' crop row is DROPPED (user decision 2026-06-13, \"cut no
need\").  The command itself survives, reachable via
\\[execute-extended-command] or a user rebinding."
  (require 'kao-menu)
  (should-not (lookup-key kao-user-map "x"))
  (should (commandp #'kao-crop-lines)))

(ert-deftest kao-motion-extra-word-chars-defcustom ()
  "`kao-extra-word-chars' shapes word categorisation (extra_word_chars port).
Default: `_' is word, `-' is punct; adding `-' makes \"foo-bar\" one word."
  ;; Default categorisation.
  (should (eq (kao-motion--category ?_) 'word))
  (should (eq (kao-motion--category ?-) 'punct))
  ;; "foo-bar": f1 o2 o3 -4 b5 a6 r7 — default `e' stops before the dash...
  (let ((s (kao-motion-tests--apply "foo-bar" 1 #'kao-motion-word-end)))
    (should (= (kao-sel-cursor s) 3)))
  ;; ...with `-' added, the word runs to the end (one word).
  (let ((kao-extra-word-chars '(?_ ?-)))
    (should (eq (kao-motion--category ?-) 'word))
    (let ((s (kao-motion-tests--apply "foo-bar" 1 #'kao-motion-word-end)))
      (should (= (kao-sel-cursor s) 7)))))

;;;; Unicode word categories (Kakoune categorize, unicode.hh:117-128)

(ert-deftest kao-motion-unicode-letters-are-word ()
  "Non-ASCII letters and digits classify as `word' (iswalnum equivalent)."
  (dolist (c '(?é ?漢 ?Я ?٣))                    ; Latin, CJK, Cyrillic, Arabic-Indic digit
    (should (eq (kao-motion--category c) 'word))))

(ert-deftest kao-motion-unicode-symbol-is-punct ()
  "A non-ASCII symbol (U+2192 RIGHTWARDS ARROW) classifies as `punct'."
  (should (eq (kao-motion--category #x2192) 'punct)))

(ert-deftest kao-motion-unicode-blanks-are-blank ()
  "Kakoune's Unicode horizontal blanks classify as `blank'."
  (dolist (c '(#x00A0 #x2003 #x3000 #x200A #xFEFF))
    (should (eq (kao-motion--category c) 'blank))))

(ert-deftest kao-motion-formfeed-is-blank ()
  "\\f is a horizontal blank (`is_horizontal_blank', unicode.hh:25-27);
\\r and \\v are not (categorize tests only is_eol/is_horizontal_blank)."
  (should (eq (kao-motion--category ?\f) 'blank))
  (should (eq (kao-motion--category ?\r) 'punct))
  (should (eq (kao-motion--category ?\v) 'punct)))

(ert-deftest kao-motion-unicode-big-word-blank-stays-blank ()
  "WORD mode: a Unicode blank stays `blank'; a symbol folds into `word'.
Faithful to `categorize' testing is_horizontal_blank BEFORE the WORD
short-circuit (unicode.hh:121-125)."
  (let ((kao-motion--big-word t))
    (should (eq (kao-motion--category #x00A0) 'blank))
    (should (eq (kao-motion--category #x2192) 'word))))

(ert-deftest kao-motion-unicode-extra-word-chars ()
  "A non-ASCII member of `kao-extra-word-chars' classifies as `word'."
  (should (eq (kao-motion--category ?·) 'punct))  ; U+00B7, gen-cat Po
  (let ((kao-extra-word-chars '(?_ ?·)))
    (should (eq (kao-motion--category ?·) 'word))))

(ert-deftest kao-motion-unicode-word-forward-walk ()
  "`w' walks through an accented word as one word (plus trailing blank)."
  ;; "café crème": c1 a2 f3 é4 SPC5 c6 r7 è8 m9 e10
  (let ((s (kao-motion-tests--apply "café crème" 1 #'kao-motion-word-forward)))
    (should (= (kao-sel-anchor s) 1))
    (should (= (kao-sel-cursor s) 5)))            ; covers "café " — é is word
  (let ((s (kao-motion-tests--apply "café crème" 6 #'kao-motion-word-end)))
    (should (= (kao-sel-cursor s) 10))))          ; "crème" to its last char

(ert-deftest kao-motion-unicode-combining-marks-are-word ()
  "Combining marks (Mn/Mc/Me) classify as `word', matching the glibc
`iswalnum'/Unicode-Alphabetic direction Kakoune runs on (unicode.hh:75-82;
Other_Alphabetic marks are alnum on the reference platform).  Without this the
vowel signs / anusvara / viramas that build Indic and decomposed words read as
`punct', fragmenting a word."
  ;; Bengali AA (Mc), I (Mc), anusvara (Mc), U (Mn); Devanagari virama (Mn).
  (dolist (c (list #x09BE #x09BF #x0982 #x09C1 #x094D))
    (should (eq (kao-motion--category c) 'word))))

(ert-deftest kao-motion-unicode-word-forward-keeps-indic-word-whole ()
  "`w' keeps a Bengali word whole (plus trailing blank): with combining marks in
the word set `বাংলা' is one word, not the `াং' mark fragment (unicode.hh:75-82)."
  ;; "বাংলা x": ব1 া2 ং3 ল4 া5 SPC6 x7 — বাংলা is ব + AA + anusvara + ল + AA.
  (let ((s (kao-motion-tests--apply "বাংলা x" 1 #'kao-motion-word-forward)))
    (should (= (kao-sel-anchor s) 1))
    (should (= (kao-sel-cursor s) 6))             ; covers "বাংলা " — whole word
    (should (kao-sel-forward-p s))))

(ert-deftest kao-motion-unicode-table-caches ()
  "The lazy table caches the computed category; repeated calls agree."
  (let ((kao-motion--unicode-table
         (let ((tbl (make-char-table 'kao-word-category nil)))
           (set-char-table-range tbl '(#x2000 . #x200A) 'blank)
           tbl)))
    (should-not (aref kao-motion--unicode-table ?é))
    (should (eq (kao-motion--category ?é) 'word))
    (should (eq (aref kao-motion--unicode-table ?é) 'word))  ; cached
    (should (eq (kao-motion--category ?é) 'word))))          ; second hit agrees

;;;; Kao-motion-skip-invisible (opt-in, CURSOR-only)

(ert-deftest kao-motion-skip-invisible-off-lands-inside-run ()
  "Default (`kao-motion-skip-invisible' nil): a char motion into an invisible
org-link span lands INSIDE it — the buffer-text-faithful behavior, unchanged.
The knob is CURSOR-only, so what edits/objects operate on never moves."
  ;; "abcHIDDENxyz": a1 b2 c3 H4 I5 D6 D7 E8 N9 x10 y11 z12; 4..9 invisible.
  (with-temp-buffer
    (insert "abcHIDDENxyz")
    (put-text-property 4 10 'invisible 'org-link)
    (let ((kao-motion-skip-invisible nil))
      (should (= (kao-sel-cursor
                  (kao-motion-right (kao-sel-make :anchor 3 :cursor 3)))
                 4))                              ; 'l' lands inside the hidden run
      (should (= (kao-sel-cursor
                  (kao-motion-left (kao-sel-make :anchor 10 :cursor 10)))
                 9)))))                           ; 'h' lands inside the hidden run

(ert-deftest kao-motion-skip-invisible-char-hops-to-visible-edge ()
  "With `kao-motion-skip-invisible' t, a char motion that lands inside an
invisible run hops the CURSOR to the far edge of the run (the
`adjust_point_for_property' discipline): `l' to the first VISIBLE char past it,
`h' to the first visible char before it."
  (with-temp-buffer
    (insert "abcHIDDENxyz")
    (put-text-property 4 10 'invisible 'org-link)
    (let ((kao-motion-skip-invisible t))
      (should (= (kao-sel-cursor
                  (kao-motion-right (kao-sel-make :anchor 3 :cursor 3)))
                 10))                             ; hops forward onto 'x' (visible)
      (should (= (kao-sel-cursor
                  (kao-motion-left (kao-sel-make :anchor 10 :cursor 10)))
                 3)))))                           ; hops back onto 'c' (visible)

(ert-deftest kao-motion-skip-invisible-find-hops-to-visible-edge ()
  "`kao-motion-skip-invisible' t also applies to the find-char paths: f/t and
their reverses find buffer text regardless of visibility, then hop the
cursor off an invisible landing to the run's far edge."
  (with-temp-buffer
    (insert "abcHIDDENxyz")
    (put-text-property 4 10 'invisible 'org-link)
    ;; Default: `f D' lands on the (invisible) 'D' at 6; `<a-f> E' on 'E' at 8.
    (should (= (kao-find--forward 3 ?D 1 t) 6))
    (should (= (kao-find--backward 12 ?E 1 t) 8))
    (let ((kao-motion-skip-invisible t))
      (should (= (kao-find--forward 3 ?D 1 t) 10))    ; forward far edge -> 'x'
      (should (= (kao-find--backward 12 ?E 1 t) 3))))) ; backward far edge -> 'c'

(ert-deftest kao-motion-skip-invisible-vertical-hops-off-fold ()
  "`kao-motion-skip-invisible' t applies to the j/k goal-column landing: when the
column resolves onto an invisible char (the eol-backoff tail of a folded run),
the cursor hops off it; default leaves the cursor inside the run."
  ;; "abcd\nefGHI\n": line2 e6 f7 G8 H9 I10 nl11; 8..10 invisible (reaches eol).
  (with-temp-buffer
    (insert "abcd\nefGHI\n")
    (put-text-property 8 11 'invisible 'org-link)
    (let ((kao-motion-skip-invisible nil))
      (let ((p (kao-sel-cursor (kao-motion-down (kao-sel-make :anchor 3 :cursor 3)))))
        (should (= p 10))                         ; default: lands on hidden 'I'
        (should (invisible-p p))))
    (let ((kao-motion-skip-invisible t))
      (let ((p (kao-sel-cursor (kao-motion-down (kao-sel-make :anchor 3 :cursor 3)))))
        (should (= p 11))                         ; skips to the far (visible) edge
        (should-not (invisible-p p))))))

(ert-deftest kao-motion-invisible-fold-seam-skip-or-reveal ()
  "Seam: with a reveal opener registered on `kao-reveal-functions', a j landing
inside a folded run either SKIPS it (opt-in `kao-motion-skip-invisible') or, with
the knob off, is REVEALED by the hook at the main cursor — the two modes of the
invisible-text stance."
  (with-temp-buffer
    (insert "abcd\nefGHI\n")
    (put-text-property 8 11 'invisible 'org-link) ; G8 H9 I10 hidden, reaches eol
    (kao-mode 1)
    (unwind-protect
        (progn
          ;; Mode 1 — skip on: j hops the cursor off the fold.
          (let* ((kao-motion-skip-invisible t)
                 (p (kao-sel-cursor
                     (kao-motion-down (kao-sel-make :anchor 3 :cursor 3)))))
            (should-not (invisible-p p)))
          ;; Mode 2 — skip off + reveal opener: j lands in the fold, the hook opens it.
          (let* ((kao-motion-skip-invisible nil)
                 (kao-reveal-functions
                  (cons (lambda (_pos)
                          (remove-text-properties (point-min) (point-max)
                                                  '(invisible nil)))
                        kao-reveal-functions))
                 (landed (kao-motion-down (kao-sel-make :anchor 3 :cursor 3))))
            (should (invisible-p (kao-sel-cursor landed)))  ; lands in the fold
            (setq kao--sels (kao-sels-make :list (list landed) :main 0))
            (kao--refresh)
            (should-not (invisible-p (point)))))            ; hook revealed it
      (kao-mode -1))))

;;;; Mode-off guard.  Every M-x-discoverable motion command
;;;; reads selection state; with kao-mode OFF `kao--sels' is nil and the command
;;;; used to die as the cryptic (wrong-type-argument kao-sels nil).  The shared
;;;; `kao--assert-mode' guard (the mode guard) now signals a clear `user-error'
;;;; first, naming the cause.  ADDITIVE pin — no existing assertion is touched.

(defconst kao-motion-tests--guarded-commands
  '(kao-left kao-right kao-down kao-up
    kao-word-forward kao-word-backward kao-word-end kao-line
    kao-WORD-forward kao-WORD-backward kao-WORD-end
    kao-extend-left kao-extend-right kao-extend-down kao-extend-up
    kao-extend-word-forward kao-extend-word-backward kao-extend-word-end
    kao-extend-WORD-forward kao-extend-WORD-backward kao-extend-WORD-end
    kao-find-to-char kao-find-until-char
    kao-find-to-char-reverse kao-find-until-char-reverse
    kao-extend-to-char kao-extend-until-char
    kao-extend-to-char-reverse kao-extend-until-char-reverse
    kao-repeat-select kao-reduce kao-flip-selections kao-ensure-forward
    kao-select-matching kao-select-matching-backward
    kao-extend-matching kao-extend-matching-backward
    kao-crop-lines)
  "Every M-x-discoverable kao-motion command carrying the `kao--assert-mode' guard.
The guard is the FIRST form, so it fires before any `read-key' the command
would otherwise issue (find-char).  Kept exhaustive on purpose: a new guarded
motion command must be added here, and an unguarded one is caught by
`kao-motion-commands-signal-user-error-with-mode-off'.")

(ert-deftest kao-motion-commands-signal-user-error-with-mode-off ()
  "Each guarded motion command signals `user-error' \"kao-mode is not active\"
when invoked with the mode off, instead of the cryptic wrong-type-argument ."
  (dolist (cmd kao-motion-tests--guarded-commands)
    (with-temp-buffer
      (fundamental-mode)                ; kao-mode is off; `kao--sels' is nil
      (insert "abc")
      (let ((err (should-error (call-interactively cmd) :type 'user-error)))
        (should (string-match-p "kao-mode is not active"
                                (error-message-string err)))))))

(provide 'kao-motion-tests)
;;; kao-motion-tests.el ends here
