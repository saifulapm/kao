;;; kao-menu-tests.el --- Tests for kao-menu -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the goto (`g') and view (`v') menus.  The read-key dispatch is
;; not batch-drivable (like the object-pending step), so the goto sub-commands
;; are exercised through the `kao-menu-tests--goto-table' thunks; the pure selectors
;; (`kao-menu--line-end' / `--line-begin' / `--first-non-blank') are checked
;; against Kakoune's selectors.cc semantics.  Window-relative goto (gt/gb/gc) and
;; the whole view menu depend on a live window, so here they are only checked to
;; no-op (and not error) in batch — their behaviour is live-smoke-tested.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-state)
(require 'kao-menu)
(require 'kao-keys)                    ; default bindings

(defun kao-menu-tests--sel (content cursor fn)
  "In CONTENT, apply FN to a collapsed selection at 1-based CURSOR.
Return the result as (ANCHOR . CURSOR)."
  (with-temp-buffer
    (insert content)
    (let ((s (funcall fn (kao-sel-make :anchor cursor :cursor cursor))))
      (cons (kao-sel-anchor s) (kao-sel-cursor s)))))

(defun kao-menu-tests--run (content thunk &optional sels-spec)
  "In CONTENT, seed `kao--sels' from SELS-SPEC then run THUNK.
SELS-SPEC is a list of (ANCHOR . CURSOR) 1-based pairs (default one sel at 1).
Return the resulting list as ((ANCHOR . CURSOR) ...).
`kao-mode' is faked on (the flag only) so a THUNK that is itself a guarded
M-x command passes `kao--assert-mode'; inert for the dispatch
thunks, which never read the mode flag."
  (with-temp-buffer
    (insert content)
    (setq-local kao-mode t)
    (setq kao--sels
          (kao-sels-make
           :list (mapcar (lambda (ac) (kao-sel-make :anchor (car ac) :cursor (cdr ac)))
                         (or sels-spec '((1 . 1))))
           :main 0))
    (funcall thunk)
    (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
            (kao-sels-list kao--sels))))

(defun kao-menu-tests--goto-table ()
  "Derive the goto sub-key -> Replace (`g') thunk alist from `kao--goto-specs'.
Each entry is (EVENT . (lambda () ...)); the menu tests drive the sub-commands
through these 0-arg thunks (the one-shot key read is not batch-drivable).  Built
fresh at call time -- production keeps no such cache; the dispatch reads
`kao--goto-specs' directly."
  (mapcar (lambda (spec)
            (let ((key (car spec)))
              (cons key (lambda () (kao-goto--dispatch key 'replace)))))
          kao--goto-specs))

;;;; Per-selection line selectors.  "foo\nbar": f1 o2 o3 \n4 b5 a6 r7

(ert-deftest kao-menu-line-end-sets-before-eol-target ()
  "`gl' and `<a-l>' set the cursor's `before-eol' (max_non_eol_column)
sticky target (selectors.cc:186); `gh' (line-begin) leaves target nil (reset,
selectors.cc:191 `{line,0}')."
  (with-temp-buffer
    (insert "foo\nbar")
    (let ((cur (kao-sel-make :anchor 2 :cursor 2)))
      (should (eq (kao-sel-target (kao-menu--line-end cur)) 'before-eol))
      (should (eq (kao-sel-target (kao-menu--line-end-sel cur)) 'before-eol))
      (should (null (kao-sel-target (kao-menu--line-begin cur)))))))

(ert-deftest kao-menu-line-end-mid-line ()
  "`gl' moves the cursor to the line's last non-eol char, collapsed."
  (should (equal '(3 . 3) (kao-menu-tests--sel "foo\nbar" 2 #'kao-menu--line-end))))

(ert-deftest kao-menu-line-end-on-eol-stays ()
  "`gl' does not go backward when the cursor is already on the eol."
  ;; cursor at 4 (the newline) -> last non-eol is 3, but max(4,3)=4: stay put.
  (should (equal '(4 . 4) (kao-menu-tests--sel "foo\nbar" 4 #'kao-menu--line-end))))

(ert-deftest kao-menu-line-end-empty-line-stays ()
  "`gl' on an empty line stays at its begin (end == begin)."
  (should (equal '(1 . 1) (kao-menu-tests--sel "\nx" 1 #'kao-menu--line-end))))

(ert-deftest kao-menu-line-begin ()
  "`gh' moves the cursor to column 0 of its line."
  ;; "foo\nbar": cursor at 6 ('a' on line 2) -> bol of line 2 = 5.
  (should (equal '(5 . 5) (kao-menu-tests--sel "foo\nbar" 6 #'kao-menu--line-begin))))

(ert-deftest kao-menu-first-non-blank ()
  "`gi' skips leading horizontal blanks to the first non-blank char."
  ;; "  ab": sp1 sp2 a3 b4 -> first non-blank = 3.
  (should (equal '(3 . 3) (kao-menu-tests--sel "  ab" 1 #'kao-menu--first-non-blank))))

(ert-deftest kao-menu-first-non-blank-all-blank-lands-on-eol ()
  "`gi' on an all-blank line lands on the newline (bounded by line end)."
  ;; "   \nx": sp1 sp2 sp3 \n4 -> lands on the \n at 4.
  (should (equal '(4 . 4) (kao-menu-tests--sel "   \nx" 2 #'kao-menu--first-non-blank))))

;;;; Coord targets (collapse the whole list to ONE selection)

(ert-deftest kao-menu-goto-buffer-top ()
  "`gg'/`gk' collapse to point-min."
  (should (equal '((1 . 1))
                 (kao-menu-tests--run
                  "abc\ndef" (cdr (assq ?g (kao-menu-tests--goto-table))) '((1 . 3) (5 . 7))))))

(ert-deftest kao-menu-goto-buffer-bottom ()
  "`gj' collapses to the first column of the last line."
  ;; "abc\ndef": last line "def" bol = 5.
  (should (equal '((5 . 5))
                 (kao-menu-tests--run "abc\ndef" (cdr (assq ?j (kao-menu-tests--goto-table)))))))

(ert-deftest kao-menu-goto-buffer-bottom-trailing-newline ()
  "`gj' on a newline-terminated buffer targets the LAST REAL line's column 0.
Kakoune's `select_coord(line_count - 1)' -> `{last line, 0}' (normal.cc:261-263):
the target is the last line that holds text, not the phantom line after the
trailing `\\n'.  On \"abc\\ndef\\n\": a1 b2 c3 \\n4 d5 e6 f7 \\n8, the last real
line \"def\" begins at 5."
  (should (equal '((5 . 5))
                 (kao-menu-tests--run "abc\ndef\n" (cdr (assq ?j (kao-menu-tests--goto-table)))))))

(ert-deftest kao-menu-goto-line-overflow-clamps-back-coord ()
  "Overflow `Ng' lands on `back_coord' (the last line's `\\n'), NOT column 0.
`Buffer::clamp' (buffer.cc:155-163) assigns the WHOLE coord `coord = back_coord'
on an out-of-range line, so an out-of-range `Ng' targets the last on-char
position = the trailing `\\n' (`1- point-max' after `kao--clamp-sel').  On
\"abc\\ndef\\n\" (point-max = 9) that is 8.  Regression lock: Task 1 must not
push `kao-menu--line-n-bol' to column 0."
  (should (equal '((8 . 8))
                 (kao-menu-tests--run "abc\ndef\n"
                                      (lambda () (kao-menu--goto-line 99))))))

(ert-deftest kao-menu-goto-buffer-end ()
  "`ge' collapses to the buffer's last char (1- point-max, family)."
  ;; "abc\ndef": point-max = 8, last on-char position = 7.
  (should (equal '((7 . 7))
                 (kao-menu-tests--run "abc\ndef" (cdr (assq ?e (kao-menu-tests--goto-table)))))))

(ert-deftest kao-menu-goto-collapses-many-to-one ()
  "Any coord goto reduces N selections to a single one (select_coord Replace)."
  (let ((res (kao-menu-tests--run
              "abc\ndef" (cdr (assq ?g (kao-menu-tests--goto-table))) '((1 . 1) (5 . 5) (7 . 7)))))
    (should (= 1 (length res)))))

(ert-deftest kao-menu-goto-uppercase-subkey-lowercased ()
  "An uppercase goto sub-key dispatches as its lowercase target.
Kakoune folds the goto sub-key with `to_lower(*cp)' (normal.cc:245), so `gJ'
== `gj', `GK' == `gk', `GE' == `ge'.  On \"abc\\ndef\": `?J' collapses to the
buffer bottom (5), `?K' to the buffer top (1), `?E' to the buffer end (7) —
identical to the lowercase keys."
  (should (equal '((5 . 5))
                 (kao-menu-tests--run
                  "abc\ndef" (lambda () (kao-goto--dispatch ?J 'replace)) '((3 . 3)))))
  (should (equal '((1 . 1))
                 (kao-menu-tests--run
                  "abc\ndef" (lambda () (kao-goto--dispatch ?K 'replace)) '((3 . 3)))))
  (should (equal '((7 . 7))
                 (kao-menu-tests--run
                  "abc\ndef" (lambda () (kao-goto--dispatch ?E 'replace)) '((3 . 3)))))
  ;; Identical to the lowercase dispatch, byte for byte.
  (should (equal (kao-menu-tests--run
                  "abc\ndef" (lambda () (kao-goto--dispatch ?J 'replace)) '((3 . 3)))
                 (kao-menu-tests--run
                  "abc\ndef" (lambda () (kao-goto--dispatch ?j 'replace)) '((3 . 3)))))
  ;; The fold is goto-only: the view table (normal.cc:422, raw codepoint) stays
  ;; case-sensitive, so an uppercase view key never matches a lowercase entry.
  (should-not (assq ?J kao--view-table)))

(ert-deftest kao-menu-goto-line-count ()
  "Count form `Ng' collapses to the first column of 1-based line N."
  ;; "l1\nl2\nl3": line bols at 1, 4, 7.
  (should (equal '((1 . 1)) (kao-menu-tests--run "l1\nl2\nl3"
                                                 (lambda () (kao-menu--goto-line 1)))))
  (should (equal '((4 . 4)) (kao-menu-tests--run "l1\nl2\nl3"
                                                 (lambda () (kao-menu--goto-line 2)))))
  (should (equal '((7 . 7)) (kao-menu-tests--run "l1\nl2\nl3"
                                                 (lambda () (kao-menu--goto-line 3))))))

;;;; Per-selection gotos preserve the selection count

(ert-deftest kao-menu-line-end-maps-over-all ()
  "`gl' applies to every selection (per-selection select<>, not a collapse)."
  ;; "ab\ncd": a1 b2 \n3 c4 d5.  cursors at 1 and 4 -> line ends 2 and 5.
  (should (equal '((2 . 2) (5 . 5))
                 (kao-menu-tests--run "ab\ncd" (cdr (assq ?l (kao-menu-tests--goto-table)))
                                      '((1 . 1) (4 . 4))))))

;;;; Dispatch table shape

(ert-deftest kao-menu-goto-table-keys ()
  "Every goto key is present; an unmapped key is not.
`r' must NOT be in the table: Kakoune leaves it unmapped in goto, and kao's
former `g r' xref extension moved to `SPC r' (the v1 free-key premise was
false for `d'/`u' — )."
  (dolist (k '(?g ?k ?j ?e ?l ?h ?i ?t ?b ?c ?f ?d ?u))
    (should (assq k (kao-menu-tests--goto-table))))
  (should-not (assq ?z (kao-menu-tests--goto-table)))
  (should-not (assq ?r (kao-menu-tests--goto-table))))

;;;; Command gotos (gf — ffap; mode-independent)

(ert-deftest kao-menu-goto-command-calls-interactively ()
  "`gf' dispatches to its command via `call-interactively'."
  (dolist (pair '((?f . find-file-at-point)))
    (let (called)
      (cl-letf (((symbol-function 'call-interactively)
                 (lambda (cmd &rest _) (push cmd called))))
        (with-temp-buffer
          (insert "abc")
          (kao-goto--dispatch (car pair) 'replace)
          ;; Extend mode dispatches the SAME command (mode-independent).
          (kao-goto--dispatch (car pair) 'extend)))
      (should (equal called (list (cdr pair) (cdr pair)))))))

(ert-deftest kao-menu-goto-file-visits ()
  "`gf' visits the file at point, end to end.
ffap PROMPTS with the guessed path pre-filled (standard ffap UX); the stub
plays the confirming RET by returning the initial input."
  (let ((file (make-temp-file "kao-gf")) buf)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (_prompt &optional dir _default _must initial
                                      &rest _)
                       (expand-file-name initial dir))))
            (with-temp-buffer
              (insert file)
              (goto-char (point-min))
              (kao-goto--dispatch ?f 'replace)))
          (setq buf (get-file-buffer file))
          (should buf))
      (when buf (kill-buffer buf))
      (delete-file file))))

(ert-deftest kao-menu-goto-command-leaves-selections ()
  "A command goto that stays in the buffer leaves the selection list and pushes
no jump (the jump push fires only on an actual buffer switch, Task 5)."
  (let ((pushed 0))
    (cl-letf (((symbol-function 'call-interactively) #'ignore)
              ((symbol-function 'kao--jump-push)
               (lambda (&rest _) (cl-incf pushed))))
      (should (equal '((1 . 3) (5 . 7))
                     (kao-menu-tests--run
                      "abc\ndef" (lambda () (kao-goto--dispatch ?f 'replace))
                      '((1 . 3) (5 . 7))))))
    (should (= pushed 0))))

(ert-deftest kao-menu-goto-command-pushes-jump-on-switch ()
  "A `command' goto (e.g. `gf') pushes a jump tagged the ORIGINATING buffer
when the command changes buffer, so `C-o' returns to the pre-`gf' selections
\(Kakoune push_jump only on an actual switch, normal.cc:358-361); a command
that stays in the buffer pushes nothing."
  (let ((a (generate-new-buffer " *kao-gf-a*"))
        (b (generate-new-buffer " *kao-gf-b*")))
    (unwind-protect
        (progn
          (with-current-buffer a (insert "aaaa") (kao-mode 1))
          (with-current-buffer b (insert "bbbb") (kao-mode 1))
          (setq kao--jumps nil kao--jump-current 0)
          (switch-to-buffer a)
          ;; Distinctive pre-jump selections in the originating buffer.
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 2 :cursor 4))
                           :main 0))
          ;; A `command' whose payload switches to B -> one jump, tagged A.
          (cl-letf (((symbol-function 'call-interactively)
                     (lambda (&rest _) (switch-to-buffer b))))
            (kao-goto--dispatch ?f 'replace))
          (should (eq (current-buffer) b))
          (should (= 1 (length kao--jumps)))
          (should (eq (car (car kao--jumps)) a))        ; tagged the ORIGIN
          ;; `C-o' returns to A and restores the pre-jump selections.
          (kao-jump-backward)
          (should (eq (current-buffer) a))
          (should (equal '((2 . 4))
                         (mapcar (lambda (s) (cons (kao-sel-anchor s)
                                                   (kao-sel-cursor s)))
                                 (kao-sels-list kao--sels))))
          ;; A `command' that does NOT switch buffers pushes nothing.
          (setq kao--jumps nil kao--jump-current 0)
          (switch-to-buffer a)
          (cl-letf (((symbol-function 'call-interactively) #'ignore))
            (kao-goto--dispatch ?f 'replace))
          (should (null kao--jumps)))
      (dolist (buf (list a b))
        (when (buffer-live-p buf)
          (with-current-buffer buf (kao-mode -1)) (kill-buffer buf))))))

;;;; Last-buffer goto (ga — normal.cc:292-302; )

(ert-deftest kao-menu-goto-last-buffer-switches-and-pushes ()
  "`ga' pushes a jump in the OLD buffer, then switches to the most recently
used other kao buffer (push_jump before change_buffer, normal.cc:299-300)."
  (let ((a (generate-new-buffer " *kao-ga-a*"))
        (b (generate-new-buffer " *kao-ga-b*")))
    (unwind-protect
        (progn
          (with-current-buffer a (insert "aaaa") (kao-mode 1))
          (with-current-buffer b (insert "bbbb") (kao-mode 1))
          (setq kao--jumps nil kao--jump-current 0)
          (switch-to-buffer a)        ; MRU order: b is now A's last buffer
          (switch-to-buffer b)
          (kao-goto--dispatch ?a 'replace)
          (should (eq (current-buffer) a))
          ;; The jump entry was pushed before the switch, tagged B.
          (should (= 1 (length kao--jumps)))
          (should (eq (car (car kao--jumps)) b)))
      (dolist (buf (list a b))
        (when (buffer-live-p buf)
          (with-current-buffer buf (kao-mode -1)) (kill-buffer buf))))))

(ert-deftest kao-menu-goto-last-buffer-none-errors ()
  "`ga' with no other kao buffer signals Kakoune's exact error, pushing NO
jump (the throw precedes push_jump, normal.cc:294-299)."
  (let ((a (generate-new-buffer " *kao-ga-solo*")))
    (unwind-protect
        (progn
          (with-current-buffer a (insert "aaaa") (kao-mode 1))
          (setq kao--jumps nil kao--jump-current 0)
          (switch-to-buffer a)
          (let ((err (should-error (kao-goto--dispatch ?a 'replace)
                                   :type 'user-error)))
            (should (equal (cadr err) "no last buffer")))
          (should (null kao--jumps)))
      (when (buffer-live-p a)
        (with-current-buffer a (kao-mode -1)) (kill-buffer a)))))

;;;; Last-change goto (g. — normal.cc:365-375; )

(defmacro kao-menu-tests--with-kao-buffer (content &rest body)
  "Run BODY in a live kao-mode buffer holding CONTENT, cleaned up after."
  (declare (indent 1))
  `(let ((buf (generate-new-buffer " *kao-gdot*")))
     (unwind-protect
         (progn
           (with-current-buffer buf (insert ,content) (kao-mode 1))
           (switch-to-buffer buf)
           (setq kao--jumps nil kao--jump-current 0)
           ,@body)
       (when (buffer-live-p buf)
         (with-current-buffer buf (kao-mode -1)) (kill-buffer buf)))))

(ert-deftest kao-menu-goto-last-change-collapses-there ()
  "`g.' collapses to the position of the current node's last modification."
  (kao-menu-tests--with-kao-buffer "abcdef"
    (goto-char 4)
    (insert "XY")                       ; captured by the kao-mode hooks
    (kao-history-commit-pending)        ; the post-command commit, driven
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0))
    (kao-goto--dispatch ?. 'replace)
    (let ((s (car (kao-sels-list kao--sels))))
      (should (= 4 (kao-sel-cursor s)))         ; the mod's pos
      (should (= 4 (kao-sel-anchor s))))
    ;; The coord arm pushed a jump (push_jump, normal.cc:366).
    (should (= 1 (length kao--jumps)))))

(ert-deftest kao-menu-goto-last-change-extend-keeps-anchor ()
  "`G .' extends: cursor to the change, anchor kept (select_coord<Extend>)."
  (kao-menu-tests--with-kao-buffer "abcdef"
    (goto-char 4)
    (insert "XY")
    (kao-history-commit-pending)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)) :main 0))
    (kao-goto--dispatch ?. 'extend)
    (let ((s (car (kao-sels-list kao--sels))))
      (should (= 2 (kao-sel-anchor s)))
      (should (= 4 (kao-sel-cursor s))))))

(ert-deftest kao-menu-goto-last-change-none-errors-after-push ()
  "Fresh buffer (history at the root): Kakoune's exact error, but the jump
WAS pushed — push_jump (normal.cc:366) precedes the nil check (:368-369)."
  (kao-menu-tests--with-kao-buffer "abcdef"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)) :main 0))
    (let ((err (should-error (kao-goto--dispatch ?. 'replace)
                             :type 'user-error)))
      (should (equal (cadr err) "no last modification position")))
    (should (= 1 (length kao--jumps)))          ; pushed despite the error
    ;; And the selection list is untouched.
    (should (= 2 (kao-sel-cursor (car (kao-sels-list kao--sels)))))))

(ert-deftest kao-menu-goto-last-change-follows-undo ()
  "After `u' the buffer sits at the root again: `g.' errors (the accessor
follows the CURRENT node, buffer.cc:674-676)."
  (kao-menu-tests--with-kao-buffer "abcdef"
    (goto-char 4)
    (insert "XY")
    (kao-history-commit-pending)
    ;; Step the tree pointer back to the root (what `u' does via
    ;; `kao-history-move-to'); the accessor must follow the CURRENT node.
    (setq kao--hist-id (kao-hist-node-parent
                        (kao--hist-node (kao-history-current-id))))
    (should-error (kao-goto--dispatch ?. 'replace) :type 'user-error)))

;;;; Display-line gotos (gd/gu — normal.cc:303-325; )

(ert-deftest kao-menu-goto-display-line-batch-fallback ()
  "Undisplayed buffer: `gd'/`gu' fall back to logical lines (the C++
windowless `offset_coord' arm), preserving the column."
  ;; "abc\ndef\nghi": a1 b2 c3 \n4 d5 e6 f7 \n8 g9 h10 i11
  (should (equal '((6 . 6))
                 (kao-menu-tests--run
                  "abc\ndef\nghi" (cdr (assq ?d (kao-menu-tests--goto-table))) '((2 . 2)))))
  (should (equal '((2 . 2))
                 (kao-menu-tests--run
                  "abc\ndef\nghi" (cdr (assq ?u (kao-menu-tests--goto-table))) '((6 . 6))))))

(ert-deftest kao-menu-goto-display-line-no-jump-push ()
  "`gd'/`gu' are selector targets: NO jump push (normal.cc:303-325 has no
push_jump, unlike the coord targets g/k/j/e)."
  (let ((pushed 0))
    (cl-letf (((symbol-function 'kao--jump-push)
               (lambda (&rest _) (cl-incf pushed))))
      (kao-menu-tests--run "abc\ndef" (cdr (assq ?d (kao-menu-tests--goto-table))) '((2 . 2)))
      (kao-menu-tests--run "abc\ndef" (cdr (assq ?u (kao-menu-tests--goto-table))) '((6 . 6))))
    (should (= pushed 0))))

(ert-deftest kao-menu-goto-display-line-extend-keeps-anchor ()
  "`G d' moves only the cursor, keeping the anchor (select<Extend>)."
  (should (equal '((2 . 6))
                 (kao-menu-tests--run
                  "abc\ndef" (lambda () (kao-goto--dispatch ?d 'extend))
                  '((2 . 2))))))

(ert-deftest kao-menu-goto-display-line-wrapped ()
  "Displayed wrapped line: `gd' moves one DISPLAY line, not one logical line.
A 200-char line wraps in the batch window (80 cols); from char 2 (screen row
0, screen col 1) one display line down lands INSIDE the same logical line, on
the next screen row at the same screen column — `vertical-motion' computes
display geometry in batch too (probed; `switch-to-buffer' makes the buffer
the selected window's = `kao-menu--displayed-p')."
  (let ((buf (generate-new-buffer "*kao-display-line*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (insert (make-string 200 ?x))
          (setq truncate-lines nil)
          (let* ((s (kao-menu--display-line
                     (kao-sel-make :anchor 2 :cursor 2) 1))
                 (p (kao-sel-cursor s)))
            (should (> p 2))                    ; moved forward...
            (should (<= p 201))                 ; ...within the SAME logical line
            (save-excursion                     ; same screen column, row +1
              (goto-char p)
              (should (= 1 (- (current-column)
                              (save-excursion (vertical-motion 0)
                                              (current-column))))))
            ;; And `gu' from there returns to the origin (symmetry).
            (should (= 2 (kao-sel-cursor (kao-menu--display-line s -1))))))
      (kill-buffer buf))))

(ert-deftest kao-menu-goto-display-line-short-row-clamps ()
  "A target row SHORTER than the origin screen column clamps to its last char.
Geometry (80-col batch window): 78 `x' then a double-width CJK char — the
wide char cannot fit before the wrap, so row 0 ends early at screen col 77
\(char 78) and the CJK char starts row 1.  From row 1 screen col 78
\(char 156), `gu' targets row-0 col 78 — `move-to-column' overshoots onto the
CJK char (char 79, visually still row 1 = no motion); the row-end clamp must
land on char 78, row 0's last char (Kakoune's `buffer_coord' clamps within
the display row)."
  (let ((buf (generate-new-buffer "*kao-display-clamp*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (insert (make-string 78 ?x) (string #x6f22) (make-string 100 ?y))
          (setq truncate-lines nil)
          (should (= 78 (kao-sel-cursor
                         (kao-menu--display-line
                          (kao-sel-make :anchor 156 :cursor 156) -1)))))
      (kill-buffer buf))))

;;;; Window-relative gotos (gt/gb/gc) — batch no-op (live-smoke covers behaviour)

(ert-deftest kao-menu-window-line-pos-nil-when-not-displayed ()
  "`kao-menu--window-line-pos' returns nil in batch (no window to measure)."
  (with-temp-buffer
    (insert "a\nb\nc")
    (should (null (kao-menu--window-line-pos 'top)))
    (should (null (kao-menu--window-line-pos 'bottom)))
    (should (null (kao-menu--window-line-pos 'center)))))

(ert-deftest kao-menu-goto-window-line-noop-in-batch ()
  "`gt'/`gb'/`gc' leave the selection list unchanged when not displayed."
  (dolist (k '(?t ?b ?c))
    (should (equal '((1 . 3) (5 . 7))
                   (kao-menu-tests--run
                    "abc\ndef" (cdr (assq k (kao-menu-tests--goto-table))) '((1 . 3) (5 . 7)))))))

(ert-deftest kao-menu-window-line-pos-arithmetic ()
  "Window-line math: top = window-start line, bottom = +(height-1), center = +height/2.
A live window is unavailable in batch, so simulate its geometry — this verifies
the `forward-line' offsets (the ported normal.cc:269-291 arithmetic); the real
`window-start'/`window-body-height' values are environmental (live-smoke)."
  (with-temp-buffer
    (insert "l1\nl2\nl3\nl4\nl5\nl6")     ; line bols: 1 4 7 10 13 16
    (cl-letf (((symbol-function 'kao-menu--displayed-p) (lambda () t))
              ((symbol-function 'window-start) (lambda (&optional _) 4))    ; line 2
              ((symbol-function 'window-body-height) (lambda (&optional _) 3)))
      (should (= 4  (kao-menu--window-line-pos 'top)))      ; window-start line
      (should (= 10 (kao-menu--window-line-pos 'bottom)))   ; +2 lines -> line 4
      (should (= 7  (kao-menu--window-line-pos 'center)))))) ; +1 line  -> line 3

;;;; goto extend (`G', goto_commands<Extend>) — coord keeps anchors, selector merges

(ert-deftest kao-menu-goto-extend-coord-keeps-anchor ()
  "`Ge' moves the cursor to the buffer end keeping the anchor (NOT a collapse)."
  ;; "abc\ndef": last on-char pos 7.  Replace `ge' would give (7 . 7).
  (should (equal '((1 . 7))
                 (kao-menu-tests--run
                  "abc\ndef" (lambda () (kao-goto--dispatch ?e 'extend))
                  '((1 . 3))))))

(ert-deftest kao-menu-goto-extend-coord-vs-replace-contrast ()
  "Replace `ge' collapses to the coord; Extend `Ge' keeps the anchor."
  (should (equal '((7 . 7))
                 (kao-menu-tests--run
                  "abc\ndef" (lambda () (kao-goto--dispatch ?e 'replace))
                  '((1 . 3)))))
  (should (equal '((1 . 7))
                 (kao-menu-tests--run
                  "abc\ndef" (lambda () (kao-goto--dispatch ?e 'extend))
                  '((1 . 3))))))

(ert-deftest kao-menu-goto-extend-selector-line-end-maps ()
  "`Gl' extends every selection to its line end (select<Extend> selector)."
  ;; "ab\ncd": a1 b2 \n3 c4 d5.  cursors 1,4 -> line ends 2,5; anchors kept.
  (should (equal '((1 . 2) (4 . 5))
                 (kao-menu-tests--run
                  "ab\ncd" (lambda () (kao-goto--dispatch ?l 'extend))
                  '((1 . 1) (4 . 4))))))

(ert-deftest kao-menu-goto-extend-coord-merges-overlap ()
  "`Gg' pulls every cursor to the top; overlapping results sort-and-merge."
  ;; "abcdef": sels [3,3],[5,5]; Gg -> backward [3->1],[5->1] cover [1,3],[1,5]
  ;; -> overlap -> one selection spanning [1,5].
  (let ((res (kao-menu-tests--run
              "abcdef" (lambda () (kao-goto--dispatch ?g 'extend))
              '((3 . 3) (5 . 5)))))
    (should (= 1 (length res)))
    (let ((s (car res)))
      (should (= (min (car s) (cdr s)) 1))
      (should (= (max (car s) (cdr s)) 5)))))

(ert-deftest kao-menu-goto-extend-count-line ()
  "`2G' extends to line 2's first column, keeping the anchor."
  ;; "l1\nl2\nl3": line bols 1,4,7.  Replace `2g' would give (4 . 4).
  (should (equal '((1 . 4))
                 (kao-menu-tests--run
                  "l1\nl2\nl3"
                  (lambda () (setq kao--count 2) (kao-goto-extend))
                  '((1 . 1))))))

(ert-deftest kao-menu-goto-extend-window-noop-in-batch ()
  "`Gt'/`Gb'/`Gc' leave the selections unchanged when not displayed."
  (dolist (k '(?t ?b ?c))
    (should (equal '((1 . 3) (5 . 7))
                   (kao-menu-tests--run
                    "abc\ndef" (lambda () (kao-goto--dispatch k 'extend))
                    '((1 . 3) (5 . 7)))))))

(ert-deftest kao-menu-goto-extend-bound ()
  "`G' is bound to `kao-goto-extend' in normal state."
  (should (eq (lookup-key kao-normal-state-map "G") #'kao-goto-extend)))

;;;; View menu.  Reposition/scroll need a live window (live-smoke); here
;;;; the sync logic is checked directly, and the commands are checked to no-op
;;;; (and not error) in batch.

(ert-deftest kao-menu-view-table-keys ()
  "Every view key is present; an unmapped key is not."
  (dolist (k '(?v ?c ?t ?b ?m ?< ?> ?j ?k ?h ?l))
    (should (assq k kao--view-table)))
  (should-not (assq ?z kao--view-table)))

(ert-deftest kao-menu-view-noop-in-batch ()
  "Every view command no-ops (no error, sels unchanged) when not displayed."
  (dolist (k '(?v ?c ?t ?b ?m ?< ?> ?j ?k ?h ?l))
    (should (equal '((1 . 3) (5 . 7))
                   (kao-menu-tests--run
                    "abc\ndef"
                    (lambda () (funcall (cdr (assq k kao--view-table)) 1))
                    '((1 . 3) (5 . 7)))))))

(ert-deftest kao-menu-sync-main-to-point-collapses-main ()
  "`kao-menu--sync-main-to-point' collapses the main onto point, keeps secondaries.
The list stays min-sorted and the main is tracked by identity."
  (with-temp-buffer
    (insert "abcdef")
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)    ; main
                                 (kao-sel-make :anchor 4 :cursor 5))   ; secondary
                     :main 0))
    (goto-char 6)
    (kao-menu--sync-main-to-point)
    ;; main collapses to (6 . 6); re-sorted -> secondary (4 . 5) first, main at idx 1.
    (should (equal '((4 . 5) (6 . 6))
                   (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                           (kao-sels-list kao--sels))))
    (should (= 1 (kao-sels-main kao--sels)))))

(ert-deftest kao-menu-scroll-preserves-sels-when-point-unmoved ()
  "A `vj'/`vk' scroll that carries no point leaves every selection verbatim.
Kakoune's `ScrollFlags::PreserveSelections' (normal.cc:449-453,
input_handler.cc:1802-1815): a scroll that moves nothing must not collapse the
main onto point (kao-menu.el half of).  With
point already == the main cursor after the scroll, `kao-menu--scroll-and-sync'
skips the sync: the multi-char main extent and the secondary both stay."
  (with-temp-buffer
    (insert "abcdef")
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 4)    ; multi-char main
                                 (kao-sel-make :anchor 5 :cursor 6))   ; secondary
                     :main 0))
    (goto-char 4)                          ; point == main cursor (kao's invariant)
    (cl-letf (((symbol-function 'kao-menu--displayed-p) (lambda () t)))
      ;; FN scrolls but moves no point (nothing to scroll onto).
      (kao-menu--scroll-and-sync (lambda (_n) nil) 1))
    (should (equal '((2 . 4) (5 . 6))
                   (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                           (kao-sels-list kao--sels))))
    (should (= 0 (kao-sels-main kao--sels)))))

(ert-deftest kao-menu-scroll-syncs-when-point-carried ()
  "A `vj'/`vk' scroll that carries point to the window edge still syncs.
When Emacs's native scroll moves point off the main cursor, the main collapses
onto the new point (approximation), secondaries preserved — the gate
only skips the sync when nothing moved."
  (with-temp-buffer
    (insert "abcdef")
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 4)    ; multi-char main
                                 (kao-sel-make :anchor 5 :cursor 6))   ; secondary
                     :main 0))
    (goto-char 4)                          ; point == main cursor at entry
    (cl-letf (((symbol-function 'kao-menu--displayed-p) (lambda () t)))
      ;; FN carries point to the buffer top, as a native scroll would.
      (kao-menu--scroll-and-sync (lambda (_n) (goto-char 1)) 1))
    ;; main collapses to (1 . 1); re-sorted -> main first, secondary (5 . 6) after.
    (should (equal '((1 . 1) (5 . 6))
                   (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                           (kao-sels-list kao--sels))))
    (should (= 0 (kao-sels-main kao--sels)))))

;;;; Locked view (`V', view_commands<true>) — the re-arm loop

(defun kao-menu-tests--locked-with-keys (keys)
  "Run `kao-view-locked' feeding KEYS to `read-key' in turn; return leftover KEYS.
A fully-consumed list means the loop read every key and exited on the terminator
(Escape / unmapped).  The view handlers no-op in batch (not displayed)."
  (with-temp-buffer
    (insert "abc")
    (setq-local kao-mode t)             ; guarded command : satisfy `kao--assert-mode'
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0))
    (let* ((pending (copy-sequence keys))
           (feed (lambda (&rest _) (pop pending))))
      (cl-letf (((symbol-function 'read-key) feed))
        (kao-view-locked))
      pending)))

(ert-deftest kao-menu-view-locked-loops-until-escape ()
  "`V' dispatches each mapped sub-key and keeps reading until Escape (27)."
  (should (null (kao-menu-tests--locked-with-keys (list ?v ?j 27)))))

(ert-deftest kao-menu-view-locked-escape-first-exits ()
  "`V' then Escape exits immediately (no dispatch, terminator consumed)."
  (should (null (kao-menu-tests--locked-with-keys (list 27)))))

(ert-deftest kao-menu-view-locked-unmapped-key-continues ()
  "An unmapped CHARACTER key messages `key not mapped' and the lock STAYS armed.
Kakoune re-arms the lock (`if (lock) view_commands<true>', normal.cc:406-407)
BEFORE raising the unmapped-key error (normal.cc:458-459), and `NextKey' has
popped its mode by then (input_handler.cc:1143-1150), so the freshly-armed lock
survives — only Escape exits.  Both feeds consume EVERY key and exit on the
trailing Escape (leftover nil); with the old throw-on-unmapped reading the first
`?z' would terminate early and leave `(27)' unconsumed."
  (should (null (kao-menu-tests--locked-with-keys (list ?z 27))))
  (should (null (kao-menu-tests--locked-with-keys (list ?z ?z ?j 27)))))

(ert-deftest kao-menu-view-locked-bound ()
  "`V' is bound to `kao-view-locked' in normal state."
  (should (eq (lookup-key kao-normal-state-map "V") #'kao-view-locked)))

;;;; space — user-mode prefix keymap

(ert-deftest kao-menu-space-is-user-map-prefix ()
  "`SPC' is the plain `kao-user-map' prefix keymap."
  (should (eq kao-user-map (lookup-key kao-normal-state-map (kbd "SPC")))))

(ert-deftest kao-menu-user-map-ships-three ()
  "`kao-user-map' ships exactly three bindings: #/d/r.
Kakoune has no default user maps; kao ships the comment toggle
\(Kakoune's own comment idiom is a user mapping; on `#' by user decision
2026-06-13) and the xref pair relocated from the goto menu (—
`g d'/`g u' are real Kakoune display-line motions; extensions live in the
user map).  No `x' -> `kao-crop-lines' row by user decision (\"cut no
need\", 2026-06-13)."
  (should (keymapp kao-user-map))
  ;; Count only non-nil bindings: an unbind elsewhere can leave a (key . nil)
  ;; cons in the alist, which is not a user-visible mapping.
  (let ((n 0))
    (map-keymap (lambda (_k v) (when v (setq n (1+ n)))) kao-user-map)
    (should (= 3 n)))
  (should (eq (lookup-key kao-user-map "#") #'kao-comment-lines))
  ;; d/r now bind the push-then-xref wrappers: they
  ;; `kao-jump-push' before the xref jump so `C-o' returns.  Legacy-pin
  ;; correction forced by the mandated rebinding (Task 2
  ;; precedent), flagged in the merge report as an out-of-Owns edit.
  (should (eq (lookup-key kao-user-map "d") #'kao-xref-find-definitions))
  (should (eq (lookup-key kao-user-map "r") #'kao-xref-find-references)))

(ert-deftest kao-menu-user-map-binding-reachable ()
  "A command bound in `kao-user-map' is reachable as `SPC <key>' through
the plain prefix."
  (unwind-protect
      (progn
        (define-key kao-user-map "f" #'ignore)
        (should (eq #'ignore
                    (lookup-key kao-normal-state-map (kbd "SPC f")))))
    (define-key kao-user-map "f" nil)))           ; clean up the global map

;;;; Autoinfo parity: info keys must match the dispatch tables exactly.

(defun kao-menu-tests--keyset (alist)
  "Return the sorted list of car keys (events) of ALIST."
  (sort (mapcar #'car alist) #'<))

(ert-deftest kao-menu-goto-info-parity ()
  "`kao--goto-info' lists exactly the keys `kao--goto-specs' dispatches."
  (should (equal (kao-menu-tests--keyset kao--goto-info)
                 (kao-menu-tests--keyset kao--goto-specs))))

(ert-deftest kao-menu-view-info-parity ()
  "`kao--view-info' lists exactly the keys `kao--view-table' dispatches."
  (should (equal (kao-menu-tests--keyset kao--view-info)
                 (kao-menu-tests--keyset kao--view-table))))

;;;; Public config registrars (kao-goto-define / kao-view-define).
;;;; The four tables are defvars; each registrar upserts a spec/table row AND
;;;; its info row atomically, keeping the parity invariant.  The defvars are
;;;; global, so every mutation pin restores them in an `unwind-protect'.

(ert-deftest kao-menu-goto-define-upserts-spec-info-and-table ()
  "`kao-goto-define' appends a new goto row across `kao--goto-specs' and
`kao--goto-info', keeps info/spec parity, and re-defining the same KEY REPLACES
rather than duplicates the row.  The derived `kao-menu-tests--goto-table' reads
the freshly registered key (production keeps no cache to rebuild)."
  (let ((specs kao--goto-specs)
        (info kao--goto-info))
    (unwind-protect
        (progn
          ;; A fresh ?W row lands in both tables at once, and the derived thunk
          ;; table sees it.
          (kao-goto-define ?W 'command #'ignore "w test")
          (should (equal (assq ?W kao--goto-specs) (list ?W 'command #'ignore)))
          (should (equal (assq ?W kao--goto-info) (cons ?W "w test")))
          (should (functionp (cdr (assq ?W (kao-menu-tests--goto-table)))))
          ;; Parity holds after the upsert.
          (should (equal (kao-menu-tests--keyset kao--goto-info)
                         (kao-menu-tests--keyset kao--goto-specs)))
          ;; Re-defining ?W REPLACES the row (spec + info), never duplicates.
          (kao-goto-define ?W 'coord #'point-min "w2")
          (should (equal (assq ?W kao--goto-specs) (list ?W 'coord #'point-min)))
          (should (equal (assq ?W kao--goto-info) (cons ?W "w2")))
          (should (= 1 (cl-count ?W kao--goto-specs :key #'car)))
          (should (= 1 (cl-count ?W kao--goto-info :key #'car)))
          (should (= 1 (cl-count ?W (kao-menu-tests--goto-table) :key #'car))))
      (setq kao--goto-specs specs
            kao--goto-info info))))

(ert-deftest kao-menu-goto-define-unknown-kind-signals ()
  "`kao-goto-define' rejects a KIND outside the spec vocabulary, mutating nothing."
  (let ((specs kao--goto-specs)
        (info kao--goto-info))
    (should-error (kao-goto-define ?W 'bogus #'ignore "x") :type 'error)
    (should (null (assq ?W kao--goto-specs)))
    (should (null (assq ?W kao--goto-info)))
    (should (eq kao--goto-specs specs))       ; the list object is untouched
    (should (eq kao--goto-info info))))

(ert-deftest kao-menu-view-define-upserts-table-and-info ()
  "`kao-view-define' appends a new view row across `kao--view-table' AND
`kao--view-info', keeps their parity, and re-defining the same KEY replaces
rather than duplicates the row."
  (let ((table kao--view-table)
        (info kao--view-info))
    (unwind-protect
        (progn
          (kao-view-define ?S #'ignore "s test")
          (should (eq (cdr (assq ?S kao--view-table)) #'ignore))
          (should (equal (assq ?S kao--view-info) (cons ?S "s test")))
          (should (equal (kao-menu-tests--keyset kao--view-info)
                         (kao-menu-tests--keyset kao--view-table)))
          (kao-view-define ?S #'kao-menu--recenter "s2")
          (should (eq (cdr (assq ?S kao--view-table)) #'kao-menu--recenter))
          (should (equal (assq ?S kao--view-info) (cons ?S "s2")))
          (should (= 1 (cl-count ?S kao--view-table :key #'car)))
          (should (= 1 (cl-count ?S kao--view-info :key #'car))))
      (setq kao--view-table table
            kao--view-info info))))

;;;; Page scroll (<c-d>/<c-u>/<c-f>/<c-b>, family).  The line arithmetic is
;;;; pure (ERT here); the live scroll needs a real window (live-smoke).

(ert-deftest kao-scroll-lines-full-and-half ()
  "`kao--scroll-lines' = (height-2)/(half?2:1)*max(1,count) (normal.cc:1598)."
  (should (= 48 (kao--scroll-lines 50 nil 1)))    ; full page
  (should (= 24 (kao--scroll-lines 50 t   1)))    ; half page
  (should (= 144 (kao--scroll-lines 50 nil 3)))   ; full * count
  (should (= 72 (kao--scroll-lines 50 t   3)))    ; half * count
  (should (= 24 (kao--scroll-lines 50 t   0))))   ; count 0 -> 1

(ert-deftest kao-scroll-lines-short-window-is-zero ()
  "A window too short to scroll yields 0 (a no-op), never negative."
  (should (= 1 (kao--scroll-lines 3 nil 1)))    ; full: (3-2)/1
  (should (= 0 (kao--scroll-lines 3 t   1)))    ; half: (3-2)/2 -> 0
  (should (= 0 (kao--scroll-lines 2 nil 1)))    ; full: (2-2)/1 -> 0
  (should (= 0 (kao--scroll-lines 1 nil 9))))   ; (1-2)=-1 clamped to 0

(ert-deftest kao-scroll-sync-merge-collapses-overlap ()
  "Sync with MERGE merges a secondary the collapsed main now overlaps (line 1833).
Without MERGE the two stay distinct — the `vj'/`vk' (PreserveSelections) path."
  (cl-flet ((run (merge)
              (with-temp-buffer
                (insert "abcdef")
                (setq kao--sels
                      (kao-sels-make
                       :list (list (kao-sel-make :anchor 1 :cursor 2)    ; main
                                   (kao-sel-make :anchor 4 :cursor 6))   ; secondary
                       :main 0))
                (goto-char 5)                ; main collapses to (5.5), inside [4,6]
                (kao-menu--sync-main-to-point merge)
                (length (kao-sels-list kao--sels)))))
    (should (= 2 (run nil)))     ; no merge -> two sels
    (should (= 1 (run t)))))     ; merge -> overlap collapses to one

(defun kao-menu-tests--scroll-pairs ()
  "Current ((anchor . cursor) ...) of `kao--sels'."
  (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
          (kao-sels-list kao--sels)))

(ert-deftest kao-scroll-full-delegates-native-page ()
  "Full page with count 1 = exactly ONE native call with a nil arg:
byte-identical paging to the native keys, `next-screen-context-lines'
respected."
  (with-temp-buffer
    (insert "abcdef")
    (setq-local kao-mode t)             ; guarded command : satisfy `kao--assert-mode'
    (goto-char 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0))
    (let ((calls nil))
      (cl-letf (((symbol-function 'kao-menu--displayed-p) (lambda () t))
                ((symbol-function 'scroll-up-command)
                 (lambda (&optional arg) (push arg calls))))
        (kao-scroll-full-down))
      (should (equal calls '(nil))))))

(ert-deftest kao-scroll-full-count-is-native-pages ()
  "Count N = N native pages: N nil-arg calls, not one call of N lines."
  (with-temp-buffer
    (insert "abcdef")
    (setq-local kao-mode t)             ; guarded command : satisfy `kao--assert-mode'
    (goto-char 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0))
    (setq kao--count 3)
    (let ((calls nil))
      (cl-letf (((symbol-function 'kao-menu--displayed-p) (lambda () t))
                ((symbol-function 'scroll-down-command)
                 (lambda (&optional arg) (push arg calls))))
        (kao-scroll-full-up))
      (should (equal calls '(nil nil nil))))))

(ert-deftest kao-scroll-half-keeps-kakoune-arg ()
  "Half page passes the explicit Kakoune line count — Emacs has no native
half page; `(50-2)/2 = 24' (normal.cc:1598)."
  (with-temp-buffer
    (insert "abcdef")
    (setq-local kao-mode t)             ; guarded command : satisfy `kao--assert-mode'
    (goto-char 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0))
    (let ((calls nil))
      (cl-letf (((symbol-function 'kao-menu--displayed-p) (lambda () t))
                ((symbol-function 'window-body-height)
                 (lambda (&optional _w _px) 50))
                ((symbol-function 'scroll-up-command)
                 (lambda (&optional arg) (push arg calls))))
        (kao-scroll-half-down))
      (should (equal calls '(24))))))

(ert-deftest kao-scroll-edge-signal-propagates ()
  "An edge signal escapes the scroll command for native feedback (a
deviation from Kakoune's silent early-return, input_handler.cc:1805)."
  (with-temp-buffer
    (insert "abcdef")
    (setq-local kao-mode t)             ; guarded command : satisfy `kao--assert-mode'
    (goto-char 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0))
    (cl-letf (((symbol-function 'kao-menu--displayed-p) (lambda () t))
              ((symbol-function 'scroll-up-command)
               (lambda (&optional _arg) (signal 'end-of-buffer nil))))
      (should-error (kao-scroll-full-down) :type 'end-of-buffer))
    (should (equal (kao-menu-tests--scroll-pairs) '((1 . 1))))))

(ert-deftest kao-scroll-syncs-on-point-move-without-window-move ()
  "`scroll-error-top-bottom' moves point to the buffer limit WITHOUT moving
the window; the sync must still fire or the post-command mirror snaps point
back (guard: point OR window moved)."
  (with-temp-buffer
    (insert "abcdef")
    (setq-local kao-mode t)             ; guarded command : satisfy `kao--assert-mode'
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)) :main 0))
    (goto-char 2)
    (cl-letf (((symbol-function 'kao-menu--displayed-p) (lambda () t))
              ((symbol-function 'scroll-up-command)
               (lambda (&optional _arg) (goto-char 6))))
      (kao-scroll-full-down))
    (should (equal (kao-menu-tests--scroll-pairs) '((6 . 6))))))

(ert-deftest kao-scroll-no-move-no-sync ()
  "A scroll that moved neither point nor the window leaves the selections
verbatim (Kakoune's unchanged-selections stance at the edge): the anchor is
NOT collapsed onto the cursor."
  (with-temp-buffer
    (insert "abcdef")
    (setq-local kao-mode t)             ; guarded command : satisfy `kao--assert-mode'
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 4)) :main 0))
    (goto-char 4)                          ; point = main cursor
    (cl-letf (((symbol-function 'kao-menu--displayed-p) (lambda () t))
              ((symbol-function 'scroll-up-command)
               (lambda (&optional _arg) nil)))
      (kao-scroll-full-down))
    (should (equal (kao-menu-tests--scroll-pairs) '((2 . 4))))))

(ert-deftest kao-scroll-mid-count-edge-keeps-prior-pages ()
  "A count>1 scroll that hits the edge mid-loop keeps the pages already
scrolled: each page syncs inside its own step BEFORE the next iteration, so
the escaping signal does not undo the first page's collapse."
  (with-temp-buffer
    (insert "abcdef")
    (setq-local kao-mode t)             ; guarded command : satisfy `kao--assert-mode'
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)) :main 0))
    (goto-char 2)
    (setq kao--count 2)
    (let ((n 0))
      (cl-letf (((symbol-function 'kao-menu--displayed-p) (lambda () t))
                ((symbol-function 'scroll-up-command)
                 (lambda (&optional _arg)
                   (if (= (setq n (1+ n)) 1)
                       (goto-char 5)                    ; page 1 lands at 5
                     (signal 'end-of-buffer nil)))))    ; page 2 hits the edge
        (should-error (kao-scroll-full-down) :type 'end-of-buffer)))
    (should (equal (kao-menu-tests--scroll-pairs) '((5 . 5))))))

(ert-deftest kao-scroll-half-commands-noop-in-batch ()
  "`<c-d>'/`<c-u>' no-op (no error, sels unchanged) when not displayed."
  (dolist (cmd (list #'kao-scroll-half-down #'kao-scroll-half-up))
    (should (equal '((1 . 3) (5 . 7))
                   (kao-menu-tests--run "abc\ndef\nghi"
                                        cmd '((1 . 3) (5 . 7)))))))

(ert-deftest kao-scroll-half-bindings ()
  "`C-d' is bound to half-page-down; `C-u' is deliberately UNBOUND:
it stays Emacs's `universal-argument' (the M-x precedent), resolving
through the emulation alist to the native binding in a kao buffer.
`kao-scroll-half-up' survives as an M-x-able command."
  (should (eq #'kao-scroll-half-down (lookup-key kao-normal-state-map (kbd "C-d"))))
  (should (null (lookup-key kao-normal-state-map (kbd "C-u"))))
  (should (commandp #'kao-scroll-half-up))
  (with-temp-buffer
    (kao-mode 1)
    (unwind-protect
        (should (eq #'universal-argument (key-binding (kbd "C-u"))))
      (kao-mode -1))))

(ert-deftest kao-scroll-full-commands-noop-in-batch ()
  "`<c-f>'/`<c-b>' no-op (no error, sels unchanged) when not displayed."
  (dolist (cmd (list #'kao-scroll-full-down #'kao-scroll-full-up))
    (should (equal '((1 . 3) (5 . 7))
                   (kao-menu-tests--run "abc\ndef\nghi"
                                        cmd '((1 . 3) (5 . 7)))))))

(ert-deftest kao-scroll-full-bindings ()
  "`C-f'/`C-b' and `<PageDown>'/`<PageUp>' are bound to full-page scroll."
  (should (eq #'kao-scroll-full-down (lookup-key kao-normal-state-map (kbd "C-f"))))
  (should (eq #'kao-scroll-full-up   (lookup-key kao-normal-state-map (kbd "C-b"))))
  (should (eq #'kao-scroll-full-down (lookup-key kao-normal-state-map (kbd "<next>"))))
  (should (eq #'kao-scroll-full-up   (lookup-key kao-normal-state-map (kbd "<prior>")))))

;;;; Jump-list auto-push from goto

(defun kao-menu-tests--jumps-after (content sels-spec thunk)
  "In CONTENT with `kao--sels' from SELS-SPEC, run THUNK; return `kao--jumps'."
  (with-temp-buffer
    (insert content)
    (setq-local kao-mode t)             ; guarded command : satisfy `kao--assert-mode'
    (setq kao--sels (kao-sels-make
                     :list (mapcar (lambda (ac)
                                     (kao-sel-make :anchor (car ac) :cursor (cdr ac)))
                                   sels-spec)
                     :main 0)
          kao--jumps nil kao--jump-current 0)
    (funcall thunk)
    kao--jumps))

(ert-deftest kao-menu-goto-coord-pushes-jump ()
  "A coord goto (`gj') pushes ONE jump capturing the pre-goto selections."
  (let ((jumps (kao-menu-tests--jumps-after
                "abc\ndef" '((1 . 3) (5 . 7))
                (cdr (assq ?j (kao-menu-tests--goto-table))))))
    (should (= 1 (length jumps)))
    (should (equal '((1 . 3) (5 . 7))
                   (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                           (kao-sels-list (cddr (car jumps))))))))

(ert-deftest kao-menu-goto-selector-no-push ()
  "A selector goto (`gl') pushes NO jump (only coord targets push)."
  (should (null (kao-menu-tests--jumps-after
                 "abc\ndef" '((2 . 2))
                 (cdr (assq ?l (kao-menu-tests--goto-table)))))))

(ert-deftest kao-menu-goto-line-count-pushes ()
  "`Ng' (count goto) pushes a jump."
  (let ((jumps (kao-menu-tests--jumps-after
                "l1\nl2\nl3" '((1 . 1))
                (lambda () (let ((kao--count 2)) (kao-goto))))))
    (should (= 1 (length jumps)))))

;;;; Line-bound selects/extends (`<a-l>' `<a-h>' `<a-L>' `<a-H>')
;; "foo\nbar": f1 o2 o3 \n4 b5 a6 r7

(ert-deftest kao-menu-select-line-end-anchors-old-cursor ()
  "`<a-l>' selects from the cursor to the line's last non-eol char."
  (should (equal '((2 . 3))
                 (kao-menu-tests--run "foo\nbar" #'kao-select-line-end
                                      '((2 . 2))))))

(ert-deftest kao-menu-select-line-end-on-eol-stays ()
  "`<a-l>' never moves the cursor backward when it sits on the eol."
  (should (equal '((4 . 4))
                 (kao-menu-tests--run "foo\nbar" #'kao-select-line-end
                                      '((4 . 4))))))

(ert-deftest kao-menu-select-line-begin-anchors-old-cursor ()
  "`<a-h>' selects from the cursor back to the line begin (backward sel)."
  (should (equal '((6 . 5))
                 (kao-menu-tests--run "foo\nbar" #'kao-select-line-begin
                                      '((6 . 6))))))

(ert-deftest kao-menu-select-line-bound-maps-over-all ()
  "The line-bound selects apply per selection across the list."
  (should (equal '((1 . 3) (5 . 7))
                 (kao-menu-tests--run "foo\nbar" #'kao-select-line-end
                                      '((1 . 1) (5 . 5))))))

(ert-deftest kao-menu-extend-line-end-keeps-anchor ()
  "`<a-L>' pulls the cursor to the line end, anchor untouched."
  (should (equal '((1 . 3))
                 (kao-menu-tests--run "foo\nbar" #'kao-extend-line-end
                                      '((1 . 2))))))

(ert-deftest kao-menu-extend-line-begin-keeps-anchor ()
  "`<a-H>' pulls the cursor to the line begin, anchor untouched."
  (should (equal '((3 . 1))
                 (kao-menu-tests--run "foo\nbar" #'kao-extend-line-begin
                                      '((3 . 2))))))

(ert-deftest kao-menu-select-line-end-count-refolds ()
  "A count folds the Replace selector (second step re-anchors, as Kakoune)."
  (should (equal '((3 . 3))
                 (kao-menu-tests--run "foo\nbar"
                                      (lambda ()
                                        (let ((kao--count 2))
                                          (kao-select-line-end)))
                                      '((1 . 1))))))

(ert-deftest kao-menu-line-bound-bindings ()
  "M-l/M-h/M-L/M-H resolve to the line-bound select/extend commands."
  (should (eq (lookup-key kao-normal-state-map (kbd "M-l")) #'kao-select-line-end))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-h")) #'kao-select-line-begin))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-L")) #'kao-extend-line-end))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-H")) #'kao-extend-line-begin)))

;;;; Register select (`"', input_handler.cc:322-336)

(defun kao-menu-tests--select-register-with (key)
  "Run `kao-select-register' with `read-key' returning KEY; return the pending.
Fakes `kao-mode' on (the flag) in the current buffer so the guarded command
\ passes `kao--assert-mode'."
  (setq-local kao-mode t)
  (cl-letf (((symbol-function 'read-key) (lambda (&rest _) key)))
    (kao-select-register))
  kao--pending-register)

(ert-deftest kao-menu-select-register-sets-pending ()
  "`\"a' sets the pending register to a (raw char, uppercase kept raw)."
  (with-temp-buffer
    (setq kao--pending-register nil)
    (should (eq (kao-menu-tests--select-register-with ?a) ?a))
    (should (eq (kao-menu-tests--select-register-with ?A) ?A))))

(ert-deftest kao-menu-select-register-escape-cancels ()
  "Escape (and any non-character key) cancels, leaving the pending unchanged."
  (with-temp-buffer
    (setq kao--pending-register ?b)
    (should (eq (kao-menu-tests--select-register-with ?\e) ?b))
    (should (eq (kao-menu-tests--select-register-with 'f1) ?b))))

(ert-deftest kao-menu-select-register-invalid-above-127 ()
  "A char above 127 reports \"invalid register\" and leaves the pending alone."
  (with-temp-buffer
    (setq kao--pending-register nil)
    (should (null (kao-menu-tests--select-register-with ?é)))))

(ert-deftest kao-menu-select-register-binding ()
  "`\"' resolves to `kao-select-register' in normal state."
  (should (eq (lookup-key kao-normal-state-map "\"") #'kao-select-register)))

(ert-deftest kao-menu-select-register-invalid-keeps-pending ()
  "A >127 char leaves an ALREADY-SET pending register intact (the C++ lambda
returns after print_status without touching m_params.reg)."
  (with-temp-buffer
    (setq kao--pending-register ?b)
    (should (eq (kao-menu-tests--select-register-with ?é) ?b))))

;;;; Insert register (`<c-r>', input_handler.cc:1319-1330)

(defmacro kao-menu-tests--insert-register-with (key &rest body)
  "Run BODY then `kao-insert-register' with `read-key' returning KEY."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'read-key) (lambda (&rest _) ,key)))
     ,@body
     (kao-insert-register)))

(ert-deftest kao-menu-insert-register-inserts-at-point ()
  "`<c-r> a' mid-insert inserts register a's string at point."
  (with-temp-buffer
    (insert "hello")
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-register-set ?a '("REG"))
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1))
                           :main 0))
          (kao--enter-insert 'insert (lambda () (goto-char 1)))
          (kao-menu-tests--insert-register-with ?a)
          (should (string= (buffer-string) "REGhello"))
          (should (= (point) 4))                ; insert leaves point after
          (kao-insert-exit)
          (should kao--normal-active))
      (kao-mode -1))))

(ert-deftest kao-menu-insert-register-replays-to-secondaries ()
  "Mid-multi-insert `<c-r>' text joins the net replay at every site."
  (with-temp-buffer
    (insert "ab cd")
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-register-set ?a '("X"))
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)
                                       (kao-sel-make :anchor 4 :cursor 4))
                           :main 0))
          (kao--enter-insert 'insert (lambda () (goto-char 1)) #'kao-sel-min)
          (kao-menu-tests--insert-register-with ?a)
          (kao-insert-exit)                     ; replay distributes "X"
          (should (string= (buffer-string) "Xab Xcd")))
      (kao-mode -1))))

(ert-deftest kao-menu-insert-register-clamps-to-last-string ()
  "The main gets `strings[min(size-1, main-index)]' (insert(), :1445)."
  (with-temp-buffer
    (insert "ab cd ef")
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-register-set ?a '("one" "two"))
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)
                                       (kao-sel-make :anchor 4 :cursor 4)
                                       (kao-sel-make :anchor 7 :cursor 7))
                           :main 2)) ; main index 2, register holds 2 strings
          (kao--enter-insert 'insert (lambda () (goto-char 7)) #'kao-sel-min)
          (kao-menu-tests--insert-register-with ?a)
          ;; main (index 2) clamps to the LAST string "two"
          (should (string= (buffer-string) "ab cd twoef")))
      (kao-mode -1))))

(ert-deftest kao-menu-insert-register-escape-cancels ()
  "Escape (or any non-character key) cancels without touching the buffer."
  (with-temp-buffer
    (insert "hello")
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-register-set ?a '("REG"))
          (kao--enter-insert 'insert (lambda () (goto-char 1)))
          (kao-menu-tests--insert-register-with ?\e)
          (kao-menu-tests--insert-register-with 'f1)
          (should (string= (buffer-string) "hello")))
      (kao-mode -1))))

(ert-deftest kao-menu-insert-register-null-and-uppercase ()
  "`_' reads empty (no-op); an uppercase name lowercases to its register."
  (with-temp-buffer
    (insert "hello")
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-register-set ?b '("B"))
          (kao--enter-insert 'insert (lambda () (goto-char 1)))
          (kao-menu-tests--insert-register-with ?_)
          (should (string= (buffer-string) "hello"))
          (kao-menu-tests--insert-register-with ?B) ; reads register b
          (should (string= (buffer-string) "Bhello")))
      (kao-mode -1))))

(ert-deftest kao-menu-insert-register-unknown-errors ()
  "An unknown name signals Kakoune's \"no such register\" (operator[])."
  (with-temp-buffer
    (insert "hello")
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--enter-insert 'insert (lambda () (goto-char 1)))
          ;; wording pinned at the accessor (kao-register-tests); type here
          (should-error (kao-menu-tests--insert-register-with ?!)
                        :type 'user-error)
          (should (string= (buffer-string) "hello")))
      (kao-mode -1))))

(ert-deftest kao-menu-insert-register-binding ()
  "C-r in the insert state map runs `kao-insert-register'."
  (should (eq (lookup-key kao-insert-state-map (kbd "C-r"))
              #'kao-insert-register)))

(ert-deftest kao-menu-insert-register-digit-capture ()
  "Mid-insert `<c-r> 1' inserts the main selection's capture group 1."
  (with-temp-buffer
    (insert "hello")
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1
                                                     :captures '("h" "CAP")))
                           :main 0))
          (kao--enter-insert 'insert (lambda () (goto-char 1)))
          (kao-menu-tests--insert-register-with ?1)
          (should (string= (buffer-string) "CAPhello"))
          (kao-insert-exit))
      (kao-mode -1))))

;;;; Prompt C-r register insert

(defmacro kao-menu-tests--with-prompt (srcbuf &rest body)
  "Run BODY in a fake minibuffer whose originating buffer is SRCBUF.
`minibuffer-selected-window' is stubbed to the selected window showing
SRCBUF (the batch translation of a live minibuffer's originating window)."
  (declare (indent 1))
  `(with-temp-buffer
     (cl-letf (((symbol-function 'minibuffer-selected-window)
                (lambda () (selected-window))))
       (cl-letf (((symbol-function 'window-buffer)
                  (lambda (&optional _w) ,srcbuf)))
         ,@body))))

(ert-deftest kao-menu-prompt-insert-register-inserts-main-value ()
  "Prompt `C-r' inserts the register value indexed by the SOURCE buffer's
main selection (= `main_sel_register_value', input_handler.cc:758-780)."
  (let ((src (generate-new-buffer " *kao-src*")))
    (unwind-protect
        (progn
          (with-current-buffer src
            (insert "ab") (kao-mode 1)
            (setq kao--sels (kao-sels-make
                             :list (list (kao-sel-make :anchor 1 :cursor 1)
                                         (kao-sel-make :anchor 2 :cursor 2))
                             :main 1)))
          (kao-register-set ?a '("x" "y"))
          (kao-menu-tests--with-prompt src
            (cl-letf (((symbol-function 'kao--read-key) (lambda (&rest _) ?a)))
              (kao-prompt-insert-register))
            (should (string= (buffer-string) "y"))))   ; main 1 -> 2nd string
      (with-current-buffer src (kao-mode -1))
      (kill-buffer src)
      (remhash ?a kao--registers))))

(ert-deftest kao-menu-prompt-insert-register-escape-cancels ()
  "Escape at the register key inserts nothing (the C++ early return)."
  (let ((src (generate-new-buffer " *kao-src*")))
    (unwind-protect
        (kao-menu-tests--with-prompt src
          (cl-letf (((symbol-function 'kao--read-key) (lambda (&rest _) ?\e)))
            (kao-prompt-insert-register))
          (should (string= (buffer-string) "")))
      (kill-buffer src))))

(ert-deftest kao-menu-prompt-insert-register-rejects-control-chars ()
  "A Ctrl-modified register key cancels silently — the QUOTED variant is
deferred (input_handler.cc:762-769); without the guard the char would reach
the resolver and signal a confusing \"no such register\"."
  (let ((src (generate-new-buffer " *kao-src*")))
    (unwind-protect
        (kao-menu-tests--with-prompt src
          (cl-letf (((symbol-function 'kao--read-key)
                     (lambda (&rest _) ?\C-a)))
            (kao-prompt-insert-register))   ; must not signal
          (should (string= (buffer-string) "")))
      (kill-buffer src))))

(ert-deftest kao-menu-prompt-insert-register-dynamic-reads-source-buffer ()
  "A dynamic register (`%' = buffer name) resolves in the ORIGINATING
buffer, not the minibuffer — the `with-current-buffer' pin."
  (let ((src (generate-new-buffer "kao-src-name-pin")))
    (unwind-protect
        (progn
          (with-current-buffer src (insert "x") (kao-mode 1))
          (kao-menu-tests--with-prompt src
            (cl-letf (((symbol-function 'kao--read-key) (lambda (&rest _) ?%)))
              (kao-prompt-insert-register))
            (should (string= (buffer-string) "kao-src-name-pin"))))
      (with-current-buffer src (kao-mode -1))
      (kill-buffer src))))

(ert-deftest kao-menu-register-info-drops-deferred ()
  "No `kao--register-info' row calls a register \"(deferred)\".
The dynamic registers `%'/`.'/`#'/0-9 ship (kao-state registrations) and the
`:' row is out of scope for reasons, not deferral — so the autoinfo box
must never render \"(deferred)\"."
  (dolist (row kao--register-info)
    (should-not (string-match-p "(deferred)" (cdr row)))))

;;;; Mode-off guard.  The goto/view/scroll/register commands
;;;; read selection or mode state; with kao-mode OFF they used to die as the
;;;; cryptic (wrong-type-argument kao-sels nil).  `kao--assert-mode' (Foundation
;;;) now signals a clear `user-error' first.  `kao-prompt-insert-register'
;;;; is EXCLUDED: it is a PromptMode command (kao-keys.el `kao-keys-prompt-alist')
;;;; that runs in the minibuffer, where kao-mode is off by design, and resolves
;;;; its register in the ORIGINATING buffer, tolerant of no selection list —
;;;; guarding it against the current buffer's mode would break every prompt.
;;;; ADDITIVE pins — no existing assertion is touched.

(defconst kao-menu-tests--guarded-commands
  '(kao-select-line-end kao-select-line-begin
    kao-extend-line-end kao-extend-line-begin
    kao-goto kao-goto-extend
    kao-view kao-view-locked
    kao-scroll-half-down kao-scroll-half-up
    kao-scroll-full-down kao-scroll-full-up
    kao-select-register kao-insert-register)
  "Every M-x-discoverable kao-menu command carrying the `kao--assert-mode' guard.
The guard is the FIRST form, so it fires before any `read-key' the command
would otherwise issue (goto/view/register).  Excludes
`kao-prompt-insert-register' (PromptMode, modeless by design).  Kept
exhaustive: an unguarded command here is caught by
`kao-menu-commands-signal-user-error-with-mode-off'.")

(ert-deftest kao-menu-commands-signal-user-error-with-mode-off ()
  "Each guarded goto/view/scroll/register command signals `user-error'
\"kao-mode is not active\" with the mode off, instead of wrong-type-argument ."
  (dolist (cmd kao-menu-tests--guarded-commands)
    (with-temp-buffer
      (fundamental-mode)                ; kao-mode is off; `kao--sels' is nil
      (insert "abc")
      (let ((err (should-error (call-interactively cmd) :type 'user-error)))
        (should (string-match-p "kao-mode is not active"
                                (error-message-string err)))))))

(ert-deftest kao-menu-prompt-insert-register-stays-modeless ()
  "`kao-prompt-insert-register' must NOT gain the mode guard: it is a PromptMode
command run in the minibuffer (kao-mode off there).  With the mode off it does
NOT signal the \"kao-mode is not active\" guard error (it cancels on the empty
prompt read instead), proving the guard was not applied."
  (with-temp-buffer
    (fundamental-mode)
    (insert "abc")
    ;; Feed an immediate Escape so the one-shot read-key cancels cleanly rather
    ;; than blocking on input; a guarded command would `user-error' before ever
    ;; reaching the read.
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\e)))
      (should-not
       (condition-case err
           (progn (call-interactively 'kao-prompt-insert-register) nil)
         (user-error
          (string-match-p "kao-mode is not active"
                          (error-message-string err))))))))

(provide 'kao-menu-tests)
;;; kao-menu-tests.el ends here
