;;; kao-edit.el --- Edits for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 4.  The editing commands:
;;
;;   i a I A o O   insert-entry commands — position point at the Kakoune-faithful
;;                 site (input_handler.cc `prepare', l.1456-1533) and hand off to
;;                 the insert state machine in kao-state.el (single-selection;
;;                 multi-selection insert-replay is a later P2 slice).
;;   y d c         yank / delete / change.
;;   p P R         paste after / before / replace (register-cycling, multi-sel).
;;   ~ ` <a-`> r   in-place case transforms and replace-char.
;;   > <           indent / deindent.
;;
;; `d' and `p'/`P'/`R' operate across ALL selections via the marker army
;; (`kao--multi-edit'): each selection's endpoints are promoted to markers, the
;; per-selection edit runs ascending, positions are demoted to integers and
;; clamped, and the whole pass is one undo unit.  `c' is still single-selection
;; (multi-`c' rides with insert-replay).

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'kao-selection)
(require 'kao-state)
(require 'kao-history)
(require 'kao-register)

(defun kao-insert (&optional count)
  "Kakoune `i': insert before each selection (at its min).
Multi-selection: the text typed at the main is replayed before every selection
on exit.  COUNT (default the pending count) is stored for `.' but has
no effect here — `prepare' ignores it for this mode (input_handler.cc:1463)."
  (interactive)
  (kao--assert-mode)
  (let ((n (or count (kao--repeat-count)))
        (p (kao-sel-min (kao--main-sel))))
    (kao--enter-insert 'insert (lambda () (goto-char p)) #'kao-sel-min)
    ;; Recorded AFTER the guarded entry: Kakoune updates `m_last_insert' only
    ;; once the ctor's `throw_if_read_only' has passed (input_handler.cc:1189+).
    (setq kao--last-insert-opener #'kao-insert
          kao--last-insert-count n)))

(defun kao-append (&optional count)
  "Kakoune `a': append after each selection (at max+1).
Multi-selection: the text typed at the main is replayed after every selection
on exit.  COUNT is stored for `.'; `prepare' ignores it here."
  (interactive)
  (kao--assert-mode)
  (let ((n (or count (kao--repeat-count)))
        (p (kao-sel-end (kao--main-sel))))
    (kao--enter-insert 'append (lambda () (goto-char (min p (point-max))))
                       (lambda (s) (min (kao-sel-end s) (point-max))))
    (setq kao--last-insert-opener #'kao-append
          kao--last-insert-count n)))

(defun kao--first-nonblank-site (pos)
  "Position of the first non-blank char on POS's line, or its bol if all blank.
Mirrors Kakoune `InsertAtLineBegin' (input_handler.cc:1518-1528): on an
all-blank line the site stays at column 0 rather than moving onto the newline."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (let ((bol (point)))
      (skip-chars-forward " \t" (line-end-position))
      (if (= (point) (line-end-position)) bol (point)))))

(defun kao--line-end-site (pos)
  "Position of the end of POS's line (before the newline)."
  (save-excursion (goto-char pos) (line-end-position)))

(defun kao-insert-line-begin (&optional count)
  "Kakoune `I': insert at the first non-blank of each selection's line.
Multi-selection: the text typed at the main is replayed at each line's first
non-blank on exit.  COUNT is stored for `.'; `prepare' ignores it here."
  (interactive)
  (kao--assert-mode)
  (let ((n (or count (kao--repeat-count)))
        (p (kao-sel-min (kao--main-sel))))
    (kao--enter-insert 'insert-line-begin
                       (lambda () (goto-char (kao--first-nonblank-site p)))
                       (lambda (s) (kao--first-nonblank-site (kao-sel-min s))))
    (setq kao--last-insert-opener #'kao-insert-line-begin
          kao--last-insert-count n)))

(defun kao-append-line-end (&optional count)
  "Kakoune `A': append at the end of each selection's line (before the newline).
Multi-selection: replayed at each line end on exit.  COUNT is stored
for `.'; `prepare' ignores it here."
  (interactive)
  (kao--assert-mode)
  (let ((n (or count (kao--repeat-count)))
        (p (kao-sel-max (kao--main-sel))))
    (kao--enter-insert 'append-line-end
                       (lambda () (goto-char (kao--line-end-site p)))
                       (lambda (s) (kao--line-end-site (kao-sel-max s))))
    (setq kao--last-insert-opener #'kao-append-line-end
          kao--last-insert-count n)))

(defcustom kao-open-indent nil
  "When non-nil, `o' / `O' indent each opened line (`indent-according-to-mode').
Off (default) is faithful bare Kakoune core: `o' / `O' open unindented lines.
On is the native-Emacs translation of the standard distribution's InsertChar
indent hooks that fire on OpenLine{Below,Above} (input_handler.cc:1497/:1515,
the power-of-Emacs family).  Does NOT apply to `<a-o>' / `<a-O>'
\(`add_empty_line' runs no InsertChar hook, normal.cc:2251)."
  :type 'boolean
  :group 'kao)

(defun kao--open-indent-sites (sites)
  "Indent each opened-line marker in SITES when `kao-open-indent' is non-nil.
The insertion-type-t site markers advance past the inserted indentation, so
each site lands after it and the replay types there.  This mirrors
Kakoune running Hook::InsertChar \"\\n\" at prepare() time, before any typing
\(input_handler.cc:1497/:1515).  A mode's broken indent function is demoted so
the open still enters insert (setup-abort path stays for hard errors)."
  (when kao-open-indent
    (dolist (m sites)
      (goto-char (marker-position m))
      (with-demoted-errors "kao-open-indent: %S"
        (indent-according-to-mode)))))

(defun kao-open-below (&optional count)
  "Kakoune `o': open COUNT blank line(s) below each selection's line.
Multi-selection: COUNT lines are opened below every selection (via the marker
army so the newline inserts coordinate), ONE insert site per opened line —
N selections become N*COUNT sites, the main site is the LAST of the original
main's group (`main_index()*count + count - 1', input_handler.cc:1483-1497);
the typed text replays on every site on exit.  COUNT defaults to the
pending count."
  (interactive)
  (kao--assert-mode)
  (let* ((n (max 1 (or count (kao--repeat-count)))) ; count>0?:1, prepare :1485/:1503
         (sels (kao-sels-list kao--sels))
         (main-idx (kao-sels-main kao--sels)))
    (kao--with-insert-setup nil
      (let* ((maxes (mapcar (lambda (s) (copy-marker (kao-sel-max s) t)) sels))
             (sites (cl-loop for m in maxes nconc
                             (progn (goto-char (marker-position m))
                                    (end-of-line)
                                    (insert (make-string n ?\n))
                                    (prog1   ; bol of each opened line
                                        (cl-loop for i from 1 to n collect
                                                 (copy-marker
                                                  (+ (- (point) n) i) t))
                                      (set-marker m nil))))))
        (kao--open-indent-sites sites)     ; opt-in autoindent
        (kao--enter-insert-at-sites sites (+ (* main-idx n) n -1))))
    (setq kao--last-insert-opener #'kao-open-below
          kao--last-insert-count n)))

(defun kao-open-above (&optional count)
  "Kakoune `O': open COUNT blank line(s) above each selection's line.
Multi-selection: COUNT lines are opened above every selection (via the marker
army), one insert site per opened line in ascending order, main site the LAST
of the original main's group (`main_index()*count + count - 1',
input_handler.cc:1499-1515); the typed text replays on every site on exit
\.  COUNT defaults to the pending count."
  (interactive)
  (kao--assert-mode)
  (let* ((n (max 1 (or count (kao--repeat-count)))) ; count>0?:1, prepare :1485/:1503
         (sels (kao-sels-list kao--sels))
         (main-idx (kao-sels-main kao--sels)))
    (kao--with-insert-setup nil
      (let* ((mins (mapcar (lambda (s) (copy-marker (kao-sel-min s) t)) sels))
             (sites (cl-loop for m in mins nconc
                             (progn (goto-char (marker-position m))
                                    (beginning-of-line)
                                    (insert (make-string n ?\n))
                                    (prog1   ; each opened blank line, ascending
                                        (cl-loop for i from 1 to n collect
                                                 (copy-marker
                                                  (- (point) (- n (1- i)))
                                                  t))
                                      (set-marker m nil))))))
        (kao--open-indent-sites sites)     ; opt-in autoindent
        (kao--enter-insert-at-sites sites (+ (* main-idx n) n -1))))
    (setq kao--last-insert-opener #'kao-open-above
          kao--last-insert-count n)))

(defun kao-add-line-below ()
  "Kakoune `<a-o>': add `count' empty line(s) below each selection's line.
Does NOT enter insert mode; the selection list keeps its order, main, and the
characters it covers (`add_empty_line<false>', normal.cc:2250).  Newlines are
inserted at the start of the line AFTER the selection's max line (Kakoune's
`max.line+1') — i.e. past that line's terminating newline — so even a whole-line
selection (cursor on its newline) stays put.  The marker army replaces Kakoune's
explicit `i*count' line-shift compensation.  One undo unit.  The list is sorted
first so the ascending marker-army precondition holds (cf. the edit siblings)."
  (interactive)
  (kao--assert-mode)
  ;; direct install (no-refresh): sort in place, then feed kao--multi-edit — the
  ;; primitive reads kao--sels at entry, no seam clamp/refresh wanted here.
  (setq kao--sels (kao-sels-sort kao--sels))
  (let ((n (kao--repeat-count)))
    (kao--multi-edit
     (lambda (am cm _i)
       (goto-char (max (marker-position am) (marker-position cm)))
       (forward-line 1)            ; bol of next line (past this line's \n)
       (insert (make-string n ?\n))
       (cons (marker-position am) (marker-position cm))))))

(defun kao-add-line-above ()
  "Kakoune `<a-O>': add `count' empty line(s) above each selection's line.
Does NOT enter insert mode; an above-insert lands at the line start before the
selection, so the insertion-type-t markers advance and each selection shifts
down with its text (`add_empty_line<true>', normal.cc:2250).  One undo unit.
The list is sorted first so the ascending marker-army precondition holds."
  (interactive)
  (kao--assert-mode)
  ;; direct install (no-refresh): sort in place, then feed kao--multi-edit — the
  ;; primitive reads kao--sels at entry, no seam clamp/refresh wanted here.
  (setq kao--sels (kao-sels-sort kao--sels))
  (let ((n (kao--repeat-count)))
    (kao--multi-edit
     (lambda (am cm _i)
       (goto-char (min (marker-position am) (marker-position cm)))
       (beginning-of-line)
       (insert (make-string n ?\n))
       (cons (marker-position am) (marker-position cm))))))

;;;; Repeat last insert (.)

(defun kao-repeat-insert ()
  "Kakoune `.': repeat the last insert with its mode at the current selections.
Re-runs the recorded opener (`kao--last-insert-opener'): an entry command
\(i/a/I/A/o/O) or the no-yank change core, which positions the insert session at
the CURRENT selections (re-running Kakoune's `prepare', so `.' after `c'
re-erases without yanking and after `o'/`O' opens fresh line(s)).  The recorded
net text (`kao--last-insert-text') is then inserted at the main and replayed at
each secondary site on exit (`repeat_last_insert', input_handler.cc:1598).
One undo unit; multi-selection repeat is free.  No-op with a message when no
insert has run yet.  Only the net text is replayed, not non-text insert keys
\(the standing trade-off).  The opener re-runs with the STORED count
\(`m_last_insert.count', input_handler.cc:1197/:1610) — a count typed before
`.' itself is ignored (`repeat_last_insert' takes unnamed NormalParams,
normal.cc:172-175), so `3.' does not multiply."
  (interactive)
  (kao--assert-mode)
  ;; Reachable mid-session only via the one-shot normal (`<a-;> .').
  ;; Kakoune replays the last insert in a Draft context kao does not model;
  ;; running the opener+exit here would close the OPEN session's undo group
  ;; out from under it, so the guard makes the unsupported edge loud.
  (when kao--insert-undo-handle
    (user-error "cannot repeat an insert from an open insert session"))
  (if (not (and kao--last-insert-opener kao--last-insert-text))
      (message "kao: no insert to repeat")
    (let ((opener kao--last-insert-opener)
          (count kao--last-insert-count)
          (text kao--last-insert-text))
      (funcall opener count)            ; re-enter with the STORED count
      (insert text)                     ; type the recorded text at the main
      (kao-insert-exit))))              ; replay at secondaries, rebuild sels

;;;; Multi-selection edit infrastructure (marker army)

(defun kao--multi-edit (per-sel)
  "Edit every selection via the marker army, as one undo unit.
Each selection's anchor and cursor are promoted to markers so positions stay
valid as the buffer changes; PER-SEL is called as (PER-SEL ANCHOR-MARKER
CURSOR-MARKER INDEX) in ascending order, performs the buffer edit, and returns
\(NEW-ANCHOR . NEW-CURSOR) as integer positions valid after the edit.  The
results are demoted to integers, clamped to on-char positions, and installed
with the original order and main index preserved (no merge — the caller
decides).  The whole pass is amalgamated into one undo step (one command =
one undo unit).

Editing ascending while reading geometry from markers keeps it correct: every
later edit is at a higher position, so an already-computed lower result never
shifts.  An OVERLAPPING list (pairwise combine installs unmerged,
normal.cc:2103) can break that ascent for the integer results; the
markers still translate each UPCOMING selection exactly (= Kakoune's per-sel
`update_ranges', selection.cc:388-397 — Kakoune never re-translates earlier
results either).  Append paste restores the full guarantee via
`last'-tracking (see `kao--paste'); Insert/Replace on an overlapping list is
the same tolerant best-effort as Kakoune's `may_append=false' branch."
  (let* ((sels (kao-sels-list kao--sels))
         (main (kao-sels-main kao--sels))
         (handle (prepare-change-group))
         (marks nil)
         (ok nil))
    (activate-change-group handle)
    (unwind-protect
        (progn
          ;; Insertion-type t: a marker advances past text inserted at its own
          ;; position, so it keeps tracking its character when an earlier
          ;; selection pastes exactly at this one's boundary (adjacent sels).
          (setq marks (mapcar (lambda (s) (cons (copy-marker (kao-sel-anchor s) t)
                                                (copy-marker (kao-sel-cursor s) t)))
                              sels))
          (let ((results (cl-loop for (am . cm) in marks for i from 0
                                  collect (funcall per-sel am cm i))))
            ;; direct install (retag-order): kao--sels-edit-pending must be set
            ;; BEFORE the refresh, but kao-set-selections-raw refreshes internally.
            (setq kao--sels
                  (kao-sels-make
                   :list (mapcar (lambda (r)
                                   (kao--clamp-sel
                                    (kao-sel-make :anchor (car r) :cursor (cdr r))))
                                 results)
                   :main main))
            ;; Post-edit selections are already in the new buffer frame; the
            ;; commit re-tags the live list, never translates it.
            (setq kao--sels-edit-pending t)
            (kao--refresh))
          (setq ok t))
      ;; Always free the markers.  On NORMAL completion amalgamate + accept the
      ;; pass as one undo unit; on a NON-LOCAL EXIT (PER-SEL errors, or `C-g'
      ;; during a pipe's shell call) CANCEL the group so the buffer is
      ;; left UNTOUCHED: one command = one undo unit, never a partial one.
      (dolist (m marks) (set-marker (car m) nil) (set-marker (cdr m) nil))
      (if ok
          (progn (undo-amalgamate-change-group handle)
                 (accept-change-group handle))
        (cancel-change-group handle)))))

(defun kao--edit-keeping-sels (edit-fn)
  "Run EDIT-FN (arbitrary buffer edits) as one undo unit, keeping the selections.
Every selection's anchor and cursor are carried on insertion-type-t markers
across the edit, then `kao--sels' is rebuilt from the markers (demoted to
integers, clamped to on-char positions) with the original main index preserved.
EDIT-FN is called with the marker list (each element a (ANCHOR-MARKER.
CURSOR-MARKER) cons, in selection order) and performs the buffer edits directly;
callers that do not need the markers ignore the argument.  This is the faithful
translation of the Kakoune commands that edit the buffer WITHOUT reselecting
\(indent/deindent, tabs<->spaces, align, copy_indent): the selections just shift
with the text (`ScopedSelectionEdition' / `SelectionList::update')."
  (let* ((sels (kao-sels-list kao--sels))
         (main (kao-sels-main kao--sels))
         (handle (prepare-change-group))
         (marks nil)
         (ok nil))
    (activate-change-group handle)
    (unwind-protect
        (progn
          (setq marks (mapcar (lambda (s) (cons (copy-marker (kao-sel-anchor s) t)
                                                (copy-marker (kao-sel-cursor s) t)))
                              sels))
          (funcall edit-fn marks)
          ;; direct install (retag-order): kao--sels-edit-pending must be set
          ;; BEFORE the refresh, but kao-set-selections-raw refreshes internally.
          (setq kao--sels
                (kao-sels-make
                 :list (mapcar (lambda (m)
                                 (kao--clamp-sel
                                  (kao-sel-make :anchor (marker-position (car m))
                                                :cursor (marker-position (cdr m)))))
                               marks)
                 :main main))
          ;; Post-edit selections are already in the new buffer frame; the
          ;; commit re-tags the live list, never translates it.
          (setq kao--sels-edit-pending t)
          (kao--refresh)
          (setq ok t))
      (dolist (m marks) (set-marker (car m) nil) (set-marker (cdr m) nil))
      ;; On a NON-LOCAL EXIT (EDIT-FN signals, or C-g mid-pass) CANCEL the group
      ;; so the buffer is left UNTOUCHED — the atomicity every edit path
      ;; gets; on normal completion amalgamate + accept as one undo unit.
      (if ok
          (progn (undo-amalgamate-change-group handle)
                 (accept-change-group handle))
        (cancel-change-group handle)))))

;;;; Public per-selection edit primitives (config substrate, A5)

(defun kao-edit-selections (fn)
  "Edit every selection via FN as one undo unit, RE-DERIVING selections.
The public form of the marker-army edit (`kao--multi-edit').  FN is called as
\(funcall FN ANCHOR-MARKER CURSOR-MARKER INDEX) for each selection in ascending
order; it performs the buffer edit and returns (NEW-ANCHOR . NEW-CURSOR) as
integer positions valid after that edit.  The returned pairs become the new
selection list (the paste/`R'/pipe-replace model — selections are derived from
FN's result), with the original order and main index preserved and the whole
pass amalgamated into one undo step.  Build each delimiter/replacement from the
marker's live `marker-position', not a captured integer, so earlier edits in
the pass stay accounted for.  Use `kao-edit-keeping-selections' when the
original selections should instead be carried across the edit."
  (kao--multi-edit fn))

(defun kao-edit-keeping-selections (fn)
  "Run FN's buffer edits as one undo unit, CARRYING the selections across them.
The public form of `kao--edit-keeping-sels'.  FN is called with the marker list
\(each element a (ANCHOR-MARKER . CURSOR-MARKER) cons, in selection order) and
edits the buffer directly; the selections are then rebuilt from the markers
\(insertion-type t, so each tracks its text through FN's inserts) with the
original main index preserved, the whole pass one undo step.  This is the
surround-add primitive: inserting an open delimiter before a selection and a
close after it leaves the original span selected.  Use `kao-edit-selections'
when FN should choose the resulting selections instead."
  (kao--edit-keeping-sels fn))

;;;; Yank / delete / change

(defun kao--yank-selections (reg)
  "Copy each selection's text to register REG (the `if (yank)' write).
Shared by `y'/`d'/`c' — the silent register write of `yank' /
`erase_selections<true>' / `change<true>' (normal.cc:785/797/835); only `y'
adds the status message.  Mirrors ALL selections (newline-joined) to the
system clipboard + `kill-ring' so an external paste receives the whole yank
\; the internal list stays canonical for kao's own per-selection
paste."
  (let* ((sels (kao-sels-list kao--sels))
         (strings (mapcar (lambda (s)
                            ;; Clamp inclusive end to point-max: empty buffer /
                            ;; phantom trailing line yield "".
                            (buffer-substring (kao-sel-beg s)
                                              (min (kao-sel-end s) (point-max))))
                          sels)))
    (kao-register-yank strings reg)))

(defun kao-yank ()
  "Kakoune `y': copy each selection's text to the pending or default register.
The faithful status message prints the RAW register char (normal.cc:786-789
prints `params.reg' before `operator[]' lowercases it for storage)."
  (interactive)
  (kao--assert-mode)
  (let ((reg (kao--register-arg kao-register-default)))
    (kao--yank-selections reg)
    (message "kao: yanked %d selections to register %c"
             (length (kao-sels-list kao--sels)) reg)))

(defun kao--delete-per-sel (am cm _index)
  "Delete the region spanned by markers AM/CM; return the gap as the new selection.
Each selection collapses to a single char at the deletion position
\(`SelectionList::erase')."
  (let* ((a (marker-position am)) (c (marker-position cm))
         (beg (min a c)) (end (min (1+ (max a c)) (point-max)))) ; clamp: empty buffer
    (delete-region beg end)
    (cons beg beg)))

(defun kao--erase-selections ()
  "Merge overlapping selections, then delete each region to its gap.
Shared core of `d'/`<a-d>' (`SelectionList::erase'): merge-before, delete via
the marker army, each region collapsing to its gap, no post-merge.  One undo
unit.  The yank is the caller's concern — the only difference between
`erase_selections<true>' (`d') and `<false>' (`<a-d>')."
  ;; direct install (no-refresh): sort+merge before kao--multi-edit — the
  ;; primitive reads kao--sels at entry, no seam clamp/refresh wanted here.
  (setq kao--sels (kao-sels-sort-and-merge-overlapping kao--sels))
  (kao--multi-edit #'kao--delete-per-sel))

(defun kao-delete ()
  "Kakoune `d': yank every selection, then delete them all.
Faithful to `erase_selections<true>'/`SelectionList::erase': the (unmerged)
selection contents are written to the pending or default register first
\(silently — only `y' prints, normal.cc:797), then the regions are deleted
via the shared `kao--erase-selections' core.  Single command = one undo unit."
  (interactive)
  (kao--assert-mode)
  (kao--yank-selections (kao--register-arg kao-register-default))
  (kao--erase-selections))

(defun kao-delete-no-yank ()
  "Kakoune `<a-d>': delete every selection WITHOUT yanking.
Faithful to `erase_selections<false>' (normal.cc:793) — the `if (yank)' register
write is skipped; the deletion is otherwise identical to `d'."
  (interactive)
  (kao--assert-mode)
  (kao--erase-selections))

(defun kao--change-selections (&optional count)
  "Merge overlapping selections, delete each region, open insert at the gaps.
COUNT is stored for `.'; `prepare(Replace)' ignores it (input_handler.cc:1467).
The shared core of `c'/`<a-c>' (`enter_insert<Replace>', input_handler.cc:1467):
the list is merged-overlapping, each region deleted via the marker army to its
gap, then the insert session opens at the gaps (main -> point, others ->
`kao--insert-secondary-sites'); the typed text replays at every gap on exit
\.  The delete and the typed text are one undo unit.  The yank is the
caller's concern (the only difference between `change<true>' and `<false>')."
  ;; Repeat (`.') re-runs THIS no-yank core, not `kao-change': `prepare(Replace)'
  ;; re-erases without yanking (input_handler.cc:1468; the yank in `change<yank>'
  ;; is outside insert mode, normal.cc:808-812).
  (let* ((merged (kao-sels-sort-and-merge-overlapping kao--sels))
         (sels (kao-sels-list merged))
         (main-idx (kao-sels-main merged)))
    (kao--with-insert-setup nil         ; change is not Append (no step-back)
      ;; Recorded only past the guard (Kakoune sets `m_last_insert' after the
      ;; ctor's `throw_if_read_only' but BEFORE `prepare's erase,
      ;; input_handler.cc:1189-1199): a read-only BUFFER must not clobber what
      ;; `.' replays, but a mid-erase throw clobbers it in Kakoune too — so
      ;; rollback deliberately does NOT restore the opener.
      (setq kao--last-insert-opener #'kao--change-selections
            kao--last-insert-count (or count (kao--repeat-count)))
      (let* ((marks (mapcar (lambda (s) (cons (copy-marker (kao-sel-anchor s) t)
                                              (copy-marker (kao-sel-cursor s) t)))
                            sels))
             (gaps (cl-loop for (am . cm) in marks collect
                            (let ((beg (min (marker-position am) (marker-position cm)))
                                  ;; clamp inclusive end to point-max: `c' in an
                                  ;; empty buffer deletes nothing, still opens
                                  ;; insert.
                                  (end (min (1+ (max (marker-position am)
                                                     (marker-position cm)))
                                            (point-max))))
                              (delete-region beg end)
                              (prog1 (copy-marker beg t)
                                (set-marker am nil) (set-marker cm nil))))))
        (kao--enter-insert-at-sites gaps main-idx)))))

(defun kao-change ()
  "Kakoune `c': yank every selection, delete them, and insert at each gap.
Faithful to `change<true>': the unmerged contents are written to the pending
or default register first (silently, normal.cc:835), then the shared
`kao--change-selections' core deletes and opens the insert session."
  (interactive)
  (kao--assert-mode)
  (kao--yank-selections (kao--register-arg kao-register-default))
  (kao--change-selections))

(defun kao-change-no-yank ()
  "Kakoune `<a-c>': change every selection WITHOUT yanking.
Faithful to `change<false>' (normal.cc:805) — the `if (yank)' register write is
skipped; the delete + insert session is otherwise identical to `c'."
  (interactive)
  (kao--assert-mode)
  (kao--change-selections))

;;;; Paste (after / before / replace)

(defun kao--register-linewise-p (strings)
  "Non-nil when STRINGS is linewise: every string is non-empty and ends in \\n.
Mirrors Kakoune's linewise test in `paste' (normal.cc:837)."
  (and strings
       (seq-every-p (lambda (s) (and (> (length s) 0)
                                     (= (aref s (1- (length s))) ?\n)))
                    strings)))

(defun kao--paste-strings ()
  "The strings to paste (clipboard bridge + the `\"' register prefix).
An EXPLICIT pending register wins outright: `\"ap' reads register a and the
external-clipboard check is skipped (the user named their source; Kakoune has
no clipboard in this path at all).  Otherwise, when the system clipboard
changed since kao's last `y' — an external copy, or kao never yanked but the
clipboard is non-empty — return that one string so it pastes to every
selection (like `yank').  Otherwise return kao's internal default-register
list, preserving the per-selection i-th->i-th cycling of a multi-selection
yank."
  (cond (kao--pending-register
         (kao-register-get kao--pending-register))
        ((kao-clipboard-external-p)
         (list (kao-clipboard-current)))
        (t (kao-register-get kao-register-default))))

(defun kao--paste-goto (mode smin smax linewise)
  "Position point for a paste of MODE over the span [SMIN, SMAX], return point.
Mirrors Kakoune's `paste_pos' (normal.cc:816) plus the Replace-mode delete that
Kakoune folds into `buffer.replace': for `replace' the [SMIN, SMAX] span is
deleted (with the empty-buffer end-clamp) and point left at SMIN; for
`insert' point goes to SMIN, or that line's start when LINEWISE; for `append'
LINEWISE goes past the max line's terminating newline (adding one at eob when
the last line has none), else charwise past SMAX (`char_next', clamped for an
empty buffer).  Callers doing `last'-tracking (`kao--paste') pass the
already-maxed SMAX on the append path; `kao--paste-all' passes the raw span."
  (cond
   ((eq mode 'replace)
    (delete-region smin (min (1+ smax) (point-max))) ; clamp: empty buffer
    (goto-char smin))
   ((eq mode 'insert)
    (goto-char smin) (when linewise (beginning-of-line)))
   ((eq mode 'append)
    (if linewise
        (progn (goto-char smax) (end-of-line)
               (if (eobp) (insert "\n") (forward-char 1)))
      (goto-char (min (1+ smax) (point-max))))))
  (point))

(defun kao--paste (mode)
  "Paste to every selection, cycling the source strings.
The i-th selection receives `strings[i mod N]' (so a prior N-selection yank
pastes the i-th string to the i-th cursor; a single string goes to all).  The
source is the system clipboard when it changed externally, else kao's internal
default register (`kao--paste-strings').  MODE is `append' (`p'),
`insert' (`P'), or `replace' (`R').  Charwise pastes at max+1 / min / over
[min,max+1]; linewise (every source string ends in \\n) pastes on the line
below max / at the line of min (Replace ignores linewise).  Each new selection
covers its pasted span; the N pastes run through the marker army as one undo
unit.

Append paste tracks `last' — the previous result's end — and pastes after
`(max smax last)', Kakoune's `std::max(max, last)' site (normal.cc:850).
That one rule covers both faithful behaviours at once: several selections on
the SAME line stack their linewise pastes below each other, and an
OVERLAPPING list (pairwise combine, the unmerged producer) keeps every
append site strictly past every earlier result, so the integer result spans
never go stale (`kao--multi-edit''s ascending-edit invariant is restored by
construction).  Insert/Replace never read `last' (Kakoune's `paste_pos'
ignores the max argument for Insert; Replace bypasses it)."
  ;; A null/unset register reads [""] (register_manager.cc:30-36), so `R' erases
  ;; every selection and `p'/`P' collapse at the paste site — never a no-op bail.
  ;; Only the `<a-p>' family (`kao--paste-all') keeps the `paste_all' throw.
  (let ((strings (or (kao--paste-strings) '(""))))
    (let ((linewise (kao--register-linewise-p strings))
          (n (length strings))
          (last 0))
      (kao--multi-edit
       (lambda (am cm i)
         (let* ((a (marker-position am)) (c (marker-position cm))
                ;; Direction from the pre-edit markers (as `kao--rotate-content'):
                ;; Kakoune's paste assigns through the direction-preserving
                ;; min()/max() refs (selection.hh:51-56, normal.cc:843-852).
                (forward-sel (>= c a))
                (smin (min a c)) (smax (max a c))
                (str (nth (mod i n) strings))
                (len (length str))
                ;; Append tracks `last' via std::max(max, last) (normal.cc:850);
                ;; Insert/Replace use the raw max (see `kao--paste-goto').
                (pos (kao--paste-goto
                      mode smin (if (eq mode 'append) (max smax last) smax)
                      linewise)))
           (insert str)
           ;; `last' stays the numeric END of the pasted span regardless of
           ;; direction (it feeds the next append's std::max(max, last)).
           (setq last (+ pos (max 0 (1- len))))
           (if forward-sel (cons pos last) (cons last pos))))))))

(defun kao-paste-after ()
  "Kakoune `p': paste after the selection (clipboard or register).
A count `Np' repeats the WHOLE paste `kao--repeat-count' times
\(`repeated<paste>', normal.cc:2283-2287/2493): each pass re-resolves the
now-grown selection spans and pastes again after them, matching Kakoune's
`do { paste } while(--count > 0)' (the operation is repeated, not the inner
insert).  The passes share one command, so they amalgamate into a single
undo unit (= Kakoune's one outer `ScopedEdition')."
  (interactive)
  (kao--assert-mode)
  (dotimes (_ (kao--repeat-count))
    (kao--paste 'append)))

(defun kao-paste-before ()
  "Kakoune `P': paste before the selection (clipboard or register).
A count `NP' repeats the whole paste `kao--repeat-count' times
\(`repeated<paste>', normal.cc:2283-2287/2494), like `p' (`kao-paste-after')."
  (interactive)
  (kao--assert-mode)
  (dotimes (_ (kao--repeat-count))
    (kao--paste 'insert)))

(defun kao-replace ()
  "Kakoune `R': replace the selection (clipboard or register)."
  (interactive)
  (kao--assert-mode)
  (kao--paste 'replace))

;;;; Paste all (every source string) — <a-p> / <a-P> / <a-R>

(defun kao--paste-all (mode)
  "Paste ALL source strings (concatenated) to every selection, selecting each.
Faithful to Kakoune `paste_all' (normal.cc:858).  MODE is `append' (`<a-p>'),
`insert' (`<a-P>'), or `replace' (`<a-R>').  Source is the clipboard-or-
register list (`kao--paste-strings'); empty strings are dropped from the pasted
text but still make the paste charwise (`linewise' needs EVERY source string
non-empty and newline-terminated).  The selection list is rebuilt to M*N
selections (M originals * N non-empty strings): each covers one pasted string
with its cursor on the string's last char; `main' = last (selection.hh:126).
Markers track positions across the inserts (= Kakoune's ForwardChangesTracker,
selection.cc:384, the non-overlap branch; for an overlapping list the markers
play the per-sel `update_ranges' role, selection.cc:388-397), so the whole
pass is one undo unit, installed in build order without a post-merge.
Unlike `paste' (normal.cc:844), Kakoune's `paste_all' has NO `last'-tracking
\(normal.cc:886-897: `paste_pos' receives the selection's own min/max), so
same-line linewise multi-paste recomputing each site from the source line —
with the later result selections built over the earlier ones — is FAITHFUL
here, not a divergence; so is the earlier-result staleness on an overlapping
list (Kakoune never re-translates already-built results)."
  (let* ((src (kao--paste-strings))
         (strings (seq-filter (lambda (s) (> (length s) 0)) src)))
    (if (null strings)
        (message "kao: nothing to paste")
      (let* ((all (apply #'concat strings))
             (lens (mapcar #'length strings))
             (linewise (kao--register-linewise-p src))
             (sels (kao-sels-list kao--sels))
             (handle (prepare-change-group))
             (marks nil)
             (result nil)
             (ok nil))
        (activate-change-group handle)
        (unwind-protect
            (progn
              (setq marks (mapcar (lambda (s)
                                    (cons (copy-marker (kao-sel-min s) t)
                                          (copy-marker (kao-sel-max s) t)))
                                  sels))
              (dolist (mk marks)
                (let* ((smin (marker-position (car mk)))
                       (smax (marker-position (cdr mk)))
                       ;; No `last'-tracking: `paste_all' passes the selection's
                       ;; own min/max (normal.cc:886-897), faithful here.
                       (pos (kao--paste-goto mode smin smax linewise)))
                  (insert all)
                  (let ((cur pos))
                    (dolist (len lens)
                      (push (kao-sel-make :anchor cur :cursor (+ cur (1- len)))
                            result)
                      (setq cur (+ cur len))))))
              (setq result (nreverse result))
              (kao-set-selections-raw result (1- (length result)))
              ;; Mark our own edit so the post-command `kao--sels-sync' does not
              ;; translate these freshly-installed selections forward through the
              ;; paste a SECOND time (as `kao--multi-edit'/`kao--edit-keeping-sels'
              ;; do — this primitive routes through neither; without the flag the
              ;; sels double-translate off the pasted text at the command boundary).
              (setq kao--sels-edit-pending t)
              (setq ok t))
          (dolist (m marks) (set-marker (car m) nil) (set-marker (cdr m) nil))
          ;; CANCEL on non-local exit (a read-only span mid-loop, C-g) so a
          ;; partial paste-all leaves the buffer untouched.
          (if ok
              (progn (undo-amalgamate-change-group handle)
                     (accept-change-group handle))
            (cancel-change-group handle)))))))

(defun kao-paste-all-after ()
  "Kakoune `<a-p>': paste every source string after each selection, select each."
  (interactive)
  (kao--assert-mode)
  (kao--paste-all 'append))

(defun kao-paste-all-before ()
  "Kakoune `<a-P>': paste every source string before each selection, select each."
  (interactive)
  (kao--assert-mode)
  (kao--paste-all 'insert))

(defun kao-replace-all ()
  "Kakoune `<a-R>': replace each selection with every source string, select each."
  (interactive)
  (kao--assert-mode)
  (kao--paste-all 'replace))

;;;; In-place case transforms (~ ` <a-`>)

(defun kao--map-chars-in-sels (char-fn)
  "Replace every char in [beg,end) of every selection with (CHAR-FN char).
The result is the same length as the original, so the captured selection
positions stay valid (returned unchanged).  Mirrors Kakoune `for_each_codepoint'
\(normal.cc:500-517): the span is inclusive [min, max] (= [beg, end)).  Routes
through `kao--multi-edit' so an interrupted pass (a read-only span mid-loop)
CANCELS the change group and leaves the buffer untouched
atomicity that benefits every edit path.

CHAR-FN is Emacs `upcase'/`downcase' (the buffer's case table), NOT Kakoune's
per-codepoint `towupper'/`towlower' (unicode.hh:131-137).  So `~' maps ß to ẞ
\(Kakoune keeps ß) and leaves dotless ı unchanged (Kakoune maps ı to I) — both
length-preserving, so selections/positions hold; a documented deviation
\, not corruption."
  (kao--multi-edit
   (lambda (am cm _i)
     (let* ((anchor (marker-position am))
            (cursor (marker-position cm))
            (s (kao-sel-make :anchor anchor :cursor cursor))
            (beg (kao-sel-beg s))
            (end (min (kao-sel-end s) (point-max))) ; clamp: empty buffer
            (old (buffer-substring beg end))
            (new (apply #'string (mapcar char-fn (string-to-list old)))))
       (unless (string= old new)
         (delete-region beg end)
         (goto-char beg)
         (insert new))
       (cons anchor cursor)))))

(defun kao--swap-case (c)
  "Swap the case of char C (Kakoune `swap_case', normal.cc:494-498)."
  (let ((d (downcase c)))
    (if (= d c) (upcase c) d)))

(defun kao-upcase ()
  "Kakoune `~': upper-case every char in each selection (span preserved)."
  (interactive)
  (kao--assert-mode)
  (kao--map-chars-in-sels #'upcase))

(defun kao-downcase ()
  "Kakoune \\=`: lower-case every char in each selection (span preserved)."
  (interactive)
  (kao--assert-mode)
  (kao--map-chars-in-sels #'downcase))

(defun kao-swapcase ()
  "Kakoune `<a-`>': swap the case of every char in each selection."
  (interactive)
  (kao--assert-mode)
  (kao--map-chars-in-sels #'kao--swap-case))

;;;; Replace char (r)

(defun kao--replace-char-with (ch)
  "Replace every char in [beg,end) of each selection with CH (span preserved).
Mirrors Kakoune `replace_with_char' (normal.cc:475-491): overlapping
selections are merged first (`sels.merge_overlapping()', normal.cc:486),
then the whole selection becomes CH repeated, so its length — and thus the
positions — are unchanged.  Routes through `kao--multi-edit' (after the merge,
which it honors — the primitive reads `kao--sels' at entry) so an interrupted
pass CANCELS the change group, leaving the buffer untouched."
  ;; direct install (no-refresh): sort+merge before kao--multi-edit — the
  ;; primitive reads kao--sels at entry, no seam clamp/refresh wanted here.
  (setq kao--sels (kao-sels-sort-and-merge-overlapping kao--sels))
  (kao--multi-edit
   (lambda (am cm _i)
     (let* ((anchor (marker-position am))
            (cursor (marker-position cm))
            (s (kao-sel-make :anchor anchor :cursor cursor))
            (beg (kao-sel-beg s))
            (end (min (kao-sel-end s) (point-max)))) ; clamp: empty buffer
       (delete-region beg end)
       (goto-char beg)
       (insert (make-string (- end beg) ch))
       (cons anchor cursor)))))

(defun kao-replace-char ()
  "Kakoune `r': read a char and replace every char in each selection with it.
Escape aborts with no edit, as in Kakoune.  Reads via `kao--read-key' so the
char is captured on the `Q' macro recorder (a bare `read-char' loses it and
replay desyncs) and normalizes Return to newline via
`kao--key-codepoint' so `r RET' inserts a real newline, not ^M.
The `characterp'/Escape guard is unchanged: GUI Escape arrives as
the symbol `escape' (fails `characterp'), tty/batch as ?\\e."
  (interactive)
  (kao--assert-mode)
  (let ((ch (kao--key-codepoint (kao--read-key "replace with character: "))))
    (when (and (characterp ch) (/= ch ?\e))
      (kao--replace-char-with ch))))

;;;; Indent / deindent (> <)

(defcustom kao-indent-width 4
  "Spaces per indent level for `>' / `<' (Kakoune `indentwidth', default 4).
When 0, `>' inserts a literal tab per count and `<' falls back to `tab-width'."
  :type 'integer
  :group 'kao)

(defun kao--indent-bols ()
  "Ascending, deduped bol markers for every line spanned by any selection.
A line covered by more than one selection is returned once, reproducing
Kakoune's `last_line' dedup (normal.cc:1366/1411).  Markers (not raw positions)
so they track edits made to earlier lines as later ones are processed."
  (let ((positions '()))
    (save-excursion
      (dolist (sel (kao-sels-list kao--sels))
        (goto-char (kao-sel-min sel))
        (beginning-of-line)
        (let ((maxp (kao-sel-max sel)))
          (while (<= (point) maxp)
            (push (point) positions)
            ;; Stop when point cannot advance a full line (last line / empty
            ;; buffer / phantom trailing line): `forward-line' returns nonzero
            ;; and leaves point at `point-max', which would loop forever.
            (unless (zerop (forward-line 1)) (setq maxp -1))))))
    (mapcar #'copy-marker (sort (delete-dups positions) #'<))))

(defun kao--indent-apply (indent-empty)
  "Indent every line any selection spans by `kao--repeat-count' levels.
Inserts count*`kao-indent-width' spaces (count tabs when the width is 0) at the
start of each spanned line.  When INDENT-EMPTY is nil (`>') empty lines are
skipped; when non-nil (`<a->>') they are indented too (`indent<false/true>',
normal.cc:1348-1369).  Lines shared by several selections are indented once
\(`last_line' dedup); the selections shift with the edit, one undo unit."
  (barf-if-buffer-read-only)
  (let* ((n (kao--repeat-count))
         (indent (if (> kao-indent-width 0)
                     (make-string (* n kao-indent-width) ?\s)
                   (make-string n ?\t)))
         (bols (kao--indent-bols)))
    (kao--edit-keeping-sels
     (lambda (_marks)
       (save-excursion
         (dolist (m bols)
           (let ((bol (marker-position m)))
             ;; Emacs has no forced trailing newline (family): a line is
             ;; "empty" when bol sits on its newline or at point-max.
             (when (or indent-empty
                       (and (< bol (point-max)) (not (eq (char-after bol) ?\n))))
               (goto-char bol)
               (insert indent)))
           (set-marker m nil)))))))

(defun kao-indent ()
  "Kakoune `>': indent every non-empty line the selections span."
  (interactive)
  (kao--assert-mode)
  (kao--indent-apply nil))

(defun kao-indent-empty ()
  "Kakoune `<a->>': indent every line the selections span, including empty ones."
  (interactive)
  (kao--assert-mode)
  (kao--indent-apply t))

(defun kao--deindent-line (bol target tabstop incomplete)
  "Remove leading whitespace at BOL, up to TARGET visual columns.
TABSTOP advances width to the next tab stop on a tab.  When INCOMPLETE is
non-nil (`<') a leading run shorter than TARGET is removed once a non-blank or
the line end follows it; when nil (`<a-<>') such a partial run is left intact —
only a run that reaches TARGET is removed (`deindent<true/false>',
normal.cc:1371-1414)."
  (let ((width 0)
        (pos bol)
        (eol (save-excursion (goto-char bol) (line-end-position)))
        (done nil))
    (while (and (not done) (<= pos eol))
      (if (>= pos eol)                          ; newline / eob = the terminator
          (progn (when (and incomplete (> width 0)) (delete-region bol pos))
                 (setq done t))
        (let ((ch (char-after pos)))
          (cond
           ((eq ch ?\t) (setq width (* (1+ (/ width tabstop)) tabstop)))
           ((eq ch ?\s) (setq width (1+ width)))
           (t (when (and incomplete (> width 0)) (delete-region bol pos))
              (setq done t)))
          (unless done
            (if (>= width target)
                (progn (delete-region bol (1+ pos)) (setq done t))
              (setq pos (1+ pos)))))))))

(defun kao--deindent-apply (incomplete)
  "Deindent every line any selection spans by `kao--repeat-count' levels.
Removes up to count*width columns per line (width = `kao-indent-width', or
`tab-width' when it is 0).  When INCOMPLETE is nil (`<a-<>') a partial leading
indent (fewer than the target columns) is left intact.  Lines shared by several
selections are deindented once; the selections shift with the edit, one undo
unit."
  (barf-if-buffer-read-only)
  (let* ((n (kao--repeat-count))
         (tabstop tab-width)
         (target (* n (if (> kao-indent-width 0) kao-indent-width tabstop)))
         (bols (kao--indent-bols)))
    (kao--edit-keeping-sels
     (lambda (_marks)
       (save-excursion
         (dolist (m bols)
           (kao--deindent-line (marker-position m) target tabstop incomplete)
           (set-marker m nil)))))))

(defun kao-deindent ()
  "Kakoune `<': remove leading indentation from each line the selections span."
  (interactive)
  (kao--assert-mode)
  (kao--deindent-apply t))

(defun kao-deindent-keep-incomplete ()
  "Kakoune `<a-<>': deindent, but leave an incomplete leading indent intact."
  (interactive)
  (kao--assert-mode)
  (kao--deindent-apply nil))

;;;; Comment toggling (SPC c — no Kakoune counterpart)

(defun kao-comment-lines ()
  "Toggle comments on every line any selection spans (`SPC c', -2).
Each selection's spanned line range goes through the native
`comment-or-uncomment-region' (newcomment decides comment-vs-uncomment per
range from the major mode's syntax); overlapping ranges are merged so a
line is toggled once.  One undo unit; the selections shift with the text
\(`kao--edit-keeping-sels').  A kao extension — Kakoune has no comment
command (its idiom is a user mapping); the user map is its kao home too."
  (interactive)
  (kao--assert-mode)
  (barf-if-buffer-read-only)
  (kao--edit-keeping-sels
   (lambda (marks)
     (let (ranges merged)
       ;; (FIRST-LINE . LAST-LINE) per selection, from the live markers.
       (dolist (m marks)
         (let ((a (marker-position (car m)))
               (c (marker-position (cdr m))))
           (push (cons (line-number-at-pos (min a c))
                       (line-number-at-pos (max a c)))
                 ranges)))
       (setq ranges (sort ranges (lambda (x y) (< (car x) (car y)))))
       (dolist (r ranges)
         (if (and merged (<= (car r) (cdr (car merged))))
             (setcdr (car merged) (max (cdr r) (cdr (car merged))))
           (push (cons (car r) (cdr r)) merged)))
       ;; MERGED is descending by start line: ranges lower in the buffer are
       ;; toggled first, so the line numbers of the ones above stay valid
       ;; even if a comment style ever adds lines.
       (save-excursion
         (dolist (r merged)
           (goto-char (point-min))
           (forward-line (1- (car r)))
           (let ((beg (point)))
             (forward-line (- (cdr r) (car r)))
             (comment-or-uncomment-region beg (line-end-position)))))))))

;;;; Convert tabs <-> spaces (@ / <a-@>)
;; Kakoune `tabs_to_spaces'/`spaces_to_tabs' (normal.cc:1890/1917): rewrite the
;; whitespace inside each selection.  Columns are measured with the buffer's
;; `tab-width' (`opt_tabstop'); the rounding target is the count, when given,
;; else `tab-width'.  Edits are collected from the ORIGINAL buffer (all columns
;; measured before any change), then applied as one batch — matching Kakoune's
;; `SelectionList::replace'.  The selections shift with the edits.

(defun kao--visual-column (pos)
  "Display column of POS on its line, tabs advancing to the next `tab-width' stop.
Each non-tab char counts its `char-width' (Emacs's `wcwidth'), so a wide/CJK
char is two columns (Kakoune `get_column', unicode.hh:100-107)."
  (save-excursion
    (goto-char pos)
    (let ((stop (max 1 tab-width))
          (col 0))
      (goto-char (line-beginning-position))
      (while (< (point) pos)
        (if (eq (char-after) ?\t)
            (setq col (* (1+ (/ col stop)) stop))
          (setq col (+ col (char-width (char-after)))))
        (forward-char 1))
      col)))

(defun kao--column-to-pos (bol lineend target)
  "Inverse of `kao--visual-column': position for visual column TARGET on a line.
Walks from BOL (not past LINEEND, the position after the line's newline or
point-max), counting tabs to the next `tab-width' stop and every other char its
`char-width' (display width), stopping before a char or tab that would overshoot
TARGET (Kakoune `get_byte_to_column', break-before rule).  Returns LINEEND when
TARGET lies past the whole line — the caller treats that as \"the column does not
exist on this line\"."
  (save-excursion
    (goto-char bol)
    (let ((stop (max 1 tab-width))
          (col 0))
      (catch 'done
        (while (and (< (point) lineend) (> target col))
          (if (eq (char-after) ?\t)
              (let ((next (* (1+ (/ col stop)) stop)))
                (when (> next target) (throw 'done nil)) ; target inside the tab
                (setq col next))
            (let ((w (char-width (char-after))))
              (when (> (+ col w) target) (throw 'done nil)) ; target inside the char
              (setq col (+ col w))))
          (forward-char 1)))
      (point))))

(defun kao--apply-edits-keeping-sels (edits)
  "Apply EDITS, each (BEG END . STRING) replacing buffer [BEG, END) with STRING.
Applied in descending position order so earlier (lower) edits stay valid, with
the selections carried across the change (`kao--edit-keeping-sels').  Edits with
a duplicate BEG (overlapping selections) are applied once.  No-op when nil."
  (when edits
    (let ((seen (make-hash-table))
          (uniq '()))
      (dolist (e edits)
        (unless (gethash (car e) seen)
          (puthash (car e) t seen)
          (push e uniq)))
      (setq uniq (sort uniq (lambda (a b) (> (car a) (car b)))))
      (kao--edit-keeping-sels
       (lambda (_marks)
         (dolist (e uniq)
           (delete-region (car e) (cadr e))
           (goto-char (car e))
           (insert (cddr e))))))))

(defun kao-tabs-to-spaces ()
  "Kakoune `@': replace every tab inside the selections with spaces.
Each tab becomes the number of spaces needed to reach the next tabstop.  The
tabstop is the count when given, else `tab-width' (`tabs_to_spaces',
normal.cc:1890)."
  (interactive)
  (kao--assert-mode)
  (barf-if-buffer-read-only)
  (let ((tabstop (kao--count-or (max 1 tab-width)))
        (edits '()))
    (dolist (sel (kao-sels-list kao--sels))
      (let ((pos (kao-sel-min sel)) (max (kao-sel-max sel)))
        (while (<= pos max)
          (when (eq (char-after pos) ?\t)
            (let* ((col (kao--visual-column pos))
                   (n (- (* (1+ (/ col tabstop)) tabstop) col)))
              (push (cons pos (cons (1+ pos) (make-string n ?\s))) edits)))
          (setq pos (1+ pos)))))
    (kao--apply-edits-keeping-sels edits)))

(defun kao-spaces-to-tabs ()
  "Kakoune `<a-@>': replace tabstop-aligned space runs inside selections with tabs.
A run of spaces that reaches a tabstop boundary becomes one tab; a run that ends
on a literal tab is absorbed into it.  The tabstop is the count when given, else
`tab-width' (`spaces_to_tabs', normal.cc:1917)."
  (interactive)
  (kao--assert-mode)
  (barf-if-buffer-read-only)
  (let ((tabstop (kao--count-or (max 1 tab-width)))
        (edits '()))
    (dolist (sel (kao-sels-list kao--sels))
      (let ((pos (kao-sel-min sel)) (end (1+ (kao-sel-max sel))))
        (while (< pos end)
          (if (eq (char-after pos) ?\s)
              (let ((beg pos)
                    (run-end (1+ pos))
                    (col (kao--visual-column (1+ pos))))
                (while (and (< run-end end)
                            (eq (char-after run-end) ?\s)
                            (/= (% col tabstop) 0))
                  (setq run-end (1+ run-end))
                  (setq col (1+ col)))
                (cond
                 ((= (% col tabstop) 0)
                  (push (cons beg (cons run-end "\t")) edits))
                 ((and (< run-end end) (eq (char-after run-end) ?\t))
                  (push (cons beg (cons (1+ run-end) "\t")) edits)))
                (setq pos run-end))
            (setq pos (1+ pos))))))
    (kao--apply-edits-keeping-sels edits)))

;;;; Align (&) / copy indent (<a-&>)
;; Kakoune `align'/`copy_indent' (normal.cc:1800/1852): both edit the buffer
;; without reselecting, so the selections shift with the text
;; (`kao--edit-keeping-sels').

(defcustom kao-align-tab nil
  "When non-nil, `&' pads with tabs (Kakoune `aligntab' option); else spaces."
  :type 'boolean
  :group 'kao)

(defun kao--align-pad (minp n)
  "Padding string inserted at MINP to advance the cursor by N visual columns.
Spaces, unless `kao-align-tab' is set — then a tab/space mix landing on the same
column (Kakoune `align' use_tabs path, normal.cc:1838-1844)."
  (if (not kao-align-tab)
      (make-string n ?\s)
    (let* ((stop (max 1 tab-width))
           (inscol (kao--visual-column minp))
           (targetcol (+ inscol n))
           (tabcol (- inscol (% inscol stop)))
           (tabs (/ (- targetcol tabcol) stop))
           (spaces (- targetcol (if (> tabs 0) (+ tabcol (* tabs stop)) inscol))))
      (concat (make-string tabs ?\t) (make-string spaces ?\s)))))

(defun kao--align-edit (marks)
  "Pad selections so each column's cursors align (Kakoune `align' body).
MARKS is the live marker list from `kao--edit-keeping-sels'.  Selections sharing
a line form successive columns; column K is the K-th selection on each line.
Each column is padded so every cursor reaches the column's widest cursor, with
columns processed in order so later ones see the earlier inserts
\(`selections.update')."
  (let ((last-line nil) (col -1) (maxcol -1) (assigned '()))
    (dolist (m marks)
      (let ((cline (line-number-at-pos (marker-position (cdr m)))))
        (setq col (if (eql cline last-line) (1+ col) 0)
              last-line cline
              maxcol (max maxcol col))
        (push (cons col m) assigned)))
    (let ((cols (make-vector (1+ maxcol) nil)))
      (dolist (a (nreverse assigned))
        (push (cdr a) (aref cols (car a))))
      (save-excursion
        (dotimes (c (1+ maxcol))
          (let ((group (nreverse (aref cols c)))
                (target 0))
            (dolist (m group)
              (setq target (max target
                                (kao--visual-column (marker-position (cdr m))))))
            (dolist (m group)
              (let* ((minp (min (marker-position (car m)) (marker-position (cdr m))))
                     (n (- target (kao--visual-column (marker-position (cdr m))))))
                (when (> n 0)
                  (goto-char minp)
                  (insert (kao--align-pad minp n)))))))))))

(defun kao-align ()
  "Kakoune `&': align the cursor of each selection by padding before its min.
Selections sharing a line form successive columns; within each column every
cursor is padded out to the column's widest cursor (with spaces, or tabs when
`kao-align-tab' is set).  Aborts with a message if any selection spans more than
one line (Kakoune throws), one undo unit (`align', normal.cc:1800)."
  (interactive)
  (kao--assert-mode)
  (barf-if-buffer-read-only)
  (let ((multiline nil))
    (dolist (sel (kao-sels-list kao--sels))
      (when (/= (line-number-at-pos (kao-sel-anchor sel))
                (line-number-at-pos (kao-sel-cursor sel)))
        (setq multiline t)))
    (if multiline
        (message "align cannot work with multi line selections")
      (kao--edit-keeping-sels #'kao--align-edit))))

(defun kao--leading-blank-end (bol)
  "Position after the leading run of spaces/tabs starting at BOL.
\(Kakoune `is_horizontal_blank' = space or tab, not newline.)"
  (let ((p bol))
    (while (and (< p (point-max)) (memq (char-after p) '(?\s ?\t)))
      (setq p (1+ p)))
    p))

(defun kao-copy-indent ()
  "Kakoune `<a-&>': copy the reference selection's indent to all spanned lines.
The reference is the count-th selection (1-based) or, with no count, the main
selection; its leading whitespace replaces the leading whitespace of every
other line any selection spans.  A count past the selection count aborts with a
message (Kakoune throws), one undo unit (`copy_indent', normal.cc:1852)."
  (interactive)
  (kao--assert-mode)
  (barf-if-buffer-read-only)
  (let* ((sels (kao-sels-list kao--sels))
         (nsel (length sels))
         (sel-index (kao--count-or (1+ (kao-sels-main kao--sels)))))
    (if (> sel-index nsel)
        (message "invalid selection index")
      (let* ((ref (nth (1- sel-index) sels))
             (ref-bol (save-excursion (goto-char (kao-sel-min ref))
                                      (line-beginning-position)))
             (indent (buffer-substring-no-properties
                      ref-bol (kao--leading-blank-end ref-bol)))
             (bols (kao--indent-bols))
             (edits '()))
        (dolist (m bols)
          (let ((bol (marker-position m)))
            (unless (= bol ref-bol)
              (push (cons bol (cons (kao--leading-blank-end bol) indent)) edits)))
          (set-marker m nil))
        (kao--apply-edits-keeping-sels edits)))))

;;;; Rotate selections content (<a-)> / <a-(>)
;; Kakoune `rotate_selections_content' (normal.cc:1670): rotate the TEXT of the
;; selections among themselves, in groups of `count' (0 / >size => one group of
;; all).  Forward (`<a-)>') moves each group's last text to its front; backward
;; (`<a-(>') the reverse.  Each selection then covers its new text (marker army,
;; direction preserved); the main follows its content.

(defun kao--rotate-content (forward)
  "Rotate the selections' text by one, in groups of `kao--count'.
FORWARD non-nil is `<a-)>', nil is `<a-(>'.  A count of 0 or one greater than
the selection count rotates a single group of all selections; otherwise the
selections are split into consecutive groups of that size and each is rotated
independently.  Each selection's text is replaced with its rotated string via
the marker army (one undo unit, direction preserved); main follows its content."
  (barf-if-buffer-read-only)
  (let* ((sels (kao-sels-list kao--sels))
         (n (length sels)))
    (when (> n 1)
      (let* ((strings (vconcat (mapcar (lambda (s)
                                         (buffer-substring-no-properties
                                          (kao-sel-beg s) (kao-sel-end s)))
                                       sels)))
             (group (if (or (= kao--count 0) (> kao--count n)) n kao--count))
             (amount (% 1 group))           ; 1 when GROUP>1, else 0 (no-op)
             (main (kao-sels-main kao--sels)))
        (when (> amount 0)
          (let ((i 0))
            (while (< i n)
              (let* ((end (min n (+ i group)))
                     (chunk (append (cl-subseq strings i end) nil)) ; vector -> list
                     (rotated (if forward
                                  (append (last chunk amount) (butlast chunk amount))
                                (append (nthcdr amount chunk)
                                        (cl-subseq chunk 0 amount)))))
                (cl-loop for k from i below end for v in rotated do
                         (aset strings k v))
                (when (and (<= i main) (< main end))
                  (let ((new-beg (if forward (- end amount) (+ i amount))))
                    (setq main (if (< main new-beg)
                                   (- end (- new-beg main))
                                 (+ i (- main new-beg))))))
                (setq i end))))
          (kao--multi-edit
           (lambda (am cm idx)
             (let* ((a (marker-position am)) (c (marker-position cm))
                    (forward-sel (>= c a))
                    (beg (min a c)) (end (1+ (max a c)))
                    (str (aref strings idx))
                    (len (length str)))
               (delete-region beg end)
               (goto-char beg)
               (insert str)
               (cond
                ((zerop len) (let ((p (if (> beg (point-min)) (1- beg) beg)))
                               (cons p p)))
                (forward-sel (cons beg (+ beg len -1)))
                (t           (cons (+ beg len -1) beg))))))
          (setf (kao-sels-main kao--sels) main)
          (kao--refresh))))))

(defun kao-rotate-content-forward ()
  "Kakoune `<a-)>': rotate the selections' text forward (in groups of `count')."
  (interactive)
  (kao--assert-mode)
  (kao--rotate-content t))

(defun kao-rotate-content-backward ()
  "Kakoune `<a-(>': rotate the selections' text backward."
  (interactive)
  (kao--assert-mode)
  (kao--rotate-content nil))

;;;; Join lines (<a-j> / <a-J>)
;; Kakoune `join_lines'/`join_lines_select_spaces' (normal.cc:1260/1234):
;; replace each line's terminating newline, plus the next line's leading
;; horizontal blanks, with one space.  `<a-J>' selects the inserted spaces;
;; `<a-j>' keeps the original selections (mapped through the edit).  The join
;; regions are built from the selections, `merge_consecutive'd (blank lines
;; collapse to one space), then each replaced with " " via the marker army.

(defun kao-join--regions ()
  "Sorted forward list of join-regions for all selections, or nil if none.
Each region spans a line's terminating newline plus the next line's leading
horizontal blanks (space/tab).  A line joins the next only when a real next line
exists; nothing is produced for a single-line selection on the last line, a
buffer-terminal newline (Emacs phantom trailing line, family), or an
unterminated last line, reproducing Kakoune `end_line' clamp (normal.cc:1234)."
  (let ((regions '()))
    (dolist (sel (kao-sels-list kao--sels))
      (save-excursion
        (goto-char (kao-sel-min sel))
        (let* ((min-bol (line-beginning-position))
               (max-bol (progn (goto-char (kao-sel-max sel))
                               (line-beginning-position)))
               (last-bol (if (= min-bol max-bol)
                             min-bol
                           (save-excursion (goto-char max-bol)
                                           (forward-line -1) (point)))))
          (goto-char min-bol)
          (catch 'done
            (while (<= (point) last-bol)
              (let ((eol (line-end-position)))
                (when (and (eq (char-after eol) ?\n) (< (1+ eol) (point-max)))
                  (let ((skip (1+ eol)))
                    (while (and (< skip (point-max))
                                (memq (char-after skip) '(?\s ?\t)))
                      (setq skip (1+ skip)))
                    (push (kao-sel-make :anchor eol :cursor (1- skip)) regions))))
              ;; Stop when point cannot advance a full line (last line / empty
              ;; buffer / phantom trailing line): `forward-line' returns nonzero
              ;; and leaves point at `point-max', which would loop forever.
              (unless (zerop (forward-line 1))
                (throw 'done nil)))))))
    (nreverse regions)))

(defun kao-join--replace-region (am cm _i)
  "Marker-army callback: replace AM..CM's region [min,max+1) with one space.
Returns the inserted space as a one-char forward selection (cons of positions)."
  (let* ((a (marker-position am)) (c (marker-position cm))
         (beg (min a c)) (end (1+ (max a c))))
    (delete-region beg end)
    (goto-char beg)
    (insert " ")
    (cons beg beg)))

(defun kao-join--apply (select-spaces)
  "Join the lines the selections span (Kakoune `join_lines[_select_spaces]').
Each newline plus the next line's leading horizontal blanks becomes one space,
via the marker army as one undo unit; blank lines collapse to one space
\(`merge_consecutive').  With SELECT-SPACES non-nil (`<a-J>') the resulting
selections are the spaces; otherwise (`<a-j>') the original selections are kept,
mapped through the edit (`join_lines's `sels.update', normal.cc:1264).  No-op
when nothing can be joined."
  (barf-if-buffer-read-only)
  (let ((regions (kao-join--regions)))
    (when regions
      (let* ((n (length regions))
             (merged (kao-sels-merge-consecutive
                      (kao-sels-make :list regions :main (1- n)))))
        ;; direct install (no-refresh): MERGED is a built kao-sels handed to
        ;; kao--multi-edit WITHOUT a refresh — both branches below stay direct.
        (if select-spaces
            (progn
              (setq kao--sels merged)
              (kao--multi-edit #'kao-join--replace-region))
          ;; `<a-j>': keep the original selections, mapped through the edit.
          (kao--edit-keeping-sels
           (lambda (_marks)
             (setq kao--sels merged)
             (kao--multi-edit #'kao-join--replace-region))))))))

(defun kao-join-select-spaces ()
  "Kakoune `<a-J>': join lines and select the spaces inserted at line breaks.
Faithful to `join_lines_select_spaces' (normal.cc:1234)."
  (interactive)
  (kao--assert-mode)
  (kao-join--apply t))

(defun kao-join-lines ()
  "Kakoune `<a-j>': join the lines each selection touches, keeping selections.
Each newline plus the next line's leading horizontal blanks becomes one space;
the original selections are restored, mapped through the edit.  Faithful to
`join_lines' (normal.cc:1260)."
  (interactive)
  (kao--assert-mode)
  (kao-join--apply nil))

;;;; Undo / redo (u / U) + history-tree navigation (<c-j> / <c-k>)
;; Kakoune `u'/`U' (normal.cc:2172-2198) undo/redo `count' buffer changes and
;; `<c-j>'/`<c-k>' (`move_in_history', :2201) jump by absolute change-id; all
;; four navigate the ONE buffer history TREE (`Buffer::move_to', buffer.cc:345).
;; kao owns that tree in kao-history.el — `u'/`U' here walk parent /
;; redo-child, the change-id keys (Task 5) walk `current ± count'.  This
;; SUPERSEDES the older `u'/`U'-on-native-`undo' mechanism (its behaviour is
;; preserved): native undo cannot express the cross-branch reach the change-id
;; keys require, and the two interfaces must share one tree.
;;
;; selection reset (family): a navigation resets `kao--sels' to
;; one selection PER MODIFIED RANGE — Kakoune's `compute_modified_ranges'
;; (selection.cc:132) installed via `selections_write_only'
;; (normal.cc:2176-2181), so a cursor lands back on every edited site.  Each
;; restored insertion (END > BEG) becomes [BEG, END) cursor-on-last-char; a
;; removal (END = BEG) collapses to BEG.  Every mod's span is folded to the
;; final buffer frame through the shipped flat ForwardChangesTracker
;; (`kao-history--translate-1' — the "coord-translation machinery" the
;; deferral once waited on) and the ranges install via
;; `kao-sels-sort-and-merge-overlapping', main on the last.  Adjacent
;; ranges coalesce exactly as Kakoune's post-pass `touches' merge does, because
;; `translate-1' grows a span's exclusive end onto an insert that abuts it and
;; the overlap merge then unions the pair.

(defun kao--hist-select-ranges (ranges)
  "Reset `kao--sels' to one selection per modified range in RANGES.
RANGES is a list of spans (BEG . END) or nil: END > BEG selects [BEG, END) with
the cursor on the last char; END = BEG collapses at BEG (Kakoune
`compute_modified_ranges', one Selection per modified range,).  Sorts
and merges overlapping ranges (`kao-sels-sort-and-merge-overlapping') and puts
the main on the last, highest selection (`SelectionList''s `m_main = size()-1',
selection.cc:27).  A nil RANGES leaves the selection untouched.  Refresh, then
flash the main landing (polish)."
  (when ranges
    (let* ((sels (mapcar
                  (lambda (span)
                    (let* ((beg (car span)) (end (cdr span))
                           (cursor (if (> end beg) (1- end) beg))
                           (anchor (if (> end beg) beg cursor)))
                      (kao--clamp-sel
                       (kao-sel-make :anchor anchor :cursor cursor))))
                  ranges))
           (merged (kao-sels-list
                    (kao-sels-sort-and-merge-overlapping
                     (kao-sels-make :list sels :main 0)))))
      ;; direct install (retag-order): kao--sels-id is re-pinned BEFORE the
      ;; refresh below, but kao-set-selections-raw refreshes internally.
      (setq kao--sels (kao-sels-make :list merged
                                     :main (1- (length merged))))))
  ;; The navigation already moved the tree (and committed any pending mods up
  ;; front); the restored ranges are in that new frame, so re-pin the live-list
  ;; tag here — `kao--refresh' must not fold them through the walk it just did.
  (kao--sels-retag)
  (kao--refresh)
  (when ranges
    (let ((m (kao--main-sel)))
      (kao--pulse-span (kao-sel-beg m) (kao-sel-end m)))
    ;; The recorder skips `kao-undo'/`kao-redo'/`kao-history-goto' wholesale, so
    ;; the value-change hook must fire here — the single seam where u/U and
    ;; <c-j>/<c-k> install the restored `kao--sels'.  Same
    ;; guarded idiom as `kao-sel-undo'; only on a real install (non-nil RANGES),
    ;; never on the no-op branch.
    (when kao-selection-change-hook
      (run-hooks 'kao-selection-change-hook))))

(defun kao--hist-navigate (target)
  "Move the history tree to node TARGET, applying every edge's modifications.
Return the LIST of modified ranges (each a cons (BEG . END) in the final buffer
frame) — one per replayed modification, Kakoune's `compute_modified_ranges'
\(selection.cc:132) over the whole navigation — or nil when nothing changed.
TARGET must be a valid id (callers validate / map nil to the empty-history
error).  Syncs the buffer-modified flag to the new position
\(`kao-history-sync-modified') so an undo/redo back onto the saved node clears
it — native undo's auto-unmodify cannot fire here (replays with
`buffer-undo-list' bound off).

Refuses UP FRONT, before any buffer edit, when the walk would touch a position
outside the accessible region (`kao-history-move-in-region-p'): replaced
native undo, whose `primitive-undo' rejects such a walk cleanly under
narrowing, and kao keeps that parity so a half-applied node can never desync
`kao--hist-id' from the buffer content.

Also refuses UP FRONT when the shared text was edited behind the tree's back
\(`buffer-chars-modified-tick' has advanced past `kao--hist-tick') — an indirect
buffer edited through its base/counterpart, `buffer-swap-text', or an
`inhibit-modification-hooks' writer.  Kakoune has no indirect buffers, so a node
can never be replayed against text the tree did not see; kao's honest analogue
is a loud refusal rather than a replay at now-wrong positions
\."
  (unless (kao-history-move-in-region-p target (point-min) (point-max))
    ;; Native `primitive-undo''s exact message (capitalized Emacs-parity
    ;; wording, not a Kakoune message) — see UX notes.
    (user-error "Changes to be undone are outside visible portion of buffer"))
  (when (and kao--hist-tick (/= (buffer-chars-modified-tick) kao--hist-tick))
    (user-error "buffer changed outside kao's history (indirect buffer / foreign edit) — history navigation refused"))
  (let ((ranges nil))
    (kao-history-move-to
     target
     (lambda (dir id)
       ;; Thread the accumulator across edges so ranges from earlier edges are
       ;; folded forward through the later edges' mods (one final frame).
       (setq ranges
             (kao--hist-apply-group (kao-hist-node-group (kao--hist-node id))
                                    dir ranges))))
    (kao-history-sync-modified)
    ranges))

(defun kao--hist-go (target what)
  "Navigate to history node TARGET and select each modified range.
A nil TARGET (exhausted history) signals Kakoune's `nothing left to WHAT'."
  (unless target
    (user-error "nothing left to %s" what))
  (kao--hist-select-ranges (kao--hist-navigate target)))

(defun kao-undo ()
  "Kakoune `u': undo COUNT changes via the history tree.
Select each region the change touched.  Signals
`buffer-read-only' up front (`Buffer::undo' calls `throw_if_read_only',
buffer.cc:309), then commits any pending modifications as a node
\(`commit_undo_group()', buffer.cc:311) — reachable mid-insert via the
one-shot normal (`<a-;> u'), where it faithfully splits the session."
  (interactive)
  (kao--assert-mode)
  (barf-if-buffer-read-only)
  (kao-history-commit-pending)
  (kao--hist-go (kao-history-undo-target (kao--repeat-count)) "undo"))

(defun kao-redo ()
  "Kakoune `U': redo COUNT changes via the history tree.
Select each region the change touched.  Signals
`buffer-read-only' up front (`Buffer::redo', buffer.cc:329).  Unlike undo
\(buffer.cc:311) and `move_to' (:352), redo never commits the pending
undo group: it REFUSES while uncommitted modifications exist
\(`not m_current_undo_group.empty()' -> return false, buffer.cc:331-333 ->
\"nothing left to redo\") — so `<a-;> U' mid-insert errors and leaves the
session's single undo group intact."
  (interactive)
  (kao--assert-mode)
  (barf-if-buffer-read-only)
  (when kao--hist-pending
    (user-error "nothing left to redo"))
  (kao--hist-go (kao-history-redo-target (kao--repeat-count)) "redo"))

(defun kao-history-goto (id)
  "Navigate the buffer history to absolute node ID, like `<c-j>'/`<c-k>'.
Validate that ID is an existing in-range HistoryId — a gc-dropped id is in
range but absent and is reported the same — then signal `buffer-read-only'
AFTER that check (faithful to `Buffer::move_to', buffer.cc:345-350), commit
any pending modifications (`commit_undo_group()', :352), apply the
lowest-common-ancestor walk, and select each modified range
\.  Return ID.  Signal `no such change: #ID (MAX)' when ID is out of range
or was dropped.  The single arbitrary-id entry shared by `<c-j>'/`<c-k>'
\(`kao--move-in-history') and the `kao-vundo' viewer.

Inhibits the selection-history recorder around the navigate: `move_in_history'
assigns via `selections_write_only' (normal.cc:2210) and records no
SelectionHistory node, exactly like u/U — so this jump pushes no node no
matter which `this-command' drives it (the vundo viewer's command, a keybinding,
etc.).  The `memq' skip list covers `<c-j>'/`<c-k>' directly; the flag covers
any other caller."
  (let ((maxid (kao-history-max-id)))
    (unless (and (integerp id) (>= id 0) (<= id maxid) (kao--hist-node id))
      (user-error "no such change: #%s (%d)" id maxid))
    (barf-if-buffer-read-only)
    (kao-history-commit-pending)
    (kao--hist-select-ranges
     (let ((kao--sel-history-inhibit t))
       (kao--hist-navigate id)))
    id))

(defun kao--move-in-history (dir)
  "Jump to the change DIR*COUNT steps away by absolute id.
Kakoune `move_in_history' (normal.cc:2201).  DIR is +1 forward (`<c-j>') /
-1 backward (`<c-k>').  The target id can land on an abandoned SIBLING
branch — the reach `u'/`U' cannot make.  Delegates the validate /
read-only / commit / navigate / select to `kao-history-goto'; out of range or a
gc-dropped target signals `no such change: #N (max)'.  MAX is read BEFORE the
delegation so the report matches the PRE-commit id `move_in_history' computed,
even when a pending in-progress edit commits as a node during the jump."
  (let* ((count (kao--repeat-count))
         (target (+ (kao-history-current-id) (* dir count)))
         (maxid (kao-history-max-id)))
    (kao-history-goto target)
    (message "moved to change #%d (%d)" target maxid)))

(defun kao-history-forward ()
  "Kakoune `<c-j>': move forward in the buffer history by change-id."
  (interactive)
  (kao--assert-mode)
  (kao--move-in-history 1))

(defun kao-history-backward ()
  "Kakoune `<c-k>': move backward in the buffer history by change-id."
  (interactive)
  (kao--assert-mode)
  (kao--move-in-history -1))

(provide 'kao-edit)
;;; kao-edit.el ends here
