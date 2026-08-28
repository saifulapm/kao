;;; kao-menu.el --- Goto / view / user-mode menus for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 2.  The `g' goto and `v' view
;; transient menus are one-shot `read-key' dispatch — the same pattern as the
;; object-pending step (kao-object.el): read ONE key, look it up in a
;; table, dispatch; an unknown key (or escape) cancels with no change.  No new
;; emulation state or keymap rebuild.  The `<space>' user mode (bottom of this
;; file) instead is a real prefix keymap, the Emacs-idiomatic shape for a
;; user-extensible leader (a real sub-keymap).  The `<a-z>'/`<a-Z>'
;; combine menu lives in kao-multi.el (it is selection algebra over a register).
;;
;; goto (`g', normal.cc:228 `goto_commands<Replace>') splits into:
;;   * coord-collapse commands — `g'/`k' buffer top, `j' bottom, `e' end —
;;     faithful to `select_coord<Replace>' (normal.cc:150), which REPLACES the
;;     whole list with one selection (`SelectionList{buffer, coord}'); and the
;;     count form `Ng' -> line N (normal.cc:230).
;;   * per-selection selectors — `l' line end, `h' line begin, `i' first
;;     non-blank — faithful to `select<mode, selector>' (normal.cc:135->:81),
;;     applied through `kao--map-selections' (which sort-and-merges).  The
;;     goto sub-keys ignore the count (they live in the `count == 0' branch), so
;;     these are NOT wrapped with `kao--repeat'.
;;   * window-relative commands — `t'/`b'/`c' window top/bottom/center — depend on
;;     a live window; they are guarded (`kao-menu--displayed-p') and no-op when
;;     the buffer is not on display (batch).
;;
;; The selectors mirror references/kakoune/src/selectors.cc:
;;   `l' -> `select_to_line_end<true>'    (l.177)
;;   `h' -> `select_to_line_begin<true>'  (l.191)
;;   `i' -> `select_to_first_non_blank'   (l.201)
;;
;; family (no forced trailing `\n'): Kakoune's `back_coord' is the buffer's
;; last char; kao's "buffer end" is `(1- (point-max))' (the last on-char position,
;; per `kao--clamp-sel'), reached by clamping `point-max'.
;;
;; view (`v', normal.cc:396 `view_commands<false>') moves the viewport without
;; moving the selection.  But Emacs keeps `point' (the main-cursor mirror)
;; visible and kao re-asserts `point = main' after every command (`kao--refresh'),
;; so a viewport cannot detach from the cursor the way Kakoune's
;; `ensure_cursor_visible = false' allows.  Hence a deliberate, documented
;; deviation:
;;   * reposition-around-cursor (`v'/`c' center, `t' top, `b' bottom, `m'
;;     center-horiz, `<' left, `>' right) keeps the cursor fixed and is faithful;
;;     these survive the post-command mirror.
;;   * detached-scroll (`j'/`k' vertical, `h'/`l' horizontal, count `max(1,n)')
;;     uses Emacs's native scroll, which carries the cursor to the window edge;
;;     kao then syncs the main selection to the new point so the mirror will not
;;     snap the view back — but only when the scroll actually carried point; a
;;     scroll that moves nothing leaves every selection verbatim (
;;     PreserveSelections).  Divergence: Kakoune leaves the cursor behind, Emacs
;;     carries it to the edge.
;; All view commands are guarded (`kao-menu--displayed-p') and no-op in batch.
;;
;; goto extend (`G', `goto_commands<Extend>') shares `kao--goto-specs' with `g':
;; coord/window targets use `select_coord<Extend>' (each cursor -> target, anchors
;; kept) and selector targets use `select<Extend>' (`kao--map-selections-extend').
;; Locked view (`V', `view_commands<true>') re-runs the `v' dispatch in a loop;
;; an unmapped key messages "key not mapped" and stays locked, so only Escape
;; (or a non-character event) exits (normal.cc:406-407).
;;
;; Remaining deviations (documented): `gf' uses `ffap' (`find-file-at-point'),
;; which reads the path from the text around point rather than Kakoune's
;; selection-content + `path'-option resolution (normal.cc:293); and the
;; `v'/`V' detached-scroll keys (`j'/`k'/`h'/`l') approximate the viewport
;; (Emacs keeps `point' visible, so the cursor is carried to the
;; window edge and the main reconciled unless nothing moved).  Everything the
;; old note called deferred now ships: `ga'/`gf'/`g.'/`gd'/`gu' (specs below),
;; the jumplist `push_jump', and the autoinfo box (`kao-info--with-box').

;;; Code:

(require 'kao-selection)
(require 'kao-state)
(require 'kao-history)                  ; `g .' last-change accessor
(require 'kao-register)                 ; `kao-register-get' for `<c-r>'
(require 'kao-motion)                   ; `kao-motion--vertical' fallback (acyclic:
                                        ; kao-motion needs only selection+state)
(require 'kao-info)

;;;; goto — per-selection selectors (selectors.cc, pure (SEL) -> SEL)

(defun kao-menu--line-end-target (cur)
  "Return the `select_to_line_end' target for a cursor at CUR.
The char before the newline (selectors.cc:182-183); an empty line's begin;
never backward from CUR when it already sits on the eol (`if (end < begin)
end = begin', :184-185)."
  (let* ((bol (save-excursion (goto-char cur) (line-beginning-position)))
         (eol (save-excursion (goto-char cur) (line-end-position)))
         (last (if (> eol bol) (1- eol) eol)))  ; last non-eol char, or bol if empty
    (max cur last)))

(defun kao-menu--line-end (sel)
  "Move SEL's cursor to its line's last non-eol char, collapsed (Kakoune `gl').
Faithful to `select_to_line_end<true>' (selectors.cc:177, only_move): both
ends land on the target (`kao-menu--line-end-target').  The cursor coord gets
the `max_non_eol_column' sticky TARGET regardless of `only_move'
\(selectors.cc:186, `{end, max_non_eol_column}'): a following `j'/`k' sticks to
the last non-eol char."
  (let ((end (kao-menu--line-end-target (kao-sel-cursor sel))))
    (kao-sel-make :anchor end :cursor end :target 'before-eol)))

(defun kao-menu--line-end-sel (sel)
  "Select from SEL's cursor to its line's last non-eol char (Kakoune `<a-l>').
Faithful to `select_to_line_end<false>' (selectors.cc:177, NOT only_move): the
anchor becomes the OLD cursor (`begin'), the cursor the line-end target.  The
cursor gets the `max_non_eol_column' sticky TARGET (selectors.cc:186,
`{end, max_non_eol_column}'): a following `j'/`k' sticks to the last non-eol
char."
  (let ((cur (kao-sel-cursor sel)))
    (kao-sel-make :anchor cur :cursor (kao-menu--line-end-target cur)
                  :target 'before-eol)))

(defun kao-menu--line-begin (sel)
  "Move SEL's cursor to its line's first column, collapsed (Kakoune `gh').
Faithful to `select_to_line_begin<true>' (selectors.cc:191, only_move): the
target is `{line, 0}'."
  (let ((bol (save-excursion (goto-char (kao-sel-cursor sel))
                             (line-beginning-position))))
    (kao-sel-make :anchor bol :cursor bol)))

(defun kao-menu--line-begin-sel (sel)
  "Select from SEL's cursor to its line begin (Kakoune `<a-h>').
Faithful to `select_to_line_begin<false>' (selectors.cc:191, NOT only_move):
anchor = the OLD cursor (`begin'), cursor = `{line, 0}'."
  (let ((cur (kao-sel-cursor sel)))
    (kao-sel-make :anchor cur
                  :cursor (save-excursion (goto-char cur)
                                          (line-beginning-position)))))

(defun kao-menu--display-line (sel dir)
  "Move SEL's cursor DIR display (wrapped) lines, preserving the screen column.
Kakoune goto `d'/`u' (normal.cc:303-325): with a window, move by display
coordinates keeping the target column; windowless, fall back to logical lines
\(`offset_coord' with tabstop).  kao mirrors the C++ `has_window()' split with
`kao-menu--displayed-p' and the `kao-motion--vertical' fallback.  The offset is
always one line: goto with a count never reads a sub-key (normal.cc:230-237),
so `params.count' is 0 inside the handler.  The cross-press sticky
`display_target' column is NOT persisted — the same per-press column
approximation as kao's `j'/`k' (P0 scope note)."
  (let ((moved nil))
    (or (when (kao-menu--displayed-p)
          (save-excursion
            (goto-char (kao-sel-cursor sel))
            (let ((scol (- (current-column)
                           (save-excursion (vertical-motion 0)
                                           (current-column)))))
              (setq moved (vertical-motion dir))
              ;; Like the C++, a target row outside the buffer (failed
              ;; `buffer_coord') falls through to the logical fallback.
              (when (= moved dir)
                ;; Column positioning by hand: `vertical-motion's (COLS . N)
                ;; form ignores COLS without a live frame, and `move-to-column'
                ;; is display-column (tabstop) aware = Kakoune's column math.
                (let ((row-start-col (current-column))
                      (row-end (save-excursion
                                 (if (zerop (vertical-motion 1)) (point)
                                   (1- (point))))))
                  (move-to-column (+ row-start-col scol))
                  (when (> (point) row-end) (goto-char row-end))
                  (let ((p (point)))
                    (kao-sel-make :anchor p :cursor p)))))))
        (kao-motion--vertical sel dir))))

(defun kao-menu--display-line-down (sel)
  "Kakoune goto `d': SEL one display line down (`kao-menu--display-line')."
  (kao-menu--display-line sel 1))

(defun kao-menu--display-line-up (sel)
  "Kakoune goto `u': SEL one display line up (`kao-menu--display-line')."
  (kao-menu--display-line sel -1))

;;;; Direct line-bound selects/extends (`<a-l>' `<a-h>' `<a-L>' `<a-H>')

;; normal.cc:2539-2542: `repeated<select<Replace|Extend, select_to_line_end|
;; begin<false>>>'.  Replace folds the selector `count' times through
;; `kao--repeat' + `kao--map-selections' (a second application re-anchors at
;; the new cursor exactly as Kakoune's `selection.cursor()' begin does);
;; Extend is the merge-based mapping (anchor preserved/pulled by
;; `merge_selections'; the per-step fold = `repeated<select<Extend>>').

(defun kao-select-line-end ()
  "Kakoune `<a-l>': select from each cursor to its line end (Replace)."
  (interactive)
  (kao--assert-mode)
  (kao--map-selections (kao--repeat #'kao-menu--line-end-sel)))

(defun kao-select-line-begin ()
  "Kakoune `<a-h>': select from each cursor to its line begin (Replace)."
  (interactive)
  (kao--assert-mode)
  (kao--map-selections (kao--repeat #'kao-menu--line-begin-sel)))

(defun kao-extend-line-end ()
  "Kakoune `<a-L>': extend each selection to its line end."
  (interactive)
  (kao--assert-mode)
  (kao--map-selections-extend #'kao-menu--line-end-sel))

(defun kao-extend-line-begin ()
  "Kakoune `<a-H>': extend each selection to its line begin."
  (interactive)
  (kao--assert-mode)
  (kao--map-selections-extend #'kao-menu--line-begin-sel))

(defun kao-menu--first-non-blank (sel)
  "Move SEL's cursor to the first non-blank char of its line, collapsed (`gi').
Faithful to `select_to_first_non_blank' (selectors.cc:201): from the line begin,
skip horizontal blanks (space/tab); on an all-blank line this lands on the
newline (the next non-horizontal-blank), bounded by the line end."
  (let ((p (save-excursion
             (goto-char (kao-sel-cursor sel))
             (beginning-of-line)
             (skip-chars-forward " \t" (line-end-position))
             (point))))
    (kao-sel-make :anchor p :cursor p)))

;;;; goto — coord targets (collapse the whole list to one selection)

(defun kao-menu--goto-coord (pos)
  "Replace the selection list with a single collapsed selection at POS.
Faithful to `select_coord<Replace>' (normal.cc:150): the whole list becomes one
selection (`SelectionList{buffer, coord}').  POS is clamped to an on-char
position by `kao--clamp-sel'."
  (kao-set-selections-raw (list (kao-sel-make :anchor pos :cursor pos)) 0))

(defun kao-menu--extend-coord (pos)
  "Move every selection's cursor to POS, keeping anchors, then sort-and-merge.
Faithful to `select_coord<Extend>' (normal.cc:158-163): unlike the Replace path
\(`kao-menu--goto-coord', which collapses the whole list to one selection), the
Extend path keeps each selection's anchor and only moves its cursor to POS.  POS
is clamped on-char; the anchor is preserved verbatim (the clamp revisit)."
  (let* ((p (kao--clamp-pos pos))
         (list (mapcar (lambda (s)
                         (kao-sel-make :anchor (kao-sel-anchor s) :cursor p))
                       (kao-sels-list kao--sels))))
    ;; direct install (no-clamp): the extend path clamps ONLY the cursor (P) and
    ;; keeps each anchor verbatim; the seam's `kao--clamp-sel' re-clamps BOTH
    ;; ends, moving boundary anchors — a regression (anchor-preservation is
    ;; reserved to `kao--extend-clamp').
    (setq kao--sels (kao-sels-sort-and-merge-overlapping
                     (kao-sels-make :list list :main (kao-sels-main kao--sels))))
    (kao--refresh)))

(defun kao-menu--goto-to (pos mode)
  "Go to POS in MODE: `replace' collapses the list to one selection at POS;
`extend' moves each cursor to POS keeping its anchor.  See
`kao-menu--goto-coord' and `kao-menu--extend-coord'."
  (if (eq mode 'extend)
      (kao-menu--extend-coord pos)
    (kao-menu--goto-coord pos)))

(defun kao-menu--buffer-bottom-pos ()
  "Buffer position of the last REAL line's first column (Kakoune `gj').
`select_coord(line_count - 1)' targets `{last line, 0}' (normal.cc:261-263).
Compute the bol from the last CHAR (`1- point-max'), not `point-max': on a
newline-terminated buffer `point-max' sits on the phantom line after the
trailing `\\n', whose bol would clamp to `back_coord' (the `\\n' itself) instead
of the last text line's column 0.  On the newline-less shape the last char is
already on the last real line, so this is a no-op there (distinct
from `ge', which targets the last char, and from overflow `Ng', which lands on
`back_coord' via `Buffer::clamp')."
  (save-excursion (goto-char (max (point-min) (1- (point-max))))
                  (line-beginning-position)))

(defun kao-menu--line-n-bol (n)
  "Buffer position of the first column of 1-based line N.
Faithful to `LineCount{count - 1}' (normal.cc:230-233): line index N-1 (0-based)
= line number N (1-based)."
  (save-excursion (goto-char (point-min))
                  (forward-line (1- n))
                  (line-beginning-position)))

(defun kao-menu--goto-line (n)
  "Collapse to the first column of 1-based line N (Kakoune count form `Ng')."
  (kao-menu--goto-coord (kao-menu--line-n-bol n)))

(defun kao-menu--last-change-pos ()
  "Position of the buffer's last modification, or Kakoune's exact error.
Goto `.' (normal.cc:365-375): `last_modification_coord' nil → throw \"no
last modification position\".  The coord dispatch arm has already pushed the
jump — Kakoune also pushes BEFORE the nil check (push_jump at :366 precedes
the throw at :368-369), so the error-after-push order is faithful.  The C++
back_coord clamp (:370-371) is covered by the coord path's `kao--clamp-sel'."
  (or (kao-history-last-modification-pos)
      (user-error "no last modification position")))

(defun kao-menu--last-buffer ()
  "Most recently used OTHER live kao buffer, or nil.
Kakoune `ga' targets `context.last_buffer()' (normal.cc:292-302); the Emacs
analog is the most recently selected other buffer (`buffer-list' order),
filtered to kao buffers (cross-buffer stance: kao jump/switch
targets are kao-mode buffers)."
  (seq-find (lambda (b) (and (not (eq b (current-buffer)))
                             (buffer-local-value 'kao-mode b)))
            (buffer-list)))

;;;; goto — window-relative targets (Task 2)

(defun kao-menu--displayed-p ()
  "Non-nil when the current buffer is the one shown in the selected window.
Window geometry (`window-start' etc.) is only meaningful then; otherwise (batch,
an undisplayed buffer) the window commands no-op.  Mirrors the guard in
`kao--window-bounds'."
  (eq (current-buffer) (window-buffer (selected-window))))

(defun kao-menu--window-line-pos (where)
  "Buffer position at the first column of the WHERE visible line, or nil.
WHERE is `top', `bottom', or `center'.  Faithful to `goto_commands'
\\=`t\\='/`b'/`c'
\(normal.cc:269-291): from the window's top line (`window-start'), bottom =
top + height - 1, center = top + height / 2.  nil when not displayed."
  (when (kao-menu--displayed-p)
    (let ((height (window-body-height)))
      (save-excursion
        (goto-char (window-start))
        (forward-line (pcase where
                        ('top 0)
                        ('bottom (1- height))
                        ('center (/ height 2))))
        ;; A window `bottom' that runs past eob lands on the phantom line after
        ;; a trailing `\n'; a display bottom targets a real line, so delegate to
        ;; `kao-menu--buffer-bottom-pos' (the sole home of the last-real-line
        ;; reasoning) rather than re-inlining it here.
        (if (and (eq where 'bottom) (eobp)
                 (> (point) (point-min)) (eq (char-before) ?\n))
            (kao-menu--buffer-bottom-pos)
          (line-beginning-position))))))

(defun kao-menu--recenter (&optional arg)
  "Recenter the cursor line, but only when the buffer is displayed (else no-op).
ARG is passed to `recenter' (nil center, 0 top, -1 bottom)."
  (when (kao-menu--displayed-p) (recenter arg)))

;;;; Goto dispatch (one-shot read-key; Replace `g' and Extend `G')

(defvar kao--goto-specs
  ;; (KEY KIND PAYLOAD).  KIND is how the target is reached:
  ;;   coord    — PAYLOAD is a 0-arg fn returning a buffer position; Replace
  ;;              collapses the list to that coord, Extend moves each cursor to it.
  ;;   selector — PAYLOAD is a per-sel (SEL)->SEL motion; Replace maps it 1:1,
  ;;              Extend merges it onto each selection.
  ;;   window   — PAYLOAD is a `kao-menu--window-line-pos' WHERE symbol; resolves
  ;;              to a coord (or nil = no-op in batch), then behaves like coord.
  ;;   command  — PAYLOAD is a command symbol, run via `call-interactively';
  ;;              mode-independent (`G' behaves like `g'), selections untouched.
  ;;   buffer   — PAYLOAD is a 0-arg fn returning a buffer or nil; jump-push
  ;;              then switch (nil = user-error); mode-independent.
  (list (list ?g 'coord    (lambda () (point-min)))   ; buffer top
        (list ?k 'coord    (lambda () (point-min)))
        (list ?j 'coord    #'kao-menu--buffer-bottom-pos)
        (list ?e 'coord    (lambda () (point-max)))   ; buffer end (clamped)
        (list ?l 'selector #'kao-menu--line-end)
        (list ?h 'selector #'kao-menu--line-begin)
        (list ?i 'selector #'kao-menu--first-non-blank)
        (list ?t 'window   'top)
        (list ?b 'window   'bottom)
        (list ?c 'window   'center)
        (list ?d 'selector #'kao-menu--display-line-down)
        (list ?u 'selector #'kao-menu--display-line-up)
        (list ?a 'buffer   #'kao-menu--last-buffer)
        (list ?. 'coord    #'kao-menu--last-change-pos)
        (list ?f 'command  #'find-file-at-point))
  "Spec for every goto sub-key, shared by Replace `g' and Extend `G'.
`g'/`k' buffer top, `j' bottom, `e' end (normal.cc:247-267); `l'/`h'/`i' line
end/begin/first-non-blank (selectors.cc); \\=`t\\='/`b'/`c' window
top/bottom/center
\(normal.cc:269-291); `d'/`u' next/previous display line (normal.cc:303-325 —
real Kakoune keys, restored from the v1 xref shadowing; xref now lives on
`SPC d'/`SPC r', see `kao-keys-user-alist'); `a' last buffer
\(normal.cc:292-302); `.' last buffer change (normal.cc:365-375); `f' goto
file (Kakoune `gf', normal.cc:293, via `find-file-at-point').  See
`kao-goto--dispatch'.")

(defun kao-goto--dispatch (key mode)
  "Run the goto target for KEY in MODE (`replace' or `extend').
Faithful to `goto_commands<mode>' (normal.cc:228): coord/window targets go
through `select_coord<mode>' (`kao-menu--goto-to'), selector targets through
`select<mode>' (`kao--map-selections' / `kao--map-selections-extend').  The
sub-key is folded to lower case before lookup (`to_lower(*cp)', normal.cc:245),
so `gJ'/`GE' behave as `gj'/`ge'; the view and combine menus switch on the raw
codepoint and stay case-sensitive.  A non-character event (the `escape' symbol)
passes the `characterp' guard unchanged; an unmapped KEY is a no-op (the caller
already handled Escape)."
  (let ((spec (assq (if (characterp key) (downcase key) key) kao--goto-specs)))
    (when spec
      (let ((kind (nth 1 spec)) (payload (nth 2 spec)))
        (pcase kind
          ;; The coord targets (g/k/j/e) push a jump first, capturing the
          ;; pre-goto selections (Kakoune `goto_commands' push_jump,
          ;; normal.cc:249/262/266 — mode-independent, so `g' and `G' both push;
          ;; selector/window targets do NOT push)..
          ('coord    (kao--jump-push)
                     (kao-menu--goto-to (funcall payload) mode))
          ('selector (if (eq mode 'extend)
                         (kao--map-selections-extend payload)
                       (kao--map-selections payload)))
          ('window   (let ((pos (kao-menu--window-line-pos payload)))
                       (when pos (kao-menu--goto-to pos mode))))
          ;; Command targets (f) run the Emacs command as-is (ffap etc.).  The
          ;; command performs any buffer switch itself, so capture the
          ;; ORIGINATING buffer and its pre-jump selections up front and, only
          ;; when the command actually changed buffer, push a jump recording
          ;; those originating selections -- faithful to Kakoune's
          ;; `if (buffer != &context.buffer()) { push_jump; change_buffer; }'
          ;; (normal.cc:358-361: push only on a real switch), so `C-o' returns
          ;; to the pre-`gf' selections.  No selection transform, and the push
          ;; is tagged the OLD buffer (mode-independent)..
          ('command  (let ((origin (current-buffer))
                           (sels kao--sels))
                       (call-interactively payload)
                       (unless (eq (current-buffer) origin)
                         (with-current-buffer origin
                           (kao--jump-push sels)))))
          ;; Buffer target (a): error BEFORE the jump push (Kakoune throws
          ;; "no last buffer" at normal.cc:294-297, ahead of the :299
          ;; push_jump), then push in the OLD buffer and switch
          ;; (`change_buffer'; mode-independent, like the C++).
          ('buffer   (let ((target (funcall payload)))
                       (unless target (user-error "no last buffer"))
                       (kao--jump-push)
                       (switch-to-buffer target))))))))

(defvar kao--goto-info
  '((?g . "buffer top")
    (?k . "buffer top")
    (?j . "buffer bottom")
    (?e . "buffer end")
    (?l . "line end")
    (?h . "line begin")
    (?i . "line non blank start")
    (?t . "window top")
    (?b . "window bottom")
    (?c . "window center")
    (?d . "next displayed line")
    (?u . "prev displayed line")
    (?a . "last buffer")
    (?. . "last buffer change")
    (?f . "file (ffap)"))
  "Autoinfo rows (EVENT . DOCSTRING) for the goto menu.
The faithful subset of Kakoune's `KeyInfo' built-ins (normal.cc:381-392) that
kao implements (`a'/`.' use Kakoune's own row texts).  Kakoune's own box
OMITS `d'/`u' even though the keymap handles them — kao shows them precisely
because that omission hid the keys from the v1 gap sweep.  Its keys are kept
in lockstep with `kao--goto-specs' by a parity test.")

(defconst kao--goto-kinds '(coord selector window command buffer)
  "Valid KIND symbols for `kao--goto-specs' / `kao-goto-define'.
See `kao--goto-specs' for what each kind means.")

(defun kao--alist-upsert (symbol key value)
  "Upsert (KEY . VALUE) into the alist held in SYMBOL's value.
When KEY already has a row, `setcdr' it to VALUE in place; otherwise set SYMBOL
to the old list with (KEY . VALUE) APPENDED AT THE END.  The append-at-end order
is load-bearing: the goto/view info rows display in registration order in the
`g'/`v' autoinfo boxes.  Private to kao-menu -- the four `kao-goto-define' /
`kao-view-define' upserts share it; no other module calls it."
  (let ((row (assq key (symbol-value symbol))))
    (if row
        (setcdr row value)
      (set symbol (append (symbol-value symbol) (list (cons key value)))))))

;;;###autoload
(defun kao-goto-define (key kind payload doc)
  "Register goto sub-KEY on the `g'/`G' menu (the public config surface).
KIND is one of `kao--goto-kinds' (coord, selector, window, command, buffer) and
governs how PAYLOAD reaches its target -- see `kao--goto-specs'.  DOC is the
autoinfo row text shown in the `g' box.  Upserts atomically: the spec row
\(KEY KIND PAYLOAD) in `kao--goto-specs' and the info row (KEY . DOC) in
`kao--goto-info' are replaced together when KEY already exists, else both are
appended -- so the parity invariant holds and a package reload no longer wipes
user rows (the tables are `defvar's; config against `kao-...', not
`kao--...' reach-ins).  Signal an error on an unknown KIND."
  (unless (memq kind kao--goto-kinds)
    (error "Unknown goto kind: %S (expected one of %S)" kind kao--goto-kinds))
  (kao--alist-upsert 'kao--goto-specs key (list kind payload))
  (kao--alist-upsert 'kao--goto-info key doc)
  key)

(defun kao-goto ()
  "Kakoune `g': go to a location (Replace mode, normal.cc:228 `goto_commands').
With a count N, go straight to line N (`Ng', normal.cc:230) and center the view;
otherwise read ONE sub-key and dispatch through `kao--goto-specs' in
Replace mode.  An unknown sub-key (or escape) cancels with no change."
  (interactive)
  (kao--assert-mode)
  (if (> kao--count 0)
      (progn (kao--jump-push)            ; `Ng' pushes a jump (normal.cc:232)
             (kao-menu--goto-line kao--count)
             (kao-menu--recenter))
    (kao-goto--dispatch
     (kao-info--with-box "goto" kao--goto-info (kao--read-key "goto"))
     'replace)))

(defun kao-goto-extend ()
  "Kakoune `G': extend to a location (Extend mode, `goto_commands<Extend>').
Same targets as `g', but each selection keeps its anchor and only its cursor
moves to the target (`select_coord<Extend>'/`select<Extend>').  With a count N,
extend to line N (`select_coord<Extend>(N-1)') and center; otherwise read ONE
sub-key and dispatch in Extend mode.  An unknown sub-key (or escape) cancels."
  (interactive)
  (kao--assert-mode)
  (if (> kao--count 0)
      (progn (kao--jump-push)            ; `NG' pushes a jump (normal.cc:232)
             (kao-menu--goto-to (kao-menu--line-n-bol kao--count) 'extend)
             (kao-menu--recenter))
    (kao-goto--dispatch
     (kao-info--with-box "goto (extend)" kao--goto-info
       (kao--read-key "goto (extend)"))
     'extend)))

;;;; View — viewport movement (all guarded by `kao-menu--displayed-p')

(defalias 'kao-menu--sync-main-to-point #'kao--sync-main-to-point
  "Thin alias for `kao--sync-main-to-point', lifted to kao-state.
The mouse stance needs it from `kao--foreign-sync' (kao-state), and
kao-menu requires kao-state, not the other way — the lift-with-alias
precedent.  The v-menu callers and tests keep this name.")

(defun kao-menu--scroll-and-sync (fn n)
  "Scroll the viewport via FN by N, syncing the main IFF the scroll carried point.
A no-op when the buffer is not displayed.  The sync
\(`kao-menu--sync-main-to-point') runs only when the scroll actually
carried point off the main cursor -- i.e. `point' no longer equals the main
selection's cursor.  Faithful to Kakoune's `ScrollFlags::PreserveSelections'
\(normal.cc:449-453, input_handler.cc:1802-1815): a scroll that moves nothing
must leave every selection (main extent + secondaries) verbatim; only when
Emacs carries point to the window edge does have a detached cursor to
reconcile.  This is the kao-menu.el half of;
gates the kao-state.el wheel/scroll branch with the same
`point != main cursor' predicate (the kao-state.el click-gate shape).  Contrast
`kao--scroll-step' (the `<c-d>'/`<c-u>' family), Kakoune's
`MoveCursorAndAnchor', whose collapse is unconditional."
  (when (kao-menu--displayed-p)
    (funcall fn n)
    (when (/= (point) (kao-sel-cursor (kao--main-sel)))
      (kao-menu--sync-main-to-point))))

(defun kao-menu--hscroll-center ()
  "Center the cursor column horizontally (Kakoune `vm').  No-op if not displayed."
  (when (kao-menu--displayed-p)
    (set-window-hscroll (selected-window)
                        (max 0 (- (current-column) (/ (window-body-width) 2))))))

(defun kao-menu--hscroll-edge (side)
  "Put the cursor column at the SIDE (`left'/`right') edge (Kakoune `v<'/`v>').
Best-effort horizontal scroll; no-op when not displayed."
  (when (kao-menu--displayed-p)
    (set-window-hscroll (selected-window)
                        (pcase side
                          ('left  (current-column))
                          ('right (max 0 (- (current-column)
                                            (1- (window-body-width)))))))))

(defvar kao--view-table
  (list
   ;; reposition around the fixed cursor (count ignored)
   (cons ?v (lambda (_n) (kao-menu--recenter nil)))
   (cons ?c (lambda (_n) (kao-menu--recenter nil)))
   (cons ?t (lambda (_n) (kao-menu--recenter 0)))
   (cons ?b (lambda (_n) (kao-menu--recenter -1)))
   (cons ?m (lambda (_n) (kao-menu--hscroll-center)))
   (cons ?< (lambda (_n) (kao-menu--hscroll-edge 'left)))
   (cons ?> (lambda (_n) (kao-menu--hscroll-edge 'right)))
   ;; scroll the viewport (count = max(1,n); carries the cursor to the edge)
   (cons ?j (lambda (n) (kao-menu--scroll-and-sync #'scroll-up-command n)))
   (cons ?k (lambda (n) (kao-menu--scroll-and-sync #'scroll-down-command n)))
   (cons ?h (lambda (n) (kao-menu--scroll-and-sync #'scroll-right n)))
   (cons ?l (lambda (n) (kao-menu--scroll-and-sync #'scroll-left n))))
  "Alist mapping a view sub-key (Emacs event) to a (COUNT) command thunk.
Reposition (cursor fixed, count ignored): `v'/`c' center, \\=`t\\=' top,
`b' bottom,
`m' center-horiz, `<' left, `>' right.  Scroll (count = `max(1,n)', cursor
carried to the edge): `j'/`k' vertical, `h'/`l' horizontal.  Faithful to
`view_commands' (normal.cc:422-457) within the Emacs viewport model.")

(defvar kao--view-info
  '((?v . "center cursor (vertically)")
    (?c . "center cursor (vertically)")
    (?m . "center cursor (horizontally)")
    (?t . "cursor on top")
    (?b . "cursor on bottom")
    (?< . "cursor on left")
    (?> . "cursor on right")
    (?h . "scroll left")
    (?j . "scroll down")
    (?k . "scroll up")
    (?l . "scroll right"))
  "Autoinfo rows (EVENT . DOCSTRING) for the view menu.
Kakoune's `KeyInfo' built-ins for `view_commands' (normal.cc:462), all of
which kao implements.  Its keys are kept in lockstep with `kao--view-table'
by a parity test.")

;;;###autoload
(defun kao-view-define (key fn doc)
  "Register view sub-KEY on the `v'/`V' menu (the public config surface).
FN is called with the numeric count as its single argument (the (N) thunk
shape, like the built-in view handlers).  DOC is the autoinfo row text shown
in the `v' box.  Upserts atomically: the row (KEY . FN) in `kao--view-table'
and the info row (KEY . DOC) in `kao--view-info' are replaced together when KEY
already exists, else both are appended -- so the parity invariant holds and a
package reload no longer wipes user rows (the tables are `defvar's)."
  (kao--alist-upsert 'kao--view-table key fn)
  (kao--alist-upsert 'kao--view-info key doc)
  key)

(defun kao-view ()
  "Kakoune `v': move the view (non-locked, normal.cc:396 `view_commands<false>').
Read ONE sub-key and dispatch through `kao--view-table'; the count
\(captured before the read) is `max(1,count)' for the scroll commands and ignored
by the reposition commands.  An unknown sub-key (or escape) cancels.  See
for why the scroll commands carry the cursor rather than detaching the viewport."
  (interactive)
  (kao--assert-mode)
  (let* ((n (kao--repeat-count))
         (entry (assq (kao-info--with-box "view" kao--view-info
                        (kao--read-key "view"))
                      kao--view-table)))
    (when entry (funcall (cdr entry) n))))

(defun kao-view-locked ()
  "Kakoune `V': move the view, staying in the menu (locked view_commands<true>).
Like `v', but the menu re-arms with the same count after each sub-key, so a
run of view keys works without re-pressing `V' (normal.cc:406-407).  An
unmapped CHARACTER key messages \"key not mapped\" and the lock STAYS armed:
Kakoune re-arms the lock (`if (lock) view_commands<true>', normal.cc:406-407)
BEFORE raising the unmapped-key error (normal.cc:458-459), and `NextKey' has
popped its mode by then (input_handler.cc:1143-1150), so the freshly-armed lock
survives -- the ONLY exit is Escape.  kao also exits on a non-character event
\(the `escape' symbol, arrows) as an Emacs-safety terminator, so an exhausted
read never hangs.  Each sub-key dispatches through `kao--view-table' via
`kao--read-key' (macro-safe); for the Emacs
viewport-vs-cursor caveats the dispatch carries."
  (interactive)
  (kao--assert-mode)
  (let ((n (kao--repeat-count)))
    (kao-info--with-box "view (lock)" kao--view-info
      (catch 'done
        (while t
          (let* ((key (kao--read-key "view (lock)"))
                 (entry (assq key kao--view-table)))
            (cond ((or (not (characterp key)) (eq key ?\e))
                   (throw 'done nil))            ; Escape / non-char event exits
                  (entry (funcall (cdr entry) n))
                  (t (message "key not mapped")))))))))  ; re-arm, do not exit

;;;; Scroll — page scroll (<c-d>/<c-u>/<c-f>/<c-b>, viewport family)
;;
;; Kakoune `scroll<direction,half>' (normal.cc:1593) scrolls the window by
;; `(window_height - 2) / (half ? 2 : 1) * count' lines with
;; `OnHiddenCursor::MoveCursorAndAnchor' (`scroll_window', input_handler.cc:1793):
;; the window moves AND the main collapses onto the cursor re-clamped into the new
;; window, then `sort_and_merge_overlapping' (line 1833).  Emacs's native
;; `scroll-up-command'/`scroll-down-command' do exactly this to `point' (they carry
;; it to the window edge), so kao = native scroll + collapse the main onto the new
;; point + merge.  Unlike `vj'/`vk' (`PreserveSelections' — a detached viewport
;; Emacs cannot model, hence the *approximation*), these are FAITHFUL:
;; `MoveCursorAndAnchor' genuinely moves+collapses the main.  `scrolloff' is
;; ignored, consistent.
;;
;; The FULL page delegates with a nil arg —
;; one native page per count, byte-identical paging to the native keys
;; (`next-screen-context-lines', `scroll-preserve-screen-position', and any
;; user tuning respected; with the defaults the native page IS Kakoune's
;; `height - 2').  The HALF page keeps the explicit Kakoune arg — Emacs has
;; no native half page.  Edge signals (`end-of-buffer'/`beginning-of-buffer')
;; PROPAGATE for native feedback — a documented deviation from Kakoune's
;; silent early-return (input_handler.cc:1805); selections are unchanged
;; either way.

(defun kao--scroll-lines (height half count)
  "Lines to scroll for a window of HEIGHT text rows.
HALF non-nil selects a half page, else a full page; COUNT is the Kakoune count.
Faithful to `(window.dimensions().line - 2) / (half ? 2 : 1) * count'
\(normal.cc:1598); returns 0 (a no-op) for a window too short to scroll.
Since only the half arm feeds the scroll commands (the full page
delegates to the native page size); the full arm remains the Kakoune-math
reference, pinned as a pure function."
  (max 0 (* (max 1 count) (/ (- height 2) (if half 2 1)))))

(defun kao--scroll-step (fn lines)
  "One native scroll via FN with arg LINES, then the MoveCursorAndAnchor sync.
LINES nil = the native full page.  Edge signals propagate exactly as
the native key would surface them.  The sync (collapse the main onto point +
sort-and-merge, `scroll_window' input_handler.cc:1810-1834) runs when point
OR the window moved: `scroll-error-top-bottom' moves point to the buffer
limit WITHOUT moving the window, and an unsynced point move would be snapped
back by the post-command mirror."
  (let ((start (window-start))
        (p (point)))
    (funcall fn lines)
    (when (or (not (eql start (window-start)))
              (/= p (point)))
      (kao-menu--sync-main-to-point t))))

(defun kao--scroll (direction half)
  "Scroll the viewport one page in DIRECTION (`down'/`up'), family.
HALF non-nil scrolls a half page instead of a full one.
Reads `kao--count'; no-op when the buffer is not displayed (batch).  The
full page is COUNT native pages — one nil-arg `scroll-up-command'/
`scroll-down-command' per page, identical to the native keys; each
page syncs before the next, so an edge signal mid-count keeps the pages
already scrolled.  The half page passes the Kakoune line count explicitly."
  (when (kao-menu--displayed-p)
    (let ((fn (if (eq direction 'down)
                  #'scroll-up-command #'scroll-down-command)))
      (if half
          (let ((lines (kao--scroll-lines (window-body-height) t
                                          (kao--repeat-count))))
            (when (> lines 0)
              (kao--scroll-step fn lines)))
        (dotimes (_ (kao--repeat-count))
          (kao--scroll-step fn nil))))))

(defun kao-scroll-half-down ()
  "Scroll half a page down (Kakoune `<c-d>', `scroll<Forward,true>')."
  (interactive)
  (kao--assert-mode)
  (kao--scroll 'down t))

(defun kao-scroll-half-up ()
  "Scroll half a page up (Kakoune `<c-u>', `scroll<Backward,true>')."
  (interactive)
  (kao--assert-mode)
  (kao--scroll 'up t))

(defun kao-scroll-full-down ()
  "Scroll a full page down (Kakoune `<c-f>'/`<PageDown>', `scroll<Forward>')."
  (interactive)
  (kao--assert-mode)
  (kao--scroll 'down nil))

(defun kao-scroll-full-up ()
  "Scroll a full page up (Kakoune `<c-b>'/`<PageUp>', `scroll<Backward>')."
  (interactive)
  (kao--assert-mode)
  (kao--scroll 'up nil))

;;;; space — user-mode prefix keymap (exec_user_mappings, normal.cc:2231/2623)

(defvar kao-user-map (make-sparse-keymap)
  "Keymap for kao's user mode, reached with `<space>' from normal state.
Faithful analog of Kakoune's user keymap mode (`exec_user_mappings',
normal.cc:2231): `<space>' (normal.cc:2623) runs the next key through the User
keymap, which Kakoune users populate with `map global user <key> <keys>'.
Kakoune ships no user mappings; kao's defaults live in
`kao-keys-user-alist' (comment toggling on `#', xref on `d'/`r').  Bind
your own with, e.g., (define-key kao-user-map \"f\" #\\='find-file).
Because it is a real Emacs prefix keymap,
multi-key sequences and a `which-key'-style listing work for free.  `SPC'
is bound straight to this map, so each key reaches its binding
directly.")

;;;; register select — the `"' prefix (input_handler.cc:322-336)

;; Kakoune handles `"' INSIDE the normal-mode key loop, next to the digit
;; and Backspace params keys: `on_next_key_with_autoinfo' reads one key and
;; sets `m_params.reg', which the next dispatched command consumes (and any
;; command resets, input_handler.cc:365-372).  The state and the reset hook
;; live with the count in kao-state.el (the `NormalParams' family);
;; the COMMAND lives here with the other one-shot read-key menus so
;; it can show the autoinfo box without kao-state depending on
;; kao-info.

(defconst kao--register-info
  '((?0 . "selections capture group [0-9]")
    (?% . "buffer name")
    (?. . "selection contents")
    (?# . "selection index")
    (?_ . "null register")
    (?\" . "default yank/paste register")
    (?@ . "default macro register")
    (?/ . "default search register")
    (?^ . "default mark register")
    (?| . "default shell command register")
    (?: . "last entered command (no ':' language in kao)"))
  "Autoinfo rows for the register prompt.
Kakoune `register_doc' (input_handler.cc:208-220); its single \"[0-9]\" line
is rendered as one `?0' row.  The dynamic registers `%'/`.'/`#'/0-9 ship
\(kao-state registrations), so their rows read plainly; only the `:' history
register is out of scope (the `:' command language and kakrc hooks are),
and its row names that rather than promising an unsupported name.")

(defun kao-select-register ()
  "Kakoune `\\=\"': read one key and set the pending register.
Ports the `\"' branch of the normal state key loop (input_handler.cc:322-336):
Escape or a non-character key cancels, leaving any pending register as it
was; a char above 127 reports \"invalid register\" as a status message (not
an error, `print_status'); anything else becomes `kao--pending-register',
consumed by the next register-using command and cleared after any other
command (`kao--maybe-reset-count').  The accumulated count is untouched, so
\\=`3\"ad' and \\=`\"a3d' both delete three to register a."
  (interactive)
  (kao--assert-mode)
  (let ((key (kao-info--with-box "register" kao--register-info
               (kao--read-key "register"))))
    (cond ((or (not (characterp key)) (eq key ?\e)) nil)
          ((> key 127) (message "invalid register '%c'" key))
          (t (setq kao--pending-register key)))))

(defun kao-insert-register ()
  "Kakoune insert-mode `<c-r>': insert a register's contents at the cursors.
Ports the `ctrl(\\='r\\=')' branch of the insert key loop
\(input_handler.cc:1319-1330): a one-shot read-key under the register
autoinfo box; Escape or a non-character key cancels; the name resolves
through the faithful accessor (lowercased, validated, `_' reads empty),
so an unknown name signals \"no such register\" exactly as
`RegisterManager::operator[]' does (there is no >127 special case here,
unlike the `\\='\"' prefix).  Kakoune hands the i-th selection the i-th
string clamped to the last (`insert(strings)', :1445); kao
inserts the MAIN selection's string at point and the exit replay
distributes it to the secondary sites — the identical end result when the
register holds one string (the common case), a documented approximation
for a multi-string register across multiple cursors (each would get its
own string in Kakoune; in kao all get the main's)."
  (interactive)
  (kao--assert-mode)
  (let ((key (kao-info--with-box "register" kao--register-info
               (kao--read-key "register"))))
    (when (and (characterp key) (/= key ?\e))
      (let ((strings (kao-register-get key)))
        (when strings
          (insert (nth (min (kao-sels-main kao--sels)
                            (1- (length strings)))
                       strings)))))))

(defun kao-prompt-insert-register ()
  "Kakoune prompt `<c-r>': insert a register's main value into the prompt.
Ports the PromptMode `ctrl(\\='r\\=')' branch (input_handler.cc:758-780):
a one-shot read-key under the register autoinfo box; Escape or a
non-character key cancels.  The register resolves IN THE ORIGINATING
BUFFER (`minibuffer-selected-window') — dynamic registers (`%' `.' `#'
`0'-`9') read buffer-local state there, and the value is indexed by
that buffer's main selection (= `main_sel_register_value', the same
accessor the C++ calls); without a kao selection list the first string is
used.  The Alt-joined and Ctrl-quoted modifier variants are DEFERRED —
they need Kakoune's quoting machinery, the `:' command-language scope
\(family, documented in)."
  (interactive)
  (let ((key (kao-info--with-box "register" kao--register-info
               (kao--read-key "register"))))
    ;; Control chars are rejected, not looked up: in Kakoune a Ctrl-modified
    ;; key here selects the QUOTED variant (deferred, input_handler.cc:762-769)
    ;; — letting it fall through would surface a loud-but-wrong "no such
    ;; register" instead of the documented deferral.
    (when (and (characterp key) (/= key ?\e) (>= key ?\s))
      (let ((val (kao--main-sel-register-value
                  key (window-buffer (minibuffer-selected-window)))))
        (when val (insert val))))))

(provide 'kao-menu)
;;; kao-menu.el ends here
