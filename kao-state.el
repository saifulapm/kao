;;; kao-state.el --- States, keymaps, selection-list state for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 2.  This file holds the buffer-local selection
;; list — the single source of truth — and the machinery that makes it live in a
;; real buffer:
;;
;; * `kao--sels' is a `kao-sels' struct; every kao command reads and writes it,
;;   and the Emacs point/mark are only a *derived mirror* of its main selection.
;; * The normal-state keymap is installed *once* into `emulation-mode-map-alists'
;;   and gated by the buffer-local `kao--normal-active' flag.  Switching state
;;   later flips that flag — there is no per-state keymap rebuild.
;; * `kao-mode' wires a lean `post-command-hook' that mirrors the main selection
;;   and redraws the visible secondaries.

;;; Code:

(require 'cl-lib)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-history)                    ; buffer history tree
(require 'kao-register)                  ; `kao-register-define-dynamic'
                                         ; dynamic-register defs below (% . # 0-9),
                                         ; sibling to this file's `kao--register-arg'
                                         ; pending-register plumbing
                                         ; (`kao--snapshot-sels' now lives in
                                         ; kao-selection.el)

;; The mode-line lighter renders `kao-lighter' (default: the faithful Kakoune
;; `mode_info' string), which lives in kao-modeline.el (required after kao-state
;; in kao.el; the `(:eval …)' lighter is lazy, so `kao--lighter' need only exist
;; by activation time).
(declare-function kao--lighter "kao-modeline")

(defvar-local kao--sels nil
  "Buffer-local `kao-sels' struct — the single source of truth.")

(defvar-local kao--sels-id nil
  "History id the live `kao--sels' integer coordinates are valid against.
kao stores selections as integers at rest; a buffer edit made OUTSIDE a kao
command (a timer, a process filter, an async formatter) shifts the buffer but
never reconciles `kao--sels', so the next keystroke would snap the cursor to
stale offsets.  `kao--refresh' folds the live list through the tree
\(`kao--snapshot-update') whenever this tag has fallen behind
`kao-history-current-id' — the elisp analogue of Kakoune's
`Context::selections()' calling `SelectionList::update()' before every access
\(context.cc:126/150/195).  Zero cost on the hot path: an integer `/=' compare,
translating only after a foreign non-command edit advanced the tree
\.")

(defvar-local kao--sels-edit-pending nil
  "Non-nil when kao's OWN command installed `kao--sels' in the post-edit frame.
kao's edit primitives (`kao--multi-edit', `kao--edit-keeping-sels') and the
foreign-command reconciliation (`kao--foreign-sync') leave `kao--sels' already
in the current buffer frame, so once their modifications commit
\(`kao--hist-maybe-commit') the live list must be RE-TAGGED to the new id, never
folded through its own edit edge (which would double-shift it).  A foreign
non-command edit leaves this nil, so `kao--refresh' translates instead
\.")

(defvar kao--last-select nil
  "Thunk repeating the last object-select or find-char (`<a-.>').
A no-arg closure set at the object (`kao--object-dispatch') and find-char
\(`kao--select-to-char') dispatch seams — the same closure that runs the live
selection — faithful to Kakoune `set_last_select'/`repeat_last_select'
\(normal.cc:144/177; only object selects and `f'/\\=`t\\=' record one).
Client-global, not buffer-local (single-client stance, same locality
as the jump list and registers): the recorded closure is buffer-agnostic, so
`<a-.>' repeats the last select at the cursor in whatever buffer is now current,
matching Kakoune's per-client `last_select'.")

(defvar-local kao--normal-active nil
  "When non-nil, the normal-state keymap is live in this buffer.")

(defvar-local kao--insert-active nil
  "When non-nil, the insert-state keymap is live in this buffer.")

(defvar-local kao--insert-undo-handle nil
  "Change-group handle for the current insert session (undo amalgamation).")

(defvar-local kao--insert-start nil
  "Marker at the position where the current insert session began.")

(defvar-local kao--insert-end nil
  "Insertion-type-t marker bracketing the typed run with `kao--insert-start'.
Created at `kao--insert-start's position wherever the session opens; because
it advances on insertion (type t) while the start marker (type nil) does not,
text typed at point grows the [start . end] span, but a native End key
\(`end-of-line') that only moves point does not.  `kao-insert-exit' then
measures the net typed text
as `(buffer-substring kao--insert-start kao--insert-end)' — robust to
mid-insert point motion, so pre-existing content between the typed run and a
wandered point is never captured and mass-duplicated at the secondaries
\.")

(defvar-local kao--insert-restore nil
  "Non-nil when leaving insert should step the cursor back one (Append).")

(defvar-local kao--insert-secondary-sites nil
  "Non-main insert sites for a multi-selection insert session.
A list of (MARK . ANCHOR-or-nil) conses, one per non-main selection in sel
order, replacing the old hand-index-aligned marks/anchors pair.
MARK is the insertion-type-t marker at the selection's insert site: the user
types only at the main; on exit the net typed text replays at each MARK.
ANCHOR mirrors `kao--insert-main-anchor' for this selection (a marker for
`i'/`a', nil for a collapse mode).  nil (the empty list) for a single selection
or an entry command that does not opt into multi-insert.  Read and freed by
`kao-insert-exit'.")

(defvar-local kao--insert-main-anchor nil
  "Marker for the main selection's shaped insert-exit anchor, or nil.
Kakoune's `prepare' reshapes each selection at insert entry and typing extends
it, so the ORIGINAL span survives the session (input_handler.cc:1456-1533): `i'
anchors at the old max on a type-t marker that rides forward past text typed
before it, `a' anchors at the old min.  nil for the collapse modes (c/A/I/o/O),
where `kao-insert-exit' stands the cursor alone.  Read and freed by
`kao-insert-exit'.")

(defvar-local kao--insert-oneshot nil
  "Non-nil while a one-shot normal command is pending (Kakoune `<a-;>').
Ports `State::SingleCommand' (input_handler.cc:243/339-344): set by
`kao-insert-one-shot', holds (POINT TICK SELS) — the entry point,
`buffer-chars-modified-tick', and a deep copy of the synced selections — so
the pop-back (`kao--oneshot-pop') can keep the session geometry verbatim
when the command changed nothing.  Cleared by the pop-back and by the
`kao-mode' teardown.")

(defvar-local kao--input-method nil
  "Input method to restore on insert-state entry (hel-study-1).
Saved from `current-input-method' at `kao-mode' enable and kept current by the
buffer-local `input-method-activate-hook'/`input-method-deactivate-hook'
handlers.  The method is deactivated in normal state and reactivated on insert
entry — the native-mechanism family, ported from evil/hel; not a
Kakoune parity feature (Kakoune has no in-editor input method).")

(defvar-local kao--last-insert-opener nil
  "Function opening the last insert's session, for `.' to repeat.
Each entry command records itself (i/a/I/A/o/O); `kao--change-selections'
records itself (the NO-yank change core — `.' after `c' re-erases without
yanking, faithful to `change<yank>'/`prepare(Replace)',
normal.cc:806/input_handler.cc:1468).")

(defvar-local kao--last-insert-text nil
  "Net text typed in the last insert session, replayed by `.'.
Recorded by `kao-insert-exit' (Kakoune `repeat_last_insert',
input_handler.cc:1598); nil until the first insert in the buffer.")

(defvar-local kao--last-insert-count 1
  "Effective count of the last insert entry, replayed by `.'.
Every entry command stores its count (Kakoune `m_last_insert->count = count',
input_handler.cc:1197) and `.' re-enters with it (:1610) — a count typed
before `.' itself is ignored (`repeat_last_insert' takes unnamed
NormalParams, normal.cc:172-175).  Only o/O observe it (`prepare' ignores
count for every other mode, input_handler.cc:1456-1515).")

(defvar-local kao--count 0
  "Accumulated numeric count for the next normal-state command (0 = none).
Digits feed it via `kao-digit'; a non-digit command consumes it and
`kao--maybe-reset-count' clears it afterward (cf. input_handler.cc:304-313).")

(defvar-local kao--pending-register nil
  "Pending register char for the next normal-state command, or nil.
Set by `kao-select-register' (the Kakoune `\\=\"' prefix, `m_params.reg',
input_handler.cc:322-336); read through `kao--register-arg' by the
register-consuming commands; cleared by `kao--maybe-reset-count' after any
command that is not itself a params key, exactly as Kakoune resets
`m_params' when a command is dispatched (input_handler.cc:365-372).")

(defvar kao-normal-state-map
  (let ((m (make-sparse-keymap)))
    ;; Normal state must never dirty the buffer: remap `self-insert-command'
    ;; to `undefined' so every unbound printable key is inert (the meow/evil
    ;; pattern; Kakoune silently ignores unmapped normal keys -- the
    ;; `get_normal_command' else-branch, input_handler.cc:368-371).  Explicit
    ;; bindings (digits, motions, edits) take precedence over the remap.
    (suppress-keymap m t)
    m)
  "Keymap for kao normal state.
Only the map and its `suppress-keymap' buffer-integrity mechanism
live here; every default binding — including RET -> `undefined' (Kakoune
leaves <ret> unmapped in normal state, so it must not insert) — is
declared in the `kao-keys-normal-alist' table (kao-keys.el).")

(defvar kao-insert-state-map (make-sparse-keymap)
  "Keymap for kao insert state.
Nearly empty so native editing flows through — the DECIDED stance:
Kakoune's insert-state erase/commit keys (`<c-w>' `<c-u>' `<c-k>'
...) are deliberately NOT bound; Emacs's own editing commands serve insert
state (the power-of-Emacs goal).  A faithful-keys opt-in defcustom is
deferred until someone asks for it.  What IS bound (by the
`kao-keys-insert-alist' table, kao-keys.el): escape (exit), `M-;'
\(one-shot normal), `C-r' (insert register), and `C-n'/`C-p'
\(native capf completion — corfu/company shadow these keys with
their own next/prev maps once the popup is live).")

(defvar kao-prompt-map (make-sparse-keymap)
  "Keymap composed over the minibuffer map in kao's prompts.
The four kao prompts (search regex, incsearch, pipe command, object desc)
are Kakoune PromptMode prompts; this map layers their faithful keys —
`C-r' register insert (PromptMode `ctrl(\\='r\\=')',
input_handler.cc:758-780), bound by the `kao-keys-prompt-alist' table
\(kao-keys.el) — over whatever local map the reader installs,
via `kao--prompt-setup' in `minibuffer-with-setup-hook'.  Everything else
stays native (the prompt-keys stance).")

(defvar kao-regex-prompt-map (make-sparse-keymap)
  "Extra keymap for the REGEX prompts only.
Composed over `kao-prompt-map' by `kao--regex-prompt-setup' (kao-multi):
TAB -> `completion-at-point' for the buffer word-db completer Kakoune
passes to `regex_prompt' (normal.cc:962-985).  Bound by the
`kao-keys-regex-prompt-alist' table (kao-keys.el).")

(defun kao--prompt-setup ()
  "Compose `kao-prompt-map' over the minibuffer's local map.
Run from `minibuffer-with-setup-hook' by every kao prompt; composing (not
mutating) keeps the shared minibuffer maps untouched for non-kao prompts."
  (use-local-map (make-composed-keymap kao-prompt-map (current-local-map))))

(defvar kao--emulation-mode-map-alist
  (list (cons 'kao--normal-active kao-normal-state-map)
        (cons 'kao--insert-active kao-insert-state-map))
  "Keymap alist installed once into `emulation-mode-map-alists'.")

;;;; Per-major-mode normal-state bindings (`kao-define-key')

;; The escape hatch for mode-specific bindings (the evil-define-key idea,
;; sized to kao): each registered major mode owns one normal-state keymap;
;; a kao buffer composes a BUFFER-LOCAL value of
;; `kao--emulation-mode-map-alist' with the maps matching its `major-mode'
;; (most-derived first) prepended to the default alist, so they shadow
;; `kao-normal-state-map'.  Elements of `emulation-mode-map-alists' that
;; are symbols are read in the current buffer, so the local value is all it
;; takes — no keymap rebuild, no extra emulation entry (the evil pattern).
;; Normal state only: the mode maps share the `kao--normal-active' gate,
;; and insert state deliberately stays native.

(defvar kao--mode-keymaps (make-hash-table :test #'eq)
  "Major-mode symbol -> normal-state keymap registered by `kao-define-key'.")

(defun kao--mode-map-entries ()
  "Emulation-alist entries for the current `major-mode', most-derived first.
Collects every registered mode the buffer's mode derives from and pairs its
keymap with the `kao--normal-active' gate; a more derived mode sorts before
its parent so its bindings win."
  (let (modes)
    (maphash (lambda (mode _) (when (derived-mode-p mode) (push mode modes)))
             kao--mode-keymaps)
    (mapcar (lambda (mode)
              (cons 'kao--normal-active (gethash mode kao--mode-keymaps)))
            (sort modes (lambda (a b) (provided-mode-derived-p a b))))))

(defun kao--refresh-mode-maps ()
  "(Re)compose this buffer's local emulation alist from the mode keymaps.
With no matching mode keymap the buffer falls back to the plain default
alist (the local value is removed)."
  (let ((entries (kao--mode-map-entries)))
    (if entries
        (setq-local kao--emulation-mode-map-alist
                    (append entries
                            (default-value 'kao--emulation-mode-map-alist)))
      (kill-local-variable 'kao--emulation-mode-map-alist))))

(defun kao-define-key (mode key def &rest more)
  "Bind KEY to DEF in kao normal state for buffers in major mode MODE.
MODE is a major-mode symbol; buffers in MODE or a mode derived from it get
the binding, shadowing `kao-normal-state-map'.  KEY and DEF are as in
`define-key'; MORE may hold further KEY DEF pairs.  Takes effect
immediately, including in buffers where `kao-mode' is already enabled."
  (let ((map (or (gethash mode kao--mode-keymaps)
                 (puthash mode (make-sparse-keymap) kao--mode-keymaps))))
    (while key
      (define-key map key def)
      (setq key (pop more) def (pop more))))
  ;; A buffer that enabled kao-mode before MODE had a keymap lacks the
  ;; entry (the keymap object is shared, so later bindings into an
  ;; already-composed map are live without this).
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (bound-and-true-p kao-mode) (derived-mode-p mode))
        (kao--refresh-mode-maps)))))

;;;; Terminal ESC decode (`kao-esc-mode')

;; In a `-nw' frame the escape key sends the raw ESC byte, which Emacs reads
;; as the Meta prefix -- the terminal encodes `M-x' as `ESC x'.  A bare ESC
;; therefore never produces the `escape' event that the state maps bind, so
;; insert state cannot be left in a terminal (probed: ESC is swallowed and
;; the next key is meta-ified).  Kakoune reads terminal keys itself and has
;; no such ambiguity, so this is a pure Emacs-mechanism accommodation
;; (native-mechanism family), ported from evil's proven
;; `evil-esc-mode': bind ESC in the terminal-local `input-decode-map' to a
;; `menu-item' :filter that yields `[escape]' when ESC arrived ALONE (no
;; further input within `kao-esc-delay') and defers to the saved decode map
;; otherwise, so `ESC [ A'-style function-key sequences and ESC-as-Meta
;; still decode unchanged.  Only a bare ESC pays the delay; key bursts
;; short-circuit through `sit-for's pending-input check.
;;
;; Native kmacro fixup: while a native macro is
;; being DEFINED (F3/F4, `defining-kbd-macro'), the filter rewrites the
;; macro so it records the decoded `escape' event, not the raw ESC byte
;; (`kao--esc-filter', evil's `defining-kbd-macro' fixup) — else the raw ESC
;; replays fused with the next key as a Meta chord.  kao's OWN Q/q recorder
;; is unaffected: it records POST-decode via `this-command-keys-vector'
;; (kao-macro.el), so a recorded Escape is already the `escape' event.  One
;; shared-with-evil Emacs constraint: during F3-recording `sit-for's
;; `defining-kbd-macro' branch returns immediately, so the `kao-esc-delay'
;; disambiguation window collapses to 0 while recording (a fragmented Meta
;; sequence can mis-decode live) — a native-macro-recording limitation, not
;; a steady-state one.

(defcustom kao-esc-delay 0.01
  "Seconds a terminal ESC waits for a following byte before it is `escape'.
If more input arrives within this delay the ESC keeps its Meta-prefix (or
function-key sequence) meaning; if not, it becomes the `escape' event.
0.01 is evil's long-standing default — large enough for a terminal's key
burst, small enough to feel immediate."
  :type 'number
  :group 'kao)

(defvar kao-inhibit-esc nil
  "When non-nil, `kao--esc-filter' never rewrites a terminal ESC to `escape'.
The `let'/buffer-local escape hatch for terminal extensions that need the raw
ESC byte (or their own decode) in a kao/minibuffer context — bind it around
the read, or set it buffer-local.  The filter is ALSO gated on kao being the
active model (`kao-mode' or an active minibuffer), so a non-kao buffer already
keeps ESC's terminal meaning; this knob suppresses the translation even where
kao would otherwise apply it (hel-study-5).")

(defun kao--esc-filter (map)
  "Decode a lone terminal ESC to [escape]; defer to MAP otherwise.
The :filter of the `menu-item' that `kao--esc-init-terminal' binds on ESC in
`input-decode-map'.  The just-read key is translated to the `escape' event
only when kao is the active model here — `kao-mode' on, or an active minibuffer
\(so a terminal minibuffer read still gets `escape') — and `kao-inhibit-esc' is
nil, AND it ended in a raw ESC with no further input within `kao-esc-delay'
\(`sit-for' returns nil as soon as input is pending, so a terminal
Alt/function-key burst falls through to MAP, the saved decode map).  Outside
kao it always returns MAP, so ESC keeps its terminal meaning (hel-study-5).

When translating while the variable `defining-kbd-macro' is set (a native
kmacro definition, i.e. F3-recording), rewrite the macro so it stores the
DECODED `escape' event rather than the raw ESC byte — evil's fixup verbatim
\(`evil-esc', evil-core.el:626-630): end the macro, append `[escape]' to
`last-kbd-macro', and restart recording in append mode.  Without it the raw
ESC replays fused with the following macro key as a Meta chord
\."
  (if (and (not kao-inhibit-esc)
           (or (bound-and-true-p kao-mode) (active-minibuffer-window))
           (let ((keys (this-single-command-keys)))
             (and (> (length keys) 0)
                  (eq (aref keys (1- (length keys))) ?\e)))
           (sit-for kao-esc-delay))
      (progn
        (when defining-kbd-macro
          (end-kbd-macro)
          (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
          (start-kbd-macro t t))
        [escape])
    map))

(defun kao--esc-init-terminal (&optional frame)
  "Install the ESC decode filter on FRAME's terminal (tty only, once).
Saves the terminal's current ESC decode binding in the terminal parameter
`kao-esc-saved-map' (the sentinel `none' when there was none) — both the
double-install guard and what `kao-esc-mode' restores on disable.
`input-decode-map' is terminal-local, so the filter never affects GUI
frames (their escape key already sends the `escape' event)."
  (with-selected-frame (or frame (selected-frame))
    (let ((term (frame-terminal)))
      (when (and (eq (terminal-live-p term) t) ; t = a tty terminal
                 (not (terminal-parameter term 'kao-esc-saved-map)))
        (let ((saved (lookup-key input-decode-map [?\e])))
          (set-terminal-parameter term 'kao-esc-saved-map (or saved 'none))
          (define-key input-decode-map [?\e]
            `(menu-item "" ,saved :filter ,#'kao--esc-filter)))))))

(define-minor-mode kao-esc-mode
  "Translate a lone terminal ESC to the `escape' event.
Patches the terminal-local `input-decode-map' of every tty terminal (and,
via `after-make-frame-functions', of terminals created later) with the
`kao--esc-filter' disambiguation.  Turned on by `kao-mode' — without it kao
is unusable in a `-nw' frame — and never auto-disabled (other buffers may
still be in kao-mode); toggle it off manually to restore the terminal's
original ESC decoding."
  :global t
  :group 'kao
  (if kao-esc-mode
      (progn
        (add-hook 'after-make-frame-functions #'kao--esc-init-terminal)
        (mapc #'kao--esc-init-terminal (frame-list)))
    (remove-hook 'after-make-frame-functions #'kao--esc-init-terminal)
    (let ((seen '()))
      (dolist (f (frame-list))
        (let* ((term (frame-terminal f))
               (saved (terminal-parameter term 'kao-esc-saved-map)))
          (unless (memq term seen)
            (push term seen)
            (when saved
              (with-selected-frame f
                (define-key input-decode-map [?\e]
                  (if (eq saved 'none) nil saved)))
              (set-terminal-parameter term 'kao-esc-saved-map nil))))))))

(defun kao--clamp-pos (pos)
  "Clamp POS to a valid on-char buffer position.
The last on-char position is (1- (point-max)); an empty buffer clamps to
`point-min'."
  (let ((hi (max (point-min) (1- (point-max))))
        (lo (point-min)))
    (min (max pos lo) hi)))

(defun kao--clamp-sel (sel)
  "Return a copy of SEL with anchor/cursor each clamped on-char (`kao--clamp-pos').
Safe for Replace motions, where anchor == cursor.  Extend motions instead use
`kao--extend-clamp' (the anchor is preserved, never re-clamped).
Captures and the sticky-goal `target' are carried (clamping adjusts
coordinates of the same selection); the positional `kao-sel--new' keeps this
per-keystroke site keyword-free (hot-path principle)."
  (kao-sel--new (kao--clamp-pos (kao-sel-anchor sel))
                (kao--clamp-pos (kao-sel-cursor sel))
                (kao-sel-captures sel)
                (kao-sel-target sel)))

(defun kao--extend-clamp (sel)
  "Return a copy of SEL with the cursor clamped on-char and the anchor preserved.
The clamp revisit (Open Item): `kao--clamp-sel' clamps anchor and cursor
*independently*, which can shrink a selection whose anchor sits at buffer end
once the cursor moves.  In Extend mode only the cursor is new — the motion
already produced it on-char — and the anchor was valid when the selection was
built, so it is kept verbatim.  Faithful to `move_cursor'/`select<Extend>',
where the anchor is never re-derived (normal.cc:2301, :111).
Captures and the sticky-goal `target' are carried (clamping adjusts
coordinates of the same selection); positional constructor as in
`kao--clamp-sel' (hot-path principle)."
  (kao-sel--new (kao-sel-anchor sel)
                (kao--clamp-pos (kao-sel-cursor sel))
                (kao-sel-captures sel)
                (kao-sel-target sel)))

(defun kao--snapshot-update (snap id)
  "Return a fresh `kao-sels' from SNAP, coordinate-translated from node ID.
The faithful flat `SelectionList::update(merge=true)' (selection.cc:269)
for kao's snapshot stores: ID is the buffer's `kao-history-current-id'
recorded when SNAP was taken (= the saved SelectionList's timestamp), either a
plain integer (legacy / cross-track producers) or a `(GEN . HIST-ID)' cons
carrying the tree generation.  A `(GEN . HIST-ID)' tag
whose GEN differs from the current tree's is treated exactly like a gc'd id (its
tree was replaced by a `kao-history-init', so the id can alias an unrelated
node) — no translation, clamp+merge residual.

ID = current id: the buffer content is the one SNAP was taken against —
plain clamped copy, NO merge (`update_selections' early-returns at the
current timestamp, selection.cc:233-236; an unmerged post-combine
overlapping list survives a same-state round-trip, the slice-18 producer).

ID names a live tree node: every anchor/cursor folds through the tree path
\(`kao-history-translate-position'), is clamped, then the list is
sort-and-merge-overlapping'd — Kakoune merges exactly when it translates
\(main follows the survivor by identity).

ID nil or gc'd from the tree (`kao-history-max-nodes'): translation is
unavailable — clamped copy + merge, the documented best-effort residual
\(Kakoune's history is uncapped, so it always translates; the merge keeps
the slice-18 invariant that a changed-buffer restore never installs an
overlapping list Kakoune would have merged)."
  (let* ((main (kao-sels-main snap))
         (list (kao-sels-list snap))
         ;; A tag id is either a plain integer (legacy / cross-track
         ;; producers — the z/Z selection registers) or (GEN . HIST-ID) carrying
         ;; the tree generation.  A generation mismatch means the tag was
         ;; recorded against a tree `kao-history-init' has since replaced
         ;; (`kao-mode' toggle, plain `revert-buffer', `find-alternate-file');
         ;; ids restarted at 0, so this id can alias an unrelated node — even
         ;; the same-id fast path below.  Drop to nil so the tag takes the
         ;; clamp+merge residual, exactly like a gc'd id.
         (hid (if (consp id)
                  (and (eql (kao--genid-gen id) (kao-history-generation))
                       (kao--genid-id id))
                id)))
    (if (and (integerp hid) (= hid (kao-history-current-id)))
        (kao-sels-make :list (mapcar #'kao--clamp-sel list) :main main)
      (let* ((live (and (integerp hid) (kao--hist-node hid)))
             (mapped
              (mapcar
               (lambda (s)
                 (kao--clamp-sel
                  (if live
                      (kao-sel--new
                       (kao-history-translate-position (kao-sel-anchor s) hid)
                       (kao-history-translate-position (kao-sel-cursor s) hid)
                       (copy-sequence (kao-sel-captures s))
                       (kao-sel-target s))
                    s)))
               list)))
        (kao-sels-sort-and-merge-overlapping
         (kao-sels-make :list mapped :main main))))))

(defun kao--reveal-overlay-opener (pos)
  "Open an `isearch-open-invisible' overlay fold covering POS.
Default `kao-reveal-functions' entry.  Outline, hideshow, and `reveal-mode'
folds declare an opener on their invisibility overlay; call it so the fold at
the main cursor surfaces (the isearch precedent)."
  (dolist (ov (overlays-at pos))
    (when (overlay-get ov 'invisible)
      (let ((opener (overlay-get ov 'isearch-open-invisible)))
        (when opener
          (funcall opener ov))))))

(defun kao--reveal-org-fold (pos)
  "Reveal an `org-fold' text-property fold at POS.
Default `kao-reveal-functions' entry.  org-fold folds carry no overlay, so the
overlay opener cannot reach them; `org-fold-show-context' surfaces the context
around POS in an `org-mode' buffer."
  (when (and (derived-mode-p 'org-mode) (fboundp 'org-fold-show-context))
    (save-excursion
      (goto-char pos)
      (org-fold-show-context 'isearch))))

(defvar kao-reveal-functions
  (list #'kao--reveal-overlay-opener #'kao--reveal-org-fold)
  "Abnormal hook run to reveal an invisibility fold at the main cursor.
Each function is called with one argument, the main cursor position, whenever
that position is `invisible-p' after a motion, search, or object lands there.
The default entries open an `isearch-open-invisible' overlay fold and an
`org-fold' text-property fold; register your own opener here to surface other
text-property invisibility the defaults cannot reach (substrate seam).
Only the MAIN cursor is revealed; secondaries stay folded, matching isearch.")

(defun kao--reveal-fold-at-main ()
  "Reveal an invisibility fold at the main selection's cursor (point).
Selections operate on buffer text regardless of visibility (A-22c), but a main
cursor that lands inside a fold — via search, f/t, or an object — should surface
it so the user sees where they are: the isearch/evil precedent.  Only the MAIN
selection is revealed (point is already there after `kao--mirror-main');
secondaries stay folded, matching isearch.  Cheap by design: the hook runs only
when point is actually `invisible-p', which is rare.  The openers live on the
`kao-reveal-functions' abnormal hook, run with the main cursor position."
  (when (invisible-p (point))
    (run-hook-with-args 'kao-reveal-functions (point))))

(defvar-local kao--refresh-key nil
  "Guard cons `(TICK . KAO--SELS)' recorded at the last render.
TICK is the `buffer-chars-modified-tick'.  `kao--refresh' fires TWICE per
keystroke — mid-command from every selection installer (motions, edits, the
register / menu commands) and again from the `post-command-hook' entry — over
the SAME frame.  Every installer rebuilds `kao--sels' as a
fresh struct, so its `eq'-identity changes whenever the selection does, and
every buffer edit advances the tick; an unchanged pair means the second call
would render an identical frame, so it is elided.  Buffer-local (both keys are
per-buffer).  A changed pair always re-renders, so the guard is
behavior-preserving and removes only redundant work.")

(defun kao--refresh ()
  "Mirror the main selection and redraw secondaries from `kao--sels'.
A no-op while insert state is active — the selection is suspended and point is
the live editing cursor.  Deduped per keystroke: the mid-command call and the
`post-command-hook' call share the `(buffer-chars-modified-tick . kao--sels)'
key in `kao--refresh-key', so the second collapses to nothing when neither the
buffer nor the selection object changed since the first — one keystroke walks,
mirrors, and reveals ONCE, not twice (behavior-preserving)."
  (when (and kao--sels (not kao--insert-active))
    (let ((tick (buffer-chars-modified-tick)))
      (unless (and kao--refresh-key
                   (eq (cdr kao--refresh-key) kao--sels)
                   (eql (car kao--refresh-key) tick))
        (kao--mirror-main kao--sels)
        (kao--reveal-fold-at-main)
        (kao--render kao--sels)
        (setq kao--refresh-key (cons tick kao--sels))))))

(defvar kao--scroll-rendering nil
  "Reentrancy guard for `kao--render-on-scroll' (dynamically bound).")

(defun kao--render-on-scroll (win start)
  "Redraw secondaries over WIN's post-scroll viewport.
The `window-scroll-functions' hook.  kao's per-keystroke render fires from
`post-command-hook', BEFORE redisplay decides the new `window-start'; when a
keystroke then moves point off-screen and redisplay recenters, the freshly
revealed region shows no secondary overlays until the next command renders
again.  Kakoune never has this gap: it highlights every displayed frame over
the FINAL post-scroll viewport (`compute_display_setup' scrolls `first_line'
to the cursor, then `highlight_selections' draws over that same frame,
window.cc:142-162,230-247).

`window-scroll-functions' runs during redisplay exactly when WIN's
`window-start' is about to change, with the new start as START (the
nlinum/linum precedent, and the hook this render path always
intended).  We re-run `kao--render' over WIN's new viewport — START for the
top, `(window-end WIN t)' forcing the fresh post-scroll bottom (the documented
idiom for `window-end' inside this hook).  Only when WIN shows THIS buffer in
normal state (`kao--normal-active', not `kao--insert-active', where the
selection is suspended).  Reentrancy-guarded (`kao--scroll-rendering') and
O(visible): `kao--render' is idempotent and pool-reusing, so the extra call is
cheap.

Single-range refresh: a buffer shown in two windows where only WIN scrolled has
its OTHER window's secondaries restored by the next `kao--refresh' (any command)
— an accepted transient, since a scroll of one window does not move the other."
  (when (and (not kao--scroll-rendering)
             kao--sels kao--normal-active (not kao--insert-active)
             (eq (window-buffer win) (current-buffer)))
    (let ((kao--scroll-rendering t))
      (kao--render kao--sels start
                   (min (point-max) (or (window-end win t) (point-max)))))))

(defun kao--sels-sync ()
  "Post-command hook: fold the live list through a FOREIGN non-command edit.
kao keeps selections as integers at rest; a buffer edit made OUTSIDE a kao
command — a timer, a process filter, an async formatter — shifts the buffer and
advances the history tree (once the pending mods commit, ahead of this hook in
the post-command chain) but never reconciles `kao--sels', so the next keystroke
would snap the cursor to stale offsets.  When the tag `kao--sels-id' has fallen
behind `kao-history-current-id', translate every selection through the tree
exactly as snapshot restore does (`kao--snapshot-update') — the elisp analogue
of Kakoune's `Context::selections()' calling `SelectionList::update()' before
every access (context.cc:126/150/195) — then re-pin the tag.

Deliberately a SEPARATE hook, not part of `kao--refresh': `kao--refresh' is
called mid-command by every selection installer (motions, edits, the register /
menu commands in other modules), which set `kao--sels' already in the current
frame; folding there would double-translate them.  This hook runs only on the
real command boundary, after `kao--hist-maybe-commit' has committed the foreign
edit and re-tagged kao's OWN edits (`kao--sels-edit-pending').  Skipped while
kao's own history navigation replays (`kao--hist-navigating').  Zero cost on the
hot path: an integer `/=' compare, translating only after a real foreign edit
\."
  (when (and kao--sels kao--normal-active (not kao--insert-active)
             kao--sels-id
             (not kao--hist-navigating)
             (not kao--sels-edit-pending)
             (/= kao--sels-id (kao-history-current-id)))
    ;; direct install (retag-order): history translate + id retag in one setq,
    ;; no refresh.  Sets `kao--sels' FIRST, so this is NOT the `kao--sels-retag'
    ;; id-first idiom: the tag must move only in lockstep with the translated
    ;; list here.
    (setq kao--sels (kao--snapshot-update kao--sels kao--sels-id)
          kao--sels-id (kao-history-current-id))))

(defun kao--sels-retag ()
  "Re-pin the live selection-list tag `kao--sels-id' to the current frame.
Call after installing selections that ALREADY sit in the current history frame
\(a translated snapshot, a post-edit rebuild, a jump/undo restore) so the next
`kao--refresh' / `kao--sels-sync' does not translate them a SECOND time through
an edit they already reflect.  Not needed on the normal path,
where selections are built against the live buffer and the tag is already
current.  The `kao--sels-sync' updater above is the one intentional exception:
it retags in lockstep with the list it translates, so it stays direct."
  (setq kao--sels-id (kao-history-current-id)))

(defun kao--sync-main-to-point (&optional merge)
  "Collapse the main selection onto the current point, preserving secondaries.
A native scroll keeps point visible by carrying it to the window edge; this
re-asserts kao's invariant (point == main cursor) so the post-command mirror
will not snap the view back.  The wheel/scroll branch of
`kao--foreign-sync' calls this ONLY when point was actually carried (point no
longer on the main cursor): a tick that left point ON the main cursor preserves
the whole selection verbatim (Kakoune `PreserveSelections').
`kao--scroll-step' (the `<c-d>'/`<c-u>' family,
`MoveCursorAndAnchor') calls it UNCONDITIONALLY — Kakoune collapses there too
\(input_handler.cc:1831).  The list is re-sorted (main tracked by
identity) to keep the min-sorted invariant.  With MERGE non-nil also
sort-and-merge overlapping selections, faithful to `scroll_window's
`sort_and_merge_overlapping' (input_handler.cc:1833) which is reached under
`MoveCursorAndAnchor' (the `<c-d>'/`<c-u>'/`<c-f>'/`<c-b>' path); without it
secondaries are only sorted (the `vj'/`vk' `PreserveSelections' path never
merges).  Lifted from kao-menu for the mouse stance (the wheel-scroll
sync needs it here); `kao-menu--sync-main-to-point' aliases it."
  (let* ((list (copy-sequence (kao-sels-list kao--sels)))
         (main (kao-sels-main kao--sels))
         (p (point)))
    (setf (nth main list) (kao--clamp-sel (kao-sel-make :anchor p :cursor p)))
    (let ((rebuilt (kao-sels-make :list list :main main)))
      ;; direct install (no-clamp): re-clamps only the moved main (secondaries
      ;; stay as-is) and, when MERGE is nil, sorts WITHOUT merging — a shape
      ;; neither seam offers (kao-set-selections always merges, -raw never sorts).
      (setq kao--sels (if merge
                          (kao-sels-sort-and-merge-overlapping rebuilt)
                        (kao-sels-sort rebuilt)))
      (kao--refresh))))

;;;; Foreign-command reconciliation (mouse stance, generalized)

(defcustom kao-mouse-click-commands '(mouse-set-point mouse-drag-region)
  "Native click commands after which kao re-derives the selection from point.
The post-command `kao--foreign-sync' replaces the whole selection list with
a single selection at the clicked point — Kakoune's no-Ctrl left press
\(`MouseHandler', input_handler.cc:143-154: `selections_write_only() =
{buffer, anchor}') — translated to the native idiom: the Emacs mouse
command runs untouched and kao reconciles afterwards.  Membership here
\(vs the catch-all) matters twice: a listed click collapses even when
point did not move (the catch-all's moved-point gate would skip it), and a
pending count/register SURVIVES these commands
\(`kao--maybe-reset-count', input_handler.cc:297-303)."
  :type '(repeat function)
  :group 'kao)

(defcustom kao-mouse-scroll-commands '(mwheel-scroll pixel-scroll-precision
                                       scroll-up-command scroll-down-command)
  "Native scroll commands after which the main selection follows point.
Kakoune's wheel only scrolls the viewport (`scroll_window',
input_handler.cc:177-185), but Emacs cannot detach the viewport from point
\, so scrolling gets the detached-scroll treatment: WHEN Emacs carried
point to the window edge (point no longer on the main cursor),
`kao--sync-main-to-point' collapses the main onto the carried point,
PRESERVING secondaries — the same approximation as the `v j'/`v k' scroll
family.  A tick that did NOT move point leaves the whole selection verbatim
\(Kakoune `PreserveSelections' skips the selection-mutation block entirely,
input_handler.cc:196/1802-1803/1815), so a live
multi-char main survives a wheel/trackpad scroll.  The keyboard scrolls
`scroll-up-command'/`scroll-down-command'
\(the native paging keys, untouched by kao's maps) are members for the same
reason: without the listing the catch-all would treat the carried
point as a jump and drop the secondaries.  Documented side effect: list
membership also grants the mouse params exemption
\(`kao--maybe-reset-count'), so a pending count/register survives the
keyboard scrolls too — an approximation accepted (Kakoune's own
page keys reset params, but kao's faithful page bindings are its own
scroll commands, which do)."
  :type '(repeat function)
  :group 'kao)

(defcustom kao-region-mark-commands '(er/expand-region
                                      er/contract-region
                                      mark-whole-buffer
                                      mark-defun
                                      mark-paragraph
                                      mark-sexp
                                      mark-word
                                      mark-page
                                      mark-end-of-sentence)
  "Foreign commands whose resulting region becomes the kao selection.
A command listed here that ends with an active mark→point span gets that
span ADOPTED as the one selection — the exclusive region end steps
back onto the span's last char (inclusive coords), direction
preserved.  Unlisted foreign commands keep the click collapse even
when the region is active, because kao's own mirror holds the region
active for every multi-char selection — adopting on bare region-activity
would silently turn each foreign jump (imenu, avy, xref...) into an
extend.  Add your own region-marking commands (or wrappers around them)
here; a wrapper that `setq's `this-command' to the listed command matches
too."
  :type '(repeat function)
  :group 'kao)

(defun kao--foreign-sync ()
  "Post-command hook: reconcile a foreign command's point with `kao--sels'.
Three branches, checked in order (generalized):
a click command (`kao-mouse-click-commands') replaces the selection list
with ONE selection at point — Kakoune's no-Ctrl left press; a scroll
command (`kao-mouse-scroll-commands') applies the sync (main follows
point, secondaries preserved); ANY OTHER foreign command — one whose
`this-command' is not a `kao-'-prefixed symbol — that ended with point off
the main cursor moved point authoritatively (imenu, avy, xref, isearch, a
`kao-define-key' user binding...), and gets the click collapse: without it
the post-command mirror would snap point back and the command would appear
dead (the earlier behavior).  Ahead of the catch-all sits the
region-adoption branch: a LISTED command (`kao-region-mark-commands' —
expand-region, the mark-* family) that left a real mark→point span gets
that span ADOPTED as the one selection rather than flattened.  kao's own
commands are EXEMPT by the prefix test, which must come first: they are
pure list transforms, so at sync
time point is always stale relative to the new main cursor — a moved-point
gate alone would collapse every kao motion.  A non-symbol `this-command'
\(a lambda bound via `kao-define-key') counts as foreign.  Added LAST in
the `kao-mode' enable sequence so it runs FIRST (buffer-local post-command
hooks run most-recently-added first) — before the selection-history
recorder, so the reconciled list records its node immediately, matching
the `ScopedSelectionEdition' Kakoune holds across press/release
\(input_handler.cc:145).  No-op in insert state (the selection is
suspended; Kakoune's insert-mode mouse routing, input_handler.cc:1236, is
deferred).  Drag-extend, right-click extend and Ctrl-click
multi-cursor are likewise deferred: a native drag currently acts as a
click at the release point."
  (when (and kao--sels kao--normal-active (not kao--insert-active))
    ;; direct install (no-refresh): each kao-sels-make branch below sets
    ;; kao--sels AND kao--sels-edit-pending in one setq and does NOT refresh
    ;; (post-command reconcile) — not a kao-set-selections seam site.
    (cond
     ((memq this-command kao-mouse-click-commands)
      (let ((p (point)))
        (setq kao--sels (kao-sels-make
                         :list (list (kao--clamp-sel (kao-sel--new p p)))
                         :main 0)
              ;; Reconciled to the CURRENT frame: if the foreign command also
              ;; edited, its commit re-tags this list, never translates it.
              kao--sels-edit-pending t)))
     ((memq this-command kao-mouse-scroll-commands)
      ;; Only reconcile when Emacs actually CARRIED point to keep the cursor
      ;; visible: a scroll tick that left point ON the main cursor is Kakoune's
      ;; `PreserveSelections' (the wheel dispatches
      ;; `scroll_window(..., PreserveSelections)', input_handler.cc:196, which
      ;; sets `ensure_cursor_visible=false' and SKIPS the whole
      ;; selection-mutation block, :1802-1803/:1815) — the selection list is
      ;; left verbatim.  Without the gate every wheel/trackpad tick collapses a
      ;; live multi-char main onto its own cursor, destroying the extent
      ;; of the selection.  kao only reconciles a CARRIED point.
      (when (/= (point) (kao-sel-cursor (kao--main-sel)))
        (kao--sync-main-to-point)))
     ((and (memq this-command kao-region-mark-commands)
           mark-active (mark t) (/= (mark t) (point)))
      ;; a LISTED region-marking command (expand-region, the mark-*
      ;; family...) marked a SPAN — adopt it as the one selection instead
      ;; of collapsing.  Emacs's region is half-open with one end
      ;; exclusive; kao selections are inclusive with the cursor ON a char
      ;;, so the exclusive end steps back one onto the span's
      ;; last char, direction preserved.  Membership (not a bare
      ;; `region-active-p') is the gate because kao's OWN mirror keeps the
      ;; region active for every multi-char selection — region-activity
      ;; alone would turn each foreign jump into an extend, silently
      ;; repealing the collapse.  `mark-active' rather than
      ;; `region-active-p': the mirror manages the mark directly and
      ;; `transient-mark-mode' is nil in batch.
      (let* ((p (point)) (m (mark t)))
        (setq kao--sels
              (kao-sels-make
               :list (list (kao--clamp-sel
                            (if (> p m)
                                (kao-sel-make :anchor m :cursor (1- p))
                              (kao-sel-make :anchor (1- m) :cursor p))))
               :main 0)
              kao--sels-edit-pending t)))
     ((and (not (and (symbolp this-command)
                     (string-prefix-p "kao-" (symbol-name this-command))))
           (/= (point) (kao-sel-cursor (kao--main-sel))))
      (let ((p (point)))
        (setq kao--sels (kao-sels-make
                         :list (list (kao--clamp-sel (kao-sel--new p p)))
                         :main 0)
              kao--sels-edit-pending t))))))

(defun kao--main-sel ()
  "Return the main selection of `kao--sels'."
  (nth (kao-sels-main kao--sels) (kao-sels-list kao--sels)))

(defun kao--carry-captures (res src)
  "Return RES with the `select()' capture rule applied against SRC.
Faithful to normal.cc:90-91/118-119: a result carrying its own captures
keeps them; a captureless result inherits SRC's — so motions and objects
preserve the captures a previous `s'/search filled in.  RES is mutated in
place (callers pass a freshly built selection)."
  (unless (kao-sel-captures res)
    (setf (kao-sel-captures res) (kao-sel-captures src)))
  res)

(defun kao--map-selections (fn)
  "Replace every selection with (FN sel), then clamp, sort+merge, and refresh.
Sort-and-merge-overlapping mirrors Kakoune `select' (normal.cc:131), which ends
every motion/selector by collapsing overlapping selections; the main index
follows the survivor by identity.  (Count-repeats still merge once at the
end of `kao--repeat' rather than per iteration — a P2-noted approximation.)
Captures follow the `select()' rule (`kao--carry-captures')."
  (let* ((sels kao--sels)
         (mapped (mapcar (lambda (s)
                           (kao--carry-captures
                            (kao--clamp-sel (funcall fn s)) s))
                         (kao-sels-list sels))))
    ;; direct install (perf): clamps + carries-captures per sel above, then
    ;; sort-merges and refreshes — kao-set-selections would match but re-clamps
    ;; the whole (already-clamped) list a second time on this hot select() path.
    (setq kao--sels
          (kao-sels-sort-and-merge-overlapping
           (kao-sels-make :list mapped :main (kao-sels-main sels))))
    (kao--refresh)))

(defun kao--map-filter-selections (fn &optional extend)
  "Replace (or, with EXTEND, merge) each selection with (FN sel), dropping nils.
Unlike `kao--map-selections' (1:1), this is the object/selector dispatch,
faithful to Kakoune `select' (normal.cc:81-127): a nil result drops that
selection and adjusts the main index in place (decrement when
\(and (<= i main) (/= main 0))); when every result is nil the list is left
unchanged (Kakoune throws `no_selections_remaining').  With EXTEND non-nil each
non-nil result is merged onto its selection via `kao-sel-extend-to' (anchor
kept, cursor clamped via `kao--extend-clamp') — Kakoune `select' in Extend mode
\(normal.cc:111); otherwise it replaces the selection (Replace mode).  The
nullopt drop precedes the Extend/Replace split in Kakoune, so it is identical
for both.  Survivors are clamped, then sort-and-merge-overlapping (Kakoune
`select' ends with this, normal.cc:131; main follows the survivor by identity),
and refreshed."
  (let* ((main (kao-sels-main kao--sels))
         (new-main main)
         (survivors '())
         (i 0))
    (dolist (s (kao-sels-list kao--sels))
      (let ((res (funcall fn s)))
        (if (null res)
            (when (and (<= i new-main) (/= new-main 0))
              (setq new-main (1- new-main)))
          (push (if extend
                    ;; merge keeps S's captures (kao-sel-extend-to carries
                    ;; them); a result with its own captures wins, the
                    ;; post-merge `select()' rule (normal.cc:118-119).
                    (let ((m (kao--extend-clamp (kao-sel-extend-to s res))))
                      (when (kao-sel-captures res)
                        (setf (kao-sel-captures m) (kao-sel-captures res)))
                      m)
                  (kao--carry-captures (kao--clamp-sel res) s))
                survivors)))
      (setq i (1+ i)))
    (setq survivors (nreverse survivors))
    (if (null survivors)
        (kao--refresh)                  ; faithful no_selections_remaining: unchanged
      (let ((m (min (max new-main 0) (1- (length survivors)))))
        ;; direct install (perf): clamps + carries-captures per survivor above,
        ;; then sort-merges and refreshes — kao-set-selections would match but
        ;; re-clamps the whole (already-clamped) list again on this hot path.
        (setq kao--sels
              (kao-sels-sort-and-merge-overlapping
               (kao-sels-make :list survivors :main m)))
        (kao--refresh)))))

(defun kao--map-selections-extend (motion-fn)
  "Extend every selection toward MOTION-FN's cursor (Kakoune `select<Extend>').
Per selection, fold `kao--repeat-count' times:
  sel = merge_selections(sel, (MOTION-FN sel))   ; `kao-sel-extend-to'
keeping the anchor and clamping only the new cursor (`kao--extend-clamp', the
clamp revisit).  Each step merges (NOT merge-once-after-N-raw-motions), so the
anchor is pulled to the running min/max as Kakoune accumulates it.  This matches
`repeated<select<Extend, motion>>' for the word extends (W/B/E) and reproduces
the same end position for the char/line extends (H/J/K/L, whose Kakoune binding
is `move_cursor<...,Extend>' — a single count-folded offset).  Finally
sort-and-merge-overlapping (normal.cc:131; main follows the survivor by
identity) and refresh.

The same mapping serves the char/line extends (H/J/K/L — MOTION-FN returns a
zero-width Replace result, so the merge keeps the anchor: `move_cursor<Extend>')
and the word extends (W/B/E — MOTION-FN returns a region: `select<Extend>').

A selection whose MOTION-FN returns nil mid-fold drops (the
nullopt path): W/B/E at a buffer edge drop like w/b/e (w/W symmetry), the main
index is adjusted in place, and an all-dropped pass leaves the list unchanged
\(`no_selections_remaining').  H/J/K/L never produce nil, so their fold is
unaffected."
  (let* ((n (kao--repeat-count))
         (main (kao-sels-main kao--sels))
         (new-main main)
         (survivors '())
         (i 0))
    (dolist (s (kao-sels-list kao--sels))
      (let ((cur s) (dropped nil) (k 0))
        (while (and (not dropped) (< k n))
          (let ((nxt (funcall motion-fn cur)))
            (if (null nxt)
                (setq dropped t)          ; mid-fold nullopt: this selection drops
              (setq cur (kao--extend-clamp (kao-sel-extend-to cur nxt)))
              ;; per-step `select()' rule: a motion result with its own
              ;; captures wins (normal.cc:118-119).
              (when (kao-sel-captures nxt)
                (setf (kao-sel-captures cur) (kao-sel-captures nxt)))))
          (setq k (1+ k)))
        (if dropped
            (when (and (<= i new-main) (/= new-main 0))
              (setq new-main (1- new-main)))
          (push cur survivors)))
      (setq i (1+ i)))
    (setq survivors (nreverse survivors))
    (if (null survivors)
        (kao--refresh)                    ; faithful no_selections_remaining
      (let ((m (min (max new-main 0) (1- (length survivors)))))
        ;; direct install (perf): clamps + carries-captures per survivor above,
        ;; then sort-merges and refreshes — kao-set-selections would match but
        ;; re-clamps the whole (already-clamped) list again on this hot path.
        (setq kao--sels
              (kao-sels-sort-and-merge-overlapping
               (kao-sels-make :list survivors :main m)))
        (kao--refresh)))))

(defun kao--map-selections-in-place (fn)
  "Replace every selection with (FN sel), preserving order and main; refresh.
Unlike `kao--map-selections', this does NOT sort or merge — faithful to the
direct in-place selection mutations `clear_selections'/`flip_selections'/
`ensure_forward' (normal.cc:2346-2375), which bypass `select()' and therefore
its terminating sort-and-merge.  flip/ensure-forward leave each selection's
min/max unchanged (so the sorted invariant holds), and reduce-to-cursor keeps a
non-overlapping list ordered, so no re-sort is needed."
  (let ((sels kao--sels))
    ;; direct install (no-clamp): in-place map primitive
    ;; (flip/ensure-forward/reduce) deliberately does NO clamp or sort —
    ;; kao-set-selections-raw would add a clamp.
    (setq kao--sels
          (kao-sels-make :list (mapcar fn (kao-sels-list sels))
                         :main (kao-sels-main sels)))
    (kao--refresh)))

;;;; Regex case-fold policy (regex boundary)

;; The single home of kao's search case-sensitivity, shared by every
;; Kakoune-search regex leaf (`kao-search.el', `kao-multi.el', `kao-object.el').
;; It lives HERE, not in kao-search.el, because kao-multi/kao-object load BEFORE
;; kao-search (Makefile SRC order) yet all three need it — kao-state is the
;; earliest module the three share.  Kakoune search is case-sensitive with no
;; smartcase (normal.cc:1021); only an inline `(?i)'/`(?I)' toggles it
;; (regex_vm.cc:209-210).

(defcustom kao-search-case-fold nil
  "Case-fold policy for kao's Kakoune-search regex engine (boundary).
The faithful default is nil: search is case-SENSITIVE (Kakoune has no
smartcase), and only an inline leading `(?i)' folds.  Opt-in DEVIATIONS: t
folds always; `smart' folds unless the pattern contains an uppercase letter.
An inline leading `(?i)'/`(?I)' always overrides this value (Kakoune `(?i)' is
on, `(?I)' is off, regex_vm.cc:209-210)."
  :type '(choice (const :tag "Case-sensitive — faithful (nil)" nil)
                 (const :tag "Always fold (opt-in deviation)" t)
                 (const :tag "Smartcase (opt-in deviation)" smart))
  :group 'kao)

(defun kao--regex-case-fold (pattern)
  "Return (STRIPPED-PATTERN . FOLD) for PATTERN under `kao-search-case-fold'.
Emacs regexp has no inline case flag, so every Kakoune `(?i)'/`(?I)' token is
removed from PATTERN (else Emacs would match a mid-pattern token as the literal
text `(?i)') and its effect applied globally as FOLD — the value to bind
`case-fold-search' to for the scan.  A LEADING `(?i)' yields FOLD t, a leading
`(?I)' yields nil (the last of several consecutive leading flags wins, Kakoune's
left-to-right toggle); with no leading flag FOLD is `kao-search-case-fold' (nil
case-sensitive, t always, `smart' = fold unless the stripped pattern has an
uppercase letter).  Only a LEADING flag changes FOLD: mid-pattern `(?i)'/`(?I)'
scoping is an irreducible Emacs limitation (global case-fold only), documented
and NOT faked.  Only the bare case tokens `(?i)'/`(?I)' are handled; a COMBINED
flag form like `(?is)' (Kakoune also toggles `s' = dot-matches-newline,
regex_vm.cc:208-211) is out of this case-only scope and is left in the pattern.
Idempotent on an already-stripped pattern.  Kakoune `(?i)'/`(?I)'
regex_vm.cc:209-210; no smartcase in search, normal.cc:1021."
  (let* ((leading (and (string-match "\\`\\(?:(\\?[iI])\\)+" pattern)
                       (match-string 0 pattern)))
         (stripped (replace-regexp-in-string "(\\?[iI])" "" pattern t t))
         (fold (cond
                (leading (eq (aref leading (- (length leading) 2)) ?i))
                ((eq kao-search-case-fold t) t)
                ((eq kao-search-case-fold 'smart)
                 (let ((case-fold-search nil))
                   (not (string-match-p "[[:upper:]]" stripped))))
                (t nil))))
    (cons stripped fold)))

;;;; Public selection-list API (the config substrate, Layer 0)

;; The documented elisp surface over the selection engine: a config layer
;; (surround, match-mode, increment, ...) reads and transforms the selection
;; list through these `kao-' entries instead of reaching the private `kao--'
;; internals or `setq'ing `kao--sels'.  Layer 0 names
;; `kao-map-selections'/`kao-set-selections' as this surface; these are them.

(defun kao-get-selections ()
  "Return the current selection list as a list of fresh `kao-sel' copies.
Each element is a copy, so a caller mutating a returned selection cannot
corrupt engine state.  The read side of Kakoune `SelectionList'
\(selection.cc); the list is min-sorted and the main selection's cursor is the
one mirrored to point."
  (mapcar #'kao-sel-copy (kao-sels-list kao--sels)))

;; THE `kao--sels' install seam (R9/R10).  `kao-set-selections'
;; (sort+merge) and `kao-set-selections-raw' (verbatim) share the
;; `kao--install-sels' core: clamp each sel on-char, collapse-empty-to-point,
;; clamp the main into range, install, refresh.  Route a direct
;; `(setq kao--sels ...)' through a seam ONLY when the surrounding code already
;; does exactly that (clamp + refresh, no behavior delta).  Every other direct
;; writer keeps the annotation grammar
;;   ;; direct install (REASON): <why the seam does not fit>
;; where REASON is one of:
;;   no-clamp    — must NOT apply the seam's per-sel clamp (or clamps only part)
;;   no-merge    — installs a sorted-but-unmerged shape; the seam's
;;                 `sort_and_merge_overlapping' would collapse intended distinct sels
;;   no-refresh  — must NOT refresh (feeds `kao--multi-edit', a post-command
;;                 reconcile, a deferred seed, or a mid-insert geometry sync)
;;   retag-order — an id re-pin / `kao--sels-edit-pending' set must precede the
;;                 refresh, which the seam's internal refresh would break
;;   perf        — already clamps inline on a hot path; the seam would re-clamp
;;                 the whole list a second time for no behavior gain
;; Two direct `(setq kao--sels ...)' are NOT install sites and carry no grammar:
;; this core itself (it IS the seam) and the mode-disable teardown-to-nil.
;; The remaining cross-file direct sites (kao-multi/kao-search/kao-menu) are
;; re-parked in ## Open — owns only kao-state/kao-edit here.

(defun kao--install-sels (sels main normalize)
  "Shared core under the two `kao-set-selections' install seams.
Clamp each of SELS on-char (an empty SELS collapses to a single selection at
point — Kakoune never holds zero selections), clamp MAIN into range (tracked by
identity through any merge), install the result as `kao--sels', and
refresh.  When NORMALIZE is non-nil the list is sorted and overlapping members
merged (Kakoune `sort_and_merge_overlapping', selection.cc); when nil it is
installed verbatim, preserving the given order and any intentional overlap.
The single canonical `kao--sels' writer behind `kao-set-selections' (normalize)
and `kao-set-selections-raw' (verbatim)."
  (let* ((list (if sels
                   (mapcar #'kao--clamp-sel sels)
                 (list (kao--clamp-sel (kao-sel-make :anchor (point)
                                                     :cursor (point))))))
         (m (min (max (or main 0) 0) (1- (length list))))
         (new (kao-sels-make :list list :main m)))
    (setq kao--sels (if normalize
                        (kao-sels-sort-and-merge-overlapping new)
                      new))
    (kao--refresh)))

(defun kao-set-selections (sels &optional main)
  "Install SELS (a list of `kao-sel') as the selection list; MAIN is its index.
Each selection is clamped on-char, the list is sorted and overlapping members
merged (Kakoune `sort_and_merge_overlapping', selection.cc), MAIN is clamped
into range (tracked by identity through the merge), and the buffer is
refreshed.  An empty SELS collapses to a single selection at point — Kakoune
never holds zero selections.  Use `kao-set-selections-raw' to install an
intentionally overlapping or unsorted list verbatim."
  ;; THE install seam (sort+merge form) — the canonical kao--sels writer, not a
  ;; bypass site.
  (kao--install-sels sels main t))

(defun kao-set-selections-raw (sels &optional main)
  "Install SELS verbatim (clamped, NOT sorted or merged); MAIN is its index.
Like `kao-set-selections' but skips `sort_and_merge_overlapping', preserving
the given order and any intentional overlap — the install side of an unmerged
combine and of the `-draft' restore (`kao-with-saved-selections').  An empty
SELS collapses to a single selection at point."
  ;; THE install seam (verbatim form) — the canonical kao--sels writer, not a
  ;; bypass site.
  (kao--install-sels sels main nil))

(defun kao-map-selections (fn)
  "Replace each selection with (funcall FN sel); a nil result DROPS that one.
The general selection-list transform (the engine behind every motion and
selector), routed through the drop-capable filter dispatch: FN takes a
`kao-sel' and returns a `kao-sel' or nil.  When FN drops every selection the
list is left unchanged — a silent no-op (Kakoune throws
`no_selections_remaining'; kao keeps the prior list).  Survivors are clamped,
then sorted and overlapping merged, the main index following the survivor by
identity.  Kakoune `select' (normal.cc:81-131)."
  (kao--map-filter-selections fn))

(defun kao-map-selections-extend (fn)
  "Extend each selection toward (funcall FN sel)'s cursor; nil DROPS that one.
The Extend-mode form of `kao-map-selections': each non-nil result is merged
onto its selection (anchor kept, cursor moved — `merge_selections',
normal.cc:53) rather than replacing it, the shift-motion path.  Routes through
the same drop-capable filter dispatch (one application per selection, no
implicit repeat-count fold), then sorts and merges.  Kakoune `select<Extend>'
\(normal.cc:111)."
  (kao--map-filter-selections fn t))

(defmacro kao-with-saved-selections (&rest body)
  "Run BODY, then restore the selection list as it was before (Kakoune `-draft').
Each selection's anchor and cursor are saved as insertion-type markers that
track through any edits BODY makes, together with its captures and the main
index.  On exit the markers are read back and the list is reinstalled verbatim
via `kao-set-selections-raw' (no merge — the saved order, any intentional
overlap, and the captures are kept), the markers are freed, and BODY's value is
returned.  An `unwind-protect' guarantees the restore and the marker cleanup
even if BODY signals or quits.  Insertion-type t both ends
makes a delimiter inserted at an endpoint land OUTSIDE the restored span (the
surround-add case): an open inserted before the min lands left of the anchor, a
close inserted after the max lands right of the cursor, so the restored
selection re-covers the original content.  This is the substrate for surround
delete/replace and any select-operate-return-to-prior-selection recipe."
  (declare (indent 0) (debug (body)))
  (let ((saved (gensym "kao--saved")) (main (gensym "kao--saved-main")))
    `(let ((,saved (mapcar (lambda (s)
                             (cons (kao-sel-captures s)
                                   (cons (copy-marker (kao-sel-anchor s) t)
                                         (copy-marker (kao-sel-cursor s) t))))
                           (kao-sels-list kao--sels)))
           (,main (kao-sels-main kao--sels)))
       (unwind-protect
           (progn ,@body)
         (kao-set-selections-raw
          (mapcar (lambda (e)
                    (kao-sel-make :anchor (marker-position (cadr e))
                                  :cursor (marker-position (cddr e))
                                  :captures (car e)))
                  ,saved)
          ,main)
         (dolist (e ,saved)
           (set-marker (cadr e) nil)
           (set-marker (cddr e) nil))))))

;;;; State-transition and selection-change hooks (config substrate, A3)

;; Ordinary `defvar ... nil' hook lists run via `run-hooks' at the state-flag
;; transitions and inside the selection-history recorder.  Distinct from the
;; hook-DSL exclusion (which says \"use Emacs hooks\" — there were none to
;; hang them on).  Every fire is guarded `(when HOOK (run-hooks ...))' so a
;; buffer with no listener pays zero cost on the motion hot path (P-track).

(defvar kao-insert-state-entry-hook nil
  "Hook run after kao enters insert state (normal to insert).
Fires once per real entry: `i'/`a'/`o'/`O'/`c' and the re-entry of the `<a-;>'
one-shot.  A setup that aborts before insert is established (a read-only text
property) fires a matching `kao-insert-state-exit-hook' rather than
leaving this unpaired.")

(defvar kao-insert-state-exit-hook nil
  "Hook run after kao leaves insert state (insert to normal).
Pairs with `kao-insert-state-entry-hook': fires from `kao-insert-exit', from
the `<a-;>' one-shot's flip to normal, and from a fresh setup-abort.")

(defvar kao-normal-state-entry-hook nil
  "Hook run after kao enters normal state.
Fires when `kao-mode' turns on, when insert state exits, and when the `<a-;>'
one-shot flips to normal — the place for mode-line and cursor configuration.")

(defvar kao-normal-state-exit-hook nil
  "Hook run when kao leaves normal state.
Pairs with `kao-normal-state-entry-hook' — fires from `kao--insert-begin'
before the insert-entry fire (normal to insert) and from the `kao-mode'
disable arm when normal was active, giving teardown a symmetric seam.
Does not fire on plain motions, which stay within normal state.")

(defvar kao-selection-change-hook nil
  "Hook run when the selection list changes value.
Fired from the per-command recorder (`kao--sel-history-record'), in its
vector-changed and main-changed branches only — never on a no-op command, so
there is no per-keystroke identity churn — and from `kao-sel-undo'/
`kao-sel-redo', which bypass the recorder.  Suspended during insert state (the
selection list is not live then).")

;;;; Public state read + cursor refresh

;; The substrate-blessed way to READ kao's modal state and RE-ASSERT the cursor
;; (the Layer-0 config surface).  Third-party extensions (ghostel, cursor
;; themes, repeat-mode gates) and the author's init.el hand control back on the
;; nil, so `kao-current-state' stays a THREE-way cond — nil when `kao-mode' is
;; off, never collapsed to a two-way one.  Thin public wrappers over the private
;; `kao--normal-active'/`kao--insert-active' flags and `kao--apply-cursor'; no
;; caller needs to reach a private flag.

(defun kao-current-state ()
  "Return kao's modal state in the current buffer: nil, \\='normal, or \\='insert.
Nil when `kao-mode' is off — extensions rely on that to hand control back.  The
public read for the private `kao--normal-active'/`kao--insert-active' flags."
  (cond ((not (bound-and-true-p kao-mode)) nil)
        (kao--insert-active 'insert)
        (kao--normal-active 'normal)))

(defsubst kao-normal-state-p ()
  "Non-nil when the current buffer is in kao normal state.
Gate for a `repeat-mode' / transient-map predicate (recipe)."
  (eq (kao-current-state) 'normal))

(defsubst kao-insert-state-p ()
  "Non-nil when the current buffer is in kao insert state."
  (eq (kao-current-state) 'insert))

(defun kao-refresh-cursor ()
  "Re-assert the cursor shape and color for kao's current state.
The public escape hatch after a foreign `setq cursor-type' or a theme change
clobbers it (substrate); reads `kao-current-state' and applies via
`kao--apply-cursor'."
  (kao--apply-cursor (kao-current-state)))

;;;; Dynamic registers — the `register_registers' definitions

;; The single registration site, mirroring `register_registers'
;; (main.cc:340-403).  kao-state owns the buffer-local `kao--sels' the
;; getters read, and already requires kao-register (acyclic).  The
;; selection-backed getters (`.' `#' 0-9) return nil in a buffer without a
;; selection list (no kao-mode) — Kakoune always has a selection context,
;; so nil-vs-error there is unreachable through kao's own keys; `%' reads
;; only the buffer name and works everywhere.

(kao-register-define-dynamic ?%
  (lambda () (list (buffer-name))))      ; Buffer::display_name (main.cc:352)

(kao-register-define-dynamic ?.
  (lambda ()                             ; selections_content (main.cc:357)
    (when kao--sels
      (mapcar (lambda (s)                ; empty-buffer clamp: the
                                         ; phantom end (max+1) exceeds point-max
                (buffer-substring (kao-sel-beg s)
                                  (min (kao-sel-end s) (point-max))))
              (kao-sels-list kao--sels)))))

(kao-register-define-dynamic ?#
  (lambda ()                             ; 1-based indices (main.cc:364-373)
    (when kao--sels
      (let ((res '()))
        (dotimes (i (length (kao-sels-list kao--sels)))
          (push (number-to-string (1+ i)) res))
        (nreverse res)))))

(dotimes (i 10)
  (let ((i i))                           ; fresh binding per closure pair
    (kao-register-define-dynamic
     (+ ?0 i)
     (lambda ()                          ; captures[i] or "" (main.cc:375-384)
       (when kao--sels
         (mapcar (lambda (s) (or (nth i (kao-sel-captures s)) ""))
                 (kao-sels-list kao--sels))))
     (lambda (strings)                   ; the digit SETTER (main.cc:386-398)
       (when (and strings kao--sels)
         (let ((k 0) (last (1- (length strings))))
           (dolist (s (kao-sels-list kao--sels))
             ;; copy-on-write: C++ CaptureLists are value-semantic per
             ;; Selection — never mutate a possibly shared list in place.
             (let ((caps (copy-sequence (kao-sel-captures s))))
               (when (< (length caps) (1+ i))
                 (setq caps (append caps
                                    (make-list (- (1+ i) (length caps)) ""))))
               (setf (nth i caps) (nth (min k last) strings))
               (setf (kao-sel-captures s) caps))
             (setq k (1+ k)))))))))

(defun kao--main-sel-register-value (reg &optional buffer)
  "Return REG's string at BUFFER's main-selection index, nil when REG is empty.
Ports `main_sel_register_value' (main.cc:195-198 -> `Register::get_main',
register_manager.cc:36-42): REG resolves in BUFFER (default the current buffer,
so a dynamic register reads that buffer's live state) and the string returned is
the one at the main selection's index, LAST-CLAMPED to
`content[min(main_index, size-1)]'.  Without a live selection list the front
string is used.  The `/'/`|' history registers hold one value (last-value-only),
so this clamps to that single front string — their `get_main = front'
behavior is unchanged."
  (with-current-buffer (or buffer (current-buffer))
    (let ((strings (kao-register-get reg)))
      (and strings
           (nth (min (or (and kao--sels (kao-sels-main kao--sels)) 0)
                     (1- (length strings)))
                strings)))))

;;;; Normal params — count + pending register (Kakoune `NormalParams')

(defun kao-digit ()
  "Accumulate the typed digit into `kao--count' (Kakoune count*10 + digit).
Bound to 0-9 in normal state; `kao--maybe-reset-count' keeps the count alive
across consecutive digits and clears it after the next non-digit command.
Accumulating past INT_MAX is rejected with Kakoune's exact \"parameter
overflowed\" status and the count keeps its previous value — the digit is
discarded, the pending count survives (input_handler.cc:305-310; without the
guard Emacs bignums accept any count and a huge repeat hangs)."
  (interactive)
  (let ((d (- last-command-event ?0)))
    (when (<= 0 d 9)
      (let ((new-val (+ (* kao--count 10) d)))
        (if (> new-val 2147483647)
            (message "parameter overflowed")
          (setq kao--count new-val))))))

(defcustom kao-use-prefix-arg-count t
  "When non-nil, a native prefix argument feeds kao's repeat count.
kao's own count (typed digits, `kao--count') always wins; this only fills in
when kao's count is unset, bridging `C-u'/`M-N' (`current-prefix-arg') into
`kao--repeat-count' so `C-u 3 w' counts like `3 w'.
A pure accommodation — it never overrides a kao count, and with no prefix the
faithful default of 1 is unchanged; set nil to ignore the native prefix
entirely."
  :type 'boolean
  :group 'kao)

(defun kao--count-or (default)
  "Return `kao--count' when a count was typed, otherwise DEFAULT.
Collapses the `(if (> kao--count 0) kao--count DEFAULT)' idiom for
Kakoune commands acting on DEFAULT when no count is given.  DEFAULT is
evaluated unconditionally (applicative order), so keep it side-effect-free."
  (if (> kao--count 0) kao--count default))

(defun kao--repeat-count ()
  "Effective repeat count for the next command: `kao--count', prefix, or 1.
kao's own `kao--count' (typed digits) has absolute precedence.  When it is
unset (0) and `kao-use-prefix-arg-count' is on, a native prefix argument
\(`current-prefix-arg', from `C-u'/`M-N') is bridged in
\; with neither, the faithful default is 1."
  ;; Not `(kao--count-or 1)': the prefix-arg bridge makes this a three-way
  ;; precedence (count > prefix > 1), not the two-way count-or-default idiom.
  (cond ((> kao--count 0) kao--count)
        ((and kao-use-prefix-arg-count current-prefix-arg)
         (max 1 (prefix-numeric-value current-prefix-arg)))
        (t 1)))

(defun kao--repeat (fn)
  "Return a selection transform applying FN `kao--repeat-count' times.
Composing N times in a single `kao--map-selections' pass keeps one refresh per
command (perf principle); equals Kakoune's `repeated<>' for a single selection.
Per-iteration re-sort across N>1 live selections is a P2 concern.
Short-circuits on a nullopt: once FN returns nil mid-count the
fold stops and returns nil, so the drop dispatch (`kao--map-filter-selections')
drops that selection — Kakoune's `repeated<>' propagating `nothing_here'.  FN
must never be re-invoked on nil (word selectors would signal on a nil cursor)."
  (let ((n (kao--repeat-count)))
    (lambda (sel)
      (let ((s sel) (i 0))
        (while (and s (< i n))
          (setq s (funcall fn s) i (1+ i)))
        s))))

(defun kao-count-backspace ()
  "Divide the accumulated count by 10 (Kakoune Backspace in normal mode).
Ports `m_params.count /= 10' (input_handler.cc:311-312).  Binding it also keeps
the global `delete-backward-char' from reaching the buffer in normal state."
  (interactive)
  (setq kao--count (/ kao--count 10)))

(defvar kao--macro-recording-reg)       ; kao-macro.el (loads after this file)
(defvar kao--macro-recorded-keys)
(defvar kao--macro-replaying)

(defun kao--read-key (prompt)
  "Read one key for a one-shot menu, echoing PROMPT.
Like `read-key', but when a macro is being recorded the key is pushed onto
`kao--macro-recorded-keys': the recording `pre-command-hook' captures only
`this-command-keys-vector', so the char a menu consumes via `read-key'
\(goto/view/object/combine/register) would otherwise be lost and replay
would desync — the replayed `read-key' reads from the executing macro, so
it must find this key there.  Kakoune records EVERY key at the input level
\(input_handler.cc:1712), including the `on_next_key' char and Escape, so
the push is unconditional — a recorded cancelled menu re-eats its cancel
key on replay.  Inert (a plain `read-key') until kao-macro.el is loaded
and recording is live."
  (let ((key (read-key prompt)))
    (when (and (boundp 'kao--macro-recording-reg) kao--macro-recording-reg
               (not (bound-and-true-p kao--macro-replaying)))
      (push (vector key) kao--macro-recorded-keys))
    key))

(defun kao--key-codepoint (ch)
  "Normalize CH the way Kakoune's Key::codepoint() does (keys.cc:46-53).
Return is translated to newline: the character ?\\r AND the GUI `read-key'
symbols `return' / `kp-enter' all fold to ?\\n —
a GUI Emacs frame delivers Return as the symbol `return', which a caller's
`characterp' guard would otherwise silently drop, cancelling `r RET' instead
of replacing with a newline.  Tab/Space already arrive as ?\\t/?\\s.  Every
other event passes through unchanged; callers keep their `characterp' cancel
guards, which apply AFTER this fold."
  (if (memq ch '(?\r return kp-enter)) ?\n ch))

(declare-function kao-info--show "kao-info" (name rows))
(declare-function kao-info--hide "kao-info" ())

(defun kao-on-key (prompt fn &optional info-rows)
  "Read one key (macro-safe), then call (funcall FN key); show optional INFO-ROWS.
The public form of Kakoune `on-key' (the `c'/goto/view one-shot reader).
PROMPT is echoed while waiting; INFO-ROWS, when non-nil, is an (EVENT . DOC)
alist shown in an autoinfo box that is torn down on exit — even if FN signals
\(the `unwind-protect' makes this the box-leak-free form, unlike a bare
`kao-info--show').  Reads through `kao--read-key', so a key consumed here is
pushed onto the `Q'-recording stream and macro replay stays in lockstep,
unlike a bare `read-char'.  Returns FN's value.

\(`kao-info--show'/`-hide' live in kao-info, which loads after kao-state; they
are reachable at run time — `declare-function' silences the compile-time
forward reference, the same idiom as `kao--lighter' in `kao-mode'.)"
  (unwind-protect
      (progn
        (when info-rows (kao-info--show prompt info-rows))
        (funcall fn (kao--read-key prompt)))
    (when info-rows (kao-info--hide))))

(defun kao--register-arg (default)
  "Return the pending register char, or DEFAULT when none is pending.
The raw char, exactly as Kakoune's `params.reg ? params.reg : DEFAULT'
sites (e.g. normal.cc:785): lowering happens where Kakoune lowers — inside
the store accessors (`RegisterManager::operator[]', register_manager.cc:99)
or at the explicit `to_lower' consumer sites (search/macro/selection)."
  (or kao--pending-register default))

(defun kao--maybe-reset-count ()
  "Post-command hook: reset the pending params (count + register).
Mirrors Kakoune resetting `m_params' after each dispatched or unmapped key
\(input_handler.cc:365-372).  The three params keys are exempt, and each
leaves the OTHER param untouched exactly as in the C++ key loop: a digit or
Backspace never touches `m_params.reg', and `\\='\"' never touches
`m_params.count' (input_handler.cc:304-336)."
  (unless (or (memq this-command
                    '(kao-digit kao-count-backspace kao-select-register))
              ;; Mouse-handled keys early-return BEFORE the `m_params'
              ;; reset (input_handler.cc:297-303 vs :365-372), so a
              ;; pending count/register survives mouse events — and the
              ;; one-shot state below is likewise not popped.
              (memq this-command kao-mouse-click-commands)
              (memq this-command kao-mouse-scroll-commands))
    (setq kao--count 0)
    (setq kao--pending-register nil)
    ;; One-shot normal (`<a-;>'): Kakoune's `pop_if_single_command'
    ;; wraps exactly the else-branch that resets `m_params'
    ;; (input_handler.cc:339-344 vs :365-372), so the params keys above are
    ;; also the keys that do NOT pop (the C++ set additionally holds `\'
    ;; hooks-disable, :314-321 — unmodeled in kao).  The entry command
    ;; itself must not pop the state it just set up.
    (when (and kao--insert-oneshot
               (not (eq this-command 'kao-insert-one-shot)))
      (kao--oneshot-pop))))

(defun kao-normal-escape ()
  "Quietly cancel the pending params (count + register).
Kakoune leaves Escape unmapped in normal mode: an unmapped key is silently
ignored and `m_params' is reset (the `get_normal_command' else-branch,
input_handler.cc:365-372) — this command is that reset, made explicit.  The
binding also keeps an `escape' event (GUI, or a terminal ESC decoded by
`kao-esc-mode') from falling through `local-function-key-map' back to the
ESC Meta prefix, which would block waiting for a second key.  Deliberately
NOT exempt in `kao--maybe-reset-count': the params must clear."
  (interactive)
  (setq kao--count 0)
  (setq kao--pending-register nil))

;;;; Selection history (`<a-u>' / `<a-U>')

;; Kakoune's `Context::SelectionHistory' (context.cc:175-259) records the
;; selection list so `<a-u>'/`<a-U>' (`undo_selection_change<Backward/Forward>',
;; normal.cc:2224) can step it back/forward — distinct from `u'/`U', which undo
;; buffer *text*.  A `ScopedSelectionEdition' wraps essentially every
;; selection-mutating command (nested via a `NestedBool', so it stages once at
;; command entry and commits once at exit); `end_edition' pushes a new node only
;; when the selection *vector* changed (`operator==' ignores the main index,
;; selection.hh:147) — otherwise it just updates the current node's main index.
;;
;; The native-mechanism, faithful-behaviour family: kao
;; records via a buffer-local `post-command-hook' (the idiomatic translation of
;; "wrap every command", cf. the count-reset hook) into a LINEAR snapshot
;; sequence, stored as a fixed-capacity RING so recording is O(1) per
;; command and memory is bounded by `kao-sel-history-max' (the hot-path
;; principle; the original list storage paid an O(history) copy per change).
;; Kakoune's store is a tree (parent + lazily-set redo_child), but undo
;; follows the single parent chain and redo the single last-visited child, and
;; no key navigates a sibling branch of the *selection* history (the
;; branch-navigable tree is the *buffer* history, `<c-j>'/`<c-k>', slice) — so
;; a linear stack + index is observationally identical for `<a-u>'/`<a-U>'
;; (a new change after an undo overwrites redo = truncate the tail).  Snapshots
;; reuse the canonical `kao--snapshot-sels' deep copy.  The hook is gated
;; to normal state (the selection is suspended during insert, so an insert
;; session's net change is recorded once on exit) and skips the buffer undo/redo
;; commands (`undo'/`redo' are NOT wrapped in a `ScopedSelectionEdition',
;; normal.cc:2172/2186 — they use `selections_write_only').  The change-id
;; navigation `<c-j>'/`<c-k>' (`move_in_history', normal.cc:2200-2220) and the
;; selection-undo keys `<a-u>'/`<a-U>' (`undo_selection_change', normal.cc:2224)
;; are skipped for the same reason: `move_in_history' assigns via
;; `selections_write_only' at normal.cc:2210 exactly like u/U, and
;; `undo_selection_change' runs outside any `ScopedSelectionEdition' — so none of
;; the four records a node.  The `kao-vundo'/arbitrary-id entry
;; (`kao-history-goto') let-binds `kao--sel-history-inhibit' around its navigate
;; so ANY `this-command' driving it is likewise recorder-inert.
;;
;; Deferred (family, documented not silent): the buffer-undo <->
;; selection-history coordination (`selections_write_only' overwriting the
;; current node + the `end_edition' content-undo timestamp sync); cross-buffer
;; history (`forget_buffer'/`change_buffer'); the tree storage itself (its only
;; observable effect, branch retention, has no `<a-u>'/`<a-U>' navigation path).
;; Snapshots are ((GEN . ID) . SNAP) ring entries: integer positions tagged with
;; the tree generation and the history id they were recorded at,
;; coordinate-translated through the tree path on restore
;; (`kao--snapshot-update' — the faithful `update()'; supersedes the
;; original clamp-only stance).  A gc'd id — or a stale generation, from a tree a
;; re-init replaced — falls back to clamp+merge, the
;; documented residual.

(defcustom kao-sel-history-max 1000
  "Maximum number of selection-history nodes kept per buffer.
The history is a fixed-size ring sized at `kao-mode' enable time; when full,
recording a new node evicts the oldest, so `<a-u>' bottoms out at the oldest
retained node.  Kakoune keeps an unbounded tree; the cap is the documented
Emacs-pragmatic deviation that keeps recording O(1) in both time and memory
\(the hot-path principle).  Values below 2 are treated as 2."
  :type 'natnum
  :group 'kao)

(defvar-local kao--sel-history nil
  "Buffer-local RING of `kao-sels' snapshots (selection history).
A vector of `kao-sel-history-max' slots; `kao--sel-history-start' is the
physical index of the oldest retained node, `kao--sel-history-size' the node
count, and `kao--sel-history-index' the LOGICAL live position (0 = oldest
retained).  Logical node K lives at (start + K) mod capacity —
`kao--sel-history-node'.  Node 0 is the seed (the list when `kao-mode' was
enabled) until a full ring evicts it.")

(defvar-local kao--sel-history-start 0
  "Physical ring index of the oldest retained history node.")

(defvar-local kao--sel-history-size 0
  "Number of nodes currently in the selection-history ring.")

(defvar-local kao--sel-history-index 0
  "Logical index of the live node (0 = the oldest retained node).")

(defvar kao--sel-history-inhibit nil
  "When non-nil, `kao--sel-history-record' records no node.
Let-bound around `kao--hist-navigate' by the arbitrary-id history entry
`kao-history-goto' so a `<c-j>'/`<c-k>' or `kao-vundo'-driven jump records no
selection-history node regardless of `this-command' — Kakoune's
`move_in_history' assigns via `selections_write_only' (normal.cc:2210), pushing
no SelectionHistory node, exactly like u/U.  The `memq' skip list covers the
direct commands; this flag covers any caller of `kao-history-goto'.")

(defun kao--sel-history-node (k)
  "Return logical node K of the selection-history ring (0 = oldest retained)."
  (aref kao--sel-history
        (mod (+ kao--sel-history-start k) (length kao--sel-history))))

;; A selection-history ring node is the 2-part cons ((GEN . ID) . SNAP): a
;; `(GEN . ID)' coordinate-frame head (the tree generation + history id the
;; snapshot was recorded at) consed onto the
;; `kao--snapshot-sels' SNAP.  These thin accessors are the named seam for that
;; shape — every ring build/decode goes through them instead of hand-positional
;; `car'/`cdr' surgery.  Distinct from the PARALLEL 3-part
;; `kao--snap-tag-*' family, which tags a lone snapshot's `(GEN . ID)'.
;; The `(GEN . ID)' head must stay a live cons: the recorder value-diffs it and
;; the restore re-frames it in place.  The `kao--genid-*' primitives below are the
;; single build/decode of that head cons — the ring seam and the shared
;; `kao--snapshot-update' consumer both route through them, never hand-positional
;; `car'/`cdr' surgery.
(defsubst kao--genid-make (gen id)
  "Build a `(GEN . ID)' coordinate-frame head from GEN and ID."
  (cons gen id))

(defsubst kao--genid-gen (genid)
  "Return the tree generation of `(GEN . ID)' head GENID."
  (car genid))

(defsubst kao--genid-id (genid)
  "Return the history id of `(GEN . ID)' head GENID."
  (cdr genid))

(defsubst kao--sel-ring-make (genid snap)
  "Build a selection-history ring node from GENID head and SNAP.
GENID is the `(GEN . ID)' coordinate-frame cons; SNAP is the snapshot."
  (cons genid snap))

(defsubst kao--sel-ring-genid (node)
  "Return the `(GEN . ID)' coordinate-frame head of ring NODE."
  (car node))

(defsubst kao--sel-ring-snap (node)
  "Return the stored snapshot of ring NODE."
  (cdr node))

(defsubst kao--sel-ring-gen (node)
  "Return the tree generation stamped on ring NODE."
  (kao--genid-gen (kao--sel-ring-genid node)))

(defsubst kao--sel-ring-id (node)
  "Return the history id stamped on ring NODE."
  (kao--genid-id (kao--sel-ring-genid node)))

(defun kao--sel-history-record ()
  "Post-command hook: append the post-command selection list as a history node.
One node per top-level selection-mutating command (Kakoune's per-command
`ScopedSelectionEdition', context.cc:199-227): when the selection *vector*
changed, drop any redo tail past the current index and push a fresh
`kao--snapshot-sels' (the branch-overwrite is observationally a truncate);
when only the main index changed, update the current node in place (the
`end_edition' `set_main_index' path, since `operator==' ignores the main,
selection.hh:147); when nothing changed, do nothing.  Gated to normal state
\(selection suspended during insert) and skips `kao-undo'/`kao-redo', which
Kakoune leaves unwrapped (normal.cc:2172/2186).  Also gated off during a
one-shot normal command (`<a-;>'): the insert session holds the OUTER
`ScopedSelectionEdition' (a `NestedBool' — input_handler.cc:1536), so the
nested one-shot command commits no node of its own; the session's net
selection change is recorded once on exit, exactly as in Kakoune."
  (when (and kao--sels kao--normal-active (not kao--insert-active)
             (not kao--insert-oneshot)
             kao--sel-history
             (not kao--sel-history-inhibit)
             (not (memq this-command '(kao-undo kao-redo
                                       kao-sel-undo kao-sel-redo
                                       kao-history-forward kao-history-backward))))
    (let* ((node (kao--sel-history-node kao--sel-history-index))
           (cap (length kao--sel-history))
           ;; Compute the value-diff ONCE and reuse it for the cond and the
           ;; A3 hook guard — never a doubled `equal' on the hot path (P-track).
           (vector-changed (not (equal (kao-sels-list kao--sels)
                                       (kao-sels-list (kao--sel-ring-snap node)))))
           (main-changed (/= (kao-sels-main kao--sels)
                             (kao-sels-main (kao--sel-ring-snap node)))))
      (cond
       (vector-changed
        ;; Logical truncate of the redo tail, then O(1) push; evict the
        ;; oldest node when the ring is full (cap).  The entry is
        ;; ((GEN . ID) . SNAP) — the history id is the snapshot's coordinate
        ;; frame (= Kakoune's SelectionList timestamp), stamped with the
        ;; tree generation so a superseded tree's tag cannot alias.
        (setq kao--sel-history-size (1+ kao--sel-history-index))
        (when (= kao--sel-history-size cap)
          (setq kao--sel-history-start (mod (1+ kao--sel-history-start) cap)
                kao--sel-history-size (1- kao--sel-history-size)
                kao--sel-history-index (1- kao--sel-history-index)))
        (aset kao--sel-history
              (mod (+ kao--sel-history-start kao--sel-history-size) cap)
              (kao--sel-ring-make
               (kao--genid-make (kao-history-generation) (kao-history-current-id))
               (kao--snapshot-sels kao--sels)))
        (setq kao--sel-history-size (1+ kao--sel-history-size)
              kao--sel-history-index (1- kao--sel-history-size)))
       (main-changed
        (setf (kao-sels-main (kao--sel-ring-snap node)) (kao-sels-main kao--sels))))
      (when (and (or vector-changed main-changed) kao-selection-change-hook)
        (run-hooks 'kao-selection-change-hook)))))

(defun kao--sel-history-restore ()
  "Restore the selection-history node at the live index, re-framed on access.
Loads the node into `kao--sels' coordinate-translated and re-frames the stored
ring entry in place to the current history id.
The node's positions fold through the history-tree path from the id they were
recorded at to the current id (`kao--snapshot-update' — the restored
SelectionList's `update', context.cc:195), matching the
restore paths.  When the fold crossed frames (the recorded generation/id differs
from the current), the stored ring slot is rewritten to
\((GEN . CURRENT-ID) . TRANSLATED) — the elisp analogue of Kakoune's
`sels.update()'-on-access (context.cc:188-196),
whose `update_selections' early-returns at the current timestamp
\(selection.cc:233-236) and only writes back when it actually translates.  This
keeps the recorded frame consistent with the live frame, so the NEXT command's
value-diff cannot spuriously truncate the redo tail.  The stored
SNAP is a fresh `kao--snapshot-sels' deep copy, so the live list stays
decoupled from the node (a later edit cannot mutate the history; the recorder
sees no change)."
  (let* ((slot (mod (+ kao--sel-history-start kao--sel-history-index)
                    (length kao--sel-history)))
         (node (aref kao--sel-history slot))
         (genid (kao--sel-ring-genid node))
         (translated (kao--snapshot-update (kao--sel-ring-snap node) genid)))
    ;; Re-frame unless the tag is already at the current tree generation AND
    ;; current id (no fold occurred) — a stale-generation tag always re-frames
    ;; into the live frame ((GEN . ID)).
    (unless (and (consp genid)
                 (eql (kao--sel-ring-gen node) (kao-history-generation))
                 (= (kao--sel-ring-id node) (kao-history-current-id)))
      (aset kao--sel-history slot
            (kao--sel-ring-make
             (kao--genid-make (kao-history-generation) (kao-history-current-id))
             (kao--snapshot-sels translated))))
    ;; direct install (retag-order): installs a translated snapshot + retags id
    ;; (no make); caller refreshes, so the seam's internal refresh is unwanted.
    (setq kao--sels translated)
    ;; The restored node is already folded into the CURRENT frame; re-pin the
    ;; live-list tag so the caller's `kao--refresh' does not translate it again.
    (kao--sels-retag)))

(defun kao-sel-undo ()
  "Kakoune `<a-u>': step the selection list back `count' changes.
`undo_selection_change<Backward>' (normal.cc:2224): repeat `kao--repeat-count'
times, walking `kao--sel-history-index' toward node 0 and restoring each node.
Signal `user-error' \"no selection change to undo\" at the seed (Kakoune's
wording, context.cc:245), having first refreshed any partial progress."
  (interactive)
  (kao--assert-mode)
  (let ((count (kao--repeat-count)))
    (while (> count 0)
      (when (<= kao--sel-history-index 0)
        (kao--refresh)
        (user-error "no selection change to undo"))
      (cl-decf kao--sel-history-index)
      (kao--sel-history-restore)
      (cl-decf count))
    (kao--refresh)
    (when kao-selection-change-hook (run-hooks 'kao-selection-change-hook))))

(defun kao-sel-redo ()
  "Kakoune `<a-U>': step the selection list forward `count' changes.
`undo_selection_change<Forward>' (normal.cc:2224): the mirror of `kao-sel-undo'
toward the newest node.  Signal `user-error' \"no selection change to redo\" at
the tip."
  (interactive)
  (kao--assert-mode)
  (let ((count (kao--repeat-count)))
    (while (> count 0)
      (when (>= kao--sel-history-index (1- kao--sel-history-size))
        (kao--refresh)
        (user-error "no selection change to redo"))
      (cl-incf kao--sel-history-index)
      (kao--sel-history-restore)
      (cl-decf count))
    (kao--refresh)
    (when kao-selection-change-hook (run-hooks 'kao-selection-change-hook))))

;;;; Jump list (`<c-s>' / `<c-o>' / `<c-i>')

;; Kakoune's `JumpList' (context.cc:103-155) records selection-list snapshots so
;; `<c-o>'/`<c-i>' step back/forward through "where you were", and `<c-s>' saves
;; the current selections.  Big-motion commands (goto, interactive search) also
;; push a jump so the position before the jump is recoverable.
;;
;; The native-mechanism, faithful-behaviour family: a
;; buffer-local `kao--jumps' (list of id-tagged `kao--snapshot-sels'
;; snapshots) +
;; `kao--jump-current' (0..len; len = "at the present, viewing no saved jump").
;; `kao--jumplist-push' ports `JumpList::push' (truncate the forward tail, dedup
;; by selection list IGNORING the main index per `operator==' selection.hh:147,
;; append, current=len); `kao-jump-backward'/`-forward' port `backward'/`forward'
;; (backward pushes the current selections first so `<c-i>' can return).  Since
;; `jump<dir>' wraps a `ScopedSelectionEdition' (normal.cc:1787), `<c-o>'/`<c-i>'
;; record a selection-history node (the recorder fires); `<c-s>'
;; (`push_selections', :1793) changes no selection and records none.
;;
;; Cross-buffer (resolves the deferral): the jump list is
;; GLOBAL — the faithful home for Kakoune's per-Context (per-client) JumpList
;; under kao's single-client stance — and every entry is a (BUFFER . SNAPSHOT)
;; cons (Kakoune's SelectionList carries its buffer, selection.hh:170; the
;; selection registers use the same cons shape).  `<c-o>'/`<c-i>' into
;; another buffer switch to it (the `jump<dir>' caller change_buffer's);
;; killed-buffer entries are pruned LAZILY on access with the faithful
;; `forget_buffer' index adjustment (context.cc:157-173) — the
;; `pop-global-mark' idiom (simple.el), no hook on the buffer-kill path.
;; The `update' coord translation landed: entries are
;; (BUFFER ID . SNAPSHOT) and `kao--jump-restore' translates through the
;; tree path after the buffer switch (gc'd id -> clamp+merge fallback).
;; `<c-i>' = `C-i' = TAB in a terminal, exactly as in
;; Kakoune's UI (no separate TAB binding in normal state).

(defvar kao--jumps nil
  "Global jump list: (BUFFER (GEN . ID) . SNAPSHOT) entries (Kakoune `m_jumps').
SNAPSHOT is a `kao-sels' snapshot; BUFFER is where it was pushed; (GEN . ID) is
the tree generation + history id it was tagged at.
Global across buffers like Kakoune's per-client JumpList (context.hh:29-46).")

(defvar kao--jump-current 0
  "Index into `kao--jumps' (Kakoune `m_current').
Equals the list length when at the present (viewing no saved jump).")

(defun kao--jumplist-prune ()
  "Drop entries whose buffer is dead (Kakoune `forget_buffer', context.cc:157).
Lazy, like `pop-global-mark': runs on access, not on kill.  Index adjustment
is faithful: a dropped entry before the current index pulls it back one;
dropping the current entry sends it to the (new) end."
  (let ((i 0))
    (dolist (entry (copy-sequence kao--jumps))
      (if (buffer-live-p (kao--snap-tag-buffer entry))
          (setq i (1+ i))
        (setq kao--jumps (delq entry kao--jumps))
        (cond ((< i kao--jump-current)
               (setq kao--jump-current (1- kao--jump-current)))
              ((= i kao--jump-current)
               (setq kao--jump-current (length kao--jumps))))))))

(defun kao--jumplist-push (snap)
  "Push SNAP onto `kao--jumps' (a `kao-sels' snapshot; Kakoune `JumpList::push').
Prune dead-buffer entries, truncate any forward tail, drop an existing equal
jump (same buffer and selection list, ignoring the main index — Kakoune's
`operator==' compares neither main nor timestamp), append SNAP as a
\(BUFFER (GEN . ID) . SNAP) entry — ID is the current history id, the snapshot's
coordinate frame for the restore-time translation, stamped with the tree
generation so a re-init's fresh id cannot alias it
— then point `kao--jump-current' past the end."
  (kao--jumplist-prune)
  (let ((len (length kao--jumps))
        (buf (current-buffer)))
    (when (< kao--jump-current len)
      (setq kao--jumps (cl-subseq kao--jumps 0 (1+ kao--jump-current))))
    (setq kao--jumps
          (append (cl-remove-if
                   (lambda (entry)
                     (and (eq (kao--snap-tag-buffer entry) buf)
                          (equal (kao-sels-list (kao--snap-tag-snap entry))
                                 (kao-sels-list snap))))
                   kao--jumps)
                  (list (kao--snap-tag-make buf snap)))
          kao--jump-current (length kao--jumps))))

(defun kao-jump-push (&optional sels)
  "Snapshot SELS (or the live `kao--sels') and push it (Kakoune `push_jump').
The SILENT public push — no echo-area message — so a foreign-command jump
wrapper (init.el `my/kao-goto--jump', the `SPC d'/`SPC r' xref wrappers) can
push the pre-jump selections quietly; the interactive `kao-jump-save' adds the
\"saved N\" message.  The substrate read/write for the jump list
\."
  (when kao--sels
    (kao--jumplist-push (kao--snapshot-sels (or sels kao--sels)))))

(defalias 'kao--jump-push #'kao-jump-push
  "Compatibility alias for `kao-jump-push' (promoted public).")

(defun kao--jump-target-check (entry)
  "Signal `user-error' when ENTRY's buffer cannot be jumped into.
Dead buffers are unreachable after `kao--jumplist-prune'; a live buffer with
`kao-mode' disabled aborts loudly rather than silently re-enabling the
mode — the user turned it off there (slice-10 plan decision)."
  (let ((buf (kao--snap-tag-buffer entry)))
    (unless (buffer-live-p buf)
      (user-error "jump target buffer is gone"))
    (unless (buffer-local-value 'kao-mode buf)
      (user-error "target buffer is not in kao-mode"))))

(defun kao--jump-restore ()
  "Load the jump at `kao--jump-current' into `kao--sels' (a fresh translated copy).
When the entry's buffer differs from the current one, switch to it first —
Kakoune's jump SelectionList carries its buffer and the caller calls
change_buffer (context.cc:135-155).  Positions are coordinate-translated
through the edits made since the push (`kao--snapshot-update' — the jump
list's `update' on get, context.cc:126/150); the switch happens
BEFORE the translation so the saved id reads its own buffer's tree.  Then
refresh."
  (let* ((entry (nth kao--jump-current kao--jumps))
         (buf (kao--snap-tag-buffer entry)))
    (unless (eq buf (current-buffer))
      (switch-to-buffer buf))
    ;; direct install (retag-order): installs a translated snapshot + retags id
    ;; (no make); the retag must follow the install, not route through the seam.
    (setq kao--sels (kao--snapshot-update (kao--snap-tag-snap entry)
                                          (kao--snap-tag-id entry)))
    ;; The restored list is already folded into the CURRENT frame, so re-pin the
    ;; live-list tag here — `kao--refresh' must not translate it a second time.
    (kao--sels-retag)
    (kao--refresh)
    (let ((m (kao--main-sel)))           ; polish: flash the landing
      (kao--pulse-span (kao-sel-beg m) (kao-sel-end m)))))

(defun kao-jump-save ()
  "Kakoune `<c-s>': push the current selections onto the jump list.
The interactive form: `kao-jump-push' does the silent push, this adds the
echo-area confirmation."
  (interactive)
  (kao--assert-mode)
  (kao-jump-push)
  (message "saved %d selections" (length (kao-sels-list kao--sels))))

(defun kao-jump-backward ()
  "Kakoune `<c-o>': jump backward `count' steps in the jump list.
Ports `JumpList::backward' (context.cc:135): push the current selections first
when at the present or they differ from the current jump (so `<c-i>' returns),
then step back; `user-error' \"no previous jump\" past the root."
  (interactive)
  (kao--assert-mode)
  (kao--jumplist-prune)
  (let* ((count (kao--repeat-count))
         (len (length kao--jumps))
         (cur (kao--snapshot-sels kao--sels))
         (should-push
          (or (= kao--jump-current len)
              (let ((entry (nth kao--jump-current kao--jumps)))
                (or (not (eq (kao--snap-tag-buffer entry) (current-buffer)))
                    (not (equal (kao-sels-list (kao--snap-tag-snap entry))
                                (kao-sels-list cur))))))))
    (when should-push (kao--jumplist-push cur))
    (let ((steps (+ count (if should-push 1 0))))
      (when (< (- kao--jump-current steps) 0)
        (user-error "no previous jump"))
      (kao--jump-target-check (nth (- kao--jump-current steps) kao--jumps))
      (setq kao--jump-current (- kao--jump-current steps))
      (kao--jump-restore)
      (message "jumped to #%d (%d)" kao--jump-current (1- (length kao--jumps))))))

(defun kao-jump-forward ()
  "Kakoune `<c-i>': jump forward `count' steps in the jump list.
Ports `JumpList::forward' (context.cc:119): only with a saved jump in view and
the target index is valid; otherwise `user-error' \"no next jump\"."
  (interactive)
  (kao--assert-mode)
  (kao--jumplist-prune)
  (let ((count (kao--repeat-count))
        (len (length kao--jumps)))
    (if (and (/= kao--jump-current len)
             (< (+ kao--jump-current count) len))
        (progn
          (kao--jump-target-check (nth (+ kao--jump-current count) kao--jumps))
          (setq kao--jump-current (+ kao--jump-current count))
          (kao--jump-restore)
          (message "jumped to #%d (%d)" kao--jump-current (1- (length kao--jumps))))
      (user-error "no next jump"))))

;;;; Insert state

(defun kao--open-undo-group ()
  "Begin a change group so the whole insert session is one undo unit.
A no-op when a session's group is already live: an insert entry command run
from a one-shot normal (`<a-;> i' — Kakoune's nested `push_mode')
continues the SAME session as one undo unit instead of leaking the open
handle."
  (unless kao--insert-undo-handle
    (setq kao--insert-undo-handle (prepare-change-group))
    (activate-change-group kao--insert-undo-handle)))

(defun kao--close-undo-group ()
  "End the insert change group, amalgamating it into a single undo step.
Also commit the whole insert session as one history-tree node: the
change hooks accumulated every keystroke (and the secondary replays) into
`kao--hist-pending' while `kao--hist-maybe-commit' was suppressed by
`kao--insert-active', so the session lands as exactly one node."
  (when kao--insert-undo-handle
    (undo-amalgamate-change-group kao--insert-undo-handle)
    (accept-change-group kao--insert-undo-handle)
    (setq kao--insert-undo-handle nil)
    (kao-history-commit-pending)))

(defun kao--hist-maybe-commit ()
  "Commit the current command's modifications as one history node.
Skipped while an insert session is open (`kao--insert-active') — including
the one-shot normal window (`kao--insert-oneshot'), where Kakoune's
insert undo group is likewise still open: a one-shot edit joins the
session's single node.  The session is committed once on exit by
`kao--close-undo-group'.  A non-editing command leaves `kao--hist-pending'
empty, so this is a no-op there."
  (unless (or kao--insert-active kao--insert-oneshot)
    (kao-history-commit-pending)
    ;; kao's own edit installed post-edit selections already in the new frame;
    ;; now that its modifications are committed, re-pin the live-list tag to the
    ;; new node so the next `kao--refresh' does not fold them through the very
    ;; edit that produced them.  A FOREIGN non-command edit
    ;; leaves this flag nil, so its commit falls through to translation instead.
    (when kao--sels-edit-pending
      (kao--sels-retag)
      (setq kao--sels-edit-pending nil))))

;;;; Cursor shape + optional per-state color (-2)
;; The shape (box normal / bar insert) is buffer-local and always applied.
;; The COLOR is a frame-global parameter, so it is opt-in (both defcustoms
;; default nil = zero machinery) and re-asserted on window-buffer changes so
;; an enabled color does not leak into non-kao buffers.

(defcustom kao-cursor-color-normal nil
  "Cursor color in normal state (a color string), or nil to leave it alone.
The cursor color is a frame parameter, GUI frames only."
  :type '(choice (const :tag "Frame default" nil) color)
  :group 'kao)

(defcustom kao-cursor-color-insert nil
  "Cursor color in insert state (a color string), or nil to leave it alone.
The cursor color is a frame parameter, GUI frames only."
  :type '(choice (const :tag "Frame default" nil) color)
  :group 'kao)

(defcustom kao-cursor-normal 'box
  "Cursor SHAPE in kao normal state (any `cursor-type' value).
Read by `kao--apply-cursor'; the faithful default is a box on the cursor char.
Accepts any `cursor-type': a symbol (`box'/`hollow'/`bar'/`hbar'/nil/t) or a
cons like (bar . 3) / (hbar . 3) for an N-pixel width."
  :type '(choice (const box) (const hollow) (const bar) (const hbar)
                 (const :tag "No cursor" nil) (const :tag "Frame default" t)
                 (cons :tag "Bar, N pixels wide" (const bar) integer)
                 (cons :tag "Hbar, N pixels tall" (const hbar) integer)
                 (sexp :tag "Other cursor-type value"))
  :group 'kao)

(defcustom kao-cursor-insert 'bar
  "Cursor SHAPE in kao insert state (any `cursor-type' value).
Read by `kao--apply-cursor'; the faithful default is a bar.  See
`kao-cursor-normal' for the accepted values."
  :type '(choice (const box) (const hollow) (const bar) (const hbar)
                 (const :tag "No cursor" nil) (const :tag "Frame default" t)
                 (cons :tag "Bar, N pixels wide" (const bar) integer)
                 (cons :tag "Hbar, N pixels tall" (const hbar) integer)
                 (sexp :tag "Other cursor-type value"))
  :group 'kao)

(defun kao--cursor-color-apply (state &optional frame)
  "Apply the cursor color for STATE on FRAME (nil STATE = restore original).
No-op unless one of the color defcustoms is set and FRAME is graphical.
The frame's original color is remembered in a frame parameter the first
time a state color replaces it."
  (when (and (or kao-cursor-color-normal kao-cursor-color-insert)
             (display-graphic-p frame))
    (let ((color (pcase state
                   ('insert kao-cursor-color-insert)
                   ('normal kao-cursor-color-normal)))
          (orig (frame-parameter frame 'kao--cursor-color-orig)))
      (if color
          (progn
            (unless orig
              (set-frame-parameter frame 'kao--cursor-color-orig
                                   (frame-parameter frame 'cursor-color)))
            (set-frame-parameter frame 'cursor-color color))
        (when orig
          (set-frame-parameter frame 'cursor-color orig))))))

(defun kao--apply-cursor (state)
  "Set the cursor shape (and optional color) for STATE (`normal'/`insert').
The shape reads the `kao-cursor-insert' / `kao-cursor-normal' defcustoms
\(evil-config-6); a non-insert STATE (normal or nil) uses `kao-cursor-normal'."
  (setq-local cursor-type
              (if (eq state 'insert) kao-cursor-insert kao-cursor-normal))
  (kao--cursor-color-apply state))

(defun kao--cursor-color-sync (frame)
  "Re-assert the per-state cursor color for FRAME's current buffer.
On `window-buffer-change-functions' (rare, not per-keystroke): a non-kao
buffer restores the frame's original color, a kao buffer its state's."
  (with-current-buffer (window-buffer (frame-selected-window frame))
    (kao--cursor-color-apply
     (cond ((not (bound-and-true-p kao-mode)) nil)
           (kao--insert-active 'insert)
           (t 'normal))
     frame)))

;;;; Input method (hel-study-1)

;; Per-state input-method management, ported from evil/hel (evil-core.el:124-137,
;; :405-433; hel-core.el:369-446).  Kakoune has no in-editor IME, so this is the
;; native-mechanism family, not a parity port: the method is saved
;; and deactivated on normal-state entry (a normal key then resolves to its kao
;; command, not the method's translated char) and reactivated on insert entry.

(defmacro kao--without-input-method-hooks (&rest body)
  "Run BODY with kao's input-method hook handlers suppressed.
An internal `activate-input-method'/`deactivate-input-method' toggle would
otherwise re-enter `kao--input-method-activated'/`kao--input-method-deactivated'
and fight the toggle; binding both hooks to nil detaches them for the extent of
BODY (the evil `evil-without-input-method-hooks' pattern)."
  (declare (indent 0) (debug t))
  `(let ((input-method-activate-hook nil)
         (input-method-deactivate-hook nil))
     ,@body))

(defun kao--input-method-activated ()
  "Remember the just-activated input method, dropping it in normal state.
Buffer-local `input-method-activate-hook' handler (`permanent-local-hook'):
keeps `kao--input-method' current so insert entry can restore it, then
deactivates the method in normal state so it does not swallow normal keys."
  (when (bound-and-true-p kao-mode)
    (setq kao--input-method current-input-method)
    (when kao--normal-active
      (kao--without-input-method-hooks
        (deactivate-input-method)))))
(put 'kao--input-method-activated 'permanent-local-hook t)

(defun kao--input-method-deactivated ()
  "Forget the saved input method when it is turned off explicitly.
Buffer-local `input-method-deactivate-hook' handler (`permanent-local-hook') so
a later insert entry does not resurrect a method the user switched off."
  (setq kao--input-method nil))
(put 'kao--input-method-deactivated 'permanent-local-hook t)

(defun kao--input-method-enter-normal ()
  "Deactivate the input method entering normal state (hel-study-1).
The method is saved in `kao--input-method' so `kao--input-method-enter-insert'
can restore it; the hooks are suppressed so the round-trip does not clear it."
  (when current-input-method
    (setq kao--input-method current-input-method)
    (kao--without-input-method-hooks
      (deactivate-input-method))))

(defun kao--input-method-enter-insert ()
  "Reactivate the saved input method entering insert state (hel-study-1)."
  (when kao--input-method
    (kao--without-input-method-hooks
      (activate-input-method kao--input-method))))

(defun kao--insert-begin (restore)
  "Open an insert session: undo group + flip to insert state.
RESTORE is the Append step-back flag.  The caller positions point and sets
`kao--insert-start' / `kao--insert-secondary-sites' itself.

Signals `buffer-read-only' BEFORE any state flip, faithful to Kakoune's
Insert-mode constructor calling `throw_if_read_only' up front
\(input_handler.cc:1189; \"buffer is read-only\", buffer.cc:137).  For `c'
this fires after the yank, exactly as Kakoune's change yanks before the
constructor throws."
  (barf-if-buffer-read-only)
  (kao--open-undo-group)
  (setq kao--insert-restore restore
        kao--normal-active nil
        kao--insert-active t)
  (kao--clear-overlays)
  (deactivate-mark t)                   ; force: we manage `mark-active' directly
  (kao--apply-cursor 'insert)
  (kao--input-method-enter-insert)      ; restore the user's input method
  (when kao-normal-state-exit-hook (run-hooks 'kao-normal-state-exit-hook))
  (when kao-insert-state-entry-hook (run-hooks 'kao-insert-state-entry-hook)))

(defun kao--free-insert-bounds ()
  "Free and clear the insert-session bounds markers.
The `kao--insert-start'/`kao--insert-end' pair, set to point at nil."
  (when kao--insert-start
    (set-marker kao--insert-start nil)
    (setq kao--insert-start nil))
  (when kao--insert-end
    (set-marker kao--insert-end nil)
    (setq kao--insert-end nil)))

(defun kao--insert-bounds-open ()
  "Open the insert-session bounds markers at point (the single create seam).
Frees any stale pair via `kao--free-insert-bounds', then sets
`kao--insert-start' to a plain marker at point and `kao--insert-end' to the
insertion-type t marker that rides forward past text typed there.  That
load-bearing t makes `kao-insert-exit' measure the net typed run and ignore a
native End key (`end-of-line') that only moves point; it
lives in exactly ONE place, here.  `kao--free-insert-bounds' stays the single
free seam."
  (kao--free-insert-bounds)
  (setq kao--insert-start (copy-marker (point))
        kao--insert-end (copy-marker (point) t)))

(defun kao--free-insert-anchors ()
  "Free and clear the insert-session anchor markers.
The secondary sites (each MARK and its ANCHOR) and the main anchor
\(`kao--insert-secondary-sites'/`kao--insert-main-anchor')."
  (dolist (site kao--insert-secondary-sites)
    (set-marker (car site) nil)
    (when (cdr site) (set-marker (cdr site) nil)))
  (when kao--insert-main-anchor (set-marker kao--insert-main-anchor nil))
  (setq kao--insert-secondary-sites nil
        kao--insert-main-anchor nil))

(defun kao--insert-setup-abort (fresh)
  "Abort a half-open insert session after a setup error.
FRESH non-nil means the failing entry OPENED the undo group (a normal-state
entry): the group is cancelled — restoring the buffer text to the pre-entry
state, the `atomic-change-group' idiom — the pending capture is
discarded (the cancelled edits and their compensating undos cancel out, and
pending was empty at entry since every normal-state post-command commits
it), the session markers are freed, and normal state is restored.  When the
entry ran NESTED inside a live session (a one-shot `<a-;>' edit —
the handle pre-existed), this does NOTHING: the outer session owns the
group, its partial edit stays in that one undo unit, and freeing the marks
would break the outer replay; `kao--oneshot-pop' reconciles the flags
on the next post-command."
  (when fresh
    (when kao--insert-undo-handle
      (cancel-change-group kao--insert-undo-handle)
      (setq kao--insert-undo-handle nil))
    (setq kao--hist-pending nil)
    (kao--free-insert-anchors)
    (kao--insert-beacons-detach)          ; aborted setup: tear down the beacons
    (kao--free-insert-bounds)
    (setq kao--insert-active nil
          kao--normal-active t
          kao--insert-restore nil)
    (kao--apply-cursor 'normal)
    (kao--input-method-enter-normal)    ; back to normal: drop the input method
    (kao--refresh)
    (when kao-insert-state-exit-hook (run-hooks 'kao-insert-state-exit-hook))
    (when kao-normal-state-entry-hook
      (run-hooks 'kao-normal-state-entry-hook))))

(defmacro kao--with-insert-setup (restore &rest body)
  "Open an insert session via `kao--insert-begin', then run the setup BODY.
RESTORE is the Append step-back flag.  BODY does the enter-time positioning
and edits (the o/O newline opens, the `c' deletes) and the session-site
setup.  If BODY signals — the realistic class is `text-read-only' from a
read-only text property, which Kakoune's buffer-wide `throw_if_read_only'
guard cannot anticipate — the half-open session is aborted via
`kao--insert-setup-abort' before the signal propagates.  The
handler condition is the catch-all t, so a quit mid-setup cannot leak the
session either.  FRESH (did THIS entry open the undo group?) is captured before
`kao--insert-begin', whose `kao--open-undo-group' is a no-op on a live
handle (the nested one-shot case)."
  (declare (indent 1) (debug (form body)))
  (let ((fresh (gensym "kao--insert-fresh")))
    `(let ((,fresh (null kao--insert-undo-handle)))
       (kao--insert-begin ,restore)
       (condition-case err
           (progn ,@body)
         (t (kao--insert-setup-abort ,fresh)
            (signal (car err) (cdr err)))))))

(defun kao--insert-anchor-marker (shape sel)
  "Marker carrying SEL's shaped insert-exit anchor for SHAPE, or nil to collapse.
Kakoune `prepare' reshapes each selection at insert entry (input_handler.cc:
1456-1533): Insert keeps the original span (SHAPE `keep-max' -> old max, on a
type-t marker so it rides forward past text typed before it), Append covers
original plus typed (`keep-min' -> old min).  Every other mode collapses, so a
nil SHAPE returns nil and `kao-insert-exit' stands the cursor alone."
  (cond ((eq shape 'keep-max) (copy-marker (kao-sel-max sel) t))
        ((eq shape 'keep-min) (copy-marker (kao-sel-min sel) nil))))

(defun kao--enter-insert (mode position-fn &optional site-fn)
  "Enter insert state for MODE, then run POSITION-FN to place point.
MODE is a symbol (insert/append/insert-line-begin/append-line-end/open-below/
open-above).  POSITION-FN moves point to the MAIN selection's insertion site
and, for o/O, inserts the newline — all inside the undo group.

When SITE-FN is non-nil and there is more than one selection, the insert sites
of the NON-main selections (SITE-FN applied to each `kao-sel') are promoted to
insertion-type-t markers in `kao--insert-secondary-sites'; the net text typed at
the main is replayed there on exit (record-and-replay-on-exit).  The
selection list is suspended until `kao-insert-exit' rebuilds it.

The Kakoune `prepare' shape per mode rides alongside the
insert markers: `i' keeps the original span (`keep-max'), `a' covers
original+typed (`keep-min'), and the rest collapse — a shaped anchor marker is
carried for each selection so `kao-insert-exit' can rebuild the faithful span."
  (kao--with-insert-setup (eq mode 'append)
    (let* ((sels (kao-sels-list kao--sels))
           (main-idx (kao-sels-main kao--sels))
           (shape (cond ((eq mode 'insert) 'keep-max)
                        ((eq mode 'append) 'keep-min))))
      ;; Shaped anchor for the main selection (nil = collapse at exit).
      (setq kao--insert-main-anchor
            (kao--insert-anchor-marker shape (nth main-idx sels)))
      ;; Capture the secondary sites from the live geometry — each a
      ;; (MARK . shaped-ANCHOR-or-nil) cons, so the pairing is structural.
      (setq kao--insert-secondary-sites
            (when (and site-fn (cdr sels))
              (cl-loop for s in sels for i from 0
                       unless (= i main-idx)
                       collect (cons (copy-marker (funcall site-fn s) t)
                                     (kao--insert-anchor-marker shape s)))))
      ;; Beacon the pending secondary sites for the session.
      (kao--insert-beacons-attach (mapcar #'car kao--insert-secondary-sites))
      (funcall position-fn)
      (kao--insert-bounds-open))))

(defun kao--enter-insert-at-sites (site-marks main-idx)
  "Place the insert session at SITE-MARKS — one marker per selection, sel order.
The MAIN-IDX marker becomes point and `kao--insert-start'; the rest become
`kao--insert-secondary-sites' for replay on exit.  The session (undo
group + state flip) must already be open via `kao--insert-begin', and the caller
has done any enter-time edits (delete for `c', newlines for o/O)."
  (let ((main-site (nth main-idx site-marks)))
    ;; o/O/c collapse at exit (`prepare(Replace)' and the open modes leave no
    ;; anchor, input_handler.cc:1456-1533) — every site's ANCHOR is nil.
    (setq kao--insert-main-anchor nil
          kao--insert-secondary-sites
          (cl-loop for m in site-marks for i from 0
                   unless (= i main-idx) collect (cons m nil)))
    ;; Beacon the pending secondary sites for the session.
    (kao--insert-beacons-attach (mapcar #'car kao--insert-secondary-sites))
    (goto-char (marker-position main-site))
    (set-marker main-site nil)
    (kao--insert-bounds-open)))

(defun kao--insert-cursor (site typed)
  "Cursor for an insert SITE whose replayed text has length TYPED.
Non-Append modes leave the cursor one char AFTER the typed text (`site' + TYPED
-- Kakoune has no restore-cursor there, so `i'/c/A/I/o/O land on the char after
the run, input_handler.cc:1217-1225); Append steps back onto the last typed char
\(`site' + TYPED - 1), or onto the char before the append point when nothing was
typed (`m_restore_cursor', :1177).  The shaped anchor is rebuilt separately by
`kao-insert-exit'."
  ;; Returns `site' + TYPED (minus 1 on the Append/restore path) UNCLAMPED: at
  ;; buffer end a collapse-mode c/A/I/o/O `site' + TYPED lands one past
  ;; `point-max'.  Buffer-end safety is deliberately delegated to the universal
  ;; `kao--clamp-sel' on install (`kao-insert-exit's materialize step), NOT
  ;; guarded here — a local end-guard would double-clamp what the install
  ;; already clamps.  The collapse-mode buffer-end pins in
  ;; test/kao-state-tests.el hinge on exactly this delegation; comment, not
  ;; `cl-assert', since the exit path is hot and those pins enforce it.
  (if kao--insert-restore (+ site typed -1) (+ site typed)))

(defun kao-insert-exit ()
  "Leave insert state, rebuilding each selection to its Kakoune `prepare' shape.
The net text typed at the main is replayed at every secondary insert site
\, inside the one insert undo group.  Each selection is rebuilt to the
per-mode shape carried since entry: `i' keeps the original
span (backward, cursor at its min), `a' covers original+typed with the cursor on
the last typed char, and c/A/I/o/O collapse one char after the typed text.  With
no secondary sites this is the single-selection exit."
  (interactive)
  (kao--assert-mode)
  (let* ((start (and kao--insert-start (marker-position kao--insert-start)))
         (end (and kao--insert-end (marker-position kao--insert-end)))
         ;; Net = the marker-bracketed typed run: the
         ;; type-t end marker grew with text typed at point but ignored a
         ;; native End/C-e that only moved point, so pre-existing content
         ;; between the run and a wandered point is excluded.
         (net (if (and start end (> end start)) (buffer-substring start end) ""))
         (len (length net))
         ;; Cursor from the typed run (`start' + net), not point — robust to a
         ;; native motion mid-insert, so the shape holds (x).
         (main-cur (copy-marker (kao--insert-cursor (or start (point)) len)))
         ;; (cursor-marker . anchor-marker-or-nil), main first; positions are
         ;; read after every replay so cross-site marker shifts settle.
         (pairs (list (cons main-cur kao--insert-main-anchor))))
    ;; Record the net text so `.' (`kao-repeat-insert') can replay this session.
    (setq kao--last-insert-text net)
    ;; Replay the net text at each secondary, pairing its cursor with its anchor.
    (cl-loop for (m . am) in kao--insert-secondary-sites
             do (let ((site (marker-position m)))
                  (goto-char site)
                  (insert net)
                  (push (cons (copy-marker (kao--insert-cursor site len)) am) pairs)
                  (set-marker m nil)))
    (setq kao--insert-secondary-sites nil
          kao--insert-main-anchor nil)
    (kao--insert-beacons-detach)          ; session over: tear down the beacons
    (kao--close-undo-group)
    (kao--free-insert-bounds)
    (setq kao--insert-active nil
          kao--normal-active t
          kao--insert-restore nil)
    (kao--apply-cursor 'normal)
    (kao--input-method-enter-normal)    ; back to normal: drop the input method
    ;; Materialize each (cursor . anchor) marker pair (main first) into a shaped,
    ;; clamped selection; a nil anchor collapses onto the cursor.  This
    ;; `kao--clamp-sel' is the SINGLE buffer-end guard for the exit:
    ;; `kao--insert-cursor' returned `site' + TYPED(-1) unclamped, so a
    ;; collapse-mode cursor one past `point-max' is pulled back to a valid
    ;; position here — the buffer-end pins in test/kao-state-tests.el hinge on it.
    (let ((sels (mapcar (lambda (pair)
                          (let* ((cm (car pair)) (am (cdr pair))
                                 (c (marker-position cm))
                                 (a (if am (marker-position am) c)))
                            (set-marker cm nil)
                            (when am (set-marker am nil))
                            (kao--clamp-sel (kao-sel-make :anchor a :cursor c))))
                        (nreverse pairs))))
      ;; direct install (retag-order): sorts (no merge) + retags id BEFORE the
      ;; refresh — the -raw seam skips the sort and would refresh before the
      ;; retag.
      (setq kao--sels (kao-sels-sort (kao-sels-make :list sels :main 0)))
      ;; The session already committed inline (`kao--close-undo-group' above),
      ;; so the id has advanced; the rebuilt list is in that new frame — tag it
      ;; there so `kao--refresh' does not fold it through the insert.
      (kao--sels-retag)
      (kao--refresh)))
  (when kao-insert-state-exit-hook (run-hooks 'kao-insert-state-exit-hook))
  (when kao-normal-state-entry-hook (run-hooks 'kao-normal-state-entry-hook)))

;;;; One-shot normal command from insert (`<a-;>')

;; Kakoune's insert-mode `<a-;>' pushes a single-command Normal mode
;; (input_handler.cc:1388-1392): ONE normal command runs with the insert
;; session still open (same undo group, same `m_last_insert' recording),
;; then the mode pops back to insert.  Digits, Backspace and `"' are params
;; keys and do not pop (:304-336 are handled before the
;; `pop_if_single_command' scope, :339-344); any other key — a dispatched
;; command or an unmapped one such as Escape — pops after it runs.
;;
;; kao realization (family): the selection list is suspended during
;; insert, so entry SYNCS `kao--sels' from the live session geometry (main
;; collapsed at point, secondaries collapsed at their insert marks) and the
;; pop-back re-derives the geometry from whatever the command left behind —
;; unless nothing changed, in which case point/start/marks round-trip
;; verbatim.  The net-text replay measures from `kao--insert-start',
;; so a one-shot that moves or edits restarts the measurement: text typed
;; before it is already placed at the main only (the same documented
;; approximation class as backspacing across the insert start).

(defun kao--oneshot-sync-sels ()
  "Set `kao--sels' from the live insert-session geometry.
Main = collapsed at point; each secondary = collapsed at its
`kao--insert-secondary-sites' MARK position, woven back at the suspended main
index.  This is the accommodation for Kakoune's always-live mid-insert
selections, not a selection change."
  (let* ((marks (mapcar (lambda (site) (marker-position (car site)))
                        kao--insert-secondary-sites))
         (main-idx (min (kao-sels-main kao--sels) (length marks)))
         (curs (append (cl-subseq marks 0 main-idx)
                       (list (point))
                       (nthcdr main-idx marks))))
    ;; direct install (no-refresh): sorts (no merge) and does NOT refresh — a
    ;; geometry sync; the -raw seam would refresh here.
    (setq kao--sels
          (kao-sels-sort
           (kao-sels-make
            :list (mapcar (lambda (p)
                            (kao--clamp-sel
                             (kao-sel-make :anchor p :cursor p)))
                          curs)
            :main main-idx)))))

(defun kao-insert-one-shot ()
  "Kakoune `<a-;>': run a single normal command, then return to insert.
The insert session stays open — same undo unit, same history node,
same `.'-replay recording — exactly as Kakoune pushes a single-command
Normal mode over insert (input_handler.cc:1388-1392).  Count digits,
Backspace and the `\\='\"' register prefix may precede the command without
consuming the one shot; any other key (Escape included) pops back to insert
after it runs (`kao--oneshot-pop' via `kao--maybe-reset-count')."
  (interactive)
  (kao--assert-mode "kao-insert-one-shot") ; mode-off names the cause
  (unless kao--insert-active
    (user-error "no insert session"))
  (kao--oneshot-sync-sels)
  (setq kao--insert-oneshot (list (point)
                                  (buffer-chars-modified-tick)
                                  (kao--snapshot-sels kao--sels)))
  (setq kao--insert-active nil
        kao--normal-active t)
  (kao--apply-cursor 'normal)
  (kao--input-method-enter-normal)      ; one-shot normal: drop the input method
  (kao--refresh)
  (when kao-insert-state-exit-hook (run-hooks 'kao-insert-state-exit-hook))
  (when kao-normal-state-entry-hook (run-hooks 'kao-normal-state-entry-hook)))

(defun kao--oneshot-pop ()
  "Return to insert state after the one-shot normal command.
When the command re-entered insert itself (`<a-;> i' — Kakoune's nested
push), the entry already rebuilt the session geometry: only the flag
clears.  Documented deviation: Kakoune keeps a mode STACK, so its nested
insert pops back through the single-command Normal to the OUTER insert
\(`State::PopOnEnabled', input_handler.cc:246-249/:342-343) and a second
Escape is needed; kao merges into ONE session, so one Escape ends it.
When nothing changed (same `buffer-chars-modified-tick', same
selections — e.g. `<a-;> ESC'), point and the session geometry round-trip
verbatim.  Otherwise the session re-derives from the new selections: point
at the main cursor, `kao--insert-start' restarted there, secondary insert
sites at the non-main cursors."
  (let ((entry kao--insert-oneshot))
    (setq kao--insert-oneshot nil)
    (unless kao--insert-active
      (let ((unchanged
             (and (= (buffer-chars-modified-tick) (nth 1 entry))
                  (equal (kao-sels-list kao--sels)
                         (kao-sels-list (nth 2 entry)))
                  (= (kao-sels-main kao--sels)
                     (kao-sels-main (nth 2 entry))))))
        (if unchanged
            (goto-char (nth 0 entry))
          (let ((main-idx (kao-sels-main kao--sels)))
            (goto-char (kao-sel-cursor (nth main-idx (kao-sels-list kao--sels))))
            ;; Free the previous session's secondary sites — the MARKs and the
            ;; shaped ANCHORs carried since entry, which the re-derived
            ;; (collapsed) selections no longer correspond to; drop the anchors
            ;; and let the final exit collapse to match the new geometry
            ;;.  `kao--free-insert-anchors' is the single free seam
            ;; (frees the sites + main anchor, clears both); rebuild right after.
            (kao--free-insert-anchors)
            (setq kao--insert-secondary-sites
                  (cl-loop for s in (kao-sels-list kao--sels) for i from 0
                           unless (= i main-idx)
                           collect (cons (copy-marker (kao-sel-cursor s) t) nil)))
            (kao--insert-bounds-open))))
      (setq kao--insert-active t
            kao--normal-active nil)
      (kao--clear-overlays)
      (deactivate-mark t)
      (kao--apply-cursor 'insert)
      (kao--input-method-enter-insert)  ; back to insert: restore the input method
      (when kao-insert-state-entry-hook
        (run-hooks 'kao-insert-state-entry-hook)))))

(defun kao--assert-mode (&optional context)
  "Signal `user-error' unless `kao-mode' is active in this buffer.
The shared guard for M-x-discoverable kao commands : with
the mode off, `kao--sels' is nil and a selection-reading command otherwise
dies far downstream with a low-level type error that names neither the
command nor the cause.  Call this first in every user-facing interactive
command that reads selection or mode state.
Optional CONTEXT, a string, is prefixed as \"CONTEXT: \" so a specific
command can name itself in the mode-off message; with no CONTEXT the message
is the bare contract string, unchanged."
  (unless (bound-and-true-p kao-mode)
    (if context
        (user-error "%s: kao-mode is not active in this buffer" context)
      (user-error "kao-mode is not active in this buffer"))))

(define-minor-mode kao-mode
  "Faithful Kakoune editing model for Emacs (P0 spike: render + motions)."
  :lighter (:eval (kao--lighter))
  (if kao-mode
      (progn
        (cl-pushnew 'kao--emulation-mode-map-alist emulation-mode-map-alists)
        ;; Terminal ESC decoding — kao is unusable in -nw without it.
        ;; Idempotent; never auto-disabled (other buffers may still use it).
        (unless kao-esc-mode (kao-esc-mode 1))
        ;; Per-major-mode bindings shadow the normal map via a buffer-local
        ;; alist value (no-op local fallback when none match).
        (kao--refresh-mode-maps)
        (setq kao--normal-active t)
        ;; Assert the region substrate buffer-locally:
        ;; hybrid draws the main body with the native `region' face, which
        ;; shows only under `transient-mark-mode'.  A user who disabled the global
        ;; would otherwise get an invisible main selection; evil's visual state
        ;; makes the same assertion.  Killed on disable (below) to restore the
        ;; buffer's inherited value.
        (setq-local transient-mark-mode t)
        ;; direct install (no-refresh): mode-enable seed — kao--refresh must run
        ;; AFTER kao-history-init + the sel-history seeding below, so the -raw
        ;; seam (which refreshes here) would fire too early.
        (setq kao--sels (kao-sels-make
                         :list (list (kao--clamp-sel
                                      (kao-sel-make :anchor (point) :cursor (point))))
                         :main 0))
        ;; Seed the buffer history tree + install modification capture
        ;; BEFORE the selection-history seed: ring entries are tagged with
        ;; the history id they were recorded at, so the tree must
        ;; exist first.
        (kao-history-init)
        (add-hook 'before-change-functions #'kao--hist-before-change nil t)
        (add-hook 'after-change-functions #'kao--hist-after-change nil t)
        ;; Pin the saved history id on every save so the modified flag tracks
        ;; the tree: an undo back to the saved node clears it.
        (add-hook 'after-save-hook #'kao-history-mark-saved nil t)
        ;; And on every revert (external reload / auto-revert / magit checkout):
        ;; commit the reload diff as the reload node and re-pin the saved id to
        ;; it — Kakoune `Buffer::reload' re-pins `m_last_save_history_id'
        ;; (buffer.cc:287) so the modified flag stays correct on every later
        ;; undo/redo.  The buffer-local hook fires on the
        ;; preserve-modes revert path (auto-revert/magit), which keeps locals; a
        ;; non-preserve-modes revert kills locals and re-inits kao-mode, which is
        ;; self-consistent (a fresh tree, saved at its root).
        (add-hook 'after-revert-hook #'kao-history-mark-saved nil t)
        ;; Seed the selection-history ring with the initial list (node 0;
        ;; ring storage — capacity fixed at enable time; entries are
        ;; ((GEN . ID) . SNAP)).
        (setq kao--sel-history (make-vector (max 2 kao-sel-history-max) nil)
              kao--sel-history-start 0
              kao--sel-history-size 1
              kao--sel-history-index 0)
        (aset kao--sel-history 0
              (kao--sel-ring-make (kao--genid-make (kao-history-generation)
                                                   (kao-history-current-id))
                                  (kao--snapshot-sels kao--sels)))
        ;; Seed the live-list tag at the root node: the
        ;; selections are valid against the current buffer content.
        (kao--sels-retag)
        (setq kao--sels-edit-pending nil)
        (kao--apply-cursor 'normal)
        ;; Per-state input method (hel-study-1): save what is active, watch for
        ;; the user (de)activating a method, and enter normal state with it
        ;; deactivated so normal keys are not swallowed.
        (setq kao--input-method current-input-method)
        (add-hook 'input-method-activate-hook
                  #'kao--input-method-activated nil t)
        (add-hook 'input-method-deactivate-hook
                  #'kao--input-method-deactivated nil t)
        (kao--without-input-method-hooks (deactivate-input-method))
        ;; Global, idempotent; the sync body no-ops while both color
        ;; defcustoms are nil, and the hook fires only on buffer changes
        ;; in windows — never on the keystroke hot path.
        (add-hook 'window-buffer-change-functions #'kao--cursor-color-sync)
        (setq kao--count 0)
        (add-hook 'post-command-hook #'kao--refresh nil t)
        ;; Added right after `kao--refresh' so it runs just BEFORE it (and after
        ;; `kao--hist-maybe-commit'): fold the live list through a foreign
        ;; non-command edit once its mods are committed, before the mirror draws.
        (add-hook 'post-command-hook #'kao--sels-sync nil t)
        (add-hook 'post-command-hook #'kao--maybe-reset-count nil t)
        (add-hook 'post-command-hook #'kao--sel-history-record nil t)
        (add-hook 'post-command-hook #'kao--hist-maybe-commit nil t)
        ;; LAST added = FIRST run (`add-hook' prepends): the foreign sync
        ;; must rewrite `kao--sels' before the recorder and the
        ;; mirror see it.
        (add-hook 'post-command-hook #'kao--foreign-sync nil t)
        ;; Re-render secondaries over the viewport redisplay scrolls to, so a
        ;; recenter after point moves off-screen does not leave the newly
        ;; revealed region undrawn until the next key.
        (add-hook 'window-scroll-functions #'kao--render-on-scroll nil t)
        (kao--refresh)
        (when kao-normal-state-entry-hook
          (run-hooks 'kao-normal-state-entry-hook)))
    (when kao--insert-undo-handle (kao--close-undo-group))
    (kao--free-insert-bounds)
    (kao--free-insert-anchors)
    (kao--insert-beacons-detach)          ; disable mid-insert: no dangling beacons
    (when (and kao--normal-active kao-normal-state-exit-hook)
      (run-hooks 'kao-normal-state-exit-hook))
    (setq kao--normal-active nil
          kao--insert-active nil
          kao--insert-restore nil
          kao--insert-oneshot nil)
    (kill-local-variable 'kao--emulation-mode-map-alist)
    ;; Restore the buffer's inherited `transient-mark-mode' (
    ;): kao asserted it buffer-locally on enable; hand it back on disable.
    (kill-local-variable 'transient-mark-mode)
    (setq-local cursor-type 'box)
    (kao--cursor-color-apply nil)        ; restore the frame's original color
    ;; Detach the per-state IME hooks and restore the user's input method
    ;; (hel-study-1) — remove first so the restore does not re-enter them.
    (remove-hook 'input-method-activate-hook #'kao--input-method-activated t)
    (remove-hook 'input-method-deactivate-hook #'kao--input-method-deactivated t)
    (activate-input-method kao--input-method)
    (setq kao--count 0)
    (remove-hook 'post-command-hook #'kao--refresh t)
    (remove-hook 'post-command-hook #'kao--sels-sync t)
    (remove-hook 'post-command-hook #'kao--maybe-reset-count t)
    (remove-hook 'post-command-hook #'kao--sel-history-record t)
    (remove-hook 'post-command-hook #'kao--hist-maybe-commit t)
    (remove-hook 'post-command-hook #'kao--foreign-sync t)
    (remove-hook 'window-scroll-functions #'kao--render-on-scroll t)
    (remove-hook 'before-change-functions #'kao--hist-before-change t)
    (remove-hook 'after-change-functions #'kao--hist-after-change t)
    (remove-hook 'after-save-hook #'kao-history-mark-saved t)
    (remove-hook 'after-revert-hook #'kao-history-mark-saved t)
    (setq kao--sel-history nil
          kao--sel-history-start 0
          kao--sel-history-size 0
          kao--sel-history-index 0
          kao--hist nil
          kao--hist-pending nil
          kao--hist-bc nil
          kao--hist-saved-id nil)
    (kao--clear-overlays)
    ;; Mode-disable teardown: clears kao--sels/id/edit-pending — not an install.
    (setq kao--sels nil
          kao--sels-id nil
          kao--sels-edit-pending nil)
    (deactivate-mark t)))

(provide 'kao-state)
;;; kao-state.el ends here
