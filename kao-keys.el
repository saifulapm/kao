;;; kao-keys.el --- Every default kao binding, table-driven -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The single home of every default key binding.  Each state's
;; defaults are one data table (`kao-keys-normal-alist',
;; `kao-keys-insert-alist', `kao-keys-user-alist') applied at load by
;; `kao-keys--apply' — one place to read the whole layout, one place for
;; users to remap, and the substrate for a future non-Alt layer.
;;
;; What does NOT live here (mechanism, not default bindings): the keymap
;; defvars themselves with `suppress-keymap' + the `self-insert-command'
;; remap (buffer integrity, kao-state.el); the per-major-mode
;; `kao-define-key' machinery and the terminal-ESC `input-decode-map'
;; filter (kao-state.el); and the one-shot `read-key' DISPATCH tables
;; of the goto/view/object/combine/register menus (those are
;; menus, not keymaps).
;;
;; Keys are `kbd' strings; values are command symbols (`undefined' keeps
;; RET inert).  Kakoune's `<a-…>' is Emacs `M-…'.

;;; Code:

(require 'kao-state)
(require 'kao-motion)
(require 'kao-edit)
(require 'kao-multi)
(require 'kao-object)
(require 'kao-menu)
(require 'kao-search)
(require 'kao-pipe)
(require 'kao-macro)

;; xref is a built-in; these are autoloaded, but declare them so the
;; byte-compiler is quiet about the forward reference in the SPC d/r wrappers.
(declare-function xref-find-definitions "xref" (identifier))
(declare-function xref-find-references "xref" (identifier))

;; `SPC d'/`SPC r' — xref jumps that push the jump list first, so `C-o'
;; (`kao-jump-backward') returns to the pre-jump selections.  Thin wrappers over
;; `kao-jump-push' (silent) + the interactive xref command; the landing collapse
;; is handled by the foreign-sync automatically.
(defun kao-xref-find-definitions ()
  "Push the jump list, then run `xref-find-definitions' (`SPC d').
`kao-jump-push' records the pre-jump selections silently so `kao-jump-backward'
\(`C-o') returns here after the definition jump."
  (interactive)
  (kao-jump-push)
  (call-interactively #'xref-find-definitions))

(defun kao-xref-find-references ()
  "Push the jump list, then run `xref-find-references' (`SPC r').
See `kao-xref-find-definitions' — the same push-then-jump so `C-o' returns
\."
  (interactive)
  (kao-jump-push)
  (call-interactively #'xref-find-references))

(defconst kao-keys-normal-alist
  '(; -- params: count digits, count divide, params reset
    ("0" . kao-digit) ("1" . kao-digit) ("2" . kao-digit) ("3" . kao-digit)
    ("4" . kao-digit) ("5" . kao-digit) ("6" . kao-digit) ("7" . kao-digit)
    ("8" . kao-digit) ("9" . kao-digit)
    ("DEL" . kao-count-backspace)
    ("<escape>" . kao-normal-escape)
    ("RET" . undefined)                       ; Kakoune leaves <ret> unmapped
    ;; -- register select prefix
    ("\"" . kao-select-register)
    ;; -- motions: hjkl + words + line (normal.cc:2525-2537)
    ("h" . kao-left)            ("j" . kao-down)
    ("k" . kao-up)              ("l" . kao-right)
    ("w" . kao-word-forward)    ("b" . kao-word-backward)
    ("e" . kao-word-end)        ("x" . kao-line)
    ("H" . kao-extend-left)     ("J" . kao-extend-down)
    ("K" . kao-extend-up)       ("L" . kao-extend-right)
    ("W" . kao-extend-word-forward) ("B" . kao-extend-word-backward)
    ("E" . kao-extend-word-end)
    ("M-w" . kao-WORD-forward)  ("M-b" . kao-WORD-backward)
    ("M-e" . kao-WORD-end)
    ("M-W" . kao-extend-WORD-forward) ("M-B" . kao-extend-WORD-backward)
    ("M-E" . kao-extend-WORD-end)
    ;; -- arrows / Home / End (Kakoune's default keymap, main.cc:406-419):
    ;; Left/Right/Down/Up -> h/l/j/k, shift -> H/L/J/K extend, End/Home ->
    ;; `<a-l>'/`<a-h>' (select-line-end/begin), S-End/S-Home -> `<a-L>'/`<a-H>'.
    ;; Binding them HERE (not letting them fall through to native motion) keeps
    ;; multi-selection alive: a native arrow is a foreign command and the
    ;; `kao--foreign-sync' would collapse every secondary to one.
    ("<left>" . kao-left)       ("<right>" . kao-right)
    ("<down>" . kao-down)       ("<up>" . kao-up)
    ("S-<left>" . kao-extend-left)   ("S-<right>" . kao-extend-right)
    ("S-<down>" . kao-extend-down)   ("S-<up>" . kao-extend-up)
    ("<home>" . kao-select-line-begin) ("<end>" . kao-select-line-end)
    ("S-<home>" . kao-extend-line-begin) ("S-<end>" . kao-extend-line-end)
    ;; -- find char (f/t family) + repeat-select + matching
    ("f" . kao-find-to-char)    ("t" . kao-find-until-char)
    ("M-f" . kao-find-to-char-reverse) ("M-t" . kao-find-until-char-reverse)
    ("F" . kao-extend-to-char)  ("T" . kao-extend-until-char)
    ("M-F" . kao-extend-to-char-reverse) ("M-T" . kao-extend-until-char-reverse)
    ("M-." . kao-repeat-select)
    (";" . kao-reduce)          ("M-;" . kao-flip-selections)
    ("M-:" . kao-ensure-forward)
    (":" . kao-colon-hint)      ; no `:' command language -> hint at M-x
    ("m" . kao-select-matching) ("M-m" . kao-select-matching-backward)
    ("M" . kao-extend-matching) ("M-M" . kao-extend-matching-backward)
    ;; -- line-bound selects/extends
    ("M-l" . kao-select-line-end)   ("M-h" . kao-select-line-begin)
    ("M-L" . kao-extend-line-end)   ("M-H" . kao-extend-line-begin)
    ;; -- insert entries + repeat (kao-edit)
    ("i" . kao-insert)          ("a" . kao-append)
    ("I" . kao-insert-line-begin) ("A" . kao-append-line-end)
    ("o" . kao-open-below)      ("O" . kao-open-above)
    ("M-o" . kao-add-line-below) ("M-O" . kao-add-line-above)
    ("." . kao-repeat-insert)
    ;; -- yank/delete/change/paste (kao-edit)
    ("y" . kao-yank)            ("d" . kao-delete)
    ("c" . kao-change)
    ("M-d" . kao-delete-no-yank) ("M-c" . kao-change-no-yank)
    ("p" . kao-paste-after)     ("P" . kao-paste-before)
    ("R" . kao-replace)
    ("M-p" . kao-paste-all-after) ("M-P" . kao-paste-all-before)
    ("M-R" . kao-replace-all)
    ;; -- transforms (kao-edit)
    ("~" . kao-upcase)          ("`" . kao-downcase)
    ("M-`" . kao-swapcase)
    ("r" . kao-replace-char)
    (">" . kao-indent)          ("<" . kao-deindent)
    ("M->" . kao-indent-empty)  ("M-<" . kao-deindent-keep-incomplete)
    ("@" . kao-tabs-to-spaces)  ("M-@" . kao-spaces-to-tabs)
    ("&" . kao-align)           ("M-&" . kao-copy-indent)
    ("M-)" . kao-rotate-content-forward) ("M-(" . kao-rotate-content-backward)
    ("M-j" . kao-join-lines)    ("M-J" . kao-join-select-spaces)
    ;; -- history: buffer undo/redo + change-id nav
    ("u" . kao-undo)            ("U" . kao-redo)
    ("C-j" . kao-history-forward) ("C-k" . kao-history-backward)
    ;; -- selection history + jump list
    ("M-u" . kao-sel-undo)      ("M-U" . kao-sel-redo)
    ("C-s" . kao-jump-save)
    ("C-o" . kao-jump-backward) ("C-i" . kao-jump-forward)
    ;; -- multi-selection (kao-multi)
    ("%" . kao-select-whole-buffer) ("M-s" . kao-split-lines)
    ("s" . kao-select-regex)    ("S" . kao-split-regex)
    ("M-k" . kao-keep-matching) ("M-K" . kao-keep-not-matching)
    (")" . kao-rotate-selections-forward) ("(" . kao-rotate-selections-backward)
    ("," . kao-keep-selection)  ("M-," . kao-remove-selection)
    ("M-S" . kao-select-boundaries)
    ("C" . kao-copy-selections-down) ("M-C" . kao-copy-selections-up)
    ("Z" . kao-save-selections) ("z" . kao-restore-selections)
    ("M-z" . kao-combine-from-register) ("M-Z" . kao-combine-to-register)
    ("M-_" . kao-merge-consecutive) ("M-+" . kao-merge-overlapping)
    ("_" . kao-trim-selections) ("+" . kao-duplicate-selections)
    ;; -- objects (kao-object)
    ("M-i" . kao-select-inner)  ("M-a" . kao-select-whole)
    ("M-I" . kao-select-nested-inner) ("M-A" . kao-select-nested-whole)
    ("[" . kao-object-to-begin) ("]" . kao-object-to-end)
    ("M-[" . kao-object-inner-to-begin) ("M-]" . kao-object-inner-to-end)
    ("{" . kao-object-extend-to-begin) ("}" . kao-object-extend-to-end)
    ("M-{" . kao-object-extend-inner-to-begin)
    ("M-}" . kao-object-extend-inner-to-end)
    ;; -- goto / view menus + scrolling (kao-menu)
    ("g" . kao-goto)            ("G" . kao-goto-extend)
    ("v" . kao-view)            ("V" . kao-view-locked)
    ;; <c-u> half-page-up is deliberately NOT bound: `C-u' stays Emacs's
    ;; `universal-argument' (the `<a-x>'/M-x precedent,;
    ;; `kao-scroll-half-up' remains M-x-able/user-bindable).
    ("C-d" . kao-scroll-half-down)
    ("C-f" . kao-scroll-full-down) ("C-b" . kao-scroll-full-up)
    ("<next>" . kao-scroll-full-down) ("<prior>" . kao-scroll-full-up)
    ;; -- search (kao-search)
    ("/" . kao-search-forward)  ("M-/" . kao-search-backward)
    ("?" . kao-search-extend-forward) ("M-?" . kao-search-extend-backward)
    ("n" . kao-search-next)     ("N" . kao-search-next-add)
    ("M-n" . kao-search-prev)   ("M-N" . kao-search-prev-add)
    ("*" . kao-search-set-pattern) ("M-*" . kao-search-set-pattern-raw)
    ;; -- pipes (kao-pipe)
    ("|" . kao-pipe-replace)    ("M-|" . kao-pipe-ignore)
    ("!" . kao-pipe-insert-output) ("M-!" . kao-pipe-append-output)
    ("$" . kao-pipe-keep)
    ;; -- macros (kao-macro)
    ("Q" . kao-macro-record)    ("q" . kao-macro-play))
  "Default normal-state bindings: (`kbd' KEY-STRING . COMMAND), one row each.
The single declarative home of the normal-state layout;
applied to `kao-normal-state-map' at load by `kao-keys--apply'.  `SPC'
additionally carries the `kao-user-map' prefix (attached separately --
a keymap, not a command).")

(defconst kao-keys-insert-alist
  '(("<escape>" . kao-insert-exit)
    ("M-;" . kao-insert-one-shot)               ; one-shot normal
    ("C-r" . kao-insert-register))              ; insert register
  "Default insert-state bindings, applied to `kao-insert-state-map'.
Deliberately near-empty — insert state is native Emacs editing (the
DECIDED stance; no faithful erase keys).  `C-n'/`C-p' are
deliberately UNBOUND (reverting part of): the emulation map
outranks `minor-mode-overriding-map-alist', so a binding here shadowed
the completion popup's own keys (corfu binds cycling via a remap of
`next-line', which a shadowing binding never triggers).
Unbound, they are native `next-line'/`previous-line', and the popup remaps
cycle candidates when live — completion stays fully native Emacs.")

(defconst kao-keys-user-alist
  '(("#" . kao-comment-lines)                   ; comment toggle (user pref)
    ("d" . kao-xref-find-definitions)           ; xref (push-then-jump)
    ("r" . kao-xref-find-references))            ; xref (push-then-jump)
  "Default user-map bindings, applied to `kao-user-map'.
Kakoune ships no user mappings; kao ships three: `#' (comment toggling,
-2 — Kakoune's own idiom for comments is a user mapping, so the
user map is the faithful home; on `#' by user decision 2026-06-13) and
`d'/`r' (xref definitions/references, relocated from the goto menu when
the v1 \"free goto keys\" premise proved false — `g d'/`g u' are real
Kakoune display-line motions, normal.cc:303-325.  Extensions live HERE,
the user map being Kakoune's own extension point; the goto menu stays
pure-Kakoune).  No `x' -> `kao-crop-lines' row by user decision (\"cut no
need\", 2026-06-13); the command remains, reachable via
\\[execute-extended-command] or a user rebinding.  `SPC' is the plain
prefix into this map, so each key reaches its binding directly.")

(defconst kao-keys-prompt-alist
  '(("C-r" . kao-prompt-insert-register))       ; PromptMode <c-r>
  "Default prompt-map bindings, applied to `kao-prompt-map'.
The kao prompts (search/incsearch/pipe/object-desc) are Kakoune PromptMode
prompts; `C-r' register insert (input_handler.cc:758-780) is the ONE
faithful rebind (user-decided 2026-06-12) — every other prompt key
stays native (the prompt-keys stance).")

(defconst kao-keys-regex-prompt-alist
  '(("TAB" . completion-at-point))              ; regex_prompt word-db
  "Default regex-prompt-map bindings, applied to `kao-regex-prompt-map'.
Kakoune's `regex_prompt' completes the current word against a buffer
word-db (normal.cc:962-985); kao's translation is a buffer-word capf
\(`kao--regex-capf') triggered natively by TAB.  Regex prompts ONLY —
pipe completion is `read-shell-command''s own, object-desc has none
\(`complete_nothing', normal.cc:1489).")

(defun kao-colon-hint ()
  "Point at `M-x': kao has no `:' command language (Kakoune's command prompt).
kao deliberately drops the `:' command language — native Emacs replaces it
\(out of scope, permanently).  Bound to `:' so a reflexive
Kakoune `:' gives a pointer instead of a silent no-op."
  (interactive)
  (user-error "kao has no `:' command language -- use M-x (M-x eval-expression for elisp)"))

(defun kao-keys--apply (map alist)
  "Bind every (KEY-STRING . COMMAND) row of ALIST into keymap MAP.
KEY-STRING goes through `kbd'."
  (pcase-dolist (`(,key . ,def) alist)
    (define-key map (kbd key) def)))

(kao-keys--apply kao-normal-state-map kao-keys-normal-alist)
(kao-keys--apply kao-insert-state-map kao-keys-insert-alist)
(kao-keys--apply kao-user-map kao-keys-user-alist)
(kao-keys--apply kao-prompt-map kao-keys-prompt-alist)
(kao-keys--apply kao-regex-prompt-map kao-keys-regex-prompt-alist)

;; `SPC' is the user-mode prefix: a real Emacs prefix keymap, the
;; idiomatic translation of Kakoune's `map global user' (normal.cc:2231).
;; `which-key' lists its rows for free.
(define-key kao-normal-state-map (kbd "SPC") kao-user-map)

;;;; Opt-in: route the native Emacs undo keys into kao's history tree

;; The remap half of the de-scheduled native-undo work.  The full native-undo
;; teardown is not worth it (the double-bookkeeping cost measured ~5%/key,
;; failing the perf gate) — but this one piece is, and it needs none of the
;; teardown.  A `[remap undo]' in the high-priority emulation map catches EVERY
;; native undo key (`C-/', `C-x u', `C-_', `s-z') in one shot, whichever lower
;; map bound it, and routes it to kao's tree.  Off by default (Kakoune has no
;; native undo; the faithful keys are `u'/`U' + the change-id walk `C-j'/`C-k').

(defun kao--apply-native-undo-keys (enable)
  "Add (ENABLE non-nil) or remove the native-undo-key remaps.
Remaps `undo'/`undo-redo' to `kao-undo'/`kao-redo' in `kao-normal-state-map',
or removes those remaps when ENABLE is nil."
  (if enable
      (progn
        (define-key kao-normal-state-map [remap undo] #'kao-undo)
        (define-key kao-normal-state-map [remap undo-redo] #'kao-redo))
    (define-key kao-normal-state-map [remap undo] nil)
    (define-key kao-normal-state-map [remap undo-redo] nil)))

(defcustom kao-bind-native-undo-keys nil
  "When non-nil, route the native Emacs undo keys into kao's history tree.
Remaps `undo' and `undo-redo' to `kao-undo'/`kao-redo' in
`kao-normal-state-map', so the familiar `C-/' / `C-x u' (undo) and `undo-redo'
keys walk kao's buffer-history TREE — the same engine as `u'/`U' —
instead of native undo, which would desync kao's tree position and its
selection list.

Off by default: Kakoune has no native undo, so the faithful keys are `u'/`U'
\(plus the change-id walk `C-j'/`C-k').  Enable this to use the native Emacs
undo keys coherently alongside kao.  Only normal state is remapped; insert
state stays native Emacs editing."
  :type 'boolean
  :group 'kao
  :set (lambda (sym val)
         (set-default sym val)
         (kao--apply-native-undo-keys val)))

(provide 'kao-keys)
;;; kao-keys.el ends here
