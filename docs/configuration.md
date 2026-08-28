# Configuring kao

Everything here is public API. You can build a config layer (surround,
match-mode, increment, move-lines, tree-sitter objects) entirely on these
symbols without touching a private `kao--` internal or assigning to
`kao--sels` directly. `test/kao-config-substrate-tests.el` enforces that by
rebuilding surround add, delete and replace on the public surface alone.

Private names start with `kao--`. They can change without notice.

## Keymaps

Every default binding lives in one of five keymaps:

| Keymap | State |
| --- | --- |
| `kao-normal-state-map` | normal state |
| `kao-insert-state-map` | insert state, nearly empty since insert is plain Emacs |
| `kao-user-map` | the `SPC` leader, a prefix into this map |
| `kao-prompt-map` | the search, pipe and object prompts |
| `kao-regex-prompt-map` | the regex prompts only |

`kao-normal-state-map` is `suppress-keymap`'d, so any printable key you leave
unbound signals rather than inserting itself.

```elisp
(define-key kao-normal-state-map (kbd "x") #'kao-select-line-end)
```

To bind a key only in one major mode and the modes derived from it, use
`kao-define-key`. It takes effect immediately, including in buffers where
`kao-mode` is already on:

```elisp
(kao-define-key MODE KEY DEF &rest MORE)
(kao-define-key 'org-mode (kbd "T") #'org-todo)
```

The default binding tables are also public, as alists you can read or replace
before load: `kao-keys-normal-alist`, `kao-keys-insert-alist`,
`kao-keys-user-alist`, `kao-keys-prompt-alist`, `kao-keys-regex-prompt-alist`.

## The selection list

A selection is a `kao-sel` struct holding an `anchor` and a `cursor`, both
buffer positions. Build one with `(kao-sel-make :anchor A :cursor C)`. The list
is always sorted and always holds at least one selection, exactly as in
Kakoune.

| Function | What it does |
| --- | --- |
| `(kao-get-selections)` | returns the list as fresh copies, so mutating them cannot corrupt engine state |
| `(kao-set-selections SELS &optional MAIN)` | installs SELS, sorting, clamping and merging overlaps; MAIN is the main index |
| `(kao-set-selections-raw SELS &optional MAIN)` | installs verbatim, clamped but neither sorted nor merged, for deliberately overlapping lists |
| `(kao-map-selections FN)` | replaces each selection with `(funcall FN sel)`; a nil result drops that selection |
| `(kao-map-selections-extend FN)` | extends each selection toward FN's cursor, keeping the anchor; nil drops |

Both map functions leave the list untouched when FN drops every selection,
matching Kakoune's refusal to end up with nothing selected.

A motion is just a transform. This collapses every selection onto its cursor:

```elisp
(kao-map-selections
 (lambda (sel)
   (let ((c (kao-sel-cursor sel)))
     (kao-sel-make :anchor c :cursor c))))
```

Useful accessors: `kao-sel-anchor`, `kao-sel-cursor`, `kao-sel-min`,
`kao-sel-max`, `kao-sel-beg`, `kao-sel-end`, `kao-sel-length`,
`kao-sel-forward-p`, `kao-sel-flip`, `kao-sel-collapse`, `kao-sel-extend-to`,
`kao-sel-overlaps-p`.

## Editing at every selection

Two primitives run buffer edits at all N selections as a single undo unit. They
differ in what happens to the selections afterwards.

```elisp
(kao-edit-selections FN)          ; selections re-derived from FN's result
(kao-edit-keeping-selections FN)  ; selections carried across FN's edits
```

Use `kao-edit-selections` when the edit replaces the selected text, the way
paste, `R` and pipe-replace do. Use `kao-edit-keeping-selections` when you
insert around the selection and want the original span still selected
afterwards, which is what surround-add needs.

To run a body and then put the selections back as they were, Kakoune's `-draft`
idiom:

```elisp
(kao-with-saved-selections
  (goto-char (point-min))
  (insert "header\n"))
```

The endpoints are saved as insertion-type markers, so they track through edits
the body makes rather than restoring at stale positions. An `unwind-protect`
restores them and frees the markers even if the body signals or you quit.

## Text objects

`kao-object-register` puts an object on a key, reachable from `<a-i>`, `<a-a>`,
`[` and `]` and their extend variants:

```elisp
(kao-object-register CHAR SELECTOR &optional INFO NESTED-SELECTOR)
```

SELECTOR takes `(SEL INNER TO-BEGIN TO-END &optional LEVEL)` and returns a
`kao-sel` or nil. INFO is the row text for the object menu. NESTED-SELECTOR is
the optional `<a-I>` / `<a-A>` walk, taking `(BEG END INNER &optional LEVEL)`
and returning a list of `(FIRST . LAST)` spans.

For an open/close pair you do not have to write the selector yourself:

```elisp
(kao-object-make-pair-selector OPEN CLOSE)   ; two regexes

(kao-object-register
 ?x
 (kao-object-make-pair-selector "^[ \t]*" "[ \t]*\n")
 "line")
```

Regexes are Emacs syntax. See [regex-porting.md](regex-porting.md) if you are
translating patterns from a Kakoune config.

To find an object's extent without entering the object menu, which is how
surround delete and replace locate the enclosing pair:

```elisp
(kao-object-bounds KEY SEL &optional INNER LEVEL)   ; => kao-sel or nil
```

## Goto and view menus

```elisp
(kao-goto-define KEY KIND PAYLOAD DOC)
(kao-view-define KEY FN DOC)
```

Both upsert, so re-registering a key replaces its row and its menu text
together. KIND governs how PAYLOAD reaches its target:

| KIND | PAYLOAD | Behaviour |
| --- | --- | --- |
| `coord` | zero-argument function returning a buffer position | kao pushes a jump, then moves there, so `C-o` returns |
| `selector` | function taking a `kao-sel`, returning one | applied to every selection |
| `window` | zero-argument function | window-level target, no-op in an undisplayed buffer |
| `command` | interactive command | called interactively; you handle the selection yourself |
| `buffer` | zero-argument function returning a buffer | switches to it |

`kao-view-define`'s FN receives the count.

```elisp
(kao-goto-define ?m 'coord
                 (lambda () (save-excursion
                              (goto-char (point-min))
                              (forward-line (/ (count-lines (point-min) (point-max)) 2))
                              (point)))
                 "middle line")
(kao-view-define ?r (lambda (_n) (recenter 0)) "line to top")
```

`kao-jump-push` is the public, silent jump-list push if you are writing a
`command` target that should be returnable with `C-o`.

## Hooks

| Hook | Fires |
| --- | --- |
| `kao-insert-state-entry-hook` | on every entry into insert state, including the `<a-;>` one-shot |
| `kao-insert-state-exit-hook` | on every exit, after the multi-cursor replay commits |
| `kao-normal-state-entry-hook` | on entry into normal state |
| `kao-normal-state-exit-hook` | on exit from normal state |
| `kao-selection-change-hook` | when the selection list changes *value*, not on every command |
| `kao-register-modified-hook` | when a register is written |
| `kao-history-change-hook` | when the buffer history tree changes |
| `kao-reveal-functions` | abnormal hook, run with the cursor position, to open a fold the main cursor landed in |

Entry and exit are paired: a setup that aborts before insert is established
fires a matching exit rather than leaking an unpaired entry.

`kao-selection-change-hook` fires from the history recorder, which already
computes the value diff, so an unhooked buffer pays nothing on the motion hot
path. Every fire is guarded by `(when HOOK ...)`.

```elisp
(add-hook 'kao-insert-state-entry-hook
          (lambda () (electric-indent-local-mode 1)))
```

## Reading state

```elisp
(kao-current-state)   ; => nil | 'normal | 'insert  (nil when kao-mode is off)
(kao-normal-state-p)
(kao-insert-state-p)
(kao-refresh-cursor)  ; re-assert the cursor after a theme clobbers cursor-type
(kao-mode-line-string) ; Kakoune's mode_info, "" when kao is inactive
```

`kao-current-state` returning nil is how an extension knows to hand control
back. `kao-mode-line-string` reads only buffer-local state, so any mode-line
framework can call it; set `kao-lighter` to nil to hide the built-in lighter and
render it yourself.

## Reading one key

```elisp
(kao-on-key PROMPT FN &optional INFO-ROWS)
```

This is Kakoune's `on-key`. It routes through kao's own key reader, so a `Q`
macro recording stays in lockstep, which a bare `read-char` would break.
INFO-ROWS is an alist of `(KEY . DOC)` shown in an autoinfo box that is torn
down on exit, including on a quit.

## Registers

Registers are list-valued: a yank of N selections stores N strings, and `p`
pastes the i-th string to the i-th selection.

```elisp
(kao-register-get CHAR)          ; => list of strings, or nil
(kao-register-set CHAR STRINGS)
(kao-register-yank STRINGS &optional CHAR)
(kao-register-get-macro CHAR)
(kao-register-set-macro CHAR KEYS)
(kao-register-save-selections SELS &optional CHAR)
(kao-register-get-selections &optional CHAR)
(kao-register-define-dynamic CHAR GETTER &optional SETTER)
```

CHAR may also be a Kakoune long name as a string, so `"dquote"` reaches the same
register as `?\"`.

`kao-register-define-dynamic` adds a computed register. GETTER takes no
arguments and returns a list of strings; a nil SETTER makes the register
read-only and a write to it signals Kakoune's "this register is not assignable".

## Options

`M-x customize-group RET kao` lists all of them. The ones a config most often
reaches for:

| Option | Effect |
| --- | --- |
| `kao-global-modes` | which modes kao turns on in, overriding the heuristic in either direction |
| `kao-lighter` | mode-line lighter; nil hides it, a string replaces it |
| `kao-cursor-normal` / `kao-cursor-insert` | per-state cursor shape |
| `kao-cursor-color-normal` / `kao-cursor-color-insert` | per-state cursor color |
| `kao-search-count` | append a ` N/M` match count to the mode line |
| `kao-search-case-fold` | opt into case-insensitive search; nil is faithful |
| `kao-search-highlight` / `-idle` / `-max` | live match highlighting and its budget |
| `kao-use-prefix-arg-count` | let a native `C-u 3 w` count like `3 w` |
| `kao-motion-skip-invisible` | hop the cursor past folded text after a motion |
| `kao-open-indent` | make `o` / `O` indent the opened line |
| `kao-extra-word-chars` | extra characters counted as word constituents |
| `kao-matching-pairs` | the pairs `m` and the bracket objects match |
| `kao-indent-width` / `kao-align-tab` | indent and align behaviour |
| `kao-pipe-remote` | run the pipe keys on a TRAMP buffer's remote host |
| `kao-autoinfo` / `kao-pulse` / `kao-incsearch` | menu boxes, flash, live search preview |
| `kao-sel-history-max` / `kao-history-max-nodes` | history caps |
| `kao-bind-native-undo-keys` | route `C-/` and friends into kao's history tree |
| `kao-block-foreign-undo-visualizers` | refuse to open vundo / undo-tree in kao buffers |

## Deliberately absent

- No `:` command language and no `%val{}` / `%sh{}` expansion. Config reads live
  state directly in elisp.
- No `-no-hooks` switch. `let`-bind the hook variable to nil instead.
- No bundled key collection. kao is an editing engine, not a keys-everywhere
  layer.
