# kao

> The [Kakoune](https://kakoune.org) editing model in Emacs.

[![CI](https://github.com/saifulapm/kao/actions/workflows/ci.yml/badge.svg)](https://github.com/saifulapm/kao/actions/workflows/ci.yml)

Selection-first editing: you select, you see it, then you act. Multiple
selections are the only primitive. `w` selects a word, `d`
deletes every selection, `s` splits selections on a regex into one
cursor per match.

kao is not an emulation layer over Emacs commands. It owns a real
selection list as its single source of truth; the Emacs point and
region are a mirror of the main selection, never the other way around.

## Highlights

- **Faithful.** Ported line by line from the Kakoune C++ sources.
  Every deliberate deviation is documented, never silent
  ([docs/DIFFERENCES.md](docs/DIFFERENCES.md)).
- **Complete.** Motions, objects, the multi-selection algebra,
  goto/view menus, registers, macros, the jump list, selection
  history and counts all work like Kakoune everywhere Kakoune has
  them.
- **Fast.** Selection commands are pure list transforms, with no
  per-cursor command replay and no per-keystroke marker churn. A thousand
  cursors stay responsive.
- **Native.** Insert state is plain Emacs editing: your completion,
  your erase keys. Emacs's regex, undo, clipboard and minibuffer do
  the machinery; Kakoune semantics decide the behaviour.
- **Zero dependencies.** Emacs 29.1+.

## Installation

kao is not on MELPA yet, so install it straight from Git. It has **zero package
dependencies** (Emacs 29.1+), so there is nothing else to pull in. Releases are
tagged `vX.Y.Z`, so you can pin to a release instead of tracking every commit.

The minimal path is to run the `package-vc-install` form once interactively
(`M-x eval-expression`, or in the `*scratch*` buffer) to fetch the latest
release, then keep the `require` and `kao-global-mode` lines in your init file
so kao loads every session:

```elisp
;; one-time; run interactively (M-x eval-expression, or in *scratch*):
;; `:last-release' pins to the newest tag; drop it to track the latest commit.
(package-vc-install "https://github.com/saifulapm/kao" :last-release)
```

then, in your init file (loads every session):

```elisp
(require 'kao)
(kao-global-mode 1)
```

Or declare it with your package manager:

**Emacs 30+ built-in (`use-package` + `:vc`)**

```elisp
(use-package kao
  :vc (:url "https://github.com/saifulapm/kao" :rev :last-release)
  :config
  (kao-global-mode 1))
```

`:rev :last-release` pins to the newest tagged release; use `:rev :newest` to
track the latest commit instead.

**[Elpaca](https://github.com/progfolio/elpaca)**

```elisp
(elpaca (kao :host github :repo "saifulapm/kao")
  (require 'kao)
  (kao-global-mode 1))
```

Add `:ref "v1.0.2"` (the newest tag) to the recipe to pin a release; omit it to
track the default branch.

**[straight.el](https://github.com/radian-software/straight.el)**

```elisp
(straight-use-package '(kao :host github :repo "saifulapm/kao"))

(require 'kao)
(kao-global-mode 1)
```

straight tracks the default branch; run `M-x straight-freeze-versions` to lock
the exact commit you installed.

You start in normal state: `i` enters insert, `ESC` returns. If you
know Kakoune, your hands already know the rest. `<a-…>` keys are
Emacs `M-…`. If not, any [Kakoune
primer](https://github.com/mawww/kakoune) applies nearly verbatim.

## Customization

`M-x customize-group RET kao` lists every option. Two behaviour knobs worth
knowing:

- **`kao-open-indent`** (default off). When set, `o` / `O` indent the opened
  line with `indent-according-to-mode`, the native-Emacs stand-in for Kakoune's
  filetype indent hooks. Off keeps the faithful bare-core plain newline.
- **`kao-search-highlight-idle`** (default 0.05 s) is the idle delay before the
  first search-match highlight scan of a new pattern in a displayed buffer;
  repeats are memoized and free.

## Configuring

kao is meant to be reshaped from your init file. Everything below is a public
config seam: a keymap, a registrar, a hook, or a defcustom. The full API
reference is [docs/configuration.md](docs/configuration.md);
kao ships no bundled key collection, no evil-collection equivalent. You wire
in exactly what you want.

### Rebinding keys

Every default binding lives in one of five keymaps; rebind with `define-key`:

| Keymap | State |
| --- | --- |
| `kao-normal-state-map` | normal state |
| `kao-insert-state-map` | insert state (nearly empty; native Emacs editing) |
| `kao-user-map` | the `SPC` leader (a prefix into this map) |
| `kao-prompt-map` | the search / pipe / object prompts |
| `kao-regex-prompt-map` | the regex prompts only |

```elisp
(with-eval-after-load 'kao
  ;; Normal-state `x' selects the line; make it select to the line end instead.
  (define-key kao-normal-state-map (kbd "x") #'kao-select-line-end)
  ;; Add a leader entry: `SPC g' runs Magit.
  (define-key kao-user-map (kbd "g") #'magit-status))
```

`kao-normal-state-map` is `suppress-keymap`'d: any printable key you leave
unbound is a loud no-op, never `self-insert`. Bind it explicitly to give it a
command.

### Per-major-mode keys

`kao-define-key` binds keys only in buffers of a given major mode (and modes
derived from it); they shadow `kao-normal-state-map` there, and take effect
immediately even in buffers where `kao-mode` is already on:

```elisp
(with-eval-after-load 'kao
  (kao-define-key 'org-mode (kbd "T") #'org-todo))
```

### A text object

`kao-object-register` adds a text object on a key, reachable from the
object-pending menus (`<a-i>` / `<a-a>` / `[` / `]`). The selector has the
signature `(SEL INNER TO-BEGIN TO-END &optional LEVEL)` and returns a `kao-sel`
or nil. For an open/close pair, `kao-object-make-pair-selector` builds that
selector for you from two regexes. Here is an `x` object spanning the current
line. Its delimiters are `^[ \t]*` (line start plus leading blanks) and
`[ \t]*\n` (trailing blanks plus the newline); the full worked version is in
[docs/regex-porting.md](docs/regex-porting.md):

```elisp
(require 'kao-object)
(kao-object-register
 ?x
 (kao-object-make-pair-selector "^[ \t]*" "[ \t]*\n")
 "line")
```

### Hooks, state predicates, and defcustoms

The state hooks run code on every transition. To re-enable a minor mode
whenever you enter insert state:

```elisp
(add-hook 'kao-insert-state-entry-hook
          (lambda () (electric-indent-local-mode 1)))
```

The four hooks are `kao-insert-state-entry-hook` / `kao-insert-state-exit-hook`
and `kao-normal-state-entry-hook` / `kao-normal-state-exit-hook`;
`kao-selection-change-hook` fires when the selection list changes value, and
`kao-register-modified-hook` when a register is written.

Read kao's state from elisp with the public predicates:

```elisp
(kao-current-state)   ; => nil | 'normal | 'insert  (nil when kao-mode is off)
(kao-normal-state-p)  ; (kao-insert-state-p), the two common gates
(kao-refresh-cursor)  ; re-assert the cursor after a theme clobbers cursor-type
```

Notable defcustoms (all under `M-x customize-group RET kao`):

| Defcustom | Effect |
| --- | --- |
| `kao-lighter` | mode-line lighter; nil hides it, a string replaces it |
| `kao-cursor-normal` / `kao-cursor-insert` | per-state cursor shape (`box` / `bar`) |
| `kao-search-count` | append a ` N/M` match count to the mode line |
| `kao-use-prefix-arg-count` | let a native `C-u 3 w` count like `3 w` |
| `kao-motion-skip-invisible` | hop the cursor past invisible/folded text after a motion |

```elisp
(setopt kao-cursor-insert '(bar . 2)
        kao-search-count t)
```

`kao-lighter` also relocates the indicator: set it to nil to hide the built-in
lighter (kao-mode stays on) and render `(kao-mode-line-string)` from your own
segment instead. It reads only buffer-local state and returns `""` when kao is
inactive, so any modeline framework can call it.

```elisp
(setopt kao-lighter nil)   ; then call (kao-mode-line-string) from your mode line
```

**Non-Latin input just works.** kao saves your input method on normal-state entry
and reactivates it on insert entry, so `h`/`j`/`k`/`l` stay kao commands while the
method still types in insert. No configuration beyond choosing one:

```elisp
(setq default-input-method "cyrillic-translit")   ; then C-\ toggles it
```

**Insert-exit selection shape.** Leaving insert state rebuilds a faithful Kakoune
selection instead of collapsing onto the last typed char: after `i…<esc>` the
original span stays selected, `a…<esc>` covers the original plus what you typed,
and `c`/`A`/`I`/`o`/`O` collapse just past the insertion, so the next operator
acts on the span you expect, single- or multi-cursor. During a multi-cursor insert
the pending replay sites wear the `kao-insert-site` face; restyle it to taste.

### Goto and view menus

`kao-goto-define` and `kao-view-define` add entries to the `g`/`G` goto menu and
the `v`/`V` view menu. They are the same seams the Interop recipes build on:

```elisp
(with-eval-after-load 'kao
  ;; `g m' -> jump to the middle line (a `coord' target: a zero-arg function
  ;; returning a buffer position; kao pushes a jump first, so `C-o' returns).
  (kao-goto-define
   ?m 'coord
   (lambda () (save-excursion
                (goto-char (point-min))
                (forward-line (/ (count-lines (point-min) (point-max)) 2))
                (point)))
   "middle line")
  ;; `v r' -> scroll the current line to the top (FN receives the count).
  (kao-view-define ?r (lambda (_n) (recenter 0)) "line to top"))
```

`kao-goto-define`'s KIND is one of `coord`, `selector`, `window`, `command`, or
`buffer`. See [docs/configuration.md](docs/configuration.md)
for what each does.

## Where kao is active

`kao-global-mode` turns kao on in editing buffers only. Out of the box it stays
out of:

- **`special-mode` UIs** (Magit, `*Help*`, `*Occur*`, …) and the **minibuffer**.
  They keep their own keymaps.
- **Terminals.** `ghostel-mode` and `vterm-mode` are excluded explicitly; their
  `self-insert` remaps would otherwise fool the heuristic below and kao's
  suppressed normal map would shadow the live PTY.
- Any buffer where a printable key is **not** a `self-insert` command. This is a
  meow-style heuristic that auto-exempts dired/magit-class modes even when they
  don't derive `special-mode`.

Enable it once in your init, or toggle a single buffer with `M-x kao-mode`:

```elisp
(kao-global-mode 1)
```

**Forcing the decision per mode.** The `kao-global-modes` user option overrides
the heuristic. Its default value is:

```elisp
'((not special-mode minibuffer-mode minibuffer-inactive-mode
       ghostel-mode vterm-mode)
  t)
```

Add a mode to the `(not …)` clause to exempt it. To force kao **ON** in a mode
the heuristic would skip, list it as a positive member *before* the catch-all
`t`. Listing it there skips the editing-buffer heuristic entirely and covers
modes derived from it:

```elisp
;; Force kao ON in dired (so its motions work there); everything else stays default.
(setopt kao-global-modes '(dired-mode
                           (not special-mode minibuffer-mode
                                minibuffer-inactive-mode ghostel-mode vterm-mode)
                           t))
```

## Interop

kao exposes public seams instead of bundling integrations, so you wire it into
the rest of your setup with a few lines. Every recipe below uses the public API
and loads clean.

### avy (`g w`)

Jump to a word with avy, then collapse the selection onto the landing point.
Registered as a `command` goto target; the collapse rides `kao-set-selections`
guarded by `kao-normal-state-p` (kao's `g` dispatch is exempt from the
foreign-command sync that would otherwise snap point back):

```elisp
(defun my/kao-goto-word ()
  "Jump to a word with avy, then collapse the kao selection there (`g w')."
  (interactive)
  (avy-goto-word-0 nil)
  (when (kao-normal-state-p)
    (kao-set-selections
     (list (kao-sel-make :anchor (point) :cursor (point))))))
(with-eval-after-load 'kao
  (kao-goto-define ?w 'command #'my/kao-goto-word "avy word"))
```

### xref and LSP (`g d`, `g r`, `g y`)

kao already binds `SPC d` / `SPC r` to xref definitions/references with the jump
push built in. To put goto-definition/references and an LSP type-definition on
the `g` menu too, push kao's jump list first, so `C-o` (`kao-jump-backward`)
returns to the pre-jump selections, then collapse onto the landing.
`kao-jump-push` is the public, silent push:

```elisp
(defun my/kao-goto--jump (command)
  "Push kao's jump list, run COMMAND, then collapse the selection there."
  (when (kao-normal-state-p) (kao-jump-push))
  (call-interactively command)
  (when (kao-normal-state-p)
    (kao-set-selections
     (list (kao-sel-make :anchor (point) :cursor (point))))))
(defun my/kao-goto-definition () (interactive) (my/kao-goto--jump #'xref-find-definitions))
(defun my/kao-goto-references () (interactive) (my/kao-goto--jump #'xref-find-references))
(defun my/kao-goto-type-definition () (interactive) (my/kao-goto--jump #'eglot-find-typeDefinition))
(with-eval-after-load 'kao
  (kao-goto-define ?d 'command #'my/kao-goto-definition "definition")
  (kao-goto-define ?r 'command #'my/kao-goto-references "references")
  (kao-goto-define ?y 'command #'my/kao-goto-type-definition "type definition"))
```

### org / outline folds

kao motions operate on buffer text regardless of visibility. When the
main cursor lands inside a fold, the `kao-reveal-functions` abnormal hook (run
with the cursor position) surfaces it. The defaults already open isearch overlay
folds and `org-fold` text-property folds; add your own opener for other
invisibility. To make `j` / `k` / `f` / `t` hop the cursor *past* folded runs
instead, set the defcustom:

```elisp
(setopt kao-motion-skip-invisible t)
```

### dired

Dired is auto-exempt, so its own keys work untouched. If you *want* kao's
motions in dired, force it ON with `kao-global-modes` (see
[Where kao is active](#where-kao-is-active)). kao ships no bundled dired
collection.

### which-key

The `g`, `v`, and object menus drive which-key's popup directly, but the `SPC`
leader relies on `which-key-mode`. Enable it to get the same labelled popup for
`kao-user-map`:

```elisp
(which-key-mode 1)
```

Label a leader sub-menu with a `menu-item` and which-key shows its name:

```elisp
(defvar-keymap my/kao-window-map
  :doc "Window commands on `SPC w'."
  "h" #'windmove-left  "j" #'windmove-down
  "k" #'windmove-up    "l" #'windmove-right
  "s" #'split-window-below "v" #'split-window-right
  "q" #'delete-window)
(with-eval-after-load 'kao
  (define-key kao-user-map "w" `(menu-item "window" ,my/kao-window-map)))
```

### repeat-mode and other transient maps

`repeat-mode` (and any package using `set-transient-map`) installs a map that
swallows normal keys. Gate it on the public state predicate so it only arms
outside kao normal state:

```elisp
(advice-add 'repeat-post-hook :before-while
            (lambda () (not (and (bound-and-true-p kao-mode)
                                 (kao-normal-state-p)))))
```

### Remote files (TRAMP)

By default the pipe keys (`|` `<a-|>` `!` `$`) run the shell on the **local** host,
even in a TRAMP buffer. Set `kao-pipe-remote` to run them on the buffer's remote
host instead (input is streamed over stdin):

```elisp
(setopt kao-pipe-remote t)
```

TRAMP does not forward `process-environment`, so a remote pipe command cannot read
the `$kak_*` variables. They reach only local pipes.

### expand-region

No expand-region needed: the tree-sitter module grows/shrinks the selection by
syntax node (`kao-treesit`, `<a-RET>` / `<a-S-RET>`; see
[Optional modules](#optional-modules)).

### Terminal revival (ghostel)

Reopening a killed terminal lives **outside** kao as a separate package modelled
on `hel-ghostel`. The seams it needs are public now. kao ships no terminal code;
`ghostel-mode` and `vterm-mode` are simply exempt (see above).

## Optional modules

Four opt-in extras, none loaded by `kao` itself. `require` them from your
config:

- **`kao-vundo`** is a visual, navigable view of kao's own buffer-history
  *tree* (the one `u`/`U`/`<c-j>`/`<c-k>` walk). `(require 'kao-vundo)`, bind
  it on the user map (e.g. `(define-key kao-user-map "v" #'kao-vundo)` for
  `SPC v`) or `M-x kao-vundo`. In the view, `f`/`b` move to the newer/older change, `n`/`p`
  switch sibling branches, `RET` jumps to the node at point, `q` quits. Every
  move drives the buffer exactly like the history keys, and the view updates
  live as you edit. Browse without changing history with `C-n`/`C-p`, then `d`
  shows that node's change. (vundo and undo-tree drive Emacs's native undo
  list, which kao keeps dormant, so they are blocked in kao buffers;
  `kao-vundo` is the faithful replacement.)
- **`kao-objects`** holds extra text objects a Kakoune config might define but
  that kao does not ship as built-ins (e.g. an HTML/XML `tag` object):
  `(require 'kao-objects)` then `(kao-objects-register-tag)`.
- **`kao-surround`** does surround add, delete and replace, ported from the
  Kakoune `match` / `surround-add` user-modes. Kakoune ships no surround; it is
  always config. `(require 'kao-surround)` then `(kao-surround-setup)` binds `m` to the
  `match` user-mode: `m s KEY` wraps each selection with the KEY pair, `m d KEY`
  deletes the surrounding KEY pair, `m r OLD NEW` replaces it, and `m m` / `m i` /
  `m a` are goto-matching / inner-object / whole-object (so the plain `m` lives on
  `m m`). KEY is a delimiter from `kao-surround-pairs` (brackets, quotes,
  `* _ |`, chevrons, `t` = a tag prompt). Delete and replace find the *enclosing*
  pair through kao's object system, so they work from anywhere inside it, honour
  nesting, and strip multi-character delimiters such as tags. `(kao-surround-setup
  t)` adds the tree-sitter layer: `m d t` / `m r t` target the syntax element in
  html/jsx/tsx buffers, and `m n` selects (and, repeated, grows over) the
  enclosing named node. Built entirely on kao's public config API. An unmapped
  printable key surrounds with **itself** by default (`m s ~` wraps in `~ … ~`);
  set `kao-surround-literal-fallback` to nil to require an explicit
  `kao-surround-pairs` entry, or `(push (cons ?~ (cons "~" "~")) kao-surround-pairs)`
  for an asymmetric or multi-character pair. It takes effect with no
  `kao-surround-setup` re-run.
- **`kao-treesit`** adds Helix-style tree-sitter text objects and syntax-aware
  tree motions. `(require 'kao-treesit)` then `(kao-treesit-setup t)`. Binds the
  `SPC t` tree menu (which-key "tree"): `f`/`F` function, `c`/`C` class, `a`/`A`
  parameter, `o` comment, `T` test (around/inside); `s` select-node, `P` parent,
  `i` first-child, `]`/`[` siblings, `n`/`p` next/prev function; `e`/`E`
  expand/shrink (also top-level `<a-RET>`/`<a-S-RET>`); `*` select all functions,
  `/` filter to functions, `?` ancestor scopes, `t` `treesit-explore-mode`. With
  the `t` argument it also adds the faithful object-pending keys `f` (function)
  and an augmented `u` (parameter, falling back to the regex argument object), so
  `<a-a>f` / `<a-i>f` / `[f` / `]f` / `<a-A>f` and `<a-{a,i,A}>u` work too.
  Tree-sitter `around` objects need Emacs 31+ for per-match precision; on 29/30
  same-name capture runs union into wider `<a-a>` spans (see
  [docs/DIFFERENCES.md](docs/DIFFERENCES.md)).
  Queries come from an existing Helix/Kakoune runtime (`kao-treesit-queries-dir`,
  auto-detected); per-pattern compilation tolerates grammar drift. Everything is
  gated on a loaded parser and degrades to kao's regex objects otherwise. Built
  entirely on kao's public config API. Custom captures you add to a language's
  `textobjects.scm` (and to the `kao-treesit-objects` list) are reachable without a
  bespoke command. `M-x kao-treesit-select-object` selects one at each cursor, and
  `kao-treesit-goto` is the public building block for the four-way next/prev ×
  start/end goto matrix:

  ```elisp
  (defun my/kao-goto-next-comment-start ()
    (interactive) (kao-treesit-goto "comment" t))      ; FORWARD; add END/EXTEND args
  ```

  After editing a `.scm` (or re-pointing `kao-treesit-queries-dir`), run
  `M-x kao-treesit-reload-queries` to pick it up without restarting Emacs. See
  [docs/treesit.md](docs/treesit.md).

## Documents

- [docs/DIFFERENCES.md](docs/DIFFERENCES.md) lists every deliberate
  difference from Kakoune, keys and semantics both, with the reason for each.
- [docs/configuration.md](docs/configuration.md) is the full reference for the
  public API: keymaps, the object and menu registrars, hooks, defcustoms.
- [docs/regex-porting.md](docs/regex-porting.md) translates Kakoune's PCRE
  patterns to Emacs regex syntax.
- [docs/treesit.md](docs/treesit.md) covers the optional tree-sitter module.
- `M-x customize-group RET kao` lists all options.

## License

GPL-3.0-or-later.
