# Changelog

All notable changes to kao are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and kao adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.2] - 2026-07-19

### Fixed

- **A fresh install now byte-compiles completely clean.** `package-vc-install`
  byte-compiles the whole checkout, and installing v1.0.1 printed one hard
  error and a page of warnings while doing so: `test/kao-bench-tests.el`
  failed to compile outright (its `kao-bench` neighbor lives in `test/`, which
  is not on the install load-path), `kao-edit.el` referenced the
  `kao-open-indent` option before its definition (order-dependent, so only a
  fresh alphabetical compile hit it), the modeline tests called an obsolete
  alias at 22 sites, four test docstrings overflowed 80 columns, and two test
  files leaned on `outline` happening to be preloaded. All fixed: every file
  in the checkout, package and test suite alike, now compiles with zero
  warnings in a bare `emacs -Q` session. No runtime behavior changed.

### Changed

- **The strict compile gate now covers the test suite.** `make check` and CI
  previously byte-compiled only the package sources with warnings-as-errors;
  test files were never compiled by any gate, which is exactly where the
  install-log noise accumulated. A new `compile-tests` target holds `test/`
  to the same bar across the full Emacs matrix (29.1 / 30.1 / snapshot), so
  install-log warnings cannot creep back.

## 1.0.1 - 2026-07-19

### Fixed

- **Package version metadata now matches the release.** The 1.0.0 sources
  declared `Version: 0.1.0` in their Lisp headers, so any tool that reads the
  header, meaning `package-vc-install` (including `:last-release`), MELPA and
  `M-x list-packages`, reported kao as 0.1.0. The `Version:` header and the
  `kao-version` constant are now `1.0.1`, matching the git tag, so a
  release-pinned install resolves and displays the correct version.

## 1.0.0 - 2026-07-18

First public release. The [Kakoune](https://kakoune.org) editing model in
Emacs, ported line by line from the Kakoune C++ sources. Selection-first
editing where multiple selections are the only primitive: you select, you see
it, then you act. kao owns a real selection list as its single source of truth.
The Emacs point and region mirror the main selection, never the other way
around. Zero package dependencies, Emacs 29.1+.

### The editing model

- **Multi-selection as the only primitive.** `w` selects a word, `d` deletes
  every selection, `s` splits each selection on a regex into one cursor per
  match. Motions, text objects (`<a-i>` / `<a-a>` / `[` / `]`), the full
  multi-selection algebra (split, keep/remove matching, align, rotate), and the
  operators (change, delete, yank, paste, replace) all behave like Kakoune.
- **Counts** everywhere Kakoune has them, with an optional `C-u 3 w` native
  prefix-arg path.
- **Fast by construction.** Selection commands are pure list transforms, with no
  per-cursor command replay and no per-keystroke marker churn, so a thousand
  cursors stay responsive.

### Navigation, registers, and history

- **Goto and view menus** (`g` / `G`, `v` / `V`), a **jump list** (`C-o` /
  `C-i`), and a walkable **selection history**.
- **Named registers** wired to the Emacs clipboard, and **keyboard macros**.
- **Regex search** on Emacs's own regex engine, with live match highlighting
  (idle-scan, memoized repeats) and an optional ` N/M` match count in the mode
  line.

### Shell pipes

- Pipe selections through the shell with `|`, `<a-|>`, `!`, and `$`; selection
  content flows on stdin, and the Kakoune `$kak_*` environment variables are
  exposed to the command.
- **TRAMP-aware**: set `kao-pipe-remote` to run pipes on a buffer's remote host.

### Native Emacs insert state

- Insert state is plain Emacs editing: your completion, your erase keys, undo,
  minibuffer, and input methods all work unchanged.
- **Faithful insert-exit selection shape**: after `i…<esc>` the original span
  stays selected, `a…<esc>` covers the original plus what you typed, and
  `c` / `A` / `I` / `o` / `O` collapse just past the insertion, single- or
  multi-cursor.
- **Non-Latin input just works**: kao saves your input method on normal-state
  entry and reactivates it on insert entry.
- CJK (wide-character) vertical motion and column math, plus arrow-key /
  Home / End mappings.

### Configuration substrate (public API)

kao ships no bundled key collection. It exposes public seams so you wire in
exactly what you want from your init file:

- Five documented keymaps and `kao-define-key` for per-major-mode bindings.
- `kao-object-register` and `kao-object-make-pair-selector` for custom text
  objects; `kao-goto-define` / `kao-view-define` to extend the goto/view menus.
- State-transition and selection/register hooks, and public state predicates
  (`kao-current-state`, `kao-normal-state-p`, `kao-insert-state-p`).
- Mode-line, cursor-shape, search-count, and motion defcustoms under
  `M-x customize-group RET kao`.

### Where kao is active

- `kao-global-mode` turns kao on in editing buffers only, auto-exempting
  `special-mode` UIs (Magit, `*Help*`, …), the minibuffer, and terminals
  (`vterm`, `ghostel`) via a meow-style heuristic.
- `kao-global-modes` overrides the heuristic per mode, in either direction.

### Optional modules

Four opt-in extras, none loaded by default:

- **`kao-vundo`** is a visual, navigable view of kao's own buffer-history *tree*,
  updating live as you edit.
- **`kao-surround`** does surround add, delete and replace on the `m` match
  user-mode, with an optional tree-sitter layer for html/jsx/tsx.
- **`kao-treesit`** adds Helix-style tree-sitter text objects and syntax-aware tree
  motions (function / class / parameter / comment / test), degrading to kao's
  regex objects when no parser is loaded.
- **`kao-objects`** holds extra text objects such as an HTML/XML `tag`.

### Editor interop

Copy-paste recipes (all on the public API) for **avy**, **xref / LSP**
(`eglot`), **org / outline fold reveal**, **which-key**, **repeat-mode**, and
**dired**.

### Reliability and faithfulness

- Editing commands are robust in empty, narrowed, indirect, reverted, and
  multi-window buffers; secondary selections render correctly across windows.
- Every deliberate deviation from Kakoune is documented, never silent
  ([docs/DIFFERENCES.md](docs/DIFFERENCES.md)).

### License

GPL-3.0-or-later.

[1.0.2]: https://github.com/saifulapm/kao/releases/tag/v1.0.2
