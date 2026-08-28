# Differences from Kakoune

kao is a faithful port of the Kakoune editing model, not a one-to-one clone of
the Kakoune *program*. Two kinds of differences are deliberate:

1. **Keys not bound.** Kakoune surfaces that exist only because Kakoune is a
   standalone program with no Emacs underneath. Emacs already provides the
   equivalent, so kao leaves the key native, or unbound as a loud no-op, instead
   of shadowing it.
2. **Semantics differences.** Places where kao's behaviour diverges from
   Kakoune on purpose, usually because Emacs's own machinery (regex, undo,
   clipboard, folding) is doing the work underneath.

Every row below is deliberate. Nothing here is a silent deviation. The
`file.cc:NN` citations point into the
[Kakoune sources](https://github.com/mawww/kakoune/tree/master/src).

For the regex dialect specifically, see the porting guide:
[regex-porting.md](regex-porting.md).

## Keys not bound

kao binds Kakoune's keys faithfully, `<a-…>` included. The exceptions are the
handful of keys whose only job is to reach a Kakoune subsystem that Emacs
already owns:

| Key | In Kakoune | In kao |
|-----|-----------|--------|
| `:` | opens the command-line prompt (`command_manager`) | bound to `kao-colon-hint`, a loud `user-error` pointing at `M-x`, so a reflexive Kakoune `:` gets a hint rather than a silent no-op. Elisp is the configuration and scripting language; `M-x` is the command palette, so `<a-x>` also stays Emacs's `M-x` |
| `<c-l>` | force redraw (`force_redraw`, `normal.cc:2410`) | unbound in normal state. Falls through to Emacs's `C-l` (`recenter-top-bottom`), a native superset that redraws the frame on first press |
| `<c-u>` *(insert)* | commit undo group (`input_handler.cc:1366`) | not bound as a checkpoint. `C-u` stays `universal-argument`; `u` / `<c-j>` / `<c-k>` already commit pending modifications at the same points |
| `<c-w>` *(insert)* | erase to previous word begin | native Emacs erase (`C-w` and friends). Insert state is plain Emacs editing |
| `<c-x>` / `<c-o>` *(insert)* | explicit completers / autocomplete toggle (`input_handler.cc:1339`) | native. Completion is fully Emacs's (`tab-always-indent`, `corfu-auto`, `M-TAB`); `C-x` stays the Emacs prefix, `C-o` stays `open-line` |
| `<c-n>` / `<c-p>` *(insert)* | menu-select in the completion popup (`input_handler.cc:1331`) | deliberately unbound in the insert map. Native next/previous-line, and the completion popup cycles them itself when it is live |
| `\` | disable hooks for the next command (`input_handler.cc:314`) | unbound. kao has no kakrc hooks DSL to disable, so the key has no referent; the `suppress-keymap`'d normal map makes it a loud no-op (`\ is undefined`), not a silent flag |

## Semantics differences

Where kao behaves differently from Kakoune, and why:

| Area | kao behaviour |
|------|---------------|
| **Regex dialect** | Emacs's native regexp engine, not PCRE. `/ ? n s S <a-k>` all take Emacs syntax. See [regex-porting.md](regex-porting.md) for the mapping. |
| **Search case-sensitivity** | Case-sensitive with no smartcase, matching Kakoune (`normal.cc:1021`). `kao-search-case-fold` opts into case-insensitive; only a global leading `(?i)` / `(?I)` toggles it. Mid-pattern `(?i)` scoping is an irreducible Emacs limit. |
| **Clipboard** | Hybrid: the internal list-of-strings is the source of truth. On yank kao mirrors the **main** selection's text to the system clipboard and `kill-ring`, so cross-app paste works; explicit commands mirror *all* selections. Paste (`p`) still uses the internal list, i-th string to i-th selection. |
| **Kill-ring mirroring** | No global "never touch the kill-ring" knob, matching evil's and meow's defaults. The faithful per-command opt-out is `<a-d>` / `<a-c>` (`kao-delete-no-yank` / `kao-change-no-yank`), which skip the register entirely and so never touch clipboard or kill-ring. |
| **Registers** | Store the last value only, versus Kakoune's 1000-entry per-register history. The `/`, `\|`, and named registers are populated faithfully but keep only the most recent value. |
| **Zero-width `s` / `S`** | Zero-width regex matches are kept, so `%s^`, `s$` and `s\b` work, matching Kakoune's `RegexIterator`. After a null match the scan resumes one char past it, a documented one-char approximation of Kakoune's `NotInitialNull`. |
| **`h` / `l` motion** | Buffer-bounded, not line-bounded: `l` crosses onto the next line, `h` onto the previous line's newline, `10l` walks freely. Faithful to Kakoune's `move_cursor<CharCount>` (`buffer.cc:165`), which has no line awareness. |
| **Word motions `w` / `b` / `e`** | Drop exhausted selections at an edge, since Kakoune's `select` returns `nothing_here` and the cursor is dropped. The list is left unchanged only when *every* selection would drop. |
| **`o` / `O` open** | Open a plain, unindented newline by default, the faithful bare Kakoune core. `kao-open-indent` opts into `indent-according-to-mode` at each opened site. |
| **`*` at a newline-less end-of-buffer** | A word ending at a buffer with no trailing newline yields `\bword` with no trailing `\b`. kao does not force a trailing newline, so the buffer end is Emacs-faithful; Kakoune's forced trailing `\n` never reaches this case. |
| **No overriding-map / no bundled collections** | The normal-state map is `suppress-keymap`'d. Rather than shadowing a whole major mode, kao stays *out* of dired/magit-class buffers via the self-insert heuristic. There is no `evil-make-overriding-map` analog and no evil-collection equivalent; kao is a Kakoune editing engine, not a keys-everywhere layer. If you want kao motions in such a buffer, force it on via `kao-global-modes` (see the README). |
| **No per-major-mode initial state** | Buffers always enter normal state, the faithful Kakoune behaviour (`InputHandler` unconditionally pushes Normal, `input_handler.cc:1551`). For insert-on-open, use a native hook: `(add-hook 'kao-mode-hook (lambda () (when (derived-mode-p 'message-mode) (kao-insert))))`. |
| **Undo visualizers** | `kao-block-foreign-undo-visualizers` refuses to *open* vundo or undo-tree-visualize in kao buffers, because the native `buffer-undo-list` stays dormant there. `kao-vundo` is the faithful replacement. Command-level native undo is accepted incoherence, opt-in via `kao-bind-native-undo-keys`. |
| **Mouse drag-select** | A drag collapses to a click at the release point rather than extending the main selection. Drag-extend, right-click extend, and Ctrl-click multi-cursor are deferred; the mouse stance is minimum fidelity. |
| **Tree-sitter `around` objects** | On Emacs 31+ each query match resolves its own capture group, so `<a-a>` object spans are per-match precise. On Emacs 29/30 `treesit-query-capture` has no GROUPED argument, so kao falls back to the flat same-name-capture union: a run of same-named captures reads as one object, and `<a-a>` spans can be **wider** than the 31+ examples imply. Intended, and the engine is chosen automatically at runtime. |
