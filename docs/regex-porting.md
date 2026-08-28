# Porting Kakoune regexes to kao

Kakoune uses a PCRE-style regex engine; kao uses Emacs' built-in regex engine.
Most patterns you type at `s`/`S`/`/`/`*` prompts are either identical or a
small mechanical rewrite away. A few PCRE constructs have no Emacs equivalent.
For those, write an elisp **selector** instead of a regex (see
[Unsupported](#unsupported-constructs)).

> kao changes **nothing** by default: you keep typing Emacs-syntax regexes at the
> search prompts. This page is a porting reference, not a runtime translator.
> A runtime PCRE→Emacs translator was evaluated and **deliberately not shipped**
> (see [Why no translator](#why-no-runtime-translator)).

## Safe rewrites (mechanical)

These translate one-for-one. The recurring difference is that Emacs regex
**escapes the grouping/alternation/quantifier metacharacters** that PCRE leaves
bare.

| Intent | Kakoune (PCRE) | kao (Emacs) |
|---|---|---|
| Group | `(abc)` | `\(abc\)` |
| Non-capturing group | `(?:abc)` | `\(?:abc\)` |
| Alternation | `a\|b` (bare `\|`) | `a\\\|b` (escaped `\|`) |
| Bounded repeat | `a{2,4}` | `a\{2,4\}` |
| Exactly n | `a{3}` | `a\{3\}` |
| Backreference | `\1` | `\1` (same) |
| Digit | `\d` | `[0-9]` |
| Non-digit | `\D` | `[^0-9]` |
| Any char but newline | `\N` | `.` (same) |
| Any char incl. newline | `.` | `\(?:.\|\n\)` † |
| Horizontal blank | `\h` | `[ \t]` |
| Word char | `\w` | `\w` (same; syntax-table driven) |
| Whitespace incl. newline | `\s` | `[[:space:]]` ‡ |
| Word boundary | `\b` | `\b` (same) |
| Start/end of word | `\<` / `\>` | `\<` / `\>` (same) |
| Char class | `[a-z]` | `[a-z]` (same) |
| Anchors | `^` `$` | `^` `$` (same) |
| Quantifiers | `*` `+` `?` | `*` `+` `?` (same) |
| Lazy quantifiers | `*?` `+?` `??` | `*?` `+?` `??` (same) |

**† Newline and the dot.** Kakoune's `.` matches *any* character **including
newlines by default** (`regex.asciidoc:74`; that is Kakoune's `(?s)` mode, and
`(?S)` turns it off). Emacs' `.` **never** matches a newline. It is permanently
Kakoune's `(?S)`, and there is no inline flag to change it (see
[Unsupported](#unsupported-constructs)). So a bare Kakoune `.` that was relied on
to cross a line break must be spelled `\(?:.\|\n\)` in Emacs. Conversely Kakoune's
`\N`, "any character but newline", not affected by any modifier
(`regex.asciidoc:77`), is exactly the Emacs `.`, so `\N` becomes `.`, the trivial
rewrite.

**‡ Newline and `\s`.** Kakoune's `\s` matches **all Unicode whitespace,
newline included** (`regex.asciidoc:62`; contrast `\h`, which excludes
line-breaks, `regex.asciidoc:63`, and maps cleanly to `[ \t]`). Emacs
`[[:space:]]` matches characters with *whitespace syntax*, so whether it matches a
newline is **syntax-table dependent**: inside a normal major-mode buffer the
newline has whitespace syntax and `[[:space:]]` *does* match it, but under the
standard syntax table (e.g. `string-match`, or a mode that classifies the newline
differently) it does **not**. When a newline **must** match regardless, use an
explicit class, `[ \t\r\n\f\v]` or `[[:space:]\n]`, which matches it everywhere.

**Worked example: the `line` object** (`^\h*` … `\h*\n` in `mappings.kak`):

```
Kakoune open:  ^\h*        →  Emacs:  ^[ \t]*
Kakoune close: \h*\n       →  Emacs:  [ \t]*\n
```

Register it from elisp on top of kao's public object API.
`kao-object-make-pair-selector` builds the selector from the open/close regexes
(they are passed through verbatim, never `regexp-quote`d, so spell any literal
delimiter as an escaped regex):

```elisp
(require 'kao-object)
(kao-object-register
 ?x
 (kao-object-make-pair-selector "^[ \t]*" "[ \t]*\n")
 "line")
```

## Case-insensitive search: `(?i)` / `(?I)`

kao handles the inline flags natively, with no translator needed:

- `(?i)foo` makes the rest of the pattern case-insensitive.
- `(?I)Foo` forces case-sensitive.
- The default is case-sensitive (`kao-search-case-fold` defcustom, `nil`;
  Kakoune has **no** smartcase).

So the commented-out `(?i)` search maps from a Kakoune config work as-is once
typed at kao's search prompt.

## Unsupported constructs

Emacs regex cannot express these. Don't translate them. Register an elisp
**selector** through `kao-object-register` and put the logic in code.

| PCRE construct | Example | Why unsupported | Workaround |
|---|---|---|---|
| Lookahead | `(?=…)` `(?!…)` | no Emacs equivalent | elisp selector; scan + check the following text in code |
| Lookbehind | `(?<=…)` `(?<!…)` | no Emacs equivalent | elisp selector; check the preceding char in code, or **consume a guard char** (see `tag` below) |
| Named groups | `(?<name>…)` | not supported | use a numbered group `\(…\)` |
| Keep-out | `\K` | not supported | restructure as an explicit group |
| Atomic / possessive | `(?>…)` `a++` | not supported | usually unnecessary; rewrite the quantifier |
| `\A` / `\z` / `\Z` | buffer anchors | partial Emacs support | use `\`` / `\'` |
| Literal quoting | `\Q…\E` | no Emacs inline quote | `regexp-quote` the literal run in an elisp selector, or hand-escape it |
| Dot-matches-newline modifier | `(?s)` / `(?S)` | Emacs `.` is permanently `(?S)` | can't be toggled inline; spell a newline-crossing match as `\(?:.\|\n\)` (see †) |

### Worked example: the `tag` text object (needs a selector)

A Kakoune `tag` object uses negative lookbehind to avoid self-closing tags:

```
open:  <\w[\w-]*\h*[^>]*?(?<!/)>      close:  </\w[\w-]*(?<!/)>
```

The `(?<!/)` ("a `>` not preceded by `/`") has no Emacs lookbehind, **but** the
intent (exclude `<br/>`, `<input …/>`) is expressible by *consuming* the char
before `>` and forbidding `/` there. That hand-translation is exactly the kind a
mechanical translator cannot do safely:

```
open:  <[[:word:]][-[:word:]]*\(?:[^>]*[^/>]\)?>     close:  </[[:word:]][-[:word:]]*>
```

kao ships this ready-made. Opt in with:

```elisp
(require 'kao-objects)
(kao-objects-register-tag)   ;; binds the tag object to <a-a>T / <a-i>T (and <a-A>T / <a-I>T)
```

`kao-objects.el` registers nothing on load (Kakoune has no built-in `tag`
object, so kao's default object set stays byte-identical to Kakoune's until you
opt in).

### `\Q…\E`, literal quoting

Kakoune's `\Q…\E` treats everything between the markers as a literal, so
`.\Q.^$\E$` matches any character, then the literal string `.^$`, then an end of
line (`regex.asciidoc:175`). Emacs has no inline quote. Build the literal run with
`regexp-quote` inside an elisp selector and concatenate it with the live parts:

```elisp
;; Kakoune: .\Q.^$\E$   →   Emacs, assembled in code:
(concat "." (regexp-quote ".^$") "$")
```

For a short run you can also hand-escape the metacharacters (`\.` `\^` `\$` `\\`
etc.) directly at the prompt.

### `(?s)` / `(?S)`, the dot-matches-newline modifier

Kakoune's `(?s)` lets `.` match newlines and `(?S)` prevents it
(`regex.asciidoc:170`), toggling the default described in note † above. Emacs has
**no** inline modifier for this: `.` is permanently `(?S)`, so there is nothing to
toggle. Write `\(?:.\|\n\)` where you need a newline to match. And unlike the
case flags, a typed `(?s)`/`(?S)` is **not** stripped: kao's case-fold
preprocessing (`kao--regex-case-fold`) removes only the `(?i)`/`(?I)` case tokens
(a combined form like `(?is)` is left intact), so a bare `(?s)` survives into the
Emacs pattern, where it is matched as the **literal characters** `(?s)`, almost
never what you want. Delete it and rewrite the dot instead.

## Why no runtime translator

A PCRE→Emacs translator (`'pcre` mode, allowlist, or `pcre2el`) was evaluated and
**not shipped**:

- `pcre2el` silently mis-translates Kakoune escapes into literals (`\K`→`K`,
  `\N`→`N`, `\z`→`z`), giving wrong matches with no error.
- A PCRE pattern is also *invalid Emacs syntax*, so it would be rejected by kao's
  validators before any translation ran, and the `/` register is shared with
  kao-generated Emacs-syntax patterns (`*` sets it), and translate-on-read
  would corrupt them.
- The corpus doesn't justify it: enumerating a real config (90 `.kak` files),
  the only regexes typed *interactively* are the two custom objects. `line` is a
  one-line safe rewrite; `tag` needs a selector regardless. Every other regex is
  config-internal (highlighters, `set-register /`, `exec` selections), ported
  once into the owning command and never re-typed. Exactly **one** interactive,
  safe, non-trivial pattern existed, far too few to justify a translation layer
  whose failure mode is silent corruption.

The conclusion: **rewrite by hand using this page; reach for an elisp selector
when a construct is unsupported.**
