# Tree-sitter objects and motions

`kao-treesit` adds Helix-style tree-sitter text objects (function, class,
parameter, comment, test, around and inside) and syntax-aware tree motions
(expand, shrink, parent, first child, siblings, select-node, select-all, filter,
scopes).

It is opt-in. Nothing loads it and `kao.el` does not pull it in. Every path is
gated on a loaded parser, so where there is no grammar kao falls back to its own
regex objects.

```elisp
(require 'kao-treesit)
(kao-treesit-setup t)
```

`kao-treesit-setup` binds the `SPC t` tree menu and the top-level `<a-RET>` /
`<a-S-RET>`. The optional argument also registers the object-pending keys.

## Queries

kao vendors no queries. It reads Helix-format `<lang>/textobjects.scm` files
from an existing runtime.

`kao-treesit-queries-dir` is a list of directories searched in order. Left at
nil it auto-detects, config directory first, mirroring the order `hx --health`
uses. `; inherits:` directives and shared underscore files resolve across the
whole path.

After editing a `.scm`, or after re-pointing the search path, run `M-x
kao-treesit-reload-queries` to pick it up without restarting Emacs.

Set `kao-treesit-debug` to log each dropped query pattern once.

## Capture vocabulary

kao uses the Helix convention: `.inside` maps to kao's inner object, `.around`
to the whole object. nvim's `.inner` and `.outer` are accepted as aliases.
Captures whose name starts with an underscore are predicate scratch and never
exposed.

```
function.{around,inside}   class.{around,inside}   parameter.{around,inside}
comment.{around,inside}    test.{around,inside}    entry.{around,inside}
```

`entry` is captured and usable by the engine but not bound to a key. `call`,
`loop` and `conditional` are absent from the Helix textobjects corpus, so kao
does not pretend to offer them.

To add a capture of your own, put it in the language's `textobjects.scm` and add
its base name to `kao-treesit-objects`. You then reach it without writing a
command: `M-x kao-treesit-select-object` selects one at each cursor.

## Keys

`SPC t` is a one-shot prefix, labelled "tree" in which-key.

| Key | Does |
| --- | --- |
| `f` / `F` | function, around / inside |
| `c` / `C` | class, around / inside |
| `a` / `A` | parameter, around / inside |
| `o` | comment |
| `T` | test |
| `s` | select the node at the cursor |
| `P` | parent |
| `i` | first child |
| `]` / `[` | next / previous named sibling |
| `n` / `p` | goto next / previous function |
| `e` / `E` | expand / shrink |
| `*` | select all functions |
| `/` | filter selections to functions |
| `?` | ancestor scopes, in an info box |
| `t` | `treesit-explore-mode` |

Expand and shrink are also on `<a-RET>` and `<a-S-RET>` at top level, since they
are the only repeat-heavy operation here.

With `(kao-treesit-setup t)` the object-pending keys work too: `f` for function
and an augmented `u` for parameter, so `<a-a>f`, `<a-i>f`, `[f`, `]f`, `<a-A>f`
and the `<a-{a,i,A}>u` family all resolve through tree-sitter. Bare `u` is
unchanged for anyone who does not pass the argument, and falls back to kao's
regex argument object whenever tree-sitter has nothing to offer.

Kakoune's own object keys are left alone. `a` is the angle-pair object, `c` is
the custom-description object, `[` and `]` are to-begin and to-end. Tree objects
only take keys Kakoune leaves free.

## Selection rules

For an object at the cursor, kao takes the smallest enclosing `.around` capture
that covers the cursor, breaking ties by earliest start. A LEVEL argument picks
the Nth enclosing one instead. The `.around` span includes the separator, for
example the comma after a parameter; the inside span does not, and is resolved
within the chosen outer span.

Expand and shrink keep a buffer-local stack per selection, so shrink retraces
the path expand took. Because the stack is per selection, both compose with
multiple cursors.

Siblings do not ascend: `]` and `[` stay among the named children of the current
parent rather than climbing when they run out.

## Building on it

`kao-treesit-goto` is the public building block for the four-way goto matrix:

```elisp
(kao-treesit-goto BASE FORWARD &optional END EXTEND)

(defun my/kao-goto-next-comment-start ()
  (interactive) (kao-treesit-goto "comment" t))
```

BASE is a capture base name. The whole object is selected and the cursor lands
at its start, or at its end with END non-nil. With EXTEND non-nil each selection
extends to that boundary instead of being replaced. A cursor with no matching
object ahead or behind is left alone.

The module is built entirely on kao's public config API. It registers objects
through `kao-object-register` and moves selections through `kao-map-selections`
and `kao-set-selections`, with no change to kao's core or its object dispatcher.

## Grammar drift

`treesit-query-compile` is atomic: one unknown node type kills an entire query
file. Helix ships against newer grammars than Emacs bundles, so this happens in
practice. Helix's `php/textobjects.scm` fails wholesale on Emacs's php grammar
because of `arrow_function`, `enum_case` and `property_promotion_parameter`.

kao therefore resolves inherits, splits a query into its top-level patterns, and
compiles each one inside a `condition-case`, keeping the survivors and caching
the compiled list per language. On that php file 15 of 16 patterns compile and
only the incompatible variant drops, so function, class, parameter and comment
all still work.

Emacs evaluates only `#eq?`, `#match?` and `#pred?` at capture time, so kao
strips predicates it cannot honour. The effect is over-matching rather than an
error. Set `kao-treesit-keep-predicates` to change that.

## Emacs version differences

On Emacs 31+ each query match resolves its own capture group, so `<a-a>` spans
are precise per match. On Emacs 29 and 30, `treesit-query-capture` has no
GROUPED argument and kao falls back to the flat same-name-capture union: a run
of same-named captures reads as one object, and `<a-a>` spans can be wider than
the examples here imply. kao picks the engine at runtime; there is nothing to
configure.

## Performance

Queries compile once and are never re-read from disk afterwards. An
object-at-cursor lookup scopes the query to the smallest enclosing container
subtree rather than the whole buffer; only `*` and goto run a whole-buffer
query. Within a single command, cursors sharing a container pay for one query
between them, and that memo dies with the command, so nothing can go stale.
