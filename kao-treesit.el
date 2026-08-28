;;; kao-treesit.el --- Tree-sitter text objects and motions for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; An opt-in tree-sitter layer for kao: Helix-style text objects (function /
;; class / parameter / comment / test, around + inside) and syntax-aware tree
;; motions (expand / shrink, parent / child, sibling, select-node, ...).  See
;; docs/treesit.md for the full design.
;;
;; Like `kao-surround' and `kao-objects', this module is OPT-IN and builds on
;; kao's public config substrate (the `kao-' API) — it registers NOTHING on
;; load and `kao.el' does NOT require it.  It is also SELF-CONTAINED: it
;; does not depend on `kao-surround', duplicating the few lines of tree-sitter
;; gating rather than coupling the two optional features.
;;
;; Everything is gated on a loaded parser via `kao-treesit-ready-p' and degrades
;; to kao's existing regex objects / a friendly no-op when tree-sitter is
;; unavailable, so requiring this file is always safe (the `treesit' require is
;; soft).
;;
;; This file (spec Task 0.1) provides the substrate the later tasks build on:
;; the ready-gate, the query-directory search path (Helix `textobjects.scm'
;; files, auto-detected config-first), and the customisation group.  The text
;; objects, the `<space> t' menu, and the `kao-treesit-setup' entry point arrive
;; in the following tasks.

;;; Code:

(require 'subr-x)                        ; string-trim / string-empty-p
(require 'kao-selection)                 ; kao-sel-make / -min / -max / -cursor
(require 'kao-state)                     ; selection API (map/get/set) + keymaps
(require 'kao-menu)                       ; kao-user-map (the `SPC' prefix)
(require 'kao-object)                    ; kao-object-register + regex `u'
(require 'treesit nil t)                 ; soft: degrade gracefully when absent

(defgroup kao-treesit nil
  "Tree-sitter text objects and syntax-aware motions for kao."
  :group 'kao
  :prefix "kao-treesit-")

;;;; Customisation

(defcustom kao-treesit-queries-dir nil
  "Search path for Helix `textobjects.scm' query files; nil = auto-detect.
A list of directories, searched in order, for a language's
`<lang>/textobjects.scm' (e.g. `bash/textobjects.scm').  This points at an
existing Helix (or compatible) runtime — kao vendors no queries.

When nil, `kao-treesit-queries-dirs' auto-detects the path config-first,
mirroring Helix's own runtime search order.  Set this to pin a stable location
\(such as a Kakoune runtime `queries' directory, which carries no embedded
version number and so survives upgrades)."
  :type '(choice (const :tag "Auto-detect" nil)
                 (repeat directory))
  :group 'kao-treesit
  ;; Re-pointing the path invalidates both query caches so the new location
  ;; takes effect immediately (treesit-7).  `custom-initialize-default' keeps
  ;; `:set' from firing at load time, before `kao-treesit-reload-queries' is
  ;; defined further down this file.
  :initialize #'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'kao-treesit-reload-queries)
           (kao-treesit-reload-queries))))

(defcustom kao-treesit-objects
  '("function" "class" "parameter" "comment" "test" "entry")
  "Capture base-names kao recognises from `textobjects.scm' queries.
Each name is matched against the Helix `.around'/`.inside' captures of a query
\(nvim-treesitter's `.outer'/`.inside' are accepted as aliases); a leading `_'
capture is private predicate scratch and never exposed.  This is the allow-list
the engine normalises against — which names are actually bound to keys or the
`<space> t' menu is a separate, later choice."
  :type '(repeat string)
  :group 'kao-treesit)

(defcustom kao-treesit-debug nil
  "When non-nil, log each dropped query pattern once per language.
`treesit-query-compile' is atomic — one unknown node type fails the whole query
\(grammar drift between the Helix queries and Emacs's bundled grammar makes this
real).  kao therefore compiles a language's query patterns individually and
keeps the survivors; with this on, each dropped pattern is logged once so the
drift is visible rather than silent."
  :type 'boolean
  :group 'kao-treesit)

(defcustom kao-treesit-keep-predicates '("eq?" "match?" "pred?")
  "Tree-sitter query predicate names kao keeps; every other group is stripped.
Emacs evaluates only a small set of query predicates at capture time (`#eq?',
`#match?', `#pred?' on current builds); an unknown predicate such as Helix's
`#any-of?' or `#not-kind-eq?', or a `#set!' directive, signals
`treesit-query-error' when the query runs.  kao therefore drops predicate groups
whose name is not in this list, so the surrounding pattern merely over-matches
instead of erroring (the documented worst case).  Extend this if your Emacs
supports more predicates."
  :type '(repeat string)
  :group 'kao-treesit)

;;;; Ready-gate (self-contained)

(defun kao-treesit-ready-p ()
  "Non-nil when tree-sitter is usable in the current buffer.
True when this Emacs build has tree-sitter (`treesit-available-p') and a parser
is loaded for the current buffer (`treesit-parser-list').  Every tree-sitter
object and motion checks this and degrades to the regex fallback or a no-op when
it is nil."
  (and (fboundp 'treesit-available-p) (treesit-available-p)
       (fboundp 'treesit-parser-list) (treesit-parser-list)))

;;;; Query-directory resolution (auto-detect, config-first)

(defconst kao-treesit--runtime-prefixes
  '("/opt/homebrew/share/helix/runtime"
    "/usr/local/share/helix/runtime"
    "/usr/share/helix/runtime"
    "/usr/lib/helix/runtime")
  "Well-known system install prefixes searched for a Helix runtime.
Each yields a `queries' subdirectory candidate during auto-detection.")

(defvar kao-treesit--auto-dirs 'unset
  "Memoised auto-detected query directories.
The symbol `unset' means not yet computed; otherwise a list of existing
directories (possibly nil).  Avoids re-globbing / re-spawning `hx' on every
lookup (the perf pillar).")

(defun kao-treesit--hx-runtime-dir ()
  "Return the Helix runtime directory reported by `hx --health', or nil.
A last-resort locator used only when the well-known paths miss; runs only when
an `hx' executable is on PATH."
  (when (executable-find "hx")
    (with-temp-buffer
      (ignore-errors (call-process "hx" nil t nil "--health"))
      (goto-char (point-min))
      (when (re-search-forward
             "untime director\\(?:y\\|ies\\)[^:]*:[ \t]*\\([^;\n]+\\)" nil t)
        (string-trim (match-string 1))))))

(defun kao-treesit--auto-detect-dirs ()
  "Auto-detect existing Helix query directories, config-first.  Memoised.
Searches `$HELIX_RUNTIME', then the user config runtime
\(helix/runtime under `$XDG_CONFIG_HOME' or ~/.config), then the well-known
`kao-treesit--runtime-prefixes', keeping those whose `queries' subdirectory
exists.  Only if none exist does it fall back to what `hx --health' reports.
Returns the list of existing directories (possibly nil)."
  (if (not (eq kao-treesit--auto-dirs 'unset))
      kao-treesit--auto-dirs
    (let* ((env (getenv "HELIX_RUNTIME"))
           (cfg (expand-file-name "helix/runtime"
                                  (or (getenv "XDG_CONFIG_HOME") "~/.config")))
           (runtimes (append (when env (list env))
                             (list cfg)
                             kao-treesit--runtime-prefixes))
           dirs)
      (dolist (d runtimes)
        (let ((q (expand-file-name "queries" d)))
          (when (file-directory-p q) (push q dirs))))
      (setq dirs (nreverse dirs))
      (setq kao-treesit--auto-dirs
            (or dirs
                (let ((hx (kao-treesit--hx-runtime-dir)))
                  (when hx
                    (let ((q (expand-file-name "queries" hx)))
                      (when (file-directory-p q) (list q))))))))))

(defun kao-treesit-queries-dirs ()
  "Return the effective query search path: existing directories, in order.
When `kao-treesit-queries-dir' is set, its existing entries are returned;
otherwise the path is auto-detected config-first (see
`kao-treesit--auto-detect-dirs').  The query loader (Task 0.2) searches these
for a language's `<lang>/textobjects.scm'."
  (if kao-treesit-queries-dir
      (let (out)
        (dolist (d kao-treesit-queries-dir (nreverse out))
          (let ((e (expand-file-name d)))
            (when (file-directory-p e) (push e out)))))
    (kao-treesit--auto-detect-dirs)))

;;;; Query loading: locate -> inherits -> shim -> split (Task 0.2)

(defun kao-treesit--locate-query (lang)
  "Return the path of LANG's `textobjects.scm' on the search path, or nil.
Searches `kao-treesit-queries-dirs' in order for `<lang>/textobjects.scm'; LANG
is a string or symbol."
  (let ((rel (format "%s/textobjects.scm" lang))
        (dirs (kao-treesit-queries-dirs))
        found)
    (while (and dirs (not found))
      (let ((p (expand-file-name rel (car dirs))))
        (when (file-readable-p p) (setq found p)))
      (setq dirs (cdr dirs)))
    found))

(defun kao-treesit--read-query-file (lang)
  "Return the raw `textobjects.scm' text for LANG, or \"\" when none is found.
LANG is a string or symbol; a missing or unreadable file yields \"\" (so an
absent inherited language is skipped, not fatal)."
  (let ((path (kao-treesit--locate-query lang)))
    (if (and path (file-readable-p path))
        (with-temp-buffer (insert-file-contents path) (buffer-string))
      "")))

(defun kao-treesit--expand-inherits (text reader &optional seen)
  "Resolve Helix `; inherits:' lines in TEXT, returning the expanded query.
READER maps a language name (string) to its raw query text (\"\" when missing).
Each `; inherits: a,b' comment is replaced in place by the recursively-expanded
text of those languages (Helix semantics, including underscore-prefixed shared
files); SEEN guards inheritance cycles and missing files are skipped."
  (replace-regexp-in-string
   "^;+[ \t]*inherits[ \t]*:?[ \t]*\\([a-z_,()-]+\\)[ \t]*$"
   (lambda (m)
     ;; treesit-1: `split-string' and the recursive expansion clobber the match
     ;; data `replace-regexp-in-string' relies on to splice this replacement, so
     ;; without this guard the inherits line is mangled and its last inherited
     ;; pattern dropped.  Save it around the whole body (`match-string' below
     ;; still reads the outer match, evaluated before any clobbering call).
     (save-match-data
       (mapconcat
        (lambda (lang)
          (if (member lang seen)
              ""
            (concat "\n"
                    (kao-treesit--expand-inherits (funcall reader lang)
                                                  reader (cons lang seen))
                    "\n")))
        (split-string (match-string 1 m) "[, \t]+" t)
        "")))
   text t t))

(defun kao-treesit--read-query (lang)
  "Return LANG's `textobjects.scm' with `; inherits:' resolved across the path.
Missing files (including inherited ones) yield empty text, never an error."
  (kao-treesit--expand-inherits
   (kao-treesit--read-query-file lang)
   #'kao-treesit--read-query-file
   (list (format "%s" lang))))

(defun kao-treesit--balanced-end (query i)
  "Return the index just past the group that opens at I (a paren) in QUERY.
Tracks `()'/`[]' depth, skipping strings and `;' comments."
  (let ((len (length query)) (depth 0) (done nil))
    (while (and (< i len) (not done))
      (let ((c (aref query i)))
        (cond
         ((eq c ?\;)
          (while (and (< i len) (not (eq (aref query i) ?\n))) (setq i (1+ i))))
         ((eq c ?\")
          (setq i (1+ i))
          (while (and (< i len) (not (eq (aref query i) ?\")))
            (setq i (+ i (if (eq (aref query i) ?\\) 2 1))))
          (setq i (1+ i)))
         ((memq c '(?\( ?\[)) (setq depth (1+ depth)) (setq i (1+ i)))
         ((memq c '(?\) ?\]))
          (setq depth (1- depth)) (setq i (1+ i))
          (when (<= depth 0) (setq done t)))
         (t (setq i (1+ i))))))
    i))

(defun kao-treesit--predicate-name (query i)
  "Return the predicate name (e.g. \"eq?\") of the `(#...' group at I in QUERY."
  (let ((len (length query)) (start (+ i 2)) (j (+ i 2)))
    (while (and (< j len)
                (let ((c (aref query j)))
                  (or (and (>= c ?a) (<= c ?z))
                      (and (>= c ?A) (<= c ?Z))
                      (and (>= c ?0) (<= c ?9))
                      (memq c '(?? ?! ?- ?_)))))
      (setq j (1+ j)))
    (substring query start j)))

(defun kao-treesit--strip-unsupported-predicates (query)
  "Return QUERY with predicate/directive groups Emacs cannot evaluate removed.
A `(#NAME ...)' group whose NAME is not in `kao-treesit-keep-predicates' is
dropped, so the surrounding pattern over-matches rather than signalling
`treesit-query-error' at capture.  Comment- and string-aware."
  (let ((len (length query)) (i 0) (seg 0) (parts nil))
    (while (< i len)
      (let ((c (aref query i)))
        (cond
         ((eq c ?\;)
          (while (and (< i len) (not (eq (aref query i) ?\n))) (setq i (1+ i))))
         ((eq c ?\")
          (setq i (1+ i))
          (while (and (< i len) (not (eq (aref query i) ?\")))
            (setq i (+ i (if (eq (aref query i) ?\\) 2 1))))
          (setq i (1+ i)))
         ((and (eq c ?\() (< (1+ i) len) (eq (aref query (1+ i)) ?#))
          (let ((end (kao-treesit--balanced-end query i))
                (name (kao-treesit--predicate-name query i)))
            (unless (member name kao-treesit-keep-predicates)
              (push (substring query seg i) parts)
              (setq seg end))
            (setq i end)))
         (t (setq i (1+ i))))))
    (push (substring query seg len) parts)
    (apply #'concat (nreverse parts))))

(defun kao-treesit--split-patterns (query)
  "Split QUERY text into a list of top-level pattern strings.
A top-level pattern is a balanced `(...)' or `[...]' group at depth 0 together
with any trailing captures and quantifiers; comment-, string- and paren-aware.
Splitting lets each pattern compile independently so one incompatible pattern
cannot fail the whole query."
  (let ((len (length query)) (i 0) (depth 0) (start nil) (out nil))
    (while (< i len)
      (let ((c (aref query i)))
        (cond
         ((eq c ?\;)
          (while (and (< i len) (not (eq (aref query i) ?\n))) (setq i (1+ i))))
         ((eq c ?\")
          (when (and (= depth 0) (null start)) (setq start i))
          (setq i (1+ i))
          (while (and (< i len) (not (eq (aref query i) ?\")))
            (setq i (+ i (if (eq (aref query i) ?\\) 2 1))))
          (setq i (1+ i)))
         ((memq c '(?\( ?\[))
          (when (and (= depth 0) start)
            (push (string-trim (substring query start i)) out)
            (setq start nil))
          (when (= depth 0) (setq start i))
          (setq depth (1+ depth))
          (setq i (1+ i)))
         ((memq c '(?\) ?\]))
          (setq depth (max 0 (1- depth)))
          (setq i (1+ i)))
         (t (setq i (1+ i))))))
    (when start (push (string-trim (substring query start len)) out))
    (let (res)
      (dolist (s out res)
        (unless (string-empty-p s) (push s res))))))

(defun kao-treesit--normalize-capture (name)
  "Return (BASE . PART) for capture NAME, or nil if private or unrecognised.
PART is `whole' for Helix `.around' / nvim `.outer', and `inner' for `.inside' /
`.inner'.  A leading-underscore capture is private predicate scratch and yields
nil, as does a name with no recognised suffix.  NAME is a string or symbol."
  (let ((s (if (symbolp name) (symbol-name name) name)))
    (cond
     ((or (not (stringp s)) (string-prefix-p "_" s)) nil)
     ((string-suffix-p ".around" s) (cons (substring s 0 -7) 'whole))
     ((string-suffix-p ".outer" s)  (cons (substring s 0 -6) 'whole))
     ((string-suffix-p ".inside" s) (cons (substring s 0 -7) 'inner))
     ((string-suffix-p ".inner" s)  (cons (substring s 0 -6) 'inner))
     (t nil))))

;;;; Per-pattern compile + per-language cache (Task 0.2)

(defvar kao-treesit--pattern-cache (make-hash-table :test 'equal)
  "Hash table mapping a language name (string) to its compiled-pattern list.
Populated once per language by `kao-treesit--patterns-for' and never re-read
from disk afterward (the perf pillar).")

(defun kao-treesit-reload-queries ()
  "Reload tree-sitter object queries from disk.
Empty BOTH the compiled-pattern cache AND the auto-detected query-directory memo
\(`kao-treesit--auto-dirs' back to `unset'), so the next object/motion re-reads
`textobjects.scm' from `kao-treesit-queries-dir' — or re-detects the directory —
picking up an edited query file or a re-pointed path without restarting Emacs
\(treesit-7).  Interactive.

A pure cache operation that never reads `kao--sels', so it is deliberately
EXCLUDED from the `kao--assert-mode' mode guard  — usable with
`kao-mode' off (e.g. to refresh queries before enabling the mode)."
  (interactive)
  (clrhash kao-treesit--pattern-cache)
  (setq kao-treesit--auto-dirs 'unset))

(defun kao-treesit--compile-patterns (lang patterns)
  "Compile each string in PATTERNS for LANG, returning the list of survivors.
Each is compiled eagerly inside `condition-case'; one that fails (e.g. an
unknown node type from grammar drift) is dropped, the rest kept.  With
`kao-treesit-debug' non-nil, each drop and a per-language summary are logged."
  (let ((out nil) (dropped 0))
    (dolist (p patterns)
      (condition-case err
          (push (treesit-query-compile lang p t) out)
        (error
         (setq dropped (1+ dropped))
         (when kao-treesit-debug
           (message "kao-treesit: %s dropped pattern: %s (%s)"
                    lang (car (split-string p "\n"))
                    (error-message-string err))))))
    (when (and kao-treesit-debug (> dropped 0))
      (message "kao-treesit: %s — %d/%d patterns compiled (%d dropped)"
               lang (- (length patterns) dropped) (length patterns) dropped))
    (nreverse out)))

(defun kao-treesit--patterns-for (lang)
  "Return the cached list of compiled `textobjects' patterns for LANG.
On the first call the query is located, its `; inherits:' resolved, unsupported
predicates stripped, split into top-level patterns and each compiled (survivors
kept); the result is cached, so later calls do no disk I/O.  LANG is a
language symbol (e.g. `bash')."
  (let* ((key (format "%s" lang))
         (cached (gethash key kao-treesit--pattern-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let* ((raw (kao-treesit--read-query lang))
             (shimmed (kao-treesit--strip-unsupported-predicates raw))
             (patterns (kao-treesit--split-patterns shimmed))
             (compiled (kao-treesit--compile-patterns lang patterns)))
        (puthash key compiled kao-treesit--pattern-cache)
        compiled))))

;;;; Engine: node->sel + object-at resolver (Task 0.3)

(defun kao-treesit--span->sel (beg end &optional to-begin)
  "Return a `kao-sel' for the buffer span [BEG, END).
The cursor sits on the last char (END-1), matching kao's on-char selection model
and the `kao-surround' tree-sitter precedent.  With TO-BEGIN non-nil the result
is reversed (anchor at the end, cursor at BEG) for a to-object-begin motion."
  (let ((c (max beg (1- end))))
    (if to-begin
        (kao-sel-make :anchor c :cursor beg)
      (kao-sel-make :anchor beg :cursor c))))

(defun kao-treesit--node->sel (node &optional to-begin)
  "Return a `kao-sel' spanning NODE.
Forward form is (anchor NODE-start, cursor NODE-end-1) — byte-identical to
`kao-surround--treesit-element' for the whole object; TO-BEGIN gives the
reversed (backward) form."
  (kao-treesit--span->sel (treesit-node-start node) (treesit-node-end node)
                          to-begin))

(defun kao-treesit--lang-at (pos)
  "Return the tree-sitter language at POS, or nil.
Uses `treesit-language-at' when it resolves, else the first loaded parser's
language (so it works in buffers whose parser was created directly)."
  (or (and (fboundp 'treesit-language-at) (treesit-language-at pos))
      (let ((parsers (treesit-parser-list)))
        (and parsers (treesit-parser-language (car parsers))))))

(defun kao-treesit--container-at (pos)
  "Return the top-level node (child of root) enclosing POS, or nil.
Querying this subtree rather than the whole buffer keeps object-at-point scoped
and fast while still containing the object's `around' capture and any
adjacent separator captures, which a tight per-point range would miss."
  (let ((node (treesit-node-at pos)))
    (when node
      (let ((parent (treesit-node-parent node)))
        (while (and parent (treesit-node-parent parent))
          (setq node parent
                parent (treesit-node-parent node)))
        node))))

(defvar kao-treesit--captures-memo nil
  "Command-scoped memo for `kao-treesit--matches' / `kao-treesit--captures'.
When non-nil it is a hash table (`:test #\='equal') keyed by a query scope\='s
start/end char span (a container for object insides, the buffer root for the
grouped `around' union), mapping to that scope\='s matches list.  A command that
maps an object query over several selections let-binds a fresh hash to it, so
selections sharing a scope pay one query per command (bound in
`kao-treesit-select-object' and `kao-treesit-filter-to-functions').  The
dynamic binding is discarded when the command returns, so nothing survives it
\(no premature cross-command cache).  Nil by default, so a lone call
queries directly.")

(defvar kao-treesit--grouped-cache 'unset
  "Memoised availability of the GROUPED `treesit-query-capture' argument.
The symbol `unset' means not yet probed; otherwise t or nil.  When the 6th arg
is accepted (`func-arity' max >= 6, Emacs 31+ — Emacs 29/30 top out at arity 5
and take the flat path, per the 2026-07-18 CI cross-version run and commit
0314e9a), `treesit-query-capture' can return per-match capture groups — kao
unions a match's same-name captures into one span, so a `(comment)+
@comment.around' block and a parameter with its separate comma-around each read
as one object (treesit-3).")

(defun kao-treesit--grouped-p ()
  "Non-nil when `treesit-query-capture' accepts the 6th GROUPED argument."
  (if (eq kao-treesit--grouped-cache 'unset)
      (setq kao-treesit--grouped-cache
            (condition-case nil
                (let ((max (cdr (func-arity #'treesit-query-capture))))
                  (or (eq max 'many) (and (integerp max) (>= max 6))))
              (error nil)))
    kao-treesit--grouped-cache))

(defun kao-treesit--matches-compute (scope patterns)
  "Return SCOPE's PATTERNS matches, one (CAPTURE-NAME . NODE) list per match.
Uses the GROUPED `treesit-query-capture' arg when available (per-match structure
preserved — a comment block, a parameter and its comma); otherwise each capture
is its own singleton match (the flat shape)."
  (let (acc)
    (dolist (q patterns)
      (if (kao-treesit--grouped-p)
          (setq acc (nconc acc
                           (condition-case nil
                               ;; Call via a VARIABLE (not a direct call or a
                               ;; `funcall' of a quoted symbol — the byte-compiler
                               ;; resolves both back to an arity-checked direct call)
                               ;; so the Emacs 29/30 compiler cannot static-check this
                               ;; 6-arg GROUPED form, where `treesit-query-capture' has
                               ;; max arity 5 and byte-compile-error-on-warn t would
                               ;; make the callargs warning fatal.  `kao-treesit--grouped-p'
                               ;; gates this branch OFF at runtime on those versions.
                               (let ((query-capture
                                      (symbol-function 'treesit-query-capture)))
                                 (funcall query-capture scope q nil nil nil t))
                             (error (mapcar #'list
                                            (treesit-query-capture scope q))))))
        (setq acc (nconc acc (mapcar #'list (treesit-query-capture scope q))))))
    acc))

(defun kao-treesit--matches (scope patterns)
  "Return SCOPE's PATTERNS matches (see `kao-treesit--matches-compute').
When `kao-treesit--captures-memo' is a live hash the result is cached under
SCOPE\='s `(start . end)' span, so selections sharing a scope query it once per
command rather than once per selection."
  (if kao-treesit--captures-memo
      (let* ((key (cons (treesit-node-start scope) (treesit-node-end scope)))
             (hit (gethash key kao-treesit--captures-memo 'miss)))
        (if (eq hit 'miss)
            (puthash key (kao-treesit--matches-compute scope patterns)
                     kao-treesit--captures-memo)
          hit))
    (kao-treesit--matches-compute scope patterns)))

(defun kao-treesit--captures (container patterns)
  "Return all (CAPTURE-NAME . NODE) pairs of PATTERNS within CONTAINER (flat).
The per-match grouping from `kao-treesit--matches' is flattened; memoised per
command under CONTAINER\='s span."
  (apply #'append (kao-treesit--matches container patterns)))

(defun kao-treesit--grouped-around-spans (scope patterns base)
  "Return per-match `around' spans (BEG . END) for BASE from PATTERNS in SCOPE.
Each grouped match's @BASE.around captures are unioned (min start, max end) into
one span, so a `(comment)+ @comment.around' match becomes the whole block and a
parameter's separate comma-around merges with its parameter.  Nil when the
grouped `treesit-query-capture' arg is unavailable (treesit-3)."
  (when (kao-treesit--grouped-p)
    (let (spans)
      (dolist (match (kao-treesit--matches scope patterns))
        (let (beg end)
          (dolist (cap match)
            (let ((n (kao-treesit--normalize-capture (car cap))))
              (when (and n (equal (car n) base) (eq (cdr n) 'whole))
                (let ((s (treesit-node-start (cdr cap)))
                      (e (treesit-node-end (cdr cap))))
                  (setq beg (if beg (min beg s) s))
                  (setq end (if end (max end e) e))))))
          (when beg (push (cons beg end) spans))))
      (nreverse spans))))

(defun kao-treesit--dedup-spans (spans)
  "Return SPANS with duplicate (BEG . END) conses removed, input order kept."
  (let ((seen (make-hash-table :test #'equal)) out)
    (dolist (sp spans (nreverse out))
      (unless (gethash sp seen) (puthash sp t seen) (push sp out)))))

(defun kao-treesit--sort-spans (spans)
  "Return SPANS sorted smallest-span first, ties by earliest start (levels)."
  (sort (copy-sequence spans)
        (lambda (a b)
          (let ((sa (- (cdr a) (car a))) (sb (- (cdr b) (car b))))
            (if (= sa sb) (< (car a) (car b)) (< sa sb))))))

(defun kao-treesit--spans-covering (spans pos)
  "Return the SPANS (BEG . END) that cover POS (BEG <= POS < END)."
  (let (out)
    (dolist (sp spans (nreverse out))
      (when (and (<= (car sp) pos) (< pos (cdr sp))) (push sp out)))))

(defun kao-treesit--base-nodes (caps base part)
  "Return the nodes in CAPS whose capture normalises to (BASE . PART).
PART is `whole' or `inner' (see `kao-treesit--normalize-capture')."
  (let (out)
    (dolist (c caps)
      (let ((n (kao-treesit--normalize-capture (car c))))
        (when (and n (equal (car n) base) (eq (cdr n) part))
          (push (cdr c) out))))
    (nreverse out)))

(defun kao-treesit--covering (nodes pos)
  "Return the NODES that cover POS (NODE-start <= POS < NODE-end)."
  (let (out)
    (dolist (n nodes)
      (when (and (<= (treesit-node-start n) pos) (< pos (treesit-node-end n)))
        (push n out)))
    (nreverse out)))

(defun kao-treesit--dedup-nodes (nodes)
  "Return NODES with duplicate spans (same start and end) removed.
The first node of each (start . end) span is kept and input order is preserved.
Membership is hash-backed (O(n)) rather than a `member' list scan; the dedup key
is the span, and distinct node objects can share one, so `delete-dups' on the
nodes themselves would not collapse them."
  (let ((seen (make-hash-table :test #'equal)) out)
    (dolist (n nodes)
      (let ((k (cons (treesit-node-start n) (treesit-node-end n))))
        (unless (gethash k seen)
          (puthash k t seen)
          (push n out))))
    (nreverse out)))

(defun kao-treesit--smallest (nodes)
  "Return NODES sorted smallest-span first, ties broken by earliest start.
The Nth element is the Nth enclosing object (0 = innermost),."
  (sort (copy-sequence nodes)
        (lambda (a b)
          (let ((sa (- (treesit-node-end a) (treesit-node-start a)))
                (sb (- (treesit-node-end b) (treesit-node-start b))))
            (if (= sa sb)
                (< (treesit-node-start a) (treesit-node-start b))
              (< sa sb))))))

(defun kao-treesit--contains-inside-p (node insides)
  "Non-nil when some node in INSIDES is contained within NODE."
  (let (hit)
    (dolist (i insides hit)
      (when (and (>= (treesit-node-start i) (treesit-node-start node))
                 (<= (treesit-node-end i) (treesit-node-end node)))
        (setq hit t)))))

(defun kao-treesit--self-captured-p (node insides)
  "Non-nil when NODE's exact span is also captured in INSIDES (an around==inside).
The self-captured comment shape: a lone `(comment)' bound to both `.around' and
`.inside', with no separate separator node of its own."
  (let ((s (treesit-node-start node)) (e (treesit-node-end node)) hit)
    (dolist (i insides hit)
      (when (and (= (treesit-node-start i) s) (= (treesit-node-end i) e))
        (setq hit t)))))

(defun kao-treesit--all-self-captured-p (arounds insides)
  "Non-nil when every node in AROUNDS is self-captured in INSIDES (no separators).
True for the `(comment) @x.inside' + `(comment)+ @x.around' shape; false when a
separator-only around is present (a parameter list's comma), so those spans stay
per-object."
  (and arounds
       (let ((all t))
         (dolist (a arounds all)
           (unless (kao-treesit--self-captured-p a insides) (setq all nil))))))

(defun kao-treesit--only-whitespace-between-p (a b)
  "Non-nil when the buffer text in [A, B) is empty or only whitespace."
  (or (>= a b)
      (save-excursion
        (goto-char a)
        (skip-chars-forward " \t\n\r\f" b)
        (>= (point) b))))

(defun kao-treesit--around-span (a0 arounds insides)
  "Return (BEG . END) for the `around' object based on A0, unioning separators.
A0 is the chosen `around' node covering point; AROUNDS / INSIDES are all the
`base.around' / `base.inside' nodes in the container.  Trailing `around' nodes
that follow A0 (a parameter's `,' is captured `@parameter.around' as a node of
its own) are absorbed up to the next object's `inside', but only when they
carry no `inside' of their own — so a real neighbouring object is never merged."
  (let* ((beg (treesit-node-start a0))
         (end (treesit-node-end a0))
         (next-inside (let ((m most-positive-fixnum))
                        (dolist (i insides m)
                          (let ((s (treesit-node-start i)))
                            (when (and (>= s end) (< s m)) (setq m s))))))
         (changed t))
    (while changed
      (setq changed nil)
      (dolist (n arounds)
        (let ((ns (treesit-node-start n)) (ne (treesit-node-end n)))
          (when (and (>= ns end) (> ne end) (<= ne next-inside)
                     (not (kao-treesit--contains-inside-p n insides)))
            (setq end ne changed t)))))
    ;; treesit-3 flat fallback (Emacs without GROUPED): when EVERY around is its
    ;; own inside — the self-captured `(comment) @x.inside' + `(comment)+
    ;; @x.around' shape, no separator arounds — extend A0 over immediately-
    ;; following self-captured arounds separated only by whitespace, so a comment
    ;; block reads as one object.  A parameter list (its comma-around is not
    ;; self-captured) never qualifies, so per-argument spans stay untouched.
    (when (kao-treesit--all-self-captured-p arounds insides)
      (let ((grow t))
        (while grow
          (setq grow nil)
          (dolist (n arounds)
            (let ((ns (treesit-node-start n)) (ne (treesit-node-end n)))
              (when (and (> ns end) (> ne end)
                         (kao-treesit--self-captured-p n insides)
                         (kao-treesit--only-whitespace-between-p end ns))
                (setq end ne grow t)))))))
    (cons beg end)))

(defun kao-treesit--absorb-leading-sep (span arounds insides)
  "Extend SPAN start over a leading separator for a non-first last argument.
Mirrors Kakoune `select_argument' (selectors.cc:728, the non-first last-arg
branch): the last argument — one with no trailing separator absorbed — that is
not the first absorbs the separator preceding it, so `<a-a>u' on the final
parameter includes its leading comma (treesit-2).  AROUNDS / INSIDES are the
container\\='s `base.around' / `base.inside' nodes; a separator-only around
carries no `inside' of its own (a `,' between arguments).  SPAN is returned
unchanged when it already ends at a separator (a non-last argument keeps its
trailing one), is the first argument, or no preceding separator-only around
exists (functions and comment blocks have none)."
  (let* ((beg (car span)) (end (cdr span))
         (seps (let (out)
                 (dolist (n arounds (nreverse out))
                   (unless (kao-treesit--contains-inside-p n insides)
                     (push n out)))))
         (trailing (let (hit)
                     (dolist (n seps hit)
                       (when (= (treesit-node-end n) end) (setq hit t)))))
         (non-first (let (hit)
                      (dolist (i insides hit)
                        (when (< (treesit-node-start i) beg) (setq hit t)))))
         (lead (unless (or trailing (not non-first))
                 (let (best)
                   (dolist (n seps best)
                     (let ((ne (treesit-node-end n)))
                       (when (and (<= ne beg)
                                  (or (null best)
                                      (> ne (treesit-node-end best))))
                         (setq best n))))))))
    (if lead (cons (treesit-node-start lead) end) span)))

(defun kao-treesit--inside-in (a0 insides pos)
  "Return the INSIDES node within A0 for POS, or nil.
When an inside covers POS the SMALLEST such is returned (the innermost body).
Otherwise — POS on A0\\='s header, before or after its body, so no inside covers
it — fall back to the LARGEST inside contained in A0 (treesit-5): largest so the
outer body, not a nested function\\='s, is chosen when POS is on the outer
header.  Nil when A0 contains no inside at all."
  (let (covering contained)
    (dolist (i insides)
      (when (and (>= (treesit-node-start i) (treesit-node-start a0))
                 (<= (treesit-node-end i) (treesit-node-end a0)))
        (push i contained)
        (when (and (<= (treesit-node-start i) pos) (< pos (treesit-node-end i)))
          (push i covering))))
    (if covering
        (car (kao-treesit--smallest (kao-treesit--dedup-nodes covering)))
      (car (last (kao-treesit--smallest (kao-treesit--dedup-nodes contained)))))))

;; Around-object dispatch seam — one place, two version-split engines.
;;
;; The `(or ...)' in `kao-treesit--around-spans-at' below IS the version
;; dispatch.  It picks the GROUPED union engine (`kao-treesit--grouped-around-spans',
;; gated by `kao-treesit--grouped-p') on Emacs 31+, where `treesit-query-capture'
;; accepts the 6th GROUPED argument (`func-arity' max >= 6).  On Emacs 29/30,
;; which top out at arity 5, `--grouped-around-spans' returns nil and the flat
;; `kao-treesit--around-span' heuristic runs instead.  BOTH are supported
;; runtimes (kao.el Package-Requires (emacs "29.1")) and BOTH are CI-tested (the
;; 2026-07-18 cross-version run on 29/30/31; commit 0314e9a), so NEITHER engine
;; is dead — the kickoff batch's "delete the flat engine" premise is refused
;; with evidence (see the decision note).
;;
;; The flat `kao-treesit--around-span' builder is ALSO called
;; version-INDEPENDENTLY inside the nested-spans walk of
;; `kao-treesit--make-nested-selector' (the `<a-A>'/`<a-I>' path further down),
;; so it stays live on EVERY version regardless of this seam — a second,
;; independent reason it is not removable.
(defun kao-treesit--around-spans-at (lang patterns base)
  "Return `around' spans (BEG . END) for BASE from PATTERNS in the LANG tree.
GROUPED (Emacs 31+): each match's arounds unioned, queried over the whole tree
so a comment RUN spanning several top-level siblings is one block (treesit-3).
Flat fallback (Emacs 29/30): each @BASE.around node over the whole tree, its
trailing separators / adjacent self-captured siblings absorbed via
`kao-treesit--around-span'.  Both engine paths then run through
`kao-treesit--absorb-leading-sep' so the non-first last argument takes its
leading separator (treesit-2) — this single-argument funnel is the non-nested
`<a-a>' path; the nested selector keeps `select_nested_arguments' semantics."
  (let* ((root (treesit-buffer-root-node lang))
         (caps (kao-treesit--captures root patterns))
         (arounds (kao-treesit--base-nodes caps base 'whole))
         (insides (kao-treesit--base-nodes caps base 'inner))
         (spans (or (kao-treesit--grouped-around-spans root patterns base)
                    (mapcar (lambda (a) (kao-treesit--around-span a arounds insides))
                            arounds))))
    (mapcar (lambda (sp) (kao-treesit--absorb-leading-sep sp arounds insides))
            spans)))

(defun kao-treesit--object-at (pos base part level)
  "Return a `kao-sel' for the BASE object at POS, or nil.
BASE is a capture base-name string (e.g. \"function\"); PART is `around' or
`inside'; LEVEL is the Nth enclosing object (0 = innermost).  `around'
returns the smallest per-match `@base.around' span covering POS (ties ->
earliest start), each match's captures unioned (treesit-3); `inside' returns the
`@base.inside' contained in the covering around.  Nil when tree-sitter is not
ready or nothing matches."
  (when (kao-treesit-ready-p)
    (let* ((lang (kao-treesit--lang-at pos))
           (patterns (and lang (kao-treesit--patterns-for lang))))
      (when patterns
        (pcase part
          ('around
           (let* ((spans (kao-treesit--dedup-spans
                          (kao-treesit--around-spans-at lang patterns base)))
                  (cover (kao-treesit--sort-spans
                          (kao-treesit--spans-covering spans pos)))
                  (sp (nth (or level 0) cover)))
             (when sp (kao-treesit--span->sel (car sp) (cdr sp)))))
          ('inside
           (let* ((container (kao-treesit--container-at pos))
                  (caps (and container (kao-treesit--captures container patterns)))
                  (arounds (kao-treesit--base-nodes caps base 'whole))
                  (insides (kao-treesit--base-nodes caps base 'inner))
                  (cover (kao-treesit--smallest
                          (kao-treesit--dedup-nodes
                           (kao-treesit--covering arounds pos))))
                  (a0 (nth (or level 0) cover)))
             (when a0
               (let ((inner (kao-treesit--inside-in a0 insides pos)))
                 (and inner (kao-treesit--node->sel inner))))))
          (_ nil))))))

;;;; Object-register factories + setup (Phase 1 — text objects MVP)

(defun kao-treesit--make-selector (base)
  "Return an object selector for capture BASE over the tree-sitter engine.
The `kao-object-register' SELECTOR contract `(SEL INNER TO-BEGIN TO-END
&optional LEVEL)' -> `kao-sel' or nil.  Resolves the BASE object at SEL's
cursor via `kao-treesit--object-at' (INNER picks `.inside', else `.around';
LEVEL is the Nth enclosing) and orients it: the whole object for `<a-a>'/`<a-i>'
\(TO-BEGIN and TO-END both set), else a to-begin / to-end selection from the
cursor.  Nil when not ready or no match (the dispatcher then drops it)."
  (lambda (sel inner to-begin to-end &optional level)
    (let* ((cur (kao-sel-cursor sel))
           (part (if inner 'inside 'around))
           (obj (kao-treesit--object-at cur base part (or level 0))))
      (when obj
        (let ((ob (kao-sel-min obj)) (oe (kao-sel-max obj)))
          (cond
           ((and to-begin to-end) (kao-sel-make :anchor ob :cursor oe))
           (to-begin (kao-sel-make :anchor cur :cursor ob))
           (t (kao-sel-make :anchor cur :cursor oe))))))))

(defun kao-treesit--around-for (inside arounds)
  "Return the smallest node in AROUNDS that contains INSIDE, or nil."
  (car (kao-treesit--smallest
        (let (out)
          (dolist (a arounds)
            (when (and (<= (treesit-node-start a) (treesit-node-start inside))
                       (>= (treesit-node-end a) (treesit-node-end inside)))
              (push a out)))
          out))))

(defun kao-treesit--make-nested-selector (base)
  "Return a nested object selector for capture BASE.
The `kao-object-register' NESTED-SELECTOR contract `(BEG END INNER &optional
LEVEL)' -> a list of (FIRST . LAST) char spans (LAST inclusive), powering
`<a-A>' / `<a-I>'.  Anchored on each `@base.inside' contained in [BEG, END) (one
per object): INNER returns that inside; the whole object returns its enclosing
`@base.around' unioned with any trailing separator, so `<a-A>u' spans param +
comma like the regex argument object.  Deduplicated and ascending by start; nil
when not ready or none match."
  (lambda (beg end inner &optional _level)
    (when (kao-treesit-ready-p)
      (let* ((lang (kao-treesit--lang-at beg))
             (patterns (and lang (kao-treesit--patterns-for lang)))
             (root (and patterns (treesit-buffer-root-node lang))))
        (when root
          (let (insides arounds spans)
            (dolist (q patterns)
              (dolist (cap (treesit-query-capture root q beg end))
                (let ((n (kao-treesit--normalize-capture (car cap)))
                      (node (cdr cap)))
                  (when (and n (equal (car n) base))
                    (cond ((eq (cdr n) 'inner) (push node insides))
                          ((eq (cdr n) 'whole) (push node arounds)))))))
            (dolist (i (kao-treesit--dedup-nodes insides))
              (when (and (>= (treesit-node-start i) beg)
                         (<= (treesit-node-end i) end)
                         (< (treesit-node-start i) (treesit-node-end i)))
                (if inner
                    (push (cons (treesit-node-start i)
                                (1- (treesit-node-end i)))
                          spans)
                  (let* ((a0 (kao-treesit--around-for i arounds))
                         (sp (if a0
                                 (kao-treesit--around-span a0 arounds insides)
                               (cons (treesit-node-start i)
                                     (treesit-node-end i)))))
                    (push (cons (car sp) (1- (cdr sp))) spans)))))
            (sort (delete-dups spans)
                  (lambda (a b) (< (car a) (car b))))))))))

;;;; Tree motions + `<space> t' menu commands (Phase 2)

(defun kao-treesit--no-parser-message ()
  "Emit the friendly no-op message when no tree-sitter parser is loaded."
  (message "kao-treesit: no tree-sitter parser in this buffer"))

(defun kao-treesit--node-of-sel (sel)
  "Return the smallest named node covering SEL, or nil.
Returns nil in an empty buffer (`point-min' = `point-max') — the friendly
no-op path callers already treat as \"no node\" — and clamps the end
passed to `treesit-node-on' at `point-max', so a selection at end-of-buffer
never signals `args-out-of-range' (treesit-4)."
  (unless (= (point-min) (point-max))
    (let ((node (treesit-node-on (kao-sel-min sel)
                                 (min (1+ (kao-sel-max sel)) (point-max)))))
      (while (and node (not (treesit-node-check node 'named)))
        (setq node (treesit-node-parent node)))
      node)))

(defun kao-treesit--named-parent (node)
  "Return NODE's nearest named ancestor, or nil."
  (let ((p (treesit-node-parent node)))
    (while (and p (not (treesit-node-check p 'named)))
      (setq p (treesit-node-parent p)))
    p))

(defun kao-treesit--larger-named-parent (node smin smax)
  "Return NODE's nearest named ancestor whose span strictly exceeds [SMIN, SMAX].
Climbs `kao-treesit--named-parent' past ancestors that merely re-wrap the same
span — single-child chains such as a JS no-semicolon `expression_statement' or a
YAML `block_node' > `block_mapping' — so expand / parent motions make visible
progress on every press; nil at the root (treesit-0)."
  (let ((p (kao-treesit--named-parent node)))
    (while (and p
                (= (treesit-node-start p) smin)
                (= (1- (treesit-node-end p)) smax))
      (setq p (kao-treesit--named-parent p)))
    p))

(defun kao-treesit--first-smaller-child (node)
  "Return NODE's first named descendant with a strictly smaller span, or nil.
Descends first-child links past levels that re-wrap NODE's whole span (single
descendant chains) so a child motion lands on a smaller node (treesit-0)."
  (let ((c (and (> (treesit-node-child-count node t) 0)
                (treesit-node-child node 0 t))))
    (while (and c
                (= (treesit-node-start c) (treesit-node-start node))
                (= (treesit-node-end c) (treesit-node-end node))
                (> (treesit-node-child-count c t) 0))
      (setq c (treesit-node-child c 0 t)))
    c))

;;;; Select commands — set each selection to the object under its cursor

(defun kao-treesit-select-object (base &optional part)
  "Set each selection to the BASE object at its cursor (PART `around'/`inside').
BASE is a capture base-name string; PART defaults to `around'.  Interactively,
BASE is read with completion over `kao-treesit-objects' (the allow-list), so
user-added query objects are reachable without a bespoke command.  A cursor with
no such object is dropped (`kao-map-selections'); a friendly no-op message
when no parser is loaded.  A fresh command-scoped captures memo
\(`kao-treesit--captures-memo') is bound so cursors sharing a container query it
once; the binding dies with the command."
  (interactive
   (progn
     (kao--assert-mode)                   ; guard before the completing-read prompt
     (list (completing-read "Treesit object: " kao-treesit-objects nil t) 'around)))
  (kao--assert-mode)                      ; also covers the non-interactive delegate calls
  (setq part (or part 'around))
  (if (kao-treesit-ready-p)
      (let ((kao-treesit--captures-memo (make-hash-table :test #'equal)))
        (kao-map-selections
         (lambda (sel) (kao-treesit--object-at (kao-sel-cursor sel) base part 0))))
    (kao-treesit--no-parser-message)))

(defun kao-treesit-select-function ()
  "Select the whole function around each cursor (`<space> t f')."
  (interactive) (kao-treesit-select-object "function" 'around))

(defun kao-treesit-select-function-inside ()
  "Select the inside of the function around each cursor (`<space> t F')."
  (interactive) (kao-treesit-select-object "function" 'inside))

(defun kao-treesit-select-class ()
  "Select the whole class around each cursor (`<space> t c')."
  (interactive) (kao-treesit-select-object "class" 'around))

(defun kao-treesit-select-class-inside ()
  "Select the inside of the class around each cursor (`<space> t C')."
  (interactive) (kao-treesit-select-object "class" 'inside))

(defun kao-treesit-select-parameter ()
  "Select the whole parameter around each cursor (`<space> t a')."
  (interactive) (kao-treesit-select-object "parameter" 'around))

(defun kao-treesit-select-parameter-inside ()
  "Select the inside of the parameter around each cursor (`<space> t A')."
  (interactive) (kao-treesit-select-object "parameter" 'inside))

(defun kao-treesit-select-comment ()
  "Select the comment around each cursor (`<space> t o')."
  (interactive) (kao-treesit-select-object "comment" 'around))

(defun kao-treesit-select-test ()
  "Select the test around each cursor (`<space> t T')."
  (interactive) (kao-treesit-select-object "test" 'around))

;;;; Stateless motions — parent / first-child / select-node / siblings / goto

(defun kao-treesit--parent-motion-sel (sel)
  "Return SEL grown to its first strictly-larger-span named ancestor, else SEL.
Climbs past same-span ancestors so `<space> t P' makes visible progress on
single-child chains (treesit-0)."
  (let* ((node (kao-treesit--node-of-sel sel))
         (p (and node (kao-treesit--larger-named-parent
                       node (kao-sel-min sel) (kao-sel-max sel)))))
    (if p (kao-treesit--node->sel p) sel)))

(defun kao-treesit--first-child-sel (sel)
  "Return SEL set to the first named child of its covering node, else SEL.
Descends past same-span single-child levels so the child is a visibly smaller
span (treesit-0)."
  (let* ((node (kao-treesit--node-of-sel sel))
         (c (and node (kao-treesit--first-smaller-child node))))
    (if c (kao-treesit--node->sel c) sel)))

(defun kao-treesit--select-node-sel (sel)
  "Return SEL set to its smallest covering named node, else SEL."
  (let ((node (kao-treesit--node-of-sel sel)))
    (if node (kao-treesit--node->sel node) sel)))

(defun kao-treesit--sibling-sel (sel forward)
  "Return SEL set to the next (FORWARD) or previous named sibling, else SEL.
No-ascend: at an edge with no sibling the selection is left unchanged."
  (let* ((node (kao-treesit--node-of-sel sel))
         (sib (and node (if forward (treesit-node-next-sibling node t)
                          (treesit-node-prev-sibling node t)))))
    (if sib (kao-treesit--node->sel sib) sel)))

(defun kao-treesit--adjacent-node (nodes cur forward)
  "Return the node in NODES adjacent to CUR (FORWARD = after), or nil.
NODES is ascending by start.  When CUR is inside a node, step to the next /
previous by source order; otherwise the nearest with start after / before CUR."
  (let ((idx nil) (best nil) (i 0))
    (dolist (nd nodes)               ; innermost function covering CUR
      (when (and (<= (treesit-node-start nd) cur) (< cur (treesit-node-end nd)))
        (let ((span (- (treesit-node-end nd) (treesit-node-start nd))))
          (when (or (null best) (< span best)) (setq best span idx i))))
      (setq i (1+ i)))
    (if idx
        (let ((j (if forward (1+ idx) (1- idx))))
          (and (>= j 0) (< j (length nodes)) (nth j nodes)))
      (let (target)                  ; not inside a function: nearest by start
        (if forward
            (catch 'hit
              (dolist (nd nodes)
                (when (> (treesit-node-start nd) cur) (throw 'hit nd))))
          (dolist (nd nodes)
            (when (< (treesit-node-start nd) cur) (setq target nd)))
          target)))))

(defun kao-treesit--base-whole-nodes (lang base)
  "Return LANG's BASE `.around' nodes, deduped and ascending by start.
Scans the whole buffer for BASE\\='s `.around' captures once (allowed for
goto).  Nil when LANG has no loaded parser/query or the buffer holds no BASE
objects."
  (let* ((patterns (and lang (kao-treesit--patterns-for lang)))
         (root (and patterns (treesit-buffer-root-node lang)))
         nodes)
    (when root
      (dolist (q patterns)
        (dolist (cap (treesit-query-capture root q))
          (let ((n (kao-treesit--normalize-capture (car cap))))
            (when (and n (equal (car n) base) (eq (cdr n) 'whole))
              (push (cdr cap) nodes)))))
      (sort (kao-treesit--dedup-nodes nodes)
            (lambda (a b)
              (< (treesit-node-start a) (treesit-node-start b)))))))

(defun kao-treesit--memo-base-nodes (box lang base)
  "Return LANG's BASE nodes from BOX, scanning once per language and caching.
BOX is a one-cons cell whose car holds a `(LANG . NODES)' alist.  The
whole-buffer scan (`kao-treesit--base-whole-nodes') runs the first time LANG is
seen and its result is cached in BOX, so a multi-cursor goto command pays one
query per language, not one per selection.  Because embedded
parsers can give different cursors different languages, the memo is keyed per
language; BASE is fixed for the command, so keying by LANG alone suffices.  BOX
is command-scoped (freshly allocated per command, discarded on return) so no
result survives the command."
  (let ((hit (assq lang (car box))))
    (if hit
        (cdr hit)
      (let ((nodes (kao-treesit--base-whole-nodes lang base)))
        (push (cons lang nodes) (car box))
        nodes))))

(defun kao-treesit--goto-sel (sel base forward end extend box)
  "Return SEL moved to the next (FORWARD) or previous BASE object, else SEL.
Without EXTEND the whole object is selected with the cursor at its START, or its
END when END is non-nil (the nvim start/end matrix).  With EXTEND the result is
the zero-width boundary point (start, or END\\='s last char) so
`kao-sel-extend-to' keeps the caller\\='s anchor and only moves the cursor there.
BOX carries the command-scoped `(LANG . NODES)' memo (see
`kao-treesit--memo-base-nodes'), so the whole-buffer BASE `.around' scan runs
once per language per command rather than once per selection."
  (let* ((cur (kao-sel-cursor sel))
         (nodes (kao-treesit--memo-base-nodes box (kao-treesit--lang-at cur) base))
         (target (and nodes
                      (kao-treesit--adjacent-node nodes cur forward))))
    (cond
     ((null target) sel)
     (extend (let ((pt (if end (1- (treesit-node-end target))
                         (treesit-node-start target))))
               (kao-sel-make :anchor pt :cursor pt)))
     (t (kao-treesit--node->sel target (not end))))))

(defun kao-treesit-goto (base forward &optional end extend)
  "Move each cursor to the next (FORWARD non-nil) or previous BASE object.
BASE is a capture base-name string (see `kao-treesit-objects').  The whole
object is selected; the cursor lands at its START, or its END when END is
non-nil.  With EXTEND non-nil each selection is extended to that boundary (the
anchor is preserved) rather than replaced.  FORWARD x END give the four nvim
start/end x next/prev goto cells; wrap this to build them (the shipped
`kao-treesit-goto-next-function' / `-prev-function' are two such callers).  A
cursor with no BASE object ahead/behind is left unchanged; a friendly no-op
message when no parser is loaded.  The whole-buffer BASE scan runs once per
language per command."
  (if (kao-treesit-ready-p)
      (let* ((box (list nil))
             (fn (lambda (sel)
                   (kao-treesit--goto-sel sel base forward end extend box))))
        (if extend
            (kao-map-selections-extend fn)
          (kao-map-selections fn)))
    (kao-treesit--no-parser-message)))

(defmacro kao-treesit--defmotion (name args doc &rest body)
  "Define interactive motion command NAME guarded on `kao-treesit-ready-p'.
ARGS/DOC as `defun'; BODY runs only with a parser loaded, else a no-op message.
Every such command reads/writes `kao--sels', so it opens with `kao--assert-mode'
\ — the mode-off guard fires before any grammar work."
  (declare (indent 3) (doc-string 3))
  `(defun ,name ,args ,doc
          (interactive)
          (kao--assert-mode)
          (if (kao-treesit-ready-p) (progn ,@body)
            (kao-treesit--no-parser-message))))

(kao-treesit--defmotion kao-treesit-parent ()
  "Select the named parent of each selection (`<space> t P')."
  (kao-map-selections #'kao-treesit--parent-motion-sel))

(kao-treesit--defmotion kao-treesit-first-child ()
  "Select the first named child of each selection (`<space> t i')."
  (kao-map-selections #'kao-treesit--first-child-sel))

(kao-treesit--defmotion kao-treesit-select-node ()
  "Select the smallest named node covering each selection (`<space> t s')."
  (kao-map-selections #'kao-treesit--select-node-sel))

(kao-treesit--defmotion kao-treesit-next-sibling ()
  "Select the next named sibling of each selection (`<space> t ]')."
  (kao-map-selections (lambda (sel) (kao-treesit--sibling-sel sel t))))

(kao-treesit--defmotion kao-treesit-prev-sibling ()
  "Select the previous named sibling of each selection (`<space> t [')."
  (kao-map-selections (lambda (sel) (kao-treesit--sibling-sel sel nil))))

(kao-treesit--defmotion kao-treesit-extend-next-sibling ()
  "Extend each selection toward its next named sibling."
  (kao-map-selections-extend (lambda (sel) (kao-treesit--sibling-sel sel t))))

(kao-treesit--defmotion kao-treesit-extend-prev-sibling ()
  "Extend each selection toward its previous named sibling."
  (kao-map-selections-extend (lambda (sel) (kao-treesit--sibling-sel sel nil))))

(kao-treesit--defmotion kao-treesit-goto-next-function ()
  "Select the next function after each cursor (`<space> t n')."
  (kao-treesit-goto "function" t))

(kao-treesit--defmotion kao-treesit-goto-prev-function ()
  "Select the previous function before each cursor (`<space> t p')."
  (kao-treesit-goto "function" nil))

;;;; Expand / shrink — per-selection stack (stateful)

(defvar-local kao-treesit--expand-stack nil
  "Buffer-local stack of selection snapshots `kao-treesit-shrink' retraces.")

(defvar-local kao-treesit--expand-token nil
  "Validity token (SELECTIONS . MODIFIED-TICK) for the expand stack.
Reset when the selection list or buffer changed since the last expand/shrink, so
the stack never restores a stale span.")

(defun kao-treesit--expand-stale-p ()
  "Non-nil when the expand stack is stale (buffer or selections changed)."
  (or (null kao-treesit--expand-token)
      (not (equal (car kao-treesit--expand-token) (kao-get-selections)))
      (not (eql (cdr kao-treesit--expand-token) (buffer-chars-modified-tick)))))

(defun kao-treesit--expand-remember ()
  "Record the current selections and buffer tick as the stack's validity token."
  (setq kao-treesit--expand-token
        (cons (kao-get-selections) (buffer-chars-modified-tick))))

(defun kao-treesit--expand-sel (sel)
  "Return SEL grown one tree level, or SEL unchanged at the root.
Expand-region style: the covering node when SEL is strictly inside it, else the
first strictly-larger-span named ancestor — climbing past same-span single-child
ancestors so every press grows the selection and the climb reaches the root
rather than stalling (treesit-0)."
  (let ((node (kao-treesit--node-of-sel sel)))
    (if (null node)
        sel
      (let ((nb (treesit-node-start node)) (ne (1- (treesit-node-end node)))
            (smin (kao-sel-min sel)) (smax (kao-sel-max sel)))
        (if (and (= nb smin) (= ne smax))
            (let ((p (kao-treesit--larger-named-parent node smin smax)))
              (if p (kao-treesit--node->sel p) sel))
          (kao-treesit--node->sel node))))))

(defun kao-treesit--shrink-descend-sel (sel)
  "Return SEL descended to the named child covering its cursor, else SEL.
Descends past same-span single-child levels so shrink makes visible progress,
mirroring the expand climb (treesit-0)."
  (let* ((node (kao-treesit--node-of-sel sel))
         (cur (kao-sel-cursor sel))
         (child (and node (> (treesit-node-child-count node t) 0)
                     (kao-treesit--named-child-at node cur))))
    (while (and child
                (= (treesit-node-start child) (treesit-node-start node))
                (= (treesit-node-end child) (treesit-node-end node))
                (> (treesit-node-child-count child t) 0))
      (setq child (kao-treesit--named-child-at child cur)))
    (if child (kao-treesit--node->sel child) sel)))

(defun kao-treesit--named-child-at (node pos)
  "Return NODE's named child covering POS, else its first child, else nil."
  (let ((n (treesit-node-child-count node t)) (i 0) hit)
    (while (and (< i n) (not hit))
      (let ((c (treesit-node-child node i t)))
        (when (and (<= (treesit-node-start c) pos) (< pos (treesit-node-end c)))
          (setq hit c)))
      (setq i (1+ i)))
    (or hit (and (> n 0) (treesit-node-child node 0 t)))))

(defun kao-treesit-expand ()
  "Grow each selection to its enclosing tree node, pushing the shrink stack.
Bound to `<space> t e' and top-level `<a-ret>'.  `kao-treesit-shrink' retraces.
No-op (with a message) when no tree-sitter parser is loaded."
  (interactive)
  (kao--assert-mode)
  (if (not (kao-treesit-ready-p))
      (kao-treesit--no-parser-message)
    (when (kao-treesit--expand-stale-p) (setq kao-treesit--expand-stack nil))
    (push (kao-get-selections) kao-treesit--expand-stack)
    (kao-map-selections #'kao-treesit--expand-sel)
    (kao-treesit--expand-remember)))

(defun kao-treesit-shrink ()
  "Shrink each selection: pop the expand stack, else descend to the child node.
Bound to `<space> t E' and top-level `<a-S-ret>' (round-trips expand).
No-op (with a message) when no tree-sitter parser is loaded."
  (interactive)
  (kao--assert-mode)
  (if (not (kao-treesit-ready-p))
      (kao-treesit--no-parser-message)
    (when (kao-treesit--expand-stale-p) (setq kao-treesit--expand-stack nil))
    (if kao-treesit--expand-stack
        (kao-set-selections (pop kao-treesit--expand-stack))
      (kao-map-selections #'kao-treesit--shrink-descend-sel))
    (kao-treesit--expand-remember)))

;;;; Multi-select power (Phase 3)

(defun kao-treesit--named-children (node)
  "Return NODE's named children, in order."
  (treesit-node-children node t))

(defun kao-treesit--install-spans (spans empty-message)
  "Install SPANS (a list of (FIRST . LAST)) as selections, main = last.
Show EMPTY-MESSAGE and leave the list unchanged when SPANS is nil."
  (if (null spans)
      (message "%s" empty-message)
    (kao-set-selections
     (mapcar (lambda (sp)
               (kao-sel-make :anchor (car sp) :cursor (cdr sp)))
             spans)
     (1- (length spans)))))

(defun kao-treesit-select-all-functions ()
  "Select every function in the buffer, one selection each (`<space> t *').
Main becomes the last function.  No-op message when no parser is loaded."
  (interactive)
  (kao--assert-mode)
  (if (not (kao-treesit-ready-p))
      (kao-treesit--no-parser-message)
    (kao-treesit--install-spans
     (funcall (kao-treesit--make-nested-selector "function")
              (point-min) (point-max) nil)
     "kao-treesit: no functions in buffer")))

(defun kao-treesit-select-all-siblings ()
  "Select every named sibling of the node at point (its parent's children).
Main becomes the last sibling.  No-op message when no parser is loaded."
  (interactive)
  (kao--assert-mode)
  (if (not (kao-treesit-ready-p))
      (kao-treesit--no-parser-message)
    (let* ((node (kao-treesit--node-of-sel
                  (kao-sel-make :anchor (point) :cursor (point))))
           (parent (and node (kao-treesit--named-parent node)))
           (sibs (and parent (kao-treesit--named-children parent))))
      (kao-treesit--install-spans
       (mapcar (lambda (nd) (cons (treesit-node-start nd)
                                  (1- (treesit-node-end nd))))
               sibs)
       "kao-treesit: no siblings here"))))

(defun kao-treesit-filter-to-functions ()
  "Keep only selections whose cursor is inside a function (`<space> t /').
No-op with a message when that would drop every selection.  Selections sharing a
container pay one query per command via `kao-treesit--captures-memo';
the binding dies with the command."
  (interactive)
  (kao--assert-mode)
  (if (not (kao-treesit-ready-p))
      (kao-treesit--no-parser-message)
    (let ((kao-treesit--captures-memo (make-hash-table :test #'equal))
          kept)
      (dolist (sel (kao-get-selections))
        (when (kao-treesit--object-at (kao-sel-cursor sel) "function" 'around 0)
          (push sel kept)))
      (setq kept (nreverse kept))
      (if (null kept)
          (message "kao-treesit: no selection inside a function")
        (kao-set-selections kept (1- (length kept)))))))

(defun kao-treesit--scope-rows (pos)
  "Return info-box rows (KEY . \"type [beg-end]\") of named ancestors at POS.
Innermost first.  KEY is the depth as a printable digit char (`?0' = innermost),
so the box shows depth digits rather than control-char names."
  (let ((node (treesit-node-at pos)) chain)
    (while node
      (when (treesit-node-check node 'named) (push node chain))
      (setq node (treesit-node-parent node)))
    (setq chain (nreverse chain))       ; innermost first
    (let ((i 0) rows)
      (dolist (nd chain)
        (push (cons (+ ?0 i)
                    (format "%s [%d-%d]" (treesit-node-type nd)
                            (treesit-node-start nd) (treesit-node-end nd)))
              rows)
        (setq i (1+ i)))
      (nreverse rows))))

(defun kao-treesit-scopes ()
  "Show the named-ancestor chain at point in an info box (`<space> t ?').
Graceful no-op message when no tree-sitter parser is loaded."
  (interactive)
  (if (not (kao-treesit-ready-p))
      (kao-treesit--no-parser-message)
    (let ((rows (kao-treesit--scope-rows (point))))
      (if (null rows)
          (message "kao-treesit: no node at point")
        (kao-on-key "scopes (press any key)" #'ignore rows)))))

(defun kao-treesit-explore ()
  "Open `treesit-explore-mode' for the buffer (`<space> t t').
Graceful no-op message when no tree-sitter parser is loaded."
  (interactive)
  (if (kao-treesit-ready-p)
      (treesit-explore-mode)
    (kao-treesit--no-parser-message)))

;;;; Keymap

(defvar kao-treesit-tree-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" #'kao-treesit-select-function)
    (define-key map "F" #'kao-treesit-select-function-inside)
    (define-key map "c" #'kao-treesit-select-class)
    (define-key map "C" #'kao-treesit-select-class-inside)
    (define-key map "a" #'kao-treesit-select-parameter)
    (define-key map "A" #'kao-treesit-select-parameter-inside)
    (define-key map "o" #'kao-treesit-select-comment)
    (define-key map "T" #'kao-treesit-select-test)
    (define-key map "s" #'kao-treesit-select-node)
    (define-key map "P" #'kao-treesit-parent)
    (define-key map "i" #'kao-treesit-first-child)
    (define-key map "]" #'kao-treesit-next-sibling)
    (define-key map "[" #'kao-treesit-prev-sibling)
    (define-key map "n" #'kao-treesit-goto-next-function)
    (define-key map "p" #'kao-treesit-goto-prev-function)
    (define-key map "e" #'kao-treesit-expand)
    (define-key map "E" #'kao-treesit-shrink)
    (define-key map "*" #'kao-treesit-select-all-functions)
    (define-key map "/" #'kao-treesit-filter-to-functions)
    (define-key map "?" #'kao-treesit-scopes)
    (define-key map "t" #'kao-treesit-explore)
    map)
  "The `<space> t' tree menu keymap, bound by `kao-treesit-setup'.
A one-shot prefix (a nested keymap) mirroring the user's kak `tree' menu:
object select, tree motions, expand/shrink, multi-select, scopes, explore.")

(defconst kao-treesit--which-key-labels
  '(("f" . "function") ("F" . "function inside")
    ("c" . "class") ("C" . "class inside")
    ("a" . "parameter") ("A" . "parameter inside")
    ("o" . "comment") ("T" . "test")
    ("s" . "select node") ("P" . "parent") ("i" . "first child")
    ("]" . "next sibling") ("[" . "prev sibling")
    ("n" . "next function") ("p" . "prev function")
    ("e" . "expand") ("E" . "shrink")
    ("*" . "all functions") ("/" . "filter to functions")
    ("?" . "scopes") ("t" . "explore"))
  "Display-only which-key labels for `kao-treesit-tree-map'.")

(defun kao-treesit--register-which-key ()
  "Label `kao-treesit-tree-map' in which-key (display only; soft dependency).
A no-op when which-key is absent — the bindings themselves are never touched."
  (when (fboundp 'which-key-add-keymap-based-replacements)
    (dolist (row kao-treesit--which-key-labels)
      (which-key-add-keymap-based-replacements kao-treesit-tree-map
        (car row) (cdr row)))))

;;;###autoload
(defun kao-treesit--object-dispatch-memo (thunk)
  "Run THUNK with a fresh command-scoped `kao-treesit--captures-memo'.
Registered on `kao--object-dispatch-context-functions' by `kao-treesit-setup'
so an object-pending pass (`<a-i>f' / `<a-i>u' / `<a-.>') queries each container
once across every cursor that shares it, not once per cursor.  The
memo hash dies with the pass, so nothing survives it (no cross-command
cache).  MUST return `(funcall THUNK)' — the seam contract."
  (let ((kao-treesit--captures-memo (make-hash-table :test #'equal)))
    (funcall thunk)))

(defun kao-treesit-setup (&optional objects)
  "Enable kao-treesit.  Opt-in: kao's defaults are unchanged until called.
Binds the primary surface: the `<space> t' tree menu (which-key \"tree\")
and top-level `<a-RET>' / `<a-S-RET>' for expand / shrink.

With OBJECTS non-nil, also register the bonus faithful object-pending text
objects on the free keys: `f' (function) and an augmented `u' (parameter —
tree-sitter when a parser provides one, else the exact regex
`kao-object--argument', so bare `u' is unchanged for non-setup users);
this needs no keymap change, since the object-pending keys already dispatch
through the runtime object table.  Idempotent; safe to call more than once."
  (interactive)
  (kao-treesit--register-which-key)
  (define-key kao-user-map "t" kao-treesit-tree-map)
  (when (fboundp 'which-key-add-keymap-based-replacements)
    (which-key-add-keymap-based-replacements kao-user-map "t" "tree"))
  (define-key kao-normal-state-map (kbd "M-<return>") #'kao-treesit-expand)
  (define-key kao-normal-state-map (kbd "M-S-<return>") #'kao-treesit-shrink)
  (when objects
    ;; One tree query per container per object-pending command: the
    ;; memo wraps the whole `<a-i>f'/`<a-i>u'/`<a-.>' pass via the object
    ;; dispatch-context seam.  `add-hook' is idempotent, so re-running setup
    ;; does not stack duplicate wrappers.
    (add-hook 'kao--object-dispatch-context-functions
              #'kao-treesit--object-dispatch-memo)
    (kao-object-register ?f
                         (kao-treesit--make-selector "function")
                         "function"
                         (kao-treesit--make-nested-selector "function"))
    (let ((psel (kao-treesit--make-selector "parameter"))
          (pnst (kao-treesit--make-nested-selector "parameter")))
      (kao-object-register
       ?u
       (lambda (sel inner to-begin to-end &optional level)
         (or (funcall psel sel inner to-begin to-end level)
             (kao-object--argument sel inner to-begin to-end level)))
       "argument"
       (lambda (beg end inner &optional level)
         (or (funcall pnst beg end inner level)
             (kao-object--nested-arguments beg end inner level)))))))

(provide 'kao-treesit)
;;; kao-treesit.el ends here
