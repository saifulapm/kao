;;; kao-treesit-tests.el --- Tests for kao-treesit -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for kao-treesit.  The loader-algebra tests (split,
;; inherits, predicate shim, capture normalisation) are PURE and run without any
;; grammar.  The compile/cache tests `skip-unless' the bash/javascript grammars
;; and build HERMETIC fixture query directories with `make-temp-file' so they do
;; not depend on any machine-specific query install.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'treesit nil t)
(require 'kao-treesit)

;;;; Pure — splitter

(ert-deftest kao-treesit-split-two-patterns ()
  (should (equal (kao-treesit--split-patterns
                  "(function_definition) @function.around\n(comment) @comment.inside")
                 '("(function_definition) @function.around"
                   "(comment) @comment.inside"))))

(ert-deftest kao-treesit-split-wrapped-predicate-is-one ()
  ;; A predicate-grouped pattern (outer parens) stays a single chunk.
  (should (equal (kao-treesit--split-patterns
                  "((call) @x (#eq? @x \"t\")) @function.around\n(comment) @c")
                 '("((call) @x (#eq? @x \"t\")) @function.around"
                   "(comment) @c"))))

(ert-deftest kao-treesit-split-quantifier-and-comment ()
  ;; Trailing `+' belongs to the pattern; a `;' comment is not a pattern.
  (should (equal (kao-treesit--split-patterns
                  "(comment)+ @comment.around ; trailing\n(x) @y")
                 '("(comment)+ @comment.around ; trailing" "(x) @y"))))

(ert-deftest kao-treesit-split-string-with-parens ()
  ;; A `)' inside a string must not end the group early.
  (should (equal (kao-treesit--split-patterns
                  "((a) @x (#eq? @x \"f)(\")) @z\n(b) @y")
                 '("((a) @x (#eq? @x \"f)(\")) @z" "(b) @y"))))

(ert-deftest kao-treesit-split-bracket-group ()
  (should (equal (kao-treesit--split-patterns "[(a) (b)] @c")
                 '("[(a) (b)] @c"))))

(ert-deftest kao-treesit-split-real-bash-has-five ()
  ;; The real bash textobjects.scm (5 top-level patterns, incl. (comment)+).
  (should (= 5 (length (kao-treesit--split-patterns
    (concat "(function_definition\n  body: (_) @function.inside) @function.around\n\n"
            "(command\n  argument: (_) @parameter.inside)\n\n"
            "(comment) @comment.inside\n\n"
            "(comment)+ @comment.around\n\n"
            "(array\n  (_) @entry.around)"))))))

;;;; Pure — inherits resolution (synthetic reader)

(defun kao-treesit-tests--reader (alist)
  "Return a reader function over ALIST (lang -> text); missing -> \"\"."
  (lambda (lang) (or (cdr (assoc lang alist)) "")))

(ert-deftest kao-treesit-inherits-single ()
  (let ((out (kao-treesit--expand-inherits
              "; inherits: base\n(child) @c"
              (kao-treesit-tests--reader '(("base" . "(base) @b"))))))
    (should (string-match-p "(base) @b" out))
    (should (string-match-p "(child) @c" out))
    ;; inherited content precedes the inheriting file's own
    (should (< (string-match "(base) @b" out) (string-match "(child) @c" out)))))

(ert-deftest kao-treesit-inherits-multi-and-shared ()
  (let ((out (kao-treesit--expand-inherits
              "; inherits: _javascript,ecma\n(self) @s"
              (kao-treesit-tests--reader
               '(("_javascript" . "(jsp) @j") ("ecma" . "(ecp) @e"))))))
    (should (string-match-p "(jsp) @j" out))
    (should (string-match-p "(ecp) @e" out))
    (should (string-match-p "(self) @s" out))
    ;; treesit-1: the `; inherits:' line is REPLACED, not mangled — no residue
    ;; of the comma-separated language list survives (clobbered match data used
    ;; to leak `_javascript'/`ecma' and a dangling `inherits' fragment)
    (should-not (string-match-p "_javascript" out))
    (should-not (string-match-p "ecma" out))
    (should-not (string-match-p "inherits" out))))

(ert-deftest kao-treesit-inherits-no-language-residue ()
  "Multi-inherit with own trailing patterns leaves NO language-list residue and
keeps every pattern intact (treesit-1, save-match-data)."
  (let* ((alist '(("main" . "; inherits: langa,langb\n(main_a) @a.around\n(main_b) @b.around")
                  ("langa" . "(la1) @x.around\n(la2) @y.around")
                  ("langb" . "(lb1) @p.around\n(lb2) @q.around")))
         (reader (kao-treesit-tests--reader alist))
         (out (kao-treesit--expand-inherits (funcall reader "main") reader '("main"))))
    ;; every inherited and own pattern is present…
    (dolist (tok '("(la1)" "(la2)" "(lb1)" "(lb2)" "(main_a)" "(main_b)"))
      (should (string-match-p (regexp-quote tok) out)))
    ;; …and the language list / inherits directive left no textual residue
    (should-not (string-match-p "langa" out))
    (should-not (string-match-p "langb" out))
    (should-not (string-match-p "inherits" out))))

(ert-deftest kao-treesit-inherits-multi-compiles-all-patterns ()
  "A multi-inherit on-disk fixture compiles the FULL expected pattern count; a
clobbered inherits line drops the last inherited pattern (treesit-1)."
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "; inherits: shared_a,shared_b\n(comment) @comment.inside")
                  ("shared_a" . "(function_definition) @function.around")
                  ("shared_b" . "(command) @command.around\n(variable_assignment) @variable.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    ;; shared_a (1) + shared_b (2) + main's own (1) = 4, all valid bash patterns
    (should (= 4 (length (kao-treesit--patterns-for 'bash))))))

(ert-deftest kao-treesit-inherits-missing-skipped ()
  (let ((out (kao-treesit--expand-inherits
              "; inherits: nope\n(self) @s"
              (kao-treesit-tests--reader nil))))
    (should (string-match-p "(self) @s" out))
    (should-not (string-match-p "nope" out))))

(ert-deftest kao-treesit-inherits-cycle-terminates ()
  (let* ((alist '(("a" . "(a) @a\n; inherits: b")
                  ("b" . "(b) @b\n; inherits: a")))
         (reader (kao-treesit-tests--reader alist))
         (out (kao-treesit--expand-inherits (funcall reader "a") reader '("a"))))
    (should (string-match-p "(a) @a" out))
    (should (string-match-p "(b) @b" out))))

;;;; Pure — predicate shim

(ert-deftest kao-treesit-shim-keeps-supported ()
  (let ((s (kao-treesit--strip-unsupported-predicates
            "((id) @c (#eq? @c \"x\")) @y")))
    (should (string-match-p "#eq?" s))
    (should (string-match-p "(id) @c" s))))

(ert-deftest kao-treesit-shim-strips-any-of-and-set ()
  (let ((s (kao-treesit--strip-unsupported-predicates
            "((id) @c (#any-of? @c \"a\" \"b\") (#set! foo bar)) @y")))
    (should-not (string-match-p "#any-of?" s))
    (should-not (string-match-p "#set!" s))
    (should (string-match-p "(id) @c" s))))

(ert-deftest kao-treesit-shim-strips-with-parens-in-string ()
  ;; A `)' inside the stripped predicate's string must not truncate the removal.
  (let ((s (kao-treesit--strip-unsupported-predicates
            "((id) @c (#any-of? @c \"f)(\")) @y")))
    (should-not (string-match-p "any-of" s))
    (should (string-match-p "(id) @c" s))
    (should (string-match-p "@y" s))))

(ert-deftest kao-treesit-shim-mixed-keep-and-strip ()
  (let ((s (kao-treesit--strip-unsupported-predicates
            "((id) @c (#eq? @c \"x\") (#not-kind-eq? @c \"y\")) @z")))
    (should (string-match-p "#eq?" s))
    (should-not (string-match-p "not-kind-eq" s))))

;;;; Pure — capture normalisation

(ert-deftest kao-treesit-normalize-capture-suffixes ()
  (should (equal (kao-treesit--normalize-capture "function.around") '("function" . whole)))
  (should (equal (kao-treesit--normalize-capture "function.inside") '("function" . inner)))
  (should (equal (kao-treesit--normalize-capture "param.outer") '("param" . whole)))
  (should (equal (kao-treesit--normalize-capture "param.inner") '("param" . inner)))
  (should (equal (kao-treesit--normalize-capture 'class.around) '("class" . whole))))

(ert-deftest kao-treesit-normalize-capture-private-and-plain ()
  (should-not (kao-treesit--normalize-capture "_local.around"))
  (should-not (kao-treesit--normalize-capture "noSuffix"))
  (should-not (kao-treesit--normalize-capture 42)))

;;;; Grammar-backed — compile, cache, resilience (hermetic fixtures)

(defun kao-treesit-tests--bash-p ()
  "Non-nil when the bash grammar is available."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p 'bash)))

(defun kao-treesit-tests--fixture (alist)
  "Write ALIST of (LANG . QUERY-TEXT) as `<tmp>/LANG/textobjects.scm'.
Return the temp root directory."
  (let ((root (make-temp-file "kao-ts-fix" t)))
    (dolist (e alist)
      (let ((dir (expand-file-name (car e) root)))
        (make-directory dir t)
        (with-temp-file (expand-file-name "textobjects.scm" dir)
          (insert (cdr e)))))
    root))

(ert-deftest kao-treesit-patterns-for-drops-bad-node ()
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(function_definition body: (_) @function.inside) @function.around
(comment) @comment.inside
(bogus_node_xyz) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (let ((pats (kao-treesit--patterns-for 'bash)))
      ;; the unknown node type drops; the two valid patterns survive
      (should (= 2 (length pats)))
      (with-temp-buffer
        (insert "f() { echo hi; }\n# c\n")
        (treesit-parser-create 'bash)
        (should (treesit-query-capture (treesit-buffer-root-node) (car pats)))))))

(ert-deftest kao-treesit-patterns-for-caches-no-disk ()
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture '(("bash" . "(comment) @comment.inside"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (let ((first (kao-treesit--patterns-for 'bash)))
      (should first)
      ;; a 2nd call must hit the cache and never touch disk
      (cl-letf (((symbol-function 'kao-treesit--locate-query)
                 (lambda (&rest _) (error "disk hit"))))
        (should (eq first (kao-treesit--patterns-for 'bash)))))))

(ert-deftest kao-treesit-debug-logs-drop ()
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(bogus_xyz) @function.around\n(comment) @comment.inside"))))
         (kao-treesit-queries-dir (list root))
         (kao-treesit-debug t)
         (logs nil))
    (kao-treesit-reload-queries)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) logs))))
      (kao-treesit--patterns-for 'bash))
    (should (cl-some (lambda (m) (string-match-p "dropped pattern" m)) logs))))

(ert-deftest kao-treesit-patterns-for-inherits-on-disk ()
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "; inherits: base\n(comment) @comment.inside")
                  ("base" . "(comment)+ @comment.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    ;; both the inherited and own pattern compile (against the bash grammar)
    (should (= 2 (length (kao-treesit--patterns-for 'bash))))))

(ert-deftest kao-treesit-reload-queries-picks-up-edit-bash ()
  ;; treesit-7: reload clears BOTH the pattern cache and the auto-dir memo, so an
  ;; edited textobjects.scm is recompiled on the next lookup.
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture '(("bash" . "(comment) @comment.around"))))
         (scm (expand-file-name "bash/textobjects.scm" root))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    ;; prime: the single-pattern query compiles to 1 pattern and is cached
    (should (= 1 (length (kao-treesit--patterns-for 'bash))))
    ;; edit the file on disk to a TWO-pattern query
    (with-temp-file scm
      (insert "(function_definition) @function.around\n(comment) @comment.around"))
    ;; still cached (stale) until reload
    (should (= 1 (length (kao-treesit--patterns-for 'bash))))
    ;; dirty the auto-dir memo so we can prove reload resets it
    (setq kao-treesit--auto-dirs '("/stale/dir"))
    (kao-treesit-reload-queries)
    (should (eq kao-treesit--auto-dirs 'unset))
    ;; the recompiled patterns reflect the edit
    (should (= 2 (length (kao-treesit--patterns-for 'bash))))))

(ert-deftest kao-treesit-reload-queries-picks-up-repointed-dir-bash ()
  ;; treesit-7: reload also lets a re-pointed `kao-treesit-queries-dir' take.
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((dir-a (kao-treesit-tests--fixture '(("bash" . "(comment) @comment.around"))))
         (dir-b (kao-treesit-tests--fixture
                 '(("bash" . "(function_definition) @function.around\n(comment) @comment.around")))))
    (let ((kao-treesit-queries-dir (list dir-a)))
      (kao-treesit-reload-queries)
      (should (= 1 (length (kao-treesit--patterns-for 'bash)))))
    ;; re-point to dir-b, reload, and the new dir's query wins
    (let ((kao-treesit-queries-dir (list dir-b)))
      (kao-treesit-reload-queries)
      (should (= 2 (length (kao-treesit--patterns-for 'bash)))))))

(ert-deftest kao-treesit-queries-dir-set-invalidates-caches ()
  ;; treesit-7: the defcustom `:set' re-points the path AND invalidates BOTH the
  ;; pattern cache and the auto-dir memo (via `kao-treesit-reload-queries').
  (let ((setter (get 'kao-treesit-queries-dir 'custom-set))
        (orig (default-value 'kao-treesit-queries-dir)))
    (should setter)
    (unwind-protect
        (progn
          (puthash "zz-sentinel" '(dummy) kao-treesit--pattern-cache)
          (setq kao-treesit--auto-dirs '("/stale"))
          (funcall setter 'kao-treesit-queries-dir nil)
          (should (equal nil (default-value 'kao-treesit-queries-dir)))
          (should (eq kao-treesit--auto-dirs 'unset))
          (should (eq 'miss (gethash "zz-sentinel" kao-treesit--pattern-cache 'miss))))
      (set-default 'kao-treesit-queries-dir orig)
      (kao-treesit-reload-queries))))

(ert-deftest kao-treesit-shim-makes-unsupported-predicate-capturable ()
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "((word) @parameter.inside (#any-of? @parameter.inside \"x\")) @parameter.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (let ((pats (kao-treesit--patterns-for 'bash)))
      (should (= 1 (length pats)))
      (with-temp-buffer
        (insert "echo x\n")
        (treesit-parser-create 'bash)
        ;; would signal `treesit-query-error: Invalid predicate' if #any-of?
        ;; had survived; the shim stripped it, so capture succeeds.
        (should (treesit-query-capture (treesit-buffer-root-node) (car pats)))))))

;;;; Engine — node->sel + object-at (hermetic fixtures, skip-unless grammar)

(defun kao-treesit-tests--js-p ()
  "Non-nil when the javascript grammar is available."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p 'javascript)))

(ert-deftest kao-treesit-object-at-not-ready-nil ()
  ;; No parser -> not ready -> nil, never an error (grammar-free).
  (with-temp-buffer
    (insert "x")
    (should-not (kao-treesit--object-at (point) "function" 'around 0))))

(ert-deftest kao-treesit-node->sel-formula ()
  (skip-unless (kao-treesit-tests--bash-p))
  (with-temp-buffer
    (insert "echo hi\n")
    (treesit-parser-create 'bash)
    (let ((node (treesit-node-at 1)))
      ;; forward form is surround's whole-element formula
      (should (equal (kao-treesit--node->sel node)
                     (kao-sel-make :anchor (treesit-node-start node)
                                   :cursor (1- (treesit-node-end node)))))
      ;; to-begin is the reversed form
      (should (equal (kao-treesit--node->sel node t)
                     (kao-sel-make :anchor (1- (treesit-node-end node))
                                   :cursor (treesit-node-start node)))))))

(ert-deftest kao-treesit-node->sel-matches-surround-html ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'html)))
  (require 'kao-surround)
  (with-temp-buffer
    (insert "<div><p>x</p></div>\n")
    (treesit-parser-create 'html)
    (let* ((pos (1+ (string-match "x" (buffer-string))))
           (node (kao-surround--treesit-element-node pos)))
      (should node)
      (should (equal (kao-treesit--node->sel node)
                     (kao-surround--treesit-element
                      (kao-sel-make :anchor pos :cursor pos) nil nil nil))))))

(ert-deftest kao-treesit-object-at-function-bash ()
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(function_definition body: (_) @function.inside) @function.around
(comment) @comment.inside"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (with-temp-buffer
      (insert "f() { echo hi; }\n")
      (treesit-parser-create 'bash)
      (goto-char 8) ; inside the body
      (let ((whole (kao-treesit--object-at (point) "function" 'around 0))
            (inner (kao-treesit--object-at (point) "function" 'inside 0)))
        (should whole)
        (should (equal (buffer-substring (kao-sel-min whole) (1+ (kao-sel-max whole)))
                       "f() { echo hi; }"))
        (should inner)
        (should (equal (buffer-substring (kao-sel-min inner) (1+ (kao-sel-max inner)))
                       "{ echo hi; }"))))))

(ert-deftest kao-treesit-object-at-inside-from-header-bash ()
  ;; treesit-5: cursor on the `f()' header (no inside covers it) still selects
  ;; the body via the largest-contained-inside fallback (was nil -> dropped).
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(function_definition body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (with-temp-buffer
      (insert "f() { echo hi; }\n")
      (treesit-parser-create 'bash)
      (goto-char 1) ; on the `f' of the header, before the body
      (let ((inner (kao-treesit--object-at (point) "function" 'inside 0)))
        (should inner)
        (should (equal (buffer-substring (kao-sel-min inner) (1+ (kao-sel-max inner)))
                       "{ echo hi; }"))))))

(ert-deftest kao-treesit-object-at-inside-from-header-nested-bash ()
  ;; treesit-5: cursor on the OUTER header falls back to the LARGEST inside (the
  ;; outer body), not a nested function's body.
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(function_definition body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (with-temp-buffer
      (insert "outer() { inner() { echo hi; }; }\n")
      (treesit-parser-create 'bash)
      (goto-char 1) ; on `o' of the outer header
      (let ((inner (kao-treesit--object-at (point) "function" 'inside 0)))
        (should inner)
        (should (equal (buffer-substring (kao-sel-min inner) (1+ (kao-sel-max inner)))
                       "{ inner() { echo hi; }; }"))))))

(ert-deftest kao-treesit-object-at-inside-u-from-header-bash ()
  ;; treesit-5, `u' inside: cursor on the command name (header) falls back to the
  ;; contained argument inside instead of dropping the selection.
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(command argument: (_) @parameter.inside) @parameter.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (with-temp-buffer
      (insert "echo hi\n")
      (treesit-parser-create 'bash)
      (goto-char 1) ; on `e' of `echo' (the command name / header)
      (let ((inner (kao-treesit--object-at (point) "parameter" 'inside 0)))
        (should inner)
        (should (equal (buffer-substring (kao-sel-min inner) (1+ (kao-sel-max inner)))
                       "hi"))))))

(ert-deftest kao-treesit-object-at-cursor-on-last-char ()
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(function_definition body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (with-temp-buffer
      (insert "f() { echo hi; }\n")
      (treesit-parser-create 'bash)
      (goto-char 16) ; the closing '}'
      (let ((whole (kao-treesit--object-at (point) "function" 'around 0)))
        (should whole)
        (should (equal (buffer-substring (kao-sel-min whole) (1+ (kao-sel-max whole)))
                       "f() { echo hi; }"))))))

(ert-deftest kao-treesit-object-at-no-match-nil ()
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(function_definition body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (with-temp-buffer
      (insert "f() { echo hi; }\n")
      (treesit-parser-create 'bash)
      (goto-char 8)
      (should-not (kao-treesit--object-at (point) "class" 'around 0)))))

(ert-deftest kao-treesit-object-at-parameter-union-comma-js ()
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(formal_parameters ((_) @parameter.inside . \",\"? @parameter.around) @parameter.around)"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (with-temp-buffer
      (insert "function f(aa, bb) {}\n")
      (treesit-parser-create 'javascript)
      ;; first param has a trailing comma: around unions it, inside does not
      (goto-char 12) ; 'a' of aa
      (let ((around (kao-treesit--object-at (point) "parameter" 'around 0))
            (inner (kao-treesit--object-at (point) "parameter" 'inside 0)))
        (should (equal (buffer-substring (kao-sel-min around) (1+ (kao-sel-max around)))
                       "aa,"))
        (should (equal (buffer-substring (kao-sel-min inner) (1+ (kao-sel-max inner)))
                       "aa")))
      ;; last param has no trailing comma: treesit-2 absorbs the LEADING
      ;; separator (Kakoune `select_argument' non-first last-arg branch), so
      ;; around = ", bb" — matching the regex `u' fallback, deviating from Helix.
      (goto-char 16) ; 'b' of bb
      (let ((around (kao-treesit--object-at (point) "parameter" 'around 0)))
        (should (equal (buffer-substring (kao-sel-min around) (1+ (kao-sel-max around)))
                       ", bb"))))))

(ert-deftest kao-treesit-object-at-level-nesting-js ()
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (with-temp-buffer
      (insert "function outer() { function inner() { return 1; } }\n")
      (treesit-parser-create 'javascript)
      (goto-char (1+ (string-match "return" (buffer-string)))) ; inside inner body
      (let ((lvl0 (kao-treesit--object-at (point) "function" 'around 0))
            (lvl1 (kao-treesit--object-at (point) "function" 'around 1)))
        (should (string-prefix-p "function inner"
                 (buffer-substring (kao-sel-min lvl0) (1+ (kao-sel-max lvl0)))))
        (should (string-prefix-p "function outer"
                 (buffer-substring (kao-sel-min lvl1) (1+ (kao-sel-max lvl1)))))))))

;;;; Phase 1 — object-register factories + setup

(ert-deftest kao-treesit-selector-factory-function-bash ()
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(function_definition body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root))
         (sel-fn (kao-treesit--make-selector "function")))
    (kao-treesit-reload-queries)
    (with-temp-buffer
      (insert "f() { echo hi; }\n")
      (treesit-parser-create 'bash)
      (let ((sel (kao-sel-make :anchor 8 :cursor 8)))
        ;; whole (<a-a>f): to-begin AND to-end
        (let ((w (funcall sel-fn sel nil t t 0)))
          (should (equal (buffer-substring (kao-sel-min w) (1+ (kao-sel-max w)))
                         "f() { echo hi; }")))
        ;; inner (<a-i>f)
        (let ((i (funcall sel-fn sel t t t 0)))
          (should (equal (buffer-substring (kao-sel-min i) (1+ (kao-sel-max i)))
                         "{ echo hi; }")))
        ;; to-begin only ([f): backward sel from cursor to object begin (pos 1)
        (should (equal (funcall sel-fn sel nil t nil 0)
                       (kao-sel-make :anchor 8 :cursor 1)))
        ;; to-end only (]f): from cursor to object end (last char, pos 16)
        (should (equal (funcall sel-fn sel nil nil t 0)
                       (kao-sel-make :anchor 8 :cursor 16)))
        ;; no such object -> nil
        (should-not (funcall (kao-treesit--make-selector "class") sel nil t t 0))))))

(ert-deftest kao-treesit-selector-factory-not-ready-nil ()
  ;; No parser -> nil, never an error (grammar-free).
  (with-temp-buffer
    (insert "x")
    (should-not (funcall (kao-treesit--make-selector "function")
                         (kao-sel-make :anchor 1 :cursor 1) nil t t 0))))

(ert-deftest kao-treesit-nested-factory-functions-bash ()
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(function_definition body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root))
         (nst (kao-treesit--make-nested-selector "function")))
    (kao-treesit-reload-queries)
    (with-temp-buffer
      (insert "f() { echo a; }\ng() { echo b; }\n")
      (treesit-parser-create 'bash)
      (let ((spans (funcall nst (point-min) (point-max) nil)))
        (should (= 2 (length spans)))
        ;; ascending; first span is the first function
        (should (equal (buffer-substring (caar spans) (1+ (cdar spans)))
                       "f() { echo a; }"))))))

(ert-deftest kao-treesit-setup-registers-f ()
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(function_definition body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root))
         (kao--object-runtime-table nil)
         (kao--object-runtime-info nil)
         (kao--object-runtime-nested nil)
         (kao-user-map (copy-keymap kao-user-map))
         (kao-normal-state-map (copy-keymap kao-normal-state-map)))
    (kao-treesit-reload-queries)
    (should-not (assq ?f kao--object-runtime-table))     ; nothing before setup
    (kao-treesit-setup)                                  ; no objects arg -> no-op
    (should-not (assq ?f kao--object-runtime-table))
    (kao-treesit-setup t)                                ; with objects
    (should (assq ?f kao--object-runtime-table))
    (should (equal "function" (cdr (assq ?f kao--object-runtime-info))))
    (should (assq ?f kao--object-runtime-nested))
    (kao-treesit-setup t)                                ; idempotent
    (should (= 1 (cl-count-if (lambda (e) (eq (car e) ?f))
                              kao--object-runtime-table)))
    (with-temp-buffer
      (insert "f() { echo hi; }\n")
      (treesit-parser-create 'bash)
      (let ((b (kao-object-bounds ?f (kao-sel-make :anchor 8 :cursor 8))))
        (should (equal (buffer-substring (kao-sel-min b) (1+ (kao-sel-max b)))
                       "f() { echo hi; }"))))))

(ert-deftest kao-treesit-setup-augments-u-parameter-js ()
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(formal_parameters ((_) @parameter.inside . \",\"? @parameter.around) @parameter.around)"))))
         (kao-treesit-queries-dir (list root))
         (kao--object-runtime-table nil)
         (kao--object-runtime-info nil)
         (kao--object-runtime-nested nil)
         (kao-user-map (copy-keymap kao-user-map))
         (kao-normal-state-map (copy-keymap kao-normal-state-map)))
    (kao-treesit-reload-queries)
    (kao-treesit-setup t)
    (with-temp-buffer
      (insert "function f(aa, bb) {}\n")
      (treesit-parser-create 'javascript)
      ;; treesit parameter: whole includes the trailing comma
      (let ((a (kao-object-bounds ?u (kao-sel-make :anchor 12 :cursor 12))))
        (should (equal (buffer-substring (kao-sel-min a) (1+ (kao-sel-max a)))
                       "aa,")))
      ;; inner parameter excludes it
      (let ((i (kao-object-bounds ?u (kao-sel-make :anchor 12 :cursor 12) t)))
        (should (equal (buffer-substring (kao-sel-min i) (1+ (kao-sel-max i)))
                       "aa"))))))

(ert-deftest kao-treesit-u-nested-parameter-js ()
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(formal_parameters ((_) @parameter.inside . \",\"? @parameter.around) @parameter.around)"))))
         (kao-treesit-queries-dir (list root))
         (kao--object-runtime-table nil)
         (kao--object-runtime-info nil)
         (kao--object-runtime-nested nil)
         (kao-user-map (copy-keymap kao-user-map))
         (kao-normal-state-map (copy-keymap kao-normal-state-map)))
    (kao-treesit-reload-queries)
    (kao-treesit-setup t)
    (with-temp-buffer
      (insert "function f(aa, bb) {}\n")
      (treesit-parser-create 'javascript)
      (let ((nst (kao--object-nested-selector ?u)))
        (should nst)
        ;; whole: one span per param, first unions its comma (not 3 comma-spans)
        (let ((whole (funcall nst (point-min) (point-max) nil 0)))
          (should (= 2 (length whole)))
          (should (equal (buffer-substring (caar whole) (1+ (cdar whole))) "aa,")))
        ;; inner: one span per param, no comma
        (let ((inner (funcall nst (point-min) (point-max) t 0)))
          (should (= 2 (length inner)))
          (should (equal (buffer-substring (caar inner) (1+ (cdar inner))) "aa")))))))

(ert-deftest kao-treesit-u-falls-back-to-regex ()
  ;; No parser -> the augmented `u' delegates to the exact regex argument object.
  (let ((kao--object-runtime-table nil)
        (kao--object-runtime-info nil)
        (kao--object-runtime-nested nil)
        (kao-user-map (copy-keymap kao-user-map))
        (kao-normal-state-map (copy-keymap kao-normal-state-map)))
    (kao-treesit-setup t)
    (with-temp-buffer
      (insert "foo(aa, bb)\n")
      (let ((ts (kao-object-bounds ?u (kao-sel-make :anchor 5 :cursor 5)))
            (rx (kao-object--argument (kao-sel-make :anchor 5 :cursor 5) nil t t 0)))
        (should (equal ts rx))))))

(ert-deftest kao-treesit-bare-u-is-builtin ()
  ;; Without setup, `u' is the built-in regex argument object — unchanged.
  (let ((kao--object-runtime-table nil))
    (should (eq #'kao-object--argument (kao--object-selector ?u)))))

;;;; Object-pending dispatch memo (one query per container)

(defun kao-treesit-tests--sel-strings ()
  "The current selections as buffer substrings, sorted (order-agnostic)."
  (sort (mapcar (lambda (s) (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s))))
                (kao-sels-list kao--sels))
        #'string<))

(ert-deftest kao-treesit-dispatch-memo-one-query-per-container-js ()
  ;; treesit-setup registers `kao-treesit--object-dispatch-memo' on the object
  ;; dispatch-context seam, so an object-pending `<a-i>u' pass over two cursors
  ;; that share one container (`function f(aa, bb)') queries that container ONCE,
  ;; and the selections are identical to the memo-off path.
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(formal_parameters ((_) @parameter.inside . \",\"? @parameter.around) @parameter.around)"))))
         (kao-treesit-queries-dir (list root))
         (kao--object-runtime-table nil)
         (kao--object-runtime-info nil)
         (kao--object-runtime-nested nil)
         (kao--object-dispatch-context-functions nil) ; contain the add-hook
         (kao-user-map (copy-keymap kao-user-map))
         (kao-normal-state-map (copy-keymap kao-normal-state-map)))
    (kao-treesit-reload-queries)
    (kao-treesit-setup t)
    (should (memq #'kao-treesit--object-dispatch-memo
                  kao--object-dispatch-context-functions))
    (with-temp-buffer
      (insert "function f(aa, bb) {}\n")           ; aa 12-13, bb 16-17
      (treesit-parser-create 'javascript)
      (kao-mode 1)
      (unwind-protect
          (let ((orig (symbol-function 'kao-treesit--matches-compute)))
            ;; memo ON (seam registered): shared container queried ONCE
            (let ((calls 0))
              (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?u))
                        ((symbol-function 'kao-treesit--matches-compute)
                         (lambda (&rest a) (cl-incf calls) (apply orig a))))
                (setq kao--sels (kao-sels-make
                                 :list (list (kao-sel-make :anchor 12 :cursor 12)
                                             (kao-sel-make :anchor 16 :cursor 16))
                                 :main 1))
                (kao-select-inner)
                (should (= 1 calls))
                (should (equal '("aa" "bb") (kao-treesit-tests--sel-strings)))))
            ;; memo OFF (hook cleared): identical result, one query PER cursor
            (let ((calls 0)
                  (kao--object-dispatch-context-functions nil))
              (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?u))
                        ((symbol-function 'kao-treesit--matches-compute)
                         (lambda (&rest a) (cl-incf calls) (apply orig a))))
                (setq kao--sels (kao-sels-make
                                 :list (list (kao-sel-make :anchor 12 :cursor 12)
                                             (kao-sel-make :anchor 16 :cursor 16))
                                 :main 1))
                (kao-select-inner)
                (should (= 2 calls))
                (should (equal '("aa" "bb") (kao-treesit-tests--sel-strings))))))
        (kao-mode -1)))))

;;;; Phase 2 — tree motions + `<space> t' menu

(defmacro kao-treesit-tests--with-cmd (lang content &rest body)
  "Run BODY in a `kao-mode' temp buffer with a LANG parser over CONTENT."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,content)
     (treesit-parser-create ,lang)
     (kao-mode 1)
     (unwind-protect (progn ,@body)
       (kao-mode -1))))

(ert-deftest kao-treesit-cmd-select-function-bash ()
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(function_definition body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'bash "f() { echo hi; }\n"
      (kao-set-selections (list (kao-sel-make :anchor 8 :cursor 8)))
      (kao-treesit-select-function)
      (let ((s (car (kao-get-selections))))
        (should (equal (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s)))
                       "f() { echo hi; }"))))))

(ert-deftest kao-treesit-cmd-not-ready-noop ()
  ;; No parser -> friendly message, selections untouched, never an error.
  (with-temp-buffer
    (insert "x")
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
          (kao-treesit-select-function)
          (kao-treesit-parent)
          (kao-treesit-expand)
          (should (equal (kao-get-selections)
                         (list (kao-sel-make :anchor 1 :cursor 1)))))
      (kao-mode -1))))

(ert-deftest kao-treesit-cmd-motions-js ()
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "function a(){}\nfunction b(){}\n"
      (cl-flet ((txt () (let ((s (car (kao-get-selections))))
                          (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s))))))
        (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
        (kao-treesit-select-node)
        (should (string-prefix-p "function a" (txt)))
        (kao-treesit-next-sibling)
        (should (string-prefix-p "function b" (txt)))
        (kao-treesit-prev-sibling)
        (should (string-prefix-p "function a" (txt)))
        ;; parent = program (contains both functions)
        (kao-treesit-parent)
        (should (string-match-p "function b" (txt)))
        ;; first named child of program = function a
        (kao-treesit-first-child)
        (should (string-prefix-p "function a" (txt)))))))

(ert-deftest kao-treesit-cmd-goto-function-js ()
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "function a(){}\nfunction b(){}\n"
      (cl-flet ((txt () (let ((s (car (kao-get-selections))))
                          (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s))))))
        (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
        (kao-treesit-goto-next-function)
        (should (string-prefix-p "function b" (txt)))
        (kao-treesit-goto-prev-function)
        (should (string-prefix-p "function a" (txt)))))))

;;;; treesit-6 — public kao-treesit-select-object + kao-treesit-goto

(ert-deftest kao-treesit-select-object-class-inside-js ()
  ;; treesit-6: the public select-object reaches a non-`f' base + `inside' part.
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(class_declaration body: (_) @class.inside) @class.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "class C { m() {} }\n"
      (kao-set-selections (list (kao-sel-make :anchor 12 :cursor 12))) ; inside body
      (kao-treesit-select-object "class" 'inside)
      (let ((s (car (kao-get-selections))))
        (should (equal (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s)))
                       "{ m() {} }"))))))

(ert-deftest kao-treesit-select-object-part-defaults-around ()
  ;; treesit-6: PART is optional and defaults to `around'.
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(function_definition body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'bash "f() { echo hi; }\n"
      (kao-set-selections (list (kao-sel-make :anchor 8 :cursor 8)))
      (kao-treesit-select-object "function") ; no PART -> around
      (let ((s (car (kao-get-selections))))
        (should (equal (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s)))
                       "f() { echo hi; }"))))))

(ert-deftest kao-treesit-goto-generalized-js ()
  ;; treesit-6: the public goto lands on the next/prev BASE object, END orients
  ;; the cursor to the object start (nil) or end (non-nil), EXTEND keeps anchor.
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "function a(){}\nfunction b(){}\n"
      (cl-flet ((txt () (let ((s (car (kao-get-selections))))
                          (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s))))))
        ;; next, START orientation (default): span = function b, cursor at min
        (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
        (kao-treesit-goto "function" t)
        (should (string-prefix-p "function b" (txt)))
        (let ((s (car (kao-get-selections))))
          (should (= (kao-sel-cursor s) (kao-sel-min s))))
        ;; prev lands back on function a
        (kao-treesit-goto "function" nil)
        (should (string-prefix-p "function a" (txt)))
        ;; next, END orientation: cursor at max
        (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
        (kao-treesit-goto "function" t t)
        (let ((s (car (kao-get-selections))))
          (should (string-prefix-p "function b"
                   (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s)))))
          (should (= (kao-sel-cursor s) (kao-sel-max s))))
        ;; EXTEND from function a keeps the anchor and reaches function b's start
        (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
        (kao-treesit-goto "function" t nil t)
        (let ((s (car (kao-get-selections)))
              (fb (1+ (string-match "function b" (buffer-string)))))
          (should (= (kao-sel-min s) 1))           ; anchor preserved
          (should (= (kao-sel-max s) fb)))))))     ; cursor extended to func b start

(ert-deftest kao-treesit-goto-and-select-nondefault-base-bash ()
  ;; treesit-6: a non-default base (`comment') is reachable through BOTH the
  ;; public select-object and the public goto.
  (skip-unless (kao-treesit-tests--bash-p))
  ;; The `# one'/`# two' around span assumes per-match GROUPED resolution; on the
  ;; flat path (Emacs < 31, no 6th treesit-query-capture arg) adjacent same-name
  ;; captures union (spec 05, D-T risk 0.3), so guard this to the grouped path.
  (skip-unless (kao-treesit--grouped-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(comment) @comment.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'bash "# one\n# two\necho hi\n"
      ;; select-object reaches the comment under the cursor
      (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 2))) ; in "# one"
      (kao-treesit-select-object "comment" 'around)
      (let ((s (car (kao-get-selections))))
        (should (equal (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s)))
                       "# one")))
      ;; goto reaches the next comment
      (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 2)))
      (kao-treesit-goto "comment" t)
      (let ((s (car (kao-get-selections))))
        (should (equal (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s)))
                       "# two"))))))

(ert-deftest kao-treesit-cmd-expand-shrink-roundtrip-js ()
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript
        "function outer(){ function inner(){ return 1; } }\n"
      (goto-char (1+ (string-match "return" (buffer-string))))
      (kao-set-selections (list (kao-sel-make :anchor (point) :cursor (point))))
      (let ((orig (kao-get-selections)))
        (kao-treesit-expand)
        (kao-treesit-expand)
        (should-not (equal orig (kao-get-selections)))
        (kao-treesit-shrink)
        (kao-treesit-shrink)
        (should (equal orig (kao-get-selections)))))))

(ert-deftest kao-treesit-cmd-expand-stack-invalidation-js ()
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript
        "function outer(){ function inner(){ return 1; } }\n"
      (goto-char (1+ (string-match "return" (buffer-string))))
      (kao-set-selections (list (kao-sel-make :anchor (point) :cursor (point))))
      (kao-treesit-expand)
      (should-not (kao-treesit--expand-stale-p))     ; fresh right after expand
      ;; external selection change invalidates the stack
      (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
      (should (kao-treesit--expand-stale-p))
      ;; and a buffer edit invalidates via the modified tick
      (kao-treesit-expand)
      (should-not (kao-treesit--expand-stale-p))
      (goto-char (point-max)) (insert "x")
      (should (kao-treesit--expand-stale-p)))))

(ert-deftest kao-treesit-cmd-sibling-edge-and-extend-js ()
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "function a(){}\nfunction b(){}\n"
      (cl-flet ((txt () (let ((s (car (kao-get-selections))))
                          (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s))))))
        (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
        (kao-treesit-select-node)               ; function a (first sibling)
        (let ((before (kao-get-selections)))
          (kao-treesit-prev-sibling)            ; no-op at the edge
          (should (equal before (kao-get-selections))))
        (kao-treesit-extend-next-sibling)       ; extend a -> spans a..b
        (should (string-match-p "function a" (txt)))
        (should (string-match-p "function b" (txt)))))))

(ert-deftest kao-treesit-cmd-expand-multi-and-root-js ()
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "function a(){}\nfunction b(){}\n"
      ;; per-selection across two cursors (one in each function)
      (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 2)
                                (kao-sel-make :anchor 17 :cursor 17)))
      (kao-treesit-expand)
      (let ((sels (kao-get-selections)))
        (should (= 2 (length sels)))
        (should (string-prefix-p "function a"
                 (buffer-substring (kao-sel-min (nth 0 sels))
                                   (1+ (kao-sel-max (nth 0 sels))))))
        (should (string-prefix-p "function b"
                 (buffer-substring (kao-sel-min (nth 1 sels))
                                   (1+ (kao-sel-max (nth 1 sels)))))))
      ;; no-op at the root
      (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 2)))
      (kao-treesit-expand)                       ; function a
      (kao-treesit-expand)                       ; program (root)
      (let ((at-root (kao-get-selections)))
        (kao-treesit-expand)                     ; no parent -> unchanged
        (should (equal at-root (kao-get-selections)))))))

;;;; Phase 3 — multi-select power

(ert-deftest kao-treesit-cmd-select-all-functions-js ()
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "function a(){}\nfunction b(){}\n"
      (cl-flet ((txt (s) (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s)))))
        (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
        (kao-treesit-select-all-functions)
        (let ((sels (kao-get-selections)))
          (should (= 2 (length sels)))
          (should (string-prefix-p "function a" (txt (nth 0 sels))))
          (should (string-prefix-p "function b" (txt (nth 1 sels))))
          (should (= 1 (kao-sels-main kao--sels))))))))     ; main = last

(ert-deftest kao-treesit-cmd-select-all-siblings-js ()
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "function a(){}\nfunction b(){}\n"
      (goto-char 2)                              ; inside function a
      (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 2)))
      (kao-treesit-select-all-siblings)          ; program's children = a, b
      (let ((sels (kao-get-selections)))
        (should (= 2 (length sels)))
        (should (= 1 (kao-sels-main kao--sels)))  ; main = last
        (should (string-prefix-p "function a"
                 (buffer-substring (kao-sel-min (nth 0 sels))
                                   (1+ (kao-sel-max (nth 0 sels))))))))))

(ert-deftest kao-treesit-cmd-filter-to-functions-js ()
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "function a(){}\nlet z = 1;\n"
      ;; cursor 2 is inside function a; cursor 17 is in the let (not a function)
      (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 2)
                                (kao-sel-make :anchor 17 :cursor 17)))
      (kao-treesit-filter-to-functions)
      (let ((sels (kao-get-selections)))
        (should (= 1 (length sels)))
        (should (<= (kao-sel-min (car sels)) 2))
        (should (< 2 14)))                       ; the kept cursor is inside a
      ;; all-dropped -> message + no-op (a multi-char span so a wrong
      ;; collapse-to-point would be visible, not equal to the original)
      (kao-set-selections (list (kao-sel-make :anchor 17 :cursor 20)))
      (let ((before (kao-get-selections)))
        (kao-treesit-filter-to-functions)
        (should (equal before (kao-get-selections)))))))

(ert-deftest kao-treesit-scope-rows-js ()
  (skip-unless (kao-treesit-tests--js-p))
  (kao-treesit-tests--with-cmd 'javascript "function a(){ return 1; }\n"
    (let* ((pos (1+ (string-match "return" (buffer-string))))
           (rows (kao-treesit--scope-rows pos))
           (types (mapconcat #'cdr rows " ")))
      (should rows)
      (should (string-match-p "function_declaration" types))
      (should (string-match-p "program" types)))))

(ert-deftest kao-treesit-explore-graceful-without-parser ()
  ;; M-x-able; no parser -> message, never an error (does not open the mode).
  (with-temp-buffer
    (insert "x")
    (should (commandp #'kao-treesit-explore))
    (kao-treesit-explore)))

(ert-deftest kao-treesit-setup-binds-menu ()
  ;; Keymaps isolated via copy so the global maps are not polluted.
  (let ((kao-user-map (copy-keymap kao-user-map))
        (kao-normal-state-map (copy-keymap kao-normal-state-map)))
    (should-not (lookup-key kao-user-map "t"))         ; free before setup
    (kao-treesit-setup)                                 ; menu bound even w/o objects
    (should (eq kao-treesit-tree-map (lookup-key kao-user-map "t")))
    (should (eq #'kao-treesit-select-function
                (lookup-key kao-treesit-tree-map "f")))
    (should (eq #'kao-treesit-select-function-inside    ; capital read shifted
                (lookup-key kao-treesit-tree-map "F")))
    (should (eq #'kao-treesit-select-all-functions
                (lookup-key kao-treesit-tree-map "*")))
    (should (eq #'kao-treesit-filter-to-functions
                (lookup-key kao-treesit-tree-map "/")))
    (should (eq #'kao-treesit-scopes (lookup-key kao-treesit-tree-map "?")))
    (should (eq #'kao-treesit-explore (lookup-key kao-treesit-tree-map "t")))
    (should (eq #'kao-treesit-expand
                (lookup-key kao-normal-state-map (kbd "M-<return>"))))
    (should (eq #'kao-treesit-shrink
                (lookup-key kao-normal-state-map (kbd "M-S-<return>"))))))

;;;; Task 3 -- command-scoped captures memo (part B)

(ert-deftest kao-treesit-captures-memo-shares-container-query-js ()
  "Two cursors sharing a container run its capture query once, not per cursor.
The command-scoped captures memo makes the second cursor a memo hit, so
`treesit-query-capture' fires once per container per command."
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root))
         (calls 0))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "function a(){ let x=1; let y=2; }\n"
      (let ((p1 (1+ (string-match "x=1" (buffer-string))))
            (p2 (1+ (string-match "y=2" (buffer-string))))
            (orig (symbol-function 'treesit-query-capture)))
        (cl-letf (((symbol-function 'treesit-query-capture)
                   (lambda (&rest args) (setq calls (1+ calls)) (apply orig args))))
          (kao-set-selections (list (kao-sel-make :anchor p1 :cursor p1)
                                    (kao-sel-make :anchor p2 :cursor p2)))
          (setq calls 0)
          (kao-treesit-select-function)     ; SPC t f -- memo-bound path
          ;; one pattern, one shared container -> exactly one query for the batch
          (should (= calls 1)))))))

(ert-deftest kao-treesit-captures-memo-filter-shares-container-query-js ()
  "filter-to-functions with two cursors in one container queries it once."
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root))
         (calls 0))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "function a(){ let x=1; let y=2; }\n"
      (let ((p1 (1+ (string-match "x=1" (buffer-string))))
            (p2 (1+ (string-match "y=2" (buffer-string))))
            (orig (symbol-function 'treesit-query-capture)))
        (cl-letf (((symbol-function 'treesit-query-capture)
                   (lambda (&rest args) (setq calls (1+ calls)) (apply orig args))))
          (kao-set-selections (list (kao-sel-make :anchor p1 :cursor p1)
                                    (kao-sel-make :anchor p2 :cursor p2)))
          (setq calls 0)
          (kao-treesit-filter-to-functions)     ; SPC t / -- memo-bound path
          (should (= calls 1))
          (should (= 2 (length (kao-get-selections)))))))))

(ert-deftest kao-treesit-captures-memo-equiv-select-js ()
  "Multi-cursor select-function equals single-cursor-looped results (memo-neutral).
Two cursors in two different functions: a wrong memo key would give the second
cursor the first container's captures and mis-select or drop it."
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "function a(){ let x=1; }\nfunction b(){ let y=2; }\n"
      (let ((p1 (1+ (string-match "x=1" (buffer-string))))
            (p2 (1+ (string-match "y=2" (buffer-string)))))
        (kao-set-selections (list (kao-sel-make :anchor p1 :cursor p1)
                                  (kao-sel-make :anchor p2 :cursor p2)))
        (kao-treesit-select-function)
        (let ((multi (kao-get-selections)))
          (kao-set-selections (list (kao-sel-make :anchor p1 :cursor p1)))
          (kao-treesit-select-function)
          (let ((s1 (car (kao-get-selections))))
            (kao-set-selections (list (kao-sel-make :anchor p2 :cursor p2)))
            (kao-treesit-select-function)
            (let ((s2 (car (kao-get-selections))))
              (should (equal multi (list s1 s2))))))))))

(ert-deftest kao-treesit-captures-memo-equiv-filter-js ()
  "Multi-cursor filter-to-functions keeps exactly the individually-in-function cursors."
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "function a(){ let x=1; }\nfunction b(){ let y=2; }\n"
      (let ((p1 (1+ (string-match "x=1" (buffer-string))))
            (p2 (1+ (string-match "y=2" (buffer-string)))))
        ;; both cursors are inside a function -> both kept, distinct containers
        (kao-set-selections (list (kao-sel-make :anchor p1 :cursor p1)
                                  (kao-sel-make :anchor p2 :cursor p2)))
        (kao-treesit-filter-to-functions)
        (should (equal (kao-get-selections)
                       (list (kao-sel-make :anchor p1 :cursor p1)
                             (kao-sel-make :anchor p2 :cursor p2))))))))

(ert-deftest kao-treesit-captures-memo-equiv-goto-js ()
  "Multi-cursor goto-next-function equals single-cursor-looped results (Task 1 box memo)."
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "function a(){}\nfunction b(){}\nfunction c(){}\n"
      (let ((p1 2) (p2 17))            ; p1 inside a, p2 inside b
        (kao-set-selections (list (kao-sel-make :anchor p1 :cursor p1)
                                  (kao-sel-make :anchor p2 :cursor p2)))
        (kao-treesit-goto-next-function)
        (let ((multi (kao-get-selections)))
          (kao-set-selections (list (kao-sel-make :anchor p1 :cursor p1)))
          (kao-treesit-goto-next-function)
          (let ((s1 (car (kao-get-selections))))
            (kao-set-selections (list (kao-sel-make :anchor p2 :cursor p2)))
            (kao-treesit-goto-next-function)
            (let ((s2 (car (kao-get-selections))))
              (should (equal multi (list s1 s2))))))))))

(ert-deftest kao-treesit-captures-memo-off-path-js ()
  "With the memo nil (default), object-at resolves correctly (nil path works)."
  (skip-unless (kao-treesit-tests--js-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("javascript" . "(function_declaration body: (_) @function.inside) @function.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (kao-treesit-tests--with-cmd 'javascript "function a(){ let x=1; }\n"
      (let ((kao-treesit--captures-memo nil)
            (p (1+ (string-match "x=1" (buffer-string)))))
        (let ((obj (kao-treesit--object-at p "function" 'around 0)))
          (should obj)
          (should (string-prefix-p "function a"
                                   (buffer-substring (kao-sel-min obj)
                                                     (1+ (kao-sel-max obj))))))))))

;;;; Block 1 regression pins (audit fixes)

;;; treesit-4 — empty-buffer clamp: no `args-out-of-range'

(ert-deftest kao-treesit-empty-buffer-motions-no-crash ()
  "Tree motions in an empty parsed buffer are a friendly no-op, never
`args-out-of-range' (treesit-4)."
  (skip-unless (kao-treesit-tests--bash-p))
  (with-temp-buffer
    (treesit-parser-create 'bash)          ; live parser over an EMPTY buffer
    (kao-mode 1)
    (unwind-protect
        (let ((orig (kao-get-selections)))
          (should (= (point-min) (point-max)))
          ;; none of these signal; the (empty) selection is left unchanged
          (kao-treesit-expand)
          (kao-treesit-select-node)
          (kao-treesit-select-all-siblings)
          (kao-treesit-parent)
          (kao-treesit-first-child)
          (should (equal orig (kao-get-selections))))
      (kao-mode -1))))

(ert-deftest kao-treesit-node-of-sel-empty-buffer-nil ()
  "`kao-treesit--node-of-sel' returns nil in an empty buffer (no signal)."
  (skip-unless (kao-treesit-tests--bash-p))
  (with-temp-buffer
    (treesit-parser-create 'bash)
    (should-not (kao-treesit--node-of-sel (kao-sel-make :anchor 1 :cursor 1)))))

;;; treesit-0 — strictly-larger ancestor climb: expand never stalls

(defun kao-treesit-tests--yaml-p ()
  "Non-nil when the yaml grammar is available."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p 'yaml)))

(ert-deftest kao-treesit-expand-climbs-past-same-span-js ()
  "Expand from a no-semicolon call climbs strictly each press, escaping the
same-span `expression_statement' wrapper and reaching the function/root — the
old code stalled at the `g(1)' span forever (treesit-0)."
  (skip-unless (kao-treesit-tests--js-p))
  (kao-treesit-tests--with-cmd 'javascript "function f() {\n  g(1)\n}\n"
    (goto-char (1+ (string-match "g(1)" (buffer-string))))
    (kao-set-selections (list (kao-sel-make :anchor (point) :cursor (point))))
    (let (spans)
      (dotimes (_ 6)
        (kao-treesit-expand)
        (let ((s (car (kao-get-selections))))
          (push (cons (kao-sel-min s) (kao-sel-max s)) spans)))
      (setq spans (nreverse spans))
      ;; monotonic nesting: each span contains the previous
      (cl-loop for (a b) on spans while b do
               (should (and (<= (car b) (car a)) (>= (cdr b) (cdr a)))))
      ;; strictly grows until the root: >=4 DISTINCT spans (g -> g(1) -> block ->
      ;; function -> program); the old same-span stall reached only one span
      (should (>= (length (delete-dups (copy-sequence spans))) 4))
      ;; the climb reaches the whole function within the six presses
      (let ((top (car (last spans))))
        (should (string-prefix-p "function f"
                                 (buffer-substring (car top) (1+ (cdr top)))))))))

(ert-deftest kao-treesit-expand-climbs-past-same-span-yaml ()
  "Expand up a YAML same-span block chain reaches the document root, growing
each press and never repeating a span mid-climb (treesit-0)."
  (skip-unless (kao-treesit-tests--yaml-p))
  (kao-treesit-tests--with-cmd 'yaml "top:\n  a: 1\n  b: 2\n"
    (goto-char 8)                          ; on `a', deep in the same-span chain
    (kao-set-selections (list (kao-sel-make :anchor 8 :cursor 8)))
    (let (spans)
      (dotimes (_ 6)
        (kao-treesit-expand)
        (let ((s (car (kao-get-selections))))
          (push (cons (kao-sel-min s) (kao-sel-max s)) spans)))
      (setq spans (nreverse spans))
      (cl-loop for (a b) on spans while b do
               (should (and (<= (car b) (car a)) (>= (cdr b) (cdr a)))))
      (should (>= (length (delete-dups (copy-sequence spans))) 3))
      ;; climbs all the way to the document root (span reaches buffer start)
      (should (= 1 (car (car (last spans))))))))

(ert-deftest kao-treesit-shrink-descend-past-same-span-yaml ()
  "Shrink with an empty stack descends past a same-span single-child level and
makes visible progress — the mirror of the expand climb (treesit-0)."
  (skip-unless (kao-treesit-tests--yaml-p))
  (kao-treesit-tests--with-cmd 'yaml "top:\n  a: 1\n  b: 2\n"
    ;; select the whole document [1-20]; its cursor sits inside the `a: 1' pair
    (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 9)))
    (let ((before (car (kao-get-selections))))
      (kao-treesit-shrink)                 ; empty stack -> descend
      (let ((after (car (kao-get-selections))))
        ;; strictly smaller span (descended past the [1-20] block/mapping chain)
        (should (or (> (kao-sel-min after) (kao-sel-min before))
                    (< (kao-sel-max after) (kao-sel-max before))))
        (should-not (equal before after))))))

;;; treesit-3 — grouped per-match `comment.around' is the whole block

(ert-deftest kao-treesit-comment-around-is-block-not-line ()
  "`comment.around' selects the whole contiguous comment block while `inside'
stays one line — per-match GROUPED union, so around != inside (treesit-3)."
  (skip-unless (kao-treesit-tests--bash-p))
  ;; GROUPED-only behavior: the flat path (Emacs < 31) can't produce the per-match
  ;; block union, so this assertion only holds where the 6th arg is available.
  (skip-unless (kao-treesit--grouped-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(comment) @comment.inside\n(comment)+ @comment.around"))))
         (kao-treesit-queries-dir (list root)))
    (kao-treesit-reload-queries)
    (with-temp-buffer
      (insert "# line one\n# line two\n# line three\necho hi\n")
      (treesit-parser-create 'bash)
      (goto-char 13)                       ; inside the middle comment
      (let ((around (kao-treesit--object-at (point) "comment" 'around 0))
            (inner (kao-treesit--object-at (point) "comment" 'inside 0)))
        (should around)
        (should (equal (buffer-substring (kao-sel-min around) (1+ (kao-sel-max around)))
                       "# line one\n# line two\n# line three"))
        (should inner)
        (should (equal (buffer-substring (kao-sel-min inner) (1+ (kao-sel-max inner)))
                       "# line two"))
        ;; the treesit-3 regression: around used to collapse to the single line
        (should-not (equal around inner))))))

(ert-deftest kao-treesit-flat-engine-cross-version-coverage-comment-block ()
  "Cross-version coverage pin (NOT a legacy shim): binds `kao-treesit--grouped-cache'
nil to exercise the Emacs 29/30 FLAT around-engine (`kao-treesit--around-span')
even on a grouped-capable (31+) Emacs, so CI running on 31 still covers the
29/30 dispatch path.  Asserts the flat heuristic still merges a self-captured
comment block into one `around' object (treesit-3 fallback path)."
  (skip-unless (kao-treesit-tests--bash-p))
  (let* ((root (kao-treesit-tests--fixture
                '(("bash" . "(comment) @comment.inside\n(comment)+ @comment.around"))))
         (kao-treesit-queries-dir (list root))
         (kao-treesit--grouped-cache nil))   ; force the flat (29/30) engine here
    (kao-treesit-reload-queries)
    (with-temp-buffer
      (insert "# line one\n# line two\n# line three\necho hi\n")
      (treesit-parser-create 'bash)
      (goto-char 3)                          ; the first comment line
      (let ((around (kao-treesit--object-at (point) "comment" 'around 0)))
        (should around)
        (should (equal (buffer-substring (kao-sel-min around) (1+ (kao-sel-max around)))
                       "# line one\n# line two\n# line three"))))))

;;;; Mode-off guard — M-x commands in a non-kao buffer

(defun kao-treesit-tests--mode-off-user-errors-p (cmd)
  "Non-nil when CMD via `call-interactively' in a mode-off buffer signals a
`user-error' matching \"kao-mode is not active\".
Grammar-free: the `kao--assert-mode' guard fires before any tree-sitter work, so
no parser fixture is needed (kept grammar-free per commit 0314e9a).
`completing-read' is stubbed so an object-reading command's interactive form
cannot block batch on the minibuffer before the guard runs."
  (with-temp-buffer
    (fundamental-mode)                       ; kao-mode is OFF here
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "function")))
      (condition-case e
          (progn (call-interactively cmd) nil)
        (user-error (and (stringp (cadr e))
                         (string-match-p "kao-mode is not active" (cadr e))
                         t))))))

(ert-deftest kao-treesit-select-object-mode-off-user-errors ()
  "`kao-treesit-select-object' in a mode-off buffer signals the guard
`user-error' (before the completing-read prompt), not the cryptic
`(wrong-type-argument kao-sels nil)' the guard replaces ."
  (should (kao-treesit-tests--mode-off-user-errors-p 'kao-treesit-select-object)))

(ert-deftest kao-treesit-goto-family-mode-off-user-errors ()
  "The `kao-treesit-goto' motion family in a mode-off buffer signals the guard
`user-error' before any grammar work ."
  (should (kao-treesit-tests--mode-off-user-errors-p 'kao-treesit-goto-next-function))
  (should (kao-treesit-tests--mode-off-user-errors-p 'kao-treesit-goto-prev-function)))

(ert-deftest kao-treesit-selection-commands-mode-off-user-error ()
  "Every M-x-discoverable kao-treesit command that reads or writes `kao--sels'
signals the guard `user-error' with the mode off — the object-select family, the
tree motions, expand/shrink, and the multi-select commands ."
  (dolist (cmd '(kao-treesit-select-function kao-treesit-select-function-inside
                 kao-treesit-select-class kao-treesit-select-class-inside
                 kao-treesit-select-parameter kao-treesit-select-parameter-inside
                 kao-treesit-select-comment kao-treesit-select-test
                 kao-treesit-parent kao-treesit-first-child kao-treesit-select-node
                 kao-treesit-next-sibling kao-treesit-prev-sibling
                 kao-treesit-extend-next-sibling kao-treesit-extend-prev-sibling
                 kao-treesit-expand kao-treesit-shrink
                 kao-treesit-select-all-functions kao-treesit-select-all-siblings
                 kao-treesit-filter-to-functions))
    (should (kao-treesit-tests--mode-off-user-errors-p cmd))))

(ert-deftest kao-treesit-reload-queries-usable-mode-off ()
  "`kao-treesit-reload-queries' is a cache operation, deliberately EXCLUDED from
the mode guard — it never touches `kao--sels', so it stays usable with
`kao-mode' off.  It runs without signalling in a non-kao buffer."
  (with-temp-buffer
    (fundamental-mode)
    (should-not (kao-treesit-tests--mode-off-user-errors-p 'kao-treesit-reload-queries))
    (kao-treesit-reload-queries)))           ; and it actually runs, no error

(provide 'kao-treesit-tests)
;;; kao-treesit-tests.el ends here
