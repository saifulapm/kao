;;; kao-surround-tests.el --- Tests for kao-surround -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT pins for `kao-surround' — add/delete/replace built on the public config
;; substrate, the multi-selection one-undo-unit guarantee, delete/replace
;; finding the ENCLOSING pair via `kao-object-bounds' (non-adjacent, nested,
;; multi-character/tag), and direction preservation.  Treesit pins live in the
;; tree-sitter section and `skip-unless' a loaded grammar.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-state)
(require 'kao-edit)
(require 'kao-object)
(require 'kao-objects)
(require 'kao-keys)
(require 'kao-surround)
(require 'treesit nil t)

(defmacro kao-surround-tests--with (content &rest body)
  "Run BODY in a `kao-mode' temp buffer initialised to CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (buffer-enable-undo)
     (kao-mode 1)
     (unwind-protect (progn ,@body)
       (kao-mode -1))))

(defun kao-surround-tests--pairs ()
  "Return the live selections as a list of (ANCHOR . CURSOR) integer conses."
  (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
          (kao-get-selections)))

;;;; resolve-pair

(ert-deftest kao-surround-resolve-pair-literal ()
  "`kao-surround--resolve-pair' returns the (OPEN . CLOSE) for a literal entry."
  (should (equal (kao-surround--resolve-pair ?\() '("(" . ")")))
  (should (equal (kao-surround--resolve-pair ?\)) '("(" . ")")))   ; both ends
  (let ((kao-surround-literal-fallback nil))                       ; strict
    (should (null (kao-surround--resolve-pair ?Z)))))              ; unmapped

(ert-deftest kao-surround-resolve-pair-tag-function ()
  "The `t' entry is a function that prompts and returns the tag pair."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "div")))
    (should (equal (kao-surround--resolve-pair ?t) '("<div>" . "</div>")))))

;;;; Literal self-pair fallback + [t] add-map default

(ert-deftest kao-surround-resolve-pair-literal-fallback ()
  "An unmapped printable key falls back to the literal self-pair (default t).
Blanks/control chars never self-pair; `kao-surround-literal-fallback' nil
restores the strict unmapped -> nil behaviour."
  (should (equal (kao-surround--resolve-pair ?~) '("~" . "~")))
  (should (equal (kao-surround--resolve-pair ?Z) '("Z" . "Z")))
  (should (equal (kao-surround--resolve-pair ?#) '("#" . "#")))
  (should (null (kao-surround--resolve-pair ?\t)))     ; tab: control, not printable
  (should (null (kao-surround--resolve-pair ?\e)))     ; escape
  (let ((kao-surround-literal-fallback nil))
    (should (null (kao-surround--resolve-pair ?~)))))   ; strict: no fallback

(ert-deftest kao-surround-add-literal-self-pair ()
  "`m s ~' (a key absent from `kao-surround-pairs') wraps with the literal `~'."
  (kao-surround-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 4)))   ; "bcd"
    (let ((last-command-event ?~))
      (kao-surround-add-dwim))
    (should (string= (buffer-string) "a~bcd~ef"))
    (should (equal (kao-surround-tests--pairs) '((3 . 5))))))

(ert-deftest kao-surround-add-literal-fallback-off-messages ()
  "With `kao-surround-literal-fallback' nil, an unmapped `m s' key messages."
  (kao-surround-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 4)))
    (let ((kao-surround-literal-fallback nil) (last-command-event ?~) msg)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (kao-surround-add-dwim))
      (should (string= (buffer-string) "abcdef"))
      (should (string-match-p "no surround pair" msg)))))

(ert-deftest kao-surround-add-map-default-binding-routes-unmapped ()
  "`kao-surround-add-map' has a `[t]' default binding to add-dwim.
So a key absent from `kao-surround-pairs' at setup time still routes to the dwim
resolver (the key set no longer freezes at setup)."
  (should (eq #'kao-surround-add-dwim (lookup-key kao-surround-add-map [t])))
  (should (eq #'kao-surround-add-dwim
              (lookup-key kao-surround-add-map (vector ?~) t))))

;;;; add

(ert-deftest kao-surround-add-wraps-and-reselects ()
  "`kao-surround-add' wraps the selection and keeps the original span selected."
  (kao-surround-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 4)))   ; "bcd"
    (kao-surround-add "(" ")")
    (should (string= (buffer-string) "a(bcd)ef"))
    (should (equal (kao-surround-tests--pairs) '((3 . 5))))))

(ert-deftest kao-surround-add-multi-one-undo ()
  "`kao-surround-add' wraps multiple selections as ONE undo unit."
  (kao-surround-tests--with "ab cd"
    (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 2)
                              (kao-sel-make :anchor 4 :cursor 5)))
    (undo-boundary)
    (kao-surround-add "(" ")")
    (should (string= (buffer-string) "(ab) (cd)"))
    (should (equal (kao-surround-tests--pairs) '((2 . 3) (7 . 8))))
    (primitive-undo 1 buffer-undo-list)
    (should (string= (buffer-string) "ab cd"))))

(ert-deftest kao-surround-add-multichar-tag ()
  "`kao-surround-add' supports multi-character delimiters (a tag)."
  (kao-surround-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 4)))   ; "bcd"
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "x")))
      (let ((pair (kao-surround--resolve-pair ?t)))
        (kao-surround-add (car pair) (cdr pair))))
    (should (string= (buffer-string) "a<x>bcd</x>ef"))
    (should (equal (kao-surround-tests--pairs) '((5 . 7))))))      ; "bcd" shifted +3

(ert-deftest kao-surround-add-dwim-uses-invoking-key ()
  "`kao-surround-add-dwim' wraps with the pair for `last-command-event'."
  (kao-surround-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 4)))
    (let ((last-command-event ?\())
      (kao-surround-add-dwim))
    (should (string= (buffer-string) "a(bcd)ef"))))

;;;; delete

(ert-deftest kao-surround-delete-adjacent ()
  "`kao-surround-delete' strips the pair and re-selects the inner content."
  (kao-surround-tests--with "a(bcd)ef"
    (kao-set-selections (list (kao-sel-make :anchor 3 :cursor 5)))   ; "bcd"
    (kao-surround-delete ?\()
    (should (string= (buffer-string) "abcdef"))
    (should (equal (kao-surround-tests--pairs) '((2 . 4))))))

(ert-deftest kao-surround-delete-non-adjacent-enclosing ()
  "`kao-surround-delete' finds the ENCLOSING pair from a cursor inside it."
  (kao-surround-tests--with "x(foo bar)y"
    (kao-set-selections (list (kao-sel-make :anchor 7 :cursor 7)))   ; on "b"
    (kao-surround-delete ?\()
    (should (string= (buffer-string) "xfoo bary"))
    (should (equal (kao-surround-tests--pairs) '((2 . 8))))))       ; "foo bar"

(ert-deftest kao-surround-delete-nested-innermost ()
  "`kao-surround-delete' removes the INNERMOST enclosing pair (level 0)."
  (kao-surround-tests--with "((x))"
    (kao-set-selections (list (kao-sel-make :anchor 3 :cursor 3)))   ; on "x"
    (kao-surround-delete ?\()
    (should (string= (buffer-string) "(x)"))
    (should (equal (kao-surround-tests--pairs) '((2 . 2))))))

(ert-deftest kao-surround-delete-preserves-backward-direction ()
  "`kao-surround-delete' preserves a backward selection's anchor/cursor order."
  (kao-surround-tests--with "a(bcd)ef"
    (kao-set-selections-raw (list (kao-sel-make :anchor 4 :cursor 3))) ; backward
    (kao-surround-delete ?\()
    (should (string= (buffer-string) "abcdef"))
    (should (equal (kao-surround-tests--pairs) '((4 . 2))))))        ; still backward

(ert-deftest kao-surround-delete-no-pair-noop ()
  "`kao-surround-delete' leaves a selection with no enclosing pair unchanged."
  (kao-surround-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 4)))
    (kao-surround-delete ?\()
    (should (string= (buffer-string) "abcdef"))
    (should (equal (kao-surround-tests--pairs) '((2 . 4))))))

(ert-deftest kao-surround-delete-key-reads-object ()
  "`kao-surround-delete-key' reads the object key via `kao-on-key'."
  (kao-surround-tests--with "a(bcd)ef"
    (kao-set-selections (list (kao-sel-make :anchor 3 :cursor 5)))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\()))
      (kao-surround-delete-key))
    (should (string= (buffer-string) "abcdef"))
    (should (equal (kao-surround-tests--pairs) '((2 . 4))))))

(ert-deftest kao-surround-delete-tag-multichar ()
  "`kao-surround-delete' strips multi-character tag delimiters (regex tag on ?t)."
  (let ((kao--object-runtime-table nil)
        (kao--object-runtime-info nil)
        (kao--object-runtime-nested nil))
    (kao-objects-register-tag ?t)
    (kao-surround-tests--with "a<b>cd</b>e"
      (kao-set-selections (list (kao-sel-make :anchor 5 :cursor 5))) ; on "c"
      (kao-surround-delete ?t)
      (should (string= (buffer-string) "acde"))
      (should (equal (kao-surround-tests--pairs) '((2 . 3)))))))     ; "cd"

;;;; replace

(ert-deftest kao-surround-replace-swaps-pair ()
  "`kao-surround-replace' swaps the pair, keeping the inner content selected."
  (kao-surround-tests--with "a(bcd)ef"
    (kao-set-selections (list (kao-sel-make :anchor 3 :cursor 5)))   ; "bcd"
    (kao-surround-replace ?\( "{" "}")
    (should (string= (buffer-string) "a{bcd}ef"))
    (should (equal (kao-surround-tests--pairs) '((3 . 5))))))

(ert-deftest kao-surround-replace-multi-one-undo ()
  "`kao-surround-replace' over multiple selections is one undo unit."
  (kao-surround-tests--with "(a) (b)"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 2)
                              (kao-sel-make :anchor 6 :cursor 6)))
    (undo-boundary)
    (kao-surround-replace ?\( "[" "]")
    (should (string= (buffer-string) "[a] [b]"))
    (primitive-undo 1 buffer-undo-list)
    (should (string= (buffer-string) "(a) (b)"))))

;;;; Replace is bounds-first (no wrap when no old pair)

(ert-deftest kao-surround-replace-no-old-pair-noop ()
  "`kao-surround-replace' on a selection with NO enclosing OLD pair is a no-op.
Bounds-first: nothing to strip means nothing is wrapped (evil-surround `cs')."
  (kao-surround-tests--with "plain text here"
    (kao-set-selections (list (kao-sel-make :anchor 7 :cursor 10)))  ; "text"
    (cl-letf (((symbol-function 'message) #'ignore))
      (kao-surround-replace ?\( "[" "]"))
    (should (string= (buffer-string) "plain text here"))
    (should (equal (kao-surround-tests--pairs) '((7 . 10))))))

(ert-deftest kao-surround-replace-escape-old-noop ()
  "An unbound OLD key (Escape, 27) resolves no pair, so replace no-ops."
  (kao-surround-tests--with "a(bcd)ef"
    (kao-set-selections (list (kao-sel-make :anchor 3 :cursor 5)))
    (cl-letf (((symbol-function 'message) #'ignore))
      (kao-surround-replace 27 "(" ")"))          ; 27 = Escape, no object
    (should (string= (buffer-string) "a(bcd)ef"))
    (should (equal (kao-surround-tests--pairs) '((3 . 5))))))

(ert-deftest kao-surround-replace-mixed-edits-only-enclosed ()
  "Multi-cursor replace edits only the selection whose OLD pair is found.
`foo (bar) baz qux' with cursors on `bar' (in parens) and `qux' (no pair):
only `bar' is re-wrapped; `qux' stays byte-identical."
  (kao-surround-tests--with "foo (bar) baz qux"
    (kao-set-selections (list (kao-sel-make :anchor 6 :cursor 8)      ; bar in parens
                              (kao-sel-make :anchor 15 :cursor 17)))  ; qux no pair
    (kao-surround-replace ?\( "[" "]")
    (should (string= (buffer-string) "foo [bar] baz qux"))
    (should (equal (kao-surround-tests--pairs) '((6 . 8) (15 . 17))))))

(ert-deftest kao-surround-replace-multichar-new-pair ()
  "Bounds-first replace still handles a multi-character new pair (width shift)."
  (kao-surround-tests--with "a(bcd)ef"
    (kao-set-selections (list (kao-sel-make :anchor 3 :cursor 5)))    ; "bcd"
    (kao-surround-replace ?\( "<<" ">>")
    (should (string= (buffer-string) "a<<bcd>>ef"))
    (should (equal (kao-surround-tests--pairs) '((4 . 6))))))         ; "bcd" shifted +1

(ert-deftest kao-surround-delete-multi-one-undo ()
  "`kao-surround-delete' strips pairs around multiple selections as one undo unit."
  (kao-surround-tests--with "(a) (b)"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 2)
                              (kao-sel-make :anchor 6 :cursor 6)))
    (undo-boundary)
    (kao-surround-delete ?\()
    (should (string= (buffer-string) "a b"))
    (should (equal (kao-surround-tests--pairs) '((1 . 1) (3 . 3))))
    (primitive-undo 1 buffer-undo-list)
    (should (string= (buffer-string) "(a) (b)"))))

;;;; Tree-sitter layer (opt-in) — skip-unless a grammar is installed

(ert-deftest kao-surround-treesit-element-bounds-html ()
  "The tree-sitter element selector returns the whole and inner spans (html)."
  (skip-unless (and (fboundp 'treesit-available-p) (treesit-available-p)
                    (treesit-language-available-p 'html)))
  (with-temp-buffer
    (insert "<div>hello</div>")              ; <div>=1-5 hello=6-10 </div>=11-16
    (treesit-parser-create 'html)
    (let ((whole (kao-surround--treesit-element
                  (kao-sel-make :anchor 7 :cursor 7) nil t t))
          (inner (kao-surround--treesit-element
                  (kao-sel-make :anchor 7 :cursor 7) t t t)))
      (should (= (kao-sel-min whole) 1)) (should (= (kao-sel-max whole) 16))
      (should (= (kao-sel-min inner) 6)) (should (= (kao-sel-max inner) 10)))))

(ert-deftest kao-surround-treesit-delete-element-html ()
  "`kao-surround-delete' strips multi-char element tags via tree-sitter (html)."
  (skip-unless (and (fboundp 'treesit-available-p) (treesit-available-p)
                    (treesit-language-available-p 'html)))
  (let ((kao--object-runtime-table nil) (kao--object-runtime-info nil)
        (kao--object-runtime-nested nil) (kao-surround-treesit t))
    (kao-objects-register-tag ?T)
    (kao-object-register ?t #'kao-surround--tag-selector "tag")
    (with-temp-buffer
      (insert "<div>hello</div>")
      (treesit-parser-create 'html)
      (buffer-enable-undo)
      (kao-mode 1)
      (unwind-protect
          (progn
            (kao-set-selections (list (kao-sel-make :anchor 7 :cursor 7)))
            (kao-surround-delete ?t)
            (should (string= (buffer-string) "hello"))
            (should (equal (kao-surround-tests--pairs) '((1 . 5)))))
        (kao-mode -1)))))

(ert-deftest kao-surround-treesit-tag-selector-falls-back-to-regex ()
  "With treesit on but NO parser, the `?t' selector falls back to the regex tag."
  (let ((kao--object-runtime-table nil) (kao--object-runtime-info nil)
        (kao--object-runtime-nested nil) (kao-surround-treesit t))
    (kao-objects-register-tag ?T)
    (kao-object-register ?t #'kao-surround--tag-selector "tag")
    (with-temp-buffer
      (insert "a<b>cd</b>e")                  ; no parser -> regex tag on ?T
      (let ((whole (kao-object-bounds ?t (kao-sel-make :anchor 5 :cursor 5))))
        (should whole)
        (should (= (kao-sel-min whole) 2))     ; "<b>" start
        (should (= (kao-sel-max whole) 10))))))  ; "</b>" end

(ert-deftest kao-surround-treesit-select-node-json ()
  "`kao-surround-select-node' selects the enclosing named node, growing on repeat."
  (skip-unless (and (fboundp 'treesit-available-p) (treesit-available-p)
                    (treesit-language-available-p 'json)))
  (with-temp-buffer
    (insert "[1, 22]")                        ; [=1 1=2 ,=3 _=4 2=5 2=6 ]=7
    (treesit-parser-create 'json)
    (buffer-enable-undo)
    (let ((kao-surround-treesit t))
      (kao-mode 1)
      (unwind-protect
          (progn
            (kao-set-selections (list (kao-sel-make :anchor 5 :cursor 5)))  ; on "22"
            (kao-surround-select-node)
            (should (equal (kao-surround-tests--pairs) '((5 . 6))))    ; "22"
            (kao-surround-select-node)
            (should (equal (kao-surround-tests--pairs) '((1 . 7)))))   ; "[1, 22]"
        (kao-mode -1)))))

(ert-deftest kao-surround-select-node-no-treesit-message ()
  "`kao-surround-select-node' is a no-op (with a message) when no parser is loaded."
  (kao-surround-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 4)))
    (let (msg)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (kao-surround-select-node))
      (should (and msg (string-match-p "tree-sitter" msg))))
    (should (equal (kao-surround-tests--pairs) '((2 . 4))))))

;;;; setup

(ert-deftest kao-surround-setup-binds-match-map ()
  "`kao-surround-setup' binds `m' to the match map and populates the add map."
  (let ((kao--object-runtime-table nil)
        (kao--object-runtime-info nil)
        (kao--object-runtime-nested nil)
        (orig (lookup-key kao-normal-state-map "m")))
    (unwind-protect
        (progn
          (kao-surround-setup)
          (should (eq (lookup-key kao-normal-state-map "m") kao-match-map))
          (should (eq (lookup-key kao-match-map "s") kao-surround-add-map))
          (should (eq (lookup-key kao-surround-add-map "(") #'kao-surround-add-dwim))
          (should (eq (lookup-key kao-surround-add-map "t") #'kao-surround-add-dwim)))
      (define-key kao-normal-state-map "m" orig))))

;;;; which-key labels (m s) + the m r read-then-apply control flow

(ert-deftest kao-surround-add-info-rows-generated-from-pairs ()
  "`kao-surround--add-info-rows' labels each pair (glyphs) and the tag entry."
  (let ((rows (kao-surround--add-info-rows)))
    (should (equal (cdr (assq ?\( rows)) "( )"))
    (should (equal (cdr (assq ?{ rows)) "{ }"))
    (should (equal (cdr (assq ?t rows)) "tag"))))

;; The `m r'/`m d' key-reader tests below run in `kao-surround-tests--with'
;; (kao-mode active) so the mode guard passes; in-Owns
;; mechanical wrap left every `should'/`should-not' value below unchanged.
(ert-deftest kao-surround-replace-key-reads-then-applies ()
  "`m r' reads the from+to keys, then calls `kao-surround-replace' with the new pair.
The resolve/apply runs AFTER both reads, so a `t' tag prompt is never nested in a
key read (the misrouted-input bug); no autoinfo box is shown either."
  ;; every `should' value below is unchanged.
  (kao-surround-tests--with "x"
    (let ((keys (list ?\( ?{)) got rows-seen)
      (cl-letf (((symbol-function 'kao-on-key)
                 (lambda (_prompt fn &optional info-rows)
                   (push info-rows rows-seen)
                   (funcall fn (pop keys))))
                ((symbol-function 'kao-surround-replace)
                 (lambda (old open close) (setq got (list old open close)))))
        (kao-surround-replace-key))
      (should (equal got '(?\( "{" "}")))            ; old key + resolved new pair
      (should (equal rows-seen '(nil nil))))))        ; NO autoinfo rows passed

(ert-deftest kao-surround-replace-key-unknown-to-key-noops ()
  "With the literal fallback off, an unmapped to-key yields no replace (message).
\(By default `kao-surround-literal-fallback' would self-pair it;
this pins the strict path.)"
  ;; the `should-not' below is unchanged.
  (kao-surround-tests--with "x"
    (let ((keys (list ?\( ?z)) called
          (kao-surround-literal-fallback nil))
      (cl-letf (((symbol-function 'kao-on-key)
                 (lambda (_prompt fn &optional _rows) (funcall fn (pop keys))))
                ((symbol-function 'kao-surround-replace)
                 (lambda (&rest _) (setq called t)))
                ((symbol-function 'message) #'ignore))
        (kao-surround-replace-key))
      (should-not called))))

(ert-deftest kao-surround-replace-key-escape-old-cancels ()
  "An Escape OLD key cancels `m r' before any edit."
  (kao-surround-tests--with "a(bcd)ef"
    (kao-set-selections (list (kao-sel-make :anchor 3 :cursor 5)))
    (let ((keys (list ?\e ?{)) called)
      (cl-letf (((symbol-function 'kao-on-key)
                 (lambda (_p fn &optional _r) (funcall fn (pop keys))))
                ((symbol-function 'kao-surround-replace)
                 (lambda (&rest _) (setq called t)))
                ((symbol-function 'message) #'ignore))
        (kao-surround-replace-key))
      (should-not called)
      (should (string= (buffer-string) "a(bcd)ef")))))

(ert-deftest kao-surround-replace-key-escape-new-cancels ()
  "An Escape NEW key cancels `m r' before any edit."
  (kao-surround-tests--with "a(bcd)ef"
    (kao-set-selections (list (kao-sel-make :anchor 3 :cursor 5)))
    (let ((keys (list ?\( ?\e)) called)
      (cl-letf (((symbol-function 'kao-on-key)
                 (lambda (_p fn &optional _r) (funcall fn (pop keys))))
                ((symbol-function 'kao-surround-replace)
                 (lambda (&rest _) (setq called t)))
                ((symbol-function 'message) #'ignore))
        (kao-surround-replace-key))
      (should-not called))))

(ert-deftest kao-surround-replace-key-noncharacter-cancels ()
  "A non-character OLD key (a function-key event) cancels `m r' before editing."
  (kao-surround-tests--with "a(bcd)ef"
    (kao-set-selections (list (kao-sel-make :anchor 3 :cursor 5)))
    (let ((keys (list 'f7 ?{)) called)
      (cl-letf (((symbol-function 'kao-on-key)
                 (lambda (_p fn &optional _r) (funcall fn (pop keys))))
                ((symbol-function 'kao-surround-replace)
                 (lambda (&rest _) (setq called t)))
                ((symbol-function 'message) #'ignore))
        (kao-surround-replace-key))
      (should-not called))))

(ert-deftest kao-surround-delete-key-passes-no-autoinfo ()
  "`m d' reads the object key with NO autoinfo rows (raw prompt)."
  ;; the `should' value below is unchanged.
  (kao-surround-tests--with "x"
    (let (rows-seen)
      (cl-letf (((symbol-function 'kao-on-key)
                 (lambda (_prompt _fn &optional info-rows) (setq rows-seen (list info-rows)))))
        (kao-surround-delete-key))
      (should (equal rows-seen '(nil))))))

(ert-deftest kao-surround-setup-registers-which-key-labels ()
  "`kao-surround-setup' labels the add-map keys in which-key (display only)."
  (let (recorded
        (orig (lookup-key kao-normal-state-map "m")))
    (cl-letf (((symbol-function 'which-key-add-keymap-based-replacements)
               (lambda (km key repl &rest _) (push (list km key repl) recorded))))
      (unwind-protect
          (progn
            (kao-surround-setup)
            (should (member (list kao-surround-add-map "(" "( )") recorded))
            (should (member (list kao-surround-add-map "t" "tag") recorded))
            ;; bindings themselves are untouched (dispatch unchanged)
            (should (eq (lookup-key kao-surround-add-map "(")
                        #'kao-surround-add-dwim)))
        (define-key kao-normal-state-map "m" orig)))))

;;;; Mode-off guard  — the `m'-mode surround commands

(ert-deftest kao-surround-commands-guard-mode-off ()
  "Every selection-touching `m'-mode surround command signals the shared
mode-off `user-error' in a non-kao buffer instead of reaching a nil selection
list.  `kao-surround-setup' is deliberately excluded — it is a
config entry point that binds keymaps and never reads selection state, so it
must stay callable with the mode off."
  (dolist (cmd '(kao-surround-select-node kao-surround-add-dwim
                 kao-surround-delete-key kao-surround-replace-key))
    (with-temp-buffer
      (fundamental-mode)                        ; kao-mode off -> kao--sels nil
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\())
                ((symbol-function 'read-char) (lambda (&rest _) ?\())
                ((symbol-function 'read-string) (lambda (&rest _) "div")))
        (let ((err (should-error (call-interactively cmd) :type 'user-error)))
          (should (string-match-p "kao-mode is not active" (cadr err))))))))

(provide 'kao-surround-tests)
;;; kao-surround-tests.el ends here
