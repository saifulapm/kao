;;; kao-surround.el --- Surround (add/delete/replace) for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; An opt-in surround feature for kao, ported from the user's Kakoune `match'
;; user-mode (Kakoune itself ships no surround; it is always user config).  Like
;; `kao-objects', this module builds ENTIRELY on kao's public config substrate
;; (the `kao-' API) — no `kao--' reach-ins, no `setq kao--sels' — so it is a
;; worked example of a real feature written against the documented surface.
;;
;; It registers NOTHING on load.  Opt in from your config:
;;
;;   (require 'kao-surround)
;;   (kao-surround-setup)            ; binds `m' -> the `match' user-mode
;;
;; Then, in normal state (mirroring the Kakoune `match'/`surround-add' modes):
;;
;;   m m         goto matching delimiter        (kao's `m')
;;   m i KEY     select inner object            (<a-i>)
;;   m a KEY     select whole object            (<a-a>)
;;   m s KEY     surround: wrap each selection with the KEY pair
;;   m d KEY     delete the KEY pair surrounding each selection
;;   m r OLD NEW replace the OLD surrounding pair with the NEW one
;;
;; KEY is a delimiter from `kao-surround-pairs' (brackets, quotes, `* _ |',
;; chevrons, "t" = an HTML/XML tag prompt, ...).  Delete and replace FIND the
;; enclosing pair through kao's object system (`kao-object-bounds') — exactly as
;; evil-surround reuses text objects and the user's kak uses `<a-a>KEY' — so they
;; work from anywhere inside the pair, honour nesting, and strip BOTH delimiter
;; spans (correct for multi-character delimiters such as tags, where the kak's
;; strip-one-char-each-end is not).  Multiple selections are wrapped/stripped as
;; one undo unit.  The optional tree-sitter layer (see `kao-surround-treesit')
;; upgrades the tag/element handling and adds `m n' (enclosing named node).

;;; Code:

(require 'kao-selection)
(require 'kao-object)
(require 'kao-edit)
(require 'kao-state)
(require 'kao-objects)
(require 'treesit nil t)                 ; soft: the optional tree-sitter layer

(defgroup kao-surround nil
  "Surround (add/delete/replace) for kao."
  :group 'kao
  :prefix "kao-surround-")

(defun kao-surround-read-tag ()
  "Read an HTML/XML tag name and return its (\"<name>\" . \"</name>\") pair.
The tag entry of `kao-surround-pairs' (the user's kak `surround-tag' prompt)."
  (let ((name (read-string "tag: ")))
    (cons (concat "<" name ">") (concat "</" name ">"))))

(defcustom kao-surround-pairs
  '((?\( . ("(" . ")"))
    (?\) . ("(" . ")"))
    (?\{ . ("{" . "}"))
    (?\} . ("{" . "}"))
    (?\[ . ("[" . "]"))
    (?\] . ("[" . "]"))
    (?<  . ("<" . ">"))
    (?>  . ("<" . ">"))
    (?\" . ("\"" . "\""))
    (?\' . ("'" . "'"))
    (?\` . ("`" . "`"))
    (?*  . ("*" . "*"))
    (?_  . ("_" . "_"))
    (?|  . ("|" . "|"))
    (?\s . (" " . " "))
    (?«  . ("«" . "»"))
    (?»  . ("«" . "»"))
    (?“  . ("“" . "”"))
    (?”  . ("“" . "”"))
    (?t  . kao-surround-read-tag))
  "Surround delimiter table: trigger character -> the pair to insert.
Each value is either a cons of strings (OPEN . CLOSE) inserted verbatim, or a
function of no arguments returning such a cons (used for the tag entry, which
prompts for a tag name).  Ported from the user's Kakoune `surround-add' mode.
Both
ends of a pair map to the same entry (so `(' and `)' behave identically)."
  :type '(alist :key-type character
                :value-type (choice (cons (string :tag "Open")
                                          (string :tag "Close"))
                                    (function :tag "Returns (OPEN . CLOSE)")))
  :group 'kao-surround)

(defcustom kao-surround-literal-fallback t
  "When non-nil, an unmapped printable surround key is its own literal delimiter.
`kao-surround--resolve-pair' falls back to the self-pair (CHAR . CHAR) for a
printable, non-blank CHAR that has no `kao-surround-pairs' entry — the
evil-surround / hel-surround default, so e.g. `m s ~' wraps with `~ ... ~'
without a table entry.  Set to nil to require an explicit `kao-surround-pairs'
entry (the old strict behaviour).  Name FROZEN."
  :type 'boolean
  :group 'kao-surround)

(defun kao-surround--resolve-pair (char)
  "Return the (OPEN . CLOSE) string pair for CHAR, or nil if CHAR is unmapped.
Calls the entry when it is a function (the tag prompt).  When CHAR is unmapped
and `kao-surround-literal-fallback' is non-nil and CHAR is a printable,
non-blank character, fall back to the literal self-pair (CHAR . CHAR) — the
evil-surround / hel-surround default."
  (let ((v (cdr (assq char kao-surround-pairs))))
    (cond ((functionp v) (funcall v))
          (v v)
          ((and kao-surround-literal-fallback
                (characterp char) (> char ?\s) (/= char ?\d))
           (let ((s (char-to-string char))) (cons s s))))))

;;;; which-key labels for the `m s' add-map

(defun kao-surround--add-info-rows ()
  "Build (EVENT . DOC) label rows from `kao-surround-pairs'.
Each entry maps its trigger key to a short label — the open/close strings, or
\"tag\" for the function-valued entry.  Feeds the `m s' which-key labels, so they
track the pair table; `m d' / `m r' show no autoinfo box on purpose."
  (mapcar (lambda (e)
            (let ((v (cdr e)))
              (cons (car e)
                    (if (functionp v) "tag" (format "%s %s" (car v) (cdr v))))))
          kao-surround-pairs))

;;;; Primitives — on the public substrate (kao-edit-* + kao-object-bounds)

(defun kao-surround-add (open close)
  "Wrap each selection with the OPEN and CLOSE strings, keeping it selected.
One undo unit across all selections (`kao-edit-keeping-selections', the faithful
`add-surrounding-pair' = `-draft P'/`p')."
  (kao-edit-keeping-selections
   (lambda (marks)
     (dolist (m marks)
       (let ((beg (min (marker-position (car m)) (marker-position (cdr m))))
             (end (1+ (max (marker-position (car m)) (marker-position (cdr m))))))
         (save-excursion
           (goto-char end) (insert close)
           (goto-char beg) (insert open)))))))

(defun kao-surround-delete (key)
  "Delete the delimiters of the KEY pair surrounding each selection.
KEY is an object key (e.g. ?\\( or the registered tag key ?t).  For each
selection the enclosing whole and inner spans are located via
`kao-object-bounds'; the open delimiter [whole-min, inner-min) and the close
delimiter (inner-max, whole-max] are removed and the inner content re-selected.
Works for multi-character delimiters (tags), unlike a strip-one-char approach.
A selection with no enclosing KEY pair is left unchanged.  One undo unit."
  (kao-edit-selections
   (lambda (am cm _i)
     (let* ((a (marker-position am)) (c (marker-position cm))
            (fwd (<= a c))
            (sel (kao-sel-make :anchor a :cursor c))
            (whole (kao-object-bounds key sel))
            (inner (kao-object-bounds key sel t)))
       (if (and whole inner)
           (let* ((wmin (kao-sel-min whole)) (wmax (kao-sel-max whole))
                  (imin (kao-sel-min inner)) (imax (kao-sel-max inner))
                  (w (- imin wmin)))               ; open-delimiter width
             (delete-region (1+ imax) (1+ wmax))   ; close delimiter (higher first)
             (delete-region wmin imin)             ; open delimiter
             (let ((nb wmin) (ne (- imax w)))      ; inner, shifted left by W
               (if fwd (cons nb ne) (cons ne nb))))
         (cons a c))))))                           ; no pair: keep as-is

(defun kao-surround-replace (old-key open close)
  "Replace the OLD-KEY surrounding pair with the OPEN/CLOSE strings.
Bounds-first: for each selection the enclosing OLD-KEY whole
and inner spans are resolved via `kao-object-bounds' BEFORE any edit; only a
selection whose old pair is found is stripped of the old delimiters and
re-wrapped with the new pair (its inner content re-selected).  A selection with
no enclosing OLD-KEY pair is left BYTE-IDENTICAL — never wrapped with a stray
new pair (the evil-surround / hel-surround `cs' semantics, and the user's kak
`surround-replace' which runs `<a-a>KEY' first so a missing pair aborts before
any edit).  A message when NO selection matched (the all-dropped precedent).
One undo unit."
  (let ((matched 0))
    (kao-edit-selections
     (lambda (am cm _i)
       (let* ((a (marker-position am)) (c (marker-position cm))
              (fwd (<= a c))
              (sel (kao-sel-make :anchor a :cursor c))
              (whole (kao-object-bounds old-key sel))
              (inner (kao-object-bounds old-key sel t)))
         (if (not (and whole inner))
             (cons a c)                           ; no pair: keep byte-identical
           (let* ((wmin (kao-sel-min whole)) (wmax (kao-sel-max whole))
                  (imin (kao-sel-min inner)) (imax (kao-sel-max inner))
                  (ow (- imin wmin))              ; old open-delimiter width
                  (no (length open)))
             (setq matched (1+ matched))
             ;; higher position first so the lower ones stay valid
             (goto-char (1+ imax)) (delete-region (1+ imax) (1+ wmax))
             (insert close)                       ; new close delimiter
             (goto-char wmin) (delete-region wmin imin)
             (insert open)                        ; new open delimiter
             (let ((nb (+ wmin no))               ; inner shifted -OW +NO
                   (ne (+ (- imax ow) no)))
               (if fwd (cons nb ne) (cons ne nb))))))))
    (when (zerop matched)
      (message "kao-surround: no pair to replace"))))

;;;; Interactive commands (the keymap entry points)

(defun kao-surround-add-dwim ()
  "Surround each selection with the pair for the key that invoked this command.
Bound to every delimiter key of `kao-surround-add-map' (the `m s' sub-mode)."
  (interactive)
  (kao--assert-mode)
  (let ((pair (kao-surround--resolve-pair last-command-event)))
    (if pair
        (kao-surround-add (car pair) (cdr pair))
      (message "no surround pair for that key"))))

(defun kao-surround-delete-key ()
  "Read an object key and delete that surrounding pair (`m d')."
  (interactive)
  (kao--assert-mode)
  (kao-on-key "delete surround: " #'kao-surround-delete))

(defun kao-surround-replace-key ()
  "Read the old object key then the new delimiter key and replace (`m r').
Both keys are read first; the new pair is resolved and applied AFTERWARD, so a
tag prompt runs in a clean command context instead of nested in a key read
\(nesting misroutes its input to the buffer)."
  (interactive)
  (kao--assert-mode)
  (let (old new)
    (kao-on-key "replace surround (from): " (lambda (k) (setq old k)))
    (kao-on-key "replace surround (to): " (lambda (k) (setq new k)))
    ;; Cancel before any edit when either key is a non-character or Escape
    ;; — a stray `<escape>' must not misfire a replace.
    (if (not (and (characterp old) (/= old ?\e)
                  (characterp new) (/= new ?\e)))
        (message "kao-surround: replace cancelled")
      (let ((pair (kao-surround--resolve-pair new)))
        (if pair
            (kao-surround-replace old (car pair) (cdr pair))
          (message "no surround pair for that key"))))))

;;;; Tree-sitter layer (opt-in, gated on a loaded grammar)

(defcustom kao-surround-treesit nil
  "When non-nil, kao-surround uses tree-sitter for the tag/element object.
With a parser loaded in the buffer the `?t' object (so `m d t' / `m r t' and
`<a-a>t') targets the enclosing element node (html/jsx/tsx) for node-accurate
bounds, falling back to the regex tag (kept on `?T') when no parser is present.
`m n' (select the enclosing named node) is available whenever a parser is
loaded.  Enable via this option or `(kao-surround-setup t)'."
  :type 'boolean
  :group 'kao-surround)

(defconst kao-surround--element-types '("element" "jsx_element")
  "Tree-sitter node types treated as surroundable elements (open + close tag).")

(defun kao-surround--treesit-ready-p ()
  "Non-nil when tree-sitter is available and a parser is loaded for this buffer."
  (and (fboundp 'treesit-available-p) (treesit-available-p)
       (fboundp 'treesit-parser-list) (treesit-parser-list)))

(defun kao-surround--treesit-element-node (pos &optional level)
  "Return the enclosing element node at POS, or nil.
LEVEL (default 0) is the count-th enclosing element (0 = innermost)."
  (let ((node (treesit-node-at pos)) (n (or level 0)))
    (catch 'hit
      (while node
        (when (member (treesit-node-type node) kao-surround--element-types)
          (if (<= n 0) (throw 'hit node) (setq n (1- n))))
        (setq node (treesit-node-parent node)))
      nil)))

(defun kao-surround--treesit-element (sel inner _to-begin _to-end &optional level)
  "Tree-sitter element object around SEL's cursor, as a `kao-sel' or nil.
A `kao-object-register' selector: the whole object spans the element, INNER
spans between its open and close tags (the first and last child nodes), and
LEVEL is the count-th enclosing element."
  (let ((node (kao-surround--treesit-element-node (kao-sel-cursor sel) level)))
    (when node
      (if inner
          (let* ((count (treesit-node-child-count node))
                 (first (and (> count 0) (treesit-node-child node 0)))
                 (last (and (> count 0) (treesit-node-child node (1- count))))
                 (ibeg (and first (treesit-node-end first)))
                 (iend (and last (1- (treesit-node-start last)))))
            (when (and ibeg iend (<= ibeg iend))
              (kao-sel-make :anchor ibeg :cursor iend)))
        (kao-sel-make :anchor (treesit-node-start node)
                      :cursor (1- (treesit-node-end node)))))))

(defun kao-surround--tag-selector (sel inner to-begin to-end &optional level)
  "The `?t' object selector: a tree-sitter element when ready, else the regex tag.
For SEL with INNER/TO-BEGIN/TO-END/LEVEL (the `kao-object-register' selector
contract), try the tree-sitter element, falling back to the regex tag kept on
`?T' (so a tag in a non-tree-sitter buffer still works)."
  (or (and kao-surround-treesit (kao-surround--treesit-ready-p)
           (kao-surround--treesit-element sel inner to-begin to-end level))
      (kao-object-bounds ?T sel inner level)))

(defun kao-surround--treesit-node-bounds (sel)
  "Return the enclosing named node around SEL as a `kao-sel'.
Grows to the named parent when SEL already exactly covers the node, so repeated
selection expands outward (expand-region style)."
  (let ((node (treesit-node-on (kao-sel-min sel) (1+ (kao-sel-max sel)))))
    (while (and node (not (treesit-node-check node 'named)))
      (setq node (treesit-node-parent node)))
    (when node
      (let ((nb (treesit-node-start node)) (ne (1- (treesit-node-end node))))
        (when (and (= nb (kao-sel-min sel)) (= ne (kao-sel-max sel)))
          (let ((p (treesit-node-parent node)))
            (while (and p (not (treesit-node-check p 'named)))
              (setq p (treesit-node-parent p)))
            (when p (setq nb (treesit-node-start p) ne (1- (treesit-node-end p))))))
        (kao-sel-make :anchor nb :cursor ne)))))

(defun kao-surround-select-node ()
  "Select the enclosing named tree-sitter node under each selection (`m n').
Repeating grows to the next named ancestor (expand-region style).  A no-op with
a message when no tree-sitter parser is loaded in the buffer."
  (interactive)
  (kao--assert-mode)
  (if (kao-surround--treesit-ready-p)
      (kao-map-selections #'kao-surround--treesit-node-bounds)
    (message "tree-sitter not available in this buffer")))

;;;; Keymaps + setup

(defvar kao-surround-add-map
  (let ((map (make-sparse-keymap)))
    ;; `[t]' default binding: any key routes to add-dwim, which
    ;; resolves `last-command-event' at run time — so a `kao-surround-pairs'
    ;; entry added after setup works without re-running `kao-surround-setup'.
    (define-key map [t] #'kao-surround-add-dwim)
    map)
  "Keymap for the `m s' surround-add sub-mode.
Its `[t]' default binding routes every key to `kao-surround-add-dwim'; explicit
per-key bindings (added by `kao-surround-setup' from `kao-surround-pairs') exist
only so which-key can label the known delimiters.")

(defvar kao-match-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" #'kao-select-matching)   ; goto matching (kao's `m')
    (define-key map "i" #'kao-select-inner)       ; <a-i>
    (define-key map "a" #'kao-select-whole)       ; <a-a>
    (define-key map "s" kao-surround-add-map)     ; surround-add sub-mode
    (define-key map "d" #'kao-surround-delete-key)
    (define-key map "r" #'kao-surround-replace-key)
    map)
  "The `match' user-mode keymap, ported from the user's kak `match' mode.
Bound to `m' in `kao-normal-state-map' by `kao-surround-setup'.")

(defun kao-surround--populate-add-map ()
  "Bind every `kao-surround-pairs' key in `kao-surround-add-map' to add-dwim."
  (dolist (entry kao-surround-pairs)
    (define-key kao-surround-add-map (vector (car entry))
                #'kao-surround-add-dwim)))

(defun kao-surround--register-which-key ()
  "Label the `kao-surround-add-map' keys in which-key (display only).
Every delimiter key is bound to the same `kao-surround-add-dwim', so which-key
would otherwise show that command name for all of them; this shows the pair (or
\"tag\") instead.  A no-op when which-key is absent (soft dependency) —
the bindings themselves are never touched, so dispatch is unchanged."
  (when (fboundp 'which-key-add-keymap-based-replacements)
    (dolist (row (kao-surround--add-info-rows))
      (which-key-add-keymap-based-replacements kao-surround-add-map
        (key-description (vector (car row))) (cdr row)))))

;;;###autoload
(defun kao-surround-setup (&optional treesit)
  "Enable kao-surround: bind `m' to the `match' user-mode in normal state.
Populates `kao-surround-add-map' from `kao-surround-pairs', binds `m n' to
`kao-surround-select-node', registers the tag object on `?t' (so `m d t'/`m r t'
and `<a-a>t' work), and binds `m' -> `kao-match-map' in `kao-normal-state-map',
overriding the default `m' (goto-matching, still reachable as `m m').

With TREESIT non-nil (or `kao-surround-treesit' already set) the `?t' object
uses the tree-sitter element when a parser is loaded, falling back to the regex
tag kept on `?T'; otherwise `?t' is the regex tag everywhere.  Opt-in: kao's
default keymap is unchanged until you call this.  Safe to call more than once."
  (interactive)
  (when treesit (setq kao-surround-treesit t))
  (kao-surround--populate-add-map)
  (kao-surround--register-which-key)
  (define-key kao-match-map "n" #'kao-surround-select-node)
  (if kao-surround-treesit
      (progn
        (kao-objects-register-tag ?T)   ; regex tag fallback on ?T
        (kao-object-register ?t #'kao-surround--tag-selector "tag"))
    (kao-objects-register-tag ?t))      ; regex tag object on ?t
  (define-key kao-normal-state-map "m" kao-match-map))

(provide 'kao-surround)
;;; kao-surround.el ends here
