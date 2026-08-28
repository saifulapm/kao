;;; kao-object.el --- Text objects for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 3.  Text objects: `<a-i>' (inner) / `<a-a>'
;; (whole) enter a one-shot object-pending read and select the object
;; under every selection's cursor.  Like the motions, an object is a pure
;; per-selection transform (`kao-sel' -> `kao-sel' or nil); the multi-selection
;; dispatch is `kao--map-filter-selections', faithful to Kakoune's
;; `select' (normal.cc:81) — a cursor not on the object drops that selection.
;;
;; The selectors mirror references/kakoune/src/selectors.cc; each takes the
;; ObjectFlags as (SEL INNER TO-BEGIN TO-END) and returns a forward selection
;; when TO-END is set, a backward one (anchor = cursor) when only TO-BEGIN is:
;;   `w'/`<a-w>' -> word/WORD       `select_word'        (l.145, P3 )
;;   `<space>'   -> whitespace run  `select_whitespaces' (l.615, )
;;   `s'         -> sentence        `select_sentence'    (l.482, )
;;   `p'         -> paragraph       `select_paragraph'   (l.555, )
;;   pairs/quotes-> surrounding     `select_surrounding' (l.378, )
;; The pair objects `' `{}' `[]' `<>' and quotes `"' `'' `` ` ''
;; reach ONE generic REGEX engine (`kao-object--rx-find-opening' /
;; `--rx-find-closing' / `--find-surrounding') as `regexp-quote'd
;; single chars; the `c' custom object passes the user's regexes to the same
;; engine.  Word categories follow the kao-motion `Word' class (Unicode
;; char-table); WORD = not-blank, never crosses a newline.
;;
;; The entry keys (normal.cc:2571-2580): `<a-a>'/`<a-i>' select the whole/inner
;; object (ToBegin|ToEnd); `['/`]' select to the object begin/end (one flag);
;; `{'/`}' extend to it (SelectMode::Extend); `<a-[>'/`<a-]>'/`<a-{>'/`<a-}>' are
;; the inner counterparts.  Replace replaces each selection; Extend merges
;; \(`kao--map-filter-selections' EXTEND arm).
;;
;; `<a-I>'/`<a-A>' (ObjectFlags::Nested, ) select ALL objects of the
;; chosen type within each selection — the per-key nested walks return span
;; lists and the dispatch rebuilds the whole selection list (main = last,
;).  A count picks the count-th enclosing pair level (`select_object''s
;; `params.count - 1'), both for the regular dispatch (the surrounding
;; selector's `level') and for nested pairs (`regex_select_nested''s depth);
;; the non-pair selectors ignore it, as in Kakoune.
;;
;; The `c' custom object (normal.cc:1479-1516) prompts
;; "object desc:" for `<open>,<close>' regexes (comma split with backslash
;; escape; only backslash-comma unescapes; the exact "desc parsing failed,
;; expected <open>,<close>" error) and feeds both dispatchers — the
;; surrounding selector (level = count-1) and the nested walks (open ==
;; close -> the alternating one-regex walk).  `<a-.>' repeats the captured
;; desc without re-prompting (the recorded thunk closes over it).
;;
;; documented deviations (family, loud not silent):
;; - BACKWARD REGEX ITERATION: Kakoune compiles the delimiters with
;;   RegexCompileFlags::Backward and iterates right-to-left; Emacs has no
;;   backward regex engine, so kao collects each window's matches by a
;;   FORWARD scan and walks the list in reverse — identical "matches fully
;;   within the window" semantics, diverging only for OVERLAPPING matches
;;   of one delimiter regex ("aa" in "aaa"), which real delimiters do
;;   not produce.
;; - DESC SYNTAX: the desc parts are EMACS regexes, not Kak syntax (the
;;   engine boundary, exactly as `s' and `/'); a non-compiling part
;;   errors at parse time where the C++ Regex ctor throws.
;; - The transient "Enter object desc" format box (show_auto_info_ifn,
;;   normal.cc:1481-1485) is not shown: the which-key boxes cover
;;   read-key menus, not minibuffer prompts; the prompt text is faithful.
;;   Macro interplay: recording captures the minibuffer keystrokes through
;;   the global pre-command-hook recorder, and replay feeds
;;   `read-string' from the macro — verified end-to-end by a live -nw pty
;;   smoke (journal); batch cannot drive the interactive
;;   minibuffer (noninteractive `read-string' reads stdin).
;;
;; The arbitrary-punctuation single-delimiter branch (normal.cc:1554,
;; keys.asciidoc:719-721) is `kao--object-punctuation-key-p' in the object
;; dispatch: any punctuation key (not alnum, not blank; `_' qualifies) is a
;; non-nestable object whose open == close is the char itself.
;; Emacs has no forced trailing newline, so the sentence/paragraph end-walks
;; treat the buffer end as the terminal boundary and land on the last real char
;; (family).

;;; Code:

(require 'kao-selection)
(require 'kao-motion)
(require 'kao-state)
(require 'kao-info)

;;;; Low-level char access (mirrors a guarded `*iterator')

(defun kao-object--ch (pos)
  "Char at POS, or nil outside [`point-min', `point-max').
The sentence/paragraph/whitespace ports dereference buffer positions directly
\(Kakoune `*first' / `*last'); this stands in for that read, returning nil where
Kakoune would be at `buffer.begin()-1' or `buffer.end()' (always guarded before
use)."
  (and (>= pos (point-min)) (< pos (point-max)) (char-after pos)))

;;;; Character-category predicates (Kakoune is_word<Word> / is_word<WORD>)

(defun kao-object--word-cat-p (cat)
  "Non-nil when category CAT is an in-`Word' char (alnum + `_', ASCII)."
  (eq cat 'word))

(defun kao-object--WORD-cat-p (cat)
  "Non-nil when category CAT is an in-`WORD' char (any non-blank).
A `WORD' is `not is_blank' (unicode.hh): word OR punctuation, never a blank or
newline — so a WORD never spans a line break."
  (memq cat '(word punct)))

;;;; Word object selector (selectors.cc:145 select_word, ToBegin|ToEnd[|Inner])

(defun kao-object--word (sel inner word-pred to-begin to-end)
  "Object bounds for the word under SEL's cursor, or nil if not on a word char.
WORD-PRED maps a `kao-motion--category' to non-nil for an in-word char (`Word'
vs `WORD').  INNER non-nil selects the inner object; nil the whole object, which
additionally includes trailing horizontal blanks.  TO-BEGIN/TO-END are the
ObjectFlags: with both set this is the whole/inner `<a-a>'/`<a-i>' object; with
only one set it is the `['/`]' to-begin/to-end variant (a backward selection for
to-begin-only).  Faithful to selectors.cc:145-171 `select_word': the cursor must
sit on a word char, ToBegin walks `first' back to the word start
\(`skip_while_reverse' + `++first' guard), ToEnd walks `last' forward to the
word end, `!Inner' consuming trailing horizontal blanks, `--last' lands on the
last included char, and the unset endpoint stays at the cursor."
  (let ((cur (kao-sel-cursor sel))
        (eob (point-max)))
    (when (funcall word-pred (kao-motion--cat-at cur))
      (let ((first cur) (last cur))
        (when to-begin
          ;; ToBegin: walk back to the word start.  `kao-motion--skip-reverse'
          ;; mirrors `skip_while_reverse'; if it overshot onto a non-word char,
          ;; step forward one (the `if (not is_word(*first)) ++first' guard).
          (setq first (car (kao-motion--skip-reverse
                            cur (point-min)
                            (lambda (c) (funcall word-pred c)))))
          (unless (funcall word-pred (kao-motion--cat-at first))
            (setq first (1+ first))))
        (when to-end
          ;; ToEnd: walk forward past the word, then (unless Inner) trailing blanks.
          (while (and (< last eob) (funcall word-pred (kao-motion--cat-at last)))
            (setq last (1+ last)))
          (unless inner
            (while (and (< last eob) (eq (kao-motion--cat-at last) 'blank))
              (setq last (1+ last))))
          (setq last (1- last)))            ; --last
        ;; ToEnd ? utf8_range(first,last) : utf8_range(last,first).
        (if to-end
            (kao-sel-make :anchor first :cursor last)
          (kao-sel-make :anchor last :cursor first))))))

;;;; Whitespace object (selectors.cc:615 select_whitespaces, ToBegin|ToEnd[|Inner])

(defun kao-object--whitespace (sel inner to-begin to-end)
  "Whitespace-run object under SEL's cursor, or nil if the cursor is not on it.
Faithful to selectors.cc:615 `select_whitespaces' with ToBegin[|ToEnd][|Inner]:
whitespace is space or tab, plus newline unless INNER (so a whole whitespace
object can cross line breaks; an inner one cannot).  TO-BEGIN walks back to the
run start (`skip-chars-backward', = `skip_while_reverse' + the `++first' guard,
which it subsumes by stopping exactly at the boundary); TO-END walks forward to
the run end then steps back one (`--last'); the unset endpoint stays at the
cursor, and to-begin-only returns a backward selection."
  (let* ((set (if inner " \t" " \t\n"))
         (cur (kao-sel-cursor sel))
         (curc (kao-object--ch cur)))
    (when (or (eql curc ?\s) (eql curc ?\t) (and (not inner) (eql curc ?\n)))
      (save-excursion
        (let ((first cur) (last cur))
          (when to-begin
            (goto-char cur)
            (skip-chars-backward set)
            (setq first (point)))
          (when to-end
            (goto-char cur)
            (skip-chars-forward set)
            (setq last (1- (point))))
          (if to-end
              (kao-sel-make :anchor first :cursor last)
            (kao-sel-make :anchor last :cursor first)))))))

;;;; Number object (selectors.cc:445 select_number, ToBegin|ToEnd[|Inner])

(defun kao-object--number (sel inner to-begin to-end &optional _level)
  "Number object under SEL's cursor, or nil if not on a number char or `-'.
Faithful to selectors.cc:445-479 `select_number' with ToBegin[|ToEnd][|Inner]:
a number char is a digit, plus `.' unless INNER.  The cursor must sit on a
number char or a leading `-'.  TO-BEGIN walks `first' back over the digits
\(`skip_while_reverse' + the `++first' guard, a leading `-' kept); TO-END skips a
leading `-', walks `last' forward over the digits, then `--last'; the unset
endpoint stays at the cursor, and to-begin-only returns a backward selection.
_LEVEL is ignored exactly as the C++ selector ignores `count'."
  (let* ((bob (point-min)) (eob (point-max))
         (is-num (lambda (pos)
                   (let ((c (kao-object--ch pos)))
                     (and c (or (and (>= c ?0) (<= c ?9))
                                (and (not inner) (eql c ?.)))))))
         (cur (kao-sel-cursor sel)))
    (when (or (funcall is-num cur) (eql (kao-object--ch cur) ?-))
      (let ((first cur) (last cur))
        (when to-begin
          (while (and (/= first bob) (funcall is-num first)) (setq first (1- first)))
          ;; the `if (not is_number(*first) and *first != '-' and first+1 !=
          ;; buffer.end()) ++first' guard — step onto the number's first char
          ;; unless we landed on a kept leading `-' (or the buffer's last pos).
          (when (and (not (funcall is-num first))
                     (not (eql (kao-object--ch first) ?-))
                     (/= (1+ first) eob))
            (setq first (1+ first))))
        (when to-end
          (when (eql (kao-object--ch last) ?-) (setq last (1+ last)))
          (while (and (/= last eob) (funcall is-num last)) (setq last (1+ last)))
          (when (/= last bob) (setq last (1- last))))
        (if to-end
            (kao-sel-make :anchor first :cursor last)
          (kao-sel-make :anchor last :cursor first))))))

;;;; Indent object (selectors.cc:651 select_indent, ToBegin|ToEnd[|Inner])

(defun kao-object--indent-here (tabstop)
  "Indentation width of the line at point, Kakoune `get_indent' (selectors.cc:655).
A space adds 1; a tab advances to the next TABSTOP multiple
\(`(indent / tabstop + 1) * tabstop', kao's `kao--deindent-line' rounding); any
other char stops the scan.  The trailing newline never reaches the scan — only
\[bol, eol) is read — exactly as the C++ loop breaks on the first non-blank."
  (let ((indent 0) (pos (line-beginning-position)) (end (line-end-position)))
    (catch 'done
      (while (< pos end)
        (let ((c (char-after pos)))
          (cond
           ((eql c ?\s) (setq indent (1+ indent)))
           ((eql c ?\t) (setq indent (* (1+ (/ indent tabstop)) tabstop)))
           (t (throw 'done nil))))
        (setq pos (1+ pos))))
    indent))

(defun kao-object--blank-line-p ()
  "Non-nil if the line at point is empty, Kakoune `buffer[l] == \"\\n\"'.
A blank line has no content before its newline; a whitespace-only line is NOT
blank by this test (matching the C++ `get_current_indent'/begin-end walks)."
  (= (line-beginning-position) (line-end-position)))

(defun kao-object--only-ws-line-p ()
  "Non-nil if the line at point holds only spaces and tabs.
Kakoune `is_only_whitespaces' (selectors.cc:681): an empty line qualifies; any
non-blank char disqualifies it.  The newline always counts as whitespace and is
excluded from the [bol, eol) scan."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (eolp)))

(defun kao-object--indent-phantom-line-p ()
  "Non-nil when point is on the empty line Emacs counts after a final newline.
Kakoune's buffer always ends in a newline and exposes no such trailing line;
this guards the end walk so a buffer ending in `\\n' gains no spurious blank
line in the whole indent object."
  (and (eobp) (bolp) (> (point-max) (point-min))
       (eql (char-before (point-max)) ?\n)))

(defun kao-object--indent-ref (cur tabstop)
  "Reference indent of the block under CUR, Kakoune `get_current_indent'.
selectors.cc:666: the indent of the first non-blank line at or above CUR's line;
failing that, the first non-blank line below CUR's line; failing both, 0.
TABSTOP is the tab width."
  (save-excursion
    (goto-char cur)
    (beginning-of-line)
    (or
     ;; from CUR's line upward to the buffer start (the `for l = line; l >= 0' loop)
     (catch 'found
       (while t
         (unless (kao-object--blank-line-p)
           (throw 'found (kao-object--indent-here tabstop)))
         (when (bobp) (throw 'found nil))
         (forward-line -1)))
     ;; from the line below CUR downward to the last real line (`for l = line+1')
     (catch 'found
       (goto-char cur)
       (beginning-of-line)
       (while (and (zerop (forward-line 1))
                   (not (kao-object--indent-phantom-line-p)))
         (unless (kao-object--blank-line-p)
           (throw 'found (kao-object--indent-here tabstop))))
       nil)
     0)))

(defun kao-object--indent (sel inner to-begin to-end &optional _level)
  "Indentation-block object under SEL's cursor (Kakoune `select_indent').
Faithful to selectors.cc:651-726 with ToBegin[|ToEnd][|Inner]: the block is the
run of lines around the cursor that are blank or indented at least as far as the
reference indent (`kao-object--indent-ref'), measured with `tab-width' as the
tabstop.  TO-BEGIN walks the block start up over such lines, TO-END walks the
block end down; INNER then trims whitespace-only lines off both ends.  The whole
object runs from the start line's column 0 to the end line's last char (its
newline); a partial flag leaves the unset endpoint at the exact cursor, and
to-begin-only returns a backward selection.  Unlike the char objects this never
returns nil — every cursor sits on some line.  _LEVEL is ignored exactly as
`select_indent' ignores `count'."
  (let* ((tabstop tab-width)
         (cur (kao-sel-cursor sel))
         (ref (kao-object--indent-ref cur tabstop)))
    (save-excursion
      (goto-char cur)
      (beginning-of-line)
      ;; begin walk: extend the block start up while the line above is blank or
      ;; indented >= ref (the `--begin_line' loop then `++begin_line', :695-700).
      (when to-begin
        (while (and (not (bobp))
                    (save-excursion
                      (forward-line -1)
                      (or (kao-object--blank-line-p)
                          (>= (kao-object--indent-here tabstop) ref))))
          (forward-line -1)))
      (let ((begin-bol (point)))
        ;; end walk: extend the block end down while the next real line is blank
        ;; or indented >= ref (the `++end_line' loop then `--end_line', :702-708).
        (goto-char cur)
        (beginning-of-line)
        (when to-end
          (while (and (save-excursion
                        (and (zerop (forward-line 1))
                             (not (kao-object--indent-phantom-line-p))))
                      (save-excursion
                        (forward-line 1)
                        (or (kao-object--blank-line-p)
                            (>= (kao-object--indent-here tabstop) ref))))
            (forward-line 1)))
        (let ((end-bol (point)))
          ;; inner: trim whitespace-only lines off both ends while the block
          ;; still spans more than one line (:711-718).
          (when inner
            (while (and (< begin-bol end-bol)
                        (save-excursion (goto-char begin-bol)
                                        (kao-object--only-ws-line-p)))
              (setq begin-bol (save-excursion (goto-char begin-bol)
                                              (forward-line 1) (point))))
            (while (and (< begin-bol end-bol)
                        (save-excursion (goto-char end-bol)
                                        (kao-object--only-ws-line-p)))
              (setq end-bol (save-excursion (goto-char end-bol)
                                            (forward-line -1) (point)))))
          ;; first = to_begin ? begin_line:0 : cursor; last = to_end ?
          ;; end_line:(len-1) : cursor (:720-723).  The end line's last char is
          ;; its newline at `line-end-position'.
          (let ((first (if to-begin begin-bol cur))
                (last (if to-end
                          (save-excursion (goto-char end-bol) (line-end-position))
                        cur)))
            (if to-end
                (kao-sel-make :anchor first :cursor last)
              (kao-sel-make :anchor last :cursor first))))))))

;;;; Argument object (selectors.cc:728 select_argument, ToBegin|ToEnd[|Inner])

(defun kao-object--arg-class (c)
  "Classify char C for the argument scan (`select_argument''s `classify').
`( [ {' are Opening, `) ] }' Closing, `, ;' Delimiter; everything else —
including nil past the buffer ends — is nil (the C++ `None')."
  (cond
   ((memql c '(?\( ?\[ ?\{)) 'opening)
   ((memql c '(?\) ?\] ?\})) 'closing)
   ((memql c '(?\, ?\;))     'delimiter)
   (t nil)))

(defun kao-object--arg-blank-p (pos)
  "Non-nil if the char at POS is a Kakoune `is_blank' (the ASCII subset).
`is_blank' (unicode.hh:49) is the horizontal blanks plus the line terminators
\\n/\\r/\\v, so this matches space, tab, \\n, \\r, \\v and \\f — the
whole-argument trailing-blank swallow and the inner-edge trim therefore cross
line breaks exactly as the C++ does (the non-ASCII unicode blanks are out of
kao's scope, as in the other object ports)."
  (memql (kao-object--ch pos) '(?\s ?\t ?\n ?\r ?\v ?\f)))

(defun kao-object--argument (sel inner to-begin to-end &optional level)
  "Argument object under SEL's cursor, bracket-nesting aware; never returns nil.
Faithful to selectors.cc:728-816 `select_argument' with ToBegin[|ToEnd][|Inner]
and LEVEL — the count-th enclosing bracket level (`params.count - 1',
normal.cc:1435), so `2<a-a>u' selects one bracket level out.  This is the first
non-pair object to honour the count.  An Opening or Delimiter under the cursor
steps `pos' back one (the C++ initial switch; Closing is commented out there).
The backward scan walks `begin' to the argument start (a Closing raises the
level, an Opening at level 0 is the enclosing open -> `first-arg' and step in,
a Delimiter at level 0 is the previous boundary); the forward scan walks `end'
to the argument end symmetrically (a Closing at level 0 -> `last-arg', a
Delimiter at level 0 ends it, swallowing trailing blanks only for the first
whole argument).  INNER drops a non-last argument's closing delimiter then trims
blank edges; a non-inner non-first last argument instead takes its leading
delimiter.  Returns a backward selection when TO-BEGIN is set without TO-END;
otherwise the anchor is the LEVEL-adjusted begin (or the cursor when TO-BEGIN is
unset) and the cursor is end."
  (let* ((bob (point-min)) (eob (point-max))
         (level (or level 0))
         (cur (kao-sel-cursor sel))
         (pos cur)
         (begin nil) (end nil) (first-arg nil) (last-arg nil) (lev level))
    ;; initial pos adjust: an Opening/Delimiter under the cursor steps back one
    (when (and (memq (kao-object--arg-class (kao-object--ch pos))
                     '(opening delimiter))
               (/= pos bob))
      (setq pos (1- pos)))
    ;; backward scan for the argument begin
    (setq begin pos lev level)
    (catch 'done
      (while (/= begin bob)
        (let ((c (kao-object--arg-class (kao-object--ch begin))))
          (cond
           ((eq c 'closing) (setq lev (1+ lev)))
           ((eq c 'opening)
            (if (= lev 0)
                (progn (setq first-arg t begin (1+ begin)) (throw 'done nil))
              (setq lev (1- lev))))
           ((and (eq c 'delimiter) (= lev 0))
            (setq begin (1+ begin))
            (throw 'done nil)))
          (setq begin (1- begin)))))
    ;; forward scan for the argument end
    (setq end pos lev level)
    (catch 'done
      (while (/= end eob)
        (let ((c (kao-object--arg-class (kao-object--ch end))))
          (cond
           ((eq c 'opening) (setq lev (1+ lev)))
           ((and (/= end pos) (eq c 'closing))
            (if (= lev 0)
                (progn (setq last-arg t end (1- end)) (throw 'done nil))
              (setq lev (1- lev))))
           ((and (eq c 'delimiter) (= lev 0))
            ;; blanks after the delimiter join the FIRST whole argument only
            (when (and first-arg (not inner))
              (while (and (/= (1+ end) eob) (kao-object--arg-blank-p (1+ end)))
                (setq end (1+ end))))
            (throw 'done nil)))
          (setq end (1+ end)))))
    (if inner
        (progn
          (unless last-arg (setq end (1- end)))
          ;; skip_while(begin, end, is_blank): trim leading blanks
          (while (and (/= begin end) (kao-object--arg-blank-p begin))
            (setq begin (1+ begin)))
          ;; skip_while_reverse(end, begin, is_blank): trim trailing blanks
          (while (and (/= end begin) (kao-object--arg-blank-p end))
            (setq end (1- end))))
      ;; non-inner: a non-first last argument takes its leading delimiter
      (when (and (not first-arg) last-arg (/= begin bob))
        (setq begin (1- begin))))
    (when (= end eob) (setq end (1- end)))
    (if (and to-begin (not to-end))
        (kao-sel-make :anchor pos :cursor begin)
      (kao-sel-make :anchor (if to-begin begin pos) :cursor end))))

;;;; Sentence object (selectors.cc:482 select_sentence, ToBegin|ToEnd[|Inner])

(defconst kao-object--sentence-enders '(?. ?\; ?! ??)
  "Chars that terminate a sentence (Kakoune `is_end_of_sentence': . ; ! ?).")

(defun kao-object--sentence (sel inner to-begin to-end &optional level)
  "Sentence object under SEL's cursor (nil only in the degenerate empty buffer).
Faithful to selectors.cc:482 `select_sentence' with ToBegin[|ToEnd][|Inner].
TO-BEGIN walks `first' back to the sentence start
\(past the previous terminator or a blank-line boundary, then over leading
horizontal blanks); TO-END walks `last' forward to the next terminator /
blank-line, the whole object additionally consuming trailing horizontal blanks
\(INNER omits them).
When TO-END is unset a leading adjust first moves `first' onto the previous
terminator (so `['s grabs the current sentence), and the result is the backward
selection (anchor = cursor, cursor = sentence start).

LEVEL is Kakoune's `count' (`params.count - 1', normal.cc:1435): the walk runs
\(1+ (or LEVEL 0)) times (selectors.cc:493 `for i <= count'), `first'/`last'
carrying over between passes (`last' resets to `first' only on the first pass),
so a whole-object count (`<a-a>') spans LEVEL+1 sentences forward.

Emacs has no forced trailing newline (family): where Kakoune's end-walk
stops on the buffer's terminal `\\n', here it can reach `point-max'; the result
is clamped back to the last real char so the cursor stays on a char."
  (let* ((bob (point-min)) (eob (point-max))
         (level (or level 0))
         (eos-p (lambda (pos) (memql (kao-object--ch pos) kao-object--sentence-enders)))
         (hbl-p (lambda (pos) (let ((c (kao-object--ch pos)))
                                (or (eql c ?\s) (eql c ?\t)))))
         (eol-p (lambda (pos) (eql (kao-object--ch pos) ?\n)))
         (cur (kao-sel-cursor sel)))
    (when (< cur eob)                   ; empty buffer / cursor past eob -> no object
      (let ((first cur) last)
        (dotimes (i (1+ level))         ; `for (int i = 0; i <= count; ++i)'
          ;; Leading adjust (only `not ToEnd'): if the previous non-blank char is
          ;; a sentence terminator, move `first' onto it (skip_while_reverse).
          (when (and (not to-end) (/= first bob))
            (let ((pnb (1- first)))
              (while (and (/= pnb bob) (or (funcall hbl-p pnb) (funcall eol-p pnb)))
                (setq pnb (1- pnb)))
              (when (funcall eos-p pnb) (setq first pnb))))
          (when (= i 0) (setq last first)) ; i == 0
          (when to-begin
            ;; ToBegin: walk back to the sentence start.
            (let ((saw-non-blank nil))
              (catch 'done
                (while (/= first bob)
                  (let ((prev (1- first)))
                    (unless (funcall hbl-p first) (setq saw-non-blank t))
                    (cond
                     ((and (funcall eol-p prev) (funcall eol-p first) (/= (1+ first) eob))
                      (setq first (1+ first)) (throw 'done nil))
                     ((funcall eos-p prev)
                      (if saw-non-blank
                          (throw 'done nil)
                        ;; ToEnd: mark the previous sentence end.
                        (when to-end (setq last (1- first)))))))
                  (setq first (1- first))))
              ;; skip_while(first, end, horizontal_blank)
              (while (and (< first eob) (funcall hbl-p first)) (setq first (1+ first)))))
          (when to-end
            ;; ToEnd: walk forward to the sentence end.
            (catch 'done
              (while (/= last eob)
                (when (or (funcall eos-p last)
                          (and (funcall eol-p last)
                               (or (= (1+ last) eob) (funcall eol-p (1+ last)))))
                  (throw 'done nil))
                (setq last (1+ last))))
            (when (and (not inner) (/= last eob))
              (setq last (1+ last))
              (while (and (< last eob) (funcall hbl-p last)) (setq last (1+ last)))
              (setq last (1- last)))
            ;; family: a walk that reached eob lands on the last real char.
            (when (>= last eob) (setq last (1- eob)))))
        (when (< last first) (setq last first))
        (if to-end
            (kao-sel-make :anchor first :cursor last)
          (kao-sel-make :anchor last :cursor first))))))

;;;; Paragraph object (selectors.cc:555 select_paragraph, ToBegin|ToEnd[|Inner])

(defun kao-object--paragraph (sel inner to-begin to-end &optional level)
  "Paragraph object under SEL's cursor (nil only in the degenerate empty buffer).
Faithful to selectors.cc:555 `select_paragraph' with ToBegin[|ToEnd][|Inner].
Paragraphs are separated by blank lines (consecutive newlines).
TO-BEGIN walks `first' back to the paragraph start (the char after a blank line,
or buffer start); TO-END walks `last' forward to the blank line after it, the
whole object additionally consuming the trailing blank line(s), the INNER one
stopping at the first.  The leading adjust differs by flag: with TO-END a cursor
on a blank line steps into the next paragraph; without TO-END a cursor after
a blank line steps back onto it.  The unset endpoint stays at the cursor, so
to-begin-only returns a backward selection.

LEVEL is Kakoune's `count' (`params.count - 1', normal.cc:1435): the walk runs
\(1+ (or LEVEL 0)) times (selectors.cc:562 `for i <= count'), `first'/`last'
carrying over between passes (the top `last' reset is guarded to the first pass;
the ToBegin `last = first' fires every pass, as in the C++), so a whole-object
count (`<a-a>') spans LEVEL+1 paragraphs.

Emacs has no forced trailing newline (family): the last paragraph's
end-walk reaches `point-max', and the unconditional `--last' then lands on the
last real char."
  (let* ((bob (point-min)) (eob (point-max))
         (level (or level 0))
         (eol-p (lambda (pos) (eql (kao-object--ch pos) ?\n)))
         (cur (kao-sel-cursor sel)))
    (when (< cur eob)                   ; empty buffer / cursor past eob -> no object
      (let ((first cur) last)
        (dotimes (i (1+ level))         ; `for (int i = 0; i <= count; ++i)'
          ;; Leading adjust.
          (cond
           ;; `not ToEnd': step back onto a preceding blank line's newline.
           ((and (not to-end) (> first (1+ bob))
                 (funcall eol-p (1- first)) (/= (1- first) bob) (funcall eol-p (- first 2)))
            (setq first (1- first)))
           ;; ToEnd: a cursor on a blank line steps into the next paragraph.
           ((and to-end (/= first bob) (/= (1+ first) eob)
                 (funcall eol-p (1- first)) (funcall eol-p first))
            (setq first (1+ first))))
          (when (= i 0) (setq last first)) ; i == 0
          ;; ToBegin: back over blank lines, then to the paragraph start.
          (when (and to-begin (/= first bob))
            (while (and (/= first bob) (funcall eol-p first)) (setq first (1- first)))
            (when to-end (setq last first))
            (catch 'done
              (while (/= first bob)
                (when (and (funcall eol-p (1- first)) (funcall eol-p first))
                  (setq first (1+ first)) (throw 'done nil))
                (setq first (1- first)))))
          (when to-end
            ;; ToEnd: forward to the blank line after the paragraph.
            (when (and (/= last eob) (funcall eol-p last)) (setq last (1+ last)))
            (catch 'done
              (while (/= last eob)
                (when (and (/= last bob) (funcall eol-p last) (funcall eol-p (1- last)))
                  (unless inner
                    (while (and (< last eob) (funcall eol-p last)) (setq last (1+ last))))
                  (throw 'done nil))
                (setq last (1+ last))))
            (setq last (1- last))       ; unconditional --last (lands on a real char)
            (when (< last bob) (setq last bob))))
        (when (< last first) (setq last first))
        (if to-end
            (kao-sel-make :anchor first :cursor last)
          (kao-sel-make :anchor last :cursor first))))))

;;;; Surrounding-pair objects (selectors.cc:378 select_surrounding et al.)
;;
;; ONE regex engine: `find_opening' / `find_closing' /
;; `find_surrounding' (selectors.cc:278-376) are generic over REGEX
;; delimiters operating on match RANGES; the named pairs and quotes
;; (normal.cc:1532-1540) are single literal chars (`\Q(' …, normal.cc:1551)
;; reaching the same engine through `regexp-quote'.  Kakoune compiles the
;; delimiters with RegexCompileFlags::Backward and iterates them
;; right-to-left; Emacs has no backward regex engine, so kao collects the
;; window's matches by a FORWARD scan and iterates the list in reverse —
;; exact \"matches fully within the window\" semantics, differing from the
;; C++ only for OVERLAPPING matches of one delimiter regex (e.g. \"aa\" in
;; \"aaa\"), which real-world delimiters do not produce.  Documented
;; deviation, family.

(defun kao-object--rx-matches (regex beg end)
  "All matches of REGEX within [BEG, END) as (MBEG . MEND), in order.
Zero-width matches are KEPT (the scan steps forward one char so it always
terminates) — Kakoune's RegexIterator yields them too, which is why
`find_surrounding' carries the empty-match guards (selectors.cc:341)."
  (let* ((cf (kao--regex-case-fold regex))   ;: (?i)/(?I) strip + fold policy
         (regex (car cf))
         (case-fold-search (cdr cf))
         (matches '()))
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (and (< (point) end) (re-search-forward regex end t))
          (let ((mb (match-beginning 0)) (me (match-end 0)))
            (push (cons mb me) matches)
            (goto-char (if (> me mb) me (1+ me)))))))
    (nreverse matches)))

(defun kao-object--rx-find-opening (pos open close level nestable)
  "Port of `find_opening' (selectors.cc:278-308) for regex OPEN/CLOSE.
Search backward from POS for the OPEN match that encloses it, counting
nesting via CLOSE matches (when NESTABLE).  LEVEL is the starting nesting
offset (>0 skips that many enclosing pairs, for the parent re-search).
Return the OPEN match as (MBEG . MEND), or nil.

The C++ iterates the window's matches right-to-left: per candidate the
CLOSE matches in [candidate-end, window-hi) raise the level, a level-0
candidate wins, and window-hi narrows to the failed candidate's start."
  (let ((bob (point-min)))
    ;; \"When on the token of a non-nestable block, consider it opening\":
    ;; a CLOSE match ending exactly at POS steps POS back to its start
    ;; (guarded by NESTABLE, exactly as the C++ condition is).
    (when nestable
      (let ((last-close (car (last (kao-object--rx-matches close bob pos)))))
        (when (and last-close (= (cdr last-close) pos))
          (setq pos (car last-close)))))
    (let ((cands (nreverse (kao-object--rx-matches open bob pos)))
          (window-hi pos))            ; upper bound of the CLOSE-count window
      (catch 'found
        (dolist (m cands)
          (when nestable
            (setq level (+ level (length (kao-object--rx-matches
                                          close (cdr m) window-hi)))))
          (when (or (not nestable) (= level 0))
            (throw 'found m))
          (setq window-hi (car m)
                level (1- level)))
        nil))))

(defun kao-object--rx-find-closing (pos open close level nestable)
  "Port of `find_closing' (selectors.cc:310-332) for regex OPEN/CLOSE.
Search forward from POS for the CLOSE match that matches, counting nesting
via OPEN matches (when NESTABLE).  LEVEL is the starting nesting offset.
Return the CLOSE match as (MBEG . MEND), or nil.  Per candidate the OPEN
matches in [window-lo, candidate-start) raise the level; window-lo narrows
to the failed candidate's end."
  (let ((eob (point-max))
        (window-lo pos))
    (catch 'found
      (dolist (m (kao-object--rx-matches close pos eob))
        (when nestable
          (setq level (+ level (length (kao-object--rx-matches
                                        open window-lo (car m))))))
        (when (or (not nestable) (= level 0))
          (throw 'found m))
        (setq window-lo (cdr m)
              level (1- level)))
      nil)))

(defun kao-object--find-surrounding (cursor open close inner to-begin to-end level)
  "Find the OPEN/CLOSE pair around CURSOR (`find_surrounding', selectors.cc:334).
OPEN and CLOSE are REGEX strings; flags ToBegin[|ToEnd][|Inner].  Return
\(FIRST . LAST) positions, or nil.
INNER selects between the delimiter matches, else the span includes them.
LEVEL is the nesting offset for the enclosing-opening search.  TO-BEGIN
finds the enclosing open match (first = Inner ? its end : its start); when
TO-END is unset `last' stays at the cursor (a later backward selection).
TO-END finds the enclosing close match (last = (Inner ? its start : its
end) - 1, the `utf8::previous'); when TO-BEGIN is unset and an opening
match STARTS exactly at the cursor, the closing scan starts past it
\(Kakoune's `Skip opening match if pos lies on it', :361).  A zero-width
delimiter match counts one char wide where the C++ applies the empty-match
guards (:346/:361)."
  (let* ((nestable (not (string= open close)))
         (first cursor) (last cursor) (ok t))
    (if to-begin
        (let ((op (kao-object--rx-find-opening (1+ cursor) open close
                                               level nestable)))
          (if op
              (progn
                (setq first (if inner (cdr op) (car op)))
                ;; ToEnd: last = empty ? next(mend) : mend; level resets to 0.
                (when to-end
                  (setq last (if (= (car op) (cdr op)) (1+ (cdr op)) (cdr op))
                        level 0)))
            (setq ok nil)))
      ;; ToBegin unset: an opening match STARTING exactly at the cursor is
      ;; skipped (regex_search(pos, end) + res[0].first == pos, :358-361).
      ;; fold this direct OPEN scan like every other leaf (the one
      ;; re-search not routed through `kao-object--rx-matches').
      (let* ((cf (kao--regex-case-fold open))
             (open (car cf))
             (case-fold-search (cdr cf)))
        (save-excursion
          (save-match-data
            (goto-char cursor)
            (when (and (re-search-forward open nil t)
                       (= (match-beginning 0) cursor))
              (let ((me (match-end 0)))
                (setq last (if (= me cursor) (1+ me) me))))))))
    (when (and ok to-end)
      (let ((cl (kao-object--rx-find-closing last open close level nestable)))
        (if cl
            (setq last (1- (if inner (car cl) (cdr cl))))
          (setq ok nil))))
    (when (and ok (<= first last))
      (cons first last))))

(defun kao-object--surrounding (sel inner open close to-begin to-end
                                    &optional level)
  "Surrounding-pair object under SEL's cursor, or nil if no enclosing pair.
Port of selectors.cc:378 `select_surrounding' (flags ToBegin[|ToEnd][|Inner]):
select the OPEN/CLOSE pair enclosing the cursor (whole), its contents
\(INNER), or — for the `['/`]' variants — from the cursor to the enclosing open
\(TO-BEGIN only, a backward selection) or close (TO-END only).  OPEN and
CLOSE are REGEX strings (the named single-char pairs arrive `regexp-quote'd;
the `c' custom object passes the user's regexes).  LEVEL (default
0) is the count-th enclosing level, `select_object''s `params.count - 1'
\(normal.cc:1435) reaching `find_surrounding' unchanged.  If the changing
end didn't move (`<a-a>' on an exact pair), re-search one level out (parent
expansion, LEVEL+1)."
  (let* ((level (or level 0))
         (cursor (kao-sel-cursor sel))
         (res (kao-object--find-surrounding cursor open close inner to-begin to-end level)))
    ;; Parent re-search: re-expand only when each *changing* end equals the current
    ;; selection's min/max (Kakoune `select_surrounding' :388-392).
    (when (and res (not inner)
               (or (= (car res) (kao-sel-min sel)) (not to-begin))
               (or (= (cdr res) (kao-sel-max sel)) (not to-end)))
      (setq res (kao-object--find-surrounding cursor open close inner to-begin to-end (1+ level))))
    (when res
      ;; ToEnd ? utf8_range(first,last) : utf8_range(last,first).
      (if to-end
          (kao-sel-make :anchor (car res) :cursor (cdr res))
        (kao-sel-make :anchor (cdr res) :cursor (car res))))))

(defun kao-object-make-pair-selector (open close)
  "Return a SELECTOR for the OPEN/CLOSE pair, for `kao-object-register'.
The returned function has the object-selector signature
\(SEL INNER TO-BEGIN TO-END &optional LEVEL) and picks the OPEN..CLOSE pair
enclosing SEL's cursor — the whole pair, its contents (INNER), or a `['/`]'
partial variant — exactly like the built-in bracket and quote objects.
Register it under a key with `kao-object-register' to add a custom pair object.

OPEN and CLOSE are REGEX strings, matched as-is.  This constructor does NOT
`regexp-quote' them, so a literal delimiter must be escaped by the caller (a
literal `[' spelled `\\['): the same contract as the interactive `c' custom
object, which reads two regexes.  Use it for pairs whose delimiters ARE
regexes — for example the `line' object (`^[ \\t]*' … `[ \\t]*\\n') in the
README and docs/regex-porting.md recipes.

Only the flat `<a-i>'/`<a-a>'/`['/`]' selector is built; `<a-I>'/`<a-A>'
nested selection still needs a hand-written NESTED-SELECTOR (a nested-pair
constructor is out of scope)."
  (lambda (sel inner to-begin to-end &optional level)
    (kao-object--surrounding sel inner open close
                             to-begin to-end (or level 0))))

(defconst kao-object--pairs
  '((?\( ?\) ?b "parenthesis block")
    (?{  ?}  ?B "brace block")
    (?\[ ?\] ?r "bracket block")
    (?<  ?>  ?a "angle block")
    (?\" ?\" ?Q "double quote string")
    (?\' ?\' ?q "single quote string")
    (?\` ?\` ?g "grave quote string"))
  "The surrounding-pair objects (normal.cc:1532-1540) as (OPEN CLOSE NAME DOC).
Single source for both the dispatch entries (`kao-object--pair-entries') and
the autoinfo rows (`kao-object--pair-info'); DOC is the faithful Kakoune
docstring.  Brackets are nestable (open != close); quotes are non-nestable.")

(defun kao-object--pair-entries ()
  "Build the `kao--object-table' entries for the surrounding-pair objects.
Each pair in `kao-object--pairs' is reachable by its open char, close char,
or mnemonic name; all map to the same (SEL INNER) selector bound to that
pair's delimiters as `regexp-quote'd regexes (the `\\=\\Q' compile of
normal.cc:1551, reaching the ONE regex engine)."
  (let ((entries '()))
    (dolist (p kao-object--pairs)
      (let* ((open (nth 0 p)) (close (nth 1 p)) (name (nth 2 p))
             (open-rx (regexp-quote (char-to-string open)))
             (close-rx (regexp-quote (char-to-string close)))
             (fn (lambda (sel inner to-begin to-end &optional level)
                   (kao-object--surrounding sel inner open-rx close-rx
                                            to-begin to-end level))))
        (push (cons open fn) entries)
        (unless (= close open) (push (cons close fn) entries))
        (push (cons name fn) entries)))
    (nreverse entries)))

(defun kao-object--pair-info ()
  "Build the autoinfo rows (EVENT . DOCSTRING) for the surrounding pairs.
Mirrors `kao-object--pair-entries' key for key (open, close, name) from the
shared `kao-object--pairs', so a parity test keeps the box and the dispatch
table in lockstep."
  (let ((rows '()))
    (dolist (p kao-object--pairs)
      (let ((open (nth 0 p)) (close (nth 1 p)) (name (nth 2 p)) (doc (nth 3 p)))
        (push (cons open doc) rows)
        (unless (= close open) (push (cons close doc) rows))
        (push (cons name doc) rows)))
    (nreverse rows)))

;;;; Nested objects — <a-I>/<a-A> (ObjectFlags::Nested, selectors.cc:817-1030)
;;
;; Per current selection, select ALL objects of the chosen type within
;; [min, max+1) (`for_each_sel', selectors.cc:817-827).  Each walk returns a
;; list of (FIRST . LAST) inclusive spans, in buffer order; the dispatch
;; rebuilds the whole selection list from them (main = last) and
;; reports the faithful "nothing selected" when every selection yields none.

(defun kao-object--nested-words (beg end inner word-pred)
  "All word spans in [BEG, END) as (FIRST . LAST) conses, in order.
Port of `select_nested_words' (selectors.cc:830): skip non-word chars (hitting
END mid-skip selects nothing more), take the word run, and—unless INNER—the
trailing horizontal blanks.  WORD-PRED maps a `kao-motion--category' to
non-nil for an in-word char (`Word' vs `WORD'), as in `kao-object--word'."
  (let ((res '()) (pos beg))
    (while (< pos end)
      (while (and (< pos end)
                  (not (funcall word-pred (kao-motion--cat-at pos))))
        (setq pos (1+ pos)))
      (when (< pos end)
        (let ((start pos))
          (while (and (< pos end) (funcall word-pred (kao-motion--cat-at pos)))
            (setq pos (1+ pos)))
          (unless inner
            (while (and (< pos end) (eq (kao-motion--cat-at pos) 'blank))
              (setq pos (1+ pos))))
          (push (cons start (1- pos)) res))))
    (nreverse res)))

(defun kao-object--nested-sentences (beg end inner)
  "All sentence spans in [BEG, END) as (FIRST . LAST) conses, in order.
Port of `select_nested_sentences' (selectors.cc:874): skip leading
space/newline/tab (hitting END mid-skip selects nothing more), run to the
sentence ender (`kao-object--sentence-enders'); when one is found it is
included and—unless INNER—the trailing spaces too; a run reaching END
without an ender is still a sentence."
  (let ((res '()) (pos beg))
    (while (< pos end)
      (while (and (< pos end) (memq (kao-object--ch pos) '(?\s ?\n ?\t)))
        (setq pos (1+ pos)))
      (when (< pos end)
        (let ((start pos))
          (while (and (< pos end)
                      (not (memq (kao-object--ch pos)
                                 kao-object--sentence-enders)))
            (setq pos (1+ pos)))
          (when (< pos end)             ; the ender is included
            (setq pos (1+ pos))
            (unless inner
              (while (and (< pos end) (eql (kao-object--ch pos) ?\s))
                (setq pos (1+ pos)))))
          (push (cons start (1- pos)) res))))
    (nreverse res)))

(defun kao-object--nested-paragraphs (beg end inner)
  "All paragraph spans in [BEG, END) as (FIRST . LAST) conses, in order.
Port of `select_nested_paragraphs' (selectors.cc:896): skip leading newlines
to the paragraph start, then walk until a blank line — INNER stops AT the
second consecutive newline (the separator stays out), whole runs past the
blank run to just before the next paragraph's first char."
  (let ((res '()) (pos beg))
    (while (< pos end)
      (let ((start pos))
        (while (and (< start end) (eql (kao-object--ch start) ?\n))
          (setq start (1+ start)))
        (setq pos start)
        (let ((eols 0) (done nil))
          (while (and (< pos end) (not done))
            (cond ((eql (kao-object--ch pos) ?\n)
                   (setq eols (1+ eols))
                   (if (and (= eols 2) inner)
                       (setq done t)
                     (setq pos (1+ pos))))
                  ((>= eols 2) (setq done t))
                  (t (setq eols 0) (setq pos (1+ pos))))))
        (push (cons start (1- pos)) res)))
    (nreverse res)))

(defun kao-object--nested-whitespaces (beg end inner)
  "All whitespace-run spans in [BEG, END) as (FIRST . LAST) conses, in order.
Port of `select_nested_whitespaces' (selectors.cc:923): whitespace is space
or tab, plus newline unless INNER (as in `kao-object--whitespace'); skip
non-whitespace (hitting END mid-skip selects nothing more), take the run."
  (let ((ws-p (lambda (pos)
                (let ((c (kao-object--ch pos)))
                  (or (eql c ?\s) (eql c ?\t)
                      (and (not inner) (eql c ?\n))))))
        (res '()) (pos beg))
    (while (< pos end)
      (while (and (< pos end) (not (funcall ws-p pos)))
        (setq pos (1+ pos)))
      (when (< pos end)
        (let ((start pos))
          (while (and (< pos end) (funcall ws-p pos))
            (setq pos (1+ pos)))
          (push (cons start (1- pos)) res))))
    (nreverse res)))

(defun kao-object--nested-numbers (beg end inner &optional _level)
  "All number spans in [BEG, END) as (FIRST . LAST) conses, in order.
Port of `select_nested_numbers' (selectors.cc:851-870): a number is a digit
run, plus `.' unless INNER, with an optional leading `-'.  Skip non-(`-' or
number) chars; consume a run; emit it only when it holds at least one DIGIT (a
lone `-' or `.' is dropped); the `(= it start)' progress guard advances past a
stuck char so the walk always terminates.  _LEVEL is ignored (the C++ ignores
`count')."
  (let ((is-digit (lambda (pos)
                    (let ((c (kao-object--ch pos)))
                      (and c (>= c ?0) (<= c ?9)))))
        (is-num (lambda (pos)
                  (let ((c (kao-object--ch pos)))
                    (and c (or (and (>= c ?0) (<= c ?9))
                               (and (not inner) (eql c ?.)))))))
        (res '()) (pos beg))
    (while (< pos end)
      ;; skip chars that are neither a number nor a leading minus
      (while (and (< pos end)
                  (not (or (eql (kao-object--ch pos) ?-) (funcall is-num pos))))
        (setq pos (1+ pos)))
      (when (< pos end)
        (let ((start pos))
          (when (eql (kao-object--ch pos) ?-) (setq pos (1+ pos)))
          (while (and (< pos end) (funcall is-num pos)) (setq pos (1+ pos)))
          (cond
           ((= pos start) (setq pos (1+ pos))) ; nothing consumed: progress guard
           (t
            ;; emit only when the [start, pos) span holds at least one digit
            (let ((p start) (has-digit nil))
              (while (and (< p pos) (not has-digit))
                (when (funcall is-digit p) (setq has-digit t))
                (setq p (1+ p)))
              (when has-digit
                (push (cons start (1- pos)) res))))))))
    (nreverse res)))

(defun kao-object--nested-indents (_beg _end _inner &optional _level)
  "Signal that the nested indent object is unimplemented, Kakoune-faithfully.
Port of `select_nested_indents' (selectors.cc:939), which is a real
`ObjectType' nested slot that `throw'-s `runtime_error(\"nested indents are not
implemented\")' (normal.cc:1457 pairs `i' with it).  Keeping this throwing entry
in `kao--object-nested-table' makes `<a-I>i'/`<a-A>i' surface that error — not a
silent no-op — and keeps the nested table's key set exactly equal to
`kao--object-table' (the parity test needs no relaxation)."
  (user-error "kao: nested indents are not implemented"))

(defun kao-object--nested-arguments (beg end inner &optional _level)
  "All argument spans in [BEG, END) as (FIRST . LAST) conses, in order.
Port of `select_nested_arguments' (selectors.cc:944-976): walk the region
tracking bracket depth (Opening +1, Closing -1); at every top-level (depth 0)
`,'/`;' emit the span from the running `start' to the delimiter — INNER excludes
it (`char_prev'), the whole object includes it — then for INNER skip the
whitespace (space/tab/newline, the C++ local `is_whitespace') after it; a
trailing fragment after the last delimiter is emitted too.  Pushes are
UNGUARDED, so an empty inner argument yields a backward span exactly as the C++
pushes unconditionally.  _LEVEL is ignored (the C++ ignores `count')."
  (let ((is-ws (lambda (pos)
                 (memql (kao-object--ch pos) '(?\s ?\t ?\n))))
        (start beg) (level 0) (pos beg) (res '()))
    (while (< pos end)
      (let ((c (kao-object--ch pos)))
        (cond
         ((memql c '(?\( ?\[ ?\{)) (setq level (1+ level) pos (1+ pos)))
         ((memql c '(?\) ?\] ?\})) (setq level (1- level) pos (1+ pos)))
         ((and (memql c '(?\, ?\;)) (= level 0))
          (push (cons start (if inner (1- pos) pos)) res)
          (setq pos (1+ pos))
          (when inner
            (while (and (< pos end) (funcall is-ws pos)) (setq pos (1+ pos))))
          (setq start pos))
         (t (setq pos (1+ pos))))))
    (when (/= start pos)
      (push (cons start (1- pos)) res))
    (nreverse res)))

(defun kao-object--delim-ranges (beg end regex)
  "All matches of delimiter REGEX in [BEG, END) as (MBEG . MEND), ascending.
The nested walks' `RegexIterator' stream (selectors.cc:985-986/:1024);
thin alias for `kao-object--rx-matches'."
  (kao-object--rx-matches regex beg end))

(defun kao-object--nested-pairs (beg end inner open close level)
  "All OPEN/CLOSE pair spans at nesting depth LEVEL+1 in [BEG, END).
Port of the two-regex `regex_select_nested' (selectors.cc:978-1014), OPEN
and CLOSE regex strings: the opening and closing match streams are merged
in match-start order; the depth counter starts at -LEVEL-1 (LEVEL is
`select_object''s `params.count - 1', so no count selects depth 1), an
opening raising it to 0 starts a span (INNER: after the match), the
closing taking it back from 0 ends one (INNER: before the match; empty
inner spans are dropped), and a span still open when both streams end runs
to the region end.  Faithfully to the C++ loop, processing STOPS as soon as
either stream is exhausted — trailing delimiters of the other kind are
never seen, so an opening there cannot start a dangling span."
  (let ((opens (kao-object--delim-ranges beg end open))
        (closes (kao-object--delim-ranges beg end close))
        (lvl (- -1 level)) (start nil) (res '()))
    (while (and opens closes)
      (while (and opens (or (null closes) (< (caar opens) (caar closes))))
        (setq lvl (1+ lvl))
        (when (= lvl 0)
          (setq start (if inner (cdar opens) (caar opens))))
        (pop opens))
      (while (and closes (or (null opens) (< (caar closes) (caar opens))))
        (when (and (= lvl 0) start)
          ;; end = char_prev(inner ? close.first : close.second) (:1005)
          (let ((last (1- (if inner (caar closes) (cdar closes)))))
            (when (<= start last) (push (cons start last) res))
            (setq start nil)))
        (setq lvl (1- lvl))
        (pop closes)))
    (when start (push (cons start (1- end)) res))
    (nreverse res)))

(defun kao-object--nested-delims (beg end inner delim)
  "All spans between alternating DELIM matches in [BEG, END).
Port of the one-regex `regex_select_nested' (selectors.cc:1017-1041), the
open == close case (quotes), DELIM a regex string: matches alternate
start/end (INNER: inside the matches; empty inner spans dropped); an
unmatched start runs to the region end.  The count is ignored, as in
Kakoune."
  (let ((start nil) (res '()))
    (dolist (m (kao-object--delim-ranges beg end delim))
      (if (not start)
          (setq start (if inner (cdr m) (car m)))
        ;; end = char_prev(inner ? m.first : m.second) (:1031)
        (let ((last (1- (if inner (car m) (cdr m)))))
          (when (<= start last) (push (cons start last) res))
          (setq start nil))))
    (when start (push (cons start (1- end)) res))
    (nreverse res)))

(defun kao-object--nested-pair-entries ()
  "Build the `kao--object-nested-table' entries for the surrounding pairs.
Mirrors `kao-object--pair-entries' key for key from `kao-object--pairs';
each entry dispatches like Kakoune's surrounding-pair nested clause
\(normal.cc:1546-1551): nestable pairs (open != close) through
`kao-object--nested-pairs' (LEVEL honoured), quotes (open == close) through
`kao-object--nested-delims' (LEVEL ignored)."
  (let ((entries '()))
    (dolist (p kao-object--pairs)
      (let* ((open (nth 0 p)) (close (nth 1 p)) (name (nth 2 p))
             (open-rx (regexp-quote (char-to-string open)))
             (close-rx (regexp-quote (char-to-string close)))
             (fn (lambda (beg end inner level)
                   (if (= open close)
                       (kao-object--nested-delims beg end inner open-rx)
                     (kao-object--nested-pairs beg end inner open-rx close-rx
                                               level)))))
        (push (cons open fn) entries)
        (unless (= close open) (push (cons close fn) entries))
        (push (cons name fn) entries)))
    (nreverse entries)))

(defconst kao--object-nested-table
  (append
   (list (cons ?w    (lambda (beg end inner _level)
                       (kao-object--nested-words
                        beg end inner #'kao-object--word-cat-p)))
         (cons ?\M-w (lambda (beg end inner _level)
                       (kao-object--nested-words
                        beg end inner #'kao-object--WORD-cat-p)))
         (cons ?\s   (lambda (beg end inner _level)
                       (kao-object--nested-whitespaces beg end inner)))
         (cons ?s    (lambda (beg end inner _level)
                       (kao-object--nested-sentences beg end inner)))
         (cons ?p    (lambda (beg end inner _level)
                       (kao-object--nested-paragraphs beg end inner)))
         (cons ?n    #'kao-object--nested-numbers)
         (cons ?i    #'kao-object--nested-indents)
         (cons ?u    #'kao-object--nested-arguments))
   (kao-object--nested-pair-entries))
  "Object key (event) -> nested walk (BEG END INNER LEVEL) -> span list.
The `nested_func' column of Kakoune's `ObjectType' table plus the
surrounding-pair clause (normal.cc:1444-1551), over the same key set as
`kao--object-table' (lockstep-pinned by a parity test).  Kakoune's
`select_nested_indents' throws \"not implemented\"; kao keeps the matching `i'
entry as a throwing `kao-object--nested-indents' (faithful, and so the key set
stays exactly equal to `kao--object-table' — no parity relaxation).")

(defun kao-object--apply-nested (fn inner level)
  "Rebuild the selection list from FN's spans within every selection.
FN is a `kao--object-nested-table' walk called with each selection's
\[min, max+1) region, INNER, and LEVEL.  No span anywhere leaves the list
unchanged (Kakoune `for_each_sel' throws \"nothing selected\",
selectors.cc:825); otherwise the results replace the list — sorted, main =
last (the `SelectionList' ctor), the `kao--select-regex-apply' shape."
  (let ((result '()))
    (dolist (sel (kao-sels-list kao--sels))
      (dolist (span (funcall fn (kao-sel-min sel) (1+ (kao-sel-max sel))
                             inner level))
        (push (kao-sel-make :anchor (car span) :cursor (cdr span)) result)))
    (if (null result)
        (message "kao: nothing selected")
      (let ((lst (kao-sels-list
                  (kao-sels-sort (kao-sels-make :list (nreverse result)
                                                :main 0)))))
        ;; LST is already sorted; the -raw seam installs it verbatim (no merge)
        ;; and clamps each member (a no-op on the built list).
        (kao-set-selections-raw lst (1- (length lst)))))))

;;;; Object table + object-pending dispatch (normal.cc:1416)

(defconst kao--object-table
  (append
   (list (cons ?w    (lambda (sel inner to-begin to-end &optional _level)
                       (kao-object--word sel inner #'kao-object--word-cat-p
                                         to-begin to-end)))
         (cons ?\M-w (lambda (sel inner to-begin to-end &optional _level)
                       (kao-object--word sel inner #'kao-object--WORD-cat-p
                                         to-begin to-end)))
         (cons ?\s   (lambda (sel inner to-begin to-end &optional _level)
                       (kao-object--whitespace sel inner to-begin to-end)))
         (cons ?s    (lambda (sel inner to-begin to-end &optional level)
                       (kao-object--sentence sel inner to-begin to-end level)))
         (cons ?p    (lambda (sel inner to-begin to-end &optional level)
                       (kao-object--paragraph sel inner to-begin to-end level)))
         (cons ?n    #'kao-object--number)
         (cons ?i    #'kao-object--indent)
         (cons ?u    #'kao-object--argument))
   (kao-object--pair-entries))
  "Object key (event) -> selector (SEL INNER TO-BEGIN TO-END LEVEL) -> sel/nil.
Word/non-pair keys: `w' -> Word, `<a-w>' -> WORD, `<space>' -> whitespace,
`s' -> sentence, `p' -> paragraph, `n' -> number, `i' -> indent, `u' -> argument
\(selectors.cc:1452-1459); their selectors ignore LEVEL exactly as the C++
selectors ignore `count' — except `u' (`select_argument'), which honours
LEVEL as the count-th enclosing bracket level.  Surrounding
pairs (normal.cc:1532-1540) are appended by `kao-object--pair-entries' — each
reachable by open char, close char, or name: `()'/`b', `{}'/`B', `[]'/`r',
`<>'/`a', `\"'/`Q', `''/`q', `` ` ``/`g' — and honour LEVEL (the count-th
enclosing level).")

(defconst kao--object-info
  (append
   '((?w   . "word")
     (?\M-w . "WORD")
     (?\s  . "whitespaces")
     (?s   . "sentence")
     (?p   . "paragraph")
     (?n   . "number")
     (?i   . "indent")
     (?u   . "argument"))
   (kao-object--pair-info)
   '((?c   . "custom object desc")))
  "Autoinfo rows (EVENT . DOCSTRING) for the object-pending menu.
The faithful subset of Kakoune's `KeyInfo' built-ins (normal.cc:1565) that
kao implements; only the deferred `<a-;>' row is still omitted (`n' is the
number object, `i' the indent object, `u' the argument object,
selectors.cc:445/651/728).  Its
keys are kept in lockstep with `kao--object-table' by a parity test —
except `c' (normal.cc:1581), which both dispatchers handle OUTSIDE the
static table, exactly as the C++ `c' branch falls outside its ObjectType
array (normal.cc:1479).")

;;;; Runtime object registration (the elisp object-set extension)

(defvar kao--object-runtime-table nil
  "Runtime object selectors as an alist (CHAR . SELECTOR).
Consulted BEFORE the frozen `kao--object-table'; populated by
`kao-object-register'.  The built-in defconst stays untouched so the parity
tests keep their lockstep over the built-in subset only.")

(defvar kao--object-runtime-info nil
  "Autoinfo rows (CHAR . DOCSTRING) for runtime-registered objects.
Shown ahead of `kao--object-info' in the object-pending box.")

(defvar kao--object-runtime-nested nil
  "Nested walks (CHAR . NESTED-SELECTOR) for runtime-registered objects.
Consulted before `kao--object-nested-table' for `<a-I>'/`<a-A>'.")

(defun kao-object-register (char selector &optional info nested-selector)
  "Register an object on key CHAR for the object-pending menus.
SELECTOR is the object selector `(SEL INNER TO-BEGIN TO-END &optional LEVEL)'
returning a `kao-sel' or nil — the `kao--object-table' contract used by
`<a-i>'/`<a-a>'/`['/`]' and their extend/inner variants.  INFO is the autoinfo
docstring shown in the object box (omitted = no box row).  NESTED-SELECTOR,
when given, is the `<a-I>'/`<a-A>' nested walk `(BEG END INNER &optional LEVEL)'
returning a list of (FIRST . LAST) spans — the `kao--object-nested-table'
contract.

This is kao's elisp equivalent of Kakoune's object set (kao keeps the menu a
one-shot dispatch, not a remappable KeymapMode): a config or a
tree-sitter integration adds object keys without editing the core defconsts.
The three runtime alists are updated atomically; re-registering CHAR replaces
its entry (and drops a now-absent info/nested row).  A runtime key shadows a
built-in of the same CHAR.

The selectors run on the same machinery as the built-in `c' custom object, so
a NESTED-SELECTOR that fails to advance hangs exactly as Kakoune's own
same-position pair walk does (H-2) — kao adds no progress guard to a
user selector (faithful); the built-in walks all advance by construction."
  (unless (functionp selector)
    (user-error "kao-object-register: SELECTOR for %c must be a function (got %S)"
                char selector))
  (when (and nested-selector (not (functionp nested-selector)))
    (user-error "kao-object-register: NESTED-SELECTOR for %c must be a function (got %S)"
                char nested-selector))
  (setf (alist-get char kao--object-runtime-table) selector)
  (if info
      (setf (alist-get char kao--object-runtime-info) info)
    (setf (alist-get char kao--object-runtime-info nil 'remove) nil))
  (if nested-selector
      (setf (alist-get char kao--object-runtime-nested) nested-selector)
    (setf (alist-get char kao--object-runtime-nested nil 'remove) nil)))

(defun kao--object-selector (key)
  "Return the object selector bound to KEY, or nil.
A runtime registration wins over the built-in `kao--object-table' entry; nil
lets the caller's `c' branch handle the `c' custom object."
  (or (cdr (assq key kao--object-runtime-table))
      (cdr (assq key kao--object-table))))

(defun kao-object-bounds (key sel &optional inner level)
  "Return the text object KEY around SEL's cursor as a `kao-sel', or nil.
The public form of the `<a-a>'/`<a-i>' selector lookup.  KEY is the object
character or a key registered with `kao-object-register'.  With INNER non-nil
the inner object is selected, otherwise the whole object.  LEVEL (default 0) is
the count-th enclosing level.  Pure: it reads the buffer to locate the object
but never mutates the buffer or the selection list, so config code (surround and
similar) can query an object's span without moving the selection list."
  (let ((fn (kao--object-selector key)))
    (when fn (funcall fn sel inner t t (or level 0)))))

(defun kao--object-nested-selector (key)
  "Return the nested walk bound to KEY, or nil.
A runtime registration wins over the built-in `kao--object-nested-table'."
  (or (cdr (assq key kao--object-runtime-nested))
      (cdr (assq key kao--object-nested-table))))

(defun kao--object-info-rows ()
  "Return the merged autoinfo rows for the object-pending box.
Runtime registrations come first (overriding a built-in of the same key), then
the remaining built-in `kao--object-info' rows."
  (append kao--object-runtime-info
          (seq-remove (lambda (row) (assq (car row) kao--object-runtime-info))
                      kao--object-info)))

;;;; Custom object — the `c' desc prompt (normal.cc:1479-1516)

(defun kao-object--parse-desc (desc)
  "Parse DESC \"<open>,<close>\" into a regex-string pair (OPEN . CLOSE).
Ports the `c' prompt callback (normal.cc:1491-1500): DESC is split on
unescaped commas — a backslash escapes the NEXT character during the scan
\(Kakoune's comma split with backslash escape) — then only backslash-comma
is unescaped to a comma (the Kakoune unescape, string_utils.cc:60-72; a
doubled backslash stays a literal backslash pair).  Anything but exactly
two non-empty parts signals Kakoune's exact \"desc parsing failed\" error.
Each part must compile as an EMACS regex (engine boundary; the C++
Regex ctor throws inside the prompt callback the same way)."
  (let ((parts '()) (cur "") (i 0) (n (length desc)))
    (while (< i n)
      (let ((ch (aref desc i)))
        (cond ((and (eq ch ?\\) (< (1+ i) n))    ; escape eats the next char
               (setq cur (concat cur (substring desc i (+ i 2)))
                     i (+ i 2)))
              ((eq ch ?,)                        ; unescaped comma: split
               (push cur parts)
               (setq cur "" i (1+ i)))
              (t (setq cur (concat cur (string ch))
                       i (1+ i))))))
    (push cur parts)
    (setq parts (mapcar (lambda (p)              ; unescape \, ->, only
                          (replace-regexp-in-string "\\\\," "," p t t))
                        (nreverse parts)))
    (unless (and (= (length parts) 2)
                 (not (string-empty-p (nth 0 parts)))
                 (not (string-empty-p (nth 1 parts))))
      (user-error "desc parsing failed, expected <open>,<close>"))
    (dolist (p parts)
      (condition-case err
          (string-match-p p "")
        (invalid-regexp
         (user-error "invalid regexp '%s': %s" p (cadr err)))))
    (cons (nth 0 parts) (nth 1 parts))))

(defun kao-object--read-desc ()
  "Read and parse the custom-object desc (the faithful \"object desc:\" prompt).
The minibuffer read is drivable through the real event loop (macro replay
feeds it; recording captures the minibuffer keystrokes through the global
`pre-command-hook' recorder — no extra hook needed).  Read under the
kao prompt map (`C-r' register insert)."
  (kao-object--parse-desc
   (minibuffer-with-setup-hook #'kao--prompt-setup
     (read-string "object desc:"))))

(defun kao-object--title (inner to-begin to-end extend)
  "The object-pending menu title (Kakoune `get_title', normal.cc:1418-1427).
With both TO-BEGIN and TO-END this is the whole (or INNER)
`<a-a>'/`<a-i>' object;
with one only it is the `to ... begin'/`to ... end' variant.  EXTEND selects the
`extend' verb."
  (let ((whole (and to-begin to-end)))
    (format "%s %s%ssurrounding object%s"
            (if extend "extend" "select")
            (if whole "" "to ")
            (if inner "inner " "")
            (if whole "" (if to-begin " begin" " end")))))

(defvar kao--object-dispatch-context-functions nil
  "Abnormal hook wrapping an object-pending dispatch pass in a shared context.
Each element is a one-argument function `(THUNK) -> value' that MUST call and
return `(funcall THUNK)', optionally establishing dynamic state around it (for
example a command-scoped query memo).  `kao--object-run' nests the wrappers
around the recorded `<a-.>' thunk, so one context spans the whole multi-cursor
pass and a later `<a-.>' repeat rather than one context per selection.  Empty
\(the default) makes `kao--object-run' the identity, so non-treesit dispatch is
byte-identical and pays nothing.  Internal — not a public
symbol; kao-treesit registers `kao-treesit--object-dispatch-memo' here.")

(defun kao--object-run (thunk)
  "Run THUNK inside every wrapper on `kao--object-dispatch-context-functions'.
Folds the wrappers (each `(THUNK) -> value') around THUNK, the first hook
element outermost, and returns THUNK's value.  Identity when the hook is empty."
  (let ((run thunk))
    (dolist (w (reverse kao--object-dispatch-context-functions))
      (let ((inner run) (wrapper w))
        (setq run (lambda () (funcall wrapper inner)))))
    (funcall run)))

(defun kao--object-punctuation-key-p (key)
  "Non-nil when KEY is an arbitrary-punctuation single-delimiter object key.
Faithful to Kakoune `is_punctuation(cp, {})' (normal.cc:1554,
keys.asciidoc:719-721): KEY is a character that is neither a word char (alnum
ONLY — `kao-extra-word-chars' is deliberately NOT consulted, so `_' qualifies)
nor a blank (space, tab, newline, return, vertical tab).  Any such char is a
valid object whose open and close delimiters are the char itself."
  (and (characterp key)
       (not (string-match-p "[[:alnum:]]" (char-to-string key)))
       (not (memq key '(?\s ?\t ?\n ?\r ?\v)))))

(defun kao--object-punct-fallthrough (key nested)
  "Selector for KEY as its own single-delimiter punctuation object, or nil.
Faithful to the C++ ObjectType array's `is_punctuation' fall-through
\(normal.cc:1554): a non-word, non-blank KEY (see
`kao--object-punctuation-key-p') is a valid object whose open and close
delimiters are the char itself, `regexp-quote'd.  Registered pairs already
won above, so this is the last resort.  NESTED nil returns the flat pair
selector from `kao-object-make-pair-selector' (the `<a-i>'/`<a-a>'/`['/`]'
dispatch); NESTED non-nil returns the open==close `kao-object--nested-delims'
walk (the `<a-I>'/`<a-A>' dispatch, mirroring the `c' single-delimiter case)."
  (when (kao--object-punctuation-key-p key)
    (let ((rx (regexp-quote (char-to-string key))))
      (if nested
          (lambda (beg end inner _level)
            (kao-object--nested-delims beg end inner rx))
        (kao-object-make-pair-selector rx rx)))))

(defun kao--object-dispatch (inner to-begin to-end extend prompt)
  "Read the object key once and select that object under every cursor.
INNER selects the inner object; TO-BEGIN/TO-END are the ObjectFlags (both set =
`<a-a>'/`<a-i>' whole/inner; one set = the `['/`]' begin/end variant); EXTEND
merges onto each selection instead of replacing (the `{'/`}' family, Kakoune
SelectMode::Extend).  PROMPT echoes the pending key.  The object-pending step is
a one-shot `read-key', faithful to Kakoune's `on_next_key_with_autoinfo'; an
unknown key (or escape) cancels with no change.  The per-selection selector runs
through `kao--map-filter-selections' (with EXTEND), so a cursor off the object
drops that selection.  A pending count selects the count-th enclosing
pair level (`select_object''s `count = params.count - 1', normal.cc:1435;
captured before the read, like the count in the view menu); the non-pair
selectors ignore it."
  (let* ((level (max 0 (1- kao--count)))
         (key (kao-info--with-box
                  (kao-object--title inner to-begin to-end extend)
                  (kao--object-info-rows)
                (kao--read-key prompt)))
         ;; runtime registrations + the static table first, then `c' — the C++
         ;; falls through its ObjectType array to the `c' branch (normal.cc:1479)
         (sel-fn (or (kao--object-selector key)
                     (when (eql key ?c)
                       (let* ((desc (kao-object--read-desc))
                              (open (car desc)) (close (cdr desc)))
                         (lambda (sel inner to-begin to-end &optional level)
                           (kao-object--surrounding sel inner open close
                                                    to-begin to-end level))))
                     ;; Fall through to any punctuation char as its own
                     ;; single-delimiter object (open == close, non-nestable).
                     (kao--object-punct-fallthrough key nil))))
    (when sel-fn
      ;; Record the selection so `<a-.>' can repeat it (Kakoune
      ;; `select_and_set_last', normal.cc:144): the recorded thunk IS the live
      ;; selection (count + the parsed `c' desc included — the C++ lambdas
      ;; capture them), so the repeat is exact and never re-prompts.  The pass
      ;; runs through `kao--object-run', so a registered context (the treesit
      ;; captures memo) wraps the whole multi-cursor map once — both
      ;; now and on a later `<a-.>' repeat.
      (let ((pass (lambda ()
                    (kao--map-filter-selections
                     (lambda (sel) (funcall sel-fn sel inner to-begin to-end level))
                     extend))))
        (setq kao--last-select (lambda () (kao--object-run pass)))
        (funcall kao--last-select)))))

(defun kao-select-inner ()
  "Select the inner text object under each selection's cursor (`<a-i>')."
  (interactive)
  (kao--assert-mode)
  (kao--object-dispatch t t t nil "<a-i>"))

(defun kao-select-whole ()
  "Select the whole text object under each cursor plus trailing blanks (`<a-a>')."
  (interactive)
  (kao--assert-mode)
  (kao--object-dispatch nil t t nil "<a-a>"))

(defun kao-object-to-begin ()
  "Select to the whole surrounding object start (`[')."
  (interactive)
  (kao--assert-mode)
  (kao--object-dispatch nil t nil nil "["))

(defun kao-object-to-end ()
  "Select to the whole surrounding object end (`]')."
  (interactive)
  (kao--assert-mode)
  (kao--object-dispatch nil nil t nil "]"))

(defun kao-object-inner-to-begin ()
  "Select to the surrounding inner object start (`<a-[>')."
  (interactive)
  (kao--assert-mode)
  (kao--object-dispatch t t nil nil "<a-[>"))

(defun kao-object-inner-to-end ()
  "Select to the surrounding inner object end (`<a-]>')."
  (interactive)
  (kao--assert-mode)
  (kao--object-dispatch t nil t nil "<a-]>"))

(defun kao-object-extend-to-begin ()
  "Extend selections to the whole surrounding object start (`{')."
  (interactive)
  (kao--assert-mode)
  (kao--object-dispatch nil t nil t "{"))

(defun kao-object-extend-to-end ()
  "Extend selections to the whole surrounding object end (`}')."
  (interactive)
  (kao--assert-mode)
  (kao--object-dispatch nil nil t t "}"))

(defun kao-object-extend-inner-to-begin ()
  "Extend selections to the surrounding inner object start (`<a-{>')."
  (interactive)
  (kao--assert-mode)
  (kao--object-dispatch t t nil t "<a-{>"))

(defun kao-object-extend-inner-to-end ()
  "Extend selections to the surrounding inner object end (`<a-}>')."
  (interactive)
  (kao--assert-mode)
  (kao--object-dispatch t nil t t "<a-}>"))

(defun kao--object-dispatch-nested (inner prompt)
  "Read the object key once and select every nested object (Nested).
INNER selects the inner objects (`<a-I>'), nil the whole ones (`<a-A>');
PROMPT echoes the pending key.  A pending count picks the pair nesting
depth (`select_object''s `count = params.count - 1' feeding the
`regex_select_nested' level); the non-pair walks ignore it.  The thunk is
recorded for `<a-.>' (Kakoune `select_nested_and_set_last',
normal.cc:1436-1444)."
  (let* ((level (max 0 (1- kao--count)))
         (key (kao-info--with-box
                  (if inner "select inner nested object"
                    "select nested object")
                  (kao--object-info-rows)
                (kao--read-key prompt)))
         ;; runtime registrations + the static nested table first, then `c':
         ;; open == close -> the one-regex alternating walk, else the
         ;; two-regex merged-stream walk (normal.cc:1505-1509)
         (fn (or (kao--object-nested-selector key)
                 (when (eql key ?c)
                    (let* ((desc (kao-object--read-desc))
                           (open (car desc)) (close (cdr desc)))
                      (if (string= open close)
                          (lambda (beg end inner _level)
                            (kao-object--nested-delims beg end inner open))
                        (lambda (beg end inner level)
                          (kao-object--nested-pairs beg end inner open close
                                                    level)))))
                 ;; Punctuation fall-through: the open == close nested walk,
                 ;; mirroring the `c' single-delimiter case.
                 (kao--object-punct-fallthrough key t))))
    (when fn
      ;; Same dispatch-context wrap as `kao--object-dispatch': the
      ;; nested pass and its `<a-.>' repeat run inside one shared context.
      (let ((pass (lambda () (kao-object--apply-nested fn inner level))))
        (setq kao--last-select (lambda () (kao--object-run pass)))
        (funcall kao--last-select)))))

(defun kao-select-nested-inner ()
  "Select every inner nested object within each selection (`<a-I>')."
  (interactive)
  (kao--assert-mode)
  (kao--object-dispatch-nested t "<a-I>"))

(defun kao-select-nested-whole ()
  "Select every whole nested object within each selection (`<a-A>')."
  (interactive)
  (kao--assert-mode)
  (kao--object-dispatch-nested nil "<a-A>"))

(provide 'kao-object)
;;; kao-object.el ends here
