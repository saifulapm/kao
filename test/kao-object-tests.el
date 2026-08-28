;;; kao-object-tests.el --- Tests for kao-object -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for kao text objects.  The pure word/WORD selector is checked
;; against Kakoune's selectors.cc `select_word' semantics (l.145): inner vs
;; whole (trailing-blank) bounds, the cursor-must-be-on-a-word-char rule, Word
;; vs WORD categories, and the bob/eob/newline edges.  Multi-selection dispatch
;; (Task 2/3) is tested via `kao--map-filter-selections' since the read-key
;; object-pending step is not batch-drivable.

;;; Code:

(require 'ert)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-motion)
(require 'kao-state)
(require 'kao-object)
(require 'kao-keys)                    ; default bindings

(defun kao-object-tests--word (content cursor inner word-pred)
  "In CONTENT, return (kao-object--word) for a cursor at CURSOR.
INNER and WORD-PRED are passed through.  CURSOR is 1-based (buffer position)."
  (with-temp-buffer
    (insert content)
    (kao-object--word (kao-sel-make :anchor cursor :cursor cursor)
                      inner word-pred t t)))

;;;; Word object — bounds.  "foo bar baz": f1 o2 o3 _4 b5 a6 r7 _8 b9 a10 z11

(ert-deftest kao-object-word-inner-mid-word ()
  "`<a-i>w' from inside a word selects the whole inner word, no trailing blank."
  (let ((s (kao-object-tests--word "foo bar baz" 6 t #'kao-object--word-cat-p)))
    (should (= (kao-sel-anchor s) 5))             ; b
    (should (= (kao-sel-cursor s) 7))))           ; r  -> "bar"

(ert-deftest kao-object-word-inner-at-word-start ()
  "Inner word seeded at the word's first char."
  (let ((s (kao-object-tests--word "foo bar baz" 5 t #'kao-object--word-cat-p)))
    (should (= (kao-sel-anchor s) 5))
    (should (= (kao-sel-cursor s) 7))))

(ert-deftest kao-object-word-inner-at-word-end ()
  "Inner word seeded at the word's last char (skip_while_reverse to the start)."
  (let ((s (kao-object-tests--word "foo bar baz" 7 t #'kao-object--word-cat-p)))
    (should (= (kao-sel-anchor s) 5))
    (should (= (kao-sel-cursor s) 7))))

(ert-deftest kao-object-word-whole-includes-trailing-blank ()
  "`<a-a>w' (whole) extends past the word over trailing horizontal blanks."
  (let ((s (kao-object-tests--word "foo bar baz" 6 nil #'kao-object--word-cat-p)))
    (should (= (kao-sel-anchor s) 5))
    (should (= (kao-sel-cursor s) 8))))           ; "bar " incl. the space

(ert-deftest kao-object-word-on-blank-returns-nil ()
  "A cursor not on a word char yields no object (nullopt -> drop)."
  (should (null (kao-object-tests--word "foo bar baz" 4 t #'kao-object--word-cat-p)))
  (should (null (kao-object-tests--word "foo bar baz" 8 nil #'kao-object--word-cat-p))))

(ert-deftest kao-object-word-at-bob ()
  "A word touching beginning-of-buffer keeps its first char (no over-step)."
  (let ((s (kao-object-tests--word "foo bar baz" 1 t #'kao-object--word-cat-p)))
    (should (= (kao-sel-anchor s) 1))
    (should (= (kao-sel-cursor s) 3))))           ; "foo"

(ert-deftest kao-object-word-empty-and-eob-nil ()
  "Empty buffer or a cursor at/after the last char (no word char) -> nil."
  (should (null (kao-object-tests--word "" 1 t #'kao-object--word-cat-p)))
  ;; "foo " : cursor on the trailing space (4) has no word char.
  (should (null (kao-object-tests--word "foo " 4 t #'kao-object--word-cat-p))))

;;;; WORD object.  "foo-bar baz": f1 o2 o3 -4 b5 a6 r7 _8 b9 a10 z11

(ert-deftest kao-object-WORD-spans-punctuation ()
  "`WORD' (not-blank) treats `foo-bar' as a single object; `Word' stops at `-'."
  ;; Word stops at the punctuation.
  (let ((w (kao-object-tests--word "foo-bar baz" 2 t #'kao-object--word-cat-p)))
    (should (= (kao-sel-anchor w) 1))
    (should (= (kao-sel-cursor w) 3)))            ; "foo"
  ;; WORD spans it.
  (let ((bw (kao-object-tests--word "foo-bar baz" 2 t #'kao-object--WORD-cat-p)))
    (should (= (kao-sel-anchor bw) 1))
    (should (= (kao-sel-cursor bw) 7))))          ; "foo-bar"

(ert-deftest kao-object-WORD-whole-trailing-blank ()
  "Whole WORD includes the trailing space."
  (let ((bw (kao-object-tests--word "foo-bar baz" 2 nil #'kao-object--WORD-cat-p)))
    (should (= (kao-sel-anchor bw) 1))
    (should (= (kao-sel-cursor bw) 8))))          ; "foo-bar "

(ert-deftest kao-object-WORD-does-not-cross-newline ()
  "A WORD is `not is_blank' and so stops at the newline (never spans lines)."
  ;; "ab\ncd": a1 b2 \n3 c4 d5
  (let ((bw (kao-object-tests--word "ab\ncd" 1 t #'kao-object--WORD-cat-p)))
    (should (= (kao-sel-anchor bw) 1))
    (should (= (kao-sel-cursor bw) 2))))          ; "ab" only

;;;; Whitespace object (selectors.cc:615).  "foo  bar": f1 o2 o3 _4 _5 b6 a7 r8

(defun kao-object-tests--ws (content cursor inner)
  "In CONTENT, return (kao-object--whitespace) for a cursor at CURSOR (1-based)."
  (with-temp-buffer
    (insert content)
    (kao-object--whitespace (kao-sel-make :anchor cursor :cursor cursor) inner t t)))

(ert-deftest kao-object-ws-inner-run ()
  "`<a-i><space>' from inside a blank run selects the whole space/tab run."
  (let ((s (kao-object-tests--ws "foo  bar" 4 t)))
    (should (= (kao-sel-anchor s) 4))
    (should (= (kao-sel-cursor s) 5)))             ; "  "
  ;; Seeded from the run's second char — walks back to its start.
  (let ((s (kao-object-tests--ws "foo  bar" 5 t)))
    (should (= (kao-sel-min s) 4))
    (should (= (kao-sel-max s) 5))))

(ert-deftest kao-object-ws-on-non-blank-nil ()
  "A cursor not on whitespace yields no object (nullopt -> drop)."
  (should (null (kao-object-tests--ws "foo  bar" 1 t)))
  (should (null (kao-object-tests--ws "foo  bar" 6 nil))))

(ert-deftest kao-object-ws-tab ()
  "A tab is whitespace too."
  (let ((s (kao-object-tests--ws "a\tb" 2 t)))
    (should (= (kao-sel-anchor s) 2))
    (should (= (kao-sel-cursor s) 2))))            ; the lone tab

(ert-deftest kao-object-ws-whole-crosses-newline-inner-does-not ()
  "Whole whitespace spans a newline; inner stops at it.  \"a \\n b\": a1 _2 \\n3 b4."
  (let ((whole (kao-object-tests--ws "a \nb" 2 nil)))
    (should (= (kao-sel-anchor whole) 2))
    (should (= (kao-sel-cursor whole) 3)))         ; " \n"
  (let ((inner (kao-object-tests--ws "a \nb" 2 t)))
    (should (= (kao-sel-anchor inner) 2))
    (should (= (kao-sel-cursor inner) 2))))        ; just the space

(ert-deftest kao-object-ws-run-at-eob ()
  "A trailing whitespace run ends on the last real char (no over-step)."
  ;; "a  ": a1 _2 _3
  (let ((s (kao-object-tests--ws "a  " 2 t)))
    (should (= (kao-sel-anchor s) 2))
    (should (= (kao-sel-cursor s) 3))))            ; "  "

;;;; Sentence object (selectors.cc:482).  "One. Two.": O1 n2 e3 .4 _5 T6 w7 o8 .9

(defun kao-object-tests--sentence (content cursor inner &optional level)
  "In CONTENT, return (kao-object--sentence) for a cursor at CURSOR (1-based).
LEVEL is the count-1 (`<a-a>'/`<a-i>' loop runs LEVEL+1 passes)."
  (with-temp-buffer
    (insert content)
    (kao-object--sentence (kao-sel-make :anchor cursor :cursor cursor) inner t t level)))

(ert-deftest kao-object-sentence-whole-mid ()
  "`<a-a>s' from inside the second sentence selects it incl. its terminator."
  (let ((s (kao-object-tests--sentence "One. Two." 7 nil)))
    (should (= (kao-sel-anchor s) 6))
    (should (= (kao-sel-cursor s) 9))))            ; "Two."

(ert-deftest kao-object-sentence-whole-trailing-blank ()
  "A non-final sentence's whole object includes the trailing space."
  (let ((s (kao-object-tests--sentence "One. Two." 2 nil)))
    (should (= (kao-sel-anchor s) 1))
    (should (= (kao-sel-cursor s) 5))))            ; "One. " incl. the space

(ert-deftest kao-object-sentence-inner-drops-trailing-blank ()
  "`<a-i>s' stops at the terminator (no trailing space)."
  (let ((s (kao-object-tests--sentence "One. Two." 2 t)))
    (should (= (kao-sel-anchor s) 1))
    (should (= (kao-sel-cursor s) 4))))            ; "One."

(ert-deftest kao-object-sentence-eob-no-terminator ()
  "At a buffer with no trailing newline/terminator, the sentence ends on the
last real char (family).  \"One. Two\": no final period."
  (let ((s (kao-object-tests--sentence "One. Two" 7 nil)))
    (should (= (kao-sel-anchor s) 6))
    (should (= (kao-sel-cursor s) 8))))            ; "Two", cursor on the last char

(ert-deftest kao-object-sentence-empty-buffer-nil ()
  "An empty buffer (cursor at eob, no char) yields no object."
  (should (null (kao-object-tests--sentence "" 1 t))))

(ert-deftest kao-object-sentence-multi ()
  "Two cursors, one per sentence, select both sentences simultaneously."
  (let ((sels (kao-object-tests--dispatch
               "One. Two." '(2 7)
               (lambda (s) (kao-object--sentence s nil t t)))))
    (should (equal (mapcar #'kao-sel-min sels) '(1 6)))
    (should (equal (mapcar #'kao-sel-max sels) '(5 9)))))

;;;; Paragraph object (selectors.cc:555).
;;;; "para1\n\npara2": p1 a2 r3 a4 1·5 \n6 \n7 p8 a9 r10 a11 2·12

(defun kao-object-tests--paragraph (content cursor inner &optional level)
  "In CONTENT, return (kao-object--paragraph) for a cursor at CURSOR (1-based).
LEVEL is the count-1 (`<a-a>'/`<a-i>' loop runs LEVEL+1 passes)."
  (with-temp-buffer
    (insert content)
    (kao-object--paragraph (kao-sel-make :anchor cursor :cursor cursor) inner t t level)))

(ert-deftest kao-object-paragraph-whole-includes-blank-line ()
  "`<a-a>p' selects the paragraph plus its trailing blank line."
  (let ((s (kao-object-tests--paragraph "para1\n\npara2" 3 nil)))
    (should (= (kao-sel-anchor s) 1))
    (should (= (kao-sel-cursor s) 7))))            ; "para1\n\n"

(ert-deftest kao-object-paragraph-inner-stops-at-blank-line ()
  "`<a-i>p' stops at the first blank line (keeps the paragraph's own newline)."
  (let ((s (kao-object-tests--paragraph "para1\n\npara2" 3 t)))
    (should (= (kao-sel-anchor s) 1))
    (should (= (kao-sel-cursor s) 6))))            ; "para1\n"

(ert-deftest kao-object-paragraph-last-at-eob ()
  "The final paragraph (no trailing blank line) ends on the last real char."
  (let ((s (kao-object-tests--paragraph "para1\n\npara2" 10 nil)))
    (should (= (kao-sel-anchor s) 8))
    (should (= (kao-sel-cursor s) 12))))           ; "para2"

(ert-deftest kao-object-paragraph-on-blank-line-takes-next ()
  "A cursor on the blank line between paragraphs selects the following one."
  (let ((s (kao-object-tests--paragraph "para1\n\npara2" 7 nil)))
    (should (= (kao-sel-anchor s) 8))
    (should (= (kao-sel-cursor s) 12))))           ; steps into "para2"

(ert-deftest kao-object-paragraph-empty-buffer-nil ()
  "An empty buffer yields no paragraph object."
  (should (null (kao-object-tests--paragraph "" 1 t))))

(ert-deftest kao-object-paragraph-multi ()
  "Two cursors, one per paragraph, select both paragraphs (touching, not merged)."
  (let ((sels (kao-object-tests--dispatch
               "para1\n\npara2" '(3 10)
               (lambda (s) (kao-object--paragraph s nil t t)))))
    (should (equal (mapcar #'kao-sel-min sels) '(1 8)))
    (should (equal (mapcar #'kao-sel-max sels) '(7 12)))))

;;;; Counted s/p objects (selectors.cc:493/562 `for i <= count'; the
;;;; count is `params.count - 1', normal.cc:1435, threaded as LEVEL).  The walk
;;;; runs LEVEL+1 passes; `first'/`last' carry over.  WHOLE objects (`<a-a>')
;;;; span LEVEL+1 objects forward.  INNER objects (`<a-i>') are a FAITHFUL
;;;; no-op: `last' rests on the sentence terminator / first paragraph
;;;; separator, so the next pass's ToEnd walk breaks immediately (no `++last'
;;;; past the terminator in the inner branch, selectors.cc:542)..
;;;; "One. Two. Three.": O1 n2 e3 .4 _5 T6 w7 o8 .9 _10 T11 h12 r13 e14 e15 .16

(ert-deftest kao-object-sentence-count-whole-spans ()
  "`2<a-a>s' spans two sentences, `3<a-a>s' three (LEVEL = count-1)."
  (let ((s2 (kao-object-tests--sentence "One. Two. Three." 1 nil 1)))
    (should (= (kao-sel-anchor s2) 1))
    (should (= (kao-sel-cursor s2) 10)))           ; "One. Two. "
  (let ((s3 (kao-object-tests--sentence "One. Two. Three." 1 nil 2)))
    (should (= (kao-sel-anchor s3) 1))
    (should (= (kao-sel-cursor s3) 16))))          ; "One. Two. Three."

(ert-deftest kao-object-sentence-count-whole-from-mid ()
  "`2<a-a>s' from inside the 2nd sentence spans it and the 3rd."
  (let ((s (kao-object-tests--sentence "One. Two. Three." 6 nil 1)))
    (should (= (kao-sel-anchor s) 6))
    (should (= (kao-sel-cursor s) 16))))           ; "Two. Three."

(ert-deftest kao-object-sentence-count-inner-no-op ()
  "`2<a-i>s'/`3<a-i>s' == `<a-i>s' — INNER `last' rests on the terminator, so
the next pass breaks immediately (faithful to selectors.cc:542; no `++last')."
  (let ((s1 (kao-object-tests--sentence "One. Two. Three." 1 t 0))
        (s2 (kao-object-tests--sentence "One. Two. Three." 1 t 1))
        (s3 (kao-object-tests--sentence "One. Two. Three." 1 t 2)))
    (should (= (kao-sel-cursor s1) 4))             ; "One."
    (should (equal s2 s1))
    (should (equal s3 s1))))

(ert-deftest kao-object-sentence-count-default-unchanged ()
  "Explicit LEVEL 0 is byte-identical to the count-1 default (regression guard)."
  (should (equal (kao-object-tests--sentence "One. Two." 2 nil 0)
                 (kao-object-tests--sentence "One. Two." 2 nil)))
  (should (equal (kao-object-tests--sentence "One. Two." 2 t 0)
                 (kao-object-tests--sentence "One. Two." 2 t))))

(ert-deftest kao-object-paragraph-count-whole-spans ()
  "`2<a-a>p' spans two paragraphs, `3<a-a>p' three.  \"a\\n\\nb\\n\\nc\":
a1 \\n2 \\n3 b4 \\n5 \\n6 c7."
  (let ((p2 (kao-object-tests--paragraph "a\n\nb\n\nc" 1 nil 1)))
    (should (= (kao-sel-anchor p2) 1))
    (should (= (kao-sel-cursor p2) 6)))            ; "a\n\nb\n\n"
  (let ((p3 (kao-object-tests--paragraph "a\n\nb\n\nc" 1 nil 2)))
    (should (= (kao-sel-anchor p3) 1))
    (should (= (kao-sel-cursor p3) 7))))           ; "a\n\nb\n\nc" (last on real char)

(ert-deftest kao-object-paragraph-count-whole-two-paras ()
  "`2<a-a>p' from inside para1 spans para1 and para2 (`para1\\n\\npara2')."
  (let ((p (kao-object-tests--paragraph "para1\n\npara2" 3 nil 1)))
    (should (= (kao-sel-anchor p) 1))
    (should (= (kao-sel-cursor p) 12))))           ; whole "para1\n\npara2"

(ert-deftest kao-object-paragraph-count-tobegin-last-reset ()
  "The ToBegin `last = first' (selectors.cc:579) fires EVERY pass, not just the
first.  From a cursor ON the blank line (pos 3 in \"a\\n\\nb\\n\\nc\") the leading
adjust steps into para \"b\"; with count>1 the per-pass reset keeps the whole
object pinned to \"b\\n\\n\" [4,6] instead of running on into \"c\".  Guarding
that line to the first pass would diverge to cursor 7 — this pin bites it."
  (let ((p1 (kao-object-tests--paragraph "a\n\nb\n\nc" 3 nil 1))
        (p2 (kao-object-tests--paragraph "a\n\nb\n\nc" 3 nil 2)))
    (should (= (kao-sel-anchor p1) 4))
    (should (= (kao-sel-cursor p1) 6))             ; "b\n\n", NOT extended to "c"
    (should (equal p2 p1))))                       ; stable across the count

(ert-deftest kao-object-paragraph-count-inner-no-op ()
  "`2<a-i>p' == `<a-i>p' — INNER `last' rests on the first blank-line separator,
trapping the next pass (faithful)."
  (let ((p1 (kao-object-tests--paragraph "a\n\nb\n\nc" 1 t 0))
        (p2 (kao-object-tests--paragraph "a\n\nb\n\nc" 1 t 1)))
    (should (= (kao-sel-cursor p1) 2))             ; "a\n"
    (should (equal p2 p1))))

(ert-deftest kao-object-paragraph-count-default-unchanged ()
  "Explicit LEVEL 0 is byte-identical to the count-1 default (regression guard)."
  (should (equal (kao-object-tests--paragraph "para1\n\npara2" 3 nil 0)
                 (kao-object-tests--paragraph "para1\n\npara2" 3 nil)))
  (should (equal (kao-object-tests--paragraph "para1\n\npara2" 3 t 0)
                 (kao-object-tests--paragraph "para1\n\npara2" 3 t))))

(ert-deftest kao-object-nested-sp-count-agnostic ()
  "Nested `s'/`p' ignore the count: `select_nested_*' do not loop and the table
lambdas drop LEVEL (selectors.cc:874/896), so the split is identical for any
count."
  (with-temp-buffer
    (insert "Hi. There")                           ; 9 chars; region [1,10)
    (let ((sfn (cdr (assq ?s kao--object-nested-table)))
          (pfn (cdr (assq ?p kao--object-nested-table))))
      (should (equal (funcall sfn 1 10 t 0) (funcall sfn 1 10 t 5)))
      (should (equal (funcall pfn 1 10 nil 0) (funcall pfn 1 10 nil 5))))))

;;;; Surrounding bracket pairs (selectors.cc:378).
;;;; "foo(bar)baz": f1 o2 o3 (4 b5 a6 r7 )8 b9 a10 z11

(defun kao-object-tests--pair (content cursor inner open close)
  "In CONTENT, return (kao-object--surrounding) for a cursor at CURSOR (1-based).
OPEN/CLOSE are chars, regexp-quoted here (the engine takes regex strings)."
  (with-temp-buffer
    (insert content)
    (kao-object--surrounding (kao-sel-make :anchor cursor :cursor cursor)
                             inner
                             (regexp-quote (char-to-string open))
                             (regexp-quote (char-to-string close))
                             t t)))

(ert-deftest kao-object-pair-inner-and-whole ()
  "`<a-i>(' selects the contents; `<a-a>(' the whole incl. the brackets."
  (let ((i (kao-object-tests--pair "foo(bar)baz" 6 t ?\( ?\))))
    (should (= (kao-sel-anchor i) 5))
    (should (= (kao-sel-cursor i) 7)))             ; "bar"
  (let ((w (kao-object-tests--pair "foo(bar)baz" 6 nil ?\( ?\))))
    (should (= (kao-sel-anchor w) 4))
    (should (= (kao-sel-cursor w) 8))))            ; "(bar)"

(ert-deftest kao-object-pair-cursor-on-delimiter ()
  "A cursor on the opening or the closing delimiter selects the whole pair."
  (let ((on-open (kao-object-tests--pair "foo(bar)baz" 4 nil ?\( ?\))))
    (should (= (kao-sel-anchor on-open) 4))
    (should (= (kao-sel-cursor on-open) 8)))
  (let ((on-close (kao-object-tests--pair "foo(bar)baz" 8 nil ?\( ?\))))
    (should (= (kao-sel-anchor on-close) 4))
    (should (= (kao-sel-cursor on-close) 8))))

(ert-deftest kao-object-pair-nesting-innermost ()
  "Nestable pairs pick the innermost enclosing pair.  \"(a(b)c)\"."
  ;; ( 1 a2 ( 3 b4 ) 5 c6 ) 7
  (let ((i (kao-object-tests--pair "(a(b)c)" 4 t ?\( ?\))))
    (should (= (kao-sel-anchor i) 4))
    (should (= (kao-sel-cursor i) 4)))             ; "b"
  (let ((w (kao-object-tests--pair "(a(b)c)" 4 nil ?\( ?\))))
    (should (= (kao-sel-anchor w) 3))
    (should (= (kao-sel-cursor w) 5))))            ; "(b)"

(ert-deftest kao-object-pair-outer-from-between ()
  "From between a nested pair's close and the outer close, pick the outer pair."
  ;; "(a(b)c)" cursor=6 (the `c') is enclosed only by the outer ().
  (let ((w (kao-object-tests--pair "(a(b)c)" 6 nil ?\( ?\))))
    (should (= (kao-sel-anchor w) 1))
    (should (= (kao-sel-cursor w) 7))))            ; "(a(b)c)"

(ert-deftest kao-object-pair-not-enclosed-nil ()
  "A cursor with no enclosing pair yields nil (dropped)."
  (should (null (kao-object-tests--pair "foo(bar)baz" 2 t ?\( ?\)))))

(ert-deftest kao-object-pair-braces-brackets-angles ()
  "The other nestable pairs work the same: {} [] <>."
  (should (equal (let ((s (kao-object-tests--pair "x{ab}y" 3 t ?{ ?})))
                   (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                 '(3 . 4)))                        ; "ab"
  (should (equal (let ((s (kao-object-tests--pair "x[ab]y" 3 nil ?\[ ?\])))
                   (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                 '(2 . 5)))                        ; "[ab]"
  (should (equal (let ((s (kao-object-tests--pair "x<ab>y" 3 t ?< ?>)))
                   (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                 '(3 . 4))))                       ; "ab"

(ert-deftest kao-object-pair-by-name-key ()
  "The mnemonic name key `b' resolves to the same selector as `(' / `)'."
  (let ((by-name (cdr (assq ?b kao--object-table)))
        (by-open (cdr (assq ?\( kao--object-table)))
        (sel (kao-sel-make :anchor 6 :cursor 6)))
    (with-temp-buffer
      (insert "foo(bar)baz")
      (let ((a (funcall by-name sel nil t t))
            (b (funcall by-open sel nil t t)))
        (should (= (kao-sel-anchor a) (kao-sel-anchor b)))
        (should (= (kao-sel-cursor a) (kao-sel-cursor b)))
        (should (= (kao-sel-anchor a) 4))
        (should (= (kao-sel-cursor a) 8))))))

(ert-deftest kao-object-pair-multi ()
  "`<a-i>(' over N cursors selects each enclosing pair's contents.  \"(x)(y)\"."
  ;; ( 1 x2 ) 3 ( 4 y5 ) 6
  (let ((sels (kao-object-tests--dispatch
               "(x)(y)" '(2 5)
               (lambda (s) (kao-object--surrounding s t "(" ")" t t)))))
    (should (equal (mapcar #'kao-sel-min sels) '(2 5)))
    (should (equal (mapcar #'kao-sel-max sels) '(2 5)))))

;;;; Quote pairs (non-nestable, open==close).
;;;; "say \"hi\" now": s1 a2 y3 _4 "5 h6 i7 "8 _9 n10 o11 w12

(ert-deftest kao-object-quote-inner-and-whole ()
  "`<a-i>\"' selects between the quotes; `<a-a>\"' includes them."
  (let ((i (kao-object-tests--pair "say \"hi\" now" 6 t ?\" ?\")))
    (should (= (kao-sel-anchor i) 6))
    (should (= (kao-sel-cursor i) 7)))             ; "hi"
  (let ((w (kao-object-tests--pair "say \"hi\" now" 6 nil ?\" ?\")))
    (should (= (kao-sel-anchor w) 5))
    (should (= (kao-sel-cursor w) 8))))            ; the quoted span incl. quotes

(ert-deftest kao-object-quote-on-opening-delimiter ()
  "A cursor on the opening quote selects to the next quote (non-nestable scan)."
  (let ((w (kao-object-tests--pair "say \"hi\" now" 5 nil ?\" ?\")))
    (should (= (kao-sel-anchor w) 5))
    (should (= (kao-sel-cursor w) 8))))

(ert-deftest kao-object-quote-single-and-backtick ()
  "The other quote pairs work the same: `'' and `` ` ''."
  (should (equal (let ((s (kao-object-tests--pair "a'bc'd" 3 t ?\' ?\')))
                   (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                 '(3 . 4)))                        ; "bc"
  (should (equal (let ((s (kao-object-tests--pair "a`bc`d" 3 nil ?\` ?\`)))
                   (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                 '(2 . 5))))                       ; "`bc`"

(ert-deftest kao-object-quote-multi ()
  "`<a-i>\"' over N cursors selects each quoted region.  \"x\" \"y\"."
  ;; "1 x2 "3 _4 "5 y6 "7
  (let ((sels (kao-object-tests--dispatch
               "\"x\" \"y\"" '(2 6)
               (lambda (s) (kao-object--surrounding s t "\"" "\"" t t)))))
    (should (equal (mapcar #'kao-sel-min sels) '(2 6)))
    (should (equal (mapcar #'kao-sel-max sels) '(2 6)))))

;;;; Parent expansion (select_surrounding "ends didn't move" branch, level+1)

(ert-deftest kao-object-pair-parent-expansion ()
  "`<a-a>(' on a selection that already equals a pair expands to the enclosing one.
\"(a(b)c)\": the inner whole \"(b)\" (3..5) expands to the outer \"(a(b)c)\" (1..7)."
  (let ((s (with-temp-buffer
             (insert "(a(b)c)")
             (kao-object--surrounding (kao-sel-make :anchor 3 :cursor 5)
                                      nil "(" ")" t t))))
    (should (= (kao-sel-anchor s) 1))
    (should (= (kao-sel-cursor s) 7))))

(ert-deftest kao-object-pair-parent-expansion-no-grandparent-nil ()
  "Parent expansion on the outermost pair (no enclosing pair) drops the selection."
  (should (null (with-temp-buffer
                  (insert "(a(b)c)")
                  (kao-object--surrounding (kao-sel-make :anchor 1 :cursor 7)
                                           nil "(" ")" t t)))))

;;;; Multi-selection dispatch.  read-key is not batch-drivable, so drive the
;;;; dispatched transform — what kao--object-dispatch runs once the key is read.

(defun kao-object-tests--dispatch (content cursors selector)
  "Enable kao-mode in CONTENT, seed selections at CURSORS, run SELECTOR via filter.
SELECTOR is a (SEL) -> sel/nil function (already bound to inner/whole); returns
the resulting selection list (the `kao--map-filter-selections' dispatch path)."
  (with-temp-buffer
    (insert content)
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels
                (kao-sels-make
                 :list (mapcar (lambda (c) (kao-sel-make :anchor c :cursor c)) cursors)
                 :main 0))
          (kao--map-filter-selections selector)
          (kao-sels-list kao--sels))
      (kao-mode -1))))

(ert-deftest kao-object-ws-multi-drops-and-merges ()
  "Multi `<a-i><space>': non-blank cursors drop; same-run cursors merge.
\"a  b  c\": a1 _2 _3 b4 _5 _6 c7."
  ;; Two cursors in distinct runs, one on a letter (dropped).
  (let ((sels (kao-object-tests--dispatch
               "a  b  c" '(2 4 5)
               (lambda (s) (kao-object--whitespace s t t t)))))
    (should (equal (mapcar #'kao-sel-min sels) '(2 5)))   ; b@4 dropped
    (should (equal (mapcar #'kao-sel-max sels) '(3 6))))
  ;; Two cursors in the SAME run merge to one.
  (let ((sels (kao-object-tests--dispatch
               "a  b  c" '(2 3)
               (lambda (s) (kao-object--whitespace s t t t)))))
    (should (= 1 (length sels)))
    (should (= (kao-sel-min (car sels)) 2))
    (should (= (kao-sel-max (car sels)) 3))))

(defun kao-object-tests--select (content cursors inner word-pred)
  "Enable kao-mode in CONTENT, seed selections at CURSORS, run the word object.
Apply `kao-object--word' to every selection through `kao--map-filter-selections'
(the dispatch path) and return the resulting selection list."
  (with-temp-buffer
    (insert content)
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels
                (kao-sels-make
                 :list (mapcar (lambda (c) (kao-sel-make :anchor c :cursor c))
                               cursors)
                 :main 0))
          (kao--map-filter-selections
           (lambda (s) (kao-object--word s inner word-pred t t)))
          (kao-sels-list kao--sels))
      (kao-mode -1))))

(ert-deftest kao-object-multi-inner-word ()
  "`<a-i>w' over N cursors selects each inner word simultaneously."
  ;; "foo bar baz": foo 1-3, bar 5-7, baz 9-11.
  (let ((sels (kao-object-tests--select "foo bar baz" '(2 6 10) t
                                        #'kao-object--word-cat-p)))
    (should (equal (mapcar #'kao-sel-min sels) '(1 5 9)))
    (should (equal (mapcar #'kao-sel-cursor sels) '(3 7 11)))))

(ert-deftest kao-object-multi-drops-cursor-not-on-word ()
  "A cursor not on a word char is dropped from the result (select())."
  (let ((sels (kao-object-tests--select "foo bar baz" '(2 4 10) t
                                        #'kao-object--word-cat-p)))
    (should (equal (mapcar #'kao-sel-min sels) '(1 9))))) ; space@4 dropped

(ert-deftest kao-object-multi-whole-word-trailing-blank ()
  "`<a-a>w' over N cursors includes each word's trailing blank."
  (let ((sels (kao-object-tests--select "foo bar baz" '(2 6) nil
                                        #'kao-object--word-cat-p)))
    (should (equal (mapcar #'kao-sel-anchor sels) '(1 5)))
    (should (equal (mapcar #'kao-sel-cursor sels) '(4 8))))) ; "foo " / "bar "

(ert-deftest kao-object-multi-same-word-merges ()
  "Two cursors in the same word both select it, then collapse to one selection.
Kakoune `select' ends with sort_and_merge_overlapping (normal.cc:131)."
  ;; cursors at 5 and 6 are both inside "bar" (5-7).
  (let ((sels (kao-object-tests--select "foo bar baz" '(5 6) t
                                        #'kao-object--word-cat-p)))
    (should (= 1 (length sels)))
    (should (= (kao-sel-min (car sels)) 5))
    (should (= (kao-sel-cursor (car sels)) 7))))   ; single "bar"

(ert-deftest kao-object-info-parity ()
  "`kao--object-info' lists exactly the keys the menu dispatches.
That is the `kao--object-table' keys plus `c' (normal.cc:1581), which both
dispatchers handle OUTSIDE the static table exactly as the C++ `c' branch
falls outside its ObjectType array (normal.cc:1479) — asserted explicitly
so a dropped `c' row is caught."
  (should (equal (sort (mapcar #'car kao--object-info) #'<)
                 (sort (cons ?c (mapcar #'car kao--object-table)) #'<)))
  (should (assq ?c kao--object-info))
  (should-not (assq ?c kao--object-table))
  (should-not (assq ?c kao--object-nested-table)))

;;;; Partial-flag object variants — `[' `]' `<a-[>' `<a-]>' (Replace).
;;;; ToBegin-only is a BACKWARD selection (anchor = cursor, cursor = object start);
;;;; ToEnd-only is forward (anchor = cursor, cursor = object end).  selectors.cc:
;;;; `(flags & ToEnd) ? utf8_range(first,last) : utf8_range(last,first)'.

(defun kao-object-tests--word/f (content cursor inner to-begin to-end)
  "Word selector in CONTENT at CURSOR (1-based) with explicit flags."
  (with-temp-buffer
    (insert content)
    (kao-object--word (kao-sel-make :anchor cursor :cursor cursor)
                      inner #'kao-object--word-cat-p to-begin to-end)))

(ert-deftest kao-object-word-to-end-whole ()
  "`]w' from mid-word selects forward to the word end + trailing blank (anchor kept).
\"foo bar baz\": cursor 6 (`a' in bar)."
  (let ((s (kao-object-tests--word/f "foo bar baz" 6 nil nil t)))
    (should (= (kao-sel-anchor s) 6))             ; anchor stays at the cursor
    (should (= (kao-sel-cursor s) 8))             ; "ar " (incl. trailing space)
    (should (kao-sel-forward-p s))))

(ert-deftest kao-object-word-to-end-inner ()
  "`<a-]>w' selects forward to the word end, no trailing blank (inner)."
  (let ((s (kao-object-tests--word/f "foo bar baz" 6 t nil t)))
    (should (= (kao-sel-anchor s) 6))
    (should (= (kao-sel-cursor s) 7))))           ; "ar"

(ert-deftest kao-object-word-to-begin-backward ()
  "`[w' from mid-word selects BACKWARD to the word start (anchor = cursor).
\"foo bar baz\": cursor 6 (`a') -> [5,6] with cursor on the start `b' (5)."
  (let ((s (kao-object-tests--word/f "foo bar baz" 6 nil t nil)))
    (should (= (kao-sel-anchor s) 6))             ; anchor at the cursor
    (should (= (kao-sel-cursor s) 5))             ; cursor walks to the word start
    (should (not (kao-sel-forward-p s)))
    (should (= (kao-sel-min s) 5))
    (should (= (kao-sel-max s) 6))))

(defun kao-object-tests--ws/f (content cursor inner to-begin to-end)
  "Whitespace selector in CONTENT at CURSOR with explicit flags."
  (with-temp-buffer
    (insert content)
    (kao-object--whitespace (kao-sel-make :anchor cursor :cursor cursor)
                            inner to-begin to-end)))

(ert-deftest kao-object-ws-to-end-and-to-begin ()
  "`]'/`[' on a blank run.  \"foo  bar\": run 4-5."
  ;; ]<space> from 4: forward to the run end.
  (let ((s (kao-object-tests--ws/f "foo  bar" 4 nil nil t)))
    (should (= (kao-sel-anchor s) 4))
    (should (= (kao-sel-cursor s) 5)))
  ;; [<space> from 5: backward to the run start.
  (let ((s (kao-object-tests--ws/f "foo  bar" 5 nil t nil)))
    (should (= (kao-sel-anchor s) 5))             ; anchor at the cursor
    (should (= (kao-sel-cursor s) 4))             ; cursor at the run start
    (should (= (kao-sel-min s) 4))
    (should (= (kao-sel-max s) 5))))

(defun kao-object-tests--sentence/f (content cursor inner to-begin to-end)
  "Sentence selector in CONTENT at CURSOR with explicit flags."
  (with-temp-buffer
    (insert content)
    (kao-object--sentence (kao-sel-make :anchor cursor :cursor cursor)
                          inner to-begin to-end)))

(ert-deftest kao-object-sentence-to-end ()
  "`]s' from inside a sentence selects forward to its terminator.
\"One. Two.\": cursor 7 (`w') -> [7,9] \"wo.\"."
  (let ((s (kao-object-tests--sentence/f "One. Two." 7 nil nil t)))
    (should (= (kao-sel-anchor s) 7))
    (should (= (kao-sel-cursor s) 9))))

(ert-deftest kao-object-sentence-to-begin-mid ()
  "`[s' from inside a sentence selects BACKWARD to its start.
\"One. Two.\": cursor 7 (`w') -> start `T' (6), cursor on 6."
  (let ((s (kao-object-tests--sentence/f "One. Two." 7 nil t nil)))
    (should (= (kao-sel-anchor s) 7))
    (should (= (kao-sel-cursor s) 6))
    (should (= (kao-sel-min s) 6))))

(ert-deftest kao-object-sentence-to-begin-leading-adjust ()
  "`[s' on the blank between sentences grabs the PREVIOUS sentence (leading adjust,
selectors.cc:495-502 `not ToEnd').  \"One. Two.\": cursor 5 (space) -> [1,4] \"One.\"."
  (let ((s (kao-object-tests--sentence/f "One. Two." 5 nil t nil)))
    (should (= (kao-sel-min s) 1))                ; back through "One."
    (should (= (kao-sel-max s) 4))
    (should (= (kao-sel-cursor s) 1))))           ; cursor on the sentence start

(defun kao-object-tests--paragraph/f (content cursor inner to-begin to-end)
  "Paragraph selector in CONTENT at CURSOR with explicit flags."
  (with-temp-buffer
    (insert content)
    (kao-object--paragraph (kao-sel-make :anchor cursor :cursor cursor)
                           inner to-begin to-end)))

(ert-deftest kao-object-paragraph-to-end-and-begin ()
  "`]p'/`[p'.  \"para1\\n\\npara2\": p1..1·5 \\n6 \\n7 p8..2·12."
  ;; ]p from 3: forward to the trailing blank line (whole).
  (let ((s (kao-object-tests--paragraph/f "para1\n\npara2" 3 nil nil t)))
    (should (= (kao-sel-anchor s) 3))
    (should (= (kao-sel-cursor s) 7)))
  ;; [p from 3: backward to the paragraph start.
  (let ((s (kao-object-tests--paragraph/f "para1\n\npara2" 3 nil t nil)))
    (should (= (kao-sel-anchor s) 3))
    (should (= (kao-sel-cursor s) 1))
    (should (= (kao-sel-min s) 1))))

(defun kao-object-tests--pair/f (content cursor inner open close to-begin to-end)
  "Surrounding selector in CONTENT at CURSOR with explicit flags.
OPEN/CLOSE are chars, regexp-quoted here (the engine takes regex strings)."
  (with-temp-buffer
    (insert content)
    (kao-object--surrounding (kao-sel-make :anchor cursor :cursor cursor)
                             inner
                             (regexp-quote (char-to-string open))
                             (regexp-quote (char-to-string close))
                             to-begin to-end)))

(ert-deftest kao-object-pair-to-end-and-begin ()
  "`]'/`[' on a bracket pair.  \"foo(bar)baz\": ( 4 b5 a6 r7 ) 8."
  ;; ] from inside: forward to the enclosing close.
  (let ((s (kao-object-tests--pair/f "foo(bar)baz" 6 nil ?\( ?\) nil t)))
    (should (= (kao-sel-anchor s) 6))
    (should (= (kao-sel-cursor s) 8)))            ; cursor on the close
  ;; [ from inside: backward to the enclosing open.
  (let ((s (kao-object-tests--pair/f "foo(bar)baz" 6 nil ?\( ?\) t nil)))
    (should (= (kao-sel-anchor s) 6))
    (should (= (kao-sel-cursor s) 4))             ; cursor on the open
    (should (= (kao-sel-min s) 4))))

(ert-deftest kao-object-pair-to-end-cursor-on-open ()
  "`]' with the cursor ON the opening delimiter skips past it to the close
\(find_surrounding else-branch, selectors.cc:358-362).  cursor 4 (`(') -> [4,8]."
  (let ((s (kao-object-tests--pair/f "foo(bar)baz" 4 nil ?\( ?\) nil t)))
    (should (= (kao-sel-anchor s) 4))
    (should (= (kao-sel-cursor s) 8))))           ; whole "(bar)"

(ert-deftest kao-object-pair-inner-to-begin ()
  "`<a-[>' (inner, to-begin) lands on the inner start.  cursor 6 -> [5,6]."
  (let ((s (kao-object-tests--pair/f "foo(bar)baz" 6 t ?\( ?\) t nil)))
    (should (= (kao-sel-anchor s) 6))
    (should (= (kao-sel-cursor s) 5))             ; inner start (`b'), not the `('
    (should (= (kao-sel-min s) 5))))

(ert-deftest kao-object-quote-to-end-inner ()
  "`<a-]>' on a quote selects forward to the inner end.  \"say \\\"hi\\\" now\": \"5 h6 i7 \"8."
  (let ((s (kao-object-tests--pair/f "say \"hi\" now" 6 t ?\" ?\" nil t)))
    (should (= (kao-sel-anchor s) 6))
    (should (= (kao-sel-cursor s) 7))))           ; "hi", stops before the close quote

(ert-deftest kao-object-bracket-bindings ()
  "The four Replace bracket-object keys are bound in the normal map."
  (should (eq (lookup-key kao-normal-state-map (kbd "["))   #'kao-object-to-begin))
  (should (eq (lookup-key kao-normal-state-map (kbd "]"))   #'kao-object-to-end))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-[")) #'kao-object-inner-to-begin))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-]")) #'kao-object-inner-to-end)))

(ert-deftest kao-object-title-faithful ()
  "`kao-object--title' reproduces Kakoune get_title (normal.cc:1418-1427)."
  (should (equal (kao-object--title nil t t nil) "select surrounding object"))
  (should (equal (kao-object--title t t t nil) "select inner surrounding object"))
  (should (equal (kao-object--title nil t nil nil) "select to surrounding object begin"))
  (should (equal (kao-object--title nil nil t nil) "select to surrounding object end"))
  (should (equal (kao-object--title t nil t t) "extend to inner surrounding object end")))

;;;; Extend object variants — `{' `}' `<a-{>' `<a-}>'.
;;;; Reuse the `kao--map-filter-selections' EXTEND arm (merge_selections):
;;;; the cursor moves to the object end/start, the anchor is pulled only when both
;;;; directions agree (else kept).  Driven via the filter (read-key isn't batchable).

(defun kao-object-tests--extend (content seeds selector)
  "kao-mode CONTENT, seed SEEDS (list of (ANCHOR CURSOR)), apply SELECTOR with
extend; return the resulting selection list."
  (with-temp-buffer
    (insert content)
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels
                (kao-sels-make
                 :list (mapcar (lambda (s) (kao-sel-make :anchor (nth 0 s)
                                                         :cursor (nth 1 s)))
                               seeds)
                 :main 0))
          (kao--map-filter-selections selector t)
          (kao-sels-list kao--sels))
      (kao-mode -1))))

(ert-deftest kao-object-extend-word-to-end-keeps-anchor ()
  "`}w' extends the cursor to the word end, keeping the original anchor.
\"foo bar baz\": seed [1,2] -> [1,4] (vs Replace `]w' which moves the anchor)."
  (let ((sels (kao-object-tests--extend
               "foo bar baz" '((1 2))
               (lambda (s) (kao-object--word s nil #'kao-object--word-cat-p nil t)))))
    (should (= 1 (length sels)))
    (should (= (kao-sel-anchor (car sels)) 1))
    (should (= (kao-sel-cursor (car sels)) 4))))

(ert-deftest kao-object-extend-pair-to-end-keeps-anchor ()
  "`}' on a bracket pair extends the cursor to the close, keeping the anchor.
\"foo(bar)baz\": seed [6,7] -> [6,8]."
  (let ((sels (kao-object-tests--extend
               "foo(bar)baz" '((6 7))
               (lambda (s) (kao-object--surrounding s nil "(" ")" nil t)))))
    (should (= (kao-sel-anchor (car sels)) 6))
    (should (= (kao-sel-cursor (car sels)) 8))))

(ert-deftest kao-object-extend-pair-to-begin-backward ()
  "`{' on a bracket pair moves the cursor to the open (backward new), anchor kept.
\"foo(bar)baz\": forward seed [6,7] -> cursor 4, anchor 6 (min 4)."
  (let ((sels (kao-object-tests--extend
               "foo(bar)baz" '((6 7))
               (lambda (s) (kao-object--surrounding s nil "(" ")" t nil)))))
    (should (= (kao-sel-anchor (car sels)) 6))
    (should (= (kao-sel-cursor (car sels)) 4))
    (should (= (kao-sel-min (car sels)) 4))))

(ert-deftest kao-object-extend-drops-off-object ()
  "A cursor off the object drops even under Extend (the nullopt drop precedes the
Extend/Replace split, normal.cc:104-117).  \"foo bar baz\": [1,1] keeps, [4,4]
\(a space) drops."
  (let ((sels (kao-object-tests--extend
               "foo bar baz" '((1 1) (4 4))
               (lambda (s) (kao-object--word s nil #'kao-object--word-cat-p nil t)))))
    (should (= 1 (length sels)))
    (should (= (kao-sel-anchor (car sels)) 1))
    (should (= (kao-sel-cursor (car sels)) 4))))

(ert-deftest kao-object-extend-bindings ()
  "The four Extend bracket-object keys are bound in the normal map."
  (should (eq (lookup-key kao-normal-state-map (kbd "{"))
              #'kao-object-extend-to-begin))
  (should (eq (lookup-key kao-normal-state-map (kbd "}"))
              #'kao-object-extend-to-end))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-{"))
              #'kao-object-extend-inner-to-begin))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-}"))
              #'kao-object-extend-inner-to-end)))

;;;; Repeat last select (<a-.>) for objects — set_last_select via dispatch

(ert-deftest kao-object-repeat-select-word ()
  "`<a-.>' repeats the last object-select (records `<a-i>w', re-selects at new cursor).
In batch the which-key box is inert, so `read-key' is stubbed to drive the
object-pending dispatch — exercising the real `kao--last-select' recording."
  (with-temp-buffer
    (insert "foo bar baz")                      ; foo 1-3, bar 5-7, baz 9-11
    (kao-mode 1)
    (unwind-protect
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1))
                           :main 0))
          (kao-select-inner)                    ; <a-i> w: inner word -> [1,3]
          (should (= (kao-sel-min (kao--main-sel)) 1))
          (should (= (kao-sel-max (kao--main-sel)) 3))
          ;; Move the cursor into "bar" and repeat WITHOUT re-reading a key.
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 5 :cursor 5))
                           :main 0))
          (kao-repeat-select)                   ; <a-.>: inner word at 5 -> [5,7]
          (should (= (kao-sel-min (kao--main-sel)) 5))
          (should (= (kao-sel-max (kao--main-sel)) 7)))
      (kao-mode -1))))

;;;; Count -> enclosing pair level (select_object `params.count - 1', )

(ert-deftest kao-object-count-selects-enclosing-level ()
  "A pending count picks the count-th enclosing pair (normal.cc:1440).
In `(((x)))' (x at 4): no count/1 -> innermost inner [4,4], 2 -> [3,5],
3 -> [2,6].  The non-pair selectors ignore the count."
  (with-temp-buffer
    (insert "(((x)))")
    (kao-mode 1)
    (unwind-protect
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\()))
          (dolist (case '((0 . (4 . 4)) (1 . (4 . 4)) (2 . (3 . 5)) (3 . (2 . 6))))
            (setq kao--sels (kao-sels-make
                             :list (list (kao-sel-make :anchor 4 :cursor 4))
                             :main 0))
            (setq kao--count (car case))
            (kao-select-inner)
            (should (= (kao-sel-min (kao--main-sel)) (cadr case)))
            (should (= (kao-sel-max (kao--main-sel)) (cddr case)))))
      (setq kao--count 0)
      (kao-mode -1))))

(ert-deftest kao-object-count-whole-pair-level ()
  "`2<a-a>(' selects the 2nd enclosing pair including delimiters."
  (with-temp-buffer
    (insert "(((x)))")
    (kao-mode 1)
    (unwind-protect
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\()))
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 4 :cursor 4))
                           :main 0))
          (setq kao--count 2)
          (kao-select-whole)                    ; 2<a-a>( -> [2,6] (whole)
          (should (= (kao-sel-min (kao--main-sel)) 2))
          (should (= (kao-sel-max (kao--main-sel)) 6)))
      (setq kao--count 0)
      (kao-mode -1))))

(ert-deftest kao-object-repeat-keeps-level ()
  "`<a-.>' repeats with the captured level (the C++ lambda captures count)."
  (with-temp-buffer
    (insert "(((x)))\n(((y)))")                 ; second group: y at 12
    (kao-mode 1)
    (unwind-protect
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\()))
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 4 :cursor 4))
                           :main 0))
          (setq kao--count 2)
          (kao-select-inner)                    ; 2<a-i>( -> [3,5]
          (should (= (kao-sel-min (kao--main-sel)) 3))
          (setq kao--count 0)                   ; count reset, as post-command would
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 12 :cursor 12))
                           :main 0))
          (kao-repeat-select)                   ; repeat at y: still level 1
          (should (= (kao-sel-min (kao--main-sel)) 11))
          (should (= (kao-sel-max (kao--main-sel)) 13)))
      (setq kao--count 0)
      (kao-mode -1))))

;;;; Per-command dispatch-context seam

(ert-deftest kao-object-run-identity-when-empty ()
  "`kao--object-run' is the identity when the context hook is empty.
An empty hook must leave a dispatch pass byte-identical and pay nothing, so
non-treesit users see zero overhead from the seam."
  (let ((kao--object-dispatch-context-functions nil))
    (should (equal 42 (kao--object-run (lambda () 42))))))

(ert-deftest kao-object-run-nests-wrappers ()
  "`kao--object-run' nests every wrapper around the thunk, each entered once.
The first hook element is the outermost wrapper; the thunk runs in the middle;
exit is symmetric (`(funcall THUNK)' returned up the stack)."
  (let* ((trace '())
         (kao--object-dispatch-context-functions
          (list (lambda (thunk) (push 'a-in trace)
                  (prog1 (funcall thunk) (push 'a-out trace)))
                (lambda (thunk) (push 'b-in trace)
                  (prog1 (funcall thunk) (push 'b-out trace))))))
    (should (equal 7 (kao--object-run (lambda () (push 'body trace) 7))))
    (should (equal '(a-in b-in body b-out a-out) (nreverse trace)))))

(ert-deftest kao-object-dispatch-context-wraps-pass-once ()
  "A context wrapper wraps a whole multi-cursor `<a-i>' pass exactly once.
The wrapper on `kao--object-dispatch-context-functions' is entered once for the
entire pass (context around the pass, not per selection), and its inner thunk
still selects every cursor's object."
  (with-temp-buffer
    (insert "foo bar baz")                       ; foo 1-3, bar 5-7, baz 9-11
    (kao-mode 1)
    (let ((entries 0))
      (unwind-protect
          (let ((kao--object-dispatch-context-functions
                 (list (lambda (thunk) (cl-incf entries) (funcall thunk)))))
            (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
              (setq kao--sels (kao-sels-make
                               :list (list (kao-sel-make :anchor 1 :cursor 1)
                                           (kao-sel-make :anchor 5 :cursor 5)
                                           (kao-sel-make :anchor 9 :cursor 9))
                               :main 0))
              (kao-select-inner)                  ; <a-i>w over 3 cursors
              (should (= entries 1))              ; ONE context for the whole pass
              (should (equal (kao-object-tests--spans)
                             '((1 . 3) (5 . 7) (9 . 11))))))
        (kao-mode -1)))))

(ert-deftest kao-object-dispatch-context-wraps-repeat ()
  "A later `<a-.>' repeat also runs inside one fresh context."
  (with-temp-buffer
    (insert "foo bar baz")
    (kao-mode 1)
    (let ((entries 0))
      (unwind-protect
          (let ((kao--object-dispatch-context-functions
                 (list (lambda (thunk) (cl-incf entries) (funcall thunk)))))
            (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
              (setq kao--sels (kao-sels-make
                               :list (list (kao-sel-make :anchor 1 :cursor 1))
                               :main 0))
              (kao-select-inner)                  ; records <a-i>w, enters once
              (should (= entries 1))
              (setq kao--sels (kao-sels-make
                               :list (list (kao-sel-make :anchor 5 :cursor 5))
                               :main 0))
              (kao-repeat-select)                 ; <a-.>: re-enters the context once
              (should (= entries 2))
              (should (= (kao-sel-min (kao--main-sel)) 5))
              (should (= (kao-sel-max (kao--main-sel)) 7))))
        (kao-mode -1)))))

;;;; Arbitrary-punctuation single-delimiter object

(ert-deftest kao-object-punctuation-key-p ()
  "`kao--object-punctuation-key-p' matches Kakoune `is_punctuation(cp, {})'.
Any non-alnum, non-blank CHARACTER qualifies (`_' too — extra-word-chars is not
consulted); alnum and blanks and non-characters never do."
  (dolist (k '(?/ ?* ?_ ?# ?. ?, ?| ?~ ?% ?!))
    (should (kao--object-punctuation-key-p k)))
  (dolist (k '(?a ?z ?A ?Z ?0 ?9))
    (should-not (kao--object-punctuation-key-p k)))
  (dolist (k '(?\s ?\t ?\n ?\r ?\v))
    (should-not (kao--object-punctuation-key-p k)))
  (should-not (kao--object-punctuation-key-p 'return)))  ; a non-character event

(ert-deftest kao-object-punctuation-whole-slash ()
  "`<a-a>/' on `/home/bar' (cursor on `o') selects `/home/' incl. both slashes."
  (with-temp-buffer
    (insert "/home/bar")                          ; /1 h2 o3 m4 e5 /6 b7 a8 r9
    (kao-mode 1)
    (unwind-protect
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?/)))
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 3 :cursor 3))
                           :main 0))
          (kao-select-whole)                      ; <a-a>/
          (should (equal (buffer-substring (kao-sel-min (kao--main-sel))
                                           (1+ (kao-sel-max (kao--main-sel))))
                         "/home/")))
      (kao-mode -1))))

(ert-deftest kao-object-punctuation-inner-star ()
  "`<a-i>*' inside `a *bold* b' selects `bold'."
  (with-temp-buffer
    (insert "a *bold* b")                         ; a1 _2 *3 b4 o5 l6 d7 *8 _9 b10
    (kao-mode 1)
    (unwind-protect
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?*)))
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 5 :cursor 5))
                           :main 0))
          (kao-select-inner)                      ; <a-i>*
          (should (equal (buffer-substring (kao-sel-min (kao--main-sel))
                                           (1+ (kao-sel-max (kao--main-sel))))
                         "bold")))
      (kao-mode -1))))

(ert-deftest kao-object-punctuation-inner-underscore ()
  "`<a-i>_' works — `_' is punctuation (extra-word-chars is not consulted)."
  (with-temp-buffer
    (insert "x _yo_ z")                           ; x1 _2 _3 y4 o5 _6 _7 z8
    (kao-mode 1)
    (unwind-protect
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?_)))
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 4 :cursor 4))
                           :main 0))
          (kao-select-inner)                      ; <a-i>_
          (should (equal (buffer-substring (kao-sel-min (kao--main-sel))
                                           (1+ (kao-sel-max (kao--main-sel))))
                         "yo")))
      (kao-mode -1))))

(ert-deftest kao-object-punctuation-nested-star-multi ()
  "`<a-A>*' over `*a* *b*' yields the multi-span nested set."
  (with-temp-buffer
    (insert "*a* *b*")                            ; *1 a2 *3 _4 *5 b6 *7
    (kao-mode 1)
    (unwind-protect
        (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?*)))
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 7))
                           :main 0))
          (kao-select-nested-whole)               ; <a-A>*
          (should (equal (kao-object-tests--spans) '((1 . 3) (5 . 7)))))
      (kao-mode -1))))

(ert-deftest kao-object-punctuation-word-key-cancels ()
  "A non-punctuation UNBOUND key does not trigger the fallthrough — it cancels.
The predicate excludes alnum, so an unbound word key (`z') leaves the selection
unchanged and `kao--last-select' untouched (cancel).  (`a' is the angle
object, `<space>' the whitespace object — both are bound, so `z'/tab are the
clean unbound word/blank probes.)"
  (with-temp-buffer
    (insert "/home/bar")
    (kao-mode 1)
    (unwind-protect
        (let ((kao--last-select 'sentinel))
          (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?z)))
            (setq kao--sels (kao-sels-make
                             :list (list (kao-sel-make :anchor 3 :cursor 3))
                             :main 0))
            (kao-select-inner)
            (should (eq kao--last-select 'sentinel))
            (should (= (kao-sel-min (kao--main-sel)) 3))
            (should (= (kao-sel-max (kao--main-sel)) 3))))
      (kao-mode -1))))

(ert-deftest kao-object-punctuation-blank-key-cancels ()
  "A blank UNBOUND key (tab) does not trigger the fallthrough — it cancels."
  (with-temp-buffer
    (insert "/home/bar")
    (kao-mode 1)
    (unwind-protect
        (let ((kao--last-select 'sentinel))
          (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\t)))
            (setq kao--sels (kao-sels-make
                             :list (list (kao-sel-make :anchor 3 :cursor 3))
                             :main 0))
            (kao-select-inner)
            (should (eq kao--last-select 'sentinel))
            (should (= (kao-sel-min (kao--main-sel)) 3))))
      (kao-mode -1))))

;;;; Nested objects <a-I>/<a-A> (ObjectFlags::Nested, )

(defmacro kao-object-tests--nested (text sels &rest body)
  "Run BODY in a kao TEXT buffer with selections SELS ((ANCHOR . CURSOR)...)."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,text)
     (kao-mode 1)
     (unwind-protect
         (progn
           (setq kao--sels
                 (kao-sels-make
                  :list (mapcar (lambda (s)
                                  (kao-sel-make :anchor (car s) :cursor (cdr s)))
                                ,sels)
                  :main (1- (length ,sels))))
           ,@body)
       (setq kao--count 0)
       (kao-mode -1))))

(defun kao-object-tests--spans ()
  "The current selections as a list of (MIN . MAX) conses."
  (mapcar (lambda (s) (cons (kao-sel-min s) (kao-sel-max s)))
          (kao-sels-list kao--sels)))

;;;; Number object (select_number / select_nested_numbers, selectors.cc:445/851)

(defun kao-object-tests--number/f (content cursor inner to-begin to-end)
  "Number selector in CONTENT at CURSOR (1-based) with explicit flags."
  (with-temp-buffer
    (insert content)
    (kao-object--number (kao-sel-make :anchor cursor :cursor cursor)
                        inner to-begin to-end)))

(ert-deftest kao-object-number-whole-mid ()
  "`<a-a>n' from a middle digit selects the whole number incl. the dot.
\"ab12.5cd\": a1 b2 1=3 2=4 .=5 5=6 c7 d8."
  (let ((s (kao-object-tests--number/f "ab12.5cd" 4 nil t t)))
    (should (= (kao-sel-anchor s) 3))             ; '1' (number start)
    (should (= (kao-sel-cursor s) 6))             ; '5' (number end, incl '.')
    (should (kao-sel-forward-p s))))

(ert-deftest kao-object-number-inner-excludes-dot ()
  "`<a-i>n' (inner) stops at the dot — `.' is not an inner number char.
\"12.5\": 1=1 2=2 .=3 5=4."
  (let ((s (kao-object-tests--number/f "12.5" 2 t t t)))
    (should (= (kao-sel-anchor s) 1))             ; '1'
    (should (= (kao-sel-cursor s) 2))))           ; '2'  -> "12", dot excluded

(ert-deftest kao-object-number-includes-leading-minus ()
  "A leading `-' is part of the whole number.  \"-42\": -=1 4=2 2=3."
  (let ((s (kao-object-tests--number/f "-42" 2 nil t t)))
    (should (= (kao-sel-anchor s) 1))             ; '-'
    (should (= (kao-sel-cursor s) 3))))           ; '2'  -> "-42"

(ert-deftest kao-object-number-to-end-and-to-begin ()
  "`]n' is forward to the number end (anchor kept); `[n' backward to the start.
\"x123y\": x1 1=2 2=3 3=4 y5, cursor 3 ('2')."
  (let ((e (kao-object-tests--number/f "x123y" 3 nil nil t)))
    (should (= (kao-sel-anchor e) 3))             ; cursor kept
    (should (= (kao-sel-cursor e) 4))             ; '3' (end)
    (should (kao-sel-forward-p e)))
  (let ((b (kao-object-tests--number/f "x123y" 3 nil t nil)))
    (should (= (kao-sel-anchor b) 3))             ; cursor kept
    (should (= (kao-sel-cursor b) 2))             ; '1' (start)
    (should-not (kao-sel-forward-p b))))

(ert-deftest kao-object-number-off-number-nil ()
  "A cursor not on a digit, `.', or `-' yields no object (drop)."
  (should (null (kao-object-tests--number/f "ab12cd" 1 t t t)))   ; on 'a'
  (should (null (kao-object-tests--number/f "" 1 t t t))))        ; empty buffer

(ert-deftest kao-object-number-dispatch-and-registered ()
  "The `n' key is registered in all three tables and selects the number span."
  (let ((sels (kao-object-tests--dispatch
               "ab12cd" '(4)                       ; cursor on '2'
               (lambda (s) (kao-object--number s t t t)))))
    (should (equal (mapcar #'kao-sel-min sels) '(3)))
    (should (equal (mapcar #'kao-sel-max sels) '(4))))   ; "12"
  (should (eq (cdr (assq ?n kao--object-table)) #'kao-object--number))
  (should (eq (cdr (assq ?n kao--object-nested-table)) #'kao-object--nested-numbers))
  (should (equal (cdr (assq ?n kao--object-info)) "number")))

(ert-deftest kao-object-nested-numbers ()
  "`<a-I>n' selects every number in the region, skipping a lone `-'/`.'.
\"a12 -3 b. 9\": a1 1=2 2=3 _4 -=5 3=6 _7 b8 .=9 _10 9=11."
  (kao-object-tests--nested "a12 -3 b. 9" '((1 . 11))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?n)))
      (kao-select-nested-inner)
      ;; "12" (2-3), "-3" (5-6), "9" (11); the lone "." at 9 is dropped
      (should (equal (kao-object-tests--spans) '((2 . 3) (5 . 6) (11 . 11))))
      (should (= (kao-sels-main kao--sels) 2)))))   ; main = last

;;;; Indent object (select_indent / select_nested_indents, selectors.cc:651/939)

(defun kao-object-tests--indent/f (content cursor inner to-begin to-end &optional tabstop)
  "Indent selector in CONTENT at CURSOR (1-based) with explicit flags.
TABSTOP sets the buffer `tab-width' (default 8)."
  (with-temp-buffer
    (insert content)
    (setq tab-width (or tabstop 8))
    (kao-object--indent (kao-sel-make :anchor cursor :cursor cursor)
                        inner to-begin to-end)))

(ert-deftest kao-object-indent-whole-mid ()
  "`<a-a>i' from an indented line selects the indent block incl. the last newline.
\"a\\n  b\\n  c\\nd\\n\": a1 _2 __3-4 b5 _6 __7-8 c9 _10 d11 _12; cursor `b'=5."
  (let ((s (kao-object-tests--indent/f "a\n  b\n  c\nd\n" 5 nil t t)))
    (should (= (kao-sel-anchor s) 3))             ; line 2 column 0
    (should (= (kao-sel-cursor s) 10))            ; line 3's trailing newline
    (should (kao-sel-forward-p s))))

(ert-deftest kao-object-indent-whole-includes-blank-edges ()
  "`<a-a>i' keeps blank lines flanking the block (Kakoune `buffer[l]==\"\\n\"').
\"x\\n\\n  b\\n\\ny\\n\": x1 _2 _3(blank) __4-5 b6 _7 _8(blank) y9 _10; cursor `b'=6."
  (let ((s (kao-object-tests--indent/f "x\n\n  b\n\ny\n" 6 nil t t)))
    (should (= (kao-sel-anchor s) 3))             ; blank line 2 (its newline)
    (should (= (kao-sel-cursor s) 8))))           ; blank line 4 (its newline)

(ert-deftest kao-object-indent-inner-trims-blank-edges ()
  "`<a-i>i' trims whitespace-only edge lines but keeps the leading indentation.
Same buffer as the whole case; inner collapses to just line 3 `  b' + newline."
  (let ((s (kao-object-tests--indent/f "x\n\n  b\n\ny\n" 6 t t t)))
    (should (= (kao-sel-anchor s) 4))             ; line 3 column 0 (leading ws kept)
    (should (= (kao-sel-cursor s) 7))))           ; line 3's trailing newline

(ert-deftest kao-object-indent-tabstop-aware ()
  "`get_indent' rounds a tab to the next `tab-width'; the block groups by indent.
\"a\\n\\tb\\n    c\\nd\\n\": with tab-width 4 the tab line and the 4-space line share
indent 4 and group; with tab-width 8 the tab is indent 8 and they split."
  (let ((g (kao-object-tests--indent/f "a\n\tb\n    c\nd\n" 4 nil t t 4)))
    (should (= (kao-sel-anchor g) 3))             ; tab line start
    (should (= (kao-sel-cursor g) 11)))           ; through the 4-space line's newline
  (let ((s (kao-object-tests--indent/f "a\n\tb\n    c\nd\n" 4 nil t t 8)))
    (should (= (kao-sel-anchor s) 3))
    (should (= (kao-sel-cursor s) 5))))           ; only the tab line (its newline)

(ert-deftest kao-object-indent-to-begin-backward ()
  "`[i' extends back to the block start as a BACKWARD selection (cursor kept anchor).
\"a\\n  b\\n  c\\nd\\n\", cursor `c'=9: start walks up over `  b' to line 2 (pos 3)."
  (let ((b (kao-object-tests--indent/f "a\n  b\n  c\nd\n" 9 nil t nil)))
    (should (= (kao-sel-anchor b) 9))             ; cursor kept
    (should (= (kao-sel-cursor b) 3))             ; block start (line 2 col 0)
    (should-not (kao-sel-forward-p b))))

(ert-deftest kao-object-indent-to-end-forward ()
  "`]i' extends forward to the block end (anchor kept at the exact cursor).
\"a\\n  b\\n  c\\nd\\n\", cursor `c'=9: end stops at line 3 (no further indent)."
  (let ((e (kao-object-tests--indent/f "a\n  b\n  c\nd\n" 9 nil nil t)))
    (should (= (kao-sel-anchor e) 9))             ; cursor kept
    (should (= (kao-sel-cursor e) 10))            ; line 3's trailing newline
    (should (kao-sel-forward-p e))))

(ert-deftest kao-object-indent-registered-and-parity ()
  "`i' is wired into all three built-in tables; both key sets stay exactly equal."
  (should (eq (cdr (assq ?i kao--object-table)) #'kao-object--indent))
  (should (eq (cdr (assq ?i kao--object-nested-table)) #'kao-object--nested-indents))
  (should (equal (cdr (assq ?i kao--object-info)) "indent"))
  ;; the throwing nested entry keeps parity exact — no relaxation
  (should (equal (mapcar #'car kao--object-nested-table)
                 (mapcar #'car kao--object-table))))

(ert-deftest kao-object-indent-dispatch-end-to-end ()
  "`<a-i>i' (object-pending read-key) selects the indent block under the cursor.
\"x\\n  a\\n  b\\nc\\n\", cursor `a'=5: inner block is lines 2-3 `  a\\n  b' + newline."
  (kao-object-tests--nested "x\n  a\n  b\nc\n" '((5 . 5))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?i)))
      (kao-select-inner)
      (should (equal (kao-object-tests--spans) '((3 . 10)))))))

(ert-deftest kao-object-nested-indents-throws ()
  "`<a-I>i' surfaces Kakoune's \"not implemented\" error (`select_nested_indents')."
  (kao-object-tests--nested "  a\n  b\n" '((1 . 6))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?i)))
      (should-error (kao-select-nested-inner) :type 'user-error))))

;;;; Argument object (select_argument / select_nested_arguments, selectors.cc:728/944)

(defun kao-object-tests--argument/f (content cursor inner to-begin to-end &optional level)
  "Argument selector in CONTENT at CURSOR (1-based) with explicit flags and LEVEL."
  (with-temp-buffer
    (insert content)
    (kao-object--argument (kao-sel-make :anchor cursor :cursor cursor)
                          inner to-begin to-end level)))

(ert-deftest kao-object-argument-whole-mid ()
  "`<a-a>u' on a middle arg spans from after the prev delimiter to the next delimiter.
\"f(a, bb, c)\": f1 (2 a3 ,4 _5 b6 b7 ,8 _9 c10 )11; cursor `b'=6."
  (let ((s (kao-object-tests--argument/f "f(a, bb, c)" 6 nil t t)))
    (should (= (kao-sel-anchor s) 5))             ; the space after the first comma
    (should (= (kao-sel-cursor s) 8))             ; the trailing comma
    (should (kao-sel-forward-p s))))

(ert-deftest kao-object-argument-inner-mid ()
  "`<a-i>u' on a middle arg trims the leading blank and the trailing delimiter.
Same buffer as the whole case; cursor `b'=6 -> just \"bb\"."
  (let ((s (kao-object-tests--argument/f "f(a, bb, c)" 6 t t t)))
    (should (= (kao-sel-anchor s) 6))             ; 'b'
    (should (= (kao-sel-cursor s) 7))))           ; 'b' -> "bb"

(ert-deftest kao-object-argument-first-arg ()
  "The first arg's whole object swallows the trailing blank; inner is just the arg.
\"f(a, bb, c)\", cursor `a'=3."
  (let ((w (kao-object-tests--argument/f "f(a, bb, c)" 3 nil t t)))
    (should (= (kao-sel-anchor w) 3))             ; 'a'
    (should (= (kao-sel-cursor w) 5)))            ; ',' then the space after (first-arg)
  (let ((i (kao-object-tests--argument/f "f(a, bb, c)" 3 t t t)))
    (should (= (kao-sel-anchor i) 3))
    (should (= (kao-sel-cursor i) 3))))           ; "a"

(ert-deftest kao-object-argument-last-arg ()
  "The last arg's whole object takes its LEADING delimiter; inner is just the arg.
\"f(a, bb, c)\", cursor `c'=10."
  (let ((w (kao-object-tests--argument/f "f(a, bb, c)" 10 nil t t)))
    (should (= (kao-sel-anchor w) 8))             ; the comma before (leading delim)
    (should (= (kao-sel-cursor w) 10)))           ; 'c' -> ", c"
  (let ((i (kao-object-tests--argument/f "f(a, bb, c)" 10 t t t)))
    (should (= (kao-sel-anchor i) 10))
    (should (= (kao-sel-cursor i) 10))))          ; "c"

(ert-deftest kao-object-argument-level-enclosing ()
  "LEVEL selects the count-th enclosing bracket level (`2<a-i>u').
\"f(a, g(b, c), d)\": f1 (2 a3 ,4 _5 g6 (7 b8 ,9 _10 c11 )12 ,13 _14 d15 )16.
Cursor `b'=8: level 0 selects \"b\"; level 1 the whole \"g(b, c)\" outer arg."
  (let ((l0 (kao-object-tests--argument/f "f(a, g(b, c), d)" 8 t t t 0)))
    (should (= (kao-sel-anchor l0) 8))            ; 'b'
    (should (= (kao-sel-cursor l0) 8)))           ; "b" (innermost)
  (let ((l1 (kao-object-tests--argument/f "f(a, g(b, c), d)" 8 t t t 1)))
    (should (= (kao-sel-anchor l1) 6))            ; 'g' (start of the outer arg)
    (should (= (kao-sel-cursor l1) 12))))         ; ')' -> "g(b, c)"

(ert-deftest kao-object-argument-dispatch-and-registered ()
  "The `u' key is registered in all three tables and selects the argument span."
  (let ((sels (kao-object-tests--dispatch
               "f(a, bb, c)" '(6)                  ; cursor on the first 'b'
               (lambda (s) (kao-object--argument s t t t)))))
    (should (equal (mapcar #'kao-sel-min sels) '(6)))
    (should (equal (mapcar #'kao-sel-max sels) '(7))))   ; "bb"
  (should (eq (cdr (assq ?u kao--object-table)) #'kao-object--argument))
  (should (eq (cdr (assq ?u kao--object-nested-table)) #'kao-object--nested-arguments))
  (should (equal (cdr (assq ?u kao--object-info)) "argument")))

(ert-deftest kao-object-argument-dispatch-end-to-end ()
  "`<a-i>u' (object-pending read-key) selects the argument under the cursor.
\"f(a, bb, c)\", cursor `b'=6 -> inner \"bb\" (6-7)."
  (kao-object-tests--nested "f(a, bb, c)" '((6 . 6))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?u)))
      (kao-select-inner)
      (should (equal (kao-object-tests--spans) '((6 . 7)))))))

(ert-deftest kao-object-nested-arguments-inner ()
  "`<a-I>u' (inner) splits a comma list into each argument, blanks trimmed.
\"a, b, c\": a1 ,2 _3 b4 ,5 _6 c7."
  (kao-object-tests--nested "a, b, c" '((1 . 7))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?u)))
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans) '((1 . 1) (4 . 4) (7 . 7))))
      (should (= (kao-sels-main kao--sels) 2)))))   ; main = last

(ert-deftest kao-object-nested-arguments-whole ()
  "`<a-A>u' (whole) keeps each delimiter and the leading blank of later args.
\"a, b, c\": each span includes its comma (and the space for non-first args)."
  (kao-object-tests--nested "a, b, c" '((1 . 7))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?u)))
      (kao-select-nested-whole)
      (should (equal (kao-object-tests--spans) '((1 . 2) (3 . 5) (6 . 7)))))))

(ert-deftest kao-object-nested-arguments-bracket-level ()
  "A bracketed group counts as ONE argument (depth tracking, not split on its commas).
\"a, (b, c), d\": a1 ,2 _3 (4 b5 ,6 _7 c8 )9 ,10 _11 d12 -> 3 args."
  (kao-object-tests--nested "a, (b, c), d" '((1 . 12))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?u)))
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans) '((1 . 1) (4 . 9) (12 . 12)))))))

(ert-deftest kao-object-argument-registered-and-parity ()
  "`u' is wired into all three built-in tables; both key sets stay exactly equal."
  (should (eq (cdr (assq ?u kao--object-table)) #'kao-object--argument))
  (should (eq (cdr (assq ?u kao--object-nested-table)) #'kao-object--nested-arguments))
  (should (equal (cdr (assq ?u kao--object-info)) "argument"))
  (should (equal (mapcar #'car kao--object-nested-table)
                 (mapcar #'car kao--object-table))))

;;;; Public kao-object-register (runtime object-set extension)

(defmacro kao-object-tests--with-runtime (&rest body)
  "Run BODY with empty, isolated runtime object alists (restored on exit)."
  (declare (indent 0))
  `(let ((kao--object-runtime-table nil)
         (kao--object-runtime-info nil)
         (kao--object-runtime-nested nil))
     ,@body))

(ert-deftest kao-object-register-runtime-not-builtin ()
  "`kao-object-register' fills the runtime alists; the frozen defconsts are untouched."
  (kao-object-tests--with-runtime
    (kao-object-register ?Z #'kao-object--number "zed" #'kao-object--nested-numbers)
    (should (eq (kao--object-selector ?Z) #'kao-object--number))
    (should (eq (kao--object-nested-selector ?Z) #'kao-object--nested-numbers))
    (should (equal (cdr (assq ?Z (kao--object-info-rows))) "zed"))
    (should-not (assq ?Z kao--object-table))
    (should-not (assq ?Z kao--object-nested-table))
    (should-not (assq ?Z kao--object-info))))

(ert-deftest kao-object-register-no-info-no-nested ()
  "Without INFO/NESTED the key dispatches but adds no box row and no nested walk."
  (kao-object-tests--with-runtime
    (kao-object-register ?Z #'kao-object--number)
    (should (eq (kao--object-selector ?Z) #'kao-object--number))
    (should (null (kao--object-nested-selector ?Z)))
    (should-not (assq ?Z (kao--object-info-rows)))))

(ert-deftest kao-object-register-reregister-replaces ()
  "Re-registering a key replaces its selector and drops a now-absent info/nested."
  (kao-object-tests--with-runtime
    (kao-object-register ?Z #'kao-object--number "zed" #'kao-object--nested-numbers)
    (kao-object-register ?Z #'kao-object--whitespace)        ; no info, no nested
    (should (eq (kao--object-selector ?Z) #'kao-object--whitespace))
    (should (null (kao--object-nested-selector ?Z)))         ; dropped
    (should-not (assq ?Z (kao--object-info-rows)))))         ; dropped

(ert-deftest kao-object-register-overrides-builtin ()
  "A runtime key shadows a built-in of the same key; its info row replaces (not dups)."
  (kao-object-tests--with-runtime
    (should (eq (kao--object-selector ?n) #'kao-object--number))   ; built-in
    (kao-object-register ?n #'kao-object--whitespace "ws-override")
    (should (eq (kao--object-selector ?n) #'kao-object--whitespace))
    (should (equal (cdr (assq ?n (kao--object-info-rows))) "ws-override"))
    (should (= 1 (seq-count (lambda (r) (eq (car r) ?n)) (kao--object-info-rows))))))

(ert-deftest kao-object-register-dispatch-end-to-end ()
  "A registered selector dispatches through `<a-i>' (the object-pending read-key)."
  (kao-object-tests--with-runtime
    (kao-object-register ?Z #'kao-object--number "zed")
    (kao-object-tests--nested "ab12cd" '((4 . 4))            ; cursor on '2'
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?Z)))
        (kao-select-inner)
        (should (equal (kao-object-tests--spans) '((3 . 4))))))))   ; "12"

(ert-deftest kao-object-register-rejects-non-function-selector ()
  "A non-function SELECTOR is caught AT register time, naming both
`kao-object-register' and the key — not left to die far away as
\(invalid-function …).  Covers the swapped selector/info
mistake `(kao-object-register ?q \"docstring\" #'ignore)' and a bare
non-function selector."
  (kao-object-tests--with-runtime
    (dolist (form '((kao-object-register ?q "docstring" #'ignore) ; swapped selector/info
                    (kao-object-register ?q "not-a-fn")))         ; non-function selector
      (let ((err (should-error (eval form t) :type 'user-error)))
        (should (string-match-p "kao-object-register" (cadr err)))
        (should (string-match-p "\\bq\\b" (cadr err)))))
    ;; a valid registration was never written by a rejected call
    (should-not (assq ?q kao--object-runtime-table))))

(ert-deftest kao-object-register-rejects-non-function-nested-selector ()
  "A non-nil, non-function NESTED-SELECTOR is caught AT register time, naming
both `kao-object-register' and the key (same failure class as SELECTOR — an
in-spirit extension of)."
  (kao-object-tests--with-runtime
    (let ((err (should-error
                (kao-object-register ?q #'kao-object--number "info" "not-a-fn")
                :type 'user-error)))
      (should (string-match-p "kao-object-register" (cadr err)))
      (should (string-match-p "\\bq\\b" (cadr err))))
    (should-not (assq ?q kao--object-runtime-table))))

;;;; Mode-off guard  — M-x-discoverable object commands

(ert-deftest kao-object-commands-guard-mode-off ()
  "Every M-x-discoverable object command signals the shared mode-off
`user-error' in a non-kao buffer instead of dying far away with the cryptic
wrong-type-argument crash on a nil selection list.  The
`kao--assert-mode' guard fires before the object-pending key read, so the
mocked `read-key' (needed only to keep the pre-guard code from blocking)
never matters."
  (dolist (cmd '(kao-select-inner kao-select-whole
                 kao-object-to-begin kao-object-to-end
                 kao-object-inner-to-begin kao-object-inner-to-end
                 kao-object-extend-to-begin kao-object-extend-to-end
                 kao-object-extend-inner-to-begin kao-object-extend-inner-to-end
                 kao-select-nested-inner kao-select-nested-whole))
    (with-temp-buffer
      (fundamental-mode)                        ; kao-mode off -> kao--sels nil
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
        (let ((err (should-error (call-interactively cmd) :type 'user-error)))
          (should (string-match-p "kao-mode is not active" (cadr err))))))))

;;;; Public kao-object-make-pair-selector (pair-selector constructor)

(defun kao-object-tests--x-line-spans (selector cmd)
  "Register `?x' with SELECTOR (a `line' object), dispatch CMD, return spans.
CMD is `kao-select-inner' (<a-i>x) or `kao-select-whole' (<a-a>x); the cursor
sits on the second line of a leading-blank buffer so the `^[ \\t]*'/`[ \\t]*\\n'
pair actually encloses it."
  (kao-object-tests--with-runtime
    (kao-object-register ?x selector "line")
    (kao-object-tests--nested "  hello\nworld\n" '((10 . 10))   ; cursor on 'r'
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?x)))
        (funcall cmd)
        (kao-object-tests--spans)))))

(ert-deftest kao-object-make-pair-selector-identical-to-private-lambda ()
  "An `x' line-object built with `kao-object-make-pair-selector' selects the
same `<a-i>x'/`<a-a>x' spans as the earlier recipe's hand-written
`kao-object--surrounding' lambda (behavior-identical, API surface only)."
  (let ((lambda-sel (lambda (sel inner to-begin to-end &optional level)
                      (kao-object--surrounding sel inner "^[ \t]*" "[ \t]*\n"
                                               to-begin to-end (or level 0))))
        (ctor-sel (kao-object-make-pair-selector "^[ \t]*" "[ \t]*\n")))
    (dolist (cmd '(kao-select-inner kao-select-whole))
      (let ((lambda-spans (kao-object-tests--x-line-spans lambda-sel cmd))
            (ctor-spans (kao-object-tests--x-line-spans ctor-sel cmd)))
        (should ctor-spans)                     ; the object actually matched
        (should (equal lambda-spans ctor-spans))))))

(ert-deftest kao-object-nested-table-parity ()
  "`kao--object-nested-table' covers exactly the `kao--object-table' keys.
Kakoune's ObjectType table pairs every `func' with a `nested_func'
\(normal.cc:1444-1456) and the pair clause serves both — the kao subsets
must stay in lockstep."
  (should (equal (mapcar #'car kao--object-nested-table)
                 (mapcar #'car kao--object-table))))

(ert-deftest kao-object-nested-words-inner-and-whole ()
  "`<a-I>w' selects every inner word; `<a-A>w' adds trailing blanks
\(select_nested_words, selectors.cc:830)."
  (kao-object-tests--nested "foo bar baz" '((1 . 11))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans) '((1 . 3) (5 . 7) (9 . 11))))
      (should (= (kao-sels-main kao--sels) 2))  ; main = last
      (setq kao--sels (kao-sels-make
                       :list (list (kao-sel-make :anchor 1 :cursor 11))
                       :main 0))
      (kao-select-nested-whole)
      (should (equal (kao-object-tests--spans) '((1 . 4) (5 . 8) (9 . 11)))))))

(ert-deftest kao-object-nested-whitespaces ()
  "Inner whitespace runs exclude newlines; whole ones include them
\(select_nested_whitespaces, selectors.cc:923)."
  (kao-object-tests--nested "a b\nc" '((1 . 5))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\s)))
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans) '((2 . 2))))
      (setq kao--sels (kao-sels-make
                       :list (list (kao-sel-make :anchor 1 :cursor 5))
                       :main 0))
      (kao-select-nested-whole)
      (should (equal (kao-object-tests--spans) '((2 . 2) (4 . 4)))))))

(ert-deftest kao-object-nested-paragraphs ()
  "Inner paragraphs stop at the blank line; whole ones run past it
\(select_nested_paragraphs, selectors.cc:896)."
  (kao-object-tests--nested "aa\n\nbb" '((1 . 6))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?p)))
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans) '((1 . 3) (5 . 6))))
      (setq kao--sels (kao-sels-make
                       :list (list (kao-sel-make :anchor 1 :cursor 6))
                       :main 0))
      (kao-select-nested-whole)
      (should (equal (kao-object-tests--spans) '((1 . 4) (5 . 6)))))))

(ert-deftest kao-object-nested-sentences ()
  "Each run to a sentence ender is a sentence; whole adds trailing spaces;
a run reaching the region end without an ender still counts
\(select_nested_sentences, selectors.cc:874)."
  (kao-object-tests--nested "Hi. There" '((1 . 9))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?s)))
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans) '((1 . 3) (5 . 9))))
      (setq kao--sels (kao-sels-make
                       :list (list (kao-sel-make :anchor 1 :cursor 9))
                       :main 0))
      (kao-select-nested-whole)
      ;; Whole consumes the space after the ender into the FIRST sentence.
      (should (equal (kao-object-tests--spans) '((1 . 4) (5 . 9)))))))

(ert-deftest kao-object-nested-pairs-and-depth ()
  "`<a-I>(' selects every depth-1 inner pair; a count picks the depth
\(regex_select_nested, selectors.cc:978: level starts at -count-1)."
  (kao-object-tests--nested "(a)(b)" '((1 . 6))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\()))
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans) '((2 . 2) (5 . 5))))
      (setq kao--sels (kao-sels-make
                       :list (list (kao-sel-make :anchor 1 :cursor 6))
                       :main 0))
      (kao-select-nested-whole)
      (should (equal (kao-object-tests--spans) '((1 . 3) (4 . 6))))))
  (kao-object-tests--nested "((a)(b))" '((1 . 8))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\()))
      ;; No count: the outermost (depth-1) pair, inner.
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans) '((2 . 7))))
      ;; Count 2: the depth-2 pairs.
      (setq kao--sels (kao-sels-make
                       :list (list (kao-sel-make :anchor 1 :cursor 8))
                       :main 0))
      (setq kao--count 2)
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans) '((3 . 3) (6 . 6)))))))

(ert-deftest kao-object-nested-pairs-stream-exhaustion ()
  "Processing stops when either delimiter stream runs out: in `(a)((' the
two trailing openings are never seen, so no dangling span is produced
\(the C++ merged-stream loop's outer condition, selectors.cc:996)."
  (kao-object-tests--nested "(a)((" '((1 . 5))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\()))
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans) '((2 . 2)))))))

(ert-deftest kao-object-nested-quotes-alternate ()
  "open == close delimiters alternate start/end; an unmatched start runs to
the region end (regex_select_nested one-regex form, selectors.cc:1017)."
  (kao-object-tests--nested "'a' b 'c'" '((1 . 9))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?')))
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans) '((2 . 2) (8 . 8))))))
  (kao-object-tests--nested "'a' 'x" '((1 . 6))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?')))
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans) '((2 . 2) (6 . 6)))))))

(ert-deftest kao-object-nested-empty-inner-dropped ()
  "An empty inner pair produces no span (`start <= end' guard); whole still
selects the delimiters; all-empty reports \"nothing selected\" and leaves
the list unchanged."
  (kao-object-tests--nested "x()" '((1 . 3))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\()))
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans) '((1 . 3))))  ; unchanged
      (kao-select-nested-whole)
      (should (equal (kao-object-tests--spans) '((2 . 3)))))))

(ert-deftest kao-object-nested-multi-selection-input ()
  "Each selection contributes its own spans, concatenated in order
\(for_each_sel, selectors.cc:817); main = last."
  (kao-object-tests--nested "ab cd\nef gh" '((1 . 5) (7 . 11))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans)
                     '((1 . 2) (4 . 5) (7 . 8) (10 . 11))))
      (should (= (kao-sels-main kao--sels) 3)))))

(ert-deftest kao-object-nested-repeat-and-bindings ()
  "`<a-.>' repeats the nested select (select_nested_and_set_last); M-I/M-A
are bound."
  (should (eq (lookup-key kao-normal-state-map (kbd "M-I"))
              #'kao-select-nested-inner))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-A"))
              #'kao-select-nested-whole))
  (kao-object-tests--nested "ab cd" '((1 . 5))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
      (kao-select-nested-inner)
      (should (equal (kao-object-tests--spans) '((1 . 2) (4 . 5))))
      ;; Collapse to one selection and repeat WITHOUT re-reading a key.
      (setq kao--sels (kao-sels-make
                       :list (list (kao-sel-make :anchor 4 :cursor 5))
                       :main 0))
      (kao-repeat-select)
      (should (equal (kao-object-tests--spans) '((4 . 5)))))))

;;;; The regex engine on multi-char delimiters
;;;; "x begin a begin b end c end y": b3..g7=begin1, b11..g15=begin2,
;;;;  b17, e19..d21=end1, c23, e25..d27=end2, y29

(ert-deftest kao-object-rx-pair-whole-and-inner ()
  "Multi-char nestable pair: whole spans open MBEG..close MEND-1, inner between."
  ;; cursor 17 = the b between the inner begin/end
  (let ((w (with-temp-buffer
             (insert "x begin a begin b end c end y")
             (kao-object--surrounding (kao-sel-make :anchor 17 :cursor 17)
                                      nil "begin" "end" t t))))
    (should (= (kao-sel-anchor w) 11))           ; inner "begin" start
    (should (= (kao-sel-cursor w) 21)))          ; inner "end" last char
  (let ((i (with-temp-buffer
             (insert "x begin a begin b end c end y")
             (kao-object--surrounding (kao-sel-make :anchor 17 :cursor 17)
                                      t "begin" "end" t t))))
    (should (= (kao-sel-anchor i) 16))           ; after inner "begin"
    (should (= (kao-sel-cursor i) 18))))         ; before inner "end"

(ert-deftest kao-object-rx-pair-level-selects-parent ()
  "LEVEL 1 (count 2) selects the enclosing outer multi-char pair."
  (let ((w (with-temp-buffer
             (insert "x begin a begin b end c end y")
             (kao-object--surrounding (kao-sel-make :anchor 17 :cursor 17)
                                      nil "begin" "end" t t 1))))
    (should (= (kao-sel-anchor w) 3))            ; outer "begin" start
    (should (= (kao-sel-cursor w) 27))))         ; outer "end" last char

(ert-deftest kao-object-rx-pair-cursor-mid-open-token ()
  "A cursor INSIDE an open token: that match is not fully within
\=[bob, pos+1), so the enclosing (outer) pair is found — the same window
Kakoune's backward iteration over [begin, pos+1) sees."
  ;; cursor 13 = the g of the inner "begin"
  (let ((w (with-temp-buffer
             (insert "x begin a begin b end c end y")
             (kao-object--surrounding (kao-sel-make :anchor 13 :cursor 13)
                                      nil "begin" "end" t t))))
    (should (= (kao-sel-anchor w) 3))
    (should (= (kao-sel-cursor w) 27))))         ; outer "end" last char (the
                                                 ; closing scan level-counts the
                                                 ; inner begin, so outer pairs
                                                 ; with OUTER end)

(ert-deftest kao-object-rx-quote-regex-alternates ()
  "open == close regex (non-nestable): matches alternate like quotes."
  ;; "a %% b %% c": %%=4..5, %%=9..10 -> inner span 6..8 " b "
  (let ((i (with-temp-buffer
             (insert "a %% b %% c")
             (kao-object--surrounding (kao-sel-make :anchor 7 :cursor 7)
                                      t "%%" "%%" t t))))
    (should (= (kao-sel-anchor i) 5))            ; after first %%
    (should (= (kao-sel-cursor i) 7))))

(ert-deftest kao-object-rx-matches-zero-width-terminates ()
  "A zero-width-capable regex terminates and yields the empty matches."
  (with-temp-buffer
    (insert "bab")
    (let ((ms (kao-object--rx-matches "a*" 1 4)))
      (should (equal ms '((1 . 1) (2 . 3) (3 . 3)))))))

(ert-deftest kao-object-rx-nested-multichar-walk ()
  "Nested <a-I>-style walk with multi-char delimiters: every depth-1 span."
  ;; "begin a end begin b end": spans " a " (6..8+...) per pair
  (with-temp-buffer
    (insert "begin a end begin b end")
    (let ((spans (kao-object--nested-pairs 1 24 t "begin" "end" 0)))
      ;; begin1=1..5, a=7, end1=9..11; begin2=13..17, b=19, end2=21..23
      (should (equal spans '((6 . 8) (18 . 20)))))))

;;;; Custom object — the `c' desc prompt (normal.cc:1479-1516)

(ert-deftest kao-object-parse-desc ()
  "Desc parsing: comma split with backslash escape, only backslash-comma
unescaped, exactly two non-empty parts."
  (should (equal (kao-object--parse-desc "a,b") '("a" . "b")))
  (should (equal (kao-object--parse-desc "begin,end") '("begin" . "end")))
  ;; escaped comma inside a part
  (should (equal (kao-object--parse-desc "a\\,b,c") '("a,b" . "c")))
  ;; a part that IS a literal comma
  (should (equal (kao-object--parse-desc "\\,,x") '("," . "x")))
  ;; doubled backslash survives (it is NOT an escaped comma)
  (should (equal (kao-object--parse-desc "a\\\\,b") (cons "a\\\\" "b"))))

(ert-deftest kao-object-parse-desc-errors ()
  "Anything but exactly two non-empty parts: the EXACT Kakoune error."
  (dolist (bad '("" "a" "a,b,c" "a," ",b" ","))
    (let ((err (should-error (kao-object--parse-desc bad) :type 'user-error)))
      (should (equal (cadr err)
                     "desc parsing failed, expected <open>,<close>"))))
  ;; a part that does not compile as an Emacs regex is loud at parse time
  (should-error (kao-object--parse-desc "[x,y") :type 'user-error))

(defmacro kao-object-tests--with-desc (desc &rest body)
  "Run BODY with `read-string' returning DESC (counting calls in CALLS)."
  (declare (indent 1))
  `(let ((calls 0))
     (ignore calls)
     (cl-letf (((symbol-function 'read-string)
                (lambda (&rest _) (setq calls (1+ calls)) ,desc)))
       ,@body)))

(ert-deftest kao-object-custom-inner-and-whole ()
  "`<a-i> c' / `<a-a> c' with a multi-char desc select inner/whole."
  ;; "x begin a end y": begin=3..7, a=9, end=11..13
  (kao-object-tests--nested "x begin a end y" '((9 . 9))
    (kao-object-tests--with-desc "begin,end"
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?c)))
        (kao-select-inner)
        (should (equal (kao-object-tests--spans) '((8 . 10))))
        (setq kao--sels (kao-sels-make
                         :list (list (kao-sel-make :anchor 9 :cursor 9))
                         :main 0))
        (kao-select-whole)
        (should (equal (kao-object-tests--spans) '((3 . 13))))))))

(ert-deftest kao-object-custom-count-selects-parent ()
  "`2 <a-a> c' selects the level-1 (parent) custom pair."
  ;; "begin begin x end end": b1..5, b7..11, x13, e15..17, e19..21
  (kao-object-tests--nested "begin begin x end end" '((13 . 13))
    (kao-object-tests--with-desc "begin,end"
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?c)))
        (setq kao--count 2)
        (kao-select-whole)
        (should (equal (kao-object-tests--spans) '((1 . 21))))))))

(ert-deftest kao-object-custom-nested-walk ()
  "`<a-I> c' selects every depth-1 custom span within the selection."
  (kao-object-tests--nested "begin a end begin b end" '((1 . 23))
    (kao-object-tests--with-desc "begin,end"
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?c)))
        (kao-select-nested-inner)
        (should (equal (kao-object-tests--spans) '((6 . 8) (18 . 20))))))))

(ert-deftest kao-object-custom-nested-quote-style ()
  "`<a-I> c' with open == close alternates like quotes."
  ;; "a %% b %% c": %%=4..5/9..10 -> inner 6..8
  (kao-object-tests--nested "a %% b %% c" '((1 . 11))
    (kao-object-tests--with-desc "%%,%%"
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?c)))
        (kao-select-nested-inner)
        (should (equal (kao-object-tests--spans) '((5 . 7))))))))

(ert-deftest kao-object-custom-repeat-no-reprompt ()
  "`<a-.>' after `c' repeats the captured desc WITHOUT re-prompting.
select_and_set_last records the selector closure (normal.cc:144); the
desc lives in it."
  (kao-object-tests--nested "x begin a end y" '((9 . 9))
    (kao-object-tests--with-desc "begin,end"
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?c)))
        (kao-select-inner))
      (should (= calls 1))
      ;; move off, repeat: the thunk re-runs with the captured desc
      (setq kao--sels (kao-sels-make
                       :list (list (kao-sel-make :anchor 9 :cursor 9))
                       :main 0))
      (funcall kao--last-select)
      (should (= calls 1))                       ; read-string NOT called again
      (should (equal (kao-object-tests--spans) '((8 . 10)))))))

(ert-deftest kao-object-rx-empty-open-guard-to-begin ()
  "A zero-width open match takes the empty-match guard (selectors.cc:346):
last starts one past the empty match, so the closing scan finds the NEXT
zero-width close and the whole span is the single cursor char, not nil."
  (let ((w (with-temp-buffer
             (insert "abcdefgh")
             (kao-object--surrounding (kao-sel-make :anchor 6 :cursor 6)
                                      nil "q*" "q*" t t))))
    (should w)
    (should (= (kao-sel-anchor w) 6))
    (should (= (kao-sel-cursor w) 6))))

(ert-deftest kao-object-rx-empty-open-guard-skip-at-cursor ()
  "ToBegin unset: a zero-width open match AT the cursor is skipped one past
\(the :361 guard), so the to-end variant lands on the cursor char, not nil."
  (let ((w (with-temp-buffer
             (insert "abcdefgh")
             (kao-object--surrounding (kao-sel-make :anchor 6 :cursor 6)
                                      nil "q*" "q*" nil t))))
    (should w)
    (should (= (kao-sel-anchor w) 6))
    (should (= (kao-sel-cursor w) 6))))

;;;; Case-fold in the object regex matcher

(ert-deftest kao-object-casefold-rx-matches ()
  "`kao-object--rx-matches': case-sensitive default, `(?i)' folds (token stripped)."
  (let ((kao-search-case-fold nil))
    (with-temp-buffer
      (insert "FOO foo")
      (should (= (length (kao-object--rx-matches "foo" (point-min) (point-max))) 1))
      (should (= (length (kao-object--rx-matches "(?i)foo" (point-min) (point-max))) 2)))))

(ert-deftest kao-object-casefold-find-surrounding ()
  "The pair-matcher folds with `(?i)' open/close; case-sensitive by default."
  (let ((kao-search-case-fold nil))
    (with-temp-buffer
      (insert "XhelloY")                        ; X=1 hello=2-6 Y=7
      ;; cursor at 4 (inside hello), whole pair: (?i)x..(?i)y matches X..Y
      (let ((r (kao-object--find-surrounding 4 "(?i)x" "(?i)y" nil t t 0)))
        (should r)
        (should (= (car r) 1))                  ; open match at X
        (should (= (cdr r) 7)))                 ; close match at Y
      ;; without (?i): lowercase x/y do not match X/Y -> no enclosing pair
      (should-not (kao-object--find-surrounding 4 "x" "y" nil t t 0)))))

;;;; Public `kao-object-bounds' — the surround substrate primitive (A5 family)

(ert-deftest kao-object-bounds-whole-pair ()
  "`kao-object-bounds' returns the WHOLE enclosing pair for a cursor inside it."
  (with-temp-buffer
    (insert "a(bcd)ef")                          ; (=2 b=3 c=4 d=5 )=6
    (let ((b (kao-object-bounds ?\( (kao-sel-make :anchor 4 :cursor 4))))
      (should b)
      (should (= (kao-sel-min b) 2))             ; the (
      (should (= (kao-sel-max b) 6)))))          ; the )

(ert-deftest kao-object-bounds-inner-pair ()
  "`kao-object-bounds' with INNER returns the contents between the delimiters."
  (with-temp-buffer
    (insert "a(bcd)ef")
    (let ((b (kao-object-bounds ?\( (kao-sel-make :anchor 4 :cursor 4) t)))
      (should b)
      (should (= (kao-sel-min b) 3))             ; b
      (should (= (kao-sel-max b) 5)))))          ; d  -> "bcd"

(ert-deftest kao-object-bounds-no-pair-nil ()
  "`kao-object-bounds' returns nil when the cursor has no enclosing pair."
  (with-temp-buffer
    (insert "abcdef")
    (should (null (kao-object-bounds ?\( (kao-sel-make :anchor 3 :cursor 3))))))

(ert-deftest kao-object-bounds-level-selects-outer ()
  "LEVEL selects the count-th enclosing pair (1 = the next one out)."
  (with-temp-buffer
    (insert "((x))")                             ; (=1 (=2 x=3 )=4 )=5
    (let ((b0 (kao-object-bounds ?\( (kao-sel-make :anchor 3 :cursor 3) nil 0))
          (b1 (kao-object-bounds ?\( (kao-sel-make :anchor 3 :cursor 3) nil 1)))
      (should (= (kao-sel-min b0) 2)) (should (= (kao-sel-max b0) 4))
      (should (= (kao-sel-min b1) 1)) (should (= (kao-sel-max b1) 5)))))

(ert-deftest kao-object-bounds-resolves-runtime-key ()
  "A `kao-object-register'-ed runtime key resolves through `kao-object-bounds'."
  (let ((kao--object-runtime-table nil)
        (kao--object-runtime-info nil)
        (kao--object-runtime-nested nil))
    (kao-object-register
     ?Z (lambda (_sel _inner _to-begin _to-end &optional _level)
          (kao-sel-make :anchor 1 :cursor 2)))
    (with-temp-buffer
      (insert "abc")
      (let ((b (kao-object-bounds ?Z (kao-sel-make :anchor 1 :cursor 1))))
        (should b)
        (should (= (kao-sel-anchor b) 1))
        (should (= (kao-sel-cursor b) 2))))))

(ert-deftest kao-object-bounds-unknown-key-nil ()
  "An unbound KEY yields nil (no selector)."
  (with-temp-buffer
    (insert "abc")
    (should (null (kao-object-bounds ?z (kao-sel-make :anchor 1 :cursor 1))))))

(provide 'kao-object-tests)
;;; kao-object-tests.el ends here
