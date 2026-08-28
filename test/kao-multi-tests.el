;;; kao-multi-tests.el --- Tests for kao-multi -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the P2 selection algebra: creation (% / <a-s>), main management
;; (rotate / keep / remove), and regex selection (s / S / <a-k> / <a-K>).  Each
;; enables `kao-mode', seeds the selection list, runs a command (regex commands
;; via their `*-apply' core), and checks the resulting list + main index.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-state)
(require 'kao-multi)
(require 'kao-edit)                     ; for the select->delete integration test
(require 'kao-keys)                    ; default bindings
(require 'kao-search)                  ;: `n' walks an `s'-written `/' pattern

(defmacro kao-multi-tests--with (content &rest body)
  "Run BODY in a `kao-mode' temp buffer of CONTENT (point at `point-min').
A clean, isolated system clipboard is bound so clipboard-aware paste
reads the internal register path deterministically."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (kao-mode 1)
     (let ((kill-ring nil) (kill-ring-yank-pointer nil)
           (interprogram-cut-function nil) (interprogram-paste-function nil)
           (kao--clipboard-yank nil))
       (unwind-protect (progn ,@body)
         (kao-mode -1)))))

(defun kao-multi-tests--span (anchor cursor &optional main)
  "Set `kao--sels' to a single selection ANCHOR..CURSOR (MAIN defaults to 0)."
  (setq kao--sels (kao-sels-make
                   :list (list (kao-sel-make :anchor anchor :cursor cursor))
                   :main (or main 0))))

(defun kao-multi-tests--list (sels &optional main)
  "Set `kao--sels' from SELS, a list of (anchor . cursor) pairs."
  (setq kao--sels (kao-sels-make
                   :list (mapcar (lambda (p) (kao-sel-make :anchor (car p) :cursor (cdr p)))
                                 sels)
                   :main (or main 0))))

(defun kao-multi-tests--pairs ()
  "Return `kao--sels' as a list of (anchor . cursor) pairs."
  (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
          (kao-sels-list kao--sels)))

(defmacro kao-multi-tests--with-key (key &rest body)
  "Run BODY with `read-key' stubbed to return KEY once."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'read-key) (lambda (&optional _p) ,key)))
     ,@body))

;;;; % — select whole buffer

(ert-deftest kao-multi-whole-buffer ()
  "`%' selects the whole buffer as one selection (anchor pmin, cursor last char)."
  (kao-multi-tests--with "abc\ndef"               ; pmax = 8
    (kao-multi-tests--span 5 5)
    (kao-select-whole-buffer)
    (should (= 1 (length (kao-sels-list kao--sels))))
    (should (= 0 (kao-sels-main kao--sels)))
    (let ((s (car (kao-sels-list kao--sels))))
      (should (= (kao-sel-anchor s) 1))
      (should (= (kao-sel-cursor s) 7))           ; clamped to pmax-1
      (should (eq (kao-sel-target s) 'eol)))))    ;: max_column sticky-eol

(ert-deftest kao-multi-whole-buffer-trailing-newline ()
  "`%' on a buffer ending in a newline clamps the cursor to the last char."
  (kao-multi-tests--with "ab\n"                   ; a1 b2 \n3, pmax 4
    (kao-multi-tests--span 1 1)
    (kao-select-whole-buffer)
    (let ((s (car (kao-sels-list kao--sels))))
      (should (= (kao-sel-anchor s) 1))
      (should (= (kao-sel-cursor s) 3)))))         ; pmax-1 = the final newline

;;;; <a-s> — split on line ends

(ert-deftest kao-multi-split-lines-three ()
  "`<a-s>' splits a 3-line selection into one selection per line; main = last."
  (kao-multi-tests--with "abc\ndef\nghi"          ; \n at 4 and 8, pmax 12
    (kao-multi-tests--span 1 11)                  ; whole buffer, forward
    (kao-split-lines)
    (should (equal (kao-multi-tests--pairs) '((1 . 4) (5 . 8) (9 . 11))))
    (should (= 2 (kao-sels-main kao--sels)))))    ; last selection

(ert-deftest kao-multi-split-lines-single-line-unchanged ()
  "A single-line selection is left intact by `<a-s>'."
  (kao-multi-tests--with "abc"
    (kao-multi-tests--span 1 3)
    (kao-split-lines)
    (should (equal (kao-multi-tests--pairs) '((1 . 3))))
    (should (= 0 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-split-lines-keeps-backward-direction ()
  "Split chunks preserve a backward selection's direction (cursor < anchor)."
  (kao-multi-tests--with "ab\ncd"                 ; \n at 3, pmax 6
    (kao-multi-tests--span 5 1)                   ; backward whole selection
    (kao-split-lines)
    (should (equal (kao-multi-tests--pairs) '((3 . 1) (5 . 4))))
    (dolist (s (kao-sels-list kao--sels))
      (should-not (kao-sel-forward-p s)))))

;;;; Counted N<a-s> (N-lines-per-chunk, normal.cc:1192-1218)

(ert-deftest kao-multi-split-lines-counted-two ()
  "`2<a-s>' splits a 6-line selection into 3 two-line chunks; main = last."
  ;; a=1 \n=2 b=3 \n=4 c=5 \n=6 d=7 \n=8 e=9 \n=10 f=11, pmax 12
  (kao-multi-tests--with "a\nb\nc\nd\ne\nf"
    (kao-multi-tests--span 1 11)                  ; whole buffer, forward
    (setq kao--count 2)
    (kao-split-lines)
    (should (equal (kao-multi-tests--pairs) '((1 . 4) (5 . 8) (9 . 11))))
    (should (= 2 (kao-sels-main kao--sels)))))    ; last chunk

(ert-deftest kao-multi-split-lines-counted-final-chunk-clamped ()
  "An odd line count under `2<a-s>' clamps the short final chunk to one line."
  ;; a=1 \n=2 b=3 \n=4 c=5 \n=6 d=7 \n=8 e=9, pmax 10 — 5 lines
  (kao-multi-tests--with "a\nb\nc\nd\ne"
    (kao-multi-tests--span 1 9)
    (setq kao--count 2)
    (kao-split-lines)
    (should (equal (kao-multi-tests--pairs) '((1 . 4) (5 . 8) (9 . 9))))))

(ert-deftest kao-multi-split-lines-count-exceeds-lines ()
  "A count larger than the spanned lines yields one chunk = the whole selection."
  ;; a=1 \n=2 b=3 \n=4 c=5, pmax 6 — 3 lines
  (kao-multi-tests--with "a\nb\nc"
    (kao-multi-tests--span 1 5)
    (setq kao--count 5)
    (kao-split-lines)
    (should (equal (kao-multi-tests--pairs) '((1 . 5))))
    (should (= 0 (kao-sels-main kao--sels)))))

;;;; ) ( — rotate main

(ert-deftest kao-multi-rotate-forward ()
  "`)' moves the main forward by the count, wrapping modularly."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((1 . 1) (3 . 3) (5 . 5)) 0)
    (kao-rotate-selections-forward)
    (should (= 1 (kao-sels-main kao--sels)))       ; 0 -> 1
    (setq kao--count 2)
    (kao-rotate-selections-forward)
    (should (= 0 (kao-sels-main kao--sels)))))      ; 1 + 2 = 3 % 3 = 0

(ert-deftest kao-multi-rotate-backward-wraps ()
  "`(' moves the main backward, wrapping from 0 to the last."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((1 . 1) (3 . 3) (5 . 5)) 0)
    (kao-rotate-selections-backward)
    (should (= 2 (kao-sels-main kao--sels)))))      ; 0 -> last (2)

(ert-deftest kao-multi-rotate-noop-single ()
  "Rotating with one selection is a no-op."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--span 1 1)
    (kao-rotate-selections-forward)
    (should (= 0 (kao-sels-main kao--sels)))
    (should (= 1 (length (kao-sels-list kao--sels))))))

;;;;, — keep selection

(ert-deftest kao-multi-keep-main ()
  "`,' keeps only the main selection (main becomes 0)."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((1 . 1) (3 . 3) (5 . 5)) 1)
    (kao-keep-selection)
    (should (equal (kao-multi-tests--pairs) '((3 . 3))))
    (should (= 0 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-keep-count-th ()
  "`3,' keeps the third selection (1-based count)."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((1 . 1) (3 . 3) (5 . 5)) 0)
    (setq kao--count 3)
    (kao-keep-selection)
    (should (equal (kao-multi-tests--pairs) '((5 . 5))))))

(ert-deftest kao-multi-keep-bad-index-noop ()
  "An out-of-range count leaves the selection list unchanged."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((1 . 1) (3 . 3) (5 . 5)) 0)
    (setq kao--count 5)
    (kao-keep-selection)
    (should (= 3 (length (kao-sels-list kao--sels))))))

;;;; <a-,> — remove selection

(ert-deftest kao-multi-remove-main-keeps-index ()
  "Removing the (non-last) main keeps the main index, now on the next selection."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((1 . 1) (3 . 3) (5 . 5)) 1)
    (kao-remove-selection)
    (should (equal (kao-multi-tests--pairs) '((1 . 1) (5 . 5))))
    (should (= 1 (kao-sels-main kao--sels)))))      ; now points to (5 . 5)

(ert-deftest kao-multi-remove-before-main-decrements ()
  "Removing a selection before the main decrements the main index."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((1 . 1) (3 . 3) (5 . 5)) 2)
    (setq kao--count 1)                             ; remove the 1st selection
    (kao-remove-selection)
    (should (equal (kao-multi-tests--pairs) '((3 . 3) (5 . 5))))
    (should (= 1 (kao-sels-main kao--sels)))))      ; still on (5 . 5)

(ert-deftest kao-multi-remove-last-main-clamps ()
  "Removing the last selection when it is the main clamps the main to the new last."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((1 . 1) (3 . 3) (5 . 5)) 2)
    (kao-remove-selection)
    (should (equal (kao-multi-tests--pairs) '((1 . 1) (3 . 3))))
    (should (= 1 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-remove-single-refused ()
  "Removing the only selection is refused (list unchanged)."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--span 3 3)
    (kao-remove-selection)
    (should (= 1 (length (kao-sels-list kao--sels))))))

(ert-deftest kao-multi-keys-bound ()
  "% <a-s> ) ( , <a-,> are bound in the normal-state map."
  (should (eq (lookup-key kao-normal-state-map "%") #'kao-select-whole-buffer))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-s")) #'kao-split-lines))
  (should (eq (lookup-key kao-normal-state-map ")") #'kao-rotate-selections-forward))
  (should (eq (lookup-key kao-normal-state-map "(") #'kao-rotate-selections-backward))
  (should (eq (lookup-key kao-normal-state-map ",") #'kao-keep-selection))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-,")) #'kao-remove-selection)))

;;;; s — select regex matches

(ert-deftest kao-multi-select-regex-single-chars ()
  "`s' with \"a\" over \"banana\" selects each 'a' as a one-char selection; main=last."
  (kao-multi-tests--with "banana"                 ; b1 a2 n3 a4 n5 a6
    (kao-multi-tests--span 1 6)
    (kao--select-regex-apply "a")
    (should (equal (kao-multi-tests--pairs) '((2 . 2) (4 . 4) (6 . 6))))
    (should (= 2 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-select-regex-multichar ()
  "`s' with a multi-char pattern selects whole matches (cursor on last char)."
  (kao-multi-tests--with "foo bar foo"            ; foo at [1,4) and [9,12)
    (kao-multi-tests--span 1 11)
    (kao--select-regex-apply "foo")
    (should (equal (kao-multi-tests--pairs) '((1 . 3) (9 . 11))))
    (should (= 1 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-select-regex-keeps-direction ()
  "Matches of `s' inherit the original selection's direction."
  (kao-multi-tests--with "abXab"                  ; a1 b2 X3 a4 b5
    (kao-multi-tests--span 5 1)                   ; backward whole selection
    (kao--select-regex-apply "ab")
    (should (equal (kao-multi-tests--pairs) '((2 . 1) (5 . 4))))
    (dolist (s (kao-sels-list kao--sels))
      (should-not (kao-sel-forward-p s)))))

(ert-deftest kao-multi-select-regex-no-match-noop ()
  "A pattern that matches nothing leaves the selection list unchanged."
  (kao-multi-tests--with "abc"
    (kao-multi-tests--span 1 3)
    (kao--select-regex-apply "z")
    (should (equal (kao-multi-tests--pairs) '((1 . 3))))))

(ert-deftest kao-multi-select-regex-reads-prompt ()
  "`kao-select-regex' (incsearch off) reads via `kao--read-regex' then applies it."
  (kao-multi-tests--with "banana"
    (kao-multi-tests--span 1 6)
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "an")))
        (kao-select-regex)))
    (should (equal (kao-multi-tests--pairs) '((2 . 3) (4 . 5))))))  ; "an" at 2 and 4

;;;; Ns — count selects the capture group (select_matches capture_idx)

(ert-deftest kao-multi-select-regex-capture-group ()
  "`2s' selects each match's group 2; full submatch list still carried."
  (kao-multi-tests--with "a1 b2"                  ; a1 12 sp3 b4 25
    (kao-multi-tests--span 1 5)
    (kao--select-regex-apply "\\([a-z]\\)\\([0-9]\\)" 2)
    (should (equal (kao-multi-tests--pairs) '((2 . 2) (5 . 5))))
    (should (= 1 (kao-sels-main kao--sels)))
    (should (equal (kao-sel-captures (nth 0 (kao-sels-list kao--sels)))
                   '("a1" "a" "1")))))

(ert-deftest kao-multi-select-regex-capture-unmatched-skipped ()
  "A match whose group did not participate is skipped (capture.matched,
selectors.cc:1168)."
  (kao-multi-tests--with "y x y"                  ; y1 sp2 x3 sp4 y5
    (kao-multi-tests--span 1 5)
    (kao--select-regex-apply "\\(x\\)\\|y" 1)
    (should (equal (kao-multi-tests--pairs) '((3 . 3))))
    (should (equal (kao-sel-captures (nth 0 (kao-sels-list kao--sels)))
                   '("x" "x")))))

(ert-deftest kao-multi-select-regex-capture-zero-width-group ()
  "A zero-width group yields a single-position selection at the group start
\(begin == end ? end : prev(end), selectors.cc:1177-1180)."
  (kao-multi-tests--with "abc"
    (kao-multi-tests--span 1 3)
    (kao--select-regex-apply "a\\(\\)b" 1)
    (should (equal (kao-multi-tests--pairs) '((2 . 2))))))

(ert-deftest kao-multi-select-regex-capture-at-sel-end-skipped ()
  "A group starting at the selection's exclusive end is skipped
\(capture.first == sel_end, selectors.cc:1168)."
  (kao-multi-tests--with "abc"
    (kao-multi-tests--span 1 2)                   ; sel \"ab\", exclusive end 3
    (kao--select-regex-apply "b\\(\\)" 1)         ; group at [3,3) = sel_end
    (should (equal (kao-multi-tests--pairs) '((1 . 2))))))  ; nothing selected

(ert-deftest kao-multi-select-regex-capture-invalid-number ()
  "A capture index beyond the regex's group count leaves the list unchanged
and messages Kakoune's \"invalid capture number\"."
  (kao-multi-tests--with "abc"
    (kao-multi-tests--span 1 3)
    (let ((logged nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq logged (apply #'format fmt args)))))
        (kao--select-regex-apply "\\(a\\)" 2))
      (should (equal logged "kao: invalid capture number")))
    (should (equal (kao-multi-tests--pairs) '((1 . 3))))))

(ert-deftest kao-multi-select-regex-capture-prompt-and-count ()
  "`kao-select-regex' reads the raw count as the capture index and shows the
faithful \"select (capture N):\" prompt (normal.cc:1163-1166)."
  (kao-multi-tests--with "a1 b2"
    (kao-register-set ?/ nil)                 ; empty register -> no `format-prompt' default
    (kao-multi-tests--span 1 5)
    (let ((kao-incsearch nil)
          (prompt-seen nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _) (setq prompt-seen prompt)
                   "\\([a-z]\\)\\([0-9]\\)")))
        (setq kao--count 2)
        (kao-select-regex))
      (should (equal prompt-seen "select (capture 2):")))
    (should (equal (kao-multi-tests--pairs) '((2 . 2) (5 . 5))))))

;;;; Zero-width matches — s keeps them (RegexIterator null matches)

(ert-deftest kao-multi-select-regex-zero-width-bol ()
  "`s^' puts a single-position cursor at every line start (zero-width match kept).
Kakoune `%s^' is the standard cursor-per-line idiom; kao dropped these."
  (kao-multi-tests--with "foo\nbar\nbaz"           ; bol at 1, 5, 9
    (kao-multi-tests--span 1 11)                   ; whole buffer (cursor clamped)
    (kao--select-regex-apply "^")
    (should (equal (kao-multi-tests--pairs) '((1 . 1) (5 . 5) (9 . 9))))
    (should (= 2 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-select-regex-zero-width-eol-no-signal ()
  "`s$' selects each line end WITHOUT signaling end-of-buffer (crash regression).
The zero-width match at point-max = sel_end is skipped by the sel_end guard; the
eob step is `goto-char', not `forward-char', so it clamps instead of crashing."
  (kao-multi-tests--with "foo\nbar\nbaz"           ; eol at 4, 8, and eob 12=sel_end
    (kao-multi-tests--span 1 11)
    (kao--select-regex-apply "$")                  ; must not signal
    (should (equal (kao-multi-tests--pairs) '((4 . 4) (8 . 8))))))

(ert-deftest kao-multi-select-regex-zero-width-at-sel-end-skipped ()
  "A whole-match zero-width match at the selection's exclusive end is skipped
\(`capture.first == sel_end'), while an interior one is kept: `s\\b' over \"ab\"."
  (kao-multi-tests--with "ab cd"                   ; word bounds 1,3,4,6
    (kao-multi-tests--span 1 2)                    ; sel \"ab\", exclusive end 3
    (kao--select-regex-apply "\\b")                ; bow at 1 kept, eow at 3 skipped
    (should (equal (kao-multi-tests--pairs) '((1 . 1))))))

;;;; incsearch live-preview (Kakoune incsearch, regex_prompt)

(ert-deftest kao-multi-incsearch-default-on ()
  "`kao-incsearch' defaults on (faithful: Kakoune incsearch is on by default)."
  (should (eq kao-incsearch t)))

(ert-deftest kao-multi-incsearch-preview-non-cumulative ()
  "Previewing \"an\" after \"a\" re-derives from the ORIGINAL selections, not the \"a\"
result — the Kakoune `selections_write_only() = selections' restore-then-apply."
  (kao-multi-tests--with "banana"                 ; b1 a2 n3 a4 n5 a6
    (kao-multi-tests--span 1 6)
    (let ((orig (kao--snapshot-sels kao--sels)))
      (kao--incsearch-preview #'kao--select-regex-apply orig "a")
      (should (equal (kao-multi-tests--pairs) '((2 . 2) (4 . 4) (6 . 6))))
      ;; Second preview from the same originals — must equal a fresh "an", not
      ;; "an" applied on top of the "a" matches.
      (kao--incsearch-preview #'kao--select-regex-apply orig "an")
      (should (equal (kao-multi-tests--pairs) '((2 . 3) (4 . 5)))))))

(ert-deftest kao-multi-incsearch-preview-empty-restores ()
  "An empty/invalid preview regex restores the original selections."
  (kao-multi-tests--with "banana"
    (kao-multi-tests--span 1 6)
    (let ((orig (kao--snapshot-sels kao--sels)))
      (kao--incsearch-preview #'kao--select-regex-apply orig "a")
      (kao--incsearch-preview #'kao--select-regex-apply orig "")
      (should (equal (kao-multi-tests--pairs) '((1 . 6)))))))

(ert-deftest kao-multi-incsearch-commit ()
  "`kao-select-regex' with incsearch on commits the validated regex (RET)."
  (kao-multi-tests--with "banana"
    (kao-multi-tests--span 1 6)
    (let ((kao-incsearch t))
      (cl-letf (((symbol-function 'read-from-minibuffer) (lambda (&rest _) "an")))
        (kao-select-regex)))
    (should (equal (kao-multi-tests--pairs) '((2 . 3) (4 . 5))))))

(ert-deftest kao-multi-incsearch-abort-restores ()
  "Aborting the incsearch prompt (C-g/quit) leaves the selections unchanged."
  (kao-multi-tests--with "banana"
    (kao-multi-tests--span 1 6)
    (let ((kao-incsearch t))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) (signal 'quit nil))))
        (kao-select-regex)))
    (should (equal (kao-multi-tests--pairs) '((1 . 6))))))

(ert-deftest kao-multi-incsearch-empty-commit-noop ()
  "Validating an empty incsearch prompt WITH AN EMPTY REGISTER commits nothing.
Kakoune also no-ops here: `regex_prompt' substitutes `default_regex' for an
empty entry, but the default is itself empty so the resulting regex is empty
and the selector's `if (regex.empty())' guard fires (normal.cc:1090/1172).
The register-backed fallback is exercised separately."
  (kao-multi-tests--with "banana"
    (kao-register-set ?/ nil)                       ; empty `/' register
    (kao-multi-tests--span 1 6)
    (let ((kao-incsearch t))
      (cl-letf (((symbol-function 'read-from-minibuffer) (lambda (&rest _) "")))
        (kao-select-regex)))
    (should (equal (kao-multi-tests--pairs) '((1 . 6))))))

;;;; M2 — committing an invalid regex reports it (2026-07-18);
;;;; the live preview already refuses it silently, the commit was the gap.

(ert-deftest kao-multi-incsearch-invalid-commit-reports ()
  "Committing a typed-but-invalid regex restores the originals AND reports it.
Kakoune/isearch echo the compile error on a bad pattern; kao messages
\"kao: invalid regexp\" on the invalid-commit branch of `kao--regex-prompt'
\(the empty-entry/no-default cancel below stays silent).  `[' never compiles."
  (kao-multi-tests--with "banana"
    (kao-multi-tests--span 1 6)
    (let ((logged nil)
          (kao-incsearch t))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq logged (apply #'format fmt args))))
                ((symbol-function 'read-from-minibuffer) (lambda (&rest _) "[")))
        (kao-select-regex))
      (should (equal logged "kao: invalid regexp")))
    (should (equal (kao-multi-tests--pairs) '((1 . 6))))))   ; originals restored

(ert-deftest kao-multi-incsearch-empty-cancel-silent ()
  "An empty entry with no register default is a plain cancel: it restores the
originals WITHOUT the \"kao: invalid regexp\" feedback (only a typed-but-invalid
pattern reports).  Pins the other half of the invalid-commit branch so the fix
cannot over-fire on a silent Kakoune cancel."
  (kao-multi-tests--with "banana"
    (kao-register-set ?/ nil)                       ; empty `/' register -> no default
    (kao-multi-tests--span 1 6)
    (let ((logged nil)
          (kao-incsearch t))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq logged (apply #'format fmt args))))
                ((symbol-function 'read-from-minibuffer) (lambda (&rest _) "")))
        (kao-select-regex))
      (should-not logged))                          ; silent cancel, no message
    (should (equal (kao-multi-tests--pairs) '((1 . 6))))))

;;;; Incsearch memoizes the preview across unchanged post-command fires

(ert-deftest kao-multi-incsearch-memoizes-unchanged-preview ()
  "Two post-command fires with the SAME minibuffer contents apply ONCE.
`kao--regex-prompt' closes over a per-read `last' memo and skips the preview when
the current contents equal the last previewed pattern — the C++ `m_line_changed'
gate (input_handler.cc:790).  The count is taken via a `cl-letf' advice on the
apply fn while the live thunk (`kao--incsearch-active') is fired directly."
  (kao-multi-tests--with "banana"
    (kao-register-set ?/ nil)               ; empty register: the "" commit is a no-op
    (kao-multi-tests--span 1 6)
    (let ((applied 0))
      (cl-letf* ((real (symbol-function 'kao--select-regex-apply))
                 ((symbol-function 'kao--select-regex-apply)
                  (lambda (&rest args) (setq applied (1+ applied))
                    (apply real args)))
                 ;; The stub stands in for the minibuffer read: it fires the live
                 ;; preview thunk twice with unchanged contents, then returns "".
                 ((symbol-function 'minibuffer-contents-no-properties)
                  (lambda () "an"))
                 ((symbol-function 'read-from-minibuffer)
                  (lambda (&rest _)
                    (funcall kao--incsearch-active)   ; fire 1: previews "an"
                    (funcall kao--incsearch-active)   ; fire 2: unchanged -> skipped
                    "")))
        (let ((kao-incsearch t))
          (kao-select-regex)))
      (should (= applied 1)))))               ; the second, unchanged, fire is free

;;;; Empty prompt falls back to the register's pattern (regex_prompt
;;;; default_regex, normal.cc:958/:1021)

(ert-deftest kao-multi-select-regex-empty-commit-reapplies-register ()
  "`s<ret>' with a stored `/' pattern re-applies it (default_regex fallback).
The empty entry falls back to the register's first string WITHOUT rewriting it
\(normal.cc:1016 skips the register write on an empty validate)."
  (kao-multi-tests--with "banana"                   ; b1 a2 n3 a4 n5 a6
    (kao-register-set ?/ '("an"))
    (kao-multi-tests--span 1 6)
    (let ((kao-incsearch t))
      (cl-letf (((symbol-function 'read-from-minibuffer) (lambda (&rest _) "")))
        (kao-select-regex)))
    (should (equal (kao-multi-tests--pairs) '((2 . 3) (4 . 5))))  ; "an" re-applied
    (should (equal (kao-register-get ?/) '("an")))))              ; not rewritten

(ert-deftest kao-multi-select-regex-empty-commit-reapplies-register-incsearch-off ()
  "Same fallback in the one-shot `kao--read-regex' path (incsearch off)."
  (kao-multi-tests--with "banana"
    (kao-register-set ?/ '("an"))
    (kao-multi-tests--span 1 6)
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
        (kao-select-regex)))
    (should (equal (kao-multi-tests--pairs) '((2 . 3) (4 . 5))))
    (should (equal (kao-register-get ?/) '("an")))))

(ert-deftest kao-multi-regex-default-reads-main-indexed-register ()
  "The empty-entry regex default reads the register at the MAIN selection index.
`main_sel_register_value' indexes a redirected TEXT register by the main
selection, last-clamped (`content[min(main, size-1)]',
register_manager.cc:36-42), so `\"as<ret>' with register a's main pointing at
the third string defaults to that string, not the front.  The `/' history
register is last-value-only, so main-indexing clamps to its single front value
— the `get_main = front' invariant holds unchanged."
  (kao-multi-tests--with "abcdef"                      ; a1 b2 c3 d4 e5 f6
    (kao-multi-tests--list '((1 . 1) (3 . 3) (5 . 5)) 2)   ; 3 sels, main index 2
    (kao-register-set ?a '("aa" "bb" "cc"))            ; 3 valid-regex strings
    (should (equal (kao--regex-prompt-default ?a) "cc"))   ; main 2 -> 3rd string
    (kao-register-set ?a '("aa" "bb"))                 ; main past end...
    (should (equal (kao--regex-prompt-default ?a) "bb"))   ; ...clamps to last
    (kao-register-set ?/ '("zz"))                      ; history reg: one value
    (should (equal (kao--regex-prompt-default ?/) "zz"))))  ; still front

;;;; S — split on regex matches

(ert-deftest kao-multi-split-regex-basic ()
  "`S' on \"a,b,c\" with \",\" yields the three between-comma segments; main=last."
  (kao-multi-tests--with "a,b,c"                  ; a1 ,2 b3 ,4 c5
    (kao-multi-tests--span 1 5)
    (kao--split-regex-apply ",")
    (should (equal (kao-multi-tests--pairs) '((1 . 1) (3 . 3) (5 . 5))))
    (should (= 2 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-split-regex-leading-delimiter ()
  "A match at the selection start produces no empty leading segment."
  (kao-multi-tests--with ",ab"                    ; ,1 a2 b3
    (kao-multi-tests--span 1 3)
    (kao--split-regex-apply ",")
    (should (equal (kao-multi-tests--pairs) '((2 . 3))))
    (should (= 0 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-split-regex-adjacent-matches ()
  "Adjacent delimiters yield a single-position selection between them."
  (kao-multi-tests--with "a,,b"                   ; a1 ,2 ,3 b4
    (kao-multi-tests--span 1 4)
    (kao--split-regex-apply ",")
    (should (equal (kao-multi-tests--pairs) '((1 . 1) (3 . 3) (4 . 4))))))

(ert-deftest kao-multi-split-regex-no-match-keeps-whole ()
  "Splitting on a non-matching pattern keeps the selection as one segment."
  (kao-multi-tests--with "abc"
    (kao-multi-tests--span 1 3)
    (kao--split-regex-apply "z")
    (should (equal (kao-multi-tests--pairs) '((1 . 3))))))

(ert-deftest kao-multi-split-regex-zero-width-bol-per-line ()
  "`S^' splits per line: each zero-width bol delimiter opens a new segment."
  (kao-multi-tests--with "foo\nbar\nbaz"           ; bol at 1, 5, 9
    (kao-multi-tests--span 1 11)
    (kao--split-regex-apply "^")
    (should (equal (kao-multi-tests--pairs) '((1 . 4) (5 . 8) (9 . 11))))
    (should (= 2 (kao-sels-main kao--sels)))))

;;;; NS — count splits on the capture group (split_on_matches capture_idx)

(ert-deftest kao-multi-split-regex-capture-group ()
  "`1S' splits on group 1: match chars BEFORE the group stay in the
preceding segment (end = capture.first, selectors.cc:1208)."
  (kao-multi-tests--with "ax-b"                   ; a1 x2 -3 b4
    (kao-multi-tests--span 1 4)
    (kao--split-regex-apply "x\\(-\\)" 1)
    (should (equal (kao-multi-tests--pairs) '((1 . 2) (4 . 4))))))  ; "ax" "b"

(ert-deftest kao-multi-split-regex-capture-tail-joins-next ()
  "Match chars AFTER the group join the next segment (begin = capture.second,
selectors.cc:1217)."
  (kao-multi-tests--with "a-yb"                   ; a1 -2 y3 b4
    (kao-multi-tests--span 1 4)
    (kao--split-regex-apply "\\(-\\)y" 1)
    (should (equal (kao-multi-tests--pairs) '((1 . 1) (3 . 4))))))  ; "a" "yb"

(ert-deftest kao-multi-split-regex-capture-unmatched-not-delimiter ()
  "A match whose group did not participate is not a delimiter."
  (kao-multi-tests--with "a,b;c"                  ; a1 ,2 b3 ;4 c5
    (kao-multi-tests--span 1 5)
    (kao--split-regex-apply "\\(,\\)\\|;" 1)
    (should (equal (kao-multi-tests--pairs) '((1 . 1) (3 . 5))))))  ; "a" "b;c"

(ert-deftest kao-multi-split-regex-capture-at-eob-not-delimiter ()
  "A group starting at point-max is not a delimiter (end == buf_end guard,
selectors.cc:1209-1210)."
  (kao-multi-tests--with "ab"                     ; a1 b2; point-max 3
    (kao-multi-tests--span 1 2)
    (kao--split-regex-apply "b\\(\\)" 1)          ; group at [3,3) = point-max
    (should (equal (kao-multi-tests--pairs) '((1 . 2))))))

(ert-deftest kao-multi-split-regex-capture-invalid-number ()
  "A capture index beyond the regex's group count leaves the list unchanged
and messages Kakoune's \"invalid capture number\"."
  (kao-multi-tests--with "a,b"
    (kao-multi-tests--span 1 3)
    (let ((logged nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq logged (apply #'format fmt args)))))
        (kao--split-regex-apply "\\(,\\)" 9))
      (should (equal logged "kao: invalid capture number")))
    (should (equal (kao-multi-tests--pairs) '((1 . 3))))))

(ert-deftest kao-multi-split-regex-capture-prompt-and-count ()
  "`kao-split-regex' reads the raw count as the capture index and shows the
faithful \"split (on capture N):\" prompt (normal.cc:1178-1181)."
  (kao-multi-tests--with "ax-b"
    (kao-register-set ?/ nil)                 ; empty register -> no `format-prompt' default
    (kao-multi-tests--span 1 4)
    (let ((kao-incsearch nil)
          (prompt-seen nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _) (setq prompt-seen prompt) "x\\(-\\)")))
        (setq kao--count 1)
        (kao-split-regex))
      (should (equal prompt-seen "split (on capture 1):")))
    (should (equal (kao-multi-tests--pairs) '((1 . 2) (4 . 4))))))

;;;; <a-k> / <a-K> — keep / keep-not-matching

(ert-deftest kao-multi-keep-matching ()
  "`<a-k>' keeps only selections that contain a match; main = last kept."
  (kao-multi-tests--with "foo\nbar\nfoo"          ; 3 line selections
    (kao-multi-tests--list '((1 . 4) (5 . 8) (9 . 11)) 0)
    (kao--keep-apply "foo" t)
    (should (equal (kao-multi-tests--pairs) '((1 . 4) (9 . 11))))
    (should (= 1 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-keep-not-matching ()
  "`<a-K>' keeps only selections that do NOT contain a match."
  (kao-multi-tests--with "foo\nbar\nfoo"
    (kao-multi-tests--list '((1 . 4) (5 . 8) (9 . 11)) 0)
    (kao--keep-apply "foo" nil)
    (should (equal (kao-multi-tests--pairs) '((5 . 8))))
    (should (= 0 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-keep-none-kept-noop ()
  "Keeping with a pattern no selection contains leaves the list unchanged."
  (kao-multi-tests--with "foo\nbar\nfoo"
    (kao-multi-tests--list '((1 . 4) (5 . 8) (9 . 11)) 0)
    (kao--keep-apply "zzz" t)
    (should (= 3 (length (kao-sels-list kao--sels))))))

(ert-deftest kao-multi-regex-keys-bound ()
  "s S <a-k> <a-K> are bound in the normal-state map."
  (should (eq (lookup-key kao-normal-state-map "s") #'kao-select-regex))
  (should (eq (lookup-key kao-normal-state-map "S") #'kao-split-regex))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-k")) #'kao-keep-matching))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-K")) #'kao-keep-not-matching)))

;;;; Register plumbing & jump-push shared by s/S/<a-k>/<a-K> (regex_prompt)

(ert-deftest kao-multi-select-regex-writes-search-register ()
  "`s foo RET' stores \"foo\" in the `/' register, so `n' then walks foo."
  (kao-multi-tests--with "foo bar foo"            ; foo at [1,4) and [9,12)
    (kao-register-set ?/ '("stale"))              ; must be overwritten
    (kao-multi-tests--span 1 3)                   ; select the first foo's span
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "foo")))
        (kao-select-regex)))
    (should (equal (kao-register-get ?/) '("foo")))
    (kao-search-next)                             ; `n' reads `/' = foo
    (should (equal (kao-multi-tests--pairs) '((9 . 11))))))

(ert-deftest kao-multi-select-regex-writes-named-register ()
  "`\"a s bar RET' stores the pattern in register a, leaving `/' untouched."
  (kao-multi-tests--with "foo bar foo"
    (remhash ?a kao--registers)
    (kao-register-set ?/ '("old"))
    (kao-multi-tests--span 1 11)
    (setq kao--pending-register ?a)               ; buffer-local: isolated to temp buf
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "bar")))
        (kao-select-regex)))
    (should (equal (kao-register-get ?a) '("bar")))
    (should (equal (kao-register-get ?/) '("old")))   ; `/' untouched
    (remhash ?a kao--registers)))

(ert-deftest kao-multi-select-regex-commit-pushes-jump ()
  "A committed `s' pushes ONE jump = the PRE-prompt selections, not the match."
  (kao-multi-tests--with "foo bar foo"
    (kao-multi-tests--span 1 11)                  ; whole buffer
    (setq kao--jumps nil kao--jump-current 0)
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "bar")))
        (kao-select-regex)))
    (should (equal (kao-multi-tests--pairs) '((5 . 7))))   ; moved to the bar match
    (should (= 1 (length kao--jumps)))
    (should (equal '((1 . 11))                             ; jump = where we were
                   (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                           (kao-sels-list (cddr (car kao--jumps))))))))

(ert-deftest kao-multi-select-regex-abort-pushes-no-jump-no-write ()
  "An aborted `s' (empty prompt, EMPTY register) writes no register, no jump.
With a non-empty `/' register an empty entry would instead re-apply it;
the abort path is exactly the empty-prompt/empty-register case."
  (kao-multi-tests--with "foo bar foo"
    (kao-register-set ?/ nil)                         ; empty register -> empty entry aborts
    (kao-multi-tests--span 1 3)
    (setq kao--jumps nil kao--jump-current 0)
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))  ; empty = abort
        (kao-select-regex)))
    (should (null kao--jumps))
    (should (null (kao-register-get ?/)))))

(ert-deftest kao-multi-keep-matching-writes-register-and-jumps ()
  "`<a-k>' shares the plumbing: it stores its pattern and pushes a jump."
  (kao-multi-tests--with "foo\nbar\nfoo"
    (kao-register-set ?/ '("stale"))
    (kao-multi-tests--list '((1 . 4) (5 . 8) (9 . 11)) 0)
    (setq kao--jumps nil kao--jump-current 0)
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "foo")))
        (kao-keep-matching)))
    (should (equal (kao-multi-tests--pairs) '((1 . 4) (9 . 11))))  ; kept
    (should (equal (kao-register-get ?/) '("foo")))
    (should (= 1 (length kao--jumps)))))

;;;; Integration — create selections then edit them

(ert-deftest kao-multi-select-then-delete ()
  "End-to-end: `%' `s'(\"a\") then `d' deletes every match across the buffer."
  (kao-multi-tests--with "a.a.aZ"                 ; a1 .2 a3 .4 a5 Z6
    (kao-select-whole-buffer)
    (kao--select-regex-apply "a")                 ; 3 one-char selections, main=last
    (kao-delete)
    (should (string= (buffer-string) "..Z"))
    (should (equal (kao-multi-tests--pairs) '((1 . 1) (2 . 2) (3 . 3))))
    (should (= 2 (kao-sels-main kao--sels)))))     ; main rides through the delete

(ert-deftest kao-multi-yank-then-paste-round-trip ()
  "`s' then `y' then `p': each cursor pastes its OWN yanked string (i-th->i-th)."
  (kao-multi-tests--with "a-b-c"                  ; a1 -2 b3 -4 c5
    (kao-select-whole-buffer)
    (kao--select-regex-apply "[abc]")             ; 3 sels at a, b, c
    (kao-yank)
    (should (equal (kao-register-get kao-register-default) '("a" "b" "c")))
    (kao-paste-after)                             ; a after a, b after b, c after c
    (should (string= (buffer-string) "aa-bb-cc"))))

(ert-deftest kao-multi-select-then-insert ()
  "End-to-end: `%' `s'(\"a\") then `i' types a prefix at every match.
On exit each match's ORIGINAL span is kept, not collapsed on the typed char
\(shaped `i')."
  (kao-multi-tests--with "a.a.a"                  ; a1 .2 a3 .4 a5
    (kao-select-whole-buffer)
    (kao--select-regex-apply "a")                 ; 3 sels at a, a, a (main=last)
    (kao-insert)
    (insert ">")                                  ; typed only at the main
    (kao-insert-exit)
    (should (string= (buffer-string) ">a.>a.>a"))
    ;; Shaped `i': each 1-char "a" span is kept, shifted past its own ">".
    (should (equal (kao-multi-tests--pairs) '((2 . 2) (5 . 5) (8 . 8))))))

(ert-deftest kao-multi-select-then-change ()
  "End-to-end: `%' `s'(\"foo\") then `c' changes every match to the typed text.
Each match collapses one char after its typed text (shaped `c')."
  (kao-multi-tests--with "foo bar foo"            ; foo at [1,3] and [9,11]
    (kao-select-whole-buffer)
    (kao--select-regex-apply "foo")               ; 2 sels, main = last
    (kao-change)
    (insert "X")                                  ; typed only at the main
    (kao-insert-exit)
    (should (string= (buffer-string) "X bar X"))
    ;; Shaped `c': collapse one char after each typed "X"; the last "X" is at
    ;; buffer end, so its cursor clamps back onto it.
    (should (equal (kao-multi-tests--pairs) '((2 . 2) (7 . 7))))
    (should (= 1 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-select-then-open-below ()
  "End-to-end: `%' `s'(\"[pqr]\") then `o' opens a line under every line."
  (kao-multi-tests--with "p\nq\nr"                ; p1 \n2 q3 \n4 r5
    (kao-select-whole-buffer)
    (kao--select-regex-apply "[pqr]")             ; one selection per line
    (kao-open-below)
    (insert "-")                                  ; typed only at the main
    (kao-insert-exit)
    (should (string= (buffer-string) "p\n-\nq\n-\nr\n-"))))

;;;; Z / z — save and restore selections

(ert-deftest kao-multi-save-restore-roundtrip ()
  "`Z' then a clobber then `z' restores the saved list and main index."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdefgh"
    (kao-multi-tests--list '((1 . 2) (4 . 4) (6 . 7)) 2)
    (kao-save-selections)
    (kao-multi-tests--span 1 1)                   ; clobber the live list
    (should (= 1 (length (kao-sels-list kao--sels))))
    (kao-restore-selections)
    (should (equal (kao-multi-tests--pairs) '((1 . 2) (4 . 4) (6 . 7))))
    (should (= 2 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-restore-decoupled-from-store ()
  "Editing the live list after a restore does not corrupt the stored snapshot."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdefgh"
    (kao-multi-tests--list '((1 . 2) (4 . 5)) 0)
    (kao-save-selections)
    (kao-restore-selections)
    (setf (kao-sel-cursor (car (kao-sels-list kao--sels))) 7)   ; mutate live
    (kao-restore-selections)                                     ; restore again
    (should (equal (kao-multi-tests--pairs) '((1 . 2) (4 . 5))))))

(ert-deftest kao-multi-restore-clamps-to-bounds ()
  "Restoring a snapshot saved against a larger buffer clamps to current bounds."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdefgh"             ; last on-char = 8
    (kao-register-save-selections
     (kao-sels-make :list (list (kao-sel-make :anchor 5 :cursor 99)) :main 0))
    (kao-restore-selections)
    (should (equal (kao-multi-tests--pairs) '((5 . 8))))))   ; 99 clamped to 8

(ert-deftest kao-multi-restore-translates-through-edits ()
  "`z' after an edit translates the saved coordinates, not just clamps.
The saved id folds through the history-tree path (`SelectionList::update()',
selection.cc:269): an insertion before the saved selection shifts it right."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((3 . 4)) 0)          ; "cd"
    (kao-save-selections)
    (save-excursion (goto-char 1) (insert "XX"))  ; "XXabcdef"
    (kao-history-commit-pending)
    (kao-restore-selections)
    (should (equal (kao-multi-tests--pairs) '((5 . 6))))  ; still "cd"
    (should (string= "cd" (buffer-substring 5 7)))))

(ert-deftest kao-multi-restore-after-shrink-merges-overlap ()
  "`z' after a deletion merges selections the translation collapsed together.
Kakoune `update(merge=true)' merges exactly when it translates
\(selection.cc:265-267) — the slice-18 follow-up: a changed-buffer restore
never installs an overlapping list Kakoune would have merged."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((1 . 2) (4 . 5)) 0)
    (kao-save-selections)
    (delete-region 3 7)                           ; "ab": cdef gone
    (kao-history-commit-pending)
    (kao-restore-selections)
    ;; (4 . 5) collapses into the erase begin 3, clamps to 2, overlaps (1 . 2).
    (should (equal (kao-multi-tests--pairs) '((1 . 2))))))

(ert-deftest kao-multi-restore-same-id-keeps-overlap-unmerged ()
  "`z' with no edits since `Z' restores verbatim — even an overlapping list.
`update_selections' early-returns at the current timestamp
\(selection.cc:233-236), so a post-combine unmerged list (the slice-18
producer) survives a same-state round-trip."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((1 . 4) (2 . 3)) 0)  ; overlapping (nested)
    (kao-save-selections)
    (kao-multi-tests--span 6 6)                   ; clobber
    (kao-restore-selections)
    (should (equal (kao-multi-tests--pairs) '((1 . 4) (2 . 3))))))

(ert-deftest kao-multi-combine-translates-saved-list ()
  "`<a-z>' translates the register list before combining (list.update(),
normal.cc:2085): the saved selection is combined in CURRENT coordinates."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((1 . 2)) 0)
    (kao-save-selections)
    (save-excursion (goto-char 1) (insert "XX"))  ; saved (1 . 2) -> (3 . 4)
    (kao-history-commit-pending)
    (kao-multi-tests--list '((5 . 6)) 0)
    (kao-multi-tests--with-key ?u (kao-combine-from-register))
    (should (equal (kao-multi-tests--pairs) '((3 . 6))))))

(ert-deftest kao-multi-combine-to-translates-saved-list ()
  "`<a-Z>' translates the register list before combining, like `<a-z>'
\(both reach `list.update()' inside `combine_selections', normal.cc:2085):
the union saved back is computed in CURRENT coordinates."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((1 . 2)) 0)
    (kao-save-selections)
    (save-excursion (goto-char 1) (insert "XX"))  ; saved (1 . 2) -> (3 . 4)
    (kao-history-commit-pending)
    (kao-multi-tests--list '((5 . 6)) 0)
    (kao-multi-tests--with-key ?u (kao-combine-to-register))
    (let ((stored (cddr (kao-register-get-selections))))
      (should (equal (kao-multi-tests--sels-pairs stored) '((3 . 6)))))))

(ert-deftest kao-multi-restore-empty-register-noop ()
  "`z' on an empty register leaves the live list unchanged."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdefgh"
    (kao-multi-tests--list '((1 . 2)) 0)
    (kao-restore-selections)
    (should (equal (kao-multi-tests--pairs) '((1 . 2))))))

(ert-deftest kao-multi-restore-cross-buffer-switches ()
  "`z' on a register saved in another buffer switches to it and restores there.
Faithful to `restore_selections' calling change_buffer (normal.cc:2164-2165);
repins the pre-slice-10 cross-buffer no-op."
  (clrhash kao--selection-registers)
  (let ((other (generate-new-buffer " *kao-other*")))
    (unwind-protect
        (progn
          (with-current-buffer other
            (insert "xxxxxx")
            (kao-mode 1)
            (kao-register-save-selections
             (kao-sels-make :list (list (kao-sel-make :anchor 2 :cursor 5)) :main 0)))
          (kao-multi-tests--with "abcdefgh"
            (kao-multi-tests--list '((3 . 4)) 0)
            (kao-restore-selections)              ; register tagged with `other'
            (should (eq (current-buffer) other))
            (should (equal '((2 . 5))
                           (mapcar (lambda (s) (cons (kao-sel-anchor s)
                                                     (kao-sel-cursor s)))
                                   (kao-sels-list kao--sels))))))
      (when (buffer-live-p other)
        (with-current-buffer other (kao-mode -1))
        (kill-buffer other)))))

(ert-deftest kao-multi-restore-cross-buffer-target-not-kao-errors ()
  "`z' into a live buffer with `kao-mode' disabled aborts loudly (no silent
re-enable — slice-10 plan decision, as for jumps); live sels unchanged."
  (clrhash kao--selection-registers)
  (let ((other (generate-new-buffer " *kao-other*")))
    (unwind-protect
        (progn
          (with-current-buffer other
            (insert "xxxxxx")
            (kao-register-save-selections        ; saved with kao-mode OFF there
             (kao-sels-make :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0)))
          (kao-multi-tests--with "abcdefgh"
            (kao-multi-tests--list '((3 . 4)) 0)
            (should-error (kao-restore-selections) :type 'user-error)
            (should (equal (kao-multi-tests--pairs) '((3 . 4))))))
      (kill-buffer other))))

(ert-deftest kao-multi-restore-dead-source-errors ()
  "`z' on a register whose source buffer was killed signals the faithful
\"no such buffer\" (`get_buffer', buffer_manager.cc:77); live sels unchanged."
  (clrhash kao--selection-registers)
  (let ((other (generate-new-buffer " *kao-other*")))
    (with-current-buffer other
      (insert "xxxxxx")
      (kao-register-save-selections
       (kao-sels-make :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0)))
    (kill-buffer other)
    (kao-multi-tests--with "abcdefgh"
      (kao-multi-tests--list '((3 . 4)) 0)
      (let ((err (should-error (kao-restore-selections) :type 'user-error)))
        (should (string= "no such buffer" (cadr err))))
      (should (equal (kao-multi-tests--pairs) '((3 . 4)))))))

(ert-deftest kao-multi-combine-from-cross-buffer-errors ()
  "`<a-z>' with a register saved in another buffer refuses with Kakoune's
exact wording BEFORE the combine-op key is read (normal.cc:2074-2075)."
  (clrhash kao--selection-registers)
  (let ((other (generate-new-buffer " *kao-other*")))
    (unwind-protect
        (progn
          (with-current-buffer other
            (insert "xxxxxx")
            (kao-mode 1)
            (kao-register-save-selections
             (kao-sels-make :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0)))
          (kao-multi-tests--with "abcdefgh"
            (kao-multi-tests--list '((3 . 4)) 0)
            (cl-letf (((symbol-function 'read-key)
                       (lambda (&rest _) (ert-fail "combine op key was read"))))
              (let ((err (should-error (kao-combine-from-register)
                                       :type 'user-error)))
                (should (string= "cannot combine selections from different buffers"
                                 (cadr err)))))
            (should (equal (kao-multi-tests--pairs) '((3 . 4))))))
      (when (buffer-live-p other)
        (with-current-buffer other (kao-mode -1))
        (kill-buffer other)))))

(ert-deftest kao-multi-combine-to-cross-buffer-errors ()
  "`<a-Z>' with a non-empty register from another buffer refuses identically;
the stored register is left untouched."
  (clrhash kao--selection-registers)
  (let ((other (generate-new-buffer " *kao-other*")))
    (unwind-protect
        (progn
          (with-current-buffer other
            (insert "xxxxxx")
            (kao-mode 1)
            (kao-register-save-selections
             (kao-sels-make :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0)))
          (kao-multi-tests--with "abcdefgh"
            (kao-multi-tests--list '((3 . 4)) 0)
            (cl-letf (((symbol-function 'read-key)
                       (lambda (&rest _) (ert-fail "combine op key was read"))))
              (let ((err (should-error (kao-combine-to-register)
                                       :type 'user-error)))
                (should (string= "cannot combine selections from different buffers"
                                 (cadr err)))))
            (let ((stored (kao-register-get-selections)))
              (should (eq (car stored) other))   ; untouched, still other's
              (should (= 1 (kao-sel-cursor
                            (car (kao-sels-list (cddr stored)))))))))
      (when (buffer-live-p other)
        (with-current-buffer other (kao-mode -1))
        (kill-buffer other)))))

;;;; Combine — pure algebra (kao--combine-selections)

(defun kao-multi-tests--sels (pairs &optional main)
  "Build a `kao-sels' from PAIRS, a list of (anchor . cursor); MAIN defaults to 0."
  (kao-sels-make
   :list (mapcar (lambda (p) (kao-sel-make :anchor (car p) :cursor (cdr p))) pairs)
   :main (or main 0)))

(defun kao-multi-tests--sels-pairs (sels)
  "Return SELS as a list of (anchor . cursor) pairs."
  (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
          (kao-sels-list sels)))

(ert-deftest kao-multi-combine-union ()
  "`union' spans both selections, forward (anchor=min, cursor=max)."
  (let ((r (kao--combine-selections (kao-multi-tests--sels '((1 . 3)))
                                    (kao-multi-tests--sels '((2 . 6))) 'union)))
    (should (equal (kao-multi-tests--sels-pairs r) '((1 . 6))))))

(ert-deftest kao-multi-combine-union-can-overlap ()
  "Pairwise `union' can return an OVERLAPPING list, installed unmerged.
Kakoune installs the pairwise combine result without a merge
\(normal.cc:2103, vs `append''s explicit `sort_and_merge_overlapping');
this is the live producer of overlapping selection lists that the paste
`last'-tracking (normal.cc:850) must survive — pinned here so a future
\"merge everywhere\" cleanup can't silently break the parity."
  (let ((r (kao--combine-selections
            (kao-multi-tests--sels '((1 . 5) (10 . 15)))
            (kao-multi-tests--sels '((3 . 12) (14 . 16))) 'union)))
    (should (equal (kao-multi-tests--sels-pairs r) '((1 . 12) (10 . 16))))
    (should (= 2 (length (kao-sels-list r))))))

(ert-deftest kao-multi-combine-intersect-overlap ()
  "`intersect' of overlapping selections is [max-of-mins, min-of-maxs], forward."
  (let ((r (kao--combine-selections (kao-multi-tests--sels '((1 . 5)))
                                    (kao-multi-tests--sels '((3 . 8))) 'intersect)))
    (should (equal (kao-multi-tests--sels-pairs r) '((3 . 5))))))

(ert-deftest kao-multi-combine-intersect-disjoint-inverts ()
  "A disjoint `intersect' yields an inverted (anchor > cursor) sel, as Kakoune does."
  (let ((r (kao--combine-selections (kao-multi-tests--sels '((1 . 2)))
                                    (kao-multi-tests--sels '((5 . 7))) 'intersect)))
    (should (equal (kao-multi-tests--sels-pairs r) '((5 . 2))))))

(ert-deftest kao-multi-combine-leftmost-rightmost ()
  "`leftmost'/`rightmost' keep the whole sel with the smaller/larger cursor."
  (let ((reg (kao-multi-tests--sels '((3 . 5))))   ; cursor 5
        (cur (kao-multi-tests--sels '((1 . 2)))))  ; cursor 2
    (should (equal (kao-multi-tests--sels-pairs
                    (kao--combine-selections reg cur 'leftmost)) '((1 . 2))))
    (should (equal (kao-multi-tests--sels-pairs
                    (kao--combine-selections reg cur 'rightmost)) '((3 . 5))))))

(ert-deftest kao-multi-combine-longest-shortest ()
  "`longest'/`shortest' keep the whole sel with the larger/smaller char length."
  (let ((reg (kao-multi-tests--sels '((1 . 2))))   ; length 2
        (cur (kao-multi-tests--sels '((4 . 8)))))  ; length 5
    (should (equal (kao-multi-tests--sels-pairs
                    (kao--combine-selections reg cur 'longest)) '((4 . 8))))
    (should (equal (kao-multi-tests--sels-pairs
                    (kao--combine-selections reg cur 'shortest)) '((1 . 2))))))

(ert-deftest kao-multi-combine-ties-keep-register ()
  "On a tie the register selection A wins (mirrors `combine_selection' mutating A)."
  (let ((reg (kao-multi-tests--sels '((3 . 5))))   ; cursor 5, length 3
        (cur (kao-multi-tests--sels '((1 . 5)))))  ; cursor 5, length 5
    ;; equal cursors -> leftmost/rightmost keep A
    (should (equal (kao-multi-tests--sels-pairs
                    (kao--combine-selections reg cur 'leftmost)) '((3 . 5))))
    (should (equal (kao-multi-tests--sels-pairs
                    (kao--combine-selections reg cur 'rightmost)) '((3 . 5)))))
  (let ((reg (kao-multi-tests--sels '((1 . 3))))   ; length 3
        (cur (kao-multi-tests--sels '((5 . 7)))))  ; length 3
    ;; equal lengths -> longest/shortest keep A
    (should (equal (kao-multi-tests--sels-pairs
                    (kao--combine-selections reg cur 'longest)) '((1 . 3))))
    (should (equal (kao-multi-tests--sels-pairs
                    (kao--combine-selections reg cur 'shortest)) '((1 . 3))))))

(ert-deftest kao-multi-combine-pairwise-keeps-current-main ()
  "A pairwise (non-append) combine keeps the CURRENT main index."
  (let ((r (kao--combine-selections
            (kao-multi-tests--sels '((1 . 2) (3 . 4)))
            (kao-multi-tests--sels '((5 . 6) (7 . 8)) 1) 'union)))
    (should (equal (kao-multi-tests--sels-pairs r) '((1 . 6) (3 . 8))))
    (should (= 1 (kao-sels-main r)))))

(ert-deftest kao-multi-combine-size-mismatch-errors ()
  "A pairwise combine of differently-sized lists signals an error."
  (should-error (kao--combine-selections
                 (kao-multi-tests--sels '((1 . 2)))
                 (kao-multi-tests--sels '((5 . 6) (7 . 8))) 'union)))

(ert-deftest kao-multi-combine-append-concatenates-main-offset ()
  "`append' concatenates register++current; main = register-len + current main."
  (let ((r (kao--combine-selections
            (kao-multi-tests--sels '((1 . 2)))
            (kao-multi-tests--sels '((5 . 6) (8 . 9)) 1) 'append)))
    (should (equal (kao-multi-tests--sels-pairs r) '((1 . 2) (5 . 6) (8 . 9))))
    (should (= 2 (kao-sels-main r)))))           ; 1 (reg len) + 1 (cur main)

(ert-deftest kao-multi-combine-append-sorts-and-merges ()
  "`append' sort-and-merges overlaps; the main follows the survivor by identity."
  (let ((r (kao--combine-selections
            (kao-multi-tests--sels '((1 . 4)))
            (kao-multi-tests--sels '((3 . 7))) 'append)))
    (should (equal (kao-multi-tests--sels-pairs r) '((1 . 7))))
    (should (= 0 (kao-sels-main r)))))

;;;; Combine menu — <a-z> / <a-Z> dispatch (read-key stubbed)

(ert-deftest kao-multi-combine-table-keys ()
  "Every combine op key maps to its symbol; an unmapped key is absent."
  (should (eq 'append    (cdr (assq ?a kao--combine-table))))
  (should (eq 'union     (cdr (assq ?u kao--combine-table))))
  (should (eq 'intersect (cdr (assq ?i kao--combine-table))))
  (should (eq 'leftmost  (cdr (assq ?< kao--combine-table))))
  (should (eq 'rightmost (cdr (assq ?> kao--combine-table))))
  (should (eq 'longest   (cdr (assq ?+ kao--combine-table))))
  (should (eq 'shortest  (cdr (assq ?- kao--combine-table))))
  (should-not (assq ?z kao--combine-table)))

(ert-deftest kao-multi-combine-from-union ()
  "`<a-z>' with `u' unions the register list with the current list, set live."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdefgh"
    (kao-multi-tests--list '((1 . 3)) 0)
    (kao-save-selections)                       ; register = ((1 . 3))
    (kao-multi-tests--list '((2 . 6)) 0)        ; current  = ((2 . 6))
    (kao-multi-tests--with-key ?u (kao-combine-from-register))
    (should (equal (kao-multi-tests--pairs) '((1 . 6))))))

(ert-deftest kao-multi-combine-from-append ()
  "`<a-z>' with `a' appends the register list to the current and sort-merges."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdefgh"
    (kao-multi-tests--list '((6 . 7)) 0)
    (kao-save-selections)                       ; register = ((6 . 7))
    (kao-multi-tests--list '((1 . 2)) 0)        ; current  = ((1 . 2))
    (kao-multi-tests--with-key ?a (kao-combine-from-register))
    (should (equal (kao-multi-tests--pairs) '((1 . 2) (6 . 7))))))

(ert-deftest kao-multi-combine-from-size-mismatch-noop ()
  "A pairwise `<a-z>' on lists of different sizes aborts without changing state."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdefgh"
    (kao-multi-tests--list '((1 . 2)) 0)
    (kao-save-selections)                       ; register has 1
    (kao-multi-tests--list '((3 . 4) (5 . 6)) 1) ; current has 2
    (kao-multi-tests--with-key ?u (kao-combine-from-register))
    (should (equal (kao-multi-tests--pairs) '((3 . 4) (5 . 6))))  ; unchanged
    (should (= 1 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-combine-from-empty-register-noop ()
  "`<a-z>' on an empty register changes nothing (the op key is never read)."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdefgh"
    (kao-multi-tests--list '((3 . 4)) 0)
    (kao-multi-tests--with-key ?u (kao-combine-from-register))
    (should (equal (kao-multi-tests--pairs) '((3 . 4))))))

(ert-deftest kao-multi-combine-to-empty-register-plain-save ()
  "`<a-Z>' on an empty register is a plain save of the current selections."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdefgh"
    (kao-multi-tests--list '((2 . 5)) 0)
    (kao-combine-to-register)                   ; empty -> plain save (no key read)
    (let ((stored (cddr (kao-register-get-selections))))
      (should (equal (kao-multi-tests--sels-pairs stored) '((2 . 5)))))))

(ert-deftest kao-multi-combine-to-union-saves-back ()
  "`<a-Z>' with `u' saves the union of register and current back to the register."
  (clrhash kao--selection-registers)
  (kao-multi-tests--with "abcdefgh"
    (kao-multi-tests--list '((1 . 3)) 0)
    (kao-save-selections)                       ; register = ((1 . 3))
    (kao-multi-tests--list '((2 . 6)) 0)        ; current  = ((2 . 6))
    (kao-multi-tests--with-key ?u (kao-combine-to-register))
    (let ((stored (cddr (kao-register-get-selections))))
      (should (equal (kao-multi-tests--sels-pairs stored) '((1 . 6)))))
    ;; current selections are NOT modified by combine-to
    (should (equal (kao-multi-tests--pairs) '((2 . 6))))))

(ert-deftest kao-multi-combine-info-parity ()
  "`kao--combine-info' lists exactly the keys `kao--combine-table'
dispatches, so the autoinfo box never drifts from the real menu."
  (should (equal (sort (mapcar #'car kao--combine-info) #'<)
                 (sort (mapcar #'car kao--combine-table) #'<))))

;;;; <a-_> / <a-+> — merge consecutive / overlapping (ensure_forward + merge)

(ert-deftest kao-multi-merge-consecutive-touching ()
  "`<a-_>' merges selections that touch (zero gap)."
  (kao-multi-tests--with "abcdefg"
    (kao-multi-tests--list '((1 . 2) (3 . 4)) 0)   ; char_next(2)=3 >= min 3 -> touch
    (kao-merge-consecutive)
    (should (equal (kao-multi-tests--pairs) '((1 . 4))))
    (should (= 0 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-merge-consecutive-gap-kept ()
  "`<a-_>' does NOT merge across a one-char gap."
  (kao-multi-tests--with "abcdefg"
    (kao-multi-tests--list '((1 . 2) (4 . 5)) 0)   ; char_next(2)=3 < min 4 -> gap
    (kao-merge-consecutive)
    (should (equal (kao-multi-tests--pairs) '((1 . 2) (4 . 5))))))

(ert-deftest kao-multi-merge-consecutive-ensures-forward ()
  "`<a-_>' flips backward selections forward before merging touching ones."
  (kao-multi-tests--with "abcdefg"
    (kao-multi-tests--list '((2 . 1) (4 . 3)) 0)   ; backward; forward -> (1.2)(3.4) touch
    (kao-merge-consecutive)
    (should (equal (kao-multi-tests--pairs) '((1 . 4))))))

(ert-deftest kao-multi-merge-consecutive-tracks-main ()
  "The main follows its selection through a merge."
  (kao-multi-tests--with "abcdefg"
    (kao-multi-tests--list '((1 . 2) (3 . 4) (6 . 7)) 1)  ; main = (3.4)
    (kao-merge-consecutive)
    (should (equal (kao-multi-tests--pairs) '((1 . 4) (6 . 7))))
    (should (= 0 (kao-sels-main kao--sels)))))     ; (3.4) merged into survivor 0

(ert-deftest kao-multi-merge-overlapping-only-overlap ()
  "`<a-+>' merges overlapping selections but leaves merely-touching ones apart."
  (kao-multi-tests--with "abcdefg"
    (kao-multi-tests--list '((1 . 3) (2 . 5)) 0)   ; overlap on 2..3
    (kao-merge-overlapping)
    (should (equal (kao-multi-tests--pairs) '((1 . 5)))))
  (kao-multi-tests--with "abcdefg"
    (kao-multi-tests--list '((1 . 2) (3 . 4)) 0)   ; touch but no overlap
    (kao-merge-overlapping)
    (should (equal (kao-multi-tests--pairs) '((1 . 2) (3 . 4))))))

(ert-deftest kao-multi-merge-bindings ()
  "`M-_'/`M-+' are bound to the merge commands."
  (should (eq #'kao-merge-consecutive (lookup-key kao-normal-state-map (kbd "M-_"))))
  (should (eq #'kao-merge-overlapping (lookup-key kao-normal-state-map (kbd "M-+")))))

;;;; _ — trim selections

(ert-deftest kao-multi-trim-leading-trailing ()
  "`_' strips leading and trailing blanks, keeping the non-blank core."
  (kao-multi-tests--with "  ab  "                  ; sp1 sp2 a3 b4 sp5 sp6
    (kao-multi-tests--span 1 6)
    (kao-trim-selections)
    (should (equal (kao-multi-tests--pairs) '((3 . 4))))))

(ert-deftest kao-multi-trim-preserves-direction ()
  "`_' keeps a backward selection backward."
  (kao-multi-tests--with "  ab  "
    (kao-multi-tests--span 6 1)                     ; backward
    (kao-trim-selections)
    (should (equal (kao-multi-tests--pairs) '((4 . 3))))
    (should-not (kao-sel-forward-p (car (kao-sels-list kao--sels))))))

(ert-deftest kao-multi-trim-drops-blank-adjusts-main ()
  "A wholly-blank selection is dropped and the main index adjusted."
  (kao-multi-tests--with "a  b"                     ; a1 sp2 sp3 b4
    (kao-multi-tests--list '((1 . 1) (2 . 3) (4 . 4)) 2)  ; main = (4.4)
    (kao-trim-selections)
    (should (equal (kao-multi-tests--pairs) '((1 . 1) (4 . 4))))
    (should (= 1 (kao-sels-main kao--sels)))))      ; main followed past the drop

(ert-deftest kao-multi-trim-drops-main-blank-middle ()
  "When the main is itself the dropped blank (not last), main follows the survivor
that shifts into its slot — Kakoune `remove' (index < main is false; main keeps its
index), NOT `select()'s i<=main rule which would pick the previous survivor."
  (kao-multi-tests--with "a  b"                     ; a1 sp2 sp3 b4
    (kao-multi-tests--list '((1 . 1) (2 . 3) (4 . 4)) 1)  ; main = blank (2.3)
    (kao-trim-selections)
    (should (equal (kao-multi-tests--pairs) '((1 . 1) (4 . 4))))
    (should (= 1 (kao-sels-main kao--sels)))))      ; -> (4.4), NOT (1.1)

(ert-deftest kao-multi-trim-drops-main-blank-last ()
  "When the dropped main is the last selection, it decrements to the new last
(`m_main == new_size' branch of `SelectionList::remove')."
  (kao-multi-tests--with "a  "                       ; a1 sp2 sp3
    (kao-multi-tests--list '((1 . 1) (2 . 3)) 1)     ; main = last, blank (2.3)
    (kao-trim-selections)
    (should (equal (kao-multi-tests--pairs) '((1 . 1))))
    (should (= 0 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-trim-all-blank-unchanged ()
  "When every selection is blank the list is left unchanged (with a message)."
  (kao-multi-tests--with "   "                      ; three spaces
    (kao-multi-tests--span 1 3)
    (kao-trim-selections)
    (should (equal (kao-multi-tests--pairs) '((1 . 3))))))

(ert-deftest kao-multi-trim-all-blank-messages-with-prefix ()
  "Trimming a wholly-blank selection list to empty emits the `kao:'-prefixed
\"no selections remaining\", matching the keep sites `kao--keep-apply'
(normal.cc:1299/1341/1982) for repo-wide message consistency."
  (kao-multi-tests--with "   "                      ; three spaces
    (kao-multi-tests--span 1 3)
    (let ((logged nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq logged (apply #'format fmt args)))))
        (kao-trim-selections))
      (should (equal logged "kao: no selections remaining")))))

(ert-deftest kao-multi-trim-non-blank-untouched ()
  "`_' leaves a selection with no edge blanks unchanged."
  (kao-multi-tests--with "abcd"
    (kao-multi-tests--span 1 4)
    (kao-trim-selections)
    (should (equal (kao-multi-tests--pairs) '((1 . 4))))))

;;;; + — duplicate selections

(ert-deftest kao-multi-duplicate-default-two ()
  "`+' with no count duplicates each selection into two; main -> last copy."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--span 2 3)
    (kao-duplicate-selections)
    (should (equal (kao-multi-tests--pairs) '((2 . 3) (2 . 3))))
    (should (= 1 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-duplicate-count-n ()
  "`N+' makes N copies of each selection."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--span 2 3)
    (setq kao--count 3)
    (kao-duplicate-selections)
    (should (equal (kao-multi-tests--pairs) '((2 . 3) (2 . 3) (2 . 3))))
    (should (= 2 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-duplicate-consecutive-dedup ()
  "A selection identical to its predecessor is duplicated once, not COUNT times."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((1 . 1) (1 . 1)) 0)    ; two identical
    (kao-duplicate-selections)                       ; count 2
    (should (equal (kao-multi-tests--pairs) '((1 . 1) (1 . 1) (1 . 1))))
    (should (= 1 (kao-sels-main kao--sels)))))       ; last copy of the first

(ert-deftest kao-multi-duplicate-tracks-main-last-copy ()
  "The main follows the last copy of the original main selection."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--list '((1 . 1) (3 . 3)) 1)    ; main = (3.3)
    (kao-duplicate-selections)                       ; count 2
    (should (equal (kao-multi-tests--pairs) '((1 . 1) (1 . 1) (3 . 3) (3 . 3))))
    (should (= 3 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-trim-duplicate-bindings ()
  "`_' and `+' are bound to trim and duplicate."
  (should (eq #'kao-trim-selections (lookup-key kao-normal-state-map "_")))
  (should (eq #'kao-duplicate-selections (lookup-key kao-normal-state-map "+"))))

;;;; <a-S> — select boundaries

(ert-deftest kao-multi-boundaries-splits-multichar ()
  "`<a-S>' turns a multi-char selection into its first and last char."
  (kao-multi-tests--with "abcde"                  ; a1 b2 c3 d4 e5
    (kao-multi-tests--span 2 4)                    ; covers b c d
    (kao-select-boundaries)
    (should (equal (kao-multi-tests--pairs) '((2 . 2) (4 . 4))))
    (should (= 1 (kao-sels-main kao--sels)))))     ; main = last boundary

(ert-deftest kao-multi-boundaries-single-char-stays ()
  "`<a-S>' leaves a single-char selection as one boundary."
  (kao-multi-tests--with "abc"
    (kao-multi-tests--span 2 2)
    (kao-select-boundaries)
    (should (equal (kao-multi-tests--pairs) '((2 . 2))))
    (should (= 0 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-boundaries-multi-sel-sorted-main-last ()
  "`<a-S>' across selections yields sorted boundaries, main = the last one."
  (kao-multi-tests--with "abcdefgh"               ; positions 1..8
    (kao-multi-tests--list '((1 . 2) (5 . 7)) 0)  ; [1,2] and [5,7]
    (kao-select-boundaries)
    (should (equal (kao-multi-tests--pairs)
                   '((1 . 1) (2 . 2) (5 . 5) (7 . 7))))
    (should (= 3 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-boundaries-key-bound ()
  "<a-S> is bound to `kao-select-boundaries'."
  (should (eq #'kao-select-boundaries
              (lookup-key kao-normal-state-map (kbd "M-S")))))

;;;; C / <a-C> — copy selections onto following / preceding lines

(ert-deftest kao-multi-copy-down-one ()
  "`C' duplicates the selection onto the next line at the same column."
  (kao-multi-tests--with "abc\ndef\nghi"
    (kao-multi-tests--span 2 2)                    ; 'b', col 1
    (kao-copy-selections-down)
    (should (equal (kao-multi-tests--pairs) '((2 . 2) (6 . 6))))
    (should (= 1 (kao-sels-main kao--sels)))))     ; main = the copy

(ert-deftest kao-multi-copy-down-propagates-target ()
  "`C' propagates the source cursor's sticky `target' into the copies
\(normal.cc:1646); placement is unchanged (by the real column, normal.cc:1604)."
  (kao-multi-tests--with "abc\ndef\nghi"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2 :target 'eol))
                     :main 0))
    (kao-copy-selections-down)
    (should (equal (kao-multi-tests--pairs) '((2 . 2) (6 . 6))))   ; placement same
    (dolist (s (kao-sels-list kao--sels))
      (should (eq (kao-sel-target s) 'eol)))))

(ert-deftest kao-multi-copy-down-count ()
  "A count makes that many copies of `C'."
  (kao-multi-tests--with "abc\ndef\nghi\njkl"
    (kao-multi-tests--span 2 2)
    (setq kao--count 2)
    (kao-copy-selections-down)
    (should (equal (kao-multi-tests--pairs) '((2 . 2) (6 . 6) (10 . 10))))
    (should (= 2 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-copy-down-main-index-in-range ()
  "`C': `kao-sels-main' follows the last copy of the old main and stays in
range [0, N) whether main starts at the first, a middle, or the last selection.
Characterization pin for `kao--copy-on-lines' main-index ."
  ;; Three selections on lines 1/3/5 (cols 0); each down-copy lands on the
  ;; empty-of-selections line below (2/4/6 -> pos 5/13/21), so nothing merges
  ;; and the final list stays at N = 6 in ascending position order.
  (dolist (case '((0 . 1)      ; main at FIRST sel  -> its copy at final index 1
                  (1 . 3)      ; main at MIDDLE sel -> its copy at final index 3
                  (2 . 5)))    ; main at LAST sel   -> its copy at final index 5
    (kao-multi-tests--with "abc\ndef\nghi\njkl\nmno\npqr"
      (kao-multi-tests--list '((1 . 1) (9 . 9) (17 . 17)) (car case))
      (kao-copy-selections-down)
      (let ((n (length (kao-sels-list kao--sels)))
            (main (kao-sels-main kao--sels)))
        (should (= n 6))
        (should (= main (cdr case)))       ; lands on the expected copy
        (should (and (<= 0 main) (< main n)))))))  ; and stays in range

(ert-deftest kao-multi-copy-down-skips-short-line ()
  "`C' skips a line too short for the column, copying onto the next that fits."
  (kao-multi-tests--with "abc\nx\nghi"             ; line 2 'x' has no col 2
    (kao-multi-tests--span 3 3)                    ; 'c', col 2
    (kao-copy-selections-down)
    (should (equal (kao-multi-tests--pairs) '((3 . 3) (9 . 9))))
    (should (= 1 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-copy-down-at-buffer-end-noop ()
  "`C' with no line below leaves the selection unchanged."
  (kao-multi-tests--with "abc"
    (kao-multi-tests--span 1 1)
    (kao-copy-selections-down)
    (should (equal (kao-multi-tests--pairs) '((1 . 1))))
    (should (= 0 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-copy-up-one ()
  "`<a-C>' duplicates onto the preceding line; main follows the copy through sort."
  (kao-multi-tests--with "abc\ndef\nghi"
    (kao-multi-tests--span 10 10)                  ; 'h' on line 3, col 1
    (kao-copy-selections-up)
    (should (equal (kao-multi-tests--pairs) '((6 . 6) (10 . 10))))
    (should (= 0 (kao-sels-main kao--sels)))))     ; the copy sorts to the front

(ert-deftest kao-multi-copy-down-multichar-preserves-columns ()
  "`C' copies a multi-char selection preserving both endpoint columns."
  (kao-multi-tests--with "abcd\nefgh"             ; a1 b2 c3 d4 \n5 e6 f7 g8 h9
    (kao-multi-tests--span 2 3)                    ; [b,c] forward, cols 1..2
    (kao-copy-selections-down)
    (should (equal (kao-multi-tests--pairs) '((2 . 3) (7 . 8))))
    (should (= 1 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-copy-down-cjk-display-column ()
  "`C' places the copy by the REAL DISPLAY column across CJK chars
\.  The cursor after \"漢字\" sits at display
column 4, so the copy lands on 'e' (col 4) of \"abcdef\", not 'c' (col 2) as a
codepoint count would give."
  (kao-multi-tests--with "漢字X\nabcdef"          ; 漢1 字2 X3 \n4 a5 b6 c7 d8 e9 f10
    (kao-multi-tests--span 3 3)                    ; 'X' after 漢字, display col 4
    (kao-copy-selections-down)
    (should (equal (kao-multi-tests--pairs) '((3 . 3) (9 . 9))))  ; copy on 'e'
    (should (= (char-after 9) ?e))
    (should (= 1 (kao-sels-main kao--sels)))))

(ert-deftest kao-multi-copy-down-multiline-uses-height ()
  "`C' offsets a multi-line selection by its line height."
  (kao-multi-tests--with "ab\ncd\nef\ngh"          ; lines: ab cd ef gh
    (kao-multi-tests--span 1 4)                    ; line 1 col 0 .. line 2 col 0
    (kao-copy-selections-down)
    (should (equal (kao-multi-tests--pairs) '((1 . 4) (7 . 10))))
    (should (= 1 (kao-sels-main kao--sels)))))

;;;; Relative-walk edge cases (equivalence oracle for the old absolute walk)

(ert-deftest kao-multi-copy-down-no-trailing-newline-eob ()
  "`C' onto the final line with NO trailing newline still copies (bolp guard).
The relative walk lands on the last line's bol (`forward-line' returns 0, `bolp'
true), so the copy is created; the OLD absolute walk did the same."
  (kao-multi-tests--with "ab\ncd"                  ; no trailing newline; line 2 = "cd"
    (kao-multi-tests--span 1 1)                    ; 'a' on line 1, col 0
    (kao-copy-selections-down)
    (should (equal (kao-multi-tests--pairs) '((1 . 1) (4 . 4))))))  ; onto "cd" bol

(ert-deftest kao-multi-copy-down-past-eob-no-newline-noop ()
  "`C' targeting a line PAST a no-trailing-newline eob is skipped (bolp guard).
`forward-line' cannot complete the move (returns non-zero), so no phantom copy is
made over the final partial line — the exact `[1, maxline]' guard, relatively."
  (kao-multi-tests--with "ab"                      ; single line, no newline
    (kao-multi-tests--span 1 1)
    (kao-copy-selections-down)
    (should (equal (kao-multi-tests--pairs) '((1 . 1))))))         ; nothing below

(ert-deftest kao-multi-copy-down-tabs-column-geometry ()
  "`C' places the copy by the REAL visual column across a tab (unchanged).
Verified byte-identical to the old absolute-walk implementation."
  (kao-multi-tests--with "\tab\n\tcd"              ; \t a b \n \t c d  (pos 1..7)
    (kao-multi-tests--span 2 2)                    ; 'a' just after the tab on line 1
    (kao-copy-selections-down)
    ;; line 2 = "\tcd" at pos 5; 'a's visual column after the tab lands on 'c'@6
    (should (equal (kao-multi-tests--pairs) '((2 . 2) (6 . 6))))))

(ert-deftest kao-multi-copy-up-backward-selection-preserves-direction ()
  "`<a-C>' on a BACKWARD selection copies upward keeping the anchor>cursor order."
  (kao-multi-tests--with "abcd\nefgh\nijkl"        ; 3 lines of 4
    (kao-multi-tests--span 8 6)                    ; backward [g..e] on line 2, cols 3..1
    (kao-copy-selections-up)
    ;; onto line 1: same cols, same backward direction (anchor 3, cursor 1)
    (should (equal (kao-multi-tests--pairs) '((3 . 1) (8 . 6))))))

(ert-deftest kao-multi-copy-down-empty-lines-skipped-for-column ()
  "`C' skips blank lines that lack the source column, landing on the next fit.
Verified byte-identical to the old absolute-walk implementation."
  (kao-multi-tests--with "abc\n\nghi"              ; a1 b2 c3 \n4 \n5(empty) g6 h7 i8
    (kao-multi-tests--span 3 3)                    ; 'c' col 2 on line 1
    (setq kao--count 2)
    (kao-copy-selections-down)
    ;; empty line 2 (pos 5) has no col 2 -> skipped; line 3 fits -> one copy at 'i'@8
    (should (equal (kao-multi-tests--pairs) '((3 . 3) (8 . 8))))))

(ert-deftest kao-multi-copy-down-count-off-edge-stops ()
  "`C' with a count larger than the lines available stops at the buffer edge."
  (kao-multi-tests--with "a\nb\nc\n"               ; 3 lines + trailing newline
    (kao-multi-tests--span 1 1)                    ; 'a' on line 1
    (setq kao--count 9)                            ; ask for 9, only 2 lines below
    (kao-copy-selections-down)
    (should (equal (kao-multi-tests--pairs) '((1 . 1) (3 . 3) (5 . 5))))))

(ert-deftest kao-multi-copy-down-multi-selection-main-tracks ()
  "`C' over MULTIPLE selections tracks the main through the sort/merge.
Verified byte-identical to the old absolute-walk implementation."
  (kao-multi-tests--with "abc\ndef\nghi\njkl"      ; a1..\n4 d5 e6 f7 \n8 g9 h10 i11 \n12 j13..
    (kao-multi-tests--list '((2 . 2) (7 . 7)) 1)   ; 'b'@2 (line1) and 'f'@7 (line2); main=2nd
    (kao-copy-selections-down)
    ;; 'b'@2 -> col 1 on line 2 = 'e'@6 ; 'f'@7 -> col 2 on line 3 = 'i'@11
    (should (equal (kao-multi-tests--pairs) '((2 . 2) (6 . 6) (7 . 7) (11 . 11))))
    (should (= (kao-sel-min (kao--main-sel)) 11)))) ; main follows its own copy

(ert-deftest kao-multi-copy-lines-keys-bound ()
  "C and <a-C> are bound in the normal-state map."
  (should (eq #'kao-copy-selections-down (lookup-key kao-normal-state-map "C")))
  (should (eq #'kao-copy-selections-up
              (lookup-key kao-normal-state-map (kbd "M-C")))))

;;;; Pending register (the `"' prefix)

(ert-deftest kao-multi-selection-register-named-roundtrip ()
  "`\"c Z' saves to selection register c; `\"c z' restores from it.
The default `^' register is untouched (to_lower(params.reg ? : '^'),
normal.cc:2118/:2149)."
  (kao-multi-tests--with "abcdef"
    (let ((caret-before (kao-register-get-selections)))
      (remhash ?c kao--selection-registers)
      (kao-multi-tests--span 2 4)
      (setq kao--pending-register ?C)          ; raw uppercase: lowered at use
      (kao-save-selections)
      (should (kao-register-get-selections ?c))
      (should (eq (kao-register-get-selections) caret-before))
      (kao-multi-tests--span 1 1)
      (setq kao--pending-register ?c)
      (kao-restore-selections)
      (let ((s (car (kao-sels-list kao--sels))))
        (should (= 2 (kao-sel-anchor s)))
        (should (= 4 (kao-sel-cursor s))))
      (remhash ?c kao--selection-registers))))

(ert-deftest kao-multi-combine-to-named-register ()
  "`\"c <a-Z>' on an empty register c falls back to a plain save INTO c."
  (kao-multi-tests--with "abcdef"
    (remhash ?c kao--selection-registers)
    (kao-multi-tests--span 2 4)
    (setq kao--pending-register ?c)
    (kao-combine-to-register)                  ; empty -> plain save (to c)
    (should (kao-register-get-selections ?c))
    (remhash ?c kao--selection-registers)))

(ert-deftest kao-multi-selection-register-alpha-guard ()
  "Selection registers accept only `^' + alphabetic (normal.cc:2117-2120/:1989-1991)."
  (kao-multi-tests--with "abcdef"
    (kao-multi-tests--span 2 4)
    (setq kao--pending-register ?/)
    (let ((err (should-error (kao-save-selections) :type 'user-error)))
      (should (equal (cadr err)
                     (format-message
                      "selections can only be saved to the '^' and alphabetic registers"))))
    (setq kao--pending-register ?/)
    (should-error (kao-restore-selections) :type 'user-error)))

;;;; Captures: population in s, none in S, retention in <a-k>

(ert-deftest kao-multi-select-regex-fills-captures ()
  "`s' fills each result's captures with the full submatch list."
  (kao-multi-tests--with "ab1 cd2"
    (kao-select-whole-buffer)
    (kao--select-regex-apply "\\([a-z]+\\)\\([0-9]\\)")
    (let ((lst (kao-sels-list kao--sels)))
      (should (= (length lst) 2))
      (should (equal (kao-sel-captures (nth 0 lst)) '("ab1" "ab" "1")))
      (should (equal (kao-sel-captures (nth 1 lst)) '("cd2" "cd" "2"))))))

(ert-deftest kao-multi-select-regex-unmatched-group-empty ()
  "An unmatched alternative group reads \"\" (Kakoune pushes the empty string)."
  (kao-multi-tests--with "ab"
    (kao-select-whole-buffer)
    (kao--select-regex-apply "\\(a\\)\\|\\(z\\)")
    (let ((caps (kao-sel-captures (car (kao-sels-list kao--sels)))))
      (should (equal (nth 0 caps) "a"))
      (should (equal (nth 1 caps) "a"))
      ;; group 2 never matched: "" when present, absent when Emacs truncates
      ;; trailing unmatched groups -- both read "" through the digit registers
      (should (member (nth 2 caps) '(nil ""))))))

(ert-deftest kao-multi-select-regex-overwrites-captures ()
  "A second `s' replaces the previous captures (the result's captures win)."
  (kao-multi-tests--with "ab1 cd2"
    (kao-select-whole-buffer)
    (kao--select-regex-apply "\\(ab\\)1")
    (should (equal (kao-sel-captures (car (kao-sels-list kao--sels)))
                   '("ab1" "ab")))
    (kao--select-regex-apply "\\(b\\)")
    (should (equal (kao-sel-captures (car (kao-sels-list kao--sels)))
                   '("b" "b")))))

(ert-deftest kao-multi-split-regex-no-captures ()
  "`S' segments carry no captures (split_on_matches pushes plain selections)."
  (kao-multi-tests--with "a,b,c"
    (kao-select-whole-buffer)
    (kao--split-regex-apply ",")
    (should (= (length (kao-sels-list kao--sels)) 3))
    (dolist (s (kao-sels-list kao--sels))
      (should (null (kao-sel-captures s))))))

(ert-deftest kao-multi-keep-retains-captures ()
  "`<a-k>' keeps the same selection objects, captures included (normal.cc:1287)."
  (kao-multi-tests--with "ab1 cd2"
    (kao-select-whole-buffer)
    (kao--select-regex-apply "\\([a-z]+\\)\\([0-9]\\)")
    (kao--keep-apply "cd" t)
    (let ((lst (kao-sels-list kao--sels)))
      (should (= (length lst) 1))
      (should (equal (kao-sel-captures (car lst)) '("cd2" "cd" "2"))))))

(ert-deftest kao-multi-real-motion-after-s-keeps-captures ()
  "A real motion command after a real `s' keeps the captures end-to-end.
The integration-level pin of the select() rule (normal.cc:118-119): `w'
funnels through `kao--map-selections', whose carry covers every motion."
  (kao-multi-tests--with "ab1 cd2"
    (kao-select-whole-buffer)
    (kao--select-regex-apply "\\(ab\\)1")
    (kao-word-forward)
    (should (equal (kao-sel-captures (car (kao-sels-list kao--sels)))
                   '("ab1" "ab")))))


;;;; Regex-prompt buffer-word completion

(defmacro kao-multi-tests--with-src-words (content &rest body)
  "Run BODY with `minibuffer-selected-window' resolving to a CONTENT buffer."
  (declare (indent 1))
  `(let ((src (generate-new-buffer " *kao-words*")))
     (unwind-protect
         (progn
           (with-current-buffer src (insert ,content))
           (cl-letf (((symbol-function 'minibuffer-selected-window)
                      (lambda () (selected-window)))
                     ((symbol-function 'window-buffer)
                      (lambda (&optional _w) src)))
             ,@body))
       (kill-buffer src))))

(ert-deftest kao-multi-capf-bounds-plain-word ()
  "The capf walk: start at the first word char before point."
  (with-temp-buffer
    (insert "x* fo")
    (let ((capf (kao--regex-capf)))
      (should (= (nth 0 capf) 4))       ; the 'f'
      (should (= (nth 1 capf) 6)))))

(ert-deftest kao-multi-capf-backslash-drops-escaped-char ()
  "An odd backslash count before the word start escapes its first char:
the prefix of `\\bfoo' is `foo' (normal.cc:969-975 substr)."
  (with-temp-buffer
    (insert "\\bfo")
    (let ((capf (kao--regex-capf)))
      (should (= (nth 0 capf) 3))))     ; past the escaped 'b'
  ;; EVEN backslashes = a literal backslash, no escape: word intact.
  (with-temp-buffer
    (insert "\\\\bfo")
    (let ((capf (kao--regex-capf)))
      (should (= (nth 0 capf) 3)))))    ; the 'b' starts the word

(ert-deftest kao-multi-capf-empty-word-allowed ()
  "At a non-word char the word is empty — the C++ completes \"\" against
every buffer word; the capf returns an empty-span region, not nil."
  (with-temp-buffer
    (insert "(")
    (let ((capf (kao--regex-capf)))
      (should (= (nth 0 capf) (nth 1 capf))))))

(ert-deftest kao-multi-prompt-words-from-source-buffer ()
  "Candidates come from the ORIGINATING buffer; word-START matches only
\(`xalphaz' contains the prefix mid-word: a word-db holds whole words, so
neither `xalphaz' nor its tail may appear); deduped."
  (kao-multi-tests--with-src-words "alpha beta xalphaz alphard alpha"
    (should (equal (kao--prompt-buffer-words "alph")
                   '("alpha" "alphard")))))

(ert-deftest kao-multi-prompt-words-capped-at-100 ()
  "The candidate list stops at Kakoune's max_count = 100 (normal.cc:979)."
  (kao-multi-tests--with-src-words
      (mapconcat (lambda (i) (format "w%03d" i)) (number-sequence 1 150) " ")
    (should (= (length (kao--prompt-buffer-words "w")) 100))))

(ert-deftest kao-multi-regex-prompt-setup-wires-capf-and-tab ()
  "`kao--regex-prompt-setup' = base prompt map (C-r) + TAB completion +
buffer-local capf; the BASE `kao--prompt-setup' must NOT gain TAB (pipe
and object-desc prompts keep their own completion stories)."
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (kao--regex-prompt-setup)
    (should (eq (lookup-key (current-local-map) (kbd "TAB"))
                #'completion-at-point))
    (should (eq (lookup-key (current-local-map) (kbd "C-r"))
                #'kao-prompt-insert-register))
    (should (memq #'kao--regex-capf completion-at-point-functions)))
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (kao--prompt-setup)
    (should-not (eq (lookup-key (current-local-map) (kbd "TAB"))
                    #'completion-at-point))))

;;;; Case-fold in the s/<a-k> regex leaves

(ert-deftest kao-multi-casefold-regex-leaves ()
  "`kao--regex-matches-in'/`-search-in': case-sensitive default, `(?i)' folds."
  (let ((kao-search-case-fold nil))
    (with-temp-buffer
      (insert "FOO BAR")                       ; all uppercase
      (should-not (kao--regex-search-in "foo" (point-min) (point-max)))   ; sensitive
      (should (kao--regex-search-in "(?i)foo" (point-min) (point-max)))   ; folds
      (should (= (length (kao--regex-matches-in "foo" (point-min) (point-max))) 0))
      (should (= (length (kao--regex-matches-in "(?i)bar" (point-min) (point-max))) 1)))))
;; NB: the buffer-word completion scan (`kao--prompt-buffer-words', :188) is
;; deliberately NOT a case-fold leaf — verified by the diff leaving it untouched;
;; it is minibuffer-window-coupled and not reliably testable in batch.

;;;; Mode-off guard sweep — (ADDITIVE pins)

;; An M-x-discoverable command run in a buffer where `kao-mode' is off used to
;; die far from the call as (wrong-type-argument kao-sels nil) — `kao--sels' is
;; nil and a selection-reading command trips its struct accessor.  The shared
;; `kao--assert-mode' guard (the mode guard) turns that into a named
;; `user-error'.  Representative commands only (the guard is uniform); each is
;; driven via `call-interactively' in a fundamental-mode buffer, the M-x path.

(ert-deftest kao-multi-select-regex-mode-off-guards ()
  "I4: `s' (`kao-select-regex') via M-x with `kao-mode' off signals the shared
guard, not the cryptic (wrong-type-argument kao-sels nil)."
  (with-temp-buffer                     ; fundamental-mode temp buffer: mode OFF
    (insert "abc")
    (let ((err (should-error (call-interactively #'kao-select-regex)
                             :type 'user-error)))
      (should (string-match-p "kao-mode is not active" (cadr err))))))

(ert-deftest kao-multi-split-lines-mode-off-guards ()
  "I4: `<a-s>' (`kao-split-lines') via M-x with `kao-mode' off signals the
shared guard."
  (with-temp-buffer
    (insert "abc")
    (let ((err (should-error (call-interactively #'kao-split-lines)
                             :type 'user-error)))
      (should (string-match-p "kao-mode is not active" (cadr err))))))

(provide 'kao-multi-tests)
;;; kao-multi-tests.el ends here
