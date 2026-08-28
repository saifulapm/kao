;;; kao-search-tests.el --- Tests for kao-search -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the P4 search core: the find-next-match buffer scan and the
;; merge_selections helper (Task 1), the Replace path (`/'/`<a-/>', Task 2), and
;; the Extend path (`?'/`<a-?>', Task 3).  Buffer-coupled scans run in a
;; `kao-mode' temp buffer; the apply cores take the regex string directly so they
;; are exercised without the minibuffer (same pattern as the kao-multi tests).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-state)
(require 'kao-multi)
(require 'kao-register)
(require 'kao-search)

(defmacro kao-search-tests--with (content &rest body)
  "Run BODY in a `kao-mode' temp buffer of CONTENT (point at `point-min')."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (kao-mode 1)
     (unwind-protect (progn ,@body)
       (kao-mode -1))))

(defun kao-search-tests--span (anchor cursor &optional main)
  "Set `kao--sels' to a single selection ANCHOR..CURSOR (MAIN defaults to 0)."
  (setq kao--sels (kao-sels-make
                   :list (list (kao-sel-make :anchor anchor :cursor cursor))
                   :main (or main 0))))

(defun kao-search-tests--list (sels &optional main)
  "Set `kao--sels' from SELS, a list of (anchor . cursor) pairs (MAIN default 0)."
  (setq kao--sels (kao-sels-make
                   :list (mapcar (lambda (p) (kao-sel-make :anchor (car p) :cursor (cdr p)))
                                 sels)
                   :main (or main 0))))

(defun kao-search-tests--pairs ()
  "Return `kao--sels' as a list of (anchor . cursor) pairs."
  (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
          (kao-sels-list kao--sels)))

;;;; Task 1 — find-next-match

(ert-deftest kao-search-find-forward-basic ()
  "Forward search lands on the next match with the cursor on its last char."
  (kao-search-tests--with "foo bar foo"          ; positions: foo=1-3, bar=5-7, foo=9-11
    (let* ((sel (kao-sel-make :anchor 1 :cursor 1))   ; on first f
           (r (kao-search--find-next-match sel t "foo")))
      (should r)
      (should (null (cdr r)))                          ; not wrapped
      (should (= (kao-sel-anchor (car r)) 9))
      (should (= (kao-sel-cursor (car r)) 11)))))      ; last char of 2nd foo

(ert-deftest kao-search-find-forward-wraps ()
  "Forward search with no match ahead wraps to `point-min' and flags wrapped."
  (kao-search-tests--with "foo bar baz"
    (let* ((sel (kao-sel-make :anchor 9 :cursor 11))   ; on baz, no foo ahead
           (r (kao-search--find-next-match sel t "foo")))
      (should r)
      (should (cdr r))                                  ; wrapped
      (should (= (kao-sel-anchor (car r)) 1))
      (should (= (kao-sel-cursor (car r)) 3)))))

(ert-deftest kao-search-find-backward-basic ()
  "Backward search lands on the previous match, cursor on the match start."
  (kao-search-tests--with "foo bar foo"
    (let* ((sel (kao-sel-make :anchor 9 :cursor 11))   ; on 2nd foo
           (r (kao-search--find-next-match sel nil "foo")))
      (should r)
      (should (null (cdr r)))
      ;; backward: anchor on last char, cursor on first char (then keep-direction)
      ;; ref is forward (9<=11) so keep-direction makes anchor=min, cursor=max
      (should (= (kao-sel-min (car r)) 1))
      (should (= (kao-sel-max (car r)) 3)))))

(ert-deftest kao-search-find-backward-wraps ()
  "Backward search with no earlier match wraps to `point-max'."
  (kao-search-tests--with "bar foo bar"
    (let* ((sel (kao-sel-make :anchor 1 :cursor 1))    ; on first bar, no foo before
           (r (kao-search--find-next-match sel nil "foo")))
      (should r)
      (should (cdr r))                                  ; wrapped
      (should (= (kao-sel-min (car r)) 5))
      (should (= (kao-sel-max (car r)) 7)))))

(ert-deftest kao-search-find-preserves-direction ()
  "A backward source selection yields a backward match selection."
  (kao-search-tests--with "foo bar foo"
    (let* ((sel (kao-sel-make :anchor 3 :cursor 1))    ; backward sel on 1st foo
           (r (kao-search--find-next-match sel t "foo")))
      (should r)
      ;; backward ref: keep-direction => anchor=max(11), cursor=min(9)
      (should (= (kao-sel-anchor (car r)) 11))
      (should (= (kao-sel-cursor (car r)) 9)))))

(ert-deftest kao-search-find-no-match-nil ()
  "A pattern matching nowhere returns nil (no match in the whole buffer)."
  (kao-search-tests--with "foo bar foo"
    (let ((sel (kao-sel-make :anchor 1 :cursor 1)))
      (should (null (kao-search--find-next-match sel t "zzz")))
      (should (null (kao-search--find-next-match sel nil "zzz"))))))

(ert-deftest kao-search-find-single-match-forward-wraps-to-self ()
  "With one match only, forward from past it wraps back to the same match."
  (kao-search-tests--with "xx foo xx"               ; foo = 4-6
    (let* ((sel (kao-sel-make :anchor 4 :cursor 6))    ; on foo
           (r (kao-search--find-next-match sel t "foo")))
      (should r)
      (should (cdr r))                                  ; wrapped
      (should (= (kao-sel-min (car r)) 4))
      (should (= (kao-sel-max (car r)) 6)))))

;;;; Task 1 — merge_selections

(ert-deftest kao-search-merge-forward ()
  "Forward+forward merge pulls the anchor to the min and takes the new cursor."
  (let* ((sel (kao-sel-make :anchor 3 :cursor 5))
         (new (kao-sel-make :anchor 8 :cursor 10))
         (m (kao-search--merge-selections sel new)))
    (should (= (kao-sel-anchor m) 3))   ; min(3,8)
    (should (= (kao-sel-cursor m) 10)))) ; new cursor

(ert-deftest kao-search-merge-backward ()
  "Backward+backward merge pushes the anchor to the max and takes the new cursor."
  (let* ((sel (kao-sel-make :anchor 8 :cursor 6))     ; backward
         (new (kao-sel-make :anchor 5 :cursor 2))     ; backward
         (m (kao-search--merge-selections sel new)))
    (should (= (kao-sel-anchor m) 8))   ; max(8,5)
    (should (= (kao-sel-cursor m) 2))))

(ert-deftest kao-search-merge-keeps-anchor-on-direction-flip ()
  "When directions differ the anchor is unchanged; only the cursor moves."
  (let* ((sel (kao-sel-make :anchor 3 :cursor 5))     ; forward
         (new (kao-sel-make :anchor 10 :cursor 8))    ; backward
         (m (kao-search--merge-selections sel new)))
    (should (= (kao-sel-anchor m) 3))   ; unchanged
    (should (= (kao-sel-cursor m) 8))))

;;;; Task 2 — Replace path (select-next-match)

(ert-deftest kao-search-select-next-single ()
  "`/' moves the single selection to its next match (count 1)."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 1 1)
    (kao-search--select-next t "foo" 1)
    (should (equal (kao-search-tests--pairs) '((9 . 11))))))

(ert-deftest kao-search-select-next-all-selections-move ()
  "`/' moves EVERY selection to its own next match (select_next_matches)."
  (kao-search-tests--with "ax bx cx"             ; x at 2, 5, 8
    (kao-search-tests--list '((1 . 1) (4 . 4)))  ; on a, on b
    (kao-search--select-next t "x" 1)
    ;; first sel -> x@2, second sel -> x@5
    (should (equal (kao-search-tests--pairs) '((2 . 2) (5 . 5))))))

(ert-deftest kao-search-select-next-count-nth ()
  "A count selects the Nth next match."
  (kao-search-tests--with "x x x x"              ; x at 1,3,5,7
    (kao-search-tests--span 1 1)                 ; on first x
    (kao-search--select-next t "x" 2)            ; skip to the 2nd-next
    (should (equal (kao-search-tests--pairs) '((5 . 5))))))

(ert-deftest kao-search-select-next-wraps ()
  "`/' with no match ahead wraps to the first match in the buffer."
  (kao-search-tests--with "foo bar baz"
    (kao-search-tests--span 9 11)                ; on baz
    (kao-search--select-next t "foo" 1)
    (should (equal (kao-search-tests--pairs) '((1 . 3))))))

(ert-deftest kao-search-select-next-no-match-unchanged ()
  "A pattern matching nowhere leaves the selection list unchanged."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 5 6)
    (kao-search--select-next t "zzz" 1)
    (should (equal (kao-search-tests--pairs) '((5 . 6))))))

(ert-deftest kao-search-select-next-merges-coincident ()
  "Two selections whose next match is the same merge into one."
  (kao-search-tests--with "a foo b foo"          ; foo at 3-5 and 9-11
    (kao-search-tests--list '((1 . 1) (2 . 2)))  ; both before the first foo
    (kao-search--select-next t "foo" 1)
    ;; both land on foo@3-5 -> merge to a single selection
    (should (equal (kao-search-tests--pairs) '((3 . 5))))))

(ert-deftest kao-search-select-stores-register ()
  "`kao-search' stores the typed pattern in the `/' register."
  (kao-search-tests--with "foo bar"
    (kao-search-tests--span 1 1)
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'kao--read-regex) (lambda (_p &optional _r) "bar")))
        (kao-search t nil)))
    (should (equal (kao-register-get kao-search-register) '("bar")))
    (should (equal (kao-search-tests--pairs) '((5 . 7))))))

(ert-deftest kao-search-backward-command ()
  "`<a-/>' (kao-search-backward) selects the previous match."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 9 11)                ; on 2nd foo
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'kao--read-regex) (lambda (_p &optional _r) "foo")))
        (kao-search-backward)))
    (should (equal (kao-search-tests--pairs) '((1 . 3))))))

;;;; Task 3 — Extend path (extend-to-next-match)

(ert-deftest kao-search-extend-grows-selection ()
  "`?' extends the selection from its anchor to the next match's cursor."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 1 1)                 ; forward, on first f
    (kao-search--extend-next t "foo" 1)
    ;; anchor stays at 1, cursor moves to the 2nd foo's last char (11)
    (should (equal (kao-search-tests--pairs) '((1 . 11))))))

(ert-deftest kao-search-extend-no-sort-merge ()
  "Extend does not sort-and-merge: an overlapping result is kept as extended."
  (kao-search-tests--with "foo foo foo"          ; foo at 1-3, 5-7, 9-11
    (kao-search-tests--span 1 3)                 ; on first foo
    (kao-search--extend-next t "foo" 1)          ; extend to 2nd foo
    (should (equal (kao-search-tests--pairs) '((1 . 7))))))

(ert-deftest kao-search-extend-backward ()
  "`<a-?>' extends backward to the previous match (anchor preserved)."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 11 9)                ; backward sel on 2nd foo (anchor 11)
    (kao-search--extend-next nil "foo" 1)        ; previous foo is 1-3
    ;; backward both: anchor=max(11,?) cursor=match start(1)
    (should (equal (kao-search-tests--pairs) '((11 . 1))))))

(ert-deftest kao-search-extend-drops-wrapped ()
  "A selection whose next match wraps is dropped; survivors keep the others."
  (kao-search-tests--with "foo bar foo"          ; foo 1-3, 9-11 ; bar 5-7
    ;; sel A on bar -> next foo @9 (no wrap, kept/extended)
    ;; sel B on 2nd foo -> next foo wraps to @1 (dropped)
    (kao-search-tests--list '((5 . 7) (9 . 11)))
    (kao-search--extend-next t "foo" 1)
    (should (equal (kao-search-tests--pairs) '((5 . 11))))))

(ert-deftest kao-search-extend-drop-adjusts-main ()
  "Dropping a selection at/before main decrements the main index."
  (kao-search-tests--with "foo bar foo qux foo"  ; foo 1-3, 9-11, 17-19
    ;; A on 2nd foo @9 -> next foo @17 (kept) ; B on 3rd foo @17 -> wraps (dropped)
    ;; main starts at index 1 (B); after dropping B main falls to the survivor.
    (kao-search-tests--list '((9 . 11) (17 . 19)) 1)
    (kao-search--extend-next t "foo" 1)
    (should (equal (kao-search-tests--pairs) '((9 . 19))))
    (should (= (kao-sels-main kao--sels) 0))))

(ert-deftest kao-search-extend-all-wrapped-unchanged ()
  "When every selection's next match wraps, the list is left unchanged."
  (kao-search-tests--with "xx foo xx"            ; only one foo @4-6
    (kao-search-tests--span 4 6)                 ; on the only foo -> wraps to itself
    (kao-search--extend-next t "foo" 1)
    (should (equal (kao-search-tests--pairs) '((4 . 6))))))

(ert-deftest kao-search-extend-command-binding ()
  "`kao-search-extend-forward' (`?') threads extend through `kao-search'."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 1 1)
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'kao--read-regex) (lambda (_p &optional _r) "foo")))
        (kao-search-extend-forward)))
    (should (equal (kao-search-tests--pairs) '((1 . 11))))
    (should (equal (kao-register-get kao-search-register) '("foo")))))

(ert-deftest kao-search-extend-backward-command-binding ()
  "`kao-search-extend-backward' (`<a-?>') extends to the previous match."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 11 9)                ; backward sel on 2nd foo
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'kao--read-regex) (lambda (_p &optional _r) "foo")))
        (kao-search-extend-backward)))
    ;; previous foo (1-3); both backward: anchor=max(11,3)=11, cursor=start(1)
    (should (equal (kao-search-tests--pairs) '((11 . 1))))))

;;;; incsearch — live-preview commit / abort for the search prompt

(ert-deftest kao-search-incsearch-commit-stores-register ()
  "`/' with incsearch on commits the selector and stores the validated pattern."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 1 1)                 ; on the first foo
    (let ((kao-incsearch t))
      (cl-letf (((symbol-function 'read-from-minibuffer) (lambda (&rest _) "foo")))
        (kao-search t nil)))
    (should (equal (kao-search-tests--pairs) '((9 . 11))))   ; moved to the next foo
    (should (equal (kao-register-get kao-search-register) '("foo")))))

(ert-deftest kao-search-incsearch-abort-unchanged ()
  "Aborting the incsearch search prompt restores selections and stores nothing."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 1 1)
    (kao-register-set kao-search-register '("old"))
    (let ((kao-incsearch t))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) (signal 'quit nil))))
        (kao-search t nil)))
    (should (equal (kao-search-tests--pairs) '((1 . 1))))     ; selections untouched
    (should (equal (kao-register-get kao-search-register) '("old")))))  ; not overwritten

;;;; Empty search prompt re-uses the `/' register's pattern (default_regex)

(ert-deftest kao-search-empty-commit-reapplies-register ()
  "`/<ret>' with a stored `/' pattern re-searches with it (default_regex).
Kakoune's `regex_prompt' substitutes the register content for an empty entry
\(normal.cc:958/:1021) and does not rewrite the register on that path (:1016)."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 1 1)                 ; on the first foo
    (kao-register-set kao-search-register '("foo"))
    (let ((kao-incsearch t))
      (cl-letf (((symbol-function 'read-from-minibuffer) (lambda (&rest _) "")))
        (kao-search t nil)))
    (should (equal (kao-search-tests--pairs) '((9 . 11))))   ; moved to the next foo
    (should (equal (kao-register-get kao-search-register) '("foo")))))  ; unchanged

(ert-deftest kao-search-empty-commit-reapplies-register-incsearch-off ()
  "Same fallback in the one-shot `kao--read-regex' path (incsearch off)."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 1 1)
    (kao-register-set kao-search-register '("foo"))
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
        (kao-search t nil)))
    (should (equal (kao-search-tests--pairs) '((9 . 11))))
    (should (equal (kao-register-get kao-search-register) '("foo")))))

(ert-deftest kao-search-empty-commit-empty-register-noop ()
  "An empty prompt with an empty `/' register stays a no-op (both empty)."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 1 1)
    (kao-register-set kao-search-register nil)
    (setq kao--jumps nil kao--jump-current 0)
    (let ((kao-incsearch t))
      (cl-letf (((symbol-function 'read-from-minibuffer) (lambda (&rest _) "")))
        (kao-search t nil)))
    (should (equal (kao-search-tests--pairs) '((1 . 1))))       ; unchanged
    (should (null (kao-register-get kao-search-register)))
    (should (null kao--jumps))))

;;;; P4 step 2 Task 1 — search_next (n / N / <a-n> / <a-N>)

(ert-deftest kao-search-next-replace-main-only ()
  "`n' moves ONLY the main selection to its next match; others stay put."
  (kao-search-tests--with "foo bar foo baz foo"   ; foo 1-3, 9-11, 17-19
    (kao-register-set kao-search-register '("foo"))
    (kao-search-tests--list '((1 . 3) (5 . 7)) 0) ; main on 1st foo, a 2nd sel on bar
    (kao-search--search-next 'replace t 1)
    ;; main 1-3 -> next foo 9-11 ; the bar sel is untouched
    (should (equal (kao-search-tests--pairs) '((5 . 7) (9 . 11))))
    ;; main follows its selection by identity after the sort
    (should (= (kao-sel-min (kao--main-sel)) 9))))

(ert-deftest kao-search-next-add-appends ()
  "`N' adds the main's next match as a new selection and makes it the main."
  (kao-search-tests--with "foo bar foo"
    (kao-register-set kao-search-register '("foo"))
    (kao-search-tests--span 1 3)                  ; main on 1st foo
    (kao-search--search-next 'append t 1)
    (should (equal (kao-search-tests--pairs) '((1 . 3) (9 . 11))))
    (should (= (kao-sel-min (kao--main-sel)) 9)))) ; new sel is main

(ert-deftest kao-search-next-backward ()
  "`<a-n>' moves the main to the previous match."
  (kao-search-tests--with "foo bar foo"
    (kao-register-set kao-search-register '("foo"))
    (kao-search-tests--span 9 11)                 ; on 2nd foo
    (kao-search--search-next 'replace nil 1)
    (should (equal (kao-search-tests--pairs) '((1 . 3))))))

(ert-deftest kao-search-next-count ()
  "A count repeats search_next, advancing the main N matches."
  (kao-search-tests--with "x x x x"               ; x at 1,3,5,7
    (kao-register-set kao-search-register '("x"))
    (kao-search-tests--span 1 1)
    (kao-search--search-next 'replace t 2)        ; skip 2 ahead -> x@5
    (should (equal (kao-search-tests--pairs) '((5 . 5))))))

(ert-deftest kao-search-next-add-count-adds-two ()
  "`2N' adds two consecutive matches (each from the prior new main)."
  (kao-search-tests--with "x x x x"
    (kao-register-set kao-search-register '("x"))
    (kao-search-tests--span 1 1)                  ; main on x@1
    (kao-search--search-next 'append t 2)
    (should (equal (kao-search-tests--pairs) '((1 . 1) (3 . 3) (5 . 5))))
    (should (= (kao-sel-min (kao--main-sel)) 5))))

(ert-deftest kao-search-next-empty-register ()
  "`n' with no stored pattern leaves the list unchanged (no search pattern)."
  (kao-search-tests--with "foo bar"
    (kao-register-set kao-search-register nil)
    (kao-search-tests--span 1 3)
    (kao-search--search-next 'replace t 1)
    (should (equal (kao-search-tests--pairs) '((1 . 3))))))

(ert-deftest kao-search-next-merges-onto-existing ()
  "`N' whose new match coincides with an existing selection merges."
  (kao-search-tests--with "foo bar foo"
    (kao-register-set kao-search-register '("foo"))
    (kao-search-tests--list '((1 . 3) (9 . 11)) 0) ; main on 1st foo, 2nd sel already on 2nd foo
    (kao-search--search-next 'append t 1)
    ;; main's next foo is 9-11, which already exists -> merge back to two sels
    (should (equal (kao-search-tests--pairs) '((1 . 3) (9 . 11))))))

(ert-deftest kao-search-next-command-binding ()
  "`kao-search-next' (`n') reads the count and dispatches replace-forward."
  (kao-search-tests--with "foo bar foo"
    (kao-register-set kao-search-register '("foo"))
    (kao-search-tests--span 1 3)
    (kao-search-next)
    (should (equal (kao-search-tests--pairs) '((9 . 11))))))

;;;; P4 step 2 Task 2 — use_selection_as_search_pattern (* / <a-*>)

(ert-deftest kao-search-set-pattern-smart-word ()
  "`*' on a whole word wraps it in `\\b' boundaries."
  (kao-search-tests--with "foo bar"
    (kao-search-tests--span 1 3)                  ; whole "foo"
    (kao-search-set-pattern)
    (should (equal (kao-register-get kao-search-register) '("\\bfoo\\b")))))

(ert-deftest kao-search-set-pattern-raw-no-boundaries ()
  "`<a-*>' is literal — no `\\b' even on a whole word."
  (kao-search-tests--with "foo bar"
    (kao-search-tests--span 1 3)
    (kao-search-set-pattern-raw)
    (should (equal (kao-register-get kao-search-register) '("foo")))))

(ert-deftest kao-search-set-pattern-mid-word-no-boundary ()
  "A selection starting mid-word gets no leading `\\b' (is_bow nil)."
  (kao-search-tests--with "foobar baz"
    (kao-search-tests--span 4 6)                  ; "bar" inside "foobar"
    (kao-search-set-pattern)
    ;; beg is mid-word (prev char 'o' is a word char) -> no leading \b;
    ;; end is at a word boundary (space follows) -> trailing \b
    (should (equal (kao-register-get kao-search-register) '("bar\\b")))))

(ert-deftest kao-search-set-pattern-escapes-metachars ()
  "Regex metacharacters in the text are quoted for the Emacs engine."
  (kao-search-tests--with "a.c*d"
    (kao-search-tests--span 1 5)                  ; "a.c*d"
    (kao-search-set-pattern-raw)
    (should (equal (kao-register-get kao-search-register)
                   (list (regexp-quote "a.c*d"))))
    ;; sanity: the stored pattern matches the literal text, not "a<any>c<star>"
    (should (string-match-p (car (kao-register-get kao-search-register)) "a.c*d"))
    (should-not (string-match-p (car (kao-register-get kao-search-register)) "axcxxd"))))

(ert-deftest kao-search-set-pattern-multi-dedup-join ()
  "Multiple selections dedup and join with `\\|' (trailing newline = clean parity)."
  (kao-search-tests--with "foo bar foo\n"          ; \n after the last foo (is_eow true)
    (kao-search-tests--list '((1 . 3) (5 . 7) (9 . 11)))  ; foo, bar, foo
    (kao-search-set-pattern)
    ;; "\\bfoo\\b" (dup removed) and "\\bbar\\b", first-occurrence order
    (should (equal (kao-register-get kao-search-register)
                   '("\\bfoo\\b\\|\\bbar\\b")))))

(ert-deftest kao-search-set-pattern-eob-no-trailing-boundary ()
  "family: a word at a no-trailing-newline eob gets no trailing `\\b'.
`is_eow' returns false at `buffer.is_end' (Emacs has no forced trailing newline),
so the final word's pattern is `\\bfoo' — a faithful, documented divergence from
Kakoune, whose buffer always ends in `\\n'."
  (kao-search-tests--with "x foo"                  ; foo at 3-5, end = point-max
    (kao-search-tests--span 3 5)
    (kao-search-set-pattern)
    (should (equal (kao-register-get kao-search-register) '("\\bfoo")))))

(ert-deftest kao-search-set-pattern-then-next-round-trip ()
  "`*' then `n' jumps the main to the next occurrence of the selected word."
  (kao-search-tests--with "foo bar foo baz foo"
    (kao-search-tests--span 1 3)                  ; select 1st "foo"
    (kao-search-set-pattern)                      ; / register = \bfoo\b
    (kao-search-next)                             ; n -> next foo
    (should (equal (kao-search-tests--pairs) '((9 . 11))))))

;;;; Jump-list auto-push on search commit

(ert-deftest kao-search-commit-pushes-jump ()
  "A committed search pushes ONE jump = the PRE-search selections (not the match)."
  (kao-search-tests--with "foo bar"
    (kao-search-tests--span 1 1)
    (setq kao--jumps nil kao--jump-current 0)
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'kao--read-regex) (lambda (_p &optional _r) "bar")))
        (kao-search t nil)))
    (should (equal (kao-search-tests--pairs) '((5 . 7))))   ; moved to the match
    (should (= 1 (length kao--jumps)))
    (should (equal '((1 . 1))                                ; jump = where we were
                   (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                           (kao-sels-list (cddr (car kao--jumps))))))))

(ert-deftest kao-search-abort-pushes-no-jump ()
  "An aborted search (empty regex) pushes no jump and writes no register.
Drives the real `kao--regex-command' (which now owns the jump/register
plumbing) with a nil-returning reader."
  (kao-search-tests--with "foo bar"
    (kao-search-tests--span 1 1)
    (setq kao--jumps nil kao--jump-current 0)
    (kao-register-set kao-search-register '("keep"))
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'kao--read-regex) (lambda (_p &optional _r) nil)))
        (kao-search t nil)))
    (should (null kao--jumps))
    (should (equal (kao-register-get kao-search-register) '("keep")))))

(ert-deftest kao-search-next-pushes-no-jump ()
  "`n' (search-next) does not push a jump (only the interactive prompt does)."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 1 3)
    (setq kao--jumps nil kao--jump-current 0)
    (kao-register-set kao-search-register (list "foo"))
    (kao-search-next)
    (should (equal (kao-search-tests--pairs) '((9 . 11))))   ; n moved
    (should (null kao--jumps))))

;;;; Pending register (the `"' prefix)

(ert-deftest kao-search-star-writes-named-register ()
  "`\"s *' stores the selection pattern in register s (to_lower, :1179)."
  (kao-search-tests--with "foo bar foo"
    (remhash ?s kao--registers)
    (kao-search-tests--span 1 3)               ; "foo"
    (setq kao--pending-register ?S)
    (kao-search-set-pattern-raw)
    (should (equal (kao-register-get ?s) '("foo")))
    (remhash ?s kao--registers)))

(ert-deftest kao-search-next-reads-named-register ()
  "`\"s n' repeats the pattern stored in register s, not `/'."
  (kao-search-tests--with "foo bar foo"
    (remhash ?s kao--registers)
    (kao-register-set ?s '("bar"))
    (kao-register-set kao-search-register '("zzz"))   ; `/' must NOT be used
    (kao-search-tests--span 1 1)
    (setq kao--pending-register ?s)
    (kao-search-next)
    (should (equal (kao-search-tests--pairs) '((5 . 7))))
    (remhash ?s kao--registers)))

(ert-deftest kao-search-slash-stores-to-named-register ()
  "A committed `\"s /' search saves the regex to register s (normal.cc:1085-97)."
  (kao-search-tests--with "foo bar foo"
    (remhash ?s kao--registers)
    (kao-search-tests--span 1 1)
    (setq kao--pending-register ?s)
    (let ((kao-incsearch nil))
      (cl-letf (((symbol-function 'kao--read-regex) (lambda (_p &optional _r) "bar")))
        (kao-search t nil)))
    (should (equal (kao-register-get ?s) '("bar")))
    (remhash ?s kao--registers)))

;;;; Search-match highlighting (-2)

(defun kao-search-tests--hl-spans ()
  "Return the live highlight overlays as sorted (BEG . END) pairs."
  (sort (mapcar (lambda (ov) (cons (overlay-start ov) (overlay-end ov)))
                kao--search-hl-overlays)
        (lambda (a b) (< (car a) (car b)))))

(ert-deftest kao-search-hl-set-covers-all-matches ()
  "`kao-search--hl-set' overlays every match with `kao-search-match'."
  (kao-search-tests--with "foo bar foo"
    (kao-search--hl-set "foo")
    (should (equal (kao-search-tests--hl-spans) '((1 . 4) (9 . 12))))
    (dolist (ov kao--search-hl-overlays)
      (should (eq (overlay-get ov 'face) 'kao-search-match)))
    (kao-search--hl-clear)))

;;;; The highlight scan is memoized across unchanged pattern/buffer

(ert-deftest kao-search-hl-memoizes-unchanged ()
  "Consecutive `hl-set' with unchanged pattern AND buffer scan the body ONCE.
The buffer-local memo keys on (pattern . `buffer-chars-modified-tick'); a repeat
`n' press with the overlays still live short-circuits — the actual hot path."
  (kao-search-tests--with "foo bar foo"
    (let ((scans 0))
      (cl-letf* ((real (symbol-function 'kao-search--hl-scan))
                 ((symbol-function 'kao-search--hl-scan)
                  (lambda (&rest args) (setq scans (1+ scans)) (apply real args))))
        (kao-search--hl-set "foo")            ; scan 1
        (kao-search--hl-set "foo")            ; unchanged -> memoized, no scan
        (should (= scans 1))
        (should (equal (kao-search-tests--hl-spans) '((1 . 4) (9 . 12))))))
    (kao-search--hl-clear)))

(ert-deftest kao-search-hl-memo-invalidates-on-edit ()
  "A buffer edit (tick bump) invalidates the memo, so `hl-set' re-scans."
  (kao-search-tests--with "foo bar foo"
    (let ((scans 0))
      (cl-letf* ((real (symbol-function 'kao-search--hl-scan))
                 ((symbol-function 'kao-search--hl-scan)
                  (lambda (&rest args) (setq scans (1+ scans)) (apply real args))))
        (kao-search--hl-set "foo")            ; scan 1
        (goto-char (point-max)) (insert " foo")  ; bumps chars-modified-tick
        (kao-search--hl-set "foo")            ; buffer changed -> scan 2
        (should (= scans 2))
        (should (equal (kao-search-tests--hl-spans) '((1 . 4) (9 . 12) (13 . 16))))))
    (kao-search--hl-clear)))

(ert-deftest kao-search-hl-memo-invalidates-on-pattern-change ()
  "A different pattern invalidates the memo even with the buffer unchanged."
  (kao-search-tests--with "foo bar foo"
    (let ((scans 0))
      (cl-letf* ((real (symbol-function 'kao-search--hl-scan))
                 ((symbol-function 'kao-search--hl-scan)
                  (lambda (&rest args) (setq scans (1+ scans)) (apply real args))))
        (kao-search--hl-set "foo")            ; scan 1
        (kao-search--hl-set "bar")            ; new pattern -> scan 2
        (should (= scans 2))
        (should (equal (kao-search-tests--hl-spans) '((5 . 8))))))
    (kao-search--hl-clear)))

(ert-deftest kao-search-hl-search-next-sets-and-motion-clears ()
  "`n' installs the highlight; the next non-search command clears it."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 1 1)
    (kao-register-set kao-search-register '("foo"))
    (kao-search-next)
    (should (= 2 (length kao--search-hl-overlays)))
    (should (memq #'kao-search--hl-maybe-clear
                  (buffer-local-value 'post-command-hook (current-buffer))))
    ;; A search command keeps it; a motion clears it (the hook decides).
    (let ((this-command 'kao-search-next))
      (kao-search--hl-maybe-clear)
      (should (= 2 (length kao--search-hl-overlays))))
    (let ((this-command 'kao-move-right))
      (kao-search--hl-maybe-clear))
    (should (null kao--search-hl-overlays))
    (should-not (memq #'kao-search--hl-maybe-clear
                      (buffer-local-value 'post-command-hook (current-buffer))))
    (remhash kao-search-register kao--registers)))

(ert-deftest kao-search-hl-params-keys-keep-highlight ()
  "The pending-params keys (digits/DEL/\") leave the highlight alive."
  (kao-search-tests--with "aaa"
    (kao-search--hl-set "a")
    (dolist (cmd '(kao-digit kao-count-backspace kao-select-register))
      (let ((this-command cmd))
        (kao-search--hl-maybe-clear))
      (should (= 3 (length kao--search-hl-overlays))))
    (kao-search--hl-clear)))

(ert-deftest kao-search-hl-pattern-set-star-highlights ()
  "`*' (set pattern from selection) highlights immediately."
  (kao-search-tests--with "foo bar foo"
    (kao-search-tests--span 1 3)                  ; "foo"
    (kao-search--set-search-pattern nil)          ; <a-*> literal
    (should (= 2 (length kao--search-hl-overlays)))
    (kao-search--hl-clear)
    (remhash kao-search-register kao--registers)))

(ert-deftest kao-search-hl-defcustom-off-means-none ()
  "With `kao-search-highlight' nil no overlays appear, and no hook installs."
  (kao-search-tests--with "foo foo"
    (let ((kao-search-highlight nil))
      (kao-search--hl-set "foo"))
    (should (null kao--search-hl-overlays))
    (should-not (memq #'kao-search--hl-maybe-clear
                      (buffer-local-value 'post-command-hook (current-buffer))))))

(ert-deftest kao-search-hl-refresh-reuses-overlays ()
  "A refresh reuses pooled overlays and deletes the surplus."
  (kao-search-tests--with "aaa b"
    (kao-search--hl-set "a")                      ; 3 overlays
    (let ((old (copy-sequence kao--search-hl-overlays)))
      (kao-search--hl-set "b")                    ; 1 overlay
      (should (= 1 (length kao--search-hl-overlays)))
      (should (memq (car kao--search-hl-overlays) old))
      ;; surplus overlays were detached, not leaked
      (should (= 1 (length (seq-filter #'overlay-buffer old)))))
    (kao-search--hl-clear)))

(ert-deftest kao-search-hl-max-caps-and-zero-width-safe ()
  "The overlay count is capped; a zero-width-matching pattern terminates."
  (kao-search-tests--with "aaaaaaaa"
    (let ((kao-search-highlight-max 3))
      (kao-search--hl-set "a")
      (should (= 3 (length kao--search-hl-overlays))))
    ;; `b*' matches empty everywhere: must terminate and create nothing.
    (kao-search--hl-set "b*")
    (should (null kao--search-hl-overlays))))

;;;; Captures: the search scan fills captures

(ert-deftest kao-search-find-next-match-fills-captures ()
  "The shared scan attaches the full submatch list to its result."
  (kao-search-tests--with "x ab1 y"
    (kao-search-tests--span 1 1)
    (let ((r (kao-search--find-next-match
              (car (kao-sels-list kao--sels)) t
              "\\([a-z]+\\)\\([0-9]\\)")))
      (should r)
      (should (equal (kao-sel-captures (car r)) '("ab1" "ab" "1"))))))

(ert-deftest kao-search-next-fills-captures-on-live-list ()
  "`n' leaves the landed selection carrying the match's captures."
  (kao-search-tests--with "x ab1 cd2"
    (kao-search-tests--span 1 1)
    (kao-register-set ?/ (list "\\([a-z]+\\)\\([0-9]\\)"))
    (kao-search--select-next t "\\([a-z]+\\)\\([0-9]\\)" 1)
    (should (equal (kao-sel-captures (car (kao-sels-list kao--sels)))
                   '("ab1" "ab" "1")))))

;;;; Case-fold in search + highlight

(ert-deftest kao-search-casefold-default-sensitive ()
  "Default `/' is case-sensitive: `foo' skips `FOO', lands on lowercase `foo'."
  (let ((kao-search-case-fold nil))
    (kao-search-tests--with "xFOOfoox"        ; x=1 F=2 O=3 O=4 f=5 o=6 o=7 x=8
      (let* ((sel (kao-sel-make :anchor 1 :cursor 1))
             (r (kao-search--find-next-match sel t "foo")))
        (should r)
        (should (= (kao-sel-min (car r)) 5))))))   ; lowercase foo, not FOO

(ert-deftest kao-search-casefold-inline-i ()
  "`(?i)foo' folds (token stripped): lands on `FOO'."
  (let ((kao-search-case-fold nil))
    (kao-search-tests--with "xFOOx"           ; x=1 F=2 O=3 O=4 x=5
      (let* ((sel (kao-sel-make :anchor 1 :cursor 1))
             (r (kao-search--find-next-match sel t "(?i)foo")))
        (should r)
        (should (= (kao-sel-min (car r)) 2))))))   ; matched FOO at 2-4

(ert-deftest kao-search-casefold-highlight-matches-search ()
  "Highlight honors the same fold as search: `(?i)foo' highlights both cases."
  (let ((kao-search-case-fold nil)
        (kao-search-highlight t))
    (kao-search-tests--with "FOO foo"
      (kao-search--hl-set "(?i)foo")
      (should (= (length kao--search-hl-overlays) 2))
      (kao-search--hl-set "foo")              ; default sensitive: only lowercase
      (should (= (length kao--search-hl-overlays) 1)))))

;;;; Opt-in search count (hel-study-6)

(ert-deftest kao-search-count-off-records-nothing ()
  "With `kao-search-count' nil a search landing records no count — zero cost, the
faithful default (hel-study-6)."
  (kao-search-tests--with "x x x x x"         ; x at 1 3 5 7 9 = 5 matches
    (let ((kao-search-count nil))
      (kao-register-set kao-search-register '("x"))
      (kao-search-tests--span 1 1)
      (kao-search--search-next 'replace t 1)
      (should (null kao--search-count)))))

(ert-deftest kao-search-count-records-index-and-total ()
  "With `kao-search-count' set, landing on the 2nd of 5 matches records
`(2 . 5)' in `kao--search-count' (hel-study-6)."
  (kao-search-tests--with "x x x x x"
    (let ((kao-search-count t))
      (kao-register-set kao-search-register '("x"))
      (kao-search-tests--span 1 1)            ; main on match 1 (x@1)
      (kao-search--search-next 'replace t 1)  ; n -> match 2 (x@3)
      (should (equal kao--search-count '(2 . 5))))))

(ert-deftest kao-search-count-advances-across-repeated-next ()
  "Each `n' re-records the position even though the pattern and buffer are
unchanged: the count is recorded at the LANDING, so the highlight scan's memo
never stales it (hel-study-6)."
  (kao-search-tests--with "x x x x x"
    (let ((kao-search-count t))
      (kao-register-set kao-search-register '("x"))
      (kao-search-tests--span 1 1)
      (kao-search--search-next 'replace t 1)  ; -> match 2
      (should (equal kao--search-count '(2 . 5)))
      (kao-search--search-next 'replace t 1)  ; -> match 3 (memo would stale a scan-time index)
      (should (equal kao--search-count '(3 . 5))))))

;;;; Mode-off guard sweep — (ADDITIVE pins)

;; An M-x-discoverable search command run with `kao-mode' off signals the shared
;; `kao--assert-mode' user-error instead of walking into a nil `kao--sels'.

(ert-deftest kao-search-next-mode-off-guards ()
  "I4: `n' (`kao-search-next') via M-x with `kao-mode' off signals the shared
guard, not the cryptic (wrong-type-argument kao-sels nil)."
  (with-temp-buffer                     ; fundamental-mode temp buffer: mode OFF
    (insert "abc")
    (let ((err (should-error (call-interactively #'kao-search-next)
                             :type 'user-error)))
      (should (string-match-p "kao-mode is not active" (cadr err))))))

(ert-deftest kao-search-next-add-mode-off-guards ()
  "I4: `N' (`kao-search-next-add') via M-x with `kao-mode' off signals the
shared guard."
  (with-temp-buffer
    (insert "abc")
    (let ((err (should-error (call-interactively #'kao-search-next-add)
                             :type 'user-error)))
      (should (string-match-p "kao-mode is not active" (cadr err))))))

(provide 'kao-search-tests)
;;; kao-search-tests.el ends here
