;;; kao-selection-tests.el --- Tests for kao-selection -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the kao selection-list algebra.  These tests are pure and
;; buffer-free: they construct selections from integer positions and assert the
;; algebra matches Kakoune's behaviour (see references/kakoune/src/selection.cc).

;;; Code:

(require 'ert)
(require 'kao-selection)

;;;; Single-selection operations

(ert-deftest kao-sel-min-max-forward ()
  "Forward selection: anchor is min, cursor is max."
  (let ((s (kao-sel-make :anchor 1 :cursor 3)))
    (should (= (kao-sel-min s) 1))
    (should (= (kao-sel-max s) 3))
    (should (kao-sel-forward-p s))
    (should (= (kao-sel-length s) 3))
    (should (= (kao-sel-beg s) 1))
    (should (= (kao-sel-end s) 4))))

(ert-deftest kao-sel-min-max-backward ()
  "Backward selection: cursor is min, anchor is max; span unchanged."
  (let ((s (kao-sel-make :anchor 3 :cursor 1)))
    (should (= (kao-sel-min s) 1))
    (should (= (kao-sel-max s) 3))
    (should-not (kao-sel-forward-p s))
    (should (= (kao-sel-length s) 3))
    (should (= (kao-sel-beg s) 1))
    (should (= (kao-sel-end s) 4))))

(ert-deftest kao-sel-single-char ()
  "Single-char selection (anchor = cursor): forward, length 1."
  (let ((s (kao-sel-make :anchor 2 :cursor 2)))
    (should (= (kao-sel-min s) 2))
    (should (= (kao-sel-max s) 2))
    (should (kao-sel-forward-p s))
    (should (= (kao-sel-length s) 1))
    (should (= (kao-sel-beg s) 2))
    (should (= (kao-sel-end s) 3))))

(ert-deftest kao-sel-collapse ()
  "Collapse reduces a selection to its cursor as a single char."
  (let ((s (kao-sel-collapse (kao-sel-make :anchor 1 :cursor 3))))
    (should (= (kao-sel-anchor s) 3))
    (should (= (kao-sel-cursor s) 3))
    (should (= (kao-sel-length s) 1)))
  (let ((s (kao-sel-collapse (kao-sel-make :anchor 5 :cursor 2))))
    (should (= (kao-sel-anchor s) 2))
    (should (= (kao-sel-cursor s) 2))))

(ert-deftest kao-sel-flip ()
  "Flip swaps anchor and cursor, reversing direction but not span."
  (let* ((s (kao-sel-make :anchor 1 :cursor 3))
         (f (kao-sel-flip s)))
    (should (= (kao-sel-anchor f) 3))
    (should (= (kao-sel-cursor f) 1))
    (should-not (kao-sel-forward-p f))
    (should (= (kao-sel-min f) 1))
    (should (= (kao-sel-max f) 3))))

(ert-deftest kao-sel-ensure-forward ()
  "ensure-forward yields cursor>=anchor, flipping only backward selections."
  (let ((f (kao-sel-ensure-forward (kao-sel-make :anchor 3 :cursor 1))))
    (should (kao-sel-forward-p f))
    (should (= (kao-sel-anchor f) 1))
    (should (= (kao-sel-cursor f) 3)))
  (let ((f (kao-sel-ensure-forward (kao-sel-make :anchor 1 :cursor 3))))
    (should (= (kao-sel-anchor f) 1))
    (should (= (kao-sel-cursor f) 3))))

;;;; Pairwise predicates

(ert-deftest kao-sel-overlaps ()
  "Overlap is inclusive and direction-agnostic; adjacency is not overlap."
  (should (kao-sel-overlaps-p (kao-sel-make :anchor 0 :cursor 2)
                              (kao-sel-make :anchor 1 :cursor 3)))
  (should (kao-sel-overlaps-p (kao-sel-make :anchor 0 :cursor 5)
                              (kao-sel-make :anchor 2 :cursor 3)))
  (should-not (kao-sel-overlaps-p (kao-sel-make :anchor 0 :cursor 0)
                                  (kao-sel-make :anchor 1 :cursor 1)))
  (should-not (kao-sel-overlaps-p (kao-sel-make :anchor 0 :cursor 1)
                                  (kao-sel-make :anchor 5 :cursor 6)))
  ;; backward selections overlap by span, irrespective of direction
  (should (kao-sel-overlaps-p (kao-sel-make :anchor 2 :cursor 0)
                              (kao-sel-make :anchor 3 :cursor 1))))

(ert-deftest kao-sel-touches ()
  "Touching covers overlap and immediate adjacency, but not a gap."
  (should (kao-sel-touches-p (kao-sel-make :anchor 0 :cursor 0)
                             (kao-sel-make :anchor 1 :cursor 1)))
  (should (kao-sel-touches-p (kao-sel-make :anchor 0 :cursor 2)
                             (kao-sel-make :anchor 1 :cursor 3)))
  (should-not (kao-sel-touches-p (kao-sel-make :anchor 0 :cursor 0)
                                 (kao-sel-make :anchor 2 :cursor 2))))

(ert-deftest kao-sel-compare ()
  "Sort order is by min, ties broken by max."
  (should (kao-sel-compare (kao-sel-make :anchor 0 :cursor 1)
                           (kao-sel-make :anchor 2 :cursor 3)))
  (should-not (kao-sel-compare (kao-sel-make :anchor 2 :cursor 3)
                               (kao-sel-make :anchor 0 :cursor 1)))
  (should (kao-sel-compare (kao-sel-make :anchor 1 :cursor 2)
                           (kao-sel-make :anchor 1 :cursor 5)))
  (should-not (kao-sel-compare (kao-sel-make :anchor 1 :cursor 5)
                               (kao-sel-make :anchor 1 :cursor 2))))

;;;; Selection-list algebra

(ert-deftest kao-sels-sort-tracks-main ()
  "Sorting reorders by position and follows the main selection."
  (let* ((a (kao-sel-make :anchor 10 :cursor 10))
         (b (kao-sel-make :anchor 2 :cursor 2))
         (c (kao-sel-make :anchor 5 :cursor 5))
         (sels (kao-sels-make :list (list a b c) :main 0))
         (sorted (kao-sels-sort sels)))
    (should (equal (mapcar #'kao-sel-min (kao-sels-list sorted)) '(2 5 10)))
    (should (= (kao-sels-main sorted) 2))
    (should (eq (nth (kao-sels-main sorted) (kao-sels-list sorted)) a))))

(ert-deftest kao-sels-merge-overlapping-follows-main ()
  "Overlapping neighbours merge; main folds into the survivor."
  (let* ((a (kao-sel-make :anchor 0 :cursor 2))
         (b (kao-sel-make :anchor 1 :cursor 3))
         (c (kao-sel-make :anchor 10 :cursor 10))
         (sels (kao-sels-make :list (list a b c) :main 1))
         (merged (kao-sels-merge-overlapping sels))
         (lst (kao-sels-list merged)))
    (should (= (length lst) 2))
    (should (= (kao-sel-min (nth 0 lst)) 0))
    (should (= (kao-sel-max (nth 0 lst)) 3))
    (should (= (kao-sel-min (nth 1 lst)) 10))
    (should (= (kao-sels-main merged) 0))))

(ert-deftest kao-sels-merge-overlapping-after-main ()
  "A merge occurring after the main selection leaves the main index intact."
  (let* ((a (kao-sel-make :anchor 0 :cursor 0))
         (b (kao-sel-make :anchor 5 :cursor 8))
         (c (kao-sel-make :anchor 6 :cursor 7))
         (sels (kao-sels-make :list (list a b c) :main 0))
         (merged (kao-sels-merge-overlapping sels))
         (lst (kao-sels-list merged)))
    (should (= (length lst) 2))
    (should (= (kao-sels-main merged) 0))
    (should (= (kao-sel-min (nth 1 lst)) 5))
    (should (= (kao-sel-max (nth 1 lst)) 8))))

(ert-deftest kao-sels-merge-preserves-direction ()
  "The survivor of a merge keeps its direction while its span grows."
  (let* ((a (kao-sel-make :anchor 2 :cursor 0))   ; backward, span 0..2
         (b (kao-sel-make :anchor 1 :cursor 4))   ; span 1..4, overlaps a
         (sels (kao-sels-make :list (list a b) :main 0))
         (merged (kao-sels-merge-overlapping sels))
         (s (nth 0 (kao-sels-list merged))))
    (should (= (length (kao-sels-list merged)) 1))
    (should (= (kao-sel-min s) 0))
    (should (= (kao-sel-max s) 4))
    (should-not (kao-sel-forward-p s))
    (should (= (kao-sel-anchor s) 4))
    (should (= (kao-sel-cursor s) 0))))

(ert-deftest kao-sels-merge-consecutive ()
  "Adjacent single-char selections merge under merge-consecutive."
  (let* ((a (kao-sel-make :anchor 0 :cursor 0))
         (b (kao-sel-make :anchor 1 :cursor 1))
         (c (kao-sel-make :anchor 2 :cursor 2))
         (d (kao-sel-make :anchor 10 :cursor 11))
         (sels (kao-sels-make :list (list a b c d) :main 0))
         (merged (kao-sels-merge-consecutive sels))
         (lst (kao-sels-list merged)))
    (should (= (length lst) 2))
    (should (= (kao-sel-min (nth 0 lst)) 0))
    (should (= (kao-sel-max (nth 0 lst)) 2))
    (should (= (kao-sel-min (nth 1 lst)) 10))))

(ert-deftest kao-sels-sort-and-merge ()
  "Sort-and-merge sorts first, then folds overlaps, tracking main."
  (let* ((a (kao-sel-make :anchor 5 :cursor 7))
         (b (kao-sel-make :anchor 0 :cursor 2))
         (c (kao-sel-make :anchor 6 :cursor 9))
         (sels (kao-sels-make :list (list a b c) :main 2))
         (res (kao-sels-sort-and-merge-overlapping sels))
         (lst (kao-sels-list res)))
    (should (= (length lst) 2))
    (should (= (kao-sel-min (nth 0 lst)) 0))
    (should (= (kao-sel-min (nth 1 lst)) 5))
    (should (= (kao-sel-max (nth 1 lst)) 9))
    (should (= (kao-sels-main res) 1))))

(ert-deftest kao-sels-sort-and-merge-fast-path ()
  "An ascending non-overlapping list survives verbatim (fast path).
The result is value-equal with the main preserved, but a distinct struct, so
a caller's `setf' of the result's main cannot corrupt the input."
  (let* ((a (kao-sel-make :anchor 0 :cursor 2))
         (b (kao-sel-make :anchor 7 :cursor 4))     ; backward, min 4 > max 2
         (c (kao-sel-make :anchor 9 :cursor 9))     ; adjacent, min 9 > max 7
         (sels (kao-sels-make :list (list a b c) :main 1))
         (res (kao-sels-sort-and-merge-overlapping sels)))
    (should (equal (kao-sels-list res) (list a b c)))
    (should (= (kao-sels-main res) 1))
    (should-not (eq res sels))
    (setf (kao-sels-main res) 0)
    (should (= (kao-sels-main sels) 1))))

(ert-deftest kao-sels-sort-and-merge-sorted-but-overlapping ()
  "A sorted list with an overlap still merges (the fused test is not
order-only): the fast-path predicate must reject B.min <= A.max even when
the order itself is fine."
  (let* ((a (kao-sel-make :anchor 0 :cursor 5))
         (b (kao-sel-make :anchor 5 :cursor 8))     ; sorted, but overlaps a
         (sels (kao-sels-make :list (list a b) :main 1))
         (res (kao-sels-sort-and-merge-overlapping sels))
         (lst (kao-sels-list res)))
    (should (= (length lst) 1))
    (should (= (kao-sel-min (nth 0 lst)) 0))
    (should (= (kao-sel-max (nth 0 lst)) 8))
    (should (= (kao-sels-main res) 0))))

(ert-deftest kao-sels-sort-and-merge-equal-min-tie ()
  "An equal-min adjacent pair takes the full path and merges (ties always
overlap, since both selections cover their shared min)."
  (let* ((a (kao-sel-make :anchor 3 :cursor 3))
         (b (kao-sel-make :anchor 3 :cursor 6))
         (sels (kao-sels-make :list (list a b) :main 0))
         (res (kao-sels-sort-and-merge-overlapping sels))
         (lst (kao-sels-list res)))
    (should (= (length lst) 1))
    (should (= (kao-sel-min (nth 0 lst)) 3))
    (should (= (kao-sel-max (nth 0 lst)) 6))))

(ert-deftest kao-sels-sort-and-merge-unsorted-main-identity ()
  "An unsorted non-overlapping list sorts with main tracked by identity."
  (let* ((a (kao-sel-make :anchor 10 :cursor 12))
         (b (kao-sel-make :anchor 0 :cursor 2))
         (sels (kao-sels-make :list (list a b) :main 0))
         (res (kao-sels-sort-and-merge-overlapping sels))
         (lst (kao-sels-list res)))
    (should (= (length lst) 2))
    (should (eq (nth 0 lst) b))
    (should (eq (nth 1 lst) a))
    (should (= (kao-sels-main res) 1))))

(ert-deftest kao-sels-sort-duplicate-min-tracks-main ()
  "Stable sort keeps original order among equal-min selections and main with it."
  (let* ((a (kao-sel-make :anchor 5 :cursor 9))   ; min 5, later max
         (b (kao-sel-make :anchor 5 :cursor 6))   ; min 5, earlier max -> sorts first
         (c (kao-sel-make :anchor 0 :cursor 0))
         (sels (kao-sels-make :list (list a b c) :main 0)) ; main = a
         (sorted (kao-sels-sort sels))
         (lst (kao-sels-list sorted)))
    ;; order: c(0), b(5,6), a(5,9)
    (should (eq (nth 0 lst) c))
    (should (eq (nth 1 lst) b))
    (should (eq (nth 2 lst) a))
    (should (= (kao-sels-main sorted) 2))
    (should (eq (nth (kao-sels-main sorted) lst) a))))

(ert-deftest kao-sels-merge-overlapping-chain-main-last ()
  "A 3-way overlap chain folds into one selection; trailing main follows to 0."
  (let* ((a (kao-sel-make :anchor 0 :cursor 3))
         (b (kao-sel-make :anchor 2 :cursor 6))   ; overlaps a
         (c (kao-sel-make :anchor 5 :cursor 8))   ; overlaps b (via extended survivor)
         (sels (kao-sels-make :list (list a b c) :main 2)) ; main = c
         (merged (kao-sels-merge-overlapping sels))
         (lst (kao-sels-list merged)))
    (should (= (length lst) 1))
    (should (= (kao-sel-min (nth 0 lst)) 0))
    (should (= (kao-sel-max (nth 0 lst)) 8))
    (should (= (kao-sels-main merged) 0))))

;;;; kao-sel-extend-to (merge_selections, normal.cc:53) — Extend-mode core

(defun kao-selection-tests--merge (a c na nc)
  "Return (ANCHOR . CURSOR) of `kao-sel-extend-to' on (A.C) toward (NA.NC)."
  (let ((r (kao-sel-extend-to (kao-sel-make :anchor a :cursor c)
                              (kao-sel-make :anchor na :cursor nc))))
    (cons (kao-sel-anchor r) (kao-sel-cursor r))))

(ert-deftest kao-sel-extend-to-merge-selection-vectors ()
  "Port of Kakoune's `test_merge_selection' (normal.cc:68-76), abstract positions.
`kao-sel-extend-to' is buffer-free, so the C++ vectors map directly: a
Kakoune `Selection{{0,a},{0,c}}' is a kao-sel anchor=a cursor=c."
  (let ((merge #'kao-selection-tests--merge))
    (should (equal '(1 . 4) (funcall merge 1 2 3 4)))   ; both forward -> anchor min
    (should (equal '(1 . 2) (funcall merge 1 2 1 2)))   ; identical
    (should (equal '(1 . 0) (funcall merge 1 2 0 0)))   ; zero-width new: anchor kept
    (should (equal '(0 . 3) (funcall merge 1 2 0 3)))   ; forward, new anchor earlier
    (should (equal '(1 . 2) (funcall merge 1 3 4 2)))   ; new backward, sel fwd: kept
    (should (equal '(1 . 1) (funcall merge 1 2 1 1))))) ; zero-width new at anchor

(ert-deftest kao-sel-extend-to-backward-pushes-anchor-to-max ()
  "Two backward selections: the anchor is pushed to the max of the two."
  ;; sel backward a8 c4 ; new backward a9 c6 -> anchor max(8,9)=9, cursor 6.
  (let ((r (kao-sel-extend-to (kao-sel-make :anchor 8 :cursor 4)
                              (kao-sel-make :anchor 9 :cursor 6))))
    (should (= (kao-sel-anchor r) 9))
    (should (= (kao-sel-cursor r) 6))))

;;;; Captures: carry rules in the pure algebra

(ert-deftest kao-sel-captures-carried-by-collapse-flip-ensure-forward ()
  "Collapse/flip/ensure-forward carry captures (C++ in-place mutations)."
  (let ((s (kao-sel-make :anchor 1 :cursor 3 :captures '("ab" "a"))))
    (should (equal (kao-sel-captures (kao-sel-collapse s)) '("ab" "a")))
    (should (equal (kao-sel-captures (kao-sel-flip s)) '("ab" "a")))
    (should (equal (kao-sel-captures (kao-sel-ensure-forward s)) '("ab" "a")))
    ;; backward input exercises ensure-forward's flip arm
    (let ((b (kao-sel-make :anchor 3 :cursor 1 :captures '("x"))))
      (should (equal (kao-sel-captures (kao-sel-ensure-forward b)) '("x"))))))

(ert-deftest kao-sel-captures-extend-to-keeps-sel-not-new ()
  "`kao-sel-extend-to' carries SEL's captures, not NEW-SEL's.
merge_selections (normal.cc:53) mutates SEL in place; the `select()' rule
that lets a result's captures win lives in the callers (normal.cc:118-119)."
  (let ((r (kao-sel-extend-to
            (kao-sel-make :anchor 1 :cursor 3 :captures '("old"))
            (kao-sel-make :anchor 5 :cursor 7 :captures '("new")))))
    (should (equal (kao-sel-captures r) '("old")))))

(ert-deftest kao-sel-captures-merge-overlapping-keeps-survivor ()
  "Merging overlapping selections keeps the survivor's captures.
merge_overlapping (selection.cc:99-125) widens begin[i] in place; the
absorbed selection's captures drop."
  (let* ((a (kao-sel-make :anchor 1 :cursor 5 :captures '("first")))
         (b (kao-sel-make :anchor 4 :cursor 8 :captures '("second")))
         (merged (kao-sels-merge-overlapping
                  (kao-sels-make :list (list a b) :main 0)))
         (lst (kao-sels-list merged)))
    (should (= (length lst) 1))
    (should (equal (kao-sel-captures (car lst)) '("first")))))

(ert-deftest kao-sel-target-collapse-preserves-flip-ensure-forward-reset ()
  "Target rides the cursor coord: `;' preserves it; `<a-;>'/`<a-:>' reset.
collapse keeps the cursor unchanged so target survives; flip's new cursor is
the old anchor, and `ensure_forward' assigns a plain BufferCoord into the cursor
unconditionally (normal.cc:2365) — both reset target to nil, even the
already-forward ensure-forward arm."
  (let ((s (kao-sel-make :anchor 1 :cursor 5 :target 7)))
    (should (eql (kao-sel-target (kao-sel-collapse s)) 7))
    (should (null (kao-sel-target (kao-sel-flip s))))
    (should (null (kao-sel-target (kao-sel-ensure-forward s))))))

(ert-deftest kao-sel-target-extend-to-takes-new-sel ()
  "`kao-sel-extend-to' carries NEW-SEL's target (the moved cursor), not SEL's.
captures ride the match (SEL's), target rides the cursor (NEW-SEL's)."
  (let ((r (kao-sel-extend-to
            (kao-sel-make :anchor 1 :cursor 3 :captures '("old") :target 2)
            (kao-sel-make :anchor 5 :cursor 7 :captures '("new") :target 9))))
    (should (eql (kao-sel-target r) 9))
    (should (equal (kao-sel-captures r) '("old")))))

;;;; Sorted flag: set ONLY where sortedness is proved

(ert-deftest kao-sels-make-defaults-sorted-nil ()
  "A raw `kao-sels-make' defaults `sorted' nil = unknown — the conservative
value every non-normalising site inherits (the render walk then tests every
selection rather than early-breaking)."
  (should-not (kao-sels-sorted
               (kao-sels-make :list (list (kao-sel-make :anchor 5 :cursor 5)
                                          (kao-sel-make :anchor 1 :cursor 1))
                              :main 0))))

(ert-deftest kao-sels-sort-marks-sorted ()
  "`kao-sels-sort' stamps its result `sorted' t — the multi-element sort branch
and the trivially-ordered single-element copy branch both."
  (should (kao-sels-sorted
           (kao-sels-sort (kao-sels-make
                           :list (list (kao-sel-make :anchor 10 :cursor 10)
                                       (kao-sel-make :anchor 2 :cursor 2))
                           :main 0))))
  (should (kao-sels-sorted
           (kao-sels-sort (kao-sels-make
                           :list (list (kao-sel-make :anchor 5 :cursor 5))
                           :main 0)))))

(ert-deftest kao-sels-merge-marks-sorted ()
  "`kao-sels-merge-overlapping'/`-consecutive' stamp `sorted' t (sorted input
in -> sorted out), covering the multi-element fold and the single-element copy."
  (should (kao-sels-sorted
           (kao-sels-merge-overlapping
            (kao-sels-make :list (list (kao-sel-make :anchor 0 :cursor 2)
                                       (kao-sel-make :anchor 1 :cursor 3))
                           :main 0))))
  (should (kao-sels-sorted
           (kao-sels-merge-consecutive
            (kao-sels-make :list (list (kao-sel-make :anchor 0 :cursor 0)
                                       (kao-sel-make :anchor 1 :cursor 1))
                           :main 0))))
  (should (kao-sels-sorted
           (kao-sels-merge-overlapping
            (kao-sels-make :list (list (kao-sel-make :anchor 3 :cursor 3))
                           :main 0)))))

(ert-deftest kao-sels-sort-and-merge-marks-sorted ()
  "`kao-sels-sort-and-merge-overlapping' returns `sorted' t on BOTH the
ascending fast path (the scan just proved it) and the sort+merge slow path."
  ;; fast path: already ascending, non-overlapping
  (should (kao-sels-sorted
           (kao-sels-sort-and-merge-overlapping
            (kao-sels-make :list (list (kao-sel-make :anchor 0 :cursor 2)
                                       (kao-sel-make :anchor 9 :cursor 9))
                           :main 0))))
  ;; slow path: unsorted input forces sort+merge, which still yields sorted t
  (should (kao-sels-sorted
           (kao-sels-sort-and-merge-overlapping
            (kao-sels-make :list (list (kao-sel-make :anchor 9 :cursor 9)
                                       (kao-sel-make :anchor 0 :cursor 2))
                           :main 0)))))

(ert-deftest kao-sels-copy-carries-sorted ()
  "`kao-sels-copy' carries the `sorted' flag through a copy."
  (let ((s (kao-sels-sort (kao-sels-make
                           :list (list (kao-sel-make :anchor 2 :cursor 2)
                                       (kao-sel-make :anchor 5 :cursor 5))
                           :main 0))))
    (should (kao-sels-sorted s))
    (should (kao-sels-sorted (kao-sels-copy s)))))

(ert-deftest kao-snapshot-sels-carries-sorted ()
  "`kao--snapshot-sels' carries the source's `sorted' flag: the deep copy is an
order-preserving rebuild with each min/max unchanged, so a sorted source stays
sorted and a raw (nil) source stays nil."
  (let ((sorted-src (kao-sels-sort (kao-sels-make
                                    :list (list (kao-sel-make :anchor 2 :cursor 2)
                                                (kao-sel-make :anchor 5 :cursor 5))
                                    :main 0)))
        (raw-src (kao-sels-make
                  :list (list (kao-sel-make :anchor 5 :cursor 5)
                              (kao-sel-make :anchor 2 :cursor 2))
                  :main 0)))
    (should (kao-sels-sorted (kao--snapshot-sels sorted-src)))
    (should-not (kao-sels-sorted (kao--snapshot-sels raw-src)))))

(provide 'kao-selection-tests)
;;; kao-selection-tests.el ends here
