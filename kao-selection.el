;;; kao-selection.el --- Selection-list engine for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The selection engine is the heart of kao.  Where Emacs has a single
;; point+mark region, Kakoune has a *list* of selections, each an (anchor
;; . cursor) pair with a direction, one of which is the "main" selection.  This
;; file implements that model as pure, buffer-free integer-position algebra, in
;; faithful correspondence with Kakoune's references/kakoune/src/selection.cc.
;;
;; Position convention: a kao coordinate is the buffer
;; position *before* the character the coordinate sits on.  A selection covers
;; the inclusive character span [min .. max]; in Emacs half-open region terms
;; that is [min, max+1).  A single-character selection has anchor = cursor.
;;
;; Matching Kakoune's BasicSelection: when anchor <= cursor the anchor is the
;; minimum and the cursor the maximum (a forward selection has its cursor on the
;; last character).
;;
;;   anchor=1 cursor=3  (forward "abc")   -> min 1, max 3, region [1,4), 3 chars
;;   anchor=3 cursor=1  (backward "abc")  -> min 1, max 3, region [1,4), 3 chars
;;   anchor=2 cursor=2  (single char)     -> min 2, max 2, region [2,3), 1 char

;;; Code:

(require 'cl-lib)

;;;; Selection struct and single-selection operations

(cl-defstruct (kao-sel (:constructor kao-sel-make)
                       (:constructor
                        kao-sel--new (anchor cursor &optional captures target))
                       (:copier kao-sel-copy))
  "A single Kakoune-style selection.
ANCHOR is the fixed end, CURSOR the moving end where the block cursor sits.
Both are integer buffer positions (the position before the character the
coordinate sits on).  CAPTURES is the list of regex submatch strings from
the last capture-producing command (`s' and the search family) — Kakoune's
`CaptureList' (selection.hh:75); nil = empty.  Element 0 is the whole
match, element i the i-th group; read by the digit registers `0'-`9'.
TARGET is the sticky goal column on the CURSOR (Kakoune `cursor.target',
coord.hh:75): nil = none (the real column, the C++ -1); an integer = a
desired display column; `eol' = the `max_column' sentinel (stick to the
newline); `before-eol' = `max_non_eol_column' (stick to the last non-eol
char).  Carried across vertical motions so `j'/`k' restore the column."
  anchor cursor captures target)

(defun kao-sel-min (sel)
  "Return the minimum (leftmost) position of SEL.
Matches Kakoune `BasicSelection::min': anchor when anchor <= cursor."
  (let ((a (kao-sel-anchor sel)) (c (kao-sel-cursor sel)))
    (if (<= a c) a c)))

(defun kao-sel-max (sel)
  "Return the maximum (rightmost) position of SEL.
Matches Kakoune `BasicSelection::max': cursor when anchor <= cursor."
  (let ((a (kao-sel-anchor sel)) (c (kao-sel-cursor sel)))
    (if (<= a c) c a)))

(defun kao-sel-forward-p (sel)
  "Return non-nil when SEL points forward (cursor at or after anchor)."
  (<= (kao-sel-anchor sel) (kao-sel-cursor sel)))

(defun kao-sel-length (sel)
  "Return the number of characters covered by SEL (always >= 1)."
  (1+ (- (kao-sel-max sel) (kao-sel-min sel))))

(defun kao-sel-beg (sel)
  "Return the Emacs region start of SEL (inclusive, = `kao-sel-min')."
  (kao-sel-min sel))

(defun kao-sel-end (sel)
  "Return the Emacs region end of SEL (exclusive, = max + 1)."
  (1+ (kao-sel-max sel)))

(defun kao-sel-collapse (sel)
  "Return a copy of SEL reduced to its cursor (Kakoune `;').
The anchor is moved onto the cursor, yielding a single-character selection.
Captures are carried (the C++ mutates the same `Selection' in place); the
cursor coordinate is unchanged, so its `target' (sticky goal column) is
preserved too."
  (kao-sel-make :anchor (kao-sel-cursor sel) :cursor (kao-sel-cursor sel)
                :captures (kao-sel-captures sel)
                :target (kao-sel-target sel)))

(defun kao-sel-flip (sel)
  "Return a copy of SEL with anchor and cursor swapped (Kakoune `<a-;>').
Captures are carried (the C++ mutates the same `Selection' in place)."
  (kao-sel-make :anchor (kao-sel-cursor sel) :cursor (kao-sel-anchor sel)
                :captures (kao-sel-captures sel)))

(defun kao-sel-ensure-forward (sel)
  "Return a forward copy of SEL (Kakoune `<a-:>': cursor after anchor).
If SEL is already forward it is copied unchanged; otherwise it is flipped.
Kakoune `ensure_forward' (normal.cc:2365) assigns a plain `BufferCoord' into
the cursor unconditionally, resetting its `target' to -1 even when already
forward — so the forward branch drops `target' (nil) too, not just the flip."
  (if (kao-sel-forward-p sel)
      (kao-sel-make :anchor (kao-sel-anchor sel) :cursor (kao-sel-cursor sel)
                    :captures (kao-sel-captures sel))
    (kao-sel-flip sel)))

(defun kao-sel-extend-to (sel new-sel)
  "Return SEL extended toward NEW-SEL's cursor (Kakoune `merge_selections').
Faithful to normal.cc:53: the cursor always becomes NEW-SEL's cursor; when SEL
and NEW-SEL share a direction the anchor is pulled along — a forward pair takes
the min of the two anchors, a backward pair the max — otherwise the anchor is
kept.  Note the new-direction tests are strict (`>'/`<'), so a zero-width
NEW-SEL adjusts neither anchor: the result is then simply (anchor unchanged,
cursor = NEW-SEL's cursor).

This is the per-selection core of every Extend-mode motion/selector:
`select<Extend>' passes a region NEW-SEL, while `move_cursor<Extend>' is the
zero-width case (the keep-anchor rule falls out of the strict tests above).

Distinct from the private `kao-sel--extend', which grows a selection to *cover*
another (the `merge_overlapping' fold); this one only ever moves the cursor to
NEW-SEL's cursor and never extends past it."
  (let* ((a (kao-sel-anchor sel)) (c (kao-sel-cursor sel))
         (na (kao-sel-anchor new-sel)) (nc (kao-sel-cursor new-sel))
         (anchor a))
    (when (and (>= c a) (> nc na)) (setq anchor (min a na)))   ; both forward
    (when (and (<= c a) (< nc na)) (setq anchor (max a na)))   ; both backward
    ;; merge_selections mutates SEL in place, so SEL's captures survive.  The
    ;; cursor becomes NEW-SEL's cursor, so its `target' (sticky goal column)
    ;; comes from NEW-SEL — captures ride the match, target rides the cursor.
    (kao-sel-make :anchor anchor :cursor nc
                  :captures (kao-sel-captures sel)
                  :target (kao-sel-target new-sel))))

;;;; Selection-list container and algebra

(cl-defstruct (kao-sels (:constructor kao-sels-make)
                        (:copier kao-sels-copy))
  "A list of selections with a designated main, mirroring Kakoune SelectionList.
LIST is a list of `kao-sel'; the invariant (after normalisation) is that it is
sorted by `kao-sel-min'.  MAIN is the index into LIST of the main selection.
SORTED is a metadata flag: non-nil only when LIST is KNOWN to be ascending by
`kao-sel-min'.  It is set solely by the normalising constructors that prove
that order — `kao-sels-sort', `kao-sels--merge', and the ascending fast path of
`kao-sels-sort-and-merge-overlapping' — so every raw `kao-sels-make' site
defaults it to nil = \"unknown\", the conservative value (the render walk then
tests every selection).  SORTED is pure position-free metadata (never a buffer
read) and stays valid because selection lists are never
position-mutated in place (audited at `kao-sels-sort-and-merge-overlapping');
it lets `kao--render' early-break a sorted secondary walk."
  list main sorted)

(defun kao--snapshot-sels (sels)
  "Return a deep copy of SELS with fresh integer-position selections.
Decouples the stored snapshot from later in-place marker promotion on the live
`kao--sels': each selection is rebuilt from its current anchor/cursor.
Captures are carried (Kakoune's jump-list / selection-history snapshots copy
whole `Selection' objects, captures included); the capture list itself is
copied so a later in-place capture write cannot leak into the snapshot.  The
`sorted' flag is carried too: `mapcar' is an order-preserving rebuild with
each min/max unchanged, so a sorted source snapshots sorted (a restored
snapshot keeps the clip)."
  (kao-sels-make
   :list (mapcar (lambda (s)
                   ;; positional constructor: the recorder snapshots on every
                   ;; selection change, so this site stays keyword-free
                   ;; (hot-path principle)
                   (kao-sel--new (kao-sel-anchor s) (kao-sel-cursor s)
                                 (copy-sequence (kao-sel-captures s))
                                 (kao-sel-target s)))
                 (kao-sels-list sels))
   :main (kao-sels-main sels)
   :sorted (kao-sels-sorted sels)))

(defun kao-sel-overlaps-p (a b)
  "Return non-nil when selections A and B share at least one character.
Inclusive-interval overlap, direction-agnostic (mirrors Kakoune `overlaps')."
  (and (<= (kao-sel-min a) (kao-sel-max b))
       (<= (kao-sel-min b) (kao-sel-max a))))

(defun kao-sel-touches-p (a b)
  "Return non-nil when A and B overlap or are immediately adjacent.
A and B must be ordered so that A.min <= B.min (post-sort).  Mirrors the
touching predicate used by Kakoune `merge_consecutive'."
  (>= (1+ (kao-sel-max a)) (kao-sel-min b)))

(defun kao-sel-compare (a b)
  "Return non-nil when selection A sorts strictly before B.
Ordered by `kao-sel-min', ties broken by `kao-sel-max' (Kakoune
`compare_selections')."
  (let ((amin (kao-sel-min a)) (bmin (kao-sel-min b)))
    (if (= amin bmin)
        (< (kao-sel-max a) (kao-sel-max b))
      (< amin bmin))))

(defun kao--sels-copy-sorted (sels)
  "Return a shallow copy of SELS stamped `sorted' t.
The list cells are shared (`kao-sels-copy'), never position-mutated in place
\; only the copy's `sorted' metadata flag is set.  Shared by
the trivially-ascending paths of `kao-sels-sort' and `kao-sels--merge' and the
ascending fast path of `kao-sels-sort-and-merge-overlapping', each of which has
just proved the list ascending."
  (let ((copy (kao-sels-copy sels)))
    (setf (kao-sels-sorted copy) t)
    copy))

(defun kao-sels-sort (sels)
  "Return a copy of SELS with LIST stably sorted by position.
The main index is tracked through the reordering by selection identity, matching
the intent of Kakoune `sort_selections' (and strictly correct in the rare
same-min case where Kakoune's positional count is latently off)."
  (let ((list (kao-sels-list sels)))
    (if (null (cdr list))
        ;; 0- or 1-element list is trivially ascending: copy, then mark sorted.
        (kao--sels-copy-sorted sels)
      (let* ((main-obj (nth (kao-sels-main sels) list))
             (sorted (cl-stable-sort (copy-sequence list) #'kao-sel-compare)))
        (kao-sels-make :list sorted
                       :main (cl-position main-obj sorted :test #'eq)
                       :sorted t)))))

(defun kao-sel--extend (a b)
  "Return selection A extended to also cover B, preserving A's direction.
Callers pass sorted A, B so that A.min <= B.min; only the max side can grow.
A's captures are carried: `merge_overlapping' (selection.cc:99-125) widens
the survivor begin[i] in place, so the absorbed selection's captures drop."
  (let ((newmin (min (kao-sel-min a) (kao-sel-min b)))
        (newmax (max (kao-sel-max a) (kao-sel-max b))))
    (if (kao-sel-forward-p a)
        (kao-sel-make :anchor newmin :cursor newmax
                      :captures (kao-sel-captures a))
      (kao-sel-make :anchor newmax :cursor newmin
                    :captures (kao-sel-captures a)))))

(defun kao-sels--merge (sels mergep)
  "Fold neighbours of sorted SELS for which MERGEP returns non-nil.
MERGEP is called as (MERGEP survivor candidate).  The surviving (earlier)
selection absorbs the candidate via `kao-sel--extend', and the main index
follows the survivor, mirroring Kakoune `merge_overlapping'."
  (let ((list (kao-sels-list sels)))
    (if (null (cdr list))
        ;; sorted input in, sorted out: a 0/1-element fold is trivially sorted.
        (kao--sels-copy-sorted sels)
      (let* ((main (kao-sels-main sels))
             (vec (vconcat list))
             (n (length vec))
             (i 0))
        (cl-loop for j from 1 below n do
                 (let ((a (aref vec i)) (b (aref vec j)))
                   (if (funcall mergep a b)
                       (progn
                         (aset vec i (kao-sel--extend a b))
                         (when (< i main) (cl-decf main)))
                     (cl-incf i)
                     (unless (= i j) (aset vec i b)))))
        ;; sorted input folded in list order stays ascending by min.
        (kao-sels-make :list (cl-loop for k from 0 to i collect (aref vec k))
                       :main main
                       :sorted t)))))

(defun kao-sels-merge-overlapping (sels)
  "Merge overlapping selections in SELS, which must be position-sorted.
Returns a new `kao-sels'.  Mirrors Kakoune `merge_overlapping'."
  (kao-sels--merge sels #'kao-sel-overlaps-p))

(defun kao-sels-merge-consecutive (sels)
  "Merge overlapping or adjacent selections in position-sorted SELS.
Returns a new `kao-sels'.  Mirrors Kakoune `merge_consecutive' / `<a-_>'."
  (kao-sels--merge sels #'kao-sel-touches-p))

(defun kao-sels--ascending-p (list)
  "Return non-nil when LIST is strictly ascending and non-overlapping.
The fused test is one compare per adjacent pair: B.min > A.max.  Non-overlap
of an adjacent pair implies strict min-order (A.min <= A.max < B.min), and on
a min-sorted list any overlap shows up between neighbours (if sel_i overlaps
sel_k then min_i+1 <= min_k <= max_i overlaps sel_i+1) — so this single
condition holds exactly when sort AND `merge_overlapping' are both the
identity.  An equal-min pair always overlaps, so ties take the full path."
  (let ((ok t)
        (prev-max (and list (kao-sel-max (car list)))))
    (while (and ok (cdr list))
      (setq list (cdr list))
      (let ((sel (car list)))
        (if (> (kao-sel-min sel) prev-max)
            (setq prev-max (kao-sel-max sel))
          (setq ok nil))))
    ok))

(defun kao-sels-sort-and-merge-overlapping (sels)
  "Sort SELS by position, then merge overlapping selections.
Returns a new `kao-sels' (Kakoune `sort_and_merge_overlapping').

Fast path: motions keep the list sorted and non-overlapping,
so when `kao-sels--ascending-p' holds — one cheap early-exit scan — both
steps are the identity and a shallow struct copy suffices (list cells
shared, like the single-selection paths of `kao-sels-sort' and
`kao-sels--merge'; selection lists are never mutated in place).  The scan
just PROVED ascending order, so the copy is stamped `sorted' t; the slow
path inherits `sorted' t from `kao-sels--merge'."
  (if (kao-sels--ascending-p (kao-sels-list sels))
      (kao--sels-copy-sorted sels)
    (kao-sels-merge-overlapping (kao-sels-sort sels))))

(provide 'kao-selection)
;;; kao-selection.el ends here
