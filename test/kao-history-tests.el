;;; kao-history-tests.el --- Tests for kao-history -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the buffer history TREE.  Task 1 covers the pure
;; topology: the node table, `commit', the undo/redo target walks, and the
;; `move_to' lowest-common-ancestor traversal with an injected apply-step
;; callback (no buffer edits — those are exercised by the capture/replay tests).
;; Asserted against Kakoune's `Buffer' history (references/kakoune/src/buffer.cc).

;;; Code:

(require 'ert)
(require 'kao-history)
(require 'kao-edit)                      ; kao-mode / kao-undo for the foreign-edit guard

(defun kao-history-tests--record (id)
  "Move current node to ID, returning the list of (DIR . NODE-ID) edges."
  (let ((steps '()))
    (kao-history-move-to id (lambda (dir node) (push (cons dir node) steps)))
    (nreverse steps)))

(defun kao-history-tests--goto (id)
  "Move current node to ID with a no-op apply-step (reposition only)."
  (kao-history-move-to id (lambda (_dir _node) nil)))

(defmacro kao-history-tests--with-capture (&rest body)
  "Run BODY in a temp buffer with the tree seeded and capture hooks installed."
  (declare (indent 0))
  `(with-temp-buffer
     (kao-history-init)
     (add-hook 'before-change-functions #'kao--hist-before-change nil t)
     (add-hook 'after-change-functions #'kao--hist-after-change nil t)
     ,@body))

(defun kao-history-tests--group (id) (kao-hist-node-group (kao--hist-node id)))

;;;; Seed + commit

(ert-deftest kao-history-init-seeds-root ()
  "`kao-history-init' creates a single root node (id 0)."
  (with-temp-buffer
    (kao-history-init)
    (should (= (kao-history-current-id) 0))
    (should (= (kao-history-max-id) 0))
    (should (null (kao-hist-node-parent (kao--hist-node 0))))))

(ert-deftest kao-history-commit-advances ()
  "`kao-history-commit' appends a child and advances current; root gains a redo-child."
  (with-temp-buffer
    (kao-history-init)
    (let ((id (kao-history-commit 'ga)))
      (should (= id 1))
      (should (= (kao-history-current-id) 1))
      (should (= (kao-history-max-id) 1))
      (should (= (kao-hist-node-parent (kao--hist-node 1)) 0))
      (should (eq (kao-hist-node-group (kao--hist-node 1)) 'ga))
      (should (= (kao-hist-node-redo-child (kao--hist-node 0)) 1)))))

(ert-deftest kao-history-commit-linear-chain ()
  "Three commits form a linear parent chain 0<-1<-2<-3."
  (with-temp-buffer
    (kao-history-init)
    (kao-history-commit 'a) (kao-history-commit 'b) (kao-history-commit 'c)
    (should (= (kao-history-current-id) 3))
    (should (= (kao-hist-node-parent (kao--hist-node 3)) 2))
    (should (= (kao-hist-node-parent (kao--hist-node 2)) 1))
    (should (= (kao-hist-node-parent (kao--hist-node 1)) 0))))

;;;; undo / redo targets

(ert-deftest kao-history-undo-target-walks-parents ()
  "Undo-target walks `count' parents and stops at the root on overrun."
  (with-temp-buffer
    (kao-history-init)
    (kao-history-commit 'a) (kao-history-commit 'b) (kao-history-commit 'c)
    (should (= (kao-history-undo-target 1) 2))
    (should (= (kao-history-undo-target 2) 1))
    (should (= (kao-history-undo-target 9) 0))   ; overrun clamps at root
    (kao-history-tests--goto 0)
    (should (null (kao-history-undo-target 1))))) ; nil at the root

(ert-deftest kao-history-redo-target-walks-redo-children ()
  "Redo-target follows `redo-child' `count' steps and stops when none remain."
  (with-temp-buffer
    (kao-history-init)
    (kao-history-commit 'a) (kao-history-commit 'b) (kao-history-commit 'c)
    (kao-history-tests--goto 0)
    (should (= (kao-history-redo-target 1) 1))
    (should (= (kao-history-redo-target 2) 2))
    (should (= (kao-history-redo-target 9) 3))   ; overrun clamps at the tip
    (kao-history-tests--goto 3)
    (should (null (kao-history-redo-target 1))))) ; nil at the tip

;;;; move_to traversal (apply-step sequence)

(ert-deftest kao-history-move-to-reverts-up ()
  "Moving to an ancestor reverts each node from current up to it (no applies)."
  (with-temp-buffer
    (kao-history-init)
    (kao-history-commit 'a) (kao-history-commit 'b)   ; current = 2
    (should (equal (kao-history-tests--record 0)
                   '((revert . 2) (revert . 1))))
    (should (= (kao-history-current-id) 0))))

(ert-deftest kao-history-move-to-applies-down ()
  "Moving to a descendant applies each node from the LCA down (no reverts)."
  (with-temp-buffer
    (kao-history-init)
    (kao-history-commit 'a) (kao-history-commit 'b)
    (kao-history-tests--goto 0)
    (should (equal (kao-history-tests--record 2)
                   '((apply . 1) (apply . 2))))
    (should (= (kao-history-current-id) 2))))

(ert-deftest kao-history-move-to-cross-branch ()
  "Change-id `move_to' reaches an abandoned SIBLING branch (point):
revert the current branch to the LCA, then apply down the other branch."
  (with-temp-buffer
    (kao-history-init)
    (kao-history-commit 'a)            ; node 1 (branch A), current = 1
    (kao-history-tests--goto 0)        ; back to root
    (kao-history-commit 'b)            ; node 2 (branch B, sibling of 1), current = 2
    ;; Node 1 is on a branch u/U (parent/redo-child from node 2) cannot reach;
    ;; the change-id walk gets there: revert 2, then apply 1.
    (should (equal (kao-history-tests--record 1)
                   '((revert . 2) (apply . 1))))
    (should (= (kao-history-current-id) 1))
    ;; Descent refreshed the root's redo-child to point at the visited branch.
    (should (= (kao-hist-node-redo-child (kao--hist-node 0)) 1))))

(ert-deftest kao-history-move-to-out-of-range ()
  "`move_to' an out-of-range id is a no-op returning nil."
  (with-temp-buffer
    (kao-history-init)
    (kao-history-commit 'a)            ; max id = 1, current = 1
    (should (null (kao-history-move-to 5 (lambda (_d _i) nil))))
    (should (null (kao-history-move-to -1 (lambda (_d _i) nil))))
    (should (= (kao-history-current-id) 1))))

;;;; Gc (kao-history-max-nodes): oldest root-side nodes dropped

(ert-deftest kao-history-gc-cap-holds ()
  "Commits past the cap drop oldest root-side nodes; ids stay absolute."
  (with-temp-buffer
    (let ((kao-history-max-nodes 4))
      (kao-history-init)
      (dotimes (i 10) (kao-history-commit i))
      (should (<= (hash-table-count kao--hist) 4))
      (should (= (kao-history-current-id) 10))
      (should (= (kao-history-max-id) 10))          ; ids never renumbered
      ;; undo overrun bottoms out at the oldest RETAINED node (the new root)
      (should (= (kao-history-undo-target 99) kao--hist-root))
      (let ((root (kao--hist-node kao--hist-root)))
        (should (null (kao-hist-node-parent root)))
        (should (null (kao-hist-node-group root)))))))   ; strings freed

(ert-deftest kao-history-gc-drops-abandoned-branch ()
  "Gc deletes the sibling subtree abandoned at the old root."
  (with-temp-buffer
    (let ((kao-history-max-nodes 3))
      (kao-history-init)
      (kao-history-commit 'a)            ; node 1 (branch A)
      (kao-history-tests--goto 0)
      (kao-history-commit 'b)            ; node 2 (branch B), current
      (kao-history-commit 'c)            ; node 3
      (kao-history-commit 'd)            ; node 4 -> gc: root 0 + branch 1 drop
      (should (null (kao--hist-node 1))) ; abandoned sibling gone
      (should (null (kao--hist-node 0)))
      (should (= kao--hist-root 2))
      ;; a move_to onto a dropped id is nil, not a crash
      (should (null (kao-history-tests--goto 1)))
      (should (null (kao-history-tests--goto 0)))
      (should (= (kao-history-current-id) 4)))))

(ert-deftest kao-history-gc-current-never-dropped ()
  "Gc stops at the current node even when the cap is below the live path."
  (with-temp-buffer
    (let ((kao-history-max-nodes 1))
      (kao-history-init)
      (kao-history-commit 'a)
      (kao-history-commit 'b)
      (should (= (kao-history-current-id) 2))
      (should (kao--hist-node 2))                   ; current retained
      (should (= kao--hist-root 2))                 ; root advanced TO current
      (should (null (kao-history-undo-target 1)))   ; history fully consumed
      (should (= (hash-table-count kao--hist) 1)))))

(ert-deftest kao-history-gc-root-group-cleared-round-trip ()
  "After gc the retained root's group is nil, yet u/U round-trips exactly
\(the root's own edge is never replayed — its parent edge no longer exists)."
  (kao-history-tests--with-capture
    (let ((kao-history-max-nodes 2))
      (insert "A") (kao-history-commit-pending)     ; node 1: "A"
      (insert "B") (kao-history-commit-pending)     ; node 2 -> gc drops root 0
      (should (= kao--hist-root 1))
      (should (null (kao-hist-node-group (kao--hist-node 1))))
      (let ((step (lambda (dir id)
                    (kao--hist-apply-group
                     (kao-hist-node-group (kao--hist-node id)) dir nil))))
        (kao-history-move-to 1 step)
        (should (equal (buffer-string) "A"))        ; u to the retained root
        (kao-history-move-to 2 step)
        (should (equal (buffer-string) "AB"))))))   ; U back

;;;; Modification capture + symmetric apply/revert

(ert-deftest kao-history-capture-insertion ()
  "An insertion is captured as one modification and committed as a node."
  (kao-history-tests--with-capture
    (insert "A")
    (should (= (kao-history-commit-pending) 1))
    (let* ((g (kao-history-tests--group 1)) (m (aref g 0)))
      (should (= (length g) 1))
      (should (= (kao-hist-mod-pos m) 1))
      (should (equal (kao-hist-mod-deleted m) ""))
      (should (equal (kao-hist-mod-inserted m) "A")))))

(ert-deftest kao-history-revert-and-apply-round-trip ()
  "Reverting a node's group restores the parent buffer; applying re-does it."
  (kao-history-tests--with-capture
    (insert "A") (kao-history-commit-pending)      ; node 1: "A"
    (insert "B") (kao-history-commit-pending)      ; node 2: "AB"
    (should (equal (buffer-string) "AB"))
    (let ((g (kao-history-tests--group 2)))
      (kao--hist-apply-group g 'revert nil)
      (should (equal (buffer-string) "A"))
      (kao--hist-apply-group g 'apply nil)
      (should (equal (buffer-string) "AB")))))

(ert-deftest kao-history-apply-revert-position-safe ()
  "Interleaved modifications (insert then a shifting delete) round-trip both
ways — proving edit-time positions are safe (the multi-change crux)."
  (kao-history-tests--with-capture
    (insert "x12") (kao-history-commit-pending)    ; node 1 (parent): "x12"
    (goto-char 3) (insert "AB")                    ; -> "x1AB2"
    (delete-region 1 2)                            ; -> "1AB2" (shifts AB left)
    (should (= (kao-history-commit-pending) 2))    ; node 2: two modifications
    (should (equal (buffer-string) "1AB2"))
    (let ((g (kao-history-tests--group 2)))
      (should (= (length g) 2))
      (kao--hist-apply-group g 'revert nil)
      (should (equal (buffer-string) "x12"))       ; back to the parent
      (kao--hist-apply-group g 'apply nil)
      (should (equal (buffer-string) "1AB2")))))   ; forward to the node

(ert-deftest kao-history-revert-deletion ()
  "A deletion's group restores the deleted text on revert and removes it on apply."
  (kao-history-tests--with-capture
    (insert "abc") (kao-history-commit-pending)    ; node 1: "abc"
    (delete-region 2 3) (kao-history-commit-pending) ; node 2: "ac" (deleted "b")
    (let* ((g (kao-history-tests--group 2)) (m (aref g 0)))
      (should (equal (kao-hist-mod-deleted m) "b"))
      (kao--hist-apply-group g 'revert nil)
      (should (equal (buffer-string) "abc"))
      (kao--hist-apply-group g 'apply nil)
      (should (equal (buffer-string) "ac")))))

(ert-deftest kao-history-move-to-atomic-on-read-only ()
  "A mid-group signal (a `text-read-only' property) during a `move-to' walk
rolls the whole replay back: the buffer AND `kao--hist-id' are left exactly as
they were, never half-applied (part 2).  Two modifications in one
node so the FIRST revert succeeds and the SECOND signals — the partial-apply
that desynced the tree in the repro."
  (kao-history-tests--with-capture
    (insert "abcdef") (kao-history-commit-pending)  ; node 1: "abcdef"
    ;; One command, two mods -> node 2: insert "XYZ" at 4, then "Q" at 1.
    (goto-char 4) (insert "XYZ")                    ; "abcXYZdef"
    (goto-char 1) (insert "Q")                      ; "QabcXYZdef"
    (kao-history-commit-pending)
    (should (= (kao-history-current-id) 2))
    (should (equal (buffer-string) "QabcXYZdef"))
    ;; Make the "XYZ" span (the 2nd-reverted mod) read-only, without capturing.
    (let ((inhibit-modification-hooks t))
      (put-text-property 5 8 'read-only t))         ; Q1 a2 b3 c4 X5 Y6 Z7 ...
    (let ((step (lambda (dir id)
                  (kao--hist-apply-group
                   (kao-hist-node-group (kao--hist-node id)) dir nil))))
      ;; Revert node 2: "Q" (pos 1) deletes fine, then "XYZ" (now [4,7)) barfs.
      (should-error (kao-history-move-to 1 step) :type 'text-read-only)
      ;; Transaction rolled back: buffer whole, id still at node 2.
      (should (equal (buffer-string) "QabcXYZdef"))
      (should (= (kao-history-current-id) 2))
      ;; The move-to cleanup re-stamped the integrity tick PAST the rolled-back
      ;; replay edits, so a following navigation is not a false
      ;; refusal.
      (should (= kao--hist-tick (buffer-chars-modified-tick))))))

(ert-deftest kao-history-apply-group-collects-ranges ()
  "Apply/revert collect one modified range per replayed mod (BEG . END), the
list Kakoune's `compute_modified_ranges' installs."
  (kao-history-tests--with-capture
    (insert "abc") (kao-history-commit-pending)
    (insert "de") (kao-history-commit-pending)     ; node 2 inserts "de" at 4..6
    (let ((g (kao-history-tests--group 2)))
      ;; revert removes [4,6) -> collapses to (4 . 4)
      (should (equal (kao--hist-apply-group g 'revert nil) '((4 . 4))))
      ;; apply re-inserts -> span (4 . 6)
      (should (equal (kao--hist-apply-group g 'apply nil) '((4 . 6)))))))

;;;; Foreign-edit integrity guard

(ert-deftest kao-history-refuses-navigation-after-foreign-edit ()
  "An edit through an indirect buffer (invisible to this tree) makes the next
`u' REFUSE with a clean `user-error', never replay at now-wrong positions.
Kakoune has no indirect buffers — every change flows through the one history —
so kao's honest analogue is a loud refusal, not silent corruption.  The tick
stamp (`buffer-chars-modified-tick' vs `kao--hist-tick') also covers
`buffer-swap-text' / `inhibit-modification-hooks' writers."
  (let ((base (get-buffer-create "kao-hist-test-base")) (ind nil))
    (unwind-protect
        (progn
          (with-current-buffer base
            (insert "aaaa bbbb cccc dddd\n")
            (kao-mode 1))
          (setq ind (make-indirect-buffer base "kao-hist-test-ind" t))
          (with-current-buffer ind
            (kao-mode 1)
            (goto-char (point-max))
            (insert "YY")                     ; recorded as ind's node 1
            (kao--hist-maybe-commit)
            (should (equal (buffer-string) "aaaa bbbb cccc dddd\nYY")))
          ;; A foreign edit in the BASE buffer — ind's tree never sees it.
          (with-current-buffer base
            (goto-char 6) (insert "!!!!"))     ; "aaaa !!!!bbbb cccc dddd\nYY"
          ;; `u' in the indirect buffer refuses; both buffers' text intact.
          (with-current-buffer ind
            ;; Match a quote-free slice — `user-error' curls the apostrophe in
            ;; "kao's" via `format-message' (`text-quoting-style').
            (let ((e (should-error (kao-undo) :type 'user-error)))
              (should (string-match-p "history navigation refused" (cadr e))))
            (should (equal (buffer-string) "aaaa !!!!bbbb cccc dddd\nYY")))
          (with-current-buffer base
            (should (equal (buffer-string) "aaaa !!!!bbbb cccc dddd\nYY"))))
      (when (buffer-live-p ind)
        (with-current-buffer ind (kao-mode -1)) (kill-buffer ind))
      (when (buffer-live-p base)
        (with-current-buffer base (kao-mode -1)) (kill-buffer base)))))

;;;; Position translation — the flat ForwardChangesTracker

(defun kao-history-tests--mod (pos del ins)
  "Build a `kao-hist-mod' replacing DEL with INS at POS."
  (kao-hist-mod-make :pos pos :deleted del :inserted ins))

(ert-deftest kao-history-translate-1-erase-matrix ()
  "Erase [5,8): before keeps, at-start keeps, inside/at-end collapse, after shifts.
The flat `get_new_coord_tolerant' rules (changes.cc:28-60): `relevant' uses
strict `<' for Erase, so a position AT the erase start is untouched."
  (should (= (kao-history--translate-1 4 5 3 0) 4))
  (should (= (kao-history--translate-1 5 5 3 0) 5))
  (should (= (kao-history--translate-1 6 5 3 0) 5))   ; inside -> begin
  (should (= (kao-history--translate-1 8 5 3 0) 5))   ; at end -> begin
  (should (= (kao-history--translate-1 9 5 3 0) 6))
  (should (= (kao-history--translate-1 12 5 3 0) 9)))

(ert-deftest kao-history-translate-1-insert-matrix ()
  "Insert 2 at 5: before keeps, at-or-after shifts (`relevant' uses `<=' for Insert)."
  (should (= (kao-history--translate-1 4 5 0 2) 4))
  (should (= (kao-history--translate-1 5 5 0 2) 7))   ; AT the insert -> shifts
  (should (= (kao-history--translate-1 6 5 0 2) 8)))

(ert-deftest kao-history-translate-1-replace ()
  "Replace 2 chars by 3 at 5 composes erase-then-insert (two Kakoune changes)."
  (should (= (kao-history--translate-1 4 5 2 3) 4))
  (should (= (kao-history--translate-1 5 5 2 3) 8))   ; at start: kept, then +3
  (should (= (kao-history--translate-1 6 5 2 3) 8))   ; inside: -> 5, then +3
  (should (= (kao-history--translate-1 7 5 2 3) 8))   ; at end: -> 5, then +3
  (should (= (kao-history--translate-1 8 5 2 3) 9)))  ; after: -2, then +3

(ert-deftest kao-history-group-revert-inverts-apply ()
  "Positions outside the modified regions round-trip exactly through a group."
  (let ((g (vector (kao-history-tests--mod 3 "ab" "XYZ")
                   (kao-history-tests--mod 10 "" "Q"))))
    (dolist (p '(1 2 20 15))
      (should (= (kao-history--group-translate
                  (kao-history--group-translate p g 'apply) g 'revert)
                 p)))
    ;; Spot-check the forward fold itself: 20 -> erase[3,5) 18 -> +3 = 21
    ;; -> insert@10 +1 = 22.
    (should (= (kao-history--group-translate 20 g 'apply) 22))))

(ert-deftest kao-history-translate-position-identity-and-invalid ()
  "Same-id translation is the identity; nil or gc'd FROM-ID returns nil."
  (with-temp-buffer
    (kao-history-init)
    (kao-history-commit (vector (kao-history-tests--mod 2 "" "abc")))
    (should (= (kao-history-translate-position 7 (kao-history-current-id)) 7))
    (should (null (kao-history-translate-position 7 nil)))
    (should (null (kao-history-translate-position 7 99)))))

(ert-deftest kao-history-translate-position-gcd-id-nil ()
  "An id dropped by the gc translates to nil (the clamp fallback)."
  (with-temp-buffer
    (let ((kao-history-max-nodes 2))
      (kao-history-init)
      (kao-history-commit (vector (kao-history-tests--mod 1 "" "a")))
      (kao-history-commit (vector (kao-history-tests--mod 2 "" "b")))
      (kao-history-commit (vector (kao-history-tests--mod 3 "" "c")))
      ;; cap 2: node 0 (then 1) gc'd
      (should (null (kao--hist-node 0)))
      (should (null (kao-history-translate-position 5 0))))))

(ert-deftest kao-history-translate-position-descendant ()
  "Ancestor -> descendant folds apply edges only."
  (with-temp-buffer
    (kao-history-init)
    (kao-history-commit (vector (kao-history-tests--mod 2 "" "XYZ")))  ; node 1
    ;; From root frame: pos 4 -> insert 3 at 2 -> 7.
    (should (= (kao-history-translate-position 4 0) 7))
    ;; And up toward the root from the current node: revert edge only.
    (kao-history-tests--goto 0)
    (should (= (kao-history-translate-position 7 1) 4))))

(ert-deftest kao-history-translate-position-cross-branch ()
  "Sibling-branch translation reverts up to the LCA then applies down."
  (with-temp-buffer
    (kao-history-init)
    (kao-history-commit (vector (kao-history-tests--mod 5 "" "AA")))   ; node 1
    (kao-history-tests--goto 0)
    (kao-history-commit (vector (kao-history-tests--mod 2 "" "BBB"))) ; node 2
    ;; Current = node 2.  From node 1's frame: revert ins2@5 (erase [5,7)),
    ;; then apply ins3@2.
    (should (= (kao-history-translate-position 10 1) 11))  ; 10->8->11
    (should (= (kao-history-translate-position 5 1) 8))    ; 5->5->8
    (should (= (kao-history-translate-position 1 1) 1))))  ; untouched

(ert-deftest kao-snap-tag-accessors-roundtrip ()
  "`kao--snap-tag-*' decode exactly the parts `kao--snap-tag-make' encodes,
eq-clean, for the `(BUFFER (GEN . ID) . SNAP)' tag shape."
  (with-temp-buffer
    (let* ((kao--hist-generation 7)
           (kao--hist-id 42)
           (snap (vector 'snapshot))
           (buf (current-buffer))
           (tag (kao--snap-tag-make buf snap)))
      (should (eq (kao--snap-tag-buffer tag) buf))
      (should (eq (kao--snap-tag-snap tag) snap))
      (should (equal (kao--snap-tag-id tag) (cons 7 42)))
      ;; the encoded shape is literally (BUFFER (GEN . ID) . SNAP)
      (should (equal tag (cons buf (cons (cons 7 42) snap)))))))

;;; kao-history-tests.el ends here
