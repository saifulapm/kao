;;; kao-vundo-tests.el --- Tests for kao-vundo + its public history API -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT for the kao-vundo visual history-tree viewer and the public read-only
;; topology API it builds on (kao-vundo handoff §5/§9).  Three layers:
;;   - §5 public accessors (`kao-history-root-id', `-node-exists-p',
;;     `-node-parent/children/redo-child', `-node-summary') over a synthetic
;;     tree, incl. an absent and a gc-dropped id;
;;   - `kao-history-goto' — the shared validated arbitrary-id nav entry — driven
;;     in a real `kao-mode' buffer (content, current-id, selection span);
;;   - the viewer itself: pure layout, render reverse-map, and navigation
;;     integration (added with the module).

;;; Code:

(require 'ert)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-history)
(require 'kao-state)
(require 'kao-register)
(require 'kao-edit)
(require 'kao-keys)                     ; kao-mode default bindings
(require 'kao-vundo)

;;;; Helpers

(defun kao-vundo-tests--reposition (id)
  "Move the tree's current node to ID with a no-op apply-step (no buffer edit)."
  (kao-history-move-to id (lambda (_dir _node) nil)))

(defun kao-vundo-tests--build-branched ()
  "Seed a branched tree 0 -> {1,4}, 1 -> {2,3}, 4 -> {5}; leave current at 5.
   0
   ├─ 1
   │  ├─ 2
   │  └─ 3
   └─ 4
      └─ 5"
  (kao-history-init)
  (kao-history-commit 'a)               ; 1
  (kao-history-commit 'b)               ; 2
  (kao-vundo-tests--reposition 1)
  (kao-history-commit 'c)               ; 3  (1 -> {2,3})
  (kao-vundo-tests--reposition 0)
  (kao-history-commit 'd)               ; 4
  (kao-history-commit 'e))              ; 5  (4 -> {5}); current 5

(defmacro kao-vundo-tests--with (content cursor &rest body)
  "Run BODY in a `kao-mode' temp buffer of CONTENT, main selection at CURSOR.
The change hooks are live (via `kao-mode'), so `insert' captures modifications;
commit them into nodes with `kao--hist-maybe-commit', exactly as the edit keys
do per command."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,content)
     (buffer-enable-undo)
     (kao-mode 1)
     (unwind-protect
         (progn
           (setq kao--sels (kao-sels-make
                            :list (list (kao-sel-make :anchor ,cursor :cursor ,cursor))
                            :main 0))
           ,@body)
       (kao-mode -1))))

;;;; §5 public topology accessors

(ert-deftest kao-vundo-accessors-on-a-branched-tree ()
  "The public accessors report the topology of a hand-built branched tree.
Tree: 0 -> 1 -> {2, 3} (3 a sibling branch off node 1, the C-j cross-branch
reach)."
  (with-temp-buffer
    (kao-history-init)
    (kao-history-commit 'a)               ; node 1 (current)
    (kao-history-commit 'b)               ; node 2
    (kao-vundo-tests--reposition 1)       ; back to node 1
    (kao-history-commit 'c)               ; node 3, sibling of 2
    ;; root
    (should (= (kao-history-root-id) 0))
    ;; existence (present / never-created / nil)
    (should (kao-history-node-exists-p 0))
    (should (kao-history-node-exists-p 3))
    (should-not (kao-history-node-exists-p 99))
    (should-not (kao-history-node-exists-p nil))
    ;; parent
    (should (null (kao-history-node-parent 0)))
    (should (= (kao-history-node-parent 1) 0))
    (should (= (kao-history-node-parent 2) 1))
    (should (= (kao-history-node-parent 3) 1))
    (should (null (kao-history-node-parent 99)))
    ;; children (as a set — storage order is push-order)
    (should (equal (sort (copy-sequence (kao-history-node-children 0)) #'<) '(1)))
    (should (equal (sort (copy-sequence (kao-history-node-children 1)) #'<) '(2 3)))
    (should (null (kao-history-node-children 2)))   ; leaf
    (should (null (kao-history-node-children 99)))
    ;; redo-child (last-applied child)
    (should (= (kao-history-node-redo-child 0) 1))
    (should (= (kao-history-node-redo-child 1) 3))  ; node 3 committed last
    (should (null (kao-history-node-redo-child 2)))
    (should (null (kao-history-node-redo-child 99)))))

(ert-deftest kao-vundo-node-summary-counts-chars ()
  "`kao-history-node-summary' sums inserted/deleted chars; nil for root/absent."
  (with-temp-buffer
    (kao-history-init)
    (kao-history-commit
     (vector (kao-hist-mod-make :pos 1 :deleted "" :inserted "hello")
             (kao-hist-mod-make :pos 1 :deleted "xy" :inserted "")))  ; +5 / -2
    (should (equal (kao-history-node-summary 1) "+5/-2"))
    (should (null (kao-history-node-summary 0)))    ; root: no group
    (should (null (kao-history-node-summary 99))))) ; absent

(ert-deftest kao-vundo-node-changes-returns-mods ()
  "`kao-history-node-changes' returns (POS DELETED INSERTED) triples in order;
nil for root/absent."
  (with-temp-buffer
    (kao-history-init)
    (kao-history-commit
     (vector (kao-hist-mod-make :pos 5 :deleted "old" :inserted "new")
             (kao-hist-mod-make :pos 9 :deleted "" :inserted "x")))
    (should (equal (kao-history-node-changes 1)
                   '((5 "old" "new") (9 "" "x"))))
    (should (null (kao-history-node-changes 0)))      ; root: no group
    (should (null (kao-history-node-changes 99)))))   ; absent

(ert-deftest kao-vundo-accessors-handle-gc-dropped-id ()
  "A node dropped by the `kao-history-max-nodes' gc is absent (nil), root moves.
Cap 2 + three linear commits forces the root to advance past the oldest nodes."
  (with-temp-buffer
    (let ((kao-history-max-nodes 2))
      (kao-history-init)                  ; {0}
      (kao-history-commit 'a)             ; {0,1}
      (kao-history-commit 'b)             ; gc drops 0, root -> 1, {1,2}
      (kao-history-commit 'c)             ; gc drops 1, root -> 2, {2,3}
      (should (= (kao-history-root-id) 2))
      (should-not (kao-history-node-exists-p 0))   ; dropped
      (should-not (kao-history-node-exists-p 1))   ; dropped
      (should (kao-history-node-exists-p 2))
      (should (kao-history-node-exists-p 3))
      (should (null (kao-history-node-parent 0)))      ; absent -> nil, no error
      (should (null (kao-history-node-redo-child 0)))
      (should (null (kao-history-node-summary 0))))))

(ert-deftest kao-vundo-accessors-no-tree-do-not-error ()
  "In a buffer with no tree (`kao--hist' nil) the accessors return nil, no error."
  (with-temp-buffer
    (should (null (kao-history-root-id)))
    (should-not (kao-history-node-exists-p 0))
    (should (null (kao-history-node-parent 0)))
    (should (null (kao-history-node-children 0)))
    (should (null (kao-history-node-redo-child 0)))
    (should (null (kao-history-node-summary 0)))))

;;;; kao-history-goto — the shared arbitrary-id nav entry

(ert-deftest kao-vundo-goto-moves-content-and-selection ()
  "`kao-history-goto' replays to the target node: content, current-id, span.
Undo collapses the reverted-insertion selection to the gap; redo selects the
re-inserted region with the cursor on its last char."
  (kao-vundo-tests--with "" 1
    (insert "aaa") (kao--hist-maybe-commit)   ; node 1: "aaa"
    (insert "bbb") (kao--hist-maybe-commit)   ; node 2: "aaabbb"
    (should (= (kao-history-current-id) 2))
    ;; goto 1 (undo the "bbb" insertion)
    (should (= (kao-history-goto 1) 1))        ; returns the id
    (should (= (kao-history-current-id) 1))
    (should (string= (buffer-string) "aaa"))
    (let ((s (kao--main-sel)))
      ;; the gap (4) is at eob in "aaa", so the on-char model clamps the
      ;; collapsed cursor onto the last char (3) — same clamp the edit keys use.
      (should (= (kao-sel-anchor s) 3))
      (should (= (kao-sel-cursor s) 3)))
    ;; goto 2 (redo)
    (kao-history-goto 2)
    (should (string= (buffer-string) "aaabbb"))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) 4))        ; [4,7) -> cursor on last char
      (should (= (kao-sel-cursor s) 6)))))

(ert-deftest kao-vundo-goto-reaches-sibling-branch ()
  "`kao-history-goto' makes the C-j cross-branch reach (LCA walk), unlike u/U."
  (kao-vundo-tests--with "" 1
    (insert "aaa") (kao--hist-maybe-commit)    ; node 1: "aaa"
    (kao-history-goto 0)                        ; back to root
    (should (string= (buffer-string) ""))
    (insert "ZZ") (kao--hist-maybe-commit)      ; node 2: "ZZ", sibling of 1
    (should (= (kao-history-current-id) 2))
    (kao-history-goto 1)                         ; cross-branch to sibling 1
    (should (string= (buffer-string) "aaa"))
    (should (= (kao-history-current-id) 1))))

(ert-deftest kao-vundo-goto-out-of-range-errors ()
  "An out-of-range or absent id signals the faithful `no such change'."
  (kao-vundo-tests--with "" 1
    (insert "x") (kao--hist-maybe-commit)       ; max id 1
    (let ((e (should-error (kao-history-goto 9) :type 'user-error)))
      (should (equal (cadr e) "no such change: #9 (1)")))
    (let ((e (should-error (kao-history-goto -1) :type 'user-error)))
      (should (equal (cadr e) "no such change: #-1 (1)")))))

(ert-deftest kao-vundo-goto-read-only-ordering ()
  "Range check precedes the read-only barf (buffer.cc:345-350):
an in-range goto in a read-only buffer signals `buffer-read-only'; an
out-of-range one still reports `no such change'."
  (kao-vundo-tests--with "abc" 1
    (insert "X") (kao--hist-maybe-commit)
    (let ((buffer-read-only t))
      (should-error (kao-history-goto 0) :type 'buffer-read-only)   ; in range
      (should-error (kao-history-goto 99) :type 'user-error))))     ; out of range

(ert-deftest kao-vundo-move-in-history-reports-pre-commit-max ()
  "C-j/C-k report the PRE-commit max-id even when a pending edit commits during
the jump: `kao--move-in-history' reads `maxid' BEFORE `kao-history-goto' (which
runs `kao-history-commit-pending' and so can create a node).  Guards the
documented invariant the refactor onto `kao-history-goto' relies on."
  (kao-vundo-tests--with "" 1
    (insert "a") (kao--hist-maybe-commit)        ; node 1 "a"
    (insert "b") (kao--hist-maybe-commit)        ; node 2 "ab", max 2
    (kao-history-goto 0)                           ; back to root ""
    (insert "X")                                   ; pending, uncommitted
    (let (captured)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq captured (apply #'format fmt args)))))
        (kao-history-forward))                     ; <c-j>: target = 0 + 1 = 1
      ;; the pending "X" committed as node 3 (max -> 3) during the jump...
      (should (= (kao-history-max-id) 3))
      (should (= (kao-history-current-id) 1))
      (should (string= (buffer-string) "a"))
      ;; ...yet the report uses the PRE-commit max (2), not 3
      (should (equal captured "moved to change #1 (2)")))))

;;;; kao-history-change-hook (live-refresh substrate)

(ert-deftest kao-vundo-change-hook-fires-on-commit-not-navigation ()
  "`kao-history-change-hook' fires once per committed node and NOT during a
navigation (which appends no node)."
  (with-temp-buffer
    (kao-history-init)
    (let* ((n 0)
           (kao-history-change-hook (list (lambda () (setq n (1+ n))))))
      (kao-history-commit 'a)
      (should (= n 1))
      (kao-history-commit 'b)
      (should (= n 2))
      ;; navigation appends no node -> no fire
      (kao-history-move-to 0 (lambda (_d _i) nil))
      (kao-history-move-to 2 (lambda (_d _i) nil))
      (should (= n 2)))))

(ert-deftest kao-vundo-change-hook-no-listener-is-safe ()
  "A commit with no `kao-history-change-hook' listener does not error."
  (with-temp-buffer
    (kao-history-init)
    (let ((kao-history-change-hook nil))
      (should (= (kao-history-commit 'a) 1)))))

;;;; Layout (pure)

(ert-deftest kao-vundo-layout-orders-and-positions-nodes ()
  "Layout is DFS pre-order, children ascending; depth = topological depth;
every existing node appears exactly once."
  (with-temp-buffer
    (kao-vundo-tests--build-branched)
    (let ((rows (kao-vundo--layout (kao-history-root-id))))
      ;; DFS pre-order, children ascending by id
      (should (equal (mapcar #'kao-vundo-row-id rows) '(0 1 2 3 4 5)))
      (should (equal (mapcar #'kao-vundo-row-depth rows) '(0 1 2 2 1 2)))
      ;; each row's depth equals the independent parent-walk depth
      (dolist (r rows)
        (should (= (kao-vundo-row-depth r)
                   (let ((d 0) (id (kao-vundo-row-id r)))
                     (while (kao-history-node-parent id)
                       (setq id (kao-history-node-parent id) d (1+ d)))
                     d))))
      ;; every existing node, exactly once
      (should (= (length rows) 6))
      (should (equal (sort (mapcar #'kao-vundo-row-id rows) #'<) '(0 1 2 3 4 5))))))

(ert-deftest kao-vundo-layout-draws-connectors ()
  "`kao-vundo--row-prefix' draws the box connectors for the branched tree."
  (with-temp-buffer
    (kao-vundo-tests--build-branched)
    (let ((rows (kao-vundo--layout (kao-history-root-id))))
      (should (equal (mapcar #'kao-vundo--row-prefix rows)
                     '(""            ; 0 root
                       "├─ "         ; 1 (sibling 4 below)
                       "│  ├─ "      ; 2 (under 1, sibling 3 below)
                       "│  └─ "      ; 3 (last under 1)
                       "└─ "         ; 4 (last under 0)
                       "   └─ "))))))  ; 5 (under 4, which is last -> blank column)

(ert-deftest kao-vundo-layout-empty-root-only ()
  "A fresh tree lays out a single root row."
  (with-temp-buffer
    (kao-history-init)
    (let ((rows (kao-vundo--layout (kao-history-root-id))))
      (should (= (length rows) 1))
      (should (= (kao-vundo-row-id (car rows)) 0))
      (should (string= (kao-vundo--row-prefix (car rows)) "")))))

;;;; Render (reverse map + current highlight)

(ert-deftest kao-vundo-render-tags-and-highlights ()
  "Render tags each line with its id, marks the current node `●', returns its
glyph position, and draws the others `○'."
  (with-temp-buffer
    (kao-vundo-tests--build-branched)             ; current = 5
    (let* ((rows (kao-vundo--layout (kao-history-root-id)))
           (current (kao-history-current-id)))
      ;; nil summaries: this tree carries symbol groups (topology only), and
      ;; render mechanics (glyph + id property) do not depend on the label.
      (with-temp-buffer
        (let ((target (kao-vundo--render rows current nil)))
          ;; the returned position carries the current id and the `●' glyph
          (should (eq (get-text-property target 'kao-vundo-id) current))
          (should (string= (buffer-substring target (1+ target)) "●"))
          ;; both glyphs appear; current is unique
          (should (string-match-p "●" (buffer-string)))
          (should (string-match-p "○" (buffer-string)))
          ;; the id property round-trips for every node, exactly the node set
          (let ((seen '()) (pos (point-min)))
            (while (< pos (point-max))
              (let ((id (get-text-property pos 'kao-vundo-id)))
                (when (and id (not (memq id seen))) (push id seen)))
              (setq pos (1+ pos)))
            (should (equal (sort seen #'<) '(0 1 2 3 4 5)))))))))

;;;; Navigation integration (drives the origin via the real command)

(ert-deftest kao-vundo-navigation-drives-origin ()
  "Opening `kao-vundo' and driving parent / redo-child / sibling moves the
ORIGIN buffer's content, current-id and selection exactly like the keys."
  (kao-vundo-tests--with "" 1
    (insert "aaa") (kao--hist-maybe-commit)       ; node 1 "aaa"
    (kao-history-goto 0)                            ; back to root
    (insert "ZZ") (kao--hist-maybe-commit)          ; node 2 "ZZ", sibling of 1
    (should (= (kao-history-current-id) 2))         ; tree 0 -> {1,2}, current 2
    (let ((origin (current-buffer)))
      (kao-vundo)
      (unwind-protect
          (with-current-buffer "*kao-vundo*"
            (should (eq kao-vundo--origin origin))
            ;; parent: 2 -> root 0
            (kao-vundo-goto-parent)
            (should (= (with-current-buffer origin (kao-history-current-id)) 0))
            (should (string= (with-current-buffer origin (buffer-string)) ""))
            ;; redo-child: 0 -> 2 (last applied), re-selects the re-inserted span
            (kao-vundo-goto-redo-child)
            (should (= (with-current-buffer origin (kao-history-current-id)) 2))
            (should (string= (with-current-buffer origin (buffer-string)) "ZZ"))
            (let ((s (with-current-buffer origin (kao--main-sel))))
              (should (= (kao-sel-anchor s) 1))      ; [1,3) cursor on last char
              (should (= (kao-sel-cursor s) 2)))
            ;; sibling: 2 -> 1 (cross-branch, the C-j reach)
            (kao-vundo-goto-prev-sibling)
            (should (= (with-current-buffer origin (kao-history-current-id)) 1))
            (should (string= (with-current-buffer origin (buffer-string)) "aaa")))
        (kill-buffer "*kao-vundo*")))))

(ert-deftest kao-vundo-goto-point-jumps-to-node-under-point ()
  "RET (`kao-vundo-goto-point') moves the origin to the node tagged at point."
  (kao-vundo-tests--with "" 1
    (insert "aaa") (kao--hist-maybe-commit)         ; node 1
    (insert "bbb") (kao--hist-maybe-commit)         ; node 2, current 2
    (let ((origin (current-buffer)))
      (kao-vundo)
      (unwind-protect
          (with-current-buffer "*kao-vundo*"
            ;; move point onto node 1's line, then RET
            (goto-char (point-min))
            (let ((m (text-property-search-forward 'kao-vundo-id 1 t)))
              (should m)
              (goto-char (prop-match-beginning m)))
            (kao-vundo-goto-point)
            (should (= (with-current-buffer origin (kao-history-current-id)) 1))
            (should (string= (with-current-buffer origin (buffer-string)) "aaa")))
        (kill-buffer "*kao-vundo*")))))

(ert-deftest kao-vundo-navigation-read-only-origin-graceful ()
  "A nav move on a read-only origin surfaces a message, not an error, and leaves
the origin untouched (handoff gotcha 5)."
  (kao-vundo-tests--with "abc" 1
    (insert "X") (kao--hist-maybe-commit)           ; node 1, current 1
    (let ((origin (current-buffer)))
      (kao-vundo)
      (unwind-protect
          (with-current-buffer "*kao-vundo*"
            (with-current-buffer origin (setq buffer-read-only t))
            (kao-vundo-goto-parent)                  ; would barf read-only -> caught
            (should (with-current-buffer origin buffer-read-only))
            (should (= (with-current-buffer origin (kao-history-current-id)) 1)))
        (with-current-buffer origin (setq buffer-read-only nil))
        (kill-buffer "*kao-vundo*")))))

(ert-deftest kao-vundo-command-rejects-non-kao-buffer ()
  "`kao-vundo' errors in a buffer with no kao history."
  (with-temp-buffer
    (should-error (kao-vundo) :type 'user-error)))

;;;; Live refresh integration (the open view tracks origin commits)

(ert-deftest kao-vundo-live-refresh-on-origin-commit ()
  "Committing a new node in the origin while the view is open re-renders it."
  (kao-vundo-tests--with "" 1
    (insert "aaa") (kao--hist-maybe-commit)        ; node 1, current 1
    (let ((origin (current-buffer)))
      (kao-vundo)
      (unwind-protect
          (progn
            ;; the view shows node 1 but not yet node 2
            (with-current-buffer "*kao-vundo*"
              (goto-char (point-min))
              (should-not (text-property-search-forward 'kao-vundo-id 2 t)))
            ;; edit + commit in the origin -> node 2; the hook live-refreshes
            (with-current-buffer origin
              (goto-char (point-max))
              (insert "bbb") (kao--hist-maybe-commit))
            ;; the view now carries node 2, current highlight on it
            (with-current-buffer "*kao-vundo*"
              (goto-char (point-min))
              (should (text-property-search-forward 'kao-vundo-id 2 t))))
        (kill-buffer "*kao-vundo*")))))

(ert-deftest kao-vundo-teardown-detaches-hook-on-view-kill ()
  "Killing the view removes the live-refresh hook from the origin."
  (kao-vundo-tests--with "" 1
    (insert "a") (kao--hist-maybe-commit)
    (let ((origin (current-buffer)))
      (kao-vundo)
      (should (with-current-buffer origin
                (memq #'kao-vundo--on-origin-change kao-history-change-hook)))
      (kill-buffer "*kao-vundo*")
      (should-not (with-current-buffer origin
                    (memq #'kao-vundo--on-origin-change kao-history-change-hook))))))

(ert-deftest kao-vundo-repoint-detaches-previous-origin ()
  "Opening the view for a second origin detaches the hook from the first.
\(`kao-vundo' selects the view window, so read the hook via `with-current-buffer'
the origin, not bare.)"
  (let (origin-a origin-b)
    (unwind-protect
        (kao-vundo-tests--with "aaa" 1
          (setq origin-a (current-buffer))
          (insert "x") (kao--hist-maybe-commit)
          (kao-vundo)
          (should (with-current-buffer origin-a
                    (memq #'kao-vundo--on-origin-change kao-history-change-hook)))
          ;; open the view for a DIFFERENT origin: re-point
          (kao-vundo-tests--with "bbb" 1
            (setq origin-b (current-buffer))
            (insert "y") (kao--hist-maybe-commit)
            (kao-vundo)
            (should (with-current-buffer origin-b
                      (memq #'kao-vundo--on-origin-change kao-history-change-hook)))
            ;; the first origin's hook was detached on re-point
            (should-not (with-current-buffer origin-a
                          (memq #'kao-vundo--on-origin-change kao-history-change-hook)))))
      (when (get-buffer "*kao-vundo*") (kill-buffer "*kao-vundo*")))))

(ert-deftest kao-vundo-live-refresh-error-is-demoted ()
  "A failing refresh is demoted to a message and never breaks the origin commit."
  (kao-vundo-tests--with "" 1
    (insert "a") (kao--hist-maybe-commit)
    (let ((origin (current-buffer)))
      (kao-vundo)
      (unwind-protect
          (cl-letf (((symbol-function 'kao-vundo--refresh)
                     (lambda () (error "boom"))))
            (let ((debug-on-error nil))            ; let with-demoted-errors catch
              (with-current-buffer origin
                (goto-char (point-max))
                (insert "b")
                (kao--hist-maybe-commit)           ; must NOT raise
                (should (string= (buffer-string) "ab"))
                (should (= (kao-history-current-id) 2)))))
        (kill-buffer "*kao-vundo*")))))

;;;; Diff preview (d)

(ert-deftest kao-vundo-show-diff-displays-node-change ()
  "`d' shows the node-at-point's change in *kao-vundo-diff* (a + inserted line)."
  (kao-vundo-tests--with "" 1
    (insert "aaa") (kao--hist-maybe-commit)         ; node 1: + "aaa", current
    (kao-vundo)
    (unwind-protect
        (with-current-buffer "*kao-vundo*"
          (kao-vundo-show-diff)                      ; point on node 1's glyph
          (let ((buf (get-buffer "*kao-vundo-diff*")))
            (should buf)
            (with-current-buffer buf
              (should (string-match-p "Change #1" (buffer-string)))
              (should (string-match-p "^\\+ aaa$" (buffer-string)))
              (should-not (string-match-p "^- " (buffer-string))))))
      (when (get-buffer "*kao-vundo-diff*") (kill-buffer "*kao-vundo-diff*"))
      (when (get-buffer "*kao-vundo*") (kill-buffer "*kao-vundo*")))))

(ert-deftest kao-vundo-show-diff-replace-shows-both-sides ()
  "`d' on a replace node shows both a `- DELETED' and a `+ INSERTED' line."
  (kao-vundo-tests--with "hello world" 1
    (goto-char 7) (delete-region 7 12) (insert "emacs")   ; replace "world"->"emacs"
    (kao--hist-maybe-commit)
    (kao-vundo)
    (unwind-protect
        (with-current-buffer "*kao-vundo*"
          (kao-vundo-show-diff)                            ; point on the replace node
          (with-current-buffer "*kao-vundo-diff*"
            (should (string-match-p "^- world$" (buffer-string)))
            (should (string-match-p "^\\+ emacs$" (buffer-string)))))
      (when (get-buffer "*kao-vundo-diff*") (kill-buffer "*kao-vundo-diff*"))
      (when (get-buffer "*kao-vundo*") (kill-buffer "*kao-vundo*")))))

(ert-deftest kao-vundo-show-diff-no-buffer-for-root-or-off-node ()
  "`d' creates no diff buffer for the root (no change) or off any node."
  (when (get-buffer "*kao-vundo-diff*") (kill-buffer "*kao-vundo-diff*"))
  (kao-vundo-tests--with "" 1
    (insert "a") (kao--hist-maybe-commit)
    (kao-vundo)
    (unwind-protect
        (with-current-buffer "*kao-vundo*"
          ;; the root line (node 0): no change -> message, no buffer
          (goto-char (point-min))
          (should (eq (get-text-property (point) 'kao-vundo-id) 0))
          (kao-vundo-show-diff)
          (should-not (get-buffer "*kao-vundo-diff*"))
          ;; past all nodes (no kao-vundo-id): message, no buffer
          (goto-char (point-max))
          (kao-vundo-show-diff)
          (should-not (get-buffer "*kao-vundo-diff*")))
      (when (get-buffer "*kao-vundo-diff*") (kill-buffer "*kao-vundo-diff*"))
      (when (get-buffer "*kao-vundo*") (kill-buffer "*kao-vundo*")))))

;;;; Killed / reverted origin resilience

(ert-deftest kao-vundo-dead-origin-commands-error-cleanly ()
  "Every viewer command reports a clean `user-error' — never a raw
`Selecting deleted buffer' — once the origin buffer has been killed out from
under the open view."
  (let ((origin (generate-new-buffer "kao-vundo-origin")))
    (unwind-protect
        (progn
          (with-current-buffer origin
            (insert "aaa")
            (kao-mode 1)
            (kao--hist-maybe-commit)
            (kao-vundo))
          (kill-buffer origin)
          (should (get-buffer "*kao-vundo*"))
          (with-current-buffer "*kao-vundo*"
            (should-not (buffer-live-p kao-vundo--origin))
            ;; The render left point on the current node's glyph (has an id), so
            ;; the point-reading commands (RET / d) still reach the origin gate.
            (should (get-text-property (point) 'kao-vundo-id))
            (dolist (cmd '(kao-vundo-goto-parent
                           kao-vundo-goto-redo-child
                           kao-vundo-goto-next-sibling
                           kao-vundo-goto-prev-sibling
                           kao-vundo-goto-point
                           kao-vundo-show-diff
                           kao-vundo-refresh))
              (let ((err (should-error (funcall cmd) :type 'user-error)))
                (should (string-match-p "origin buffer is gone"
                                        (error-message-string err)))))))
      (when (buffer-live-p origin) (kill-buffer origin))
      (when (get-buffer "*kao-vundo*") (kill-buffer "*kao-vundo*")))))

(ert-deftest kao-vundo-refresh-resubscribes-after-origin-reset ()
  "After a non-preserve-modes revert resets the origin's buffer-locals — dropping
the live-refresh subscription — `kao-vundo--refresh' (g) re-installs it so live
refresh recovers."
  (let ((origin (generate-new-buffer "kao-vundo-origin")))
    (unwind-protect
        (progn
          (with-current-buffer origin
            (insert "aaa")
            (kao-mode 1)
            (kao--hist-maybe-commit)
            (kao-vundo))
          ;; The view subscribed the origin at open.
          (with-current-buffer origin
            (should (memq #'kao-vundo--on-origin-change kao-history-change-hook)))
          ;; A non-preserve-modes revert: `kill-all-local-variables' drops the
          ;; subscription; `kao-global-mode' re-enables kao-mode (fresh tree) but
          ;; nothing re-adds the vundo hook.
          (with-current-buffer origin
            (kill-all-local-variables)
            (kao-mode 1)
            (should-not (memq #'kao-vundo--on-origin-change
                              kao-history-change-hook)))
          ;; g (refresh) must re-subscribe so live refresh recovers.
          (with-current-buffer "*kao-vundo*"
            (kao-vundo-refresh))
          (with-current-buffer origin
            (should (memq #'kao-vundo--on-origin-change kao-history-change-hook))))
      (when (buffer-live-p origin) (kill-buffer origin))
      (when (get-buffer "*kao-vundo*") (kill-buffer "*kao-vundo*")))))

;;;; Mode-off guard sweep — (ADDITIVE pin)

;; `kao-vundo' is the M-x entry point invoked from the editing buffer; with
;; `kao-mode' off it must signal the shared `kao--assert-mode' guard (Foundation
;), the same named `user-error' as every other kao command.  (The in-view
;; navigation commands run in the `*kao-vundo*' special-mode buffer where
;; `kao-mode' is off by design, so they are deliberately NOT guarded.)

(ert-deftest kao-vundo-mode-off-guards ()
  "I4: `kao-vundo' via M-x with `kao-mode' off signals the shared guard."
  (with-temp-buffer                     ; fundamental-mode temp buffer: mode OFF
    (insert "abc")
    (let ((err (should-error (call-interactively #'kao-vundo)
                             :type 'user-error)))
      (should (string-match-p "kao-mode is not active" (cadr err))))))

(ert-deftest kao-vundo-mode-off-names-the-command ()
  "`kao-vundo' via M-x with `kao-mode' off names the command
in the guard message (`kao--assert-mode' optional context), so the mode-off
cause is legible.  Additive to `kao-vundo-mode-off-guards' — the older
substring pin still matches the prefixed message."
  (with-temp-buffer                     ; fundamental-mode temp buffer: mode OFF
    (insert "abc")
    (let ((err (should-error (call-interactively #'kao-vundo)
                             :type 'user-error)))
      (should (string-match-p "^kao-vundo: kao-mode is not active"
                              (cadr err))))))

(provide 'kao-vundo-tests)
;;; kao-vundo-tests.el ends here
