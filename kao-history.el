;;; kao-history.el --- Buffer history tree -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 1: the buffer-undo history TREE.
;;
;; A faithful port of Kakoune's `Buffer' history (buffer.cc:278-403).  Kakoune
;; keeps one tree of `HistoryNode{parent, redo_child, undo_group}' per buffer;
;; `u'/`U' walk parent/redo-child while `<c-j>'/`<c-k>' navigate by absolute
;; change-id (`move_in_history', normal.cc:2201 -> `Buffer::move_to').  Because
;; the change-id walk can land on a node of an ABANDONED sibling branch, the
;; four history keys must share ONE tree — a linear undo model cannot
;; express the cross-branch reach.
;;
;; This file holds the tree itself plus the PURE topology (node table, the
;; lowest-common-ancestor `move_to' walk, and the undo/redo target computation)
;; AND the buffer capture/replay engine itself: the change-hook recorder and the
;; symmetric apply/revert of each node's edits (lower half).  `move_to' takes an
;; injected APPLY-STEP callback so the topology walk stays unit-testable with no
;; buffer; kao-state/kao-edit only INSTALL the change hooks and DRIVE the
;; per-command commits and the `u'/`U'/`<c-j>'/`<c-k>' navigation on top.
;;
;; The "native mechanism, faithful behaviour" family:
;; the change CAPTURE rides native buffer-local change hooks
;; (`before-change-functions'/`after-change-functions'), but each node stores
;; kao's own chronological `kao-hist-mod' group and apply/revert are explicit
;; buffer edits (`delete-region'+`insert', `deleted'/`inserted' swapped to
;; revert) — `buffer-undo-list' is bound off during the replay so native undo
;; is never polluted, and kao adds the tree topology on top.  Two rejected
;; capture mechanisms (see the 2026-06-11 ledger): `primitive-undo' of
;; `buffer-undo-list' fragments (no redo records for a never-undone sibling
;; branch) and parsing fragments at commit (stale-position reads) — edit-time
;; `kao-hist-mod's are the only position-safe, direction-symmetric capture.
;; Native Emacs undo stays ON for the insert/edit change-group amalgamation but
;; is NOT the history source.  This supersedes the older `u'/`U'-on-native-undo
;; mechanism (their behaviour is preserved) so all four keys share this tree.

;;; Code:

(require 'cl-lib)

(defcustom kao-history-max-nodes 10000
  "Maximum number of buffer-history tree nodes kept per buffer.
When a commit pushes the tree past this cap, the oldest root-side nodes are
dropped — the root advances one step toward the current node, discarding the
abandoned sibling subtrees hanging off it (the Emacs `undo-limit' philosophy).
The current node is never dropped; `u' bottoms out at the oldest RETAINED
node, and a `<c-j>'/`<c-k>' jump to a dropped id reports \"no such change\".
Kakoune keeps an unbounded tree; the cap is the documented Emacs-pragmatic
deviation that bounds memory (family — cf. `kao-sel-history-max')."
  :type 'natnum
  :group 'kao)

(cl-defstruct (kao-hist-node (:constructor kao-hist-node-make))
  "One node of the buffer history tree (Kakoune `HistoryNode')."
  parent              ; integer id of the parent node, or nil for the root
  (redo-child nil)    ; integer id of the last-applied child, or nil
  (children nil)      ; integer ids of all children (for subtree gc)
  (group nil))        ; chronological `kao-hist-mod' vector (root: nil)

(defvar-local kao--hist nil
  "Hash table mapping HistoryId (integer) -> `kao-hist-node'.
Gc'd ids (see `kao-history-max-nodes') are absent: ids are absolute and never
renumbered, so a dropped id simply has no entry.")
(defvar-local kao--hist-id nil
  "HistoryId of the node the buffer currently sits at.")
(defvar-local kao--hist-next nil
  "Next HistoryId to assign; counts every node ever created (gc'd ones too).")
(defvar-local kao--hist-root nil
  "HistoryId of the tree's current root (0 until gc advances it).")
(defvar-local kao--hist-saved-id nil
  "HistoryId whose buffer content matches the file on disk, or nil for none.
Kakoune's `m_saved_history_id' (buffer.hh): the buffer `is_modified' iff the
current id differs from this (or pending modifications exist — see
`kao-history-sync-modified').  Seeded to the root (0) when `kao-history-init'
runs on an unmodified buffer and to nil when it runs on an already-modified one
\(no saved node yet), then re-pinned to the current id on every save by
`kao-history-mark-saved'.")

(cl-defstruct (kao-hist-mod (:constructor kao-hist-mod-make))
  "One primitive buffer change (Kakoune `Modification'): at POS the text
DELETED was replaced by INSERTED.  Either string may be empty."
  pos deleted inserted)

(defvar-local kao--hist-pending nil
  "Reverse-chronological list of `kao-hist-mod' for the in-progress command.")
(defvar-local kao--hist-bc nil
  "Before-change stash: (BEG . DELETED-TEXT) for the next after-change, or nil.")
(defvar-local kao--hist-navigating nil
  "Non-nil while replaying a group: suppresses capture and mutes native undo.")
(defvar-local kao--hist-tick nil
  "`buffer-chars-modified-tick' as of the last change the tree saw.
Refreshed on capture, commit, init, and after every `kao-history-move-to' walk.
A mismatch against the live tick at navigation time means the shared text was
edited behind the tree's back — an indirect buffer, `buffer-swap-text', or an
`inhibit-modification-hooks' writer — so `kao--hist-navigate' refuses rather
than replay a node at now-wrong positions.")

(defvar kao--hist-generation-source 0
  "Module-global monotonic counter handing out tree generations.
Each `kao-history-init' takes the next value for its buffer's
`kao--hist-generation'.  Never restarts, so no two trees — across re-inits or
buffers — ever share a generation.")
(defvar-local kao--hist-generation nil
  "This buffer's history-tree generation, bumped on every `kao-history-init'.
`kao-mode' restarts HistoryIds at 0 on every enable (also plain `revert-buffer',
`find-alternate-file'), so a snapshot tag from the previous tree can alias
an unrelated node in the fresh one.  Stamping the generation into every tag lets
`kao--snapshot-update' spot a superseded tree and fall to the clamp+merge
residual instead of translating through — or matching verbatim — unrelated ids
\.")

(defun kao-history-init ()
  "Seed the tree with the root node (id 0) and place the buffer at it."
  (setq kao--hist (make-hash-table :test 'eq)
        kao--hist-next 0)
  (puthash 0 (kao-hist-node-make :parent nil) kao--hist)
  (setq kao--hist-next 1
        kao--hist-id 0
        kao--hist-root 0
        ;; An unmodified buffer's on-disk content IS the root node; a buffer
        ;; already modified when `kao-mode' turns on has no saved node yet.
        kao--hist-saved-id (if (buffer-modified-p) nil 0)
        kao--hist-pending nil
        kao--hist-bc nil
        kao--hist-navigating nil
        kao--hist-tick (buffer-chars-modified-tick)
        ;; A fresh tree gets a fresh generation so its tags can never be
        ;; confused with a superseded tree's.
        kao--hist-generation (setq kao--hist-generation-source
                                   (1+ kao--hist-generation-source))))

(defsubst kao--hist-node (id)
  "Return the `kao-hist-node' with HistoryId ID."
  (gethash id kao--hist))

(defun kao-history-current-id ()
  "Return the current HistoryId."
  kao--hist-id)

(defun kao-history-generation ()
  "Return this buffer's tree generation (bumped on every `kao-history-init').
nil outside a kao buffer.  snapshot tags stamp it so `kao--snapshot-update'
can reject a tag recorded against a tree that has since been replaced
\."
  kao--hist-generation)

(defun kao--snap-tag-make (buffer snap)
  "Return a (BUFFER (GEN . ID) . SNAP) tagged snapshot for BUFFER.
GEN/ID are BUFFER's current `kao-history-generation' /
`kao-history-current-id' — the coordinate frame the restore-time
translation (`kao--snapshot-update') consumes.  The ONLY constructor of
this shape."
  (cons buffer
        (cons (cons (kao-history-generation) (kao-history-current-id))
              snap)))

(defsubst kao--snap-tag-buffer (tag) "TAG's source buffer." (car tag))
(defsubst kao--snap-tag-id (tag) "TAG's (GEN . ID) frame." (cadr tag))
(defsubst kao--snap-tag-snap (tag) "TAG's `kao-sels' snapshot." (cddr tag))

(defun kao-history-max-id ()
  "Return the highest assigned HistoryId (Kakoune `next_history_id() - 1')."
  (1- kao--hist-next))

(defun kao-history-last-modification-pos ()
  "Position of the current node's last modification, or nil.
Port of `Buffer::last_modification_coord' (buffer.cc:672-677): nil at the
root (`HistoryId::First'), else `undo_group.back().coord' — the position of
the CURRENT node's last modification.  Pending (uncommitted) mods are
ignored, exactly as in the C++ (they live outside the committed node).  A
gc-advanced root's cleared group also yields nil — the honest
cap-floor answer."
  (let ((group (kao-hist-node-group (kao--hist-node kao--hist-id))))
    (when (and group (> (length group) 0))
      (kao-hist-mod-pos (aref group (1- (length group)))))))

;;;; Public read-only topology accessors (kao-vundo §5)

;; Thin public wrappers over the tree so a viewer (`kao-vundo') or a config
;; builds on `kao-' symbols rather than the `kao--hist*' internals (
;; substrate rule, ).  Each tolerates an absent id — never
;; created, or dropped by the `kao-history-max-nodes' gc — by returning nil,
;; and a buffer with no tree (`kao--hist' nil, i.e. no `kao-mode') without
;; erroring.  Read-only: they never mutate the tree.

(defun kao-history-root-id ()
  "Return the HistoryId of the current tree's root, or nil with no tree.
The root is 0 until the `kao-history-max-nodes' gc advances it, so walk
the tree from this id rather than assuming 0."
  kao--hist-root)

(defun kao-history-node-exists-p (id)
  "Return non-nil when history node ID exists in the current buffer's tree.
Nil for an absent/gc-dropped id and in a buffer with no tree."
  (and kao--hist (kao--hist-node id) t))

(defun kao-history-node-parent (id)
  "Return the parent HistoryId of node ID, or nil (the root, or an absent ID)."
  (let ((node (and kao--hist (kao--hist-node id))))
    (and node (kao-hist-node-parent node))))

(defun kao-history-node-children (id)
  "Return the child HistoryIds of node ID as a list, or nil.
Nil for a leaf or an absent ID.  The list is what makes the history a tree."
  (let ((node (and kao--hist (kao--hist-node id))))
    (and node (kao-hist-node-children node))))

(defun kao-history-node-redo-child (id)
  "Return the last-applied child HistoryId of node ID, or nil.
This is the `U'/redo path out of ID (nil at a leaf or for an absent ID)."
  (let ((node (and kao--hist (kao--hist-node id))))
    (and node (kao-hist-node-redo-child node))))

(defun kao-history-node-summary (id)
  "Return a short \"+INS/-DEL\" edit-size label for node ID, or nil.
Sum the inserted and deleted characters across the node's modification group;
nil for an absent ID or a group-less node (the root, or a gc-cleared root)."
  (let* ((node (and kao--hist (kao--hist-node id)))
         (group (and node (kao-hist-node-group node))))
    (when group
      (let ((ins 0) (del 0) (n (length group)))
        (dotimes (i n)
          (let ((m (aref group i)))
            (setq ins (+ ins (length (kao-hist-mod-inserted m)))
                  del (+ del (length (kao-hist-mod-deleted m))))))
        (format "+%d/-%d" ins del)))))

(defun kao-history-node-changes (id)
  "Return node ID's modifications as a list of (POS DELETED INSERTED), or nil.
Each element is one primitive change (`kao-hist-mod') in chronological order:
at POS the DELETED text was replaced by INSERTED (either string may be empty).
nil for an absent ID or a group-less node (the root, or a gc-cleared root).
Read-only: the returned strings are the node's own; do not mutate them."
  (let* ((node (and kao--hist (kao--hist-node id)))
         (group (and node (kao-hist-node-group node))))
    (when group
      (let ((out '()) (n (length group)))
        (dotimes (i n)
          (let ((m (aref group i)))
            (push (list (kao-hist-mod-pos m)
                        (kao-hist-mod-deleted m)
                        (kao-hist-mod-inserted m))
                  out)))
        (nreverse out)))))

(defvar kao-history-change-hook nil
  "Hook run after a new node is committed to the buffer-history tree.
Fires once per `kao-history-commit' — so once per `kao-history-commit-pending'
that had pending modifications — AFTER the node is appended and the gc has run,
and NEVER during navigation/replay (`kao-history-move-to' appends no node).
Each fire is guarded `(when kao-history-change-hook (run-hooks ...))', so a
buffer with no listener pays only a nil-check on the per-command commit path
\(the `kao-selection-change-hook' pattern; the per-keystroke motion path never
commits, so the benched hot path is untouched).  `kao-vundo' subscribes
buffer-locally on its origin buffer to live-refresh the open view.")

(defun kao-history-commit (group)
  "Append a child node carrying GROUP under the current node; advance current.
Port of `Buffer::commit_undo_group' (buffer.cc:291): the new node's parent is
the current node, the current node's `redo-child' is set to the new node, and
the new node becomes current.  Returns the new HistoryId.  Runs
`kao-history-change-hook' after the append + gc (guarded)."
  (let ((id kao--hist-next)
        (parent (kao--hist-node kao--hist-id)))
    (puthash id (kao-hist-node-make :parent kao--hist-id :group group) kao--hist)
    (setf (kao-hist-node-redo-child parent) id)
    (push id (kao-hist-node-children parent))
    (setq kao--hist-id id
          kao--hist-next (1+ kao--hist-next))
    ;; The committed group came from edits the tree saw (capture already stamped
    ;; the tick); re-affirm the integrity stamp on the commit path too.  A commit
    ;; only runs when there were pending mods, so it never masks a foreign edit —
    ;; that path (no pending) leaves `kao--hist-tick' stale for the guard to catch.
    (setq kao--hist-tick (buffer-chars-modified-tick))
    (kao--hist-gc)
    (when kao-history-change-hook (run-hooks 'kao-history-change-hook))
    id))

(defun kao--hist-delete-subtree (id)
  "Remove node ID and its entire subtree from the table."
  (dolist (c (kao-hist-node-children (kao--hist-node id)))
    (kao--hist-delete-subtree c))
  (remhash id kao--hist))

(defun kao--hist-gc ()
  "Drop oldest root-side nodes while the tree exceeds `kao-history-max-nodes'.
Advances the root one step toward the current node per round, deleting every
sibling subtree abandoned at the old root.  The kept child becomes the new
root: its parent link is severed and its group cleared (the old root is gone,
so the edge can never be reverted — clearing frees the captured strings).
Stops when the root IS the current node (the current node is never dropped).

O(1) per round apart from the dropped nodes themselves: for every proper
ancestor of the current node the `redo-child' link points along the path to
the current node (`kao-history-commit' sets it; the `kao-history-move-to'
descent refreshes it), so the root's kept child is simply its `redo-child'."
  (while (and (> (hash-table-count kao--hist) kao-history-max-nodes)
              (/= kao--hist-root kao--hist-id))
    (let* ((root-node (kao--hist-node kao--hist-root))
           (keep (kao-hist-node-redo-child root-node)))
      (dolist (c (kao-hist-node-children root-node))
        (unless (eq c keep)
          (kao--hist-delete-subtree c)))
      (remhash kao--hist-root kao--hist)
      (let ((keep-node (kao--hist-node keep)))
        (setf (kao-hist-node-parent keep-node) nil
              (kao-hist-node-group keep-node) nil))
      (setq kao--hist-root keep))))

(defun kao--hist-depth (id)
  "Return the depth of node ID (root = 0)."
  (let ((d 0))
    (while (kao-hist-node-parent (kao--hist-node id))
      (setq id (kao-hist-node-parent (kao--hist-node id))
            d (1+ d)))
    d))

(defun kao--hist-lca (a b)
  "Return the lowest common ancestor HistoryId of nodes A and B.
Port of `find_lowest_common_parent' (buffer.cc:354)."
  (let ((da (kao--hist-depth a))
        (db (kao--hist-depth b)))
    (while (> da db)
      (setq a (kao-hist-node-parent (kao--hist-node a))
            da (1- da)))
    (while (> db da)
      (setq b (kao-hist-node-parent (kao--hist-node b))
            db (1- db)))
    (while (/= a b)
      (setq a (kao-hist-node-parent (kao--hist-node a))
            b (kao-hist-node-parent (kao--hist-node b))))
    a))

(defun kao-history-move-to (id apply-step)
  "Move the current node to ID, invoking APPLY-STEP for each edge.
APPLY-STEP is called as (APPLY-STEP DIR NODE-ID): DIR is `revert' while walking
up to the lowest common ancestor and `apply' while walking back down to ID; the
caller performs the matching buffer edit.  `redo-child' links are refreshed on
the descent so a later `U' follows this path.  Returns t, or nil when ID is out
of range (Kakoune `Buffer::move_to', buffer.cc:345) or when ID was dropped by
the `kao-history-max-nodes' gc (no Kakoune analogue — its tree is uncapped)."
  (when (and (integerp id) (>= id 0) (< id kao--hist-next)
             (kao--hist-node id))
    (let ((parent (kao--hist-lca kao--hist-id id))
          ;; The replay is a TRANSACTION: a mid-walk signal — a
          ;; `text-read-only' property, or any error out of APPLY-STEP — must
          ;; leave the buffer AND `kao--hist-id' exactly as they were, never a
          ;; half-applied node.  `buffer-undo-list' is disabled around the group
          ;; so `accept-change-group' DISCARDS the recorded replay on success
          ;; (native undo is dormant, not the history source);
          ;; `activate-change-group' re-enables recording just for the group so
          ;; `cancel-change-group' can roll it back on abort.
          ;; `kao--hist-navigating' spans the cancel too, so kao's own change
          ;; hooks ignore the rollback edits as well as the forward replay.
          (buffer-undo-list t)
          (kao--hist-navigating t)
          (handle (prepare-change-group))
          (ok nil))
      (activate-change-group handle)
      (unwind-protect
          (progn
            ;; Undo up to the common parent.
            (let ((cur kao--hist-id))
              (while (/= cur parent)
                (funcall apply-step 'revert cur)
                (setq cur (kao-hist-node-parent (kao--hist-node cur)))))
            ;; Re-apply from the common parent down to ID (parent-first recursion).
            (cl-labels ((down (target)
                          (unless (eq target parent)
                            (let* ((node (kao--hist-node target))
                                   (p (kao-hist-node-parent node)))
                              (down p)
                              (setf (kao-hist-node-redo-child (kao--hist-node p))
                                    target)
                              (funcall apply-step 'apply target)))))
              (down id))
            (setq kao--hist-id id
                  ok t))
        (if ok
            (accept-change-group handle)
          (cancel-change-group handle))
        ;; Whether the walk committed (content = target node) or rolled back
        ;; (content restored to the unchanged current node), the buffer now
        ;; matches `kao--hist-id'; re-stamp the integrity tick PAST the replay
        ;; (and past any rollback edits) so the next navigation is not a false
        ;; refusal.
        (setq kao--hist-tick (buffer-chars-modified-tick)))
      ok)))

(defun kao-history--move-edges (id)
  "Return the edges of a `kao-history-move-to' ID walk in traversal order.
Each element is a cons (DIR . NODE-ID): DIR `revert' for the edges from the
current node up to (excluding) the lowest common ancestor, then DIR `apply'
for the edges from the LCA down to ID — the exact sequence `kao-history-move-to'
would drive APPLY-STEP with.  The pure-topology mirror of that walk (no buffer,
no mutation), used to pre-validate a navigation.  Returns nil for an
out-of-range or dropped ID (nothing to walk / caller falls through to
`kao-history-move-to''s own nil)."
  (when (and (integerp id) (>= id 0) (< id kao--hist-next) (kao--hist-node id))
    (let ((lca (kao--hist-lca kao--hist-id id))
          (revs nil) (down nil))
      (let ((cur kao--hist-id))
        (while (/= cur lca)
          (push (cons 'revert cur) revs)
          (setq cur (kao-hist-node-parent (kao--hist-node cur)))))
      (let ((cur id))
        (while (/= cur lca)
          (push (cons 'apply cur) down)       ; parent-first: child-of-LCA .. ID
          (setq cur (kao-hist-node-parent (kao--hist-node cur)))))
      (append (nreverse revs) down))))

(defun kao-history-move-in-region-p (id pmin pmax)
  "Return non-nil when navigating to ID touches only positions within [PMIN, PMAX].
Pre-walks the `kao-history-move-to' ID edges (`kao-history--move-edges') in the
same direction-symmetric frames `kao--hist-apply-group' replays them in (revert
reverses the group with deleted/inserted swapped), tracking the accessible
upper bound as each edit would shift it.  So it predicts EXACTLY when
`kao--hist-apply-group''s raw `delete-region'/`insert' would land outside the
narrowed region: t when the whole walk stays inside, nil when any edit escapes
[PMIN, PMAX].  Buffer-free — PMIN/PMAX are the caller's `point-min'/`point-max'
and PMAX is advanced by each edit's (inserted - deleted) length delta, mirroring
how an insertion/deletion inside the narrowing moves `point-max'.

Only the DELETION extent `[pos, pos + deleted)' must lie inside the region: a
`delete-region' outside it signals `args-out-of-range'/`text-read-only', while
the following `insert' merely grows the accessible region from a valid point
`pos' (an insertion whose point escapes is caught by `pos' itself, an insertion
AT `point-max' is legal).  The REFINED-REC's `max(deleted, inserted)' span
over-rejects a valid insertion whose result would overrun `point-max' (e.g. a
cross-branch redo re-inserting into a shorter buffer), so the check folds the
per-direction deletion length, not the max.  The refusal that keeps
navigation at native-`primitive-undo' parity under narrowing."
  (let ((ok t))
    (dolist (edge (kao-history--move-edges id))
      (when ok
        (let* ((dir (car edge))
               (group (kao-hist-node-group (kao--hist-node (cdr edge))))
               (n (length group)))
          (dotimes (k n)
            (when ok
              ;; Same per-mod order as `kao--hist-apply-group': forward for
              ;; `apply', reversed with deleted/inserted swapped for `revert'.
              (let* ((i (if (eq dir 'apply) k (- n 1 k)))
                     (m (aref group i))
                     (dl (length (kao-hist-mod-deleted m)))
                     (il (length (kao-hist-mod-inserted m)))
                     (del (if (eq dir 'apply) dl il))   ; chars deleted here
                     (ins (if (eq dir 'apply) il dl))   ; chars inserted here
                     (pos (kao-hist-mod-pos m)))
                (if (or (< pos pmin) (> (+ pos del) pmax))
                    (setq ok nil)
                  (setq pmax (+ pmax (- ins del))))))))))
    ok))

(defun kao-history-undo-target (count)
  "Return the HistoryId COUNT steps toward the root, or nil if at the root.
Stops at the root if COUNT overruns (Kakoune `Buffer::undo', buffer.cc:307)."
  (and (kao-hist-node-parent (kao--hist-node kao--hist-id))
       (let ((id kao--hist-id))
         (while (and (> count 0) (kao-hist-node-parent (kao--hist-node id)))
           (setq id (kao-hist-node-parent (kao--hist-node id))
                 count (1- count)))
         id)))

(defun kao-history-redo-target (count)
  "Return the HistoryId COUNT steps along `redo-child', or nil if none.
Stops when no `redo-child' remains (Kakoune `Buffer::redo', buffer.cc:327)."
  (and (kao-hist-node-redo-child (kao--hist-node kao--hist-id))
       (let ((id kao--hist-id))
         (while (and (> count 0) (kao-hist-node-redo-child (kao--hist-node id)))
           (setq id (kao-hist-node-redo-child (kao--hist-node id))
                 count (1- count)))
         id)))

;;;; Modification capture (edit-time, via the change hooks)

;; Native Emacs undo stays on (kao's insert/edit change-group amalgamation is
;; untouched) but is NOT the history source.  kao captures its own chronological
;; `kao-hist-mod' groups via `before-change-functions'/`after-change-functions'
;; — positions recorded when valid — so apply/revert are position-safe in both
;; directions, including descent into a never-undone sibling branch.

(defun kao--hist-before-change (beg end)
  "Stash the text about to be removed in [BEG,END) for the next after-change.
Installed on `before-change-functions'.  Only stashes a non-empty region; a
pure insertion leaves the stash nil so the deletion text is the empty string."
  (unless kao--hist-navigating
    (setq kao--hist-bc (and (> end beg)
                            (cons beg (buffer-substring-no-properties beg end))))))

(defun kao--hist-after-change (beg end _len)
  "Append the modification just made in [BEG,END) to `kao--hist-pending'.
Installed on `after-change-functions'.  The deleted text comes from the
before-change stash when its position matches; the inserted text is the current
[BEG,END) content."
  (unless kao--hist-navigating
    (let ((deleted (if (and kao--hist-bc (= (car kao--hist-bc) beg))
                       (cdr kao--hist-bc)
                     ""))
          (inserted (buffer-substring-no-properties beg end)))
      (setq kao--hist-bc nil)
      (push (kao-hist-mod-make :pos beg :deleted deleted :inserted inserted)
            kao--hist-pending)
      ;; The tree just saw this edit — keep the integrity stamp in sync so a
      ;; later navigation only refuses on edits the tree did NOT see.
      (setq kao--hist-tick (buffer-chars-modified-tick)))))

(defun kao-history-commit-pending ()
  "Commit the accumulated modifications as one history node.
Returns the new HistoryId, or nil when nothing was pending."
  (when kao--hist-pending
    (let ((group (vconcat (nreverse kao--hist-pending))))  ; chronological vector
      (setq kao--hist-pending nil)
      (kao-history-commit group))))

;;;; Buffer-modified flag (Kakoune `Buffer::is_modified', buffer.cc)

;; re-points `u'/`U'/`<c-j>'/`<c-k>' off native Emacs undo and replays each
;; edit inside `kao-history-move-to''s change group with `buffer-undo-list'
;; disabled (accepted-and-discarded on success), so every revert is a fresh
;; `delete-region'+`insert' that bumps the buffer's MODIFF.
;; Emacs's automatic "undo back to the saved state clears the modified flag"
;; therefore never fires for kao navigation — kao must drive the flag itself,
;; faithfully to Kakoune: the buffer is modified iff the current history id
;; differs from the saved one, OR uncommitted modifications are pending.

(defun kao-history-sync-modified ()
  "Set the buffer-modified flag to match the history-tree position.
Port of Kakoune `Buffer::is_modified' (buffer.cc): modified iff there are
pending uncommitted modifications, or the current history id differs from the
saved id (`kao--hist-saved-id').  Called after every tree navigation so an
undo/redo back onto the saved node clears the flag (and the mode line) and a
move away from it re-marks the buffer.  No-ops outside a kao buffer and skips
the redundant `set-buffer-modified-p' when the flag already matches (so file
locking is not churned on every navigation)."
  (when kao--hist
    (let ((modified (or (and kao--hist-pending t)
                        (not (and kao--hist-saved-id
                                  (eq kao--hist-id kao--hist-saved-id))))))
      (unless (eq (and (buffer-modified-p) t) modified)
        (set-buffer-modified-p modified)))))

(defun kao-history-mark-saved ()
  "Pin the saved history id to the current node — the buffer matches its file.
Port of Kakoune `Buffer::notify_saved' (buffer.cc:570-577): commit any pending
modifications as a node first (`commit_undo_group()'), then pin
`m_last_save_history_id = m_history_id'.  Installed on `after-save-hook' by
`kao-mode'; Emacs has already cleared the modified flag by the time this runs.
Committing first is what makes a save taken mid-insert split the session at the
save point — the saved node carries exactly the on-disk content, and further
typing starts a fresh group — so the buffer reads unmodified right after the
save (`is_modified', buffer.cc:563-568)."
  (when kao--hist
    (kao-history-commit-pending)
    (setq kao--hist-saved-id kao--hist-id)))

;;;; Symmetric apply / revert of a node group

(defun kao--hist-edit (pos del ins)
  "Replace the DEL string at POS with the INS string.
Return the touched region as a cons (BEG . END)."
  (goto-char pos)
  (delete-region pos (+ pos (length del)))
  (insert ins)
  (cons pos (+ pos (length ins))))

(defun kao--hist-apply-group (group dir ranges)
  "Apply GROUP (a vector of `kao-hist-mod') to the buffer in direction DIR.
DIR `apply' replays the modifications forward (chronological order); DIR
`revert' replays them in reverse with `deleted'/`inserted' swapped (Kakoune
`apply_modification'/`inverse').

Collects one modified range per replayed mod, faithful to Kakoune
`compute_modified_ranges' (selection.cc:132-160): after each edit, every span
already in RANGES is folded forward through that edit
\(`kao-history--translate-1') so the earlier ranges stay in the current buffer
frame, then the mod's own post-edit inserted span (POS . POS+len) is pushed — a
pure deletion contributes the collapsed point (POS . POS).  Returns the updated
RANGES (each a cons (BEG . END), all in the post-walk frame); the caller threads
it across every edge of the walk so the whole navigation yields one selection
per modified range.

Sets `kao--hist-navigating' so kao's change hooks ignore the replay; the caller
\(`kao-history-move-to') owns the change group that makes the whole walk one
atomic, discardable transaction — this helper no longer binds
`buffer-undo-list' itself, so the group can capture and roll back the replay
\(keeps native undo dormant via the group's discard)."
  (let ((kao--hist-navigating t)
        (n (length group)))
    (dotimes (k n)
      (let* ((i (if (eq dir 'apply) k (- n 1 k)))
             (m (aref group i))
             (pos (kao-hist-mod-pos m))
             (del (if (eq dir 'apply) (kao-hist-mod-deleted m)
                    (kao-hist-mod-inserted m)))
             (ins (if (eq dir 'apply) (kao-hist-mod-inserted m)
                    (kao-hist-mod-deleted m)))
             (dl (length del))
             (il (length ins)))
        (kao--hist-edit pos del ins)
        (setq ranges
              (mapcar (lambda (r)
                        (cons (kao-history--translate-1 (car r) pos dl il)
                              (kao-history--translate-1 (cdr r) pos dl il)))
                      ranges))
        (push (cons pos (+ pos il)) ranges)))
    ranges))

;;;; Position translation through the tree

;; The elisp ForwardChangesTracker (changes.cc:5-68, changes.hh:33-80),
;; flattened to integer positions.  Kakoune's forward/backward run-splitting
;; exists so each selection does ONE pass over many changes; applying the
;; changes one at a time to every position is semantically identical (each
;; step keeps positions and the next change in the same coordinate frame) and
;; right for the cold restore paths this serves.  Each `kao-hist-mod' is an
;; Erase-then-Insert pair at `pos' — exactly Kakoune's two separate
;; `Buffer::Change' records for a replace (`forward_sorted_until' breaks the
;; run at equal begin, so they translate as two single-change runs there too).

(defun kao-history--translate-1 (p pos dl il)
  "Translate position P through one modification at POS.
The modification erased DL chars then inserted IL chars at POS.  The two
primitive rules are Kakoune's `update_range' (changes.hh:33) flattened:
erase [b,e): p<=b keeps p, b<p<=e collapses to b (`get_new_coord_tolerant'),
p>e shifts left; insert at b: p<b keeps p, p>=b shifts right (`relevant'
uses `begin <= coord' for Insert — an insert AT the position is consumed)."
  (setq p (cond ((<= p pos) p)
                ((<= p (+ pos dl)) pos)
                (t (- p dl))))
  (if (< p pos) p (+ p il)))

(defun kao-history--group-translate (p group dir)
  "Translate position P through GROUP (a `kao-hist-mod' vector) along DIR.
DIR `apply' folds the modifications in chronological order; DIR `revert'
folds the inverses in reverse order (deleted/inserted swapped) — the same
direction-symmetric frames as `kao--hist-apply-group', so every mod's `pos'
is valid when it is folded."
  (let ((n (length group)))
    (dotimes (k n)
      (let* ((i (if (eq dir 'apply) k (- n 1 k)))
             (m (aref group i))
             (del (length (kao-hist-mod-deleted m)))
             (ins (length (kao-hist-mod-inserted m))))
        (setq p (if (eq dir 'apply)
                    (kao-history--translate-1 p (kao-hist-mod-pos m) del ins)
                  (kao-history--translate-1 p (kao-hist-mod-pos m) ins del))))))
  p)

(defun kao-history-translate-position (pos from-id)
  "Translate POS from the buffer content at node FROM-ID to the current node.
The faithful flat-integer `SelectionList::update()' coordinate walk
\(selection.cc:233-274): fold POS through every tree edge on the path from
FROM-ID to the current node — revert edges up to the lowest common ancestor,
apply edges down (`Buffer::move_to''s walk, here over coordinates instead of
content).  Returns POS unchanged when FROM-ID is the current id, or nil when
FROM-ID is nil or was dropped by the `kao-history-max-nodes' gc — the caller
falls back to clamping (documented residual; Kakoune's history is uncapped).
The LCA's own group is never folded, so the gc-advanced root's cleared group
is unreachable from any live pair of nodes."
  (cond
   ((or (null from-id) (null (kao--hist-node from-id))) nil)
   ((= from-id kao--hist-id) pos)
   (t
    (let ((lca (kao--hist-lca from-id kao--hist-id)))
      ;; Revert edges: FROM-ID up to (excluding) the LCA.
      (let ((cur from-id))
        (while (/= cur lca)
          (setq pos (kao-history--group-translate
                     pos (kao-hist-node-group (kao--hist-node cur)) 'revert)
                cur (kao-hist-node-parent (kao--hist-node cur)))))
      ;; Apply edges: LCA down to the current node, parent-first.
      (let ((path nil)
            (cur kao--hist-id))
        (while (/= cur lca)
          (push cur path)
          (setq cur (kao-hist-node-parent (kao--hist-node cur))))
        (dolist (id path)
          (setq pos (kao-history--group-translate
                     pos (kao-hist-node-group (kao--hist-node id)) 'apply))))
      pos))))

(provide 'kao-history)
;;; kao-history.el ends here
