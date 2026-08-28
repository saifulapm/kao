;;; kao-vundo.el --- Opt-in visual viewer for kao's history tree -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A vundo/undo-tree-style VISUAL TREE VIEWER for kao's OWN buffer-history tree
;; (`kao--hist'), with keyboard navigation that drives kao's integrated
;; history machinery so selections, overlays and the change-id position stay
;; coherent.  See
;;
;; Why not vundo / undo-tree?  Both visualize and navigate the native
;; `buffer-undo-list' via `primitive-undo', which kao keeps dormant and
;; suppresses during its own replay — so they desync kao's state in a kao
;; buffer and are blocked there (`kao-block-foreign-undo-visualizers', kao.el).
;; kao-vundo is the faithful replacement: it renders kao's real tree and
;; navigates by `kao-history-goto', the same validated entry the `<c-j>'/`<c-k>'
;; keys use.  No `buffer-undo-list' parsing — kao hands us a real tree.
;;
;; This file is OPT-IN: `kao.el' does NOT require it (mirror `kao-objects.el').
;; Enable it from your config:
;;
;;   (require 'kao-vundo)
;;   ;; then `M-x kao-vundo' in a kao buffer, or bind it on the user map, e.g.
;;   ;; (define-key kao-user-map "v" #'kao-vundo)   ; SPC v
;;   ;; (do NOT bind `U' — it is `kao-redo'.)
;;
;; In the `*kao-vundo*' view: f / b move to the newer (redo-child) / older
;; (parent) change, n / p switch sibling branches, RET goes to the node at
;; point, g refreshes, q quits.  Every move drives the origin buffer exactly
;; like `u'/`U'/`<c-j>'/`<c-k>'.  Browse without changing history with C-n / C-p
;; (move point between nodes), then d shows that node's change or RET jumps to it.

;;; Code:

(require 'cl-lib)
(require 'kao-history)                  ; the tree + the §5 public accessors
(require 'kao-edit)                     ; `kao-history-goto'
(require 'kao-state)                    ; the `kao-mode' symbol

;;;; Faces

(defface kao-vundo-node '((t :inherit default))
  "Face for a non-current node glyph in the kao-vundo tree."
  :group 'kao)

(defface kao-vundo-current '((t :inherit warning :weight bold))
  "Face for the current node glyph in the kao-vundo tree."
  :group 'kao)

(defface kao-vundo-label '((t :inherit shadow))
  "Face for a node's id/size label in the kao-vundo tree."
  :group 'kao)

;;;; Layout (pure)

(cl-defstruct (kao-vundo-row (:constructor kao-vundo-row--make))
  "One display row of the kao-vundo tree: a node and its connector context.
ID is the HistoryId; DEPTH is the depth from the root (0 = root); BARS is the
list (root-side first) of ancestor columns that still continue below this row
\(draw a vertical bar); LAST is non-nil when the node is the last child of its
parent (the root counts as last)."
  id depth bars last)

(defun kao-vundo--layout (root)
  "Return the display rows for the history tree rooted at ROOT.
A list of `kao-vundo-row' in top-to-bottom display order: DFS pre-order with
children ascending by id (stable, so the view does not jump around).  Pure —
reads only the public topology accessors of the current buffer, performs no
buffer edits, and needs no display.  Returns nil when ROOT does not exist."
  (let ((rows '()))
    (cl-labels
        ((walk (id depth bars last)
           (push (kao-vundo-row--make :id id :depth depth :bars bars :last last)
                 rows)
           (let* ((children (sort (copy-sequence (kao-history-node-children id))
                                  #'<))
                  (n (length children))
                  ;; A child's ancestor columns are mine plus my own connector
                  ;; column, which becomes a vertical bar exactly when I have a
                  ;; sibling below me (I am not the last child).  The root has
                  ;; no connector column, so it contributes none.
                  (child-bars (if (zerop depth)
                                  bars
                                (append bars (list (not last)))))
                  (i 0))
             (dolist (c children)
               (walk c (1+ depth) child-bars (= i (1- n)))
               (setq i (1+ i))))))
      (when (kao-history-node-exists-p root)
        (walk root 0 '() t)))
    (nreverse rows)))

(defun kao-vundo--row-prefix (row)
  "Return the box-drawing prefix string for ROW, before its node glyph.
Each ancestor column is a vertical bar or a blank gap; the node's own column is
a branch (`├') or last-branch (`└') connector.  The root has no connector."
  (let ((s ""))
    (dolist (bar (kao-vundo-row-bars row))
      (setq s (concat s (if bar "│  " "   "))))
    (if (zerop (kao-vundo-row-depth row))
        s
      (concat s (if (kao-vundo-row-last row) "└─ " "├─ ")))))

;;;; Render

(defvar-local kao-vundo--origin nil
  "The kao buffer whose history this `*kao-vundo*' view drives.")

(defun kao-vundo--origin-or-error ()
  "Return the live origin buffer, or signal a clean `user-error' if it is gone.
Every viewer command reads the origin through this so a killed origin yields a
friendly message instead of a raw `Selecting deleted buffer' (kao's
dead-buffer precedent)."
  (if (buffer-live-p kao-vundo--origin)
      kao-vundo--origin
    (user-error "kao-vundo: origin buffer is gone — q to quit")))

(defun kao-vundo--render (rows current summaries)
  "Draw ROWS into the current buffer and return CURRENT's glyph position.
CURRENT is the current HistoryId (highlighted); SUMMARIES is an alist of
\(ID . SUMMARY-STRING).  Each line is tagged with its node id (text property
`kao-vundo-id') for a point->id reverse lookup."
  (let ((inhibit-read-only t)
        (target nil))
    (erase-buffer)
    (dolist (row rows)
      (let* ((id (kao-vundo-row-id row))
             (curp (eql id current))
             (summary (cdr (assq id summaries)))
             (label (format "#%d%s" id (if summary (concat "  " summary) "")))
             (beg (point)))
        (insert (kao-vundo--row-prefix row))
        (when curp (setq target (point)))
        (insert (propertize (if curp "●" "○")
                            'face (if curp 'kao-vundo-current 'kao-vundo-node)))
        (insert " " (propertize label 'face 'kao-vundo-label))
        (put-text-property beg (point) 'kao-vundo-id id)
        (insert "\n")))
    target))

(defun kao-vundo--refresh ()
  "Rebuild the `*kao-vundo*' view from the origin buffer's current history tree.
Place point on the current node's glyph.  Signals a clean `user-error' when the
origin has been killed.  Also re-subscribes the origin's live-refresh hook when
it is missing — a non-preserve-modes revert runs `kill-all-local-variables',
dropping the buffer-local subscription, so `g' recovers live refresh
\."
  (let* ((origin (kao-vundo--origin-or-error))
         rows current summaries)
    (with-current-buffer origin
      (add-hook 'kao-history-change-hook #'kao-vundo--on-origin-change nil t)
      (setq current (kao-history-current-id)
            rows (kao-vundo--layout (kao-history-root-id))
            summaries (mapcar (lambda (r)
                                (let ((id (kao-vundo-row-id r)))
                                  (cons id (kao-history-node-summary id))))
                              rows)))
    (goto-char (or (kao-vundo--render rows current summaries) (point-min)))))

;;;; Navigation (drives the origin buffer via `kao-history-goto')

(defun kao-vundo--navigate-origin (id)
  "Drive the origin buffer's history to node ID via `kao-history-goto'.
Run in the origin window when it is live so its display point follows the new
selection; surface a read-only or out-of-range error as a message instead of
letting it escape the viewer."
  (let* ((origin (kao-vundo--origin-or-error))
         (win (get-buffer-window origin)))
    (condition-case err
        (if (window-live-p win)
            (with-selected-window win (kao-history-goto id))
          (with-current-buffer origin (kao-history-goto id)))
      (error (message "kao-vundo: %s" (error-message-string err))))))

(defun kao-vundo--go (target what)
  "Move the origin to history node TARGET, then re-render; message WHAT if nil."
  (if (null target)
      (message "kao-vundo: no %s" what)
    (kao-vundo--navigate-origin target)
    (kao-vundo--refresh)))

(defun kao-vundo-goto-parent ()
  "Move the origin buffer to the parent (older) change."
  (interactive)
  (kao-vundo--go (with-current-buffer (kao-vundo--origin-or-error)
                   (kao-history-node-parent (kao-history-current-id)))
                 "older change"))

(defun kao-vundo-goto-redo-child ()
  "Move the origin buffer to the redo-child (newer) change."
  (interactive)
  (kao-vundo--go (with-current-buffer (kao-vundo--origin-or-error)
                   (kao-history-node-redo-child (kao-history-current-id)))
                 "newer change"))

(defun kao-vundo--sibling-target (dir)
  "Return the id of the DIR (+1 next / -1 previous) sibling, or nil."
  (with-current-buffer (kao-vundo--origin-or-error)
    (let* ((cur (kao-history-current-id))
           (parent (kao-history-node-parent cur)))
      (when parent
        (let* ((sibs (sort (copy-sequence (kao-history-node-children parent)) #'<))
               (pos (cl-position cur sibs)))
          (when pos
            (let ((i (+ pos dir)))
              (and (>= i 0) (< i (length sibs)) (nth i sibs)))))))))

(defun kao-vundo-goto-next-sibling ()
  "Switch the origin buffer to the next sibling branch of the current change."
  (interactive)
  (kao-vundo--go (kao-vundo--sibling-target 1) "next sibling"))

(defun kao-vundo-goto-prev-sibling ()
  "Switch the origin buffer to the previous sibling branch of the current change."
  (interactive)
  (kao-vundo--go (kao-vundo--sibling-target -1) "previous sibling"))

(defun kao-vundo-goto-point ()
  "Move the origin buffer to the history node at point in the view."
  (interactive)
  (let ((id (get-text-property (point) 'kao-vundo-id)))
    (if (null id)
        (message "kao-vundo: no node here")
      (kao-vundo--go id "node"))))

(defun kao-vundo-refresh ()
  "Rebuild the view from the origin buffer's current history."
  (interactive)
  (kao-vundo--refresh))

(defun kao-vundo-show-diff ()
  "Show the change made by the history node at point in `*kao-vundo-diff*'.
Each primitive change of the node prints as a `- DELETED' and/or `+ INSERTED'
line (empty sides omitted).  The root (and any group-less node) has no change."
  (interactive)
  (let ((id (get-text-property (point) 'kao-vundo-id)))
    (cond
     ((null id) (message "kao-vundo: no node here"))
     (t
      (let ((changes (with-current-buffer (kao-vundo--origin-or-error)
                       (kao-history-node-changes id))))
        (if (null changes)
            (message "kao-vundo: #%d has no change" id)
          (with-output-to-temp-buffer "*kao-vundo-diff*"
            (princ (format "Change #%d\n\n" id))
            (dolist (c changes)
              (let ((del (nth 1 c)) (ins (nth 2 c)))
                (when (> (length del) 0) (princ (format "- %s\n" del)))
                (when (> (length ins) 0) (princ (format "+ %s\n" ins))))))))))))

;;;; Mode + command

(defvar kao-vundo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f"            #'kao-vundo-goto-redo-child)
    (define-key map (kbd "<right>") #'kao-vundo-goto-redo-child)
    (define-key map "b"            #'kao-vundo-goto-parent)
    (define-key map (kbd "<left>") #'kao-vundo-goto-parent)
    (define-key map "n"            #'kao-vundo-goto-next-sibling)
    (define-key map (kbd "<down>") #'kao-vundo-goto-next-sibling)
    (define-key map "p"            #'kao-vundo-goto-prev-sibling)
    (define-key map (kbd "<up>")   #'kao-vundo-goto-prev-sibling)
    (define-key map (kbd "RET")    #'kao-vundo-goto-point)
    (define-key map "d"            #'kao-vundo-show-diff)
    (define-key map "g"            #'kao-vundo-refresh)
    (define-key map "q"            #'quit-window)
    map)
  "Keymap for `kao-vundo-mode'.")

(define-derived-mode kao-vundo-mode special-mode "Kao-Vundo"
  "Major mode for the kao history-tree viewer (`kao-vundo').
Navigating the tree drives the origin buffer's history exactly like
`u'/`U'/`<c-j>'/`<c-k>'.  Derives from `special-mode' (read-only, and kept out
of `kao-global-mode', whose predicate excludes `special-mode')."
  (setq-local truncate-lines t))

;;;; Live refresh — the open view tracks the origin's history changes

(defun kao-vundo--on-origin-change ()
  "Live-refresh the `*kao-vundo*' view when its origin's history changes.
Runs in the origin buffer via that buffer's local `kao-history-change-hook'
\(installed by `kao-vundo' while the view is open).  A refresh failure is
demoted to a message so a viewer bug never breaks the user's edit command."
  (let ((view (get-buffer "*kao-vundo*")))
    (when (and view
               (eq (buffer-local-value 'kao-vundo--origin view) (current-buffer)))
      (with-demoted-errors "kao-vundo: live refresh failed: %S"
        (with-current-buffer view (kao-vundo--refresh))))))

(defun kao-vundo--teardown ()
  "Detach the change hook from the origin when the view buffer is killed.
A buffer-local `kill-buffer-hook' on `*kao-vundo*'."
  (when (buffer-live-p kao-vundo--origin)
    (with-current-buffer kao-vundo--origin
      (remove-hook 'kao-history-change-hook #'kao-vundo--on-origin-change t))))

;;;###autoload
(defun kao-vundo ()
  "Visualize the kao buffer-history tree and navigate it in a side window.
Open `*kao-vundo*' showing the current buffer's history tree (`kao--hist')
with the current node highlighted.  In the view: f / b move to the newer
\(redo-child) / older (parent) change, n / p switch sibling branches, RET goes
to the node at point, d shows that node's change, g refreshes, q quits — every
move drives the origin buffer through kao's history exactly like
`u'/`U'/`<c-j>'/`<c-k>' (same content, selection and overlays), via
`kao-history-goto'.  `<c-n>'/`<c-p>' browse the tree WITHOUT changing history
\(move point between nodes), then d shows that node's change or RET jumps to it.

Signal an error unless the current buffer is a kao buffer with a history tree."
  (interactive)
  (kao--assert-mode "kao-vundo")         ; shared mode-off guard, named
  (unless (kao-history-node-exists-p (kao-history-root-id))
    (user-error "kao-vundo: no history tree"))
  (let ((origin (current-buffer))
        (view (get-buffer-create "*kao-vundo*")))
    (with-current-buffer view
      (unless (derived-mode-p 'kao-vundo-mode)
        (kao-vundo-mode))
      ;; re-point: detach the live-refresh hook from a previous, different origin
      (when (and (buffer-live-p kao-vundo--origin)
                 (not (eq kao-vundo--origin origin)))
        (with-current-buffer kao-vundo--origin
          (remove-hook 'kao-history-change-hook #'kao-vundo--on-origin-change t)))
      (setq kao-vundo--origin origin)
      (add-hook 'kill-buffer-hook #'kao-vundo--teardown nil t)
      (kao-vundo--refresh))
    ;; subscribe the view to the origin's history changes (buffer-local, idempotent)
    (with-current-buffer origin
      (add-hook 'kao-history-change-hook #'kao-vundo--on-origin-change nil t))
    (let ((win (display-buffer
                view
                '((display-buffer-in-side-window)
                  (side . bottom)
                  (slot . 0)
                  ;; fit to the tree, but keep a small floor so a one- or
                  ;; two-node tree is not a single-line sliver.
                  (window-height . (lambda (w) (fit-window-to-buffer w nil 4)))))))
      (when (window-live-p win)
        (select-window win)))))

(provide 'kao-vundo)
;;; kao-vundo.el ends here
