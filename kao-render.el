;;; kao-render.el --- Rendering for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 1.  kao's renderer is a *hybrid*: the MAIN
;; selection uses the real Emacs `point'/`mark' plus the native `region' face, so
;; single-selection editing runs with zero overlay code; secondary selections are
;; drawn only when 2+ exist, via pooled overlays restricted to the visible window.
;;
;; Main-cursor rendering is *on-char* (a deliberate sub-decision): an
;; inclusive Kakoune selection covers chars [min..max], but Emacs's region is the
;; half-open [point, mark) and its block cursor is a display illusion over the
;; char to the right of point.  Setting
;;
;;   point = cursor                 (block lands on the Kakoune cursor char)
;;   mark  = anchor                 (forward) | anchor+1 (backward)
;;
;; makes the native region face plus Emacs's own block cursor cover exactly
;; [min..max], with no overlay for the lone main selection:
;;
;;   forward  a=2 c=5 -> point 5, mark 2 : region [2,5)=chars 2 3 4 + cursor 5
;;   backward a=5 c=2 -> point 2, mark 6 : region [2,6)=chars 2 3 4 5 + cursor 2
;;   single   a=c=3   -> point 3, mark 3 : region empty, just the cursor block

;;; Code:

(require 'cl-lib)
(require 'kao-selection)

(defface kao-selection '((t :inherit secondary-selection))
  "Face for the body of secondary selections.
Defaults to `secondary-selection' so secondary bodies read as visually
distinct from the region-faced main selection (the Kakoune PrimarySelection
vs SecondarySelection distinction, face_registry.cc:182-183; hybrid
keeps the main = native `region').  Set it back to `region' for a uniform look."
  :group 'kao)

(defface kao-cursor '((t :inverse-video t))
  "Face for the cursor character of each secondary selection."
  :group 'kao)

(defface kao-insert-site '((t :inherit kao-cursor))
  "Face for the beacon over each pending secondary insert site.
Drawn during a multi-cursor insert session; inherits
`kao-cursor' by default so the pending sites read like cursors — restyle it to
tell the beacons apart from the real cursor."
  :group 'kao)

(defun kao--main-point-mark (sel)
  "Return (POINT . MARK) buffer positions mirroring main SEL on-char.
POINT is the cursor char; MARK is the anchor, or anchor+1 when SEL is backward,
so the native region face covers the inclusive span [min..max]."
  (let ((cur (kao-sel-cursor sel))
        (anc (kao-sel-anchor sel)))
    (cons cur (if (<= anc cur) anc (min (1+ anc) (point-max))))))

(defun kao--mirror-main (sels)
  "Mirror the main selection of SELS onto point/mark, activating the region.
The region is left active only when it is non-empty (a multi-char selection);
for a single-char selection just the block cursor shows."
  (let* ((main (nth (kao-sels-main sels) (kao-sels-list sels)))
         (pm (kao--main-point-mark main)))
    (set-marker (mark-marker) (cdr pm))
    (goto-char (car pm))
    (setq mark-active (/= (car pm) (cdr pm)))))

;;;; Secondary-selection overlays (pooled, visible-window only)

(defvar-local kao--overlay-pool nil
  "Vector of reusable overlays for secondary selections (nil = empty pool).
Surplus overlays are detached with `delete-overlay' (the object survives) and
re-attached with `move-overlay'; never freed and reallocated per keystroke.
A vector with doubling growth keeps per-overlay access O(1) — the original
list pool paid O(N²) in `append'/`nth' on an N-cursor render.")

(defvar-local kao--overlay-live 0
  "How many pooled overlays are currently attached (the pool's active prefix).
Bounds the surplus-detach loop in `kao--render' so a steady-state render with
no secondaries performs zero overlay operations.")

(defun kao--overlay-pool-ensure (n)
  "Grow the pool to at least N slots (doubling); return the pool vector.
Every slot of the returned vector holds a (possibly detached) overlay."
  (let ((len (length kao--overlay-pool)))
    (when (< len n)
      (let ((new (make-vector (max n (* 2 len) 4) nil)))
        (dotimes (i len) (aset new i (aref kao--overlay-pool i)))
        (cl-loop for i from len below (length new)
                 do (aset new i (let ((ov (make-overlay 1 1)))
                                  (delete-overlay ov)   ; born detached
                                  ov)))
        (setq kao--overlay-pool new))))
  kao--overlay-pool)

(defun kao--window-bounds (&optional window)
  "Return (WBEG . WEND) of WINDOW (default `selected-window'), buffer-clamped.
`window-start'/`window-end' report on WINDOW's buffer, so they are only
meaningful when this buffer is the one WINDOW displays; otherwise (batch, an
undisplayed buffer) fall back to the whole buffer.

This is the per-window helper that `kao--window-ranges' calls per window, and
is RETAINED by name for contract: the frozen bench row stubs it via
`cl-letf' (test/kao-bench.el:127) with a zero-arg replacement, so per-window
bounds must keep flowing through it or the windowed bench silently renders
every secondary."
  (let ((w (or window (selected-window))))
    (if (eq (current-buffer) (window-buffer w))
        (cons (window-start w)
              (min (point-max) (or (window-end w t) (point-max))))
      (cons (point-min) (point-max)))))

(defun kao--window-ranges ()
  "Return the visible ranges of the current buffer, one per window showing it.
Each range is a (WBEG . WEND) cons of `window-start' .. `window-end' (buffer-max
clamped), computed by `kao--window-bounds' per window over the windows from
`get-buffer-window-list' (ALL-FRAMES set to the symbol visible).  Falls back to
a single `kao--window-bounds' range when the buffer is undisplayed (batch: the
window list is empty and the helper returns the whole buffer, or the stubbed
bench viewport).  Never returns nil."
  (let ((windows (get-buffer-window-list nil nil 'visible)))
    (if windows
        (mapcar #'kao--window-bounds windows)
      (list (kao--window-bounds)))))

(defun kao--span-in-ranges-p (beg max ranges)
  "Non-nil if the inclusive span BEG..MAX intersects any range in RANGES.
Each range is a half-open (RBEG . REND) cons; the span meets it when
BEG < REND and MAX >= RBEG (MAX+1 > RBEG for integer positions)."
  (cl-some (lambda (r) (and (< beg (cdr r)) (>= max (car r)))) ranges))

(defun kao--pos-in-ranges-p (pos ranges)
  "Non-nil if POS falls in any half-open (RBEG . REND) range of RANGES."
  (cl-some (lambda (r) (and (>= pos (car r)) (< pos (cdr r)))) ranges))

(defun kao--render (sels &optional wbeg wend)
  "Draw secondary selections of SELS as pooled overlays, visible ranges only.
The ranges default to `kao--window-ranges' (one (WBEG . WEND) per window showing
the buffer -- the union across windows, so the same buffer in two windows shows
every secondary in either viewport).  WBEG/WEND override to a single range (the
legacy 2-bounds form, a single-range convenience used only by tests);
multi-window tests stub `kao--window-ranges' instead.  The main selection is
rendered by `kao--mirror-main', never as an overlay.  Each visible secondary
contributes ONE body overlay over its inclusive span clamped to the range hull
and a cursor overlay over its cursor char."
  (let* ((list (kao-sels-list sels))
         (main-idx (kao-sels-main sels))
         (sorted (kao-sels-sorted sels))      ; LIST ascending by min -> clip walk
         (ranges (if wbeg
                     (list (cons wbeg (or wend (point-max))))
                   (kao--window-ranges)))
         ;; HULL = (min of starts . max of ends).  A single range (every
         ;; 2-bounds caller and the common one-window live case) makes the
         ;; hull the range itself, so MULTI is nil and the per-range test
         ;; below is skipped -- the single-window walk cost is unchanged.
         (hbeg (apply #'min (mapcar #'car ranges)))
         (hend (apply #'max (mapcar #'cdr ranges)))
         (multi (cdr ranges))                       ; 2+ windows on this buffer
         (specs nil)
         (idx 0))
    ;; The walk bails per selection with minimal accessor work (early
    ;; clip) against the HULL: the raw min/max reads replace the
    ;; `kao-sel-beg'/`kao-sel-end' wrappers (each a second nested funcall),
    ;; max is read FIRST so a selection entirely before every viewport — the
    ;; bulk of a big list when the viewports sit late in the buffer — costs
    ;; one read, and the cursor slot is read only for visible selections.
    ;; Breaking out once past the hull is CONDITIONAL on SORTED:
    ;; when the list is known ascending by min, the first selection whose min
    ;; is >= HEND proves every later min is too — none can reach the hull — so
    ;; the walk stops (the huge below-viewport tail of a windowed ~1k-cursor
    ;; list costs nothing).  When SORTED is nil the list may be out of min-order
    ;; — the pairwise combine ops install unmerged lists, and e.g.
    ;; select-longest can pick far-apart spans — so no break is safe and every
    ;; selection is tested, exactly as before.  Only hull-intersecting
    ;; selections pay the per-range visibility test, and only with 2+ windows
    ;; (bounded by window count); the hull alone must NOT be the visibility
    ;; test, else two windows at opposite buffer ends would degrade to
    ;; whole-buffer rendering.  The body overlay is emitted ONCE per selection,
    ;; clamped to the hull (overlay cost is per-object; off-screen middles
    ;; between viewports simply do not display).
    (catch 'kao--render-clipped
      (dolist (sel list)
        (unless (= idx main-idx)
          (let ((m (kao-sel-max sel)))
            (when (>= m hbeg)                        ; end = m+1 > hbeg
              (let ((b (kao-sel-min sel)))
                (if (< b hend)                       ; body intersects hull
                    (when (or (not multi)
                              (kao--span-in-ranges-p b m ranges))
                      (push (list (max b hbeg) (min (1+ m) hend) 'kao-selection 1)
                            specs)
                      (let ((cur (kao-sel-cursor sel)))
                        (when (if multi              ; cursor char on screen
                                  (kao--pos-in-ranges-p cur ranges)
                                (and (>= cur hbeg) (< cur hend)))
                          (push (list cur (1+ cur) 'kao-cursor 100) specs))))
                  ;; b >= hend: this selection starts past the hull.  On a
                  ;; SORTED list every later min is >= b, so none can intersect
                  ;; — stop; otherwise keep testing (a later sel may be visible).
                  (when sorted (throw 'kao--render-clipped nil)))))))
        (setq idx (1+ idx))))
    (setq specs (nreverse specs))
    (let ((i 0))
      (when specs
        (let ((pool (kao--overlay-pool-ensure (length specs))))
          (dolist (spec specs)
            (let ((ov (aref pool i)))
              (move-overlay ov (nth 0 spec) (nth 1 spec))
              (overlay-put ov 'face (nth 2 spec))
              (overlay-put ov 'priority (nth 3 spec)))
            (setq i (1+ i)))))
      ;; Detach only the previously-attached surplus [i, live): with no
      ;; secondaries and an already-idle pool this loop is empty, so the
      ;; N=1 render path performs zero overlay operations.
      (cl-loop for j from i below kao--overlay-live
               do (delete-overlay (aref kao--overlay-pool j)))
      (setq kao--overlay-live i))))

(defun kao--clear-overlays ()
  "Detach every pooled overlay and drop the pool."
  (mapc #'delete-overlay kao--overlay-pool)
  (setq kao--overlay-pool nil
        kao--overlay-live 0))

;;;; Secondary insert-site beacons

(defvar-local kao--insert-beacons nil
  "Overlays marking the pending secondary insert sites.
One `kao-insert-site'-faced overlay per secondary insert mark, attached at
insert entry and detached at exit/abort.  Static: the underlying markers
auto-track typing, so the beacons cost nothing per keystroke — attach once,
detach once, independent of `kao--refresh' (which is gated off during insert).")

(defun kao--insert-beacons-detach ()
  "Delete every secondary insert-site beacon overlay."
  (mapc #'delete-overlay kao--insert-beacons)
  (setq kao--insert-beacons nil))

(defun kao--insert-beacons-attach (marks)
  "Draw a `kao-insert-site' beacon over [m, m+1) for each marker in MARKS.
Any existing beacons are cleared first, so the call is idempotent; an empty or
nil MARKS (a single-selection session) just leaves no beacons attached."
  (kao--insert-beacons-detach)
  (setq kao--insert-beacons
        (mapcar (lambda (m)
                  (let* ((pos (marker-position m))
                         (ov (make-overlay pos (1+ pos))))
                    (overlay-put ov 'face 'kao-insert-site)
                    (overlay-put ov 'priority 100)
                    ov))
                marks)))

;;;; Pulse flash (no Kakoune counterpart)

(defcustom kao-pulse t
  "When non-nil, jump-list and history navigation flash the landing span.
Uses the built-in pulse.el momentary highlight; only when the buffer is
displayed in a window."
  :type 'boolean
  :group 'kao)

(declare-function pulse-momentary-highlight-region "pulse")

(defun kao--pulse-span (beg end)
  "Briefly flash BEG..END via pulse.el (-2 polish).
No-op when `kao-pulse' is nil, the buffer is not displayed (batch included),
or pulse.el is unavailable."
  (when (and kao-pulse
             (get-buffer-window (current-buffer) 'visible)
             (require 'pulse nil t))
    (pulse-momentary-highlight-region beg end)))

(provide 'kao-render)
;;; kao-render.el ends here
