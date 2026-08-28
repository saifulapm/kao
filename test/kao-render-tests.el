;;; kao-render-tests.el --- Tests for kao-render -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the kao renderer.  These are buffer-based (unlike the pure
;; selection-algebra tests).  This file covers the main-selection point/mark
;; mirror (on-char rule); secondary-overlay tests live alongside.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-state)                     ; insert-session seams drive the beacons

(defmacro kao-render-tests--with (content &rest body)
  "Run BODY in a temp buffer containing CONTENT with point at `point-min'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(defun kao-render-tests--mirror (anchor cursor)
  "Mirror a single selection ANCHOR/CURSOR as the main selection."
  (kao--mirror-main
   (kao-sels-make :list (list (kao-sel-make :anchor anchor :cursor cursor))
                  :main 0)))

(defun kao-render-tests--mark ()
  "Return the mark position without requiring the region to be active."
  (marker-position (mark-marker)))

;;;; Face defaults (Kakoune Primary vs Secondary distinction)

(ert-deftest kao-render-selection-face-inherits-secondary-selection ()
  "The `kao-selection' defface default inherits `secondary-selection', not
`region', so secondary bodies read as visually distinct from the region-faced
main selection.  This is the Kakoune PrimarySelection-vs-SecondarySelection
distinction (face_registry.cc:182-183) the `)'/`(' main-cycling workflow
depends on; hybrid keeps the main = native `region'."
  (let* ((spec (get 'kao-selection 'face-defface-spec))
         (inherit (plist-get (cdr (assq t spec)) :inherit)))
    (should (eq inherit 'secondary-selection))
    (should-not (eq inherit 'region))))

(ert-deftest kao-render-insert-site-face-inherits-cursor ()
  "The `kao-insert-site' beacon face inherits `kao-cursor' by default.
So the pending secondary insert sites read like cursors; a
user can restyle `kao-insert-site' to tell the beacons from the real cursor."
  (let* ((spec (get 'kao-insert-site 'face-defface-spec))
         (inherit (plist-get (cdr (assq t spec)) :inherit)))
    (should (eq inherit 'kao-cursor))))

;;;; Secondary insert-site beacons

(defun kao-render-tests--insert-site-beacons ()
  "Return the live `kao-insert-site' beacon overlays in the buffer."
  (seq-filter (lambda (o) (eq (overlay-get o 'face) 'kao-insert-site))
              (overlays-in (point-min) (point-max))))

(ert-deftest kao-render-insert-beacons-mark-each-secondary-site ()
  "A multi-site insert session draws one [m, m+1) beacon per secondary mark."
  (kao-render-tests--with "abcdef"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)
                                       (kao-sel-make :anchor 3 :cursor 3)
                                       (kao-sel-make :anchor 5 :cursor 5))
                           :main 0))
          ;; main = sel 0 (cursor 1); secondaries at cursors 3 and 5.
          (kao--enter-insert 'insert (lambda () (goto-char 1)) #'kao-sel-cursor)
          (should (= 2 (length kao--insert-beacons)))
          (let ((beacons (kao-render-tests--insert-site-beacons)))
            (should (= 2 (length beacons)))
            (dolist (o beacons)                       ; each spans exactly one char
              (should (= 1 (- (overlay-end o) (overlay-start o)))))
            (should (equal '(3 5)
                           (sort (mapcar #'overlay-start beacons) #'<)))))
      (kao-mode -1))))

(ert-deftest kao-render-insert-beacons-detach-on-exit ()
  "`kao-insert-exit' tears down every beacon — no dangling overlays."
  (kao-render-tests--with "abcdef"
    (buffer-enable-undo)
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)
                                       (kao-sel-make :anchor 3 :cursor 3)
                                       (kao-sel-make :anchor 5 :cursor 5))
                           :main 0))
          (kao--enter-insert 'insert (lambda () (goto-char 1)) #'kao-sel-cursor)
          (should (= 2 (length kao--insert-beacons)))
          (kao-insert-exit)
          (should (null kao--insert-beacons))
          (should (null (kao-render-tests--insert-site-beacons))))
      (kao-mode -1))))

(ert-deftest kao-render-insert-beacons-detach-on-aborted-setup ()
  "A setup error aborts the half-open session and tears down every beacon."
  (kao-render-tests--with "abcdef"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)
                                       (kao-sel-make :anchor 3 :cursor 3)
                                       (kao-sel-make :anchor 5 :cursor 5))
                           :main 0))
          ;; The position-fn signals AFTER the marks (and beacons) are set.
          (should-error
           (kao--enter-insert 'insert (lambda () (error "boom")) #'kao-sel-cursor))
          (should-not kao--insert-active)
          (should (null kao--insert-beacons))
          (should (null (kao-render-tests--insert-site-beacons))))
      (kao-mode -1))))

(ert-deftest kao-render-insert-beacons-detach-on-disable-mid-insert ()
  "Disabling kao-mode mid-session tears down every beacon — no dangling overlays."
  (kao-render-tests--with "abcdef"
    (kao-mode 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 3 :cursor 3)
                                 (kao-sel-make :anchor 5 :cursor 5))
                     :main 0))
    (kao--enter-insert 'insert (lambda () (goto-char 1)) #'kao-sel-cursor)
    (should (= 2 (length kao--insert-beacons)))
    (kao-mode -1)                                 ; disable while still inserting
    (should (null kao--insert-beacons))
    (should (null (kao-render-tests--insert-site-beacons)))))

;;;; Main-selection mirror (on-char)

(ert-deftest kao-render-mirror-forward ()
  "Forward main: point on cursor char, mark on anchor, region covers [min,max)."
  (kao-render-tests--with "xabcy\n"      ; x=1 a=2 b=3 c=4 y=5 \n=6
    (kao-render-tests--mirror 2 4)       ; covers a,b,c
    (should (= (point) 4))
    (should (= (kao-render-tests--mark) 2))
    (should mark-active)
    (should (= (region-beginning) 2))
    (should (= (region-end) 4))          ; body region; cursor shows char c
    (should (= (char-after (point)) ?c))))

(ert-deftest kao-render-mirror-backward ()
  "Backward main: point on cursor char (min), mark = anchor+1; region=[min,max+1)."
  (kao-render-tests--with "xabcy\n"
    (kao-render-tests--mirror 5 2)       ; covers a,b,c,y, cursor on a
    (should (= (point) 2))
    (should (= (kao-render-tests--mark) 6))
    (should mark-active)
    (should (= (region-beginning) 2))
    (should (= (region-end) 6))
    (should (= (char-after (point)) ?a))))

(ert-deftest kao-render-mirror-single ()
  "Single-char main: point=mark, region inactive, just the cursor block."
  (kao-render-tests--with "xabcy\n"
    (kao-render-tests--mirror 3 3)
    (should (= (point) 3))
    (should (= (kao-render-tests--mark) 3))
    (should-not mark-active)
    (should (= (char-after (point)) ?b))))

(ert-deftest kao-render-mirror-on-newline ()
  "Cursor on the trailing newline is a valid on-char position."
  (kao-render-tests--with "ab\n"         ; a=1 b=2 \n=3
    (kao-render-tests--mirror 1 3)       ; covers a,b,\n
    (should (= (point) 3))
    (should (= (kao-render-tests--mark) 1))
    (should (= (region-beginning) 1))
    (should (= (region-end) 3))
    (should (= (char-after (point)) ?\n))))

(ert-deftest kao-render-mirror-backward-eob-clamps-mark ()
  "Backward selection whose anchor is the last char clamps mark to point-max."
  (kao-render-tests--with "ab"           ; no trailing newline; point-max = 3
    (kao-render-tests--mirror 2 1)       ; backward, anchor=2 ('b'), cursor=1 ('a')
    (should (= (point) 1))
    (should (= (kao-render-tests--mark) 3))   ; anchor+1 = 3 = point-max (clamped)
    (should (= (region-beginning) 1))
    (should (= (region-end) 3))))

(ert-deftest kao-render-mirror-empty-buffer ()
  "Empty buffer: degenerate single selection at point-min, region inactive."
  (kao-render-tests--with ""
    (kao-render-tests--mirror 1 1)
    (should (= (point) 1))
    (should (= (kao-render-tests--mark) 1))
    (should-not mark-active)))

;;;; Secondary-selection overlays (pooled, visible-window only)

(defun kao-render-tests--sels ()
  "Three selections in a \"0123456789\" buffer: main at `0', secondaries `23',`6'."
  (kao-sels-make
   :list (list (kao-sel-make :anchor 1 :cursor 1)    ; main, pos 1 = ?0
               (kao-sel-make :anchor 3 :cursor 4)    ; sec, pos 3..4 = ?2 ?3
               (kao-sel-make :anchor 7 :cursor 7))   ; sec, pos 7 = ?6
   :main 0))

(defun kao-render-tests--attached ()
  "Return the list of currently attached pooled overlays."
  (seq-filter #'overlay-buffer kao--overlay-pool))

(defmacro kao-render-tests--with-ranges (ranges &rest body)
  "Run BODY with `kao--window-ranges' stubbed to return RANGES.
This is the generalized visible-range override: tests inject the window ranges
`kao--render' clips to (one or many (WBEG . WEND) conses) instead of passing the
legacy WBEG/WEND 2-bounds parameters.  The 2-bounds form of `kao--render' still
delegates to a single range -- exercised directly by the tests, with no live
caller depending on it; see `kao-render-wbeg-wend-delegates-to-single-range'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'kao--window-ranges) (lambda () ,ranges)))
     ,@body))

(ert-deftest kao-render-single-selection-no-overlays ()
  "A lone main selection draws no overlays."
  (kao-render-tests--with "0123456789\n"
    (kao--render (kao-sels-make :list (list (kao-sel-make :anchor 1 :cursor 1))
                                :main 0)
                 (point-min) (point-max))
    (should (null (kao-render-tests--attached)))
    (should (= 0 (length (overlays-in (point-min) (point-max)))))))

(ert-deftest kao-render-secondaries-body-and-cursor ()
  "Each visible secondary draws a body + a cursor overlay; main is not drawn."
  (kao-render-tests--with "0123456789\n"
    (kao--render (kao-render-tests--sels) (point-min) (point-max))
    (let ((attached (kao-render-tests--attached)))
      (should (= 4 (length attached)))
      (should (= 2 (seq-count (lambda (o) (eq (overlay-get o 'face) 'kao-selection))
                              attached)))
      (should (= 2 (seq-count (lambda (o) (eq (overlay-get o 'face) 'kao-cursor))
                              attached))))
    ;; the main char (pos 1) is untouched by overlays
    (should (null (overlays-at 1)))
    ;; sec1 body spans 3..5, its cursor-char overlay spans 4..5
    (should (cl-find-if (lambda (o) (and (eq (overlay-get o 'face) 'kao-selection)
                                         (= (overlay-start o) 3) (= (overlay-end o) 5)))
                        kao--overlay-pool))
    (should (cl-find-if (lambda (o) (and (eq (overlay-get o 'face) 'kao-cursor)
                                         (= (overlay-start o) 4) (= (overlay-end o) 5)))
                        kao--overlay-pool))))

(ert-deftest kao-render-off-window-not-drawn ()
  "A secondary outside the visible range is not drawn (via the ranges override)."
  (kao-render-tests--with "0123456789\n"
    (kao-render-tests--with-ranges (list (cons 1 6))  ; excludes the pos-7 secondary
      (kao--render (kao-render-tests--sels)))
    (let ((attached (kao-render-tests--attached)))
      (should (= 2 (length attached)))            ; only sec1 (body + cursor)
      (should (null (cl-find-if (lambda (o) (= (overlay-start o) 7)) attached))))))

(ert-deftest kao-render-unsorted-list-clips-correctly ()
  "An UNSORTED secondary list renders exactly the window-intersecting sels.
The pairwise combine ops install unmerged — and possibly min-unsorted —
lists, so the render walk must test every selection rather than early-break
once past the window (a sorted-assumption regression would drop the late
in-window selection here)."
  (kao-render-tests--with "0123456789\n0123456789\n0123456789\n"
    ;; mins out of order: 14, 3, 25; window [1,12) shows only the pos-3 one.
    (kao-render-tests--with-ranges (list (cons 1 12))
      (kao--render (kao-sels-make
                    :list (list (kao-sel-make :anchor 1 :cursor 1)    ; main
                                (kao-sel-make :anchor 14 :cursor 15)  ; off-window
                                (kao-sel-make :anchor 3 :cursor 4)    ; IN window
                                (kao-sel-make :anchor 25 :cursor 25)) ; off-window
                    :main 0)))
    (let ((attached (kao-render-tests--attached)))
      (should (= 2 (length attached)))                    ; body + cursor
      (should (cl-find-if (lambda (o) (and (eq (overlay-get o 'face) 'kao-selection)
                                           (= (overlay-start o) 3)
                                           (= (overlay-end o) 5)))
                          attached))
      (should (cl-find-if (lambda (o) (and (eq (overlay-get o 'face) 'kao-cursor)
                                           (= (overlay-start o) 4)))
                          attached))
      (should (null (cl-find-if (lambda (o) (>= (overlay-start o) 14))
                                attached))))))

(ert-deftest kao-render-pool-reused-not-reallocated ()
  "Re-rendering with fewer secondaries reuses objects and detaches surplus."
  (kao-render-tests--with "0123456789\n"
    (kao--render (kao-render-tests--sels) (point-min) (point-max))
    (let ((pool-before (copy-sequence kao--overlay-pool))
          (len-before (length kao--overlay-pool)))
      (should (= 4 len-before))
      (kao--render (kao-sels-make
                    :list (list (kao-sel-make :anchor 1 :cursor 1)
                                (kao-sel-make :anchor 3 :cursor 4))
                    :main 0)
                   (point-min) (point-max))
      (should (= len-before (length kao--overlay-pool)))     ; pool did not shrink
      (should (eq (aref pool-before 0) (aref kao--overlay-pool 0)))  ; same objects
      (should (eq (aref pool-before 1) (aref kao--overlay-pool 1)))
      (should (= 2 (length (kao-render-tests--attached))))   ; surplus detached
      (should (null (overlay-buffer (aref kao--overlay-pool 2))))
      (should (null (overlay-buffer (aref kao--overlay-pool 3)))))))

;;;; Multi-window rendering (union of the windows showing the buffer)

(ert-deftest kao-render-two-windows-both-viewports-drawn ()
  "Two disjoint viewports on one buffer: secondaries visible in EITHER window are
drawn; a selection in the gap between the viewports is not.  This is the
fix — render clips to the union of window ranges, not one window's viewport."
  (kao-render-tests--with "0123456789\n0123456789\n0123456789"  ; 1-11 | 12-22 | 23-32
    ;; window A shows line 1 (1..12); window B shows line 3 (23..33); line 2 = gap.
    (kao-render-tests--with-ranges (list (cons 1 12) (cons 23 33))
      (kao--render (kao-sels-make
                    :list (list (kao-sel-make :anchor 1 :cursor 1)    ; main
                                (kao-sel-make :anchor 3 :cursor 4)    ; window A
                                (kao-sel-make :anchor 14 :cursor 15)  ; the GAP (line 2)
                                (kao-sel-make :anchor 25 :cursor 26)) ; window B
                    :main 0)))
    (let ((attached (kao-render-tests--attached)))
      (should (= 4 (length attached)))          ; 2 bodies + 2 cursors; gap sel skipped
      ;; window A's secondary: body 3..5
      (should (cl-find-if (lambda (o) (and (eq (overlay-get o 'face) 'kao-selection)
                                           (= (overlay-start o) 3) (= (overlay-end o) 5)))
                          attached))
      ;; window B's secondary: body 25..27
      (should (cl-find-if (lambda (o) (and (eq (overlay-get o 'face) 'kao-selection)
                                           (= (overlay-start o) 25) (= (overlay-end o) 27)))
                          attached))
      ;; nothing rendered in the gap between the two viewports (line 2)
      (should (null (overlays-at 14)))
      (should (null (cl-find-if (lambda (o) (and (>= (overlay-start o) 12)
                                                 (< (overlay-start o) 23)))
                                attached))))))

(ert-deftest kao-render-other-window-keeps-first-viewport ()
  "Switching the selected window (C-x o) does not detach the other viewport's
overlays: the render takes the UNION of every window showing the buffer, so a
secondary visible only in the first window stays drawn once the second window is
selected (the pre-fix render re-clipped to the selected window)."
  (let ((buf (generate-new-buffer " kao-render-2win"))
        (config (current-window-configuration)))
    (unwind-protect
        (with-current-buffer buf
          (insert (mapconcat #'identity (make-list 6 "0123456789") "\n"))
          (let* ((w1 (frame-selected-window))
                 (w2 (split-window w1)))
            (set-window-buffer w1 buf)
            (set-window-buffer w2 buf)
            ;; Deterministic per-window bounds (batch `window-end' is unreliable):
            ;; w1 shows the head [1,12), w2 a later slice [20, point-max).  The
            ;; no-window-arg call mirrors the pre-fix selected-window behaviour.
            (cl-letf (((symbol-function 'kao--window-bounds)
                       (lambda (&optional w)
                         (let ((win (or w (selected-window))))
                           (cond ((eq win w1) (cons 1 12))
                                 ((eq win w2) (cons 20 (point-max)))
                                 (t (cons (point-min) (point-max))))))))
              (let ((sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)   ; main
                                       (kao-sel-make :anchor 3 :cursor 3)   ; only in w1
                                       (kao-sel-make :anchor 25 :cursor 25)); only in w2
                           :main 0)))
                (select-window w1)
                (kao--render sels)
                (should (overlays-at 3))    ; w1's secondary drawn
                (should (overlays-at 25))   ; w2's secondary drawn too (union)
                ;; C-x o: select the OTHER window and re-render
                (select-window w2)
                (kao--render sels)
                (should (overlays-at 3))    ; STILL drawn — not detached
                (should (overlays-at 25))))))
      (set-window-configuration config)
      (kill-buffer buf))))

(ert-deftest kao-render-undisplayed-buffer-renders-fallback ()
  "An undisplayed buffer (batch — no window) still renders its secondaries: with
no window ranges the render falls back to the whole buffer.  keeps the
whole-buffer fallback for genuinely undisplayed buffers only."
  (kao-render-tests--with "0123456789\n"
    (kao--render (kao-render-tests--sels))    ; no bounds; the temp buffer is undisplayed
    (let ((attached (kao-render-tests--attached)))
      (should (= 4 (length attached)))        ; both secondaries: 2 bodies + 2 cursors
      (should (overlays-at 3))                ; sec1 (pos 3) drawn
      (should (overlays-at 7)))))             ; sec2 (pos 7) drawn — whole buffer visible

(ert-deftest kao-window-ranges-undisplayed-is-whole-buffer ()
  "In batch (no window showing the buffer) `kao--window-ranges' returns a single
whole-buffer range and never nil."
  (kao-render-tests--with "0123456789\n"
    (let ((ranges (kao--window-ranges)))
      (should ranges)                         ; never nil
      (should (= 1 (length ranges)))
      (should (equal (car ranges) (cons (point-min) (point-max)))))))

(ert-deftest kao-window-ranges-two-windows-one-range-each ()
  "With the buffer shown in two windows `kao--window-ranges' returns one
(WBEG . WEND) range per window — the union input the renderer clips against."
  (let ((buf (generate-new-buffer " kao-render-ranges"))
        (config (current-window-configuration)))
    (unwind-protect
        (with-current-buffer buf
          (insert (mapconcat #'identity (make-list 6 "0123456789") "\n"))
          (let* ((w1 (frame-selected-window))
                 (w2 (split-window w1)))
            (set-window-buffer w1 buf)
            (set-window-buffer w2 buf)
            (let ((ranges (kao--window-ranges)))
              (should (= 2 (length ranges)))
              (should (cl-every (lambda (r) (and (consp r)
                                                 (integerp (car r)) (integerp (cdr r))
                                                 (<= (car r) (cdr r))))
                                ranges)))))
      (set-window-configuration config)
      (kill-buffer buf))))

(ert-deftest kao-render-wbeg-wend-delegates-to-single-range ()
  "The legacy 2-bounds WBEG/WEND form still clips to a single range, producing the
same overlays as a one-range `kao--window-ranges' override.  The tests exercise
this single-range form directly; no live caller depends on it."
  (kao-render-tests--with "0123456789\n"
    (cl-flet ((snapshot ()
                (sort (mapcar (lambda (o) (list (overlay-start o) (overlay-end o)
                                                (overlay-get o 'face)))
                              (kao-render-tests--attached))
                      (lambda (a b) (< (car a) (car b))))))
      (kao--render (kao-render-tests--sels) 1 6)       ; legacy 2-bounds override
      (let ((legacy (snapshot)))
        (should legacy)                                ; sanity: something drew
        (kao--clear-overlays)
        (kao-render-tests--with-ranges (list (cons 1 6)) ; same span via ranges override
          (kao--render (kao-render-tests--sels)))
        (should (equal legacy (snapshot)))))))

;;;; Sorted-flag clipped walk == full walk (behaviour identity)

(defun kao-render-tests--spec-snapshot ()
  "Return sorted (START END FACE) triples of the currently attached overlays."
  (sort (mapcar (lambda (o) (list (overlay-start o) (overlay-end o)
                                  (overlay-get o 'face)))
                (kao-render-tests--attached))
        (lambda (a b) (or (< (car a) (car b))
                          (and (= (car a) (car b)) (< (nth 1 a) (nth 1 b)))))))

(defun kao-render-tests--assert-sorted-identity (list main ranges)
  "Assert a min-sorted LIST (with MAIN) renders identically `sorted' t vs nil.
The t form takes the clipped secondary walk (early-break past the hull); the
nil form takes the full walk.  Both must emit the same overlay specs — the clip
is behaviour-preserving.  Returns the shared spec snapshot.  RANGES is stubbed
into `kao--window-ranges', so this drives the same clip path as live."
  (kao--clear-overlays)
  (kao-render-tests--with-ranges ranges
    (kao--render (kao-sels-make :list list :main main :sorted nil)))
  (let ((raw (kao-render-tests--spec-snapshot)))
    (kao--clear-overlays)
    (kao-render-tests--with-ranges ranges
      (kao--render (kao-sels-make :list list :main main :sorted t)))
    (should (equal raw (kao-render-tests--spec-snapshot)))
    raw))

(ert-deftest kao-render-sorted-flag-identity-single-window ()
  "A min-sorted secondary list renders the same specs with `sorted' t (clipped
walk that breaks once past the window) as with `sorted' nil (full walk): the
clip is behaviour-preserving."
  (kao-render-tests--with "0123456789\n0123456789\n0123456789\n"
    ;; mins ascending 1,3,14,25; window [1,12) shows only the pos-3 secondary,
    ;; so the pos-14 sel (min 14 >= hend 12) triggers the sorted early-break.
    (let ((snap (kao-render-tests--assert-sorted-identity
                 (list (kao-sel-make :anchor 1 :cursor 1)     ; main
                       (kao-sel-make :anchor 3 :cursor 4)     ; in window
                       (kao-sel-make :anchor 14 :cursor 15)   ; past window -> break
                       (kao-sel-make :anchor 25 :cursor 25))  ; never reached (sorted)
                 0 (list (cons 1 12)))))
      (should (equal snap (list (list 3 5 'kao-selection)
                                (list 4 5 'kao-cursor)))))))

(ert-deftest kao-render-sorted-flag-identity-multi-window ()
  "The clipped walk matches the full walk across a multi-window union too:
a gap selection between two viewports is dropped by both forms, and a selection
past the last viewport triggers the sorted early-break identically."
  (kao-render-tests--with "0123456789\n0123456789\n0123456789\n"
    ;; window A [1,12) (line 1) + window B [14,20) (start of line 2); hend=20.
    ;; mins ascending 1,3,15,25: pos-3 in A, pos-15 in B, pos-25 past hend=20
    ;; -> sorted break.  The full walk reaches pos-25 and skips it; identical.
    (let ((snap (kao-render-tests--assert-sorted-identity
                 (list (kao-sel-make :anchor 1 :cursor 1)     ; main
                       (kao-sel-make :anchor 3 :cursor 4)     ; window A
                       (kao-sel-make :anchor 15 :cursor 16)   ; window B
                       (kao-sel-make :anchor 25 :cursor 26))  ; past both -> break
                 0 (list (cons 1 12) (cons 14 20)))))
      (should (equal snap (list (list 3 5 'kao-selection)
                                (list 4 5 'kao-cursor)
                                (list 15 17 'kao-selection)
                                (list 16 17 'kao-cursor)))))))

;;;; Overlay-operation counting (N=1 renders are free)

(defmacro kao-render-tests--counting-ops (&rest body)
  "Run BODY counting overlay operations; return the total count.
Counts every call to `move-overlay', `delete-overlay' and `overlay-put'."
  `(let ((kao-render-tests--ops 0))
     (cl-letf* ((real-move (symbol-function 'move-overlay))
                (real-del (symbol-function 'delete-overlay))
                (real-put (symbol-function 'overlay-put))
                ((symbol-function 'move-overlay)
                 (lambda (&rest args)
                   (cl-incf kao-render-tests--ops) (apply real-move args)))
                ((symbol-function 'delete-overlay)
                 (lambda (&rest args)
                   (cl-incf kao-render-tests--ops) (apply real-del args)))
                ((symbol-function 'overlay-put)
                 (lambda (&rest args)
                   (cl-incf kao-render-tests--ops) (apply real-put args))))
       ,@body)
     kao-render-tests--ops))

(ert-deftest kao-render-single-sel-zero-overlay-ops ()
  "A steady-state single-selection render performs ZERO overlay operations."
  (kao-render-tests--with "0123456789\n"
    (let ((sels (kao-sels-make
                 :list (list (kao-sel-make :anchor 2 :cursor 5)) :main 0)))
      (should (= 0 (kao-render-tests--counting-ops
                    (kao--render sels (point-min) (point-max))
                    (kao--render sels (point-min) (point-max))))))))

(ert-deftest kao-render-multi-to-single-detach-once ()
  "Dropping to one selection detaches the surplus ONCE; later renders are free."
  (kao-render-tests--with "0123456789\n"
    (let ((single (kao-sels-make
                   :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0)))
      ;; multi render attaches overlays (4 specs: 2 bodies + 2 cursors)
      (should (> (kao-render-tests--counting-ops
                  (kao--render (kao-render-tests--sels) (point-min) (point-max)))
                 0))
      ;; first single render pays exactly the 4 surplus detaches, nothing else
      (should (= 4 (kao-render-tests--counting-ops
                    (kao--render single (point-min) (point-max)))))
      ;; second single render is entirely free
      (should (= 0 (kao-render-tests--counting-ops
                    (kao--render single (point-min) (point-max))))))))

;;;; Per-keystroke refresh dedupe

(defmacro kao-render-tests--counting-renders (&rest body)
  "Run BODY counting how many times `kao--render' actually draws; return count.
`kao--refresh' mirrors + reveals + renders once per changed frame, so counting
`kao--render' calls counts the render walks the dedupe guard was meant to halve."
  `(let ((kao-render-tests--renders 0))
     (cl-letf* ((real-render (symbol-function 'kao--render))
                ((symbol-function 'kao--render)
                 (lambda (&rest args)
                   (cl-incf kao-render-tests--renders) (apply real-render args))))
       ,@body)
     kao-render-tests--renders))

(ert-deftest kao-render-refresh-dedupes-doubled-per-key-render ()
  "The mid-command refresh and the `post-command-hook' refresh collapse to ONE
render walk when neither the buffer nor `kao--sels' changed since the first
\.  Two `kao--refresh' calls over an unchanged frame render
once, not twice — the guard keys on `(buffer-chars-modified-tick . kao--sels)'."
  (kao-render-tests--with "0123456789\n"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-render-tests--sels))
          (should (= 1 (kao-render-tests--counting-renders
                        (kao--refresh)       ; command-body refresh
                        (kao--refresh)))))   ; post-command-hook refresh: deduped
      (kao-mode -1))))

(ert-deftest kao-render-refresh-rerenders-on-selection-change ()
  "A fresh `kao--sels' object re-renders: every selection installer rebuilds
`kao--sels' as a new struct, so the guard's `eq'-identity key changes and the
next refresh draws again."
  (kao-render-tests--with "0123456789\n"
    (kao-mode 1)
    (unwind-protect
        (should (= 2 (kao-render-tests--counting-renders
                      (setq kao--sels (kao-render-tests--sels))
                      (kao--refresh)
                      (setq kao--sels (kao-render-tests--sels)) ; new object, same span
                      (kao--refresh))))
      (kao-mode -1))))

(ert-deftest kao-render-refresh-rerenders-on-buffer-edit ()
  "A buffer edit advances `buffer-chars-modified-tick', so the next refresh over
the SAME `kao--sels' object still re-renders — the frame genuinely changed
\."
  (kao-render-tests--with "0123456789\n"
    (buffer-enable-undo)
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-render-tests--sels))
          (should (= 2 (kao-render-tests--counting-renders
                        (kao--refresh)
                        (save-excursion (goto-char (point-max)) (insert "x"))
                        (kao--refresh)))))
      (kao-mode -1))))

;;;; Pulse flash (-2)

(ert-deftest kao-render-pulse-noop-in-batch ()
  "`kao--pulse-span' never fires for an undisplayed buffer (batch included)."
  (require 'pulse)                       ; load BEFORE stubbing (require inside)
  (let ((calls 0))
    (cl-letf (((symbol-function 'pulse-momentary-highlight-region)
               (lambda (&rest _) (cl-incf calls))))
      (with-temp-buffer
        (insert "abc")
        (kao--pulse-span 1 3)))
    (should (= calls 0))))

(ert-deftest kao-render-pulse-defcustom-gates ()
  "With a window present, `kao-pulse' nil suppresses the flash; t fires it."
  (require 'pulse)
  (let ((calls 0))
    (cl-letf (((symbol-function 'pulse-momentary-highlight-region)
               (lambda (&rest _) (cl-incf calls)))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) t)))
      (with-temp-buffer
        (insert "abc")
        (let ((kao-pulse nil)) (kao--pulse-span 1 3))
        (should (= calls 0))
        (let ((kao-pulse t)) (kao--pulse-span 1 3))
        (should (= calls 1))))))

(provide 'kao-render-tests)
;;; kao-render-tests.el ends here
