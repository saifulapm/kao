;;; kao-state-tests.el --- Tests for kao-state -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the kao minor mode, the buffer-local selection list, and the
;; list-transform mapper.

;;; Code:

(require 'ert)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-state)
(require 'kao-keys)                    ; default bindings
(require 'outline)                     ; the A-22c fold-reveal tests

(defmacro kao-state-tests--with (content &rest body)
  "Run BODY in a temp buffer containing CONTENT with point at `point-min'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;;; Minor mode lifecycle

(ert-deftest kao-state-enable-initializes-one-selection ()
  "Enabling `kao-mode' seeds a single selection at point as the main."
  (kao-state-tests--with "0123456789ab"
    (goto-char 4)
    (kao-mode 1)
    (unwind-protect
        (progn
          (should kao-mode)
          (should kao--normal-active)
          (should (= 1 (length (kao-sels-list kao--sels))))
          (should (= 0 (kao-sels-main kao--sels)))
          (let ((s (car (kao-sels-list kao--sels))))
            (should (= (kao-sel-anchor s) 4))
            (should (= (kao-sel-cursor s) 4)))
          (should (memq 'kao--refresh post-command-hook))
          (should (memq 'kao--emulation-mode-map-alist emulation-mode-map-alists)))
      (kao-mode -1))))

(ert-deftest kao-state-disable-tears-down ()
  "Disabling `kao-mode' clears the list, overlays, keymap flag, and region."
  (kao-state-tests--with "0123456789ab"
    (kao-mode 1)
    ;; install two secondaries so the overlay pool is non-empty
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 4 :cursor 5)
                                 (kao-sel-make :anchor 8 :cursor 8))
                     :main 0))
    (kao--refresh)
    (should kao--overlay-pool)
    (kao-mode -1)
    (should-not kao-mode)
    (should-not kao--normal-active)
    (should-not kao--sels)
    (should-not kao--overlay-pool)
    (should-not (memq 'kao--refresh post-command-hook))
    (should-not mark-active)))

(ert-deftest kao-state-enable-asserts-buffer-local-transient-mark-mode ()
  "Enabling `kao-mode' asserts buffer-local `transient-mark-mode' even when the
user disabled the global, so the main-body region highlight (the native
`region' face) no longer silently depends on that global (evil visual-state
makes the same assertion).  Disabling kills the local, restoring
inheritance of the global."
  (let ((transient-mark-mode nil))          ; a user who turned the global off
    (kao-state-tests--with "0123456789"
      (should-not transient-mark-mode)      ; inherits the (nil) global before kao
      (kao-mode 1)
      (unwind-protect
          (progn
            (should (local-variable-p 'transient-mark-mode))
            (should transient-mark-mode))   ; buffer-local t asserted
        (kao-mode -1))
      (should-not (local-variable-p 'transient-mark-mode)) ; local killed on disable
      (should-not transient-mark-mode))))   ; back to the inherited (nil) global

;;;; Fold reveal at the main selection (A-22c ratified stance)

(ert-deftest kao-state-refresh-reveals-fold-at-main ()
  "A main selection landing inside an `outline-hide-subtree' fold is revealed
after `kao--refresh' — the isearch/evil precedent (A-22c): selections operate
on buffer text regardless of visibility, but the fold at the MAIN cursor opens
so the user sees where they are."
  (with-temp-buffer
    (insert "* A\nb1\nb2\n* B\n")               ; b1@5, b2@8 hidden under "* A"
    (outline-mode)
    (goto-char (point-min))
    (outline-hide-subtree)
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make          ; main cursor lands in the fold
                           :list (list (kao-sel-make :anchor 5 :cursor 5))
                           :main 0))
          (should (invisible-p 5))                ; precondition: cursor hidden
          (kao--refresh)
          (should-not (invisible-p 5)))           ; fold opened at the main cursor
      (kao-mode -1))))

(ert-deftest kao-state-refresh-no-fold-leaves-cursor-visible ()
  "With no fold the reveal guard is a single `invisible-p' check that returns
nil — the main cursor stays put and visible (A-22c cost guard)."
  (with-temp-buffer
    (insert "plain text\nsecond line\n")
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 3 :cursor 3))
                           :main 0))
          (kao--refresh)
          (should-not (invisible-p (point)))
          (should (= (point) 3)))
      (kao-mode -1))))

(ert-deftest kao-state-reveal-functions-opener-invoked-at-main ()
  "`kao-reveal-functions' is an abnormal hook run with the main cursor position
whenever that position is invisible after a refresh.  A user-registered opener —
the org-fold text-property seam — is invoked AT the main cursor and can clear
text-property invisibility that the overlay/`org-fold' defaults cannot reach
\(no overlay carries an `isearch-open-invisible' opener; the buffer is not org)."
  (with-temp-buffer
    (insert "abcdefghij")                         ; 1..10
    ;; A text-property invisible span with no overlay — the org-fold/org-link
    ;; mechanism the default entries cannot open outside org-mode.
    (put-text-property 4 8 'invisible 'my-fold)   ; chars 4..7 invisible
    (kao-mode 1)
    (unwind-protect
        (let* ((called-at nil)
               (kao-reveal-functions
                (cons (lambda (pos)
                        (setq called-at pos)
                        (remove-text-properties (point-min) (point-max)
                                                '(invisible nil)))
                      kao-reveal-functions)))
          (setq kao--sels (kao-sels-make          ; main cursor lands in the span
                           :list (list (kao-sel-make :anchor 5 :cursor 5))
                           :main 0))
          (should (invisible-p 5))                ; precondition: text-prop hidden
          (kao--refresh)
          (should (= called-at 5))                ; opener ran AT the main cursor
          (should-not (invisible-p 5)))           ; and cleared it (defaults can't)
      (kao-mode -1))))

;;;; List-transform mapper

(ert-deftest kao-state-map-selections-sorts-and-tracks-main ()
  "Mapping re-sorts by position and follows the main by identity."
  (kao-state-tests--with "0123456789ab"   ; point-max 13, last on-char pos 12
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 10 :cursor 10)   ; main
                                 (kao-sel-make :anchor 2 :cursor 2)
                                 (kao-sel-make :anchor 5 :cursor 5))
                     :main 0))
    (kao--map-selections #'identity)
    (should (equal (mapcar #'kao-sel-min (kao-sels-list kao--sels)) '(2 5 10)))
    (should (= (kao-sels-main kao--sels) 2))))   ; the pos-10 sel is now last

(ert-deftest kao-state-map-selections-applies-transform ()
  "Mapping applies the per-sel transform to every selection."
  (kao-state-tests--with "0123456789ab"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 10 :cursor 10)
                                 (kao-sel-make :anchor 2 :cursor 2)
                                 (kao-sel-make :anchor 5 :cursor 5))
                     :main 0))
    (kao--map-selections
     (lambda (s) (kao-sel-make :anchor (1+ (kao-sel-anchor s))
                               :cursor (1+ (kao-sel-cursor s)))))
    (should (equal (mapcar #'kao-sel-min (kao-sels-list kao--sels)) '(3 6 11)))
    (should (= (kao-sels-main kao--sels) 2))))

(ert-deftest kao-state-clamp-keeps-positions-on-char ()
  "Clamp pins anchor/cursor to the last on-char position."
  (kao-state-tests--with "ab"             ; point-max 3, last on-char pos 2
    (let ((c (kao--clamp-sel (kao-sel-make :anchor 99 :cursor -3))))
      (should (= (kao-sel-anchor c) 2))
      (should (= (kao-sel-cursor c) 1)))))

;;;; Kao--map-filter-selections (faithful select drop + main-adjust)

(ert-deftest kao-state-filter-drops-before-main-keeps-main ()
  "Dropping a selection before the main shifts the index but keeps main by id."
  (kao-state-tests--with "0123456789ab"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)    ; i0, dropped
                                 (kao-sel-make :anchor 5 :cursor 5)    ; i1
                                 (kao-sel-make :anchor 10 :cursor 10)) ; i2 = main
                     :main 2))
    (kao--map-filter-selections (lambda (s) (unless (= (kao-sel-min s) 2) s)))
    (should (equal (mapcar #'kao-sel-min (kao-sels-list kao--sels)) '(5 10)))
    (should (= (kao-sels-main kao--sels) 1))))    ; the pos-10 sel still main

(ert-deftest kao-state-filter-drops-main-lands-on-previous ()
  "Dropping the main lands main on the previous survivor (i<=main, main!=0)."
  (kao-state-tests--with "0123456789ab"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)    ; i0
                                 (kao-sel-make :anchor 5 :cursor 5)    ; i1 = main, dropped
                                 (kao-sel-make :anchor 10 :cursor 10)) ; i2
                     :main 1))
    (kao--map-filter-selections (lambda (s) (unless (= (kao-sel-min s) 5) s)))
    (should (equal (mapcar #'kao-sel-min (kao-sels-list kao--sels)) '(2 10)))
    (should (= (kao-sels-main kao--sels) 0))))    ; pos-2, the previous survivor

(ert-deftest kao-state-filter-drop-main-at-zero-keeps-first ()
  "Dropping main at index 0 keeps main at the first survivor (main!=0 guard)."
  (kao-state-tests--with "0123456789ab"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)    ; i0 = main, dropped
                                 (kao-sel-make :anchor 5 :cursor 5)
                                 (kao-sel-make :anchor 10 :cursor 10))
                     :main 0))
    (kao--map-filter-selections (lambda (s) (unless (= (kao-sel-min s) 2) s)))
    (should (equal (mapcar #'kao-sel-min (kao-sels-list kao--sels)) '(5 10)))
    (should (= (kao-sels-main kao--sels) 0))))    ; first survivor

(ert-deftest kao-state-filter-all-dropped-leaves-unchanged ()
  "When every result is nil the selection list is left intact (no-selections)."
  (kao-state-tests--with "0123456789ab"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)
                                 (kao-sel-make :anchor 5 :cursor 5))
                     :main 1))
    (kao--map-filter-selections #'ignore)
    (should (equal (mapcar #'kao-sel-min (kao-sels-list kao--sels)) '(2 5)))
    (should (= (kao-sels-main kao--sels) 1))))

(ert-deftest kao-state-filter-resorts-and-clamps-survivors ()
  "Survivors from an unsorted list are re-sorted and clamped on-char."
  (kao-state-tests--with "0123456789ab"     ; point-max 13, last on-char pos 12
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 5 :cursor 5)
                                 (kao-sel-make :anchor 2 :cursor 2))
                     :main 0))
    ;; identity, but push the pos-5 cursor past point-max to prove the clamp;
    ;; the two stay far apart so they do not merge.
    (kao--map-filter-selections
     (lambda (s) (if (= (kao-sel-anchor s) 5)
                     (kao-sel-make :anchor 5 :cursor 99)
                   s)))
    (should (equal (mapcar #'kao-sel-min (kao-sels-list kao--sels)) '(2 5)))
    (should (= (kao-sel-max (cadr (kao-sels-list kao--sels))) 12)))) ; clamped

(ert-deftest kao-state-filter-merges-overlapping-results ()
  "Overlapping survivors collapse (Kakoune `select' sort_and_merge, normal.cc:131)."
  (kao-state-tests--with "0123456789ab"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 5)
                                 (kao-sel-make :anchor 4 :cursor 8))
                     :main 0))
    (kao--map-filter-selections #'identity)  ; [2,5] and [4,8] overlap -> [2,8]
    (should (= 1 (length (kao-sels-list kao--sels))))
    (let ((s (car (kao-sels-list kao--sels))))
      (should (= (kao-sel-min s) 2))
      (should (= (kao-sel-max s) 8)))
    (should (= (kao-sels-main kao--sels) 0))))

(ert-deftest kao-state-filter-extend-merges-onto-selection ()
  "With EXTEND, a non-nil result is merged onto the selection (anchor kept)."
  (kao-state-tests--with "0123456789ab"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3))
                     :main 0))
    (kao--map-filter-selections (lambda (_s) (kao-sel-make :anchor 3 :cursor 5)) t)
    (let ((s (car (kao-sels-list kao--sels))))
      (should (= (kao-sel-anchor s) 1))      ; anchor kept (extend)
      (should (= (kao-sel-cursor s) 5)))))   ; cursor moved to the result's cursor

(ert-deftest kao-state-filter-replace-uses-result ()
  "Without EXTEND, a non-nil result replaces the selection (anchor = result's)."
  (kao-state-tests--with "0123456789ab"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3))
                     :main 0))
    (kao--map-filter-selections (lambda (_s) (kao-sel-make :anchor 3 :cursor 5)))
    (let ((s (car (kao-sels-list kao--sels))))
      (should (= (kao-sel-anchor s) 3))      ; replaced
      (should (= (kao-sel-cursor s) 5)))))

;;;; kao--extend-clamp + kao--map-selections-extend (Extend mode, normal.cc)

(ert-deftest kao-state-extend-clamp-preserves-anchor ()
  "`kao--extend-clamp' clamps only the cursor on-char; the anchor is verbatim."
  (kao-state-tests--with "ab"             ; point-max 3, last on-char pos 2
    ;; Anchor sitting past the on-char end is kept as-is (the clamp revisit);
    ;; only the cursor is pinned.  `kao--clamp-sel' would instead shrink it.
    (let ((c (kao--extend-clamp (kao-sel-make :anchor 99 :cursor 99))))
      (should (= (kao-sel-anchor c) 99))            ; preserved, not shrunk
      (should (= (kao-sel-cursor c) 2)))            ; cursor clamped on-char
    (let ((c (kao--clamp-sel (kao-sel-make :anchor 99 :cursor 99))))
      (should (= (kao-sel-anchor c) 2)))))          ; contrast: Replace clamp shrinks

(ert-deftest kao-state-map-extend-keeps-anchor-moves-cursor ()
  "Char-style extend (zero-width motion result) keeps the anchor, moves cursor.
This is the `move_cursor<Extend>' case routed through the merge mapping."
  (kao-state-tests--with "0123456789ab"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 3 :cursor 5))  ; forward [3,5]
                     :main 0))
    ;; motion: collapse one char to the right of the cursor (zero-width result).
    (kao--map-selections-extend
     (lambda (s) (let ((p (1+ (kao-sel-cursor s))))
                   (kao-sel-make :anchor p :cursor p))))
    (let ((s (car (kao-sels-list kao--sels))))
      (should (= (kao-sel-anchor s) 3))             ; anchor preserved
      (should (= (kao-sel-cursor s) 6)))))          ; cursor advanced

(ert-deftest kao-state-map-extend-backward-keeps-anchor ()
  "A backward selection extended leftward keeps its (higher) anchor."
  (kao-state-tests--with "0123456789ab"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 8 :cursor 5))  ; backward
                     :main 0))
    (kao--map-selections-extend
     (lambda (s) (let ((p (1- (kao-sel-cursor s))))
                   (kao-sel-make :anchor p :cursor p))))
    (let ((s (car (kao-sels-list kao--sels))))
      (should (= (kao-sel-anchor s) 8))             ; anchor kept (still backward)
      (should (= (kao-sel-cursor s) 4)))))

(ert-deftest kao-state-map-extend-word-region-merges-anchor ()
  "Region motion (W-style) merges: a forward extend pulls the anchor to the min."
  (kao-state-tests--with "0123456789ab"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 5 :cursor 7))  ; forward [5,7]
                     :main 0))
    ;; motion returns a forward region [2,9] (anchor before the current anchor).
    (kao--map-selections-extend
     (lambda (_s) (kao-sel-make :anchor 2 :cursor 9)))
    (let ((s (car (kao-sels-list kao--sels))))
      (should (= (kao-sel-anchor s) 2))             ; anchor pulled to min(5,2)
      (should (= (kao-sel-cursor s) 9)))))          ; cursor = region end

(ert-deftest kao-state-map-extend-folds-count-per-step ()
  "Count folds the merge per step (`repeated<select<Extend>>'): 3 right steps."
  (kao-state-tests--with "0123456789ab"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 3 :cursor 3))
                     :main 0)
          kao--count 3)
    (kao--map-selections-extend
     (lambda (s) (let ((p (1+ (kao-sel-cursor s))))
                   (kao-sel-make :anchor p :cursor p))))
    (let ((s (car (kao-sels-list kao--sels))))
      (should (= (kao-sel-anchor s) 3))             ; anchor fixed across the fold
      (should (= (kao-sel-cursor s) 6)))))          ; 3 -> 6 (three steps)

(ert-deftest kao-state-map-extend-multi-and-sort-merge ()
  "Extend maps over every selection, then sort-and-merges overlapping results."
  (kao-state-tests--with "0123456789ab"
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 3)
                                 (kao-sel-make :anchor 5 :cursor 6))
                     :main 0))
    ;; extend each cursor to 7: [2,7] and [5,7] overlap -> merge to [2,7].
    (kao--map-selections-extend
     (lambda (_s) (kao-sel-make :anchor 7 :cursor 7)))
    (should (= 1 (length (kao-sels-list kao--sels))))
    (let ((s (car (kao-sels-list kao--sels))))
      (should (= (kao-sel-min s) 2))
      (should (= (kao-sel-max s) 7)))))

;;;; Insert state machine

(ert-deftest kao-state-enter-insert-flips-and-suspends ()
  "Entering insert flips the state flags, suspends the region, places point."
  (kao-state-tests--with "abcdef"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 2 :cursor 4)) :main 0))
          (kao--refresh)
          (should mark-active)                  ; multi-char region is shown
          (kao--enter-insert 'insert (lambda () (goto-char 2)))
          (should kao--insert-active)
          (should-not kao--normal-active)
          (should-not mark-active)              ; region suspended during insert
          (should (= (point) 2)))
      (kao-mode -1))))

(ert-deftest kao-state-exit-insert-rebuilds-selection ()
  "Exiting `i' after typing keeps the original 1-char span, shifted past the text.
The shaped `i' rebuild preserves the pre-insert selection
rather than collapsing on the typed char."
  (kao-state-tests--with "abcdef"
    (buffer-enable-undo)
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 2 :cursor 2)) ; "b"
                           :main 0))
          (kao--enter-insert 'insert (lambda () (goto-char 2)))
          (insert "X")                          ; "aXbcdef", point 3
          (kao-insert-exit)
          (should-not kao--insert-active)
          (should kao--normal-active)
          (should (string= (buffer-string) "aXbcdef"))
          (let ((s (kao--main-sel)))
            (should (= (kao-sel-cursor s) 3))   ; on 'b', the shifted original char
            (should (= (kao-sel-anchor s) 3))))
      (kao-mode -1))))

(ert-deftest kao-state-exit-insert-append-steps-back ()
  "Append (`a') with nothing typed steps the cursor back one on exit."
  (kao-state-tests--with "abcdef"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--enter-insert 'append (lambda () (goto-char 4)))
          (kao-insert-exit)                     ; nothing typed
          (should (= (kao-sel-cursor (kao--main-sel)) 3)))
      (kao-mode -1))))

;;;; Keys-edit-0 (arm A) — faithful Kakoune insert-exit selection shapes

;; Kakoune's `prepare' carries a per-mode anchor through the insert session and
;; restores a SHAPED selection at exit (input_handler.cc:1456-1533): `i' keeps
;; the ORIGINAL span (backward, cursor at its min); `a' covers original+typed
;; with the cursor stepped back onto the last typed char (the only restore-cursor
;; mode); c/A/I/o/O collapse one char AFTER the typed text.  These pins drive the
;; real entry commands over the canonical "abcdef" span [1,3] ("abc"), single-
;; and multi-selection, composing with the net-text markers.

(defmacro kao-state-tests--insert (content sels main &rest body)
  "kao-mode temp buffer of CONTENT, selection list SELS main MAIN, then BODY.
A clean, isolated kill/clipboard state is bound so the `c' yank path is
deterministic in batch regardless of test order."
  (declare (indent 3))
  `(kao-state-tests--with ,content
     (buffer-enable-undo)
     (kao-mode 1)
     (let ((kill-ring nil) (kill-ring-yank-pointer nil)
           (interprogram-cut-function nil) (interprogram-paste-function nil))
       (unwind-protect
           (progn
             (setq kao--sels (kao-sels-make :list ,sels :main ,main))
             ,@body)
         (kao-mode -1)))))

(defun kao-state-tests--collapsed-after-x-p ()
  "Non-nil when the main selection is collapsed one char AFTER the typed \"X\".
Independent check of the c/A/I/o/O exit shape: locate the single typed \"X\" and
assert the main cursor sits on the position immediately after it."
  (let* ((s (kao--main-sel))
         (xpos (save-excursion (goto-char (point-min))
                               (search-forward "X") (match-beginning 0))))
    (and (= (kao-sel-anchor s) (kao-sel-cursor s))     ; collapsed
         (= (kao-sel-cursor s) (1+ xpos)))))           ; one char AFTER "X"

(ert-deftest kao-state-insert-exit-i-keeps-original-span ()
  "`i' + type + exit keeps the ORIGINAL span selected, backward, cursor at min."
  (kao-state-tests--insert "abcdef"
      (list (kao-sel-make :anchor 1 :cursor 3)) 0    ; span "abc"
    (kao-insert)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "Xabcdef"))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) 4))              ; old max, shifted past "X"
      (should (= (kao-sel-cursor s) 2))              ; old min, shifted; cursor at min
      (should (string= (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s)))
                       "abc")))))                     ; original content still covered

(ert-deftest kao-state-insert-exit-a-covers-original-plus-typed ()
  "`a' + type + exit selects original+typed, cursor onto the last typed char."
  (kao-state-tests--insert "abcdef"
      (list (kao-sel-make :anchor 1 :cursor 3)) 0
    (kao-append)
    (insert "XY")
    (kao-insert-exit)
    (should (string= (buffer-string) "abcXYdef"))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) 1))              ; old min
      (should (= (kao-sel-cursor s) 5))              ; last typed char ("Y")
      (should (= (char-after (kao-sel-cursor s)) ?Y))
      (should (string= (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s)))
                       "abcXY")))))

(ert-deftest kao-state-insert-exit-a-nothing-typed-restores-span ()
  "`a' + immediate exit steps back onto the original span (`m_restore_cursor')."
  (kao-state-tests--insert "abcdef"
      (list (kao-sel-make :anchor 1 :cursor 3)) 0
    (kao-append)
    (kao-insert-exit)                                ; nothing typed
    (should (string= (buffer-string) "abcdef"))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) 1))
      (should (= (kao-sel-cursor s) 3)))))           ; back onto the last char

(ert-deftest kao-state-insert-exit-c-collapses-after-typed ()
  "`c' + type + exit collapses one char AFTER the typed text (no restore-cursor)."
  (kao-state-tests--insert "abcdef"
      (list (kao-sel-make :anchor 1 :cursor 3)) 0
    (kao-change)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "Xdef"))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) (kao-sel-cursor s)))  ; collapsed
      (should (= (char-after (kao-sel-cursor s)) ?d)))))   ; one char after "X"

(ert-deftest kao-state-insert-exit-I-collapses-after-typed ()
  "`I' + type + exit collapses one char after the typed text."
  (kao-state-tests--insert "abcdef"
      (list (kao-sel-make :anchor 1 :cursor 3)) 0
    (kao-insert-line-begin)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "Xabcdef"))
    (should (kao-state-tests--collapsed-after-x-p))))

(ert-deftest kao-state-insert-exit-A-collapses-after-typed ()
  "`A' + type + exit collapses one char after the typed text."
  (kao-state-tests--insert "abcdef\ngh"
      (list (kao-sel-make :anchor 1 :cursor 3)) 0
    (kao-append-line-end)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "abcdefX\ngh"))
    (should (kao-state-tests--collapsed-after-x-p))))

(ert-deftest kao-state-insert-exit-o-collapses-after-typed ()
  "`o' + type + exit collapses one char after the typed text."
  (kao-state-tests--insert "abcdef\ngh"
      (list (kao-sel-make :anchor 1 :cursor 3)) 0
    (kao-open-below)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "abcdef\nX\ngh"))
    (should (kao-state-tests--collapsed-after-x-p))))

(ert-deftest kao-state-insert-exit-O-collapses-after-typed ()
  "`O' + type + exit collapses one char after the typed text."
  (kao-state-tests--insert "abcdef"
      (list (kao-sel-make :anchor 1 :cursor 3)) 0
    (kao-open-above)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "X\nabcdef"))
    (should (kao-state-tests--collapsed-after-x-p))))

(ert-deftest kao-state-insert-exit-multi-i-keeps-every-span ()
  "Multi `i' + type + exit keeps every selection's ORIGINAL span (Kakoune `%s..i').
The signature `%s foo<ret>i>escape' workflow leaves every match selected for the
next operation, backward with the cursor at its min."
  (kao-state-tests--insert "cat cat cat"
      (list (kao-sel-make :anchor 1 :cursor 3)        ; "cat"
            (kao-sel-make :anchor 5 :cursor 7)        ; "cat"
            (kao-sel-make :anchor 9 :cursor 11))      ; "cat"
      2                                                ; main = last
    (kao-insert)                                       ; site = each min
    (insert ">")                                      ; typed only at the main
    (kao-insert-exit)
    (should (string= (buffer-string) ">cat >cat >cat"))
    (should (= 3 (length (kao-sels-list kao--sels))))
    (dolist (s (kao-sels-list kao--sels))
      (should (string= (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s)))
                       "cat"))
      (should (> (kao-sel-anchor s) (kao-sel-cursor s))))))  ; backward, cursor at min

(ert-deftest kao-state-insert-exit-shapes-hold-under-point-motion ()
  "Native point motion mid-insert neither mass-duplicates nor corrupts the shaped
exit selections (x): the net measured is the typed run
only, and every selection keeps its original span."
  (kao-state-tests--insert "foo bar baz T"
      (list (kao-sel-make :anchor 1 :cursor 3)        ; "foo"
            (kao-sel-make :anchor 5 :cursor 7)        ; "bar"
            (kao-sel-make :anchor 9 :cursor 11))      ; "baz"
      2                                                ; main on "baz"
    (kao-insert)                                       ; i at each min
    (insert "X")                                      ; net = "X"
    (end-of-line)                                     ; native motion mid-insert
    (kao-insert-exit)
    (should (string= (buffer-string) "Xfoo Xbar Xbaz T"))  ; no mass-duplication
    (should (= 3 (length (kao-sels-list kao--sels))))
    (dolist (s (kao-sels-list kao--sels))             ; every span still shaped
      (should (member (buffer-substring (kao-sel-min s) (1+ (kao-sel-max s)))
                      '("foo" "bar" "baz")))
      (should (> (kao-sel-anchor s) (kao-sel-cursor s))))))

;;;; Input method (hel-study-1) — per-state IME management, ported from evil/hel

(ert-deftest kao-state-input-method-round-trips-across-states ()
  "The input method drops in normal state and restores on insert entry.
Activating a method in normal state saves it and deactivates it (so kao keys
are not swallowed); `i' brings it back; `<esc>' drops it again (hel-study-1)."
  (skip-unless (ignore-errors (require 'quail) t))
  (kao-state-tests--with "abcdef"
    (unwind-protect
        (progn
          (kao-mode 1)
          ;; Activate a method while in normal state.
          (activate-input-method "cyrillic-translit")
          ;; kao saves it and deactivates it — a normal key now resolves to its
          ;; kao command, not the method's translated char.
          (should (null current-input-method))
          (should (equal kao--input-method "cyrillic-translit"))
          ;; Enter insert: the saved method comes back.
          (kao--enter-insert 'insert (lambda () (goto-char 2)))
          (should (equal current-input-method "cyrillic-translit"))
          ;; Leave insert: off again in normal, still remembered.
          (kao-insert-exit)
          (should (null current-input-method))
          (should (equal kao--input-method "cyrillic-translit"))
          ;; Round-trip repeats.
          (kao--enter-insert 'insert (lambda () (goto-char 2)))
          (should (equal current-input-method "cyrillic-translit")))
      (when (bound-and-true-p kao-mode) (kao-mode -1))
      (deactivate-input-method))))

(ert-deftest kao-state-input-method-restored-on-disable ()
  "Disabling `kao-mode' reactivates the method it deactivated (hel-study-1)."
  (skip-unless (ignore-errors (require 'quail) t))
  (kao-state-tests--with "abcdef"
    (unwind-protect
        (progn
          (kao-mode 1)
          (activate-input-method "cyrillic-translit")   ; saved + dropped (normal)
          (should (null current-input-method))
          (kao-mode -1)                                  ; restores the user's method
          (should (equal current-input-method "cyrillic-translit")))
      (when (bound-and-true-p kao-mode) (kao-mode -1))
      (deactivate-input-method))))

(ert-deftest kao-state-input-method-round-trips-through-one-shot ()
  "The one-shot `<a-;>' keeps the input method per state (hel-study-1).
Insert has the method; the one-shot pop to normal drops it; the pop back to
insert restores it — the same normal↔insert discipline as `<esc>'/`i'."
  (skip-unless (ignore-errors (require 'quail) t))
  (kao-state-tests--with "abcdef"
    (unwind-protect
        (progn
          (kao-mode 1)
          (activate-input-method "cyrillic-translit")   ; saved + dropped (normal)
          (kao--enter-insert 'insert (lambda () (goto-char 2)))
          (should (equal current-input-method "cyrillic-translit"))   ; on in insert
          (kao-insert-one-shot)                          ; `<a-;>' → one-shot normal
          (should (null current-input-method))           ; off during the one shot
          (kao--oneshot-pop)                             ; back to insert
          (should (equal current-input-method "cyrillic-translit")))  ; on again
      (when (bound-and-true-p kao-mode) (kao-mode -1))
      (deactivate-input-method))))

(ert-deftest kao-state-input-method-off-stays-off ()
  "With no method active, the state round-trip never fabricates one (hel-study-1)."
  (kao-state-tests--with "abcdef"
    (unwind-protect
        (progn
          (kao-mode 1)
          (should (null current-input-method))
          (kao--enter-insert 'insert (lambda () (goto-char 2)))
          (should (null current-input-method))          ; nothing to restore
          (kao-insert-exit)
          (should (null current-input-method)))
      (when (bound-and-true-p kao-mode) (kao-mode -1)))))

(ert-deftest kao-state-insert-is-one-undo-unit ()
  "All edits in an insert session collapse into a single undo step."
  (kao-state-tests--with "hello"
    (buffer-enable-undo)
    (goto-char (point-min))
    (undo-boundary)
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--enter-insert 'insert (lambda () (goto-char (point-min))))
          (insert "X") (undo-boundary)
          (insert "Y") (undo-boundary)
          (insert "Z")
          (kao-insert-exit)
          (should (string= (buffer-string) "XYZhello"))
          (primitive-undo 1 buffer-undo-list)   ; one logical undo
          (should (string= (buffer-string) "hello")))
      (kao-mode -1))))

(ert-deftest kao-state-refresh-noop-during-insert ()
  "`kao--refresh' does nothing while insert is active."
  (kao-state-tests--with "abcdef"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--enter-insert 'insert (lambda () (goto-char 3)))
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 5)) :main 0))
          (kao--refresh)                        ; guarded: must not mirror
          (should-not mark-active)
          (should (= (point) 3)))               ; mirror would have moved point to 5
      (kao-mode -1))))

(ert-deftest kao-state-refresh-dedupe-preserves-final-frame ()
  "The per-key dedupe guard never suppresses the FIRST render: after the doubled
refresh (command body + `post-command-hook') the main cursor is still mirrored
to point, the region is active, and the secondary overlay is drawn.  The guard
elides only the redundant second walk — the final frame is identical to a single
refresh (behavior-preserving)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 2 :cursor 4)  ; main span
                                       (kao-sel-make :anchor 7 :cursor 7)) ; secondary
                           :main 0))
          (kao--refresh)                        ; command-body refresh
          (kao--refresh)                        ; post-command-hook refresh (deduped)
          (should (= (point) 4))                ; main cursor mirrored to point
          (should mark-active)                  ; multi-char main region active
          (should (cl-find-if (lambda (o) (eq (overlay-get o 'face) 'kao-cursor))
                              (overlays-at 7)))) ; secondary cursor overlay drawn
      (kao-mode -1))))

(ert-deftest kao-state-exit-insert-empty-buffer ()
  "Exiting insert in an empty buffer yields a valid clamped selection."
  (kao-state-tests--with ""
    (buffer-enable-undo)
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--enter-insert 'insert (lambda () (goto-char (point-min))))
          (kao-insert-exit)                     ; nothing typed, empty buffer
          (should (= (kao-sel-cursor (kao--main-sel)) 1))
          (should (= (kao-sel-anchor (kao--main-sel)) 1)))
      (kao-mode -1))))

(ert-deftest kao-state-exit-insert-net-text-robust-to-point-motion ()
  "Insert exit replays only the typed run, never pre-existing text.
`i X C-e ESC' on three cursors over \"foo bar baz TRAILING\" must yield
\"Xfoo Xbar Xbaz TRAILING\": the End moves point past the typed run, but the
net measured for the secondary replay is exactly the typed \"X\", not the
positional `buffer-substring' from insert-start to the wandered point (which
mass-duplicated the pre-existing \"baz TRAILING\" at every secondary)."
  (kao-state-tests--with "foo bar baz TRAILING"
    (buffer-enable-undo)
    (kao-mode 1)
    (unwind-protect
        (progn
          ;; three cursors on foo/bar/baz, main on baz (the last match)
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 3)
                                       (kao-sel-make :anchor 5 :cursor 7)
                                       (kao-sel-make :anchor 9 :cursor 11))
                           :main 2))
          (kao--enter-insert 'insert (lambda () (goto-char 9)) #'kao-sel-min)
          (insert "X")                          ; main types "X" at baz start
          (end-of-line)                         ; native point motion mid-insert
          (kao-insert-exit)
          (should (string= (buffer-string) "Xfoo Xbar Xbaz TRAILING")))
      (kao-mode -1))))

(ert-deftest kao-state-disable-mid-insert-is-clean ()
  "Disabling kao-mode mid-insert closes the undo group and resets insert state."
  (kao-state-tests--with "hello"
    (buffer-enable-undo)
    (goto-char (point-min))
    (undo-boundary)
    (kao-mode 1)
    (kao--enter-insert 'insert (lambda () (goto-char (point-min))))
    (insert "Z")
    (kao-mode -1)                               ; disable while still inserting
    (should-not kao--insert-active)
    (should-not kao--insert-undo-handle)
    (should-not kao--insert-start)
    (should (string= (buffer-string) "Zhello"))
    (primitive-undo 1 buffer-undo-list)          ; session still collapses to one unit
    (should (string= (buffer-string) "hello"))))

;;;; One-shot normal from insert (`<a-;>')

(ert-deftest kao-state-oneshot-enters-normal-and-syncs ()
  "`kao-insert-one-shot' flips to normal lookup with sels synced at point."
  (kao-state-tests--with "abcdef"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--enter-insert 'insert (lambda () (goto-char 2)))
          (insert "X")                          ; "aXbcdef", point 3
          (kao-insert-one-shot)
          (should kao--insert-oneshot)
          (should kao--normal-active)
          (should-not kao--insert-active)
          (should (eq cursor-type 'box))
          (should (= 1 (length (kao-sels-list kao--sels))))
          (should (= (kao-sel-cursor (kao--main-sel)) 3))
          (should (= (kao-sel-anchor (kao--main-sel)) 3)))
      (kao-mode -1))))

(ert-deftest kao-state-oneshot-requires-insert-session ()
  "`kao-insert-one-shot' outside insert state signals a `user-error'."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (unwind-protect
        (should-error (kao-insert-one-shot) :type 'user-error)
      (kao-mode -1))))

(ert-deftest kao-state-oneshot-mode-on-no-session-message ()
  "With `kao-mode' ON but no insert session,
`kao-insert-one-shot' still signals EXACTLY \"no insert session\".  The added
`kao--assert-mode' context guard passes (mode is on), so the mode-on cause is
preserved, distinct from the mode-off \"names the command\" path.  Additive."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (unwind-protect
        (let ((err (should-error (kao-insert-one-shot) :type 'user-error)))
          (should (equal (cadr err) "no insert session")))
      (kao-mode -1))))

(ert-deftest kao-state-oneshot-syncs-multi-selection-marks ()
  "The sync weaves the main (point) back among the secondary marks."
  (kao-state-tests--with "foo bar baz"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 3)
                                       (kao-sel-make :anchor 5 :cursor 7))
                           :main 1))
          (kao--enter-insert 'insert (lambda () (goto-char 5)) #'kao-sel-min)
          (insert "X")                          ; main types at 5
          (kao-insert-one-shot)
          (should (= 2 (length (kao-sels-list kao--sels))))
          (should (= 1 (kao-sels-main kao--sels)))
          ;; secondary collapsed at its mark (site 1), main at point (6)
          (should (= (kao-sel-cursor (nth 0 (kao-sels-list kao--sels))) 1))
          (should (= (kao-sel-cursor (nth 1 (kao-sels-list kao--sels))) 6)))
      (kao-mode -1))))

(ert-deftest kao-state-oneshot-params-keys-do-not-pop ()
  "Digits, DEL, and `\"' keep the one shot pending (input_handler.cc:339)."
  (kao-state-tests--with "abcdef"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--enter-insert 'insert (lambda () (goto-char 2)))
          (kao-insert-one-shot)
          (dolist (cmd '(kao-digit kao-count-backspace kao-select-register))
            (let ((this-command cmd)) (kao--maybe-reset-count))
            (should kao--insert-oneshot)
            (should kao--normal-active))
          ;; the entry command's own hook pass must not pop either
          (let ((this-command 'kao-insert-one-shot)) (kao--maybe-reset-count))
          (should kao--insert-oneshot))
      (kao-mode -1))))

(ert-deftest kao-state-oneshot-unchanged-roundtrips-verbatim ()
  "A no-op one shot (e.g. `<a-;> ESC') keeps point, start, and marks."
  (kao-state-tests--with "foo bar"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)
                                       (kao-sel-make :anchor 5 :cursor 5))
                           :main 1))
          (kao--enter-insert 'insert (lambda () (goto-char 5)) #'kao-sel-min)
          (let ((start kao--insert-start)
                (sites kao--insert-secondary-sites))
            (setq kao--count 12)
            (kao-insert-one-shot)
            (kao-normal-escape)
            (let ((this-command 'kao-normal-escape)) (kao--maybe-reset-count))
            (should-not kao--insert-oneshot)
            (should kao--insert-active)
            (should (= kao--count 0))           ; ESC cleared the params
            (should (= (point) 5))
            (should (eq start kao--insert-start))
            (should (eq sites kao--insert-secondary-sites))
            (should (eq cursor-type 'bar))))
      (kao-mode -1))))

(ert-deftest kao-state-oneshot-changed-rederives-geometry ()
  "A selection-changing one shot restarts the session at the new cursors."
  (kao-state-tests--with "foo bar baz"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--enter-insert 'insert (lambda () (goto-char 1)))
          (kao-insert-one-shot)
          ;; simulate a normal command leaving two sels, main on "baz"
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 5 :cursor 7)
                                       (kao-sel-make :anchor 9 :cursor 11))
                           :main 1))
          (kao--refresh)
          (let ((this-command 'kao-word-forward)) (kao--maybe-reset-count))
          (should-not kao--insert-oneshot)
          (should kao--insert-active)
          (should (= (point) 11))               ; main cursor
          (should (= (marker-position kao--insert-start) 11))
          (should (equal (mapcar (lambda (site) (marker-position (car site)))
                                 kao--insert-secondary-sites)
                         '(7))))                ; non-main cursor
      (kao-mode -1))))

(ert-deftest kao-state-oneshot-edit-joins-session-history-node ()
  "Edits made during the one shot land in the session's single node."
  (kao-state-tests--with "hello"
    (buffer-enable-undo)
    (kao-mode 1)
    (unwind-protect
        (let ((id0 (kao-history-max-id)))
          (kao--enter-insert 'insert (lambda () (goto-char 1)))
          (insert "X")
          (kao-insert-one-shot)
          ;; a one-shot edit command: inserts "Q" and, like any real edit,
          ;; leaves the selection on its result (cursor past the insertion)
          (insert "Q")
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 3 :cursor 3))
                           :main 0))
          (kao--refresh)
          (let ((this-command 'kao-paste)) (kao--hist-maybe-commit))
          (should (= (kao-history-max-id) id0)) ; NOT committed mid-session
          (let ((this-command 'kao-paste)) (kao--maybe-reset-count))
          (insert "Y")
          (kao-insert-exit)
          (should (= (kao-history-max-id) (1+ id0))) ; exactly one node
          (should (string= (buffer-string) "XQYhello")))
      (kao-mode -1))))

(ert-deftest kao-state-oneshot-no-selection-history-node ()
  "The recorder is gated off while the one shot is pending (NestedBool)."
  (kao-state-tests--with "foo bar"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--enter-insert 'insert (lambda () (goto-char 1)))
          (kao-insert-one-shot)
          (let ((size kao--sel-history-size))
            (setq kao--sels (kao-sels-make
                             :list (list (kao-sel-make :anchor 5 :cursor 7))
                             :main 0))
            (let ((this-command 'kao-word-forward)) (kao--sel-history-record))
            (should (= kao--sel-history-size size))))
      (kao-mode -1))))

(ert-deftest kao-state-oneshot-insert-entry-continues-session ()
  "`<a-;> i' re-enters insert inside the SAME session (one undo unit)."
  (kao-state-tests--with "hello"
    (buffer-enable-undo)
    (goto-char (point-min))
    (undo-boundary)
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--enter-insert 'insert (lambda () (goto-char 1)))
          (insert "X")
          (kao-insert-one-shot)
          (let ((handle kao--insert-undo-handle))
            ;; an insert entry command runs as the one-shot command
            (kao--enter-insert 'insert (lambda () (goto-char (point))))
            (should (eq handle kao--insert-undo-handle)) ; same group kept
            (let ((this-command 'kao-insert)) (kao--maybe-reset-count))
            (should-not kao--insert-oneshot)
            (should kao--insert-active))
          (insert "Y")
          (kao-insert-exit)
          (should (string= (buffer-string) "XYhello"))
          (primitive-undo 1 buffer-undo-list)   ; still ONE logical undo
          (should (string= (buffer-string) "hello")))
      (kao-mode -1))))

(ert-deftest kao-state-oneshot-teardown-clears-flag ()
  "Disabling `kao-mode' mid-one-shot clears the pending flag."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (kao--enter-insert 'insert (lambda () (goto-char 1)))
    (kao-insert-one-shot)
    (kao-mode -1)
    (should-not kao--insert-oneshot)))

(ert-deftest kao-state-oneshot-binding ()
  "M-; in the insert state map runs `kao-insert-one-shot'."
  (should (eq (lookup-key kao-insert-state-map (kbd "M-;"))
              #'kao-insert-one-shot)))

(ert-deftest kao-state-insert-cn-cp-unbound ()
  "Insert-state C-n/C-p are UNBOUND (amending capf binding).
The insert emulation map outranks `minor-mode-overriding-map-alist', so any
binding here shadows the completion popup's own keymap — corfu cycles via
`<remap> <next-line>', which only fires when the key falls through to
native `next-line'/`previous-line'."
  (should-not (lookup-key kao-insert-state-map (kbd "C-n")))
  (should-not (lookup-key kao-insert-state-map (kbd "C-p"))))

;;;; Count reader

(ert-deftest kao-state-digit-accumulates ()
  "`kao-digit' builds a multi-digit count: 3 then 0 then 5 -> 305."
  (kao-state-tests--with "x"
    (kao-mode 1)
    (unwind-protect
        (progn
          (should (= kao--count 0))
          (let ((last-command-event ?3)) (kao-digit))
          (should (= kao--count 3))
          (let ((last-command-event ?0)) (kao-digit))
          (should (= kao--count 30))
          (let ((last-command-event ?5)) (kao-digit))
          (should (= kao--count 305)))
      (kao-mode -1))))

(ert-deftest kao-state-digit-overflow-keeps-count ()
  "A digit that would push the count past INT_MAX is discarded.
Kakoune keeps the pending count unchanged and prints \"parameter overflowed\"
\(input_handler.cc:305-310) — the count is NOT reset, the digit is simply
dropped."
  (kao-state-tests--with "x"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--count 2147483647)  ; INT_MAX already accumulated
          (cl-letf (((symbol-function 'message) #'ignore))
            (let ((last-command-event ?0)) (kao-digit)))
          (should (= kao--count 2147483647))
          ;; The pending count also survives the post-command reset: the
          ;; running command is still `kao-digit', which the reset exempts.
          (let ((this-command 'kao-digit)) (kao--maybe-reset-count))
          (should (= kao--count 2147483647)))
      (kao-mode -1))))

(ert-deftest kao-state-digit-overflow-message-exact ()
  "Overflow emits Kakoune's exact lowercase \"parameter overflowed\"."
  (kao-state-tests--with "x"
    (kao-mode 1)
    (unwind-protect
        (let ((captured nil))
          (setq kao--count 214748365)   ; *10 exceeds INT_MAX for any digit
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq captured (apply #'format fmt args)))))
            (let ((last-command-event ?9)) (kao-digit)))
          (should (equal captured "parameter overflowed"))
          (should (= kao--count 214748365)))
      (kao-mode -1))))

(ert-deftest kao-state-digit-boundary-reaches-int-max ()
  "Accumulation up to exactly INT_MAX (2147483647) is accepted."
  (kao-state-tests--with "x"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--count 214748364)
          (let ((last-command-event ?7)) (kao-digit))
          (should (= kao--count 2147483647)))
      (kao-mode -1))))

(ert-deftest kao-state-digit-leading-zero-harmless ()
  "A leading 0 keeps the count at 0, then digits build normally."
  (kao-state-tests--with "x"
    (kao-mode 1)
    (unwind-protect
        (progn
          (let ((last-command-event ?0)) (kao-digit))
          (should (= kao--count 0))
          (let ((last-command-event ?7)) (kao-digit))
          (should (= kao--count 7)))
      (kao-mode -1))))

(ert-deftest kao-state-repeat-count-defaults-to-one ()
  "`kao--repeat-count' is 1 when no count was entered, else the count."
  (kao-state-tests--with "x"
    (setq kao--count 0)
    (should (= (kao--repeat-count) 1))
    (setq kao--count 4)
    (should (= (kao--repeat-count) 4))))

;;;; Kao--count-or — count-with-default helper

(ert-deftest kao-state-count-or-picks-count-or-default ()
  "`kao--count-or' returns DEFAULT when unset (`kao--count' 0), else the count."
  (with-temp-buffer
    (setq kao--count 0)
    (should (= (kao--count-or 7) 7))       ; unset -> default
    (setq kao--count 4)
    (should (= (kao--count-or 7) 4))))     ; set -> count verbatim

(ert-deftest kao-state-repeat-short-circuits-on-nil ()
  "`kao--repeat' stops and returns nil once FN returns nil mid-count, instead
of feeding nil back into FN.  A count-3 fold whose FN returns nil on
the 2nd step returns nil and calls FN exactly twice."
  (let* ((kao--count 3)
         (calls 0)
         (fn (lambda (_s) (setq calls (1+ calls)) (if (= calls 2) nil 'step))))
    (should (null (funcall (kao--repeat fn) 'start)))
    (should (= calls 2))))

(ert-deftest kao-state-maybe-reset-clears-after-non-digit ()
  "The count resets after a non-digit command, but survives a digit command."
  (kao-state-tests--with "x"
    (setq kao--count 9)
    (let ((this-command 'kao-digit)) (kao--maybe-reset-count))
    (should (= kao--count 9))                     ; digit -> count survives
    (let ((this-command 'kao-word-forward)) (kao--maybe-reset-count))
    (should (= kao--count 0))))                   ; non-digit -> reset

;;;; Native prefix-arg count bridge

(ert-deftest kao-state-repeat-count-bridges-prefix-when-unset ()
  "With no kao count, a native `current-prefix-arg' becomes the repeat count."
  (with-temp-buffer
    (setq kao--count 0)
    (let ((current-prefix-arg 3))
      (should (= (kao--repeat-count) 3)))))

(ert-deftest kao-state-repeat-count-bridges-c-u ()
  "A bare `C-u' (raw prefix (4)) bridges to a count of 4."
  (with-temp-buffer
    (setq kao--count 0)
    (let ((current-prefix-arg '(4)))
      (should (= (kao--repeat-count) 4)))))

(ert-deftest kao-state-repeat-count-kao-digits-win-over-prefix ()
  "A typed kao count keeps absolute precedence over a native prefix arg."
  (with-temp-buffer
    (setq kao--count 2)
    (let ((current-prefix-arg 3))
      (should (= (kao--repeat-count) 2)))))

(ert-deftest kao-state-repeat-count-prefix-gated-off ()
  "With `kao-use-prefix-arg-count' nil, a native prefix arg is ignored (-> 1)."
  (with-temp-buffer
    (setq kao--count 0)
    (let ((kao-use-prefix-arg-count nil)
          (current-prefix-arg 3))
      (should (= (kao--repeat-count) 1)))))

(ert-deftest kao-state-repeat-count-no-prefix-is-one ()
  "No kao count and no prefix arg keeps the faithful default of 1."
  (with-temp-buffer
    (setq kao--count 0)
    (let ((current-prefix-arg nil))
      (should (= (kao--repeat-count) 1)))))

;;;; Selection history (recording)

(defun kao-state-tests--sels (pairs main)
  "Build a `kao-sels' from PAIRS ((anchor . cursor) ...) with MAIN as the index."
  (kao-sels-make
   :list (mapcar (lambda (p) (kao-sel-make :anchor (car p) :cursor (cdr p))) pairs)
   :main main))

(defun kao-state-tests--node (k)
  "Selection snapshot of logical ring node K (unwraps the id tag)."
  (cdr (kao--sel-history-node k)))

(defun kao-state-tests--seed-history (nodes &optional index cap)
  "Replace the selection-history ring with NODES (oldest first).
Each node is wrapped as a ((GEN . ID) . SNAP) entry tagged with the CURRENT tree
generation and history id.  INDEX is the live
logical index (default: the last node).  CAP is the ring capacity (default
`kao-sel-history-max'), letting cap/eviction tests use a small ring."
  (let ((ring (make-vector (max 2 (or cap kao-sel-history-max)) nil))
        (i 0))
    (dolist (n nodes)
      (aset ring i (cons (cons (kao-history-generation) (kao-history-current-id))
                         n))
      (setq i (1+ i)))
    (setq kao--sel-history ring
          kao--sel-history-start 0
          kao--sel-history-size (length nodes)
          kao--sel-history-index (or index (1- (length nodes))))))

(ert-deftest kao-sel-ring-accessor-roundtrip ()
  "`kao--sel-ring-*' round-trip a ((GEN . ID) . SNAP) history-ring node.
`kao--sel-ring-make' builds the node from a (GEN . ID) head and a snapshot;
the four readers reproduce the genid, snapshot, generation, and id."
  (let* ((genid (cons 7 42))
         (snap 'snap-sentinel)
         (node (kao--sel-ring-make genid snap)))
    (should (eq (kao--sel-ring-genid node) genid))
    (should (eq (kao--sel-ring-snap node) snap))
    (should (= (kao--sel-ring-gen node) 7))
    (should (= (kao--sel-ring-id node) 42))))

(ert-deftest kao-state-sel-history-seeds-at-activation ()
  "Enabling `kao-mode' seeds the history with node 0 = the initial list."
  (kao-state-tests--with "0123456789"
    (goto-char 4)
    (kao-mode 1)
    (unwind-protect
        (progn
          (should (= 1 kao--sel-history-size))
          (should (= 0 kao--sel-history-index))
          (should (equal (kao-sels-list (kao-state-tests--node 0))
                         (kao-sels-list kao--sels))))
      (kao-mode -1))))

(ert-deftest kao-state-sel-history-records-list-change ()
  "A selection-list change pushes a node and advances the index."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((3 . 5)) 0))
          (let ((this-command 'kao-word-forward)) (kao--sel-history-record))
          (should (= 2 kao--sel-history-size))
          (should (= 1 kao--sel-history-index))
          (should (equal (kao-sels-list (kao-state-tests--node 1))
                         (kao-sels-list kao--sels))))
      (kao-mode -1))))

(ert-deftest kao-state-sel-history-main-only-updates-in-place ()
  "A main-index-only change updates the current node, never pushes (== ignores main)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((1 . 1) (4 . 4)) 0))
          (kao-state-tests--seed-history
           (list (kao--snapshot-sels kao--sels)) 0)
          (setq kao--sels (kao-state-tests--sels '((1 . 1) (4 . 4)) 1))
          (let ((this-command 'kao-rotate-main)) (kao--sel-history-record))
          (should (= 1 kao--sel-history-size))          ; no push
          (should (= 0 kao--sel-history-index))
          (should (= 1 (kao-sels-main (kao-state-tests--node 0))))) ; main updated
      (kao-mode -1))))

(ert-deftest kao-state-sel-history-no-change-no-record ()
  "A command that changes nothing leaves the history untouched."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (let ((this-command 'kao-word-forward)) (kao--sel-history-record))
          (should (= 1 kao--sel-history-size))
          (should (= 0 kao--sel-history-index)))
      (kao-mode -1))))

(ert-deftest kao-state-sel-history-truncates-redo-tail ()
  "A new change after an undo drops the redo tail (tree branch-overwrite)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((1 . 1)) 0)
                 (kao-state-tests--sels '((2 . 2)) 0)
                 (kao-state-tests--sels '((3 . 3)) 0))
           1)                                    ; "undone" to node 1
          (setq kao--sels (kao-state-tests--sels '((5 . 5)) 0))
          (let ((this-command 'kao-word-forward)) (kao--sel-history-record))
          (should (= 3 kao--sel-history-size))   ; node 2 (3.3) dropped, 5.5 pushed
          (should (= 2 kao--sel-history-index))
          (should (= 5 (kao-sel-cursor
                        (car (kao-sels-list (kao-state-tests--node 2)))))))
      (kao-mode -1))))

(ert-deftest kao-state-sel-history-skips-during-insert ()
  "The recorder is a no-op while insert state is active (selection suspended)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--insert-active t)
          (setq kao--sels (kao-state-tests--sels '((7 . 7)) 0))
          (let ((this-command 'self-insert-command)) (kao--sel-history-record))
          (should (= 1 kao--sel-history-size)))
      (setq kao--insert-active nil)
      (kao-mode -1))))

(ert-deftest kao-state-sel-history-skips-buffer-undo ()
  "Buffer `u'/`U' do not push a node (Kakoune leaves undo/redo unwrapped)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((8 . 8)) 0))
          (let ((this-command 'kao-undo)) (kao--sel-history-record))
          (should (= 1 kao--sel-history-size))
          (let ((this-command 'kao-redo)) (kao--sel-history-record))
          (should (= 1 kao--sel-history-size)))
      (kao-mode -1))))

;;;; Selection history (undo/redo commands)

(ert-deftest kao-state-sel-undo-restores-previous ()
  "`<a-u>' steps back one node, restoring its list and main index."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((1 . 1) (4 . 4)) 1)
                 (kao-state-tests--sels '((3 . 5)) 0))
           1)
          (setq kao--sels (kao--snapshot-sels (kao-state-tests--node 1)))
          (kao-sel-undo)
          (should (= 0 kao--sel-history-index))
          (should (equal (kao-sels-list kao--sels)
                         (kao-sels-list (kao-state-tests--node 0))))
          (should (= 1 (kao-sels-main kao--sels))))
      (kao-mode -1))))

(ert-deftest kao-state-sel-undo-clamps-after-shrink ()
  "`<a-u>' after a buffer-shrinking edit restores CLAMPED positions.
documents \"snapshots are integer positions clamped on restore\" — the
restore must clamp like the paths, not hand back out-of-range
integers."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((9 . 9)) 0)
                 (kao-state-tests--sels '((2 . 2)) 0))
           1)
          (setq kao--sels (kao--snapshot-sels (kao-state-tests--node 1)))
          (delete-region 4 (point-max))  ; point-max 4, last on-char pos 3
          (kao-sel-undo)
          (let ((s (car (kao-sels-list kao--sels))))
            (should (= 3 (kao-sel-anchor s)))
            (should (= 3 (kao-sel-cursor s)))))
      (kao-mode -1))))

(ert-deftest kao-state-sel-undo-clamp-leaves-node-verbatim ()
  "The restore clamp never mutates the STORED node, and the restored copy
stays decoupled from it (a later buffer regrowth must see the original
positions verbatim)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((9 . 9)) 0)
                 (kao-state-tests--sels '((2 . 2)) 0))
           1)
          (setq kao--sels (kao--snapshot-sels (kao-state-tests--node 1)))
          (delete-region 4 (point-max))
          (kao-sel-undo)
          ;; Stored node keeps 9 (NOT clamped in place).
          (let ((stored (car (kao-sels-list (kao-state-tests--node 0)))))
            (should (= 9 (kao-sel-anchor stored)))
            (should (= 9 (kao-sel-cursor stored))))
          ;; Mutating the live copy does not touch the node (decoupling).
          (setf (kao-sel-cursor (car (kao-sels-list kao--sels))) 1)
          (should (= 9 (kao-sel-cursor
                        (car (kao-sels-list (kao-state-tests--node 0)))))))
      (kao-mode -1))))

(ert-deftest kao-state-sel-redo-restores-next ()
  "`<a-U>' steps forward one node."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((1 . 1)) 0)
                 (kao-state-tests--sels '((3 . 5)) 0))
           0)
          (setq kao--sels (kao--snapshot-sels (kao-state-tests--node 0)))
          (kao-sel-redo)
          (should (= 1 kao--sel-history-index))
          (should (equal (kao-sels-list kao--sels)
                         (kao-sels-list (kao-state-tests--node 1)))))
      (kao-mode -1))))

(ert-deftest kao-state-sel-undo-count ()
  "A count steps back that many nodes (2<a-u> over three nodes)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((1 . 1)) 0)
                 (kao-state-tests--sels '((2 . 2)) 0)
                 (kao-state-tests--sels '((3 . 3)) 0))
           2)
          (setq kao--count 2)
          (kao-sel-undo)
          (should (= 0 kao--sel-history-index))
          (should (= 1 (kao-sel-cursor (car (kao-sels-list kao--sels))))))
      (kao-mode -1))))

(ert-deftest kao-state-sel-undo-errors-at-seed ()
  "`<a-u>' at node 0 signals a user-error with Kakoune's wording."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((1 . 1)) 0)) 0)
          (should-error (kao-sel-undo) :type 'user-error))
      (kao-mode -1))))

(ert-deftest kao-state-sel-redo-errors-at-tip ()
  "`<a-U>' at the newest node signals a user-error."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((1 . 1)) 0)
                 (kao-state-tests--sels '((3 . 3)) 0))
           1)
          (should-error (kao-sel-redo) :type 'user-error))
      (kao-mode -1))))

(ert-deftest kao-state-sel-undo-redo-round-trips ()
  "Undo then redo returns to the same node."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((1 . 1)) 0)
                 (kao-state-tests--sels '((6 . 6)) 0))
           1)
          (setq kao--sels (kao--snapshot-sels (kao-state-tests--node 1)))
          (kao-sel-undo)
          (should (= 0 kao--sel-history-index))
          (kao-sel-redo)
          (should (= 1 kao--sel-history-index))
          (should (= 6 (kao-sel-cursor (car (kao-sels-list kao--sels))))))
      (kao-mode -1))))

(ert-deftest kao-state-sel-history-restore-is-decoupled ()
  "Restoring copies the node; mutating `kao--sels' after does not corrupt history."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((1 . 1)) 0)
                 (kao-state-tests--sels '((4 . 4)) 0))
           1)
          (kao-sel-undo)                        ; restores node 0 into kao--sels
          (setf (kao-sel-cursor (car (kao-sels-list kao--sels))) 9)
          (should (= 1 (kao-sel-cursor
                        (car (kao-sels-list (kao-state-tests--node 0)))))))
      (kao-mode -1))))

(ert-deftest kao-state-sel-undo-redo-bindings ()
  "`M-u'/`M-U' resolve to the selection undo/redo commands."
  (should (eq (lookup-key kao-normal-state-map (kbd "M-u")) #'kao-sel-undo))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-U")) #'kao-sel-redo)))

;;;; Selection history (ring cap / wraparound)

(defun kao-state-tests--record-change (pairs)
  "Set `kao--sels' to PAIRS and run the recorder as a list-changing command."
  (setq kao--sels (kao-state-tests--sels pairs 0))
  (let ((this-command 'kao-word-forward)) (kao--sel-history-record)))

(defun kao-state-tests--nav-then-record (fn cmd)
  "Run navigation FN as command CMD, then the post-command recorder.
Simulates the real buffer-local `post-command-hook' order — the command runs,
then `kao--sel-history-record' fires with `this-command' bound to CMD (the
sequence that exposes)."
  (let ((this-command cmd))
    (funcall fn)
    (kao--sel-history-record)))

;;;; Selection history + jumps (coordinate translation on restore)

(ert-deftest kao-state-sel-undo-survives-post-command-recorder ()
  "The recorder that fires AFTER `<a-u>'/`<a-U>' must not truncate
the ring or freeze the index once a buffer edit has advanced the history id.
Record two selection nodes, delete the text under them (so the recorded frames
differ from the live buffer), then run the FULL post-command sequence
\(navigation + recorder as its own command): `<a-u> <a-u>' walks the index
3->2->1 back onto both recorded nodes and `<a-U> <a-U>' returns to the tip,
while `kao--sel-history-size' stays 4 the whole way — no spurious truncation."
  (kao-state-tests--with "0123456789abcdef"
    (kao-mode 1)
    (unwind-protect
        (progn
          ;; Two recorded selection nodes at id 0 (nodes 1 and 2 after the seed).
          (kao-state-tests--record-change '((11 . 13)))   ; node 1
          (kao-state-tests--record-change '((1 . 3)))     ; node 2
          ;; `d': delete "012" (positions 1..3), commit as a tree node (id -> 1),
          ;; then the recorder records the collapsed selection as node 3.
          (delete-region 1 4)
          (kao-history-commit-pending)
          (kao-state-tests--nav-then-record
           (lambda () (setq kao--sels (kao-state-tests--sels '((1 . 1)) 0)))
           'kao-delete)
          (should (= 4 kao--sel-history-size))
          (should (= 3 kao--sel-history-index))
          ;; <a-u>: back to node 2 — the recorder must NOT truncate/freeze here.
          (kao-state-tests--nav-then-record #'kao-sel-undo 'kao-sel-undo)
          (should (= 2 kao--sel-history-index))
          (should (= 4 kao--sel-history-size))
          ;; <a-u>: back to node 1 (the (11..13) node -> (8..10) after the delete).
          (kao-state-tests--nav-then-record #'kao-sel-undo 'kao-sel-undo)
          (should (= 1 kao--sel-history-index))
          (should (= 4 kao--sel-history-size))
          (let ((s (car (kao-sels-list kao--sels))))
            (should (= 8 (kao-sel-anchor s)))
            (should (= 10 (kao-sel-cursor s))))
          ;; <a-U> <a-U>: return forward to the tip, still no truncation.
          (kao-state-tests--nav-then-record #'kao-sel-redo 'kao-sel-redo)
          (should (= 2 kao--sel-history-index))
          (kao-state-tests--nav-then-record #'kao-sel-redo 'kao-sel-redo)
          (should (= 3 kao--sel-history-index))
          (should (= 4 kao--sel-history-size)))
      (kao-mode -1))))

(ert-deftest kao-state-sel-history-restore-reframes-node-on-access ()
  "Re-frame half: a cross-frame `<a-u>' restore rewrites the
stored ring node to the current history id (`update()'-on-access), so a
following NON-navigation command's recorder sees no value-diff and neither
truncates the ring nor advances the index off the restored node."
  (kao-state-tests--with "0123456789abcdef"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--record-change '((11 . 13)))   ; node 1 @ id 0
          (kao-state-tests--record-change '((1 . 3)))     ; node 2 @ id 0
          (delete-region 1 4)                             ; edit under them
          (kao-history-commit-pending)                    ; -> id 1
          (kao-state-tests--nav-then-record
           (lambda () (setq kao--sels (kao-state-tests--sels '((1 . 1)) 0)))
           'kao-delete)                                   ; node 3 @ id 1, idx 3
          (kao-sel-undo)                                  ; -> node 2, translated + re-framed
          (should (= 2 kao--sel-history-index))
          ;; The stored node is re-framed to (GEN . current-id) (recorded @ 0);
          ;; the generation stamp travels with the id.
          (should (equal (car (kao--sel-history-node 2))
                         (cons (kao-history-generation) 1)))
          ;; An ordinary command's recorder now fires (NOT skip-listed) yet
          ;; sees no change: size stays 4, index stays on the restored node.
          (let ((this-command 'kao-word-forward)) (kao--sel-history-record))
          (should (= 4 kao--sel-history-size))
          (should (= 2 kao--sel-history-index)))
      (kao-mode -1))))

(ert-deftest kao-state-history-nav-records-no-phantom-node ()
  "`<c-j>'/`<c-k>' (`move_in_history') must push NO selection-history
node — Kakoune assigns via `selections_write_only' exactly like u/U
\(normal.cc:2210), recording nothing.  After a committed edit + selection move +
`kao-undo', running `kao-history-forward' as its own command leaves the ring
size unchanged, and a following `kao-sel-undo' steps to the previous REAL
selection change, not a phantom the navigation invented."
  (kao-state-tests--with "aaa bbb"
    (kao-mode 1)
    (unwind-protect
        (progn
          ;; A committed edit gives a forward history node to jump to.
          (goto-char 1) (insert "X")            ; "Xaaa bbb"
          (kao-history-commit-pending)          ; buffer history current=1 max=1
          ;; Move the selection -> a real selection-history node (size 2).
          (kao-state-tests--record-change '((5 . 5)))
          (should (= 2 kao--sel-history-size))
          (should (= 1 kao--sel-history-index))
          ;; Buffer undo already skips the recorder.
          (kao-state-tests--nav-then-record #'kao-undo 'kao-undo)
          (should (= 2 kao--sel-history-size))
          ;; <c-j>: move_in_history forward, recorder fires as kao-history-forward.
          ;; MUST record no phantom selection-history node.
          (kao-state-tests--nav-then-record #'kao-history-forward 'kao-history-forward)
          (should (= 2 kao--sel-history-size))   ; no phantom node pushed
          (should (= 1 kao--sel-history-index))
          ;; A following <a-u> steps to the previous REAL node (the seed).
          (kao-sel-undo)
          (should (= 0 kao--sel-history-index)))
      (kao-mode -1))))

(ert-deftest kao-state-history-nav-fires-change-hook-but-records-no-node ()
  "`<c-j>'/`<c-k>' fire the change hook yet record no selection-history node.
x: `kao-history-forward'/`kao-history-backward' route
through `kao-history-goto' into `kao--hist-select-ranges', which INSTALLS the
restored selections and so MUST fire `kao-selection-change-hook',
while the recorder stays inert — the `memq' skip list plus the
`kao--sel-history-inhibit' that `kao-history-goto' let-binds — so
the ring gains no node.  Drives both directions across a committed edit."
  (kao-state-tests--with "aaa bbb"
    (kao-mode 1)
    (unwind-protect
        (let ((fires 0))
          ;; Two committed edits give ids 1 and 2 to move between.
          (goto-char 1) (insert "X") (kao-history-commit-pending)   ; -> id 1
          (goto-char 1) (insert "Y") (kao-history-commit-pending)   ; -> id 2 (current)
          ;; A real selection move records one node past the seed (size 2).
          (kao-state-tests--record-change '((5 . 5)))
          (should (= 2 kao--sel-history-size))
          (should (= 1 kao--sel-history-index))
          (add-hook 'kao-selection-change-hook
                    (lambda () (setq fires (1+ fires))) nil t)
          ;; <c-k>: move_in_history back to id 1 installs the reverted-frame
          ;; selection (fires the hook) but records no phantom node.
          (kao-state-tests--nav-then-record #'kao-history-backward
                                            'kao-history-backward)
          (should (= 1 fires))                 ; fire happened
          (should (= 2 kao--sel-history-size)) ; recorder inert
          (should (= 1 kao--sel-history-index))
          ;; <c-j>: move_in_history forward to id 2 — both halves, other way.
          (kao-state-tests--nav-then-record #'kao-history-forward
                                            'kao-history-forward)
          (should (= 2 fires))
          (should (= 2 kao--sel-history-size))
          (should (= 1 kao--sel-history-index)))
      (kao-mode -1))))

(ert-deftest kao-state-sel-undo-translates-through-edits ()
  "`<a-u>' translates the restored node through edits made since it was
recorded (the restore's `update()', context.cc:195), and `<a-U>'
translates equally on the way forward."
  (kao-state-tests--with "abcdef"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--record-change '((3 . 4)))   ; node 1 @ id 0: "cd"
          (save-excursion (goto-char 1) (insert "XX"))  ; "XXabcdef"
          (kao-history-commit-pending)                  ; -> id 1
          (kao-state-tests--record-change '((1 . 1)))   ; node 2 @ id 1
          (kao-sel-undo)                                ; node 1, translated
          (should (equal (mapcar (lambda (s) (cons (kao-sel-anchor s)
                                                   (kao-sel-cursor s)))
                                 (kao-sels-list kao--sels))
                         '((5 . 6))))                   ; still "cd"
          (kao-sel-undo)                                ; node 0 (seed @ id 0)
          (should (= 3 (kao-sel-cursor (car (kao-sels-list kao--sels)))))
          (kao-sel-redo)                                ; back to node 1
          (should (= 6 (kao-sel-cursor (car (kao-sels-list kao--sels))))))
      (kao-mode -1))))

(ert-deftest kao-state-sel-undo-gcd-id-falls-back-to-clamp ()
  "`<a-u>' to a node whose history id was gc'd restores clamped, no error.
The documented residual: `kao-history-max-nodes' dropped the id's tree
node, so the path fold is unavailable and the restore clamps (+ merges)."
  (kao-state-tests--with "abcdef"
    (kao-mode 1)
    (unwind-protect
        (let ((kao-history-max-nodes 2))
          (kao-state-tests--record-change '((3 . 4)))   ; node @ id 0
          (dotimes (i 3)                                ; ids 1..3; id 0 gc'd
            (save-excursion (goto-char 1) (insert (format "%d" i)))
            (kao-history-commit-pending))
          (should (null (kao--hist-node 0)))
          (kao-state-tests--record-change '((1 . 1)))
          (kao-sel-undo)
          ;; Fallback: original integers clamped (buffer grew, so verbatim).
          (should (= 4 (kao-sel-cursor (car (kao-sels-list kao--sels))))))
      (kao-mode -1))))

(ert-deftest kao-state-jump-translates-through-edits ()
  "`<c-o>' translates the restored jump through edits made since the save
\(the jump list's `update()' on get, context.cc:126/150)."
  (kao-state-tests--with "abcdef"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--jumps nil kao--jump-current 0
                kao--sels (kao-state-tests--sels '((3 . 4)) 0))
          (kao-jump-save)                               ; entry @ id 0
          (save-excursion (goto-char 1) (insert "XX"))
          (kao-history-commit-pending)                  ; -> id 1
          (setq kao--sels (kao-state-tests--sels '((1 . 1)) 0))
          (kao-jump-backward)
          (should (equal (mapcar (lambda (s) (cons (kao-sel-anchor s)
                                                   (kao-sel-cursor s)))
                                 (kao-sels-list kao--sels))
                         '((5 . 6)))))                  ; still "cd"
      (kao-mode -1))))

(ert-deftest kao-state-sel-history-cap-evicts-oldest ()
  "A full ring evicts the oldest node and keeps recording O(1)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((1 . 1)) 0)) 0 3) ; cap 3
          (kao-state-tests--record-change '((2 . 2)))
          (kao-state-tests--record-change '((3 . 3)))
          (should (= 3 kao--sel-history-size))
          (kao-state-tests--record-change '((4 . 4)))   ; full: (1.1) evicted
          (should (= 3 kao--sel-history-size))
          (should (= 2 kao--sel-history-index))
          (should (= 2 (kao-sel-cursor
                        (car (kao-sels-list (kao-state-tests--node 0))))))
          (should (= 4 (kao-sel-cursor
                        (car (kao-sels-list (kao-state-tests--node 2)))))))
      (kao-mode -1))))

(ert-deftest kao-state-sel-history-wraparound-walk ()
  "Undo walks correct nodes across the physical ring seam after eviction."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((1 . 1)) 0)) 0 3)
          (dolist (c '(2 3 4 5))                       ; two evictions: keep 3 4 5
            (kao-state-tests--record-change (list (cons c c))))
          (should (= 3 kao--sel-history-size))
          (kao-sel-undo)
          (should (= 4 (kao-sel-cursor (car (kao-sels-list kao--sels)))))
          (kao-sel-undo)
          (should (= 3 (kao-sel-cursor (car (kao-sels-list kao--sels)))))
          (kao-sel-redo)
          (should (= 4 (kao-sel-cursor (car (kao-sels-list kao--sels))))))
      (kao-mode -1))))

(ert-deftest kao-state-sel-undo-bottoms-out-at-oldest-retained ()
  "After eviction, `<a-u>' errors at the oldest RETAINED node (cap deviation)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((1 . 1)) 0)) 0 2)
          (kao-state-tests--record-change '((2 . 2)))
          (kao-state-tests--record-change '((3 . 3)))  ; (1.1) evicted
          (kao-sel-undo)                               ; -> (2.2), index 0
          (should (= 2 (kao-sel-cursor (car (kao-sels-list kao--sels)))))
          (should-error (kao-sel-undo) :type 'user-error))
      (kao-mode -1))))

(ert-deftest kao-state-sel-history-truncate-then-push-on-wrapped-ring ()
  "Redo-tail truncation works when the live window already wraps the seam."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((1 . 1)) 0)) 0 3)
          (dolist (c '(2 3 4))                         ; window wraps: 2 3 4
            (kao-state-tests--record-change (list (cons c c))))
          (kao-sel-undo)                               ; live = (3.3), redo = (4.4)
          (kao-state-tests--record-change '((7 . 7)))  ; truncates (4.4), pushes
          (should (= 3 kao--sel-history-size))
          (should (= 2 kao--sel-history-index))
          (should (= 7 (kao-sel-cursor
                        (car (kao-sels-list (kao-state-tests--node 2))))))
          (should-error (kao-sel-redo) :type 'user-error))
      (kao-mode -1))))

(ert-deftest kao-state-sel-history-main-only-update-on-wrapped-node ()
  "A main-only change updates the live node in place across the ring seam."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao-state-tests--seed-history
           (list (kao-state-tests--sels '((1 . 1) (4 . 4)) 0)) 0 2)
          (kao-state-tests--record-change '((2 . 2) (5 . 5)))
          (kao-state-tests--record-change '((3 . 3) (6 . 6))) ; evict + wrap
          (setq kao--sels (kao-state-tests--sels '((3 . 3) (6 . 6)) 1))
          (let ((this-command 'kao-rotate-main)) (kao--sel-history-record))
          (should (= 2 kao--sel-history-size))                 ; no push
          (should (= 1 (kao-sels-main
                        (kao-state-tests--node kao--sel-history-index)))))
      (kao-mode -1))))

;;;; Jump list (cross-buffer — global (BUFFER . SNAPSHOT) entries)

(defun kao-state-tests--jump-entry (spec main)
  "A jump-list entry for SPEC/MAIN tagged with the current buffer and (GEN . ID).
The tree generation + history id stamp mirrors production
\."
  (cons (current-buffer)
        (cons (cons (kao-history-generation) (kao-history-current-id))
              (kao-state-tests--sels spec main))))

(ert-deftest kao-state-jump-save-pushes ()
  "`<c-s>' pushes the current selections; current points past the end."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--jumps nil kao--jump-current 0
                kao--sels (kao-state-tests--sels '((4 . 4)) 0))
          (kao-jump-save)
          (should (= 1 (length kao--jumps)))
          (should (= 1 kao--jump-current))
          (should (= 4 (kao-sel-cursor (car (kao-sels-list (cddr (car kao--jumps))))))))
      (kao-mode -1))))

(ert-deftest kao-state-jump-push-is-public-and-silent ()
  "`kao-jump-push' pushes onto the jump list with NO echo-area message —
the wrapper-safe form (`kao-jump-save' keeps the \"saved N\" message)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (let ((messaged nil))
          (setq kao--jumps nil kao--jump-current 0
                kao--sels (kao-state-tests--sels '((4 . 4)) 0))
          (cl-letf (((symbol-function 'message)
                     (lambda (&rest args) (when (car args) (setq messaged t)))))
            (kao-jump-push))
          (should-not messaged)
          (should (= 1 (length kao--jumps)))
          (should (= 4 (kao-sel-cursor
                        (car (kao-sels-list (cddr (car kao--jumps))))))))
      (kao-mode -1))))

(ert-deftest kao-state-jump-push-legacy-alias ()
  "The private `kao--jump-push' name survives as a compatibility alias so no
in-tree caller breaks after the promotion."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (should (fboundp 'kao--jump-push))
          (setq kao--jumps nil kao--jump-current 0
                kao--sels (kao-state-tests--sels '((4 . 4)) 0))
          (kao--jump-push)
          (should (= 1 (length kao--jumps))))
      (kao-mode -1))))

(ert-deftest kao-state-jumplist-push-dedups ()
  "Pushing an equal jump drops the old one and moves it to the end.
Dedup compares the selection LIST ignoring the main index (Kakoune `operator==',
selection.hh:147): a same-list/different-main push still dedups, and the appended
copy carries the new main."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--jumps nil kao--jump-current 0)
          (kao--jumplist-push (kao-state-tests--sels '((1 . 1) (3 . 3)) 0))
          (kao--jumplist-push (kao-state-tests--sels '((5 . 5)) 0))
          ;; same list as the first jump, but main 1 -> must still dedup it
          (kao--jumplist-push (kao-state-tests--sels '((1 . 1) (3 . 3)) 1))
          (should (= 2 (length kao--jumps)))      ; not 3 — the first was deduped
          (should (= 2 kao--jump-current))
          (should (= 5 (kao-sel-cursor (car (kao-sels-list (cddr (nth 0 kao--jumps)))))))
          (should (= 1 (kao-sel-cursor (car (kao-sels-list (cddr (nth 1 kao--jumps)))))))
          (should (eq (current-buffer) (car (nth 1 kao--jumps)))) ; buffer-tagged
          (should (= 1 (kao-sels-main (cddr (nth 1 kao--jumps))))))  ; carries the new main
      (kao-mode -1))))

(ert-deftest kao-state-jumplist-push-truncates-forward ()
  "Pushing while a jump is in view drops the forward tail."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--jumps (list (kao-state-tests--jump-entry '((1 . 1)) 0)
                                 (kao-state-tests--jump-entry '((2 . 2)) 0)
                                 (kao-state-tests--jump-entry '((3 . 3)) 0))
                kao--jump-current 1)
          (kao--jumplist-push (kao-state-tests--sels '((5 . 5)) 0))
          (should (= 3 (length kao--jumps)))      ; [A,B,D] — C dropped
          (should (= 3 kao--jump-current))
          (should (= 5 (kao-sel-cursor (car (kao-sels-list (cddr (nth 2 kao--jumps))))))))
      (kao-mode -1))))

(ert-deftest kao-state-jump-backward-from-present ()
  "`<c-o>' from the present pushes the current selections, then lands on the prior jump."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--jumps (list (kao-state-tests--jump-entry '((1 . 1)) 0))
                kao--jump-current 1
                kao--sels (kao-state-tests--sels '((5 . 5)) 0))
          (kao-jump-backward)
          (should (= 0 kao--jump-current))
          (should (= 1 (kao-sel-cursor (car (kao-sels-list kao--sels)))))
          (should (= 2 (length kao--jumps))))     ; current selections were pushed
      (kao-mode -1))))

(ert-deftest kao-state-jump-forward-after-backward ()
  "`<c-i>' after a backward returns to the more-recent jump."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--jumps (list (kao-state-tests--jump-entry '((1 . 1)) 0)
                                 (kao-state-tests--jump-entry '((5 . 5)) 0))
                kao--jump-current 0
                kao--sels (kao-state-tests--sels '((1 . 1)) 0))
          (kao-jump-forward)
          (should (= 1 kao--jump-current))
          (should (= 5 (kao-sel-cursor (car (kao-sels-list kao--sels))))))
      (kao-mode -1))))

(ert-deftest kao-state-jump-forward-at-present-errors ()
  "`<c-i>' with no jump in view signals a user-error."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--jumps (list (kao-state-tests--jump-entry '((1 . 1)) 0))
                kao--jump-current 1)
          (should-error (kao-jump-forward) :type 'user-error))
      (kao-mode -1))))

(ert-deftest kao-state-jump-backward-at-root-errors ()
  "`<c-o>' with nothing behind signals a user-error (after pushing current)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--jumps nil kao--jump-current 0
                kao--sels (kao-state-tests--sels '((5 . 5)) 0))
          (should-error (kao-jump-backward) :type 'user-error))
      (kao-mode -1))))

(ert-deftest kao-state-jump-backward-count ()
  "A count steps back that many jumps (plus the pushed current)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--jumps (list (kao-state-tests--jump-entry '((1 . 1)) 0)
                                 (kao-state-tests--jump-entry '((2 . 2)) 0)
                                 (kao-state-tests--jump-entry '((3 . 3)) 0))
                kao--jump-current 3
                kao--sels (kao-state-tests--sels '((9 . 9)) 0))
          (setq kao--count 2)
          (kao-jump-backward)
          (should (= 1 kao--jump-current))
          (should (= 2 (kao-sel-cursor (car (kao-sels-list kao--sels))))))
      (kao-mode -1))))

(ert-deftest kao-state-jump-restore-is-decoupled ()
  "Restoring copies the jump; a later edit to `kao--sels' leaves the stored jump intact.
Also exercises the should-not-push path (the current jump equals the live selections)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--jumps (list (kao-state-tests--jump-entry '((1 . 1)) 0)
                                 (kao-state-tests--jump-entry '((5 . 5)) 0))
                kao--jump-current 1
                kao--sels (kao-state-tests--sels '((5 . 5)) 0))
          (kao-jump-backward)                     ; jumps[1]==live -> no push; lands on (1)
          (should (= 2 (length kao--jumps)))      ; nothing pushed
          (should (= 1 (kao-sel-cursor (car (kao-sels-list kao--sels)))))
          (setf (kao-sel-cursor (car (kao-sels-list kao--sels))) 9)
          (should (= 1 (kao-sel-cursor (car (kao-sels-list (cddr (nth 0 kao--jumps))))))))
      (kao-mode -1))))

(ert-deftest kao-state-jump-bindings ()
  "`C-s'/`C-o'/`C-i' resolve to the jump-list commands."
  (should (eq (lookup-key kao-normal-state-map (kbd "C-s")) #'kao-jump-save))
  (should (eq (lookup-key kao-normal-state-map (kbd "C-o")) #'kao-jump-backward))
  (should (eq (lookup-key kao-normal-state-map (kbd "C-i")) #'kao-jump-forward)))

;;;; Cross-buffer jump list (— resolves the deferral)

(defmacro kao-state-tests--with-two-kao-buffers (&rest body)
  "Run BODY with `a' and `b' bound to two live kao-mode buffers, `a' current.
The global jump list is reset before BODY and cleaned up (with both buffers)
afterwards.  Anaphoric on purpose: tests refer to `a'/`b' directly."
  (declare (indent 0))
  `(let ((a (generate-new-buffer " *kao-jump-a*"))
         (b (generate-new-buffer " *kao-jump-b*")))
     (unwind-protect
         (progn
           (with-current-buffer a (insert "aaaaaaaaaa") (kao-mode 1))
           (with-current-buffer b (insert "bbbbbbbbbb") (kao-mode 1))
           (setq kao--jumps nil kao--jump-current 0)
           (switch-to-buffer a)
           (ignore b)
           ,@body)
       (setq kao--jumps nil kao--jump-current 0)
       (when (buffer-live-p a)
         (with-current-buffer a (kao-mode -1)) (kill-buffer a))
       (when (buffer-live-p b)
         (with-current-buffer b (kao-mode -1)) (kill-buffer b)))))

(ert-deftest kao-state-jump-cross-buffer-backward-switches ()
  "`<c-o>' onto a jump saved in another buffer switches to that buffer.
Faithful to the jump SelectionList carrying its buffer + the caller
change_buffer (context.cc:135-155)."
  (kao-state-tests--with-two-kao-buffers
    (setq kao--sels (kao-state-tests--sels '((2 . 2)) 0))
    (kao--jump-push)                              ; entry tagged A
    (switch-to-buffer b)
    (setq kao--sels (kao-state-tests--sels '((3 . 3)) 0))
    (kao-jump-backward)                           ; pushes B's current, lands on A's
    (should (eq (current-buffer) a))
    (should (= 2 (kao-sel-cursor (car (kao-sels-list kao--sels)))))
    (should (= 0 kao--jump-current))))

(ert-deftest kao-state-jump-cross-buffer-forward-returns ()
  "`<c-i>' after a cross-buffer `<c-o>' returns to the original buffer."
  (kao-state-tests--with-two-kao-buffers
    (setq kao--sels (kao-state-tests--sels '((2 . 2)) 0))
    (kao--jump-push)
    (switch-to-buffer b)
    (setq kao--sels (kao-state-tests--sels '((3 . 3)) 0))
    (kao-jump-backward)                           ; now in A
    (kao-jump-forward)                            ; back to B's pushed jump
    (should (eq (current-buffer) b))
    (should (= 3 (kao-sel-cursor (car (kao-sels-list kao--sels)))))))

(ert-deftest kao-state-jump-killed-buffer-pruned ()
  "A killed buffer's entries are pruned lazily with faithful index adjustment
(`forget_buffer', context.cc:157-173); navigation never lands on them."
  (kao-state-tests--with-two-kao-buffers
    (setq kao--jumps (list (kao-state-tests--jump-entry '((1 . 1)) 0)
                           (with-current-buffer b (kao-state-tests--jump-entry '((2 . 2)) 0))
                           (kao-state-tests--jump-entry '((3 . 3)) 0))
          kao--jump-current 3)
    (kill-buffer b)
    (setq kao--sels (kao-state-tests--sels '((5 . 5)) 0))
    (kao-jump-backward)                           ; prune B -> [A1 A3], push A5, step
    (should (= 3 (length kao--jumps)))            ; A1 A3 A5
    (should (cl-every (lambda (e) (eq (car e) a)) kao--jumps))
    (should (= 3 (kao-sel-cursor (car (kao-sels-list kao--sels)))))))

(ert-deftest kao-state-jump-prune-adjusts-index-before-current ()
  "Pruning a dead entry BEFORE the current index pulls the index back one
\(`forget_buffer' first branch, context.cc:163-164), keeping the same jump
in view."
  (kao-state-tests--with-two-kao-buffers
    (setq kao--jumps (list (with-current-buffer b (kao-state-tests--jump-entry '((2 . 2)) 0))
                           (kao-state-tests--jump-entry '((1 . 1)) 0)
                           (kao-state-tests--jump-entry '((3 . 3)) 0))
          kao--jump-current 2)                    ; viewing A3
    (kill-buffer b)
    (kao--jumplist-prune)
    (should (= 2 (length kao--jumps)))
    (should (= 1 kao--jump-current))              ; pulled back one
    (should (= 3 (kao-sel-cursor                  ; same jump still in view
                  (car (kao-sels-list (cddr (nth kao--jump-current kao--jumps)))))))))

(ert-deftest kao-state-jump-prune-current-on-dead-goes-to-end ()
  "Pruning the entry the current index points AT sends the index to the new
end — \"at the present\" (`forget_buffer' second branch, context.cc:165-166);
`<c-i>' then has no next jump."
  (kao-state-tests--with-two-kao-buffers
    (setq kao--jumps (list (kao-state-tests--jump-entry '((1 . 1)) 0)
                           (with-current-buffer b (kao-state-tests--jump-entry '((2 . 2)) 0))
                           (kao-state-tests--jump-entry '((3 . 3)) 0))
          kao--jump-current 1)                    ; viewing the B jump
    (kill-buffer b)
    (kao--jumplist-prune)
    (should (= 2 (length kao--jumps)))
    (should (= 2 kao--jump-current))              ; at the present
    (setq kao--sels (kao-state-tests--sels '((5 . 5)) 0))
    (should-error (kao-jump-forward) :type 'user-error)))

(ert-deftest kao-state-jump-dedup-is-per-buffer ()
  "Equal selection lists in DIFFERENT buffers are distinct jumps; dedup only
collapses a same-buffer same-list push."
  (kao-state-tests--with-two-kao-buffers
    (kao--jumplist-push (kao-state-tests--sels '((1 . 1)) 0))   ; tagged A
    (switch-to-buffer b)
    (kao--jumplist-push (kao-state-tests--sels '((1 . 1)) 0))   ; tagged B — kept
    (should (= 2 (length kao--jumps)))
    (kao--jumplist-push (kao-state-tests--sels '((1 . 1)) 0))   ; B again — dedups
    (should (= 2 (length kao--jumps)))
    (should (eq (car (nth 0 kao--jumps)) a))
    (should (eq (car (nth 1 kao--jumps)) b))))

(ert-deftest kao-state-jump-cross-buffer-count ()
  "A count steps across a mixed-buffer list and lands in the right buffer."
  (kao-state-tests--with-two-kao-buffers
    (setq kao--jumps (list (kao-state-tests--jump-entry '((1 . 1)) 0)
                           (with-current-buffer b (kao-state-tests--jump-entry '((2 . 2)) 0))
                           (kao-state-tests--jump-entry '((3 . 3)) 0))
          kao--jump-current 3)
    (switch-to-buffer b)
    (setq kao--sels (kao-state-tests--sels '((9 . 9)) 0))
    (setq kao--count 2)
    (kao-jump-backward)                           ; push B9, step 3 -> entry #1 (B)
    (should (eq (current-buffer) b))
    (should (= 2 (kao-sel-cursor (car (kao-sels-list kao--sels)))))
    (should (= 1 kao--jump-current))))

(ert-deftest kao-state-jump-backward-push-tags-pushing-buffer ()
  "The current-selections push that `<c-o>' makes is tagged with the buffer
the command ran in."
  (kao-state-tests--with-two-kao-buffers
    (setq kao--sels (kao-state-tests--sels '((2 . 2)) 0))
    (kao--jump-push)                              ; A
    (switch-to-buffer b)
    (setq kao--sels (kao-state-tests--sels '((3 . 3)) 0))
    (kao-jump-backward)
    (should (eq (car (car (last kao--jumps))) b))))

(ert-deftest kao-state-jump-target-not-kao-mode-errors ()
  "A jump into a live buffer where the user disabled `kao-mode' aborts loudly
(slice-10 plan decision: no silent re-enable).  The pre-step push of the
current selections remains (it happened before validation, as in Kakoune's
backward); buffer and live selections are untouched."
  (kao-state-tests--with-two-kao-buffers
    (setq kao--sels (kao-state-tests--sels '((2 . 2)) 0))
    (kao--jump-push)                              ; A entry
    (with-current-buffer a (kao-mode -1))         ; user turns kao off in A
    (switch-to-buffer b)
    (setq kao--sels (kao-state-tests--sels '((3 . 3)) 0))
    (should-error (kao-jump-backward) :type 'user-error)
    (should (eq (current-buffer) b))
    (should (= 3 (kao-sel-cursor (car (kao-sels-list kao--sels)))))))

;;;; Buffer history tree integration

(ert-deftest kao-state-hist-seeds-root ()
  "Enabling `kao-mode' seeds the history tree at the root node."
  (kao-state-tests--with ""
    (kao-mode 1)
    (should (= (kao-history-current-id) 0))
    (should (= (kao-history-max-id) 0))
    (kao-mode -1)))

(ert-deftest kao-state-hist-one-node-per-command ()
  "Each edit + command boundary (`kao--hist-maybe-commit') is one node."
  (kao-state-tests--with ""
    (kao-mode 1)
    (insert "X") (kao--hist-maybe-commit)        ; node 1
    (insert "Y") (kao--hist-maybe-commit)        ; node 2
    (should (= (kao-history-current-id) 2))
    (should (= (kao-history-max-id) 2))
    (should (= (kao-hist-node-parent (kao--hist-node 2)) 1))
    (should (= (kao-hist-node-parent (kao--hist-node 1)) 0))
    (should (equal (buffer-string) "XY"))
    (kao-mode -1)))

(ert-deftest kao-state-hist-no-node-without-edit ()
  "A command boundary with no buffer edit commits no node."
  (kao-state-tests--with ""
    (kao-mode 1)
    (kao--hist-maybe-commit)
    (kao--hist-maybe-commit)
    (should (= (kao-history-current-id) 0))
    (should (= (kao-history-max-id) 0))
    (kao-mode -1)))

(ert-deftest kao-state-hist-insert-session-one-node ()
  "An insert session commits as exactly ONE node on exit (REQ-5): mid-session
boundaries are suppressed and `kao--close-undo-group' commits the whole group."
  (kao-state-tests--with ""
    (buffer-enable-undo)
    (kao-mode 1)
    (setq kao--insert-active t)
    (kao--open-undo-group)
    (insert "a") (insert "b")                    ; two modifications, one session
    (kao--hist-maybe-commit)                     ; mid-session: suppressed, no node
    (should (= (kao-history-current-id) 0))
    (kao--close-undo-group)                      ; commits the session as one node
    (setq kao--insert-active nil)
    (should (= (kao-history-current-id) 1))
    (should (= (kao-history-max-id) 1))
    (should (= (length (kao-hist-node-group (kao--hist-node 1))) 2))
    (kao-mode -1)))

;;;; Normal state never dirties the buffer (suppress-keymap)

(defun kao-state-tests--normal-binding (key)
  "Resolve KEY (a key vector) in normal state, following command remapping.
`key-binding' does not apply `command-remapping' (the command loop does), so
follow it here the way `command-execute' would."
  (let ((cmd (key-binding key)))
    (or (and (symbolp cmd) (command-remapping cmd)) cmd)))

(ert-deftest kao-state-normal-printable-keys-never-self-insert ()
  "No printable key resolves to `self-insert-command' in normal state.
Kakoune silently ignores unmapped normal keys (input_handler.cc:368-371); the
keymap remaps `self-insert-command' to `undefined' (`suppress-keymap')."
  (kao-state-tests--with "stable"
    (kao-mode 1)
    (unwind-protect
        (let ((c 32))
          (while (<= c 126)
            (should-not (eq (kao-state-tests--normal-binding (vector c))
                            'self-insert-command))
            (setq c (1+ c))))
      (kao-mode -1))))

(ert-deftest kao-state-normal-ret-and-del-do-not-edit ()
  "RET resolves to `undefined' and DEL to `kao-count-backspace' in normal state.
Neither may reach the global `newline'/`delete-backward-char'."
  (kao-state-tests--with "stable"
    (kao-mode 1)
    (unwind-protect
        (progn
          (should (eq (kao-state-tests--normal-binding (kbd "RET")) #'undefined))
          (should (eq (kao-state-tests--normal-binding (kbd "DEL"))
                      #'kao-count-backspace)))
      (kao-mode -1))))

(ert-deftest kao-state-normal-unbound-key-leaves-buffer-unmodified ()
  "Executing an unbound printable key's binding does not modify the buffer."
  (kao-state-tests--with "stable"
    (kao-mode 1)
    (unwind-protect
        (let ((before (buffer-string)))
          (dolist (key (list (vector ?') (vector ?=) (kbd "RET") (kbd "DEL")))
            (let ((cmd (kao-state-tests--normal-binding key)))
              (when (commandp cmd)
                (cl-letf (((symbol-function 'ding) #'ignore)
                          ((symbol-function 'message) #'ignore))
                  (call-interactively cmd)))))
          (should (equal (buffer-string) before)))
      (kao-mode -1))))

(ert-deftest kao-state-count-backspace-divides-by-ten ()
  "DEL divides the pending count by 10 (input_handler.cc:311-312)."
  (kao-state-tests--with "stable"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--count 123)
          (kao-count-backspace)
          (should (= kao--count 12))
          (kao-count-backspace)
          (kao-count-backspace)
          (should (= kao--count 0)))
      (kao-mode -1))))

(ert-deftest kao-state-count-survives-backspace-reset ()
  "`kao--maybe-reset-count' keeps the count across `kao-count-backspace'."
  (kao-state-tests--with "stable"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--count 12)
          (let ((this-command 'kao-count-backspace))
            (kao--maybe-reset-count))
          (should (= kao--count 12))
          (let ((this-command 'forward-char))
            (kao--maybe-reset-count))
          (should (= kao--count 0)))
      (kao-mode -1))))

;;;; Pending register (the `"' prefix)

(ert-deftest kao-state-pending-register-survives-params-keys ()
  "The pending register survives digits, DEL, and `\"' itself.
Kakoune's digit/Backspace branches never touch `m_params.reg'
\(input_handler.cc:304-313), and `\"' never touches the count."
  (kao-state-tests--with "stable"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--pending-register ?a
                kao--count 3)
          (dolist (cmd '(kao-digit kao-count-backspace kao-select-register))
            (let ((this-command cmd))
              (kao--maybe-reset-count))
            (should (eq kao--pending-register ?a))
            (should (= kao--count 3))))
      (kao-mode -1))))

(ert-deftest kao-state-pending-register-cleared-after-command ()
  "Any non-params command clears BOTH pending params (input_handler.cc:365-372)."
  (kao-state-tests--with "stable"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--pending-register ?a
                kao--count 3)
          (let ((this-command 'forward-char))
            (kao--maybe-reset-count))
          (should (null kao--pending-register))
          (should (= kao--count 0)))
      (kao-mode -1))))

(ert-deftest kao-state-register-arg-pending-or-default ()
  "`kao--register-arg' returns the raw pending char, else DEFAULT."
  (with-temp-buffer
    (setq kao--pending-register nil)
    (should (eq (kao--register-arg ?\") ?\"))
    (setq kao--pending-register ?A)            ; raw: lowering is the stores'
    (should (eq (kao--register-arg ?\") ?A))))

;;;; Terminal ESC decode (kao-esc-mode) + normal-state escape

(ert-deftest kao-state-esc-filter-translates-lone-esc ()
  "A key sequence ending in raw ESC with no pending input becomes [escape].
kao-mode is ENABLED so the hel-study-5 gate (kao-mode / minibuffer / inhibit)
passes — kao-buffer behavior is unchanged from the pre-gate filter."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (unwind-protect
        (cl-letf (((symbol-function 'this-single-command-keys) (lambda () [?\e]))
                  ((symbol-function 'sit-for) (lambda (_) t)))
          (should (equal [escape] (kao--esc-filter 'saved-map))))
      (kao-mode -1))))

(ert-deftest kao-state-esc-filter-defers-on-pending-input ()
  "More input within the delay (sit-for nil) keeps the saved decode map."
  (cl-letf (((symbol-function 'this-single-command-keys) (lambda () [?\e]))
            ((symbol-function 'sit-for) (lambda (_) nil)))
    (should (eq 'saved-map (kao--esc-filter 'saved-map)))))

(ert-deftest kao-state-esc-filter-defers-when-not-esc ()
  "A sequence not ending in ESC is never translated."
  (cl-letf (((symbol-function 'this-single-command-keys) (lambda () [?x]))
            ((symbol-function 'sit-for) (lambda (_) t)))
    (should (eq 'saved-map (kao--esc-filter 'saved-map)))))

(ert-deftest kao-state-esc-filter-records-decoded-escape-in-kmacro ()
  "During a native kmacro the [escape] branch records the DECODED escape event,
not the raw ESC byte — evil's `defining-kbd-macro' fixup (evil-core.el:626-630)
ported so a terminal-recorded Escape replays as Escape, not fused with the next
key into a Meta chord.  kao-mode is ENABLED so a future
esc-filter gate ('s kao-inhibit-esc / kao-mode / minibuffer checks) cannot
flip this pin red.  The tty fuse itself is not batch-reproducible; this pins the
branch structure (the fixup runs `end-kbd-macro', appends `escape' to
`last-kbd-macro', and restarts recording)."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (unwind-protect
        (let ((defining-kbd-macro t)
              (last-kbd-macro [?i ?x])
              (ended nil) (restarted nil))
          (cl-letf (((symbol-function 'this-single-command-keys) (lambda () [?\e]))
                    ((symbol-function 'sit-for) (lambda (_) t))
                    ((symbol-function 'end-kbd-macro)
                     (lambda (&rest _) (setq ended t)))
                    ((symbol-function 'start-kbd-macro)
                     (lambda (&rest _) (setq restarted t))))
            (should (equal [escape] (kao--esc-filter 'saved-map)))
            (should ended)                            ; the recording was ended
            (should restarted)                        ; and restarted (append)
            ;; the decoded escape event is now IN the macro, not the raw ESC byte
            (should (equal last-kbd-macro (vconcat [?i ?x] [escape])))))
      (kao-mode -1))))

(ert-deftest kao-state-esc-filter-no-kmacro-fixup-when-not-recording ()
  "Outside a kmacro the [escape] branch runs NO fixup: `last-kbd-macro' and the
recording state are untouched (regression guard on the non-recording path)."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (unwind-protect
        (let ((defining-kbd-macro nil)
              (last-kbd-macro [?i ?x])
              (touched nil))
          (cl-letf (((symbol-function 'this-single-command-keys) (lambda () [?\e]))
                    ((symbol-function 'sit-for) (lambda (_) t))
                    ((symbol-function 'end-kbd-macro)
                     (lambda (&rest _) (setq touched t)))
                    ((symbol-function 'start-kbd-macro)
                     (lambda (&rest _) (setq touched t))))
            (should (equal [escape] (kao--esc-filter 'saved-map)))
            (should-not touched)
            (should (equal last-kbd-macro [?i ?x]))))
      (kao-mode -1))))

(ert-deftest kao-state-esc-filter-defers-in-non-kao-buffer ()
  "In a non-kao buffer with no active minibuffer a lone ESC is NOT translated:
the filter defers to the saved decode map, so ESC keeps its terminal meaning
outside kao (hel-study-5)."
  (with-temp-buffer
    (should-not (bound-and-true-p kao-mode))
    (cl-letf (((symbol-function 'this-single-command-keys) (lambda () [?\e]))
              ((symbol-function 'sit-for) (lambda (_) t)))
      (should (eq 'saved-map (kao--esc-filter 'saved-map))))))

(ert-deftest kao-state-esc-filter-inhibit-defers-in-kao-buffer ()
  "`kao-inhibit-esc' non-nil defers to the saved map even in a kao buffer —
the let/buffer-local escape hatch for terminal extensions (hel-study-5)."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (unwind-protect
        (let ((kao-inhibit-esc t))
          (cl-letf (((symbol-function 'this-single-command-keys) (lambda () [?\e]))
                    ((symbol-function 'sit-for) (lambda (_) t)))
            (should (eq 'saved-map (kao--esc-filter 'saved-map)))))
      (kao-mode -1))))

(ert-deftest kao-state-esc-filter-translates-in-minibuffer ()
  "An active minibuffer (no kao-mode) still gets the [escape] translation, so
`C-g'/ESC works to exit a minibuffer read in a terminal (hel-study-5)."
  (with-temp-buffer
    (cl-letf (((symbol-function 'this-single-command-keys) (lambda () [?\e]))
              ((symbol-function 'sit-for) (lambda (_) t))
              ((symbol-function 'active-minibuffer-window) (lambda () t)))
      (should (equal [escape] (kao--esc-filter 'saved-map))))))

(ert-deftest kao-state-esc-init-installs-once-and-restores ()
  "Install patches input-decode-map once (guarded); disable restores it.
The batch terminal is a tty (`terminal-live-p' = t), so the real install
path runs.  The prior ESC binding is remembered via the terminal parameter
`kao-esc-saved-map' (`none' sentinel when there was none)."
  ;; NB: `lookup-key' APPLIES a menu-item :filter during lookup, so the
  ;; assertions read the RAW sparse-keymap entry for ESC instead.
  (cl-flet ((raw-esc () (cdr (assq ?\e (cdr input-decode-map)))))
    (let ((term (frame-terminal))
          (had-mode kao-esc-mode))
      (unwind-protect
          (progn
            (kao-esc-mode -1)           ; force a restored baseline
            (let ((orig (raw-esc)))
              (should-not (terminal-parameter term 'kao-esc-saved-map))
              (kao-esc-mode 1)
              (let ((bound (raw-esc)))
                (should (eq 'menu-item (car-safe bound)))
                (should (memq #'kao--esc-filter bound)))
              (should (terminal-parameter term 'kao-esc-saved-map))
              ;; Idempotent: re-init must not wrap the menu-item again.
              (let ((before (raw-esc)))
                (kao--esc-init-terminal)
                (should (equal before (raw-esc))))
              (kao-esc-mode -1)
              (should (equal orig (raw-esc)))
              (should-not (terminal-parameter term 'kao-esc-saved-map))))
        (kao-esc-mode (if had-mode 1 -1))))))

(ert-deftest kao-state-kao-mode-turns-esc-mode-on ()
  "Enabling `kao-mode' enables the global `kao-esc-mode'."
  (let ((had-mode kao-esc-mode))
    (unwind-protect
        (kao-state-tests--with "abc"
          (kao-mode 1)
          (unwind-protect
              (should kao-esc-mode)
            (kao-mode -1)))
      (unless had-mode (kao-esc-mode -1)))))

(ert-deftest kao-state-normal-escape-clears-pending-params ()
  "`kao-normal-escape' resets count and pending register, quietly."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--count 12
                kao--pending-register ?a)
          (kao-normal-escape)
          (should (= 0 kao--count))
          (should-not kao--pending-register))
      (kao-mode -1))))

(ert-deftest kao-state-normal-escape-binding ()
  "[escape] is bound to `kao-normal-escape' in normal state."
  (should (eq #'kao-normal-escape
              (lookup-key kao-normal-state-map [escape]))))

;;;; Per-major-mode normal-state bindings (kao-define-key)

(defmacro kao-state-tests--with-mode-keys (&rest body)
  "Run BODY with `kao--mode-keymaps' isolated (restored afterwards)."
  (declare (indent 0))
  `(let ((kao--mode-keymaps (make-hash-table :test #'eq)))
     ,@body))

(ert-deftest kao-state-define-key-shadows-normal-map ()
  "A mode binding wins over `kao-normal-state-map' in a matching buffer."
  (kao-state-tests--with-mode-keys
    (kao-define-key 'text-mode "w" #'ignore)
    (with-temp-buffer
      (text-mode)
      (insert "abc")
      (kao-mode 1)
      (unwind-protect
          (should (eq #'ignore (key-binding "w")))
        (kao-mode -1)))))

(ert-deftest kao-state-define-key-leaves-other-modes-alone ()
  "A text-mode binding does not leak into other major modes."
  (kao-state-tests--with-mode-keys
    (kao-define-key 'text-mode "w" #'ignore)
    (with-temp-buffer                   ; fundamental-mode
      (insert "abc")
      (kao-mode 1)
      (unwind-protect
          ;; Self-contained: whatever "w" resolves to here (kao-motion's
          ;; binding under make test, the suppress-keymap `undefined'
          ;; standalone), it must NOT be the text-mode map's command.
          (should-not (eq #'ignore (key-binding "w")))
        (kao-mode -1)))))

(ert-deftest kao-state-define-key-applies-to-derived-modes ()
  "A prog-mode binding reaches a buffer in a derived mode."
  (kao-state-tests--with-mode-keys
    (kao-define-key 'prog-mode "w" #'ignore)
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "abc")
      (kao-mode 1)
      (unwind-protect
          (should (eq #'ignore (key-binding "w")))
        (kao-mode -1)))))

(ert-deftest kao-state-define-key-most-derived-wins ()
  "When parent and child modes bind the same key, the child's wins."
  (kao-state-tests--with-mode-keys
    (kao-define-key 'prog-mode "w" #'ignore)
    (kao-define-key 'emacs-lisp-mode "w" #'forward-char)
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "abc")
      (kao-mode 1)
      (unwind-protect
          (should (eq #'forward-char (key-binding "w")))
        (kao-mode -1)))))

(ert-deftest kao-state-define-key-reaches-enabled-buffers ()
  "A binding declared AFTER kao-mode enabled still reaches the buffer."
  (kao-state-tests--with-mode-keys
    (with-temp-buffer
      (text-mode)
      (insert "abc")
      (kao-mode 1)
      (unwind-protect
          (progn
            (kao-define-key 'text-mode "w" #'ignore)
            (should (eq #'ignore (key-binding "w"))))
        (kao-mode -1)))))

(ert-deftest kao-state-define-key-pairs-and-teardown ()
  "KEY DEF pairs all bind; disabling kao-mode removes the local alist."
  (kao-state-tests--with-mode-keys
    (kao-define-key 'text-mode "w" #'ignore "e" #'forward-char)
    (with-temp-buffer
      (text-mode)
      (insert "abc")
      (kao-mode 1)
      (unwind-protect
          (progn
            (should (eq #'ignore (key-binding "w")))
            (should (eq #'forward-char (key-binding "e")))
            (should (local-variable-p 'kao--emulation-mode-map-alist)))
        (kao-mode -1))
      (should-not (local-variable-p 'kao--emulation-mode-map-alist)))))

(ert-deftest kao-state-define-key-only-normal-state ()
  "Mode bindings ride the `kao--normal-active' gate (insert stays native)."
  (kao-state-tests--with-mode-keys
    (kao-define-key 'text-mode "w" #'ignore)
    (with-temp-buffer
      (text-mode)
      (insert "abc")
      (kao-mode 1)
      (unwind-protect
          (dolist (entry kao--emulation-mode-map-alist)
            (should (memq (car entry)
                          '(kao--normal-active kao--insert-active))))
        (kao-mode -1)))))

;;;; Pulse wiring (-2)

(ert-deftest kao-state-jump-restore-pulses-landing ()
  "`kao--jump-restore' flashes the restored main selection's span."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (let (spans)
          (cl-letf (((symbol-function 'kao--pulse-span)
                     (lambda (b e) (push (cons b e) spans))))
            (setq kao--sels (kao-state-tests--sels '((3 . 5)) 0)
                  kao--jumps (list (cons (current-buffer)
                                         (cons (cons (kao-history-generation)
                                                     (kao-history-current-id))
                                               (kao--snapshot-sels kao--sels))))
                  kao--jump-current 0)
            (kao--jump-restore))
          ;; anchor 3, cursor 5 -> inclusive span [3, 6)
          (should (equal spans '((3 . 6)))))
      (kao-mode -1))))

;;;; Per-state cursor (-2)

(ert-deftest kao-state-cursor-shape-per-state ()
  "`kao--apply-cursor' sets a bar for insert, a box for normal."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--apply-cursor 'insert)
          (should (eq cursor-type 'bar))
          (kao--apply-cursor 'normal)
          (should (eq cursor-type 'box)))
      (kao-mode -1))))

(ert-deftest kao-state-cursor-shape-defcustoms-configurable ()
  "`kao--apply-cursor' reads the per-state shape defcustoms (evil-config-6);
defaults stay box/normal, bar/insert, and any `cursor-type' value is honored."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (unwind-protect
        (progn
          ;; Faithful defaults unchanged.
          (should (eq kao-cursor-normal 'box))
          (should (eq kao-cursor-insert 'bar))
          ;; A custom insert shape (a cons cursor-type) is applied verbatim.
          (let ((kao-cursor-insert '(bar . 3)))
            (kao--apply-cursor 'insert)
            (should (equal cursor-type '(bar . 3))))
          ;; A custom normal shape too.
          (let ((kao-cursor-normal 'hbar))
            (kao--apply-cursor 'normal)
            (should (eq cursor-type 'hbar))))
      (kao-mode -1))))

(ert-deftest kao-state-cursor-color-default-inert ()
  "With both color defcustoms nil no frame parameter is ever touched."
  (let ((calls 0)
        (kao-cursor-color-normal nil)
        (kao-cursor-color-insert nil))
    (cl-letf (((symbol-function 'set-frame-parameter)
               (lambda (&rest _) (cl-incf calls))))
      (kao--cursor-color-apply 'normal)
      (kao--cursor-color-apply 'insert)
      (kao--cursor-color-apply nil))
    (should (= calls 0))))

(ert-deftest kao-state-cursor-color-needs-graphics ()
  "A configured color still touches nothing on a non-graphic display (batch)."
  (let ((calls 0)
        (kao-cursor-color-normal "red"))
    (cl-letf (((symbol-function 'set-frame-parameter)
               (lambda (&rest _) (cl-incf calls))))
      (kao--cursor-color-apply 'normal))
    (should (= calls 0))))

(ert-deftest kao-state-cursor-color-applies-and-restores ()
  "On a (stubbed) graphic frame the state color applies; nil restores the
remembered original, which the second apply does not clobber."
  (let ((params '((cursor-color . "black")))
        (kao-cursor-color-normal "red")
        (kao-cursor-color-insert "green"))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'frame-parameter)
               (lambda (_f p) (alist-get p params)))
              ((symbol-function 'set-frame-parameter)
               (lambda (_f p v) (setf (alist-get p params) v))))
      (kao--cursor-color-apply 'normal)
      (should (equal (alist-get 'cursor-color params) "red"))
      (should (equal (alist-get 'kao--cursor-color-orig params) "black"))
      (kao--cursor-color-apply 'insert)
      (should (equal (alist-get 'cursor-color params) "green"))
      (should (equal (alist-get 'kao--cursor-color-orig params) "black"))
      (kao--cursor-color-apply nil)
      (should (equal (alist-get 'cursor-color params) "black")))))

;;;; Public state read + cursor refresh

(ert-deftest kao-state-current-state-nil-when-off ()
  "`kao-current-state' is nil (and both predicates false) with `kao-mode' off.
Extensions (ghostel, cursor themes) rely on the nil to hand control back — the
three-way cond must not collapse to a two-way one."
  (kao-state-tests--with "abc"
    (should-not kao-mode)
    (should (eq (kao-current-state) nil))
    (should-not (kao-normal-state-p))
    (should-not (kao-insert-state-p))))

(ert-deftest kao-state-current-state-normal-in-fresh-buffer ()
  "A fresh kao buffer reads as normal state via the public predicates."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (unwind-protect
        (progn
          (should (eq (kao-current-state) 'normal))
          (should (kao-normal-state-p))
          (should-not (kao-insert-state-p)))
      (kao-mode -1))))

(ert-deftest kao-state-current-state-insert-in-session ()
  "Inside an insert session `kao-current-state' reads \\='insert."
  (kao-state-tests--with "abcdef"
    (kao-mode 1)
    (unwind-protect
        (progn
          (kao--enter-insert 'insert (lambda () (goto-char 2)))
          (should (eq (kao-current-state) 'insert))
          (should (kao-insert-state-p))
          (should-not (kao-normal-state-p)))
      (kao-mode -1))))

(ert-deftest kao-state-refresh-cursor-reapplies-shape ()
  "`kao-refresh-cursor' re-asserts the state's cursor after a foreign clobber."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq cursor-type 'hollow)              ; a foreign `setq cursor-type'
          (kao-refresh-cursor)
          (should (eq cursor-type 'box)))         ; normal-state box re-applied
      (kao-mode -1))))

;;;; Normal-state exit hook

(ert-deftest kao-state-normal-exit-hook-fires-on-insert-entry ()
  "`kao-normal-state-exit-hook' fires once when normal state is left for insert.
The symmetric partner of `kao-normal-state-entry-hook'; fires in
`kao--insert-begin' before the insert-entry fire."
  (kao-state-tests--with "abcdef"
    (kao-mode 1)
    (unwind-protect
        (let ((n 0))
          (add-hook 'kao-normal-state-exit-hook (lambda () (setq n (1+ n))) nil t)
          (kao--enter-insert 'insert (lambda () (goto-char 2)))
          (should (= n 1)))
      (kao-mode -1))))

(ert-deftest kao-state-normal-exit-hook-fires-on-mode-disable-from-normal ()
  "`kao-normal-state-exit-hook' fires once when `kao-mode' is disabled from normal.
Symmetric teardown seam: leaving normal via the mode disable arm is observable."
  (kao-state-tests--with "abcdef"
    (kao-mode 1)
    (let ((n 0))
      (add-hook 'kao-normal-state-exit-hook (lambda () (setq n (1+ n))) nil t)
      (kao-mode -1)
      (should (= n 1)))))

(ert-deftest kao-state-normal-exit-hook-not-on-motion ()
  "`kao-normal-state-exit-hook' does NOT fire on a plain motion command.
The hook marks leaving normal state, not every keystroke within it."
  (kao-state-tests--with "abcdef"
    (kao-mode 1)
    (unwind-protect
        (let ((n 0))
          (add-hook 'kao-normal-state-exit-hook (lambda () (setq n (1+ n))) nil t)
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0))
          (call-interactively 'kao-right)
          (should (= n 0)))
      (kao-mode -1))))

;;;; Captures: the select rule through the mapping helpers

(ert-deftest kao-state-map-selections-inherits-captures ()
  "A captureless motion result inherits the source's captures (normal.cc:118-119)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 4
                                               :captures '("234")))
                     :main 0))
    (kao--map-selections
     (lambda (_s) (kao-sel-make :anchor 6 :cursor 7)))
    (should (equal (kao-sel-captures (car (kao-sels-list kao--sels)))
                   '("234")))))

(ert-deftest kao-state-map-selections-result-captures-win ()
  "A motion result carrying its own captures overwrites the source's."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 4
                                               :captures '("old")))
                     :main 0))
    (kao--map-selections
     (lambda (_s) (kao-sel-make :anchor 6 :cursor 7 :captures '("new"))))
    (should (equal (kao-sel-captures (car (kao-sels-list kao--sels)))
                   '("new")))))

(ert-deftest kao-state-filter-captures-both-arms ()
  "Filter dispatch: Replace inherits when captureless; Extend keeps the
source's and lets a result with captures win."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    ;; Replace arm: inherit
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 4
                                               :captures '("kept")))
                     :main 0))
    (kao--map-filter-selections
     (lambda (_s) (kao-sel-make :anchor 5 :cursor 6)))
    (should (equal (kao-sel-captures (car (kao-sels-list kao--sels)))
                   '("kept")))
    ;; Extend arm: source captures survive the merge
    (kao--map-filter-selections
     (lambda (_s) (kao-sel-make :anchor 8 :cursor 8)) t)
    (should (equal (kao-sel-captures (car (kao-sels-list kao--sels)))
                   '("kept")))
    ;; Extend arm: a result with captures wins (post-merge select() rule)
    (kao--map-filter-selections
     (lambda (_s) (kao-sel-make :anchor 9 :cursor 9 :captures '("win"))) t)
    (should (equal (kao-sel-captures (car (kao-sels-list kao--sels)))
                   '("win")))))

(ert-deftest kao-state-map-extend-carries-captures ()
  "The extend fold keeps the selection's captures across steps."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2
                                               :captures '("c")))
                     :main 0)
          kao--count 2)
    (kao--map-selections-extend
     (lambda (s) (kao-sel-make :anchor (1+ (kao-sel-cursor s))
                               :cursor (1+ (kao-sel-cursor s)))))
    (should (equal (kao-sel-captures (car (kao-sels-list kao--sels)))
                   '("c")))))

(ert-deftest kao-state-clamp-carries-captures ()
  "Both clamps carry captures (coordinate adjustment, same selection)."
  (kao-state-tests--with "0123"
    (let ((s (kao-sel-make :anchor 2 :cursor 99 :captures '("z"))))
      (should (equal (kao-sel-captures (kao--clamp-sel s)) '("z")))
      (should (equal (kao-sel-captures (kao--extend-clamp s)) '("z"))))))

(ert-deftest kao-state-clamp-carries-target ()
  "Both clamps carry the sticky-goal TARGET through the hot path.
The positional `kao-sel--new' would silently drop a 4th arg; the clamp sites
pass `target' explicitly so a goal column survives every motion's clamp."
  (kao-state-tests--with "0123"
    (let ((i (kao-sel-make :anchor 2 :cursor 99 :target 7))
          (e (kao-sel-make :anchor 2 :cursor 99 :target 'eol)))
      (should (eql (kao-sel-target (kao--clamp-sel i)) 7))
      (should (eql (kao-sel-target (kao--extend-clamp i)) 7))
      (should (eq (kao-sel-target (kao--clamp-sel e)) 'eol)))))

(ert-deftest kao-state-snapshot-copies-captures-decoupled ()
  "`kao--snapshot-sels' carries captures as a decoupled copy."
  (kao-state-tests--with "0123456789"
    (let* ((caps (list "a" "b"))
           (live (kao-sels-make
                  :list (list (kao-sel-make :anchor 1 :cursor 3
                                            :captures caps))
                  :main 0))
           (snap (kao--snapshot-sels live)))
      (should (equal (kao-sel-captures (car (kao-sels-list snap)))
                     '("a" "b")))
      ;; mutate the live capture list in place: the snapshot must not move
      (setcar caps "MUT")
      (should (equal (kao-sel-captures (car (kao-sels-list snap)))
                     '("a" "b"))))))

(ert-deftest kao-state-snapshot-carries-target ()
  "`kao--snapshot-sels' carries the sticky `target' through the positional
constructor (the recorder/register snapshot site, kao-register.el:248)."
  (kao-state-tests--with "0123456789"
    (let* ((live (kao-sels-make
                  :list (list (kao-sel-make :anchor 1 :cursor 3 :target 'eol)
                              (kao-sel-make :anchor 5 :cursor 6 :target 4))
                  :main 0))
           (snap (kao--snapshot-sels live))
           (lst (kao-sels-list snap)))
      (should (eq (kao-sel-target (nth 0 lst)) 'eol))
      (should (eql (kao-sel-target (nth 1 lst)) 4)))))

;;;; Revert re-pins the saved history id

(ert-deftest kao-state-revert-re-pins-saved-history-id ()
  "A preserve-modes revert re-pins the saved history id to the reload node.
After an external reload, `kao-undo' back past the reload marks the buffer
modified and `kao-redo' onto the reload node clears it:
kao installs an `after-revert-hook' that runs `kao-history-mark-saved', the
elisp analogue of Kakoune `Buffer::reload' re-pinning
`m_last_save_history_id = m_history_id' (buffer.cc:287).  Without it the flag
inverts — `u' reads unmodified on content that differs from disk."
  (let ((file (make-temp-file "kao-revert-" nil ".txt" "original\n"))
        (make-backup-files nil))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (unwind-protect
              (progn
                (kao-mode 1)
                (goto-char (point-max))
                (insert "edit1\n")
                (kao--hist-maybe-commit)              ; committed edit node
                (save-buffer)                          ; after-save-hook pins saved id
                (should-not (buffer-modified-p))
                ;; External rewrite, then reload the auto-revert / magit way.
                (write-region "EXTERNAL\n" nil file nil 'no-message)
                (revert-buffer 'ignore-auto 'noconfirm 'preserve-modes)
                (kao--hist-maybe-commit)               ; reload diff -> reload node
                (should (equal (buffer-string) "EXTERNAL\n"))
                (should-not (buffer-modified-p))        ; matches disk right after reload
                (kao-undo)                              ; back past the reload node
                (should (buffer-modified-p))            ; differs from disk -> modified
                (kao-redo)                              ; onto the reload node
                (should-not (buffer-modified-p)))       ; matches disk -> unmodified
            (kao-mode -1)
            (set-buffer-modified-p nil)
            (kill-buffer)))
      (delete-file file))))

;;;; Tree-generation stamp on snapshot tags

(ert-deftest kao-state-stale-generation-jump-tag-clamps ()
  "A jump tag from a tree `kao-history-init' has since replaced takes the
clamp+merge fallback, never the same-id verbatim fast path.
kao restarts HistoryIds at 0 on every `kao-mode' enable, so a stale id can alias
a fresh, unrelated node — even the `(= id (kao-history-current-id))' verbatim
fast path.  A tree-generation stamp on every tag makes `kao--snapshot-update'
treat a generation mismatch exactly like a gc'd id: no translation, clamp+merge
residual.  Here two over-length selections clamp onto the same point; the
fallback sort-and-merges them into ONE, whereas the buggy fast path installs
both verbatim (two selections)."
  (let ((kao--jumps nil) (kao--jump-current 0))
    (kao-state-tests--with "alpha beta gamma delta epsilon"
      (kao-mode 1)
      (unwind-protect
          (progn
            ;; Old tree: one committed edit -> current id 1.
            (goto-char (point-max)) (insert "Z") (kao--hist-maybe-commit)
            (should (= (kao-history-current-id) 1))
            ;; Push a jump whose two selections both sit past the FUTURE short
            ;; buffer's end, tagged at old id 1 (of the old tree generation).
            (setq kao--sels (kao-sels-make
                             :list (list (kao-sel-make :anchor 10 :cursor 12)
                                         (kao-sel-make :anchor 20 :cursor 22))
                             :main 1))
            (kao--jump-push)
            (should (= 1 (length kao--jumps)))
            ;; Re-init the tree over completely different, shorter content.
            (kao-mode -1)
            (erase-buffer)
            (insert "short")
            (kao-mode 1)
            (should (= (kao-history-current-id) 0))
            ;; One new edit so the fresh tree's current id is again 1 == the
            ;; stale tag's id — the same-id verbatim fast path the finding hits.
            (goto-char (point-max)) (insert "X") (kao--hist-maybe-commit)
            (should (= (kao-history-current-id) 1))
            ;; Restore the stale-generation jump: it must clamp+merge, collapsing
            ;; the two over-length selections into one — not install both verbatim.
            (setq kao--jump-current 0)
            (kao--jump-restore)
            (should (= 1 (length (kao-sels-list kao--sels)))))
        (kao-mode -1)))))

;;;; Dynamic registers — the real % . # 0-9 definitions

(ert-deftest kao-state-dynreg-percent-buffer-name ()
  "`%' reads the buffer display name; writing it is not assignable."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (should (equal (kao-register-get ?%) (list (buffer-name))))
    (let ((err (should-error (kao-register-set ?% '("v"))
                             :type 'user-error)))
      (should (equal (cadr err) "this register is not assignable")))))

(ert-deftest kao-state-dynreg-dot-selection-contents ()
  "`.' reads each selection's content (selections_content)."
  (kao-state-tests--with "ab cd"
    (kao-mode 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)
                                 (kao-sel-make :anchor 4 :cursor 5))
                     :main 1))
    (should (equal (kao-register-get ?.) '("ab" "cd")))))

(ert-deftest kao-state-dynreg-dot-empty-buffer-reads-empty ()
  "`.' in an empty buffer reads (\"\") — the phantom selection end is clamped
to `point-max' rather than signalling `args-out-of-range'."
  (kao-state-tests--with ""
    (kao-mode 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1))
                     :main 0))
    (should (equal (kao-register-get ?.) '("")))))

(ert-deftest kao-state-dynreg-hash-indices ()
  "`#' reads the 1-based selection indices."
  (kao-state-tests--with "ab cd"
    (kao-mode 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)
                                 (kao-sel-make :anchor 4 :cursor 5))
                     :main 1))
    (should (equal (kao-register-get ?#) '("1" "2")))))

(ert-deftest kao-state-dynreg-digit-reads-captures ()
  "`0'-`9' read per-selection captures[i]; out of range reads \"\"."
  (kao-state-tests--with "ab cd"
    (kao-mode 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2
                                               :captures '("ab" "a"))
                                 (kao-sel-make :anchor 4 :cursor 5))
                     :main 1))
    (should (equal (kao-register-get ?0) '("ab" "")))
    (should (equal (kao-register-get ?1) '("a" "")))
    (should (equal (kao-register-get ?9) '("" "")))))

(ert-deftest kao-state-dynreg-digit-setter-pads-and-clamps ()
  "The digit setter pads captures with \"\" and clamps to the last value.
3 selections, 2 strings: the third selection receives string 2
\(values[min(sel_index, values.size()-1)], main.cc:386-398)."
  (kao-state-tests--with "a b c"
    (kao-mode 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 3 :cursor 3)
                                 (kao-sel-make :anchor 5 :cursor 5))
                     :main 2))
    (kao-register-set ?2 '("x" "y"))
    (let ((lst (kao-sels-list kao--sels)))
      (should (equal (kao-sel-captures (nth 0 lst)) '("" "" "x")))
      (should (equal (kao-sel-captures (nth 1 lst)) '("" "" "y")))
      (should (equal (kao-sel-captures (nth 2 lst)) '("" "" "y"))))
    ;; read back through the digit getter
    (should (equal (kao-register-get ?2) '("x" "y" "y")))))

(ert-deftest kao-state-dynreg-digit-setter-empty-noop ()
  "Writing an empty value list to a digit register is a no-op."
  (kao-state-tests--with "ab"
    (kao-mode 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2
                                               :captures '("ab")))
                     :main 0))
    (kao-register-set ?0 nil)
    (should (equal (kao-sel-captures (car (kao-sels-list kao--sels)))
                   '("ab")))))

(ert-deftest kao-state-dynreg-digit-setter-copy-on-write ()
  "The digit setter never mutates a shared capture list in place.
C++ CaptureLists are value-semantic per Selection; two kao selections
sharing one list object (via carry) must not both change."
  (kao-state-tests--with "a b"
    (kao-mode 1)
    (let ((shared (list "s0")))
      (setq kao--sels (kao-sels-make
                       :list (list (kao-sel-make :anchor 1 :cursor 1
                                                 :captures shared)
                                   (kao-sel-make :anchor 3 :cursor 3
                                                 :captures shared))
                       :main 1))
      (kao-register-set ?0 '("first" "second"))
      (should (equal shared '("s0")))   ; the original list object untouched
      (should (equal (kao-register-get ?0) '("first" "second"))))))

(ert-deftest kao-state-dynreg-nil-sels-reads-nil ()
  "In a buffer without a selection list the selection-backed getters read nil."
  (with-temp-buffer
    (insert "x")
    (should (null (kao-register-get ?.)))
    (should (null (kao-register-get ?#)))
    (should (null (kao-register-get ?4)))))

;;;; Mouse stance (minimum fidelity)

(defun kao-state-tests--mouse-pairs ()
  "Current ((anchor . cursor) ...) of `kao--sels'."
  (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
          (kao-sels-list kao--sels)))

(ert-deftest kao-state-mouse-click-collapses-to-point ()
  "A click command replaces the whole list with one selection at point —
Kakoune's no-Ctrl left press (`selections_write_only() = {buffer, anchor}',
input_handler.cc:143-154)."
  (kao-state-tests--with "0123456789\n0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((2 . 4) (13 . 15)) 1))
          (goto-char 7)
          (let ((this-command 'mouse-set-point)) (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((7 . 7))))
          (should (= 0 (kao-sels-main kao--sels))))
      (kao-mode -1))))

(ert-deftest kao-state-mouse-click-clamps-at-eob ()
  "A click at `point-max' clamps the selection onto the last real char."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (unwind-protect
        (progn
          (goto-char (point-max))                  ; 4, past the last char
          (let ((this-command 'mouse-drag-region)) (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((3 . 3)))))
      (kao-mode -1))))

(ert-deftest kao-state-mouse-scroll-syncs-main-keeps-secondaries ()
  "A wheel command collapses only the MAIN onto point (treatment);
secondaries survive and the list is re-sorted."
  (kao-state-tests--with "0123456789\n0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((2 . 4) (13 . 15)) 1))
          (goto-char 7)
          (let ((this-command 'mwheel-scroll)) (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((2 . 4) (7 . 7))))
          (should (= 1 (kao-sels-main kao--sels))))
      (kao-mode -1))))

(ert-deftest kao-state-mouse-scroll-unmoved-point-preserves-main ()
  "A wheel/scroll tick that did NOT carry point (point still ON the main cursor)
leaves the selection verbatim — Kakoune's `PreserveSelections' touches nothing
\(input_handler.cc:196, 1802-1803/1815).  Only a CARRIED point (Emacs scrolled
the cursor to the window edge) collapses the main onto it; the
`kao-state-mouse-scroll-syncs-main-keeps-secondaries' pin above covers that moved
case."
  (kao-state-tests--with "alpha"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((1 . 5)) 0))
          (goto-char 5)                         ; point == main cursor: unmoved
          (let ((this-command 'mwheel-scroll)) (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((1 . 5))))
          (should (= 0 (kao-sels-main kao--sels))))
      (kao-mode -1))))

(ert-deftest kao-state-window-scroll-renders-revealed-secondary ()
  "A `window-scroll-functions' scroll re-renders secondaries over the NEW
viewport: a secondary beyond the OLD `window-end' gets its overlay drawn
without a further keystroke.  Kakoune highlights every
displayed frame over the FINAL post-scroll viewport (window.cc:142-162,230-247);
kao's post-command render clipped to the pre-scroll bounds, so the newly
revealed region stayed undrawn until the next key.  The pin drives the scroll
hook directly with a synthetic new START (batch has no live redisplay)."
  (let ((buf (get-buffer-create "kao-scroll-test")))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert (make-string 80 ?x))         ; positions 1..80, one long line
          (goto-char (point-min))
          (set-window-buffer (selected-window) buf)
          (kao-mode 1)
          ;; main at pos 1, one secondary far down at 50 (off the initial screen)
          (setq kao--sels (kao-state-tests--sels '((1 . 1) (50 . 50)) 0))
          ;; Pre-scroll render clipped to the OLD viewport [1,10): pos 50 is
          ;; off-screen and gets NO overlay — the bug's starting state.
          (kao--render kao--sels 1 10)
          (should (null (cl-find-if (lambda (o) (and (overlay-buffer o)
                                                     (= (overlay-start o) 50)))
                                    kao--overlay-pool)))
          ;; Redisplay scrolls the window; `window-scroll-functions' fires with
          ;; the new START.  The revealed secondary must now be drawn.
          (kao--render-on-scroll (selected-window) 40)
          (should (cl-find-if (lambda (o) (and (overlay-buffer o)
                                               (eq (overlay-get o 'face) 'kao-cursor)
                                               (= (overlay-start o) 50)))
                              kao--overlay-pool)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (kao-mode -1))
        (kill-buffer buf)))))

(ert-deftest kao-state-foreign-unlisted-mouse-command-collapses ()
  "An unlisted mouse command that moved point is caught by the
catch-all (it is a foreign command like any other): the list collapses to
point.  Supersedes the earlier point-neutral snap-back default."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((2 . 4)) 0))
          (goto-char 8)
          (let ((this-command 'mouse-save-then-kill))
            (kao--foreign-sync)
            (kao--refresh))
          (should (equal (kao-state-tests--mouse-pairs) '((8 . 8))))
          (should (= (point) 8)))
      (kao-mode -1))))

(ert-deftest kao-state-foreign-command-moved-point-collapses ()
  "A foreign command (imenu, avy, xref...) that ended with point off the
main cursor moved point authoritatively: the whole list becomes ONE
selection at point (click semantics, generalized).
Pre-the mirror snapped point back and the command appeared dead."
  (kao-state-tests--with "0123456789\n0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((2 . 4) (13 . 15)) 1))
          (goto-char 9)
          (let ((this-command 'imenu)) (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((9 . 9))))
          (should (= 0 (kao-sels-main kao--sels))))
      (kao-mode -1))))

(ert-deftest kao-state-foreign-region-mark-command-adopted-forward ()
  "A LISTED region-marking command's span becomes the selection:
the exclusive region end steps back onto the span's last char (Emacs
half-open vs kao inclusive)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((2 . 4)) 0))
          (set-mark 5) (goto-char 9)       ; the command marked [5,9)
          (let ((this-command 'er/expand-region)) (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((5 . 8)))))
      (kao-mode -1))))

(ert-deftest kao-state-foreign-region-mark-command-adopted-backward ()
  "Backward span (point < mark): direction preserved, the exclusive MARK
end steps back onto the span's last char."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((2 . 4)) 0))
          (set-mark 9) (goto-char 5)       ; region [5,9), point at start
          (let ((this-command 'er/expand-region)) (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((8 . 5)))))
      (kao-mode -1))))

(ert-deftest kao-state-foreign-region-mark-command-zero-width-collapses ()
  "A listed command that left mark == point marked no span
catch-all point collapse applies (adoption needs a real extent)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((2 . 4)) 0))
          (set-mark 7) (goto-char 7)
          (let ((this-command 'er/expand-region)) (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((7 . 7)))))
      (kao-mode -1))))

(ert-deftest kao-state-foreign-unlisted-command-active-mark-collapses ()
  "An UNLISTED foreign command collapses even with an active mark span —
kao's own mirror keeps the region active for every multi-char selection,
so bare region-activity must NOT trigger adoption (it would repeal the
collapse for every foreign jump; the membership gate is the point)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((2 . 4)) 0))
          (set-mark 5) (goto-char 9)       ; active span, but imenu-like jump
          (let ((this-command 'imenu)) (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((9 . 9)))))
      (kao-mode -1))))

(ert-deftest kao-state-foreign-point-neutral-is-noop ()
  "A foreign command that left point on the main cursor changes nothing —
the catch-all fires only on a real point move (REQ-2)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((2 . 4)) 0))
          (goto-char 4)                          ; point == main cursor
          (let ((this-command 'save-buffer)) (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((2 . 4))))
          (should (= 0 (kao-sels-main kao--sels))))
      (kao-mode -1))))

(ert-deftest kao-state-foreign-kao-command-exempt ()
  "kao's own commands are pure list transforms: at sync time point is
STALE relative to the new main cursor.  The `kao-' prefix exemption must
keep the catch-all away from them — without it every kao motion would be
collapsed back to the pre-motion point (the load-bearing branch, REQ-3)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          ;; Simulate a kao motion's post-command moment: sels already
          ;; transformed to (6 . 6), point still at the old cursor 2.
          (setq kao--sels (kao-state-tests--sels '((6 . 6)) 0))
          (goto-char 2)
          (let ((this-command 'kao-word-forward)) (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((6 . 6)))))
      (kao-mode -1))))

(ert-deftest kao-state-foreign-lambda-command-counts-as-foreign ()
  "A non-symbol `this-command' (a lambda bound via `kao-define-key')
cannot be classified by name and counts as foreign: a point move
collapses; `symbol-name' must not be reached (no error)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((2 . 4)) 0))
          (goto-char 7)
          (let ((this-command (lambda () (interactive) nil)))
            (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((7 . 7)))))
      (kao-mode -1))))

(ert-deftest kao-state-foreign-keyboard-scroll-gets-d19-treatment ()
  "`scroll-up-command'/`scroll-down-command' (C-v/M-v) are scroll-list
members by default: a native scroll carrying point to the window edge gets
the sync (main follows point, secondaries PRESERVED), not the
click collapse."
  (should (memq 'scroll-up-command kao-mouse-scroll-commands))
  (should (memq 'scroll-down-command kao-mouse-scroll-commands))
  (kao-state-tests--with "0123456789\n0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((2 . 4) (13 . 15)) 1))
          (goto-char 7)
          (let ((this-command 'scroll-up-command)) (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((2 . 4) (7 . 7))))
          (should (= 1 (kao-sels-main kao--sels))))
      (kao-mode -1))))

(ert-deftest kao-state-foreign-collapse-clamps-at-eob ()
  "A foreign point move to `point-max' clamps the collapsed selection onto
the last real char (the click-branch clamp, shared)."
  (kao-state-tests--with "abc"
    (kao-mode 1)
    (unwind-protect
        (progn
          (goto-char (point-max))                  ; 4, past the last char
          (let ((this-command 'imenu)) (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((3 . 3)))))
      (kao-mode -1))))

(ert-deftest kao-state-foreign-jump-records-selection-history-node ()
  "Through the REAL post-command hook sequence (the foreign sync runs
first, before the recorder), a foreign jump records one node and
`kao-sel-undo' restores the pre-jump list."
  (kao-state-tests--with "0123456789\n0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((2 . 4) (13 . 15)) 1))
          (let ((this-command 'kao-word-forward))   ; seed a node for the list
            (run-hooks 'post-command-hook))
          (goto-char 9)
          (let ((this-command 'imenu))
            (run-hooks 'post-command-hook))
          (should (equal (kao-state-tests--mouse-pairs) '((9 . 9))))
          (kao-sel-undo)
          (should (equal (kao-state-tests--mouse-pairs) '((2 . 4) (13 . 15)))))
      (kao-mode -1))))

(ert-deftest kao-state-mouse-preserves-count-and-register ()
  "A pending count/register survives a mouse event — the mouse handler
early-returns BEFORE the `m_params' reset (input_handler.cc:297-303 vs
:365-372); a normal command still resets both."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--count 3 kao--pending-register ?a)
          (let ((this-command 'mouse-set-point)) (kao--maybe-reset-count))
          (should (= kao--count 3))
          (should (eq kao--pending-register ?a))
          (let ((this-command 'mwheel-scroll)) (kao--maybe-reset-count))
          (should (= kao--count 3))
          (let ((this-command 'kao-word-forward)) (kao--maybe-reset-count))
          (should (= kao--count 0))
          (should (null kao--pending-register)))
      (kao-mode -1))))

(ert-deftest kao-state-mouse-sync-noop-in-insert ()
  "The mouse sync no-ops in insert state (the selection is suspended;
Kakoune's insert-mode mouse routing is deferred)."
  (kao-state-tests--with "0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((2 . 4)) 0))
          (kao--enter-insert 'insert (lambda () (goto-char 6)))
          (goto-char 9)
          (let ((this-command 'mouse-set-point)) (kao--foreign-sync))
          (should (equal (kao-state-tests--mouse-pairs) '((2 . 4)))))
      (kao-mode -1))))

(ert-deftest kao-state-mouse-click-records-selection-history-node ()
  "Through the REAL post-command hook sequence (mouse sync runs first), a
click records one node and `kao-sel-undo' restores the pre-click list
— the `ScopedSelectionEdition' Kakoune holds across press/release."
  (kao-state-tests--with "0123456789\n0123456789"
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-state-tests--sels '((2 . 4) (13 . 15)) 1))
          (let ((this-command 'kao-word-forward))   ; seed a node for the list
            (run-hooks 'post-command-hook))
          (goto-char 7)
          (let ((this-command 'mouse-set-point))
            (run-hooks 'post-command-hook))
          (should (equal (kao-state-tests--mouse-pairs) '((7 . 7))))
          (kao-sel-undo)
          (should (equal (kao-state-tests--mouse-pairs) '((2 . 4) (13 . 15)))))
      (kao-mode -1))))

;;;; Live selection list translated through non-command edits

(ert-deftest kao-state-live-sels-translate-through-foreign-edit ()
  "A non-command edit folds the live list so the next keystroke is not stale.
G2-render-perf-1: an `insert' outside any command shifts the buffer; the live
list (integers at rest) goes stale, but the next `kao-right' must land on the
char after the ORIGINAL cursor (?d→?e at pos 7), not two chars back on ?c (5),
because the list is folded through the tree once the foreign edit is committed."
  (kao-state-tests--with "abcdef"
    (buffer-enable-undo)
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 4 :cursor 4)) :main 0))
          (kao--refresh)                ; tag the live list at the current node
          ;; a non-command buffer edit (the timer / process-filter shape)
          (save-excursion (goto-char (point-min)) (insert "XX"))
          ;; drive the next keystroke through the full command pipeline
          (let ((this-command 'kao-right))
            (call-interactively 'kao-right)
            (run-hooks 'post-command-hook))
          (should (= (point) 7))                          ; on ?e, not ?c (5)
          (should (= (kao-sel-cursor (kao--main-sel)) 7))
          (should (char-equal (char-after (point)) ?e)))
      (kao-mode -1))))

(ert-deftest kao-state-live-sels-own-edit-not-retranslated ()
  "A kao edit's post-edit selections are re-tagged, never folded through its own edge.
The live-list translation must fire only for FOREIGN edits: kao's own edit
installs post-edit selections already in the new frame, so the post-command
commit re-tags the list rather than translating it through
its own insertion edge (which would double-shift the cursor by +2 to 7)."
  (kao-state-tests--with "abcdef"
    (buffer-enable-undo)
    (kao-mode 1)
    (unwind-protect
        (progn
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 3 :cursor 3)) :main 0))
          (kao--refresh)
          ;; a kao buffer edit through the atomic primitive: insert "XY" at ?c
          (kao--multi-edit
           (lambda (am cm _i)
             (goto-char (marker-position cm))
             (insert "XY")
             (cons (marker-position am) (marker-position cm))))
          (should (= (kao-sel-cursor (kao--main-sel)) 5)) ; ?c now at 5 after "XY"
          ;; the edit's own commit runs at post-command; the live list must NOT
          ;; fold through the edit edge (that would shift the cursor to 7)
          (let ((this-command 'kao-paste))
            (run-hooks 'post-command-hook))
          (should (= (kao-sel-cursor (kao--main-sel)) 5))
          (should (char-equal (char-after (kao-sel-cursor (kao--main-sel))) ?c)))
      (kao-mode -1))))

(ert-deftest kao-state-prompt-setup-composes-map ()
  "`kao--prompt-setup' layers `kao-prompt-map' over the local map without
mutating it: C-r resolves to the register insert, other keys fall
through to the underlying map."
  (with-temp-buffer
    (let ((base (make-sparse-keymap)))
      (define-key base (kbd "C-q") #'ignore)
      (use-local-map base)
      (kao--prompt-setup)
      (should (eq (lookup-key (current-local-map) (kbd "C-r"))
                  #'kao-prompt-insert-register))
      (should (eq (lookup-key (current-local-map) (kbd "C-q")) #'ignore))
      ;; The shared base map itself is untouched.
      (should-not (lookup-key base (kbd "C-r"))))))

;;;; Regex case-fold policy (`kao--regex-case-fold', )

(ert-deftest kao-casefold-default-sensitive ()
  "Default (nil) is case-sensitive and leaves a flagless pattern intact."
  (let ((kao-search-case-fold nil))
    (should (equal (kao--regex-case-fold "Foo") '("Foo")))))

(ert-deftest kao-casefold-leading-i-folds ()
  "A leading `(?i)' strips the token and folds."
  (let ((kao-search-case-fold nil))
    (should (equal (kao--regex-case-fold "(?i)foo") '("foo" . t)))))

(ert-deftest kao-casefold-leading-cap-i-no-fold ()
  "A leading `(?I)' strips the token and forces no-fold even when default folds."
  (let ((kao-search-case-fold t))
    (should (equal (kao--regex-case-fold "(?I)Foo") '("Foo")))))

(ert-deftest kao-casefold-consecutive-leading-last-wins ()
  "Consecutive leading flags toggle; the last wins (Kakoune left-to-right)."
  (let ((kao-search-case-fold nil))
    (should (equal (kao--regex-case-fold "(?i)(?I)foo") '("foo")))
    (should (equal (kao--regex-case-fold "(?I)(?i)foo") '("foo" . t)))))

(ert-deftest kao-casefold-midpattern-stripped-not-scoped ()
  "A mid-pattern token is stripped (never matched literally) but does NOT scope;
fold stays the global default — the documented Emacs limitation."
  (let ((kao-search-case-fold nil))
    (should (equal (kao--regex-case-fold "foo(?i)bar") '("foobar"))))
  (let ((kao-search-case-fold t))
    (should (equal (kao--regex-case-fold "foo(?I)bar") '("foobar" . t)))))

(ert-deftest kao-casefold-always-and-smart ()
  "t folds always; `smart' folds only when the stripped pattern is lowercase."
  (let ((kao-search-case-fold t))
    (should (equal (kao--regex-case-fold "Foo") '("Foo" . t))))
  (let ((kao-search-case-fold 'smart))
    (should (equal (kao--regex-case-fold "foo") '("foo" . t)))     ; no upper -> fold
    (should (equal (kao--regex-case-fold "Foo") '("Foo"))))        ; has upper -> sensitive
  (let ((kao-search-case-fold 'smart))                             ; leading flag wins
    (should (equal (kao--regex-case-fold "(?i)Foo") '("Foo" . t)))))

(ert-deftest kao-casefold-idempotent ()
  "Stripping an already-stripped pattern is a no-op (leaves are re-entrant)."
  (let ((kao-search-case-fold nil))
    (should (equal (kao--regex-case-fold "foobar") '("foobar")))))

(ert-deftest kao-key-codepoint-normalizes-return ()
  "Return normalizes to newline: the char ?\\r AND the GUI `read-key' symbols
`return'/`kp-enter' all fold to ?\\n (bis), so a GUI `r RET' becomes a
newline instead of being dropped by a caller's `characterp' guard.  Other key
events pass through unchanged; the guard applies AFTER this fold."
  (should (eq (kao--key-codepoint ?\r) ?\n))          ; Return char -> newline
  (should (eq (kao--key-codepoint 'return) ?\n))      ; GUI Return symbol -> newline
  (should (eq (kao--key-codepoint 'kp-enter) ?\n))    ; keypad Enter symbol -> newline
  (should (eq (kao--key-codepoint ?x) ?x))            ; ordinary char unchanged
  (should (eq (kao--key-codepoint ?\t) ?\t))          ; Tab already arrives as ?\t
  (should (eq (kao--key-codepoint 'escape) 'escape))) ; other symbol event unchanged

(ert-deftest kao-assert-mode-guards-mode-off ()
  "`kao--assert-mode' signals a `user-error' (with the contract message) in a
non-kao buffer, and returns nil when `kao-mode' is active ."
  (with-temp-buffer
    (fundamental-mode)
    (let ((e (should-error (kao--assert-mode) :type 'user-error)))
      (should (string-match-p "kao-mode is not active in this buffer" (cadr e)))))
  (with-temp-buffer
    (kao-mode 1)
    (should (eq (kao--assert-mode) nil))
    (kao-mode -1)))

;;;; Mode-off guard sweep — (ADDITIVE pins)

;; The kao-state.el interactive commands over `kao--sels' (jump list, selection
;; history) share the same mode-off failure: with `kao-mode' off `kao--sels' is
;; nil and the command trips its struct accessor as (wrong-type-argument
;; kao-sels nil).  The shared `kao--assert-mode' guard (the mode guard) turns
;; that into a named `user-error'.  Driven via `call-interactively' — the M-x
;; path — in a fundamental-mode buffer.

(ert-deftest kao-state-jump-save-mode-off-guards ()
  "I4: `<c-s>' (`kao-jump-save') via M-x with `kao-mode' off signals the shared
guard, not the cryptic (wrong-type-argument kao-sels nil)."
  (with-temp-buffer                     ; fundamental-mode temp buffer: mode OFF
    (insert "abc")
    (let ((err (should-error (call-interactively #'kao-jump-save)
                             :type 'user-error)))
      (should (string-match-p "kao-mode is not active" (cadr err))))))

(ert-deftest kao-state-sel-undo-mode-off-guards ()
  "I4: `<a-u>' (`kao-sel-undo') via M-x with `kao-mode' off signals the shared
guard."
  (with-temp-buffer
    (insert "abc")
    (let ((err (should-error (call-interactively #'kao-sel-undo)
                             :type 'user-error)))
      (should (string-match-p "kao-mode is not active" (cadr err))))))

(ert-deftest kao-state-oneshot-mode-off-names-the-command ()
  "`kao-insert-one-shot' via M-x with `kao-mode' off names the
command in the guard message — its first body form is `kao--assert-mode' with
context, so the mode-off cause is legible instead of the mode-on \"no insert
session\".  Additive to the sweep."
  (with-temp-buffer                     ; fundamental-mode temp buffer: mode OFF
    (insert "abc")
    (let ((err (should-error (call-interactively #'kao-insert-one-shot)
                             :type 'user-error)))
      (should (string-match-p "^kao-insert-one-shot: kao-mode is not active"
                              (cadr err))))))

(provide 'kao-state-tests)
;;; kao-state-tests.el ends here
