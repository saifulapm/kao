;;; kao-config-substrate-tests.el --- Tests for kao's public config substrate -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT pins for — the public config substrate that lets a config layer
;; build on `kao-' symbols alone: the selection-list API (A1), the per-selection
;; edit primitives (A5), the `-draft' macro (A2), state/selection hooks (A3),
;; `kao-on-key' (A4), and the surround gate (A-demo) proving surround add/delete/
;; replace are buildable with NO private `kao--' symbol and NO `setq kao--sels'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-state)
(require 'kao-register)
(require 'kao-edit)
(require 'kao-info)
(require 'kao-macro)                     ; macro-record state for the A4 pin
(require 'kao-keys)                      ; default bindings

(defmacro kao-substrate-tests--with (content &rest body)
  "Run BODY in a `kao-mode' temp buffer initialised to CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (buffer-enable-undo)
     (kao-mode 1)
     (unwind-protect (progn ,@body)
       (kao-mode -1))))

(defun kao-substrate-tests--pairs ()
  "Return the live selections as a list of (ANCHOR . CURSOR) integer conses."
  (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
          (kao-sels-list kao--sels)))

;;;; A1 — public selection-list API

(ert-deftest kao-substrate-get-selections-returns-copies ()
  "`kao-get-selections' returns copies; mutating one cannot corrupt the engine."
  (kao-substrate-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 2)))
    (let ((got (kao-get-selections)))
      (should (= (length got) 1))
      (setf (kao-sel-cursor (car got)) 99)
      (should (= (kao-sel-cursor (kao--main-sel)) 2)))))

(ert-deftest kao-substrate-set-selections-normalizes ()
  "`kao-set-selections' sorts and merges overlapping members."
  (kao-substrate-tests--with "abcdef"
    ;; two overlapping spans [2..5] and [1..3] out of order -> one [1..5]
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 5)
                              (kao-sel-make :anchor 1 :cursor 3)))
    (should (equal (kao-substrate-tests--pairs) '((1 . 5))))))

(ert-deftest kao-substrate-set-selections-raw-keeps-overlap ()
  "`kao-set-selections-raw' installs an overlapping list verbatim."
  (kao-substrate-tests--with "abcdef"
    (kao-set-selections-raw (list (kao-sel-make :anchor 1 :cursor 3)
                                  (kao-sel-make :anchor 2 :cursor 5)))
    (should (equal (kao-substrate-tests--pairs) '((1 . 3) (2 . 5))))))

(ert-deftest kao-substrate-set-selections-empty-collapses ()
  "`kao-set-selections' with an empty list keeps one selection at point."
  (kao-substrate-tests--with "abcdef"
    (goto-char 3)
    (kao-set-selections nil)
    (should (= (length (kao-sels-list kao--sels)) 1))))

(ert-deftest kao-substrate-set-selections-main-clamped ()
  "`kao-set-selections' clamps an out-of-range MAIN into the list."
  (kao-substrate-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)
                              (kao-sel-make :anchor 3 :cursor 3))
                        9)
    (should (= (kao-sels-main kao--sels) 1))))

(ert-deftest kao-substrate-map-selections-drops-on-nil ()
  "`kao-map-selections' drops a selection when FN returns nil."
  (kao-substrate-tests--with "abcdef"
    (kao-set-selections-raw (list (kao-sel-make :anchor 1 :cursor 1)
                                  (kao-sel-make :anchor 3 :cursor 3)
                                  (kao-sel-make :anchor 5 :cursor 5)))
    (kao-map-selections (lambda (s) (if (= (kao-sel-cursor s) 3) nil s)))
    (should (equal (kao-substrate-tests--pairs) '((1 . 1) (5 . 5))))))

(ert-deftest kao-substrate-map-selections-all-dropped-noop ()
  "`kao-map-selections' dropping every selection is a silent no-op (D-A1c)."
  (kao-substrate-tests--with "abcdef"
    (kao-set-selections-raw (list (kao-sel-make :anchor 1 :cursor 1)
                                  (kao-sel-make :anchor 3 :cursor 3)))
    (kao-map-selections (lambda (_s) nil))
    (should (equal (kao-substrate-tests--pairs) '((1 . 1) (3 . 3))))))

(ert-deftest kao-substrate-map-selections-extend-merges ()
  "`kao-map-selections-extend' moves the cursor, keeping the anchor (extend)."
  (kao-substrate-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 2)))
    (kao-map-selections-extend (lambda (_s) (kao-sel-make :anchor 5 :cursor 5)))
    (should (equal (kao-substrate-tests--pairs) '((2 . 5))))))

;;;; A5 — public per-selection edit primitives

(ert-deftest kao-substrate-edit-selections-one-undo-unit ()
  "`kao-edit-selections' edits every selection as one undo unit, re-deriving sels."
  (kao-substrate-tests--with "abc"
    (undo-boundary)
    (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)
                              (kao-sel-make :anchor 2 :cursor 2)
                              (kao-sel-make :anchor 3 :cursor 3)))
    (kao-edit-selections
     (lambda (am _cm _i)
       (let* ((p (marker-position am))
              (ch (upcase (char-after p))))
         (delete-region p (1+ p))
         (save-excursion (goto-char p) (insert (char-to-string ch)))
         (cons p p))))
    (should (string= (buffer-string) "ABC"))
    (should (equal (kao-substrate-tests--pairs) '((1 . 1) (2 . 2) (3 . 3))))
    (primitive-undo 1 buffer-undo-list)
    (should (string= (buffer-string) "abc"))))

(ert-deftest kao-substrate-edit-keeping-selections-carries ()
  "`kao-edit-keeping-selections' keeps the original span selected across inserts."
  (kao-substrate-tests--with "abcdef"
    (undo-boundary)
    ;; one selection covering "bcd" = [2..4]
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 4)))
    (kao-edit-keeping-selections
     (lambda (marks)
       (dolist (m marks)
         (let* ((a (car m)) (c (cdr m))
                (beg (min (marker-position a) (marker-position c)))
                (end (1+ (max (marker-position a) (marker-position c)))))
           (save-excursion
             (goto-char end) (insert ")")
             (goto-char beg) (insert "("))))))
    (should (string= (buffer-string) "a(bcd)ef"))
    ;; original span "bcd" stays selected, shifted right by the inserted "("
    (should (equal (kao-substrate-tests--pairs) '((3 . 5))))
    (primitive-undo 1 buffer-undo-list)
    (should (string= (buffer-string) "abcdef"))))

;;;; A2 — kao-with-saved-selections (markers -draft)

(ert-deftest kao-substrate-draft-restores-edit-tracked ()
  "`kao-with-saved-selections' restores selections at edit-shifted positions."
  (kao-substrate-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 5 :cursor 6)))   ; "ef"
    (kao-with-saved-selections
      (goto-char 1) (insert "XYZ"))            ; saved span shifts +3
    (should (string= (buffer-string) "XYZabcdef"))
    (should (equal (kao-substrate-tests--pairs) '((8 . 9))))))

(ert-deftest kao-substrate-draft-restores-on-error ()
  "`kao-with-saved-selections' restores the selection even when BODY errors."
  (kao-substrate-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 3)))   ; "bc"
    (should-error
     (kao-with-saved-selections
       (goto-char 1) (insert "X")
       (error "boom")))
    (should (string= (buffer-string) "Xabcdef"))
    (should (equal (kao-substrate-tests--pairs) '((3 . 4))))))

(ert-deftest kao-substrate-draft-surround-add-recover ()
  "Insert open-before-min + close-after-max in BODY; the draft re-covers content."
  (kao-substrate-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 4)))   ; "bcd"
    (kao-with-saved-selections
      (let* ((s (car (kao-get-selections)))
             (beg (min (kao-sel-anchor s) (kao-sel-cursor s)))
             (end (1+ (max (kao-sel-anchor s) (kao-sel-cursor s)))))
        (goto-char end) (insert ")")
        (goto-char beg) (insert "(")))
    (should (string= (buffer-string) "a(bcd)ef"))
    (should (equal (kao-substrate-tests--pairs) '((3 . 5))))))   ; "bcd" re-covered

(ert-deftest kao-substrate-draft-restores-after-shrink ()
  "`kao-with-saved-selections' restores across a deletion inside the span."
  (kao-substrate-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 5)))   ; "bcde"
    (kao-with-saved-selections
      (delete-region 4 5))                     ; delete "d"
    (should (string= (buffer-string) "abcef"))
    (should (equal (kao-substrate-tests--pairs) '((2 . 4))))))

(ert-deftest kao-substrate-draft-preserves-captures ()
  "`kao-with-saved-selections' restores a selection's captures."
  (kao-substrate-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 3
                                            :captures '("bc" "x"))))
    (kao-with-saved-selections
      (goto-char 1) (insert "Z"))
    (should (equal (kao-sel-captures (kao--main-sel)) '("bc" "x")))))

(ert-deftest kao-substrate-draft-preserves-overlap ()
  "`kao-with-saved-selections' restores an overlapping list un-merged (raw)."
  (kao-substrate-tests--with "abcdef"
    (kao-set-selections-raw (list (kao-sel-make :anchor 1 :cursor 3)
                                  (kao-sel-make :anchor 2 :cursor 5)))
    (kao-with-saved-selections
      (ignore))
    (should (equal (kao-substrate-tests--pairs) '((1 . 3) (2 . 5))))))

;;;; A3 — state-transition and selection-change hooks

(ert-deftest kao-substrate-hook-insert-entry-exit-fire-once ()
  "Insert entry/exit hooks fire exactly once per `kao-insert'/`kao-insert-exit'."
  (kao-substrate-tests--with "abc"
    (let ((entries 0) (exits 0))
      (let ((kao-insert-state-entry-hook (list (lambda () (cl-incf entries))))
            (kao-insert-state-exit-hook (list (lambda () (cl-incf exits)))))
        (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
        (kao-insert)
        (should (= entries 1))
        (should (= exits 0))
        (kao-insert-exit)
        (should (= entries 1))
        (should (= exits 1))))))

(ert-deftest kao-substrate-hook-open-below-one-entry ()
  "`o' (open-below) fires exactly one insert-entry."
  (kao-substrate-tests--with "abc"
    (let ((entries 0))
      (let ((kao-insert-state-entry-hook (list (lambda () (cl-incf entries)))))
        (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
        (kao-open-below)
        (should (= entries 1))
        (kao-insert-exit)))))

(ert-deftest kao-substrate-hook-oneshot-cmd-reentry ()
  "`<a-;>' to normal fires exit+normal-entry; the pop back fires one entry."
  (kao-substrate-tests--with "abc"
    (let ((entries 0) (exits 0) (normals 0))
      (let ((kao-insert-state-entry-hook (list (lambda () (cl-incf entries))))
            (kao-insert-state-exit-hook (list (lambda () (cl-incf exits))))
            (kao-normal-state-entry-hook (list (lambda () (cl-incf normals)))))
        (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
        (kao-insert)                       ; entry 1
        (kao-insert-one-shot)              ; exit 1 + normal 1
        (should (= entries 1))
        (should (= exits 1))
        (should (= normals 1))
        (kao--oneshot-pop)                 ; entry 2 (re-entry, nothing changed)
        (should (= entries 2))
        (should (= exits 1))))))

(ert-deftest kao-substrate-hook-oneshot-nested-insert-single-entry ()
  "`<a-;> i' fires exit then exactly one entry; the pop fires no extra entry."
  (kao-substrate-tests--with "abc"
    (let ((entries 0) (exits 0))
      (let ((kao-insert-state-entry-hook (list (lambda () (cl-incf entries))))
            (kao-insert-state-exit-hook (list (lambda () (cl-incf exits)))))
        (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
        (kao-insert)                       ; entry 1
        (kao-insert-one-shot)              ; exit 1
        (kao-insert)                       ; nested re-entry: entry 2
        (should (= entries 2))
        (kao--oneshot-pop)                 ; insert already active -> no entry
        (should (= entries 2))
        (should (= exits 1))
        (kao-insert-exit)))))

(ert-deftest kao-substrate-hook-setup-abort-pairs ()
  "A setup-abort (read-only text mid-buffer) fires a paired entry+exit."
  (kao-substrate-tests--with "ab\ncd"
    (let ((entries 0) (exits 0))
      (let ((kao-insert-state-entry-hook (list (lambda () (cl-incf entries))))
            (kao-insert-state-exit-hook (list (lambda () (cl-incf exits)))))
        (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
        (put-text-property (point-min) (point-max) 'read-only t)
        (should-error (kao-open-below))    ; the newline insert hits read-only
        (should (= entries 1))             ; entry fired by begin
        (should (= exits 1))               ; abort fired the paired exit
        (should-not kao--insert-active)
        (let ((inhibit-read-only t))
          (remove-text-properties (point-min) (point-max) '(read-only nil)))))))

(ert-deftest kao-substrate-hook-selection-change-fires-on-change ()
  "`kao-selection-change-hook' fires when a command changes the selection."
  (kao-substrate-tests--with "abcdef"
    (let ((changes 0))
      (let ((kao-selection-change-hook (list (lambda () (cl-incf changes)))))
        (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
        (kao-map-selections (lambda (_s) (kao-sel-make :anchor 3 :cursor 3)))
        (kao--sel-history-record)          ; the post-command recorder
        (should (= changes 1))))))

(ert-deftest kao-substrate-hook-selection-change-silent-on-noop ()
  "`kao-selection-change-hook' does NOT fire when the selection is unchanged."
  (kao-substrate-tests--with "abcdef"
    (let ((changes 0))
      (let ((kao-selection-change-hook (list (lambda () (cl-incf changes)))))
        (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 2)))
        (kao--sel-history-record)          ; records the change
        (setq changes 0)
        (kao--sel-history-record)          ; nothing changed -> no fire
        (should (= changes 0))))))

(ert-deftest kao-substrate-hook-selection-change-fires-on-sel-undo ()
  "`kao-selection-change-hook' fires on `kao-sel-undo' (recorder bypassed)."
  (kao-substrate-tests--with "abcdef"
    (let ((changes 0))
      (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 1)))
      (kao--sel-history-record)
      (kao-set-selections (list (kao-sel-make :anchor 4 :cursor 4)))
      (kao--sel-history-record)
      (let ((kao-selection-change-hook (list (lambda () (cl-incf changes)))))
        (kao-sel-undo)
        (should (>= changes 1))))))

;;;; A4 — public kao-on-key (macro-safe one-shot read)

(ert-deftest kao-substrate-on-key-dispatches ()
  "`kao-on-key' reads one key and passes it to FN."
  (kao-substrate-tests--with "abc"
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?z)))
      (let (got)
        (kao-on-key "pick: " (lambda (k) (setq got k)))
        (should (= got ?z))))))

(ert-deftest kao-substrate-on-key-macro-safe ()
  "`kao-on-key' pushes the consumed key onto the recorded macro stream."
  (kao-substrate-tests--with "abc"
    (let ((kao--macro-recording-reg ?@)
          (kao--macro-recorded-keys nil)
          (kao--macro-replaying nil))
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
        (let (got)
          (kao-on-key "p: " (lambda (k) (setq got k)))
          (should (= got ?w))
          (should (equal kao--macro-recorded-keys (list (vector ?w)))))))))

(ert-deftest kao-substrate-on-key-box-torn-down ()
  "`kao-on-key' tears down the info box on exit, even when FN errors."
  (kao-substrate-tests--with "abc"
    (let ((shows 0) (hides 0))
      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?x))
                ((symbol-function 'kao-info--show)
                 (lambda (&rest _) (setq shows (1+ shows))))
                ((symbol-function 'kao-info--hide)
                 (lambda (&rest _) (setq hides (1+ hides)))))
        (kao-on-key "p: " #'ignore '((?x . "do x")))
        (should (= shows 1))
        (should (= hides 1))
        (should-error (kao-on-key "p: " (lambda (_k) (error "boom"))
                                  '((?x . "do x"))))
        (should (= hides 2))))))

;;;; A-demo — surround built on PUBLIC symbols only (the §00 binding gate)

;; A self-contained surround implementation using ONLY public `kao-' symbols
;; (no `kao--', no `setq kao--sels') — the proof that the substrate is
;; sufficient to build a real config feature in plain elisp on top of kao.
;; Symbols exercised: kao-edit-keeping-selections, kao-edit-selections,
;; kao-on-key, kao-map-selections, kao-with-saved-selections, kao-get/set-
;; selections, kao-register-set/-get, and the public kao-sel-* accessors.

(defconst kao-substrate-demo--pairs
  '((?\( ?\( . ?\)) (?\) ?\( . ?\))
    (?{  ?{  . ?})  (?}  ?{  . ?})
    (?\" ?\" . ?\"))
  "Delimiter key -> (OPEN . CLOSE) surround pair table.")

(defun kao-substrate-demo-read-pair ()
  "Read a delimiter via `kao-on-key' and return its (OPEN . CLOSE) pair."
  (kao-on-key "surround: "
              (lambda (k) (cdr (assq k kao-substrate-demo--pairs)))))

(defun kao-substrate-demo-surround-add (open close)
  "Wrap each selection with OPEN/CLOSE, keeping the original span selected."
  (kao-edit-keeping-selections
   (lambda (marks)
     (dolist (m marks)
       (let ((beg (min (marker-position (car m)) (marker-position (cdr m))))
             (end (1+ (max (marker-position (car m)) (marker-position (cdr m))))))
         (save-excursion
           (goto-char end) (insert (char-to-string close))
           (goto-char beg) (insert (char-to-string open))))))))

(defun kao-substrate-demo-surround-delete ()
  "Delete the one-char delimiters around each selection, reselecting the inside."
  (kao-edit-selections
   (lambda (am cm _i)
     (let* ((a (marker-position am)) (c (marker-position cm))
            (beg (min a c)) (end (max a c)) (fwd (<= a c)))
       (delete-region (1+ end) (+ 2 end))   ; the close, after the span
       (delete-region (1- beg) beg)         ; the open, before the span
       (if fwd (cons (1- beg) (1- end)) (cons (1- end) (1- beg)))))))

(defun kao-substrate-demo-surround-replace (open close)
  "Replace the surrounding pair with OPEN/CLOSE (delete old, then add new)."
  (kao-substrate-demo-surround-delete)
  (kao-substrate-demo-surround-add open close))

(ert-deftest kao-substrate-demo-add ()
  "Surround add (public edit primitive + `kao-on-key') wraps and reselects."
  (kao-substrate-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 4)))   ; "bcd"
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?\()))
      (let ((pair (kao-substrate-demo-read-pair)))
        (kao-substrate-demo-surround-add (car pair) (cdr pair))))
    (should (string= (buffer-string) "a(bcd)ef"))
    (should (equal (kao-substrate-tests--pairs) '((3 . 5))))))

(ert-deftest kao-substrate-demo-delete ()
  "Surround delete removes the pair and reselects the inner content."
  (kao-substrate-tests--with "a(bcd)ef"
    (kao-set-selections (list (kao-sel-make :anchor 3 :cursor 5)))   ; "bcd"
    (kao-substrate-demo-surround-delete)
    (should (string= (buffer-string) "abcdef"))
    (should (equal (kao-substrate-tests--pairs) '((2 . 4))))))

(ert-deftest kao-substrate-demo-replace ()
  "Surround replace swaps the pair, keeping the inner content selected."
  (kao-substrate-tests--with "a(bcd)ef"
    (kao-set-selections (list (kao-sel-make :anchor 3 :cursor 5)))   ; "bcd"
    (kao-substrate-demo-surround-replace ?{ ?})
    (should (string= (buffer-string) "a{bcd}ef"))
    (should (equal (kao-substrate-tests--pairs) '((3 . 5))))))

(ert-deftest kao-substrate-demo-add-multi-one-undo ()
  "Surround add wraps multiple selections as one undo unit."
  (kao-substrate-tests--with "ab cd"
    (kao-set-selections (list (kao-sel-make :anchor 1 :cursor 2)
                              (kao-sel-make :anchor 4 :cursor 5)))
    (undo-boundary)
    (kao-substrate-demo-surround-add ?\( ?\))
    (should (string= (buffer-string) "(ab) (cd)"))
    (should (equal (kao-substrate-tests--pairs) '((2 . 3) (7 . 8))))
    (primitive-undo 1 buffer-undo-list)
    (should (string= (buffer-string) "ab cd"))))

(ert-deftest kao-substrate-demo-draft-register-map ()
  "A draft + register + map recipe, public-symbols only."
  (kao-substrate-tests--with "abcdef"
    (kao-set-selections (list (kao-sel-make :anchor 2 :cursor 3)))   ; "bc"
    ;; stash the selection text in a public text register
    (let ((s (car (kao-get-selections))))
      (kao-register-set ?a (list (buffer-substring (kao-sel-beg s)
                                                   (kao-sel-end s)))))
    (should (equal (kao-register-get ?a) '("bc")))
    ;; surround inside a draft so the ORIGINAL span is restored afterward
    (kao-with-saved-selections
      (kao-substrate-demo-surround-add ?\( ?\)))
    (should (string= (buffer-string) "a(bc)def"))
    (should (equal (kao-substrate-tests--pairs) '((3 . 4))))
    ;; the drop-capable transform runs (identity here) and keeps the span
    (kao-map-selections #'identity)
    (should (equal (kao-substrate-tests--pairs) '((3 . 4))))))

(provide 'kao-config-substrate-tests)
;;; kao-config-substrate-tests.el ends here
