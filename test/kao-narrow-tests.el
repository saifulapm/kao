;;; kao-narrow-tests.el --- Narrowed-buffer tests for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for kao under `narrow-to-region'.  The stance
;; Narrowing: kao operates within the accessible region
;; exactly like native Emacs commands — every clamp and walk is
;; `point-min'/`point-max'-relative, so a narrowed buffer behaves like a
;; buffer containing only the accessible text.  Kakoune has no narrowing;
;; this is Emacs-native behaviour, not a parity surface.

;;; Code:

(require 'ert)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-state)
(require 'kao-motion)
(require 'kao-register)
(require 'kao-edit)
(require 'kao-multi)
(require 'kao-object)
(require 'kao-menu)
(require 'kao-search)

;; Shared layout: "alpha beta\ngamma delta\nepsilon zeta\n", narrowed to the
;; middle line WITHOUT its trailing newline: accessible = "gamma delta",
;; point-min 12, point-max 23 (chars g12 a13 m14 m15 a16 SPC17 d18 e19 l20
;; t21 a22; the \n at 23 is outside).  The missing accessible newline also
;; exercises the final-line family at the narrow edge.

(defmacro kao-narrow-tests--with (&rest body)
  "Run BODY in a `kao-mode' temp buffer narrowed to \"gamma delta\".
A clean, isolated system clipboard is bound so clipboard-aware paste
reads the internal register path deterministically."
  (declare (indent 0))
  `(with-temp-buffer
     (insert "alpha beta\ngamma delta\nepsilon zeta\n")
     (buffer-enable-undo)
     (narrow-to-region 12 23)
     (goto-char (point-min))
     (kao-mode 1)
     (let ((kill-ring nil) (kill-ring-yank-pointer nil)
           (interprogram-cut-function nil) (interprogram-paste-function nil)
           (kao--clipboard-yank nil))
       (unwind-protect (progn ,@body)
         (kao-mode -1)))))

(defun kao-narrow-tests--span (anchor cursor &optional main)
  "Set `kao--sels' to a single selection ANCHOR..CURSOR (MAIN defaults to 0)."
  (setq kao--sels (kao-sels-make
                   :list (list (kao-sel-make :anchor anchor :cursor cursor))
                   :main (or main 0))))

(defun kao-narrow-tests--main ()
  "Return the main selection as a cons (ANCHOR . CURSOR)."
  (let ((s (kao--main-sel)))
    (cons (kao-sel-anchor s) (kao-sel-cursor s))))

;;;; hjkl at the narrow edges

(ert-deftest kao-narrow-hl-clamp-at-edges ()
  "`h' at the narrow start and `l' at the narrow end stay on-char inside."
  (kao-narrow-tests--with
    (kao-narrow-tests--span 12 12)
    (kao-left)
    (should (equal (kao-narrow-tests--main) '(12 . 12)))
    (kao-narrow-tests--span 22 22)
    (kao-right)
    (should (equal (kao-narrow-tests--main) '(22 . 22)))))

(ert-deftest kao-narrow-jk-noop-on-sole-accessible-line ()
  "`j'/`k' cannot leave the single accessible line."
  (kao-narrow-tests--with
    (kao-narrow-tests--span 14 14)
    (kao-down)
    (should (equal (kao-narrow-tests--main) '(14 . 14)))
    (kao-up)
    (should (equal (kao-narrow-tests--main) '(14 . 14)))))

;;;; Word motions

(ert-deftest kao-narrow-word-forward-and-end-clamp ()
  "`w' from the first word; `e' on the last word stops at the last char."
  (kao-narrow-tests--with
    (kao-narrow-tests--span 12 12)
    (kao-word-forward)                  ; "gamma " incl. trailing blank
    (should (equal (kao-narrow-tests--main) '(12 . 17)))
    (kao-narrow-tests--span 18 18)
    (kao-word-end)                      ; "delta", end clamped in-region
    (should (equal (kao-narrow-tests--main) '(18 . 22)))))

(ert-deftest kao-narrow-word-backward-stops-at-narrow-start ()
  "`b' from \"delta\" lands on \"gamma\"; from the start it cannot escape
into the (inaccessible) line above."
  (kao-narrow-tests--with
    (kao-narrow-tests--span 18 18)
    (kao-word-backward)
    (should (= (cdr (kao-narrow-tests--main)) 12))   ; cursor on 'g'
    (kao-narrow-tests--span 12 12)
    (kao-word-backward)                 ; at the edge: never reaches "beta"
    (should (>= (cdr (kao-narrow-tests--main)) 12))))

;;;; x — whole line, end clamped to the accessible region

(ert-deftest kao-narrow-line-select-clamps-to-region ()
  "`x' selects the accessible line; the cursor clamps to the last accessible
char (the line's newline sits outside the narrowing — family)."
  (kao-narrow-tests--with
    (kao-narrow-tests--span 14 14)
    (kao-line)
    (should (equal (kao-narrow-tests--main) '(12 . 22)))))

;;;; goto — top/end resolve to the accessible bounds

(ert-deftest kao-narrow-goto-top-and-end ()
  "`gg' goes to the narrow start; `ge' to the last accessible char."
  (kao-narrow-tests--with
    (kao-narrow-tests--span 18 18)
    (kao-goto--dispatch ?g 'replace)
    (should (equal (kao-narrow-tests--main) '(12 . 12)))
    (kao-goto--dispatch ?e 'replace)
    (should (equal (kao-narrow-tests--main) '(22 . 22)))))

;;;; % — whole "buffer" is the accessible region

(ert-deftest kao-narrow-whole-buffer-is-accessible-region ()
  "`%' selects exactly [point-min, point-max - 1]."
  (kao-narrow-tests--with
    (kao-narrow-tests--span 18 18)
    (kao-select-whole-buffer)
    (should (equal (kao-narrow-tests--main) '(12 . 22)))))

;;;; Word object at the narrow edges

(ert-deftest kao-narrow-object-word-stays-inside ()
  "The word object never walks past the accessible bounds."
  (kao-narrow-tests--with
    ;; "gamma" at the narrow start: begin-walk stops at point-min.
    (let ((s (kao-object--word (kao-sel-make :anchor 14 :cursor 14)
                               t #'kao-object--word-cat-p t t)))
      (should (= (kao-sel-min s) 12))
      (should (= (kao-sel-max s) 16)))
    ;; "delta" at the narrow end: end-walk stops at the last accessible char.
    (let ((s (kao-object--word (kao-sel-make :anchor 20 :cursor 20)
                               t #'kao-object--word-cat-p t t)))
      (should (= (kao-sel-min s) 18))
      (should (= (kao-sel-max s) 22)))))

;;;; Edits — delete and paste at the narrow edges

(ert-deftest kao-narrow-delete-clamps-and-paste-restores ()
  "`d' at the narrow end leaves a valid clamped selection; `p' re-inserts
within the accessible region; the text outside the narrowing is untouched."
  (kao-narrow-tests--with
    (kao-narrow-tests--span 18 22)      ; "delta"
    (kao-yank)
    (kao-delete)
    (should (string= (buffer-string) "gamma "))
    ;; Gap at old 18 = new point-max; selection clamps to the last char.
    (should (equal (kao-narrow-tests--main) '(17 . 17)))
    (kao-paste-after)                   ; after max -> back at the end
    (should (string= (buffer-string) "gamma delta"))
    (should (equal (kao-narrow-tests--main) '(18 . 22)))
    (widen)
    (should (string= (buffer-string)
                     "alpha beta\ngamma delta\nepsilon zeta\n"))))

(ert-deftest kao-narrow-paste-at-end-lands-before-excluded-newline ()
  "`p' on the last selection inserts at the accessible end — BEFORE the
excluded newline, never beyond the narrowing."
  (kao-narrow-tests--with
    (kao-narrow-tests--span 18 22)      ; "delta"
    (kao-yank)
    (kao-paste-after)
    (should (string= (buffer-string) "gamma deltadelta"))
    (should (equal (kao-narrow-tests--main) '(23 . 27)))
    (widen)
    (should (string= (buffer-string)
                     "alpha beta\ngamma deltadelta\nepsilon zeta\n"))))

;;;; Search — matches cannot escape the accessible region

(ert-deftest kao-narrow-search-confined-to-region ()
  "A pattern present only outside the narrowing finds nothing; an inside
pattern is found normally."
  (kao-narrow-tests--with
    (kao-narrow-tests--span 12 12)
    (should-not (kao-search--find-next-match
                 (kao--main-sel) t "zeta"))         ; only in line 3
    (should-not (kao-search--find-next-match
                 (kao--main-sel) nil "alpha"))      ; only in line 1
    (let ((hit (kao-search--find-next-match (kao--main-sel) t "delta")))
      (should hit)
      (should (= (kao-sel-min (car hit)) 18))
      (should (= (kao-sel-max (car hit)) 22)))))

;;;; Clamp — stale out-of-region positions pull into the accessible region

(ert-deftest kao-narrow-clamp-pulls-stale-positions-inside ()
  "`kao--clamp-sel' clamps positions outside the narrowing to its bounds."
  (kao-narrow-tests--with
    (let ((s (kao--clamp-sel (kao-sel-make :anchor 1 :cursor 35))))
      (should (= (kao-sel-anchor s) 12))
      (should (= (kao-sel-cursor s) 22)))))

(ert-deftest kao-narrow-edit-family-empty-region-no-signal ()
  "Edit commands clamp to the narrowed `point-max' — no `args-out-of-range' in
an empty accessible region, and the excluded text is untouched."
  (with-temp-buffer
    (insert "abc")
    (buffer-enable-undo)
    (narrow-to-region 2 2)              ; empty accessible region: point-min = point-max
    (kao-mode 1)
    (let ((kill-ring nil) (kill-ring-yank-pointer nil)
          (interprogram-cut-function nil) (interprogram-paste-function nil)
          (kao--clipboard-yank nil))
      (unwind-protect
          (progn
            (setq kao--sels (kao-sels-make
                             :list (list (kao-sel-make :anchor 2 :cursor 2)) :main 0))
            (kao-yank)
            (should (equal '("") (kao-register-get kao-register-default)))
            (kao-delete)
            (kao--replace-char-with ?x)
            (kao-upcase)
            (should (= (point-min) (point-max)))   ; region still empty
            (widen)
            (should (string= "abc" (buffer-string))))  ; excluded text untouched
        (kao-mode -1)))))

;;;; History navigation under narrowing — native-undo-parity refusal

;; replaced native undo, whose primitive-undo refuses cleanly when an undo
;; record lies outside the accessible region ("Changes to be undone are outside
;; visible portion of buffer").  kao's tree navigation must keep that refusal:
;; a walk touching a position outside [point-min, point-max] is rejected UP
;; FRONT, before any buffer edit, so `kao--hist-id' never desyncs from content.

(ert-deftest kao-narrow-undo-refuses-out-of-region-walk ()
  "Undo under narrowing whose node touches text outside the accessible region
refuses cleanly (native-undo message), leaving the buffer AND `kao--hist-id'
untouched; a subsequent widened undo then restores the original exactly."
  (with-temp-buffer
    (insert (make-string 200 ?x))            ; original 200-char fixture
    (buffer-enable-undo)
    (set-buffer-modified-p nil)
    (kao-mode 1)
    (unwind-protect
        (progn
          ;; ONE command, TWO modifications -> one node: FAR far outside a later
          ;; narrowing, NEAR inside it.  Revert order (NEAR then FAR) is what
          ;; half-applied the node in the repro.
          (goto-char 140) (insert "FAR")
          (goto-char 10) (insert "NEAR")
          (kao--hist-maybe-commit)
          (should (= (kao-history-current-id) 1))
          (let ((full (buffer-string)))       ; 207 chars, still widened
            (should (= (length full) 207))
            (narrow-to-region 1 60)           ; FAR (~144) is outside; NEAR is in
            (let ((err (should-error (kao-undo) :type 'user-error)))
              (should (string-match-p "outside visible portion of buffer"
                                      (cadr err))))
            ;; Refused up front: nothing changed.
            (should (= (kao-history-current-id) 1))
            (widen)
            (should (string= (buffer-string) full))
            ;; Widened, the same undo now succeeds and restores the original.
            (kao-undo)
            (should (= (kao-history-current-id) 0))
            (should (string= (buffer-string) (make-string 200 ?x)))))
      (kao-mode -1))))

(ert-deftest kao-narrow-undo-inside-region-succeeds ()
  "An undo whose modifications lie entirely inside the narrowing still works —
the refusal must not over-reject in-region walks (regression guard)."
  (with-temp-buffer
    (insert (make-string 200 ?x))
    (buffer-enable-undo)
    (set-buffer-modified-p nil)
    (kao-mode 1)
    (unwind-protect
        (progn
          (goto-char 30) (insert "YY")        ; one mod at 30, inside [1,60]
          (kao--hist-maybe-commit)
          (should (= (kao-history-current-id) 1))
          (narrow-to-region 1 60)
          (kao-undo)                          ; entirely inside -> succeeds
          (should (= (kao-history-current-id) 0))
          (widen)
          (should (string= (buffer-string) (make-string 200 ?x))))
      (kao-mode -1))))

(provide 'kao-narrow-tests)
;;; kao-narrow-tests.el ends here
