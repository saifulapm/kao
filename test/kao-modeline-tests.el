;;; kao-modeline-tests.el --- Tests for kao-modeline -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the faithful Kakoune `mode_info' string (kao-modeline.el).
;; `kao-mode-line-string' is a pure reader over the buffer-local state, so each
;; branch is exercised by binding `kao--sels' / `kao--insert-active' /
;; `kao--count' in a temp buffer — no frame needed.  The lighter-wiring
;; integration tests (Task 2) live here too.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'kao-modeline)
(require 'kao-macro)                     ; `kao--macro-recording-reg' (recording flag)
(require 'kao-search)                    ; `kao-search-count' / `kao--search-count' (N/M)

(defun kao-modeline-tests--sels (n main)
  "Build a `kao-sels' of N trivial selections with main index MAIN."
  (kao-sels-make
   :list (cl-loop for i from 1 to n
                  collect (kao-sel-make :anchor i :cursor i))
   :main main))

;;;; Pure mode_info string (Task 1)

(ert-deftest kao-modeline-inactive-empty ()
  "No selection list (kao inactive) yields the empty string."
  (with-temp-buffer
    (setq kao--sels nil)
    (should (equal (kao-mode-line-string) ""))))

(ert-deftest kao-modeline-one-selection ()
  "One selection -> \" 1 sel\" (input_handler.cc:393-395)."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 1 0)
          kao--insert-active nil
          kao--count 0)
    (should (equal (kao-mode-line-string) " 1 sel"))))

(ert-deftest kao-modeline-many-selections-main-index ()
  "N selections -> \" {N} sels ({main+1})\" (1-based, :399-401)."
  (with-temp-buffer
    (setq kao--insert-active nil kao--count 0)
    (setq kao--sels (kao-modeline-tests--sels 3 0))
    (should (equal (kao-mode-line-string) " 3 sels (1)"))
    (setq kao--sels (kao-modeline-tests--sels 3 2))
    (should (equal (kao-mode-line-string) " 3 sels (3)"))))

(ert-deftest kao-modeline-normal-pending-count ()
  "A pending count appends \" param={count}\" in normal state (:406-409)."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 1 0)
          kao--insert-active nil
          kao--count 2)
    (should (equal (kao-mode-line-string) " 1 sel param=2"))))

(ert-deftest kao-modeline-normal-no-count-no-param ()
  "Count 0 shows no param segment."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 2 1)
          kao--insert-active nil
          kao--count 0)
    (should (equal (kao-mode-line-string) " 2 sels (2)"))))

(ert-deftest kao-modeline-insert-prefix ()
  "Insert state prefixes \"insert \" before the selection count (:1413-1417)."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 1 0)
          kao--insert-active t
          kao--count 0)
    (should (equal (kao-mode-line-string) " insert 1 sel"))
    (setq kao--sels (kao-modeline-tests--sels 3 1))
    (should (equal (kao-mode-line-string) " insert 3 sels (2)"))))

(ert-deftest kao-modeline-insert-ignores-count ()
  "Insert `mode_info' has no count segment, even with a count set (:1409-1419)."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 1 0)
          kao--insert-active t
          kao--count 5)
    (should (equal (kao-mode-line-string) " insert 1 sel"))))

;;;; Lighter wiring (Task 2)

(ert-deftest kao-modeline-lighter-is-eval-construct ()
  "kao-mode's lighter is the dynamic `(:eval (kao--lighter))' construct.
`kao--lighter' dispatches on `kao-lighter'; this is the wiring check, the actual
display is confirmed by a live `-nw' pty smoke."
  (require 'kao-state)
  (should (equal (cdr (assq 'kao-mode minor-mode-alist))
                 '((:eval (kao--lighter))))))

(ert-deftest kao-modeline-activated-state-renders ()
  "After `kao-mode' is enabled the lighter string reflects the live selections."
  (require 'kao-multi)
  (with-temp-buffer
    (insert "foo bar foo")
    (goto-char (point-min))
    (kao-mode 1)
    (should (equal (kao-mode-line-string) " 1 sel"))      ; fresh: one selection
    (kao-select-whole-buffer)                              ; `%' so `s' has a range
    (kao--select-regex-apply "foo")                        ; -> two matches
    (should (equal (kao-mode-line-string) " 2 sels (2)")) ; main = last
    (kao-mode -1)
    (should (equal (kao-mode-line-string) ""))))          ; disabled -> inactive

;;;; Recording flag (Task 3) — Kakoune `[recording ({reg})]' (client.cc:156)

(ert-deftest kao-modeline-recording-flag ()
  "While a macro records, the indicator appends \" [recording ({reg})]\"."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 1 0)
          kao--insert-active nil
          kao--count 0)
    (let ((kao--macro-recording-reg ?@))
      (should (equal (kao-mode-line-string) " 1 sel [recording (@)]")))
    (let ((kao--macro-recording-reg nil))
      (should (equal (kao-mode-line-string) " 1 sel")))))

(ert-deftest kao-modeline-recording-composes-with-count ()
  "The recording flag appends after the selection + pending-count segment."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 3 2)
          kao--insert-active nil
          kao--count 4)
    (let ((kao--macro-recording-reg ?@))
      (should (equal (kao-mode-line-string)
                     " 3 sels (3) param=4 [recording (@)]")))))

(ert-deftest kao-modeline-recording-shows-in-insert ()
  "The recording flag also shows in insert state (a client flag, not mode-bound)."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 1 0)
          kao--insert-active t
          kao--count 0)
    (let ((kao--macro-recording-reg ?@))
      (should (equal (kao-mode-line-string) " insert 1 sel [recording (@)]")))))

(ert-deftest kao-modeline-recording-inactive-stays-empty ()
  "An inactive buffer stays empty even while recording (no lighter to show)."
  (with-temp-buffer
    (setq kao--sels nil)
    (let ((kao--macro-recording-reg ?@))
      (should (equal (kao-mode-line-string) "")))))

(ert-deftest kao-modeline-pending-register-flag ()
  "A pending register appends \" reg=x\" after the param segment, RAW char
\(input_handler.cc:411-415 prints `m_params.reg' unlowered)."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 1 0)
          kao--insert-active nil
          kao--count 0
          kao--pending-register ?a)
    (should (equal (kao-mode-line-string) " 1 sel reg=a"))
    (setq kao--count 4
          kao--pending-register ?A)
    (should (equal (kao-mode-line-string) " 1 sel param=4 reg=A"))))

(ert-deftest kao-modeline-pending-register-not-in-insert ()
  "Insert `mode_info' never shows the reg segment (it is Normal's m_params)."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 1 0)
          kao--insert-active t
          kao--count 0
          kao--pending-register ?a)
    (should (equal (kao-mode-line-string) " insert 1 sel"))))

(ert-deftest kao-modeline-recording-named-register ()
  "The recording flag renders the actual (lowered) register: `[recording (b)]'."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 1 0)
          kao--insert-active nil
          kao--count 0
          kao--pending-register nil)
    (let ((kao--macro-recording-reg ?b))
      (should (equal (kao-mode-line-string) " 1 sel [recording (b)]")))))

;;;; Public rename + `kao-lighter' (dx-docs-7)

(ert-deftest kao-modeline-public-name-returns-string ()
  "`kao-mode-line-string' (public) returns the content the private name did."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 1 0)
          kao--insert-active nil
          kao--count 0)
    (should (equal (kao-mode-line-string) " 1 sel"))))

(ert-deftest kao-modeline-obsolete-alias-resolves ()
  "The old `kao--mode-line-string' name resolves to the public function.
The ONE deliberate call through the obsolete name (suppressed); every other
test goes through the public `kao-mode-line-string'."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 2 1)
          kao--insert-active nil
          kao--count 0)
    (should (eq (indirect-function 'kao--mode-line-string)
                (indirect-function 'kao-mode-line-string)))
    (should (equal (with-suppressed-warnings
                       ((obsolete kao--mode-line-string))
                     (kao--mode-line-string))
                   (kao-mode-line-string)))))

(ert-deftest kao-modeline-lighter-nil-hides ()
  "`kao-lighter' set to nil yields an empty lighter while kao-mode stays on."
  (with-temp-buffer
    (insert "abc")
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (let ((kao-lighter nil))
          (should kao-mode)
          (should (equal (kao--lighter) "")))
      (kao-mode -1))))

(ert-deftest kao-modeline-lighter-string-replaces ()
  "A string `kao-lighter' replaces the mode_info verbatim."
  (let ((kao-lighter " Kao"))
    (should (equal (kao--lighter) " Kao"))))

(ert-deftest kao-modeline-lighter-default-renders-mode-info ()
  "The default `(:eval (kao-mode-line-string))' lighter renders the mode_info."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 1 0)
          kao--insert-active nil
          kao--count 0)
    (should (equal kao-lighter '(:eval (kao-mode-line-string))))
    (should (equal (kao--lighter) " 1 sel"))))

;;;; Opt-in search count N/M segment (hel-study-6)

(ert-deftest kao-modeline-search-count-off-byte-identical ()
  "With `kao-search-count' nil the string carries no N/M — byte-identical to the
faithful mode_info, even with a stale recorded pair (hel-study-6, zero cost off)."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 1 0)
          kao--insert-active nil
          kao--count 0)
    (let ((kao-search-count nil)
          (kao--search-count '(2 . 5)))       ; a stale pair must NOT show when off
      (should (equal (kao-mode-line-string) " 1 sel")))))

(ert-deftest kao-modeline-search-count-appends-n-of-m ()
  "With `kao-search-count' set and a recorded (INDEX . TOTAL), the string ends in
` N/M' (hel-study-6)."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 1 0)
          kao--insert-active nil
          kao--count 0)
    (let ((kao-search-count t)
          (kao--search-count '(2 . 5)))
      (should (equal (kao-mode-line-string) " 1 sel 2/5")))))

(ert-deftest kao-modeline-search-count-absent-pair-no-segment ()
  "With `kao-search-count' set but no recorded pair (point not on a match), no
N/M is appended — only when the pair is available (hel-study-6)."
  (with-temp-buffer
    (setq kao--sels (kao-modeline-tests--sels 1 0)
          kao--insert-active nil
          kao--count 0)
    (let ((kao-search-count t)
          (kao--search-count nil))
      (should (equal (kao-mode-line-string) " 1 sel")))))

(provide 'kao-modeline-tests)
;;; kao-modeline-tests.el ends here
