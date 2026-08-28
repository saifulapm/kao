;;; kao-objects-tests.el --- Tests for kao-objects -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the opt-in `tag' text object.  The
;; delimiter regexps are pinned against fixtures FIRST (the hand-translated
;; negative-lookbehind must exclude self-closing tags), then the object is
;; exercised at the selector level, the nested-walk level, and end-to-end
;; through the `<a-i>'/`<a-a>' object-pending dispatch via `kao-object-register'.
;; Registration must NOT touch the built-in object set (parity).

;;; Code:

(require 'ert)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-motion)
(require 'kao-state)
(require 'kao-object)
(require 'kao-keys)
(require 'kao-objects)

;;;; The hand-translated delimiter regexps (prototype pins).

(defun kao-objects-tests--match (regexp s)
  "Return the first match of REGEXP in string S, or nil."
  (when (string-match regexp s)
    (match-string 0 s)))

(ert-deftest kao-objects-tag-open-regexp-matches-open-tags ()
  "The open regexp matches opening tags, attributes, hyphenated and bare names."
  (should (equal (kao-objects-tests--match kao-objects-tag-open-regexp "<div>") "<div>"))
  (should (equal (kao-objects-tests--match kao-objects-tag-open-regexp "<a>") "<a>"))
  (should (equal (kao-objects-tests--match kao-objects-tag-open-regexp "<my-tag>") "<my-tag>"))
  (should (equal (kao-objects-tests--match kao-objects-tag-open-regexp "<div class=\"x\">")
                 "<div class=\"x\">"))
  (should (equal (kao-objects-tests--match kao-objects-tag-open-regexp "<p>text") "<p>")))

(ert-deftest kao-objects-tag-open-regexp-excludes-self-closing ()
  "The consumed-guard-char rewrite excludes self-closing tags (the `(?<!/)')."
  (should-not (kao-objects-tests--match kao-objects-tag-open-regexp "<br/>"))
  (should-not (kao-objects-tests--match kao-objects-tag-open-regexp "<input type=\"x\"/>"))
  ;; The open regexp must not match a CLOSING tag either.
  (should-not (kao-objects-tests--match kao-objects-tag-open-regexp "</div>")))

(ert-deftest kao-objects-tag-close-regexp ()
  "The close regexp matches closing tags only, not opening ones."
  (should (equal (kao-objects-tests--match kao-objects-tag-close-regexp "</div>") "</div>"))
  (should-not (kao-objects-tests--match kao-objects-tag-close-regexp "<div>")))

;;;; Selector level (mirror kao-object-tests--pair).

(defun kao-objects-tests--sel (content cursor inner)
  "Return `kao-objects--tag' for a cursor at CURSOR (1-based) in CONTENT.
INNER selects between the tags; whole includes them."
  (with-temp-buffer
    (insert content)
    (kao-objects--tag (kao-sel-make :anchor cursor :cursor cursor) inner t t)))

(ert-deftest kao-objects-tag-inner-and-whole ()
  "`<a-i>T' selects between the tags; `<a-a>T' the whole pair.  \"<p>hi</p>\"."
  ;; < 1 p2 > 3 h4 i5 < 6 / 7 p8 > 9
  (let ((i (kao-objects-tests--sel "<p>hi</p>" 4 t)))
    (should (= (kao-sel-min i) 4))
    (should (= (kao-sel-max i) 5)))               ; "hi"
  (let ((w (kao-objects-tests--sel "<p>hi</p>" 4 nil)))
    (should (= (kao-sel-min w) 1))
    (should (= (kao-sel-max w) 9))))              ; "<p>hi</p>"

(ert-deftest kao-objects-tag-nesting-innermost ()
  "A nested cursor picks the innermost enclosing tag pair.  \"<div><p>x</p></div>\"."
  ;; < 1 d2 i3 v4 > 5 < 6 p7 > 8 x9 < 10 / 11 p12 > 13 < 14 / 15 d16 i17 v18 > 19
  (let ((i (kao-objects-tests--sel "<div><p>x</p></div>" 9 t)))
    (should (= (kao-sel-min i) 9))
    (should (= (kao-sel-max i) 9)))               ; "x"
  (let ((w (kao-objects-tests--sel "<div><p>x</p></div>" 9 nil)))
    (should (= (kao-sel-min w) 6))
    (should (= (kao-sel-max w) 13))))             ; "<p>x</p>"

(ert-deftest kao-objects-tag-not-enclosed-nil ()
  "A cursor with no enclosing tag pair yields nil (dropped)."
  (should (null (kao-objects-tests--sel "x<p>a</p>" 1 t)))
  ;; A self-closing tag forms no pair, so a cursor on it has no enclosing tag.
  (should (null (kao-objects-tests--sel "<br/>" 3 nil))))

;;;; Nested walk (<a-I>T / <a-A>T) — the span list.

(ert-deftest kao-objects-tag-nested-walk ()
  "The nested walk returns every tag pair's inner / whole span.
\"<a>x</a><b>y</b>\": two sibling pairs."
  ;; < 1 a2 > 3 x4 < 5 / 6 a7 > 8 < 9 b10 > 11 y12 < 13 / 14 b15 > 16
  (with-temp-buffer
    (insert "<a>x</a><b>y</b>")
    (should (equal (kao-objects--tag-nested 1 (point-max) t)
                   '((4 . 4) (12 . 12))))          ; the "x" and "y"
    (should (equal (kao-objects--tag-nested 1 (point-max) nil)
                   '((1 . 8) (9 . 16))))))         ; "<a>x</a>" and "<b>y</b>"

;;;; Registration (opt-in) — must not touch the built-in object set.

(defmacro kao-objects-tests--with-runtime (&rest body)
  "Run BODY with empty, isolated runtime object alists (restored on exit)."
  (declare (indent 0))
  `(let ((kao--object-runtime-table nil)
         (kao--object-runtime-info nil)
         (kao--object-runtime-nested nil))
     ,@body))

(ert-deftest kao-objects-tag-not-registered-by-default ()
  "Loading kao-objects registers nothing — `T' has no object until you opt in."
  (kao-objects-tests--with-runtime
    (should (null (kao--object-selector ?T)))
    (should-not (assq ?T kao--object-table))      ; never a built-in (parity)
    (should-not (assq ?T kao--object-nested-table))))

(ert-deftest kao-objects-register-tag-wires-selector ()
  "`kao-objects-register-tag' registers the selector, nested walk, and info row."
  (kao-objects-tests--with-runtime
    (kao-objects-register-tag)
    (should (eq (kao--object-selector ?T) #'kao-objects--tag))
    (should (eq (kao--object-nested-selector ?T) #'kao-objects--tag-nested))
    (should (equal (cdr (assq ?T (kao--object-info-rows))) "tag"))
    ;; The frozen built-in defconsts stay untouched.
    (should-not (assq ?T kao--object-table))
    (should-not (assq ?T kao--object-info))))

(ert-deftest kao-objects-register-tag-custom-key ()
  "A custom KEY binds the tag object to that key instead of `T'."
  (kao-objects-tests--with-runtime
    (kao-objects-register-tag ?t)
    (should (eq (kao--object-selector ?t) #'kao-objects--tag))
    (should (null (kao--object-selector ?T)))))

;;;; End-to-end through the object-pending dispatch.

(ert-deftest kao-objects-tag-dispatch-end-to-end ()
  "After registering, `<a-i>T' selects the inner tag through the real dispatch."
  (kao-objects-tests--with-runtime
    (kao-objects-register-tag)
    (with-temp-buffer
      (insert "<p>hi</p>")
      (kao-mode 1)
      (unwind-protect
          (progn
            (setq kao--sels (kao-sels-make
                             :list (list (kao-sel-make :anchor 4 :cursor 4))
                             :main 0))
            (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?T)))
              (kao-select-inner))
            (should (equal (mapcar (lambda (s) (cons (kao-sel-min s) (kao-sel-max s)))
                                   (kao-sels-list kao--sels))
                           '((4 . 5)))))            ; "hi"
        (kao-mode -1)))))

(provide 'kao-objects-tests)
;;; kao-objects-tests.el ends here
