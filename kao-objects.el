;;; kao-objects.el --- Opt-in extra text objects for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Optional, opt-in
;; text objects that build ON TOP of kao's public object API
;; (`kao-object-register') rather than the core defconsts — the home for
;; objects that a Kakoune config defined with `map ... object', which kao does
;; NOT ship as built-ins (Kakoune has no built-in `tag' object, so registering
;; one by default would break the built-in-object parity the core guarantees).
;;
;; This file registers NOTHING on load.  Opt in from your config:
;;
;;   (require 'kao-objects)
;;   (kao-objects-register-tag)   ; <a-a>T / <a-i>T and <a-A>T / <a-I>T
;;
;; The `tag' object is the worked example of an "untranslatable" regex object
;; from docs/regex-porting.md: Kakoune's tag delimiters use a negative
;; lookbehind `(?<!/)' (a `>' not preceded by `/', to skip self-closing tags),
;; which Emacs regex cannot express directly.  kao hand-translates it by
;; CONSUMING the guard char and forbidding `/' there (`[^/>]' before `>') — a
;; rewrite a mechanical translator could not do safely.  The object then
;; delegates to the same generic pair engine as the built-in `c' custom object
;; (`kao-object--surrounding' / `kao-object--nested-pairs'), so it
;; nests and honours a count exactly like the bracket objects.

;;; Code:

(require 'kao-object)

(defconst kao-objects-tag-open-regexp
  "<[[:word:]][-[:word:]]*\\(?:[^>]*[^/>]\\)?>"
  "Emacs regexp matching an opening HTML/XML tag, excluding self-closing ones.
Hand-translated from Kakoune's tag open `<\\w[\\w-]*\\h*[^>]*?(?<!/)>': the
negative lookbehind `(?<!/)' has no Emacs equivalent, so the char before `>'
is consumed and forbidden to be `/' via `[^/>]'.  So `<br/>' and `<input ./>'
are NOT opening tags.  See docs/regex-porting.md.")

(defconst kao-objects-tag-close-regexp
  "</[[:word:]][-[:word:]]*>"
  "Emacs regexp matching a closing HTML/XML tag.
From Kakoune's tag close `</\\w[\\w-]*(?<!/)>'; the `(?<!/)' is vacuous here (a
close tag's char before `>' is always a word or `-' char), so it drops.")

(defun kao-objects--tag (sel inner to-begin to-end &optional level)
  "Select the HTML/XML tag pair around SEL's cursor.
A `kao-object-register' selector \(SEL INNER TO-BEGIN TO-END &optional LEVEL):
delegates to `kao-object--surrounding' with the tag delimiter regexps, exactly
as the built-in pair objects do.  INNER selects between the tags; TO-BEGIN and
TO-END are the object flags; LEVEL is the count-th enclosing level."
  (kao-object--surrounding sel inner
                           kao-objects-tag-open-regexp
                           kao-objects-tag-close-regexp
                           to-begin to-end (or level 0)))

(defun kao-objects--tag-nested (beg end inner &optional level)
  "Select every tag pair within \[BEG, END).
A `kao-object-register' nested selector \(BEG END INNER &optional LEVEL)
returning a list of \(FIRST . LAST) spans; delegates to
`kao-object--nested-pairs' with the tag delimiter regexps."
  (kao-object--nested-pairs beg end inner
                            kao-objects-tag-open-regexp
                            kao-objects-tag-close-regexp
                            (or level 0)))

;;;###autoload
(defun kao-objects-register-tag (&optional key)
  "Register the `tag' text object on KEY (default ?T) via `kao-object-register'.
Opt-in: kao ships no built-in `tag' object, so the default object set stays
byte-identical until you call this.  Afterwards `<a-a>KEY'/`<a-i>KEY' select
the whole/inner enclosing tag pair and `<a-A>KEY'/`<a-I>KEY' select every tag
pair, all on the public object API."
  (kao-object-register (or key ?T)
                       #'kao-objects--tag
                       "tag"
                       #'kao-objects--tag-nested))

(provide 'kao-objects)
;;; kao-objects.el ends here
