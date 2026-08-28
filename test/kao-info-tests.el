;;; kao-info-tests.el --- Tests for kao-info -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the which-key autoinfo adapter.  The pure parts —
;; building the display keymap and rendering its descriptions through
;; which-key's own `which-key--get-keymap-bindings' — are checked here; the
;; actual popup needs a live frame and is live-smoke-tested (like the menus).
;; `kao-info--show'/`--hide' must be inert in batch, so they are checked to
;; no-op without error.

;;; Code:

(require 'ert)
(require 'cl-lib)                        ; cl-letf / cl-letf* for the degrade pins
;; which-key is built-in only since Emacs 30.1; soft-require so this file loads
;; on Emacs 29 (the tests that need it `skip-unless' it is present).  The
;; declaration keeps the byte-compiler quiet where the soft require found
;; nothing to load.
(require 'which-key nil t)
(declare-function which-key--get-keymap-bindings "ext:which-key")
(require 'kao-info)

(ert-deftest kao-info-make-keymap-renders-docstrings ()
  "Each (EVENT . DOC) row renders as `key -> DOC' via which-key."
  (skip-unless (fboundp 'which-key--get-keymap-bindings))
  (let* ((rows '((?g . "buffer top") (?j . "buffer bottom")))
         (map (kao-info--make-keymap rows))
         (bindings (which-key--get-keymap-bindings map)))
    (should (equal (assoc "g" bindings) '("g" . "buffer top")))
    (should (equal (assoc "j" bindings) '("j" . "buffer bottom")))))

(ert-deftest kao-info-make-keymap-handles-meta-event ()
  "A meta event (e.g. `<a-w>') binds and renders too."
  (skip-unless (fboundp 'which-key--get-keymap-bindings))
  (let* ((map (kao-info--make-keymap (list (cons ?\M-w "WORD"))))
         (bindings (which-key--get-keymap-bindings map)))
    (should (equal (cdr (assoc "M-w" bindings)) "WORD"))))

(ert-deftest kao-info-make-keymap-empty-rows ()
  "Empty ROWS yields a keymap with no bindings."
  (skip-unless (fboundp 'which-key--get-keymap-bindings))
  (let ((map (kao-info--make-keymap '())))
    (should (keymapp map))
    (should (null (which-key--get-keymap-bindings map)))))

(ert-deftest kao-info-show-hide-noop-in-batch ()
  "`kao-info--show'/`--hide' are inert in batch (no popup, no error)."
  (should-not (kao-info--available-p))      ; batch -> unavailable
  (kao-info--show "test" '((?g . "buffer top")))
  (kao-info--hide))

(ert-deftest kao-info-disabled-when-autoinfo-nil ()
  "`kao-autoinfo' nil disables the box even outside batch."
  (let ((kao-autoinfo nil))
    (should-not (kao-info--available-p))))

(ert-deftest kao-info-with-box-returns-body-value ()
  "`kao-info--with-box' returns BODY's value (the wrapped read-key result)."
  (should (eq 'the-key
             (kao-info--with-box "test" '((?g . "buffer top"))
               'the-key))))

(ert-deftest kao-info-show-swallows-which-key-error ()
  "A `which-key' that signals is swallowed; autoinfo degrades and warns once.
`which-key--show-keymap' is private and may drift on Emacs 29,
so `kao-info--show' must not let a menu-open press error out."
  (let ((kao-info--warned nil)
        (warn-count 0))
    (cl-letf (((symbol-function 'kao-info--available-p) (lambda () t))
              ((symbol-function 'which-key--show-keymap)
               (lambda (&rest _) (error "which-key drift")))
              ((symbol-function 'which-key--hide-popup)
               (lambda (&rest _) (error "which-key drift")))
              ((symbol-function 'display-warning)
               (lambda (&rest _) (setq warn-count (1+ warn-count)))))
      ;; The show call swallows the error instead of propagating it.
      (should (progn (kao-info--show "test" '((?g . "buffer top"))) t))
      ;; `kao-info--with-box' still returns BODY's value despite the failing box.
      (should (eq 'body-val
                  (kao-info--with-box "test" '((?g . "buffer top")) 'body-val)))
      ;; The degradation warning fires exactly once across repeated failures.
      (kao-info--show "test" '((?g . "buffer top")))
      (should (= warn-count 1))
      (should kao-info--warned))))

(ert-deftest kao-info-show-noop-when-unbound ()
  "When the `which-key' show fn is absent, `kao-info--show' no-ops silently.
Absence (Emacs 29 without `which-key', or a stripped build) is not an error, so
it must neither invoke the fn nor emit the incompatibility warning -- distinct
from a present-but-erroring fn, which degrades and warns."
  (let ((kao-info--warned nil)
        (warned nil))
    (cl-letf (((symbol-function 'kao-info--available-p) (lambda () t))
              ;; nil function cell -> `fboundp' returns nil for this symbol.
              ((symbol-function 'which-key--show-keymap) nil)
              ((symbol-function 'display-warning)
               (lambda (&rest _) (setq warned t))))
      (should-not (fboundp 'which-key--show-keymap))   ; precondition: absent
      (kao-info--show "test" '((?g . "buffer top")))   ; no void-function error
      (should-not warned)                              ; absence is silent
      (should-not kao-info--warned))))

(provide 'kao-info-tests)
;;; kao-info-tests.el ends here
