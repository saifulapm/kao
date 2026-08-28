;;; kao-info.el --- Autoinfo boxes (which-key backend) for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 2.  Kakoune shows an info box of the available
;; keys while a one-shot menu key is pending — `on_next_key_with_autoinfo'
;; (normal.cc:182) renders `build_autoinfo_for_mapping', a list of `keys:
;; docstring' rows, for the goto / view / object / combine menus.  This file is
;; the kao translation, decided as:
;;
;;   * Backend = built-in `which-key' (core since Emacs 30.1), as a SOFT
;;     dependency: `(require 'which-key nil t)' so kao still loads on Emacs 29
;;     without the package — autoinfo then simply no-ops.
;;   * kao's four menus are commands that block on `read-key', NOT prefix
;;     keymaps, so which-key cannot auto-fire.  kao drives it: synthesize a
;;     display-only keymap from a `(EVENT . DOCSTRING)' alist (the idiomatic
;;     `(STRING . DEFN)' menu-label binding form, which which-key renders as the
;;     description), `which-key--show-keymap NAME MAP nil nil t' (the trailing
;;     `t' is no-paging = show-but-don't-read) before the read-key, and
;;     `which-key--hide-popup' after.  Dispatch stays pure — the display
;;     keymap is never consulted for the key; kao's own `read-key' reads it.
;;
;; This is the same "native mechanism, faithful behaviour" family as the
;; native regex engine and the native viewport: the box CONTENT (the
;; faithful `key -> docstring' rows) is what parity requires; which-key's grid
;; chrome stands in for Kakoune's bordered box.  Each menu keeps a per-menu
;; `(EVENT . DOC)' info constant SEPARATE from its dispatch table, faithful to
;; Kakoune's separate `KeyInfo' built-ins array; a parity ERT test per menu
;; guards the two against drift.
;;
;; `<space>' stays a real prefix keymap, so it gets which-key's NATIVE
;; auto-display when `which-key-mode' is enabled — not special-cased here.

;;; Code:

(require 'which-key nil t)

;; which-key became built-in in Emacs 30.1; on Emacs 29 without it installed the
;; soft `require' above yields nothing, so declare the two functions kao drives —
;; otherwise the strict byte-compile gate (`byte-compile-error-on-warn') errors
;; with "not known to be defined" on a which-key-less build.  The calls are
;; runtime-guarded by `kao-info--available-p', so nothing is invoked when absent.
(declare-function which-key--show-keymap "which-key")
(declare-function which-key--hide-popup "which-key")

;; The `kao' customize group is defined once in kao.el (loaded before this file
;; in the package; other modules likewise reference it without redefining).

(defcustom kao-autoinfo t
  "When non-nil, show an autoinfo box of keys during a pending menu.
Pending menus are object, goto, view, combine; faithful to Kakoune's
`autoinfo' option.  Rendered via `which-key'; has no effect when
`which-key' is unavailable or in batch."
  :type 'boolean
  :group 'kao)

(defvar kao-info--warned nil
  "Non-nil once a `which-key' incompatibility has disabled kao autoinfo.
Set on the first error signalled by a private `which-key' display function
so the box degrades to no-op for the rest of the session and the warning
fires only once.  Reset it (or restart Emacs) after fixing `which-key'.")

(defun kao-info--available-p ()
  "Return non-nil when an autoinfo box can be shown.
Requires `kao-autoinfo', a loaded `which-key', an interactive session (no
popup is spawned in batch, so ERT runs stay clean), and no prior `which-key'
incompatibility (`kao-info--warned')."
  (and kao-autoinfo (not kao-info--warned)
       (featurep 'which-key) (not noninteractive)))

(defun kao-info--make-keymap (rows)
  "Build a display-only keymap from ROWS, an alist of (EVENT . DOCSTRING).
Each EVENT is bound to the idiomatic `(STRING . DEFN)' menu-label form so
which-key renders DOCSTRING as the binding description; the DEFN (`ignore')
is never run — kao's own `read-key' dispatches."
  (let ((map (make-sparse-keymap)))
    (dolist (row rows map)
      (define-key map (vector (car row)) (cons (cdr row) #'ignore)))))

(defun kao-info--degrade (err)
  "Disable kao autoinfo after a `which-key' failure ERR, warning once.
Sets `kao-info--warned' so the box no-ops thereafter and the incompatibility
warning is emitted a single time per session."
  (unless kao-info--warned
    (setq kao-info--warned t)
    (display-warning
     'kao
     (format "which-key is incompatible; kao autoinfo disabled (%S)" err)
     :warning)))

(defmacro kao-info--guard (&rest body)
  "Run BODY, degrading autoinfo to no-op if a `which-key' call errors.
which-key's private display API is unstable across versions, so a drift must
never abort a menu-open press -- `kao-info--degrade' swallows the error and
disables the box after warning once."
  (declare (indent 0) (debug t))
  `(condition-case err
       (progn ,@body)
     (error (kao-info--degrade err))))

(defun kao-info--show (name rows)
  "Show an autoinfo box titled NAME listing ROWS (an (EVENT . DOC) alist).
No-op unless `kao-info--available-p'.  which-key renders but does NOT
consume input (the trailing no-paging t); the caller's `read-key' reads
the key.  A `which-key' that errors degrades to no box (`kao-info--guard');
an absent private fn is skipped via `fboundp'."
  (when (kao-info--available-p)
    (kao-info--guard
      (when (fboundp 'which-key--show-keymap)
        (which-key--show-keymap name (kao-info--make-keymap rows) nil nil t)))))

(defun kao-info--hide ()
  "Hide the autoinfo box.  No-op unless `kao-info--available-p'.
Like `kao-info--show', a `which-key' error or an absent private fn
degrades to no-op, via `kao-info--guard' and `fboundp' respectively."
  (when (kao-info--available-p)
    (kao-info--guard
      (when (fboundp 'which-key--hide-popup)
        (which-key--hide-popup)))))

(defmacro kao-info--with-box (name rows &rest body)
  "Show the autoinfo box (NAME, ROWS), run BODY, hide the box on exit.
Hides even if BODY errors (`unwind-protect'); returns BODY's value, so the
common shape is to wrap a `read-key' directly."
  (declare (indent 2))
  `(unwind-protect
       (progn (kao-info--show ,name ,rows) ,@body)
     (kao-info--hide)))

(provide 'kao-info)
;;; kao-info.el ends here
