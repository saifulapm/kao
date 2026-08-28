;;; kao-tests.el --- Tests for kao.el (package level) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the package-level pieces in kao.el — currently
;; `kao-global-mode' (daily-driver enablement).

;;; Code:

(require 'ert)
(require 'kao)

;; Defined at runtime inside the exclusion tests when the real packages are
;; absent (`emacs -Q -batch') — minimal stand-ins with the same shape.
(declare-function vterm-mode "ext:vterm" ())
(declare-function vterm--self-insert "ext:vterm" ())
(declare-function ghostel-mode "ext:ghostel" ())

(defmacro kao-tests--with-global-mode (&rest body)
  "Run BODY with `kao-global-mode' enabled, disabling it afterwards."
  (declare (indent 0))
  `(unwind-protect
       (progn (kao-global-mode 1) ,@body)
     (kao-global-mode -1)))

(ert-deftest kao-global-mode-enables-in-editing-buffers ()
  "Global mode turns kao-mode on in ordinary editing buffers, new and old."
  (let ((existing (generate-new-buffer "kao-tests-existing")))
    (unwind-protect
        (progn
          (with-current-buffer existing (text-mode))
          (kao-tests--with-global-mode
            (should (buffer-local-value 'kao-mode existing))
            (with-temp-buffer            ; created after enabling
              (text-mode)
              (should kao-mode))))
      (kill-buffer existing))))

(ert-deftest kao-global-mode-skips-special-and-minibuffer-modes ()
  "The predicate keeps kao out of special-mode UIs."
  (kao-tests--with-global-mode
    (with-temp-buffer
      (special-mode)
      (should-not kao-mode))))

(ert-deftest kao-global-mode-disable-turns-kao-off-everywhere ()
  "Disabling the global mode disables kao-mode in affected buffers."
  (let ((buf (generate-new-buffer "kao-tests-off")))
    (unwind-protect
        (progn
          (with-current-buffer buf (text-mode))
          (kao-global-mode 1)
          (should (buffer-local-value 'kao-mode buf))
          (kao-global-mode -1)
          (should-not (buffer-local-value 'kao-mode buf)))
      (kao-global-mode -1)
      (kill-buffer buf))))

(ert-deftest kao-global-mode-predicate-option-exists ()
  "The :predicate generates the `kao-global-modes' user option."
  (should (boundp 'kao-global-modes))
  ;; First predicate clause is the (not MODE...) exclusion list.
  (should (memq 'special-mode (car kao-global-modes))))

(ert-deftest kao-global-mode-skips-dired ()
  "The self-insert heuristic keeps kao out of dired.
`dired-mode' does NOT derive `special-mode' (probed, Emacs 32), so the
mode-class predicate alone would let kao's suppressed normal map shadow
every dired key."
  (require 'dired)
  (let ((dir (make-temp-file "kao-tests-dired" t)))
    (unwind-protect
        (kao-tests--with-global-mode
          (let ((buf (dired-noselect dir)))
            (unwind-protect
                (should-not (buffer-local-value 'kao-mode buf))
              (kill-buffer buf))))
      (delete-directory dir))))

(ert-deftest kao-global-mode-forces-on-explicitly-listed-mode ()
  "An explicit positive member of `kao-global-modes' forces kao ON.
The heuristic keeps kao out of dired BY DEFAULT (previous test),
but the knob promises to \"force the decision either way\": a user
naming `dired-mode' as a positive member of
`kao-global-modes' has made the decision explicitly, so `kao--turn-on'
must skip the `kao--editing-buffer-p' guard — which dired always fails
\(`a' runs a dired command, never self-insert) — and enable the mode."
  (require 'dired)
  (let ((dir (make-temp-file "kao-tests-dired-force" t))
        ;; Derive from the default so the exclusion list can't silently
        ;; drift: the default value IS ((not ...) t), so consing dired-mode
        ;; on front reconstructs the force-on list and tracks the default.
        (kao-global-modes (cons 'dired-mode (default-value 'kao-global-modes))))
    (unwind-protect
        (kao-tests--with-global-mode
          (let ((buf (dired-noselect dir)))
            (unwind-protect
                (should (buffer-local-value 'kao-mode buf))
              (kill-buffer buf))))
      (delete-directory dir))))

(ert-deftest kao-global-mode-excludes-ghostel-by-default ()
  "`ghostel-mode' is in the default `kao-global-modes' exclusion list."
  (should (memq 'ghostel-mode (car kao-global-modes))))

(ert-deftest kao-global-mode-skips-ghostel ()
  "Explicit predicate exclusion keeps kao out of ghostel terminals.
`ghostel-mode' derives from `fundamental-mode' (so the mode-class predicate
misses it) and the real package remaps `self-insert-command' to
`ghostel--self-insert' (whose name matches the `kao--editing-buffer-p' regex,
fooling the heuristic).  Only the explicit exclusion stops kao's suppressed
normal map from shadowing the live PTY.  In `emacs -Q -batch' the real
package isn't on the load path, so a minimal stand-in deriving from
`fundamental-mode' reproduces the case (there `a' self-inserts, so the
heuristic would pass — proving the exclusion, not the heuristic, is at work)."
  (unless (fboundp 'ghostel-mode)
    (define-derived-mode ghostel-mode fundamental-mode "Ghostel"))
  (kao-tests--with-global-mode
    (with-temp-buffer
      (ghostel-mode)
      (should-not kao-mode))))

(ert-deftest kao-global-mode-excludes-vterm-by-default ()
  "`vterm-mode' is in the default `kao-global-modes' exclusion list."
  (should (memq 'vterm-mode (car kao-global-modes))))

(ert-deftest kao-global-mode-skips-vterm ()
  "Explicit predicate exclusion keeps kao out of vterm terminals.
`vterm-mode' has the exact ghostel failure shape:
it derives from `fundamental-mode' (so the mode-class predicate misses
it) and the real package remaps `self-insert-command' to
`vterm--self-insert' — whose name matches the `kao--editing-buffer-p'
regex, fooling the heuristic.  Only the explicit exclusion keeps kao's
suppressed normal map off the live PTY.  In `emacs -Q -batch' the real
package isn't on the load path, so a minimal stand-in with the same
shape — derived mode plus the remap to a command literally named
`vterm--self-insert' — reproduces the case."
  (unless (fboundp 'vterm-mode)
    (defun vterm--self-insert ()
      "Stand-in for vterm's input command; the NAME fools the heuristic."
      (interactive))
    (define-derived-mode vterm-mode fundamental-mode "VTerm")
    (define-key vterm-mode-map [remap self-insert-command]
                #'vterm--self-insert))
  (kao-tests--with-global-mode
    (with-temp-buffer
      (vterm-mode)
      (should-not kao-mode))))

(ert-deftest kao-global-mode-covers-fundamental-at-display ()
  "A buffer BORN in fundamental-mode gets kao at first display.
`get-buffer-create' sets no major mode, so `after-change-major-mode-hook'
\(the globalized macro's only enable path) never fires — the buffer
escapes the global mode until `kao--display-turn-on' catches it from
`window-buffer-change-functions'.  Batch has no real display cycle, so
the hook function is driven directly after `set-window-buffer'."
  (kao-tests--with-global-mode
    ;; Born AFTER the global mode is on — the gap being fixed.
    (let ((buf (get-buffer-create "kao-tests-fundamental")))
      (unwind-protect
          (progn
            ;; The documented gap: no mode change, no enable.
            (should-not (buffer-local-value 'kao-mode buf))
            (set-window-buffer (selected-window) buf)
            (kao--display-turn-on (selected-frame))
            (should (buffer-local-value 'kao-mode buf)))
        (kill-buffer buf)))))

(ert-deftest kao-global-mode-display-hook-lifecycle ()
  "`kao--display-turn-on' is hooked exactly while the global mode is on."
  (kao-tests--with-global-mode
    (should (memq #'kao--display-turn-on window-buffer-change-functions)))
  (should-not (memq #'kao--display-turn-on window-buffer-change-functions)))

(ert-deftest kao-global-mode-display-hook-honours-predicate ()
  "The display-time enable consults the `kao-global-modes' user knob:
a user exclusion of `fundamental-mode' is honoured, same as the macro path."
  (kao-tests--with-global-mode
    (let ((buf (get-buffer-create "kao-tests-fundamental-excluded")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) buf)
            (let ((kao-global-modes '((not fundamental-mode) t)))
              (kao--display-turn-on (selected-frame))
              (should-not (buffer-local-value 'kao-mode buf))))
        (kill-buffer buf)))))

(ert-deftest kao-global-mode-editing-buffer-heuristic ()
  "`kao--editing-buffer-p' is the meow self-insert rule."
  ;; Editing buffer: letters self-insert.
  (with-temp-buffer
    (text-mode)
    (should (kao--editing-buffer-p)))
  ;; A local map that binds the letter away fails the heuristic.
  (with-temp-buffer
    (let ((map (make-sparse-keymap)))
      (define-key map "a" #'ignore)
      (use-local-map map)
      (should-not (kao--editing-buffer-p))))
  ;; A kao-enabled buffer resolves `a' to a kao command — false, and
  ;; documented harmless (the mode is already on; turn-on only turns ON).
  (with-temp-buffer
    (text-mode)
    (kao-mode 1)
    (unwind-protect
        (should-not (kao--editing-buffer-p))
      (kao-mode -1))))

;;;; Foreign-undo visualizer guard (vundo / undo-tree)

(ert-deftest kao-foreign-undo-guard-decision ()
  "`kao--block-foreign-undo' errors in a kao buffer (guard on), silent otherwise."
  (should kao-block-foreign-undo-visualizers)          ; defaults on
  (with-temp-buffer
    (text-mode)
    (kao-mode 1)
    (unwind-protect
        (progn
          ;; Guard on (default): a kao buffer signals.
          (should-error (kao--block-foreign-undo) :type 'user-error)
          ;; Guard off: silent pass-through even in a kao buffer.
          (let ((kao-block-foreign-undo-visualizers nil))
            (should (null (kao--block-foreign-undo)))))
      (kao-mode -1)))
  ;; Outside kao-mode: silent regardless — those tools keep working elsewhere.
  (with-temp-buffer
    (should (null (kao--block-foreign-undo)))))

(defvar kao-tests--fake-vundo-ran nil
  "Set by `kao-tests--fake-vundo' when it runs (guard end-to-end test).")

(defun kao-tests--fake-vundo ()
  "A `vundo'-style stand-in command for the foreign-undo guard test."
  (interactive)
  (setq kao-tests--fake-vundo-ran t))

(ert-deftest kao-foreign-undo-guard-blocks-the-command ()
  "Advised onto a visualizer command, the guard aborts it in a kao buffer but
lets it run elsewhere — proves the `:before' wiring without any real vundo."
  (advice-add 'kao-tests--fake-vundo :before #'kao--block-foreign-undo)
  (unwind-protect
      (progn
        (with-temp-buffer
          (text-mode)
          (kao-mode 1)
          (setq kao-tests--fake-vundo-ran nil)
          (unwind-protect
              (progn
                (should-error (call-interactively 'kao-tests--fake-vundo)
                              :type 'user-error)
                (should-not kao-tests--fake-vundo-ran))
            (kao-mode -1)))
        (with-temp-buffer
          (setq kao-tests--fake-vundo-ran nil)
          (call-interactively 'kao-tests--fake-vundo)
          (should kao-tests--fake-vundo-ran)))
    (advice-remove 'kao-tests--fake-vundo #'kao--block-foreign-undo)))

(ert-deftest kao-foreign-undo-popup-predicate ()
  "`kao--block-foreign-undo-popup-p' refuses the vundo-popup in kao buffers only."
  (with-temp-buffer
    (text-mode)
    (kao-mode 1)
    (unwind-protect
        (progn
          (should (kao--block-foreign-undo-popup-p))               ; guard on
          (let ((kao-block-foreign-undo-visualizers nil))
            (should-not (kao--block-foreign-undo-popup-p))))        ; guard off
      (kao-mode -1)))
  (with-temp-buffer
    (should-not (kao--block-foreign-undo-popup-p))))                ; non-kao buffer

;;;; Package autoloads (dx-docs-1)

(ert-deftest kao-autoload-kao-mode-targets-kao ()
  "The generated `kao-mode' autoload loads kao.el, not kao-state.el.
A cookie directly on the `define-minor-mode' in kao-state.el makes
package autoloads load ONLY kao-state.el — none of kao-keys (bindings),
kao-edit/motion (commands) or kao-modeline (lighter) — so `M-x kao-mode'
yields an unusable buffer (audit dx-docs-1).  The evil-core idiom (an
explicit `autoload' form cookie in the package's main file) routes the
autoload through kao.el, which requires every module in layer order.
Autoloads are generated fresh into a temp file; the repo is never
written."
  (skip-unless (fboundp 'loaddefs-generate))
  (let ((root (file-name-directory (locate-library "kao")))
        (out (make-temp-file "kao-tests-loaddefs-" nil ".el"))
        target)
    (unwind-protect
        (progn
          ;; `loaddefs-generate' is incremental over an EXISTING output
          ;; file (its fresh mtime would mark every source up-to-date and
          ;; yield an empty loaddefs); delete it for a full generation.
          (delete-file out)
          (loaddefs-generate root out)
          (with-temp-buffer
            (insert-file-contents out)
            (goto-char (point-min))
            (let (form)
              (while (and (not target)
                          (setq form (ignore-error end-of-file
                                       (read (current-buffer)))))
                (when (and (eq (car-safe form) 'autoload)
                           (equal (cadr form) '(quote kao-mode)))
                  (setq target (nth 2 form))))))
          ;; The target may carry a path prefix; only the base name — the
          ;; feature file the autoload will load — matters.
          (should (equal (and target (file-name-nondirectory target))
                         "kao")))
      (when (file-exists-p out) (delete-file out)))))

(provide 'kao-tests)
;;; kao-tests.el ends here
