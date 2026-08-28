;;; kao.el --- Faithful Kakoune editing model for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Saiful Islam

;; Author: Saiful Islam <saifulapm@gmail.com>
;; Maintainer: Saiful Islam <saifulapm@gmail.com>
;; Version: 1.0.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, emulations
;; URL: https://github.com/saifulapm/kao
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; kao brings Kakoune's editing model to Emacs faithfully: selection-first
;; (noun->verb) editing with first-class multiple selections as the only
;; primitive.  Unlike Vim emulation (Evil) or other modal packages, kao owns a
;; real selection-list as its source of truth rather than bolting multiple
;; cursors onto Emacs's single region.
;;
;; This top-level file currently only establishes the package and its
;; customization group.  The engine lives in kao-selection.el.

;;; Code:

(defgroup kao nil
  "Faithful Kakoune editing model for Emacs."
  :group 'editing
  :prefix "kao-"
  :link '(url-link "https://github.com/saifulapm/kao"))

(defconst kao-version "1.0.2"
  "Current version of kao.")

;; Every module the package assembles, in layer order — explicit, not
;; transitive (kao-selection/kao-render previously rode in through kao-state).
(require 'kao-selection)
(require 'kao-render)
(require 'kao-history)
(require 'kao-state)
(require 'kao-motion)
(require 'kao-register)
(require 'kao-edit)
(require 'kao-info)
(require 'kao-multi)
(require 'kao-object)
(require 'kao-menu)
(require 'kao-search)
(require 'kao-pipe)
(require 'kao-modeline)
(require 'kao-macro)
(require 'kao-keys)                     ; default bindings, applied last

;; `M-x kao-mode' must load the WHOLE package, not just kao-state.el where
;; the minor mode is defined (audit dx-docs-1): a cookie on the
;; `define-minor-mode' would generate an autoload targeting kao-state, which
;; requires neither kao-keys (bindings) nor kao-modeline (lighter) — an
;; unusable buffer.  The evil-core idiom pins the autoload to THIS file,
;; which requires every module in layer order above.
;;;###autoload (autoload 'kao-mode "kao" "Toggle the Kakoune editing model in this buffer." t)

;;;; Global enablement (daily-driver infrastructure)

;; `kao--display-turn-on' and the `define-globalized-minor-mode' expansion both
;; call the internal `easy-mmode--globalized-predicate-p'; require easy-mmode so
;; it is defined at compile and run time.
(require 'easy-mmode)

(defun kao--editing-buffer-p ()
  "Heuristic: non-nil when letters self-insert here (meow's rule).
Some UI major modes bind printable keys without deriving `special-mode'
\(Dired is the canonical case — probed on Emacs 32), so the
`kao-global-mode' mode-class predicate alone would let kao's suppressed
normal map shadow their entire keymap.  A buffer where `a' is not a
self-insert-style command is not an editing buffer; kao stays out.  In a
buffer where `kao-mode' is already on, `a' resolves to a kao command and
the heuristic is false — harmless, the mode is already enabled and
`kao--turn-on' only ever turns ON."
  (let ((cmd (key-binding "a")))
    (and (symbolp cmd) cmd
         (string-match-p "self-insert" (symbol-name cmd)))))

;; Forward declarations: `define-globalized-minor-mode' (below) generates
;; these two, but `kao--forced-on-p' and `kao--display-turn-on' reference
;; them earlier in the file.
(defvar kao-global-mode)
(defvar kao-global-modes)

(defun kao--forced-on-p ()
  "Non-nil when `major-mode' is an explicit positive member of `kao-global-modes'.
Delegates to `easy-mmode--globalized-predicate-p' over that positive
subset — the same matcher the enable gate uses.  See `kao-global-mode'
for the full force-on/force-off narration."
  (and (consp kao-global-modes)
       (easy-mmode--globalized-predicate-p
        (seq-filter (lambda (entry) (and entry (symbolp entry) (not (eq entry t))))
                    kao-global-modes))))

(defun kao--turn-on ()
  "Enable `kao-mode' in editing buffers (for `kao-global-mode').
Runs only after the `kao-global-modes' predicate has approved the
buffer, enabling when it is force-listed (`kao--forced-on-p') or the
`kao--editing-buffer-p' heuristic approves it.  The on-display
path (`kao--display-turn-on') routes through here too, so both enable
paths share these semantics.  See `kao-global-mode' for the
force-on/force-off narration."
  (when (or (kao--forced-on-p)
            (kao--editing-buffer-p))
    (kao-mode 1)))

(defun kao--display-turn-on (frame)
  "Enable kao in FRAME's displayed `fundamental-mode' buffers.
`define-globalized-minor-mode' enables ONLY through
`after-change-major-mode-hook' (verified by macroexpansion), which a
buffer BORN in `fundamental-mode' never fires — `get-buffer-create' +
`switch-to-buffer' (the `C-x b' new-buffer path) sets no major mode, so
the buffer silently escapes `kao-global-mode'.  Display time is the
first moment such a buffer can matter to a user, and internal/temp
buffers are never displayed, so hooking
`window-buffer-change-functions' (global form: called with the FRAME)
keeps the hot path flat.  Gated to `fundamental-mode' precisely because
every OTHER major mode runs the hook the macro already covers, and
through the same `kao-global-modes' predicate the macro consults
\(`easy-mmode--globalized-predicate-p'), so a user exclusion of
`fundamental-mode' is honoured here too."
  (when kao-global-mode
    (dolist (win (window-list frame 'nomini))
      (with-current-buffer (window-buffer win)
        (when (and (not kao-mode)
                   (eq major-mode 'fundamental-mode)
                   (easy-mmode--globalized-predicate-p kao-global-modes))
          (kao--turn-on))))))

;;;###autoload
(define-globalized-minor-mode kao-global-mode kao-mode kao--turn-on
  :group 'kao
  ;; The evil/meow stance: modal editing belongs in editing buffers.  The
  ;; generated `kao-global-modes' user option holds this predicate —
  ;; special-mode UIs (magit, help, …) and the minibuffer keep their own
  ;; keymaps; everything else is gated by the `kao--editing-buffer-p'
  ;; self-insert heuristic (catching dired-class modes that bind printables
  ;; without deriving `special-mode').  Customize `kao-global-modes' to
  ;; force the decision per mode class — see
  ;; `kao-global-mode's doc for the full force-on/force-off narration.
  ;;
  ;; `ghostel-mode' (the libghostty terminal) is excluded EXPLICITLY because
  ;; neither guard catches it: it derives from `fundamental-mode' (not
  ;; `special-mode'), and its semi-char map remaps `self-insert-command' to
  ;; `ghostel--self-insert' — whose name matches the heuristic's "self-insert"
  ;; regex, so `kao--editing-buffer-p' is fooled into reporting an editing
  ;; buffer.  Left unguarded, kao's suppressed normal map would shadow the
  ;; live PTY and the terminal becomes untypable.  kao stays OUT; the terminal
  ;; keeps its own keymap (the same stance evil takes absent evil-ghostel).
  ;;
  ;; `vterm-mode' shares the exact ghostel failure shape (audit
  ;): it derives from `fundamental-mode' and remaps
  ;; `self-insert-command' to `vterm--self-insert', whose name fools the
  ;; same heuristic — the same untypable terminal, so the same explicit
  ;; exclusion.  eat and term are unaffected (`eat-self-input' /
  ;; `term-send-raw' don't match the regex).
  :predicate '((not special-mode
                    minibuffer-mode minibuffer-inactive-mode
                    ghostel-mode vterm-mode)
               t)
  ;; cover buffers born in fundamental-mode (no major-mode change,
  ;; no `after-change-major-mode-hook') at first display.
  (if kao-global-mode
      (add-hook 'window-buffer-change-functions #'kao--display-turn-on)
    (remove-hook 'window-buffer-change-functions #'kao--display-turn-on)))

;; `define-globalized-minor-mode' offers no docstring slot — it generates
;; `kao-global-mode's doc itself.  Extend the visible doc with the full
;; `kao-global-modes' semantics through the
;; `function-documentation' property, which `documentation' consults
;; before the function object; reading the raw doc off the function
;; object (never the symbol) keeps a re-load from appending twice.
(put 'kao-global-mode 'function-documentation
     (concat
      (documentation (symbol-function 'kao-global-mode) t)
      "\n\n`kao-global-modes' forces the decision per mode class: a major
mode listed as an explicit positive member is forced ON (the
`kao--editing-buffer-p' heuristic is skipped), a mode inside the
\(not ...) clause is forced OFF, and modes approved only by the
catch-all t are decided by the heuristic.  A positive member
covers derived modes, exactly as the predicate grammar itself does."))

;;;; Safety guard: block native-undo visualizers in kao buffers

;; vundo and undo-tree VISUALIZE and NAVIGATE the native `buffer-undo-list'
;; via `primitive-undo' — they keep no history of their own.  But kao keeps
;; native undo dormant and binds `buffer-undo-list' to t during its OWN
;; history replay (kao-history.el), so kao's undos never reach the
;; native list: it drifts out of sync with the real buffer.  These tools
;; still OPEN in a kao buffer (the list is a cons at rest) yet navigating
;; them desyncs kao's selection list / history position and can corrupt the
;; buffer.  kao's faithful history nav is `u'/`U' (undo/redo) and
;; `C-j'/`C-k' (jump by absolute change-id); a visual tree over kao's OWN
;; history is the opt-in `kao-vundo' (`M-x kao-vundo'; `(require 'kao-vundo)').

(defcustom kao-block-foreign-undo-visualizers t
  "When non-nil, refuse to open vundo / undo-tree in a `kao-mode' buffer.
They drive the native `buffer-undo-list', which kao keeps dormant and
suppresses during its own history replay, so navigating them desyncs
kao's selection list and history position and can corrupt the buffer.  For a
visual tree of kao's OWN history use the opt-in `kao-vundo' (`M-x kao-vundo');
kao's keyboard history nav is `u'/`U' and `C-j'/`C-k'.  Set to nil to lift the
guard (you accept the divergence)."
  :type 'boolean
  :group 'kao)

(defun kao--block-foreign-undo (&rest _)
  "Signal in a `kao-mode' buffer when `kao-block-foreign-undo-visualizers'.
A `:before' guard advised onto the vundo / undo-tree visualizer commands (see
the `with-eval-after-load' forms below); a no-op everywhere else, so those
tools keep working in non-kao buffers."
  (when (and kao-block-foreign-undo-visualizers (bound-and-true-p kao-mode))
    (user-error
     "kao: vundo/undo-tree desync kao's history tree — use M-x kao-vundo (or u/U, C-j/C-k)")))

(defun kao--block-foreign-undo-popup-p ()
  "Non-nil to suppress vundo-popup's auto-popup in a `kao-mode' buffer.
Composed onto vundo-popup's own `vundo-prevent-popup-predicate' (its sanctioned
refuse-a-popup hook), so the popup is declined silently rather than erroring
after the undo has run.  Gated by `kao-block-foreign-undo-visualizers'."
  (and kao-block-foreign-undo-visualizers (bound-and-true-p kao-mode)))

;; Advise/extend only once each tool is actually present, so kao keeps no
;; dependency on them (`with-eval-after-load' fires immediately if already
;; loaded).
(with-eval-after-load 'vundo
  (advice-add 'vundo :before #'kao--block-foreign-undo))
(with-eval-after-load 'undo-tree
  (advice-add 'undo-tree-visualize :before #'kao--block-foreign-undo))
;; `vundo-popup' is NOT a command: it is `vundo-popup-mode', a global minor
;; mode that advises `undo'/`undo-redo' to auto-pop the visualizer.  Decline
;; through its own predicate (checked before each popup) so the refusal is
;; silent — rather than letting the popup's call to `vundo' error after the
;; undo already ran.
(defvar vundo-prevent-popup-predicate)
(with-eval-after-load 'vundo-popup
  (add-function :before-until vundo-prevent-popup-predicate
                #'kao--block-foreign-undo-popup-p))

(provide 'kao)
;;; kao.el ends here
