;;; kao-modeline.el --- Faithful Kakoune mode_info for the mode line -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 2.  Kakoune's status line ends with a
;; per-mode `mode_info' segment; this file reproduces THAT segment — and only
;; that — in the Emacs mode line, through the `kao-mode' lighter.
;;
;; The faithful content (verified against the C++) is small:
;;
;;   * Normal mode `mode_info' (input_handler.cc:382-416):
;;       "{N} sel"            when there is one selection
;;       "{N} sels ({main+1})" when there are several (1-based main index)
;;       + " param={count}"   while a count is being accumulated
;;       + " reg={x}"         while a register is pending (the `"' prefix)
;;   * Insert mode `mode_info' (input_handler.cc:1409-1419):
;;       "insert " + the same selection count
;;
;; One context flag IS reproduced: `[recording ({reg})]' while a macro records
;; (Kakoune `client.cc:156'), since kao now carries that state and a user
;; needs the feedback.  Everything else Kakoune's default `modelinefmt' carries is
;; deliberately NOT reproduced here, because Emacs's own mode line already shows it
;; or kao has no state for it: the buffer name and `line:col' position (native
;; Emacs mode line), `client@[session]' (no remote clients), the `[+]'/etc. context
;; flags, the StatusLine* faces, and the window-coupled `(hidden)' selection
;; suffix.  This keeps the kao addition to exactly the editing-model context
;; Emacs lacks, nothing invented.
;;
;; Same "native mechanism, faithful content" family as the native regex engine
;; and the native viewport: the mode-line lighter is the idiomatic Emacs home
;; for modal state; the string it renders is the faithful Kakoune `mode_info'.

;;; Code:

(require 'kao-state)

(defun kao-mode-line-string ()
  "Return Kakoune's `mode_info' for the current buffer, as a mode-line string.
Reads the buffer-local state directly: `kao--sels' (the selection list),
`kao--insert-active' (mode), and `kao--count' (pending count).  Leads with a
space so it separates cleanly from any preceding lighter; returns the empty
string when kao is inactive here (`kao--sels' nil).

Faithful to `mode_info' (input_handler.cc:382-416 normal / :1409-1419 insert):
one selection shows \"{N} sel\"; several show \"{N} sels ({main+1})\" with the
1-based main index.  In normal state a pending count is appended as
\" param={count}\" and a pending register as \" reg={x}\" — the RAW char, as
the C++ prints `m_params.reg' unlowered (:411-415); insert state shows neither,
matching the C++.  While a macro records, \" [recording ({reg})]\" is appended
in either state (Kakoune's client recording flag, client.cc:156;
`kao--macro-recording-reg' is global, read defensively so kao-modeline needs no
dependency on kao-macro)."
  (if (not kao--sels)
      ""
    (let* ((list (kao-sels-list kao--sels))
           (n (length list))
           (main (kao-sels-main kao--sels))
           (sel (if (= n 1)
                    (format "%d sel" n)
                  (format "%d sels (%d)" n (1+ main))))
           (base (if kao--insert-active
                     (concat " insert " sel)
                   (concat " " sel
                           (if (> kao--count 0)
                               (format " param=%d" kao--count)
                             "")
                           (if kao--pending-register
                               (format " reg=%c" kao--pending-register)
                             ""))))
           (rec (let ((reg (bound-and-true-p kao--macro-recording-reg)))
                  (if reg (format " [recording (%c)]" reg) "")))
           ;; Opt-in current/total search position (hel-study-6): append ` N/M'
           ;; when `kao-search-count' is on and a landing recorded the pair.
           ;; Read defensively (like the recording flag) so kao-modeline needs
           ;; no dependency on kao-search; nil pair or off = no segment, so the
           ;; faithful string is byte-identical when the feature is off.
           (cnt (let ((pair (and (bound-and-true-p kao-search-count)
                                 (bound-and-true-p kao--search-count))))
                  (if pair (format " %d/%d" (car pair) (cdr pair)) ""))))
      (concat base rec cnt))))

(define-obsolete-function-alias 'kao--mode-line-string
  'kao-mode-line-string "kao 1.x")

(defcustom kao-lighter '(:eval (kao-mode-line-string))
  "Mode-line lighter for `kao-mode', a Layer-0 config seam (dx-docs-7).
The default keeps the faithful Kakoune `mode_info' — the `(:eval …)' construct
that calls the public `kao-mode-line-string'.  nil HIDES the lighter while
`kao-mode' stays on; a string REPLACES it verbatim; any other value is treated
as a mode-line construct (e.g. relocated into a `doom-modeline' segment that
itself calls `kao-mode-line-string')."
  :type '(choice (const :tag "Hidden" nil)
                 (string :tag "Literal string")
                 (sexp :tag "Mode-line construct"))
  :group 'kao)

(defun kao--lighter ()
  "Render `kao-lighter' to a string for the `kao-mode' lighter.
nil yields the empty string (the lighter is hidden, kao-mode stays on); a
string is used verbatim; a lone `(:eval FORM)' construct — the default — has
FORM evaluated, so `(:eval (kao-mode-line-string))' shows the faithful
`mode_info'; any other mode-line construct is rendered via `format-mode-line'."
  (cond ((null kao-lighter) "")
        ((stringp kao-lighter) kao-lighter)
        ((and (consp kao-lighter) (eq (car kao-lighter) :eval))
         (or (eval (cadr kao-lighter) t) ""))
        (t (format-mode-line kao-lighter))))

(provide 'kao-modeline)
;;; kao-modeline.el ends here
