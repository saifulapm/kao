;;; kao-macro.el --- Keyboard macro recording / replay for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 6.  Kakoune macros record the keys you press
;; into a register (`Q' starts/ends recording) and replay them (`q'), defaulting
;; to the `@' register.
;;
;; native mechanism, faithful behaviour (regex / count reader /
;; clipboard / undo family):
;;
;;   * RECORDING is custom (like the count reader over native `prefix-arg'): a
;;     global `pre-command-hook' (`kao--macro-record-key'), installed ONCE at load
;;     and gated by `kao--macro-recording-reg', captures `this-command-keys-vector'
;;     for each command while recording.  Custom — not native `kmacro' — to avoid
;;     the kmacro-ring + the uncertain handling of the key that ends recording, and
;;     to stay batch-testable.  The hook is always installed (not added on
;;     start/removed on stop) because a hook added mid-command is not seen by an
;;     in-progress `execute-kbd-macro' replay; gated, it is a single nil-check per
;;     command when idle, so the perf principle holds (no allocation off the
;;     recording path).  The start `Q' is not recorded (its pre-command-hook fired
;;     while the register was still nil); the stop `Q' IS recorded and then dropped
;;     (faithful to `stop_recording' popping the last key, input_handler.cc:1754
;;     "Forget the key that got us to exit recording").  Recording is global so it
;;     spans buffers, like Emacs's own `defining-kbd-macro'.
;;   * REPLAY is native: `execute-kbd-macro' feeds the stored key vector back
;;     through the real command loop (= Kakoune feeding keys through `handle_key',
;;     normal.cc:1772) the requested COUNT times, as one undo unit (`ScopedEdition').
;;   * The store is a third parallel native store in kao-register.el (the
;;     register precedent): a key vector per register, default `@'.
;;
;; Registers: the pending `"x' prefix or the `@' default, restricted to
;; `@' + alphabetic by the faithful guard shared with replay
;; (normal.cc:1741-1744 / :1986-1990).

;;; Code:

(require 'kao-state)
(require 'kao-register)

(defvar kao--macro-recording-reg nil
  "The register char currently being recorded to, or nil when not recording.
Global (recording spans buffers, like the variable `defining-kbd-macro')
and read by the mode-line indicator.  Faithful to
`InputHandler::m_recording_reg'.")

(defvar kao--macro-recorded-keys nil
  "Keys recorded so far this session, most-recent first (a list of key vectors).
`kao--macro-stop' drops the head — the `Q' that ended recording — then reverses
and `vconcat's the rest into the stored macro (Kakoune `m_recorded_keys').")

(defvar kao--macro-replaying nil
  "Non-nil while a kao macro is replaying via `execute-kbd-macro'.
Suppresses recording of replayed sub-keys, faithful to Kakoune recording only at
the top key level (`m_handle_key_level == m_recording_level',
input_handler.cc:1712): a `q' recorded inside a macro stays `q', not its
expansion.")

(defun kao--macro-record-key ()
  "Push the current command's keys onto `kao--macro-recorded-keys'.
A global `pre-command-hook' installed once at load (see end of file); a single
nil-check per command unless recording.  Skips replayed sub-keys
\(`kao--macro-replaying') so only the user's top-level keys are captured
\(Kakoune `m_handle_key_level == m_recording_level', input_handler.cc:1712)."
  (when (and kao--macro-recording-reg (not kao--macro-replaying))
    (push (this-command-keys-vector) kao--macro-recorded-keys)))

(defun kao--macro-start (reg)
  "Begin recording keys to register REG.
The start `Q' is not recorded: its `pre-command-hook' already fired (while the
register was still nil) before this command body runs — matching Kakoune
`start_recording' clearing the buffer (input_handler.cc:1736)."
  (setq kao--macro-recording-reg reg
        kao--macro-recorded-keys nil))

(defun kao--macro-stop ()
  "End recording: store the recorded keys minus the stop `Q' to the register.
Faithful to `stop_recording' (input_handler.cc:1749).  Store only when keys
were recorded (the `not m_recorded_keys.empty()' guard, always true in the
real path, where the stop `Q' was just recorded); drop that stop `Q' (the
list head); set the register to whatever remains.  An immediate `QQ' thus
stores an EMPTY macro, clobbering any prior one, like Kakoune's \"\" (which
then errors \"register is empty\" on replay)."
  (when kao--macro-recorded-keys
    ;; Drop the stop `Q' (the head), then restore typed order.
    (let ((keys (reverse (cdr kao--macro-recorded-keys))))
      (kao-register-set-macro (apply #'vconcat keys) kao--macro-recording-reg)))
  (setq kao--macro-recording-reg nil
        kao--macro-recorded-keys nil))

(defun kao--macro-register-arg ()
  "The pending or default `@' macro register, lowered and validated.
Ports `to_lower(params.reg ? params.reg : \\='@\\=')' plus the alpha-only
guard shared by record and replay (normal.cc:1741-1744 / :1986-1990):
macros live only in `@' and the alphabetic registers."
  (let ((reg (downcase (kao--register-arg kao-macro-register-default))))
    (unless (or (<= ?a reg ?z) (eq reg ?@))
      (user-error "macros can only use the '@' and alphabetic registers"))
    reg))

(defun kao-macro-record ()
  "Start or end macro recording (Kakoune `Q', `start_or_end_macro_recording').
Recording → stop and store to the register; not recording → start to the
pending or default `@' register, lowered and alpha-checked up front
\(normal.cc:1741-1744) so the recording flag and the stop-store both see the
canonical name.  A pending register on the STOP press is ignored, as in the
C++ (the recording branch never reads `params.reg')."
  (interactive)
  (if kao--macro-recording-reg
      (kao--macro-stop)
    (kao--macro-start (kao--macro-register-arg))))

(defvar kao--macros-running nil
  "Registers whose macro is currently replaying (the recursion-guard set).
Faithful to `replay_macro's `running_macros' (normal.cc:1754): a macro that
replays itself errors rather than recursing forever.")

(defun kao-macro-play ()
  "Replay a register's macro `count' times (Kakoune `q', `replay_macro').
The register is the pending or default `@', lowered (`to_lower(params.reg)',
normal.cc:1741).  Errors when the register is empty (\"register \\='x\\=' is
empty\") or already replaying (\"recursive macros call detected\"), faithful
to normal.cc:1748.  The whole replay (all COUNT iterations) is one undo unit
\(Kakoune `ScopedEdition'), and the replayed keys are not re-recorded into an
in-progress recording (`kao--macro-replaying', the m_handle_key_level guard).
COUNT is `max(1,count)', faithful to the `do..while' loop."
  (interactive)
  (let* ((reg (kao--macro-register-arg))
         (count (kao--repeat-count))
         (keys (kao-register-get-macro reg)))
    (when (or (null keys) (= (length keys) 0))
      (user-error "register '%c' is empty" reg))
    (when (memq reg kao--macros-running)
      (user-error "recursive macros call detected"))
    (let ((handle (prepare-change-group)))
      (activate-change-group handle)
      (unwind-protect
          (let ((kao--macros-running (cons reg kao--macros-running))
                (kao--macro-replaying t))
            (execute-kbd-macro keys count))
        (undo-amalgamate-change-group handle)
        (accept-change-group handle)))))

;;;; Commands

;; The recording hook is always installed and gated by `kao--macro-recording-reg'
;; (see `kao--macro-record-key'): a hook added mid-command is not seen by an
;; in-progress `execute-kbd-macro', so it must pre-exist the command loop.
(add-hook 'pre-command-hook #'kao--macro-record-key)

(provide 'kao-macro)
;;; kao-macro.el ends here
