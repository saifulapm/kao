;;; kao-macro-tests.el --- Tests for kao-macro -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the macro slice: `Q' recording (Task 1) and `q' replay
;; (Task 2).
;;
;; Two layers of coverage:
;;
;;   * Unit layer — the record / store / drop-stop-key logic driven by calling the
;;     functions, with `kao-macro-tests--feed' simulating the command loop firing
;;     `pre-command-hook' per key (it stubs `this-command-keys-vector').  Fast,
;;     buffer-free, independent of dispatch.
;;   * End-to-end layer — the real `kao-macro-play' / `execute-kbd-macro'
;;     driving kao's emulation map in batch.  The earlier note here that batch
;;     "cannot drive the emulation map" was wrong: batch `execute-kbd-macro'
;;     dispatches into the SELECTED WINDOW's buffer, so the only requirement is
;;     `(set-window-buffer (selected-window) buf)' — the `kao-macro-tests--with-window'
;;     helper, the same technique `test/kao-tests.el' uses.  With that, replay edits
;;     the buffer and updates `kao--sels' for real, and the recording
;;     `pre-command-hook' fires per key under `execute-kbd-macro' too.
;;
;; Global macro state is reset around every test (`kao-macro-tests--clean').

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-state)
(require 'kao-register)
(require 'kao-macro)
(require 'kao-object)                  ; the menu-char end-to-end test
(require 'kao-keys)                    ; default bindings

(defmacro kao-macro-tests--clean (&rest body)
  "Run BODY with the global macro state reset before and after."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (setq kao--macro-recording-reg nil
               kao--macro-recorded-keys nil
               kao--macro-replaying nil
               kao--macros-running nil)
         (clrhash kao--macro-registers)
         ,@body)
     (setq kao--macro-recording-reg nil
           kao--macro-recorded-keys nil
           kao--macro-replaying nil
           kao--macros-running nil)
     (clrhash kao--macro-registers)))

(defun kao-macro-tests--feed (&rest key-vectors)
  "Fire the recording hook once per KEY-VECTOR, as the command loop would.
Stubs `this-command-keys-vector' to each vector in turn so
`kao--macro-record-key' captures it (only when recording, faithful to the
real per-command firing)."
  (dolist (kv key-vectors)
    (cl-letf (((symbol-function 'this-command-keys-vector) (lambda () kv)))
      (kao--macro-record-key))))

(defmacro kao-macro-tests--with-window (text cursor &rest body)
  "Run BODY in a fresh kao-mode buffer holding TEXT, one selection at CURSOR.
The buffer is put in the selected window — load-bearing, because batch
`execute-kbd-macro' dispatches into the selected window's buffer (the
recipe, the same technique `test/kao-tests.el' uses).  BODY runs with that
buffer current; point and the sole `kao--sels' selection both start at CURSOR.
Undo stays enabled (the name has no leading space) so the change-group test can
undo.  The previous window buffer is restored and the scratch buffer killed."
  (declare (indent 2) (debug (form form body)))
  `(let ((buf (generate-new-buffer "kao-macro-e2e"))
         (prev (window-buffer (selected-window))))
     (unwind-protect
         (with-current-buffer buf
           (insert ,text)
           (kao-mode 1)
           (goto-char ,cursor)
           (setq kao--sels (kao-sels-make
                            :list (list (kao-sel-make :anchor ,cursor
                                                      :cursor ,cursor))
                            :main 0))
           (set-window-buffer (selected-window) buf)
           ,@body)
       (set-window-buffer (selected-window) prev)
       (kill-buffer buf))))

;;;; Task 1 — recording (`Q')

(ert-deftest kao-macro-record-key-captures-while-recording ()
  "The hook captures the current command's keys only while recording."
  (kao-macro-tests--clean
    (kao-macro-tests--feed [?x])           ; not recording yet
    (should (null kao--macro-recorded-keys))
    (kao--macro-start ?@)
    (kao-macro-tests--feed [?x] [?y])
    (should (equal kao--macro-recorded-keys (list [?y] [?x])))))  ; most-recent first

(ert-deftest kao-macro-record-key-skips-during-replay ()
  "Replayed sub-keys are not re-recorded (the m_handle_key_level guard)."
  (kao-macro-tests--clean
    (kao--macro-start ?@)
    (let ((kao--macro-replaying t))
      (kao-macro-tests--feed [?x] [?y]))
    (should (null kao--macro-recorded-keys))))

(ert-deftest kao-macro-record-drops-stop-key ()
  "Stopping stores the recorded keys minus the final `Q' that ended recording."
  (kao-macro-tests--clean
    (kao--macro-start ?@)
    (kao-macro-tests--feed [?l] [?l] [?Q])  ; two motions, then the stop `Q'
    (kao--macro-stop)
    (should (equal (kao-register-get-macro) [?l ?l]))
    (should (null kao--macro-recording-reg))))

(ert-deftest kao-macro-record-empty-clobbers ()
  "An immediate `Q Q' (only the stop key recorded) stores an empty macro.
Faithful to Kakoune storing \"\" — it clobbers any prior macro in the register."
  (kao-macro-tests--clean
    (kao-register-set-macro [?l ?l])        ; a prior macro
    (kao--macro-start ?@)
    (kao-macro-tests--feed [?Q])            ; only the stop `Q'
    (kao--macro-stop)
    (should (equal (kao-register-get-macro) []))))

(ert-deftest kao-macro-record-toggles ()
  "`kao-macro-record' starts on the first call and stops on the next."
  (kao-macro-tests--clean
    (kao-macro-record)                      ; start
    (should (eq kao--macro-recording-reg ?@))
    (kao-macro-tests--feed [?l] [?Q])       ; a key, then the stop `Q'
    (kao-macro-record)                      ; stop
    (should (null kao--macro-recording-reg))
    (should (equal (kao-register-get-macro) [?l]))))

(ert-deftest kao-macro-record-bound ()
  "`Q' is bound to `kao-macro-record' in the normal-state map."
  (should (eq (lookup-key kao-normal-state-map "Q") #'kao-macro-record)))

;;;; Task 2 — replay (`q')
;;
;; These unit tests stub `execute-kbd-macro' to assert the arguments and dynamic
;; bindings `kao-macro-play' passes (count, register-on-the-running-set, the
;; replaying flag) — a cheap check of the plumbing.  The actual replay EFFECT on
;; the buffer is covered end-to-end further down (`Task 2 — end-to-end …').  The
;; error paths run unstubbed.

(ert-deftest kao-macro-play-empty-register-errors ()
  "Replaying an empty/unset register errors (Kakoune \"register '@' is empty\")."
  (kao-macro-tests--clean
    (should-error (kao-macro-play) :type 'user-error)         ; unset
    (kao-register-set-macro [])                               ; empty vector
    (should-error (kao-macro-play) :type 'user-error)))

(ert-deftest kao-macro-play-recursion-guard ()
  "Replaying a register already replaying errors (no infinite recursion)."
  (kao-macro-tests--clean
    (kao-register-set-macro [?l])
    (let ((kao--macros-running (list ?@)))
      (should-error (kao-macro-play) :type 'user-error))))

(ert-deftest kao-macro-play-passes-count-keys-and-bindings ()
  "Replay feeds the stored keys to `execute-kbd-macro' COUNT = max(1,count) times,
with the register on the running set and `kao--macro-replaying' bound during it."
  (kao-macro-tests--clean
    (with-temp-buffer
      (kao-register-set-macro [?l ?l])
      (setq kao--count 3)
      (let (captured)
        (cl-letf (((symbol-function 'execute-kbd-macro)
                   (lambda (keys &optional count &rest _)
                     (setq captured (list keys count
                                          (and (memq ?@ kao--macros-running) t)
                                          kao--macro-replaying)))))
          (kao-macro-play))
        (should (equal (nth 0 captured) [?l ?l]))   ; the stored macro
        (should (= (nth 1 captured) 3))             ; count = max(1,3)
        (should (nth 2 captured))                    ; reg on the running set during replay
        (should (nth 3 captured)))                   ; replaying flag bound during replay
      ;; The running set + flag are unwound after replay.
      (should (null kao--macros-running))
      (should (null kao--macro-replaying)))))

(ert-deftest kao-macro-play-default-count-is-one ()
  "With no count, replay runs once (COUNT 1)."
  (kao-macro-tests--clean
    (with-temp-buffer
      (kao-register-set-macro [?l])
      (let (count)
        (cl-letf (((symbol-function 'execute-kbd-macro)
                   (lambda (_keys &optional c &rest _) (setq count c))))
          (kao-macro-play))
        (should (= count 1))))))

(ert-deftest kao-macro-play-bound ()
  "`q' is bound to `kao-macro-play' in the normal-state map."
  (should (eq (lookup-key kao-normal-state-map "q") #'kao-macro-play)))

;;;; Task 2 — end-to-end replay & record through the emulation map
;;
;; The flagship feature — record keys, replay N times over selections — driven for
;; real: `kao-macro-play' -> `execute-kbd-macro' -> kao's emulation map -> buffer
;; edits + `kao--sels' updates.  No `execute-kbd-macro' stub.  Expected outputs are
;; the ones the verifier reproduced independently.

(ert-deftest kao-macro-replay-plain-deletes-selected-word ()
  "Replaying [w d] selects the first word (with its trailing space) and deletes it."
  (kao-macro-tests--clean
    (kao-macro-tests--with-window "alpha beta gamma" 1
      (kao-register-set-macro [?w ?d])
      (kao-macro-play)
      (should (equal (buffer-string) "beta gamma"))
      ;; The replay left a single live selection (`d' collapses to a caret).
      (should (= (length (kao-sels-list kao--sels)) 1)))))

(ert-deftest kao-macro-replay-count-repeats-whole-macro ()
  "COUNT>1 runs the WHOLE [w d] macro that many times (Kakoune `q' with a count)."
  (kao-macro-tests--clean
    (kao-macro-tests--with-window "one two three four" 1
      (kao-register-set-macro [?w ?d])
      (setq kao--count 2)                 ; two words dropped: "one " then "two "
      (kao-macro-play)
      (setq kao--count 0)                 ; leave the buffer-local count clean
      (should (equal (buffer-string) "three four")))))

(ert-deftest kao-macro-replay-menu-char-stays-synced ()
  "A one-shot menu char inside a macro replays in sync: [<a-i> w d] deletes the
inner word.  This is the replay half of the desync bug the recording test pins —
`kao--read-key' pulls the object char `w' from the executing macro, so the
following `d' still lands on `d'."
  (kao-macro-tests--clean
    (kao-macro-tests--with-window "foo bar baz" 6      ; cursor inside "bar"
      (kao-register-set-macro [?\M-i ?w ?d])
      (kao-macro-play)
      (should (equal (buffer-string) "foo  baz")))))

(ert-deftest kao-macro-replay-count-undoes-as-one-unit ()
  "The whole COUNT>1 replay is a single undo unit (the change-group path,
kao-macro.el:137-144, unexercised with real edits until now): one undo step
restores BOTH deleted words, not just the last."
  (kao-macro-tests--clean
    (kao-macro-tests--with-window "one two three four" 1
      (undo-boundary)                    ; seal the setup edits off from the replay
      (kao-register-set-macro [?w ?d])
      (setq kao--count 2)
      (kao-macro-play)
      (setq kao--count 0)
      (should (equal (buffer-string) "three four"))
      (primitive-undo 1 buffer-undo-list)
      (should (equal (buffer-string) "one two three four")))))

(ert-deftest kao-macro-record-captures-real-dispatch-end-to-end ()
  "Recording captures keys fed through the REAL command loop in batch.
The `pre-command-hook' fires per key with `this-command-keys-vector' set even
under `execute-kbd-macro' — disproving the old \"batch cannot drive recording\"
note.  [w d] dispatched live is stored verbatim (the stop `Q' dropped)."
  (kao-macro-tests--clean
    (kao-macro-tests--with-window "alpha beta gamma" 1
      (kao--macro-start ?@)
      (execute-kbd-macro [?w ?d])        ; real dispatch → hook records each key
      (kao-macro-tests--feed [?Q])       ; the stop key, as the loop would fire it
      (kao--macro-stop)
      (should (equal (kao-register-get-macro ?@) [?w ?d])))))

;;;; Pending register (the `"' prefix)

(ert-deftest kao-macro-record-to-named-register ()
  "`\"B Q' records to register b — lowered up front (normal.cc:1750), so the
recording flag and the stop-store both see the canonical name."
  (kao-macro-tests--clean
    (with-temp-buffer
      (setq kao--pending-register ?B)
      (kao-macro-record)                       ; start
      (should (eq kao--macro-recording-reg ?b))
      (kao-macro-tests--feed [?l] [?Q])
      (kao-macro-record)                       ; stop (drops the stop `Q')
      (should (equal (kao-register-get-macro ?b) [?l]))
      (should (null (kao-register-get-macro ?@))))))

(ert-deftest kao-macro-play-from-named-register ()
  "`\"b q' replays register b's keys (to_lower(params.reg), normal.cc:1741)."
  (kao-macro-tests--clean
    (with-temp-buffer
      (kao-register-set-macro [?l ?l] ?b)
      (setq kao--pending-register ?B)
      (let (got)
        (cl-letf (((symbol-function 'execute-kbd-macro)
                   (lambda (keys count &rest _) (setq got (cons keys count)))))
          (kao-macro-play))
        (should (equal got (cons [?l ?l] 1)))))))

(ert-deftest kao-macro-play-named-register-empty-error ()
  "An empty named register errors with ITS name (\"register 'b' is empty\")."
  (kao-macro-tests--clean
    (with-temp-buffer
      (setq kao--pending-register ?b)
      (let ((err (should-error (kao-macro-play) :type 'user-error)))
        (should (equal (cadr err) (format-message "register 'b' is empty")))))))

(ert-deftest kao-macro-register-alpha-guard ()
  "Macros accept only `@' + alphabetic registers (normal.cc:1741-1744/:1986-1990)."
  (kao-macro-tests--clean
    (with-temp-buffer
      (setq kao--pending-register ?\")
      (let ((err (should-error (kao-macro-record) :type 'user-error)))
        (should (equal (cadr err)
                       (format-message
                        "macros can only use the '@' and alphabetic registers"))))
      (should (null kao--macro-recording-reg))   ; record did NOT start
      (let ((err (should-error (kao-macro-play) :type 'user-error)))
        (should (string-match-p "macros can only use" (cadr err)))))))

;;;; One-shot menu chars are recorded (`kao--read-key', )
;;
;; The recording hook captures only `this-command-keys-vector', so the char a
;; one-shot menu consumes via `read-key' (goto/view/object/combine/register)
;; was lost and replay desynced — the replayed `read-key' reads from the
;; executing macro, so the recorded stream must contain the menu char.
;; `kao--read-key' (kao-state) pushes it; these tests drive the wrapper
;; directly and end-to-end through the object menu.

(ert-deftest kao-macro-read-key-records-while-recording ()
  "`kao--read-key' pushes the menu char onto the recorder while recording."
  (kao-macro-tests--clean
    (setq kao--macro-recording-reg ?@)
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
      (should (eq (kao--read-key "object") ?w)))
    (should (equal kao--macro-recorded-keys (list [?w])))))

(ert-deftest kao-macro-read-key-records-cancel-key ()
  "The cancel key is recorded too.
Kakoune records EVERY key at the input level (input_handler.cc:1712), and a
replayed cancelled menu must re-eat its cancel key to stay in sync."
  (kao-macro-tests--clean
    (setq kao--macro-recording-reg ?@)
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) 'escape)))
      (kao--read-key "goto"))
    (should (equal kao--macro-recorded-keys (list [escape])))))

(ert-deftest kao-macro-read-key-skips-during-replay ()
  "No double-record while a macro replays through the menus."
  (kao-macro-tests--clean
    (setq kao--macro-recording-reg ?@
          kao--macro-replaying t)
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
      (kao--read-key "object"))
    (should (null kao--macro-recorded-keys))))

(ert-deftest kao-macro-read-key-inert-when-not-recording ()
  "Without a live recording the wrapper is a plain `read-key'."
  (kao-macro-tests--clean
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
      (should (eq (kao--read-key "object") ?w)))
    (should (null kao--macro-recorded-keys))))

(ert-deftest kao-macro-records-object-menu-char-end-to-end ()
  "A recorded `<a-i>w' stores BOTH keys in typed order.
The open-item repro: the stored macro used to contain only the `<a-i>', so
replay ate the next macro key as the object char and desynced."
  (kao-macro-tests--clean
    (with-temp-buffer
      (insert "foo bar")
      (kao-mode 1)
      (unwind-protect
          (cl-letf (((symbol-function 'read-key) (lambda (&rest _) ?w)))
            (setq kao--sels (kao-sels-make
                             :list (list (kao-sel-make :anchor 1 :cursor 1))
                             :main 0))
            (kao--macro-start ?@)
            (kao-macro-tests--feed [?\M-i]) ; the command's own keys (hook)
            (kao-select-inner)              ; menu consumes `w' via kao--read-key
            (kao-macro-tests--feed [?Q])    ; the stop key (hook), then stop
            (kao--macro-stop)
            (should (equal (kao-register-get-macro ?@) [?\M-i ?w])))
        (kao-mode -1)))))

(provide 'kao-macro-tests)
;;; kao-macro-tests.el ends here
