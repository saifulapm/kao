;;; kao-edit-tests.el --- Tests for kao-edit -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the P1 insert-entry commands.  Each enables `kao-mode', seeds a
;; single-char main selection, runs the command, and checks the insertion site
;; (and, after typing + exit, the buffer text and rebuilt cursor).

;;; Code:

(require 'ert)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-state)
(require 'kao-register)
(require 'kao-edit)
(require 'kao-keys)                    ; default bindings

(defmacro kao-edit-tests--with (content cursor &rest body)
  "Run BODY in a `kao-mode' temp buffer of CONTENT with main selection at CURSOR.
A clean, isolated system clipboard is bound so clipboard-aware paste is
deterministic regardless of test order — an empty clipboard means paste reads
the internal register path."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,content)
     (buffer-enable-undo)
     (kao-mode 1)
     (let ((kill-ring nil) (kill-ring-yank-pointer nil)
           (interprogram-cut-function nil) (interprogram-paste-function nil)
           (kao--clipboard-yank nil))
       (unwind-protect
           (progn
             (setq kao--sels (kao-sels-make
                              :list (list (kao-sel-make :anchor ,cursor :cursor ,cursor))
                              :main 0))
             ,@body)
         (kao-mode -1)))))

(ert-deftest kao-edit-i-inserts-before ()
  "`i' enters insert at the selection min; on exit the ORIGINAL span is kept.
The shaped `i' rebuild preserves the pre-insert selection,
shifted past the typed text, rather than collapsing on the typed char."
  (kao-edit-tests--with "abc" 2          ; cursor on 'b'
    (kao-insert)
    (should kao--insert-active)
    (should (= (point) 2))
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "aXbc"))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-cursor s) 3))    ; on 'b', the shifted original char
      (should (= (kao-sel-anchor s) 3)))))

(ert-deftest kao-edit-a-appends-after ()
  "`a' enters insert at max+1; on exit the selection covers original+typed.
The cursor steps onto the last typed char."
  (kao-edit-tests--with "abc" 2
    (kao-append)
    (should (= (point) 3))
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "abXc"))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-cursor s) 3))    ; the typed 'X'
      (should (= (kao-sel-anchor s) 2)))))  ; old min — span "bX"

(ert-deftest kao-edit-multi-insert-replays-at-all ()
  "`i' with N selections replays the net typed text before every selection."
  (kao-edit-tests--with "abc\ndef" 1               ; a1 b2 c3 \n4 d5 e6 f7
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 5 :cursor 5))
                     :main 0))
    (kao-insert)
    (should (= (point) 1))
    (insert "X")                                   ; types only at the main
    (kao-insert-exit)
    (should (string= (buffer-string) "Xabc\nXdef"))
    ;; Shaped `i': each 1-char span is kept, shifted past its "X".
    (should (equal (kao-edit-tests--pairs) '((2 . 2) (7 . 7))))
    (should (= 0 (kao-sels-main kao--sels)))))

(ert-deftest kao-edit-multi-append-replays-at-all ()
  "`a' with N selections replays the net typed text after every selection."
  (kao-edit-tests--with "ab\ncd" 1                 ; a1 b2 \n3 c4 d5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 4 :cursor 4))
                     :main 0))
    (kao-append)
    (should (= (point) 2))
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "aXb\ncXd"))
    ;; Shaped `a': each selection covers original+typed, cursor on "X".
    (should (equal (kao-edit-tests--pairs) '((1 . 2) (5 . 6))))))

(ert-deftest kao-edit-multi-insert-one-undo-unit ()
  "A multi-selection insert (type + replay) is a single undo unit."
  (kao-edit-tests--with "ab" 1
    (undo-boundary)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 2 :cursor 2))
                     :main 0))
    (kao-insert)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "XaXb"))
    (primitive-undo 1 buffer-undo-list)
    (should (string= (buffer-string) "ab"))))

(ert-deftest kao-edit-multi-append-nothing-typed-steps-back ()
  "`a' + immediate exit steps every cursor back to its selection (no edit)."
  (kao-edit-tests--with "ab\ncd" 1                 ; a1 b2 \n3 c4 d5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 4 :cursor 4))
                     :main 0))
    (kao-append)
    (kao-insert-exit)                              ; nothing typed
    (should (string= (buffer-string) "ab\ncd"))    ; buffer untouched
    (should (equal (kao-edit-tests--pairs) '((1 . 1) (4 . 4))))))

(ert-deftest kao-edit-multi-insert-three-lines-tracks-main ()
  "`i' across 3 lines (main = middle) prepends to each; the main is tracked."
  (kao-edit-tests--with "x\ny\nz" 1                ; x1 \n2 y3 \n4 z5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 3 :cursor 3)
                                 (kao-sel-make :anchor 5 :cursor 5))
                     :main 1))                     ; main = the y line
    (kao-insert)
    (insert "<")
    (kao-insert-exit)
    (should (string= (buffer-string) "<x\n<y\n<z"))
    ;; Shaped `i': each 1-char span kept, shifted past its "<".
    (should (equal (kao-edit-tests--pairs) '((2 . 2) (5 . 5) (8 . 8))))
    (should (= 1 (kao-sels-main kao--sels)))))      ; still the middle ("<y")

(ert-deftest kao-edit-multi-insert-disable-is-clean ()
  "Disabling kao-mode mid multi-insert frees the secondary markers cleanly."
  (kao-edit-tests--with "ab" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 2 :cursor 2))
                     :main 0))
    (kao-insert)
    (insert "Z")
    (should kao--insert-secondary-sites)           ; armed during the session
    (kao-mode -1)                                  ; abnormal exit (no replay)
    (should-not kao--insert-secondary-sites)
    (should-not kao--insert-active)
    (should (string= (buffer-string) "Zab"))))      ; only the main got the text

(ert-deftest kao-edit-I-inserts-at-first-nonblank ()
  "`I' enters insert at the first non-blank char of the line."
  (kao-edit-tests--with "  fo" 4          ; \"  fo\": indent then 'fo'
    (kao-insert-line-begin)
    (should (= (point) 3))))

(ert-deftest kao-edit-I-on-all-blank-line-stays-at-column-0 ()
  "`I' on an all-blank line stays at column 0 (not the trailing newline).
Mirrors Kakoune `InsertAtLineBegin' guard (input_handler.cc:1525)."
  (kao-edit-tests--with "   \nx" 2          ; line 1 is all blanks: sp1 sp2 sp3 \n4 x5
    (kao-insert-line-begin)
    (should (= (point) 1)))                 ; beginning of the blank line
  ;; all-blank line with no trailing newline (buffer end)
  (kao-edit-tests--with "   " 2
    (kao-insert-line-begin)
    (should (= (point) 1))))

(ert-deftest kao-edit-A-appends-at-line-end ()
  "`A' enters insert at end of line, before the newline."
  (kao-edit-tests--with "foo\nbar" 2
    (kao-append-line-end)
    (should (= (point) 4))))

(ert-deftest kao-edit-multi-I-replays-at-first-nonblank ()
  "`I' across lines replays the typed text at each line's first non-blank."
  (kao-edit-tests--with "  ab\n    cd" 3           ; first-nonblank: 3 and 10
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 3 :cursor 3)
                                 (kao-sel-make :anchor 10 :cursor 10))
                     :main 0))
    (kao-insert-line-begin)
    (should (= (point) 3))
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "  Xab\n    Xcd"))
    ;; Shaped `I' collapses one char after each typed "X".
    (should (equal (kao-edit-tests--pairs) '((4 . 4) (12 . 12))))))

(ert-deftest kao-edit-multi-A-replays-at-line-end ()
  "`A' across lines replays the typed text at each line end."
  (kao-edit-tests--with "ab\ncd" 1                 ; line ends: 3 and 6
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 4 :cursor 4))
                     :main 0))
    (kao-append-line-end)
    (should (= (point) 3))
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "abX\ncdX"))
    ;; Shaped `A' collapses one char after each "X"; the last is at buffer end,
    ;; so its cursor clamps back onto its "X".
    (should (equal (kao-edit-tests--pairs) '((4 . 4) (7 . 7))))))

(ert-deftest kao-edit-o-opens-line-below ()
  "`o' opens a new line below and inserts there."
  (kao-edit-tests--with "foo\nbar" 1
    (kao-open-below)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "foo\nX\nbar"))
    ;; Shaped collapse: the cursor lands one char after the typed "X".
    (should (= (char-before (kao-sel-cursor (kao--main-sel))) ?X))))

(ert-deftest kao-edit-O-opens-line-above ()
  "`O' opens a new line above the current line and inserts there."
  (kao-edit-tests--with "foo\nbar" 5      ; cursor on 'b' of line 2
    (kao-open-above)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "foo\nX\nbar"))
    ;; Shaped collapse: the cursor lands one char after the typed "X".
    (should (= (char-before (kao-sel-cursor (kao--main-sel))) ?X))))

(ert-deftest kao-edit-multi-open-below-replays ()
  "`o' opens a blank line below each selection's line and replays the typed text."
  (kao-edit-tests--with "ab\ncd" 1                 ; a1 b2 \n3 c4 d5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)   ; line 1
                                 (kao-sel-make :anchor 5 :cursor 5))  ; line 2
                     :main 0))
    (kao-open-below)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "ab\nX\ncd\nX"))  ; a line X under each
    ;; Shaped `o' collapses one char after each "X"; the last is at buffer end,
    ;; so its cursor clamps back onto its "X".
    (should (equal (kao-edit-tests--pairs) '((5 . 5) (9 . 9))))))

(ert-deftest kao-edit-multi-open-below-one-undo-unit ()
  "A multi-selection `o' (open + replay) is a single undo unit."
  (kao-edit-tests--with "a\nb" 1                   ; a1 \n2 b3
    (undo-boundary)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 3 :cursor 3))
                     :main 0))
    (kao-open-below)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "a\nX\nb\nX"))
    (primitive-undo 1 buffer-undo-list)
    (should (string= (buffer-string) "a\nb"))))

(ert-deftest kao-edit-multi-open-above-replays ()
  "`O' opens a blank line above each selection's line and replays the typed text."
  (kao-edit-tests--with "ab\ncd" 1                 ; a1 b2 \n3 c4 d5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)   ; line 1
                                 (kao-sel-make :anchor 4 :cursor 4))  ; line 2
                     :main 0))
    (kao-open-above)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "X\nab\nX\ncd"))  ; a line X above each
    ;; Shaped `O' collapses one char after each typed "X".
    (should (equal (kao-edit-tests--pairs) '((2 . 2) (7 . 7))))))

(ert-deftest kao-edit-count-o-opens-n-lines ()
  "`3o' opens 3 lines below the selection's line, one selection per line.
Text typed replays on every opened line; the main is the LAST of the group
\(`main_index()*count + count - 1', input_handler.cc:1483-1497)."
  (kao-edit-tests--with "foo\nbar" 1
    (setq kao--count 3)
    (kao-open-below)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "foo\nX\nX\nX\nbar"))
    ;; Shaped collapse: each cursor lands one char after its "X".
    (should (equal (kao-edit-tests--pairs) '((6 . 6) (8 . 8) (10 . 10))))
    (should (= (kao-sel-cursor (kao--main-sel)) 10))))

(ert-deftest kao-edit-count-O-opens-n-lines-above ()
  "`2O' opens 2 lines above, ascending, main the last of the group."
  (kao-edit-tests--with "foo\nbar" 5      ; cursor on 'b' of line 2
    (setq kao--count 2)
    (kao-open-above)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "foo\nX\nX\nbar"))
    ;; Shaped collapse: each cursor lands one char after its "X".
    (should (equal (kao-edit-tests--pairs) '((6 . 6) (8 . 8))))
    (should (= (kao-sel-cursor (kao--main-sel)) 8))))

(ert-deftest kao-edit-count-o-multi-sel-main-mapping ()
  "Multi-selection `2o': N*count sites; main site = main*count+count-1.
Original main is selection 1 (of 0..1), so the final main is the LAST opened
line of the second group."
  (kao-edit-tests--with "ab\ncd" 1                 ; a1 b2 \n3 c4 d5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)   ; line 1
                                 (kao-sel-make :anchor 5 :cursor 5))  ; line 2
                     :main 1))
    (setq kao--count 2)
    (kao-open-below)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "ab\nX\nX\ncd\nX\nX"))
    ;; Shaped collapse: each cursor lands one char after its "X"; the last "X" is
    ;; at buffer end, so its cursor clamps back onto it.
    (should (equal (kao-edit-tests--pairs) '((5 . 5) (7 . 7) (12 . 12) (13 . 13))))
    (should (= (kao-sel-cursor (kao--main-sel)) 13))))

(ert-deftest kao-edit-count-dot-replays-stored-count ()
  "`.' after `2o' re-opens 2 lines: the stored count replays
\(`m_last_insert.count', input_handler.cc:1197/:1610)."
  (kao-edit-tests--with "a" 1
    (setq kao--count 2)
    (kao-open-below)
    (insert "Y")
    (kao-insert-exit)
    (should (string= (buffer-string) "a\nY\nY"))
    ;; Collapse to ONE selection (on the 'a' line) so the assertion isolates
    ;; the STORED count — with the two live Y selections kept, `.' would
    ;; faithfully open 2 lines under EACH (prepare runs per selection).
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1))
                     :main 0))
    (setq kao--count 0)                  ; no pending count before `.'
    (kao-repeat-insert)
    (should (string= (buffer-string) "a\nY\nY\nY\nY"))))

(ert-deftest kao-edit-count-dot-ignores-typed-count ()
  "A count typed before `.' is IGNORED — `5.' does not multiply
\(`repeat_last_insert' takes unnamed NormalParams, normal.cc:172-175)."
  (kao-edit-tests--with "a" 1
    (kao-open-below)                     ; stored count = 1
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "a\nX"))
    (setq kao--count 5)
    (kao-repeat-insert)
    (should (string= (buffer-string) "a\nX\nX"))))  ; ONE line, not five

(ert-deftest kao-edit-count-insert-stores-but-no-repeat ()
  "`3i' does NOT repeat the typed text — `prepare' ignores count for plain
insert (input_handler.cc:1463); the count is merely stored for `.'."
  (kao-edit-tests--with "ab" 1
    (setq kao--count 3)
    (kao-insert)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "Xab"))   ; one X, not three
    (should (= kao--last-insert-count 3))
    (setq kao--count 0)
    (kao-repeat-insert)
    (should (string= (buffer-string) "XXab")))) ; `.' inserts once too

(ert-deftest kao-edit-keys-bound ()
  "The entry commands are bound in the normal-state map."
  (should (eq (lookup-key kao-normal-state-map "i") #'kao-insert))
  (should (eq (lookup-key kao-normal-state-map "a") #'kao-append))
  (should (eq (lookup-key kao-normal-state-map "I") #'kao-insert-line-begin))
  (should (eq (lookup-key kao-normal-state-map "A") #'kao-append-line-end))
  (should (eq (lookup-key kao-normal-state-map "o") #'kao-open-below))
  (should (eq (lookup-key kao-normal-state-map "O") #'kao-open-above)))

;;;; Kao-open-indent — opt-in autoindent for o/O

(defun kao-edit-tests--indent-to-2 ()
  "A deterministic `indent-line-function': indent the current line to column 2.
Grammar-free stand-in for a prog mode's indenter so `indent-according-to-mode'
yields a fixed, machine-independent result (see CI-skip caveat)."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (delete-region (line-beginning-position) (point))
    (insert "  ")))

(ert-deftest kao-edit-open-indent-default-off-column-0 ()
  "With `kao-open-indent' nil (default), `o' opens at column 0 — bare core."
  (kao-edit-tests--with "foo\nbar" 1
    (setq-local indent-line-function #'kao-edit-tests--indent-to-2)
    (let ((kao-open-indent nil))
      (kao-open-below)
      (insert "X")
      (kao-insert-exit)
      (should (string= (buffer-string) "foo\nX\nbar"))
      ;; Shaped collapse: cursor one char after the typed "X".
      (should (= (char-before (kao-sel-cursor (kao--main-sel))) ?X)))))

(ert-deftest kao-edit-open-indent-on-indents-site ()
  "With `kao-open-indent' t, `o' indents the opened line and inserts after it."
  (kao-edit-tests--with "foo\nbar" 1
    (setq-local indent-line-function #'kao-edit-tests--indent-to-2)
    (let ((kao-open-indent t))
      (kao-open-below)
      (insert "X")
      (kao-insert-exit)
      (should (string= (buffer-string) "foo\n  X\nbar"))
      ;; The site advanced past the indentation; the shaped collapse then lands
      ;; the cursor one char after the typed 'X'.
      (should (= (char-before (kao-sel-cursor (kao--main-sel))) ?X)))))

(ert-deftest kao-edit-open-indent-on-above-indents-site ()
  "With `kao-open-indent' t, `O' indents the opened line above too."
  (kao-edit-tests--with "foo\nbar" 5      ; cursor on 'b' of line 2
    (setq-local indent-line-function #'kao-edit-tests--indent-to-2)
    (let ((kao-open-indent t))
      (kao-open-above)
      (insert "X")
      (kao-insert-exit)
      (should (string= (buffer-string) "foo\n  X\nbar"))
      ;; Shaped collapse: cursor one char after the typed "X".
      (should (= (char-before (kao-sel-cursor (kao--main-sel))) ?X)))))

(ert-deftest kao-edit-open-indent-multi-selection-replay ()
  "`o' with `kao-open-indent' t replays the typed text after each line's indent.
Each opened line is indented independently (replay lands after its own
indentation)."
  (kao-edit-tests--with "ab\ncd" 1                 ; a1 b2 \n3 c4 d5
    (setq-local indent-line-function #'kao-edit-tests--indent-to-2)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)   ; line 1
                                 (kao-sel-make :anchor 5 :cursor 5))  ; line 2
                     :main 0))
    (let ((kao-open-indent t))
      (kao-open-below)
      (insert "X")
      (kao-insert-exit)
      (should (string= (buffer-string) "ab\n  X\ncd\n  X"))
      ;; Shaped collapse: each cursor lands one char after its own "X" (past that
      ;; line's indentation); the last "X" is at buffer end, so its cursor clamps
      ;; back onto it.
      (should (equal (kao-edit-tests--pairs) '((7 . 7) (13 . 13)))))))

(ert-deftest kao-edit-open-indent-dot-repeat-reindents ()
  "`.' after an indented `o' re-indents the freshly opened line."
  (kao-edit-tests--with "foo" 1
    (setq-local indent-line-function #'kao-edit-tests--indent-to-2)
    (let ((kao-open-indent t))
      (kao-open-below)
      (insert "X")
      (kao-insert-exit)
      (should (string= (buffer-string) "foo\n  X"))
      (setq kao--sels (kao-sels-make
                       :list (list (kao-sel-make :anchor 1 :cursor 1))
                       :main 0))
      (setq kao--count 0)
      (kao-repeat-insert)
      (should (string= (buffer-string) "foo\n  X\n  X")))))

;;;; Yank / delete / change

(ert-deftest kao-edit-y-yanks-to-register ()
  "`y' copies the selection text to the register and the kill-ring; no edit."
  (kao-edit-tests--with "hello" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 5)) :main 0))
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (kao-yank)
      (should (equal (kao-register-get kao-register-default) '("hello")))
      (should (string= (current-kill 0) "hello"))
      (should (string= (buffer-string) "hello"))           ; buffer unchanged
      (should (= (kao-sel-cursor (kao--main-sel)) 5)))))    ; selection unchanged

(ert-deftest kao-edit-d-deletes-and-yanks ()
  "`d' yanks the selection, deletes it, and collapses to the gap."
  (kao-edit-tests--with "abcdef" 2
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 3)) :main 0))
    (kao-delete)
    (should (string= (buffer-string) "adef"))
    (should (equal (kao-register-get kao-register-default) '("bc")))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) 2))
      (should (= (kao-sel-cursor s) 2)))
    (should (= (char-after 2) ?d))))

(ert-deftest kao-edit-d-at-end-clamps ()
  "Deleting the last char clamps the collapsed cursor to the new last char."
  (kao-edit-tests--with "abc" 3                            ; cursor on 'c'
    (kao-delete)
    (should (string= (buffer-string) "ab"))
    (should (= (kao-sel-cursor (kao--main-sel)) 2))))       ; clamped to 'b'

(ert-deftest kao-edit-y-multi-mirrors-all-joined ()
  "`y' with N selections stores N strings; ALL of them (joined) mirror to clipboard."
  (kao-edit-tests--with "a b c" 1                  ; a1 _2 b3 _4 c5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 3 :cursor 3)
                                 (kao-sel-make :anchor 5 :cursor 5))
                     :main 1))                     ; main = "b"
    (kao-yank)
    (should (equal (kao-register-get kao-register-default) '("a" "b" "c")))
    (should (string= (current-kill 0) "a\nb\nc"))))  ; all selections, joined

(ert-deftest kao-edit-d-multi-deletes-all ()
  "`d' with N selections deletes every region, each collapsing to its gap."
  (kao-edit-tests--with "abc\ndef\nghi" 1          ; a1..\n4 d5..\n8 g9..i11
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 5 :cursor 5)
                                 (kao-sel-make :anchor 9 :cursor 9))
                     :main 0))
    (kao-delete)
    (should (string= (buffer-string) "bc\nef\nhi"))
    (should (equal (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                           (kao-sels-list kao--sels))
                   '((1 . 1) (4 . 4) (7 . 7))))))

(ert-deftest kao-edit-d-multi-one-undo-unit ()
  "A multi-selection delete is a single undo unit."
  (kao-edit-tests--with "ab\ncd" 1                 ; a1 b2 \n3 c4 d5
    (undo-boundary)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 4 :cursor 4))
                     :main 0))
    (kao-delete)
    (should (string= (buffer-string) "b\nd"))
    (primitive-undo 1 buffer-undo-list)
    (should (string= (buffer-string) "ab\ncd"))))

(ert-deftest kao-edit-d-adjacent-coincident-cursors ()
  "Adjacent deletions collapse to the same point; kao does not post-merge them."
  (kao-edit-tests--with "abcd" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 2 :cursor 2))
                     :main 0))
    (kao-delete)
    (should (string= (buffer-string) "cd"))
    (should (equal (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                           (kao-sels-list kao--sels))
                   '((1 . 1) (1 . 1))))))           ; two cursors persist at the gap

(ert-deftest kao-edit-c-changes-then-inserts ()
  "`c' yanks, deletes, enters insert; change + typed text is one undo unit."
  (kao-edit-tests--with "abc" 1                            ; cursor on 'a'
    (kao-change)
    (should (string= (buffer-string) "bc"))                ; 'a' deleted
    (should kao--insert-active)
    (should (= (point) 1))
    (should (equal (kao-register-get kao-register-default) '("a")))
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "Xbc"))
    (should-not kao--insert-active)
    (primitive-undo 1 buffer-undo-list)                    ; one logical undo
    (should (string= (buffer-string) "abc"))))

(ert-deftest kao-edit-multi-change-replaces-all ()
  "`c' with N selections deletes each and replays the typed text at every gap."
  (kao-edit-tests--with "ab cd" 1                  ; a1 b2 _3 c4 d5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)   ; "ab"
                                 (kao-sel-make :anchor 4 :cursor 5))  ; "cd"
                     :main 0))
    (kao-change)
    (should (equal (kao-register-get kao-register-default) '("ab" "cd")))
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "X X"))
    ;; Shaped `c' collapses one char after each "X"; the last is at buffer end,
    ;; so its cursor clamps back onto its "X".
    (should (equal (kao-edit-tests--pairs) '((2 . 2) (3 . 3))))))

(ert-deftest kao-edit-multi-change-nothing-typed-just-deletes ()
  "`c' + immediate exit deletes every selection, cursors on the gaps."
  (kao-edit-tests--with "Xab cdY" 1                ; X1 a2 b3 _4 c5 d6 Y7
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 3)   ; "ab"
                                 (kao-sel-make :anchor 5 :cursor 6))  ; "cd"
                     :main 0))
    (kao-change)
    (kao-insert-exit)
    (should (string= (buffer-string) "X Y"))
    (should (equal (kao-edit-tests--pairs) '((2 . 2) (3 . 3))))))

(ert-deftest kao-edit-multi-change-one-undo-unit ()
  "A multi-selection change (delete + replay) is a single undo unit."
  (kao-edit-tests--with "ab cd" 1
    (undo-boundary)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)
                                 (kao-sel-make :anchor 4 :cursor 5))
                     :main 0))
    (kao-change)
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "X X"))
    (primitive-undo 1 buffer-undo-list)
    (should (string= (buffer-string) "ab cd"))))

;;;; Paste (after / before / replace)

(ert-deftest kao-edit-p-paste-after-charwise ()
  "`p' inserts the register after the selection; new sel covers the paste."
  (kao-edit-tests--with "abc" 2                  ; cursor on 'b'
    (kao-register-set kao-register-default '("X"))
    (kao-paste-after)
    (should (string= (buffer-string) "abXc"))
    (should (= (kao-sel-cursor (kao--main-sel)) 3))
    (should (= (char-after 3) ?X))))

(ert-deftest kao-edit-P-paste-before-charwise ()
  "`P' inserts the register before the selection."
  (kao-edit-tests--with "abc" 2
    (kao-register-set kao-register-default '("X"))
    (kao-paste-before)
    (should (string= (buffer-string) "aXbc"))
    (should (= (kao-sel-cursor (kao--main-sel)) 2))))

(ert-deftest kao-edit-R-replace-charwise ()
  "`R' replaces the selection with the register."
  (kao-edit-tests--with "abc" 2
    (kao-register-set kao-register-default '("X"))
    (kao-replace)
    (should (string= (buffer-string) "aXc"))
    (should (= (kao-sel-cursor (kao--main-sel)) 2))))

(ert-deftest kao-edit-R-replace-multichar-span ()
  "`R' over a multi-char selection covers the whole replacement span."
  (kao-edit-tests--with "abcd" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)) :main 0)) ; "ab"
    (kao-register-set kao-register-default '("XY"))
    (kao-replace)
    (should (string= (buffer-string) "XYcd"))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) 1))
      (should (= (kao-sel-cursor s) 2)))))         ; covers "XY"

(ert-deftest kao-edit-paste-preserves-selection-direction ()
  "`R' keeps each selection's direction over the pasted span.
Mirrors `kao-edit-rotate-content-preserves-direction': Kakoune's paste assigns
through the direction-preserving min()/max() refs (selection.hh:51-56)."
  (kao-edit-tests--with "abcdef" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 3 :cursor 1)   ; backward "abc"
                                 (kao-sel-make :anchor 4 :cursor 6))  ; forward "def"
                     :main 0))
    (kao-register-set kao-register-default '("XY"))
    (kao-replace)
    (should (string= (buffer-string) "XYXY"))
    (should (equal (kao-edit-tests--pairs) '((2 . 1) (3 . 4))))))  ; backward, forward

(ert-deftest kao-edit-paste-after-preserves-backward-direction ()
  "`p' keeps a backward selection backward over the pasted span."
  (kao-edit-tests--with "abc" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 3 :cursor 1)) :main 0)) ; backward "abc"
    (kao-register-set kao-register-default '("XY"))
    (kao-paste-after)
    (should (string= (buffer-string) "abcXY"))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) 5))            ; anchor at the pasted span's end
      (should (= (kao-sel-cursor s) 4)))))         ; cursor at its start (backward kept)

;;;; Empty-buffer policy
;; In an empty accessible region the inclusive [beg,end] span degenerates to
;; "" — every edit-family command clamps its inclusive end to `point-max' and
;; MUST NOT signal `args-out-of-range' (kao-append precedent, kao-edit.el:55).

(ert-deftest kao-edit-yank-empty-buffer-yanks-empty ()
  "`y' in an empty buffer yanks the empty string (no signal)."
  (kao-edit-tests--with "" 1
    (kao-yank)
    (should (equal '("") (kao-register-get kao-register-default)))
    (should (string= "" (buffer-string)))))

(ert-deftest kao-edit-delete-empty-buffer-noop ()
  "`d' in an empty buffer deletes nothing (no signal)."
  (kao-edit-tests--with "" 1
    (kao-delete)
    (should (string= "" (buffer-string)))))

(ert-deftest kao-edit-change-empty-buffer-enters-insert ()
  "`c' in an empty buffer still opens the insert session (no signal)."
  (kao-edit-tests--with "" 1
    (kao-change)
    (should kao--insert-active)
    (should (string= "" (buffer-string)))))

(ert-deftest kao-edit-upcase-empty-buffer-noop ()
  "`~' in an empty buffer is a no-op (no signal)."
  (kao-edit-tests--with "" 1
    (kao-upcase)
    (should (string= "" (buffer-string)))))

(ert-deftest kao-edit-downcase-empty-buffer-noop ()
  "\\=` in an empty buffer is a no-op (no signal)."
  (kao-edit-tests--with "" 1
    (kao-downcase)
    (should (string= "" (buffer-string)))))

(ert-deftest kao-edit-swapcase-empty-buffer-noop ()
  "`<a-`>' in an empty buffer is a no-op (no signal)."
  (kao-edit-tests--with "" 1
    (kao-swapcase)
    (should (string= "" (buffer-string)))))

(ert-deftest kao-edit-replace-char-empty-buffer-noop ()
  "`r' in an empty buffer replaces nothing (no signal)."
  (kao-edit-tests--with "" 1
    (kao--replace-char-with ?x)
    (should (string= "" (buffer-string)))))

(ert-deftest kao-edit-R-empty-buffer-inserts-register ()
  "`R' in an empty buffer inserts the (non-empty) register with no signal."
  (kao-edit-tests--with "" 1
    (kao-register-set kao-register-default '("X"))
    (kao-replace)
    (should (string= "X" (buffer-string)))))

(ert-deftest kao-edit-replace-all-empty-buffer-inserts-register ()
  "`<a-R>' in an empty buffer inserts the register strings with no signal."
  (kao-edit-tests--with "" 1
    (kao-register-set kao-register-default '("Y"))
    (kao-replace-all)
    (should (string= "Y" (buffer-string)))))

(ert-deftest kao-edit-p-paste-after-linewise ()
  "Linewise `p' pastes on the line below the selection's line."
  (kao-edit-tests--with "foo\nbar" 1
    (kao-register-set kao-register-default '("L1\n"))
    (kao-paste-after)
    (should (string= (buffer-string) "foo\nL1\nbar"))))

(ert-deftest kao-edit-P-paste-before-linewise ()
  "Linewise `P' pastes on the line above the selection's line."
  (kao-edit-tests--with "foo\nbar" 5              ; cursor on 'b' of line 2
    (kao-register-set kao-register-default '("L1\n"))
    (kao-paste-before)
    (should (string= (buffer-string) "foo\nL1\nbar"))))

(ert-deftest kao-edit-p-linewise-last-line-no-newline ()
  "Linewise `p' on a last line with no trailing newline adds one first."
  (kao-edit-tests--with "foo\nbar" 5
    (kao-register-set kao-register-default '("L1\n"))
    (kao-paste-after)
    (should (string= (buffer-string) "foo\nbar\nL1\n"))))

(ert-deftest kao-edit-replace-empty-register-erases ()
  "`R' with an unset register reads [\"\"] and erases the span.
register_manager.cc:30-36 returns [\"\"] on empty, so Replace deletes the
selection and pastes nothing; the selection collapses at the erase site."
  (kao-edit-tests--with "abc" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3)) :main 0)) ; "abc"
    (kao-register-set kao-register-default nil)
    (kao-replace)
    (should (string= (buffer-string) ""))          ; span erased
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) (kao-sel-cursor s))))))   ; collapsed

(ert-deftest kao-edit-paste-after-empty-register-collapses ()
  "`p' with an unset register reads [\"\"] and collapses at the paste position.
The buffer text is unchanged (nothing inserted), but the paste no longer bails
out with \"nothing to paste\" — the selection collapses at the append site."
  (kao-edit-tests--with "abc" 2                  ; cursor on 'b'
    (kao-register-set kao-register-default nil)
    (kao-paste-after)
    (should (string= (buffer-string) "abc"))       ; nothing inserted
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) (kao-sel-cursor s)))   ; collapsed
      (should (= (kao-sel-cursor s) 3)))))                 ; at the append position

(ert-deftest kao-edit-replace-all-empty-register-refuses ()
  "`<a-R>' with an unset register still refuses: `paste_all' throws (normal.cc:877-878).
Only the paste_all family bails on an empty register; `R'/`p'/`P' proceed as [\"\"]."
  (kao-edit-tests--with "abc" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3)) :main 0)) ; "abc"
    (kao-register-set kao-register-default nil)
    (let (captured)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq captured (apply #'format fmt args)))))
        (kao-replace-all))
      (should (equal captured "kao: nothing to paste"))
      (should (string= (buffer-string) "abc")))))            ; buffer untouched

(ert-deftest kao-edit-replace-empty-register-empty-buffer-clean ()
  "`R' with an unset register in an empty buffer is a clean no-op.
The [\"\"] read must not signal `args-out-of-range' at the inclusive-end clamp."
  (kao-edit-tests--with "" 1
    (kao-register-set kao-register-default nil)
    (kao-replace)                                  ; must not signal
    (should (string= (buffer-string) ""))))

(ert-deftest kao-edit-yank-then-paste-duplicates ()
  "Round-trip: `y' then `p' duplicates the selection text."
  (kao-edit-tests--with "abc" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3)) :main 0)) ; "abc"
    (kao-yank)
    (kao-paste-after)
    (should (string= (buffer-string) "abcabc"))))

(ert-deftest kao-edit-paste-uses-external-clipboard ()
  "When the clipboard changed externally, `p' pastes it to every selection."
  (kao-edit-tests--with "ab" 1                       ; a1 b2
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 2 :cursor 2))
                     :main 0))
    (kao-register-set kao-register-default '("INT"))  ; kao's internal register
    (kill-new "EXT")                                  ; another app copied (differs)
    (kao-paste-after)
    (should (string= (buffer-string) "aEXTbEXT"))))   ; external wins, to all

(ert-deftest kao-edit-yank-then-multi-paste-cycles ()
  "After a real `y', `p' uses the internal cycling list, not the joined clipboard."
  (kao-edit-tests--with "ab" 1                       ; a1 b2
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 2 :cursor 2))
                     :main 0))
    (kao-yank)                                        ; register ("a" "b"); clip "a\nb"
    (kao-paste-after)
    (should (string= (buffer-string) "aabb"))))       ; i-th->i-th, NOT "a\nb" to all

(ert-deftest kao-edit-paste-keys-bound ()
  "p/P/R are bound in the normal-state map."
  (should (eq (lookup-key kao-normal-state-map "p") #'kao-paste-after))
  (should (eq (lookup-key kao-normal-state-map "P") #'kao-paste-before))
  (should (eq (lookup-key kao-normal-state-map "R") #'kao-replace)))

;;;; Multi-selection paste — register cycling

(defun kao-edit-tests--pairs ()
  "Return `kao--sels' as a list of (anchor . cursor) pairs."
  (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
          (kao-sels-list kao--sels)))

(ert-deftest kao-edit-multi-paste-after-cycles ()
  "`p' with two selections pastes the i-th register string to the i-th cursor."
  (kao-edit-tests--with "ab\ncd" 1                 ; a1 b2 \n3 c4 d5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 4 :cursor 4))
                     :main 0))
    (kao-register-set kao-register-default '("X" "Y"))
    (kao-paste-after)
    (should (string= (buffer-string) "aXb\ncYd"))
    (should (equal (kao-edit-tests--pairs) '((2 . 2) (6 . 6))))))

(ert-deftest kao-edit-multi-paste-single-string-to-all ()
  "A one-string register pastes that string at every selection (i mod 1 = 0)."
  (kao-edit-tests--with "ab" 1                      ; a1 b2
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 2 :cursor 2))
                     :main 0))
    (kao-register-set kao-register-default '("Z"))
    (kao-paste-after)
    (should (string= (buffer-string) "aZbZ"))
    (should (equal (kao-edit-tests--pairs) '((2 . 2) (4 . 4))))))

(ert-deftest kao-edit-multi-paste-before-cycles ()
  "`P' cycles the register before each selection."
  (kao-edit-tests--with "ab" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 2 :cursor 2))
                     :main 0))
    (kao-register-set kao-register-default '("X" "Y"))
    (kao-paste-before)
    (should (string= (buffer-string) "XaYb"))
    (should (equal (kao-edit-tests--pairs) '((1 . 1) (3 . 3))))))

(ert-deftest kao-edit-multi-replace-cycles ()
  "`R' replaces each selection with its cycled register string."
  (kao-edit-tests--with "ab" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 2 :cursor 2))
                     :main 0))
    (kao-register-set kao-register-default '("X" "Y"))
    (kao-replace)
    (should (string= (buffer-string) "XY"))
    (should (equal (kao-edit-tests--pairs) '((1 . 1) (2 . 2))))))

(ert-deftest kao-edit-multi-paste-linewise-distinct-lines ()
  "Linewise `p' pastes the register line below each selection's line."
  (kao-edit-tests--with "a\nb" 1                   ; a1 \n2 b3
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 3 :cursor 3))
                     :main 0))
    (kao-register-set kao-register-default '("L\n"))  ; linewise (ends in \n)
    (kao-paste-after)
    (should (string= (buffer-string) "a\nL\nb\nL\n"))))   ; lines: a L b L

(ert-deftest kao-edit-p-linewise-same-line-stacks ()
  "Linewise `p' with several selections on the SAME line stacks the pastes.
The `last'-tracking site `max(max, last)' (normal.cc:850): the second
selection pastes below the FIRST PASTED line, not below the shared source
line, and both result selections cover their own pasted line."
  (kao-edit-tests--with "ab\ncd" 1                 ; a1 b2 \n3 c4 d5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 2 :cursor 2))
                     :main 0))
    (kao-register-set kao-register-default '("L\n"))
    (kao-paste-after)
    (should (string= (buffer-string) "ab\nL\nL\ncd"))
    (should (equal (kao-edit-tests--pairs) '((4 . 5) (6 . 7))))))

(ert-deftest kao-edit-p-overlapping-nested ()
  "Charwise `p' on a NESTED (overlapping) list keeps every result span valid.
Overlapping lists are producible (pairwise combine installs unmerged,
normal.cc:2103); `last'-tracking puts the inner selection's paste
strictly after the outer's result, so neither integer span goes stale."
  (kao-edit-tests--with "abcdef" 1                 ; a1 b2 c3 d4 e5 f6
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 5)
                                 (kao-sel-make :anchor 2 :cursor 3))
                     :main 0))
    (kao-register-set kao-register-default '("X" "Y"))
    (kao-paste-after)
    (should (string= (buffer-string) "abcdeXYf"))
    (should (equal (kao-edit-tests--pairs) '((6 . 6) (7 . 7))))))

(ert-deftest kao-edit-p-coincident-sels ()
  "Charwise `p' on two COINCIDENT selections pastes in order, both spans valid.
Coincident selections arise from clamped snapshot restores; without
`last'-tracking the second paste would land at the same site and shift the
first result stale."
  (kao-edit-tests--with "abc" 1                    ; a1 b2 c3
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)
                                 (kao-sel-make :anchor 1 :cursor 2))
                     :main 0))
    (kao-register-set kao-register-default '("X" "Y"))
    (kao-paste-after)
    (should (string= (buffer-string) "abXYc"))
    (should (equal (kao-edit-tests--pairs) '((3 . 3) (4 . 4))))))

(ert-deftest kao-edit-r-merges-overlapping-first ()
  "`r' merges overlapping selections before replacing (normal.cc:486).
The live list ends MERGED — one selection over the union span — exactly as
Kakoune's `sels.merge_overlapping()' leaves it."
  (kao-edit-tests--with "abcdef" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 4)
                                 (kao-sel-make :anchor 2 :cursor 6))
                     :main 0))
    (kao--replace-char-with ?x)
    (should (string= (buffer-string) "xxxxxx"))
    (should (equal (kao-edit-tests--pairs) '((1 . 6))))))

(ert-deftest kao-edit-multi-paste-one-undo-unit ()
  "A multi-selection paste is a single undo unit."
  (kao-edit-tests--with "ab" 1
    (undo-boundary)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 2 :cursor 2))
                     :main 0))
    (kao-register-set kao-register-default '("X" "Y"))
    (kao-paste-after)
    (should (string= (buffer-string) "aXbY"))
    (primitive-undo 1 buffer-undo-list)
    (should (string= (buffer-string) "ab"))))

;;;; Counted Np/NP (repeated<paste>, normal.cc:2283/2493-2494)

(ert-deftest kao-edit-counted-paste-after-thrice ()
  "`3p' repeats the paste, each pass appending after the prior grown span."
  (kao-edit-tests--with "abc" 2                  ; cursor on 'b'
    (kao-register-set kao-register-default '("X"))
    (setq kao--count 3)
    (kao-paste-after)
    (should (string= (buffer-string) "abXXXc"))
    (should (= (kao-sel-cursor (kao--main-sel)) 5))))

(ert-deftest kao-edit-counted-paste-before-thrice ()
  "`3P' repeats the before-paste three times."
  (kao-edit-tests--with "abc" 2
    (kao-register-set kao-register-default '("X"))
    (setq kao--count 3)
    (kao-paste-before)
    (should (string= (buffer-string) "aXXXbc"))
    (should (= (kao-sel-cursor (kao--main-sel)) 2))))

(ert-deftest kao-edit-counted-paste-one-undo-unit ()
  "`3p' is a single undo unit (Kakoune's one outer `ScopedEdition')."
  (kao-edit-tests--with "abc" 2
    (undo-boundary)
    (kao-register-set kao-register-default '("X"))
    (setq kao--count 3)
    (kao-paste-after)
    (should (string= (buffer-string) "abXXXc"))
    (primitive-undo 1 buffer-undo-list)
    (should (string= (buffer-string) "abc"))))

(ert-deftest kao-edit-replace-count-agnostic ()
  "`R' ignores the count — NOT `repeated<>' (normal.cc:2497)."
  (kao-edit-tests--with "abc" 2
    (kao-register-set kao-register-default '("X"))
    (setq kao--count 3)
    (kao-replace)
    (should (string= (buffer-string) "aXc"))))     ; replaced once, not thrice

(ert-deftest kao-edit-alt-p-paste-all-count-agnostic ()
  "`<a-p>' ignores the count — `paste_all' is NOT `repeated<>' (normal.cc:2495)."
  (kao-edit-tests--with "ab" 1
    (kao-register-set kao-register-default '("X" "Y"))
    (setq kao--count 3)
    (kao-paste-all-after)
    (should (string= (buffer-string) "aXYb"))))    ; pasted once, not thrice

;;;; Case transforms (~ ` <a-`>)

(ert-deftest kao-edit-upcase-selection ()
  "`~' upper-cases the selected span and leaves the selection in place."
  (kao-edit-tests--with "hello" 1                 ; h1 e2 l3 l4 o5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 4)) :main 0)) ; "ell"
    (kao-upcase)
    (should (string= (buffer-string) "hELLo"))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) 2))
      (should (= (kao-sel-cursor s) 4)))))

(ert-deftest kao-edit-downcase-selection ()
  "\\=` lower-cases the selected span."
  (kao-edit-tests--with "HELLO" 1                 ; H1 E2 L3 L4 O5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 4)) :main 0)) ; "ELL"
    (kao-downcase)
    (should (string= (buffer-string) "HellO"))))

(ert-deftest kao-edit-swapcase-selection ()
  "`<a-`>' swaps the case of each char; non-cased chars are untouched."
  (kao-edit-tests--with "HeL2o" 1                 ; H1 e2 L3 2_4 o5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 5)) :main 0)) ; whole
    (kao-swapcase)
    (should (string= (buffer-string) "hEl2O"))    ; digit '2' unchanged
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) 1))
      (should (= (kao-sel-cursor s) 5)))))

(ert-deftest kao-edit-case-keys-bound ()
  "~ ` and M-` are bound in the normal-state map."
  (should (eq (lookup-key kao-normal-state-map "~") #'kao-upcase))
  (should (eq (lookup-key kao-normal-state-map "`") #'kao-downcase))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-`")) #'kao-swapcase)))

;;;; Replace char (r)

(ert-deftest kao-edit-replace-char-core ()
  "`kao--replace-char-with' fills the selection with the char, span preserved."
  (kao-edit-tests--with "hello" 1                 ; h1 e2 l3 l4 o5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 4)) :main 0)) ; "ell"
    (kao--replace-char-with ?x)
    (should (string= (buffer-string) "hxxxo"))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) 2))
      (should (= (kao-sel-cursor s) 4)))))

(ert-deftest kao-edit-replace-char-single ()
  "`r' on a one-char selection replaces just that char."
  (kao-edit-tests--with "abc" 2                   ; cursor on 'b'
    (let ((unread-command-events (list ?Z)))
      (kao-replace-char))
    (should (string= (buffer-string) "aZc"))))

(ert-deftest kao-edit-replace-char-escape-aborts ()
  "Escape during `r' leaves the buffer untouched."
  (kao-edit-tests--with "abc" 2
    (let ((unread-command-events (list ?\e)))
      (kao-replace-char))
    (should (string= (buffer-string) "abc"))))

(ert-deftest kao-edit-replace-key-bound ()
  "`r' is bound in the normal-state map."
  (should (eq (lookup-key kao-normal-state-map "r") #'kao-replace-char)))

;;;; Indent / deindent (> <)

(defun kao-edit-tests--span (anchor cursor)
  "Set `kao--sels' to a single selection spanning ANCHOR..CURSOR."
  (setq kao--sels (kao-sels-make
                   :list (list (kao-sel-make :anchor anchor :cursor cursor))
                   :main 0)))

(ert-deftest kao-edit-indent-single-line ()
  "`>' inserts `kao-indent-width' spaces at the start of the line."
  (kao-edit-tests--with "abc" 1
    (kao-edit-tests--span 1 3)
    (let ((kao-indent-width 4)) (kao-indent))
    (should (string= (buffer-string) "    abc"))))

(ert-deftest kao-edit-indent-multiline-skips-empty ()
  "`>' indents each non-empty line in range; a blank line is left untouched."
  (kao-edit-tests--with "ab\n\ncd" 1              ; a1 b2 \n3 \n4 c5 d6
    (kao-edit-tests--span 1 6)
    (let ((kao-indent-width 4)) (kao-indent))
    (should (string= (buffer-string) "    ab\n\n    cd"))))

(ert-deftest kao-edit-indent-count-multiplies ()
  "A count multiplies the indent width: `2>' inserts 8 spaces."
  (kao-edit-tests--with "abc" 1
    (kao-edit-tests--span 1 3)
    (let ((kao-indent-width 4))
      (setq kao--count 2)
      (kao-indent))
    (should (string= (buffer-string) "        abc"))))

(ert-deftest kao-edit-deindent-spaces ()
  "`<' removes a full indent-width of leading spaces."
  (kao-edit-tests--with "    abc" 5               ; 4 spaces then abc
    (kao-edit-tests--span 5 7)
    (let ((kao-indent-width 4)) (kao-deindent))
    (should (string= (buffer-string) "abc"))))

(ert-deftest kao-edit-deindent-tab ()
  "`<' removes a leading tab (tab advances width to the next `tab-width' stop)."
  (kao-edit-tests--with "\tabc" 2
    (kao-edit-tests--span 2 4)
    (let ((kao-indent-width 4) (tab-width 8)) (kao-deindent))
    (should (string= (buffer-string) "abc"))))

(ert-deftest kao-edit-deindent-incomplete ()
  "`<' removes an incomplete leading run (fewer than width spaces) entirely."
  (kao-edit-tests--with "  abc" 3                 ; 2 spaces then abc
    (kao-edit-tests--span 3 5)
    (let ((kao-indent-width 4)) (kao-deindent))
    (should (string= (buffer-string) "abc"))))

(ert-deftest kao-edit-deindent-count-multiplies ()
  "A count multiplies the removal width: `2<' removes 8 spaces."
  (kao-edit-tests--with "        abc" 9           ; 8 spaces then abc
    (kao-edit-tests--span 9 11)
    (let ((kao-indent-width 4))
      (setq kao--count 2)
      (kao-deindent))
    (should (string= (buffer-string) "abc"))))

(ert-deftest kao-edit-indent-keys-bound ()
  "> and < are bound in the normal-state map."
  (should (eq (lookup-key kao-normal-state-map ">") #'kao-indent))
  (should (eq (lookup-key kao-normal-state-map "<") #'kao-deindent)))

(ert-deftest kao-edit-indent-empty-includes-blank-lines ()
  "`<a->>' indents every line in range, including the blank one."
  (kao-edit-tests--with "ab\n\ncd" 1              ; a1 b2 \n3 \n4 c5 d6
    (kao-edit-tests--span 1 6)
    (let ((kao-indent-width 4)) (kao-indent-empty))
    (should (string= (buffer-string) "    ab\n    \n    cd"))))

(ert-deftest kao-edit-indent-multi-selection ()
  "`>' indents the line of each selection; selections shift with the text."
  (kao-edit-tests--with "ab\ncd\nef" 1            ; a1 b2 \n3 c4 d5 \n6 e7 f8
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 7 :cursor 7))
                     :main 0))
    (let ((kao-indent-width 2)) (kao-indent))
    (should (string= (buffer-string) "  ab\ncd\n  ef"))  ; line 2 untouched
    (should (equal (kao-edit-tests--pairs) '((3 . 3) (11 . 11))))))

(ert-deftest kao-edit-indent-last-line-dedup ()
  "Two selections on the same line indent it once (Kakoune `last_line' dedup)."
  (kao-edit-tests--with "abcd" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 3 :cursor 3))
                     :main 0))
    (let ((kao-indent-width 4)) (kao-indent))
    (should (string= (buffer-string) "    abcd"))))   ; 4 spaces, not 8

(ert-deftest kao-edit-deindent-keep-incomplete-leaves-partial ()
  "`<a-<>' leaves an incomplete leading indent (fewer than width) intact."
  (kao-edit-tests--with "  abc" 3                 ; 2 spaces < width 4
    (kao-edit-tests--span 3 5)
    (let ((kao-indent-width 4)) (kao-deindent-keep-incomplete))
    (should (string= (buffer-string) "  abc"))))

(ert-deftest kao-edit-deindent-keep-incomplete-removes-full ()
  "`<a-<>' still removes a leading run that reaches the full indent width."
  (kao-edit-tests--with "    abc" 5               ; 4 spaces = width 4
    (kao-edit-tests--span 5 7)
    (let ((kao-indent-width 4)) (kao-deindent-keep-incomplete))
    (should (string= (buffer-string) "abc"))))

(ert-deftest kao-edit-indent-variant-keys-bound ()
  "<a->> and <a-<> are bound to the flag variants."
  (should (eq (lookup-key kao-normal-state-map (kbd "M->")) #'kao-indent-empty))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-<"))
              #'kao-deindent-keep-incomplete)))

;;;; Convert tabs <-> spaces (@ / <a-@>)

(ert-deftest kao-edit-tabs-to-spaces-leading ()
  "`@' converts a leading tab to spaces up to the next tabstop."
  (kao-edit-tests--with "\tab" 1                  ; \t1 a2 b3
    (kao-edit-tests--span 1 3)
    (let ((tab-width 4)) (kao-tabs-to-spaces))
    (should (string= (buffer-string) "    ab"))))

(ert-deftest kao-edit-tabs-to-spaces-midline ()
  "`@' pads a tab to the next tabstop based on its column."
  (kao-edit-tests--with "ab\tc" 1                 ; a1 b2 \t3 c4
    (kao-edit-tests--span 1 4)
    (let ((tab-width 4)) (kao-tabs-to-spaces))
    (should (string= (buffer-string) "ab  c"))))  ; tab at col 2 -> 2 spaces

(ert-deftest kao-edit-tabs-to-spaces-count-tabstop ()
  "A count overrides the rounding tabstop for `@'."
  (kao-edit-tests--with "\tab" 1
    (kao-edit-tests--span 1 3)
    (let ((tab-width 8)) (setq kao--count 2) (kao-tabs-to-spaces))
    (should (string= (buffer-string) "  ab"))))   ; col 0, tabstop 2 -> 2 spaces

(ert-deftest kao-edit-tabs-to-spaces-multi-sel ()
  "`@' converts the tab in every selection."
  (kao-edit-tests--with "\ta\n\tb" 1              ; \t1 a2 \n3 \t4 b5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)
                                 (kao-sel-make :anchor 4 :cursor 5))
                     :main 0))
    (let ((tab-width 2)) (kao-tabs-to-spaces))
    (should (string= (buffer-string) "  a\n  b"))))

(ert-deftest kao-edit-tabs-to-spaces-cjk-display-width ()
  "`@' measures the tab's column in DISPLAY width.
A CJK char before the tab is two columns wide, so \"漢\\tx\" at tab-width 8 has
the tab at column 2 and pads to 6 spaces — not 7, which a codepoint count gives."
  (kao-edit-tests--with "漢\tx" 1                 ; 漢1 \t2 x3
    (kao-edit-tests--span 1 3)
    (let ((tab-width 8)) (kao-tabs-to-spaces))
    (should (string= (buffer-string) "漢      x"))))   ; 漢 + 6 spaces + x

(ert-deftest kao-edit-spaces-to-tabs-boundary ()
  "`<a-@>' collapses a tabstop-aligned run of spaces into a tab."
  (kao-edit-tests--with "    ab" 1                ; 4 spaces, a5 b6
    (kao-edit-tests--span 1 6)
    (let ((tab-width 4)) (kao-spaces-to-tabs))
    (should (string= (buffer-string) "\tab"))))

(ert-deftest kao-edit-spaces-to-tabs-absorb-tab ()
  "`<a-@>' absorbs a partial space run that ends on a literal tab."
  (kao-edit-tests--with "  \tab" 1                ; sp1 sp2 \t3 a4 b5
    (kao-edit-tests--span 1 5)
    (let ((tab-width 4)) (kao-spaces-to-tabs))
    (should (string= (buffer-string) "\tab"))))

(ert-deftest kao-edit-spaces-to-tabs-partial-kept ()
  "`<a-@>' leaves a partial space run (not aligned, no following tab) intact."
  (kao-edit-tests--with "  ab" 1                  ; sp1 sp2 a3 b4
    (kao-edit-tests--span 1 4)
    (let ((tab-width 4)) (kao-spaces-to-tabs))
    (should (string= (buffer-string) "  ab"))))

(ert-deftest kao-edit-tabs-spaces-keys-bound ()
  "@ and <a-@> are bound in the normal-state map."
  (should (eq (lookup-key kao-normal-state-map "@") #'kao-tabs-to-spaces))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-@"))
              #'kao-spaces-to-tabs)))

;;;; Align (&) / copy indent (<a-&>)

(ert-deftest kao-edit-align-single-column ()
  "`&' pads each cursor in a column out to the widest cursor."
  (kao-edit-tests--with "a x\nbb x" 1            ; a1 sp2 x3 \n4 b5 b6 sp7 x8
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 3 :cursor 3)
                                 (kao-sel-make :anchor 8 :cursor 8))
                     :main 0))
    (let ((tab-width 8)) (kao-align))
    (should (string= (buffer-string) "a  x\nbb x"))))

(ert-deftest kao-edit-align-two-columns ()
  "`&' aligns successive columns; a later column sees the earlier inserts.
Column 0 pads `a' out to `c''s column, which shifts `b' right; column 1 then
pads the shifted `b' to `d''s column — a naive precompute would over-pad."
  (kao-edit-tests--with "ab\nxcyd" 1             ; a1 b2 \n3 x4 c5 y6 d7
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)   ; a, col 0
                                 (kao-sel-make :anchor 2 :cursor 2)   ; b, col 1
                                 (kao-sel-make :anchor 5 :cursor 5)   ; c, col 0
                                 (kao-sel-make :anchor 7 :cursor 7))  ; d, col 1
                     :main 0))
    (let ((tab-width 8)) (kao-align))
    (should (string= (buffer-string) " a b\nxcyd"))))

(ert-deftest kao-edit-align-multiline-aborts ()
  "`&' is a no-op (with a message) when a selection spans multiple lines."
  (kao-edit-tests--with "ab\ncd" 1               ; a1 b2 \n3 c4 d5
    (kao-edit-tests--span 1 4)                    ; line1..line2
    (kao-align)
    (should (string= (buffer-string) "ab\ncd"))))

(ert-deftest kao-edit-align-with-tabs ()
  "`&' pads with tabs when `kao-align-tab' is set."
  (kao-edit-tests--with "x\nyyyyx" 1             ; x1 \n2 y3 y4 y5 y6 x7
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 7 :cursor 7))
                     :main 0))
    (let ((tab-width 2) (kao-align-tab t)) (kao-align))
    (should (string= (buffer-string) "\t\tx\nyyyyx"))))

(ert-deftest kao-edit-align-cjk-display-width ()
  "`&' aligns to VISUAL columns, so a CJK cell pads the shorter row correctly
\.  The `=' after \"漢字\" sits at display column 4;
the `=' after \"ab\" at column 2, so it is padded out by two spaces."
  (kao-edit-tests--with "漢字=1\nab=2" 1          ; 漢1 字2 =3 14 \n5 a6 b7 =8 29
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 3 :cursor 3)   ; = on line 1
                                 (kao-sel-make :anchor 8 :cursor 8))  ; = on line 2
                     :main 0))
    (let ((tab-width 8)) (kao-align))
    (should (string= (buffer-string) "漢字=1\nab  =2"))))

(ert-deftest kao-edit-align-cjk-already-aligned-noop ()
  "`&' leaves a CJK table already aligned in display width unchanged
\.  Both `='s sit at display column 2; a codepoint
count would see column 1 vs 2 and wrongly pad the first row."
  (kao-edit-tests--with "漢=1\nab=2" 1            ; 漢1 =2 13 \n4 a5 b6 =7 28
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)   ; = on line 1
                                 (kao-sel-make :anchor 7 :cursor 7))  ; = on line 2
                     :main 0))
    (let ((tab-width 8)) (kao-align))
    (should (string= (buffer-string) "漢=1\nab=2"))))

(ert-deftest kao-edit-copy-indent-from-main ()
  "`<a-&>' copies the main selection's indent to every other spanned line."
  (kao-edit-tests--with "    a\nb\n  c" 1
    (kao-edit-tests--span 1 11)                   ; spans all 3 lines, main = ref
    (kao-copy-indent)
    (should (string= (buffer-string) "    a\n    b\n    c"))))

(ert-deftest kao-edit-copy-indent-count-selects-ref ()
  "A count selects the reference selection for `<a-&>' (1-based)."
  (kao-edit-tests--with "  a\n    b\nc" 1         ; selection 2 (line 2) has 4-sp indent
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 3 :cursor 3)
                                 (kao-sel-make :anchor 9 :cursor 9)
                                 (kao-sel-make :anchor 11 :cursor 11))
                     :main 0))
    (setq kao--count 2)
    (kao-copy-indent)
    (should (string= (buffer-string) "    a\n    b\n    c"))))

(ert-deftest kao-edit-copy-indent-bad-count-aborts ()
  "`<a-&>' with a count past the selection count is a no-op (with a message)."
  (kao-edit-tests--with "  a\nb" 1
    (kao-edit-tests--span 1 4)                    ; one selection
    (setq kao--count 2)
    (kao-copy-indent)
    (should (string= (buffer-string) "  a\nb"))))

(ert-deftest kao-edit-align-copy-indent-keys-bound ()
  "& and <a-&> are bound in the normal-state map."
  (should (eq (lookup-key kao-normal-state-map "&") #'kao-align))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-&")) #'kao-copy-indent)))

;;;; <a-)> / <a-(> — rotate selections content

(ert-deftest kao-edit-rotate-content-forward ()
  "`<a-)>' rotates the selections' text forward; main follows its content."
  (kao-edit-tests--with "abc" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 2 :cursor 2)
                                 (kao-sel-make :anchor 3 :cursor 3))
                     :main 0))
    (kao-rotate-content-forward)
    (should (string= (buffer-string) "cab"))
    (should (equal (kao-edit-tests--pairs) '((1 . 1) (2 . 2) (3 . 3))))
    (should (= 1 (kao-sels-main kao--sels)))))     ; 'a' moved to index 1

(ert-deftest kao-edit-rotate-content-backward ()
  "`<a-(>' rotates the selections' text backward."
  (kao-edit-tests--with "abc" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 2 :cursor 2)
                                 (kao-sel-make :anchor 3 :cursor 3))
                     :main 0))
    (kao-rotate-content-backward)
    (should (string= (buffer-string) "bca"))
    (should (= 2 (kao-sels-main kao--sels)))))     ; 'a' moved to index 2

(ert-deftest kao-edit-rotate-content-count-groups ()
  "A count groups the selections and rotates each group independently."
  (kao-edit-tests--with "abcd" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 2 :cursor 2)
                                 (kao-sel-make :anchor 3 :cursor 3)
                                 (kao-sel-make :anchor 4 :cursor 4))
                     :main 0))
    (setq kao--count 2)
    (kao-rotate-content-forward)
    (should (string= (buffer-string) "badc"))      ; [a b]->[b a], [c d]->[d c]
    (should (= 1 (kao-sels-main kao--sels)))))

(ert-deftest kao-edit-rotate-content-different-lengths ()
  "`<a-)>' rotates texts of different lengths, positions tracked by markers."
  (kao-edit-tests--with "ab.c" 1                 ; a1 b2 .3 c4
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)   ; "ab"
                                 (kao-sel-make :anchor 4 :cursor 4))  ; "c"
                     :main 0))
    (kao-rotate-content-forward)
    (should (string= (buffer-string) "c.ab"))
    (should (equal (kao-edit-tests--pairs) '((1 . 1) (3 . 4))))
    (should (= 1 (kao-sels-main kao--sels)))))

(ert-deftest kao-edit-rotate-content-preserves-direction ()
  "`<a-)>' keeps each selection's direction after replacing its text."
  (kao-edit-tests--with "abcd" 1                 ; a1 b2 c3 d4
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 1)   ; backward "ab"
                                 (kao-sel-make :anchor 3 :cursor 4))  ; forward "cd"
                     :main 0))
    (kao-rotate-content-forward)
    (should (string= (buffer-string) "cdab"))
    (should (equal (kao-edit-tests--pairs) '((2 . 1) (3 . 4))))
    (should (= 1 (kao-sels-main kao--sels)))))

(ert-deftest kao-edit-rotate-content-keys-bound ()
  "<a-)> and <a-(> are bound in the normal-state map."
  (should (eq #'kao-rotate-content-forward
              (lookup-key kao-normal-state-map (kbd "M-)"))))
  (should (eq #'kao-rotate-content-backward
              (lookup-key kao-normal-state-map (kbd "M-(")))))

;;;; <a-d> / <a-c> — delete / change without yanking

(ert-deftest kao-edit-alt-d-deletes-without-yanking ()
  "`<a-d>' deletes the selection but leaves the register untouched."
  (kao-edit-tests--with "abcdef" 2          ; cursor on 'b'
    (kao-register-set kao-register-default '("SENTINEL"))
    (kao-delete-no-yank)
    (should (string= (buffer-string) "acdef"))
    (should (equal (kao-register-get kao-register-default) '("SENTINEL")))))

(ert-deftest kao-edit-d-yanks-then-deletes ()
  "`d' (contrast) overwrites the register with the deleted text."
  (kao-edit-tests--with "abcdef" 2
    (kao-register-set kao-register-default '("SENTINEL"))
    (kao-delete)
    (should (string= (buffer-string) "acdef"))
    (should (equal (kao-register-get kao-register-default) '("b")))))

(ert-deftest kao-edit-alt-d-multi-selection ()
  "`<a-d>' deletes every selection, still without yanking."
  (kao-edit-tests--with "abcdef" 1
    (kao-register-set kao-register-default '("SENTINEL"))
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)    ; 'b'
                                 (kao-sel-make :anchor 4 :cursor 4))   ; 'd'
                     :main 0))
    (kao-delete-no-yank)
    (should (string= (buffer-string) "acef"))
    (should (equal (kao-register-get kao-register-default) '("SENTINEL")))))

(ert-deftest kao-edit-alt-c-changes-without-yanking ()
  "`<a-c>' deletes + enters insert without yanking; typed text lands at the gap."
  (kao-edit-tests--with "abcdef" 2
    (kao-register-set kao-register-default '("SENTINEL"))
    (kao-change-no-yank)
    (should kao--insert-active)
    (should (equal (kao-register-get kao-register-default) '("SENTINEL")))
    (insert "X")
    (kao-insert-exit)
    (should (string= (buffer-string) "aXcdef"))))

(ert-deftest kao-edit-noyank-bindings-present ()
  "`<a-d>'/`<a-c>' are bound in normal state."
  (should (eq (lookup-key kao-normal-state-map (kbd "M-d")) #'kao-delete-no-yank))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-c")) #'kao-change-no-yank)))

;;;; <a-o> / <a-O> — add empty line below / above (no insert)

(ert-deftest kao-edit-alt-o-adds-line-below ()
  "`<a-o>' opens a blank line below the line; selection unchanged, no insert."
  (kao-edit-tests--with "abc\ndef" 2          ; cursor on 'b' (line 1)
    (kao-add-line-below)
    (should (string= (buffer-string) "abc\n\ndef"))
    (should-not kao--insert-active)
    (should (= (char-after (kao-sel-cursor (kao--main-sel))) ?b))))

(ert-deftest kao-edit-alt-O-adds-line-above ()
  "`<a-O>' opens a blank line above; the selection shifts down with its text."
  (kao-edit-tests--with "abc\ndef" 6          ; cursor on 'e' (line 2)
    (kao-add-line-above)
    (should (string= (buffer-string) "abc\n\ndef"))
    (should-not kao--insert-active)
    (should (= (char-after (kao-sel-cursor (kao--main-sel))) ?e))))

(ert-deftest kao-edit-alt-o-count-adds-n-lines ()
  "`2<a-o>' opens two blank lines below."
  (kao-edit-tests--with "abc\ndef" 2
    (setq kao--count 2)
    (kao-add-line-below)
    (should (string= (buffer-string) "abc\n\n\ndef"))))

(ert-deftest kao-edit-alt-o-multi-selection ()
  "`<a-o>' opens a blank line below every selection's line; both preserved."
  (kao-edit-tests--with "abc\ndef\nghi" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)    ; 'b' line 1
                                 (kao-sel-make :anchor 6 :cursor 6))   ; 'e' line 2
                     :main 0))
    (kao-add-line-below)
    (should (string= (buffer-string) "abc\n\ndef\n\nghi"))
    (let ((lst (kao-sels-list kao--sels)))
      (should (= 2 (length lst)))
      (should (= (char-after (kao-sel-cursor (nth 0 lst))) ?b))
      (should (= (char-after (kao-sel-cursor (nth 1 lst))) ?e)))))

(ert-deftest kao-edit-alt-o-whole-line-keeps-text ()
  "`x' then `<a-o>': the whole-line selection still covers exactly its own line.
Regression: a cursor on the line's terminating newline must not be dragged onto
the inserted blank line (the insert lands at `max.line+1', past the newline)."
  (kao-edit-tests--with "l1\nl2\nl3\n" 1
    (kao-line)                              ; x -> selection covers "l1\n" ([1,3])
    (should (string= (buffer-substring (kao-sel-beg (kao--main-sel))
                                       (kao-sel-end (kao--main-sel))) "l1\n"))
    (kao-add-line-below)
    (should (string= (buffer-string) "l1\n\nl2\nl3\n"))
    (should (string= (buffer-substring (kao-sel-beg (kao--main-sel))
                                       (kao-sel-end (kao--main-sel))) "l1\n"))))

(ert-deftest kao-edit-alt-o-multi-whole-line-keeps-text ()
  "`<a-o>' over several whole-line selections keeps each covering its own line."
  (kao-edit-tests--with "l1\nl2\nl3\n" 1
    ;; whole-line selections "l1\n" [1,3] and "l2\n" [4,6]
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3)
                                 (kao-sel-make :anchor 4 :cursor 6))
                     :main 0))
    (kao-add-line-below)
    (should (string= (buffer-string) "l1\n\nl2\n\nl3\n"))
    (let ((lst (kao-sels-list kao--sels)))
      (should (string= (buffer-substring (kao-sel-beg (nth 0 lst))
                                         (kao-sel-end (nth 0 lst))) "l1\n"))
      (should (string= (buffer-substring (kao-sel-beg (nth 1 lst))
                                         (kao-sel-end (nth 1 lst))) "l2\n")))))

(ert-deftest kao-edit-add-line-bindings-present ()
  "`<a-o>'/`<a-O>' are bound in normal state."
  (should (eq (lookup-key kao-normal-state-map (kbd "M-o")) #'kao-add-line-below))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-O")) #'kao-add-line-above)))

;;;; <a-p> / <a-P> — paste_all after / before (select each pasted string)

(ert-deftest kao-edit-alt-p-pastes-all-after-and-selects-each ()
  "`<a-p>' pastes every source string after the selection; one sel per string."
  (kao-edit-tests--with "ab" 1               ; cursor on 'a'
    (kao-register-set kao-register-default '("X" "Y"))
    (kao-paste-all-after)
    (should (string= (buffer-string) "aXYb"))
    (let ((lst (kao-sels-list kao--sels)))
      (should (= 2 (length lst)))
      (should (equal (mapcar #'kao-sel-cursor lst) '(2 3)))   ; on 'X','Y'
      (should (= 1 (kao-sels-main kao--sels))))))             ; main = last

(ert-deftest kao-edit-alt-p-multi-selection ()
  "`<a-p>' over two selections gives M*N = 4 selections."
  (kao-edit-tests--with "abcd" 1
    (kao-register-set kao-register-default '("X" "Y"))
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)   ; 'a'
                                 (kao-sel-make :anchor 3 :cursor 3))  ; 'c'
                     :main 0))
    (kao-paste-all-after)
    (should (string= (buffer-string) "aXYbcXYd"))
    (let ((lst (kao-sels-list kao--sels)))
      (should (= 4 (length lst)))
      (should (equal (mapcar #'kao-sel-cursor lst) '(2 3 6 7)))
      (should (= 3 (kao-sels-main kao--sels))))))

(ert-deftest kao-edit-alt-P-pastes-all-before ()
  "`<a-P>' pastes every source string before the selection."
  (kao-edit-tests--with "ab" 2               ; cursor on 'b'
    (kao-register-set kao-register-default '("X" "Y"))
    (kao-paste-all-before)
    (should (string= (buffer-string) "aXYb"))
    (should (equal (mapcar #'kao-sel-cursor (kao-sels-list kao--sels)) '(2 3)))))

(ert-deftest kao-edit-alt-P-survives-command-boundary ()
  "`<a-P>' selections stay on the pasted text after `post-command-hook'.
Regression pin (2026-07-18): `kao--paste-all' must set
`kao--sels-edit-pending' so the post-command `kao--sels-sync' does not
translate the freshly-installed result selections forward through the
paste a SECOND time.  Without the flag the sels shift off the pasted
text at the command boundary — invisible to a test that never fires the
hook.  This test drives the real boundary."
  (kao-edit-tests--with "ab" 2               ; cursor on 'b'
    (kao-register-set kao-register-default '("X" "Y"))
    (kao-paste-all-before)
    (should (string= (buffer-string) "aXYb"))
    (should (equal (mapcar #'kao-sel-cursor (kao-sels-list kao--sels)) '(2 3)))
    (run-hooks 'post-command-hook)           ; the seam the bug corrupts
    (should (string= (buffer-string) "aXYb"))
    (should (equal (mapcar #'kao-sel-cursor (kao-sels-list kao--sels)) '(2 3)))))

(ert-deftest kao-edit-alt-p-single-string ()
  "`<a-p>' with one source string pastes it once per original selection."
  (kao-edit-tests--with "ab" 1
    (kao-register-set kao-register-default '("Z"))
    (kao-paste-all-after)
    (should (string= (buffer-string) "aZb"))
    (let ((lst (kao-sels-list kao--sels)))
      (should (= 1 (length lst)))
      (should (= (kao-sel-cursor (nth 0 lst)) 2)))))

(ert-deftest kao-edit-alt-p-linewise ()
  "`<a-p>' linewise pastes whole lines below; each sel covers one pasted line."
  (kao-edit-tests--with "abc\ndef" 1         ; cursor on line 1
    (kao-register-set kao-register-default '("L1\n" "L2\n"))
    (kao-paste-all-after)
    (should (string= (buffer-string) "abc\nL1\nL2\ndef"))
    (let ((lst (kao-sels-list kao--sels)))
      (should (= 2 (length lst)))
      (should (string= (buffer-substring (kao-sel-beg (nth 0 lst))
                                         (kao-sel-end (nth 0 lst))) "L1\n"))
      (should (string= (buffer-substring (kao-sel-beg (nth 1 lst))
                                         (kao-sel-end (nth 1 lst))) "L2\n")))))

(ert-deftest kao-edit-alt-p-linewise-last-line-no-newline ()
  "`<a-p>' linewise on a last line with no trailing newline adds one first.
Mirrors `kao-edit-p-linewise-last-line-no-newline' for the second copy of the
paste positioning cond (`kao--paste-all'), pinning its eob insert-\"\\n\" branch."
  (kao-edit-tests--with "foo\nbar" 5
    (kao-register-set kao-register-default '("L1\n"))
    (kao-paste-all-after)
    (should (string= (buffer-string) "foo\nbar\nL1\n"))))

(ert-deftest kao-edit-alt-p-empty-source-noop ()
  "`<a-p>' with an empty source leaves the buffer unchanged."
  (kao-edit-tests--with "ab" 1
    (kao-register-set kao-register-default nil)
    (kao-paste-all-after)
    (should (string= (buffer-string) "ab"))))

(ert-deftest kao-edit-paste-all-after-before-bindings-present ()
  "`<a-p>'/`<a-P>' are bound in normal state."
  (should (eq (lookup-key kao-normal-state-map (kbd "M-p")) #'kao-paste-all-after))
  (should (eq (lookup-key kao-normal-state-map (kbd "M-P")) #'kao-paste-all-before)))

;;;; <a-R> — paste_all replace (replace selection with every source string)

(ert-deftest kao-edit-alt-R-replace-all ()
  "`<a-R>' replaces the selection with every source string; one sel per string."
  (kao-edit-tests--with "abc" 2               ; cursor on 'b'
    (kao-register-set kao-register-default '("X" "Y"))
    (kao-replace-all)
    (should (string= (buffer-string) "aXYc"))
    (let ((lst (kao-sels-list kao--sels)))
      (should (= 2 (length lst)))
      (should (equal (mapcar #'kao-sel-cursor lst) '(2 3)))
      (should (= 1 (kao-sels-main kao--sels))))))

(ert-deftest kao-edit-alt-R-replace-all-multi ()
  "`<a-R>' over two selections replaces each with every string (M*N = 4)."
  (kao-edit-tests--with "abcd" 1
    (kao-register-set kao-register-default '("X" "Y"))
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)   ; 'a'
                                 (kao-sel-make :anchor 3 :cursor 3))  ; 'c'
                     :main 0))
    (kao-replace-all)
    (should (string= (buffer-string) "XYbXYd"))
    (let ((lst (kao-sels-list kao--sels)))
      (should (= 4 (length lst)))
      (should (equal (mapcar #'kao-sel-cursor lst) '(1 2 4 5)))
      (should (= 3 (kao-sels-main kao--sels))))))

(ert-deftest kao-edit-alt-R-replace-all-linewise ()
  "`<a-R>' over a whole-line selection replaces it with the pasted lines."
  (kao-edit-tests--with "abc\ndef" 1
    (setq kao--sels (kao-sels-make            ; whole line "abc\n" ([1,4], cursor on \n)
                     :list (list (kao-sel-make :anchor 1 :cursor 4))
                     :main 0))
    (kao-register-set kao-register-default '("L1\n" "L2\n"))
    (kao-replace-all)
    (should (string= (buffer-string) "L1\nL2\ndef"))
    (let ((lst (kao-sels-list kao--sels)))
      (should (= 2 (length lst)))
      (should (string= (buffer-substring (kao-sel-beg (nth 0 lst))
                                         (kao-sel-end (nth 0 lst))) "L1\n"))
      (should (string= (buffer-substring (kao-sel-beg (nth 1 lst))
                                         (kao-sel-end (nth 1 lst))) "L2\n")))))

(ert-deftest kao-edit-replace-all-binding-present ()
  "`<a-R>' is bound in normal state."
  (should (eq (lookup-key kao-normal-state-map (kbd "M-R")) #'kao-replace-all)))

;;;; Undo / redo (u / U) — history tree
;; NOTE: `u'/`U' navigate kao's own history tree (kao-history.el), not native
;; undo.  Each test calls `kao--hist-maybe-commit' after a kao edit to stand in
;; for the command-loop boundary that commits one node per command in live use
;; (the post-command-hook does not fire in batch); an insert session commits
;; itself on `kao-insert-exit'.  Chaining is intrinsic — each `kao-undo' walks
;; one node toward the root, so repeated presses need no `last-command' juggling.

(ert-deftest kao-edit-undo-restores-deleted-text ()
  "`u' undoes a deletion and selects the restored text."
  (kao-edit-tests--with "hello world" 7      ; cursor on 'w'
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 7 :cursor 11))  ; "world"
                     :main 0))
    (kao-delete)
    (should (string= (buffer-string) "hello "))
    (kao--hist-maybe-commit)
    (kao-undo)
    (should (string= (buffer-string) "hello world"))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) 7))      ; selection covers restored "world"
      (should (= (kao-sel-cursor s) 11)))))

(ert-deftest kao-edit-undo-cursor-on-last-restored-char ()
  "Undo's restored-text selection puts the cursor on the LAST char ([beg, end-1]).
Mid-buffer so the eob clamp can't mask an `end' vs `end-1' off-by-one."
  (kao-edit-tests--with "abcXYZdef" 4         ; a1 b2 c3 X4 Y5 Z6 d7 e8 f9
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 4 :cursor 6))  ; "XYZ"
                     :main 0))
    (kao-delete)
    (should (string= (buffer-string) "abcdef"))
    (kao--hist-maybe-commit)
    (kao-undo)
    (should (string= (buffer-string) "abcXYZdef"))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) 4))
      (should (= (kao-sel-cursor s) 6)))))     ; on 'Z' (end-1=6), NOT 'd' (end=7)

(ert-deftest kao-edit-undo-removes-inserted-text ()
  "`u' undoes an insertion and collapses the selection to the gap."
  (kao-edit-tests--with "abc" 2              ; cursor on 'b'
    (kao-insert)
    (insert "XY")                            ; "aXYbc"
    (kao-insert-exit)                        ; the insert session commits one node
    (should (string= (buffer-string) "aXYbc"))
    (kao-undo)
    (should (string= (buffer-string) "abc"))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) (kao-sel-cursor s)))   ; collapsed at the gap
      (should (= (kao-sel-cursor s) 2)))))

(ert-deftest kao-edit-redo-reapplies-change ()
  "`U' redoes a change undone by `u'."
  (kao-edit-tests--with "hello world" 7
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 7 :cursor 11)) :main 0))
    (kao-delete)
    (kao--hist-maybe-commit)
    (kao-undo)
    (should (string= (buffer-string) "hello world"))
    (kao-redo)
    (should (string= (buffer-string) "hello "))))

(ert-deftest kao-edit-undo-fires-selection-change-hook ()
  "`u' fires `kao-selection-change-hook' on the undo itself.
The per-command recorder skips `kao-undo'/`kao-redo' wholesale (they are
recorder-inert), so the extended hook contract is honored at the shared history
install seam (`kao--hist-select-ranges').  A buffer-local listener counts the
value changes: `%' -> 1 and `d' -> 2 through the recorder (stood in for here --
`post-command-hook' does not run in batch), then `u' -> 3 ON the undo, not one
command late.  The fire is guarded, so an empty hook stays a clean no-op."
  (kao-edit-tests--with "abcdef" 1
    (let ((changes 0))
      (add-hook 'kao-selection-change-hook (lambda () (cl-incf changes)) nil t)
      (kao-select-whole-buffer)            ; `%'
      (kao--sel-history-record)            ; the recorder fires on the change
      (should (= changes 1))
      (kao-delete)                         ; `d' -> buffer ""
      (kao--hist-maybe-commit)
      (kao--sel-history-record)
      (should (= changes 2))
      (let ((this-command 'kao-undo)) (kao-undo))   ; restores "abcdef"
      (should (string= (buffer-string) "abcdef"))
      (should (= changes 3))               ; fired ON the undo (seam), not late
      ;; Guarded: empty the hook and redo again through the same seam -- the
      ;; guarded fire is a clean no-op (no error, no phantom increment).
      (setq-local kao-selection-change-hook nil)
      (let ((this-command 'kao-redo)) (kao-redo))
      (should (= changes 3)))))

;; Buffer-modified flag follows the history tree (Kakoune `is_modified').
;; replays edits with `buffer-undo-list' bound off, so native undo's
;; auto-unmodify cannot fire; kao drives the flag from the saved history id.
;; A real save is mirrored here by `(set-buffer-modified-p nil)' (what Emacs's
;; save does) + `kao-history-mark-saved' (the `after-save-hook' handler).

(ert-deftest kao-edit-modified-init-root-is-saved-when-clean ()
  "Enabling kao-mode on an unmodified buffer makes the root the saved node."
  (with-temp-buffer
    (insert "abc")
    (set-buffer-modified-p nil)              ; as if freshly loaded from disk
    (kao-mode 1)
    (unwind-protect
        (progn
          (should (equal kao--hist-saved-id 0))
          (should-not (buffer-modified-p)))
      (kao-mode -1))))

(ert-deftest kao-edit-modified-init-no-saved-node-when-dirty ()
  "Enabling kao-mode on a modified buffer leaves no saved node.
The root is NOT a saved state, so the buffer stays modified until a real save
pins a node (`kao--hist-saved-id' nil never `eq's any id)."
  (kao-edit-tests--with "abc" 2
    (should (null kao--hist-saved-id))
    (should (buffer-modified-p))))

(ert-deftest kao-edit-modified-cleared-on-undo-to-saved ()
  "Undo back onto the saved history node clears the buffer-modified flag.
This is the reported bug: edit then undo left the buffer showing modified."
  (kao-edit-tests--with "hello world" 7
    (set-buffer-modified-p nil)              ; saved baseline at the root
    (kao-history-mark-saved)
    (should-not (buffer-modified-p))
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 7 :cursor 11)) :main 0))
    (kao-delete)                             ; "hello " — a new node
    (kao--hist-maybe-commit)
    (should (buffer-modified-p))
    (kao-undo)                               ; back onto the saved node
    (should (string= (buffer-string) "hello world"))
    (should-not (buffer-modified-p))))

(ert-deftest kao-edit-modified-set-when-undo-past-saved ()
  "Undo onto the saved node clears the flag; undo PAST it re-marks modified."
  (kao-edit-tests--with "" 1
    (insert "aaa") (kao--hist-maybe-commit)   ; node 1
    (set-buffer-modified-p nil)
    (kao-history-mark-saved)                  ; saved-id = node 1
    (insert "bbb") (kao--hist-maybe-commit)   ; node 2
    (should (buffer-modified-p))
    (kao-undo)                                ; -> node 1 == saved: clears
    (should (string= (buffer-string) "aaa"))
    (should-not (buffer-modified-p))
    (kao-undo)                                ; -> node 0, past saved: re-marks
    (should (string= (buffer-string) ""))
    (should (buffer-modified-p))))

(ert-deftest kao-edit-modified-cleared-on-redo-to-saved ()
  "Redo back onto the saved node clears the modified flag (same chokepoint)."
  (kao-edit-tests--with "" 1
    (insert "aaa") (kao--hist-maybe-commit)   ; node 1
    (set-buffer-modified-p nil)
    (kao-history-mark-saved)                  ; saved-id = node 1
    (kao-undo)                                ; -> node 0, modified
    (should (buffer-modified-p))
    (kao-redo)                                ; -> node 1 == saved: clears
    (should (string= (buffer-string) "aaa"))
    (should-not (buffer-modified-p))))

(ert-deftest kao-edit-modified-save-mid-insert-commits-pending ()
  "A save mid-insert commits the pending group and pins it (Kakoune `notify_saved').
The saved node carries the on-disk content, so the buffer reads unmodified
right after the save even though an insert session is still open."
  (kao-edit-tests--with "abc" 2
    (set-buffer-modified-p nil)
    (kao-history-mark-saved)                  ; baseline at root
    (kao-insert)
    (insert "XY")                             ; pending, not yet committed
    (should kao--hist-pending)
    ;; A save taken mid-insert: Emacs clears the flag, then `after-save-hook'.
    (set-buffer-modified-p nil)
    (kao-history-mark-saved)
    (should-not kao--hist-pending)            ; committed as a node
    (should-not (buffer-modified-p))          ; saved at that node
    (let ((saved kao--hist-saved-id))
      (kao-insert-exit)                       ; no pending left -> no new node
      (should (equal kao--hist-saved-id saved)))))

(ert-deftest kao-edit-undo-count-undoes-n-groups ()
  "`u' with a count undoes COUNT change groups in one call (Kakoune count)."
  (kao-edit-tests--with "" 1
    (insert "aaa") (kao--hist-maybe-commit)
    (insert "bbb") (kao--hist-maybe-commit)
    (insert "ccc") (kao--hist-maybe-commit)
    (should (string= (buffer-string) "aaabbbccc"))
    (setq kao--count 2)                      ; 2u
    (kao-undo)
    (should (string= (buffer-string) "aaa"))))

(ert-deftest kao-edit-undo-count-selects-all-restored-regions ()
  "A count-undo selects the span from the first to the last restored region.
Two mid-buffer deletions restored by `2u' span [4,9]; this exercises the
min-beg/max-end accumulation across multiple `after-change-functions' calls,
and lands within bounds so no clamp masks the min/max."
  (kao-edit-tests--with "aaabbbccc" 1        ; a1 a2 a3 b4 b5 b6 c7 c8 c9
    (delete-region 7 10) (kao--hist-maybe-commit)  ; delete "ccc"
    (delete-region 4 7) (kao--hist-maybe-commit)   ; delete "bbb" -> "aaa"
    (setq kao--count 2)                      ; 2u restores "bbb" then "ccc"
    (kao-undo)
    (should (string= (buffer-string) "aaabbbccc"))
    ;; The two restored regions [4,7) and [7,10) are ADJACENT, so
    ;; `compute_modified_ranges' merges them (Kakoune's `touches', selection.cc:198)
    ;; — kao's per-range restore folds them the same way (`translate-1' grows the
    ;; first span's exclusive end onto the second, then the overlap merge unions
    ;; them), yielding the one covering selection [4,9].
    (should (= 1 (length (kao-sels-list kao--sels))))
    (let ((s (kao--main-sel)))
      (should (= (kao-sel-anchor s) 4))      ; min-beg of the restored regions
      (should (= (kao-sel-cursor s) 9)))))   ; max-end-1 (last 'c')

(ert-deftest kao-edit-undo-restores-one-selection-per-modified-range ()
  "Undo of a multi-cursor delete restores ONE selection per modified range.
Kakoune's undo calls `compute_modified_ranges' (selection.cc:132) and installs
one selection per modified range via `selections_write_only' (normal.cc:2176-2181),
so a cursor lands back on every edited site — NOT a single covering span.  The
three well-separated \"aaa\" runs stay three selections, the main on the last."
  (kao-edit-tests--with "aaa bbb aaa bbb aaa" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3)     ; first "aaa"
                                 (kao-sel-make :anchor 9 :cursor 11)    ; second "aaa"
                                 (kao-sel-make :anchor 17 :cursor 19))  ; third "aaa"
                     :main 2))
    (kao-delete)
    (should (string= (buffer-string) " bbb  bbb "))
    (kao--hist-maybe-commit)
    (kao-undo)
    (should (string= (buffer-string) "aaa bbb aaa bbb aaa"))
    ;; three restored ranges, NOT the old single (1 . 13) covering span
    (should (equal (kao-edit-tests--pairs) '((1 . 3) (9 . 11) (17 . 19))))
    (should (= (kao-sels-main kao--sels) 2))          ; main on the last range
    (should (= (kao-sel-cursor (kao--main-sel)) 19)))) ; cursor on the last "aaa"

(ert-deftest kao-edit-undo-empty-history-message ()
  "`u' with nothing to undo signals the faithful message, no crash."
  (kao-edit-tests--with "abc" 1              ; initial content not on the undo list
    (let ((e (should-error (kao-undo) :type 'user-error)))
      (should (equal (cadr e) "nothing left to undo")))))

(ert-deftest kao-edit-redo-empty-history-message ()
  "`U' with nothing to redo signals the faithful message, no crash."
  (kao-edit-tests--with "abc" 1
    (let ((e (should-error (kao-redo) :type 'user-error)))
      (should (equal (cadr e) "nothing left to redo")))))

(ert-deftest kao-edit-undo-mid-oneshot-commits-pending-first ()
  "`<a-;> u' commits the typed-so-far text as a node, then undoes it.
Faithful to `Buffer::undo' calling `commit_undo_group()' first
\(buffer.cc:311) — the session splits at the navigation, as in Kakoune."
  (kao-edit-tests--with "hello" 1
    (let ((id0 (kao-history-max-id)))
      (kao-insert)
      (insert "X")                            ; "Xhello", pending uncommitted
      (kao-insert-one-shot)
      (kao-undo)                              ; the one-shot command
      (should (= (kao-history-max-id) (1+ id0))) ; pending committed as a node
      (should (string= (buffer-string) "hello")) ; ...then undone
      (let ((this-command 'kao-undo)) (kao--maybe-reset-count))
      (should kao--insert-active)             ; session continues
      (insert "Y")
      (kao-insert-exit)
      (should (string= (buffer-string) "Yhello"))
      ;; the post-undo typing landed on a sibling branch of the X node
      (should (= (kao-history-max-id) (+ 2 id0))))))

(ert-deftest kao-edit-redo-refuses-while-pending ()
  "`U' refuses while uncommitted modifications exist and commits NOTHING.
`Buffer::redo' returns false when `m_current_undo_group' is non-empty
\(buffer.cc:331-333) — even with a redo child available — so `<a-;> U'
mid-insert errors and the session's single undo group stays intact."
  (kao-edit-tests--with "hello" 1
    (goto-char 1)
    (insert "X") (kao--hist-maybe-commit)     ; "Xhello"
    (kao-undo)                                ; back to "hello", redo child SET
    (let ((id0 (kao-history-max-id)))
      (kao-insert)
      (insert "Z")                            ; "Zhello", pending uncommitted
      (kao-insert-one-shot)
      (let ((e (should-error (kao-redo) :type 'user-error)))
        (should (equal (cadr e) "nothing left to redo")))
      (should (string= (buffer-string) "Zhello")) ; untouched
      (should (= (kao-history-max-id) id0))   ; NOT committed by redo
      ;; the session survives intact: pop back, finish, ONE node commits
      (let ((this-command 'kao-redo)) (kao--maybe-reset-count))
      (should kao--insert-active)
      (kao-insert-exit)
      (should (= (kao-history-max-id) (1+ id0))))))

(ert-deftest kao-edit-move-in-history-commits-pending-first ()
  "`<c-k>' commits pending first, keeping the pre-commit target id
\(buffer.cc:352 — commit AFTER the validation: the range check sees the
pre-commit ids, so the target is computed and validated before the split)."
  (kao-edit-tests--with "hello" 1
    (goto-char 1)
    (insert "A") (kao--hist-maybe-commit)     ; node 1: "Ahello"
    (let ((id0 (kao-history-max-id)))         ; 1
      (kao-insert)
      (insert "X")                            ; "XAhello", pending uncommitted
      (kao-insert-one-shot)
      (kao-history-backward)                  ; <c-k>: target = current(1) - 1
      ;; pending committed (max grew by 1), then navigated to the
      ;; PRE-commit target 0 — reverting both X and A
      (should (= (kao-history-max-id) (1+ id0)))
      (should (string= (buffer-string) "hello")))))

(ert-deftest kao-edit-history-jump-dropped-id-errors ()
  "`<c-k>' to an id dropped by the `kao-history-max-nodes' gc reports the
faithful \"no such change\" — in range (ids are absolute, never renumbered)
but absent.  The validation precedes the read-only barf (buffer.cc:345-350),
so a read-only buffer reports the same, not `buffer-read-only'."
  (kao-edit-tests--with "hello" 1
    (let ((kao-history-max-nodes 2))
      (goto-char 1)
      (insert "A") (kao--hist-maybe-commit)     ; node 1
      (insert "B") (kao--hist-maybe-commit)     ; node 2
      (insert "C") (kao--hist-maybe-commit)     ; node 3 -> gc dropped 0 and 1
      (should (null (kao--hist-node 1)))
      (let ((text (buffer-string)))
        (setq kao--count 2)                     ; target = 3 - 2 = 1 (dropped)
        (should-error (kao--move-in-history -1) :type 'user-error)
        (should (string= (buffer-string) text))
        ;; read-only ordering: still "no such change", not buffer-read-only
        (let ((buffer-read-only t))
          (setq kao--count 2)
          (should-error (kao--move-in-history -1) :type 'user-error))))))

(ert-deftest kao-edit-repeat-insert-mid-session-signals ()
  "`.' from an open insert session (only reachable via `<a-;>') is refused."
  (kao-edit-tests--with "abc" 1
    (kao-insert)
    (insert "X")
    (kao-insert-exit)                         ; record a repeatable insert
    (kao-insert)                              ; open a new session
    (kao-insert-one-shot)
    (should-error (kao-repeat-insert) :type 'user-error)))

(ert-deftest kao-edit-undo-chains-across-presses ()
  "Repeated `u' walks further back in history (not undo-then-redo).
Each `kao-undo' walks one node toward the root, so two presses reach the
grandparent — chaining is intrinsic to the tree (no `this-command'/`last-command'
juggling).  Without it the second `u' would redo the first instead."
  (kao-edit-tests--with "base" 1
    (goto-char (point-max)) (insert "AAA") (kao--hist-maybe-commit)  ; node 1
    (insert "BBB") (kao--hist-maybe-commit)                          ; node 2
    (kao-undo)
    (should (string= (buffer-string) "baseAAA"))   ; back to node 1
    (kao-undo)
    (should (string= (buffer-string) "base"))))    ; chained back to the root

(ert-deftest kao-edit-undo-redo-bindings-present ()
  "`u'/`U' are bound in normal state."
  (should (eq (lookup-key kao-normal-state-map "u") #'kao-undo))
  (should (eq (lookup-key kao-normal-state-map "U") #'kao-redo)))

;;;; History-tree change-id navigation (<c-j> / <c-k>)

(ert-deftest kao-edit-hist-cross-branch-reach ()
  "`<c-k>' reaches a node on an ABANDONED sibling branch — the cross-branch reach
`u'/`U' cannot make (the reason these keys exist)."
  (kao-edit-tests--with "" 1
    (insert "A") (kao--hist-maybe-commit)     ; node 1 (branch A): "A"
    (kao-undo)                                ; back to the root: ""
    (should (string= (buffer-string) ""))
    (insert "B") (kao--hist-maybe-commit)     ; node 2 (branch B, sibling): "B"
    (should (string= (buffer-string) "B"))
    (should (= (kao-history-current-id) 2))
    ;; <c-k> by 1: target id = 2 - 1 = 1, node 1 on the other branch.
    (kao-history-backward)
    (should (string= (buffer-string) "A"))    ; reached the abandoned branch
    (should (= (kao-history-current-id) 1))
    ;; <c-j> by 1: back forward to id 2 (branch B).
    (kao-history-forward)
    (should (string= (buffer-string) "B"))
    (should (= (kao-history-current-id) 2))))

(ert-deftest kao-edit-hist-out-of-range-error ()
  "Navigating past the ends signals Kakoune's `no such change: #N (max)'."
  (kao-edit-tests--with "" 1                  ; root only: current 0, max 0
    (let ((e (should-error (kao-history-forward) :type 'user-error)))
      (should (equal (cadr e) "no such change: #1 (0)")))
    (let ((e (should-error (kao-history-backward) :type 'user-error)))
      (should (equal (cadr e) "no such change: #-1 (0)")))))

(ert-deftest kao-edit-hist-message ()
  "A successful change-id move prints `moved to change #N (max)'.
\(`current-message' is nil in batch, so capture the `message' call directly.)"
  (kao-edit-tests--with "" 1
    (insert "A") (kao--hist-maybe-commit)     ; node 1, max 1
    (kao-undo)                                ; at the root (id 0)
    (let (captured)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq captured (apply #'format fmt args)))))
        (kao-history-forward))               ; to id 1
      (should (equal captured "moved to change #1 (1)")))))

(ert-deftest kao-edit-hist-count-moves-n ()
  "A count moves COUNT change-ids in one press."
  (kao-edit-tests--with "" 1
    (insert "a") (kao--hist-maybe-commit)     ; node 1
    (insert "b") (kao--hist-maybe-commit)     ; node 2
    (insert "c") (kao--hist-maybe-commit)     ; node 3: "abc", current 3
    (setq kao--count 2)                       ; 2<c-k>: target 3 - 2 = 1
    (kao-history-backward)
    (should (string= (buffer-string) "a"))
    (should (= (kao-history-current-id) 1))))

(ert-deftest kao-edit-hist-nav-bindings-present ()
  "`<c-j>'/`<c-k>' are bound to the change-id navigation commands."
  (should (eq (lookup-key kao-normal-state-map (kbd "C-j")) #'kao-history-forward))
  (should (eq (lookup-key kao-normal-state-map (kbd "C-k")) #'kao-history-backward)))

;;;; Join lines (<a-j> / <a-J>)

(defun kao-edit-tests--regions ()
  "Return `kao-join--regions' as a list of (anchor . cursor) pairs."
  (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
          (kao-join--regions)))

(ert-deftest kao-join-regions-single-line ()
  "A single-line selection yields the join-region of its newline with the next line."
  (kao-edit-tests--with "abc\ndef" 2     ; a1 b2 c3 \n4 d5 e6 f7
    (should (equal '((4 . 4)) (kao-edit-tests--regions)))))

(ert-deftest kao-join-regions-multi-line ()
  "A multi-line selection yields a region per newline except the last line's."
  (kao-edit-tests--with "ab\ncd\nef" 1   ; \n at 3 and 6
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 8)) :main 0))
    (should (equal '((3 . 3) (6 . 6)) (kao-edit-tests--regions)))))

(ert-deftest kao-join-regions-skips-leading-blanks ()
  "The region extends over the next line's leading horizontal blanks."
  (kao-edit-tests--with "ab\n   cd" 1     ; \n at 3, spaces 4 5 6, c at 7
    (should (equal '((3 . 6)) (kao-edit-tests--regions)))))

(ert-deftest kao-join-regions-last-line-single-noop ()
  "A single-line selection on the last line joins nothing."
  (kao-edit-tests--with "abc\ndef" 6     ; cursor on 'e' (last line)
    (should (null (kao-join--regions)))))

(ert-deftest kao-join-regions-trailing-newline-phantom-noop ()
  "A buffer-terminal newline is not a real next line — no region."
  (kao-edit-tests--with "abc\n" 2        ; \n at 4 is the final char
    (should (null (kao-join--regions)))))

;; NB: these assert `kao-join--regions' RETURNS (nil) rather than looping forever
;; on a line whose bol is `point-max' (`forward-line' cannot advance).  `with-timeout'
;; does not interrupt a tight loop in batch, so termination is verified by the test
;; suite completing at all — a reintroduced loop would hang `make test'.

(ert-deftest kao-join-regions-empty-buffer-no-hang ()
  "`kao-join--regions' terminates with no region in an empty buffer."
  (with-temp-buffer
    (kao-mode 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0))
    (should (null (kao-join--regions)))
    (kao-mode -1)))

(ert-deftest kao-join-regions-phantom-trailing-line-no-hang ()
  "A selection on Emacs's phantom trailing empty line yields nothing (no hang)."
  (with-temp-buffer
    (insert "a\n")                       ; a1 \n2 ; phantom empty line at pos 3
    (kao-mode 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 3 :cursor 3)) :main 0))
    (should (null (kao-join--regions)))
    (kao-mode -1)))

;; Same termination guard as the join no-hang pins above: `kao--indent-bols'
;; loops `forward-line' over each selection's lines, so a bol at `point-max'
;; (empty buffer / phantom trailing line) would loop forever without the
;; can't-advance break.  Verified by the suite completing at all.

(ert-deftest kao-indent-empty-buffer-no-hang ()
  "`kao-indent' terminates and leaves an empty buffer unchanged (no hang)."
  (with-temp-buffer
    (kao-mode 1)
    (kao-indent)
    (should (string= "" (buffer-string)))
    (kao-mode -1)))

(ert-deftest kao-indent-bols-phantom-trailing-line-no-hang ()
  "A selection on Emacs's phantom trailing line terminates `kao--indent-bols'."
  (with-temp-buffer
    (insert "a\n")                       ; a1 \n2 ; phantom line bol at 3 = point-max
    (kao-mode 1)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 3 :cursor 3)) :main 0))
    (should (equal '(3) (mapcar #'marker-position (kao--indent-bols))))
    (kao-mode -1)))

(ert-deftest kao-join-regions-stops-at-blank-line ()
  "The leading-blank skip stops at the next newline — it does not over-reach."
  (kao-edit-tests--with "ab\n\ncd" 1     ; ab \n(3) \n(4) cd : line 2 is empty
    (should (equal '((3 . 3)) (kao-edit-tests--regions)))))

(ert-deftest kao-join-select-spaces-basic ()
  "`<a-J>' replaces the newline with a space and selects it."
  (kao-edit-tests--with "abc\ndef" 2
    (kao-join-select-spaces)
    (should (string= (buffer-string) "abc def"))
    (should (equal '((4 . 4)) (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                                      (kao-sels-list kao--sels))))
    (should (= 0 (kao-sels-main kao--sels)))))

(ert-deftest kao-join-select-spaces-trims-leading-blanks ()
  "`<a-J>' collapses the newline and the next line's leading blanks to one space."
  (kao-edit-tests--with "ab\n   cd" 1
    (kao-join-select-spaces)
    (should (string= (buffer-string) "ab cd"))
    (should (equal '((3 . 3)) (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                                      (kao-sels-list kao--sels))))))

(ert-deftest kao-join-select-spaces-multi-line-and-main-last ()
  "A multi-line `<a-J>' makes one space per join; main is the last (size-1)."
  (kao-edit-tests--with "a\nb\nc" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 5)) :main 0))
    (kao-join-select-spaces)
    (should (string= (buffer-string) "a b c"))
    (should (equal '((2 . 2) (4 . 4)) (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                                              (kao-sels-list kao--sels))))
    (should (= 1 (kao-sels-main kao--sels)))))

(ert-deftest kao-join-select-spaces-merges-blank-lines ()
  "Consecutive blank-line joins collapse to a single space (`merge_consecutive')."
  (kao-edit-tests--with "a\n\n\nb" 1     ; \n at 2,3,4 ; b at 5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 5)) :main 0))
    (kao-join-select-spaces)
    (should (string= (buffer-string) "a b"))
    (should (equal '((2 . 2)) (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                                      (kao-sels-list kao--sels))))))

(ert-deftest kao-join-select-spaces-last-line-noop ()
  "`<a-J>' on the last line is a no-op (buffer and selection unchanged)."
  (kao-edit-tests--with "abc\ndef" 6
    (kao-join-select-spaces)
    (should (string= (buffer-string) "abc\ndef"))
    (should (equal '((6 . 6)) (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                                      (kao-sels-list kao--sels))))))

(ert-deftest kao-join-select-spaces-readonly-errors ()
  "`<a-J>' refuses to edit a read-only buffer."
  (kao-edit-tests--with "abc\ndef" 2
    (setq buffer-read-only t)
    (should-error (kao-join-select-spaces) :type 'buffer-read-only)
    (setq buffer-read-only nil)))

(ert-deftest kao-join-select-spaces-one-undo ()
  "`<a-J>' is a single undo unit."
  (kao-edit-tests--with "a\nb\nc" 1
    (undo-boundary)
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 5)) :main 0))
    (kao-join-select-spaces)
    (should (string= (buffer-string) "a b c"))
    (primitive-undo 1 buffer-undo-list)
    (should (string= (buffer-string) "a\nb\nc"))))

(ert-deftest kao-join-select-spaces-binding ()
  "`M-J' is bound to join-and-select-spaces."
  (should (eq (lookup-key kao-normal-state-map (kbd "M-J")) #'kao-join-select-spaces)))

(ert-deftest kao-join-lines-keeps-selection ()
  "`<a-j>' joins like `<a-J>' but keeps the original selection."
  (kao-edit-tests--with "abc\ndef" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3)) :main 0))
    (kao-join-lines)
    (should (string= (buffer-string) "abc def"))
    (should (equal '((1 . 3)) (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                                      (kao-sels-list kao--sels))))))

(ert-deftest kao-join-lines-maps-selection-through-edit ()
  "`<a-j>' maps the kept selection through the length-changing edit."
  (kao-edit-tests--with "a\n  b" 1       ; a1 \n2 sp3 sp4 b5
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 5)) :main 0))
    (kao-join-lines)
    (should (string= (buffer-string) "a b"))   ; "\n  " -> " "
    (should (equal '((1 . 3)) (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                                      (kao-sels-list kao--sels))))))   ; cursor 5 -> 3 (on b)

(ert-deftest kao-join-lines-same-buffer-as-select-spaces ()
  "`<a-j>' and `<a-J>' produce the same buffer; only the selections differ."
  (let ((via-j (kao-edit-tests--with "a\nb\nc" 1
                 (setq kao--sels (kao-sels-make
                                  :list (list (kao-sel-make :anchor 1 :cursor 5)) :main 0))
                 (kao-join-lines)
                 (buffer-string)))
        (via-bigj (kao-edit-tests--with "a\nb\nc" 1
                    (setq kao--sels (kao-sels-make
                                     :list (list (kao-sel-make :anchor 1 :cursor 5)) :main 0))
                    (kao-join-select-spaces)
                    (buffer-string))))
    (should (string= via-j "a b c"))
    (should (string= via-j via-bigj))))

(ert-deftest kao-join-lines-last-line-noop ()
  "`<a-j>' on the last line leaves the buffer and selection unchanged."
  (kao-edit-tests--with "abc\ndef" 6
    (kao-join-lines)
    (should (string= (buffer-string) "abc\ndef"))
    (should (equal '((6 . 6)) (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                                      (kao-sels-list kao--sels))))))

(ert-deftest kao-join-lines-readonly-errors ()
  "`<a-j>' refuses to edit a read-only buffer."
  (kao-edit-tests--with "abc\ndef" 2
    (setq buffer-read-only t)
    (should-error (kao-join-lines) :type 'buffer-read-only)
    (setq buffer-read-only nil)))

(ert-deftest kao-join-lines-multi-sel-keeps-each ()
  "`<a-j>' with N selections keeps each, mapped through the edit."
  (kao-edit-tests--with "a\nb\n\nc\nd" 1   ; a1 \n2 b3 \n4 \n5 c6 \n7 d8
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)    ; on line 1 (a)
                                 (kao-sel-make :anchor 6 :cursor 6))   ; on line 4 (c)
                     :main 0))
    (kao-join-lines)
    (should (string= (buffer-string) "a b\n\nc d"))
    (should (equal '((1 . 1) (6 . 6))     ; both kept (newline->space is same length)
                   (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
                           (kao-sels-list kao--sels))))))

(ert-deftest kao-join-lines-binding ()
  "`M-j' is bound to join-lines."
  (should (eq (lookup-key kao-normal-state-map (kbd "M-j")) #'kao-join-lines)))

;;;; Repeat last insert (.) — repeat_last_insert

(ert-deftest kao-edit-repeat-insert-i ()
  "`.' re-inserts the last `i' text before the current selection."
  (kao-edit-tests--with "abc" 1
    (kao-insert) (insert "X") (kao-insert-exit)   ; i X: "Xabc", records i + "X"
    (should (string= (buffer-string) "Xabc"))
    ;; Move the cursor onto 'c' (now at 4) and repeat.
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 4 :cursor 4)) :main 0))
    (kao-repeat-insert)                            ; . -> insert "X" before 'c'
    (should (string= (buffer-string) "XabXc"))
    ;; Shaped `i': the original 'c' span is kept, shifted past "X".
    (should (= (kao-sel-cursor (kao--main-sel)) 5))))

(ert-deftest kao-edit-repeat-insert-a ()
  "`.' re-appends the last `a' text after the current selection."
  (kao-edit-tests--with "abc" 1
    (kao-append) (insert "Z") (kao-insert-exit)    ; a Z: "aZbc", records a + "Z"
    (should (string= (buffer-string) "aZbc"))
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0))
    (kao-repeat-insert)                            ; . -> append "Z" after 'a'@1
    (should (string= (buffer-string) "aZZbc"))))

(ert-deftest kao-edit-repeat-insert-change-reerases-no-yank ()
  "`.' after `c' re-erases the CURRENT selection without yanking, then inserts.
The register keeps its value across the repeat (the yank is not repeated —
`prepare(Replace)' erases, the yank is outside insert mode)."
  (kao-edit-tests--with "foobar" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3)) :main 0)) ; "foo"
    (kao-change) (insert "X") (kao-insert-exit)    ; c X: "Xbar", reg="foo"
    (should (string= (buffer-string) "Xbar"))
    (should (equal (kao-register-get kao-register-default) '("foo")))
    ;; Select "bar" (X1 b2 a3 r4) and overwrite the register with a sentinel.
    (kao-register-set kao-register-default '("SENTINEL"))
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 4)) :main 0))
    (kao-repeat-insert)                            ; . -> erase "bar", insert "X"
    (should (string= (buffer-string) "XX"))
    (should (equal (kao-register-get kao-register-default) '("SENTINEL")))))

(ert-deftest kao-edit-repeat-insert-open-opens-fresh-line ()
  "`.' after `o' opens a fresh line below the current selection and inserts there."
  (kao-edit-tests--with "a\nb" 1
    (kao-open-below) (insert "X") (kao-insert-exit) ; o X: "a\nX\nb", records o + "X"
    (should (string= (buffer-string) "a\nX\nb"))
    ;; Put the cursor on the last line ("b") and repeat.
    (goto-char (point-max))
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor (point) :cursor (point)))
                     :main 0))
    (kao-repeat-insert)                             ; . -> open below "b", insert "X"
    (should (string= (buffer-string) "a\nX\nb\nX"))))

(ert-deftest kao-edit-repeat-insert-multi ()
  "`.' replays the recorded text at EVERY current selection."
  (kao-edit-tests--with "abc\ndef" 1               ; a1 b2 c3 \n4 d5 e6 f7
    (kao-insert) (insert "X") (kao-insert-exit)    ; i X at 'a': "Xabc\ndef"
    (should (string= (buffer-string) "Xabc\ndef"))
    ;; Two selections: on 'b' (now 3) and on 'e' (now 7).
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 3 :cursor 3)
                                 (kao-sel-make :anchor 7 :cursor 7))
                     :main 0))
    (kao-repeat-insert)                            ; . -> "X" before both
    (should (string= (buffer-string) "XaXbc\ndXef"))
    (should (= 2 (length (kao-sels-list kao--sels))))))

(ert-deftest kao-edit-repeat-insert-no-record-noop ()
  "`.' with nothing recorded is a no-op (message); the buffer is unchanged."
  (kao-edit-tests--with "abc" 2
    (setq kao--last-insert-opener nil
          kao--last-insert-text nil)
    (kao-repeat-insert)
    (should (string= (buffer-string) "abc"))))

(ert-deftest kao-edit-repeat-insert-binding ()
  "`.' is bound to `kao-repeat-insert' in normal state."
  (should (eq (lookup-key kao-normal-state-map ".") #'kao-repeat-insert)))

;;;; Read-only buffers (Buffer::throw_if_read_only, buffer.cc:137)

(ert-deftest kao-edit-read-only-insert-entry-errors-before-state-flip ()
  "Insert entry in a read-only buffer signals BEFORE any state flip.
Faithful to the Insert-mode constructor's up-front `throw_if_read_only'
\(input_handler.cc:1189): still normal state, no open change group, no
secondary marks, no insert-start marker."
  (kao-edit-tests--with "abc" 2
    (setq kao--last-insert-opener #'kao-append)   ; the previous session's opener
    (setq buffer-read-only t)
    (should-error (kao-insert) :type 'buffer-read-only)
    (should kao--normal-active)
    (should-not kao--insert-active)
    (should-not kao--insert-undo-handle)
    (should-not kao--insert-start)
    (should-not kao--insert-secondary-sites)
    ;; `.' still replays the PREVIOUS insert: Kakoune sets `m_last_insert'
    ;; only after the ctor's throw (input_handler.cc:1189+).
    (should (eq kao--last-insert-opener #'kao-append))
    (setq buffer-read-only nil)))

(ert-deftest kao-edit-read-only-change-yanks-then-errors ()
  "`c' in a read-only buffer yanks, then the insert entry signals.
Faithful to Kakoune `change': the yank precedes the constructor throw, so the
register IS mutated while the buffer and state are untouched."
  (kao-edit-tests--with "abc" 2
    (kao-register-set ?\" (list "SENTINEL"))
    (setq buffer-read-only t)
    (should-error (kao-change) :type 'buffer-read-only)
    (should (equal '("b") (kao-register-get ?\")))   ; yank happened (faithful)
    (should (string= (buffer-string) "abc"))
    (should kao--normal-active)
    (should-not kao--insert-active)
    (setq buffer-read-only nil)))

(ert-deftest kao-edit-read-only-delete-native-signal-keeps-state ()
  "`d' in a read-only buffer: the native modification signal fires after the
yank (= Kakoune's buffer-primitive throw); kao stays consistent — normal
state, buffer intact, and a subsequent command still works."
  (kao-edit-tests--with "abc" 2
    (setq buffer-read-only t)
    (should-error (kao-delete))
    (should (equal '("b") (kao-register-get ?\")))   ; yank happened (faithful)
    (should (string= (buffer-string) "abc"))
    (should kao--normal-active)
    (setq buffer-read-only nil)
    (kao-delete)                                     ; still fully functional
    (should (string= (buffer-string) "ac"))))

(ert-deftest kao-edit-read-only-undo-redo-error-up-front ()
  "u/U in a read-only buffer signal `buffer-read-only' first.
`Buffer::undo'/`redo' call `throw_if_read_only' BEFORE the at-root check
\(buffer.cc:309/329) — the read-only signal outranks the \"nothing left to
undo\" exhaustion error."
  (kao-edit-tests--with "abc" 2
    (setq buffer-read-only t)
    (should-error (kao-undo) :type 'buffer-read-only)
    (should-error (kao-redo) :type 'buffer-read-only)
    (setq buffer-read-only nil)))

(ert-deftest kao-edit-read-only-history-move-range-checks-first ()
  "<c-j>/<c-k>: the range check precedes the read-only throw.
`Buffer::move_to' returns false for an out-of-range id BEFORE
`throw_if_read_only' (buffer.cc:345-350): out-of-range in a read-only buffer
reports \"no such change\"; only an IN-range move signals `buffer-read-only'."
  (kao-edit-tests--with "abc" 2
    ;; No history yet: both directions are out of range -> "no such change",
    ;; even read-only.
    (setq buffer-read-only t)
    (should-error (kao--move-in-history 1) :type 'user-error)
    (should-error (kao--move-in-history -1) :type 'user-error)
    (setq buffer-read-only nil)
    ;; Make one history node, then lock: the backward move is IN range ->
    ;; the read-only throw fires.
    (kao-insert)
    (insert "X")
    (kao-insert-exit)
    (should (= 1 (kao-history-current-id)))
    (setq buffer-read-only t)
    (should-error (kao--move-in-history -1) :type 'buffer-read-only)
    (setq buffer-read-only nil)))

;;;; Pending register (the `"' prefix)

(ert-deftest kao-edit-yank-to-named-register ()
  "`\"a y' stores to register a; the default `\"' register is untouched.
The status message prints the RAW register char (normal.cc:786-789)."
  (kao-edit-tests--with "alpha beta" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 5)) :main 0))
    (remhash ?a kao--registers)
    (kao-register-set ?\" '("old"))
    (setq kao--pending-register ?A)        ; raw uppercase: storage lowers
    (let (msg)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (kao-yank))
      (should (equal msg "kao: yanked 1 selections to register A")))
    (should (equal (kao-register-get ?a) '("alpha")))
    (should (equal (kao-register-get ?\") '("old")))
    (remhash ?a kao--registers)))

(ert-deftest kao-edit-delete-to-named-register ()
  "`\"a d' writes the deleted text to register a, silently (normal.cc:797)."
  (kao-edit-tests--with "abcdef" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3)) :main 0))
    (remhash ?a kao--registers)
    (setq kao--pending-register ?a)
    (kao-delete)
    (should (equal (kao-register-get ?a) '("abc")))
    (should (equal (buffer-string) "def"))
    (remhash ?a kao--registers)))

(ert-deftest kao-edit-paste-named-register-bypasses-clipboard ()
  "`\"a p' reads register a even when the clipboard changed externally.
The external-clipboard check applies only to the registerless paste."
  (kao-edit-tests--with "x" 1
    (remhash ?a kao--registers)
    (kao-register-set ?a '("REG"))
    (kill-new "EXTERNAL")                  ; an external copy (clipboard-yank nil)
    (should (kao-clipboard-external-p))
    (setq kao--pending-register ?a)
    (kao-paste-after)
    (should (equal (buffer-string) "xREG"))
    (remhash ?a kao--registers)))

(ert-deftest kao-edit-change-to-named-register ()
  "`\"b c' yanks the changed text to register b before opening insert."
  (kao-edit-tests--with "abcdef" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3)) :main 0))
    (remhash ?b kao--registers)
    (setq kao--pending-register ?b)
    (kao-change)
    (kao-insert-exit)
    (should (equal (kao-register-get ?b) '("abc")))
    (should (equal (buffer-string) "def"))
    (remhash ?b kao--registers)))

;;;; Dynamic registers end-to-end

(ert-deftest kao-edit-paste-dot-register-i-th ()
  "`\".p' pastes each selection's own content after it (i-th to i-th)."
  (kao-edit-tests--with "ab cd" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)
                                 (kao-sel-make :anchor 4 :cursor 5))
                     :main 1))
    (setq kao--pending-register ?.)
    (kao-paste-after)
    (should (equal (buffer-string) "abab cdcd"))))

(ert-deftest kao-edit-paste-hash-register-indices ()
  "`\"#p' pastes each selection's 1-based index after it."
  (kao-edit-tests--with "ab cd" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)
                                 (kao-sel-make :anchor 4 :cursor 5))
                     :main 1))
    (setq kao--pending-register ?#)
    (kao-paste-after)
    (should (equal (buffer-string) "ab1 cd2"))))

(ert-deftest kao-edit-paste-percent-register-buffer-name ()
  "`\"%p' pastes the buffer name (a single string repeats to every selection)."
  (kao-edit-tests--with "x" 1
    (setq kao--pending-register ?%)
    (kao-paste-after)
    (should (equal (buffer-string) (concat "x" (buffer-name))))))

(ert-deftest kao-edit-paste-digit-register-after-s ()
  "`s' then `\"1p' pastes each selection's capture group 1 after it."
  (kao-edit-tests--with "ab1 cd2" 1
    (kao-select-whole-buffer)
    (kao--select-regex-apply "\\([a-z]+\\)[0-9]")
    (setq kao--pending-register ?1)
    (kao-paste-after)
    (should (equal (buffer-string) "ab1ab cd2cd"))))

(ert-deftest kao-edit-paste-dot-register-bypasses-clipboard ()
  "An explicit dynamic pending register bypasses the clipboard check."
  (kao-edit-tests--with "ab" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)) :main 0))
    (kill-new "EXTERNAL")                  ; an external copy (clipboard-yank nil)
    (should (kao-clipboard-external-p))
    (setq kao--pending-register ?.)
    (kao-paste-after)
    (should (equal (buffer-string) "abab"))))

(ert-deftest kao-edit-yank-to-percent-errors-before-message ()
  "`\"%y' signals \"this register is not assignable\"; no yank message prints.
The register write precedes the status message (normal.cc:786-789)."
  (kao-edit-tests--with "ab" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)) :main 0))
    (setq kao--pending-register ?%)
    (let (msg)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (setq msg (apply #'format fmt args))))))
        (let ((err (should-error (kao-yank) :type 'user-error)))
          (should (equal (cadr err) "this register is not assignable"))))
      (should (null msg)))))

(ert-deftest kao-edit-yank-to-digit-writes-captures ()
  "`\"1y' writes each selection's text into its captures[1] (the digit setter)."
  (kao-edit-tests--with "ab cd" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)
                                 (kao-sel-make :anchor 4 :cursor 5))
                     :main 1))
    (setq kao--pending-register ?1)
    (kao-yank)
    (let ((lst (kao-sels-list kao--sels)))
      (should (equal (kao-sel-captures (nth 0 lst)) '("" "ab")))
      (should (equal (kao-sel-captures (nth 1 lst)) '("" "cd"))))
    (should (equal (kao-register-get ?1) '("ab" "cd")))))

;;;; Comment toggling (SPC c)

(ert-deftest kao-edit-comment-single-line-toggles ()
  "`SPC c' comments the selection's line; a second press uncomments it."
  (kao-edit-tests--with "abc\ndef" 1
    (setq-local comment-start "# ")
    (kao-comment-lines)
    (should (string= (buffer-string) "# abc\ndef"))
    (kao-comment-lines)
    (should (string= (buffer-string) "abc\ndef"))))

(ert-deftest kao-edit-comment-multi-sel-distinct-lines ()
  "Two selections comment their own lines; the line between is untouched."
  (kao-edit-tests--with "abc\ndef\nghi" 1          ; a1 … \n4 d5 … \n8 g9
    (setq-local comment-start "# ")
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 9 :cursor 9))
                     :main 0))
    (kao-comment-lines)
    (should (string= (buffer-string) "# abc\ndef\n# ghi"))))

(ert-deftest kao-edit-comment-overlapping-ranges-merge ()
  "A line covered by two selections is toggled ONCE (ranges merge)."
  (kao-edit-tests--with "abc\ndef" 1
    (setq-local comment-start "# ")
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 5)  ; lines 1-2
                                 (kao-sel-make :anchor 6 :cursor 6)) ; line 2
                     :main 0))
    (kao-comment-lines)
    (should (string= (buffer-string) "# abc\n# def"))))

(ert-deftest kao-edit-comment-multiline-selection-spans ()
  "A multi-line selection comments every line it spans."
  (kao-edit-tests--with "abc\ndef\nghi" 1
    (setq-local comment-start "# ")
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 6)) ; lines 1-2
                     :main 0))
    (kao-comment-lines)
    (should (string= (buffer-string) "# abc\n# def\nghi"))))

(ert-deftest kao-edit-comment-keeps-selections-one-undo ()
  "Selections shift with the comment text; the whole toggle is one undo unit."
  (kao-edit-tests--with "abc\ndef" 1
    (setq-local comment-start "# ")
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 2)
                                 (kao-sel-make :anchor 5 :cursor 5))
                     :main 0))
    (kao-comment-lines)
    ;; insertion-type-t markers ride the "# " inserts: 1->3, 2->4, 5->9.
    (should (equal (kao-edit-tests--pairs) '((3 . 4) (9 . 9))))
    (primitive-undo 1 buffer-undo-list)
    (should (string= (buffer-string) "abc\ndef"))))

;;;; Pulse wiring (-2)

(ert-deftest kao-edit-hist-select-ranges-pulses ()
  "A history navigation's selection reset flashes the main modified span; a nil
range list (nothing replayed) does not."
  (kao-edit-tests--with "abcdef" 1
    (let (spans)
      (cl-letf (((symbol-function 'kao--pulse-span)
                 (lambda (b e) (push (cons b e) spans))))
        (kao--hist-select-ranges '((2 . 5)))     ; insertion: sel [2,4], span [2,5)
        (kao--hist-select-ranges nil))
      (should (equal spans '((2 . 5)))))))

;;;; Insert-entry setup rollback

(ert-deftest kao-edit-change-setup-error-rolls-back ()
  "A `c' failing mid-multi-delete on read-only text rolls back atomically.
The first selection's delete succeeds, the second hits a `read-only' text
property and signals;: the change group this entry opened is cancelled
\(buffer text verbatim), the pending capture is discarded, session
markers/flags reset, and the selection list is untouched.  The opener IS
clobbered — faithful: Kakoune's Insert ctor sets `m_last_insert' before
`prepare's erase (input_handler.cc:1189-1199)."
  (kao-edit-tests--with "abc def" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)
                                 (kao-sel-make :anchor 5 :cursor 7))
                     :main 0))
    (put-text-property 5 8 'read-only t)
    (setq kao--last-insert-opener #'ignore)
    (let ((nodes (hash-table-count kao--hist)))
      (should-error (kao-change-no-yank) :type 'text-read-only)
      ;; Buffer restored verbatim (the successful first delete undone).
      (should (equal (buffer-string) "abc def"))
      ;; Session fully closed.
      (should-not kao--insert-active)
      (should kao--normal-active)
      (should-not kao--insert-undo-handle)
      (should-not kao--insert-secondary-sites)
      (should-not kao--insert-start)
      (should-not kao--insert-restore)
      ;; Selection list untouched (positions valid again post-rollback).
      (should (equal (kao-edit-tests--pairs) '((1 . 1) (5 . 7))))
      ;; No spurious node: capture discarded, a commit is a no-op.
      (should-not kao--hist-pending)
      (kao--hist-maybe-commit)
      (should (= (hash-table-count kao--hist) nodes))
      ;; Faithful opener clobber (NOT restored by the rollback).
      (should (eq kao--last-insert-opener #'kao--change-selections)))
    ;; The machinery still works: a clean change on the writable region.
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0))
    (kao-change-no-yank)
    (should kao--insert-active)
    (insert "X")
    (kao-insert-exit)
    (should (equal (buffer-string) "Xbc def"))))

(ert-deftest kao-edit-open-below-setup-error-rolls-back ()
  "An `o' whose newline insert hits read-only text rolls back atomically.
Whole-line `read-only' property: `end-of-line' + `(insert \"\\n\")' between
two read-only chars signals; same rollback assertions as the `c' case."
  (kao-edit-tests--with "ab\ncd" 1
    (put-text-property 1 6 'read-only t)
    (should-error (kao-open-below) :type 'text-read-only)
    (should (equal (buffer-string) "ab\ncd"))
    (should-not kao--insert-active)
    (should kao--normal-active)
    (should-not kao--insert-undo-handle)
    (should-not kao--insert-secondary-sites)
    (should-not kao--insert-start)
    (should-not kao--hist-pending)
    (should (equal (kao-edit-tests--pairs) '((1 . 1))))
    ;; The opener is untouched here: `o' records it AFTER the setup form.
    (should-not (eq kao--last-insert-opener #'kao-open-below))))

(ert-deftest kao-edit-nested-oneshot-setup-error-keeps-outer-session ()
  "A failing `c' inside a one-shot `<a-;>' does NOT cancel the outer session.
The entry's `kao--open-undo-group' no-ops on the live handle, so the
abort must leave the outer group (and the text typed so far) alone: the
partial edit stays in the outer session's single undo unit and the session
remains resumable (REQ-3)."
  (kao-edit-tests--with "abc" 2
    (kao-insert)                        ; outer session at 'b' (sel min = 2)
    (insert "X")                        ; typed text in the outer group
    (let ((handle kao--insert-undo-handle))
      (should handle)
      (kao-insert-one-shot)             ; one-shot normal window
      ;; Collapsed sel sits at point (after "X"); make its char read-only so
      ;; the one-shot `c' fails on its very first delete.
      (put-text-property (point) (1+ (point)) 'read-only t)
      (should-error (kao-change-no-yank) :type 'text-read-only)
      ;; Outer group untouched: same live handle, typed text still present.
      (should (eq kao--insert-undo-handle handle))
      (should (equal (buffer-string) "aXbc"))
      ;; The nested entry re-flipped to insert; the session resumes and exits
      ;; cleanly as ONE undo unit.
      (should kao--insert-active)
      (kao-insert-exit)
      (should-not kao--insert-undo-handle)
      (should kao--normal-active))))

;;;; Kao--multi-edit rolls back on non-local exit

(ert-deftest kao-edit-multi-edit-rolls-back-on-nonlocal-exit ()
  "A non-local exit mid-pass cancels the change group — buffer untouched.
The pipe family runs its shell call inside this primitive, so a `C-g' that
quits mid-pass must leave no partial multi-edit."
  (kao-edit-tests--with "aaa bbb ccc" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3)
                                 (kao-sel-make :anchor 5 :cursor 7)
                                 (kao-sel-make :anchor 9 :cursor 11))
                     :main 0))
    (should-error
     (kao--multi-edit
      (lambda (am _cm i)
        (goto-char (marker-position am))
        (insert "X")                        ; edits selection 0, then 1...
        (when (= i 1) (error "boom"))        ; ...non-local exit on the 2nd
        (cons (point) (point)))))
    (should (string= (buffer-string) "aaa bbb ccc"))     ; rolled back, untouched
    (should (= (length (kao-sels-list kao--sels)) 3))))   ; sels untouched

;;;; Case transforms (~) and r cancel their change group mid-pass

(ert-deftest kao-edit-upcase-read-only-mid-pass-rolls-back ()
  "`~' aborting on a read-only selection mid-pass leaves the buffer verbatim
and the selection list untouched — case transforms route through
`kao--multi-edit', so an interrupted pass cancels its change group.
Selection 1 upcases fine; selection 2's span is read-only."
  (kao-edit-tests--with "abc def" 1        ; a1 b2 c3 SPC4 d5 e6 f7
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3)   ; "abc"
                                 (kao-sel-make :anchor 5 :cursor 7))  ; "def"
                     :main 0))
    (put-text-property 5 8 'read-only t)
    (should-error (kao-upcase) :type 'text-read-only)
    (should (equal (buffer-string) "abc def"))            ; first upcase undone
    (should (equal (kao-edit-tests--pairs) '((1 . 3) (5 . 7))))))

(ert-deftest kao-edit-replace-char-read-only-mid-pass-rolls-back ()
  "`r' aborting on a read-only selection mid-pass leaves the buffer verbatim
and the selections untouched — `kao--replace-char-with' routes through
`kao--multi-edit'."
  (kao-edit-tests--with "abc def" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3)
                                 (kao-sel-make :anchor 5 :cursor 7))
                     :main 0))
    (put-text-property 5 8 'read-only t)
    (should-error (kao--replace-char-with ?x) :type 'text-read-only)
    (should (equal (buffer-string) "abc def"))
    (should (equal (kao-edit-tests--pairs) '((1 . 3) (5 . 7))))))

;;;; The two remaining scaffolds cancel their change group on abort

(ert-deftest kao-edit-comment-lines-read-only-mid-pass-rolls-back ()
  "`SPC c' aborting on the 2nd merged range (a read-only line) leaves the buffer
verbatim and the selections untouched — `kao--edit-keeping-sels' cancels its
change group on non-local exit.  Merged ranges run
descending: line 3 comments first, then line 2's comment insert hits the
front-sticky read-only span and barfs.  `front-sticky' is needed because
`comment-region' under a change group dodges a plain `read-only' insert-before."
  (kao-edit-tests--with "abc\ndef\nghi" 1   ; abc(1-3) \n4 def(5-7) \n8 ghi(9-11)
    (setq-local comment-start "# ")
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 5 :cursor 5)   ; line 2
                                 (kao-sel-make :anchor 9 :cursor 9))  ; line 3
                     :main 0))
    (add-text-properties 5 8 '(read-only t front-sticky t))  ; "def" read-only
    (should-error (kao-comment-lines) :type 'text-read-only)
    (should (equal (buffer-string) "abc\ndef\nghi"))
    (should (equal (kao-edit-tests--pairs) '((5 . 5) (9 . 9))))))

(ert-deftest kao-edit-replace-all-read-only-mid-pass-rolls-back ()
  "`<a-R>' aborting on a read-only selection mid-loop leaves the buffer verbatim
and the selections untouched — `kao--paste-all' cancels its change group on
non-local exit."
  (kao-edit-tests--with "abc def" 1          ; a1 b2 c3 SPC4 d5 e6 f7
    (kao-register-set kao-register-default '("X"))
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 1)   ; 'a' replaced 1st
                                 (kao-sel-make :anchor 5 :cursor 5))  ; 'd' read-only
                     :main 0))
    (put-text-property 5 6 'read-only t)     ; sel 1's char read-only
    (should-error (kao-replace-all) :type 'text-read-only)
    (should (equal (buffer-string) "abc def"))
    (should (equal (kao-edit-tests--pairs) '((1 . 1) (5 . 5))))))

;;;; R records its char (macro-safe) and maps RET to newline

(ert-deftest kao-edit-replace-char-records-key-while-recording ()
  "`r' pushes its replacement char onto the macro recorder: it now
reads via `kao--read-key', not bare `read-char', so `Q r Z Q' replays `Z' and
does not consume the following macro key.  Mirrors
kao-macro-read-key-records-while-recording (test/kao-macro-tests.el is frozen)."
  (kao-edit-tests--with "abc" 2                     ; cursor on 'b'
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 2 :cursor 2)) :main 0))
    (let ((kao--macro-recording-reg ?@)
          (kao--macro-recorded-keys (list [?r]))    ; the `r' keypress, recorded
          (kao--macro-replaying nil)
          (unread-command-events (list ?Z)))
      (kao-replace-char)
      ;; `Z' landed right after `r' (push prepends), so replay stays in sync.
      (should (equal kao--macro-recorded-keys (list [?Z] [?r]))))
    (should (string= (buffer-string) "aZc"))))       ; and the edit happened

(ert-deftest kao-edit-replace-char-return-maps-to-newline ()
  "`r RET' replaces each selected char with a newline, not a literal ^M
\(r-site, `kao--key-codepoint')."
  (kao-edit-tests--with "abc" 1
    (setq kao--sels (kao-sels-make
                     :list (list (kao-sel-make :anchor 1 :cursor 3)) :main 0)) ; "abc"
    (let ((unread-command-events (list ?\r)))
      (kao-replace-char))
    (should (string= (buffer-string) "\n\n\n"))
    (should-not (string-match-p "\r" (buffer-string)))))   ; no ^M

;;;; Mode-off guard sweep — (ADDITIVE pins)

;; An M-x-discoverable edit command run in a buffer where `kao-mode' is off used
;; to die far from the call as (wrong-type-argument kao-sels nil) — `kao--sels'
;; is nil and a selection-reading command trips its struct accessor.  The shared
;; `kao--assert-mode' guard (the mode guard) turns that into a named `user-error'.
;; Representative commands only (the guard is uniform); each is driven via
;; `call-interactively' in a fundamental-mode buffer, the M-x path.

(ert-deftest kao-edit-delete-mode-off-guards ()
  "I4: `d' (`kao-delete') via M-x with `kao-mode' off signals the shared guard,
not the cryptic (wrong-type-argument kao-sels nil)."
  (with-temp-buffer                     ; fundamental-mode temp buffer: mode OFF
    (insert "abc")
    (let ((err (should-error (call-interactively #'kao-delete)
                             :type 'user-error)))
      (should (string-match-p "kao-mode is not active" (cadr err))))))

(ert-deftest kao-edit-yank-mode-off-guards ()
  "I4: `y' (`kao-yank') via M-x with `kao-mode' off signals the shared guard."
  (with-temp-buffer
    (insert "abc")
    (let ((err (should-error (call-interactively #'kao-yank)
                             :type 'user-error)))
      (should (string-match-p "kao-mode is not active" (cadr err))))))

(ert-deftest kao-edit-insert-mode-off-guards ()
  "I4: `i' (`kao-insert') via M-x with `kao-mode' off signals the shared guard."
  (with-temp-buffer
    (insert "abc")
    (let ((err (should-error (call-interactively #'kao-insert)
                             :type 'user-error)))
      (should (string-match-p "kao-mode is not active" (cadr err))))))

(ert-deftest kao-edit-undo-mode-off-guards ()
  "I4: `u' (`kao-undo') via M-x with `kao-mode' off signals the shared guard."
  (with-temp-buffer
    (insert "abc")
    (let ((err (should-error (call-interactively #'kao-undo)
                             :type 'user-error)))
      (should (string-match-p "kao-mode is not active" (cadr err))))))

(provide 'kao-edit-tests)
;;; kao-edit-tests.el ends here
