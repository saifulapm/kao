;;; kao-pipe-tests.el --- Tests for kao-pipe -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the P4 pipe family.  The shell-out boundary (`kao--pipe-eval')
;; and the apply cores take the command string directly, so they run without the
;; minibuffer (the kao-search `*-apply' pattern).  Integration uses portable
;; POSIX filters — `cat' (identity), `tr a-z A-Z' (upper, same length), `tr -d'
;; (shrink/erase) — to avoid the trailing-newline ambiguity of `sed'/`awk'.  The
;; trailing-newline rule is locked separately as a pure unit on `kao--pipe-trim-output'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'kao-selection)
(require 'kao-render)
(require 'kao-state)
(require 'kao-register)
(require 'kao-edit)
(require 'kao-pipe)

(defmacro kao-pipe-tests--with (content &rest body)
  "Run BODY in a `kao-mode' temp buffer of CONTENT (point at `point-min')."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (buffer-enable-undo)
     (kao-mode 1)
     (unwind-protect (progn ,@body)
       (kao-mode -1))))

(defun kao-pipe-tests--list (sels &optional main)
  "Set `kao--sels' from SELS, a list of (anchor . cursor) pairs (MAIN default 0)."
  (setq kao--sels (kao-sels-make
                   :list (mapcar (lambda (p) (kao-sel-make :anchor (car p) :cursor (cdr p)))
                                 sels)
                   :main (or main 0))))

(defun kao-pipe-tests--pairs ()
  "Return `kao--sels' as a list of (anchor . cursor) pairs."
  (mapcar (lambda (s) (cons (kao-sel-anchor s) (kao-sel-cursor s)))
          (kao-sels-list kao--sels)))

;;;; Shell-out boundary — kao--pipe-eval

(ert-deftest kao-pipe-eval-cat-identity ()
  "`cat' round-trips stdin unchanged."
  (should (string= (kao--pipe-eval "cat" "hello") "hello")))

(ert-deftest kao-pipe-eval-tr-upper ()
  "`tr a-z A-Z' upper-cases stdin (same length)."
  (should (string= (kao--pipe-eval "tr a-z A-Z" "hello") "HELLO")))

(ert-deftest kao-pipe-eval-tr-delete ()
  "`tr -d' erases the given chars from stdin."
  (should (string= (kao--pipe-eval "tr -d b" "abc") "ac"))
  (should (string= (kao--pipe-eval "tr -d abc" "abc") "")))

;;;; Trailing-newline rule — kao--pipe-trim-output (pure)

(ert-deftest kao-pipe-trim-strips-one-newline ()
  "Input lacking a trailing newline + output ending in one drops exactly one."
  (should (string= (kao--pipe-trim-output "ab" "AB\n") "AB"))
  (should (string= (kao--pipe-trim-output "ab" "AB\n\n") "AB\n")))

(ert-deftest kao-pipe-trim-keeps-when-input-ends-newline ()
  "When the input already ends in a newline, the output newline is preserved."
  (should (string= (kao--pipe-trim-output "ab\n" "AB\n") "AB\n")))

(ert-deftest kao-pipe-trim-keeps-when-output-has-no-newline ()
  "No trailing newline on the output means nothing to strip."
  (should (string= (kao--pipe-trim-output "ab" "AB") "AB")))

(ert-deftest kao-pipe-trim-keeps-empty-output ()
  "Empty output is returned unchanged (no underflow)."
  (should (string= (kao--pipe-trim-output "ab" "") "")))

;;;; Pipe environment — kao--pipe-environment (empty-buffer policy)

(ert-deftest kao-pipe-env-empty-buffer-no-signal ()
  "`kao--pipe-environment' in an empty buffer yields empty selection vars with
no signal — the phantom selection end (max+1) is clamped to `point-max'."
  (kao-pipe-tests--with ""
    (kao-pipe-tests--list '((1 . 1)))
    (let ((env (kao--pipe-environment "$kak_selection $kak_selections")))
      (should (member "kak_selection=" env))
      (should (member "kak_selections=" env)))))

;;;; Empty-buffer clamp — all five pipe commands no-signal (search-0)
;; in an empty buffer the inclusive span end (max+1) exceeds point-max,
;; so every apply read site must clamp to point-max and pipe "" — never signal
;; `args-out-of-range'.  One pin per command (`|' `<a-|>' `!' `<a-!>' `$').

(ert-deftest kao-pipe-replace-empty-buffer-no-signal ()
  "`|' on an empty buffer pipes \"\" with no `args-out-of-range'; the selection
stays `(1 . 1)' (the clamped read+delete span degenerates to empty)."
  (kao-pipe-tests--with ""
    (kao-pipe-tests--list '((1 . 1)))
    (kao-pipe--replace-apply "cat")                  ; must not signal
    (should (string= (buffer-string) ""))
    (should (equal (kao-pipe-tests--pairs) '((1 . 1))))))

(ert-deftest kao-pipe-ignore-empty-buffer-no-signal ()
  "`<a-|>' on an empty buffer is a no-op success — reads \"\" via the clamped
`kao--pipe-sel-text', edits nothing."
  (kao-pipe-tests--with ""
    (kao-pipe-tests--list '((1 . 1)))
    (kao-pipe--ignore-apply "cat")                   ; must not signal
    (should (string= (buffer-string) ""))
    (should (equal (kao-pipe-tests--pairs) '((1 . 1))))))

(ert-deftest kao-pipe-insert-output-empty-buffer-no-signal ()
  "`!' on an empty buffer pipes \"\" with no signal; empty output collapses the
selection to the insertion point `(1 . 1)'."
  (kao-pipe-tests--with ""
    (kao-pipe-tests--list '((1 . 1)))
    (kao-pipe--insert-output-apply "true" 'insert)   ; must not signal
    (should (string= (buffer-string) ""))
    (should (equal (kao-pipe-tests--pairs) '((1 . 1))))))

(ert-deftest kao-pipe-append-output-empty-buffer-no-signal ()
  "`<a-!>' on an empty buffer pipes \"\" with no signal; the append position is
clamped to `point-max'."
  (kao-pipe-tests--with ""
    (kao-pipe-tests--list '((1 . 1)))
    (kao-pipe--insert-output-apply "true" 'append)   ; must not signal
    (should (string= (buffer-string) ""))
    (should (equal (kao-pipe-tests--pairs) '((1 . 1))))))

(ert-deftest kao-pipe-keep-empty-buffer-no-signal ()
  "`$' on an empty buffer pipes \"\" via the clamped `kao--pipe-sel-text';
`true' exits 0 so the selection is kept."
  (kao-pipe-tests--with ""
    (kao-pipe-tests--list '((1 . 1)))
    (kao-pipe--keep-apply "true")                    ; must not signal
    (should (string= (buffer-string) ""))
    (should (equal (kao-pipe-tests--pairs) '((1 . 1))))
    (should (= (kao-sels-main kao--sels) 0))))

;;;; Replace path — kao-pipe--replace-apply (`|')

(ert-deftest kao-pipe-replace-single-upper ()
  "`|' tr-uppers a single selection in place; the span is preserved."
  (kao-pipe-tests--with "hello world"        ; hello = 1-5
    (kao-pipe-tests--list '((1 . 5)))
    (kao-pipe--replace-apply "tr a-z A-Z")
    (should (string= (buffer-string) "HELLO world"))
    (should (equal (kao-pipe-tests--pairs) '((1 . 5))))))

(ert-deftest kao-pipe-replace-preserves-direction ()
  "A backward selection stays backward after replace (anchor=max, cursor=min)."
  (kao-pipe-tests--with "hello world"
    (kao-pipe-tests--list '((5 . 1)))        ; backward: anchor 5, cursor 1
    (kao-pipe--replace-apply "tr a-z A-Z")
    (should (string= (buffer-string) "HELLO world"))
    (should (equal (kao-pipe-tests--pairs) '((5 . 1))))))

(ert-deftest kao-pipe-replace-shrink ()
  "Output shorter than the input shrinks the selection to span it."
  (kao-pipe-tests--with "abc"                ; abc = 1-3
    (kao-pipe-tests--list '((1 . 3)))
    (kao-pipe--replace-apply "tr -d b")      ; "abc" -> "ac"
    (should (string= (buffer-string) "ac"))
    (should (equal (kao-pipe-tests--pairs) '((1 . 2))))))

(ert-deftest kao-pipe-replace-empty-collapses ()
  "Empty output collapses the selection to the char before the gap."
  (kao-pipe-tests--with "xabcy"              ; a-b-c = 2-4
    (kao-pipe-tests--list '((2 . 4)))
    (kao-pipe--replace-apply "tr -d abc")    ; "abc" -> ""
    (should (string= (buffer-string) "xy"))
    (should (equal (kao-pipe-tests--pairs) '((1 . 1))))))   ; on 'x', before the gap

(ert-deftest kao-pipe-replace-multi ()
  "`|' pipes every selection independently; order and main are preserved."
  (kao-pipe-tests--with "ab cd"              ; ab = 1-2, cd = 4-5
    (kao-pipe-tests--list '((1 . 2) (4 . 5)) 1)
    (kao-pipe--replace-apply "tr a-z A-Z")
    (should (string= (buffer-string) "AB CD"))
    (should (equal (kao-pipe-tests--pairs) '((1 . 2) (4 . 5))))
    (should (= (kao-sels-main kao--sels) 1))))

;;;; Ignore path — kao-pipe--ignore-apply (`<a-|>')

(ert-deftest kao-pipe-ignore-leaves-buffer-and-sels ()
  "`<a-|>' pipes each selection but never edits the buffer or selection list."
  (let ((tmp (make-temp-file "kao-pipe-test")))
    (unwind-protect
        (kao-pipe-tests--with "ab cd"            ; ab = 1-2, cd = 4-5
          (kao-pipe-tests--list '((1 . 2) (4 . 5)) 1)
          (kao-pipe--ignore-apply (format "cat >> %s" (shell-quote-argument tmp)))
          (should (string= (buffer-string) "ab cd"))           ; buffer unchanged
          (should (equal (kao-pipe-tests--pairs) '((1 . 2) (4 . 5))))
          (should (= (kao-sels-main kao--sels) 1))              ; sels/main unchanged
          (with-temp-buffer
            (insert-file-contents tmp)
            (should (string= (buffer-string) "abcd"))))         ; each sel was piped
      (delete-file tmp))))

(ert-deftest kao-pipe-ignore-single ()
  "`<a-|>' pipes a lone selection's text to the command's stdin."
  (let ((tmp (make-temp-file "kao-pipe-test")))
    (unwind-protect
        (kao-pipe-tests--with "hello world"      ; hello = 1-5
          (kao-pipe-tests--list '((1 . 5)))
          (kao-pipe--ignore-apply (format "tr a-z A-Z >> %s" (shell-quote-argument tmp)))
          (should (string= (buffer-string) "hello world"))     ; unchanged
          (with-temp-buffer
            (insert-file-contents tmp)
            (should (string= (buffer-string) "HELLO"))))
      (delete-file tmp))))

;;;; Insert-output path — kao-pipe--insert-output-apply (`!' / `<a-!>')

(ert-deftest kao-pipe-insert-output-before ()
  "`!' inserts the raw output before the selection; the sel spans the output."
  (kao-pipe-tests--with "abc"                ; b = 2
    (kao-pipe-tests--list '((2 . 2)))
    (kao-pipe--insert-output-apply "tr a-z A-Z" 'insert)
    (should (string= (buffer-string) "aBbc"))            ; "B" inserted before 'b'
    (should (equal (kao-pipe-tests--pairs) '((2 . 2))))  ; sel on the inserted 'B'
    (should (= (char-after 2) ?B))))

(ert-deftest kao-pipe-append-output-after ()
  "`<a-!>' appends the raw output after the selection; the sel spans the output."
  (kao-pipe-tests--with "abc"                ; b = 2
    (kao-pipe-tests--list '((2 . 2)))
    (kao-pipe--insert-output-apply "tr a-z A-Z" 'append)
    (should (string= (buffer-string) "abBc"))            ; "B" inserted after 'b'
    (should (equal (kao-pipe-tests--pairs) '((3 . 3))))
    (should (= (char-after 3) ?B))))

(ert-deftest kao-pipe-insert-output-preserves-direction ()
  "A backward selection stays backward; the new sel spans the inserted output."
  (kao-pipe-tests--with "abc"
    (kao-pipe-tests--list '((3 . 1)))        ; backward: anchor 3, cursor 1
    (kao-pipe--insert-output-apply "cat" 'insert)
    (should (string= (buffer-string) "abcabc"))          ; "abc" inserted before
    (should (equal (kao-pipe-tests--pairs) '((3 . 1)))))) ; spans output, backward

(ert-deftest kao-pipe-insert-output-multi ()
  "`<a-!>' appends to every selection independently; order and main preserved."
  (kao-pipe-tests--with "a b"                ; a = 1, b = 3
    (kao-pipe-tests--list '((1 . 1) (3 . 3)) 1)
    (kao-pipe--insert-output-apply "tr a-z A-Z" 'append)
    (should (string= (buffer-string) "aA bB"))
    (should (equal (kao-pipe-tests--pairs) '((2 . 2) (5 . 5))))
    (should (= (kao-sels-main kao--sels) 1))))

(ert-deftest kao-pipe-insert-output-empty-collapses-to-pos ()
  "Empty output collapses the selection to the insertion point (no step-back)."
  (kao-pipe-tests--with "xabcy"              ; a-b-c = 2-4
    (kao-pipe-tests--list '((2 . 4)))
    (kao-pipe--insert-output-apply "tr -d abc" 'insert)  ; out ""
    (should (string= (buffer-string) "xabcy"))           ; nothing inserted
    (should (equal (kao-pipe-tests--pairs) '((2 . 2)))))) ; collapsed to min (pos)

(ert-deftest kao-pipe-append-output-at-eob ()
  "`<a-!>' on the last char appends at `point-max' (no spurious newline)."
  (kao-pipe-tests--with "ab"                 ; b = 2 (last char, no trailing \n)
    (kao-pipe-tests--list '((2 . 2)))
    (kao-pipe--insert-output-apply "tr a-z A-Z" 'append)
    (should (string= (buffer-string) "abB"))
    (should (equal (kao-pipe-tests--pairs) '((3 . 3))))))

;;;; Shell boundary refactor — kao--pipe-call / kao--pipe-status

(ert-deftest kao-pipe-call-returns-code-and-stdout ()
  "`kao--pipe-call' returns (EXIT-CODE . STDOUT)."
  (should (equal (kao--pipe-call "tr a-z A-Z" "ab") '(0 . "AB"))))

(ert-deftest kao-pipe-status-exit-code ()
  "`kao--pipe-status' returns the command's exit status."
  (should (= (kao--pipe-status "true" "x") 0))
  (should (= (kao--pipe-status "false" "x") 1))
  (should (= (kao--pipe-status "grep -q foo" "foo bar") 0))
  (should (= (kao--pipe-status "grep -q foo" "bar bar") 1)))

;;;; Keep-pipe filter — kao-pipe--keep-apply (`$')

(ert-deftest kao-pipe-keep-drops-non-matching ()
  "`$' keeps the selections whose command exits 0; the buffer is untouched."
  (kao-pipe-tests--with "foo\nbar\nfoo"      ; foo=1-3, bar=5-7, foo=9-11
    (kao-pipe-tests--list '((1 . 3) (5 . 7) (9 . 11)) 0)
    (kao-pipe--keep-apply "grep -q foo")
    (should (string= (buffer-string) "foo\nbar\nfoo"))    ; unchanged
    (should (equal (kao-pipe-tests--pairs) '((1 . 3) (9 . 11))))
    (should (= (kao-sels-main kao--sels) 0))))

(ert-deftest kao-pipe-keep-main-retracks-to-first-survivor ()
  "Main moves to the first survivor whose original index was >= the old main."
  (kao-pipe-tests--with "bar\nfoo\nfoo"      ; bar=1-3, foo=5-7, foo=9-11
    (kao-pipe-tests--list '((1 . 3) (5 . 7) (9 . 11)) 1)
    (kao-pipe--keep-apply "grep -q foo")     ; drops index 0 (bar)
    (should (equal (kao-pipe-tests--pairs) '((5 . 7) (9 . 11))))
    (should (= (kao-sels-main kao--sels) 0)))) ; old main (1) survives at new index 0

(ert-deftest kao-pipe-keep-main-falls-to-last-survivor ()
  "When every survivor precedes the old main, main becomes the last survivor."
  (kao-pipe-tests--with "foo\nfoo\nbar"      ; foo=1-3, foo=5-7, bar=9-11
    (kao-pipe-tests--list '((1 . 3) (5 . 7) (9 . 11)) 2)
    (kao-pipe--keep-apply "grep -q foo")     ; drops index 2 (bar), main was 2
    (should (equal (kao-pipe-tests--pairs) '((1 . 3) (5 . 7))))
    (should (= (kao-sels-main kao--sels) 1)))) ; last survivor

(ert-deftest kao-pipe-keep-empty-leaves-unchanged ()
  "When nothing survives, the selection list is left unchanged (Kakoune throws)."
  (kao-pipe-tests--with "bar bar"            ; bar=1-3, bar=5-7
    (kao-pipe-tests--list '((1 . 3) (5 . 7)) 1)
    (kao-pipe--keep-apply "grep -q foo")     ; both drop
    (should (string= (buffer-string) "bar bar"))
    (should (equal (kao-pipe-tests--pairs) '((1 . 3) (5 . 7))))
    (should (= (kao-sels-main kao--sels) 1))))

;;;; Command resolution — kao--read-pipe-command

(ert-deftest kao-pipe-read-uses-entered-command ()
  "A non-empty entry wins over the `|' register default."
  (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "cat")))
    (kao-register-set kao-pipe-register '("rev"))
    (should (string= (kao--read-pipe-command "pipe:") "cat"))))

(ert-deftest kao-pipe-read-falls-back-to-register ()
  "An empty entry falls back to the first string of the `|' register."
  (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "")))
    (kao-register-set kao-pipe-register '("rev"))
    (should (string= (kao--read-pipe-command "pipe:") "rev"))))

(ert-deftest kao-pipe-read-empty-and-no-register-is-nil ()
  "An empty entry and an empty `|' register yield nil (the Kakoune no-op)."
  (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "")))
    (kao-register-set kao-pipe-register nil)
    (should (null (kao--read-pipe-command "pipe:")))))

(ert-deftest kao-pipe-read-default-reads-main-indexed-register ()
  "The empty-entry pipe default reads the pending register at the MAIN index.
`\"a|<ret>' redirects the read to register a and defaults to its main-indexed,
last-clamped string (`main_sel_register_value',
register_manager.cc:36-42) — not the front `car'.  The same accessor `<c-r>'
and the regex prompt now share."
  (kao-pipe-tests--with "abcdef"                         ; a1 b2 c3 d4 e5 f6
    (kao-pipe-tests--list '((1 . 1) (3 . 3) (5 . 5)) 2)   ; 3 sels, main index 2
    (kao-register-set ?a '("cmd0" "cmd1" "cmd2"))
    (let ((kao--pending-register ?a))                     ; the `"a' redirect
      (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "")))
        (should (string= (kao--read-pipe-command "pipe:") "cmd2"))))))

(ert-deftest kao-pipe-read-uses-shell-command-reader ()
  "The pipe prompt reads via `read-shell-command' with the pipe history
\(native completion standing in for Kakoune's `shell_complete',
normal.cc:692-695/:909-912)."
  (let (got-hist)
    (cl-letf (((symbol-function 'read-shell-command)
               (lambda (_prompt _init hist &rest _)
                 (setq got-hist hist) "ok")))
      (should (string= (kao--read-pipe-command "pipe:") "ok"))
      (should (eq got-hist 'kao--pipe-history)))))

;;;; `|' register written on validated entries (history_push)

(ert-deftest kao-pipe-read-writes-pipe-register ()
  "A validated non-blank entry is stored in the FIXED `|' register
\(history_push, input_handler.cc:1131) so `|<ret>' can repeat it."
  (kao-pipe-tests--with "x"
    (kao-register-set kao-pipe-register nil)
    (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "sort")))
      (kao--read-pipe-command "pipe:"))
    (should (equal (kao-register-get ?|) '("sort")))))

(ert-deftest kao-pipe-read-register-repeats-last-command ()
  "`|sort<ret>' then `|<ret>': the stored entry becomes the empty-entry default,
so the second prompt re-runs sort (the FIXED `|' history register)."
  (kao-pipe-tests--with "x"
    (kao-register-set kao-pipe-register nil)
    (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "sort")))
      (should (string= (kao--read-pipe-command "pipe:") "sort")))
    (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "")))
      (should (string= (kao--read-pipe-command "pipe:") "sort")))))

(ert-deftest kao-pipe-read-skips-blank-prefix-entry ()
  "A leading-space/tab entry is USED but NOT remembered
\(DropHistoryEntriesWithBlankPrefix, input_handler.cc:1126-1129)."
  (kao-pipe-tests--with "x"
    (kao-register-set kao-pipe-register '("prev"))
    (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) " sort")))
      (should (string= (kao--read-pipe-command "pipe:") " sort")))  ; run as typed
    (should (equal (kao-register-get ?|) '("prev")))))              ; register unchanged

(ert-deftest kao-pipe-read-empty-entry-does-not-write ()
  "An empty entry writes nothing to `|' — the default is not re-pushed and no
empty string is stored (history_push's empty early return)."
  (kao-pipe-tests--with "x"
    (kao-register-set kao-pipe-register nil)
    (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "")))
      (should (null (kao--read-pipe-command "pipe:"))))
    (should (null (kao-register-get ?|)))))            ; nothing written

(ert-deftest kao-pipe-read-writes-fixed-register-not-pending ()
  "The write target is the FIXED `|', NOT the pending register: with `\"c'
pending, `|sort' reads c's default but still writes \"sort\" to `|'.  Kakoune
hardcodes '|' as the history register (normal.cc:694/:911); `params.reg'
redirects only the READ default — so `\"c|sort<ret>' leaves c untouched."
  (kao-pipe-tests--with "x"
    (remhash ?c kao--registers)
    (kao-register-set kao-pipe-register nil)
    (kao-register-set ?c '("seed"))
    (setq kao--pending-register ?C)          ; raw uppercase, lowered at read
    (unwind-protect
        (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "sort")))
          (kao--read-pipe-command "pipe:")
          (should (equal (kao-register-get ?|) '("sort")))    ; FIXED `|' written
          (should (equal (kao-register-get ?c) '("seed"))))   ; pending NOT written
      (remhash ?c kao--registers)
      (setq kao--pending-register nil))))

;;;; Pending register (the `"' prefix)

(ert-deftest kao-pipe-default-command-from-named-register ()
  "An empty prompt entry falls back to the PENDING register's command
\(main_sel_register_value(params.reg ? : '|'), normal.cc:690)."
  (kao-pipe-tests--with "x"
    (remhash ?c kao--registers)
    (kao-register-set ?c (list "tr a b"))
    (kao-register-set kao-pipe-register (list "wrong"))
    (setq kao--pending-register ?C)            ; raw uppercase: lowered at read
    (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "")))
      (should (equal (kao--read-pipe-command "pipe:") "tr a b")))
    (remhash ?c kao--registers)))

;;;; Kak_* environment

(ert-deftest kao-pipe-env-selection-main-and-plural ()
  "`kak_selection' is the MAIN selection text; `kak_selections' all, ' '-joined."
  (kao-pipe-tests--with "foo bar baz"          ; foo=1-3 bar=5-7 baz=9-11
    (kao-pipe-tests--list '((1 . 3) (5 . 7) (9 . 11)) 1)   ; main = bar
    (let ((kao--pipe-env (kao--pipe-environment
                          "$kak_selection $kak_selections")))
      (should (string= (kao--pipe-eval "printf %s \"$kak_selection\"" "") "bar"))
      (should (string= (kao--pipe-eval "printf %s \"$kak_selections\"" "")
                       "foo bar baz")))))

(ert-deftest kao-pipe-env-desc-byte-column ()
  "`kak_selection_desc' is `line.col,line.col' — 1-based line + BYTE column."
  (kao-pipe-tests--with "abc\ndef"             ; line 2 = def at cols 1..3
    (kao-pipe-tests--list '((5 . 7)))          ; def, cursor on f (line 2 col 3)
    (let ((alist (kao--pipe-environment
                  "$kak_selection_desc $kak_cursor_line $kak_cursor_column")))
      (should (member "kak_selection_desc=2.1,2.3" alist))
      (should (member "kak_cursor_line=2" alist))
      (should (member "kak_cursor_column=3" alist)))))

(ert-deftest kao-pipe-env-desc-multibyte-byte-column ()
  "The desc/cursor column counts BYTES, not characters (multi-byte line)."
  (kao-pipe-tests--with "héllo"                ; h=1 é=2 (2 bytes) l=3 l=4 o=5
    (kao-pipe-tests--list '((1 . 3)))          ; cursor on the first l after é
    ;; bytes before pos 3: h(1) + é(2) = 3 -> 1-based byte column = 4
    (should (member "kak_selection_desc=1.1,1.4"
                    (kao--pipe-environment "$kak_selection_desc")))))

(ert-deftest kao-pipe-env-selections-desc-main-first ()
  "`kak_selections_desc' is rotated MAIN-FIRST (main.cc:268); `kak_selections'
content stays PLAIN order (context.cc:396)."
  (kao-pipe-tests--with "ab cd ef"             ; ab=1-2 cd=4-5 ef=7-8
    (kao-pipe-tests--list '((1 . 2) (4 . 5) (7 . 8)) 1)   ; main = cd (index 1)
    (let ((alist (kao--pipe-environment
                  "$kak_selections_desc $kak_selections")))
      ;; desc rotated main-first: cd, then ef, then ab
      (should (member "kak_selections_desc=1.4,1.5 1.7,1.8 1.1,1.2" alist))
      (let ((kao--pipe-env alist))             ; content stays plain order
        (should (string= (kao--pipe-eval "printf %s \"$kak_selections\"" "")
                         "ab cd ef"))))))

(ert-deftest kao-pipe-env-bufname ()
  "`kak_bufname' is the buffer name, reaching the shell."
  (kao-pipe-tests--with "x"
    (rename-buffer "kao-env-test" t)
    (kao-pipe-tests--list '((1 . 1)))
    (let ((kao--pipe-env (kao--pipe-environment "$kak_bufname")))
      (should (string= (kao--pipe-eval "printf %s \"$kak_bufname\"" "")
                       "kao-env-test")))))

(ert-deftest kao-pipe-env-register-collapse ()
  "A list register collapses to `kak_reg_<c>' (raw ' '-join)."
  (kao-pipe-tests--with "x"
    (kao-pipe-tests--list '((1 . 1)))
    (kao-register-set ?a '("one" "two"))
    (unwind-protect
        (should (member "kak_reg_a=one two"
                        (kao--pipe-environment "$kak_reg_a")))
      (remhash ?a kao--registers))))

(ert-deftest kao-pipe-env-plain-filter-unaffected ()
  "With `kao--pipe-env' nil a plain filter runs bare; no kak_* leaks in."
  (kao-pipe-tests--with "hello"
    (kao-pipe-tests--list '((1 . 5)))
    (should (string= (kao--pipe-eval "cat" "hello") "hello"))   ; kao--pipe-env nil
    (should (string= (kao--pipe-eval "printf %s \"${kak_selection:-EMPTY}\"" "")
                     "EMPTY"))))

;;;; Lazy referenced-vars environment (E2BIG + bare-env regression)

(ert-deftest kao-pipe-replace-large-buffer-no-e2big ()
  "`%' + `|wc -c' on a ~1 MB buffer succeeds — the env is built only from the
cmdline's kak_* references (none here), so no multi-megabyte arg list reaches
execve.  Pre-fix this signals file-error \"Argument list too long\" on macOS
because the whole buffer was exported in kak_selection ×4."
  (kao-pipe-tests--with (make-string 1000000 ?x)      ; ~1 MB buffer
    (kao-pipe-tests--list '((1 . 1000000)))           ; whole buffer = one sel (`%')
    (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "wc -c")))
      (kao-pipe-replace))
    (should (string= (string-trim (buffer-string)) "1000000"))))

(ert-deftest kao-pipe-replace-plain-filter-no-kak-env ()
  "Through the REAL `|' command path, a plain filter that names no kak_* var
spawns with ZERO kak_* environment entries — like Kakoune's generate_env, which
exports only cmdline-referenced vars.  Pre-fix the full env was
always exported, so the count is non-zero."
  (kao-pipe-tests--with "hello"
    (kao-pipe-tests--list '((1 . 5)))
    (cl-letf (((symbol-function 'read-shell-command)
               (lambda (&rest _) "env | grep -c '^kak' || true")))
      (kao-pipe-replace))
    (should (string= (string-trim (buffer-string)) "0"))))

(ert-deftest kao-pipe-env-lazy-only-referenced ()
  "The env holds ONLY the kak_* vars the cmdline names: a cmdline referencing
just the `a' register gets its raw + quoted pair and NOTHING else — no selection
vars leak in (pre-fix the full env was always built)."
  (kao-pipe-tests--with "hello world"
    (kao-pipe-tests--list '((1 . 5)))
    (kao-register-set ?a '("x y"))
    (unwind-protect
        (let ((env (kao--pipe-environment
                    "echo $kak_reg_a $kak_quoted_reg_a")))
          (should (equal (sort (copy-sequence env) #'string<)
                         (sort (list "kak_reg_a=x y"
                                     (concat "kak_quoted_reg_a="
                                             (shell-quote-argument "x y")))
                               #'string<)))
          ;; laziness: no selection var leaked in
          (should-not (seq-find (lambda (e) (string-prefix-p "kak_selection" e))
                                env)))
      (remhash ?a kao--registers))))

;;;; search-1 — per-selection kak_* environment rotation

(ert-deftest kao-pipe-env-rotates-selection-per-sel ()
  "Each selection's shell call sees ITS OWN `$kak_selection', not the main's —
`generate_env' re-runs per eval with the context pointed at the current
selection (`insert_output' `set_main_index(index)', normal.cc:930).  `!' with
`printf %s \"$kak_selection\"' inserts each selection's own text before it, so
`ab'/`cd' become `abab'/`cdcd' (pre-fix the once-from-main snapshot — or a nil
env through the direct apply — inserts the same or empty text for both)."
  (kao-pipe-tests--with "ab cd"                ; ab=1-2 cd=4-5
    (kao-pipe-tests--list '((1 . 2) (4 . 5)) 0)   ; main = ab
    (kao-pipe--insert-output-apply "printf %s \"$kak_selection\"" 'insert)
    (should (string= (buffer-string) "abab cdcd"))))

(ert-deftest kao-pipe-env-replace-narrows-selections ()
  "`|' narrows `kak_selections' to just the piped selection (`pipe<true>':
`selections_write_only().set({sel}, 0)', normal.cc:735-737), so replacing each
selection with `$kak_selections' yields its OWN text — not the joined
`ab cd'.  The buffer is unchanged and the spans are preserved."
  (kao-pipe-tests--with "ab cd"                ; ab=1-2 cd=4-5
    (kao-pipe-tests--list '((1 . 2) (4 . 5)) 0)
    (kao-pipe--replace-apply "printf %s \"$kak_selections\"")
    (should (string= (buffer-string) "ab cd"))
    (should (equal (kao-pipe-tests--pairs) '((1 . 2) (4 . 5))))))

(ert-deftest kao-pipe-env-selection-rotates-desc-cursor ()
  "`kao--pipe-env-selection' points `selection'/`selection_desc'/`cursor_*' at the
PASSED selection as the current main at its INDEX (search-1), not the original
main: the main is `ab' (index 0), but the builder for `cd' (index 1) reports
`cd''s text, desc, and byte cursor column."
  (kao-pipe-tests--with "ab cd"                ; ab=1-2 cd=4-5
    (kao-pipe-tests--list '((1 . 2) (4 . 5)) 0)   ; main = ab
    (let* ((names (kao--pipe-referenced-vars
                   "$kak_selection $kak_selection_desc $kak_cursor_line $kak_cursor_column"))
           (env (kao--pipe-env-selection names
                                         (kao-sel-make :anchor 4 :cursor 5) 1 nil)))
      (should (member "kak_selection=cd" env))
      (should (member "kak_selection_desc=1.4,1.5" env))
      (should (member "kak_cursor_line=1" env))
      (should (member "kak_cursor_column=5" env)))))

(ert-deftest kao-pipe-env-selection-plural-snapshot ()
  "With a `(TEXTS DESCS CHAR-DESCS)' PLURAL snapshot (SINGLE nil) `kak_selections'
uses the snapshot content in PLAIN order, `kak_selections_length' the per-text
CHAR lengths in PLAIN order, and `kak_selections_desc'/`kak_selections_char_desc'
rotate it MAIN-FIRST by INDEX — the editing verbs' pre-edit plural view
\(search-1/-8, main.cc:268/:273)."
  (let* ((names (kao--pipe-referenced-vars
                 (concat "$kak_selections $kak_selections_desc "
                         "$kak_selections_char_desc $kak_selections_length")))
         (snap (list '("ab" "cd" "ef")
                     '("1.1,1.2" "1.4,1.5" "1.7,1.8")       ; byte descs
                     '("1.1,1.2" "1.4,1.5" "1.7,1.8")))     ; char descs
         (env (kao--pipe-env-selection names nil 1 nil snap)))   ; index 1 = main
    (should (member "kak_selections=ab cd ef" env))              ; plain order
    (should (member "kak_selections_length=2 2 2" env))          ; plain-order lengths
    (should (member "kak_selections_desc=1.4,1.5 1.7,1.8 1.1,1.2" env))        ; byte
    (should (member "kak_selections_char_desc=1.4,1.5 1.7,1.8 1.1,1.2" env)))) ; char

(ert-deftest kao-pipe-env-insert-output-plural-pre-edit-snapshot ()
  "In an EDITING verb the plural `kak_selections' keeps its pipe-start snapshot,
not stale mid-edit coords: `!' with `[$kak_selections]' inserts the SAME
`[ab cd]' before each selection (marker army carries only the current
selection into the body, so the plural is captured once pre-edit — search-1)."
  (kao-pipe-tests--with "ab cd"                ; ab=1-2 cd=4-5
    (kao-pipe-tests--list '((1 . 2) (4 . 5)) 0)
    (kao-pipe--insert-output-apply "printf %s \"[$kak_selections]\"" 'insert)
    (should (string= (buffer-string) "[ab cd]ab [ab cd]cd"))))

(ert-deftest kao-pipe-env-rotation-nil-fast-path ()
  "The per-selection split preserves the nil fast path: a plain filter
naming no `kak_*' var makes both builders yield nil, so `kao--pipe-env' stays nil
and no buffer content reaches the argv (E2BIG guarantee)."
  (kao-pipe-tests--with "ab cd"
    (kao-pipe-tests--list '((1 . 2) (4 . 5)))
    (let ((names (kao--pipe-referenced-vars "sort")))
      (should (null names))
      (should (null (kao--pipe-env-static names)))
      (should (null (kao--pipe-env-selection
                     names (kao-sel-make :anchor 1 :cursor 2) 0 nil))))))

;;;; search-8 — editing-relevant kak_* env vars (cond-table additions)

(ert-deftest kao-pipe-env-selection-count ()
  "`kak_selection_count' is the number of selections (main.cc:297), built once in
the selection-INDEPENDENT static half."
  (kao-pipe-tests--with "ab cd"                ; ab=1-2 cd=4-5
    (kao-pipe-tests--list '((1 . 2) (4 . 5)) 0)
    (should (member "kak_selection_count=2"
                    (kao--pipe-environment "$kak_selection_count")))))

(ert-deftest kao-pipe-env-buf-line-count ()
  "`kak_buf_line_count' is the line count over the accessible region (main.cc:154)."
  (kao-pipe-tests--with "a\nb\nc"              ; three lines
    (kao-pipe-tests--list '((1 . 1)))
    (should (member "kak_buf_line_count=3"
                    (kao--pipe-environment "$kak_buf_line_count")))))

(ert-deftest kao-pipe-env-timestamp-is-history-id ()
  "`kak_timestamp' maps to the buffer's history id — kao's nearest analogue to
Kakoune's buffer timestamp (main.cc:157); a fresh tree is at id 0."
  (kao-pipe-tests--with "abc"
    (kao-pipe-tests--list '((1 . 1)))
    (should (member "kak_timestamp=0"
                    (kao--pipe-environment "$kak_timestamp")))))

(ert-deftest kao-pipe-env-char-vars-multibyte ()
  "The `char_*' vars count CODEPOINTS, distinct from the BYTE cursor_column /
selection_desc: on an `é'-led span the char column (2) / char desc (1.1,1.2)
differ from the byte column (3) / byte desc (1.1,1.3).  `kak_selection_length' is
the clamped CHAR length, `kak_cursor_char_value' the cursor codepoint, and
`kak_cursor_byte_offset' the 0-based byte distance from buffer start
\(main.cc:234-296)."
  (kao-pipe-tests--with "ér"                    ; é=1 (2 bytes) r=2
    (kao-pipe-tests--list '((1 . 2)))          ; "ér", cursor on r (pos 2)
    (let ((env (kao--pipe-environment
                (concat "$kak_selection_length $kak_cursor_char_column "
                        "$kak_cursor_char_value $kak_cursor_byte_offset "
                        "$kak_selections_char_desc "
                        "$kak_cursor_column $kak_selection_desc"))))
      ;; CHAR-counted vars
      (should (member "kak_selection_length=2" env))
      (should (member "kak_cursor_char_column=2" env))         ; char col
      (should (member "kak_cursor_char_value=114" env))        ; ?r
      (should (member "kak_cursor_byte_offset=2" env))         ; "é" = 2 bytes
      (should (member "kak_selections_char_desc=1.1,1.2" env)) ; char cols
      ;; the existing BYTE vars are unchanged (not conflated)
      (should (member "kak_cursor_column=3" env))              ; byte col
      (should (member "kak_selection_desc=1.1,1.3" env)))))    ; byte desc

(ert-deftest kao-pipe-env-selections-length-plain-order ()
  "`kak_selections_length' is the per-selection CHAR length in PLAIN order
\(main.cc:290), space-joined — NOT main-first — reflecting multibyte char counts."
  (kao-pipe-tests--with "ér xyz"               ; ér=1-2 (é 2 bytes) xyz=4-6
    (kao-pipe-tests--list '((1 . 2) (4 . 6)) 1)   ; main = xyz (index 1)
    (should (member "kak_selections_length=2 3"   ; plain order, not main-first
                    (kao--pipe-environment "$kak_selections_length")))))

(ert-deftest kao-pipe-env-selections-char-desc-main-first ()
  "`kak_selections_char_desc' is CHAR-column descs rotated MAIN-FIRST
\(main.cc:273), distinct from the BYTE `kak_selections_desc'."
  (kao-pipe-tests--with "ér cd"                ; ér=1-2 cd=4-5
    (kao-pipe-tests--list '((1 . 2) (4 . 5)) 1)   ; main = cd (index 1)
    (should (member "kak_selections_char_desc=1.4,1.5 1.1,1.2"   ; cd then ér, char
                    (kao--pipe-environment "$kak_selections_char_desc")))))

(ert-deftest kao-pipe-env-insert-output-selections-length-snapshot ()
  "In an EDITING verb `kak_selections_length' reads the pre-edit plural snapshot,
so `!' with `[$kak_selections_length]' inserts the SAME `[2 2]' before each
selection (marker army carries only the current selection — search-8/-1)."
  (kao-pipe-tests--with "ab cd"                ; ab=1-2 cd=4-5
    (kao-pipe-tests--list '((1 . 2) (4 . 5)) 0)
    (kao-pipe--insert-output-apply "printf %s \"[$kak_selections_length]\"" 'insert)
    (should (string= (buffer-string) "[2 2]ab [2 2]cd"))))

;;;; search-2 — multi-char kak_reg_* through register name aliases

(ert-deftest kao-pipe-env-reg-alias-multichar ()
  "A multi-char `kak_reg_' suffix resolves through the register name aliases
\(`kak_reg_slash' -> `/'; main.cc:195 / register_manager.cc:73-85), raw and
`quoted_' shell-quoted.  A KNOWN-but-empty name (`underscore' -> the null `_')
still exports the empty string; an UNKNOWN name is dropped, unchanged."
  (kao-pipe-tests--with "x"
    (kao-pipe-tests--list '((1 . 1)))
    (kao-register-set ?/ '("search pat"))
    (unwind-protect
        (let ((env (kao--pipe-environment
                    (concat "$kak_reg_slash $kak_quoted_reg_slash "
                            "$kak_reg_underscore $kak_reg_bogus"))))
          (should (member "kak_reg_slash=search pat" env))          ; raw, space kept
          (should (member (concat "kak_quoted_reg_slash="
                                  (shell-quote-argument "search pat"))
                          env))                                     ; shell-quoted
          (should (member "kak_reg_underscore=" env))               ; resolved, empty
          (should-not (seq-find (lambda (e) (string-prefix-p "kak_reg_bogus" e))
                                env)))                              ; unknown -> dropped
      (remhash ?/ kao--registers))))

(ert-deftest kao-pipe-env-reg-single-char-unchanged ()
  "The single-char alnum `kak_reg_' branch is unchanged: `kak_reg_a' raw and
`kak_quoted_reg_a' shell-quoted still resolve register `a'."
  (kao-pipe-tests--with "x"
    (kao-pipe-tests--list '((1 . 1)))
    (kao-register-set ?a '("a b"))
    (unwind-protect
        (let ((env (kao--pipe-environment "$kak_reg_a $kak_quoted_reg_a")))
          (should (member "kak_reg_a=a b" env))
          (should (member (concat "kak_quoted_reg_a=" (shell-quote-argument "a b"))
                          env)))
      (remhash ?a kao--registers))))

;;;; Sync pipe interrupt safety

(ert-deftest kao-pipe-interrupt-leaves-buffer-untouched ()
  "A non-local exit mid-pipe (e.g. `C-g' during a shell call) rolls the buffer
back — no partial replace survives."
  (kao-pipe-tests--with "aaa bbb ccc"          ; aaa=1-3 bbb=5-7 ccc=9-11
    (kao-pipe-tests--list '((1 . 3) (5 . 7) (9 . 11)))
    (let ((n 0))
      (cl-letf (((symbol-function 'kao--pipe-eval)
                 (lambda (&rest _)
                   (setq n (1+ n))
                   (if (= n 2) (error "interrupted") "X"))))   ; quit on 2nd sel
        (should-error (kao-pipe--replace-apply "cmd"))))
    (should (string= (buffer-string) "aaa bbb ccc"))      ; UNTOUCHED
    (should (= (length (kao-sels-list kao--sels)) 3))))    ; selections unchanged

;;;; Tramp-aware pipe boundary — kao-pipe-remote

(ert-deftest kao-pipe-remote-defcustom-defaults-nil ()
  "`kao-pipe-remote' is a customizable boolean defaulting to nil.
The pipe family runs on the LOCAL host unless this is opted in."
  (should (boundp 'kao-pipe-remote))
  (should (eq (default-value 'kao-pipe-remote) nil)))

(ert-deftest kao-pipe-remote-nil-local-path-unchanged ()
  "With `kao-pipe-remote' nil the local `call-process-region' path is unchanged:
a plain filter still round-trips stdin (byte-identical to the historical
behavior)."
  (let ((kao-pipe-remote nil))
    (should (string= (kao--pipe-eval "cat" "hi") "hi"))
    (should (string= (kao--pipe-eval "tr a-z A-Z" "abc") "ABC"))))

(ert-deftest kao-pipe-remote-buffile-file-local-name ()
  "`kak_buffile' exports a HOST-VALID path via `file-local-name': a TRAMP
buffer whose `buffer-file-name' is \"/ssh:host:/tmp/f\" reaches the shell as
\"/tmp/f\", not the raw remote spec."
  (kao-pipe-tests--with "x"
    (kao-pipe-tests--list '((1 . 1)))
    (setq buffer-file-name "/ssh:host:/tmp/f")
    (let ((kao-pipe-remote t))
      (should (member "kak_buffile=/tmp/f"
                      (kao--pipe-environment "$kak_buffile"))))))

(ert-deftest kao-pipe-remote-bufname-file-local-name ()
  "`kak_bufname' also strips the TRAMP prefix via `file-local-name' so a remote
script gets a host-valid name."
  (kao-pipe-tests--with "x"
    (rename-buffer "/ssh:host:/tmp/f" t)
    (kao-pipe-tests--list '((1 . 1)))
    (let ((kao-pipe-remote t))
      (should (member "kak_bufname=/tmp/f"
                      (kao--pipe-environment "$kak_bufname"))))))

;;;; Mode-off guard sweep — (ADDITIVE pins)

;; The pipe commands prompt for a shell command first, so the guard must fire
;; BEFORE the prompt: an M-x invocation with `kao-mode' off signals the shared
;; `kao--assert-mode' user-error, never reaching `read-shell-command' (stubbed
;; here only so the pre-guard RED path does not read the minibuffer in batch).

(ert-deftest kao-pipe-replace-mode-off-guards ()
  "I4: `|' (`kao-pipe-replace') via M-x with `kao-mode' off signals the shared
guard before the shell-command prompt."
  (with-temp-buffer                     ; fundamental-mode temp buffer: mode OFF
    (insert "abc")
    (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "cat")))
      (let ((err (should-error (call-interactively #'kao-pipe-replace)
                               :type 'user-error)))
        (should (string-match-p "kao-mode is not active" (cadr err)))))))

(ert-deftest kao-pipe-keep-mode-off-guards ()
  "I4: `<a-|>'-family `kao-pipe-keep' via M-x with `kao-mode' off signals the
shared guard before the shell-command prompt."
  (with-temp-buffer
    (insert "abc")
    (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "cat")))
      (let ((err (should-error (call-interactively #'kao-pipe-keep)
                               :type 'user-error)))
        (should (string-match-p "kao-mode is not active" (cadr err)))))))

(provide 'kao-pipe-tests)
;;; kao-pipe-tests.el ends here
