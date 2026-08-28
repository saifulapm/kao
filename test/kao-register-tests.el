;;; kao-register-tests.el --- Tests for kao-register -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the global list-valued register store and the clipboard mirror.

;;; Code:

(require 'ert)
(require 'kao-register)
(require 'kao-state)                     ; `kao-mode', `kao--hist-maybe-commit',
                                         ; `kao--snapshot-update' — the decoder
                                         ; the z/Z tag feeds

(defun kao-register-tests--reset ()
  "Clear all registers for an isolated test."
  (clrhash kao--registers))

(ert-deftest kao-register-set-get-roundtrip ()
  "Setting then getting a register returns the same list of strings."
  (kao-register-tests--reset)
  (kao-register-set ?a '("one" "two"))
  (should (equal (kao-register-get ?a) '("one" "two")))
  (should (null (kao-register-get ?z))))          ; unset register is nil

(ert-deftest kao-register-set-copies ()
  "The stored list is independent of the caller's list."
  (kao-register-tests--reset)
  (let ((src (list "x" "y")))
    (kao-register-set ?a src)
    (setcar src "MUTATED")
    (should (equal (kao-register-get ?a) '("x" "y")))))

;;;; Long-name register aliases (`reg_names', register_manager.cc:73-85)

(ert-deftest kao-register-name-alias-roundtrip ()
  "A long register name reaches the same register as its char (both ways).
`(kao-register-get \"dquote\")' ≡ `(kao-register-get ?\\\")'.  Excludes `_'
(null register, discards) and the dynamic `%'/`.'/`#' (no plain backing)."
  (kao-register-tests--reset)
  (dolist (pair '(("dquote" . ?\") ("caret" . ?^) ("arobase" . ?@)
                  ("slash" . ?/) ("pipe" . ?|)))
    (let ((name (car pair)) (ch (cdr pair)))
      (kao-register-set name '("v1"))            ; set via name
      (should (equal (kao-register-get ch) '("v1")))    ; read via char
      (kao-register-set ch '("v2"))              ; set via char
      (should (equal (kao-register-get name) '("v2"))))))  ; read via name

(ert-deftest kao-register-name-length1-string-is-char ()
  "A length-1 string register name is its char (operator[](StringView) len==1)."
  (should (eq (kao--register-resolve "a") (kao--register-resolve ?a)))
  (should (eq (kao--register-resolve "\"") (kao--register-resolve ?\")))
  (should (eq (kao--register-resolve "A") ?a)))   ; lowercases, like the char path

(ert-deftest kao-register-name-alias-resolve-parity ()
  "Every alias resolves to its char; case-sensitive; `colon' stays unsupported."
  (dolist (pair '(("dquote" . ?\") ("caret" . ?^) ("arobase" . ?@)
                  ("slash" . ?/) ("pipe" . ?|) ("underscore" . ?_)
                  ("percent" . ?%) ("dot" . ?.) ("hash" . ?#)))
    (should (eq (kao--register-resolve (car pair)) (cdr pair))))
  ;; `colon' maps to ?: which kao reports unsupported — identical to the bare char.
  (should-error (kao--register-resolve "colon") :type 'user-error)
  (should-error (kao--register-resolve ?:) :type 'user-error)
  ;; Case-sensitive (Kakoune's HashMap is): an uppercased name is unknown.
  (should-error (kao--register-resolve "DQUOTE") :type 'user-error)
  ;; An unknown multi-char name signals "no such register".
  (should-error (kao--register-resolve "nope") :type 'user-error))

(defmacro kao-register-tests--with-clipboard (&rest body)
  "Run BODY with a clean, isolated system clipboard."
  (declare (indent 0))
  `(let ((kill-ring nil) (kill-ring-yank-pointer nil)
         (interprogram-cut-function nil) (interprogram-paste-function nil)
         (kao--clipboard-yank nil))
     ,@body))

(ert-deftest kao-register-yank-stores-and-mirrors ()
  "Yank stores the list under the default register and mirrors it to kill-ring."
  (kao-register-tests--reset)
  (kao-register-tests--with-clipboard
    (kao-register-yank '("hello"))
    (should (equal (kao-register-get kao-register-default) '("hello")))
    (should (string= (current-kill 0) "hello"))))

(ert-deftest kao-register-yank-multi-mirrors-all-joined ()
  "With several selections, ALL strings (newline-joined) hit the clipboard."
  (kao-register-tests--reset)
  (kao-register-tests--with-clipboard
    (kao-register-yank '("a" "b" "c"))
    (should (equal (kao-register-get kao-register-default) '("a" "b" "c")))
    (should (string= (current-kill 0) "a\nb\nc"))))

(ert-deftest kao-register-null-yank-leaves-clipboard-untouched ()
  "A `_' yank never mirrors to the kill-ring/clipboard.
Kakoune's `NullRegister::set' is a no-op (register_manager.hh:104-106), so the
whole write — including the system-clipboard mirror — is discarded; a normal
yank still mirrors."
  (kao-register-tests--reset)
  (kao-register-tests--with-clipboard
    (kill-new "old-clip")
    (kao-register-yank '("secret") ?_)
    (should (string= (current-kill 0) "old-clip"))   ; mirror skipped
    (should (null (kao-register-get ?_)))            ; internal store discards
    ;; a normal (default-register) yank still mirrors to the clipboard
    (kao-register-yank '("x"))
    (should (string= (current-kill 0) "x"))))

(ert-deftest kao-clipboard-set-pushes-and-remembers ()
  "`kao-clipboard-set' pushes to the kill-ring and records the string."
  (kao-register-tests--with-clipboard
    (kao-clipboard-set "mine")
    (should (string= (current-kill 0) "mine"))
    (should (string= kao--clipboard-yank "mine"))))

(ert-deftest kao-clipboard-current-empty-nil ()
  "`kao-clipboard-current' is nil on an empty clipboard, the head otherwise."
  (kao-register-tests--with-clipboard
    (should (null (kao-clipboard-current)))
    (kill-new "x")
    (should (string= (kao-clipboard-current) "x"))))

(ert-deftest kao-clipboard-external-p-detects-change ()
  "After kao's own set the clipboard is not external; an external copy is."
  (kao-register-tests--with-clipboard
    (kao-clipboard-set "mine")
    (should-not (kao-clipboard-external-p))         ; still kao's own yank
    (kill-new "theirs")                             ; another app copied
    (should (kao-clipboard-external-p))))

(ert-deftest kao-clipboard-external-p-when-never-yanked ()
  "Any non-empty clipboard is external when kao never yanked; empty is not."
  (kao-register-tests--with-clipboard
    (should-not (kao-clipboard-external-p))         ; empty
    (kill-new "theirs")
    (should (kao-clipboard-external-p))))

;;;; Selection registers

(defun kao-register-tests--reset-sels ()
  "Clear all selection registers for an isolated test."
  (clrhash kao--selection-registers))

(ert-deftest kao-register-selection-roundtrip ()
  "Saving then getting a selection register returns (BUFFER ID . kao-sels)."
  (kao-register-tests--reset-sels)
  (with-temp-buffer
    (insert "abcdef")
    (kao-register-save-selections
     (kao-sels-make :list (list (kao-sel-make :anchor 1 :cursor 2)
                                (kao-sel-make :anchor 4 :cursor 4))
                    :main 1))
    (let ((entry (kao-register-get-selections)))
      (should (eq (car entry) (current-buffer)))
      (should (= 2 (length (kao-sels-list (cddr entry)))))
      (should (= 1 (kao-sels-main (cddr entry)))))
    (should (null (kao-register-get-selections ?x)))))   ; unset register

(ert-deftest kao-register-selection-snapshot-decoupled ()
  "The stored snapshot is a deep copy: mutating the source selection won't leak."
  (kao-register-tests--reset-sels)
  (with-temp-buffer
    (insert "abcdef")
    (let ((sel (kao-sel-make :anchor 1 :cursor 2)))
      (kao-register-save-selections (kao-sels-make :list (list sel) :main 0))
      (setf (kao-sel-cursor sel) 5)               ; mutate the original after save
      (let ((stored (car (kao-sels-list (cddr (kao-register-get-selections))))))
        (should (= 2 (kao-sel-cursor stored)))))))  ; snapshot kept the old value

(ert-deftest kao-register-selection-default-is-caret ()
  "The default selection register is `^' (Kakoune)."
  (should (= kao-selection-register-default ?^)))

(ert-deftest kao-register-stale-generation-selection-tag-clamps ()
  "A z/Z tag from a tree `kao-history-init' has since replaced clamps+merges.
kao restarts HistoryIds at 0 on every `kao-mode' enable (also plain
`revert-buffer', `find-alternate-file'), so a stale id can alias a fresh,
unrelated node — even the `(= id (kao-history-current-id))' verbatim fast path.
Stamping the tree generation into the z/Z tag — `(GEN . ID)'
stamp, reused verbatim in the register producer — makes `kao--snapshot-update'
treat a generation mismatch exactly like a gc'd id: no translation, clamp+merge
residual.  Here two over-length selections clamp onto the same point past the
short buffer's end; the fallback sort-and-merges them into ONE, whereas the
buggy same-id fast path installs both verbatim (two selections).  Mirrors
own half, `kao-state-stale-generation-jump-tag-clamps', so the two
halves agree (register half)."
  (kao-register-tests--reset-sels)
  (with-temp-buffer
    (insert "alpha beta gamma delta epsilon")
    (goto-char (point-min))
    (kao-mode 1)
    (unwind-protect
        (progn
          ;; Old tree (generation G1): one committed edit -> current id 1.
          (goto-char (point-max)) (insert "Z") (kao--hist-maybe-commit)
          (should (= (kao-history-current-id) 1))
          ;; Save two selections that BOTH sit past the FUTURE short buffer's
          ;; end, tagged at old id 1 of the old tree generation.
          (kao-register-save-selections
           (kao-sels-make :list (list (kao-sel-make :anchor 10 :cursor 12)
                                      (kao-sel-make :anchor 20 :cursor 22))
                          :main 1))
          ;; Re-init the tree over completely different, shorter content
          ;; (`kao-mode' toggle -> fresh generation, HistoryIds restart at 0).
          (kao-mode -1)
          (erase-buffer)
          (insert "short")
          (kao-mode 1)
          (should (= (kao-history-current-id) 0))
          ;; One new edit so the fresh tree's current id is again 1 == the stale
          ;; tag's id — the same-id verbatim fast path the finding hits.
          (goto-char (point-max)) (insert "X") (kao--hist-maybe-commit)
          (should (= (kao-history-current-id) 1))
          ;; Feed the saved tag's id + snap through the restore decoder exactly
          ;; as `kao-restore-selections' does ((cadr entry)/(cddr entry)).  It
          ;; must clamp+merge, collapsing the two over-length selections into
          ;; one — not install both verbatim.
          (let* ((entry (kao-register-get-selections))
                 (restored (kao--snapshot-update (cddr entry) (cadr entry))))
            (should (= 1 (length (kao-sels-list restored))))))
      (kao-mode -1))))

;;;; Register-name resolution (`RegisterManager::operator[]')

(ert-deftest kao-register-resolve-lowercases ()
  "Register names are lowercased: `\"A' is register a (register_manager.cc:99)."
  (kao-register-set ?A '("upper"))
  (should (equal (kao-register-get ?a) '("upper")))
  (should (equal (kao-register-get ?A) '("upper")))
  (remhash ?a kao--registers))

(ert-deftest kao-register-resolve-rejects-unknown ()
  "An unknown register name signals Kakoune's \"no such register\" error."
  (should-error (kao-register-get ?!) :type 'user-error)
  (should-error (kao-register-set ?! '("x")) :type 'user-error))

(ert-deftest kao-register-resolve-rejects-deferred-history ()
  "The history registers kao defers (`:' `\\=\\') signal an explicit error.
Was the full dynamic rejection; `%' `.' `#' 0-9 are now dispatched — see the
dynamic-register tests below."
  (dolist (c '(?: ?\\))
    (should-error (kao-register-get c) :type 'user-error)))

(ert-deftest kao-register-null-register-discards ()
  "`_' is the faithful null register: writes discarded, reads empty."
  (kao-register-set ?_ '("gone"))
  (should (null (kao-register-get ?_)))
  (kao-register-set-macro [?l ?l] ?_)
  (should (null (kao-register-get-macro ?_)))
  (with-temp-buffer
    (insert "ab")
    (kao-register-save-selections
     (kao-sels-make :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0)
     ?_)
    (should (null (kao-register-get-selections ?_)))))

(ert-deftest kao-register-macro-store-lowercases ()
  "The macro and selection stores resolve names through the same rule."
  (kao-register-set-macro [?w] ?B)
  (should (equal (kao-register-get-macro ?b) [?w]))
  (remhash ?b kao--macro-registers))

(ert-deftest kao-register-defaults-are-customizable ()
  "The three register defaults are user options with the faithful values."
  (dolist (pair '((kao-register-default . ?\")
                  (kao-selection-register-default . ?^)
                  (kao-macro-register-default . ?@)))
    (should (eq (symbol-value (car pair)) (cdr pair)))
    (should (custom-variable-p (car pair)))))

;;;; Session persistence (savehist)

(ert-deftest kao-register-savehist-registration ()
  "Loading savehist registers the text+macro stores (and ONLY those two)."
  (require 'savehist)
  (defvar savehist-additional-variables)
  (should (memq 'kao--registers savehist-additional-variables))
  (should (memq 'kao--macro-registers savehist-additional-variables))
  ;; The selection registers hold buffer objects — must stay excluded.
  (should-not (memq 'kao--selection-registers savehist-additional-variables)))

(ert-deftest kao-register-stores-print-readably ()
  "Seeded text+macro stores survive a prin1/read round-trip (savehist's
serialisation): same keys, `equal' values."
  (let ((kao--registers (make-hash-table :test 'eq))
        (kao--macro-registers (make-hash-table :test 'eq)))
    (puthash ?a '("one" "two\nlines") kao--registers)
    (puthash ?\" '("def") kao--registers)
    (puthash ?@ [?l ?l ?x] kao--macro-registers)
    (dolist (var '(kao--registers kao--macro-registers))
      (let* ((orig (symbol-value var))
             (back (read (prin1-to-string orig))))
        (should (hash-table-p back))
        (should (= (hash-table-count back) (hash-table-count orig)))
        (maphash (lambda (k v) (should (equal v (gethash k back)))) orig)))))

;;;; Dynamic-register kind — dispatch machinery

(ert-deftest kao-register-resolve-accepts-dynamic-names ()
  "`%' `.' `#' and 0-9 are valid register names; `:' `\\=\\' still error."
  (dolist (c '(?% ?. ?# ?0 ?5 ?9))
    (should (eq (kao--register-resolve c) c)))
  (dolist (c '(?: ?\\))
    (let ((err (should-error (kao--register-resolve c) :type 'user-error)))
      (should (string-match-p "is not supported in kao" (cadr err))))))

(ert-deftest kao-register-unsupported-error-says-out-of-scope ()
  "The `:'/`\\=\\' registers error with out-of-scope wording, never \"deferred\".
Those two are unsupported because the `:' command language and kakrc hooks are
out of scope; the shipped dynamics `%'/`.'/`#'/0-9 are NOT deferred, so the
error must not say so."
  (dolist (c '(?: ?\\))
    (let ((err (should-error (kao--register-resolve c) :type 'user-error)))
      (should (string-match-p "out of scope" (cadr err)))
      (should-not (string-match-p "deferred" (cadr err))))))

(ert-deftest kao-register-dynamic-get-dispatches-to-getter ()
  "A dynamic register's read calls its GETTER (DynamicRegister::get)."
  (let ((kao--dynamic-registers (make-hash-table :test 'eq)))
    (kao-register-define-dynamic ?% (lambda () '("computed")))
    (should (equal (kao-register-get ?%) '("computed")))
    ;; never materialised in the static store (savehist never sees it)
    (should (null (gethash ?% kao--registers)))))

(ert-deftest kao-register-dynamic-read-only-set-errors ()
  "Writing a getter-only dynamic register signals the faithful error."
  (let ((kao--dynamic-registers (make-hash-table :test 'eq)))
    (kao-register-define-dynamic ?% (lambda () '("x")))
    (let ((err (should-error (kao-register-set ?% '("v"))
                             :type 'user-error)))
      (should (equal (cadr err) "this register is not assignable")))
    ;; nothing leaked into the static store
    (should (null (gethash ?% kao--registers)))))

(ert-deftest kao-register-dynamic-set-dispatches-to-setter ()
  "A dynamic register with a SETTER receives the written strings."
  (let ((kao--dynamic-registers (make-hash-table :test 'eq))
        (got 'unset))
    (kao-register-define-dynamic ?1 (lambda () nil)
                                 (lambda (strings) (setq got strings)))
    (kao-register-set ?1 '("a" "b"))
    (should (equal got '("a" "b")))
    (should (null (gethash ?1 kao--registers)))))

(ert-deftest kao-register-dynamic-unregistered-falls-back-to-error ()
  "A dynamic NAME with no table entry keeps the explicit unsupported error.
Covers kao-register loaded standalone, without kao-state's registrations —
both the read and the write path.  The dynamics ship, so the message must not
call them \"deferred\"."
  (let ((kao--dynamic-registers (make-hash-table :test 'eq)))
    (dolist (c '(?% ?. ?# ?3))
      (let ((err (should-error (kao-register-get c) :type 'user-error)))
        (should (string-match-p "is not supported in kao" (cadr err)))
        (should-not (string-match-p "deferred" (cadr err))))
      (let ((err (should-error (kao-register-set c '("v")) :type 'user-error)))
        (should (string-match-p "is not supported in kao" (cadr err)))
        (should-not (string-match-p "deferred" (cadr err)))))
    ;; nothing leaked into the static store
    (should (null (gethash ?3 kao--registers)))))

;;;; RegisterModified hook — kao-register-modified-hook

(ert-deftest kao-register-modified-hook-fires-resolved-char ()
  "A static register write runs the hook with the resolved char.
Kakoune fires `RegisterModified' after every register write with the register
name (register_manager.cc:8); kao passes the resolved char (lowercased, aliases
mapped)."
  (kao-register-tests--reset)
  (let* ((seen nil)
         (kao-register-modified-hook (list (lambda (c) (push c seen)))))
    (kao-register-set ?a '("v"))
    (should (equal seen '(?a)))
    ;; `"A' resolves to register a — the hook sees the resolved char.
    (setq seen nil)
    (kao-register-set ?A '("v"))
    (should (equal seen '(?a)))
    ;; a long alias name resolves too (`dquote' -> ?\").
    (setq seen nil)
    (kao-register-set "dquote" '("v"))
    (should (equal seen (list ?\")))))

(ert-deftest kao-register-modified-hook-silent-for-null-register ()
  "The discarded `_' write fires no hook (NullRegister no-op)."
  (kao-register-tests--reset)
  (let* ((seen nil)
         (kao-register-modified-hook (list (lambda (c) (push c seen)))))
    (kao-register-set ?_ '("v"))
    (should (null seen))))

(ert-deftest kao-register-modified-hook-fires-on-macro-and-selection-writes ()
  "Macro and selection register writes fire the hook too.
`kao-register-set-macro' and `kao-register-save-selections' are static write
seams; Kakoune's `RegisterModified' fires on every register write.  A `_' write
to either stays silent."
  (kao-register-tests--reset)
  (kao-register-tests--reset-sels)
  (let* ((seen nil)
         (kao-register-modified-hook (list (lambda (c) (push c seen)))))
    (kao-register-set-macro [?w] ?q)
    (should (equal seen '(?q)))
    (setq seen nil)
    (with-temp-buffer
      (insert "abc")
      (kao-register-save-selections
       (kao-sels-make :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0)
       ?z))
    (should (equal seen '(?z)))
    ;; `_' macro / selection writes are discarded — no hook.
    (setq seen nil)
    (kao-register-set-macro [?w] ?_)
    (with-temp-buffer
      (insert "abc")
      (kao-register-save-selections
       (kao-sels-make :list (list (kao-sel-make :anchor 1 :cursor 1)) :main 0)
       ?_))
    (should (null seen))))

(ert-deftest kao-register-modified-hook-add-hook-recipe ()
  "The documented add-hook recipe observes a `/' write.
A clipboard-manager or search-ring mirror can watch a specific register without
advising private setters — the native-Emacs fall-through for
`RegisterModified'."
  (kao-register-tests--reset)
  (let ((slash-writes 0)
        (fn nil))
    (setq fn (lambda (c) (when (eq c ?/) (setq slash-writes (1+ slash-writes)))))
    (unwind-protect
        (progn
          (add-hook 'kao-register-modified-hook fn)
          (kao-register-set ?a '("x"))     ; not `/' — ignored
          (kao-register-set ?/ '("pat"))   ; observed
          (should (= slash-writes 1)))
      (remove-hook 'kao-register-modified-hook fn))))

(provide 'kao-register-tests)
;;; kao-register-tests.el ends here
