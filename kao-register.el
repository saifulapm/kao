;;; kao-register.el --- List-valued registers for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 6.  Kakoune registers are LIST-valued: a
;; yank of N selections stores N strings, and `p' pastes the i-th string to the
;; i-th selection.  The Emacs kill-ring cannot model this, so kao keeps its own
;; global store mapping a register char to a list of strings.  The default
;; register is `"'.
;;
;; CLIPBOARD BRIDGE (refining).  Emacs HAS a native system clipboard
;; (the kill-ring <-> clipboard sync), so kao does not need Kakoune's
;; `<space>'-user-map clipboard workaround; the bridge lives on the direct keys.
;; On yank, kao mirrors ALL selections (newline-joined) to the system clipboard +
;; kill-ring through the sole `kill-new' site `kao-clipboard-set' (the
;; system-clipboard boundary, cf. the regex boundary) so an external paste
;; receives the whole yank; the internal list stays canonical.  Paste consults
;; `kao-clipboard-external-p' so that a copy made in another application is picked
;; up (`current-kill', exactly as `yank' does), falling back to the internal
;; cycling list when the clipboard is still kao's own last yank.
;;
;; SELECTION registers are a second, parallel store.  Kakoune's `Z'/`z'
;; save and restore the whole selection list to/from a register (default `^'),
;; serialising each selection to a `bufname@timestamp@coords' string.  kao keeps
;; the selection list as NATIVE `kao-sels' geometry instead of strings (the same
;; "internal representation is canonical" choice as) — a snapshot is a
;; deep copy of the integer-position selections plus the source buffer.  The
;; coord-string serialisation (for `:'-command / cross-session interop) is a P5
;; concern deferred behind this clean boundary.
;;
;; DYNAMIC registers.  `%' `.' `#' and the capture registers
;; 0-9 are computed, not stored — the `make_dyn_reg' kind
;; (register_manager.hh:88-101, main.cc:352-398).  kao keeps a char ->
;; (GETTER . SETTER) table here; the definitions live in kao-state (the
;; single registration site mirroring `register_registers'), which owns the
;; buffer-local selection list the getters read.  `%' `.' `#' are read-only
;; (a write signals the faithful "this register is not assignable"); the
;; digit registers also accept writes, setting captures[i] on each live
;; selection.  Dynamic values never enter `kao--registers', so savehist
;; never persists them.  Only `:' and `\' remain unsupported: the `:'
;; command language and kakrc hooks are out of scope, so those two names
;; are the whole of the deferral.
;;
;; HISTORY-REGISTER deviation (documented not silent).  In Kakoune the
;; `/', `|' (and `:', `\') registers are HistoryRegisters
;; (register_manager.cc:44-71): `set' DEDUPS the value to the FRONT of a list
;; capped at 1000 entries and `get_main' returns the most recent — prompt
;; history.  kao stores the LAST value only.  Both `/' and `|' are now WRITTEN
;; on every validated prompt entry (`/' after each committed search; `|' via
;; `history_push'), so `|<ret>' repeats the last pipe —
;; earlier `|' was read as a default but never written, a dead fallback.  Zero
;; user-visible impact from the last-value-only storage while kao has no
;; prompt-history UI: every kao consumer (`n'/`N', the `*' family, the pipe
;; default command) reads only the most recent value, which kao has.  Revisit
;; if a prompt-history UI ever lands.

;;; Code:

(defvar kao--registers (make-hash-table :test 'eq)
  "Global register store: a register char maps to a list of strings.")

(defcustom kao-register-default ?\"
  "The default text register (Kakoune `\"')."
  :type 'character
  :group 'kao)

(defvar kao--dynamic-registers (make-hash-table :test 'eq)
  "Dynamic-register table: a register char maps to (GETTER . SETTER).
The `make_dyn_reg' kind (register_manager.hh:88-101, main.cc:352-398):
GETTER is called with no arguments and returns the register's list of
strings computed from the current buffer; SETTER (nil = read-only) is
called with the list of strings being written.  Registered from kao-state
\(the single registration site mirroring `register_registers'), which owns
the buffer-local selection list the getters read.")

(defun kao-register-define-dynamic (char getter &optional setter)
  "Define dynamic register CHAR computed by GETTER, written by SETTER.
GETTER takes no arguments and returns a list of strings; SETTER nil makes
the register read-only — a write then signals Kakoune's exact
\"this register is not assignable\" (register_manager.hh:90)."
  (puthash char (cons getter setter) kao--dynamic-registers))

(defconst kao--register-name-aliases
  '(("slash"      . ?/)
    ("dquote"     . ?\")
    ("pipe"       . ?|)
    ("caret"      . ?^)
    ("arobase"    . ?@)
    ("percent"    . ?%)
    ("dot"        . ?.)
    ("hash"       . ?#)
    ("underscore" . ?_)
    ("colon"      . ?:))
  "Kakoune long register names mapped to their single-char register.
Verbatim port of `reg_names' (register_manager.cc:73-85) so a config that
passes a register name as a string (`set-register dquote …') resolves the
same register as the char.  Lookup is case-sensitive (Kakoune's
`HashMap<StringView, Codepoint>' is), matching `colon' to `:' — which kao
then reports unsupported, exactly as the bare `:' char does.")

(defun kao--register-resolve (char)
  "Normalise register name CHAR; signal on names kao cannot store.
Ports `RegisterManager::operator[]' (register_manager.cc:88-105).  CHAR may
be a single char or a string register name: a length-1 string uses its
char and a longer string resolves through `kao--register-name-aliases'
\(case-sensitive, like `operator[](StringView)'); an unknown name signals
\"no such register\".  The char path lowercases the name (so `\"A' is
register a) and an unknown char signals Kakoune's exact \"no such register\"
error.  kao stores the static registers a-z `\"' `^' `@' plus the `/' and
`|' history registers and the `_' null register; the dynamic registers `%'
`.' `#' 0-9 dispatch through `kao--dynamic-registers'.  The remaining
history registers \(`:' `\\=\\') have no kao backing (the `:' command
language and kakrc hooks are out of scope), so using one signals an
explicit error rather than silently behaving as an empty static register
\(documented deviation)."
  (cond
   ;; String name path — `RegisterManager::operator[](StringView reg)'.
   ((stringp char)
    (cond ((= (length char) 1)
           (kao--register-resolve (aref char 0)))
          ((assoc char kao--register-name-aliases)
           (kao--register-resolve (cdr (assoc char kao--register-name-aliases))))
          (t (user-error "no such register: '%s'" char))))
   ;; Char path — `RegisterManager::operator[](Codepoint c)'.
   (t
    (let ((c (downcase char)))
      (cond ((or (<= ?a c ?z) (<= ?0 c ?9)
                 (memq c '(?\" ?^ ?@ ?/ ?| ?_ ?% ?. ?#)))
             c)
            ((memq c '(?: ?\\))
             (user-error "register '%c' is not supported in kao (the ':' command language and kakrc hooks are out of scope)"
                         c))
            (t (user-error "no such register: '%c'" c)))))))

(defun kao--register-dynamic-name-p (char)
  "Non-nil when CHAR names a dynamic register (`%' `.' `#' 0-9)."
  (or (<= ?0 char ?9) (memq char '(?% ?. ?#))))

(defun kao--register-dynamic (char)
  "Return CHAR's (GETTER . SETTER) entry, erroring on an unregistered name.
A dynamic name with no registered entry (kao-register loaded standalone,
without kao-state's registrations) keeps the explicit unsupported error
rather than silently acting as an empty static register."
  (or (gethash char kao--dynamic-registers)
      (user-error "register '%c' is not supported in kao (its dynamic backing is registered by kao-state, which is not loaded)"
                  char)))

(defvar kao-register-modified-hook nil
  "Abnormal hook run after a register is written, with the resolved char.
Each function is called with one argument: the register character the write
resolved to (lowercased, aliases mapped).  kao's native-Emacs fall-through
for Kakoune's `RegisterModified' hook (hooks.asciidoc:194-195), which fires
after every register write with the register name (register_manager.cc:8/46).
Fired from the static write seams — `kao-register-set',
`kao-register-set-macro', `kao-register-save-selections' — so a
clipboard-manager or search-history mirror can observe writes without advising
private setters.  A discarded `_' null-register write fires nothing (Kakoune
`NullRegister' writes nothing).  Guarded by the zero-cost
`(when kao-register-modified-hook …)' idiom, so an unset hook costs nothing
\(the A3/guarded-defvar-hook pattern).")

(defun kao-register-set (char strings)
  "Set register CHAR to STRINGS (a list of strings).
The list is copied so later mutation of the caller's list does not leak in.
A write to the `_' null register is discarded (Kakoune `NullRegister').
A dynamic register dispatches to its SETTER; a read-only one (`%' `.' `#')
signals the faithful \"this register is not assignable\"
\(register_manager.hh:90).  A static write runs `kao-register-modified-hook'
with the resolved char."
  (let ((c (kao--register-resolve char)))
    (cond ((kao--register-dynamic-name-p c)
           (let ((dyn (kao--register-dynamic c)))
             (if (cdr dyn)
                 (funcall (cdr dyn) strings)
               (user-error "this register is not assignable"))))
          ((eq c ?_) nil)
          (t (puthash c (copy-sequence strings) kao--registers)
             (when kao-register-modified-hook
               (run-hook-with-args 'kao-register-modified-hook c))))))

(defun kao-register-get (char)
  "Return the list of strings stored in register CHAR, or nil.
The `_' null register always reads back nil (Kakoune `NullRegister').
A dynamic register computes its value through its GETTER (the
`DynamicRegister::get' shape, register_manager.hh:66-71)."
  (let ((c (kao--register-resolve char)))
    (if (kao--register-dynamic-name-p c)
        (funcall (car (kao--register-dynamic c)))
      (gethash c kao--registers))))

;;;; System-clipboard boundary — the sole kill-new / current-kill site

(defvar kao--clipboard-yank nil
  "The exact string kao last pushed to the system clipboard via `y'.
Lets paste tell kao's own yank (use the internal cycling list) from an external
clipboard change (paste that text to every selection), exactly as `yank' picks
up external copies through `current-kill'.")

(defun kao-clipboard-set (string)
  "Push STRING to the `kill-ring' + system clipboard, remembered for paste.
The sole `kill-new' site (the system-clipboard boundary, cf. the regex
boundary): a yank mirrors ALL its selections here so an external paste receives
the whole yank, while `kao--clipboard-yank' records what we put there."
  (kill-new string)
  (setq kao--clipboard-yank string))

(defun kao-clipboard-current ()
  "Return the current clipboard / `kill-ring' head, or nil when empty.
Honors `interprogram-paste-function' so a copy made in another application is
picked up, exactly as `current-kill' does for `yank'."
  (let ((s (ignore-errors (current-kill 0 t))))
    (and (stringp s) (> (length s) 0) s)))

(defun kao-clipboard-external-p ()
  "Non-nil when the clipboard differs from kao's last `y' (an external copy).
True also when kao never yanked but the clipboard is non-empty.  When true,
paste uses the clipboard string; otherwise kao's internal register list (with
its per-selection cycling) is authoritative."
  (let ((cur (kao-clipboard-current)))
    (and cur (not (equal cur kao--clipboard-yank)))))

(defun kao-register-yank (strings &optional char)
  "Store STRINGS in register CHAR (default `\"') and mirror them to the clipboard.
All selections are newline-joined and pushed to the system clipboard via
`kao-clipboard-set' so an external paste receives the whole yank; the
internal list stays canonical for kao's own per-selection paste.
A yank to the `_' null register discards the whole write — the clipboard
mirror included (Kakoune `NullRegister::set' is a no-op,
register_manager.hh:104-106)."
  (kao-register-set (or char kao-register-default) strings)
  (unless (eq (kao--register-resolve (or char kao-register-default)) ?_)
    (when strings
      (let ((joined (mapconcat #'identity strings "\n")))
        (when (> (length joined) 0)
          (kao-clipboard-set joined))))))

;;;; Selection registers — native `kao-sels' snapshots

(require 'kao-selection)
(require 'kao-history)                  ; `kao-history-current-id'/`-generation'
                                        ; snapshot tag (acyclic: kao-history needs
                                        ; cl-lib only)

(defvar kao--selection-registers (make-hash-table :test 'eq)
  "Global selection-register store: a register char maps to a saved tag.
The value is (BUFFER (GEN . ID) . SELS): SELS is a `kao-sels' snapshot
\(integer positions); BUFFER is the buffer the selections were saved from;
\(GEN . ID) is BUFFER's tree generation and its
`kao-history-current-id' at save time — the timestamp Kakoune's saved
`SelectionList' carries, consumed by the restore-time coordinate translation
\(`kao--snapshot-update').  The generation stamp (tag
format, reused verbatim) lets the decoder reject a tag recorded against a tree
`kao-history-init' has since replaced — falling back to clamp+merge instead of
aliasing an unrelated node.
Cross-buffer : `z' restore switches to BUFFER (Kakoune
`change_buffer', normal.cc:2164-2165) BEFORE translating, so the id is read
against its own buffer's tree; combine refuses with the faithful cross-buffer
error (normal.cc:2074-2075).")

(defcustom kao-selection-register-default ?^
  "The default selection register (Kakoune `^')."
  :type 'character
  :group 'kao)

(defun kao-register-save-selections (sels &optional char)
  "Save SELS to selection register CHAR (default `^'), tagged buffer + id.
A deep copy is stored as (BUFFER (GEN . ID) . SNAP); later edits to the live
list cannot leak into the save, and (GEN . ID) — the buffer's tree generation
and `kao-history-current-id' — lets the restore translate coordinates through
the changes made since, or fall back to clamp+merge when the tag outlives its
tree (Kakoune's saved-SelectionList timestamp).
A save to the `_' null register is discarded (Kakoune `NullRegister')."
  (let ((c (kao--register-resolve (or char kao-selection-register-default))))
    (unless (eq c ?_)
      (puthash c (kao--snap-tag-make (current-buffer) (kao--snapshot-sels sels))
               kao--selection-registers)
      (when kao-register-modified-hook
        (run-hook-with-args 'kao-register-modified-hook c)))))

(defun kao-register-get-selections (&optional char)
  "Return (BUFFER (GEN . ID) . SELS) for selection register CHAR (default `^')."
  (gethash (kao--register-resolve (or char kao-selection-register-default))
           kao--selection-registers))

;;;; Macro registers — native Emacs key vectors

;; A third parallel store (cf. the text + selection stores above).  Kakoune
;; keeps macros in the unified RegisterManager as a key-notation string; kao keeps
;; the NATIVE Emacs key vector that `execute-kbd-macro' replays — the same
;; "internal representation is canonical" choice as the selection registers.  The
;; key-notation-string serialisation + unification with the text registers is
;; interop deferral.  Default register `@' (Kakoune's default macro
;; register); the `"x' prefix reaches the alphabetic registers, with the
;; faithful `@'+alpha guard living in kao-macro (`kao--macro-register-arg').

(defvar kao--macro-registers (make-hash-table :test 'eq)
  "Global macro-register store: a register char maps to a key vector.")

(defcustom kao-macro-register-default ?@
  "The default macro register (Kakoune `@')."
  :type 'character
  :group 'kao)

(defun kao-register-set-macro (keys &optional char)
  "Store KEYS (a key vector) in macro register CHAR (default `@').
A store to the `_' null register is discarded (Kakoune `NullRegister')."
  (let ((c (kao--register-resolve (or char kao-macro-register-default))))
    (unless (eq c ?_)
      (puthash c keys kao--macro-registers)
      (when kao-register-modified-hook
        (run-hook-with-args 'kao-register-modified-hook c)))))

(defun kao-register-get-macro (&optional char)
  "Return the key vector stored in macro register CHAR (default `@'), or nil."
  (gethash (kao--register-resolve (or char kao-macro-register-default))
           kao--macro-registers))

;;;; Session persistence (savehist)

;; When the user runs `savehist-mode', the TEXT and MACRO register stores
;; persist across sessions for free: both are hash tables of printable
;; values (char -> list of strings / char -> key vector), which savehist
;; round-trips through `prin1'/`read'.  Deliberately EXCLUDED:
;; `kao--selection-registers' (snapshots are tagged with live BUFFER
;; objects — unprintable, and meaningless in the next session) and the
;; jump list / selection history (buffer-local state).  Kakoune itself
;; persists nothing; this is a kao extension, inert without savehist.
(with-eval-after-load 'savehist
  (defvar savehist-additional-variables)
  (add-to-list 'savehist-additional-variables 'kao--registers)
  (add-to-list 'savehist-additional-variables 'kao--macro-registers))

(provide 'kao-register)
;;; kao-register.el ends here
