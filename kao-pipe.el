;;; kao-pipe.el --- Pipe selections through the shell for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 8.  The pipe family runs each selection's text
;; through an external shell command:
;;
;;   |     pipe-replace   — replace each selection with the command's stdout.
;;   <a-|> pipe-ignore    — run the command per selection, ignore the output.
;;   !     insert-output  — insert the raw stdout before each selection.
;;   <a-!> append-output  — append the raw stdout after each selection.
;;   $     keep-pipe      — keep only the selections whose command exits 0.
;;
;; This file is the SHELL-OUT BOUNDARY: every `call-process' / `shell-file-name'
;; touch that runs a user command lives in `kao--pipe-call' (`kao--pipe-eval'/
;; `kao--pipe-status' derive stdout/exit-status from it), exactly as the regex
;; engine boundary lives in the `kao--regex-*' helpers.  A later sandbox/async
;; shell layer can slot in without touching the commands.  The one process reach
;; outside this file is the optional `hx --health' locator in kao-treesit.el, a
;; fixed-argv probe that runs no shell.
;;
;; The default command comes from the `|' register (`kao-pipe-register', a text
;; register in `kao--registers').  Faithful to Kakoune (`pipe', normal.cc:687):
;; the register is READ as the empty-entry default AND written with every
;; validated non-blank entry (history_push, input_handler.cc:1124-1131), so
;; `|<ret>'/`!<ret>'/`$<ret>' repeat the last command; the minibuffer history
;; adds native up-arrow recall on top.  Replace edits run through the marker
;; army (`kao--multi-edit') as one undo unit, the selection list kept in its
;; original order with no merge — faithful to `selections_write_only'
;; (normal.cc:765).

;;; Code:

(require 'cl-lib)                        ; kao--pipe-ctx (selection-env builders)
(require 'kao-selection)
(require 'kao-state)
(require 'kao-register)
(require 'kao-edit)                      ; kao--multi-edit (marker army)

(defconst kao-pipe-register ?|
  "The pipe-command register (Kakoune `|').
A text register in `kao--registers'; holds the last pipe command.  Read as the
empty-entry default and written on every validated non-blank entry
\(history_push, input_handler.cc:1131) — the `|<ret>' repeat register.")

;;;; Shell-out boundary — the one site that runs a user command

(defvar kao--pipe-env nil
  "The Kakoune `kak_*' environment entries for the in-progress shell call, or nil.
A list of \"NAME=VALUE\" strings prepended to `process-environment' by
`kao--pipe-call'.  Each apply loop REBINDS it per selection (search-1): the
selection-INDEPENDENT half (`kao--pipe-env-static' — `bufname'/`buffile'/
`opt_filetype'/`reg_<c>') is built ONCE per pipe, and the selection-DEPENDENT
half (`kao--pipe-env-selection' — `selection'/`selection_desc'/`cursor_*'/
`selections') is rebuilt for the CURRENT selection before its call, so every
per-selection shell spawn sees its own values — faithful to Kakoune re-running
`generate_env' per `ShellManager::eval' with the context pointed at the piped
selection (normal.cc:735-737/:770-777/:930/:1331-1333).  nil means a bare
environment — a plain filter like `sort' that references no `kak_*' var builds
nothing (no buffer content in the argv, so no E2BIG).")

(defcustom kao-pipe-remote nil
  "When non-nil, run the pipe family on the buffer's remote host.
In a TRAMP buffer (a remote `default-directory') the pipe commands then
execute through `process-file' — which dispatches to the remote host —
instead of `call-process-region', which always runs locally.  The
selection text still travels on stdin (through a `make-nearby-temp-file')
and the user command line stays a single argv to the remote `sh -c'.
TRAMP does not propagate `process-environment', so the `kak_*' variables
are NOT visible on the remote side — a documented limitation.  Default
nil: the pipe family runs on the LOCAL host, byte-identical to before,
even inside a remote buffer (the behavior this option gates)."
  :type 'boolean
  :group 'kao)

(defun kao--pipe-call (cmdline input)
  "Run CMDLINE through the shell with INPUT on stdin; return (EXIT-CODE . STDOUT).
CMDLINE is executed via `shell-file-name' `shell-command-switch' (the same `sh
-c' Kakoune's `ShellManager' uses).  Synchronous (`call-process-region',
Kakoune `WaitForStdout'): the return value is the exit status, the temp buffer
holds stdout.  stderr is left to the inferior shell.  The Kakoune `kak_*'
environment (`kao--pipe-env') is prepended to `process-environment' for the
call.  A runaway command stays interruptible with `C-g' (Emacs signals the
inferior SIGINT, then SIGKILL on a second `C-g'); the resulting quit unwinds
through `kao--multi-edit', which cancels its change group so the buffer is left
untouched.  This is the primary external-process boundary in kao (the
user's shell command); the only other process reach is the optional `hx
--health' runtime probe in kao-treesit.el (a fixed-argv locator, no shell).

When `kao-pipe-remote' is non-nil AND the buffer is remote, the call runs on
the buffer's host instead: INPUT is written to a `make-nearby-temp-file' (on
that host) for stdin and CMDLINE dispatches through `process-file', so the same
single-argv `sh -c' executes remotely.  TRAMP does not
carry `process-environment', so `kao--pipe-env' reaches only the local path;
the temp file is removed in an `unwind-protect'."
  (if (and kao-pipe-remote (file-remote-p default-directory))
      ;; Remote path: same single-argv `sh -c', dispatched to the buffer's host.
      (let ((infile (make-nearby-temp-file "kao-pipe")))
        (unwind-protect
            (with-temp-buffer
              (write-region input nil infile nil 'silent)
              (let* ((process-environment
                      (append kao--pipe-env process-environment))
                     (code (process-file shell-file-name infile t nil
                                         shell-command-switch cmdline)))
                (cons code (buffer-string))))
          (delete-file infile)))
    ;; Local path — byte-identical to the historical behavior.
    (with-temp-buffer
      (insert input)
      (let* ((process-environment (append kao--pipe-env process-environment))
             (code (call-process-region (point-min) (point-max)
                                        shell-file-name t t nil
                                        shell-command-switch cmdline)))
        (cons code (buffer-string))))))

(defun kao--pipe-eval (cmdline input)
  "Run CMDLINE with INPUT on stdin; return its stdout (`|'/`!'/`<a-|>'/`<a-!>')."
  (cdr (kao--pipe-call cmdline input)))

(defun kao--pipe-status (cmdline input)
  "Run CMDLINE with INPUT on stdin; return its exit status (`$' keep-pipe).
Kakoune's `keep_pipe' reads the status only (`ShellManager::Flags::None')."
  (car (kao--pipe-call cmdline input)))

(defun kao--pipe-trim-output (input out)
  "Return OUT with one trailing newline removed under the Kakoune pipe rule.
Mirrors normal.cc:744: when INPUT does not end in a newline yet OUT is non-empty
and ends in one, drop exactly one trailing newline (so `echo'-style commands
that append a newline do not grow a within-line selection).  Otherwise OUT is
returned unchanged."
  (if (and (> (length input) 0)
           (/= (aref input (1- (length input))) ?\n)
           (> (length out) 0)
           (= (aref out (1- (length out))) ?\n))
      (substring out 0 (1- (length out)))
    out))

;;;; `kak_*' environment — cmdline-referenced vars only, like generate_env

(defun kao--pipe-line-col (pos)
  "Return (LINE . COLUMN) for POS — 1-based line, 1-based BYTE column.
Kakoune `BufferCoord' is (line, `ByteCount' column) (coord.hh:53); the env vars
and `selection_desc' emit line+1 / column+1 with the column a byte offset
\(main.cc:226-232, selection.cc:474 `ColumnType::Byte')."
  (cons (line-number-at-pos pos)
        (1+ (- (position-bytes pos)
               (position-bytes (save-excursion (goto-char pos)
                                                (line-beginning-position)))))))

(defun kao--pipe-coord (pos)
  "Return POS as a Kakoune `line.column' string (see `kao--pipe-line-col')."
  (let ((lc (kao--pipe-line-col pos)))
    (format "%d.%d" (car lc) (cdr lc))))

(defun kao--pipe-sel-desc (sel)
  "Return SEL as `anchor.line.col,cursor.line.col' (`kak_selection_desc')."
  (concat (kao--pipe-coord (kao-sel-anchor sel)) ","
          (kao--pipe-coord (kao-sel-cursor sel))))

(defun kao--pipe-line-col-char (pos)
  "Return (LINE . CHAR-COLUMN) for POS — 1-based line, 1-based CHAR column.
Kakoune's `char_*' env vars count CODEPOINTS from the line start
\(main.cc:240-243), distinct from the BYTE column `kao--pipe-line-col' emits.
Emacs positions are already character positions, so the column is plain
subtraction from the line beginning."
  (cons (line-number-at-pos pos)
        (1+ (- pos (save-excursion (goto-char pos)
                                   (line-beginning-position))))))

(defun kao--pipe-coord-char (pos)
  "Return POS as a Kakoune `line.char-column' string (CHAR columns).
The CHAR-column analogue of `kao--pipe-coord' (see `kao--pipe-line-col-char')."
  (let ((lc (kao--pipe-line-col-char pos)))
    (format "%d.%d" (car lc) (cdr lc))))

(defun kao--pipe-sel-desc-char (sel)
  "Return SEL as `anchor.line.char-col,cursor.line.char-col' (CHAR columns).
The CHAR-column analogue of `kao--pipe-sel-desc' (`kak_selections_char_desc',
main.cc:273)."
  (concat (kao--pipe-coord-char (kao-sel-anchor sel)) ","
          (kao--pipe-coord-char (kao-sel-cursor sel))))

(defun kao--pipe-sel-char-length (sel)
  "Return SEL's inclusive CHAR length with the empty-buffer end-clamp.
Matches `kak_selection_length' (char_length, main.cc:287) over the same clamped
span `kao--pipe-sel-text' reads, so a degenerate empty-buffer span is 0."
  (- (min (kao-sel-end sel) (point-max)) (kao-sel-beg sel)))

(defun kao--pipe-main-first (sels main)
  "Return SELS rotated so the selection at index MAIN is first, rest in order.
Kakoune `main_sel_first' (main.cc:133): `concatenated([main,end), [beg,main))'.
The order `kak_selections_desc' uses (main.cc:268); note `kak_selections'
content stays PLAIN selection order (context.cc:396), so only the `*_desc'
plural is rotated."
  (append (nthcdr main sels)
          (butlast sels (- (length sels) main))))

(defun kao--pipe-referenced-vars (cmdline)
  "Return the distinct `kak_*' variable names CMDLINE references, first-seen order.
Scans CMDLINE once for `\\bkak_\\(quoted_\\)?[A-Za-z0-9_]+' — an explicit char
class, NOT `\\w' (which is syntax-table-dependent) — and returns each whole
matched name (\"kak_selection\", \"kak_quoted_reg_a\", \"kak_selections_desc\", …)
at most once.  Greedy `[A-Za-z0-9_]+' makes \"kak_selections_desc\" match whole,
exactly as Kakoune's `generate_env' regex (shell_manager.cc:146).  The scan is
case-sensitive (env var names are lowercase `kak_')."
  (let ((case-fold-search nil)
        (start 0)
        (names nil))
    ;; Explicit leading boundary — `\`' (string start) or a non-identifier char
    ;; — instead of `\b', whose word semantics depend on the syntax table (the
    ;; same trap as `\w'; the var name is captured in group 1 so the boundary
    ;; char is not part of it).
    (while (string-match
            "\\(?:\\`\\|[^A-Za-z0-9_]\\)\\(kak_\\(?:quoted_\\)?[A-Za-z0-9_]+\\)"
            cmdline start)
      (let ((name (match-string 1 cmdline)))
        (unless (member name names) (push name names)))
      (setq start (match-end 1)))
    (nreverse names)))

(defun kao--pipe-sel-text (sel)
  "Return SEL's inclusive text with the empty-buffer end-clamp.
The phantom end (`max'+1) can exceed `point-max' in an empty region, so the end
is clamped to `point-max' at the read site (consumer idiom)."
  (buffer-substring-no-properties
   (kao-sel-beg sel)
   (min (kao-sel-end sel) (point-max))))

(defun kao--pipe-reg-value (spec quoted)
  "Return the `kak_reg_SPEC' env VALUE, or nil when SPEC names no register.
SPEC is the register name after `reg_'.  A single [A-Za-z0-9] char resolves that
letter/digit register directly.  A multi-char SPEC resolves through the register
name aliases (`slash'/`dot'/`dquote'/... to their single char) via
`kao-register-get' — the `operator[](StringView)' path (main.cc:195-198,
register_manager.cc:73-85) — so `kak_reg_slash'/`kak_reg_dot' export their
register value.  An UNKNOWN name signals in `kao--register-resolve', caught to
nil (the entry is dropped, unchanged); a resolved-but-empty register (e.g. the
null `_') yields the empty string.  The value is the register's strings
space-joined; QUOTED shell-quotes each element (shell_manager.cc:165)."
  ;; A one-element wrapper distinguishes resolved (even to an empty list, which
  ;; exports the empty string) from unresolvable (nil, so the entry is dropped).
  (let ((strings
         (if (= (length spec) 1)
             (let ((c (aref spec 0)))
               (and (or (<= ?a c ?z) (<= ?A c ?Z) (<= ?0 c ?9))
                    (list (kao-register-get c))))
           (condition-case nil (list (kao-register-get spec)) (error nil)))))
    (when strings
      (let ((vals (car strings)))
        (if quoted
            (mapconcat #'shell-quote-argument vals " ")
          (mapconcat #'identity vals " "))))))

(defun kao--pipe-var-parse (name)
  "Parse a `kak_*' env variable NAME into a (BASE . QUOTEDP) cons.
Strips the mandatory `kak_' prefix and an optional `quoted_' one — the two
spellings Kakoune's `generate_env' recognises (shell_manager.cc:146) — so
`kak_quoted_selection' is (\"selection\" . t), `kak_selection' is
\(\"selection\" . nil), and `kak_reg_a' is (\"reg_a\" . nil).  BASE is the
classification key both env halves dispatch on; QUOTEDP picks the shell-quoted
value where a var has one.  The ONE decoder shared by `kao--pipe-env-static'
and `kao--pipe-env-selection' — a var can no longer be
split-decoded between two hand-copied strippers."
  (let* ((rest (substring name 4))              ; drop "kak_"
         (quoted (string-prefix-p "quoted_" rest))
         (base (if quoted (substring rest 7) rest)))
    (cons base quoted)))

(cl-defstruct (kao--pipe-ctx (:constructor kao--pipe-ctx-create) (:copier nil))
  "Threaded context for the selection-env builders (`kao--pipe-env-build-*').
SEL is the selection being pointed at (the current main for this call); MIDX the
main index for the MAIN-FIRST plural rotation (0 when SINGLE narrows to SEL).
SINGLE collapses the plural vars to just SEL (the `|' narrowing); PLURAL, when
non-nil, is a (TEXTS DESCS CHAR-DESCS) `kao--pipe-plural-snapshot' feeding the
non-narrowing plural values.  SELF-TEXT/TEXTS/LIVE are lazy memo slots (`unset
until first use): SELF-TEXT caches SEL's text (shared by
`kak_selection'/`kak_quoted_selection'), TEXTS the `kak_selections' source list,
LIVE the live `kao--sels' list read once for the non-narrowing plural
vars, so an env naming no text/plural var never materialises them."
  sel midx single plural
  (self-text 'unset) (texts 'unset) (live 'unset))

(defun kao--pipe-ctx-live-list (ctx)
  "Return CTX's live selection list, materialised once on first use.
Only the non-narrowing plural branch (no SINGLE, no PLURAL snapshot)
reads it, so computing it lazily keeps an env that names no such var
from ever walking the selection list."
  (when (eq (kao--pipe-ctx-live ctx) 'unset)
    (setf (kao--pipe-ctx-live ctx) (kao-sels-list kao--sels)))
  (kao--pipe-ctx-live ctx))

(defun kao--pipe-ctx-plural-list (ctx map-fn plural-fn)
  "Return a plural var's per-selection value list, choosing CTX's source once.
The single/plural/live branch every plural var shares: SINGLE collapses to
\(list (MAP-FN SEL)) — the `|' narrowing; a PLURAL snapshot is read by PLURAL-FN;
otherwise MAP-FN maps the live list (`kao--pipe-ctx-live-list').  MAP-FN is the
one per-selection transform the narrow and live branches share (they only ever
differ from the snapshot, which is pre-shaped), so a caller names it and the
snapshot reader once each instead of re-spelling the `cond'."
  (cond ((kao--pipe-ctx-single ctx)
         (list (funcall map-fn (kao--pipe-ctx-sel ctx))))
        ((kao--pipe-ctx-plural ctx)
         (funcall plural-fn (kao--pipe-ctx-plural ctx)))
        (t (mapcar map-fn (kao--pipe-ctx-live-list ctx)))))

;; A plural-var snapshot is the 3-list (TEXTS DESCS CHAR-DESCS) produced by
;; `kao--pipe-plural-snapshot' — parallel per-selection lists in PLAIN order.
;; These named readers are the plural builders' PLURAL-FN, replacing the raw
;; positional `car'/`cadr'/`caddr' so the snapshot's shape lives in one place.
(defsubst kao--pipe-plural-texts (snapshot)
  "Return the clamped-text list of plural-var SNAPSHOT."
  (car snapshot))

(defsubst kao--pipe-plural-descs (snapshot)
  "Return the BYTE `line.col,line.col' desc list of plural-var SNAPSHOT."
  (cadr snapshot))

(defsubst kao--pipe-plural-char-descs (snapshot)
  "Return the CHAR-column desc list of plural-var SNAPSHOT."
  (caddr snapshot))

;;;; Per-var builders — one selection-DEPENDENT `kak_*' value each (search-1).
;; Each is called (BUILDER CTX QUOTED); it returns the var's VALUE string (or
;; nil to drop the entry).  QUOTED is meaningful only for the two vars with a
;; `quoted_' spelling; the rest ignore it (the dispatch never calls them quoted).

(defun kao--pipe-env-build-selection (ctx quoted)
  "Return kak_selection / kak_quoted_selection for CTX: SEL's clamped text.
via `kao--pipe-sel-text' (main.cc:171); QUOTED shell-quotes it
\(shell_manager.cc:165).  Memoised in CTX so both spellings read it once."
  (when (eq (kao--pipe-ctx-self-text ctx) 'unset)
    (setf (kao--pipe-ctx-self-text ctx)
          (kao--pipe-sel-text (kao--pipe-ctx-sel ctx))))
  (let ((text (kao--pipe-ctx-self-text ctx)))
    (if quoted (shell-quote-argument text) text)))

(defun kao--pipe-env-build-selections (ctx quoted)
  "Return kak_selections / kak_quoted_selections for CTX: texts, PLAIN order.
Content in plain order (main.cc:171 / shell_manager.cc:296); QUOTED
shell-quotes each element (shell_manager.cc:165).  Source memoised in CTX,
built once so both spellings share it."
  (when (eq (kao--pipe-ctx-texts ctx) 'unset)
    (setf (kao--pipe-ctx-texts ctx)
          (kao--pipe-ctx-plural-list ctx #'kao--pipe-sel-text
                                     #'kao--pipe-plural-texts)))
  (let ((texts (kao--pipe-ctx-texts ctx)))
    (if quoted (mapconcat #'shell-quote-argument texts " ")
      (mapconcat #'identity texts " "))))

(defun kao--pipe-env-build-selection-desc (ctx _quoted)
  "Return kak_selection_desc for CTX's SEL: `line.col,line.col' BYTE desc.
1-based line + byte column (selection.cc:474)."
  (kao--pipe-sel-desc (kao--pipe-ctx-sel ctx)))

(defun kao--pipe-env-build-selections-desc (ctx _quoted)
  "Return kak_selections_desc for CTX: BYTE descs, MAIN-FIRST, SEL as main.
Main.cc:268."
  (mapconcat #'identity
             (kao--pipe-main-first
              (kao--pipe-ctx-plural-list ctx #'kao--pipe-sel-desc
                                         #'kao--pipe-plural-descs)
              (kao--pipe-ctx-midx ctx))
             " "))

(defun kao--pipe-env-build-cursor-line (ctx _quoted)
  "Return kak_cursor_line for CTX's SEL: cursor line, 1-based (main.cc:220)."
  (number-to-string
   (car (kao--pipe-line-col (kao-sel-cursor (kao--pipe-ctx-sel ctx))))))

(defun kao--pipe-env-build-cursor-column (ctx _quoted)
  "Return kak_cursor_column for CTX's SEL: cursor column, 1-based BYTE.
The display column is Kakoune's separate `kak_cursor_display_column'
\(main.cc:226)."
  (number-to-string
   (cdr (kao--pipe-line-col (kao-sel-cursor (kao--pipe-ctx-sel ctx))))))

(defun kao--pipe-env-build-selection-length (ctx _quoted)
  "Return kak_selection_length for CTX's SEL: clamped CHAR length.
Main.cc:287."
  (number-to-string (kao--pipe-sel-char-length (kao--pipe-ctx-sel ctx))))

(defun kao--pipe-env-build-selections-length (ctx _quoted)
  "Return kak_selections_length for CTX: per-sel CHAR lengths, PLAIN order.
Main.cc:290."
  (mapconcat #'number-to-string
             (kao--pipe-ctx-plural-list ctx #'kao--pipe-sel-char-length
                                        (lambda (p)
                                          (mapcar #'length
                                                  (kao--pipe-plural-texts p))))
             " "))

(defun kao--pipe-env-build-cursor-char-column (ctx _quoted)
  "Return kak_cursor_char_column for CTX's SEL: cursor CHAR column, 1-based.
Main.cc:240."
  (number-to-string
   (cdr (kao--pipe-line-col-char (kao-sel-cursor (kao--pipe-ctx-sel ctx))))))

(defun kao--pipe-env-build-cursor-char-value (ctx _quoted)
  "Return kak_cursor_char_value for CTX's SEL: cursor codepoint (main.cc:234)."
  (number-to-string
   (or (char-after (kao-sel-cursor (kao--pipe-ctx-sel ctx))) 0)))

(defun kao--pipe-env-build-cursor-byte-offset (ctx _quoted)
  "Return kak_cursor_byte_offset for CTX's SEL: cursor 0-based byte offset.
Main.cc:252."
  (number-to-string
   (1- (position-bytes (kao-sel-cursor (kao--pipe-ctx-sel ctx))))))

(defun kao--pipe-env-build-selections-char-desc (ctx _quoted)
  "Return kak_selections_char_desc for CTX: CHAR descs, MAIN-FIRST.
SEL as main (main.cc:273)."
  (mapconcat #'identity
             (kao--pipe-main-first
              (kao--pipe-ctx-plural-list ctx #'kao--pipe-sel-desc-char
                                         #'kao--pipe-plural-char-descs)
              (kao--pipe-ctx-midx ctx))
             " "))

(defconst kao--pipe-selection-builders
  (list (list "selection"            t   #'kao--pipe-env-build-selection)
        (list "selections"           t   #'kao--pipe-env-build-selections)
        (list "selection_desc"       nil #'kao--pipe-env-build-selection-desc)
        (list "selections_desc"      nil #'kao--pipe-env-build-selections-desc)
        (list "cursor_line"          nil #'kao--pipe-env-build-cursor-line)
        (list "cursor_column"        nil #'kao--pipe-env-build-cursor-column)
        (list "selection_length"     nil #'kao--pipe-env-build-selection-length)
        (list "selections_length"    nil #'kao--pipe-env-build-selections-length)
        (list "cursor_char_column"   nil #'kao--pipe-env-build-cursor-char-column)
        (list "cursor_char_value"    nil #'kao--pipe-env-build-cursor-char-value)
        (list "cursor_byte_offset"   nil #'kao--pipe-env-build-cursor-byte-offset)
        (list "selections_char_desc" nil #'kao--pipe-env-build-selections-char-desc))
  "Dispatch table for the selection-DEPENDENT `kak_*' env vars.
Each entry is (BASE QUOTABLE-P BUILDER): BUILDER is `funcall'ed (BUILDER CTX
QUOTED) for a referenced NAME whose `kao--pipe-var-parse' BASE it keys,
returning the var's VALUE string.  QUOTABLE-P marks the only two vars with a
`quoted_' spelling (selection/selections, shell_manager.cc:165); a `quoted_'
prefix on any other name is dropped (the former `(quoted nil)' sentinel).  This
table's KEY SET is the single classification source:
both halves classify by `assoc' on it — `kao--pipe-env-static' skips exactly
these keys, `kao--pipe-env-selection' dispatches them — so a var is registered
in exactly one place.  A BASE in NEITHER half is silently skipped by both —
faithful `generate_env' (an unrecognised `kak_*' name exports nothing,
shell_manager.cc:144-175); do not add a warning.")

(defun kao--pipe-env-static (names)
  "Return the selection-INDEPENDENT `kak_*' env entries among NAMES (NAME=VALUE).
These vars are identical for every selection in one pipe, so an apply loop
builds them ONCE (once-per-pipe snapshot; registers materialised
here just once): `kak_bufname'/`kak_buffile' the buffer name / file, each
through `file-local-name' so a remote (TRAMP) buffer exports a host-valid name
rather than the raw `/ssh:...' spec (main.cc:142/147),
`kak_opt_filetype' a best-effort `major-mode'->filetype
\(main.cc:189), `kak_reg_<c>'/`kak_quoted_reg_<c>' a LETTER/DIGIT register
\(raw vs shell-quoted, main.cc:195), and the cheap buffer-wide counters
`kak_selection_count'/`kak_buf_line_count'/`kak_timestamp' (search-8, the last
mapped to the buffer history id).  Of these only `reg_<c>' has a `quoted_'
variant.  NAMES is the `kao--pipe-referenced-vars' scan; a selection-dependent
or unrecognised name is skipped here (`kao--pipe-env-selection' or the nil drop
handles it), so a nil NAMES — a plain filter — yields nil."
  (let ((env nil))
    (dolist (name names)
      (let* ((parsed (kao--pipe-var-parse name))
             (base (car parsed))
             (quoted (cdr parsed))
             (val
              (cond
               ;; Selection-DEPENDENT vars belong to `kao--pipe-env-selection'
               ;; the shared builders table, consulted
               ;; first via the same `assoc' idiom as the selection half, makes a
               ;; split decode impossible by construction.
               ((assoc base kao--pipe-selection-builders) nil)
               ((string-prefix-p "reg_" base)
                (kao--pipe-reg-value (substring base 4) quoted))
               ;; The remaining static vars have no shell-quoted spelling.
               (quoted nil)
               ((string= base "bufname") (file-local-name (buffer-name)))
               ((string= base "buffile")
                (file-local-name (or (buffer-file-name) "")))
               ((string= base "opt_filetype")
                (replace-regexp-in-string "\\(-ts\\)?-mode\\'" ""
                                          (symbol-name major-mode)))
               ;; Cheap buffer-wide counters — selection-INDEPENDENT (search-8).
               ((string= base "selection_count")   ; main.cc:297
                (number-to-string (length (kao-sels-list kao--sels))))
               ((string= base "buf_line_count")    ; main.cc:154
                (number-to-string (count-lines (point-min) (point-max))))
               ((string= base "timestamp")         ; buffer history id, main.cc:157
                (number-to-string (or (kao-history-current-id) 0)))
               (t nil))))
        (when val (push (concat name "=" val) env))))
    (nreverse env)))

(defun kao--pipe-plural-snapshot ()
  "Return a `(TEXTS DESCS CHAR-DESCS)' pre-edit snapshot of all selections.
A 3-list of parallel lists in PLAIN selection order — clamped text, BYTE
`line.col,line.col' desc, and CHAR-column desc.  Captured ONCE before an editing
apply loop mutates the buffer, so the non-narrowing plural vars keep a stable
pipe-start view (see `kao--pipe-env-selection'); the CHAR-desc list feeds
`kak_selections_char_desc' (search-8), which cannot be derived from the byte
desc after the edit invalidates the coordinates."
  (let ((sels (kao-sels-list kao--sels)))
    (list (mapcar #'kao--pipe-sel-text sels)
          (mapcar #'kao--pipe-sel-desc sels)
          (mapcar #'kao--pipe-sel-desc-char sels))))

(defun kao--pipe-env-selection (names sel index single &optional plural)
  "Return the selection-DEPENDENT `kak_*' env entries among NAMES for SEL.
SEL is treated as the CURRENT MAIN selection at INDEX — faithful to Kakoune
pointing the context at the piped selection before each `ShellManager::eval'
\(search-1): `pipe<true>' NARROWS to just SEL (normal.cc:735-737);
`pipe<false>'/`insert_output'/`keep_pipe' `set_main_index'
\(normal.cc:770-777/:930/:1331-1333).  An apply loop REBUILDS this half per
selection so `kak_selection', `kak_cursor_*' and `kak_selection_desc' track SEL
rather than the original main.

SINGLE non-nil collapses the plural vars
\(`kak_selections'/`kak_selections_desc'/…) to just SEL — the `|' narrowing;
otherwise they span the full list, content in PLAIN order and the `*_desc' pair
MAIN-FIRST with SEL as main (main.cc:268).  Only `selection'/`selections' have a
`quoted_' spelling.  Each referenced NAME is parsed by `kao--pipe-var-parse' and
dispatched through `kao--pipe-selection-builders' (keyed by BASE, walked in
NAMES order so env entries keep first-seen order — Kakoune `generate_env'); see
each `kao--pipe-env-build-*' builder for that var's value and C++ citation.

PLURAL, when non-nil, is a `(TEXTS DESCS CHAR-DESCS)' snapshot
\(`kao--pipe-plural-snapshot') that supplies the non-narrowing plural values —
the EDITING apply loops (`!'/`<a-!>') capture it ONCE pre-edit because their
marker army carries only SEL, not the full live list, into the per-selection
body (once-per-pipe snapshot for the plural CONTENT; Kakoune keeps live
coords).  When PLURAL is nil the plural vars read the live `kao--sels' (correct
for the non-editing `<a-|>'/`$' verbs and single-shot callers — buffer stable)."
  (let ((ctx (kao--pipe-ctx-create
              :sel sel :midx (if single 0 index) :single single :plural plural))
        (env nil))
    (dolist (name names)
      (let* ((parsed (kao--pipe-var-parse name))
             (base (car parsed))
             (quoted (cdr parsed))
             (entry (assoc base kao--pipe-selection-builders))
             ;; Dispatch by BASE.  No entry → a static/unknown var (skip; the
             ;; table's keys ARE the classification).  A `quoted_' spelling on a
             ;; non-QUOTABLE-P var → drop (the former `(quoted nil)' sentinel).
             (val (when (and entry (or (not quoted) (nth 1 entry)))
                    (funcall (nth 2 entry) ctx quoted))))
        (when val (push (concat name "=" val) env))))
    (nreverse env)))

(defun kao--pipe-environment (cmdline &optional sel index single)
  "Return the `kak_*' env entries CMDLINE references (list of NAME=VALUE).
Faithful to Kakoune's `generate_env' (shell_manager.cc:144-175): CMDLINE is
scanned ONCE (`kao--pipe-referenced-vars') and ONLY the referenced vars are
built — a plain filter naming no `kak_*' var gets nil (a bare environment),
exactly as in Kakoune.  FAITHFUL CONSEQUENCE: a script that reads $kak_selection
without naming it in the command line now gets nothing, matching Kakoune — the
old unconditional export is gone (it also broke `%' on a large
buffer with E2BIG by exporting the whole buffer ×4).

The composed entry — `kao--pipe-env-selection' (SEL as main at INDEX; SINGLE
narrows the plural vars, the `|' rule) appended to `kao--pipe-env-static'.  SEL
defaults to the current main selection and INDEX to the main index, so a
single-shot caller (or the empty-buffer pin) gets the once-per-pipe
main-oriented env.  The apply loops instead call the two builders directly,
rebuilding only the selection half per selection (search-1).  See those builders
for the per-var value semantics; not every `%val{}' is reproduced (full
expansion is the out-of-scope `:'-language surface)."
  (let ((names (kao--pipe-referenced-vars cmdline)))
    (when names
      (let* ((sels (kao-sels-list kao--sels))
             (midx (kao-sels-main kao--sels)))
        (append (kao--pipe-env-selection names (or sel (nth midx sels))
                                         (or index midx) single)
                (kao--pipe-env-static names))))))

;;;; Command resolution — read with the `|'-register default

(defvar kao--pipe-history nil
  "Minibuffer history for the pipe prompts.
The native translation of Kakoune's `'|'' prompt history register
\(normal.cc:694): up-arrow recalls a command — Kakoune's own pipe-repeat.")

(defun kao--read-pipe-command (prompt)
  "Read a shell command under PROMPT, defaulting to the pending or `|' register.
Returns the entered command, or — when the entry is empty — the pending or `|'
register's main-indexed value (`main_sel_register_value(params.reg ? : '|')',
normal.cc:690/:907).  Returns nil when both are empty (the Kakoune no-op,
normal.cc:705).  A validated non-blank entry is ALSO written to the FIXED `|'
register (history_push, input_handler.cc:1124-1131) — `params.reg' redirects
only the READ default, not this write.  The read is
`read-shell-command' — native filename/command completion standing in for
the `shell_complete' completer Kakoune passes to these prompts
\(normal.cc:692-695/:909-912), the native-mechanism translation —
under the kao prompt map (`C-r' register insert)."
  (let* ((default (kao--main-sel-register-value
                   (downcase (kao--register-arg kao-pipe-register))))
         (entered (minibuffer-with-setup-hook #'kao--prompt-setup
                    (read-shell-command prompt nil 'kao--pipe-history))))
    ;; Write the entry to the FIXED `|' history register (history_push,
    ;; input_handler.cc:1124-1131) so `|<ret>'/`!<ret>'/`$<ret>' repeat it.
    ;; Always `|' — Kakoune hardcodes it as the prompt's history register
    ;; (normal.cc:694/:911); `params.reg' redirects only the READ default above.
    ;; Skip a blank-prefix entry (DropHistoryEntriesWithBlankPrefix, :1126-1129,
    ;; set on all five pipe prompts) and the empty entry (history_push's empty
    ;; early return — the default is never re-pushed).
    (when (and entered
               (not (string-empty-p entered))
               (not (memq (aref entered 0) '(?\s ?\t))))
      (kao-register-set kao-pipe-register (list entered)))
    (cond ((and entered (not (string-empty-p entered))) entered)
          ((and default (not (string-empty-p default))) default)
          (t nil))))

;;;; Replace path — pipe<true> (normal.cc:709)

(defun kao-pipe--replace-apply (cmdline)
  "Replace every selection with CMDLINE's stdout over its text (`pipe<true>').
Each selection's text [min, max+1) is piped through CMDLINE; the trimmed stdout
replaces the span via the marker army (`kao--multi-edit', one undo unit,
original order + main preserved, no merge — faithful to `selections_write_only',
normal.cc:765).  The new selection spans the inserted output [beg, beg+len-1]
with the original direction preserved (Kakoune mutates `min()'/`max()' in place,
normal.cc:750);
empty output collapses the selection to the char before the gap
\(`char_prev(new_end)', normal.cc:758), or stays at `point-min' there."
  (barf-if-buffer-read-only)             ; Kakoune throw_if_read_only (normal.cc:711)
  (let* ((names (kao--pipe-referenced-vars cmdline))
         (static-env (and names (kao--pipe-env-static names))))  ; once per pipe
    (kao--multi-edit
     (lambda (am cm i)
       (let* ((a (marker-position am)) (c (marker-position cm))
              (forward (>= c a))
              (beg (min a c)) (end (min (1+ (max a c)) (point-max)))  ; empty-buffer end-clamp
              (input (buffer-substring-no-properties beg end))
              ;; Rebuild the selection-dependent env for THIS selection as main;
              ;; `|' NARROWS the plural vars to it (single=t, normal.cc:735-737).
              (kao--pipe-env
               (and names (append (kao--pipe-env-selection
                                   names (kao-sel-make :anchor a :cursor c) i t)
                                  static-env)))
              (out (kao--pipe-trim-output input (kao--pipe-eval cmdline input))))
         (delete-region beg end)
         (goto-char beg)
         (insert out)
         (let ((len (length out)))
           (cond
            ((zerop len)                 ; empty output: collapse to char_prev(beg)
             (let ((p (if (> beg (point-min)) (1- beg) beg)))
               (cons p p)))
            (forward (cons beg (+ beg len -1)))    ; anchor=min, cursor=max
            (t       (cons (+ beg len -1) beg)))))))))  ; backward: anchor=max, cursor=min

(defun kao-pipe-replace ()
  "Kakoune `|': pipe each selection through a shell command, replace with output.
The command defaults to the `|' register; an empty register and empty entry are
a no-op."
  (interactive)
  (kao--assert-mode)
  (let ((cmdline (kao--read-pipe-command "pipe:")))
    (when cmdline
      (kao-pipe--replace-apply cmdline))))  ; the apply owns the per-sel env (search-1)

;;;; Ignore path — pipe<false> (normal.cc:767)

(defun kao-pipe--ignore-apply (cmdline)
  "Run CMDLINE on every selection's text, ignoring stdout (`pipe<false>').
Each selection's text [min, max+1) is piped through CMDLINE for its side effect
\(e.g. copy to the system clipboard, append to a file); the buffer and the
selection list are left UNCHANGED — Kakoune only walks the main index to read
each selection's content (normal.cc:769-778), never edits."
  (let* ((names (kao--pipe-referenced-vars cmdline))
         (static-env (and names (kao--pipe-env-static names)))  ; once per pipe
         (i 0))
    (dolist (sel (kao-sels-list kao--sels))
      ;; Rebuild the selection-dependent env for THIS selection as main
      ;; (`set_main_index(i)', normal.cc:770-777); no narrowing (single=nil).
      (let ((kao--pipe-env
             (and names (append (kao--pipe-env-selection names sel i nil)
                                static-env))))
        (kao--pipe-eval cmdline (kao--pipe-sel-text sel)))  ; empty-buffer end-clamp
      (setq i (1+ i)))))

(defun kao-pipe-ignore ()
  "Kakoune `<a-|>': pipe each selection through a shell command, ignore output.
The command defaults to the `|' register; an empty register and empty entry are
a no-op."
  (interactive)
  (kao--assert-mode)
  (let ((cmdline (kao--read-pipe-command "pipe-to:")))
    (when cmdline
      (kao-pipe--ignore-apply cmdline))))  ; the apply owns the per-sel env (search-1)

;;;; Insert-output path — insert_output<Insert|Append> (normal.cc:904)

(defun kao-pipe--insert-output-apply (cmdline mode)
  "Insert CMDLINE's stdout before/after each selection (`insert_output').
MODE is `insert' (`!', output goes before the selection at its min) or `append'
\(`<a-!>', after the selection at max+1, clamped to `point-max').  Unlike
`|' the raw stdout is inserted with NO trailing-newline strip (Kakoune inserts
`out' as-is).  The edit runs through the marker army (`kao--multi-edit', one
undo unit); the selection then SPANS the inserted output [pos, pos+len-1] with
the original direction preserved (Kakoune mutates `min()'/`max()' refs,
normal.cc:938).  Empty output collapses the selection to the insertion point POS
\(an insert leaves no gap to step back from, unlike `|')."
  (let* ((names (kao--pipe-referenced-vars cmdline))
         (static-env (and names (kao--pipe-env-static names)))  ; once per pipe
         ;; This verb EDITS, so the marker army carries only the current
         ;; selection into the body.  Snapshot the full plural list ONCE pre-edit
         ;; (only when a plural var is named) so `kak_selections' keeps its stable
         ;; pipe-start view instead of reading stale mid-edit `kao--sels' coords.
         (plural (and names
                      (seq-some (lambda (n) (string-search "selections" n)) names)
                      (kao--pipe-plural-snapshot))))
    (kao--multi-edit
     (lambda (am cm i)
       (let* ((a (marker-position am)) (c (marker-position cm))
              (forward (>= c a))
              (smin (min a c)) (smax (max a c))
              (input (buffer-substring-no-properties smin (min (1+ smax) (point-max))))  ; empty-buffer end-clamp
              ;; Rebuild the selection-dependent env for THIS selection as main
              ;; (`set_main_index(index)', normal.cc:930); no narrowing (single=nil).
              (kao--pipe-env
               (and names (append (kao--pipe-env-selection
                                   names (kao-sel-make :anchor a :cursor c) i nil plural)
                                  static-env)))
              (out (kao--pipe-eval cmdline input))
              (pos (if (eq mode 'insert) smin (min (1+ smax) (point-max))))
              (len (length out)))
         (goto-char pos)
         (insert out)
         (cond
          ((zerop len) (cons pos pos))
          (forward (cons pos (+ pos len -1)))    ; anchor=min, cursor=max
          (t       (cons (+ pos len -1) pos))))))))  ; backward: anchor=max, cursor=min

(defun kao-pipe-insert-output ()
  "Kakoune `!': pipe each selection through a command, insert the output before it.
The command defaults to the `|' register; an empty register and empty entry are
a no-op."
  (interactive)
  (kao--assert-mode)
  (let ((cmdline (kao--read-pipe-command "insert-output:")))
    (when cmdline
      (kao-pipe--insert-output-apply cmdline 'insert))))  ; apply owns per-sel env

(defun kao-pipe-append-output ()
  "Kakoune `<a-!>': pipe each selection through a command, append output after it.
The command defaults to the `|' register; an empty register and empty entry are
a no-op."
  (interactive)
  (kao--assert-mode)
  (let ((cmdline (kao--read-pipe-command "append-output:")))
    (when cmdline
      (kao-pipe--insert-output-apply cmdline 'append))))  ; apply owns per-sel env

;;;; Keep-pipe filter — keep_pipe (normal.cc:1305)

(defun kao-pipe--keep-apply (cmdline)
  "Keep only the selections whose CMDLINE run exits 0 (`keep_pipe').
Each selection's text [min, max+1) is piped through CMDLINE for its EXIT STATUS
only (no buffer edit, no merge — a kept subsequence stays sorted).  The new main
is the first survivor whose original index was >= the old main, else the last
survivor (normal.cc:1330-1343).  When nothing survives the list is unchanged
\(Kakoune throws `no_selections_remaining')."
  (let* ((names (kao--pipe-referenced-vars cmdline))
         (static-env (and names (kao--pipe-env-static names)))  ; once per pipe
         (old-main (kao-sels-main kao--sels))
         (kept '())
         (new-main nil)
         (i 0))
    (dolist (sel (kao-sels-list kao--sels))
      ;; Rebuild the selection-dependent env for THIS selection as main
      ;; (`set_main_index(i)', normal.cc:1331-1333); no narrowing (single=nil).
      (when (eql (let ((kao--pipe-env
                        (and names (append (kao--pipe-env-selection names sel i nil)
                                           static-env))))
                   (kao--pipe-status
                    cmdline (kao--pipe-sel-text sel)))  ; empty-buffer end-clamp
                 0)
        (when (and (null new-main) (>= i old-main))
          (setq new-main (length kept)))   ; forward index this survivor will hold
        (push sel kept))
      (setq i (1+ i)))
    (setq kept (nreverse kept))
    (if (null kept)
        (message "kao: no selections remaining")
      (kao-set-selections-raw kept (or new-main (1- (length kept)))))))

(defun kao-pipe-keep ()
  "Kakoune `$': keep only the selections whose shell command exits successfully.
The command defaults to the `|' register; an empty register and empty entry are
a no-op."
  (interactive)
  (kao--assert-mode)
  (let ((cmdline (kao--read-pipe-command "keep pipe:")))
    (when cmdline
      (kao-pipe--keep-apply cmdline))))  ; the apply owns the per-sel env (search-1)

(provide 'kao-pipe)
;;; kao-pipe.el ends here
