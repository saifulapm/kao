;;; kao-multi.el --- Multiple-selection algebra for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 5.  The selection algebra is *pure list
;; manipulation* over the buffer-local `kao--sels' — no overlays-as-cursors, no
;; per-cursor command replay.  Motions from P1 already map over the whole list,
;; so these creators immediately give multi-cursor movement.  Commands so far:
;;
;;   %      select the whole buffer        (one selection)
;;   <a-s>  split each multi-line selection into one selection per line
;;   )  (   rotate which selection is the main, forward / backward
;;   ,      keep only the main (or count-th) selection
;;   <a-,>  remove the main (or count-th) selection
;;   s      select all regex matches within the current selections
;;   S      split the current selections on regex matches
;;   <a-k>  keep selections that contain a regex match
;;   <a-K>  keep selections that do NOT contain a regex match
;;
;; The regex commands use the Emacs-native engine, confined to the three
;; boundary helpers below so a Kakoune-syntax translator can slot in later.  They
;; read through `kao--regex-command', so with `kao-incsearch' on (the faithful
;; default) the matches preview live as you type, restoring and re-applying from
;; the original selections each keystroke (Kakoune `regex_prompt' incsearch); the
;; same prompt backs `/'/`?' in kao-search.el.
;; Multi-selection edits and insert-replay are later P2 slices.
;;
;; Main-index rule after a list rebuild: faithful to Kakoune's
;; `SelectionList(buffer, vector)' constructor, which sets the main to the LAST
;; selection (selection.cc:27); `keep' resets it to 0; `remove' re-adjusts per
;; `SelectionList::remove' (selection.cc:40-44); `rotate' moves it modularly.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'kao-selection)
(require 'kao-state)
(require 'kao-register)
(require 'kao-history)                   ; `kao--snap-tag-*' selection-register tag accessors
(require 'kao-edit)                      ; column geometry + marker-army edits
(require 'kao-motion)                    ; faithful word category (capf)
(require 'kao-info)

;;;; Selection creation

(defun kao--keep-direction (begin end ref)
  "Return a selection over BEGIN..END (BEGIN <= END) with REF's direction.
Mirrors Kakoune `keep_direction' (selectors.cc): a forward REF yields
anchor=BEGIN/cursor=END, a backward REF yields anchor=END/cursor=BEGIN."
  (if (kao-sel-forward-p ref)
      (kao-sel-make :anchor begin :cursor end)
    (kao-sel-make :anchor end :cursor begin)))

(defun kao-select-whole-buffer ()
  "Kakoune `%': select the whole buffer as one selection.
Anchor at `point-min', cursor on the last char (`select_whole_buffer').  The
cursor gets the `max_column' sticky-eol TARGET (normal.cc:2317,
`{back_coord, max_column}'): a following `j'/`k' sticks to the newline."
  (interactive)
  (kao--assert-mode)
  (kao-set-selections-raw
   (list (kao-sel-make :anchor (point-min) :cursor (point-max) :target 'eol))
   0))

(defun kao-multi--split-sel-lines (sel &optional count)
  "Return SEL split into one selection per COUNT lines (Kakoune `split_lines').
COUNT (default 1, floored at 1) is Kakoune's N-lines-per-chunk count: `2<a-s>'
splits into two-line chunks (`normal.cc:1192-1218', `line += count').  A
single-line selection returns just itself.  Each chunk spans from the
selection min (first chunk) or the chunk's first bol to the selection max (last
chunk) or the chunk's last-line eol, preserving SEL's direction.  A final chunk
with fewer than COUNT lines is clamped to the buffer end."
  (let ((count (max 1 (or count 1)))
        (minp (kao-sel-min sel))
        (maxp (kao-sel-max sel)))
    (if (= (line-number-at-pos minp) (line-number-at-pos maxp))
        (list sel)
      (let ((chunks '())
            (line-bol (save-excursion (goto-char minp) (line-beginning-position))))
        (while (<= line-bol maxp)
          ;; eol of the last line in this COUNT-group (clamped to buffer end:
          ;; `forward-line' stops at eob, so a short final chunk ends there).
          (let ((eol (save-excursion (goto-char line-bol)
                                     (forward-line (1- count))
                                     (line-end-position))))
            (push (kao--keep-direction (max minp line-bol) (min maxp eol) sel) chunks))
          (let ((next (save-excursion (goto-char line-bol) (forward-line count) (point))))
            (setq line-bol (if (= next line-bol) (1+ maxp) next)))) ; eob: force exit
        (nreverse chunks)))))

(defun kao-split-lines ()
  "Kakoune `<a-s>': split each multi-line selection into one selection per line.
A count N splits into N-lines-per-chunk groups (`N<a-s>', `normal.cc:1192').
Single-line selections are left unchanged; the new main is the last
selection."
  (interactive)
  (kao--assert-mode)
  (let ((new (let ((count (kao--repeat-count)))
               (apply #'append
                      (mapcar (lambda (s) (kao-multi--split-sel-lines s count))
                              (kao-sels-list kao--sels))))))
    (kao-set-selections-raw new (max 0 (1- (length new))))))

;;;; Regex engine boundary (Emacs-native engine)

;; These three helpers are the *only* place that touches the regex engine.  The
;; regex strings are Emacs-native syntax; a Kakoune-syntax->Emacs translator
;; can later slot in here without changing any caller.  Kakoune regex is
;; case-sensitive by default, so the search helpers force `case-fold-search' nil
;; (Emacs buffers default to case-insensitive search).

(defun kao--match-captures ()
  "Return the current match data as a Kakoune `CaptureList' (list of strings).
Element 0 is the whole match, element k group k; an unmatched group reads
\"\" (Kakoune pushes the empty string for it, selectors.cc:1139-1141).
Emacs may omit trailing unmatched groups from `match-data' — unobservable
through the digit registers, whose out-of-range read is \"\" either way."
  (let ((n (/ (length (match-data)) 2))
        (caps '()))
    (dotimes (k n)
      (push (or (match-string k) "") caps))
    (nreverse caps)))

(defun kao--regex-matches-in (regex beg end &optional with-captures group)
  "Return a list of (MBEG . MEND) for every match of REGEX within [BEG, END).
Zero-width matches are KEPT (Kakoune's RegexIterator yields them too,
regex.hh:214-218), like the codebase's own `kao-object--rx-matches'; after a
null match at P the scan resumes at P+1, a one-char step approximating
Kakoune's NotInitialNull (a documented family deviation: Kakoune can still
yield a non-null match starting at P, e.g. `\\b\\|foo' at a word start; kao
resumes at P+1).  The advance is `goto-char', not `forward-char', so a null
match at `point-max' clamps instead of signalling `end-of-buffer' (the `s $'
crash); the `(< (point) END)' guard still guarantees termination.
With WITH-CAPTURES non-nil each element is (MBEG MEND CAPTURES) instead,
CAPTURES being the match's full submatch list (`kao--match-captures') —
the `select_matches' CaptureList (selectors.cc:1170-1176).  With GROUP (a
capture index, 0 = the whole match) each element is (MBEG MEND CAPTURES GB
GE), GB/GE being GROUP's bounds in that match or nil when the group did not
participate (Kakoune `capture.matched', selectors.cc:1168); CAPTURES is nil
there unless WITH-CAPTURES is also non-nil.  Iteration is always by whole
match, like the C++ RegexIterator."
  (let* ((cf (kao--regex-case-fold regex))   ;: (?i)/(?I) strip + fold policy
         (regex (car cf))
         (case-fold-search (cdr cf))
         (matches '()))
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (and (< (point) end) (re-search-forward regex end t))
          (let ((mb (match-beginning 0)) (me (match-end 0)))
            (push (cond
                   (group
                    (list mb me (and with-captures (kao--match-captures))
                          (match-beginning group) (match-end group)))
                   (with-captures (list mb me (kao--match-captures)))
                   (t (cons mb me)))
                  matches)
            ;; Advance past the match; a null match steps one char so the scan
            ;; terminates (NotInitialNull approximation).  `goto-char' clamps at
            ;; `point-max' — `forward-char' would signal there (the `s $' crash).
            (goto-char (if (> me mb) me (1+ me)))))))
    (nreverse matches)))

(defun kao--regex-search-in (regex beg end)
  "Return non-nil when REGEX matches anywhere within [BEG, END)."
  (let* ((cf (kao--regex-case-fold regex))   ;: (?i)/(?I) strip + fold policy
         (regex (car cf))
         (case-fold-search (cdr cf)))
    (save-excursion
      (save-match-data
        (goto-char beg)
        (re-search-forward regex end t)))))

(defun kao--valid-regex-p (regex)
  "Non-nil when REGEX compiles as an Emacs regexp (syntax)."
  (condition-case nil
      (progn (string-match-p regex "") t)
    (invalid-regexp nil)))

(defun kao--prompt-word-char-p (char)
  "Non-nil when CHAR is a kao word char (Kakoune `is_word', default Word).
The regex-prompt completer walks the current word with the SAME predicate
as the word motions (normal.cc:965 `is_word'), category."
  (and char (eq (kao-motion--category char) 'word)))

(defun kao--prompt-buffer-words (prefix)
  "Words starting with PREFIX in the prompt's originating buffer, capped at 100.
The native stand-in for Kakoune's buffer word-db lookup
\(`get_word_db(context.buffer()).find_matching', normal.cc:978-984,
`max_count = 100').  Word-START matches only (the word-db holds whole
words), deduplicated in buffer order.  DEVIATIONS (documented):
case-SENSITIVE prefix matching (the user's `completion-styles' may add
flex) stands in for the C++ RankedMatch, which is SMARTCASE — a lowercase
query char matches either case (`smartcase_eq', ranked_match.cc:93-95) —
and which returns the 100 BEST matches (`for_n_best', normal.cc:982); kao
returns the first 100 in buffer order from an on-demand scan, not an
incremental WordDB."
  (let ((buf (window-buffer (minibuffer-selected-window)))
        (seen (make-hash-table :test #'equal))
        (out '()) (n 0))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (while (and (< n 100)
                      (if (string-empty-p prefix)
                          ;; Empty prefix: every word (C++ find_matching("")).
                          (re-search-forward "[^ \t\f\n]" nil t)
                        (search-forward prefix nil t)))
            (let ((b (match-beginning 0)))
              (if (or (not (kao--prompt-word-char-p (char-after b)))
                      (kao--prompt-word-char-p (char-before b)))
                  ;; Not a word char, or a mid-word hit: resume past it.
                  (goto-char (1+ b))
                (let ((e (match-end 0)))
                  (while (kao--prompt-word-char-p (char-after e))
                    (setq e (1+ e)))
                  (let ((w (buffer-substring-no-properties b e)))
                    (unless (gethash w seen)
                      (puthash w t seen)
                      (push w out)
                      (setq n (1+ n))))
                  (goto-char e))))))))
    (nreverse out)))

(defun kao--regex-capf ()
  "Buffer-word `completion-at-point' function for the regex prompts.
Ports the `regex_prompt' completer (normal.cc:962-985): the bounds are the
current word — kao word chars walked back from point, and when the word is
preceded by an ODD number of backslashes its first char is an escape
\(`\\b' in `\\bfoo' — the C++ `backslashes % 2 == 1' substr, :969-975)
and is dropped from the prefix.  An empty word is allowed (completes
against every buffer word, capped).  Candidates come from the ORIGINATING
buffer via `kao--prompt-buffer-words'."
  (let* ((end (point))
         (base (minibuffer-prompt-end))
         (start end))
    (while (and (> start base)
                (kao--prompt-word-char-p (char-before start)))
      (setq start (1- start)))
    (let ((bs 0) (p start))
      (while (and (> p base) (eq (char-before p) ?\\))
        (setq p (1- p) bs (1+ bs)))
      (when (and (cl-oddp bs) (< start end))
        (setq start (1+ start))))
    (list start end
          (completion-table-dynamic #'kao--prompt-buffer-words)
          :exclusive 'no)))

(defun kao--regex-prompt-setup ()
  "Minibuffer setup for the REGEX prompts: base prompt map + word capf.
Composes `kao-regex-prompt-map' (TAB -> `completion-at-point', the
kao-keys table row) over the base `kao--prompt-setup' compose, and adds
`kao--regex-capf' buffer-locally.  Only the regex prompts use this — the
pipe prompt keeps `read-shell-command''s own completion and the
object-desc prompt has none (`complete_nothing', normal.cc:1489)."
  (kao--prompt-setup)
  (use-local-map (make-composed-keymap kao-regex-prompt-map
                                       (current-local-map)))
  (add-hook 'completion-at-point-functions #'kao--regex-capf nil t))

(defun kao--regex-prompt-default (reg)
  "The default regexp for a REGEX prompt: REG's main-indexed string, or nil.
Ports `regex_prompt' reading `default_regex' from the prompt's register at
entry (normal.cc:958): only a non-empty, compiling pattern is offered,
so `%s<ret>' re-applies the last pattern."
  (let ((d (and reg (kao--main-sel-register-value reg))))
    (and d (not (string-empty-p d)) (kao--valid-regex-p d) d)))

(defun kao--read-regex (prompt &optional reg)
  "Read an Emacs regexp from the minibuffer under PROMPT.
Return the string, or nil when it is empty or fails to compile (syntax).
When REG is a register char whose first string is a valid regexp, that pattern
is the empty-entry DEFAULT (Kakoune `regex_prompt' default_regex): an
empty entry returns it (shown as a `format-prompt' default), and the caller
must NOT re-store it (normal.cc:1016).  Read under the kao regex-prompt setup
\(`C-r' register insert; TAB buffer-word completion)."
  (let* ((default (kao--regex-prompt-default reg))
         (s (minibuffer-with-setup-hook #'kao--regex-prompt-setup
              (read-string (if default (format-prompt prompt default) prompt)))))
    (cond ((string-empty-p s) default)
          ((kao--valid-regex-p s) s)
          (t (message "kao: invalid regexp") nil))))

;;;; Incremental live-preview prompt (Kakoune incsearch, regex_prompt normal.cc:953)

;; Kakoune's regex prompts (`s'/`S'/`<a-k>'/`<a-K>' here, `/'/`?'/`<a-…>' in
;; kao-search) preview their effect as you type and default to incremental
;; (`incsearch' on).  `regex_prompt' runs the SAME selector function on every
;; keystroke and on validate, after resetting `selections_write_only() =
;; selections' — i.e. it re-applies from the ORIGINAL selections each time, never
;; cumulatively.  kao mirrors that exactly: it deep-copies the originals once,
;; and on each minibuffer edit restores that copy then re-applies the unchanged
;; `*-apply' fn.  Abort (C-g) restores; validate (RET) commits the final regex.
;; The `*-apply' fns are thus both preview and commit, untouched and already
;; batch-tested — only this live reader is new (smoke-tested like the renderer).

(defcustom kao-incsearch t
  "Preview regex matches live as you type (Kakoune incsearch, default on).
Each keystroke restores the original selections then re-applies the typed
regex, so the preview is never cumulative; aborting (\\[keyboard-quit])
restores them and
validating (RET) commits.  When nil the regex is read in one shot
\(`kao--read-regex') and applied once."
  :type 'boolean
  :group 'kao)

(defvar kao--incsearch-active nil
  "While an incsearch read is live, a thunk that previews the current minibuffer.
Let-bound for the dynamic extent of `kao--regex-prompt'; nil otherwise so the
shared minibuffer `post-command-hook' is a no-op for any other minibuffer read.")

(defun kao--incsearch-post-command ()
  "Minibuffer `post-command-hook': run the active incsearch preview, if any.
Named (not a closure) so re-adding it across reused minibuffers is idempotent;
the per-read preview closure lives in `kao--incsearch-active', nil when off."
  (when kao--incsearch-active
    (funcall kao--incsearch-active)))

(defun kao--restore-sels (orig)
  "Make a fresh deep copy of ORIG the live selection list, then refresh.
The copy (via `kao--snapshot-sels') decouples the live list from ORIG, so the
saved originals are never mutated by a later preview."
  ;; direct install (no-clamp): installs a verbatim deep-copy snapshot
  ;; (`kao--snapshot-sels'), not a `kao-sels-make' list; the seam would re-clamp
  ;; and sort-merge the restored originals, altering the saved list.
  (setq kao--sels (kao--snapshot-sels orig))
  (kao--refresh))

(defun kao--incsearch-preview (apply-fn orig regex &optional win)
  "Restore ORIG then preview REGEX via APPLY-FN: the non-cumulative incsearch step.
`kao--sels' is reset to a fresh deep copy of ORIG (Kakoune
`selections_write_only() = selections'); when REGEX is non-empty and compiles,
APPLY-FN (an unchanged *-apply fn) transforms `kao--sels' and refreshes from
those originals, otherwise the restored originals are shown.  WIN, when a live
window, is the target window: the live read selects the minibuffer, so WIN's
point would not follow the preview — `set-window-point' nudges it to the
previewed main cursor.  Viewport clipping is `kao--refresh's job now (run by
APPLY-FN, or the else branch): it renders through `kao--window-ranges', which
includes the displayed-but-unselected WIN, so no explicit
re-clip render remains here.  WIN is nil in batch, where this is skipped."
  ;; direct install (no-clamp): verbatim deep-copy snapshot restore
  ;; (`kao--snapshot-sels'); the seam would re-clamp/merge it, and refresh is
  ;; conditional here (APPLY-FN refreshes on a compiling regex, else below).
  (setq kao--sels (kao--snapshot-sels orig))
  (if (and regex (not (string-empty-p regex)) (kao--valid-regex-p regex))
      (funcall apply-fn regex)
    (kao--refresh))
  (when (window-live-p win)
    (set-window-point win (point))))

(defun kao--regex-prompt (prompt apply-fn &optional reg)
  "Read a regexp under PROMPT with live incremental preview, returning it or nil.
Ports Kakoune `regex_prompt' (normal.cc:953).  Saves the selections; a
buffer-local minibuffer `post-command-hook' (`kao--incsearch-post-command')
previews each edit via `kao--incsearch-preview' (APPLY-FN re-applied from the
saved originals).  Quit (\\[keyboard-quit]) restores and returns nil; a
normal exit commits
the final regex (re-applied from the originals) and returns it, or restores and
returns nil when empty/invalid.  On a non-nil return `kao--sels' equals APPLY-FN
applied to that regexp; on nil it is unchanged.

When REG is a register char with a valid stored pattern, that pattern is the
empty-entry DEFAULT (Kakoune `default_regex'): it is shown via
`format-prompt', and validating an empty entry applies and returns it (the
caller must not re-store it, normal.cc:1016).

The preview is memoized and abortable, mirroring Kakoune's
`m_line_changed' gate and 50 ms idle coalescing (input_handler.cc:689-698/:790):
a per-read `last' skips the preview when the minibuffer contents are unchanged,
and the preview runs inside `while-no-input' so a pending keystroke abandons a
stale preview (the next post-command recomputes from the final text).  `last' is
a non-string sentinel so the first (empty-contents) fire still previews; it
advances only after a preview that ran to completion, never after an aborted
one."
  (let* ((target (current-buffer))
         (win (get-buffer-window target))
         (orig (kao--snapshot-sels kao--sels))
         (default (kao--regex-prompt-default reg))
         (last 'none)                        ;: last previewed contents
         (kao--incsearch-active
          (lambda ()
            (let ((rx (minibuffer-contents-no-properties)))
              (unless (equal rx last)        ; unchanged: nothing to re-preview
                (with-current-buffer target
                  ;; `while-no-input' returns t if a keystroke aborted the
                  ;; preview; only then leave `last' so the next fire recomputes.
                  (unless (while-no-input
                            (kao--incsearch-preview apply-fn orig rx win))
                    (setq last rx))))))))
    (condition-case nil
        (let ((typed
               (minibuffer-with-setup-hook
                   (lambda ()
                     (kao--regex-prompt-setup) ; C-r + word capf
                     (add-hook 'post-command-hook
                               #'kao--incsearch-post-command nil t))
                 (let ((highlight-nonselected-windows t))
                   (read-from-minibuffer
                    (if default (format-prompt prompt default) prompt))))))
          ;; A non-empty valid entry commits itself; an empty entry falls back
          ;; to the register's default (normal.cc:1021); both re-apply from the
          ;; originals.  Anything else restores and aborts.
          (let ((rx (cond ((and (not (string-empty-p typed))
                                (kao--valid-regex-p typed))
                           typed)
                          ((string-empty-p typed) default))))
            (if rx
                (progn (kao--incsearch-preview apply-fn orig rx) rx)
              ;; A non-empty TYPED pattern only reaches here when
              ;; `kao--valid-regex-p' rejected it (a valid one is RX above; an
              ;; empty entry with no register default is a silent cancel, as in
              ;; Kakoune).  Report the invalid commit like Kakoune/isearch do
              ;;  — the live preview refused it without feedback.
              (unless (string-empty-p typed)
                (message "kao: invalid regexp"))
              (kao--restore-sels orig)
              nil)))
      (quit (kao--restore-sels orig) nil))))

(defun kao--regex-command (prompt apply-fn)
  "Read a regexp under PROMPT, apply it via APPLY-FN; return the regexp or nil.
This is the port of `regex_prompt' (normal.cc:1014-1018) and owns its shared
plumbing for all six regex-prompt commands (`s'/`S'/`<a-k>'/`<a-K>' here,
`/'/`?' in kao-search): BEFORE the prompt it resolves the pending-or-`/'
register and snapshots the pre-prompt selections; on a committed (non-nil)
regexp it pushes a jump of those originals and stores the regexp in the
register.  So `s foo RET' leaves \"foo\" in `/' (`n'/`N' then repeat it) and a
pending `\"a' redirects both the store and the read to register a.

The register is lowered at the consumer site (Kakoune
`to_lower(params.reg ? params.reg : \\='/\\=')', normal.cc:1163/1178/1277 and
the search family, `kao--search-register-arg').  It is also the empty-entry
DEFAULT source (Kakoune `default_regex'): an empty prompt re-applies the
register's stored pattern (`/<ret>', `%s<ret>') WITHOUT rewriting it
\(normal.cc:1016).  The snapshot mirrors `regex_prompt' restoring the originals
before `push_jump' (the incsearch preview mutates `kao--sels' as you type, but
the jump must capture where you were, not the match).  Abort
\(quit/both-empty) writes nothing and pushes no jump.

With `kao-incsearch' (the faithful default) the read is the live
`kao--regex-prompt'; otherwise `kao--read-regex' is read once and applied.  Both
readers take REG so the empty-entry default resolves and displays.  APPLY-FN is
one of the unchanged, batch-tested *-apply fns (both preview and commit).
Callers keep only their command-specific tail (kao-search adds the
highlight; the multi commands add nothing — keeps the highlight
search-only)."
  (let* ((reg (downcase (kao--register-arg ?/)))
         (default (kao--regex-prompt-default reg))
         (before (kao--snapshot-sels kao--sels))
         (regex (if kao-incsearch
                    (kao--regex-prompt prompt apply-fn reg)
                  (let ((rx (kao--read-regex prompt reg)))
                    (when rx (funcall apply-fn rx))
                    rx))))
    (when regex
      (kao--jump-push before)              ; regex_prompt push_jump on Validate
      ;; Only a freshly typed entry writes the register; an empty-entry fallback
      ;; to DEFAULT re-applies it but leaves it untouched (normal.cc:1016).
      (unless (equal regex default)
        (kao-register-set reg (list regex))))
    regex))

;;;; Regex selection — s (select matches)

(defun kao--capture-invalid-p (regex capture)
  "Non-nil (after messaging) when CAPTURE is not a group index of REGEX.
Ports the `s'/`S' guard (selectors.cc:1153-1155/1193-1194): the index must
not exceed the regex's number of capturing groups (`regexp-opt-depth', the
Emacs-regex boundary; explicitly-numbered `\\(?9:…\\)' groups are NOT
counted by it, so their indices are rejected — documented in).
Kakoune throws runtime_error
\"invalid capture number\"; kao messages it and leaves the selection list
unchanged, the file-local \"nothing selected\" precedent — the apply runs
inside the incsearch preview loop, where a signal would break the
minibuffer."
  (when (> capture (regexp-opt-depth regex))
    (message "kao: invalid capture number")
    t))

(defun kao--select-regex-apply (regex &optional capture)
  "Replace the selection list with REGEX matches inside each selection.
Each match [mb,me) becomes a selection [mb, me-1] (cursor on the last matched
char), direction preserved; results are sorted and main becomes the last.
A regex matching nothing leaves the selection list unchanged (Kakoune throws
\"nothing selected\").  Each result carries the match's full submatch list as
its captures (`select_matches', selectors.cc:1170-1176) — read back by the
digit registers `0'-`9'.

With CAPTURE (Kakoune `params.count', normal.cc:1163) a positive group
index, the result span is that GROUP of each match (selectors.cc:1167-1181):
a match where the group did not participate, or whose group starts at the
selection's exclusive end (`capture.first == sel_end'), is skipped; a
zero-width group yields a single-position selection at the group start
\(`begin == end ? end : prev(end)'); the full submatch list is carried as
captures either way.  CAPTURE nil/0 selects the whole match, exactly the
earlier behaviour."
  (let ((capture (or capture 0)))
    (unless (kao--capture-invalid-p regex capture)
      (let ((result '()))
        (dolist (sel (kao-sels-list kao--sels))
          (let ((sel-end (kao-sel-end sel)))
            (dolist (m (kao--regex-matches-in regex (kao-sel-beg sel) sel-end
                                              t capture))
              (let ((gb (nth 3 m)) (ge (nth 4 m)))
                (when (and gb (/= gb sel-end))
                  (let ((s2 (kao--keep-direction
                             gb (if (= ge gb) gb (1- ge)) sel)))
                    (setf (kao-sel-captures s2) (nth 2 m))
                    (push s2 result)))))))
        (if (null result)
            (message "kao: nothing selected")
          (let ((lst (kao-sels-list
                      (kao-sels-sort (kao-sels-make :list (nreverse result)
                                                    :main 0)))))
            ;; LST is already sorted; the -raw seam installs it verbatim (no
            ;; merge) and clamps each member (a no-op on the built list).
            (kao-set-selections-raw lst (1- (length lst)))))))))

(defun kao-select-regex ()
  "Kakoune `s': select all matches of a regex within the current selections.
A count is the capture index: `2s' selects each match's group 2, skipping
matches where the group did not participate (`select_regex',
normal.cc:1162-1175)."
  (interactive)
  (kao--assert-mode)
  (let ((capture kao--count))
    (kao--regex-command
     (if (> capture 0) (format "select (capture %d):" capture) "select:")
     (lambda (rx) (kao--select-regex-apply rx capture)))))

;;;; Regex split — S (split on matches)

(defun kao--split-regex-apply (regex &optional capture)
  "Replace the selection list with the segments BETWEEN REGEX matches.
For each selection, the matches act as delimiters; the text between them (and
the trailing text) becomes selections, direction preserved.  Mirrors Kakoune
`split_on_matches' (selectors.cc:1191-1224): a segment is pushed for a match
unless it starts exactly at the selection min, using a single-position selection
when two matches are adjacent.  No sort; main becomes the last.  A regex
matching nothing keeps each whole selection as one segment.

With CAPTURE (Kakoune `params.count', normal.cc:1180) a positive group
index, that GROUP's span is the delimiter (selectors.cc:1205-1217): the
segment ends at the group start (`end = capture.first') and the next one
begins at the group end (`begin = capture.second'), so match characters
outside the group stay in the surrounding segments; a group starting at
`point-max' is not a delimiter (`end == buf_end' guard, :1209-1210), and
neither is a match whose group did not participate (documented
deviation — the C++ has no `matched' check here, its behaviour for an
unmatched submatch iterator is not determinable from the source; kao
mirrors the select path's skip).  Segments carry no captures (the C++
pushes bare `{begin, end}').  CAPTURE nil/0 splits on the whole match,
exactly the earlier behaviour."
  (let ((capture (or capture 0)))
    (unless (kao--capture-invalid-p regex capture)
      (let ((result '()))
        (dolist (sel (kao-sels-list kao--sels))
          (let ((sel-min (kao-sel-min sel))
                (begin (kao-sel-min sel))
                (smax (kao-sel-max sel)))
            (dolist (m (kao--regex-matches-in regex (kao-sel-beg sel)
                                              (kao-sel-end sel) nil capture))
              (let ((gb (nth 3 m)) (ge (nth 4 m)))
                (when (and gb (/= gb (point-max)))
                  (when (/= gb sel-min)
                    (push (kao--keep-direction
                           begin (if (= begin gb) gb (1- gb)) sel)
                          result))
                  (setq begin ge))))
            (when (<= begin smax)
              (push (kao--keep-direction begin smax sel) result))))
        (if (null result)
            (message "kao: nothing selected")
          (let ((lst (nreverse result)))
            (kao-set-selections-raw lst (1- (length lst)))))))))

(defun kao-split-regex ()
  "Kakoune `S': split the current selections on regex matches.
A count is the capture index: `1S' splits on each match's group 1, the
delimiter being the group's span rather than the whole match
\(`split_regex', normal.cc:1177-1190)."
  (interactive)
  (kao--assert-mode)
  (let ((capture kao--count))
    (kao--regex-command
     (if (> capture 0) (format "split (on capture %d):" capture) "split:")
     (lambda (rx) (kao--split-regex-apply rx capture)))))

;;;; Regex filter — <a-k> keep / <a-K> keep-not-matching

(defun kao--keep-apply (regex matching)
  "Keep selections whose containing-a-match status equals MATCHING.
MATCHING non-nil (`<a-k>') keeps selections that contain a REGEX match;
nil (`<a-K>') keeps those that do not (`keep', normal.cc:1273-1303).  Order is
preserved and main becomes the last kept.  When nothing is kept the
selection list is left unchanged (Kakoune throws \"no selections remaining\")."
  (let ((kept '()))
    (dolist (sel (kao-sels-list kao--sels))
      (let ((found (and (kao--regex-search-in regex (kao-sel-beg sel) (kao-sel-end sel)) t)))
        (when (eq found matching)
          (push sel kept))))
    (if (null kept)
        (message "kao: no selections remaining")
      (let ((lst (nreverse kept)))
        (kao-set-selections-raw lst (1- (length lst)))))))

(defun kao-keep-matching ()
  "Kakoune `<a-k>': keep only selections containing a regex match."
  (interactive)
  (kao--assert-mode)
  (kao--regex-command "keep matching:" (lambda (rx) (kao--keep-apply rx t))))

(defun kao-keep-not-matching ()
  "Kakoune `<a-K>': keep only selections NOT containing a regex match."
  (interactive)
  (kao--assert-mode)
  (kao--regex-command "keep not matching:" (lambda (rx) (kao--keep-apply rx nil))))

;;;; Main management — rotate / keep / remove

(defun kao--rotate-main (count forward)
  "Rotate the main index of `kao--sels' by COUNT (FORWARD non-nil = `)').
Modular, mirroring Kakoune `rotate_selections' (normal.cc:1657-1666).  No-op
when there are fewer than two selections."
  (let* ((list (kao-sels-list kao--sels))
         (num (length list))
         (main (kao-sels-main kao--sels)))
    (when (> num 1)
      ;; Re-install the SAME list with a rotated main (clamp is a no-op on the
      ;; live, already-valid selections).
      (kao-set-selections-raw
       list
       (if forward
           (mod (+ main count) num)
         (mod (+ main (- num (mod count num))) num))))))

(defun kao-rotate-selections-forward ()
  "Kakoune `)': make the next selection (by COUNT) the main."
  (interactive)
  (kao--assert-mode)
  (kao--rotate-main (kao--repeat-count) t))

(defun kao-rotate-selections-backward ()
  "Kakoune `(': make the previous selection (by COUNT) the main."
  (interactive)
  (kao--assert-mode)
  (kao--rotate-main (kao--repeat-count) nil))

(defun kao--count-index (default)
  "Return the 0-based selection index a count selects, or DEFAULT when unset.
Kakoune commands read a 1-based `count'; with no count they act on DEFAULT
\(usually the main index)."
  ;; Not `kao--count-or': the count-set branch is `(1- kao--count)', not
  ;; `kao--count' — routing would need `(1- (kao--count-or (1+ default)))',
  ;; changing the `1-' shape and hurting readability (left as-is).
  (if (> kao--count 0) (1- kao--count) default))

(defun kao-keep-selection ()
  "Kakoune `,': keep only the main selection (or the count-th, 1-based).
The result is a single selection with main 0; an out-of-range count is rejected."
  (interactive)
  (kao--assert-mode)
  (let* ((list (kao-sels-list kao--sels))
         (index (kao--count-index (kao-sels-main kao--sels))))
    (if (or (< index 0) (>= index (length list)))
        (message "kao: invalid selection index %d" (1+ index))
      (kao-set-selections-raw (list (nth index list)) 0))))

(defun kao-remove-selection ()
  "Kakoune `<a-,>': remove the main selection (or the count-th, 1-based).
Refuses when only one selection remains.  The main re-adjusts per Kakoune
`SelectionList::remove' (selection.cc:40-44): after erasing INDEX,
decrement the main when INDEX precedes it or it now points past the end."
  (interactive)
  (kao--assert-mode)
  (let* ((list (kao-sels-list kao--sels))
         (size (length list))
         (main (kao-sels-main kao--sels))
         (index (kao--count-index main)))
    (cond
     ((or (< index 0) (>= index size))
      (message "kao: invalid selection index %d" (1+ index)))
     ((= size 1)
      (message "kao: no selections remaining"))
     (t
      (let* ((new (append (seq-take list index) (seq-drop list (1+ index))))
             (new-size (1- size))
             ;; Kakoune `remove' (selection.cc:40-44): new-size here is the
             ;; POST-erase size; this keeps new-main in [0, new-size-1].
             (new-main (if (or (< index main) (= main new-size)) (1- main) main)))
        (kao-set-selections-raw new new-main))))))

;;;; Select boundaries (<a-S>)

(defun kao-select-boundaries ()
  "Kakoune `<a-S>': select the first and last character of each selection.
Each selection contributes its first char (min) as a one-char selection, plus
its last char (max) when the selection is longer than one char; a single-char
selection stays one (`select_boundaries', normal.cc:1220).  The new list is
sorted (no merge); main is the last boundary (`operator=(Vector)' →
`set(list, size-1)', selection.hh:126 / selection.cc:54)."
  (interactive)
  (kao--assert-mode)
  (let ((res '()))
    (dolist (sel (kao-sels-list kao--sels))
      (let ((mn (kao-sel-min sel)) (mx (kao-sel-max sel)))
        (push (kao-sel-make :anchor mn :cursor mn) res)
        (unless (= mn mx)
          (push (kao-sel-make :anchor mx :cursor mx) res))))
    (setq res (nreverse res))
    ;; direct install (no-merge): sorts WITHOUT merging — a shape neither seam
    ;; offers (`kao-set-selections' always merges, collapsing adjacent boundary
    ;; cursors; -raw never sorts).  Boundaries are on-char, no clamp needed.
    (setq kao--sels (kao-sels-sort
                     (kao-sels-make :list res :main (1- (length res)))))
    (kao--refresh)))

;;;; Copy selections onto following / preceding lines (C / <a-C>)

(defun kao--line-bounds-rel (pos off)
  "Return (BOL . LINEEND) of the line OFF lines from POS's line, else nil.
Relative walk: `forward-line' from POS by OFF, returning the bounds
only when it moved the full OFF lines AND landed on a `bolp' — the `(bolp)'
guard is required, since `forward-line' also returns 0 when it stops at
`point-max' over a final line with no trailing newline, and that overshoot
must read as \"no such line\" (reproducing the old `[1, maxline]' guard
exactly).  LINEEND is the position after the line's newline, or `point-max'
for the last line (so an empty line has BOL = LINEEND).  Cost is O(|OFF|)
local line steps, independent of buffer height."
  (save-excursion
    (goto-char pos)
    (when (and (zerop (forward-line off)) (bolp))
      (cons (point) (progn (forward-line 1) (point))))))

(defun kao--copy-on-lines (forward)
  "Duplicate each selection onto the following (FORWARD) / preceding lines.
Each copy preserves the anchor and cursor visual columns and the selection's
line height; up to `kao--repeat-count' copies are made per selection, skipping a
target line where either column does not exist and stopping at the buffer edge
\(Kakoune `copy_selections_on_next_lines', normal.cc:1604).  The list is then
sorted and merged; main follows the last copy of the old main.  Placement is by
the REAL visual column (normal.cc:1604, NOT the goal column); the source
cursor's `target' (sticky goal column) is propagated into the copies
\(normal.cc:1646), so an `x'/`<a-l>'-seeded sticky column survives the copy.

Target lines are resolved RELATIVE to each selection's own anchor/cursor
\(`kao--line-bounds-rel'): the copy is O(selections × copies × |offset|)
local line steps, not O(… × buffer-lines) — with no `line-number-at-pos' and
no walk from `point-min'."
  (let* ((sels (kao-sels-list kao--sels))
         (omain (kao-sels-main kao--sels))
         (maxn (kao--repeat-count))
         (dir (if forward 1 -1))
         (result '())
         (count 0)                       ; running length of RESULT (final index)
         (main-index 0))
    (cl-loop for sel in sels for si from 0 do
      (let* ((a (kao-sel-anchor sel)) (c (kao-sel-cursor sel))
             (a-col (kao--visual-column a)) (c-col (kao--visual-column c))
             ;; Height = the selection's line span = 1 + the newlines it covers,
             ;; counted locally in [min, max) (no absolute line numbers): O(len),
             ;; matching the old `1 + |a-line - c-line|'.
             (height (1+ (save-excursion
                           (goto-char (min a c))
                           (let ((n 0) (lim (max a c)))
                             (while (search-forward "\n" lim t) (setq n (1+ n)))
                             n)))))
        (when (= si omain) (setq main-index count))
        (push (kao-sel-copy sel) result)
        (setq count (1+ count))
        (let ((nb 0) (i 0) (stop nil))
          (while (and (not stop) (< nb maxn))
            (let* ((off (* dir (1+ i) height))
                   (ab (kao--line-bounds-rel a off))
                   (cb (kao--line-bounds-rel c off)))
              (setq i (1+ i))
              (if (or (null ab) (null cb))
                  (setq stop t)           ; ran off the buffer — stop this sel
                (let ((apos (kao--column-to-pos (car ab) (cdr ab) a-col))
                      (cpos (kao--column-to-pos (car cb) (cdr cb) c-col)))
                  (when (and (< apos (cdr ab)) (< cpos (cdr cb))) ; both columns fit
                    (when (= si omain) (setq main-index count))
                    (push (kao-sel-make :anchor apos :cursor cpos
                                        :target (kao-sel-target sel))
                          result)
                    (setq count (1+ count) nb (1+ nb))))))))))
    ;; Install through the sort+merge seam: `kao-set-selections' clamps each end
    ;; (`kao--clamp-sel'), sort-and-merges overlapping copies, clamps MAIN into
    ;; range, and refreshes — identical to the prior hand-rolled clamp+merge+
    ;; refresh (both ends clamp there anyway), so routing is byte-identical.
    ;; MAIN-INDEX is set to COUNT (the running RESULT length tracked above) at
    ;; the old main's last copy, so it is always in [0, COUNT); the guard makes
    ;; that invariant explicit (it never fires on a real path — an empty RESULT
    ;; is the only null-main case).  COUNT is the O(1) length already at hand.
    (cl-assert (or (null result) (< main-index count)) t
               "kao--copy-on-lines: main-index %d out of range for %d sels"
               main-index count)
    (kao-set-selections (nreverse result) main-index)))

(defun kao-copy-selections-down ()
  "Kakoune `C': duplicate each selection onto the `count' lines that follow it."
  (interactive)
  (kao--assert-mode)
  (kao--copy-on-lines t))

(defun kao-copy-selections-up ()
  "Kakoune `<a-C>': duplicate each selection onto the `count' lines preceding it."
  (interactive)
  (kao--assert-mode)
  (kao--copy-on-lines nil))

;;;; Selection registers — Z save / z restore

;; Kakoune `Z' (save_selections, normal.cc:2116) writes the whole selection list
;; to a register (default `^'); `z' (restore_selections, :2147) reads it back.
;; kao stores native `kao-sels' snapshots (kao-register.el).  Cross-buffer
;; (resolves the deferral): `z' restore from another buffer
;; SWITCHES to it first — Kakoune `restore_selections' calls
;; `context.change_buffer(selections.buffer())' (normal.cc:2164-2165) — while
;; `<a-z>'/`<a-Z>' combine refuses with Kakoune's exact wording, thrown on
;; entry to `combine_selections' before the op key is read (normal.cc:2074).
;; A killed source buffer signals the faithful `BufferManager::get_buffer'
;; error (buffer_manager.cc:77).  Still deferred: the buffer-timestamp
;; staleness fix (`SelectionList::update') — restore clamps each selection to
;; current bounds, faithful while the buffer is unchanged since the save.

(defun kao--read-selection-register (&optional char)
  "Return the (BUFFER ID . SELS) entry saved in selection register CHAR.
Returns nil (after a status message) when the register is empty; signals the
faithful \"no such buffer\" `user-error' when the source buffer was killed —
Kakoune resolves the saved buffer by name first
\(`read_selections_from_register' normal.cc:2009 via `get_buffer',
buffer_manager.cc:77).  Documented accommodation (family): Kakoune
quotes the buffer NAME in the error; a killed Emacs buffer has no name, so
kao drops it."
  (let ((entry (kao-register-get-selections char))
        (reg (or char kao-selection-register-default)))
    (cond
     ((null entry)
      (message "kao: selection register %c is empty" reg) nil)
     ((not (buffer-live-p (kao--snap-tag-buffer entry)))
      (user-error "no such buffer"))
     (t entry))))

(defun kao--set-sels-from (sels)
  "Make SELS the live selection list, clamped to the current buffer, then refresh.
Each selection is rebuilt by `kao--clamp-sel' (fresh structs), so the live list
does not share structure with a stored snapshot."
  (kao-set-selections-raw (kao-sels-list sels) (kao-sels-main sels)))

(defun kao--selection-register-arg ()
  "The pending or default `^' selection register, lowered and validated.
Ports `to_lower(params.reg ? params.reg : \\='^\\=')' plus the alpha-only
guard (save_selections normal.cc:2117-2120 and
`read_selections_from_register' :1989-1991, so all of `Z'/`z'/`<a-Z>'/
`<a-z>' share it): selections live only in `^' and the alphabetic
registers."
  (let ((reg (downcase (kao--register-arg kao-selection-register-default))))
    (unless (or (<= ?a reg ?z) (eq reg ?^))
      (user-error "selections can only be saved to the '^' and alphabetic registers"))
    reg))

(defun kao-save-selections ()
  "Kakoune `Z': save the selection list to the pending or `^' register.
A snapshot (deep copy) is stored, tagged with the current buffer."
  (interactive)
  (kao--assert-mode)
  (let ((reg (kao--selection-register-arg)))
    (kao-register-save-selections kao--sels reg)
    (message "kao: saved %d selections to register %c"
             (length (kao-sels-list kao--sels)) reg)))

(defun kao-restore-selections ()
  "Kakoune `z': replace the selection list with the pending or `^' register's.
A register saved in another buffer switches to that buffer first (Kakoune
`restore_selections' change_buffer, normal.cc:2164-2165); a live target with
`kao-mode' disabled aborts loudly (slice-10 plan decision, as for jumps).
Selections are coordinate-translated through the edits made since the save
\(`kao--snapshot-update' = the restored list's `update') — after the
buffer switch, so the saved id reads its own buffer's tree."
  (interactive)
  (kao--assert-mode)
  (let* ((reg (kao--selection-register-arg))
         (entry (kao--read-selection-register reg)))
    (when entry
      (let ((buf (kao--snap-tag-buffer entry)))
        (unless (eq buf (current-buffer))
          (unless (buffer-local-value 'kao-mode buf)
            (user-error "target buffer is not in kao-mode"))
          (switch-to-buffer buf))
        (let ((sels (kao--snapshot-update (kao--snap-tag-snap entry) (kao--snap-tag-id entry))))
          (kao--set-sels-from sels)
          (message "kao: restored %d selections from register %c"
                   (length (kao-sels-list sels)) reg))))))

;;;; Combine selections — pure algebra (combine_selection, normal.cc:2039)

(defun kao--combine-sel (a b op)
  "Combine selections A (register) and B (current) under OP, returning one sel.
Mirrors Kakoune `combine_selection' (normal.cc:2039), which mutates A using B as
the other operand, so the register selection A wins ties.  OP is one of the
symbols `union' `intersect' `leftmost' `rightmost' `longest' `shortest'.  Union
and intersect set anchor=combined-min / cursor=combined-max (forward), matching
`Selection::set(min,max)'; a non-overlapping intersection therefore yields an
inverted (anchor > cursor) selection, exactly as Kakoune does."
  (pcase op
    ('union     (kao-sel-make :anchor (min (kao-sel-min a) (kao-sel-min b))
                              :cursor (max (kao-sel-max a) (kao-sel-max b))))
    ('intersect (kao-sel-make :anchor (max (kao-sel-min a) (kao-sel-min b))
                              :cursor (min (kao-sel-max a) (kao-sel-max b))))
    ('leftmost  (if (> (kao-sel-cursor a) (kao-sel-cursor b)) b a))
    ('rightmost (if (< (kao-sel-cursor a) (kao-sel-cursor b)) b a))
    ('longest   (if (< (kao-sel-length a) (kao-sel-length b)) b a))
    ('shortest  (if (> (kao-sel-length a) (kao-sel-length b)) b a))
    (_ (error "kao: unknown combine op %S" op))))

(defun kao--combine-selections (reg-sels cur-sels op)
  "Combine REG-SELS (a register's) with CUR-SELS (current) under OP -> kao-sels.
Faithful to Kakoune `combine_selections' (normal.cc:2072).  `append' joins the
register list and the current list, sets the main to the current main offset
past the register list, then sort-and-merges overlapping (main tracked by
identity).  Every other OP combines pairwise by index and REQUIRES the two lists
to be the same length (Kakoune throws otherwise), keeping the current main."
  (let ((rl (kao-sels-list reg-sels))
        (cl (kao-sels-list cur-sels)))
    (if (eq op 'append)
        (kao-sels-sort-and-merge-overlapping
         (kao-sels-make
          :list (append (mapcar #'kao-sel-copy rl) (mapcar #'kao-sel-copy cl))
          :main (+ (length rl) (kao-sels-main cur-sels))))
      (unless (= (length rl) (length cl))
        (error "kao: selection lists differ in length (%d vs %d)"
               (length rl) (length cl)))
      (kao-sels-make
       :list (cl-mapcar (lambda (a b) (kao--combine-sel a b op)) rl cl)
       :main (kao-sels-main cur-sels)))))

;;;; Combine menu — <a-z> combine-from / <a-Z> combine-to (read-key)

;; Kakoune's combine step is a one-shot next-key (`on_next_key_with_autoinfo'
;; with KeymapMode::Combine, normal.cc:2077) — the same shape as goto/view, so
;; kao reads ONE op key rather than installing a sub-keymap.  `<a-z>'
;; (restore_selections<true>, :2147) combines the register's selections with the
;; current ones and makes the result live; `<a-Z>' (save_selections<true>, :2116)
;; combines and saves the result back to the register (a plain save when the
;; register is empty, Kakoune's `if (combine and not empty)' fallback).

(defconst kao--combine-table
  (list (cons ?a 'append)
        (cons ?u 'union)
        (cons ?i 'intersect)
        (cons ?< 'leftmost)
        (cons ?> 'rightmost)
        (cons ?+ 'longest)
        (cons ?- 'shortest))
  "Alist mapping a combine sub-key to an op symbol.
Kakoune `key_to_combine_op' (normal.cc:2106-2112): `a' append, `u' union,
`i' intersection, `<'/`>' select
leftmost/rightmost cursor, `+'/`-' select longest/shortest.")

(defconst kao--combine-info
  '((?a . "append lists")
    (?u . "union")
    (?i . "intersection")
    (?< . "select leftmost cursor")
    (?> . "select rightmost cursor")
    (?+ . "select longest")
    (?- . "select shortest"))
  "Autoinfo rows (EVENT . DOCSTRING) for the combine menu.
Kakoune's `KeyInfo' built-ins for `combine_selections' (normal.cc:2105),
all of which kao implements.  Its keys are kept in lockstep with
`kao--combine-table' by a parity test.")

(defun kao--read-combine-op (prompt)
  "Read ONE combine sub-key under PROMPT, returning its op symbol or nil.
An unknown key or escape cancels (Kakoune returns on `Escape')."
  (cdr (assq (kao-info--with-box prompt kao--combine-info (kao--read-key prompt))
             kao--combine-table)))

(defun kao--combine-entry-check (entry)
  "Signal the faithful cross-buffer combine error for ENTRY when needed.
Kakoune throws on entry to `combine_selections', BEFORE the op key is read
\(normal.cc:2074-2075); exact wording."
  (unless (eq (kao--snap-tag-buffer entry) (current-buffer))
    (user-error "cannot combine selections from different buffers")))

(defun kao-combine-from-register ()
  "Kakoune `<a-z>': combine the pending or `^' register with the current sels.
Read a combine op; the result becomes the live selection list.  A
register saved in another buffer refuses with Kakoune's exact wording before
the op key is read (normal.cc:2074-2075); a pairwise size mismatch aborts
with a message."
  (interactive)
  (kao--assert-mode)
  (let ((entry (kao--read-selection-register (kao--selection-register-arg))))
    (when entry
      (kao--combine-entry-check entry)
      (let ((saved (kao--snapshot-update (kao--snap-tag-snap entry) (kao--snap-tag-id entry)))
            (op (kao--read-combine-op "combine from:")))
        (when op
          (condition-case err
              (kao--set-sels-from (kao--combine-selections saved kao--sels op))
            (error (message "%s" (error-message-string err)))))))))

(defun kao-combine-to-register ()
  "Kakoune `<a-Z>': combine the current selections into the pending or `^' reg.
When the register is empty this is a plain save (Kakoune's fallback); otherwise
read a combine op and save the combined list back.  A register saved in another
buffer refuses with Kakoune's exact wording (normal.cc:2074-2075)."
  (interactive)
  (kao--assert-mode)
  (let* ((reg (kao--selection-register-arg))
         (entry (kao-register-get-selections reg)))
    (if (null entry)
        (kao-save-selections)                  ; empty register -> plain save
      (let ((entry (kao--read-selection-register reg)))  ; dead-buffer check
        (when entry
          (kao--combine-entry-check entry)
          (let ((saved (kao--snapshot-update (kao--snap-tag-snap entry) (kao--snap-tag-id entry)))
                (op (kao--read-combine-op "combine to:")))
            (when op
              (condition-case err
                  (let ((combined (kao--combine-selections saved kao--sels op)))
                    (kao-register-save-selections combined reg)
                    (message "kao: combined into register %c (%d selections)"
                             reg (length (kao-sels-list combined))))
                (error (message "%s" (error-message-string err)))))))))))

;;;; Merge / trim / duplicate — pure selection-list algebra
;;
;; <a-_> merge_consecutive (normal.cc:2377) / <a-+> merge_overlapping (:2384) each
;; `ensure_forward' every selection, then merge in list order — consecutive =
;; touching-or-overlapping, overlapping = overlapping only.  Neither sorts: Kakoune
;; assumes the list is already min-sorted (kao's invariant), and ensure-forward
;; leaves each min/max unchanged so the order holds.  Count is ignored.

(defun kao--ensure-forward-then (merge-fn)
  "Ensure every selection forward, then install (MERGE-FN list) and refresh.
Faithful to the `ensure_forward' prefix of `merge_consecutive' /
`merge_overlapping' (normal.cc:2377/2384): flip each selection forward
\(anchor<=cursor), then merge in list order.  No re-sort — kao keeps the list
min-sorted and ensure-forward preserves each selection's min/max."
  (let* ((fwd (mapcar #'kao-sel-ensure-forward (kao-sels-list kao--sels)))
         (sels (kao-sels-make :list fwd :main (kao-sels-main kao--sels))))
    ;; direct install (no-clamp): installs a MERGE-FN result (already a built,
    ;; already-merged `kao-sels'), not a `kao-sels-make' list; the seam would
    ;; re-clamp and sort-merge-overlapping an already-consecutive-merged list.
    (setq kao--sels (funcall merge-fn sels))
    (kao--refresh)))

(defun kao-merge-consecutive ()
  "Kakoune `<a-_>': make selections forward, then merge consecutive (touching).
Two selections are consecutive when they overlap or abut with no gap."
  (interactive)
  (kao--assert-mode)
  (kao--ensure-forward-then #'kao-sels-merge-consecutive))

(defun kao-merge-overlapping ()
  "Kakoune `<a-+>': make selections forward, then merge overlapping ones."
  (interactive)
  (kao--assert-mode)
  (kao--ensure-forward-then #'kao-sels-merge-overlapping))

;;;; _ trim_selections — shrink each selection to its non-blank core

(defun kao-trim--blank-p (ch)
  "Non-nil when CH is a Kakoune `is_blank' character.
`is_blank' (unicode.hh:49) = `is_horizontal_blank' OR a line terminator; kao
uses the ASCII subset (space, tab, form-feed, newline, return, vertical-tab) —
the Word/whitespace convention, Unicode blanks deferred."
  (and ch (memq ch '(?\s ?\t ?\f ?\n ?\r ?\v))))

(defun kao-trim--sel (sel)
  "Return SEL trimmed of leading/trailing blank chars, or nil if wholly blank.
Faithful to `trim_selections' (normal.cc:1955): walk `min'/`max' inward past
`is_blank' chars; if the selection collapses onto a single still-blank char it
is dropped (nil).  Direction is preserved."
  (let ((beg (kao-sel-min sel))
        (end (kao-sel-max sel)))
    (while (and (< beg end) (kao-trim--blank-p (char-after beg)))
      (setq beg (1+ beg)))
    (while (and (< beg end) (kao-trim--blank-p (char-after end)))
      (setq end (1- end)))
    (cond ((and (= beg end) (kao-trim--blank-p (char-after beg))) nil)
          ((kao-sel-forward-p sel) (kao-sel-make :anchor beg :cursor end))
          (t (kao-sel-make :anchor end :cursor beg)))))

(defun kao-trim-selections ()
  "Kakoune `_' (trim_selections): shrink each selection to its non-blank core.
Leading and trailing `is_blank' chars are dropped from every selection; a wholly
blank selection is removed via `SelectionList::remove' (selection.cc:39): after
each erase — processed high-index-first, as Kakoune's reverse loop does — the
main decrements when the removed index is below it OR the main was the last
selection (`m_main == new_size').  This differs from `select()'s drop rule: when
the main is itself the dropped blank and not last, it follows the survivor that
shifts into its slot.  When every selection is blank the list is left unchanged
with a message (Kakoune throws `no_selections_remaining').  Direction is
preserved; no sort or merge — trimmed ranges stay within their non-overlapping
originals (normal.cc:1955)."
  (interactive)
  (kao--assert-mode)
  (let* ((orig (kao-sels-list kao--sels))
         (n (length orig))
         (trimmed (mapcar #'kao-trim--sel orig))
         (to-remove (cl-loop for i below n for r in trimmed unless r collect i))
         (survivors (cl-loop for r in trimmed if r collect r)))
    (if (null survivors)
        (message "kao: no selections remaining")
      (let ((m (kao-sels-main kao--sels))
            (sz n))
        (dolist (idx (sort (copy-sequence to-remove) #'>))   ; high index first
          (setq sz (1- sz))                                  ; size after this erase
          (when (or (< idx m) (= m sz))
            (setq m (1- m))))
        ;; The -raw seam performs the same main clamp, (min (max m 0) (1- len)).
        (kao-set-selections-raw survivors m)))))

;;;; + duplicate_selections — duplicate each selection COUNT times

(defun kao-duplicate-selections ()
  "Kakoune `+' (duplicate_selections): duplicate each selection `count' times.
COUNT defaults to 2 (not 1).  A selection identical to its predecessor is kept
once (Kakoune's consecutive-equal dedup); the main follows the last copy of the
original main.  No sort or merge — the stacked copies are left as-is
\(normal.cc:2391)."
  (interactive)
  (kao--assert-mode)
  (let ((count (kao--count-or 2))
        (omain (kao-sels-main kao--sels))
        (result '())
        (total 0)
        (main 0)
        (last nil)
        (i 0))
    (dolist (s (kao-sels-list kao--sels))
      (let ((copies (if (and last
                             (= (kao-sel-anchor s) (kao-sel-anchor last))
                             (= (kao-sel-cursor s) (kao-sel-cursor last)))
                        1 count)))
        (dotimes (_ copies)
          (push (kao-sel-copy s) result)
          (setq total (1+ total))))
      (when (= i omain) (setq main (1- total)))
      (setq last s)
      (setq i (1+ i)))
    (kao-set-selections-raw (nreverse result) main)))

(provide 'kao-multi)
;;; kao-multi.el ends here
