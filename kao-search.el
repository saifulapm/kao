;;; kao-search.el --- Regex search producing selections for kao -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 7.  Kakoune search drives the
;; selection list off a regex: `/' moves every selection to its next match, `?'
;; extends to it, the `<a-…>' variants reverse direction.  The pattern lives in
;; the `/' register (`n'/`N'/`*' in the step-2 slice reuse it).
;;
;; Like the P2 selection-regex commands (`s'/`S'/`<a-k>'), search uses the Emacs
;; native regexp engine with `case-fold-search' forced nil — Kakoune is
;; case-sensitive by default.  The engine boundary stays in the small
;; `kao--regex-*' helpers (kao-multi.el) and the `*-find-next-match' scan here, so
;; a Kakoune-syntax->Emacs translator can slot in later without touching callers.
;;
;; The prompt is `kao--regex-command' (kao-multi.el): with `kao-incsearch' on
;; (the faithful default) the matches preview live as you type, the Replace/Extend
;; selector serving as both preview and commit (Kakoune `regex_prompt' incsearch);
;; with it off the regex is read in one shot.  Same prompt backs `s'/`S'/`<a-k>'.

;;; Code:

(require 'kao-selection)
(require 'kao-state)
(require 'kao-motion)                    ; kao-motion--cat-at (Word category, `*')
(require 'kao-multi)                     ; kao--keep-direction, kao--read-regex
(require 'kao-register)

(defconst kao-search-register ?/
  "The search-pattern register (Kakoune `/').
A text register in `kao--registers'; holds the last search regex (syntax).")

(defun kao--search-register-arg ()
  "The pending or default `/' search register, lowered.
Ports `to_lower(params.reg ? params.reg : '/')', the shared head of the
whole search family (`search'/`search_next'/
`use_selection_as_search_pattern', normal.cc:1085-1179)."
  (downcase (kao--register-arg kao-search-register)))

;;;; find-next-match — the buffer scan (find_next_match, selectors.cc:1124)

(defun kao-search--find-next-match (sel forward regex)
  "Find the next REGEX match for SEL, returning (NEWSEL . WRAPPED) or nil.
FORWARD non-nil searches forward, otherwise backward.  NEWSEL is a `kao-sel'
over the match with SEL's direction preserved (`kao--keep-direction').  WRAPPED
is non-nil when the search passed the buffer boundary.  nil means REGEX matches
nowhere in the buffer.

Faithful to `find_next_match' (selectors.cc:1124) + `find_next'/`find_prev'
\(:1090/:1105): forward starts one char past the cursor max and, failing, wraps
to `point-min'; backward starts at the cursor min and wraps to `point-max'.  The
match's last char carries the cursor (`utf8::previous(end)'); a zero-width match
keeps a single position.  A match landing at `point-max' (eob) is rejected
\(Kakoune `matches[0].first == buffer.end()').  NEWSEL carries the match's
full submatch list as its captures (selectors.cc:1139-1141) — read back by
the digit registers `0'-`9'."
  (let* ((cf (kao--regex-case-fold regex))   ;: (?i)/(?I) strip + fold policy
         (regex (car cf))
         (case-fold-search (cdr cf)))
    (save-excursion
      (save-match-data
        (let (mb me wrapped)
          (if forward
              (let ((start (min (point-max) (1+ (kao-sel-max sel)))))
                (goto-char start)
                (if (and (< start (point-max)) (re-search-forward regex nil t))
                    (setq mb (match-beginning 0) me (match-end 0) wrapped nil)
                  (goto-char (point-min))
                  (when (re-search-forward regex nil t)
                    (setq mb (match-beginning 0) me (match-end 0) wrapped t))))
            (let ((start (kao-sel-min sel)))
              (goto-char start)
              (if (and (> start (point-min)) (re-search-backward regex nil t))
                  (setq mb (match-beginning 0) me (match-end 0) wrapped nil)
                (goto-char (point-max))
                (when (re-search-backward regex nil t)
                  (setq mb (match-beginning 0) me (match-end 0) wrapped t)))))
          (when (and mb (< mb (point-max)))           ; reject eob match
            (let ((last (if (> me mb) (1- me) mb))     ; inclusive last char
                  (caps (kao--match-captures)))        ; while match data is live
              (let ((ns (kao--keep-direction mb last sel)))
                (setf (kao-sel-captures ns) caps)
                (cons ns wrapped)))))))))

;;;; merge_selections (normal.cc:53) — used by the Extend path

(defun kao-search--merge-selections (sel new-sel)
  "Return SEL extended toward NEW-SEL's cursor (`merge_selections', normal.cc:53).
Thin alias for the canonical Layer-1 `kao-sel-extend-to'; kept for the
extend-to-next-match call site and its tests."
  (kao-sel-extend-to sel new-sel))

;;;; Replace path — select_next_matches (normal.cc:1041)

(defun kao-search--next-list (forward regex)
  "Return each selection's next match (FORWARD or backward), or nil on failure.
nil means REGEX matches nowhere (a selection's `kao-search--find-next-match'
returned nil; since wrap covers the whole buffer, that holds for all of them)."
  (let ((acc '()))
    (catch 'fail
      (dolist (sel (kao-sels-list kao--sels))
        (let ((r (kao-search--find-next-match sel forward regex)))
          (unless r (throw 'fail nil))
          (push (car r) acc)))
      (nreverse acc))))

(defun kao-search--select-next (forward regex count)
  "Move each selection to its COUNT-th next REGEX match (FORWARD or back).
Ports `select_next_matches' (normal.cc:1041).
Replace mode: each selection becomes its next match (direction preserved), then
the list is sorted and overlapping selections merged once per iteration,
the main following the survivor by identity.  COUNT (>=1) selects the Nth
next match.  When REGEX matches nowhere the list is left unchanged (Kakoune
`find_next_match' throws \"no matches found\")."
  (let ((applied nil))
    (dotimes (_ (max 1 count))
      (let ((lst (kao-search--next-list forward regex)))
        (when lst
          (setq applied t)
          ;; direct install (no-refresh): sort-and-merge per iteration, but the
          ;; refresh is deferred to once after the count loop; the seam
          ;; refreshes internally each pass.
          (setq kao--sels
                (kao-sels-sort-and-merge-overlapping
                 (kao-sels-make :list (mapcar #'kao--clamp-sel lst)
                                :main (kao-sels-main kao--sels)))))))
    (if applied (kao--refresh) (message "kao: no matches"))))

;;;; Extend path — extend_to_next_matches (normal.cc:1052)

(defun kao-search--extend-next (forward regex count)
  "Extend each selection to its COUNT-th next REGEX match (FORWARD or back).
Ports `extend_to_next_matches' (normal.cc:1052).  Each selection is extended
toward its next match's cursor (`merge_selections'); a selection whose next
match required WRAPPING is DROPPED, the main index adjusting in place (Kakoune's
`new_sels.size() <= main_index and main_index != 0' decrement, normal.cc:1067).
Unlike the Replace path there is NO sort-and-merge.  When REGEX matches nowhere,
or every selection wrapped, the list is left unchanged."
  (let ((applied nil) (msg nil))
    (catch 'no-match
      (dotimes (_ (max 1 count))
        (let ((kept '())
              (new-main (kao-sels-main kao--sels)))
          (dolist (sel (kao-sels-list kao--sels))
            (let ((r (kao-search--find-next-match sel forward regex)))
              (unless r (setq msg "kao: no matches") (throw 'no-match nil))
              (if (not (cdr r))             ; not wrapped: extend
                  (push (kao-search--merge-selections sel (car r)) kept)
                (when (and (<= (length kept) new-main) (/= new-main 0))
                  (setq new-main (1- new-main))))))
          (if (null kept)
              (progn (setq msg "kao: all selections wrapped") (throw 'no-match nil))
            (setq kept (nreverse kept) applied t)
            ;; direct install (no-refresh): extend path (no sort-and-merge); the
            ;; refresh is deferred to once after the count loop, but the seam
            ;; refreshes internally each pass.
            (setq kao--sels
                  (kao-sels-make :list (mapcar #'kao--clamp-sel kept)
                                 :main (min (max new-main 0) (1- (length kept)))))))))
    (if applied (kao--refresh) (when msg (message "%s" msg)))))

;;;; Repeat stored pattern — search_next (normal.cc:1100)

(defun kao-search--with-main-replaced (newsel)
  "Return a `kao-sels' like `kao--sels' with the MAIN selection replaced by NEWSEL.
The main index is unchanged; the caller sort-and-merges afterward."
  (let ((mi (kao-sels-main kao--sels)))
    (kao-sels-make
     :list (cl-loop for s in (kao-sels-list kao--sels) for i from 0
                    collect (if (= i mi) (kao--clamp-sel newsel) s))
     :main mi)))

(defun kao-search--search-next (mode forward count)
  "Repeat the stored search COUNT times on the MAIN selection.
Ports `search_next' (normal.cc:1100).  The pattern is the first string of
the pending or `/' register (`kao--search-register-arg'); an empty one
signals \"no search pattern\".  MODE `replace' (`n'/`<a-n>') moves ONLY the
main to its next match;
MODE `append' (`N'/`<a-N>') pushes the main's next match as a new selection and
makes it the main.  Each iteration ends with sort-and-merge-overlapping
\; FORWARD chooses the direction; a wrap of the main prints the
wrapped-around status (normal.cc:1129).  Unlike `/', this touches the main only,
never the whole list (`select_next_matches' vs `search_next')."
  (let ((str (car (kao-register-get (kao--search-register-arg)))))
    (if (or (null str) (string-empty-p str))
        (message "kao: no search pattern")
      (let ((main-wrapped nil) (stop nil))
        (dotimes (_ (max 1 count))
          (unless stop
            (let ((r (kao-search--find-next-match (kao--main-sel) forward str)))
              (if (null r)
                  ;; Pattern matches nowhere (buffer changed since it was stored).
                  ;; Kakoune throws here; we stop the loop instead — unreachable in
                  ;; normal use since `/'/`*' only store a pattern that just matched.
                  (setq stop t)
                (let ((newsel (car r)))
                  ;; direct install (no-refresh): sort-and-merge per iteration,
                  ;; but the refresh is deferred to once after the count loop;
                  ;; the seam refreshes internally each pass.
                  (setq kao--sels
                        (kao-sels-sort-and-merge-overlapping
                         (if (eq mode 'append)
                             (let ((lst (append (kao-sels-list kao--sels)
                                                (list (kao--clamp-sel newsel)))))
                               (kao-sels-make :list lst :main (1- (length lst))))
                           (kao-search--with-main-replaced newsel))))
                  (when (cdr r) (setq main-wrapped t)))))))
        (kao--refresh)
        (kao-search--hl-set str)         ; match highlight
        (when stop (message "kao: no matches"))
        (when main-wrapped
          (message "kao: main selection search wrapped around buffer"))))))

;;;; Set search pattern from selection — use_selection_as_search_pattern (:1136)

(defun kao-search--word-at-p (pos)
  "Non-nil when the char at POS is a Word char (Kakoune `is_word<Word>').
ASCII alnum + `_' via `kao-motion--cat-at'; nil past `point-max'."
  (eq (kao-motion--cat-at pos) 'word))

(defun kao-search--bow-p (pos)
  "Non-nil when POS is at the beginning of a word (Kakoune `is_bow', :51).
The char at POS is a Word char and POS is `point-min' or the prior char is not."
  (and (kao-search--word-at-p pos)
       (or (<= pos (point-min))
           (not (kao-search--word-at-p (1- pos))))))

(defun kao-search--eow-p (pos)
  "Non-nil when POS is at the end of a word (Kakoune `is_eow', :60).
False at the buffer end or beginning (faithful to `buffer.is_end'/bob — the
no-trailing-newline eob is the family); otherwise the prior char is a
Word char and the char at POS is not."
  (and (> pos (point-min))
       (< pos (point-max))
       (kao-search--word-at-p (1- pos))
       (not (kao-search--word-at-p pos))))

(defun kao-search--selection-pattern (sel smart)
  "Return an Emacs regexp matching SEL's text literally.
The text spans [min, max+1) (Kakoune `char_next(max)').  It is escaped with
`regexp-quote' — the faithful Emacs translation of Kakoune's PCRE `escape'
\(it targets a different metacharacter set).  When SMART, prepend `\\b' if the
selection begins at a word boundary (`is_bow') and append `\\b' if it ends at
one (`is_eow')."
  (let* ((beg (kao-sel-min sel))
         (end (1+ (kao-sel-max sel)))
         (quoted (regexp-quote (buffer-substring-no-properties beg end))))
    (if smart
        (concat (if (kao-search--bow-p beg) "\\b" "")
                quoted
                (if (kao-search--eow-p end) "\\b" ""))
      quoted)))

(defun kao-search--set-search-pattern (smart)
  "Set the pending or `/' register to a regexp matching the selections' text.
Each selection contributes its `kao-search--selection-pattern'; duplicates are
removed (order-preserving) and the alternatives joined with `\\|' (Kakoune joins
with the PCRE `|'; the Emacs engine spells alternation `\\|').  SMART (`*')
adds word boundaries; raw (`<a-*>') is literal.  Backs
`use_selection_as_search_pattern' (normal.cc:1136, register at :1179)."
  (let* ((pats (delete-dups
                (mapcar (lambda (s) (kao-search--selection-pattern s smart))
                        (kao-sels-list kao--sels))))
         (pattern (string-join pats "\\|")))
    (kao-register-set (kao--search-register-arg) (list pattern))
    (kao-search--hl-set pattern)         ; match highlight
    (message "kao: search register set to %s" pattern)))

;;;; Search-match highlighting (no Kakoune counterpart built in)
;; Kakoune highlights `/'-register matches only through a user-configured
;; dynregex highlighter; Emacs's isearch ships lazy-highlight.  kao borrows the
;; SHAPE of that idiom: after any search command every match of the pattern the
;; command used is overlaid with a `lazy-highlight'-style face, and the next
;; non-search command clears them.  The clear hook is buffer-local and only
;; installed while highlights are live, so the idle hot path pays nothing.
;;
;; Two deviations from the isearch lazy-highlight are deliberate: the scan
;; is whole-buffer (capped at `kao-search-highlight-max'), NOT window-scoped
;; as `lazy-highlight-buffer' nil is; and it is memoized on (pattern . the buffer
;; modification tick) so a repeated `n'/`N' with the pattern and buffer unchanged
;; — the hot path, since the overlays are kept alive across those keys — re-runs
;; nothing.  The FIRST scan of a new pattern is deferred to a short idle timer
;; (`kao-search-highlight-idle') when the buffer is displayed, so the keystroke
;; that triggered it never pays the buffer walk; an undisplayed buffer (batch)
;; scans synchronously.

(defcustom kao-search-highlight t
  "When non-nil, search commands highlight all matches of the pattern.
The highlight clears on the next non-search command."
  :type 'boolean
  :group 'kao)

(defcustom kao-search-highlight-max 1000
  "Maximum number of search matches to highlight at once.
Bounds the overlay count (and the scan, for dense patterns) in large
buffers."
  :type 'natnum
  :group 'kao)

(defcustom kao-search-highlight-idle 0.05
  "Idle delay (seconds) before the initial match-highlight scan runs.
Deferring the whole-buffer scan off the triggering keystroke keeps `n'/`N'
snappy in large buffers; a repeated scan cancels and reschedules the
timer.  When nil, or when the buffer is not displayed in any window (batch),
the scan runs synchronously.  The memoized repeat path never touches the
timer."
  :type '(choice (const :tag \"Synchronous\" nil) number)
  :group 'kao)

(defcustom kao-search-count nil
  "When non-nil, show the current/total search position in the mode line.
Opt-in (the faithful default is nil — Kakoune has no built-in search count).
When set, every search landing (`n'/`N', `/', `*') records the 1-based index of
the match at the main cursor and the total match count, and
`kao-mode-line-string' appends a ` N/M' segment (hel-study-6).  Off by default
and zero cost when off: the recorder never runs and the mode-line string is
unchanged."
  :type 'boolean
  :group 'kao)

(defface kao-search-match '((t :inherit lazy-highlight))
  "Face for search-match highlights (`kao-search-highlight')."
  :group 'kao)

(defvar-local kao--search-hl-overlays nil
  "Live search-match highlight overlays, reused across refreshes.")

(defvar-local kao--search-hl-memo nil
  "Memo of the last completed scan: (PATTERN . BUFFER-CHARS-MODIFIED-TICK).
`kao-search--hl-set' short-circuits when this matches the request and overlays
are live.  Reset by `kao-search--hl-clear'.")

(defvar-local kao--search-hl-timer nil
  "The pending idle timer for a deferred initial highlight scan, or nil.")

(defvar-local kao--search-count nil
  "Recorded `(INDEX . TOTAL)' of the last search landing, or nil (hel-study-6).
INDEX is the 1-based buffer-order position of the match at the main cursor and
TOTAL the match count (capped at `kao-search-highlight-max').  Set by
`kao-search--record-count' at each landing while `kao-search-count' is on, read
by `kao-mode-line-string' for the ` N/M' segment.  nil when there is no pattern
or the cursor is not on a match.")

(defconst kao--search-hl-keep-commands
  '(kao-search-forward kao-search-backward kao-search-extend-forward
    kao-search-extend-backward kao-search-next kao-search-next-add
    kao-search-prev kao-search-prev-add kao-search-set-pattern
    kao-search-set-pattern-raw
    ;; The pending-params keys leave the highlight alone, exactly as they
    ;; leave the count and pending register (`kao--maybe-reset-count').
    kao-digit kao-count-backspace kao-select-register)
  "Commands that keep the search-match highlight alive; any other clears it.")

(defun kao-search--hl-clear ()
  "Delete every search-match highlight and uninstall the clear hook.
Also cancels any pending deferred scan and drops the scan memo."
  (when kao--search-hl-timer
    (cancel-timer kao--search-hl-timer)
    (setq kao--search-hl-timer nil))
  (setq kao--search-hl-memo nil)
  (mapc #'delete-overlay kao--search-hl-overlays)
  (setq kao--search-hl-overlays nil)
  (remove-hook 'post-command-hook #'kao-search--hl-maybe-clear t))

(defun kao-search--hl-maybe-clear ()
  "Post-command hook: clear the highlight after a non-search command."
  (unless (memq this-command kao--search-hl-keep-commands)
    (kao-search--hl-clear)))

(defun kao-search--hl-scan (regex)
  "Overlay every match of REGEX in the buffer, up to `kao-search-highlight-max'.
The whole-buffer scan (overlays every match, capped) that
`kao-search--hl-set' schedules; it pools overlays in `kao--search-hl-overlays'
\(priority 0 — under the selection overlays), installs the buffer-local clear
hook while any are live, and records the (REGEX . tick) memo so an unchanged
repeat short-circuits.  REGEX is the pre-fold pattern; the fold policy
matches search so the highlight set equals the match set."
  (setq kao--search-hl-timer nil)
  (let* ((cf (kao--regex-case-fold regex))   ;: same fold as search, so the
         (rx (car cf))                        ; highlight set == the match set
         (case-fold-search (cdr cf))
         (spare kao--search-hl-overlays)
         (live nil)
         (n 0))
    (save-excursion
      (goto-char (point-min))
      (ignore-errors                  ; a stale register pattern never errors out
        (while (and (< n kao-search-highlight-max)
                    (re-search-forward rx nil t))
          (let ((b (match-beginning 0)) (e (match-end 0)))
            (if (= b e)               ; zero-width match: skip, don't loop
                (if (eobp) (setq n kao-search-highlight-max) (forward-char 1))
              (let ((ov (or (pop spare) (make-overlay b e))))
                (move-overlay ov b e)
                (overlay-put ov 'face 'kao-search-match)
                (overlay-put ov 'priority 0)
                (push ov live)
                (setq n (1+ n))))))))
    (mapc #'delete-overlay spare)
    (setq kao--search-hl-overlays (nreverse live))
    ;; Memo the completed scan so an unchanged repeat is free; a fruitless scan
    ;; (no overlays) is NOT memoed so a later matching edit re-scans.
    (if kao--search-hl-overlays
        (progn
          (setq kao--search-hl-memo (cons regex (buffer-chars-modified-tick)))
          (add-hook 'post-command-hook #'kao-search--hl-maybe-clear nil t))
      (setq kao--search-hl-memo nil)
      (remove-hook 'post-command-hook #'kao-search--hl-maybe-clear t))))

(defun kao-search--record-count (regex)
  "Record `(INDEX . TOTAL)' for REGEX's match at the main cursor (hel-study-6).
Runs only from `kao-search--hl-set' while `kao-search-count' is on, at each
search landing — so the count refreshes on every `n'/`N' even though the
highlight scan itself is memoized (an index recorded at scan time would stale).
Its own buffer walk (same case-fold as search) counts matches in buffer
order up to `kao-search-highlight-max'; INDEX is the ordinal of the match
containing point (the just-landed main cursor).  Sets `kao--search-count' to nil
when REGEX is empty or point is not on a match."
  (setq kao--search-count nil)
  (when (and regex (not (string-empty-p regex)))
    (let* ((cf (kao--regex-case-fold regex))
           (rx (car cf))
           (case-fold-search (cdr cf))
           (cursor (point))
           (total 0)
           (index nil))
      (save-excursion
        (goto-char (point-min))
        (ignore-errors
          (while (and (< total kao-search-highlight-max)
                      (re-search-forward rx nil t))
            (let ((b (match-beginning 0)) (e (match-end 0)))
              (if (= b e)                 ; zero-width match: skip, don't loop
                  (if (eobp) (setq total kao-search-highlight-max) (forward-char 1))
                (setq total (1+ total))
                (when (and (null index) (<= b cursor) (< cursor e))
                  (setq index total)))))))
      (when index
        (setq kao--search-count (cons index total))))))

(defun kao-search--hl-set (regex)
  "Highlight every match of REGEX (up to `kao-search-highlight-max').
A nil/empty REGEX, or `kao-search-highlight' off, clears.  Otherwise the scan
\(`kao-search--hl-scan') runs, but two fast paths skip the buffer walk:
when REGEX and the buffer are unchanged since the last completed scan and the
overlays are still live, this returns immediately; and the first scan of a new
pattern is deferred to a `kao-search-highlight-idle' idle timer while the buffer
is displayed, so the triggering keystroke never pays it.  An undisplayed buffer
\(batch) or a nil delay scans synchronously.

Records the mode-line search count first when `kao-search-count' is on
\(hel-study-6), independent of the highlight so it works even with highlighting
disabled; a no-op otherwise."
  (when kao-search-count (kao-search--record-count regex))
  (if (or (not kao-search-highlight) (null regex) (string-empty-p regex))
      (kao-search--hl-clear)
    (unless (and kao--search-hl-overlays     ; Tier 1: unchanged pattern + buffer
                 (equal kao--search-hl-memo
                        (cons regex (buffer-chars-modified-tick))))
      (if (and kao-search-highlight-idle (get-buffer-window))
          (progn                              ; Tier 2: defer the initial scan
            (when kao--search-hl-timer (cancel-timer kao--search-hl-timer))
            (setq kao--search-hl-timer
                  (run-with-idle-timer
                   kao-search-highlight-idle nil
                   (let ((buf (current-buffer)))
                     (lambda ()
                       (when (buffer-live-p buf)
                         (with-current-buffer buf
                           (kao-search--hl-scan regex))))))))
        (kao-search--hl-scan regex)))))

;;;; Commands

(defun kao-search (forward extend)
  "Read a regex and select (or EXTEND) to the next match.
Backs Kakoune `/' `?' `<a-/>' `<a-?>': FORWARD non-nil searches forward, EXTEND
non-nil extends instead of replacing.  The count (typed before the key) selects
the Nth match.  The regex is read through `kao--regex-command', which owns the
shared regex_prompt plumbing (normal.cc:1014-1018): with `kao-incsearch' on the
matches preview live as you type (the Replace/Extend selector is the live
preview AND the commit), and on commit it stores the pattern in the pending or
`/' register and pushes a jump (nothing on abort).  This command adds only the
search-specific highlight tail (keeps highlighting search-only, so
s/S/<a-k>/<a-K> do NOT highlight)."
  (let* ((count (kao--repeat-count))     ; read before the minibuffer prompt
         (apply-fn (if extend
                       (lambda (rx) (kao-search--extend-next forward rx count))
                     (lambda (rx) (kao-search--select-next forward rx count))))
         (regex (kao--regex-command
                 (cond ((and forward (not extend)) "search:")
                       ((and forward extend)       "search (extend):")
                       ((not (or forward extend))  "reverse search:")
                       (t                          "reverse search (extend):"))
                 apply-fn)))
    (when regex
      (kao-search--hl-set regex))))      ; match highlight

(defun kao-search-forward ()
  "Kakoune `/': select the next match of a regex (every selection moves)."
  (interactive)
  (kao--assert-mode)
  (kao-search t nil))

(defun kao-search-backward ()
  "Kakoune `<a-/>': select the previous match of a regex."
  (interactive)
  (kao--assert-mode)
  (kao-search nil nil))

(defun kao-search-extend-forward ()
  "Kakoune `?': extend each selection to the next match of a regex."
  (interactive)
  (kao--assert-mode)
  (kao-search t t))

(defun kao-search-extend-backward ()
  "Kakoune `<a-?>': extend each selection to the previous match of a regex."
  (interactive)
  (kao--assert-mode)
  (kao-search nil t))

(defun kao-search-next ()
  "Kakoune `n': move the main selection to the next `/'-pattern match."
  (interactive)
  (kao--assert-mode)
  (kao-search--search-next 'replace t (kao--repeat-count)))

(defun kao-search-next-add ()
  "Kakoune `N': add the main's next `/'-pattern match as a new selection."
  (interactive)
  (kao--assert-mode)
  (kao-search--search-next 'append t (kao--repeat-count)))

(defun kao-search-prev ()
  "Kakoune `<a-n>': move the main selection to the previous `/'-pattern match."
  (interactive)
  (kao--assert-mode)
  (kao-search--search-next 'replace nil (kao--repeat-count)))

(defun kao-search-prev-add ()
  "Kakoune `<a-N>': add the main's previous `/'-pattern match as a new selection."
  (interactive)
  (kao--assert-mode)
  (kao-search--search-next 'append nil (kao--repeat-count)))

(defun kao-search-set-pattern ()
  "Kakoune `*': set the `/' register to the selections' text, word-boundary aware."
  (interactive)
  (kao--assert-mode)
  (kao-search--set-search-pattern t))

(defun kao-search-set-pattern-raw ()
  "Kakoune `<a-*>': set the `/' register to the selections' text, literally."
  (interactive)
  (kao--assert-mode)
  (kao-search--set-search-pattern nil))

(provide 'kao-search)
;;; kao-search.el ends here
