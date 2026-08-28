;;; kao-keys-tests.el --- Tests for kao-keys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the table-driven default bindings: every
;; table row names a real command, every row is actually bound in its map,
;; and no key is declared twice.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'kao-keys)

(defconst kao-keys-tests--tables
  (list (cons kao-keys-normal-alist kao-normal-state-map)
        (cons kao-keys-insert-alist kao-insert-state-map)
        (cons kao-keys-user-alist   kao-user-map))
  "Each (TABLE . MAP) pair `kao-keys--apply' wires at load.")

(ert-deftest kao-keys-every-entry-is-a-command ()
  "Every table row's definition is a defined interactive command."
  (pcase-dolist (`(,table . ,_map) kao-keys-tests--tables)
    (pcase-dolist (`(,key . ,def) table)
      (should (fboundp def))
      (should (commandp def))
      (should (stringp key)))))

(ert-deftest kao-keys-tables-are-applied ()
  "`lookup-key' agrees with every table row in the corresponding map."
  (pcase-dolist (`(,table . ,map) kao-keys-tests--tables)
    (pcase-dolist (`(,key . ,def) table)
      (should (eq (lookup-key map (kbd key)) def)))))

(ert-deftest kao-keys-no-duplicate-keys-per-table ()
  "No key string appears twice within one table."
  (pcase-dolist (`(,table . ,_map) kao-keys-tests--tables)
    (let ((keys (mapcar #'car table)))
      (should (equal (length keys)
                     (length (delete-dups (copy-sequence keys))))))))

(ert-deftest kao-keys-table-row-counts-pinned ()
  "Row-count snapshot: a silently DROPPED table row must bite somewhere.
The applied/integrity tests iterate the tables themselves, so a deleted
row vanishes from both sides of those assertions (evaluator finding).
Update these counts DELIBERATELY when a default binding is added or
removed."
  (should (= (length kao-keys-normal-alist) 172))   ; +12 arrow/Home/End
  (should (= (length kao-keys-insert-alist) 3))   ; dropped C-n/C-p
  (should (= (length kao-keys-user-alist) 3))
  (should (= (length kao-keys-prompt-alist) 1))
  (should (= (length kao-keys-regex-prompt-alist) 1)))

(ert-deftest kao-keys-regex-prompt-map-applied ()
  "The regex-prompt table is applied: TAB -> completion-at-point."
  (should (eq (lookup-key kao-regex-prompt-map (kbd "TAB"))
              #'completion-at-point)))

(ert-deftest kao-keys-prompt-map-applied ()
  "The prompt table is applied: `C-r' -> register insert in `kao-prompt-map'
\(user-decided faithful rebind; PromptMode ctrl(r),
input_handler.cc:758-780).  Every row is a defined command."
  (should (eq (lookup-key kao-prompt-map (kbd "C-r"))
              #'kao-prompt-insert-register))
  (pcase-dolist (`(,_key . ,def) kao-keys-prompt-alist)
    (should (commandp def))))

(ert-deftest kao-keys-regex-capf-wired-at-call-sites ()
  "call-site pin: BOTH regex prompts wire TAB+capf; pipe and desc do NOT.
Reverting either regex reader to the plain `kao--prompt-setup' (or wiring
desc/pipe with the regex setup) must bite HERE — the setup-function unit
pins alone cannot see the call sites (slice-27 evaluator blocker)."
  (let ((probe (lambda (&rest _)
                 (with-temp-buffer
                   (use-local-map (make-sparse-keymap))
                   (run-hooks 'minibuffer-setup-hook)
                   (concat
                    (if (eq (lookup-key (current-local-map) (kbd "TAB"))
                            #'completion-at-point)
                        "T" "-")
                    (if (memq #'kao--regex-capf
                              completion-at-point-functions)
                        "C" "-"))))))
    ;; Plain regex prompt: TAB + capf present.
    (cl-letf (((symbol-function 'read-string) probe))
      (should (equal (kao--read-regex "p:") "TC")))
    ;; Incsearch prompt: TAB + capf present.
    (with-temp-buffer
      (insert "x")
      (kao-mode 1)
      (unwind-protect
          (cl-letf (((symbol-function 'read-from-minibuffer) probe))
            (should (equal (kao--regex-prompt "p:" #'kao--select-regex-apply)
                           "TC")))
        (kao-mode -1)))
    ;; Pipe prompt: NEITHER (read-shell-command owns completion).
    (cl-letf (((symbol-function 'read-shell-command) probe))
      (should (equal (kao--read-pipe-command "p:") "--")))
    ;; Object-desc prompt: NEITHER (complete_nothing, normal.cc:1489).
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest args)
                 (if (equal (apply probe args) "--") "a,b" "WIRED,WIRED"))))
      (should (equal (kao-object--read-desc) '("a" . "b"))))))

(ert-deftest kao-keys-prompt-map-reaches-all-four-prompts ()
  "Each kao prompt composes `kao-prompt-map' over its minibuffer map.
Drives each reader with its underlying read fn stubbed to run the
minibuffer setup hook in a fresh buffer and probe the resulting local map
\(the `minibuffer-with-setup-hook' contract, REQ-3)."
  (let ((probe (lambda (&rest _)
                 (with-temp-buffer
                   (use-local-map (make-sparse-keymap))
                   (run-hooks 'minibuffer-setup-hook)
                   (if (eq (lookup-key (current-local-map) (kbd "C-r"))
                           #'kao-prompt-insert-register)
                       "ok" "MISSING")))))
    ;; search/select regex prompt
    (cl-letf (((symbol-function 'read-string) probe))
      (should (equal (kao--read-regex "p:") "ok")))
    ;; object desc prompt (parse needs <open>,<close>: return a valid desc)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest args)
                 (if (equal (apply probe args) "ok") "a,b" "bad"))))
      (should (equal (kao-object--read-desc) '("a" . "b"))))
    ;; pipe prompt
    (cl-letf (((symbol-function 'read-shell-command) probe))
      (should (equal (kao--read-pipe-command "p:") "ok")))
    ;; incsearch prompt (its setup lambda must ALSO compose the map)
    (with-temp-buffer
      (insert "x")
      (kao-mode 1)
      (unwind-protect
          (cl-letf (((symbol-function 'read-from-minibuffer) probe))
            (should (equal (kao--regex-prompt
                            "p:" #'kao--select-regex-apply)
                           "ok")))
        (kao-mode -1)))))

(ert-deftest kao-keys-spc-is-user-map-prefix ()
  "`SPC' is the plain `kao-user-map' prefix keymap."
  (should (eq (lookup-key kao-normal-state-map (kbd "SPC")) kao-user-map))
  ;; End-to-end in a live kao buffer: the prefix reaches the user-map rows.
  (with-temp-buffer
    (kao-mode 1)
    (unwind-protect
        (progn
          (should (keymapp (key-binding (kbd "SPC"))))
          (should (eq (key-binding (kbd "SPC #")) #'kao-comment-lines)))
      (kao-mode -1))))

(ert-deftest kao-keys-user-map-rows ()
  "User-map rows (user decisions 2026-06-13): `#' comments, no `x'/`c'."
  (should (eq (lookup-key kao-user-map (kbd "#")) #'kao-comment-lines))
  (should-not (lookup-key kao-user-map (kbd "x")))
  (should-not (lookup-key kao-user-map (kbd "c")))
  (should (commandp #'kao-crop-lines)))  ; command survives the dropped row

(ert-deftest kao-keys-spc-d-r-push-then-xref ()
  "`SPC d'/`SPC r' bind kao commands (not raw xref) that `kao-jump-push' first,
so `C-o' (`kao-jump-backward') returns to the pre-jump selections after the
xref jump."
  (with-temp-buffer
    (insert "0123456789ab")
    (goto-char 4)
    (kao-mode 1)
    (unwind-protect
        (let ((dcmd (lookup-key kao-user-map (kbd "d")))
              (rcmd (lookup-key kao-user-map (kbd "r"))))
          ;; The rows are kao wrappers now, not the raw xref commands.
          (should (commandp dcmd))
          (should (commandp rcmd))
          (should-not (eq dcmd 'xref-find-definitions))
          (should-not (eq rcmd 'xref-find-references))
          (setq kao--jumps nil kao--jump-current 0)
          ;; Stub the xref jump so it lands elsewhere (a definition jump).
          (cl-letf (((symbol-function 'xref-find-definitions)
                     (lambda (&rest _)
                       (interactive)
                       (goto-char 9)
                       (setq kao--sels (kao-sels-make
                                        :list (list (kao-sel-make :anchor 9 :cursor 9))
                                        :main 0)))))
            (call-interactively dcmd))
          ;; The pre-jump selection (cursor 4) was pushed silently; C-o returns.
          (should (= 1 (length kao--jumps)))
          (kao-jump-backward)
          (should (= 4 (kao-sel-cursor (kao--main-sel)))))
      (kao-mode -1))))

(ert-deftest kao-keys-normal-map-stays-suppressed ()
  "The buffer-integrity remap survives the table application:
unbound printables stay inert via the `self-insert-command' remap."
  (should (eq (lookup-key kao-normal-state-map [remap self-insert-command])
              #'undefined))
  (should (eq (lookup-key kao-normal-state-map (kbd "RET")) #'undefined)))

(ert-deftest kao-keys-out-of-scope-keys-stay-unbound ()
  "The stance keys (out of scope, permanently) are deliberately unbound.
`C-l' falls through to Emacs's `recenter-top-bottom' (whose first press
redraws — a native superset of Kakoune's `force_redraw'); `\\=\\' (disable
hooks, input_handler.cc:314) has no referent — no kao hook system — and
resolves to `undefined' through the `self-insert-command' remap, a
loud no-op rather than a silent flag."
  (should (null (lookup-key kao-normal-state-map (kbd "C-l"))))
  (should (null (lookup-key kao-normal-state-map (kbd "\\"))))
  (should (eq (lookup-key kao-normal-state-map [remap self-insert-command])
              #'undefined)))

(ert-deftest kao-keys-insert-native-keys-stay-unbound ()
  "The insert-key stance: Kakoune's insert
`<c-u>' (commit undo group — NOT erase, input_handler.cc:1366-1372),
`<c-c>' (leave insert), `<c-v>' (raw insert), `<c-x>'/`<c-o>' (completers/
autocomplete toggle), and the erase family stay UNBOUND in
`kao-insert-state-map' — insert state is native Emacs editing,
so each falls through to its Emacs binding (`universal-argument', the
`C-c'/`C-x' prefixes, `quoted-insert', `open-line', native erase)."
  (dolist (key '("C-u" "C-c" "C-v" "C-x" "C-o" "C-w"))
    (should (null (lookup-key kao-insert-state-map (kbd key))))))

(ert-deftest kao-keys-native-undo-keys-opt-in ()
  "`kao-bind-native-undo-keys' remaps undo/undo-redo into kao's tree, off by default.
The remap lives in `kao-normal-state-map' (NOT the alist, so the row-count pin
is unaffected); a `[remap undo]' there catches every native undo key.  Toggling
the defcustom through its own `:set' adds and removes the remap idempotently."
  ;; Default: off, no remap installed (the load-time :set with nil is a no-op).
  (should (null kao-bind-native-undo-keys))
  (should (null (lookup-key kao-normal-state-map [remap undo])))
  (should (null (lookup-key kao-normal-state-map [remap undo-redo])))
  (let ((setter (get 'kao-bind-native-undo-keys 'custom-set)))
    (should (functionp setter))                 ; defcustom wired its :set
    (unwind-protect
        (progn
          ;; Enable through the defcustom's own :set.
          (funcall setter 'kao-bind-native-undo-keys t)
          (should (eq kao-bind-native-undo-keys t))
          (should (eq (lookup-key kao-normal-state-map [remap undo]) #'kao-undo))
          (should (eq (lookup-key kao-normal-state-map [remap undo-redo])
                      #'kao-redo))
          ;; Disable: both remaps removed again.
          (funcall setter 'kao-bind-native-undo-keys nil)
          (should (null (lookup-key kao-normal-state-map [remap undo])))
          (should (null (lookup-key kao-normal-state-map [remap undo-redo]))))
      ;; Restore the off state regardless of outcome (shared global map).
      (kao--apply-native-undo-keys nil)
      (setq kao-bind-native-undo-keys nil))))

(ert-deftest kao-keys-colon-hints-at-m-x ()
  "`:' is bound to `kao-colon-hint', which signals a hint pointing at M-x
\(kao has no `:' command language) rather than a silent no-op."
  (should (eq (lookup-key kao-normal-state-map ":") #'kao-colon-hint))
  (let ((err (should-error (kao-colon-hint) :type 'user-error)))
    (should (string-match-p "M-x" (cadr err)))))

;;;; Arrow / Home / End normal mappings (main.cc:406-419)

(defconst kao-keys-tests--arrow-home-end
  '(("<left>" . kao-left)   ("<right>" . kao-right)
    ("<down>" . kao-down)   ("<up>" . kao-up)
    ("S-<left>" . kao-extend-left)  ("S-<right>" . kao-extend-right)
    ("S-<down>" . kao-extend-down)  ("S-<up>" . kao-extend-up)
    ("<home>" . kao-select-line-begin) ("<end>" . kao-select-line-end)
    ("S-<home>" . kao-extend-line-begin) ("S-<end>" . kao-extend-line-end))
  "The twelve arrow/Home/End rows, faithful to main.cc:406-419.")

(ert-deftest kao-keys-arrow-home-end-bound-to-kao-motions ()
  "All twelve arrow/Home/End keys resolve to their `kao-' motion command in
`kao-normal-state-map' (main.cc:406-419): Left/Right/Down/Up -> h/l/j/k,
shifted -> extend, End/Home -> `<a-l>'/`<a-h>' (select-line-end/begin),
S-End/S-Home -> the extend variants.  Keeping them inside the normal map
stops the native fallthrough that would trigger the foreign collapse.

The pin is slice-exhaustive in BOTH directions: the per-row loop checks each
pinned row is bound (defconst -> map), and the family slice below checks the
live `kao-keys-normal-alist' carries no arrow/Home/End row the defconst omits
(map -> defconst).  The defconst STAYS a literal lockstep against
main.cc:406-419 rather than being derived from the source alist,
which would make the pin tautological; the reverse slice closes the one
drift the literal cannot catch — a THIRTEENTH arrow-family row slipping into
kao-keys.el unpinned."
  (pcase-dolist (`(,key . ,cmd) kao-keys-tests--arrow-home-end)
    (should (eq (lookup-key kao-normal-state-map (kbd key)) cmd)))
  ;; Reverse direction: filter the live alist to the arrow/Home/End lexical
  ;; family (any key string naming an arrow, Home, or End token) and assert it
  ;; is exactly the pinned set.  A new such row in kao-keys.el would appear
  ;; here but not in the defconst and fail; a pinned row dropped from the
  ;; source would appear in the defconst but not here and fail.
  (let ((family (cl-remove-if-not
                 (lambda (row)
                   (string-match-p
                    "<left>\\|<right>\\|<up>\\|<down>\\|<home>\\|<end>"
                    (car row)))
                 kao-keys-normal-alist)))
    (should (= (length family) (length kao-keys-tests--arrow-home-end)))
    (dolist (row family)
      (should (member row kao-keys-tests--arrow-home-end)))
    (dolist (row kao-keys-tests--arrow-home-end)
      (should (member row family)))))

(ert-deftest kao-keys-arrow-right-preserves-multi-selection ()
  "Because `<right>' now resolves to `kao-right' (a `kao-'-prefixed command),
the `kao--foreign-sync' catch-all EXEMPTS it — two selections survive.
Contrast: had `<right>' fallen through to the native `right-char' (foreign),
the sync would collapse the whole list to ONE selection at point (the bug)."
  ;; "0123456789\n0123456789": main cursor 15; move point to 9 (off it).
  (with-temp-buffer
    (insert "0123456789\n0123456789")
    (kao-mode 1)
    (unwind-protect
        (progn
          ;; The `<right>' binding is exempt (kao command) -> 2 survive.
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 2 :cursor 4)
                                       (kao-sel-make :anchor 13 :cursor 15))
                           :main 1))
          (goto-char 9)
          (let ((this-command (lookup-key kao-normal-state-map (kbd "<right>"))))
            (kao--foreign-sync))
          (should (= 2 (length (kao-sels-list kao--sels))))
          ;; Contrast: a genuinely foreign command (the pre-fix fallthrough)
          ;; DOES collapse -> proves the binding is what preserves the list.
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 2 :cursor 4)
                                       (kao-sel-make :anchor 13 :cursor 15))
                           :main 1))
          (goto-char 9)
          (let ((this-command 'right-char))
            (kao--foreign-sync))
          (should (= 1 (length (kao-sels-list kao--sels)))))
      (kao-mode -1))))

(ert-deftest kao-keys-shift-arrow-extends-not-replaces ()
  "`S-<right>' resolves to `kao-extend-right' and EXTENDS (keeps the anchor,
moves the cursor) rather than replacing (main.cc:406-419, shift -> extend)."
  ;; "0123456789": sel [4,4] on '3'@4.
  (with-temp-buffer
    (insert "0123456789")
    (kao-mode 1)
    (unwind-protect
        (let ((cmd (lookup-key kao-normal-state-map (kbd "S-<right>"))))
          (should (eq cmd 'kao-extend-right))
          (setq kao--sels (kao-sels-make
                           :list (list (kao-sel-make :anchor 4 :cursor 4))
                           :main 0))
          (funcall cmd)                          ; S-<right>
          (let ((s (nth 0 (kao-sels-list kao--sels))))
            (should (= (kao-sel-anchor s) 4))    ; anchor kept (extend)
            (should (= (kao-sel-cursor s) 5))))  ; cursor moved right
      (kao-mode -1))))

(provide 'kao-keys-tests)
;;; kao-keys-tests.el ends here
