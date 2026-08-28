;;; kao-bench-tests.el --- Smoke tests for the bench harness -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tiny-scale smoke tests keeping `make bench' (kao-bench.el) from rotting:
;; each bench must run and return sane numbers.  The history bench's flatness
;; ratio is additionally pinned with a LOOSE bound — it catches a return to
;; O(history) per-command growth (class of regression), not noise.

;;; Code:

(require 'ert)

;; `kao-bench' lives beside this file in test/, which is NOT on `load-path'
;; when an installed copy is byte-compiled (only the package root is; `make
;; test' passes -L test).  Resolve it relative to this file so the require
;; succeeds under both loading and compilation.
(eval-and-compile
  (add-to-list 'load-path
               (file-name-directory (or (macroexp-file-name) buffer-file-name))))
(require 'kao-bench)

(defun kao-bench-tests--pos-finite-p (x)
  "Non-nil when X is a positive finite float-able number."
  (and (numberp x) (> x 0) (not (isnan (float x)))
       (not (= (float x) 1.0e+INF))))

(ert-deftest kao-bench-motion-single-smoke ()
  "Single-cursor motion bench runs at tiny scale and reports per-op µs."
  (let ((r (kao-bench-motion-single 50)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :us-per-op)))
    (should (= (plist-get r :reps) 50))))

(ert-deftest kao-bench-motion-1k-cursors-smoke ()
  "1k-cursor motion bench runs at tiny scale; the split found ~1k cursors."
  (let ((r (kao-bench-motion-1k-cursors 4)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :us-per-op)))
    (should (>= (plist-get r :cursors) 999))))

(ert-deftest kao-bench-motion-1k-cursors-windowed-smoke ()
  "Windowed 1k-cursor bench runs; the viewport clips most secondaries.
The simulated 40-line viewport around the main cursor must see far fewer
than the ~1k total secondaries (the whole point of the variant), but more
than zero (one cursor per visible line)."
  (let ((r (kao-bench-motion-1k-cursors-windowed 4)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :us-per-op)))
    (should (>= (plist-get r :cursors) 999))
    (should (> (plist-get r :visible) 0))
    (should (<= (plist-get r :visible) 80))))

(ert-deftest kao-bench-history-smoke-and-flat ()
  "History bench runs; per-command cost does not grow with history length.
The 8x bound is deliberately loose (timer noise at small scale) — an
O(history) regression at 2000 commands overshoots it by orders of magnitude.
The flatness bound is retried up to 3 attempts and passes if ANY attempt
clears 8: a single run flakes high under full-suite CPU contention (it passes
5/5 in isolation), but that flake is contention noise, not signal — a genuine
O(history) regression overshoots the bound \"by orders of magnitude\" on
EVERY attempt, so retries cannot mask it.  The finite/positive assertions on
the first result stay unchanged; the bound itself stays 8 (retries, not slack)."
  (let ((r (kao-bench-history 2000)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :us-per-op)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :ratio)))
    ;; `r' is attempt 1; a second/third fresh run happens only if it flaked.
    (should (or (< (plist-get r :ratio) 8)
                (< (plist-get (kao-bench-history 2000) :ratio) 8)
                (< (plist-get (kao-bench-history 2000) :ratio) 8)))))

(ert-deftest kao-bench-insert-throughput-smoke ()
  "Insert-throughput bench runs at tiny scale and reports both sides."
  (let ((r (kao-bench-insert-throughput 50)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :kao-us)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :native-us)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :ratio)))))

(ert-deftest kao-bench-attribution-smoke ()
  "Attribution harness runs at tiny scale; every row finite; stages restored.
The restore pin matters: a leaked `fset' no-op would silently corrupt every
later test in the batch."
  (let ((orig-sort (symbol-function 'kao-sels-sort-and-merge-overlapping))
        (orig-render (symbol-function 'kao--render))
        (orig-record (symbol-function 'kao--sel-history-record))
        (orig-commit (symbol-function 'kao--hist-maybe-commit))
        (orig-before (symbol-function 'kao--hist-before-change))
        (orig-after (symbol-function 'kao--hist-after-change))
        (r (let ((inhibit-message t)) (kao-bench-attribution 30 2 30))))
    (should (equal (mapcar #'car r)
                   '(motion-single motion-1k motion-1k-windowed insert)))
    (dolist (section r)
      (should (eq (car (nth 1 section)) 'baseline))
      (dolist (row (cdr section))
        (should (kao-bench-tests--pos-finite-p (cdr row)))))
    ;; insert section carries the three gate variants
    (should (equal (mapcar #'car (cdr (assq 'insert r)))
                   '(baseline change-capture native-undo both--b1a-ceiling)))
    (should (eq orig-sort (symbol-function 'kao-sels-sort-and-merge-overlapping)))
    (should (eq orig-render (symbol-function 'kao--render)))
    (should (eq orig-record (symbol-function 'kao--sel-history-record)))
    (should (eq orig-commit (symbol-function 'kao--hist-maybe-commit)))
    (should (eq orig-before (symbol-function 'kao--hist-before-change)))
    (should (eq orig-after (symbol-function 'kao--hist-after-change)))))

;;;; Perf-fix rows — loose machine-independent bounds

(ert-deftest kao-bench-copy-lines-smoke-and-flat ()
  "Copy-lines bench runs; per-op cost is flat across buffer height.
The 8x bound is loose (timer noise); the earlier absolute `point-min' walk at
20000 lines overshoots it by orders of magnitude."
  (let ((r (kao-bench-copy-lines 100)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :us-per-op)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :ratio)))
    (should (< (plist-get r :ratio) 8))))

(ert-deftest kao-bench-pipe-smoke-and-flat ()
  "Pipe-env bench runs; `|true' env cost is flat in selection count.
A reverted unconditional env build would grow with the 2000 selections and blow
the loose 8x bound."
  (let ((r (kao-bench-pipe 200)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :us-per-op)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :ratio)))
    (should (< (plist-get r :ratio) 8))))

(ert-deftest kao-bench-treesit-smoke ()
  "Treesit bench runs at tiny scale over the ~100-cursor bash fixture.
Skipped unless the bash grammar is installed."
  (skip-unless (kao-bench--treesit-available-p))
  (let ((r (kao-bench-treesit 3)))
    (should (plist-get r :available))
    (should (kao-bench-tests--pos-finite-p (plist-get r :select-us)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :goto-us)))
    (should (= (plist-get r :cursors) 100))))

(ert-deftest kao-bench-search-highlight-smoke-and-memo ()
  "Search-highlight bench runs; a repeated scan is far cheaper than the first
\(memo).  The 0.5 bound is loose — the memo makes repeat/first ~0.005."
  (let ((r (kao-bench-search-highlight 50)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :first-us)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :repeat-us)))
    (should (< (plist-get r :ratio) 0.5))))

(ert-deftest kao-bench-incsearch-smoke ()
  "Incsearch preview bench runs at tiny scale and reports per-op µs."
  (let ((r (kao-bench-incsearch 20)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :us-per-op)))))

(ert-deftest kao-bench-find-match-smoke ()
  "Find-miss / match-unbalanced worst-case bench runs; both report µs."
  (let ((r (kao-bench-find-match 8)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :find-us)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :match-us)))))

;;;; Perf-fix rows — loose machine-independent bounds

(ert-deftest kao-bench-foreign-sync-smoke ()
  "Live-list fold fires only on foreign edits.
Plain keystrokes never fold; one keystroke after a foreign edit folds exactly
once — a machine-independent count.  A regression that folded on every keystroke
would push `:folds-plain' above 0."
  (let ((r (kao-bench-foreign-sync 50)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :us-per-op)))
    (should (>= (plist-get r :cursors) 999))
    (should (= (plist-get r :folds-plain) 0))
    (should (= (plist-get r :folds-foreign) 1))))

(ert-deftest kao-bench-refresh-dedup-smoke ()
  "One keystroke renders exactly once though refresh fires twice.
The tick+identity guard collapses the doubled per-key refresh; a reverted guard
would render twice."
  (let ((r (kao-bench-refresh-dedup 20)))
    (should (kao-bench-tests--pos-finite-p (plist-get r :us-per-op)))
    (should (>= (plist-get r :cursors) 999))
    (should (= (plist-get r :renders-per-key) 1))))

(ert-deftest kao-bench-treesit-object-memo-smoke ()
  "Object-pending `<a-i>f' over ~100 cursors sharing a container queries once.
Skipped unless the bash grammar is installed.  The memo keeps `:queries' flat in
cursor count (1); without it the count equals the ~100 cursors, so the loose
`<= 2' bound proves the memo without pinning grammar internals."
  (skip-unless (kao-bench--treesit-available-p))
  (let ((r (kao-bench-treesit-object-memo 3)))
    (should (plist-get r :available))
    (should (kao-bench-tests--pos-finite-p (plist-get r :us-per-op)))
    (should (= (plist-get r :cursors) 100))
    (should (<= (plist-get r :queries) 2))))

(provide 'kao-bench-tests)
;;; kao-bench-tests.el ends here
