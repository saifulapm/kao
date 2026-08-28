EMACS ?= emacs

# All Emacs Lisp sources (excluding tests), in load order.
SRC = kao.el kao-selection.el kao-render.el kao-history.el kao-state.el \
      kao-motion.el kao-register.el kao-edit.el kao-info.el kao-multi.el \
      kao-object.el kao-menu.el kao-search.el kao-pipe.el kao-modeline.el \
      kao-macro.el kao-keys.el kao-objects.el kao-vundo.el kao-surround.el \
      kao-treesit.el

# Test-suite sources, alphabetical.  Kept in sync with the `test' target's
# -l list by hand — a file listed there but not here escapes the strict
# compile gate (and vice versa fails the suite).
TESTSRC = test/kao-bench-tests.el test/kao-bench.el \
          test/kao-config-substrate-tests.el test/kao-edit-tests.el \
          test/kao-history-tests.el test/kao-info-tests.el \
          test/kao-keys-tests.el test/kao-macro-tests.el \
          test/kao-menu-tests.el test/kao-modeline-tests.el \
          test/kao-motion-tests.el test/kao-multi-tests.el \
          test/kao-narrow-tests.el test/kao-object-tests.el \
          test/kao-objects-tests.el test/kao-pipe-tests.el \
          test/kao-register-tests.el test/kao-render-tests.el \
          test/kao-search-tests.el test/kao-selection-tests.el \
          test/kao-state-tests.el test/kao-surround-tests.el \
          test/kao-tests.el test/kao-treesit-tests.el \
          test/kao-vundo-tests.el

.PHONY: check test compile compile-tests bench bench-attr checkdoc clean clean-elc

# The one-command gate: strict byte-compile (sources AND tests),
# full ERT suite, then checkdoc.  First target, so bare `make` runs it.
check: compile compile-tests test checkdoc

# Stale .elc shadow their sources (`load' prefers .elc even when OLDER than
# the .el — `load-prefer-newer' defaults to nil), so the test and bench gates
# always start from a clean tree.
clean-elc:
	rm -f *.elc test/*.elc

# Run the ERT suite in batch mode.
test: clean-elc
	$(EMACS) -Q -batch -L . -L test \
	  -l test/kao-selection-tests.el \
	  -l test/kao-render-tests.el \
	  -l test/kao-history-tests.el \
	  -l test/kao-state-tests.el \
	  -l test/kao-motion-tests.el \
	  -l test/kao-register-tests.el \
	  -l test/kao-edit-tests.el \
	  -l test/kao-info-tests.el \
	  -l test/kao-multi-tests.el \
	  -l test/kao-object-tests.el \
	  -l test/kao-menu-tests.el \
	  -l test/kao-search-tests.el \
	  -l test/kao-pipe-tests.el \
	  -l test/kao-modeline-tests.el \
	  -l test/kao-macro-tests.el \
	  -l test/kao-keys-tests.el \
	  -l test/kao-config-substrate-tests.el \
	  -l test/kao-objects-tests.el \
	  -l test/kao-vundo-tests.el \
	  -l test/kao-surround-tests.el \
	  -l test/kao-treesit-tests.el \
	  -l test/kao-narrow-tests.el \
	  -l test/kao-bench-tests.el \
	  -l test/kao-tests.el \
	  -f ert-run-tests-batch-and-exit

# Batch benchmark harness guarding the hard performance rule.
# Depends on `compile', NOT `clean-elc': a published row must
# time the byte-compiled `.elc' users actually run, not the ~9x-slower
# interpreter.  The header prints a `byte-compiled:' flag so a row is never
# mis-compared across modes.
bench: compile
	$(EMACS) -Q -batch -L . -L test -l test/kao-bench.el -f kao-bench-run

# Per-stage µs attribution table. Observational, not a gate;
# `bench' stays the comparable-across-sessions number.  Also compiles first
# so the attribution reflects the shipped `.elc'.
bench-attr: compile
	$(EMACS) -Q -batch -L . -L test -l test/kao-bench.el -f kao-bench-attribution

# Checkdoc over every source file; fails on any diagnostic.
# Exempt class: "Messages should start with a capital letter" — kao's
# user-facing messages deliberately keep Kakoune's exact lowercase wording
# ("nothing left to undo", "no such register: ..."), pinned by parity tests.
checkdoc:
	$(EMACS) -Q -batch -L . --eval \
	  '(progn (require (quote checkdoc)) (defvar kao--checkdoc-bad nil) (advice-add (quote display-warning) :override (lambda (_type message &rest _) (unless (string-match "Messages should start with a capital letter" message) (push message kao--checkdoc-bad)))) (dolist (f (directory-files "." nil "^kao.*\\.el$$")) (checkdoc-file f)) (when kao--checkdoc-bad (dolist (m (nreverse kao--checkdoc-bad)) (princ (format "%s\n" m))) (kill-emacs 1)))'

# Byte-compile sources with warnings-as-errors: the gate fails on
# any warning so regressions can't creep back.  Bare $(SRC) — not
# $(wildcard $(SRC)) — makes a missing or renamed source fail loudly instead of
# being silently skipped.
compile:
	$(EMACS) -Q -batch -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRC)

# Same warnings-as-errors bar for the test suite: `package-vc-install' ships
# the whole checkout and byte-compiles test/ too, so a test-file warning is
# user-visible install-log noise (the log rot this gate now prevents).
compile-tests: compile
	$(EMACS) -Q -batch -L . -L test \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(TESTSRC)

clean:
	rm -f *.elc test/*.elc
