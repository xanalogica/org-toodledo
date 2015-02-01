EMACS ?= emacs
CASK ?= cask

all:
	${MAKE} clean
	${MAKE} test
	${MAKE} compile
	${MAKE} test
	${MAKE} clean

compile:
	${CASK} exec ${EMACS} -Q -batch -L . -eval \
	"(progn \
     (when (version<= \"24.3\" emacs-version) \
     (setq byte-compile-error-on-warn t)) \
     (batch-byte-compile))" org-toodledo.el
test:
	${CASK} exec ${EMACS} -Q -batch -L . -l test/org-toodledo-test.el -f ert-run-tests-batch-and-exit
clean:
	rm -f org-toodledo.elc

.PHONY: all compile test clean
