SHELL := /bin/bash
EMACS ?= emacs
ifeq ($(shell command -v pipx 2>/dev/null),)
$(error pipx not found)
endif
BIN := venvs/xjupyter/bin
PYTHON := $(BIN)/python
GIT_DIR ?= .
SRC := $(shell git ls-files lisp/*.el)
TESTSRC := $(shell git ls-files test/*.el)

.DEFAULT_GOAL := bin/app

bin/app: $(wildcard src/xjupyter/*.py src/xjupyter/jsonrpyc/*.py)
	PIPX_HOME=. PIPX_BIN_DIR=./bin PIPX_MAN_DIR=./man pipx install . --editable --quiet --force

$(BIN)/pytest: $(PYTHON)
	$(PYTHON) -m pip install --use-deprecated=legacy-resolver .[test]

$(BIN)/pylint: $(BIN)/pytest

.PHONY: pylint
pylint: $(BIN)/pylint
	$(PYTHON) -m pylint src/xjupyter --rcfile=pylintrc

.PHONY: pytest
pytest: $(BIN)/pytest
	$(PYTHON) -m pytest tests

.PHONY: compile
compile:
	$(EMACS) -batch -L lisp -L test --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(SRC) $(TESTSRC); \
	  (ret=$$? ; rm -f $(SRC:.el=.elc) $(TESTSRC:.el=.elc) && exit $$ret)

.PHONY: test
test: bin/app compile
	$(PYTHON) -m pip install -q matplotlib
	$(EMACS) --batch -L lisp -L test $(patsubst %.el,-l %,$(notdir $(TESTSRC))) -f ert-run-tests-batch-and-exit

.PHONY: dist-clean
dist-clean:
	( \
	PKG_NAME=`$(EMACS) -batch -L ./lisp -l xjupyter-package --eval "(princ (xjupyter-package-name))"`; \
	rm -rf $${PKG_NAME}; \
	rm -rf $${PKG_NAME}.tar; \
	)

.PHONY: dist
dist: dist-clean
	$(EMACS) -batch -L ./lisp -l xjupyter-package -f xjupyter-package-inception
	( \
	PKG_NAME=`$(EMACS) -batch -L ./lisp -l xjupyter-package --eval "(princ (xjupyter-package-name))"`; \
	rsync -R Makefile pyproject.toml src/xjupyter/*.py src/xjupyter/jsonrpyc/*.py $${PKG_NAME}; \
	tar cf $${PKG_NAME}.tar $${PKG_NAME}; \
	)

.PHONY: install
install: dist
	( \
	PKG_NAME=`$(EMACS) -batch -L ./lisp -l xjupyter-package --eval "(princ (xjupyter-package-name))"`; \
	$(EMACS) -Q --batch -f package-initialize \
	  --eval "(ignore-errors (apply (function package-delete) (alist-get (quote xjupyter) package-alist)))" \
	  --eval "(package-install-file \"$${PKG_NAME}.tar\")"; \
	PKG_DIR=`$(EMACS) -batch -f package-initialize --eval "(princ (package-desc-dir (car (alist-get 'xjupyter package-alist))))"`; \
	GIT_DIR=`git rev-parse --show-toplevel`/.git $(MAKE) -C $${PKG_DIR}; \
	)
	$(MAKE) dist-clean
