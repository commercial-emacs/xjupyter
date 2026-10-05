include epkg.mk
epkg.mk:
	emacs --batch -l package -f package-initialize -l epkg -f epkg-copy-mk

SHELL := /bin/bash
EMACS ?= emacs
ifeq ($(shell command -v pipx 2>/dev/null),)
$(error pipx not found)
endif
BIN := venvs/xjupyter/bin
PYTHON := $(BIN)/python
GIT_DIR ?= .
ELSRC := $(shell git ls-files lisp/*.el)
PYSRC := $(shell git ls-files src/xjupyter/*.py src/xjupyter/jsonrpyc/*.py)
TESTSRC := $(shell git ls-files test/*.el)

EPKG_FILES := $(ELSRC) Makefile pyproject.toml $(PYSRC)
EPKG_MAIN := lisp/xjupyter.el
EPKG_TEST_EL := $(TESTSRC)

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
compile: epkg-compile

.PHONY: test
test: bin/app compile
	$(PYTHON) -m pip install -q matplotlib
	$(MAKE) epkg-test

.PHONY: install
install: epkg-install
	( \
	PKG_DIR=`$(EMACS) -batch -f package-initialize --eval "(princ (package-desc-dir (car (alist-get 'xjupyter package-alist))))"`; \
	GIT_DIR=`git rev-parse --show-toplevel`/.git $(MAKE) -C $${PKG_DIR}; \
	)
