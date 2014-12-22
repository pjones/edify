################################################################################
.PHONEY: all install

################################################################################
# Set up the default target.
all::

################################################################################
# Ask `git' to update the submodule and make haskell.mk available.
util/haskell.mk:
	git submodule update --init

################################################################################
# Settings for `haskell.mk'.
CABAL_FLAGS = --flags=maintainer --enable-tests
include util/haskell.mk

################################################################################
# Destination for `make install'.
PREFIX=$(HOME)

################################################################################
.PHONEY: install

################################################################################
install: $(CABAL_SETUP_CONFIG)
	cabal build
	cabal install pandoc-citeproc
	mkdir -p $(PREFIX)/bin
	install -m 0755 .cabal-sandbox/bin/pandoc $(PREFIX)/bin
	install -m 0755 .cabal-sandbox/bin/pandoc-citeproc $(PREFIX)/bin
	install -m 0755 dist/build/edify/edify $(PREFIX)/bin
