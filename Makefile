# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.9 2003/11/06 17:10:00 simonpj Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS     = Language/Haskell Language/Haskell/TH
PACKAGE      = haskell-src
PACKAGE_DEPS = base

Language/Haskell/Parser_HC_OPTS += -Onot -fno-warn-incomplete-patterns
Language/Haskell/THSyntax_HC_OPTS += -fglasgow-exts

SRC_HADDOCK_OPTS += -t "Haskell Core Libraries (haskell-src package)" \
	-p prologue.txt

include $(TOP)/mk/target.mk
