# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.8 2003/06/07 11:20:49 ross Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS     = Language/Haskell
PACKAGE      = haskell-src
PACKAGE_DEPS = base

Language/Haskell/Parser_HC_OPTS += -Onot -fno-warn-incomplete-patterns
Language/Haskell/THSyntax_HC_OPTS += -fglasgow-exts

SRC_HADDOCK_OPTS += -t "Haskell Core Libraries (haskell-src package)" \
	-p prologue.txt

include $(TOP)/mk/target.mk
