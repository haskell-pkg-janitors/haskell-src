# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.7 2003/05/04 13:21:49 igloo Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS     = Language/Haskell
PACKAGE      = haskell-src
PACKAGE_DEPS = base

SRC_HAPPY_OPTS += $(GHC_HAPPY_OPTS)

Language/Haskell/Parser_HC_OPTS += -Onot -fno-warn-incomplete-patterns
Language/Haskell/THSyntax_HC_OPTS += -fglasgow-exts

SRC_HADDOCK_OPTS += -t "Haskell Core Libraries (haskell-src package)" \
	-p prologue.txt

include $(TOP)/mk/target.mk
