# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.6 2002/07/02 13:13:37 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS     = Language/Haskell
PACKAGE      = haskell-src
PACKAGE_DEPS = base

SRC_HAPPY_OPTS += $(GHC_HAPPY_OPTS)

Language/Haskell/Parser_HC_OPTS += -Onot -fno-warn-incomplete-patterns

SRC_HADDOCK_OPTS += -t "Haskell Core Libraries (haskell-src package)" \
	-p prologue.txt

include $(TOP)/mk/target.mk
