# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.4 2002/06/21 13:56:47 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS     = Language/Haskell
PACKAGE      = haskell-src

SRC_HAPPY_OPTS += $(GHC_HAPPY_OPTS)

Language/Haskell/Parser_HC_OPTS += -Onot -fno-warn-incomplete-patterns

SRC_HADDOCK_OPTS += -t "Haskell Core Libraries (haskell-src package)"

include $(TOP)/mk/target.mk
