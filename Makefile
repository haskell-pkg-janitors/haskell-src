# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2002/05/16 11:54:00 ross Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS     = Language/Haskell
PACKAGE      = haskell-src

SRC_HAPPY_OPTS += $(GHC_HAPPY_OPTS)

Language/Haskell/Parser_HC_OPTS += -Onot -fno-warn-incomplete-patterns

include $(TOP)/mk/target.mk
