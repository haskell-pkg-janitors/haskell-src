# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 2002/05/15 15:36:32 ross Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS     = Language/Haskell
PACKAGE      = haskell-src

# Can't do this yet, because currently Happy would generate source
# that imports the old GlaExts module from hslibs/lang
# SRC_HAPPY_OPTS += $(GHC_HAPPY_OPTS)

Language/Haskell/Parser_HC_OPTS += -Onot -fno-warn-incomplete-patterns

include $(TOP)/mk/target.mk
