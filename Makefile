# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.10 2003/11/10 15:32:44 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS     = Language/Haskell Language/Haskell/TH
PACKAGE      = haskell-src
PACKAGE_DEPS = base

Language/Haskell/Parser_HC_OPTS += -Onot -fno-warn-incomplete-patterns
Language/Haskell/THSyntax_HC_OPTS += -fglasgow-exts

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)" \
	-p prologue.txt

include $(TOP)/mk/target.mk
