# -----------------------------------------------------------------------------

TOP=..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

ifeq "$(IncludeExampleDirsInBuild)" "YES"
SUBDIRS += examples
endif

ALL_DIRS     = Language/Haskell
PACKAGE      = haskell-src
PACKAGE_DEPS = base

SRC_HC_OPTS += -fglasgow-exts -cpp 
Language/Haskell/Parser_HC_OPTS += -Onot -fno-warn-incomplete-patterns

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)"

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
