TOP = ../..
include $(TOP)/mk/boilerplate.mk

NO_BOOT_TARGET	= YES
WAYS		=

# -----------------------------------------------------------------------------

HS_FILES	:= $(wildcard *.hs)
LHS_FILES	:= $(wildcard *.lhs)
PROGRAMS	:= $(HS_FILES:.hs=$(exeext)) $(LHS_FILES:.lhs=$(exeext))
CLEAN_FILES	+= $(PROGRAMS)

SRC_HC_OPTS	+= -package haskell-src

all:: $(PROGRAMS)

%$(exeext): %.hs
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $<

%$(exeext): %.lhs
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $<

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
