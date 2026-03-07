# Copyright © 2013 Barry Schwartz
#
# This file is free software; the author gives unlimited permission to
# copy and/or distribute it, with or without modifications, as long as
# this notice is preserved.

#--------------------------------------------------------------------------
#
# Silent rules macros.

# $(call if-silent,...do-this-if-silent...[,...do-this-if-not-silent])
#
# $(call if-not-silent,...do-this-if-not-silent...[,...do-this-if-silent])
#
# DEFAULT_VERBOSITY (or, if that is not set, AM_DEFAULT_VERBOSITY)
# should be set to 0, if you want ‘silent’ by default.
#
# Setting V=0 or V=1 on the command line works as with silent rules in
# Automake.
#
if-silent = $(if $(filter 0,$(or $(V),$(if $(filter-out $(flavor DEFAULT_VERBOSITY),undefined),$(DEFAULT_VERBOSITY),$(AM_DEFAULT_VERBOSITY)))),$(1),$(2))
if-not-silent = $(call if-silent,$(2),$(1))

# $(call v,...) examples:
#
#    generated-file: prereq
#       $(call v, GEN) generate $< -o
#
#    foo.o: foo.c
#       $(call v, CC) cc -c foo.c
#
# DEFAULT_VERBOSITY (or, if that is not set, AM_DEFAULT_VERBOSITY)
# should be set to 0, if you want rules to be ‘silent’ by default.
#
# Setting V=0 or V=1 on the command line works as with silent rules in
# Automake.
#
v = $(call if-silent,@printf "  %-8s %s\n" $(1) $(@);)

#--------------------------------------------------------------------------
