# Copyright © 2026 Barry Schwartz
# SPDX-License-Identifier: MIT

.DELETE_ON_ERROR:
.PHONY: clean

CHEZ = scheme
CHIBI = chibi-scheme
GSI = gsi
GAUCHE = gosh
LOKO = loko
SAGITTARIUS = sagittarius

check = @$(foreach f,$(3),$(2)=.$${$(2)+:}$${$(2)} $(1) $(f);)

check-chez-r6rs = $(call check,$(CHEZ) --program,CHEZSCHEMELIBDIRS,$(1))
check-chibi-r7rs = $(call check,$(CHIBI),CHIBI_MODULE_PATH,$(1))
check-gauche-r7rs = $(call check,$(GAUCHE) -r7 --,GAUCHE_LOAD_PATH,$(1))
check-loko-r6rs = $(call check,$(LOKO) -std=r6rs --program,LOKO_LIBRARY_PATH,$(1))
check-loko-r7rs = $(call check,$(LOKO) -std=r7rs --script,LOKO_LIBRARY_PATH,$(1))
check-sagittarius-r6rs = $(call check,$(SAGITTARIUS) -d -r6 --,SAGITTARIUS_LOADPATH,$(1))
check-sagittarius-r7rs = $(call check,$(SAGITTARIUS) -d -r7 --,SAGITTARIUS_LOADPATH,$(1))

TSTPROG1_R6RS = tests/test-hashmap-low-level.sps
TSTPROG2_R6RS = tests/test-hashmap.sps

TSTPROG1_R7RS = tests/test-hashmap-low-level.scm
TSTPROG2_R7RS = tests/test-hashmap.scm

# To test with Chez Scheme, one must install SRFI software, such as
# chez-srfi.
.PHONY: check-chez-r6rs
check-chez-r6rs:
	$(call check-chez-r6rs, $(TSTPROG1_R6RS) $(TSTPROG2_R6RS))

# You may have to install some software with snow-chibi or by other
# means. (Also, last I tried it, Chibi’s SRFI-1 was incomplete and
# non-compliant, though it was sufficient for this software.)
.PHONY: check-chibi-r7rs
check-chibi-r7rs:
	$(call check-chibi-r7rs, $(TSTPROG1_R7RS) $(TSTPROG2_R7RS))

.PHONY: check-chicken-5-r7rs check-chicken-6-r7rs
check-chicken-5-r7rs: chicken-5/hashmap.so
	@( \
	  export CHICKEN_REPOSITORY_PATH=chicken-5:$(CHICKEN_5_REPOSITORY_PATH); \
	  $(CSI_5) -s $(TSTPROG1_R7RS); \
	  $(CSI_5) -s $(TSTPROG2_R7RS) \
	)

check-chicken-6-r7rs: chicken-6/hashmap.so
	@( \
	  export CHICKEN_REPOSITORY_PATH=chicken-6:$(CHICKEN_6_REPOSITORY_PATH); \
	  $(CSI_6) -s $(TSTPROG1_R7RS); \
	  $(CSI_6) -s $(TSTPROG2_R7RS) \
	)

# At the time of this writing, Gambit did not support R⁷RS
# syntax-rules, nor could it export macros properly for R⁷RS. What
# Gambit does have for syntax-rules is an ancient version of
# syntax-case that accepts only trailing ellipses. However, Gambit
# code can be among the fastest Scheme code, and compiled separately,
# and so Gambit seems worth supporting.
.PHONY: check-gambit-gsi-r7rs
check-gambit-gsi-r7rs:
	@( \
	  cd gambit && \
	  $(GSI) -:r7rs,search=. ../tests/test-hashmap-gambit-gsi.scm \
	)

.PHONY: check-gauche-r7rs
check-gauche-r7rs:
	@$(call check-gauche-r7rs, $(TSTPROG1_R7RS) $(TSTPROG2_R7RS))

# To test with Loko Scheme one must install SRFI software, such as
# chez-srfi.
.PHONY: check-loko-r6rs check-loko-r7rs
check-loko-r6rs:
	$(call check-loko-r6rs, $(TSTPROG1_R6RS) $(TSTPROG2_R6RS))
check-loko-r7rs:
	$(call check-loko-r7rs, $(TSTPROG1_R7RS) $(TSTPROG2_R7RS))

.PHONY: check-sagittarius-r6rs check-sagittarius-r7rs
check-sagittarius-r6rs:
	$(call check-sagittarius-r6rs, $(TSTPROG1_R6RS) $(TSTPROG2_R6RS))
check-sagittarius-r7rs:
	$(call check-sagittarius-r7rs, $(TSTPROG1_R7RS) $(TSTPROG2_R7RS))

clean::
	-rm -f *.log

#---------------------------------------------------------------------
#
# CHICKEN 5 egg.
#

EGG_5_VERSION = 0.0.0

CSI_5 = csi
CHICKEN_INSTALL_5 = chicken-install

CHICKEN_5_REPOSITORY_PATH = $(shell $(CHICKEN_INSTALL_5) -repository)

chicken-5/hashmap.egg: GNUmakefile
	awk 'BEGIN { \
	  print "((synopsis \"Hashmaps (hash array mapped tries)\")"; \
	  print " (version \"$(EGG_5_VERSION)\")"; \
	  print " (category data)"; \
	  print " (license \"MIT\")"; \
	  print " (author \"Barry Schwartz\")"; \
	  print " (dependencies r7rs srfi-1 srfi-143)"; \
	  print " (component-options"; \
	  print "  (csc-options \"-X\" \"r7rs\" \"-R\" \"r7rs\" \"-O3\""; \
	  print "               \"-fixnum-arithmetic\""; \
	  print "               ))"; \
	  print " (components"; \
	  print "  (extension hashmap.low-level"; \
	  print "   (source \"hashmap.low-level.scm\"))"; \
	  print "  (extension hashmap.define-record-factory"; \
	  print "   (source \"hashmap.define-record-factory.scm\"))"; \
	  print "  (extension hashmap.hashmap-structure"; \
	  print "   (source \"hashmap.hashmap-structure.scm\")"; \
	  print "   (component-dependencies hashmap.define-record-factory)"; \
	  print "   (component-dependencies hashmap.low-level))"; \
	  print "  (extension hashmap"; \
	  print "   (source \"hashmap.scm\")"; \
	  print "   (component-dependencies hashmap.hashmap-structure))))"; \
	}' > $(@)

chicken-5/hashmap.so: chicken-5/hashmap.egg chicken-5/hashmap.scm \
	              chicken-5/hashmap.define-record-factory.scm \
	              hashmap/hashmap-structure-implementation.scm \
	              hashmap/low-level-implementation.scm \
	              hashmap/hashmap-structure.sld \
	              hashmap/low-level.sld
	(cd chicken-5; $(CHICKEN_INSTALL_5) -n)

clean::
	-rm -f chicken-5/hashmap.build.sh
	-rm -f chicken-5/hashmap.install.sh
	-rm -f chicken-5/hashmap.egg
	-rm -f chicken-5/hashmap*.so
	-rm -f chicken-5/hashmap*.o
	-rm -f chicken-5/hashmap*.link
	-rm -f chicken-5/hashmap*.import.*

#---------------------------------------------------------------------
#
# CHICKEN 6 egg.
#

EGG_6_VERSION = 0.0.0

#
# At the time of this writing, CHICKEN 6 is not yet released. Thus I
# have my CHICKEN 6 commands installed with "-6" suffices.
#
CSI_6 = csi-6
CHICKEN_INSTALL_6 = chicken-install-6

CHICKEN_6_REPOSITORY_PATH = $(shell $(CHICKEN_INSTALL_6) -repository)

chicken-6/hashmap.egg: GNUmakefile
	awk 'BEGIN { \
	  print "((synopsis \"Hashmaps (hash array mapped tries)\")"; \
	  print " (version \"$(EGG_6_VERSION)\")"; \
	  print " (category data)"; \
	  print " (license \"MIT\")"; \
	  print " (author \"Barry Schwartz\")"; \
	  print " (dependencies srfi-1 srfi-143)"; \
	  print " (component-options"; \
	  print "  (csc-options \"-O3\""; \
	  print "               \"-fixnum-arithmetic\""; \
	  print "               ))"; \
	  print " (components"; \
	  print "  (extension hashmap.low-level"; \
	  print "   (source \"hashmap/low-level.sld\"))"; \
	  print "  (extension hashmap.define-record-factory"; \
	  print "   (source \"hashmap/define-record-factory.sld\"))"; \
	  print "  (extension hashmap.hashmap-structure"; \
	  print "   (source \"hashmap/hashmap-structure.sld\")"; \
	  print "   (component-dependencies hashmap.define-record-factory)"; \
	  print "   (component-dependencies hashmap.low-level))"; \
	  print "  (extension hashmap"; \
	  print "   (source \"hashmap.sld\")"; \
	  print "   (component-dependencies hashmap.hashmap-structure))))"; \
	}' > $(@)

chicken-6/hashmap.so: chicken-6/hashmap.egg hashmap.sld \
	              hashmap/hashmap-structure-implementation.scm \
	              hashmap/low-level-implementation.scm \
	              hashmap/define-record-factory.sld \
	              hashmap/hashmap-structure.sld \
	              hashmap/low-level.sld
	(cd chicken-6; $(CHICKEN_INSTALL_6) -n)

clean::
	-rm -f chicken-6/hashmap.build.sh
	-rm -f chicken-6/hashmap.install.sh
	-rm -f chicken-6/hashmap.egg
	-rm -f chicken-6/hashmap*.so
	-rm -f chicken-6/hashmap*.o
	-rm -f chicken-6/hashmap*.link
	-rm -f chicken-6/hashmap*.import.*

#---------------------------------------------------------------------
