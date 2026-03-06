# Copyright © 2026 Barry Schwartz
# SPDX-License-Identifier: MIT

.DELETE_ON_ERROR:
.PHONY: clean

VERSION = 0.0.0

TAR = tar
XZ = xz

CHEZ = scheme
CHIBI = chibi-scheme
GSI = gsi
GAUCHE = gosh
LOKO = loko
SAGITTARIUS = sagittarius

check-r6rs = @$(foreach f,$(3),$(2)=$(PWD)/r6rs$${$(2)+:}$${$(2)} $(1) $(f);)
check-r7rs = @$(foreach f,$(3),$(2)=$(PWD)/r7rs$${$(2)+:}$${$(2)} $(1) $(f);)

check-chez-r6rs = $(call check-r6rs,$(CHEZ) --program,CHEZSCHEMELIBDIRS,$(1))
check-chibi-r7rs = $(call check-r7rs,$(CHIBI),CHIBI_MODULE_PATH,$(1))
check-gauche-r7rs = $(call check-r7rs,$(GAUCHE) -r7 --,GAUCHE_LOAD_PATH,$(1))
check-loko-r6rs = $(call check-r6rs,$(LOKO) -std=r6rs --program,LOKO_LIBRARY_PATH,$(1))
check-loko-r7rs = $(call check-r7rs,$(LOKO) -std=r7rs --script,LOKO_LIBRARY_PATH,$(1))
check-sagittarius-r6rs = $(call check-r6rs,$(SAGITTARIUS) -d -r6 --,SAGITTARIUS_LOADPATH,$(1))
check-sagittarius-r7rs = $(call check-r7rs,$(SAGITTARIUS) -d -r7 --,SAGITTARIUS_LOADPATH,$(1))

TSTPROG1_R6RS = tests/test-hashassoc-low-level.sps
TSTPROG2_R6RS = tests/test-hashassoc.sps

TSTPROG1_R7RS = tests/test-hashassoc-low-level.scm
TSTPROG2_R7RS = tests/test-hashassoc.scm

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
check-chicken-5-r7rs: chicken-5/hashassoc.so
	@( \
	  export CHICKEN_REPOSITORY_PATH=$${PWD}/chicken-5:$(CHICKEN_5_REPOSITORY_PATH); \
	  $(CSI_5) -s $(TSTPROG1_R7RS); \
	  $(CSI_5) -s $(TSTPROG2_R7RS) \
	)

check-chicken-6-r7rs: chicken-6/hashassoc.so
	@( \
	  export CHICKEN_REPOSITORY_PATH=$${PWD}/chicken-6:$(CHICKEN_6_REPOSITORY_PATH); \
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
	  $(GSI) -:r7rs,search=. ../tests/test-hashassoc-gambit-gsi.scm \
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

EGG_5_VERSION = $(VERSION)

CSI_5 = csi
CHICKEN_INSTALL_5 = chicken-install
CHICKEN_UNINSTALL_5 = chicken-uninstall

CHICKEN_5_REPOSITORY_PATH = $(shell $(CHICKEN_INSTALL_5) -repository)

chicken-5/README.adoc: README.adoc
	@mkdir -p $(@D) && \
	rm -f $(@) && \
	cp $(<) $(@)

chicken-5/%.sld: r7rs/%.sld
	@mkdir -p $(@D) && \
	rm -f $(@) && \
	cp $(<) $(@)

chicken-5/common/%.scm: common/%.scm
	@mkdir -p $(@D) && \
	rm -f $(@) && \
	cp $(<) $(@)

chicken-5/hashassoc.egg: GNUmakefile \
	$(addprefix chicken-5/, \
		README.adoc \
		hashassoc.sld \
		hashassoc/hashassoc-structure.sld \
		hashassoc/low-level.sld \
		common/hashassoc/hashassoc-structure-implementation.scm \
		common/hashassoc/low-level-implementation.scm)
	@mkdir -p chicken-5 && \
	awk 'BEGIN { \
	  print "((synopsis \"Hashmaps (hash array mapped tries)\")"; \
	  print " (version \"$(EGG_5_VERSION)\")"; \
	  print " (category data)"; \
	  print " (license \"MIT\")"; \
	  print " (author \"Barry Schwartz\")"; \
	  print " (dependencies r7rs srfi-1 srfi-128 srfi-143)"; \
	  print " (component-options"; \
	  print "  (csc-options \"-X\" \"r7rs\" \"-R\" \"r7rs\" \"-O3\""; \
	  print "               \"-C\" \"-O3\""; \
	  print "               ))"; \
	  print " (components"; \
	  print "  (extension hashassoc.low-level"; \
	  print "   (source \"hashassoc/low-level.sld\"))"; \
	  print "  (extension hashassoc.define-record-factory"; \
	  print "   (source \"hashassoc.define-record-factory.scm\"))"; \
	  print "  (extension hashassoc.hashassoc-structure"; \
	  print "   (source \"hashassoc/hashassoc-structure.sld\")"; \
	  print "   (component-dependencies hashassoc.define-record-factory)"; \
	  print "   (component-dependencies hashassoc.low-level))"; \
	  print "  (extension hashassoc"; \
	  print "   (source \"hashassoc.sld\")"; \
	  print "   (component-dependencies hashassoc.hashassoc-structure))))"; \
	}' > $(@)

chicken-5/hashassoc.so: chicken-5/hashassoc.egg \
	                chicken-5/hashassoc.define-record-factory.scm
	@( \
	  cd chicken-5 && \
	  $(CHICKEN_INSTALL_5) -n \
	)

hashassoc-$(EGG_5_VERSION).chicken-5-egg.tar.xz: clean-chicken-5
	$(MAKE) $(MAKEFLAGS) chicken-5/hashassoc.egg
	$(TAR) -cf - chicken-5 | $(XZ) > $@

.PHONY: install-chicken-5-egg uninstall-chicken-5-egg
install-chicken-5-egg: chicken-5/hashassoc.egg
	@( \
	  cd chicken-5 && \
	  $(CHICKEN_INSTALL_5) -s \
	)
uninstall-chicken-5-egg: chicken-5/hashassoc.egg
	@( \
	  cd chicken-5 && \
	  $(CHICKEN_UNINSTALL_5) -force -s hashassoc \
	)

clean-chicken-5:
	-rm -Rf chicken-5/hashassoc chicken-5/common
	-rm -f chicken-5/README.adoc
	-rm -f chicken-5/hashassoc.sld
	-rm -f chicken-5/hashassoc.build.sh
	-rm -f chicken-5/hashassoc.install.sh
	-rm -f chicken-5/hashassoc.egg
	-rm -f chicken-5/hashassoc*.so
	-rm -f chicken-5/hashassoc*.o
	-rm -f chicken-5/hashassoc*.link
	-rm -f chicken-5/hashassoc*.import.*

clean:: clean-chicken-5

#---------------------------------------------------------------------
#
# CHICKEN 6 egg.
#

EGG_6_VERSION = $(VERSION)

#
# At the time of this writing, CHICKEN 6 is not yet released. Thus I
# have my CHICKEN 6 commands installed with "-6" suffices.
#
CSI_6 = csi-6
CHICKEN_INSTALL_6 = chicken-install-6
CHICKEN_UNINSTALL_6 = chicken-uninstall-6

CHICKEN_6_REPOSITORY_PATH = $(shell $(CHICKEN_INSTALL_6) -repository)

chicken-6/README.adoc: README.adoc
	@mkdir -p $(@D) && \
	rm -f $(@) && \
	cp $(<) $(@)

chicken-6/%.sld: r7rs/%.sld
	@mkdir -p $(@D) && \
	rm -f $(@) && \
	cp $(<) $(@)

chicken-6/common/%.scm: common/%.scm
	@mkdir -p $(@D) && \
	rm -f $(@) && \
	cp $(<) $(@)

chicken-6/hashassoc.egg: GNUmakefile \
	$(addprefix chicken-6/, \
		README.adoc \
		hashassoc.sld \
		hashassoc/define-record-factory.sld \
		hashassoc/hashassoc-structure.sld \
		hashassoc/low-level.sld \
		common/hashassoc/hashassoc-structure-implementation.scm \
		common/hashassoc/low-level-implementation.scm)
	@mkdir -p chicken-6 && \
	awk 'BEGIN { \
	  print "((synopsis \"Hashmaps (hash array mapped tries)\")"; \
	  print " (version \"$(EGG_6_VERSION)\")"; \
	  print " (category data)"; \
	  print " (license \"MIT\")"; \
	  print " (author \"Barry Schwartz\")"; \
	  print " (dependencies srfi-1 srfi-128 srfi-143)"; \
	  print " (component-options"; \
	  print "  (csc-options \"-O3\""; \
	  print "               \"-C\" \"-O3\""; \
	  print "               ))"; \
	  print " (components"; \
	  print "  (extension hashassoc.low-level"; \
	  print "   (source \"hashassoc/low-level.sld\"))"; \
	  print "  (extension hashassoc.define-record-factory"; \
	  print "   (source \"hashassoc/define-record-factory.sld\"))"; \
	  print "  (extension hashassoc.hashassoc-structure"; \
	  print "   (source \"hashassoc/hashassoc-structure.sld\")"; \
	  print "   (component-dependencies hashassoc.define-record-factory)"; \
	  print "   (component-dependencies hashassoc.low-level))"; \
	  print "  (extension hashassoc"; \
	  print "   (source \"hashassoc.sld\")"; \
	  print "   (component-dependencies hashassoc.hashassoc-structure))))"; \
	}' > $(@)

chicken-6/hashassoc.so: chicken-6/hashassoc.egg
	@( \
	  cd chicken-6 && \
	  $(CHICKEN_INSTALL_6) -n \
	)

hashassoc-$(EGG_6_VERSION).chicken-6-egg.tar.xz: clean-chicken-6
	$(MAKE) $(MAKEFLAGS) chicken-6/hashassoc.egg
	$(TAR) -cf - chicken-6 | $(XZ) > $@

.PHONY: install-chicken-6-egg uninstall-chicken-6-egg
install-chicken-6-egg: chicken-6/hashassoc.egg
	@( \
	  cd chicken-6 && \
	  $(CHICKEN_INSTALL_6) -s \
	)
uninstall-chicken-6-egg: chicken-6/hashassoc.egg
	@( \
	  cd chicken-6 && \
	  $(CHICKEN_UNINSTALL_6) -force -s hashassoc \
	)

clean-chicken-6:
	-rm -Rf chicken-6

clean:: clean-chicken-6

#---------------------------------------------------------------------
