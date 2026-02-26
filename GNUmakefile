.DELETE_ON_ERROR:

CHEZ = scheme
CHIBI = chibi-scheme
GSI = gsi
GAUCHE = gosh
LOKO = loko
SAGITTARIUS = sagittarius

check = @$(foreach f,$(3),$(2)=$${PWD}$${$(2)+:}$${$(2)} $(1) $(f);)

check-chez-r6rs = $(call check,$(CHEZ) --program,CHEZSCHEMELIBDIRS,$(1))
check-chibi-r7rs = $(call check,$(CHIBI),CHIBI_MODULE_PATH,$(1))
check-gauche-r7rs = $(call check,$(GAUCHE) -r7 --,GAUCHE_LOAD_PATH,$(1))
check-loko-r6rs = $(call check,$(LOKO) -std=r6rs --program,LOKO_LIBRARY_PATH,$(1))
#check-loko-r7rs = $(call check,$(LOKO) -std=r7rs --script,LOKO_LIBRARY_PATH,$(1))
check-sagittarius-r6rs = $(call check,$(SAGITTARIUS) -d -r6 --,SAGITTARIUS_LOADPATH,$(1))
check-sagittarius-r7rs = $(call check,$(SAGITTARIUS) -d -r7 --,SAGITTARIUS_LOADPATH,$(1))

TSTPROG1_R6RS = tests/test-hashmap-low-level.sps
TSTPROG2_R6RS = tests/test-hashmap.sps

TSTPROG1_R7RS = tests/test-hashmap-low-level.scm
TSTPROG2_R7RS = tests/test-hashmap.scm

# To test with Chez Scheme requires that one has installed SRFI
# software, such as chez-srfi.
.PHONY: check-chez-r6rs
check-chez-r6rs:
	$(call check-chez-r6rs, $(TSTPROG1_R6RS) $(TSTPROG2_R6RS))

# NOTE: I have had trouble with the SRFI-64 available in Snow.
.PHONY: check-chibi-r7rs
check-chibi-r7rs:
	$(call check-chibi-r7rs, $(TSTPROG1_R7RS) $(TSTPROG2_R7RS))

.PHONY: check-gambit-gsi-r7rs
check-gambit-gsi-r7rs:
	( \
	  cd gambit && \
	  $(GSI) -:r7rs,search=$${PWD} ../tests/test-hashmap-gambit-gsi.scm \
	)

.PHONY: check-gauche-r7rs
check-gauche-r7rs:
	$(call check-gauche-r7rs, $(TSTPROG1_R7RS) $(TSTPROG2_R7RS))

# To test with Loko Scheme requires that one has installed SRFI
# software, such as chez-srfi.
.PHONY: check-loko-r6rs # check-loko-r7rs
check-loko-r6rs:
	$(call check-loko-r6rs, $(TSTPROG1_R6RS) $(TSTPROG2_R6RS))
#check-loko-r7rs:
#	$(call check-loko-r7rs, $(TSTPROG1_R7RS) $(TSTPROG2_R7RS))

.PHONY: check-sagittarius-r6rs check-sagittarius-r7rs
check-sagittarius-r6rs:
	$(call check-sagittarius-r6rs, $(TSTPROG1_R6RS) $(TSTPROG2_R6RS))
check-sagittarius-r7rs:
	$(call check-sagittarius-r7rs, $(TSTPROG1_R7RS) $(TSTPROG2_R7RS))
