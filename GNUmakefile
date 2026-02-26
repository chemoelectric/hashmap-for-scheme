.DELETE_ON_ERROR:

CHEZ = scheme
LOKO = loko
SAGITTARIUS = sagittarius

TSTPROG_R6RS = tests/test-hashmap.sps

.PHONY: check-chez check-chez-r6rs
check-chez check-chez-r6rs:
	$(CHEZ) --program $(TSTPROG_R6RS)

# Loko test requires that one has installed the necessary SRFIs.
.PHONY: check-loko-r6rs
check-loko-r6rs:
	$(LOKO) -std=r6rs --program $(TSTPROG_R6RS)

.PHONY: check-sagittarius-r6rs
check-sagittarius-r6rs:
	$(SAGITTARIUS) -d -r6 -- $(TSTPROG_R6RS)
