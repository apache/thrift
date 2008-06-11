EBIN ?= ../ebin
ESRC ?= .
EMULATOR = beam

ERLC_WFLAGS = -W
ERLC = erlc $(ERLC_WFLAGS) $(ERLC_FLAGS)
ERL = erl -boot start_clean

$(EBIN)/%.beam: $(ESRC)/%.erl
	@echo "   ERLC  $<"
	@$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

.erl.beam:
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(dir $@) $<

