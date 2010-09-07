# ----------------------------------------------------
# Application version
# ----------------------------------------------------

APP_NAME=$(PACKAGE_NAME)
VSN=$(PACKAGE_VERSION)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_FLAGS =
ERL_INCLUDE = -I../include
ERL_BEHAV_PATH = -pz ../ebin
ERL_COMPILE_FLAGS = $(ERL_INCLUDE) $(ERL_BEHAV_PATH)

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

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES = $(shell find . -name \*.erl | sed 's:^\./::' | sed 's/\.erl//')
MODULES_STRING_LIST = $(shell find . -name \*.erl | sed 's:^\./::' | sed 's/\.erl/,/')
BEHAV_MODULES = $(shell find . -name \*.erl | xargs grep -l behaviour_info | sed 's:^\./::' | sed 's/\.erl//')

ERL_FILES= $(MODULES:%=%.erl)
DOC_FILES=$(ERL_FILES)

APP_FILE= $(APP_NAME).app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

BEAMS= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
BEHAV_BEAMS= $(BEHAV_MODULES:%=$(EBIN)/%.$(EMULATOR))
TARGET_FILES= $(BEHAV_BEAMS) $(BEAMS) $(APP_TARGET) $(APPUP_TARGET)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

all debug opt: $(EBIN) $(TARGET_FILES)

# Note: In the open-source build clean must not destroy the preloaded
# beam files.
clean:
	rm -f $(TARGET_FILES)
	rm -f *~
	rm -f core
	rm -rf $(EBIN)
	rm -rf *html

$(EBIN):
	mkdir $(EBIN)

dialyzer: $(TARGET_FILES)
	dialyzer --src -r . $(ERL_INCLUDE)

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) $(BEAMS)
	sed -e 's;%VSN%;$(VSN);' \
		-e 's;%APP_NAME%;$(APP_NAME);' \
		-e 's;%MODULES%;%MODULES%$(MODULES_STRING_LIST);' \
		$< > $<".tmp"
	sed -e 's/%MODULES%\(.*\),/\1/' \
		$<".tmp" > $@
	rm $<".tmp"
