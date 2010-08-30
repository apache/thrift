# $Id: Makefile,v 1.3 2004/08/13 16:35:59 mlogan Exp $
#
include ../build/otp.mk
include ../build/colors.mk
include ../build/buildtargets.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------

include ../vsn.mk
APP_NAME=thrift
PFX=thrift
VSN=$(THRIFT_VSN)

# ----------------------------------------------------
# Install directory specification
# WARNING: INSTALL_DIR the command to install a directory.
#          INSTALL_DST is the target directory
# ----------------------------------------------------
INSTALL_DST = $(ERLANG_OTP)/lib/$(APP_NAME)-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------


MODULES = $(shell find . -name \*.erl | sed 's:^\./::' | sed 's/\.erl//')
MODULES_STRING_LIST = $(shell find . -name \*.erl | sed 's:^\./:":' | sed 's/\.erl/",/')

HRL_FILES=
INTERNAL_HRL_FILES= $(APP_NAME).hrl
ERL_FILES= $(MODULES:%=%.erl)
DOC_FILES=$(ERL_FILES)

APP_FILE= $(APP_NAME).app
APPUP_FILE= $(APP_NAME).appup

APP_SRC= $(APP_FILE).src
APPUP_SRC= $(APPUP_FILE).src

APP_TARGET= $(EBIN)/$(APP_FILE)
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

BEAMS= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
TARGET_FILES= $(BEAMS) $(APP_TARGET) $(APPUP_TARGET)

WEB_TARGET=/var/yaws/www/$(APP_NAME)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_FLAGS +=
ERL_INCLUDE = -I../include -I../../fslib/include -I../../system_status/include
ERL_COMPILE_FLAGS += $(ERL_INCLUDE)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

all debug opt: $(EBIN) $(TARGET_FILES)

#$(EBIN)/rm_logger.beam: $(APP_NAME).hrl
include ../build/docs.mk

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

$(APP_TARGET): $(APP_SRC) ../vsn.mk $(BEAMS)
	sed -e 's;%VSN%;$(VSN);' \
		-e 's;%PFX%;$(PFX);' \
		-e 's;%APP_NAME%;$(APP_NAME);' \
		-e 's;%MODULES%;%MODULES%$(MODULES_STRING_LIST);' \
		$< > $<".tmp"
	sed -e 's/%MODULES%\(.*\),/\1/' \
		$<".tmp" > $@
	rm $<".tmp"

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(WEB_TARGET): ../markup/*
	rm -rf $(WEB_TARGET)
	mkdir $(WEB_TARGET)
	cp -r ../markup/ $(WEB_TARGET)
	cp -r ../skins/ $(WEB_TARGET)

# ----------------------------------------------------
# Install Target
# ----------------------------------------------------

install: all $(WEB_TARGET)
#	$(INSTALL_DIR) $(INSTALL_DST)/src
#	$(INSTALL_DATA) $(ERL_FILES) $(INSTALL_DST)/src
#	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) $(INSTALL_DST)/src
#	$(INSTALL_DIR) $(INSTALL_DST)/include
#	$(INSTALL_DATA) $(HRL_FILES) $(INSTALL_DST)/include
#	$(INSTALL_DIR) $(INSTALL_DST)/ebin
#	$(INSTALL_DATA) $(TARGET_FILES) $(INSTALL_DST)/ebin
