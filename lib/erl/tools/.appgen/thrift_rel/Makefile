# ----------------------------------------------------
# Make file for creating an otp release. 
# ----------------------------------------------------

##
# Basename of this release.
##
RELS=$(shell basename `pwd`)
APP_NAME=$(shell echo $(RELS) | sed s/_rel$$//)

include ../../build/otp.mk

include ./vsn.mk

#include $(ERL_TOP)/make/target.mk
#include $(ERL_TOP)/make/$(TARGET)/otp.mk

USR_LIBPATH=../../lib
INSTALL_DIR=/usr/local/lib
ABS_USER_LIBPATH=$(shell cd ../../lib;pwd)

# ----------------------------------------------------
# CREATE DIR STRUCTURE HERE
# ----------------------------------------------------

HTDOCS=$(wildcard $(ABS_USER_LIBPATH)/$(APP_NAME)/htdocs/*.html) \
	$(wildcard $(ABS_USER_LIBPATH)/$(APP_NAME)/htdocs/*.htm) \
	$(wildcard $(ABS_USER_LIBPATH)/$(APP_NAME)/htdocs/*.yaws)
BUILD_FILES=fs_boot_smithe.beam fs_lists.beam fs_lib.beam

LOCAL_DIR=local
#LOCAL_DIR=$(shell cat $(RELS).rel.src |grep -m 1 '$(APP_NAME)' |awk -F '"' '{printf "%s-%s", $$2,$$4}')

DIR_STRUCTURE= \
	$(LOCAL_DIR) \
	$(LOCAL_DIR)/log/$(REL_VSN) \
	$(LOCAL_DIR)/var/$(REL_VSN) \
	$(LOCAL_DIR)/var/$(REL_VSN)/www/conf \
	$(LOCAL_DIR)/var/$(REL_VSN)/www/htdocs 

PRODUCTION_DIR_STRUCTURE= \
	$(RELS) \
	$(RELS)/release/$(REL_VSN) \
	$(RELS)/stage \
	$(RELS)/log/$(REL_VSN) \
	$(RELS)/var/$(REL_VSN) \
	$(RELS)/var/$(REL_VSN)/www \
	$(RELS)/var/$(REL_VSN)/www/htdocs \
	$(RELS)/var/$(REL_VSN)/www/conf

# ----------------------------------------------------
SCRIPT_AND_BOOT_FILES= \
	$(RELS).script \
	$(RELS).boot

LOCAL_SCRIPT_AND_BOOT_FILES= \
	$(LOCAL_DIR)/$(RELS).script \
	$(LOCAL_DIR)/$(RELS).boot

LOCAL_HTTP_CONF= \
	$(LOCAL_DIR)/var/$(REL_VSN)/www/conf/yaws.conf \
	$(LOCAL_DIR)/var/$(REL_VSN)/www/conf/mime.types  

PRODUCTION_HTTP_CONF= \
	$(LOCAL_DIR)/var/$(REL_VSN)/www/conf/yaws.conf \
	$(LOCAL_DIR)/var/$(REL_VSN)/www/conf/mime.types  

LOCAL_TARGET_FILES=$(LOCAL_HTTP_CONF) $(LOCAL_DIR)/$(RELS).config $(LOCAL_SCRIPT_AND_BOOT_FILES)

LOCAL_TARGETS=$(LOCAL_DIR)/$(RELS).sh vsnit $(LOCAL_TARGET_FILES) 

PRODUCTION_TARGETS=$(RELS)/build/$(REL_VSN) \
	           $(RELS)/lib \
                   $(RELS)/stage/$(RELS).rel.src \
	           $(RELS)/stage/$(RELS).config.src \
	           $(RELS)/stage/yaws.conf.src \
		   $(RELS)/stage/$(RELS).sh.src  \
		   $(RELS)/var/$(REL_VSN)/www/htdocs \
		   $(RELS)/install.sh \
		   $(RELS)/release/$(REL_VSN)/clean_release 

# ----------------------------------------------------
# TARGETS
# ----------------------------------------------------

all debug opt instr script:  $(DIR_STRUCTURE) $(LOCAL_TARGETS) $(PRODUCTION_DIR_STRUCTURE) $(PRODUCTION_TARGETS)
	@echo $(HTDOCS)

install:  stage

tar: $(RELS)-$(LOCATION)-$(REL_VSN).tgz

$(DIR_STRUCTURE):
	mkdir -p $@

$(PRODUCTION_DIR_STRUCTURE):
	mkdir -p $@
	
clean:
	$(RM) $(REL_SCRIPTS) $(TARGET_FILES)
	$(RM) -r $(LOCAL_DIR) $(PRODN_DIR)
	$(RM) $(RELS).rel
	$(RM) -r $(RELS)
	$(RM) $(RELS)*.tgz
	$(RM) $(RELS).rel.src.tmp
	$(RM) $(SCRIPT_AND_BOOT_FILES)

docs:
	
# ----------------------------------------------------
# TARGETS FOR LOCAL MODE
# ----------------------------------------------------

# startup script for local mode
$(LOCAL_DIR)/$(RELS).sh: 
	@echo '#!/bin/sh' > $@
	@echo "cd $(CURDIR)/$(LOCAL_DIR)" >> $@
	@echo "erl -name $${USER}_$(RELS) -boot $(RELS) -config $(RELS).config \$$@" >> $@
	chmod +x $@
	@echo
	@echo "==== Start local node with \"sh $@\" ===="
	@echo

# Create the config file for local mode.
$(LOCAL_DIR)/$(RELS).config: $(RELS).config.src
	sed -e 's;%LOG_OTP%;$(CURDIR)/$(LOCAL_DIR)/log/$(REL_VSN);' \
	    -e 's;%VAR_OTP%;$(CURDIR)/$(LOCAL_DIR)/var/$(REL_VSN);' \
            -e 's;%RELS%;$(RELS);g' \
            -e 's;%HOME%;$(HOME);g' \
            -e 's;%BROADCAST_ADDRESS%;$(BROADCAST_ADDRESS);g' \
            -e 's;%CONTACT_NODE%;$(CONTACT_NODE);g' \
	    -e "s;%HOSTNAME%;`hostname --long`;" \
            -e 's;%APP_NAME%;$(APP_NAME);' \
	    -e 's;%APP_VERSION%;$(APP_VERSION);g' \
             $< > $@

# Create the httpd conf file for local mode.
$(LOCAL_DIR)/var/$(REL_VSN)/www/conf/yaws.conf: yaws.conf.src
	sed -e 's;%VAR_OTP%;$(CURDIR)/$(LOCAL_DIR)/var/$(REL_VSN);' \
	    -e 's;%LOG_OTP%;$(CURDIR)/$(LOCAL_DIR)/log/$(REL_VSN);' \
	    -e 's;%HTDOC_ROOT%;$(ABS_USER_LIBPATH)/$(APP_NAME)/htdocs;' \
            -e 's;%APP_NAME%;$(APP_NAME);' \
            -e 's;%RELS%;$(RELS);' \
            -e 's;%USER%;$(USER);' \
	    -e 's;%HTDOC_ROOT%;$(ABS_USER_LIBPATH);' \
            -e 's;%MHOST%;$(MHOST);' \
            $< > $@

# Create the config file for local mode.
vsnit: $(RELS).rel.src 
	sed -e 's;%REL_VSN%;$(REL_VSN);' \
	       $< > $<.tmp

# Create and position script and boot files for local mode.
$(LOCAL_SCRIPT_AND_BOOT_FILES): 
	@ erl -pz $(USR_LIBPATH)/fslib/ebin \
	      -noshell \
	      -s fs_lib s_apply fs_boot_smithe make_script_and_boot "[\"$(ERL_RUN_TOP)/*\",  \"$(USR_LIBPATH)\"]. " \
                                                                     \"$$(basename `pwd`)".rel.src.tmp\". " \
                                                                     "[local]. " \
	      -s init stop
	cp $(SCRIPT_AND_BOOT_FILES) $(LOCAL_DIR)/ 

$(LOCAL_DIR)/var/$(REL_VSN)/www/conf/mime.types: ../../build/mime.types
	cp $< $@

# ----------------------------------------------------
# TARGETS FOR PRODUCTION MODE
# ----------------------------------------------------
$(RELS)/lib: 
	# For some reason this will not happen if added to PRODUCTION_DIR_STRUCTURE
	mkdir $@
	@ erl -pz $(RELS)/build/$(REL_VSN) \
	      -noshell \
	      -s fs_lib s_apply fs_boot_smithe stage_from_relsrc "[\"$(USR_LIBPATH)\"]. " \
                                                                 \"$$(basename `pwd`)".rel.src\". " \
                                                                 \"$@\"". " \
	      -s init stop

# Move the htdocs from the local apps to the production htdoc root directory.
$(RELS)/var/$(REL_VSN)/www/htdocs/: $(HTDOCS)
	@mkdir -p $(RELS)/var/$(REL_VSN)/www/htdocs; \
	for x in $(HTDOCS);do \
 		cp  $$x $@; \
	done

# startup script for production mode
$(RELS)/stage/$(RELS).sh.src: 
	@echo '#!/bin/sh' > $@
	@echo "cd %INSTALL_DIR%/$(RELS)/release/$(REL_VSN)" >> $@
	@echo "erl -name $(RELS) -boot $(RELS) -config $(RELS).config -detached \$$@" >> $@
	chmod +x $@

$(RELS)/build/$(REL_VSN):  $(USR_LIBPATH)/fslib/ebin
	mkdir -p $(RELS)/build/$(REL_VSN) 
	cp $</fs_boot_smithe.beam $@
	cp $</fs_lib.beam $@
	cp $</fs_lists.beam $@

$(RELS)/stage/$(RELS).rel.src: $(RELS).rel.src.tmp
	cp $< $@

$(RELS)/stage/$(RELS).config.src: $(RELS).config.src
	cp $< $@

$(RELS)/stage/yaws.conf.src: yaws.conf.src
	cp $< $@

$(RELS)/install.sh: 
	@echo '#!/bin/sh' > $@
	@echo "" >> $@
	@echo "if [ \$$# -eq 1 ];then" >> $@
	@echo "    INSTALL_DIR=\$$1;" >> $@
	@echo "else" >> $@
	@echo "    INSTALL_DIR=$(INSTALL_DIR);" >> $@
	@echo "fi" >> $@
	@echo "" >> $@
	@echo "function munge() {" >> $@
	@echo "    sed -e \"s;%LOG_OTP%;\$$INSTALL_DIR/$(RELS)/log/$(REL_VSN);g\" \\" >> $@
	@echo "        -e \"s;%VAR_OTP%;\$$INSTALL_DIR/$(RELS)/var/$(REL_VSN);g\" \\" >> $@
	@echo "        -e \"s;%RELS%;$(RELS);g\" \\" >> $@
	@echo "        -e \"s;%REL_VSN%;$(REL_VSN);g\" \\" >> $@
	@echo "        -e \"s;%USER%;$$USER;g\" \\" >> $@
	@echo "        -e \"s;%HTDOC_ROOT%;\$$INSTALL_DIR/$(RELS)/var/$(REL_VSN)/www/htdocs;g\" \\" >> $@
	@echo "        -e \"s;%MHOST%;\`hostname\`;g\" \\" >> $@
	@echo "        -e \"s;%BROADCAST_ADDRESS%;$(BROADCAST_ADDRESS);g\" \\" >> $@
	@echo "        -e \"s;%INSTALL_DIR%;\$$INSTALL_DIR;g\" \\" >> $@
	@echo "        -e \"s;%CONTACT_NODE%;$(CONTACT_NODE);g\" \\" >> $@
	@echo "        -e \"s;%HOSTNAME%;\`hostname --long\`;g\" \\" >> $@
	@echo "        -e \"s;%APP_NAME%;$(APP_NAME);g\" \\" >> $@
	@echo "        -e \"s;%APP_VERSION%;$(APP_VERSION);g\" \\" >> $@
	@echo '        $$1 > $$2' >> $@
	@echo "}" >> $@
	@echo "" >> $@
	@echo "munge stage/yaws.conf.src var/$(REL_VSN)/www/conf/yaws.conf;" >> $@
	@echo "munge stage/$(RELS).config.src release/$(REL_VSN)/$(RELS).config;" >> $@
	@echo "munge stage/$(RELS).sh.src release/$(REL_VSN)/$(RELS).sh;" >> $@
	@echo "munge stage/$(RELS).rel.src release/$(REL_VSN)/$(RELS).rel;" >> $@
	@echo "chmod +x release/$(REL_VSN)/$(RELS).sh;" >> $@
	@echo "" >> $@
	@echo "cd ..;" >> $@
	@echo "find $(RELS) | cpio -o > \$$INSTALL_DIR/$(RELS).cpio;" >> $@
	@echo "cd -;" >> $@
	@echo "cd \$$INSTALL_DIR; " >> $@
	@echo "echo -n \"Unpacked: \"" >> $@
	@echo "cpio -uid < $(RELS).cpio;" >> $@
	@echo "rm $(RELS).cpio;" >> $@
	@echo "" >> $@
	@echo "echo \"pwd is \`pwd\`\";" >> $@
	@echo "cd $(RELS);" >> $@
	@echo " erl -pz build/$(REL_VSN) \\" >> $@
	@echo "      -noshell \\" >> $@
	@echo -n "      -s fs_lib s_apply fs_boot_smithe make_script_and_boot \"[\\\"$(ERL_RUN_TOP)/*\\\",  \\\"lib/\\\"]. \" " >> $@
	@echo -n        "\"\\\"stage/$$(basename `pwd`).rel.src\\\". \" " >> $@
	@echo -n        "\"[local]. \" " >> $@
	@echo           "-s init stop | egrep '*terminate*|ERROR'" >> $@
	@echo "if [ \$$? -eq 0 ]; then" >> $@
	@echo "echo \"============================================\";" >> $@
	@echo "echo \"STAGE FAILURE \$$?  - Silence the discord.\";" >> $@
	@echo "echo \"============================================\";" >> $@
	@echo "exit 1;" >> $@
	@echo "fi" >> $@
	@echo "" >> $@
	@echo "mv $(RELS).rel $(RELS).script $(RELS).boot release/$(REL_VSN);" >> $@
	@echo "" >> $@
	@echo "rm -r stage;" >> $@
	@echo "rm -r build;" >> $@
	@echo "cd -;" >> $@
	@echo "" >> $@
	@echo "chgrp -R erts $(RELS); " >> $@
	@echo "chmod -R 775 $(RELS); " >> $@
	@echo "cd -" >> $@
	@echo "" >> $@
	@echo "rm -f /usr/local/bin/$(APP_NAME);" >> $@
	@echo "ln -s \$$INSTALL_DIR/$(RELS)/release/$(REL_VSN)/$(RELS).sh /usr/local/bin/$(APP_NAME);" >> $@
	@echo "chgrp -R erts /usr/local/bin/$(APP_NAME); " >> $@
	@echo "chmod -R 775 /usr/local/bin/$(APP_NAME); " >> $@
	@echo "rm \$$INSTALL_DIR/$(RELS)/install.sh;" >> $@
	@echo "echo -n $$'\e[0;32m'" >> $@
	@echo "echo \"$(APP_NAME) installation to \$$INSTALL_DIR complete.\"" >> $@
	@echo "echo -n $$'\e[0m'" >> $@
	chmod +x $@
		

stage: $(RELS)
	cd $(RELS); \
	./install.sh; \
	cd -

$(RELS)/var/$(REL_VSN)/www/conf/mime.types: ../../build/mime.types
	cp $< $@

$(RELS)-$(LOCATION)-$(REL_VSN).tgz: $(RELS)
	tar -zcvf $@ $<

$(RELS)/release/$(REL_VSN)/clean_release: ../../tools/utilities/clean_release
	cp $< $@

