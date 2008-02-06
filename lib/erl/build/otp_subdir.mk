# Comment by tfee 2004-07-01
# ==========================
# This file is a mod of the stock OTP one.
# The change allows make to stop when a compile error occurs.
# This file needs to go into two places:
#     /usr/local/include/erlang
#     /opt/OTP_SRC/make
#
# where OTP_SRC is a symbolic link to a peer directory containing
# the otp source, e.g. otp_src_R9C-2.
#
# After installing OTP, running sudo make install in otp/build
# will push this file out to the two places listed above.
#
# The mod involves setting the shell variable $short_circuit, which we
# introduce - ie it is not in the stock file. This variable is tested
# to affect execution flow and is also returned to affect the flow in
# the calling script (this one). The latter step is necessary because
# of the recursion involved.
# =====================================================================


# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
#
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
#
#     $Id: otp_subdir.mk,v 1.5 2004/07/12 15:12:23 jeinhorn Exp $
#
#
# Make include file for otp

.PHONY: debug opt release docs release_docs tests release_tests \
	clean depend

#
# Targets that don't affect documentation directories
#
debug opt release docs release_docs tests release_tests clean depend: prepare
	@set -e ;							\
	app_pwd=`pwd` ;							\
	if test -f vsn.mk; then						\
	    echo "=== Entering application" `basename $$app_pwd` ;	\
	fi ;								\
	case "$(MAKE)" in *clearmake*) tflag="-T";; *) tflag="";; esac;	\
	short_circuit=0 ;						\
	for d in $(SUB_DIRECTORIES); do					\
	    if [[ $$short_circuit = 0 ]]; then				\
		if test -f $$d/SKIP ; then				\
		    echo "=== Skipping subdir $$d, reason:" ;		\
		    cat $$d/SKIP ;					\
		    echo "===" ;					\
		else							\
		    if test ! -d $$d ; then				\
			echo "=== Skipping subdir $$d, it is missing" ;	\
		    else						\
			xflag="" ;					\
			if test -f $$d/ignore_config_record.inf; then	\
			    xflag=$$tflag ;				\
			fi ;						\
			(cd $$d && $(MAKE) $$xflag $@) ;		\
			if [[ $$? != 0 ]]; then				\
			    short_circuit=1 ;				\
			fi ;						\
		    fi ;						\
		fi ;							\
	    fi ;							\
	done ;								\
	if test -f vsn.mk; then						\
	    echo "=== Leaving application" `basename $$app_pwd` ;	\
	fi ;								\
	exit $$short_circuit

prepare:
	echo
