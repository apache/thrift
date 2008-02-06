# for testing erlang files directly. The set up for a
# this type of test would be
# files to test reside in lib/<app_name>/src and the test files which are
# just plain erlang code reside in lib/<app_name>/test
#
# This color codes emitted while the tests run assume that you are using
# a white-on-black display schema. If not, e.g. if you use a white
# background, you will not be able to read the "WHITE" text.
# You can override this by supplying your own "white" color,
# which may in fact be black! You do this by defining an environment
# variable named "MY_WHITE" and setting it to $'\e[0;30m' (which is
# simply bash's way of specifying "Escape [ 0 ; 3 0 m").
# Similarly, you can set your versions of the standard colors
# found in colors.mk.

test:
	@TEST_MODULES=`ls *_test.erl`; \
	trap "echo $(OFF)$(NO_COLOR); exit 1;" 1 2 3 6; \
	for d in $$TEST_MODULES; do \
		echo $${MY_GREEN:-$(GREEN)}"Testing File $$d" $${MY_WHITE:-$(WHITE)}; \
		echo -n $${MY_BLUE:-$(BLUE)}; \
		erl -name $(APP_NAME) $(TEST_LIBS) \
		-s `basename $$d .erl` all -s init stop -noshell; \
		if [ $$? -ne 0 ]; then \
			echo $${MY_LRED:-$(LRED)}"$$d Test Failed"; \
		fi; \
		echo -n $(OFF)$(NO_COLOR); \
	done

