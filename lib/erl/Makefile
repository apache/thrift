MODULES = \
	src

all clean docs:
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done

check: all

distclean: clean
