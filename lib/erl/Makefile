MODULES = \
	src

all clean docs:
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done

check: all

distclean: clean

# Hack to make "make dist" work.
# This should not work, but it appears to.
distdir:
