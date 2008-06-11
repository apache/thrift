MODULES = \
	src

all clean docs:
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done

install: all
	echo 'No install target, sorry.'

check: all

distclean: clean

# Hack to make "make dist" work.
# This should not work, but it appears to.
distdir:
