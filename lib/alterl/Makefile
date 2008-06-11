MODULES = \
	src

all clean docs:
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done

install: all
	echo 'No install target, sorry.'
