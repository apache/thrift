ACLOCAL_AMFLAGS = -I ./aclocal

SUBDIRS = compiler/cpp lib test

dist-hook:
	find $(distdir) -name '.[a-zA-Z0-9]*' | xargs rm -rf

EXTRA_DIST = bootstrap.sh cleanup.sh print_version.sh doc tutorial contrib \
             CONTRIBUTORS LICENSE
