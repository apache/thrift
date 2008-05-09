EXTRA_DIST = build.xml src LICENSE

all-local:
	$(ANT)

install-exec-hook:
	$(ANT) install

# Make sure this doesn't fail if ant is not configured.
clean-local:
	ANT=$(ANT) ; if test -z "$$ANT" ; then ANT=: ; fi ; \
	$$ANT clean

check-local: all
