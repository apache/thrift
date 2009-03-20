EXTRA_DIST = build.xml src test LICENSE

all-local:
	$(ANT)

install-exec-hook:
	$(ANT) install -Dinstall.path=$(DESTDIR)$(JAVA_PREFIX) \
		-Dinstall.javadoc.path=$(DESTDIR)$(docdir)/java

# Make sure this doesn't fail if ant is not configured.
clean-local:
	ANT=$(ANT) ; if test -z "$$ANT" ; then ANT=: ; fi ; \
	$$ANT clean

check-local: all
	$(ANT) test

