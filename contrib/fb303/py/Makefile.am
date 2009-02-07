DESTDIR ?= /
EXTRA_DIST = setup.py src

all:

all-local:
	$(thrift_home)/bin/thrift --gen py $(top_srcdir)/if/fb303.thrift
	mv gen-py/fb303/* fb303
	$(PYTHON) setup.py build

# We're ignoring prefix here because site-packages seems to be
# the equivalent of /usr/local/lib in Python land.
# Old version (can't put inline because it's not portable).
#$(PYTHON) setup.py install --prefix=$(prefix) --root=$(DESTDIR) $(PYTHON_SETUPUTIL_ARGS)
install-exec-hook:
	$(PYTHON) setup.py install --root=$(DESTDIR) --prefix=$(PY_PREFIX) $(PYTHON_SETUPUTIL_ARGS)



clean:	clean-local

clean-local:
	$(RM) -r build

check-local: all
