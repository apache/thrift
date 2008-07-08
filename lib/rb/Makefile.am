EXTRA_DIST = setup.rb lib ext

all-local:
	$(RUBY) setup.rb config
	$(RUBY) setup.rb setup

install-exec-hook:
	$(RUBY) setup.rb install

clean-local:
	$(RUBY) setup.rb clean

check-local: all
