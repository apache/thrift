#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

EXTRA_DIST = \
	CHANGELOG \
	Rakefile \
	Manifest \
	setup.rb \
	lib \
	ext \
	benchmark \
	script \
	spec

DESTDIR ?= /

all-local:
	if [ -n "$(RUBY_PREFIX)" ] ; then $(RUBY) setup.rb config --prefix=$(DESTDIR)$(RUBY_PREFIX) --rbdir='$$libdir/ruby' --sodir='$$libdir/ruby' ; elif [ -n "$(DESTDIR)" ] ; then $(RUBY) setup.rb config --prefix=$(DESTDIR) ; else $(RUBY) setup.rb config ; fi
	$(RUBY) setup.rb setup

install-exec-hook:
	$(RUBY) setup.rb install --prefix=$(DESTDIR)

# Make sure this doesn't fail if Ruby is not configured.
clean-local:
	RUBY=$(RUBY) ; if test -z "$$RUBY" ; then RUBY=: ; fi ; \
	$$RUBY setup.rb clean

check-local: all
if HAVE_RSPEC
	rake spec
endif

