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

ACLOCAL_AMFLAGS = -I ./aclocal

SUBDIRS = compiler/cpp lib

if WITH_TESTS
SUBDIRS += test
endif

if WITH_TUTORIAL
SUBDIRS += tutorial
endif

clean-local:
	$(RM) -r vendor/

distclean-local:
	$(RM) -r .dub/
	$(RM) -r autom4te.cache/

CLEANFILES = \
	composer.lock \
	dub.selections.json

DISTCLEANFILES = \
	Makefile \
	Makefile.in \
	aclocal.m4 \
	apache-thrift-test-library \
	autoscan.log \
	compile \
	config.guess \
	config.hin \
	config.hin~ \
	config.log \
	config.status \
	config.sub \
	configure \
	configure.scan \
    debcomp \
    install-sh \
    ltmain.sh \
    missing \
    ylwrap

dist-hook:
	find $(distdir) -type f \( -iname ".DS_Store" -or -iname "._*" -or -iname ".gitignore" \) | xargs rm -f
	find $(distdir) -type d \( -iname ".deps" -or -iname ".libs" \) | xargs rm -rf
	find $(distdir) -type d \( -iname ".svn" -or -iname ".git" \) | xargs rm -rf

print-version:
	@echo $(PACKAGE_VERSION)

.PHONY: precross cross
precross-%: all
	$(MAKE) -C $* precross
precross: all precross-test precross-lib

empty :=
space := $(empty) $(empty)
comma := ,

CROSS_LANGS = @MAYBE_CPP@ @MAYBE_C_GLIB@ @MAYBE_CL@ @MAYBE_D@ @MAYBE_JAVA@ @MAYBE_PYTHON@ @MAYBE_PY3@ @MAYBE_RUBY@ @MAYBE_HASKELL@ @MAYBE_PERL@ @MAYBE_PHP@ @MAYBE_GO@ @MAYBE_NODEJS@ @MAYBE_DART@ @MAYBE_ERLANG@ @MAYBE_LUA@ @MAYBE_RS@ @MAYBE_NETSTD@ @MAYBE_NODETS@
CROSS_LANGS_COMMA_SEPARATED = $(subst $(space),$(comma),$(CROSS_LANGS))

if WITH_PY3
CROSS_PY=$(PYTHON3)
else
CROSS_PY=$(PYTHON)
endif

if WITH_PYTHON
crossfeature: precross
	$(CROSS_PY) test/test.py --retry-count 5 --features .* --skip-known-failures --server $(CROSS_LANGS_COMMA_SEPARATED)
else
# feature test needs python build
crossfeature:
endif

cross-%: precross crossfeature
	$(CROSS_PY) test/test.py --retry-count 5 --skip-known-failures --server $(CROSS_LANGS_COMMA_SEPARATED) --client $(CROSS_LANGS_COMMA_SEPARATED) --regex "$*"

cross: cross-.*

TIMES = 1 2 3
fail: precross
	$(CROSS_PY) test/test.py || true
	$(CROSS_PY) test/test.py --update-expected-failures=overwrite
	$(foreach var,$(TIMES),test/test.py -s || true;test/test.py --update-expected-failures=merge;)

codespell_skip_files = \
	*.jar \
	*.class \
	*.so \
	*.a \
	*.la \
	*.o \
	*.p12 \
	*OCamlMakefile \
	.keystore \
	.truststore \
	CHANGES \
	config.sub \
	configure \
	depcomp \
	libtool.m4 \
	output.* \
	rebar \
	thrift

skipped_files = $(subst $(space),$(comma),$(codespell_skip_files))

style-local:
	codespell --write-changes --skip=$(skipped_files) --disable-colors

EXTRA_DIST = \
	.asf.yaml \
	.clang-format \
	.dockerignore \
	.editorconfig \
	.eslintignore \
	.eslintrc.json \
	.flake8 \
	.gitattributes \
	.gitignore \
	.travis.yml \
	.rustfmt.toml \
	ApacheThrift.nuspec \
	appveyor.yml \
	bootstrap.sh \
	bower.json \
	build \
	CHANGES.md \
	CMakeLists.txt \
	composer.json \
	contrib \
	CONTRIBUTING.md \
	debian \
	doap.rdf \
	doc \
	dub.json \
	jitpack.yml \
	LANGUAGES.md \
	LICENSE \
	NOTICE \
	package.json \
	package-lock.json \
	phpcs.xml.dist \
	README.md \
	sonar-project.properties \
	Thrift.podspec
