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

# Make sure this doesn't fail if ant is not configured.
# We call npm install twice to work around older npm issues
# (note these issues may no longer be present, but it is ok)
#

if HAVE_NPM

SUBDIRS = test

prereq:
	$(NPM) install || $(NPM) install
	$(NPM) list

check-local: prereq all
	./node_modules/.bin/grunt

doc: prereq
	./node_modules/.bin/grunt jsdoc

endif

clean-local:
	$(RM) -r dist
	$(RM) -r doc
	$(RM) -r node_modules
	$(RM) -r test/build/
	$(RM) -r test/gen-*/

dist-hook:
	$(RM) -r $(distdir)/dist/
	$(RM) -r $(distdir)/doc/
	$(RM) -r $(distdir)/node_modules/
	$(RM) -r $(distdir)/test/build/
	$(RM) -r $(distdir)/test/gen-*/

EXTRA_DIST = \
	coding_standards.md \
	Gruntfile.js \
	package.json \
	package-lock.json \
	README.md \
	src \
	test
