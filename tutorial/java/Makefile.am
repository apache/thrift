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

export CLASSPATH

# Make sure this doesn't fail if gradle is not configured.
clean-local:
	GRADLE=$(GRADLE) ; if test -z "$$GRADLE" ; then GRADLE=: ; fi ; \
	$$GRADLE $(GRADLE_OPTS) clean --console=plain

all-local:
	$(GRADLE) $(GRADLE_OPTS) assemble \
		-Pthrift.version=$(PACKAGE_VERSION) \
		-Pthrift.compiler=$(THRIFT) \
		--console=plain

check-local: all
	$(GRADLE) $(GRADLE_OPTS) test \
		-Pthrift.version=$(PACKAGE_VERSION) \
		-Pthrift.compiler=$(THRIFT) \
		--console=plain

tutorial: all
	$(GRADLE) $(GRADLE_OPTS) tutorial \
		-Pthrift.version=$(PACKAGE_VERSION) \
		-Pthrift.compiler=$(THRIFT) \
		--console=plain

tutorialserver: all
	$(GRADLE) $(GRADLE_OPTS) tutorialServer \
		-Pthrift.version=$(PACKAGE_VERSION) \
		-Pthrift.compiler=$(THRIFT) \
		--console=plain

tutorialclient: all
	$(GRADLE) $(GRADLE_OPTS) tutorialClient \
		-Pthrift.version=$(PACKAGE_VERSION) \
		-Pthrift.compiler=$(THRIFT) \
		--console=plain

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	build.gradle \
	settings.gradle \
	src \
	README.md
