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

all-local:
	./gradlew $(GRADLE_OPTS) compile \
		-Prelease=true \
		--console=plain

install-exec-hook:
	./gradlew $(GRADLE_OPTS) publishToMavenLocal \
		-Prelease=true \
		--console=plain

clean-local:
	./gradlew $(GRADLE_OPTS) clean \
		-Prelease=true \
                --console=plain
	$(RM) -r .gradle

check-local: $(THRIFT)
	./gradlew $(GRADLE_OPTS) test \
		-Prelease=true \
		--console=plain

maven-publish:
	./gradlew $(GRADLE_OPTS) publishMavenPublicationToMavenRepository \
		-Prelease=true \
                -Psign=true \
		--console=plain

dist-hook:
	$(RM) -r $(distdir)/.gradle/

EXTRA_DIST = \
	CMakeLists.txt \
	README.md \
	build.gradle \
	coding_standards.md \
	gradle \
	gradle.properties \
	gradlew \
	gradlew.bat \
	settings.gradle \
	src
