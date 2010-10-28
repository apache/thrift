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

SUBDIRS = \
  cpp

if WITH_MONO
SUBDIRS += csharp
endif

if WITH_JAVA
SUBDIRS += java
endif

if WITH_PYTHON
SUBDIRS += py
endif

if WITH_ERLANG
SUBDIRS += erl
endif

if WITH_RUBY
SUBDIRS += rb
endif

if WITH_PERL
SUBDIRS += perl
endif

# All of the libs that don't use Automake need to go in here
# so they will end up in our release tarballs.
EXTRA_DIST = \
  cocoa \
  hs    \
  ocaml \
  php   \
  erl   \
  st
