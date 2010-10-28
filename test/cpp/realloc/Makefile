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

# This probably should not go into "make check", because it is an experiment,
# not a test.  Specifically, it is meant to determine how likely realloc is
# to avoid a copy.  This is poorly documented.

run: realloc_test
	for it in 1 4 64 ; do \
		for nb in 1 8 64 512 ; do \
			for mins in 64 512 ; do \
				for maxs in 2048 262144 ; do \
					for db in 8 64 ; do \
						./realloc_test $$nb $$mins $$maxs $$db $$it \
					; done \
				; done \
			; done \
		; done \
	; done \
	> raw_stats

CFLAGS = -Wall -g -std=c99
LDLIBS = -ldl
realloc_test: realloc_test.c
