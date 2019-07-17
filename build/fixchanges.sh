#!/usr/bin/env bash
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

#
# fixchanges will take a file as input and look for text matching
# the pattern [THRIFT-nnnn] (the number of digits is not important)
# which is not a markdown link, and change it to be a markdown link.
# The tool writes to stdout so you can redirect it to a temporary
# file and then compare against the original file before replacing
# it.
#
# This tool was developed after the 0.12.0 release to assist with
# generation of CHANGES.md content.
#

while IFS='' read -r line || [[ -n "$line" ]]; do
    if [[ "$line" =~ ^(.*)\[(THRIFT-[[:digit:]]+)\][^\(](.*)$ ]]; then
        echo "${BASH_REMATCH[1]}[${BASH_REMATCH[2]}](https://issues.apache.org/jira/browse/${BASH_REMATCH[2]}) ${BASH_REMATCH[3]}"
    else
        echo "$line"
    fi
done < "$1"
