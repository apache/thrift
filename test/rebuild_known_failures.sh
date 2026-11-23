#!/bin/bash

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

if [ -z $1 ]; then
  echo Usage: $0 LANGUAGE
  echo Re-list all failures of a specific LANGUAGE into known_failures_Linux.json
  echo LANGUAGE should be library name like cpp, java, py etc
  exit 1
fi

if [ -z $PYTHON]; then
  PYTHON=python
fi

TARGET_LANG=$1
OUT_FILE=known_failures_Linux.json
echo Rebuilding known failures for $TARGET_LANG

TMPFILE=.__tmp__rebuild__
grep -v -e "\"$1-" -e "\-$1_" $OUT_FILE > $TMPFILE
mv $TMPFILE $OUT_FILE
$PYTHON test.py --client $1
$PYTHON test.py -U merge
$PYTHON test.py --server $1
$PYTHON test.py -U merge
