#!/bin/sh

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

cd "`dirname "$0"`"

# Computing both the version and the revision on every invocation is wasteful,
# but it is cheap and avoids the use of nonportable shell functions.

VERSION=`sed -ne 's/^AC_INIT(\[thrift\], \[\(.*\)\])$/\1/p' configure.ac`

if test -d .svn ; then
  REVISION="r`svnversion`"
elif test -d .git ; then
  SHA1=`git rev-list --max-count=1 --grep='^git-svn-id:' HEAD`
  REVISION=`git cat-file commit $SHA1 | sed -ne 's/^git-svn-id:[^@]*@\([0-9][0-9]*\).*/r\1/p'`
  OFFSET=`git rev-list ^$SHA1 HEAD | wc -l | tr -d ' '`
  if test $OFFSET != 0 ; then
    REVISION="$REVISION-$OFFSET-`git rev-parse --verify HEAD | cut -c 1-7`"
  fi
else
  REVISION="exported"
fi

case "$1" in
  -v) echo $VERSION ;;
  -r) echo $REVISION ;;
  -a) echo "$VERSION-$REVISION" ;;
  *) echo "Usage: $0 -v|-r|-a"; exit 1;;
esac
