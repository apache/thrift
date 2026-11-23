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

#
# This script is intended to be used after tagging the repository and updating
# the version files for a release.  It will create a CPAN archive.  Run this
# from inside a docker image like ubuntu-jammy.
#

set -e

rm -f MANIFEST
rm -rf Thrift-*

# setup cpan without a prompt
echo | cpan
cpan install HTTP::Date Log::Log4perl
cpan install CPAN
cpan install CPAN::Meta ExtUtils::MakeMaker JSON::PP
# cpan install Module::Signature

perl Makefile.PL
rm MYMETA.yml
make manifest
make dist

#
# We unpack the archive so we can add version metadata for CPAN
# so that it properly indexes Thrift and remove unnecessary files.
#

echo '-----------------------------------------------------------'
set -x

DISTFILE=$(ls Thrift*.gz)
NEWFILE=${DISTFILE/t-v/t-}
if [[ "$DISTFILE" != "$NEWFILE" ]]; then
    mv $DISTFILE $NEWFILE
    DISTFILE="$NEWFILE"
fi
tar xzf $DISTFILE
rm $DISTFILE
DISTDIR=$(ls -d Thrift*)
# cpan doesn't like "Thrift-v0.nn.0 as a directory name
# needs to be Thrift-0.nn.0
NEWDIR=${DISTDIR/t-v/t-}
if [[ "$DISTDIR" != "$NEWDIR" ]]; then
    mv $DISTDIR $NEWDIR
    DISTDIR="$NEWDIR"
fi
cd $DISTDIR
cp -p ../Makefile.PL .
cp -pr ../gen-perl .
cp -pr ../gen-perl2 .
perl ../tools/FixupDist.pl
cd ..
tar cvzf $DISTFILE $DISTDIR
rm -r $DISTDIR
