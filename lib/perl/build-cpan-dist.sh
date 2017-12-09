#!/bin/bash
#
# This script is intended to be used after tagging the repository and updating
# the version files for a release.  It will create a CPAN archive.  Run this
# from inside a docker image like ubuntu-xenial.
#

set -e

rm MANIFEST
rm -rf Thrift-*

# setup cpan without a prompt
echo | cpan
cpan install HTTP::Date
cpan install CPAN
cpan install CPAN::Meta ExtUtils::MakeMaker JSON::PP

perl Makefile.PL
rm MYMETA.yml
make
make manifest
make dist

#
# We unpack the archive so we can add version metadata for CPAN
# so that it properly indexes Thrift and remove unnecessary files.
#

echo '-----------------------------------------------------------'
set -x

DISTFILE=$(ls Thrift*.gz)
tar xzf Thrift-*.gz
rm Thrift-*.gz
DISTDIR=$(ls -d Thrift*)
cd $DISTDIR
perl ../tools/FixupDist.pl
cd ..
tar cvzf $DISTFILE $DISTDIR
rm -r $DISTDIR
