#! /bin/bash

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
# This script allows you to run coverity on the project and submit the
# results.  Do this inside the docker build container.  Only works if
# you are a coverity scan thrift project admin with access to the
# necessary security token.
#
# Environment Variables
#
# COVERITY_SCAN_NOTIFICATION_EMAIL  - email address to notify
# COVERITY_SCAN_TOKEN               - the Coverity Scan token (should be secure)
# VERSION                           - the version to report we scanned

set -ex

wget -nv https://entrust.com/root-certificates/entrust_l1k.cer -O /tmp/scanca.cer

pushd /tmp
if [[ "$1" != "--skipdownload" ]]; then
  rm -rf coverity_tool.tgz cov-analysis*
  wget -nv -O coverity_tool.tgz https://scan.coverity.com/download/cxx/linux64 --post-data "project=thrift&token=$COVERITY_SCAN_TOKEN"
  tar xzf coverity_tool.tgz
fi
COVBIN=$(echo $(pwd)/cov-analysis*/bin)
export PATH=$COVBIN:$PATH
popd

./bootstrap.sh
./configure $*
rm -rf cov-int/
cov-build --dir cov-int make check -j3
tail -50 cov-int/build-log.txt 
tar cJf cov-int.tar.xz cov-int/
curl --cacert /tmp/scanca.cer \
     --form token="$COVERITY_SCAN_TOKEN" \
     --form email="$COVERITY_SCAN_NOTIFICATION_EMAIL" \
     --form file=@cov-int.tar.xz \
     --form version="$VERSION" \
     --form description="thrift master" \
     https://scan.coverity.com/builds?project=thrift
