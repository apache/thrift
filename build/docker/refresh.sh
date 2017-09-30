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
# Check Dockerfile hash values.
# If the values have changed, rebuild the image.
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DISTRO=$1
SRC_IMG=thrift/thrift-build:$DISTRO

function dockerfile_changed {
  docker run $SRC_IMG bash -c 'cd .. && sha512sum Dockerfile' > .Dockerfile.sha512
  sha512sum -c .Dockerfile.sha512
}

pushd ${SCRIPT_DIR}/$DISTRO
if dockerfile_changed; then
  echo Dockerfile has not changed.  No need to rebuild.
  exit 0
fi
popd

echo Dockerfile has changed.  Rebuilding docker image $DISTRO.
docker build --tag $SRC_IMG build/docker/$DISTRO

