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

set -o errexit
set -o pipefail
set -o nounset

if ! [[ $# -eq 1 && $1 =~ ^[0-9](\.[0-9][0-9]*){2}$ ]]; then
    (>&2 echo "Usage: ./publish-crate.sh [THRIFT_RELEASE_VERSION] ")
    (>&2 echo "       THRIFT_RELEASE_VERSION is in semantic versioning format, i.e. #.##.##")
    exit 1
fi

THRIFT_RELEASE_VERSION=${1:-}

echo "Updating Cargo.toml to ${THRIFT_RELEASE_VERSION}"
sed -i.old -e "s/^version = .*$/version = \"${THRIFT_RELEASE_VERSION}\"/g" Cargo.toml
rm Cargo.toml.old

echo "Committing updated Cargo.toml"
git add Cargo.toml
git commit -m "Update thrift crate version to ${THRIFT_RELEASE_VERSION}" -m "Client: rs"

echo "Packaging and releasing rust thrift crate with version ${THRIFT_RELEASE_VERSION}"
cargo clean
cargo package
cargo publish
