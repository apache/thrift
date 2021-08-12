#!/bin/bash

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
