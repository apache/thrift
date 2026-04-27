#!/bin/sh
set -eu

cd "$(dirname "$0")"

repo_root="$(git rev-parse --show-toplevel)"
export GIT_COMMIT="${GIT_COMMIT:-$(git -C "$repo_root" rev-parse HEAD)}"

python3 - "$@" <<'PY'
import json
import os
from pathlib import Path


def version_key(version):
    return tuple(int(part) for part in version.split("."))


def minor(version):
    major, minor_, _patch = version.split(".")
    return f"{major}.{minor_}"


def newest_by(items, key_func):
    result = {}
    for version, meta in items:
        key = key_func(version, meta)
        if key not in result or version_key(version) > version_key(result[key][0]):
            result[key] = (version, meta)
    return result


root = Path(".")
data = json.loads((root / "versions.json").read_text())
versions = sorted(data["versions"].items(), key=lambda item: version_key(item[0]), reverse=True)
maintained = versions[: int(data.get("maintained_releases", 2))]

git_commit = os.environ["GIT_COMMIT"]
latest_version = maintained[0][0]
latest_minor = newest_by(maintained, lambda version, _meta: minor(version))
latest_debian_base = newest_by(maintained, lambda _version, meta: meta["debian"])
latest_alpine_base = newest_by(maintained, lambda _version, meta: meta["alpine"])

print("# generated via ./generate-stackbrew-library.sh; do not edit directly")
print("Maintainers: Apache Thrift Developers <dev@thrift.apache.org> (@asfbot),")
print("             Jens Geyer (@Jens-G),")
print("             Randy Abernethy (@RandyAbernethy),")
print("             Duru Can Celasun (@dcelasun)")
print("GitRepo: https://github.com/apache/thrift.git")
print()

for version, meta in maintained:
    version_minor = minor(version)
    debian = meta["debian"]
    alpine = meta["alpine"]

    debian_tags = [version]
    if latest_minor[version_minor][0] == version:
        debian_tags.append(version_minor)
    if version == latest_version:
        debian_tags.append("latest")
    debian_tags.extend([f"{version}-{debian}"])
    if latest_minor[version_minor][0] == version:
        debian_tags.append(f"{version_minor}-{debian}")
    if latest_debian_base[debian][0] == version:
        debian_tags.append(debian)

    print(f"Tags: {', '.join(debian_tags)}")
    print("Architectures: amd64, arm64v8")
    print(f"GitCommit: {git_commit}")
    print("Directory: docker")
    print(f"File: {version}/Dockerfile")
    print()

    alpine_tags = [f"{version}-alpine{alpine}"]
    if latest_minor[version_minor][0] == version:
        alpine_tags.append(f"{version_minor}-alpine{alpine}")
    if latest_alpine_base[alpine][0] == version:
        alpine_tags.append(f"alpine{alpine}")
    alpine_tags.append(f"{version}-alpine")
    if latest_minor[version_minor][0] == version:
        alpine_tags.append(f"{version_minor}-alpine")
    if version == latest_version:
        alpine_tags.append("alpine")

    print(f"Tags: {', '.join(alpine_tags)}")
    print("Architectures: amd64, arm64v8")
    print(f"GitCommit: {git_commit}")
    print("Directory: docker")
    print(f"File: {version}/Dockerfile.alpine")
    if version != maintained[-1][0]:
        print()
PY
