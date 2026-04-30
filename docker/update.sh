#!/bin/sh
set -eu

cd "$(dirname "$0")"

if [ "$#" -ne 1 ]; then
	echo "usage: $0 <version>" >&2
	exit 64
fi

version="$1"
debian="${DEBIAN_VERSION:-trixie}"
alpine="${ALPINE_VERSION:-3.23}"
released="${RELEASED_DATE:-$(date -u +%F)}"
source_url="https://archive.apache.org/dist/thrift/$version/thrift-$version.tar.gz"
keys_url="https://downloads.apache.org/thrift/KEYS"
tmp_dir="tmp/update-$version"

mkdir -p "$tmp_dir"

check_url() {
	label="$1"
	url="$2"

	echo "Checking $label: $url" >&2
	curl -fsSL --range 0-0 -o /dev/null "$url"
}

download() {
	label="$1"
	url="$2"
	output="$3"

	echo "Downloading $label: $url" >&2
	curl -fsSL -o "$output" "$url"
}

if ! check_url "archive.apache.org source tarball" "$source_url"; then
	echo "The Docker metadata uses archive.apache.org source URLs so retained tags remain rebuildable." >&2
	echo "Wait until the ASF archive has synced this release, then rerun update.sh." >&2
	exit 1
fi

download "source tarball" "$source_url" "$tmp_dir/thrift.tar.gz"
download "source signature" "$source_url.asc" "$tmp_dir/thrift.tar.gz.asc"
download "source checksum" "$source_url.sha256" "$tmp_dir/thrift.tar.gz.sha256"
download "Thrift KEYS" "$keys_url" "$tmp_dir/KEYS"

published_sha="$(awk '{ print $1; exit }' "$tmp_dir/thrift.tar.gz.sha256")"
computed_sha="$(
	python3 - "$tmp_dir/thrift.tar.gz" <<'PY'
import hashlib
import sys

h = hashlib.sha256()
with open(sys.argv[1], "rb") as handle:
    for chunk in iter(lambda: handle.read(1024 * 1024), b""):
        h.update(chunk)
print(h.hexdigest())
PY
)"

if [ "$published_sha" != "$computed_sha" ]; then
	echo "published checksum does not match downloaded tarball" >&2
	echo "published: $published_sha" >&2
	echo "computed:  $computed_sha" >&2
	exit 1
fi

GNUPGHOME="$tmp_dir/gnupg"
export GNUPGHOME
mkdir -p "$GNUPGHOME"
chmod 700 "$GNUPGHOME"
gpg --batch --import "$tmp_dir/KEYS" >/dev/null
gpg --batch --status-fd 1 --verify "$tmp_dir/thrift.tar.gz.asc" "$tmp_dir/thrift.tar.gz" > "$tmp_dir/gpg.status"
signer="$(awk '/^\[GNUPG:\] VALIDSIG / { print $3; exit }' "$tmp_dir/gpg.status")"
gpgconf --kill all || true

if [ -z "$signer" ]; then
	echo "could not determine signer fingerprint from gpg status" >&2
	exit 1
fi

source_version="$(
	python3 - <<'PY'
import json
from pathlib import Path

data = json.loads(Path("versions.json").read_text())
versions = sorted(data["versions"], key=lambda version: tuple(int(part) for part in version.split(".")), reverse=True)
print(versions[0])
PY
)"

if [ ! -d "$version" ]; then
	cp -R "$source_version" "$version"
fi

python3 - "$version" "$source_url" "$computed_sha" "$signer" "$released" "$debian" "$alpine" <<'PY'
import json
import re
import sys
from pathlib import Path

version, source_url, sha256, signer, released, debian, alpine = sys.argv[1:]
root = Path(".")


def version_key(value):
    return tuple(int(part) for part in value.split("."))


data = json.loads((root / "versions.json").read_text())
data["versions"][version] = {
    "source": source_url,
    "sha256": sha256,
    "signer": signer,
    "released": released,
    "debian": debian,
    "alpine": alpine,
    "maintained": False,
}

maintained = set(sorted(data["versions"], key=version_key, reverse=True)[: int(data.get("maintained_releases", 2))])
for item_version, meta in data["versions"].items():
    meta["maintained"] = item_version in maintained

(root / "versions.json").write_text(json.dumps(data, indent=2, sort_keys=False) + "\n")


def replace(pattern, replacement, text):
    text, count = re.subn(pattern, replacement, text, flags=re.MULTILINE)
    if count == 0:
        raise SystemExit(f"pattern not found: {pattern}")
    return text


def update_dockerfile(path, base_builder, base_runtime):
    text = path.read_text()
    text = replace(r"^FROM .+ AS builder$", f"FROM {base_builder} AS builder", text)
    text = replace(r"^FROM (?!.* AS builder).+$", f"FROM {base_runtime}", text)
    text = replace(r"^ARG THRIFT_VERSION=.*$", f"ARG THRIFT_VERSION={version}", text)
    text = replace(r"^ARG THRIFT_SOURCE_URL=.*$", f"ARG THRIFT_SOURCE_URL={source_url}", text)
    text = replace(r"^ARG THRIFT_SOURCE_SHA256=.*$", f"ARG THRIFT_SOURCE_SHA256={sha256}", text)
    text = replace(r"^ARG THRIFT_SOURCE_SIGNER=.*$", f"ARG THRIFT_SOURCE_SIGNER={signer}", text)
    path.write_text(text)


update_dockerfile(root / version / "Dockerfile", f"debian:{debian}", f"debian:{debian}-slim")
update_dockerfile(root / version / "Dockerfile.alpine", f"alpine:{alpine}", f"alpine:{alpine}")
PY
