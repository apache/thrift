#!/bin/sh
set -eu

cd "$(dirname "$0")"

all_platforms=false
if [ "${1:-}" = "--all-platforms" ]; then
	all_platforms=true
	shift
fi

if [ "$#" -ne 0 ]; then
	echo "usage: $0 [--all-platforms]" >&2
	exit 64
fi

server_platform="$(docker version --format '{{.Server.Os}}/{{.Server.Arch}}')"
case "$server_platform" in
	linux/x86_64) server_platform=linux/amd64 ;;
	linux/aarch64) server_platform=linux/arm64 ;;
esac

if [ "$all_platforms" = true ]; then
	platforms="linux/amd64 linux/arm64"
else
	platforms="$server_platform"
fi

mkdir -p tmp

image_tag() {
	platform_slug="$(echo "$3" | tr '/' '-')"
	echo "thrift-docker-test:$1-$2-$platform_slug"
}

version_field() {
	python3 - "$1" "$2" <<'PY'
import json
import sys
from pathlib import Path

data = json.loads(Path("versions.json").read_text())
print(data["versions"][sys.argv[1]][sys.argv[2]])
PY
}

maintained_versions() {
	python3 - <<'PY'
import json
from pathlib import Path

data = json.loads(Path("versions.json").read_text())
versions = sorted(data["versions"], key=lambda version: tuple(int(part) for part in version.split(".")), reverse=True)
for version in versions[: int(data.get("maintained_releases", 2))]:
    print(version)
PY
}

file_owner() {
	if stat -c '%u:%g' "$1" >/dev/null 2>&1; then
		stat -c '%u:%g' "$1"
	else
		stat -f '%u:%g' "$1"
	fi
}

smoke_image() {
	image="$1"
	platform="$2"
	version="$3"
	variant="$4"

	if docker run --rm --platform "$platform" "$image" > tmp/no-args.out 2>&1; then
		:
	fi
	grep -F "Usage: thrift" tmp/no-args.out >/dev/null
	docker run --rm --platform "$platform" "$image" --version | grep -F "$version" >/dev/null
	docker run --rm --platform "$platform" "$image" thrift --version | grep -F "$version" >/dev/null
	docker run --rm --platform "$platform" "$image" sh -c 'command -v thrift' >/dev/null

	out_dir="tmp/out-$version-$variant-$(echo "$platform" | tr '/' '-')"
	rm -rf "$out_dir"
	mkdir -p "$out_dir"

	docker run --rm --platform "$platform" \
		-u "$(id -u):$(id -g)" \
		-v "$PWD:/data" \
		"$image" --gen json -o /data/"$out_dir" /data/tests/smoke.thrift
	find "$out_dir" -type f | grep -q .

	docker run --rm --platform "$platform" \
		-u "$(id -u):$(id -g)" \
		-v "$PWD:/data" \
		"$image" --gen py -o /data/"$out_dir" /data/tests/smoke.thrift
	find "$out_dir" -type f | grep -q .

	if [ "$platform" = "$server_platform" ]; then
		first_file="$(find "$out_dir" -type f | head -n 1)"
		expected_owner="$(id -u):$(id -g)"
		actual_owner="$(file_owner "$first_file")"
		if [ "$actual_owner" != "$expected_owner" ]; then
			echo "generated file has owner $actual_owner, expected $expected_owner: $first_file" >&2
			exit 1
		fi
	fi
}

for version in $(maintained_versions); do
	for platform in $platforms; do
		for variant in debian alpine; do
			case "$variant" in
				debian)
					dockerfile="$version/Dockerfile"
					;;
				alpine)
					dockerfile="$version/Dockerfile.alpine"
					;;
			esac

			image="$(image_tag "$version" "$variant" "$platform")"
			echo "Building $image for $platform"
			docker buildx build --platform "$platform" --load -t "$image" -f "$dockerfile" .
			echo "Testing $image for $platform"
			smoke_image "$image" "$platform" "$version" "$variant"
		done
	done
done

./generate-stackbrew-library.sh > tmp/stackbrew-library
if grep -q 'GitCommit: 0000000000000000000000000000000000000000' tmp/stackbrew-library; then
	echo "generated stackbrew metadata contains the placeholder GitCommit" >&2
	exit 1
fi

ALLOW_DIRTY_OFFICIAL_IMAGES_MANIFEST=1 ./generate-official-images-library.sh tmp/thrift-official-images-library
