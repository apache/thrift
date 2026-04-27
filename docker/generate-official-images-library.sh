#!/bin/sh
set -eu

cd "$(dirname "$0")"

usage() {
	echo "usage: $0 [output-file]" >&2
	echo "default output-file: tmp/thrift-official-images-library" >&2
}

if [ "$#" -gt 1 ]; then
	usage
	exit 64
fi

output="${1:-tmp/thrift-official-images-library}"
repo_root="$(git rev-parse --show-toplevel)"
git_commit="${GIT_COMMIT:-$(git -C "$repo_root" rev-parse HEAD)}"

case "$git_commit" in
	0000000000000000000000000000000000000000)
		echo "refusing to generate an Official Images manifest with the placeholder GitCommit" >&2
		exit 1
		;;
esac

if [ "${ALLOW_DIRTY_OFFICIAL_IMAGES_MANIFEST:-0}" != "1" ] &&
	{ ! git -C "$repo_root" diff --quiet -- docker ||
		! git -C "$repo_root" diff --cached --quiet -- docker; }; then
	echo "commit Docker packaging changes before generating the Official Images manifest" >&2
	exit 1
fi

mkdir -p "$(dirname "$output")"
target="$(cd "$(dirname "$output")" && pwd -P)/$(basename "$output")"

GIT_COMMIT="$git_commit" ./generate-stackbrew-library.sh > "$target"

if grep -q 'GitCommit: 0000000000000000000000000000000000000000' "$target"; then
	echo "generated manifest still contains the placeholder GitCommit: $target" >&2
	exit 1
fi

echo "Wrote $target with GitCommit: $git_commit"
