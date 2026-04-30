# Apache Thrift Docker Official Image Packaging

This directory contains the packaging for the Apache Thrift compiler Docker Official Image.

The image is compiler-only. It contains the `thrift` compiler/code generator and the runtime libraries needed by that binary. It does not include language-specific Thrift runtime libraries or the contributor build images from `build/docker/*`.

Do not push images from this repository. Public images are built and published by Docker Official Images after review in `docker-library/official-images`.

## Usage

Generate code from the current project directory:

```console
docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/data" thrift --gen py -o /data /data/service.thrift
```

The `-u` flag prevents generated files in the bind mount from being owned by root.

Run the compiler version check:

```console
docker run --rm thrift --version
```

Use Alpine explicitly when you need the Alpine/musl variant:

```console
docker pull thrift:0.22-alpine
```

## Files

- `versions.json` records release artifact URLs, checksums, signer fingerprints, release dates, base versions, and maintenance state.
- `0.22.0/Dockerfile` builds the Debian-based image for Thrift 0.22.0.
- `0.22.0/Dockerfile.alpine` builds the Alpine-based image for Thrift 0.22.0.
- `docker-entrypoint.sh` provides the CLI-friendly container entrypoint.
- `tests/smoke.thrift` is the shared smoke-test fixture.
- `generate-stackbrew-library.sh` prints Docker Library metadata for Bashbrew.
- `generate-official-images-library.sh` writes that metadata to `tmp/` or to a local `docker-library/official-images` checkout.

## Updating for a Release

After the ASF release vote passes, release artifacts are promoted, and the website/download page is updated:

```console
cd docker
./update.sh 0.23.0
./test.sh --all-platforms
```

`update.sh` verifies the source tarball checksum and PGP signature, records the signer fingerprint, updates Dockerfile `ARG` values, and applies the two-latest-full-releases retention policy. It requires the release tarball to be available from `archive.apache.org` because committed Docker metadata must remain rebuildable after the release leaves the active download mirrors.

Set base versions explicitly if needed:

```console
DEBIAN_VERSION=trixie ALPINE_VERSION=3.23 RELEASED_DATE=2025-05-14 ./update.sh 0.22.0
```

The script does not scrape the Thrift website. `RELEASED_DATE` is informational metadata.

## Testing

Run native-platform tests:

```console
cd docker
./test.sh
```

Run the supported platform matrix:

```console
cd docker
./test.sh --all-platforms
```

The full local matrix uses Buildx for `linux/amd64` and `linux/arm64`; arm64 tests require QEMU/binfmt support on non-arm64 hosts. Apache CI runs separate native `amd64` and `arm64` jobs.

The tests build Debian and Alpine variants, verify the compiler version, exercise entrypoint behavior, generate JSON and Python output from `tests/smoke.thrift`, and check bind-mount output ownership on the native platform.

## Docker Official Images

Docker Official Images does not run the maintenance scripts in this directory. Its tooling reads `docker-library/official-images/library/thrift`, fetches each `GitCommit`, and builds the referenced `Directory` and `File` entries.

After the Docker packaging commit is on Apache Thrift `master`, generate the submit-ready manifest into `tmp/`:

```console
./generate-official-images-library.sh
```

Or write directly into a local `docker-library/official-images` checkout:

```console
./generate-official-images-library.sh /path/to/official-images/library/thrift
```

Docker Library manifests are generated artifacts; they are not checked into this repository.

Docker Hub documentation is maintained through `docker-library/docs`.

## Release Rules

- Only voted ASF releases may be used.
- Do not publish release candidates, pre-releases, branch builds, nightlies, or continuous builds.
- The Docker Official Image is a convenience artifact built from voted ASF source releases; it is not itself the ASF release artifact.
- The Docker Official Image library tracks the two latest full Apache Thrift releases after Docker image maintenance is restored. Initial restoration starts with 0.22.0 only; 0.21.0 is not backfilled.
- Older tags can remain pullable on Docker Hub after they are removed from the library file, but they are no longer rebuilt or maintained.
