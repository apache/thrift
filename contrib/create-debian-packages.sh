#!/bin/sh
set -e

ln -s contrib/debian debian

dpkg-buildpackage -rfakeroot -tc

# results
ls ../*.deb ../*.dsc ../*.tar.gz ../*.changes

# cleanup
rm debian

