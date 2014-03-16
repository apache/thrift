some tips on Debian Packaging
- Debian New Maintainers' Guide [http://www.debian.org/doc/debian-policy/]
- Debian Policy Manual [http://www.debian.org/doc/manuals/maint-guide/]
- Machine-readable debian/copyright file [http://dep.debian.net/deps/dep5/]

build
$ dpkg-buildpackage -d -tc
  -d             do not check build dependencies and conflicts.
  -tc            clean source tree when finished.

update changelog
$ date -R

check packages
$ dpkg -c *.deb
$ lintian *.deb

todo
make it perfect!
