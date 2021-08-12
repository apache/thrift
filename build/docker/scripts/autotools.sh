#!/bin/sh
set -ev

mkdir ~/.m2
tee >~/.m2/settings.xml <<EOF
<settings xmlns='http://maven.apache.org/SETTINGS/1.0.0'>
  <mirrors>
    <mirror>
      <id>secure-central</id>
      <url>https://repo.maven.apache.org/maven2</url>
      <mirrorOf>central</mirrorOf>
    </mirror>
  </mirrors>
</settings>
EOF

./bootstrap.sh
./configure $*
make check -j3
