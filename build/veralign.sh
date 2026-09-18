#!/usr/bin/env bash
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
# 
#   http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

#
# The veralign script sets the appropriate versions in all of
# the package configuration files for all of the supported
# languages.  It is used to prepare a release or move master
# forward to the next anticipated version.
#
# USAGE
# -----------------------------------------------------------
# usage: veralign.sh <oldVersion> <newVersion>
#
# EXAMPLE
# -----------------------------------------------------------
# $ ./veralign.sh 0.12.0 1.0.0
# $ ./veralign.sh 1.0.0 1.1.0
#
# IMPORTANT USAGE NOTE
# -----------------------------------------------------------
# Define the environment variable DRYRUN to have the script
# print out all matches to the oldVersion hilighted so that
# you can verify it will change the right things.
#

declare -A FILES

# These files require a manual touch:
FILES[CHANGES.md]=manual
FILES[debian/changelog]=manual
FILES[doap.rdf]=manual

# These files can be updated automatically:
FILES[ApacheThrift.nuspec]=simpleReplace
FILES[appveyor.yml]=simpleReplace
FILES[bower.json]=jsonReplace
FILES[CMakeLists.txt]=simpleReplace
FILES[compiler/cpp/src/thrift/version.h]=simpleReplace
FILES[configure.ac]=configureReplace
FILES[contrib/Rebus/Properties/AssemblyInfo.cs]=simpleReplace
FILES[contrib/thrift.spec]=simpleReplace
FILES[contrib/zeromq/csharp/AssemblyInfo.cs]=simpleReplace
FILES[doc/specs/idl.md]=simpleReplace
FILES[lib/d/src/thrift/base.d]=simpleReplace
FILES[lib/dart/pubspec.yaml]=pubspecReplace
FILES[lib/delphi/src/Thrift.pas]=simpleReplace
FILES[lib/erl/src/thrift.app.src]=simpleReplace
FILES[lib/haxe/haxelib.json]=simpleReplace
FILES[lib/java/gradle.properties]=simpleReplace
FILES[lib/js/package-lock.json]=npmlockReplace
FILES[lib/js/package.json]=jsonReplace
FILES[lib/js/src/thrift.js]=simpleReplace
FILES[lib/lua/Thrift.lua]=simpleReplace
FILES[lib/netstd/Benchmarks/Thrift.Benchmarks/Thrift.Benchmarks.csproj]=simpleReplace
FILES[lib/netstd/Tests/Thrift.Compile.Tests/Thrift.Compile.net10/Thrift.Compile.net10.csproj]=simpleReplace
FILES[lib/netstd/Tests/Thrift.Compile.Tests/Thrift.Compile.net8/Thrift.Compile.net8.csproj]=simpleReplace
FILES[lib/netstd/Tests/Thrift.Compile.Tests/Thrift.Compile.net9/Thrift.Compile.net9.csproj]=simpleReplace
FILES[lib/netstd/Tests/Thrift.Compile.Tests/Thrift.Compile.netstd2/Thrift.Compile.netstd2.csproj]=simpleReplace
FILES[lib/netstd/Tests/Thrift.IntegrationTests/Thrift.IntegrationTests.csproj]=simpleReplace
FILES[lib/netstd/Tests/Thrift.Tests/Thrift.Tests.csproj]=simpleReplace
FILES[lib/netstd/Thrift.AspNetCore/Thrift.AspNetCore.csproj]=simpleReplace
FILES[lib/netstd/Thrift/Properties/AssemblyInfo.cs]=simpleReplace
FILES[lib/netstd/Thrift/Thrift.csproj]=simpleReplace
FILES[lib/ocaml/_oasis]=simpleReplace
FILES[lib/perl/lib/Thrift.pm]=simpleReplace
FILES[lib/py/setup.py]=simpleReplace
FILES[lib/rb/Gemfile.lock]=gemlockReplace
FILES[lib/rb/thrift.gemspec]=simpleReplace
FILES[lib/rs/Cargo.toml]=simpleReplace
FILES[lib/st/package.xml]=simpleReplace
FILES[lib/ts/package-lock.json]=npmlockReplace
FILES[lib/ts/package.json]=jsonReplace
FILES[package-lock.json]=npmlockReplace
FILES[package.json]=jsonReplace
FILES[sonar-project.properties]=simpleReplace
FILES[test/dart/recursion_depth_test/pubspec.yaml]=simpleReplace
FILES[test/dart/test_client/pubspec.yaml]=pubspecReplace
FILES[test/erl/src/thrift_test.app.src]=simpleReplace
FILES[test/netstd/Client/Client.csproj]=simpleReplace
FILES[test/netstd/Server/Server.csproj]=simpleReplace
FILES[test/rb/Gemfile.lock]=gemlockReplace
FILES[tutorial/dart/client/pubspec.yaml]=pubspecReplace
FILES[tutorial/dart/console_client/pubspec.yaml]=pubspecReplace
FILES[tutorial/dart/server/pubspec.yaml]=pubspecReplace
FILES[tutorial/delphi/DelphiClient/DelphiClient.dproj]=simpleReplace
FILES[tutorial/delphi/DelphiServer/DelphiServer.dproj]=simpleReplace
FILES[tutorial/netstd/Client/Client.csproj]=simpleReplace
FILES[tutorial/netstd/Interfaces/Interfaces.csproj]=simpleReplace
FILES[tutorial/netstd/Server/Server.csproj]=simpleReplace
FILES[tutorial/ocaml/_oasis]=simpleReplace



if [ ! -f "CHANGES.md" ]; then
    >&2 echo "error: run veralign.sh while in the thrift root directory"
    exit 1
fi

if [ $# -ne 2 ]; then
    >&2 echo "usage: veralign.sh <oldVersion> <newVersion>"
    exit 1
fi

jq --version 1>/dev/null 2>/dev/null
if [ $? -ne 0 ]; then
    >&2 echo "error: the 'jq' package is not installed"
    exit 1
fi

#
# validateVersion: check that a version matches the major.minor.patch
#   format which is the lowest common denominator supported by all
#   project systems.
# \param $1 the version
# \returns 0 if the version is compliant
#
function validateVersion
{
    local result
    local valid
    valid=$(echo "$1" | sed '/^[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+$/!{q22}')
    result=$?
    if [ $result -eq 22 ]; then
        >&2 echo "error: version '$1' does not conform to the required major.minor.patch format"
        return ${result}
    fi
}

OLDVERSION=$1
NEWVERSION=$2
validateVersion "${OLDVERSION}" || exit $?
validateVersion "${NEWVERSION}" || exit $?

#
# escapeVersion: escape the version for use as a sed search
#   Parentheses become bracket expressions, which match them literally in
#   the extended regex grep uses and in the basic regex sed uses alike.
# \param $1 the version to escape
# \output the escaped string
# \returns 0
# \example VERSEARCH=$(escapeVersion "[1.0.0]"); echo $VERSEARCH; => "\[1\.0\.0\]"
# \example VERSEARCH=$(escapeVersion "(1.0.0)"); echo $VERSEARCH; => "[(]1\.0\.0[)]"
#
function escapeVersion
{
    echo "$(echo "$1" | sed 's/\./\\./g' | sed 's/\[/\\\[/g' | sed 's/\]/\\\]/g' | sed 's/[()]/[&]/g')"
}

# Set up verbose hilighting if running interactive
if [ "$(tput colors)" -ne 0 ]; then
    reverse=$(tput rev)
    red=$(tput setaf 1)
    green=$(tput setaf 2)
    yellow=$(tput setaf 3)
    normal=$(tput sgr0)
fi

declare -A MANUAL

#
# manual: note that update of said file is manual
# \param $1 filename to do replacements on
# \returns 0
#
function manual
{
    MANUAL["$1"]=""
    return 0
}

#
# configureReplace: replace the AC_INIT field in configure.ac
# \param $1 filename to do replacements on
# \returns 0 on success
#

function configureReplace
{
    replace "$1" "[thrift], [${OLDVERSION}]" "[thrift], [${NEWVERSION}]"
}

#
# jsonReplace: replace a specific version field in a JSON file
#   must be a top level "version" field in the json structure
# \param $1 filename to do replacements on
# \returns 0 on success
#

function jsonReplace
{
    local result
    local output
    if [ ! -z "$DRYRUN" ]; then
        output=$(jq -e ".version" "$1")
    else
        output=$(jq -e ".version = \"${NEWVERSION}\"" "$1" > tmp.$$.json && mv tmp.$$.json "$1")
    fi
    result=$?
    if [ $? -ne 0 ]; then
        printf "%-60s | %5d | ${red}ERROR${normal}: version tag not found" "$1" "$count"
        echo
        return 1
    elif [ ! -z "$DRYRUN" ]; then
        output=${output%\"}
        output=${output#\"}
        printf "%-60s | %5d | MATCHES:   version: \"${reverse}${green}${output}${normal}\"" "$1" 1
        echo
        return 0
    fi
    printf "%-60s | %5d | ${green}OK${normal}" "$1" 1
    echo
    return 0
}

#
# pubspecReplace: replace a specific version field in a YAML file
#   must be a top level "version" field in the yaml structure
#   did not find a package that preserves comments so this is
#   somewhat brain-dead, but it gets the job done
# \param $1 filename to do replacements on
# \returns 0 on success
#

function pubspecReplace
{
    replace "$1" "version: ${OLDVERSION}" "version: ${NEWVERSION}"
}

#
# gemlockReplace: replace the version of the thrift gem in a Gemfile.lock
#   and of no other gem; the rest are locked at versions of their own,
#   and one of those can equal ours
# \param $1 filename to do replacements on
# \returns 0 on success
#

function gemlockReplace
{
    replace "$1" "^    thrift (${OLDVERSION})" "    thrift (${NEWVERSION})"
}

#
# npmlockReplace: replace the version of the root package in an npm
#   package-lock.json, which is the top level "version" field and its copy
#   in packages[""], and of no dependency; those are locked at versions of
#   their own, and one of those can equal ours
# \param $1 filename to do replacements on
# \returns 0 on success
#

function npmlockReplace
{
    local current
    if ! current=$(jq -r '[.version, .packages[""].version] | map(. // "") | join(" ")' "$1") \
       || [ "${current}" != "${OLDVERSION} ${OLDVERSION}" ]; then
        printf "%-60s | %5d | ${red}NOT FOUND${normal}: ${OLDVERSION} in version and packages[\"\"].version" "$1" 0
        echo
        return 1
    elif [ ! -z "$DRYRUN" ]; then
        printf "%-60s | %5d | MATCHES:   version, packages[\"\"].version: \"${reverse}${green}${OLDVERSION}${normal}\"" "$1" 2
        echo
        return 0
    fi
    if ! { jq --arg v "${NEWVERSION}" '.version = $v | .packages[""].version = $v' "$1" > tmp.$$.json \
           && mv tmp.$$.json "$1"; }; then
        rm -f tmp.$$.json
        printf "%-60s | %5d | ${red}ERROR${normal}: jq could not rewrite the file" "$1" 2
        echo
        return 1
    fi
    printf "%-60s | %5d | ${green}OK${normal}" "$1" 2
    echo
    return 0
}

#
# pomReplace: replace a specific version field in a maven pom file
#   must be a top level "version" field in the xml structure
# \param $1 filename to do replacements on
# \returns 0 on success
#

function pomReplace
{
    replace "$1" "^  <version>${OLDVERSION}<\/version>" "  <version>${NEWVERSION}<\/version>"
}

#
# replace: replace occurrences of one string with another
#     the file specified must contain the old string at least once
#     in order to be successful.
# \param $1 filename to do replacements on
# \param $2 the "old" string to be replaced
# \param $3 the "new" striing to replace it with
# \returns 0 on success
#
function replace
{
    local result
    local output
    local oldString="$2"
    local newString="$3"
    local oldRegex=$(escapeVersion "${oldString}")
    local count=$(grep -Ec "${oldRegex}" "$1")
    local verbose
    if [ $count -eq 0 ]; then
        printf "%-60s | %5d | ${red}NOT FOUND${normal}: ${oldString}" "$1" 0
        echo
        return 1
    elif [ ! -z "$DRYRUN" ]; then
        printf "%-60s | %5d | MATCHES:" "$1" "$count"
        echo
        while read -r line; do
            echo " > $(echo "$line" | sed "s/${oldRegex}/${reverse}${green}${oldString}${normal}/g")"
        done < <(grep -E "${oldRegex}" "$1")
        return 0
    fi
    output=$(sed -i "s/${oldRegex}/${newString}/g" "$1")
    result=$?
    if [ $result -ne 0 ]; then
        printf "%-60s | %5d | ${red}ERROR${normal}: %s" "$1" "$count" "$output"
        echo
        return 1
    fi
    printf "%-60s | %5d | ${green}OK${normal}" "$1" "$count"
    echo
    return 0
}

#
# simpleReplace: replace occurrences of ${OLDVERSION} with ${NEWVERSION}
#     the file specified must contain OLDVERSION at least once
#     in order to be successful.
# \param $1 filename to do replacements on
# \param $2 the "old" string to be replaced
# \param $3 the "new" striing to replace it with
# \returns 0 on success
#
function simpleReplace
{
    replace "$1" "${OLDVERSION}" "${NEWVERSION}"
}

echo ""
echo "Apache Thrift Version Alignment Tool"
echo "------------------------------------"
echo ""
echo "Previous Version: ${OLDVERSION}"
echo "     New Version: ${NEWVERSION}"
echo ""
echo "-------------------------------------------------------------+-------+----------------------"
echo "Filename                                                     | Count | Status               "
echo "-------------------------------------------------------------+-------+----------------------"

for file in $(echo "${!FILES[@]}" | sort); do
    ${FILES[$file]} $file || exit $?
done

echo
echo "Files that must be modified manually:"
echo
for manu in $(echo "${!MANUAL[@]}" | sort); do
    echo " > ${yellow}${manu}${normal}"
done

exit 0
