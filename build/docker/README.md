# Docker Integration #

Due to the large number of languages supported by Apache Thrift,
docker containers are used to build and test the project on a
variety of platforms to provide maximum test coverage.

## Appveyor Integration ##

At this time the Appveyor scripts do not use docker containers.
Once Microsoft supports Visual Studio Build Tools running inside
nano containers (instead of Core, which is huge) then we will
consider using containers for the Windows builds as well.

## Travis CI Integration ##

The Travis CI scripts use the following environment variables and
logic to determine their behavior:

### Environment Variables ###

| Variable | Default | Usage |
| -------- | ----- | ------- |
| `DISTRO` | `ubuntu-bionic` | Set by various build jobs in `.travis.yml` to run builds in different containers.  Not intended to be set externally.|
| `DOCKER_REPO` | `thrift/thrift-build` | The name of the Docker Hub repository to obtain and store docker images. |
| `DOCKER_USER` | `<none>` | The Docker Hub account name containing the repository. |
| `DOCKER_PASS` | `<none>` | The Docker Hub account password to use when pushing new tags. |

For example, the default docker image that is used in builds if no overrides are specified would be: `thrift/thrift-build:ubuntu-bionic`

### Forks ###

If you have forked the Apache Thrift repository and you would like
to use your own Docker Hub account to store thrift build images,
you can use the Travis CI web interface to set the `DOCKER_USER`,
`DOCKER_PASS`, and `DOCKER_REPO` variables in a secure manner.
Your fork builds will then pull, push, and tag the docker images
in your account.

### Logic ###

The Travis CI build runs in two phases - first the docker images are rebuilt
for each of the supported containers if they do not match the Dockerfile that
was used to build the most recent tag.  If a `DOCKER_PASS` environment
variable is specified, the docker stage builds will attempt to log into
Docker Hub and push the resulting tags.

## Supported Containers ##

The Travis CI (continuous integration) builds use the Ubuntu Bionic
(18.04 LTS) and Xenial (16.04 LTS) images to maximize language level
coverage.

### Ubuntu ###

* bionic (stable, current)
* artful (previous stable)
* xenial (legacy)

## Unsupported Containers ##

These containers may be in various states, and may not build everything.
They can be found in the `old/` subdirectory.

### CentOS ###
* 7.3
  * make check in lib/py may hang in test_sslsocket - root cause unknown

### Debian ###

* jessie
* stretch
  * make check in lib/cpp fails due to https://svn.boost.org/trac10/ticket/12507

## Building like Travis CI does, locally ##

We recommend you build locally the same way Travis CI does, so that when you
submit your pull request you will run into fewer surprises.  To make it a
little easier, put the following into your `~/.bash_aliases` file:

    # Kill all running containers.
    alias dockerkillall='docker kill $(docker ps -q)'

    # Delete all stopped containers.
    alias dockercleanc='printf "\n>>> Deleting stopped containers\n\n" && docker rm $(docker ps -a -q)'

    # Delete all untagged images.
    alias dockercleani='printf "\n>>> Deleting untagged images\n\n" && docker rmi $(docker images -q -f dangling=true)'

    # Delete all stopped containers and untagged images.
    alias dockerclean='dockercleanc || true && dockercleani'

    # Build a thrift docker image (run from top level of git repo): argument #1 is image type (ubuntu, centos, etc).
    function dockerbuild
    {
      docker build -t $1 build/docker/$1
    }

    # Run a thrift docker image: argument #1 is image type (ubuntu, centos, etc).
    function dockerrun
    {
      docker run -v $(pwd):/thrift/src -it $1 /bin/bash
    }

Then, to pull down the current image being used to build (the same way
Travis CI does it) - if it is out of date in any way it will build a
new one for you:

    thrift$ DOCKER_REPO=thrift/thrift-build DISTRO=ubuntu-bionic build/docker/refresh.sh

To run all unit tests (just like Travis CI does):

    thrift$ dockerrun thrift/thrift-build:ubuntu-bionic
    root@8caf56b0ce7b:/thrift/src# build/docker/scripts/autotools.sh

To run the cross tests (just like Travis CI does):

    thrift$ dockerrun thrift/thrift-build:ubuntu-bionic
    root@8caf56b0ce7b:/thrift/src# build/docker/scripts/cross-test.sh

When you are done, you want to clean up occasionally so that docker isn't using lots of extra disk space:

    thrift$ dockerclean

You need to run the docker commands from the root of the local clone of the
thrift git repository for them to work.

When you are done in the root docker shell you can `exit` to go back to
your user host shell.  Once the unit tests and cross test passes locally,
submit the changes, and if desired squash the pull request to one commit
to make it easier to merge (the committers can squash at commit time now
that GitHub is the master repository).  Now you are building like Travis CI does!

## Raw Commands for Building with Docker ##

If you do not want to use the same scripts Travis CI does, you can do it manually:

Build the image:

    thrift$ docker build -t thrift build/docker/ubuntu-bionic

Open a command prompt in the image:

    thrift$ docker run -v $(pwd):/thrift/src -it thrift /bin/bash

## Core Tool Versions per Dockerfile ##

Last updated: October 1, 2017

| Tool      | ubuntu-xenial | ubuntu-bionic | Notes |
| :-------- | :------------ | :------------ | :---- |
| ant       | 1.9.6         | 1.10.3        |       |
| autoconf  | 2.69          | 2.69          |       |
| automake  | 1.15          | 1.15.1        |       |
| bison     | 3.0.4         | 3.0.4         |       |
| boost     | 1.58.0        | 1.65.1        |       |
| cmake     | 3.5.1         | 3.10.2        |       |
| cppcheck  | 1.72          | 1.82          |       |
| flex      | 2.6.0         | 2.6.4         |       |
| libc6     | 2.23          | 2.27          | glibc |
| libevent  | 2.0.21        | 2.1.8         |       |
| libstdc++ | 5.4.0         | 7.3.0         |       |
| make      | 4.1           | 4.1           |       |
| openssl   | 1.0.2g        | 1.1.0g        |       |
| qt5       | 5.5.1         | 5.9.5         |       |

## Compiler/Language Versions per Dockerfile ##

| Language  | ubuntu-xenial | ubuntu-bionic | Notes |
| :-------- | :------------ | :------------ | :---- |
| as of     | Mar 06, 2018  | Jul 1, 2019   |       |
| as3       |               | 4.6.0         |       |
| C++ gcc   | 5.4.0         | 7.4.0         |       |
| C++ clang | 3.8           | 6.0           |       |
| C# (mono) | 4.2.1.0       | 4.6.2.7       |       |
| c_glib    | 2.48.2        | 2.56.4        |       |
| cl (sbcl) |               | 1.5.3         |       |
| d         | 2.075.1       | 2.087.0       |       |
| dart      | 2.0.0         | 2.4.0         |       |
| delphi    |               |               | Not in CI |
| dotnet    | 2.1.503       | 2.2.300       |       |
| erlang    | 18.3          | 22.0          |       |
| go        | 1.10.8        | 1.12.6        |       |
| haskell   | 7.10.3        | 8.0.2         |       |
| haxe      | 3.2.1         | 3.4.4         | THRIFT-4352: avoid 3.4.2 |
| java      | 1.8.0_191     | 11.0.3        |       |
| js        |               |               | Unsure how to look for version info? |
| lua       |               | 5.2.4         | Lua 5.3: see THRIFT-4386 |
| nodejs    | 6.16.0        | 10.16.0       |       |
| ocaml     |               | 4.05.0        | THRIFT-4517: ocaml 4.02.3 on xenial appears broken |
| perl      | 5.22.1        | 5.26.1        |       |
| php       | 7.0.32        | 7.2.19        |       |
| python    | 2.7.12        | 2.7.15        |       |
| python3   | 3.5.2         | 3.6.8         |       |
| ruby      | 2.3.1p112     | 2.5.1p57      |       |
| rust      | 1.34.0        | 1.35.0        |       |
| smalltalk |               |               | Not in CI |
| swift     |               | 4.2.1         |       |
