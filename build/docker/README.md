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
| `DISTRO` | `ubuntu-focal` | Set by various build jobs in `.travis.yml` to run builds in different containers.  Not intended to be set externally.|
| `DOCKER_REPO` | `thrift/thrift-build` | The name of the Docker Hub repository to obtain and store docker images. |
| `DOCKER_USER` | `<none>` | The Docker Hub account name containing the repository. |
| `DOCKER_PASS` | `<none>` | The Docker Hub account password to use when pushing new tags. |

For example, the default docker image that is used in builds if no overrides are specified would be: `thrift/thrift-build:ubuntu-focal`

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

The Travis CI (continuous integration) builds use the Ubuntu Jammy
(22.04 LTS) and Focal (20.04 LTS) images to maximize language level
coverage.

### Ubuntu ###

* focal (stable, current)
* jammy (next stable, WIP)

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

    thrift$ DOCKER_REPO=thrift/thrift-build DISTRO=ubuntu-focal build/docker/refresh.sh

To run all unit tests (just like Travis CI does):

    thrift$ dockerrun thrift/thrift-build:ubuntu-focal
    root@8caf56b0ce7b:/thrift/src# build/docker/scripts/autotools.sh

To run the cross tests (just like Travis CI does):

    thrift$ dockerrun thrift/thrift-build:ubuntu-focal
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

Linux/Mac:

    thrift$ docker build --build-arg uid=$(id -u) --build-arg gid=$(id -g) -t thrift build/docker/ubuntu-jammy

Windows:

    thrift$ docker build -t thrift build/docker/ubuntu-jammy


Open a command prompt in the image:

    thrift$ docker run -v $(pwd):/thrift/src -it thrift /bin/bash

## Core Tool Versions per Dockerfile ##

Last updated: March 5, 2024

| Tool      | ubuntu-focal  | ubuntu-jammy  | Notes |
| :-------- | :------------ | :------------ | :---- |
| ant       | 1.10.7        | 1.10.12       |       |
| autoconf  | 2.69          | 2.71          |       |
| automake  | 1.16.1        | 1.16.5        |       |
| bison     | 3.5.1         | 3.8.2         |       |
| boost     | 1.71.0        | 1.74.0        |       |
| cmake     | 3.16.3        | 3.22.1        |       |
| cppcheck  | 1.90          | 2.7           |       |
| flex      | 2.6.4         | 2.6.4         |       |
| libc6     | 2.31          | 2.35          | glibc |
| libevent  | 2.0.16        | 2.0.16        |       |
| libstdc++ | 10.5.0        | 10.5.0        |       |
| make      | 4.2.1         | 4.3           |       |
| openssl   | 1.1.1f        | 3.0.2         |       |
| qt5       | 5.12.8        | 5.15.3        |       |

## Compiler/Language Versions per Dockerfile ##

| Language  | ubuntu-focal  | ubuntu-jammy  | Notes |
| :-------- | :------------ | :------------ | :---- |
| as of     | Mar 06, 2018  | Jul 1, 2019   |       |
| as3       | 4.6.0         | 4.6.0         |       |
| C++ gcc   | 9.4.0         | 11.4.0        |       |
| C++ clang | 13.0.0        | 13.0.0        |       |
| c\_glib   | 3.2.12        | 3.2.12        |       |
| cl (sbcl) |               | 1.5.3         |       |
| d         | 2.087.0       | 2.087.0       |       |
| dart      | 2.7.2-1       | 2.7.2-1       |       |
| delphi    |               |               | Not in CI |
| erlang    | OTP-25.3.2.9  | OTP-25.3.2.9  |       |
| go        | 1.21.7        | 1.21.7        |       |
| haxe      | 4.2.1         | 4.2.1         |       |
| java      | 17            | 17            |       |
| js        | Node.js 16.20.2, npm 8.19.4 | Node.js 16.20.2, npm 8.19.4 |     |
| lua       | 5.2.4         | 5.2.4         | Lua 5.3: see THRIFT-4386 |
| netstd    | 8.0.200       | 8.0.200       |       |
| nodejs    | 16.20.2       | 16.20.2       |       |
| ocaml     | 4.08.1        | 4.13.1        |       |
| perl      | 5.30.0        | 5.34.0        |       |
| php       | 7.4.3         | 8.1.2         |       |
| python2   | 2.7.18        |               |       |
| python3   | 3.8.10        | 3.10.12       |       |
| ruby      | 2.7.0p0       | 3.0.2p107     |       |
| rust      | 1.65.0        | 1.65.0        |       |
| smalltalk |               |               | Not in CI |
| swift     | 5.7           | 5.7           |       |
