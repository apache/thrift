# Apache Thrift ActionScript Library

## Building

We use gradle and gradlefx to build the as3 library.  Unfortunately gradlefx requires
an older version of gradle (2.5) but it still works - for now.  If you use the docker
container to do the build, the Adobe Flex SDK 4.6 is installed and the FLEX_HOME
environment variable is configured:

    dev@ubuntu:~/thrift$ docker run -v $(pwd):/thrift/src:rw -it thrift/thrift-build:ubuntu-bionic /bin/bash
    root@7624b61bbf84:/thrift/src# cd lib/as3
    root@7624b61bbf84:/thrift/src/lib/as3# ./gradlew -Prelease=true compileFlex

    ...

    :compileFlex UP-TO-DATE

    BUILD SUCCESSFUL

    Total time: 10.784 secs

    root@7624b61bbf84:/thrift/src/lib/as3# ls -ls build/
    total 4
    4 -rw-r--r-- 1 root root 1379 Jan 22 19:23 libthrift-as3.swc

## Publishing

We use a similar gradle-based signing and publishing mechanism as in the java
library.  See the java library [README.md](../java/README.md) for more details.

To publish into a local .m2 repository you can mount a directory into the docker container,
for example:

    dev@ubuntu:~/thrift$ docker run -v~/.m2:/root/.m2 -v $(pwd):/thrift/src:rw -it thrift/thrift-build:ubuntu-bionic /bin/bash
    root@7624b61bbf84:/thrift/src/lib/as3# ./gradlew -Prelease=true publishToMavenLocal

You will find your `~/.m2` directory is now populated with a release build `swc`.
