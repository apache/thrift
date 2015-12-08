Why Debian? Why Ubuntu or fbthrift?

* thrift/compiler
```
docker build -t thrift/ubuntu build/docker/ubuntu
docker run -it --rm -v $(pwd):/thrift thrift/ubuntu /bin/bash -c "cd /thrift && ./bootstrap.sh && ./configure && make check"
```

* thrift/debian-base:jessie
  base image with all dependencies

* thrift/debian-compiler:`git rev-parse --short HEAD`
  just the compiler with entrypoint

releases
* thrift/compiler:`git rev-parse --short HEAD` or tag


git rev-parse --short HEAD

docker build --no-cache -t thrift/debian:base-`git rev-parse --short HEAD`  build/docker/debian/base/

sudo docker build --no-cache -t thrift/compiler-`git rev-parse --short HEAD`  build/docker/debian/compiler/
