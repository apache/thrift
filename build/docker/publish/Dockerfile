FROM gcc:8 as build

RUN apt update && apt upgrade -y && apt install flex bison cmake -y

WORKDIR /usr/src/thrift

COPY . /usr/src/thrift

RUN mkdir cmake-build && cd cmake-build && cmake -DWITH_SHARED_LIB=off .. && make

FROM ubuntu 
COPY --from=build /usr/src/thrift/cmake-build/bin/thrift /bin/thrift
WORKDIR /thrift
ENTRYPOINT ["thrift"]
