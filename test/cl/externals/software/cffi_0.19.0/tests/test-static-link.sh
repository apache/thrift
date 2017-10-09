#!/bin/sh -eux

for l in sbcl ; do # mkcl ecl clisp sbcl
    EX="$(cl-launch -l $l -sp cffi-toolchain -ip "(output-file :static-program-op :cffi-tests/example)")"
    rm -f $EX ; :
    cl-launch -l $l -sp cffi-toolchain -i "(operate :static-program-op :cffi-tests/example)"
    [ -f $EX ]
    [ "$($EX)" = "hello, world!" ]
done
