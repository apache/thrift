#!/bin/sh

set -e

curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp \
     --eval "(ignore-errors (quicklisp-quickstart:install :path \"quicklisp/\"))" \
     --eval "(load \"quicklisp/setup.lisp\")" \
     --eval "(quicklisp:bundle-systems '(#:usocket #:closer-mop #:trivial-utf-8 #:ieee-floats #:trivial-gray-streams #:alexandria #:bordeaux-threads #:cl-ppcre #:fiasco #:net.didierverna.clon) :to \"externals/\")" \
     --eval "(quit)" \
     --no-userinit
