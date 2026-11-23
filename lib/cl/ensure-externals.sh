#!/bin/bash

#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

set -e

if [[ ! -e quicklisp.lisp ]]; then curl -O https://beta.quicklisp.org/quicklisp.lisp; fi
sbcl --load quicklisp.lisp \
     --eval "(ignore-errors (quicklisp-quickstart:install :path \"quicklisp/\"))" \
     --eval "(load \"quicklisp/setup.lisp\")" \
     --eval "(quicklisp:bundle-systems '(#:puri #:usocket #:closer-mop #:trivial-utf-8 #:ieee-floats #:trivial-gray-streams #:alexandria #:bordeaux-threads #:cl-ppcre #:fiasco #:net.didierverna.clon) :to \"externals/\")" \
     --eval "(quit)" \
     --no-userinit
if [[ ! -e backport-update.zip ]]; then
    curl -O -L https://github.com/TurtleWarePL/de.setf.thrift/archive/backport-update.zip;
fi
mkdir -p lib
unzip -u backport-update.zip -d lib
