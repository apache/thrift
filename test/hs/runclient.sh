#!/bin/sh

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

# Check some basic 
if [ -z $BASE ]; then
    BASE=../..
fi

if [ -z $OUTDIR ]; then
    OUTDIR=client-bindings
fi

if [ -z $THRIFT_BIN ]; then
    THRIFT_BIN=$(which thrift)
fi

if [ ! -x "$THRIFT_BIN" ]; then
    printf "Could not find thrift binary; pass it as environment variable THRIFT_BIN\n"
    exit 1
fi

# Figure out what file to generate bindings from
if [ -z $THRIFT_FILE ]; then
    THRIFT_FILE=$BASE/test/$1.thrift
fi

if [ ! -e $THRIFT_FILE ]; then
    printf "Missing thrift file $THRIFT_FILE \n"
    exit 2
fi

# Figure out what file to run has a client
if [ -z $CLIENT_FILE ]; then
    CLIENT_FILE=$BASE/test/hs/$1_TestClient.hs
fi

if [ ! -e $CLIENT_FILE ]; then
    printf "Missing client code file $CLIENT_FILE \n"
    exit 3
fi

# Actually run the client bits
printf "Creating directory $OUTDIR to hold generated bindings... \n"
[ -d $OUTDIR ] || mkdir $OUTDIR

printf "Generating bindings... \n"
$THRIFT_BIN -o $OUTDIR --gen hs $THRIFT_FILE

printf "Starting client... \n"
runhaskell -Wall -Werror -i$BASE/lib/hs/src -i$OUTDIR/gen-hs $CLIENT_FILE
