#!/bin/sh

ERL_THRIFT=../../lib/erl

if ! [ -d ${ERL_THRIFT}/ebin ]; then
    echo "Please build the Thrift library by running \`make' in ${ERL_THRIFT}"
    exit 1
fi

if ! [ -d ../gen-erl ]; then
    echo "Please run thrift first to generate ../gen-erl/"
    exit 1
fi


erlc -I ${ERL_THRIFT}/include -I ../gen-erl -o ../gen-erl ../gen-erl/*.erl  &&
  erlc -I ${ERL_THRIFT}/include -I ../gen-erl *.erl &&
  erl +K true -pa ${ERL_THRIFT}/ebin -pa ../gen-erl 
