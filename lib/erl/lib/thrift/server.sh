#!/bin/sh
echo "Compiling user/ and tutorial/gen-erl/..."
mkdir ebin-user
erlc -I include -I tutorial/gen-erl -o ebin-user user/*.erl tutorial/gen-erl/*.erl &&
erl -pa ebin -pa ebin-user -s application start thrift # -s nh start
