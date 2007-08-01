#!/bin/sh
if ! [ -d tutorial/gen-erl ]; then
    echo generating gen-erl
    cd tutorial
    thrift -erl -rb -r tutorial.thrift
    cd ..
fi
echo "Compiling user/ and tutorial/gen-erl/..."
mkdir ebin-user
erlc -I include -I tutorial/gen-erl -o ebin-user user/*.erl tutorial/gen-erl/*.erl &&
erl +K true -pa ebin -pa ebin-user
