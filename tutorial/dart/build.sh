#!/bin/sh

set -e;
rm -r gen-dart || true;

thrift --gen dart ../shared.thrift;
cd gen-dart/shared;
pub get;
cd ../..;

thrift --gen dart ../tutorial.thrift;
cd gen-dart/tutorial;
pub get;
cd ../..;

cd client;
pub get;
cd ..;

cd server;
pub get;
cd ..;

dartfmt -w gen-dart;

echo "\nEnjoy the Dart tutorial!";
echo "\nTo run the server:";
echo "> dart server/bin/main.dart";
echo "\nTo run the client:";
echo "# Serve the app from the client directory and view in a browser";
echo "> cd client;";
echo "> pub serve;";
echo "";
