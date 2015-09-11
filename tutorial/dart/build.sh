#!/bin/sh

rm -r gen-dart || true && \
thrift --gen "dart:gen_server" "../shared.thrift" && \
	cd gen-dart/shared && pub get && cd ../.. && \
thrift --gen "dart:gen_server" "../tutorial.thrift" && \
	cd gen-dart/tutorial && pub get && cd ../.. && \
cd client && pub get && cd .. && \
cd server && pub get && cd .. && \
dartfmt -w gen-dart && \
echo "Enjoy the Dart tutorial!"
