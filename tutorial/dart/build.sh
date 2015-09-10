rm -r gen-dart && \
thrift --gen "dart:gen_server" "../shared.thrift" && \
	cd gen-dart/shared && pub get && cd ../.. && \
thrift --gen "dart:gen_server" "../tutorial.thrift";
	cd gen-dart/tutorial && pub get && cd ../.. && \
cd client && pub get && cd .. && \
echo "Enjoy the Dart tutorial!"
