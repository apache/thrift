BOOST_DIR = /usr/local/boost/include/boost-1_33_1/
THRIFT_DIR = /usr/local/include/thrift
LIB_DIR = /usr/local/lib

GEN_SRC = ../gen-cpp/SharedService.cpp ../gen-cpp/shared_types.cpp ../gen-cpp/tutorial_types.cpp ../gen-cpp/Calculator.cpp

default: server client

server: CppServer.cpp
	g++ -o CppServer -I${THRIFT_DIR} -I${BOOST_DIR}  -I../gen-cpp -L${LIB_DIR} -lthrift CppServer.cpp ${GEN_SRC}

client: CppClient.cpp
	g++ -o CppClient -I${THRIFT_DIR} -I${BOOST_DIR}  -I../gen-cpp -L${LIB_DIR} -lthrift CppClient.cpp ${GEN_SRC}

clean:
	$(RM) -r CppClient CppServer
