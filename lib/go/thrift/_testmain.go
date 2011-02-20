package main

import "./_xtest_"
import "testing"
import __regexp__ "regexp"

var tests = []testing.InternalTest{
	{"thrift_test.TestTApplicationException", thrift_test.TestTApplicationException},
	{"thrift_test.TestReadWriteBinaryProtocol", thrift_test.TestReadWriteBinaryProtocol},
	{"thrift_test.TestReadWriteCompactProtocol", thrift_test.TestReadWriteCompactProtocol},
	{"thrift_test.TestTException", thrift_test.TestTException},
	{"thrift_test.TestFramedTransport", thrift_test.TestFramedTransport},
	{"thrift_test.TestHttpClient", thrift_test.TestHttpClient},
	{"thrift_test.TestIOStreamTransport", thrift_test.TestIOStreamTransport},
	{"thrift_test.TestWriteJSONProtocolBool", thrift_test.TestWriteJSONProtocolBool},
	{"thrift_test.TestReadJSONProtocolBool", thrift_test.TestReadJSONProtocolBool},
	{"thrift_test.TestWriteJSONProtocolByte", thrift_test.TestWriteJSONProtocolByte},
	{"thrift_test.TestReadJSONProtocolByte", thrift_test.TestReadJSONProtocolByte},
	{"thrift_test.TestWriteJSONProtocolI16", thrift_test.TestWriteJSONProtocolI16},
	{"thrift_test.TestReadJSONProtocolI16", thrift_test.TestReadJSONProtocolI16},
	{"thrift_test.TestWriteJSONProtocolI32", thrift_test.TestWriteJSONProtocolI32},
	{"thrift_test.TestReadJSONProtocolI32", thrift_test.TestReadJSONProtocolI32},
	{"thrift_test.TestWriteJSONProtocolI64", thrift_test.TestWriteJSONProtocolI64},
	{"thrift_test.TestReadJSONProtocolI64", thrift_test.TestReadJSONProtocolI64},
	{"thrift_test.TestWriteJSONProtocolDouble", thrift_test.TestWriteJSONProtocolDouble},
	{"thrift_test.TestReadJSONProtocolDouble", thrift_test.TestReadJSONProtocolDouble},
	{"thrift_test.TestWriteJSONProtocolString", thrift_test.TestWriteJSONProtocolString},
	{"thrift_test.TestReadJSONProtocolString", thrift_test.TestReadJSONProtocolString},
	{"thrift_test.TestWriteJSONProtocolBinary", thrift_test.TestWriteJSONProtocolBinary},
	{"thrift_test.TestReadJSONProtocolBinary", thrift_test.TestReadJSONProtocolBinary},
	{"thrift_test.TestWriteJSONProtocolList", thrift_test.TestWriteJSONProtocolList},
	{"thrift_test.TestWriteJSONProtocolSet", thrift_test.TestWriteJSONProtocolSet},
	{"thrift_test.TestWriteJSONProtocolMap", thrift_test.TestWriteJSONProtocolMap},
	{"thrift_test.TestReadWriteJSONStruct", thrift_test.TestReadWriteJSONStruct},
	{"thrift_test.TestReadWriteJSONProtocol", thrift_test.TestReadWriteJSONProtocol},
	{"thrift_test.TestMemoryBuffer", thrift_test.TestMemoryBuffer},
	{"thrift_test.TestNonblockingTransportServerToClient", thrift_test.TestNonblockingTransportServerToClient},
	{"thrift_test.TestNonblockingTransportClientToServer", thrift_test.TestNonblockingTransportClientToServer},
	{"thrift_test.TestNothing", thrift_test.TestNothing},
	{"thrift_test.TestWriteSimpleJSONProtocolBool", thrift_test.TestWriteSimpleJSONProtocolBool},
	{"thrift_test.TestReadSimpleJSONProtocolBool", thrift_test.TestReadSimpleJSONProtocolBool},
	{"thrift_test.TestWriteSimpleJSONProtocolByte", thrift_test.TestWriteSimpleJSONProtocolByte},
	{"thrift_test.TestReadSimpleJSONProtocolByte", thrift_test.TestReadSimpleJSONProtocolByte},
	{"thrift_test.TestWriteSimpleJSONProtocolI16", thrift_test.TestWriteSimpleJSONProtocolI16},
	{"thrift_test.TestReadSimpleJSONProtocolI16", thrift_test.TestReadSimpleJSONProtocolI16},
	{"thrift_test.TestWriteSimpleJSONProtocolI32", thrift_test.TestWriteSimpleJSONProtocolI32},
	{"thrift_test.TestReadSimpleJSONProtocolI32", thrift_test.TestReadSimpleJSONProtocolI32},
	{"thrift_test.TestWriteSimpleJSONProtocolI64", thrift_test.TestWriteSimpleJSONProtocolI64},
	{"thrift_test.TestReadSimpleJSONProtocolI64", thrift_test.TestReadSimpleJSONProtocolI64},
	{"thrift_test.TestWriteSimpleJSONProtocolDouble", thrift_test.TestWriteSimpleJSONProtocolDouble},
	{"thrift_test.TestReadSimpleJSONProtocolDouble", thrift_test.TestReadSimpleJSONProtocolDouble},
	{"thrift_test.TestWriteSimpleJSONProtocolString", thrift_test.TestWriteSimpleJSONProtocolString},
	{"thrift_test.TestReadSimpleJSONProtocolString", thrift_test.TestReadSimpleJSONProtocolString},
	{"thrift_test.TestWriteSimpleJSONProtocolBinary", thrift_test.TestWriteSimpleJSONProtocolBinary},
	{"thrift_test.TestReadSimpleJSONProtocolBinary", thrift_test.TestReadSimpleJSONProtocolBinary},
	{"thrift_test.TestWriteSimpleJSONProtocolList", thrift_test.TestWriteSimpleJSONProtocolList},
	{"thrift_test.TestWriteSimpleJSONProtocolSet", thrift_test.TestWriteSimpleJSONProtocolSet},
	{"thrift_test.TestWriteSimpleJSONProtocolMap", thrift_test.TestWriteSimpleJSONProtocolMap},
	{"thrift_test.TestReadWriteSimpleJSONStruct", thrift_test.TestReadWriteSimpleJSONStruct},
	{"thrift_test.TestReadWriteSimpleJSONProtocol", thrift_test.TestReadWriteSimpleJSONProtocol},
}
var benchmarks = []testing.InternalBenchmark{ //
}

func main() {
	testing.Main(__regexp__.MatchString, tests)
	testing.RunBenchmarks(__regexp__.MatchString, benchmarks)
}
