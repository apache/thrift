import thrift = require("thrift");
var program = require('commander');
import ThriftTest = require('./gen-nodets/ThriftTest');
import test_handler = require('./test_handler');


program
  .option('--port <port>', 'Set thrift server port', 9090)
  .option('--promise', 'test with promise style functions')
  .parse(process.argv);

var port: number = program.port;
var promise: boolean = program.promise;

var options: thrift.ServerOptions = {
  transport: thrift.TBufferedTransport,
  protocol: thrift.TBinaryProtocol
};

var server: thrift.Server;
if (program.promise) {
  server = thrift.createServer(ThriftTest.ThriftTest, new test_handler.AsyncThriftTestHandler(), options);
} else {
  server = thrift.createServer(ThriftTest.ThriftTest, new test_handler.SyncThriftTestHandler(), options);
}
server.listen(port);
