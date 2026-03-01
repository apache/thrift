import thrift = require("thrift");
import { program } from 'commander';
import ThriftTest = require('./gen-nodejs/ThriftTest');
import test_handler = require('./test_handler');


program
  .option('--port <port>', 'Set thrift server port', Number.parseInt, 9090)
  .option('--promise', 'test with promise style functions')
  .option('--protocol', '"Set thrift protocol (binary) [protocol]"')
  .parse(process.argv);

var opts = program.opts();
var port: number = opts.port;

var options: thrift.ServerOptions = {
  transport: thrift.TBufferedTransport,
  protocol: thrift.TBinaryProtocol,
};

var server: thrift.Server;
if (opts.promise) {
  server = thrift.createServer(ThriftTest.Processor, new test_handler.AsyncThriftTestHandler(), options);
} else {
  server = thrift.createServer(
    ThriftTest.Processor,
    new test_handler.SyncThriftTestHandler(),
    options,
  );
}
server.listen(port);
