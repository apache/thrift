const thrift = require("../../lib/thrift");
const { createRoundtripFuzzer } = require("./fuzz_common");

module.exports.fuzz = createRoundtripFuzzer((transport) => {
  return new thrift.TJSONProtocol(transport);
}, true);
