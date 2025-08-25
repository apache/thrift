const thrift = require("../../lib/thrift");
const { createParserFuzzer } = require("./fuzz_common");

module.exports.fuzz = createParserFuzzer((transport) => {
  return new thrift.TJSONProtocol(transport);
}, true);
