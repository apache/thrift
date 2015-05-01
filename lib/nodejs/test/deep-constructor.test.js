var ttypes = require('./gen-nodejs/ThriftTest_types');
var thrift = require('thrift');
var test = require('tape');

function serialize(data) {
  var buff;
  var transport = new thrift.TBufferedTransport(null, function(msg){
    buff = msg;
  });
  var prot = new thrift.TBinaryProtocol(transport);
  data.write(prot);
  prot.flush();
  return buff;

}

function deserialize(serialized, type) {
  var t = new thrift.TFramedTransport(serialized);
  var p = new thrift.TBinaryProtocol(t);
  var data = new type();
  data.read(p);
  return data;
}

var bonkJS = {message: 'oink', type: 17};
var bonk = new ttypes.Bonk(bonkJS);

var cases = {

  "Serialize/deserialize should return equal object": function(assert){
    var received = deserialize(serialize(bonk), ttypes.Bonk);
    assert.deepEqual(bonk, received);
    assert.end();
  },

  "Nested structs initialized from plain js objects should serialize same as if initialized from thrift objects": function(assert) {

    var a = new ttypes.Xtruct2(
      {
        byte_thing: 13,
        i32_thing: 123,
        struct_thing: new ttypes.Xtruct({
          string_thing: 'hello',
          byte_thing: 17,
          i32_thing: 456,
          i64_thing: 789
        })
      }
    );

    var b = new ttypes.Xtruct2(
      {
        byte_thing: 13,
        i32_thing: 123,
        struct_thing: {
          string_thing: 'hello',
          byte_thing: 17,
          i32_thing: 456,
          i64_thing: 789
        }
      }
    );

    assert.ok(serialize(a).equals(serialize(b)));
    assert.end();
  },

  "Struct inside list should be initialized from plain js object inside array": function(assert) {
    var a = new ttypes.ListBonks({bonk: [bonk]});
    var b = new ttypes.ListBonks({bonk: [bonkJS]});
    assert.equals(a.bonk[0].message, bonk.message);
    assert.ok(serialize(a).equals(serialize(b)));
    assert.end();
  },

  "Struct inside set should be initialized from plain js object inside array": function(assert) {
    var a = new ttypes.BonkSet({bonk: [bonk]});
    var b = new ttypes.BonkSet({bonk: [bonkJS]});
    assert.equals(a.bonk[0].message, bonk.message);
    assert.ok(serialize(a).equals(serialize(b)));
    assert.end();
  },

  "Struct inside map should be initialized from plain js object inside array": function(assert) {
    var a = new ttypes.BonkMap({bonk: {abc: bonk}});
    var b = new ttypes.BonkMap({bonk: {abc: bonkJS}});
    assert.equals(a.bonk.abc.message, bonk.message);
    assert.ok(serialize(a).equals(serialize(b)));
    assert.end();
  },

  "Struct inside deeply nested containers should be initialized from plain js object inside array": function(assert) {
    var a = new ttypes.Bonkers({bonk: [[{abc: [bonk]}]]});
    var b = new ttypes.Bonkers({bonk: [[{abc: [bonkJS]}]]});
    assert.equals(a.bonk[0][0].abc[0].message, bonk.message);
    assert.ok(serialize(a).equals(serialize(b)));
    assert.end();
  }
};

Object.keys(cases).forEach(function(caseName) {
  test(caseName, cases[caseName]);
});
