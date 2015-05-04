var ttypes = require('./gen-nodejs/JsDeepConstructorTest_types');
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


function createThriftObj() {

  return new ttypes.Complex({

    struct_field: new ttypes.Simple({value: 'a'}),

    struct_list_field: [
      new ttypes.Simple({value: 'b'}),
      new ttypes.Simple({value: 'c'}),
    ],

    struct_set_field: [
      new ttypes.Simple({value: 'd'}),
      new ttypes.Simple({value: 'e'}),
    ],

    struct_map_field: {
      A: new ttypes.Simple({value: 'f'}),
      B: new ttypes.Simple({value: 'g'})
    },

    struct_nested_containers_field: [
      [
        {
          C: [
            new ttypes.Simple({value: 'h'}),
            new ttypes.Simple({value: 'i'})
          ]
        }
      ]
    ],

    struct_nested_containers_field2: {
      D: [
        {
          DA: new ttypes.Simple({value: 'j'})
        },
        {
          DB: new ttypes.Simple({value: 'k'})
        }
      ]
    }
  }
  );
}


function createJsObj() {

  return {

    struct_field: {value: 'a'},

    struct_list_field: [
      {value: 'b'},
      {value: 'c'},
    ],

    struct_set_field: [
      {value: 'd'},
      {value: 'e'},
    ],

    struct_map_field: {
      A: {value: 'f'},
      B: {value: 'g'}
    },

    struct_nested_containers_field: [
      [
        {
          C: [
            {value: 'h'},
            {value: 'i'}
          ]
        }
      ]
    ],

    struct_nested_containers_field2: {
      D: [
        {
          DA: {value: 'j'}
        },
        {
          DB: {value: 'k'}
        }
      ]
    }
  };
}


function assertValues(obj, assert) {
    assert.equals(obj.struct_field.value, 'a');
    assert.equals(obj.struct_list_field[0].value, 'b');
    assert.equals(obj.struct_list_field[1].value, 'c');
    assert.equals(obj.struct_set_field[0].value, 'd');
    assert.equals(obj.struct_set_field[1].value, 'e');
    assert.equals(obj.struct_map_field.A.value, 'f');
    assert.equals(obj.struct_map_field.B.value, 'g');
    assert.equals(obj.struct_nested_containers_field[0][0].C[0].value, 'h');
    assert.equals(obj.struct_nested_containers_field[0][0].C[1].value, 'i');
    assert.equals(obj.struct_nested_containers_field2.D[0].DA.value, 'j');
    assert.equals(obj.struct_nested_containers_field2.D[1].DB.value, 'k');
}

var cases = {

  "Serialize/deserialize should return equal object": function(assert){
    var tObj = createThriftObj();
    var received = deserialize(serialize(tObj), ttypes.Complex);
    assert.ok(tObj !== received);
    assert.deepEqual(tObj, received);
    assert.end();
  },

  "Nested structs and containers initialized from plain js objects should serialize same as if initialized from thrift objects": function(assert) {
    var tObj1 = createThriftObj();
    var tObj2 = new ttypes.Complex(createJsObj());
    assertValues(tObj2, assert);
    assert.ok(serialize(tObj2).equals(serialize(tObj1)));
    assert.end();
  },

  "Modifications to args object should not affect constructed Thrift object": function (assert) {

    var args = createJsObj();
    assertValues(args, assert);

    var tObj = new ttypes.Complex(args);
    assertValues(tObj, assert);

    args.struct_field.value = 'ZZZ';
    args.struct_list_field[0].value = 'ZZZ';
    args.struct_list_field[1].value = 'ZZZ';
    args.struct_set_field[0].value = 'ZZZ';
    args.struct_set_field[1].value = 'ZZZ';
    args.struct_map_field.A.value = 'ZZZ';
    args.struct_map_field.B.value = 'ZZZ';
    args.struct_nested_containers_field[0][0].C[0] = 'ZZZ';
    args.struct_nested_containers_field[0][0].C[1] = 'ZZZ';
    args.struct_nested_containers_field2.D[0].DA = 'ZZZ';
    args.struct_nested_containers_field2.D[0].DB = 'ZZZ';

    assertValues(tObj, assert);
    assert.end();
  }

};

Object.keys(cases).forEach(function(caseName) {
  test(caseName, cases[caseName]);
});
