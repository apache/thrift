/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * 'License'); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * 'AS IS' BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

 // This is the Node.js test driver for the standard Apache Thrift 
 // test service. The driver invokes every function defined in the 
 // Thrift Test service with a representative range of parameters.
 //
 // The ThriftTestDriver function requires a client object
 // connected to a server hosting the Thrift Test service and
 // supports an optional callback function which is called with 
 // a status message when the test is complete.

var assert = require('assert');
var ttypes = require('./gen-nodejs/ThriftTest_types');
var Int64 = require('node-int64');

var ThriftTestDriver = exports.ThriftTestDriver = function(client, callback) {
	
function checkRecursively(map1, map2) {
  if (typeof map1 !== 'function' && typeof map2 !== 'function') {
    if (!map1 || typeof map1 !== 'object') {
      //Handle int64 types (which use node-int64 in Node.js JavaScript)
      if ((typeof map1 === "number") && (typeof map2 === "object") && 
          (map2.buffer) && (map2.buffer instanceof Buffer) && (map2.buffer.length === 8)) {
        var n = new Int64(map2.buffer);
        assert.equal(map1, n.toNumber());          
      } else {
        assert.equal(map1, map2);
      }
    } else {
      for (var key in map1) {
        checkRecursively(map1[key], map2[key]);
      }
    }
  }
}

client.testVoid(function(err, response) {
  assert( ! err);
  assert.equal(undefined, response); //void
});

client.testString("Test", function(err, response) {
  assert( ! err);
  assert.equal("Test", response);
});

client.testString("", function(err, response) {
  assert( ! err);
  assert.equal("", response);
});

//all Languages in UTF-8
/*jshint -W100 */
var stringTest = "Afrikaans, Alemannisch, Aragonés, العربية, مصرى, " +
    "Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, " +
    "Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, " +
    "বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, " +
    "Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, " +
    "Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, " +
    "Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, " +
    "Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, " +
    "Avañe'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, " +
    "Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, " +
    "Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, " +
    "ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, " +
    "Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, " +
    "Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa " +
    "Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, مازِرونی, Bahasa " +
    "Melayu, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪" +
    "Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, " +
    "Occitan, Иронау, Papiamentu, Deitsch, Polski, پنجابی, پښتو, " +
    "Norfuk / Pitkern, Português, Runa Simi, Rumantsch, Romani, Română, " +
    "Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple " +
    "English, Slovenčina, Slovenščina, Српски / Srpski, Seeltersk, " +
    "Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, " +
    "Türkçe, Татарча/Tatarça, Українська, اردو, Tiếng Việt, Volapük, " +
    "Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, " +
    "Bân-lâm-gú, 粵語";
/*jshint +W100 */

client.testString(stringTest, function(err, response) {
  assert( ! err);
  assert.equal(stringTest, response);
});

var specialCharacters = 'quote: \" backslash:' +
    ' forwardslash-escaped: \/ ' +
    ' backspace: \b formfeed: \f newline: \n return: \r tab: ' +
    ' now-all-of-them-together: "\\\/\b\n\r\t' +
    ' now-a-bunch-of-junk: !@#$%&()(&%$#{}{}<><><' +
    ' char-to-test-json-parsing: ]] \"]] \\" }}}{ [[[ ';
client.testString(specialCharacters, function(err, response) {
  assert( ! err);
  assert.equal(specialCharacters, response);
});

client.testByte(1, function(err, response) {
  assert( ! err);
  assert.equal(1, response);
});

client.testByte(0, function(err, response) {
  assert( ! err);
  assert.equal(0, response);
});

client.testByte(-1, function(err, response) {
  assert( ! err);
  assert.equal(-1, response);
});

client.testByte(-127, function(err, response) {
  assert( ! err);
  assert.equal(-127, response);
});

client.testI32(-1, function(err, response) {
  assert( ! err);
  assert.equal(-1, response);
});

client.testI64(5, function(err, response) {
  assert( ! err);
  assert.equal(5, response);
});

client.testI64(-5, function(err, response) {
  assert( ! err);
  assert.equal(-5, response);
});

client.testI64(734359738368, function(err, response) {
  assert( ! err);
  assert.equal(734359738368, response);
});

client.testI64(-34359738368, function(err, response) {
  assert( ! err);
  assert.equal(-34359738368, response);
});

client.testI64(-734359738368, function(err, response) {
  assert( ! err);
  assert.equal(-734359738368, response);
});

client.testDouble(-5.2098523, function(err, response) {
  assert( ! err);
  assert.equal(-5.2098523, response);
});

client.testDouble(7.012052175215044, function(err, response) {
  assert( ! err);
  assert.equal(7.012052175215044, response);
});

var out = new ttypes.Xtruct({
  string_thing: 'Zero',
  byte_thing: 1,
  i32_thing: -3,
  i64_thing: 1000000
});
client.testStruct(out, function(err, response) {
  assert( ! err);
  checkRecursively(out, response);
});

var out2 = new ttypes.Xtruct2();
out2.byte_thing = 1;
out2.struct_thing = out;
out2.i32_thing = 5;
client.testNest(out2, function(err, response) {
  assert( ! err);
  checkRecursively(out2, response);
});

var mapout = {};
for (var i = 0; i < 5; ++i) {
  mapout[i] = i-10;
}
client.testMap(mapout, function(err, response) {
  assert( ! err);
  assert.deepEqual(mapout, response);
});

var mapTestInput = {
  "a":"123", "a b":"with spaces ", "same":"same", "0":"numeric key",
  "longValue":stringTest, "Afrikaans, Alemannisch, Aragonés, العربية, مصرى, ":"long key"
};
client.testStringMap(mapTestInput, function(err, response) {
  assert( ! err);
  assert.deepEqual(mapTestInput, response);
});

var setTestSetInput = [1,2,3];
client.testSet(setTestSetInput, function(err, response) {
  assert( ! err);
  assert.deepEqual(setTestSetInput, response);
});
var setTestListInput = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20];
client.testList(setTestListInput, function(err, response) {
  assert( ! err);
  assert.deepEqual(setTestListInput, response);
});

client.testEnum(ttypes.Numberz.ONE, function(err, response) {
  assert( ! err);
  assert.equal(ttypes.Numberz.ONE, response);
});

client.testTypedef(69, function(err, response) {
  assert( ! err);
  assert.equal(69, response);
});

var mapMapTest = {
  "4": {"1":1, "2":2, "3":3, "4":4},
  "-4": {"-4":-4, "-3":-3, "-2":-2, "-1":-1}
};
client.testMapMap(1, function(err, response) {
  assert( ! err);
  assert.deepEqual(mapMapTest, response);
});

var crazy = new ttypes.Insanity({
  "userMap":{ "5":5, "8":8 },
  "xtructs":[new ttypes.Xtruct({
      "string_thing":"Goodbye4",
      "byte_thing":4,
      "i32_thing":4,
      "i64_thing":4
    }), new ttypes.Xtruct({
      "string_thing":"Hello2",
      "byte_thing":2,
      "i32_thing":2,
      "i64_thing":2
    })]
});
var insanity = {
  "1":{ "2": crazy, "3": crazy },
  "2":{ "6":{ "userMap":{}, "xtructs":[] } }
};
client.testInsanity(crazy, function(err, response) {
  assert( ! err);
  checkRecursively(insanity, response);
});

client.testException('TException', function(err, response) {
  assert( ! response);
});

client.testException('Xception', function(err, response) {
  assert( ! response);
  assert.equal(err.errorCode, 1001);
  assert.equal('Xception', err.message);
});

client.testException('no Exception', function(err, response) {
  assert( ! err);
  assert.equal(undefined, response); //void
});

client.testOneway(0, function(err, response) {
  assert(false); //should not answer
});

(function() {
  var test_complete = false;
  var retrys = 0;
  var retry_limit = 30;
  var retry_interval = 100;
  /**
   * redo a simple test after the oneway to make sure we aren't "off by one" --
   * if the server treated oneway void like normal void, this next test will
   * fail since it will get the void confirmation rather than the correct
   * result. In this circumstance, the client will throw the exception:
   *
   * Because this is the last test against the server, when it completes
   * the entire suite is complete by definition (the tests run serially).
   */
  client.testI32(-1, function(err, response) {
    assert( ! err);
    assert.equal(-1, response);
    test_complete = true;
  });

  //We wait up to retry_limit * retry_interval for the test suite to complete
  function TestForCompletion() {
    if(test_complete) {
      if (callback) {
        callback("Server successfully tested!");
      }
    } else {
      if (++retrys < retry_limit) {
        setTimeout(TestForCompletion, retry_interval);
      } else {
        if (callback) {
          callback("Server test failed to complete after " +
                   (retry_limit*retry_interval/1000) + " seconds");
        }
      }
    }
  }

  setTimeout(TestForCompletion, retry_interval);
})();
};
