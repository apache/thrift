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

 // This is the Node test driver for the standard Apache Thrift
 // test service. The driver invokes every function defined in the
 // Thrift Test service with a representative range of parameters.
 //
 // The ThriftTestDriver function requires a client object
 // connected to a server hosting the Thrift Test service and
 // supports an optional callback function which is called with
 // a status message when the test is complete.

var assert = require('assert');
var ttypes = require('./gen-nodejs/ThriftTest_types');

var ThriftTestDriver = exports.ThriftTestDriver = function(client, callback) {

// deepEqual doesn't work with fields using node-int64
function checkRecursively(map1, map2) {
  if (typeof map1 !== 'function' && typeof map2 !== 'function') {
    if (!map1 || typeof map1 !== 'object') {
        assert.equal(map1, map2);
    } else {
      for (var key in map1) {
        checkRecursively(map1[key], map2[key]);
      }
    }
  }
}

client.testVoid()
  .then(function(response) {
    assert.equal(undefined, response); //void
  })
  .fail(function() {
    assert(false);
  });


client.testString("Test")
  .then(function(response) {
    assert.equal("Test", response);
  })
  .fail(function() {
    assert(false);
  });

client.testString("")
  .then(function(response) {
    assert.equal("", response);
  })
  .fail(function() {
    assert(false);
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

client.testString(stringTest)
  .then(function(response) {
    assert.equal(stringTest, response);
  })
  .fail(function() {
    assert(false);
  });


var specialCharacters = 'quote: \" backslash:' +
    ' forwardslash-escaped: \/ ' +
    ' backspace: \b formfeed: \f newline: \n return: \r tab: ' +
    ' now-all-of-them-together: "\\\/\b\n\r\t' +
    ' now-a-bunch-of-junk: !@#$%&()(&%$#{}{}<><><' +
    ' char-to-test-json-parsing: ]] \"]] \\" }}}{ [[[ ';
client.testString(specialCharacters)
  .then(function(response) {
    assert.equal(specialCharacters, response);
  })
  .fail(function() {
    assert(false);
  });


client.testByte(1)
  .then(function(response) {
    assert.equal(1, response);
  })
  .fail(function() {
    assert(false);
  });


client.testByte(0)
  .then(function(response) {
    assert.equal(0, response);
  })
  .fail(function() {
    assert(false);
  });

client.testByte(-1)
  .then(function(response) {
    assert.equal(-1, response);
  })
  .fail(function() {
    assert(false);
  });

client.testByte(-127)
  .then(function(response) {
    assert.equal(-127, response);
  })
  .fail(function() {
    assert(false);
  });

client.testI32(-1)
  .then(function(response) {
    assert.equal(-1, response);
  })
  .fail(function() {
    assert(false);
  });

client.testI64(5)
  .then(function(response) {
    assert.equal(5, response);
  })
  .fail(function() {
    assert(false);
  });

client.testI64(-5)
  .then(function(response) {
    assert.equal(-5, response);
  })
  .fail(function() {
    assert(false);
  });

client.testI64(-34359738368)
  .then(function(response) {
    assert.equal(-34359738368, response);
  })
  .fail(function() {
    assert(false);
  });

client.testDouble(-5.2098523)
  .then(function(response) {
    assert.equal(-5.2098523, response);
  })
  .fail(function() {
    assert(false);
  });

client.testDouble(7.012052175215044)
  .then(function(response) {
    assert.equal(7.012052175215044, response);
  })
  .fail(function() {
    assert(false);
  });

var out = new ttypes.Xtruct({
  string_thing: 'Zero',
  byte_thing: 1,
  i32_thing: -3,
  i64_thing: 1000000
});
client.testStruct(out)
  .then(function(response) {
    checkRecursively(out, response);
  })
  .fail(function() {
    assert(false);
  });

var out2 = new ttypes.Xtruct2();
out2.byte_thing = 1;
out2.struct_thing = out;
out2.i32_thing = 5;
client.testNest(out2)
  .then(function(response) {
    checkRecursively(out2, response);
  })
  .fail(function() {
    assert(false);
  });

var mapout = {};
for (var i = 0; i < 5; ++i) {
  mapout[i] = i-10;
}
client.testMap(mapout)
  .then(function(response) {
    assert.deepEqual(mapout, response);
  })
  .fail(function() {
    assert(false);
  });

var mapTestInput = {
  "a":"123", "a b":"with spaces ", "same":"same", "0":"numeric key",
  "longValue":stringTest, stringTest:"long key"
};
client.testStringMap(mapTestInput)
  .then(function(response) {
    assert.deepEqual(mapTestInput, response);
  })
  .fail(function() {
    assert(false);
  });

var setTestInput = [1,2,3];
client.testSet(setTestInput)
  .then(function(response) {
    assert.deepEqual(setTestInput, response);
  })
  .fail(function() {
    assert(false);
  });
client.testList(setTestInput)
  .then(function(response) {
    assert.deepEqual(setTestInput, response);
  })
  .fail(function() {
    assert(false);
  });

client.testEnum(ttypes.Numberz.ONE)
  .then(function(response) {
    assert.equal(ttypes.Numberz.ONE, response);
  })
  .fail(function() {
    assert(false);
  });

client.testTypedef(69)
  .then(function(response) {
    assert.equal(69, response);
  })
  .fail(function() {
    assert(false);
  });

var mapMapTest = {
  "4": {"1":1, "2":2, "3":3, "4":4},
  "-4": {"-4":-4, "-3":-3, "-2":-2, "-1":-1}
};
client.testMapMap(1)
  .then(function(response) {
    assert.deepEqual(mapMapTest, response);
  })
  .fail(function() {
    assert(false);
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
  "2":{ "6":{ "userMap":null, "xtructs":null } }
};
client.testInsanity(crazy)
  .then(function(response) {
    checkRecursively(insanity, response);
  })
  .fail(function() {
    assert(false);
  });

client.testException('TException')
  .then(function() {
    assert(false);
  });

client.testException('Xception')
  .then(function(response) {
    assert.equal(err.errorCode, 1001);
    assert.equal('Xception', err.message);
  })
  .fail(function() {
    assert(false);
  });

client.testException('no Exception')
  .then(function(response) {
    assert.equal(undefined, response); //void
  })
  .fail(function() {
    assert(false);
  });

client.testOneway(0, function(error, response) {
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
  client.testI32(-1)
    .then(function(response) {
        assert.equal(-1, response);
        test_complete = true;
    })
    .fail(function() {
      assert(false);
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
