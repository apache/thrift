/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
 
 
/*
 * JavaScript test suite
 */

var transport = new Thrift.Transport("/service");
var protocol  = new Thrift.Protocol(transport);
var client    = new ThriftTest.ThriftTestClient(protocol);

// all Languages in UTF-8
var stringTest = "Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk / Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски / Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語";
  
function checkRecursively(map1, map2) {
  if (typeof map1 !== 'function' && typeof map2 !== 'function') {
    if (!map1 || typeof map1 !== 'object') {
        equal(map1, map2);
    } else {
      for (var key in map1) {
        checkRecursively(map1[key], map2[key]);
      }
    }
  }
}

module("Base Types");

  test("Void", function() {
    equal(client.testVoid(), undefined);
  });
  test("String", function() {
    equal(client.testString(''), '');
    equal(client.testString(stringTest), stringTest);

    var specialCharacters = 'quote: \" backslash:' +
          ' forwardslash-escaped: \/ ' +
          ' backspace: \b formfeed: \f newline: \n return: \r tab: ' +
          ' now-all-of-them-together: "\\\/\b\n\r\t' +
          ' now-a-bunch-of-junk: !@#$%&()(&%$#{}{}<><><';
    equal(client.testString(specialCharacters),specialCharacters);
  });
  test("Double", function() {
    equal(client.testDouble(0), 0);
    equal(client.testDouble(-1), -1);
    equal(client.testDouble(3.14), 3.14);
    equal(client.testDouble(Math.pow(2,60)), Math.pow(2,60));
  });
  test("Byte", function() {
    equal(client.testByte(0), 0);
    equal(client.testByte(0x01), 0x01);
  });
  test("I32", function() {
    equal(client.testI32(0), 0);
    equal(client.testI32(Math.pow(2,30)), Math.pow(2,30));
    equal(client.testI32(-Math.pow(2,30)), -Math.pow(2,30));
  });
  test("I64", function() {
    equal(client.testI64(0), 0);
    equal(client.testI64(Math.pow(2,60)), Math.pow(2,60));
    equal(client.testI64(-Math.pow(2,60)), -Math.pow(2,60));
  });


module("Structured Types");

  test("Struct", function() {
    var structTestInput = new ThriftTest.Xtruct();
    structTestInput.string_thing = 'worked';
    structTestInput.byte_thing = 0x01;
    structTestInput.i32_thing = Math.pow(2,30);
    structTestInput.i64_thing = Math.pow(2,60);

    var structTestOutput = client.testStruct(structTestInput);

    equal(structTestOutput.string_thing, structTestInput.string_thing);
    equal(structTestOutput.byte_thing, structTestInput.byte_thing);
    equal(structTestOutput.i32_thing, structTestInput.i32_thing);
    equal(structTestOutput.i64_thing, structTestInput.i64_thing);
    
    equal(JSON.stringify(structTestOutput), JSON.stringify(structTestInput));
  });

  test("Nest", function() {
    var xtrTestInput = new ThriftTest.Xtruct();
    xtrTestInput.string_thing = 'worked';
    xtrTestInput.byte_thing = 0x01;
    xtrTestInput.i32_thing = Math.pow(2,30);
    xtrTestInput.i64_thing = Math.pow(2,60);
    
    var nestTestInput = new ThriftTest.Xtruct2();
    nestTestInput.byte_thing = 0x02;
    nestTestInput.struct_thing = xtrTestInput;
    nestTestInput.i32_thing = Math.pow(2,15);
    
    var nestTestOutput = client.testNest(nestTestInput);
    
    equal(nestTestOutput.byte_thing, nestTestInput.byte_thing);
    equal(nestTestOutput.struct_thing.string_thing, nestTestInput.struct_thing.string_thing);
    equal(nestTestOutput.struct_thing.byte_thing, nestTestInput.struct_thing.byte_thing);
    equal(nestTestOutput.struct_thing.i32_thing, nestTestInput.struct_thing.i32_thing);
    equal(nestTestOutput.struct_thing.i64_thing, nestTestInput.struct_thing.i64_thing);
    equal(nestTestOutput.i32_thing, nestTestInput.i32_thing);
    
    equal(JSON.stringify(nestTestOutput), JSON.stringify(nestTestInput));
  });

  test("Map", function() {
    var mapTestInput = {7:77, 8:88, 9:99};

    var mapTestOutput = client.testMap(mapTestInput);

    for (var key in mapTestOutput) {
      equal(mapTestOutput[key], mapTestInput[key]);
    }
  });

  test("StringMap", function() {
    var mapTestInput = {
      "a":"123", "a b":"with spaces ", "same":"same", "0":"numeric key",
      "longValue":stringTest, stringTest:"long key"
    };

    var mapTestOutput = client.testStringMap(mapTestInput);

    for (var key in mapTestOutput) {
      equal(mapTestOutput[key], mapTestInput[key]);
    }
  });

  test("Set", function() {
    var setTestInput = [1,2,3];
    ok(client.testSet(setTestInput), setTestInput);
  });

  test("List", function() {
    var listTestInput = [1,2,3];
    ok(client.testList(listTestInput), listTestInput);
  });

  test("Enum", function() {
    equal(client.testEnum(ThriftTest.Numberz.ONE), ThriftTest.Numberz.ONE);
  });

  test("TypeDef", function() {
    equal(client.testTypedef(69), 69);
  });


module("deeper!");

  test("MapMap", function() {
    var mapMapTestExpectedResult = {
      "4":{"1":1,"2":2,"3":3,"4":4},
      "-4":{"-4":-4, "-3":-3, "-2":-2, "-1":-1}
    };

    var mapMapTestOutput = client.testMapMap(1);


    for (var key in mapMapTestOutput) {
      for (var key2 in mapMapTestOutput[key]) {
        equal(mapMapTestOutput[key][key2], mapMapTestExpectedResult[key][key2]);
      }
    }
    
    checkRecursively(mapMapTestOutput, mapMapTestExpectedResult);
  });


module("Exception");

  test("Xception", function() {
    expect(2);
    try{
      client.testException("Xception");
    }catch(e){
      equal(e.errorCode, 1001);
      equal(e.message, "Xception");
    }
  });

  test("no Exception", 0, function() {
    try{
      client.testException("no Exception");
    }catch(e){
      ok(false);
    }
  });

  test("TException", function() {
    expect(1);
    try{
      client.testException("TException");
    } catch(e) {
      ok(true);
    }
  });


module("Insanity");

  test("testInsanity", function() {
    var insanity = {
      "1":{
        "2":{
          "userMap":{ "5":5, "8":8 },
          "xtructs":[{
              "string_thing":"Goodbye4",
              "byte_thing":4,
              "i32_thing":4,
              "i64_thing":4
            },
            {
              "string_thing":"Hello2",
              "byte_thing":2,
              "i32_thing":2,
              "i64_thing":2
            }
          ]
        },
        "3":{
          "userMap":{ "5":5, "8":8 },
          "xtructs":[{
              "string_thing":"Goodbye4",
              "byte_thing":4,
              "i32_thing":4,
              "i64_thing":4
            },
            {
              "string_thing":"Hello2",
              "byte_thing":2,
              "i32_thing":2,
              "i64_thing":2
            }
          ]
        }
      },
      "2":{ "6":{ "userMap":null, "xtructs":null } }
    };
    var res = client.testInsanity(new ThriftTest.Insanity());
    ok(res, JSON.stringify(res));
    ok(insanity, JSON.stringify(insanity));

    checkRecursively(res, insanity);
  });


//////////////////////////////////
//Run same tests asynchronously
jQuery.ajaxSetup({ timeout: 0 });
$(document).ajaxError( function() { QUnit.start(); } );

module("Async Manual");

  test("testI32", function() {
    expect( 2 );
    QUnit.stop();

    var transport = new Thrift.Transport();
    var protocol  = new Thrift.Protocol(transport);
    var client    = new ThriftTest.ThriftTestClient(protocol);

    var jqxhr = jQuery.ajax({
      url: "/service",
      data: client.send_testI32(Math.pow(-2,31)),
      type: "POST",
      cache: false,
      dataType: "text",
      success: function(res){
        transport.setRecvBuffer( res );
        equal(client.recv_testI32(), Math.pow(-2,31));
      },
      error: function() { ok(false); },
      complete: function() {
        ok(true);
        QUnit.start();
      }
    });
  });


  test("testI64", function() {
    expect( 2 );
    QUnit.stop();

    var transport = new Thrift.Transport();
    var protocol  = new Thrift.Protocol(transport);
    var client    = new ThriftTest.ThriftTestClient(protocol);

    jQuery.ajax({
      url: "/service",
      data: client.send_testI64(Math.pow(-2,61)),
      type: "POST",
      cache: false,
      dataType: "text",
      success: function(res){
        transport.setRecvBuffer( res );
        equal(client.recv_testI64(), Math.pow(-2,61));
      },
      error: function() { ok(false); },
      complete: function() {
        ok(true);
        QUnit.start();
      }
    });
  });


module("Async");

  test("Double", function() {
    expect( 1 );

    QUnit.stop();
    client.testDouble(3.14159265, function(result) {
      equal(result, 3.14159265);
      QUnit.start();
    });
  });

  test("Byte", function() {
    expect( 1 );

    QUnit.stop();
    client.testByte(0x01, function(result) {
      equal(result, 0x01);
      QUnit.start();
    });
  });

  test("I32", function() {
    expect( 3 );

    QUnit.stop();
    client.testI32(Math.pow(2,30), function(result) {
      equal(result, Math.pow(2,30));
      QUnit.start();
    });

    QUnit.stop();
    var jqxhr = client.testI32(Math.pow(-2,31), function(result) {
      equal(result, Math.pow(-2,31));
    });

    jqxhr.success(function(result) {
      equal(result, Math.pow(-2,31));
      QUnit.start();
    });
  });

  test("I64", function() {
    expect( 4 );

    QUnit.stop();
    client.testI64(Math.pow(2,60), function(result) {
      equal(result, Math.pow(2,60));
      QUnit.start();
    });

    QUnit.stop();
    client.testI64(Math.pow(-2,61), function(result) {
      equal(result, Math.pow(-2,61));
    })
    .error( function(xhr, status, e) {  ok(false, e.message); } )
    .success(function(result) {
      equal(result, Math.pow(-2,61));
    })
    .complete(function() {
      ok(true);
      QUnit.start();
    });
  });

  test("Xception", function() {
    expect( 2 );

    QUnit.stop();

    var dfd = client.testException("Xception", function(result) {
      ok(false);
      QUnit.start();
    })
    .error(function(xhr, status, e){
      equal(e.errorCode, 1001);
      equal(e.message, "Xception");
      QUnit.start();
    });
  });
