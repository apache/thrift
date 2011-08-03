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


module("Base Types");

  test("Void", function() {
    equals(client.testVoid(), undefined);
  });
  test("String", function() {
    equals(client.testString(stringTest), stringTest);

    var specialCharacters = 'quote: \" backslash:' +
          ' forwardslash-escaped: \/ ' +
          ' backspace: \b formfeed: \f newline: \n return: \r tab: ' +
          ' now-all-of-them-together: "\\\/\b\n\r\t' +
          ' now-a-bunch-of-junk: !@#$%&()(&%$#{}{}<><><';
    equals(client.testString(specialCharacters),specialCharacters);
  });
  test("Double", function() {
    equals(client.testDouble(3.14), 3.14);
  });
  test("Byte", function() {
    equals(client.testByte(0x01), 0x01);
  });
  test("I32", function() {
    equals(client.testI32(Math.pow(2,30)), Math.pow(2,30));
  });
  test("I64", function() {
    equals(client.testI64(Math.pow(2,60)), Math.pow(2,60));
  });


module("Structured Types");

  test("Struct", function() {
    var structTestInput = new ThriftTest.Xtruct();
    structTestInput.string_thing = 'worked';
    structTestInput.byte_thing = 0x01;
    structTestInput.i32_thing = Math.pow(2,30);
    structTestInput.i64_thing = Math.pow(2,60);

    var structTestOutput = client.testStruct(structTestInput);

    equals(structTestOutput.string_thing, structTestInput.string_thing);
    equals(structTestOutput.byte_thing, structTestInput.byte_thing);
    equals(structTestOutput.i32_thing, structTestInput.i32_thing);
    equals(structTestOutput.i64_thing, structTestInput.i64_thing);
    
    equals(JSON.stringify(structTestOutput), JSON.stringify(structTestInput));
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
    
    equals(nestTestOutput.byte_thing, nestTestInput.byte_thing);
    equals(nestTestOutput.struct_thing.string_thing, nestTestInput.struct_thing.string_thing);
    equals(nestTestOutput.struct_thing.byte_thing, nestTestInput.struct_thing.byte_thing);
    equals(nestTestOutput.struct_thing.i32_thing, nestTestInput.struct_thing.i32_thing);
    equals(nestTestOutput.struct_thing.i64_thing, nestTestInput.struct_thing.i64_thing);
    equals(nestTestOutput.i32_thing, nestTestInput.i32_thing);
    
    equals(JSON.stringify(nestTestOutput), JSON.stringify(nestTestInput));
  });

  test("Map", function() {
    var mapTestInput = {7:77, 8:88, 9:99};

    var mapTestOutput = client.testMap(mapTestInput);

    for (var key in mapTestOutput) {
      equals(mapTestOutput[key], mapTestInput[key]);
    }
  });

  test("StringMap", function() {
    var mapTestInput = {
      "a":"123", "a b":"with spaces ", "same":"same", "0":"numeric key",
      "longValue":stringTest, stringTest:"long key"
    };

    var mapTestOutput = client.testStringMap(mapTestInput);

    for (var key in mapTestOutput) {
      equals(mapTestOutput[key], mapTestInput[key]);
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
    equals(client.testEnum(ThriftTest.Numberz.ONE), ThriftTest.Numberz.ONE);
  });

  test("TypeDef", function() {
    equals(client.testTypedef(69), 69);
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
        equals(mapMapTestOutput[key][key2], mapMapTestExpectedResult[key][key2]);
      }
    }
    
    equals(JSON.stringify(mapMapTestOutput), JSON.stringify(mapMapTestExpectedResult));
  });


module("Exception");

  test("Xception", function() {
    expect(2);
    try{
      client.testException("Xception");
    }catch(e){
      equals(e.errorCode, 1001);
      equals(e.message, "Xception");
    }
  });

  test("no Exception", function() {
    try{
      client.testException("no Exception");
    }catch(e){
      ok(false);
    }
  });

  test("unexpected Application Exception", function() {
    expect(1);
    try{
      client.testException("ApplicationException");
    } catch(e) {
      ok(true); //@HACK: ignore faulty java server response for exceptions
      //equals(e.message, "ApplicationException");
    }
  });


module("Insanity");

  test("testInsanity", function() {
    var insanity = {
      "1":{
        "3":{
          "userMap":{ "8":8, "5":5 },
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
        "2":{
          "userMap":{ "8":8, "5":5 },
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
    var res = client.testInsanity("");
    ok(res, JSON.stringify(res));
    ok(insanity, JSON.stringify(insanity));
    equals(JSON.stringify(res), JSON.stringify(insanity)); //TODO: read and compare maps recursively
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
        equals(client.recv_testI32(), Math.pow(-2,31));
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
        equals(client.recv_testI64(), Math.pow(-2,61));
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
      equals(result, 3.14159265);
      QUnit.start();
    });
  });

  test("Byte", function() {
    expect( 1 );

    QUnit.stop();
    client.testByte(0x01, function(result) {
      equals(result, 0x01);
      QUnit.start();
    });
  });

  test("I32", function() {
    expect( 3 );

    QUnit.stop();
    client.testI32(Math.pow(2,30), function(result) {
      equals(result, Math.pow(2,30));
      QUnit.start();
    });

    QUnit.stop();
    var jqxhr = client.testI32(Math.pow(-2,31), function(result) {
      equals(result, Math.pow(-2,31));
    });

    jqxhr.success(function(result) {
      equals(result, Math.pow(-2,31));
      QUnit.start();
    });
  });

  test("I64", function() {
    expect( 4 );

    QUnit.stop();
    client.testI64(Math.pow(2,60), function(result) {
      equals(result, Math.pow(2,60));
      QUnit.start();
    });

    QUnit.stop();
    client.testI64(Math.pow(-2,61), function(result) {
      equals(result, Math.pow(-2,61));
    })
    .error( function(e) {  ok(false); } )
    .success(function(result) {
      equals(result, Math.pow(-2,61));
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
    .error(function(e){
      equals(e.errorCode, 1001);
      equals(e.message, "Xception");
      QUnit.start();
    });
  });
