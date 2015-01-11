<?php

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
 *
 * @package thrift.test
 */

namespace Test\Thrift;

use ThriftTest\Xtruct;
use ThriftTest\Xtruct2;
use ThriftTest\Numberz;
use ThriftTest\Insanity;

class Fixtures
{
  public static $testArgs = array();

  public static function populateTestArgs()
  {
    self::$testArgs['testString1'] = "Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk / Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски / Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語";

    self::$testArgs['testString2'] =
      "quote: \\\" backslash:" .
      " forwardslash-escaped: \\/ " .
      " backspace: \b formfeed: \f newline: \n return: \r tab: " .
      " now-all-of-them-together: \"\\\/\b\n\r\t" .
      " now-a-bunch-of-junk: !@#\$%&()(&%$#{}{}<><><";

    self::$testArgs['testString3'] =
      "string that ends in double-backslash \\\\";

    self::$testArgs['testDouble'] = 3.1415926535898;

	// TODO: add testBinary() call
	
    self::$testArgs['testByte'] = 0x01;

    self::$testArgs['testI32'] = pow( 2, 30 );

    if (PHP_INT_SIZE == 8) {
      self::$testArgs['testI64'] = pow( 2, 60 );
    } else {
      self::$testArgs['testI64'] = "1152921504606847000";
    }

    self::$testArgs['testStruct'] =
      new Xtruct(
            array(
                    'string_thing' => 'worked',
                    'byte_thing' => 0x01,
                    'i32_thing' => pow( 2, 30 ),
                    'i64_thing' => self::$testArgs['testI64']
                    )
            );

    self::$testArgs['testNestNested'] =
      new Xtruct(
            array(
                    'string_thing' => 'worked',
                    'byte_thing' => 0x01,
                    'i32_thing' => pow( 2, 30 ),
                    'i64_thing' => self::$testArgs['testI64']
                    )
            );

    self::$testArgs['testNest'] =
      new Xtruct2(
            array(
                'byte_thing' => 0x01,
                'struct_thing' => self::$testArgs['testNestNested'],
                'i32_thing' => pow( 2, 15 )
                )
            );

    self::$testArgs['testMap'] =
      array(
            7 => 77,
            8 => 88,
            9 => 99
            );

    self::$testArgs['testStringMap'] =
      array(
            "a" => "123",
            "a b" => "with spaces ",
            "same" => "same",
            "0" => "numeric key",
            "longValue" => self::$testArgs['testString1'],
            self::$testArgs['testString1'] => "long key"
            );

    self::$testArgs['testSet'] = array( 1 => true, 5 => true, 6 => true );

    self::$testArgs['testList'] = array( 1, 2, 3 );

    self::$testArgs['testEnum'] = Numberz::ONE;

    self::$testArgs['testTypedef'] = 69;

    self::$testArgs['testMapMapExpectedResult'] =
      array(
            4 => array(
                       1 => 1,
                       2 => 2,
                       3 => 3,
                       4 => 4,
                       ),
            -4 => array(
                        -4 => -4,
                        -3 => -3,
                        -2 => -2,
                        -1 => -1
                        )
            );

    // testInsanity ... takes a few steps to set up!

    $xtruct1 =
      new Xtruct(
            array(
                'string_thing' => 'Goodbye4',
                'byte_thing' => 4,
                'i32_thing' => 4,
                'i64_thing' => 4
                )
            );

    $xtruct2 =
      new Xtruct(
            array(
                'string_thing' => 'Hello2',
                'byte_thing' =>2,
                'i32_thing' => 2,
                'i64_thing' => 2
                )
            );

    $userMap =
      array(
            Numberz::FIVE => 5,
            Numberz::EIGHT => 8
            );

    $insanity2 =
      new Insanity(
            array(
                'userMap' => $userMap,
                'xtructs' => array($xtruct1,$xtruct2)
                )
            );

    $insanity3 = $insanity2;

    $insanity6 =
      new Insanity(
            array(
                'userMap' => null,
                'xtructs' => null
                )
            );

    self::$testArgs['testInsanityExpectedResult'] =
      array(
            "1" => array(
                         Numberz::TWO => $insanity2,
                         Numberz::THREE => $insanity3
                    ),
            "2" => array(
                         Numberz::SIX => $insanity6
                    )
            );

  }
}
