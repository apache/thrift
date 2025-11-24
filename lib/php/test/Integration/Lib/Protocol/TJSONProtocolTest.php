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
 */

namespace Test\Thrift\Integration\Lib\Protocol;

use PHPUnit\Framework\TestCase;
use Thrift\Protocol\TJSONProtocol;
use Thrift\Transport\TMemoryBuffer;
use Basic\ThriftTest\Insanity;
use Basic\ThriftTest\Numberz;
use Basic\ThriftTest\Xtruct;
use Basic\ThriftTest\Xtruct2;

/***
 * This test suite depends on running the compiler against the ./Resources/ThriftTest.thrift file:
 * lib/php/test$ ../../../compiler/cpp/thrift --gen php:nsglobal="Basic" -r  --out ./Resources/packages/php ./Resources/ThriftTest.thrift
 */
class TJSONProtocolTest extends TestCase
{
    private const BUFFER_SIZE = 8192; //big enough to read biggest serialized Fixture arg.

    private $transport;
    private $protocol;

    public static function setUpBeforeClass(): void
    {
        if (!is_dir(__DIR__ . '/../../../Resources/packages/php')) {
            self::fail(
                'Before running Integration test suite, you must run the Thrift compiler against the ThriftTest.thrift file in the ./Resources directory.'
            );
        }
    }

    public function setUp(): void
    {
        $this->transport = new TMemoryBuffer();
        $this->protocol = new TJSONProtocol($this->transport);
        $this->transport->open();
    }

    public function testMessageReadWrite()
    {
        $input = new TJSONProtocol(new TMemoryBuffer('[1,"testString",1,0,{"0":{"str":"successResponse"}}]'));
        $service = new \Basic\ThriftTest\ThriftTestClient($input, $this->protocol);
        $result = $service->testString('test');
        $this->assertSame('successResponse', $result);
    }

    /**
     * @dataProvider writeDataProvider
     */
    public function testWrite(
        $argsClassName,
        $argsValues,
        $expected
    ) {
        $args = new $argsClassName($argsValues);
        $args->write($this->protocol);

        $actual = $this->transport->read(self::BUFFER_SIZE);

        $this->assertEquals($expected, $actual);
    }

    public function writeDataProvider()
    {
        if (!is_dir(__DIR__ . '/../../../Resources/packages/php')) {
            throw new \RuntimeException(
                'Before running Integration test suite, you must run the Thrift compiler against the ThriftTest.thrift file in the ./Resources directory.'
            );
        }

        yield 'void' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testVoid_args::class,
            'argsValues' => [],
            'expected' => '{}',
        ];
        yield 'bool true' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testBool_args::class,
            'argsValues' => [
                'thing' => true,
            ],
            'expected' => '{"1":{"tf":1}}',
        ];
        yield 'bool false' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testBool_args::class,
            'argsValues' => [
                'thing' => false,
            ],
            'expected' => '{"1":{"tf":0}}',
        ];
        yield 'string1' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testString_args::class,
            'argsValues' => [
                'thing' => "Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk / Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски / Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語",
            ],
            'expected' => '{"1":{"str":"Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ \/ ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe\'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk \/ Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски \/ Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча\/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語"}}',
        ];
        yield 'string2' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testString_args::class,
            'argsValues' => [
                'thing' => "quote: \\\" backslash:" .
                    " forwardslash-escaped: \\/ " .
                    " backspace: \b formfeed: \f newline: \n return: \r tab: " .
                    " now-all-of-them-together: \"\\\/\b\n\r\t" .
                    " now-a-bunch-of-junk: !@#\$%&()(&%$#{}{}<><><",
            ],
            'expected' => '{"1":{"str":"quote: \\\\\" backslash: forwardslash-escaped: \\\\\/  backspace: \\\\b formfeed: \f newline: \n return: \r tab:  now-all-of-them-together: \"\\\\\\\\\/\\\\b\n\r\t now-a-bunch-of-junk: !@#$%&()(&%$#{}{}<><><"}}',
        ];
        yield 'string3' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testString_args::class,
            'argsValues' => [
                'thing' => "string that ends in double-backslash \\\\",
            ],
            'expected' => '{"1":{"str":"string that ends in double-backslash \\\\\\\\"}}',
        ];
        yield 'string4 testUnicodeStringWithNonBMP' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testString_args::class,
            'argsValues' => [
                'thing' => "สวัสดี/𝒯",
            ],
            'expected' => '{"1":{"str":"สวัสดี\/𝒯"}}',
        ];
        yield 'double' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testDouble_args::class,
            'argsValues' => [
                'thing' => 3.1415926535898,
            ],
            'expected' => '{"1":{"dbl":3.1415926535898}}',
        ];
        #TODO Should be fixed in future
        yield 'double Nan' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testDouble_args::class,
            'argsValues' => [
                'thing' => NAN,
            ],
            'expected' => '{"1":{"dbl":}}',
        ];
        #TODO Should be fixed in future
        yield 'double Infinity' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testDouble_args::class,
            'argsValues' => [
                'thing' => INF,
            ],
            'expected' => '{"1":{"dbl":}}',
        ];
        yield 'byte' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testByte_args::class,
            'argsValues' => [
                'thing' => 0x01,
            ],
            'expected' => '{"1":{"i8":1}}',
        ];
        yield 'i32' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testI32_args::class,
            'argsValues' => [
                'thing' => pow(2, 30),
            ],
            'expected' => '{"1":{"i32":1073741824}}',
        ];
        if (PHP_INT_SIZE == 8) {
            yield 'i64_64Architecture' => [
                'argsClassName' => \Basic\ThriftTest\ThriftTest_testI64_args::class,
                'argsValues' => [
                    'thing' => pow(2, 60),
                ],
                'expected' => '{"1":{"i64":' . pow(2, 60) . '}}',
            ];
            yield 'struct_64Architecture' => [
                'argsClassName' => \Basic\ThriftTest\ThriftTest_testStruct_args::class,
                'argsValues' => [
                    'thing' => new Xtruct(
                        [
                            'string_thing' => 'worked',
                            'byte_thing' => 0x01,
                            'i32_thing' => pow(2, 30),
                            'i64_thing' => pow(2, 60),
                        ]
                    ),
                ],
                'expected' => '{"1":{"rec":{"1":{"str":"worked"},"4":{"i8":1},"9":{"i32":1073741824},"11":{"i64":' . pow(2, 60) . '}}}}',
            ];
            yield 'nest_64Architecture' => [
                'argsClassName' => \Basic\ThriftTest\ThriftTest_testNest_args::class,
                'argsValues' => [
                    'thing' => new Xtruct2(
                        [
                            'byte_thing' => 0x01,
                            'struct_thing' => new Xtruct(
                                [
                                    'string_thing' => 'worked',
                                    'byte_thing' => 0x01,
                                    'i32_thing' => pow(2, 30),
                                    'i64_thing' => pow(2, 60),
                                ]
                            ),
                            'i32_thing' => pow(2, 15),
                        ]
                    ),
                ],
                'expected' => '{"1":{"rec":{"1":{"i8":1},"2":{"rec":{"1":{"str":"worked"},"4":{"i8":1},"9":{"i32":1073741824},"11":{"i64":' . pow(2, 60) . '}}},"3":{"i32":32768}}}}',
            ];
        } else {
            yield 'i64_32Architecture' => [
                'argsClassName' => \Basic\ThriftTest\ThriftTest_testI64_args::class,
                'argsValues' => [
                    'thing' => "1152921504606847000",
                ],
                'expected' => '{"1":{"i64":1152921504606847000}}',
            ];
            yield 'struct_32Architecture' => [
                'argsClassName' => \Basic\ThriftTest\ThriftTest_testStruct_args::class,
                'argsValues' => [
                    'thing' => new Xtruct(
                        [
                            'string_thing' => 'worked',
                            'byte_thing' => 0x01,
                            'i32_thing' => pow(2, 30),
                            'i64_thing' => pow(2, 60),
                        ]
                    ),
                ],
                'expected' => '{"1":{"rec":{"1":{"str":"worked"},"4":{"i8":1},"9":{"i32":1073741824},"11":{"i64":1152921504606847000}}}}',
            ];
            yield 'nest_32Architecture' => [
                'argsClassName' => \Basic\ThriftTest\ThriftTest_testNest_args::class,
                'argsValues' => [
                    'thing' => new Xtruct2(
                        [
                            'byte_thing' => 0x01,
                            'struct_thing' => new Xtruct(
                                [
                                    'string_thing' => 'worked',
                                    'byte_thing' => 0x01,
                                    'i32_thing' => pow(2, 30),
                                    'i64_thing' => '1152921504606847000',
                                ]
                            ),
                            'i32_thing' => pow(2, 15),
                        ]
                    ),
                ],
                'expected' => '{"1":{"rec":{"1":{"i8":1},"2":{"rec":{"1":{"str":"worked"},"4":{"i8":1},"9":{"i32":1073741824},"11":{"i64":1152921504606847000}}},"3":{"i32":32768}}}}',
            ];
        }
        yield 'map' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testMap_args::class,
            'argsValues' => [
                'thing' => [
                    7 => 77,
                    8 => 88,
                    9 => 99,
                ],
            ],
            'expected' => '{"1":{"map":["i32","i32",3,{"7":77,"8":88,"9":99}]}}',
        ];
        yield 'stringMap' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testStringMap_args::class,
            'argsValues' => [
                'thing' => [
                    "a" => "123",
                    "a b" => "with spaces ",
                    "same" => "same",
                    "0" => "numeric key",
                    "longValue" => "Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk / Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски / Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語",
                    "Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk / Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски / Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語" => "long key",
                ],
            ],
            'expected' => '{"1":{"map":["str","str",6,{"a":"123","a b":"with spaces ","same":"same","0":"numeric key","longValue":"Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ \/ ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe\'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk \/ Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски \/ Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча\/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語","Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ \/ ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe\'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk \/ Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски \/ Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча\/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語":"long key"}]}}',
        ];
        yield 'set' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testSet_args::class,
            'argsValues' => [
                'thing' => [1 => true, 5 => true, 6 => true],
            ],
            'expected' => '{"1":{"set":["i32",3,1,5,6]}}',
        ];
        yield 'list' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testList_args::class,
            'argsValues' => [
                'thing' => [1, 2, 3],
            ],
            'expected' =>  '{"1":{"lst":["i32",3,1,2,3]}}',
        ];
        yield 'enum' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testEnum_args::class,
            'argsValues' => [
                'thing' => \Basic\ThriftTest\Numberz::SIX,
            ],
            'expected' => '{"1":{"i32":6}}',
        ];
        yield 'typedef' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testTypedef_args::class,
            'argsValues' => [
                'thing' => 69,
            ],
            'expected' => '{"1":{"i64":69}}',
        ];
    }

    /**
     * @dataProvider readDataProvider
     */
    public function testRead(
        $buffer,
        $argsClassName,
        $argsPropertyName,
        $expectedValue,
        $expectedResult
    ): void {
        $this->transport->write($buffer);
        $args = new $argsClassName();
        $result = $args->read($this->protocol);

        $this->assertEquals($expectedResult, $result);

        if (is_null($argsPropertyName)) {
            $this->assertNull($argsPropertyName);
        } elseif (is_float($expectedValue) && is_nan($expectedValue)) {
            $this->assertNan($args->{$argsPropertyName});
        } elseif (is_float($expectedValue) && is_infinite($expectedValue)) {
            $this->assertEquals($expectedValue, $args->{$argsPropertyName});
        } else {
            $this->assertEquals($expectedValue, $args->{$argsPropertyName});
        }
    }

    public function readDataProvider()
    {
        yield 'void' => [
            'buffer' => '{}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testVoid_args::class,
            'argsPropertyName' => null,
            'expectedValue' => null,
            'expectedResult' => 0,
        ];
        yield 'bool true' => [
            'buffer' => '{"1":{"tf":1}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testBool_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => true,
            'expectedResult' => 1,
        ];
        yield 'bool false' => [
            'buffer' => '{"1":{"tf":0}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testBool_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => false,
            'expectedResult' => 1,
        ];
        yield 'string1' => [
            'buffer' => '{"1":{"str":"Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ \/ ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe\'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk \/ Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски \/ Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча\/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語"}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testString_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => "Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk / Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски / Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語",
            'expectedResult' => 1,
        ];
        yield 'string2' => [
            'buffer' => '{"1":{"str":"quote: \\\\\" backslash: forwardslash-escaped: \\\\\/  backspace: \\\\b formfeed: \f newline: \n return: \r tab:  now-all-of-them-together: \"\\\\\\\\\/\\\\b\n\r\t now-a-bunch-of-junk: !@#$%&()(&%$#{}{}<><><"}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testString_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => "quote: \\\" backslash:" .
                " forwardslash-escaped: \\/ " .
                " backspace: \b formfeed: \f newline: \n return: \r tab: " .
                " now-all-of-them-together: \"\\\/\b\n\r\t" .
                " now-a-bunch-of-junk: !@#\$%&()(&%$#{}{}<><><",
            'expectedResult' => 1,
        ];
        yield 'string3' => [
            'buffer' => '{"1":{"str":"string that ends in double-backslash \\\\\\\\"}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testString_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => "string that ends in double-backslash \\\\",
            'expectedResult' => 1,
        ];
        yield 'string4' => [
            'buffer' => '{"1":{"str":"สวัสดี\/𝒯"}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testString_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => "สวัสดี/𝒯",
            'expectedResult' => 1,
        ];
        yield 'double' => [
            'buffer' => '{"1":{"dbl":3.1415926535898}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testDouble_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => 3.1415926535898,
            'expectedResult' => 1,
        ];
        yield 'double Nan' => [
            'buffer' => '{"1":{"dbl":"NaN"}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testDouble_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => NAN,
            'expectedResult' => 1,
        ];
        yield 'double Infinity' => [
            'buffer' => '{"1":{"dbl":"Infinity"}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testDouble_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => INF,
            'expectedResult' => 1,
        ];
        yield 'byte' => [
            'buffer' => '{"1":{"i8":1}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testByte_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => 0x01,
            'expectedResult' => 1,
        ];
        yield 'i32' => [
            'buffer' => '{"1":{"i32":1073741824}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testI32_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => pow(2, 30),
            'expectedResult' => 1,
        ];
        if (PHP_INT_SIZE == 8) {
            yield 'i64_64Architecture' => [
                'buffer' => '{"1":{"i64":' . pow(2, 60) . '}}',
                'argsClassName' => \Basic\ThriftTest\ThriftTest_testI64_args::class,
                'argsPropertyName' => 'thing',
                'expectedValue' => pow(2, 60),
                'expectedResult' => 1,
            ];
            yield 'struct_64Architecture' => [
                'buffer' => '{"1":{"rec":{"1":{"str":"worked"},"4":{"i8":1},"9":{"i32":1073741824},"11":{"i64":' . pow(2, 60) . '}}}}',
                'argsClassName' => \Basic\ThriftTest\ThriftTest_testStruct_args::class,
                'argsPropertyName' => 'thing',
                'expectedValue' => new Xtruct(
                    array(
                        'string_thing' => 'worked',
                        'byte_thing' => 0x01,
                        'i32_thing' => pow(2, 30),
                        'i64_thing' => pow(2, 60),
                    )
                ),
                'expectedResult' => 4,
            ];
            yield 'nest_64Architecture' => [
                'buffer' => '{"1":{"rec":{"1":{"i8":1},"2":{"rec":{"1":{"str":"worked"},"4":{"i8":1},"9":{"i32":1073741824},"11":{"i64":' . pow(2, 60) . '}}},"3":{"i32":32768}}}}',
                'argsClassName' => \Basic\ThriftTest\ThriftTest_testNest_args::class,
                'argsPropertyName' => 'thing',
                'expectedValue' => new Xtruct2(
                    array(
                        'byte_thing' => 0x01,
                        'struct_thing' => new Xtruct(
                            array(
                                'string_thing' => 'worked',
                                'byte_thing' => 0x01,
                                'i32_thing' => pow(2, 30),
                                'i64_thing' => pow(2, 60),
                            )
                        ),
                        'i32_thing' => pow(2, 15)
                    )
                ),
                'expectedResult' => 6,
            ];
        } else {
            yield 'i64_32Architecture' => [
                'buffer' => '{"1":{"i64":1152921504606847000}}',
                'argsClassName' => \Basic\ThriftTest\ThriftTest_testI64_args::class,
                'argsPropertyName' => 'thing',
                'expectedValue' => "1152921504606847000",
                'expectedResult' => 1,
            ];
            yield 'struct_32Architecture' => [
                'buffer' => '{"1":{"rec":{"1":{"str":"worked"},"4":{"i8":1},"9":{"i32":1073741824},"11":{"i64":1152921504606847000}}}}',
                'argsClassName' => \Basic\ThriftTest\ThriftTest_testStruct_args::class,
                'expectedValue' => new Xtruct(
                    array(
                        'string_thing' => 'worked',
                        'byte_thing' => 0x01,
                        'i32_thing' => pow(2, 30),
                        'i64_thing' => "1152921504606847000",
                    )
                ),
                'expectedResult' => 4,
            ];
            yield 'nest_32Architecture' => [
                'buffer' => '{"1":{"rec":{"1":{"i8":1},"2":{"rec":{"1":{"str":"worked"},"4":{"i8":1},"9":{"i32":1073741824},"11":{"i64":1152921504606847000}}},"3":{"i32":32768}}}}',
                'argsClassName' => \Basic\ThriftTest\ThriftTest_testNest_args::class,
                'argsPropertyName' => 'thing',
                'expectedValue' => new Xtruct2(
                    array(
                        'byte_thing' => 0x01,
                        'struct_thing' => new Xtruct(
                            array(
                                'string_thing' => 'worked',
                                'byte_thing' => 0x01,
                                'i32_thing' => pow(2, 30),
                                'i64_thing' => "1152921504606847000",
                            )
                        ),
                        'i32_thing' => pow(2, 15)
                    )
                ),
                'expectedResult' => 6,
            ];
        }
        yield 'map' => [
            'buffer' => '{"1":{"map":["i32","i32",3,{"7":77,"8":88,"9":99}]}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testMap_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => [
                7 => 77,
                8 => 88,
                9 => 99
            ],
            'expectedResult' => 6,
        ];
        yield 'stringMap' => [
            'buffer' => '{"1":{"map":["str","str",6,{"a":"123","a b":"with spaces ","same":"same","0":"numeric key","longValue":"Afrikaans, Alemannisch, Aragon\u00e9s, \u0627\u0644\u0639\u0631\u0628\u064a\u0629, \u0645\u0635\u0631\u0649, Asturianu, Aymar aru, Az\u0259rbaycan, \u0411\u0430\u0448\u04a1\u043e\u0440\u0442, Boarisch, \u017demait\u0117\u0161ka, \u0411\u0435\u043b\u0430\u0440\u0443\u0441\u043a\u0430\u044f, \u0411\u0435\u043b\u0430\u0440\u0443\u0441\u043a\u0430\u044f (\u0442\u0430\u0440\u0430\u0448\u043a\u0435\u0432\u0456\u0446\u0430), \u0411\u044a\u043b\u0433\u0430\u0440\u0441\u043a\u0438, Bamanankan, \u09ac\u09be\u0982\u09b2\u09be, Brezhoneg, Bosanski, Catal\u00e0, M\u00ecng-d\u0115\u0324ng-ng\u1e73\u0304, \u041d\u043e\u0445\u0447\u0438\u0439\u043d, Cebuano, \u13e3\u13b3\u13a9, \u010cesky, \u0421\u043b\u043e\u0432\u0463\u0301\u043d\u044c\u0441\u043a\u044a \/ \u2c14\u2c0e\u2c11\u2c02\u2c21\u2c10\u2c20\u2c14\u2c0d\u2c1f, \u0427\u04d1\u0432\u0430\u0448\u043b\u0430, Cymraeg, Dansk, Zazaki, \u078b\u07a8\u0788\u07ac\u0780\u07a8\u0784\u07a6\u0790\u07b0, \u0395\u03bb\u03bb\u03b7\u03bd\u03b9\u03ba\u03ac, Emili\u00e0n e rumagn\u00f2l, English, Esperanto, Espa\u00f1ol, Eesti, Euskara, \u0641\u0627\u0631\u0633\u06cc, Suomi, V\u00f5ro, F\u00f8royskt, Fran\u00e7ais, Arpetan, Furlan, Frysk, Gaeilge, \u8d1b\u8a9e, G\u00e0idhlig, Galego, Ava\u00f1e\'\u1ebd, \u0a97\u0ac1\u0a9c\u0ab0\u0abe\u0aa4\u0ac0, Gaelg, \u05e2\u05d1\u05e8\u05d9\u05ea, \u0939\u093f\u0928\u094d\u0926\u0940, Fiji Hindi, Hrvatski, Krey\u00f2l ayisyen, Magyar, \u0540\u0561\u0575\u0565\u0580\u0565\u0576, Interlingua, Bahasa Indonesia, Ilokano, Ido, \u00cdslenska, Italiano, \u65e5\u672c\u8a9e, Lojban, Basa Jawa, \u10e5\u10d0\u10e0\u10d7\u10e3\u10da\u10d8, Kongo, Kalaallisut, \u0c95\u0ca8\u0ccd\u0ca8\u0ca1, \ud55c\uad6d\uc5b4, \u041a\u044a\u0430\u0440\u0430\u0447\u0430\u0439-\u041c\u0430\u043b\u043a\u044a\u0430\u0440, Ripoarisch, Kurd\u00ee, \u041a\u043e\u043c\u0438, Kernewek, \u041a\u044b\u0440\u0433\u044b\u0437\u0447\u0430, Latina, Ladino, L\u00ebtzebuergesch, Limburgs, Ling\u00e1la, \u0ea5\u0eb2\u0ea7, Lietuvi\u0173, Latvie\u0161u, Basa Banyumasan, Malagasy, \u041c\u0430\u043a\u0435\u0434\u043e\u043d\u0441\u043a\u0438, \u0d2e\u0d32\u0d2f\u0d3e\u0d33\u0d02, \u092e\u0930\u093e\u0920\u0940, Bahasa Melayu, \u0645\u0627\u0632\u0650\u0631\u0648\u0646\u06cc, Nnapulitano, Nedersaksisch, \u0928\u0947\u092a\u093e\u0932 \u092d\u093e\u0937\u093e, Nederlands, \u202aNorsk (nynorsk)\u202c, \u202aNorsk (bokm\u00e5l)\u202c, Nouormand, Din\u00e9 bizaad, Occitan, \u0418\u0440\u043e\u043d\u0430\u0443, Papiamentu, Deitsch, Norfuk \/ Pitkern, Polski, \u067e\u0646\u062c\u0627\u0628\u06cc, \u067e\u069a\u062a\u0648, Portugu\u00eas, Runa Simi, Rumantsch, Romani, Rom\u00e2n\u0103, \u0420\u0443\u0441\u0441\u043a\u0438\u0439, \u0421\u0430\u0445\u0430 \u0442\u044b\u043b\u0430, Sardu, Sicilianu, Scots, S\u00e1megiella, Simple English, Sloven\u010dina, Sloven\u0161\u010dina, \u0421\u0440\u043f\u0441\u043a\u0438 \/ Srpski, Seeltersk, Svenska, Kiswahili, \u0ba4\u0bae\u0bbf\u0bb4\u0bcd, \u0c24\u0c46\u0c32\u0c41\u0c17\u0c41, \u0422\u043e\u04b7\u0438\u043a\u04e3, \u0e44\u0e17\u0e22, T\u00fcrkmen\u00e7e, Tagalog, T\u00fcrk\u00e7e, \u0422\u0430\u0442\u0430\u0440\u0447\u0430\/Tatar\u00e7a, \u0423\u043a\u0440\u0430\u0457\u043d\u0441\u044c\u043a\u0430, \u0627\u0631\u062f\u0648, Ti\u1ebfng Vi\u1ec7t, Volap\u00fck, Walon, Winaray, \u5434\u8bed, isiXhosa, \u05d9\u05d9\u05b4\u05d3\u05d9\u05e9, Yor\u00f9b\u00e1, Ze\u00eauws, \u4e2d\u6587, B\u00e2n-l\u00e2m-g\u00fa, \u7cb5\u8a9e","Afrikaans, Alemannisch, Aragon\u00e9s, \u0627\u0644\u0639\u0631\u0628\u064a\u0629, \u0645\u0635\u0631\u0649, Asturianu, Aymar aru, Az\u0259rbaycan, \u0411\u0430\u0448\u04a1\u043e\u0440\u0442, Boarisch, \u017demait\u0117\u0161ka, \u0411\u0435\u043b\u0430\u0440\u0443\u0441\u043a\u0430\u044f, \u0411\u0435\u043b\u0430\u0440\u0443\u0441\u043a\u0430\u044f (\u0442\u0430\u0440\u0430\u0448\u043a\u0435\u0432\u0456\u0446\u0430), \u0411\u044a\u043b\u0433\u0430\u0440\u0441\u043a\u0438, Bamanankan, \u09ac\u09be\u0982\u09b2\u09be, Brezhoneg, Bosanski, Catal\u00e0, M\u00ecng-d\u0115\u0324ng-ng\u1e73\u0304, \u041d\u043e\u0445\u0447\u0438\u0439\u043d, Cebuano, \u13e3\u13b3\u13a9, \u010cesky, \u0421\u043b\u043e\u0432\u0463\u0301\u043d\u044c\u0441\u043a\u044a \/ \u2c14\u2c0e\u2c11\u2c02\u2c21\u2c10\u2c20\u2c14\u2c0d\u2c1f, \u0427\u04d1\u0432\u0430\u0448\u043b\u0430, Cymraeg, Dansk, Zazaki, \u078b\u07a8\u0788\u07ac\u0780\u07a8\u0784\u07a6\u0790\u07b0, \u0395\u03bb\u03bb\u03b7\u03bd\u03b9\u03ba\u03ac, Emili\u00e0n e rumagn\u00f2l, English, Esperanto, Espa\u00f1ol, Eesti, Euskara, \u0641\u0627\u0631\u0633\u06cc, Suomi, V\u00f5ro, F\u00f8royskt, Fran\u00e7ais, Arpetan, Furlan, Frysk, Gaeilge, \u8d1b\u8a9e, G\u00e0idhlig, Galego, Ava\u00f1e\'\u1ebd, \u0a97\u0ac1\u0a9c\u0ab0\u0abe\u0aa4\u0ac0, Gaelg, \u05e2\u05d1\u05e8\u05d9\u05ea, \u0939\u093f\u0928\u094d\u0926\u0940, Fiji Hindi, Hrvatski, Krey\u00f2l ayisyen, Magyar, \u0540\u0561\u0575\u0565\u0580\u0565\u0576, Interlingua, Bahasa Indonesia, Ilokano, Ido, \u00cdslenska, Italiano, \u65e5\u672c\u8a9e, Lojban, Basa Jawa, \u10e5\u10d0\u10e0\u10d7\u10e3\u10da\u10d8, Kongo, Kalaallisut, \u0c95\u0ca8\u0ccd\u0ca8\u0ca1, \ud55c\uad6d\uc5b4, \u041a\u044a\u0430\u0440\u0430\u0447\u0430\u0439-\u041c\u0430\u043b\u043a\u044a\u0430\u0440, Ripoarisch, Kurd\u00ee, \u041a\u043e\u043c\u0438, Kernewek, \u041a\u044b\u0440\u0433\u044b\u0437\u0447\u0430, Latina, Ladino, L\u00ebtzebuergesch, Limburgs, Ling\u00e1la, \u0ea5\u0eb2\u0ea7, Lietuvi\u0173, Latvie\u0161u, Basa Banyumasan, Malagasy, \u041c\u0430\u043a\u0435\u0434\u043e\u043d\u0441\u043a\u0438, \u0d2e\u0d32\u0d2f\u0d3e\u0d33\u0d02, \u092e\u0930\u093e\u0920\u0940, Bahasa Melayu, \u0645\u0627\u0632\u0650\u0631\u0648\u0646\u06cc, Nnapulitano, Nedersaksisch, \u0928\u0947\u092a\u093e\u0932 \u092d\u093e\u0937\u093e, Nederlands, \u202aNorsk (nynorsk)\u202c, \u202aNorsk (bokm\u00e5l)\u202c, Nouormand, Din\u00e9 bizaad, Occitan, \u0418\u0440\u043e\u043d\u0430\u0443, Papiamentu, Deitsch, Norfuk \/ Pitkern, Polski, \u067e\u0646\u062c\u0627\u0628\u06cc, \u067e\u069a\u062a\u0648, Portugu\u00eas, Runa Simi, Rumantsch, Romani, Rom\u00e2n\u0103, \u0420\u0443\u0441\u0441\u043a\u0438\u0439, \u0421\u0430\u0445\u0430 \u0442\u044b\u043b\u0430, Sardu, Sicilianu, Scots, S\u00e1megiella, Simple English, Sloven\u010dina, Sloven\u0161\u010dina, \u0421\u0440\u043f\u0441\u043a\u0438 \/ Srpski, Seeltersk, Svenska, Kiswahili, \u0ba4\u0bae\u0bbf\u0bb4\u0bcd, \u0c24\u0c46\u0c32\u0c41\u0c17\u0c41, \u0422\u043e\u04b7\u0438\u043a\u04e3, \u0e44\u0e17\u0e22, T\u00fcrkmen\u00e7e, Tagalog, T\u00fcrk\u00e7e, \u0422\u0430\u0442\u0430\u0440\u0447\u0430\/Tatar\u00e7a, \u0423\u043a\u0440\u0430\u0457\u043d\u0441\u044c\u043a\u0430, \u0627\u0631\u062f\u0648, Ti\u1ebfng Vi\u1ec7t, Volap\u00fck, Walon, Winaray, \u5434\u8bed, isiXhosa, \u05d9\u05d9\u05b4\u05d3\u05d9\u05e9, Yor\u00f9b\u00e1, Ze\u00eauws, \u4e2d\u6587, B\u00e2n-l\u00e2m-g\u00fa, \u7cb5\u8a9e":"long key"}]}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testStringMap_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => [
                "a" => "123",
                "a b" => "with spaces ",
                "same" => "same",
                "0" => "numeric key",
                "longValue" => "Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk / Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски / Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語",
                "Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk / Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски / Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語" => "long key"
            ],
            'expectedResult' => 12,
        ];
        yield 'set' => [
            'buffer' => '{"1":{"set":["i32",3,1,5,6]}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testSet_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => [1 => true, 5 => true, 6 => true],
            'expectedResult' => 4,
        ];
        yield 'list' => [
            'buffer' => '{"1":{"lst":["i32",3,1,2,3]}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testList_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => [1, 2, 3],
            'expectedResult' => 4,
        ];
        yield 'enum' => [
            'buffer' => '{"1":{"i32":6}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testEnum_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => Numberz::SIX,
            'expectedResult' => 1,
        ];
        yield 'typedef' => [
            'buffer' => '{"1":{"i64":69}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testTypedef_args::class,
            'argsPropertyName' => 'thing',
            'expectedValue' => 69,
            'expectedResult' => 1,
        ];
        yield 'mapmap' => [
            'buffer' => '{"0":{"map":["i32","map",2,{"4":["i32","i32",4,{"1":1,"2":2,"3":3,"4":4}],"-4":["i32","i32",4,{"-4":-4,"-3":-3,"-2":-2,"-1":-1}]}]}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testMapMap_result::class,
            'argsPropertyName' => 'success',
            'expectedValue' => [
                4 => [
                    1 => 1,
                    2 => 2,
                    3 => 3,
                    4 => 4,
                ],
                -4 => [
                    -4 => -4,
                    -3 => -3,
                    -2 => -2,
                    -1 => -1
                ]
            ],
            'expectedResult' => 18,
        ];

        $xtruct1 = new Xtruct(
            [
                'string_thing' => 'Goodbye4',
                'byte_thing' => 4,
                'i32_thing' => 4,
                'i64_thing' => 4,
            ]
        );

        $xtruct2 = new Xtruct(
            [
                'string_thing' => 'Hello2',
                'byte_thing' => 2,
                'i32_thing' => 2,
                'i64_thing' => 2,
            ]
        );

        $userMap = [Numberz::FIVE => 5, Numberz::EIGHT => 8];

        $insanity2 = new Insanity(
            [
                'userMap' => $userMap,
                'xtructs' => [$xtruct1, $xtruct2],
            ]
        );

        $insanity3 = $insanity2;

        $insanity6 =
            new Insanity(
                [
                    'userMap' => null,
                    'xtructs' => null,
                ]
            );
        yield 'insanity' => [
            'buffer' => '{"0":{"map":["i64","map",2,{"1":["i32","rec",2,{"2":{"1":{"map":["i32","i64",2,{"5":5,"8":8}]},"2":{"lst":["rec",2,{"1":{"str":"Goodbye4"},"4":{"i8":4},"9":{"i32":4},"11":{"i64":4}},{"1":{"str":"Hello2"},"4":{"i8":2},"9":{"i32":2},"11":{"i64":2}}]}},"3":{"1":{"map":["i32","i64",2,{"5":5,"8":8}]},"2":{"lst":["rec",2,{"1":{"str":"Goodbye4"},"4":{"i8":4},"9":{"i32":4},"11":{"i64":4}},{"1":{"str":"Hello2"},"4":{"i8":2},"9":{"i32":2},"11":{"i64":2}}]}}}],"2":["i32","rec",1,{"6":{}}]}]}}',
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testInsanity_result::class,
            'argsPropertyName' => 'success',
            'expectedValue' => [
                "1" => [
                    Numberz::TWO => $insanity2,
                    Numberz::THREE => $insanity3,
                ],
                "2" => [
                    Numberz::SIX => $insanity6,
                ],
            ],
            'expectedResult' => 31,
        ];
    }
}
