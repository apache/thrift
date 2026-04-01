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
use Thrift\Protocol\TSimpleJSONProtocol;
use Thrift\Transport\TMemoryBuffer;
use Basic\ThriftTest\Xtruct;
use Basic\ThriftTest\Xtruct2;

/***
 * This test suite depends on running the compiler against the ./Resources/ThriftTest.thrift file:
 * lib/php/test$ ../../../compiler/cpp/thrift --gen php:nsglobal="Basic" -r  --out ./Resources/packages/php ./Resources/ThriftTest.thrift
 */
class TSimpleJSONProtocolTest extends TestCase
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
        $this->protocol = new TSimpleJSONProtocol($this->transport);
        $this->transport->open();
    }

    private static function expectedStringArg(string $value): string
    {
        return '{"thing":' . json_encode($value, JSON_UNESCAPED_SLASHES) . '}';
    }

    private static function expectedStringMapArg(array $value): string
    {
        return '{"thing":' . json_encode($value, JSON_UNESCAPED_SLASHES) . '}';
    }

    public function testMessageWrite()
    {
        $input = new TJSONProtocol(new TMemoryBuffer('[1,"testString",1,0,{"0":{"str":"successResponse"}}]'));
        $service = new \Basic\ThriftTest\ThriftTestClient($input, $this->protocol);
        $result = $service->testString('test');
        $this->assertSame('successResponse', $result);
        $this->assertSame('["testString",1,0,{"thing":"test"}]', $this->protocol->getTransport()->getBuffer());
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
            'expected' => '{"thing":1}',
        ];
        yield 'bool false' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testBool_args::class,
            'argsValues' => [
                'thing' => false,
            ],
            'expected' => '{"thing":0}',
        ];
        $string1 = "Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk / Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски / Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語";
        yield 'string1' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testString_args::class,
            'argsValues' => [
                'thing' => $string1,
            ],
            'expected' => self::expectedStringArg($string1),
        ];
        $string2 = "quote: \\\" backslash:" .
                    " forwardslash-escaped: \\/ " .
                    " backspace: \b formfeed: \f newline: \n return: \r tab: " .
                    " now-all-of-them-together: \"\\\/\b\n\r\t" .
                    " now-a-bunch-of-junk: !@#\$%&()(&%$#{}{}<><><";
        yield 'string2' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testString_args::class,
            'argsValues' => [
                'thing' => $string2,
            ],
            'expected' => self::expectedStringArg($string2),
        ];

        $string3 = "string that ends in double-backslash \\\\";
        yield 'string3' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testString_args::class,
            'argsValues' => [
                'thing' => $string3,
            ],
            'expected' => self::expectedStringArg($string3),
        ];
        $string4 = "สวัสดี/𝒯";
        yield 'string4 testUnicodeStringWithNonBMP' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testString_args::class,
            'argsValues' => [
                'thing' => $string4,
            ],
            'expected' => self::expectedStringArg($string4),
        ];
        yield 'double' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testDouble_args::class,
            'argsValues' => [
                'thing' => 3.1415926535898,
            ],
            'expected' => '{"thing":3.1415926535898}',
        ];
        #TODO Should be fixed in future
        yield 'double Nan' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testDouble_args::class,
            'argsValues' => [
                'thing' => NAN,
            ],
            'expected' => '{"thing":}',
        ];
        #TODO Should be fixed in future
        yield 'double Infinity' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testDouble_args::class,
            'argsValues' => [
                'thing' => INF,
            ],
            'expected' => '{"thing":}',
        ];
        yield 'byte' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testByte_args::class,
            'argsValues' => [
                'thing' => 0x01,
            ],
            'expected' => '{"thing":1}',
        ];
        yield 'i32' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testI32_args::class,
            'argsValues' => [
                'thing' => pow(2, 30),
            ],
            'expected' => '{"thing":1073741824}',
        ];
        if (PHP_INT_SIZE == 8) {
            yield 'i64_64Architecture' => [
                'argsClassName' => \Basic\ThriftTest\ThriftTest_testI64_args::class,
                'argsValues' => [
                    'thing' => pow(2, 60),
                ],
                'expected' => '{"thing":' . pow(2, 60) . '}',
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
                'expected' => '{"thing":{"string_thing":"worked","byte_thing":1,"i32_thing":1073741824,"i64_thing":' . pow(2, 60) . '}}',
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
                'expected' => '{"thing":{"byte_thing":1,"struct_thing":{"string_thing":"worked","byte_thing":1,"i32_thing":1073741824,"i64_thing":' . pow(2, 60) . '},"i32_thing":32768}}',
            ];
        } else {
            yield 'i64_32Architecture' => [
                'argsClassName' => \Basic\ThriftTest\ThriftTest_testI64_args::class,
                'argsValues' => [
                    'thing' => "1152921504606847000",
                ],
                'expected' => '{"thing":1152921504606847000}',
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
                'expected' => '{"thing":{"string_thing":"worked","byte_thing":1,"i32_thing":1073741824,"i64_thing":1152921504606847000}}',
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
                'expected' => '{"thing":{"byte_thing":1,"struct_thing":{"string_thing":"worked","byte_thing":1,"i32_thing":1073741824,"i64_thing":1152921504606847000},"i32_thing":32768}}',
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
            'expected' => '{"thing":{"7":77,"8":88,"9":99}}',
        ];
        $stringMap = [
                    "a" => "123",
                    "a b" => "with spaces ",
                    "same" => "same",
                    "0" => "numeric key",
                    "longValue" => "Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk / Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски / Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語",
                    "Afrikaans, Alemannisch, Aragonés, العربية, مصرى, Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška, Беларуская, Беларуская (тарашкевіца), Български, Bamanankan, বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн, Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg, Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English, Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt, Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego, Avañe'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski, Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia, Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa, ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар, Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino, Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, Bahasa Melayu, مازِرونی, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad, Occitan, Иронау, Papiamentu, Deitsch, Norfuk / Pitkern, Polski, پنجابی, پښتو, Português, Runa Simi, Rumantsch, Romani, Română, Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple English, Slovenčina, Slovenščina, Српски / Srpski, Seeltersk, Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog, Türkçe, Татарча/Tatarça, Українська, اردو, Tiếng Việt, Volapük, Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文, Bân-lâm-gú, 粵語" => "long key"
                ];
        yield 'stringMap' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testStringMap_args::class,
            'argsValues' => [
                'thing' => $stringMap,
            ],
            'expected' => self::expectedStringMapArg($stringMap),
        ];
        yield 'set' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testSet_args::class,
            'argsValues' => [
                'thing' => [1 => true, 5 => true, 6 => true],
            ],
            'expected' => '{"thing":[1,5,6]}',
        ];
        yield 'list' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testList_args::class,
            'argsValues' => [
                'thing' => [1, 2, 3],
            ],
            'expected' => '{"thing":[1,2,3]}',
        ];
        yield 'enum' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testEnum_args::class,
            'argsValues' => [
                'thing' => \Basic\ThriftTest\Numberz::SIX,
            ],
            'expected' => '{"thing":6}',
        ];
        yield 'typedef' => [
            'argsClassName' => \Basic\ThriftTest\ThriftTest_testTypedef_args::class,
            'argsValues' => [
                'thing' => 69,
            ],
            'expected' => '{"thing":69}',
        ];
    }
}
