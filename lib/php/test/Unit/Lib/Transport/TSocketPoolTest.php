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

declare(strict_types=1);

namespace Test\Thrift\Unit\Lib\Transport;

use phpmock\phpunit\PHPMock;
use PHPUnit\Framework\TestCase;
use PHPUnit\Framework\Constraint\Constraint;
use PHPUnit\Framework\Assert;
use PHPUnit\Framework\Attributes\DataProvider;
use Test\Thrift\Unit\Lib\ReflectionHelper;
use Thrift\Exception\TException;
use Thrift\Transport\TSocket;
use Thrift\Transport\TSocketPool;

class TSocketPoolTest extends TestCase
{
    use PHPMock;
    use ReflectionHelper;

    protected function setUp(): void
    {
        #need to be defined before the TSocketPool class definition
        self::defineFunctionMock('Thrift\Transport', 'function_exists');

        $this->getAccessibleProperty(TSocketPool::class, 'hasApcuCache')
             ->setValue(null, null);
        $this->getAccessibleProperty(TSocket::class, 'hasSocketsExtension')
             ->setValue(null, null);
    }

    #[DataProvider('constructDataProvider')]
    public function testConstruct(
        $hosts,
        $ports,
        $persist,
        $debugHandler,
        $expectedServers
    ) {
        $socketPool = new TSocketPool($hosts, $ports, $persist, $debugHandler);

        $this->assertEquals($expectedServers, $this->getPropertyValue($socketPool, 'servers'));
    }


    public static function constructDataProvider()
    {
        yield 'one server' => [
            ['localhost'],
            [9090],
            false,
            null,
            [
                ['host' => 'localhost', 'port' => 9090],
            ],
        ];
        yield 'two servers' => [
            ['localhost1', 'localhost2'],
            [9090, 9091],
            false,
            null,
            [
                ['host' => 'localhost1', 'port' => 9090],
                ['host' => 'localhost2', 'port' => 9091],
            ],
        ];
        yield 'one server with one port' => [
            ['localhost'],
            9090,
            false,
            null,
            [
                ['host' => 'localhost', 'port' => 9090],
            ],
        ];
        yield 'two servers with one port' => [
            ['localhost1', 'localhost2'],
            9090,
            false,
            null,
            [
                ['host' => 'localhost1', 'port' => 9090],
                ['host' => 'localhost2', 'port' => 9090],
            ],
        ];
    }

    public function testAddServer(): void
    {
        $socketPool = new TSocketPool([], []);
        $socketPool->addServer('localhost', 9090);

        $this->assertEquals([['host' => 'localhost', 'port' => 9090]], $this->getPropertyValue($socketPool, 'servers'));
    }

    public function testSetNumRetries(): void
    {
        $socketPool = new TSocketPool([], []);
        $socketPool->setNumRetries(5);

        $this->assertEquals(5, $this->getPropertyValue($socketPool, 'numRetries'));
    }

    public function testrSetRetryInterval(): void
    {
        $socketPool = new TSocketPool([], []);
        $socketPool->setRetryInterval(5);

        $this->assertEquals(5, $this->getPropertyValue($socketPool, 'retryInterval'));
    }

    public function testrSetMaxConsecutiveFailures(): void
    {
        $socketPool = new TSocketPool([], []);
        $socketPool->setMaxConsecutiveFailures(5);

        $this->assertEquals(5, $this->getPropertyValue($socketPool, 'maxConsecutiveFailures'));
    }

    public function testrSetRandomize(): void
    {
        $socketPool = new TSocketPool([], []);
        $socketPool->setRandomize(false);

        $this->assertEquals(false, $this->getPropertyValue($socketPool, 'randomize'));
    }

    public function testrSetAlwaysTryLast(): void
    {
        $socketPool = new TSocketPool([], []);
        $socketPool->setAlwaysTryLast(false);

        $this->assertEquals(false, $this->getPropertyValue($socketPool, 'alwaysTryLast'));
    }

    #[DataProvider('openDataProvider')]
    public function testOpen(
        $hosts,
        $ports,
        $persist,
        $debugHandler,
        $randomize,
        $retryInterval,
        $numRetries,
        $maxConsecutiveFailures,
        $debug,
        $servers,
        $functionExistCallParams,
        $functionExistResult,
        $apcuFetchCallParams,
        $apcuFetchResult,
        $timeResult,
        $debugHandlerCall,
        $apcuStoreCallParams,
        $fsockopenCallParams,
        $fsockopenResult,
        $expectedException,
        $expectedExceptionMessage
    ) {
        $this->getFunctionMock('Thrift\Transport', 'function_exists')
             ->expects($this->exactly(count($functionExistCallParams)))
             ->willReturnCallback(function (...$callArgs) use ($functionExistCallParams, $functionExistResult) {
                 static $iteration = 0;
                 $expected = $functionExistCallParams[$iteration];
                foreach ($expected as $i => $exp) {
                    if ($exp instanceof Constraint) {
                        $this->assertThat($callArgs[$i], $exp);
                    } else {
                        $this->assertSame($exp, $callArgs[$i]);
                    }
                }

                 return $functionExistResult[$iteration++];
             });

        $this->getFunctionMock('Thrift\Transport', 'shuffle')
             ->expects($randomize ? $this->once() : $this->never())
             ->with($servers)
             ->willReturnCallback(function (array &$servers) {
                 $servers = array_reverse($servers);

                 return true;
             });

        $this->getFunctionMock('Thrift\Transport', 'apcu_fetch')
             ->expects($this->exactly(count($apcuFetchCallParams)))
             ->willReturnCallback(function (...$callArgs) use ($apcuFetchCallParams, $apcuFetchResult) {
                 static $iteration = 0;
                 $expected = $apcuFetchCallParams[$iteration];
                foreach ($expected as $i => $exp) {
                    if ($exp instanceof Constraint) {
                        $this->assertThat($callArgs[$i], $exp);
                    } else {
                        $this->assertSame($exp, $callArgs[$i]);
                    }
                }

                 return $apcuFetchResult[$iteration++];
             });

        $this->getFunctionMock('Thrift\Transport', 'call_user_func')
             ->expects($this->exactly(count($debugHandlerCall)))
             ->willReturnCallback(function (...$callArgs) use ($debugHandlerCall) {
                 static $iteration = 0;
                 $expected = $debugHandlerCall[$iteration++];
                foreach ($expected as $i => $exp) {
                    if ($exp instanceof Constraint) {
                        $this->assertThat($callArgs[$i], $exp);
                    } else {
                        $this->assertSame($exp, $callArgs[$i]);
                    }
                }

                 return true;
             });

        $this->getFunctionMock('Thrift\Transport', 'apcu_store')
             ->expects($this->exactly(count($apcuStoreCallParams)))
             ->willReturnCallback(function (...$callArgs) use ($apcuStoreCallParams) {
                 static $iteration = 0;
                 $expected = $apcuStoreCallParams[$iteration++];
                foreach ($expected as $i => $exp) {
                    if ($exp instanceof Constraint) {
                        $this->assertThat($callArgs[$i], $exp);
                    } else {
                        $this->assertSame($exp, $callArgs[$i]);
                    }
                }

                 return true;
             });

        $this->getFunctionMock('Thrift\Transport', 'time')
             ->expects($this->exactly(count($timeResult)))
             ->willReturnOnConsecutiveCalls(...$timeResult);

        #due to the running tests in separate process we could not open stream in data provider, so we need to do it here
        foreach ($fsockopenResult as $num => $result) {
            $fsockopenResult[$num] = $result ? fopen(...$result) : $result;
        }

        $this->getFunctionMock('Thrift\Transport', $persist ? 'pfsockopen' : 'fsockopen')
             ->expects($this->exactly(count($fsockopenCallParams)))
             ->willReturnCallback(function (...$callArgs) use ($fsockopenCallParams, $fsockopenResult) {
                 static $iteration = 0;
                 $expected = $fsockopenCallParams[$iteration];
                foreach ($expected as $i => $exp) {
                    if ($exp instanceof Constraint) {
                        $this->assertThat($callArgs[$i], $exp);
                    } else {
                        $this->assertSame($exp, $callArgs[$i]);
                    }
                }

                 return $fsockopenResult[$iteration++];
             });

        $this->getFunctionMock('Thrift\Transport', 'socket_import_stream')
             ->expects(is_null($expectedException) ? $this->once() : $this->never())
             ->with(
                 $this->callback(function ($stream) {
                     return is_resource($stream);
                 })
             )
             ->willReturn(true);

        $this->getFunctionMock('Thrift\Transport', 'socket_set_option')
             ->expects(is_null($expectedException) ? $this->once() : $this->never())
             ->with(
                 Assert::anything(), #$socket,
                 SOL_TCP, #$level
                 TCP_NODELAY, #$option
                 1 #$value
             )
             ->willReturn(true);

        if ($expectedException) {
            $this->expectException($expectedException);
            $this->expectExceptionMessage($expectedExceptionMessage);
        }

        $socketPool = new TSocketPool($hosts, $ports, $persist, $debugHandler);
        $socketPool->setRandomize($randomize);
        $socketPool->setRetryInterval($retryInterval);
        $socketPool->setNumRetries($numRetries);
        $socketPool->setMaxConsecutiveFailures($maxConsecutiveFailures);
        $socketPool->setDebug($debug);

        $this->assertNull($socketPool->open());
    }

    public static function openDataProvider()
    {
        $default = [
            'hosts' => ['localhost'],
            'ports' => [9090],
            'persist' => false,
            'debugHandler' => null,
            'randomize' => true,
            'retryInterval' => 5,
            'numRetries' => 1,
            'maxConsecutiveFailures' => 1,
            'debug' => false,
            'servers' => [
                ['host' => 'localhost', 'port' => 9090],
            ],
            'functionExistCallParams' => [
                ['apcu_fetch'],
                ['socket_import_stream'],
                ['socket_set_option'],
            ],
            'functionExistResult' => [
                true,
                true,
                true,
            ],
            'apcuFetchCallParams' => [
                ['thrift_failtime:localhost:9090~', Assert::anything()],
            ],
            'apcuFetchResult' => [
                false,
            ],
            'timeResult' => [],
            'debugHandlerCall' => [],
            'apcuStoreCallParams' => [],
            'fsockopenCallParams' => [
                [
                    'localhost',
                    9090,
                    Assert::anything(), #$errno,
                    Assert::anything(), #$errstr,
                    Assert::anything(), #$this->sendTimeoutSec_ + ($this->sendTimeoutUsec_ / 1000000),
                ],
            ],
            'fsockopenResult' => [
                ['php://temp', 'r'],
            ],
            'expectedException' => null,
            'expectedExceptionMessage' => null,
        ];

        yield 'one server ready' => $default;
        yield 'one server failed' => array_merge(
            $default,
            [
                'functionExistCallParams' => [
                    ['apcu_fetch'],
                ],
                'fsockopenResult' => [
                    false,
                ],
                'apcuFetchCallParams' => [
                    ['thrift_failtime:localhost:9090~', Assert::anything()],
                    ['thrift_consecfails:localhost:9090~', Assert::anything()],
                ],
                'apcuStoreCallParams' => [
                    ['thrift_failtime:localhost:9090~', Assert::anything()],
                    ['thrift_consecfails:localhost:9090~', Assert::anything(), 0],
                ],
                'timeResult' => [
                    1,
                ],
                'expectedException' => TException::class,
                'expectedExceptionMessage' => 'TSocketPool: All hosts in pool are down. (localhost:9090)',
            ]
        );
        yield 'connect to one server on second attempt' => array_merge(
            $default,
            [
                'numRetries' => 2,
                'fsockopenCallParams' => [
                    [
                        'localhost',
                        9090,
                        Assert::anything(), #$errno,
                        Assert::anything(), #$errstr,
                        Assert::anything(), #$this->sendTimeoutSec_ + ($this->sendTimeoutUsec_ / 1000000),
                    ],
                    [
                        'localhost',
                        9090,
                        Assert::anything(), #$errno,
                        Assert::anything(), #$errstr,
                        Assert::anything(), #$this->sendTimeoutSec_ + ($this->sendTimeoutUsec_ / 1000000),
                    ],
                ],
                'fsockopenResult' => [
                    false,
                    ['php://temp', 'r'],
                ],
                'apcuStoreCallParams' => [],
            ]
        );
        yield 'last time fail time is not expired' => array_merge(
            $default,
            [
                'retryInterval' => 5,
                'apcuFetchResult' => [
                    99,
                ],
                'apcuStoreCallParams' => [
                    ['thrift_failtime:localhost:9090~', Assert::anything()],
                ],
                'timeResult' => [
                    100,
                ],
            ]
        );
        yield 'last time fail time is expired, store info to debug' => array_merge(
            $default,
            [
                'retryInterval' => 5,
                'apcuFetchResult' => [
                    90,
                ],
                'apcuStoreCallParams' => [
                    ['thrift_failtime:localhost:9090~', Assert::anything()],
                ],
                'timeResult' => [
                    100,
                ],
                'debug' => true,
                'debugHandlerCall' => [
                    ['error_log', 'TSocketPool: retryInterval (5) has passed for host localhost:9090'],
                ],
            ]
        );
        yield 'not accessible server, store info to debug' => array_merge(
            $default,
            [
                'retryInterval' => 5,
                'functionExistCallParams' => [
                    ['apcu_fetch'],
                ],
                'functionExistResult' => [
                    true,
                ],
                'apcuFetchCallParams' => [
                    ['thrift_failtime:localhost:9090~', Assert::anything()],
                    ['thrift_consecfails:localhost:9090~', Assert::anything()],
                ],
                'apcuFetchResult' => [
                    90,
                ],
                'apcuStoreCallParams' => [
                    ['thrift_failtime:localhost:9090~', Assert::anything()],
                    ['thrift_consecfails:localhost:9090~', 0],
                ],
                'timeResult' => [
                    100,
                    101,
                ],
                'fsockopenResult' => [
                    false,
                ],
                'debug' => true,
                'debugHandlerCall' => [
                    ['error_log', 'TSocketPool: retryInterval (5) has passed for host localhost:9090'],
                    ['error_log', 'TSocket: Could not connect to localhost:9090 ( [])'],
                    ['error_log', 'TSocketPool: marking localhost:9090 as down for 5 secs after 1 failed attempts.'],
                    ['error_log', 'TSocketPool: All hosts in pool are down. (localhost:9090)'],
                ],
                'expectedException' => TException::class,
                'expectedExceptionMessage' => 'TSocketPool: All hosts in pool are down. (localhost:9090)',
            ]
        );
        yield 'max consecutive failures' => array_merge(
            $default,
            [
                'maxConsecutiveFailures' => 5,
                'functionExistCallParams' => [
                    ['apcu_fetch'],
                ],
                'functionExistResult' => [
                    true,
                ],
                'apcuFetchCallParams' => [
                    ['thrift_failtime:localhost:9090~', Assert::anything()],
                    ['thrift_consecfails:localhost:9090~', Assert::anything()],
                ],
                'apcuStoreCallParams' => [
                    ['thrift_consecfails:localhost:9090~', 1],
                ],
                'timeResult' => [],
                'fsockopenResult' => [
                    false,
                ],
                'expectedException' => TException::class,
                'expectedExceptionMessage' => 'TSocketPool: All hosts in pool are down. (localhost:9090)',
            ]
        );
        yield 'apcu disabled' => array_merge(
            $default,
            [
                'functionExistCallParams' => [
                    ['apcu_fetch'],
                ],
                'functionExistResult' => [
                    false,
                ],
                'fsockopenResult' => [
                    false,
                ],
                'timeResult' => [
                    1,
                ],
                'apcuFetchCallParams' => [],
                'apcuStoreCallParams' => [],
                'expectedException' => TException::class,
                'expectedExceptionMessage' => 'TSocketPool: All hosts in pool are down. (localhost:9090)',
            ]
        );
        yield 'second host accessible' => array_merge(
            $default,
            [
                'hosts' => ['host1', 'host2'],
                'ports' => [9090, 9091],
                'servers' => [
                    ['host' => 'host1', 'port' => 9090],
                    ['host' => 'host2', 'port' => 9091],
                ],
                'fsockopenCallParams' => [
                    [
                        'host2',
                        9091,
                        Assert::anything(), #$errno,
                        Assert::anything(), #$errstr,
                        Assert::anything(), #$this->sendTimeoutSec_ + ($this->sendTimeoutUsec_ / 1000000),
                    ],
                    [
                        'host1',
                        9090,
                        Assert::anything(), #$errno,
                        Assert::anything(), #$errstr,
                        Assert::anything(), #$this->sendTimeoutSec_ + ($this->sendTimeoutUsec_ / 1000000),
                    ],
                ],
                'fsockopenResult' => [
                    false,
                    ['php://temp', 'r'],
                ],
                'apcuFetchCallParams' => [
                    ['thrift_failtime:host2:9091~', Assert::anything()],
                    ['thrift_consecfails:host2:9091~', Assert::anything()],
                    ['thrift_failtime:host1:9090~', Assert::anything()],
                ],
                'apcuStoreCallParams' => [
                    ['thrift_failtime:host2:9091~', Assert::anything()],
                    ['thrift_consecfails:host2:9091~', Assert::anything(), 0],
                ],
                'timeResult' => [
                    1,
                ],
            ]
        );
    }
}
