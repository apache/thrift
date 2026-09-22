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

namespace Test\Thrift\Integration\Lib\Transport;

use PHPUnit\Framework\Attributes\DataProvider;
use PHPUnit\Framework\Attributes\RequiresFunction;
use PHPUnit\Framework\Attributes\RequiresPhpExtension;
use PHPUnit\Framework\TestCase;
use Thrift\Exception\TTransportException;
use Thrift\Transport\TCurlClient;

/**
 * TCurlClient against two HTTP listeners on 127.0.0.1, served by a child
 * process. Listener A answers the first request with a redirect and any later
 * one with a reply; listener B, on another port, is another origin. A redirect
 * is followed only within the origin of the configured URL, and then the
 * request goes out again with its headers and body.
 */
#[RequiresPhpExtension('curl')]
#[RequiresFunction('pcntl_fork')]
class TCurlClientRedirectTest extends TestCase
{
    private const BODY = 'THRIFT-REQUEST-BODY';

    private const API_KEY = 'key-for-a';

    public static function anotherOriginDataProvider(): array
    {
        return [
            '302 Found' => [302],
            '307 Temporary Redirect' => [307],
        ];
    }

    #[DataProvider('anotherOriginDataProvider')]
    public function testRedirectToAnotherOriginIsNotFollowed(int $code): void
    {
        [$received, $error] = $this->call($code, 'http://127.0.0.1:{B}/moved');

        $this->assertSame([], $received['B'], 'what reached listener B');
        $this->assertCount(1, $received['A']);
        $this->assertInstanceOf(TTransportException::class, $error);
    }

    public static function sameOriginDataProvider(): array
    {
        return [
            '302, relative location' => [302, '/moved?call=1'],
            '307, absolute location' => [307, 'http://127.0.0.1:{A}/moved?call=1'],
        ];
    }

    #[DataProvider('sameOriginDataProvider')]
    public function testRedirectWithinTheOriginIsFollowedWithTheRequest(int $code, string $location): void
    {
        [$received, $error, $reply] = $this->call($code, $location);

        $this->assertNull($error, $error === null ? '' : $error->getMessage());
        $this->assertSame('reply from A', $reply);
        $this->assertSame([], $received['B'], 'what reached listener B');
        $this->assertCount(2, $received['A']);
        $this->assertSame('POST /moved?call=1 HTTP/1.1', $received['A'][1]['line']);
        $this->assertSame(self::API_KEY, $received['A'][1]['apiKey']);
        $this->assertSame(self::BODY, $received['A'][1]['body']);
    }

    /**
     * Makes one call to listener A, which answers it with $code and $location
     * ({A} and {B} stand for the ports of the listeners).
     *
     * @return array{array<string, list<array<string, ?string>>>, ?TTransportException, string}
     *     what each listener received, the exception flush() threw, and the reply
     */
    private function call(int $code, string $location): array
    {
        $listeners = [];
        $ports = [];
        foreach (['A', 'B'] as $name) {
            $listener = stream_socket_server('tcp://127.0.0.1:0', $errno, $errstr);
            $this->assertNotFalse($listener, $errstr);
            $listeners[$name] = $listener;
            $ports['{' . $name . '}'] = (string) parse_url('tcp://' . stream_socket_get_name($listener, false), PHP_URL_PORT);
        }
        $control = stream_socket_pair(STREAM_PF_UNIX, STREAM_SOCK_STREAM, STREAM_IPPROTO_IP);
        $this->assertNotFalse($control);

        $pid = \pcntl_fork();
        if ($pid === 0) {
            // Keep whatever the child prints on its way out to itself.
            ob_start(static function (): string {
                return '';
            });
            fclose($control[0]);
            $this->serve($listeners, $control[1], $code, strtr($location, $ports));
            exit(0);
        }
        $this->assertGreaterThan(0, $pid, 'fork failed');
        fclose($control[1]);
        foreach ($listeners as $listener) {
            fclose($listener);
        }

        $transport = new TCurlClient('127.0.0.1', (int) $ports['{A}'], '/rpc');
        $transport->setTimeoutSecs(2.0);
        $transport->addHeaders(['X-Api-Key' => self::API_KEY]);
        $transport->write(self::BODY);
        $error = null;
        $reply = '';
        try {
            $transport->flush();
            $reply = $transport->read(64);
        } catch (TTransportException $e) {
            $error = $e;
        }

        fwrite($control[0], "stop\n");
        $received = json_decode((string) stream_get_contents($control[0]), true);
        fclose($control[0]);
        \pcntl_waitpid($pid, $status);
        $this->assertIsArray($received, 'the listeners did not report');

        return [$received, $error, $reply];
    }

    /**
     * The child's side. Serves the listeners until the parent says stop, then
     * reports every connection each listener accepted, also the ones still
     * waiting to be accepted at that point.
     *
     * @param array<string, resource> $listeners
     * @param resource $control
     */
    private function serve(array $listeners, $control, int $code, string $location): void
    {
        \pcntl_signal(SIGPIPE, SIG_IGN);
        $received = array_fill_keys(array_keys($listeners), []);
        $deadline = time() + 30;
        while (time() < $deadline) {
            $read = array_values($listeners);
            $read[] = $control;
            $write = null;
            $except = null;
            if (stream_select($read, $write, $except, 1) < 1) {
                continue;
            }
            if (in_array($control, $read, true)) {
                break;
            }
            foreach ($listeners as $name => $listener) {
                if (in_array($listener, $read, true) && ($connection = @stream_socket_accept($listener, 1)) !== false) {
                    $received[$name][] = $this->answer($connection, $name, $name === 'A' && $received['A'] === [], $code, $location);
                }
            }
        }
        foreach ($listeners as $name => $listener) {
            while (($connection = @stream_socket_accept($listener, 0)) !== false) {
                $received[$name][] = ['line' => 'not served', 'apiKey' => null, 'body' => ''];
                fclose($connection);
            }
        }
        fwrite($control, (string) json_encode($received));
        fclose($control);
    }

    /**
     * Reads one request and answers it with the redirect or with a reply.
     *
     * @param resource $connection
     * @return array<string, ?string>
     */
    private function answer($connection, string $name, bool $redirect, int $code, string $location): array
    {
        stream_set_timeout($connection, 5);
        $line = rtrim((string) fgets($connection));
        $headers = [];
        while (($header = fgets($connection)) !== false && rtrim($header) !== '') {
            $parts = explode(':', $header, 2);
            $headers[strtolower(trim($parts[0]))] = trim($parts[1] ?? '');
        }
        // After a redirect curl may keep the Content-Length and send no body;
        // the read then ends when the client gives up and closes.
        $body = '';
        $length = (int) ($headers['content-length'] ?? 0);
        while (strlen($body) < $length && !feof($connection)) {
            $chunk = fread($connection, $length - strlen($body));
            if ($chunk === false || $chunk === '') {
                break;
            }
            $body .= $chunk;
        }

        if ($redirect) {
            $response = "HTTP/1.1 $code Redirect\r\nLocation: $location\r\n"
                . "Content-Length: 0\r\nConnection: close\r\n\r\n";
        } else {
            $reply = 'reply from ' . $name;
            $response = "HTTP/1.1 200 OK\r\nContent-Type: application/x-thrift\r\n"
                . 'Content-Length: ' . strlen($reply) . "\r\nConnection: close\r\n\r\n" . $reply;
        }
        @fwrite($connection, $response);
        fclose($connection);

        return ['line' => $line, 'apiKey' => $headers['x-api-key'] ?? null, 'body' => $body];
    }
}
