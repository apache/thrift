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

use Thrift\Factory\TProtocolFactory;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Type\TMessageType;

/**
 * Cross-test HTTP request handler for PHP's built-in web server (`php -S`).
 *
 * Reads php://input, peeks the Thrift message type, and for ONEWAY sends an
 * empty HTTP 200 immediately while running the handler in a forked child so
 * the client's one-way call returns without waiting for handler execution.
 *
 * Cli-server specific: closing STDIN/STDOUT/STDERR in the child relies on
 * php -S worker semantics and does not translate to php-fpm or mod_php.
 */
class HttpServer
{
    public function __construct(
        private object $processor,
        private TProtocolFactory $protocolFactory,
    ) {
    }

    public function serve(): void
    {
        $requestBody = (string) file_get_contents('php://input');

        header('Content-Type: application/x-thrift');

        if ($this->peekMessageType($requestBody) === TMessageType::ONEWAY
            && function_exists('pcntl_fork')
            && $this->dispatchOnewayAsync($requestBody)
        ) {
            return;
        }

        $this->dispatchSync($requestBody);
    }

    private function peekMessageType(string $body): ?int
    {
        if ($body === '') {
            return null;
        }
        try {
            $type = null;
            $name = null;
            $seqid = null;
            $this->protocolFactory
                ->getProtocol(new TMemoryBuffer($body))
                ->readMessageBegin($name, $type, $seqid);

            return $type;
        } catch (\Throwable) {
            return null;
        }
    }

    private function dispatchOnewayAsync(string $body): bool
    {
        header('Content-Length: 0');
        $pid = pcntl_fork();
        if ($pid > 0) {
            return true;
        }
        if ($pid === 0) {
            if (defined('STDIN')) {
                fclose(STDIN);
            }
            if (defined('STDOUT')) {
                fclose(STDOUT);
            }
            if (defined('STDERR')) {
                fclose(STDERR);
            }
            $this->processor->process(
                $this->protocolFactory->getProtocol(new TMemoryBuffer($body)),
                $this->protocolFactory->getProtocol(new TMemoryBuffer()),
            );
            exit(0);
        }

        return false;
    }

    private function dispatchSync(string $body): void
    {
        $output = new TMemoryBuffer();
        $this->processor->process(
            $this->protocolFactory->getProtocol(new TMemoryBuffer($body)),
            $this->protocolFactory->getProtocol($output),
        );
        echo $output->getBuffer();
    }
}
