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

namespace Thrift\Server;

use Thrift\Transport\TTransport;
use Thrift\Exception\TException;
use Thrift\Exception\TTransportException;

/**
 * A forking implementation of a Thrift server.
 *
 * @package thrift.server
 */
class TForkingServer extends TServer
{
    /**
     * Flag for the main serving loop
     */
    private bool $stop = false;

    /**
     * List of children.
     *
     * @var array<int, TTransport>
     */
    protected array $children = [];

    /**
     * Listens for new client using the supplied
     * transport. We fork when a new connection
     * arrives.
     */
    public function serve(): void
    {
        $this->transport->listen();

        while (!$this->stop) {
            try {
                $transport = $this->transport->accept();
                $pid = pcntl_fork();

                if ($pid > 0) {
                    $this->handleParent($transport, $pid);
                } elseif ($pid === 0) {
                    $this->handleChild($transport);
                } else {
                    throw new TException('Failed to fork');
                }
            } catch (TTransportException $e) {
            }

            $this->collectChildren();
        }
    }

    /**
     * Code run by the parent
     */
    private function handleParent(TTransport $transport, int $pid): void
    {
        $this->children[$pid] = $transport;
    }

    /**
     * Code run by the child. The child ends with its
     * client's connection, however that ends, and
     * never returns to the accept loop.
     */
    private function handleChild(TTransport $transport): void
    {
        try {
            $inputTransport = $this->inputTransportFactory->getTransport($transport);
            $outputTransport = $this->outputTransportFactory->getTransport($transport);
            $inputProtocol = $this->inputProtocolFactory->getProtocol($inputTransport);
            $outputProtocol = $this->outputProtocolFactory->getProtocol($outputTransport);
            while ($this->processor->process($inputProtocol, $outputProtocol)) {
            }
            @$transport->close();
        } catch (\Throwable $e) {
        }

        exit(0);
    }

    private function collectChildren(): void
    {
        $status = null;
        foreach ($this->children as $pid => $transport) {
            if (pcntl_waitpid($pid, $status, WNOHANG) > 0) {
                unset($this->children[$pid]);
                if ($transport) {
                    @$transport->close();
                }
            }
        }
    }

    /**
     * Stops the server running. Kills the transport
     * and then stops the main serving loop
     */
    public function stop(): void
    {
        $this->transport->close();
        $this->stop = true;
    }
}
