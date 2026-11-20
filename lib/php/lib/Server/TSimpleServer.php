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

use Thrift\Exception\TTransportException;

/**
 * Simple implemtation of a Thrift server.
 *
 * @package thrift.server
 */
class TSimpleServer extends TServer
{
    /**
     * Flag for the main serving loop
     */
    private bool $stop = false;

    /**
     * Listens for new client using the supplied
     * transport and serves one client at a time
     * until stop() is called. It handles
     * TTransportExceptions from accept(), so that
     * timeouts etc do not end it. Whatever ends a
     * client's connection - the client closing it,
     * a transport error or a request that cannot
     * be read - ends only that connection, which
     * is then closed.
     */
    public function serve(): void
    {
        $this->transport->listen();

        while (!$this->stop) {
            try {
                $transport = $this->transport->accept();
            } catch (TTransportException $e) {
                continue;
            }

            try {
                try {
                    $inputTransport = $this->inputTransportFactory->getTransport($transport);
                    $outputTransport = $this->outputTransportFactory->getTransport($transport);
                    $inputProtocol = $this->inputProtocolFactory->getProtocol($inputTransport);
                    $outputProtocol = $this->outputProtocolFactory->getProtocol($outputTransport);
                    while ($this->processor->process($inputProtocol, $outputProtocol)) {
                    }
                } finally {
                    $transport->close();
                }
            } catch (\Throwable $e) {
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
