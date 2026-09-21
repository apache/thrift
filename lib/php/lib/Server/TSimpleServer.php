<?php

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
