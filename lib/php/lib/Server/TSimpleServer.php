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
     * transport. It handles TTransportExceptions
     * to avoid timeouts etc killing it
     */
    public function serve(): void
    {
        $this->transport->listen();

        while (!$this->stop) {
            try {
                $transport = $this->transport->accept();
                $inputTransport = $this->inputTransportFactory->getTransport($transport);
                $outputTransport = $this->outputTransportFactory->getTransport($transport);
                $inputProtocol = $this->inputProtocolFactory->getProtocol($inputTransport);
                $outputProtocol = $this->outputProtocolFactory->getProtocol($outputTransport);
                while ($this->processor->process($inputProtocol, $outputProtocol)) {
                }
            } catch (TTransportException $e) {
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
