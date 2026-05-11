<?php

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
     * Code run by the child.
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
        } catch (TTransportException $e) {
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
