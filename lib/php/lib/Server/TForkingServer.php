<?php

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
     *
     * @return void
     */
    public function serve()
    {
        $this->transport->listen();

        while (!$this->stop) {
            try {
                $transport = $this->transport->accept();

                if ($transport != null) {
                    $pid = pcntl_fork();

                    if ($pid > 0) {
                        $this->handleParent($transport, $pid);
                    } elseif ($pid === 0) {
                        $this->handleChild($transport);
                    } else {
                        throw new TException('Failed to fork');
                    }
                }
            } catch (TTransportException $e) {
            }

            $this->collectChildren();
        }
    }

    /**
     * Code run by the parent
     *
     * @param TTransport $transport
     * @param int $pid
     * @return void
     */
    private function handleParent(TTransport $transport, $pid)
    {
        $this->children[$pid] = $transport;
    }

    /**
     * Code run by the child.
     *
     * @param TTransport $transport
     * @return void
     */
    private function handleChild(TTransport $transport)
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

    /**
     * Collects any children we may have
     *
     * @return void
     */
    private function collectChildren()
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
     *
     * @return void
     */
    public function stop()
    {
        $this->transport->close();
        $this->stop = true;
    }
}
