<?php

declare(strict_types=1);

namespace Thrift\Server;

use Thrift\Exception\TTransportException;
use Thrift\Transport\TTransport;

/**
 * Generic class for Server agent.
 *
 * @package thrift.transport
 */
abstract class TServerTransport
{
    abstract public function listen(): void;

    abstract public function close(): void;

    /**
     * Subclasses implement accept here.
     */
    abstract protected function acceptImpl(): ?TTransport;

    /**
     * @throws TTransportException when no transport is available
     */
    public function accept(): TTransport
    {
        $transport = $this->acceptImpl();

        if ($transport === null) {
            throw new TTransportException("accept() may not return NULL");
        }

        return $transport;
    }
}
