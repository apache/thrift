<?php

declare(strict_types=1);

namespace Thrift\Server;

use Thrift\Factory\TTransportFactoryInterface;
use Thrift\Factory\TProtocolFactory;

/**
 * Generic class for a Thrift server.
 *
 * @package thrift.server
 */
abstract class TServer
{
    public function __construct(
        protected object $processor,
        protected TServerTransport $transport,
        protected TTransportFactoryInterface $inputTransportFactory,
        protected TTransportFactoryInterface $outputTransportFactory,
        protected TProtocolFactory $inputProtocolFactory,
        protected TProtocolFactory $outputProtocolFactory,
    ) {
    }

    /**
     * Serves the server. This should never return
     * unless a problem permits it to do so or it
     * is interrupted intentionally
     */
    abstract public function serve(): void;

    abstract public function stop(): void;
}
