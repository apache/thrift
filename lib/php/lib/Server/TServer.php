<?php

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
    /**
     * Sets up all the factories, etc
     *
     * @param TProcessor $processor
     */
    public function __construct(
        protected $processor,
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
     *
     * @abstract
     * @return void
     */
    abstract public function serve();

    /**
     * Stops the server serving
     *
     * @abstract
     * @return void
     */
    abstract public function stop();
}
