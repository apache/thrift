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
     * Processor to handle new clients
     *
     * @var TProcessor
     */
    protected $processor;

    /**
     * Server transport to be used for listening
     * and accepting new clients
     *
     * @var TServerTransport
     */
    protected $transport;

    /**
     * Input transport factory
     *
     * @var TTransportFactoryInterface
     */
    protected $inputTransportFactory;

    /**
     * Output transport factory
     *
     * @var TTransportFactoryInterface
     */
    protected $outputTransportFactory;

    /**
     * Input protocol factory
     *
     * @var TProtocolFactory
     */
    protected $inputProtocolFactory;

    /**
     * Output protocol factory
     *
     * @var TProtocolFactory
     */
    protected $outputProtocolFactory;

    /**
     * Sets up all the factories, etc
     *
     * @param object $processor
     * @param TServerTransport $transport
     * @param TTransportFactoryInterface $inputTransportFactory
     * @param TTransportFactoryInterface $outputTransportFactory
     * @param TProtocolFactory $inputProtocolFactory
     * @param TProtocolFactory $outputProtocolFactory
     * @return void
     */
    public function __construct(
        $processor,
        TServerTransport $transport,
        TTransportFactoryInterface $inputTransportFactory,
        TTransportFactoryInterface $outputTransportFactory,
        TProtocolFactory $inputProtocolFactory,
        TProtocolFactory $outputProtocolFactory
    ) {
        $this->processor = $processor;
        $this->transport = $transport;
        $this->inputTransportFactory = $inputTransportFactory;
        $this->outputTransportFactory = $outputTransportFactory;
        $this->inputProtocolFactory = $inputProtocolFactory;
        $this->outputProtocolFactory = $outputProtocolFactory;
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
