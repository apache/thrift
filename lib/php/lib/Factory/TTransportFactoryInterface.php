<?php

namespace Thrift\Factory;

use Thrift\Transport\TTransport;

interface TTransportFactoryInterface
{
    /**
     * @static
     * @param TTransport $transport
     * @return TTransport
     */
    public static function getTransport(TTransport $transport);
}
