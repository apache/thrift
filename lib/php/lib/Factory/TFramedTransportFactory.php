<?php

namespace Thrift\Factory;

use Thrift\Transport\TFramedTransport;
use Thrift\Transport\TTransport;

class TFramedTransportFactory implements TTransportFactoryInterface
{
    public static function getTransport(TTransport $transport)
    {
        return new TFramedTransport($transport);
    }
}
