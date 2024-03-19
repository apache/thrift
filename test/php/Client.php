<?php

use Thrift\Protocol\TCompactProtocol;
use Thrift\Transport\THttpClient;

error_reporting(E_ALL);

require_once __DIR__ . '/../../vendor/autoload.php';

$loader = new Thrift\ClassLoader\ThriftClassLoader();
$loader->registerDefinition('ThriftTest', __DIR__ . '/../../lib/php/test/Resources/packages/phpcm');
$loader->register();


$transport = new THttpClient('localhost', 80);

$transport->setTimeoutSecs($this->timeoutSec);

$transport->addHeaders($this->generateAuthHeader());

$protocol = new TCompactProtocol($transport);

$transport->open();

$client = new \ThriftTest\ThriftTestClient($protocol);
$client->testVoid();
