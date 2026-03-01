<?php

error_reporting(E_ALL);

require_once __DIR__ . '/../../vendor/autoload.php';

$opts = getopt(
    'h::',
    [
        'port::',
        'domain-socket::',
        'pipe::',
        'server-type::',
        'transport::',
        'protocol::',
        'multiplex::',
        'abstract-namespace::',
        'ssl::',
        'zlib::',
        'processor-events::',
        'workers::',
    ]
);
if (isset($opts['h'])) {
    echo <<<HELP
      -h | --help                  produce help message
      --port=arg (9090)            Port number to listen
      --domain-socket=arg          Unix Domain Socket (e.g. /tmp/ThriftTest.thrift)
      --pipe=arg                   Windows Named Pipe (e.g. MyThriftPipe)
      --server-type=arg (simple)   type of server, "simple", "thread-pool",
                                   "threaded", or "nonblocking"
      --transport=arg (buffered)   transport: buffered, framed, http, anonpipe, zlib
      --protocol=arg (binary)      protocol: binary, compact, header, json
      --multiplex                  Add TMultiplexedProtocol service name "ThriftTest"
      --abstract-namespace         Create the domain socket in the Abstract Namespace
                                   (no connection with filesystem pathnames)
      --ssl                        Encrypted Transport using SSL
      --zlib                       Wrapped Transport using Zlib
      --processor-events           processor-events
      -n=arg | --workers=arg (=4)  Number of thread pools workers. Only valid for
                                   thread-pool server type
HELP;
    exit(0);
}

$port = $opts['port'] ?? 9090;
$transport = $opts['transport'] ?? 'buffered';


$loader = new Thrift\ClassLoader\ThriftClassLoader();
$loader->registerDefinition('ThriftTest', __DIR__ . '/../../lib/php/test/Resources/packages/phpcm');
$loader->register();

$sslOptions = \stream_context_create(
    [
        'ssl' => [
            'verify_peer' => false,
            'verify_peer_name' => false,
        ],
    ]
);

require_once __DIR__ . '/Handler.php';

switch ($transport) {
    case 'framed':
        $serverTransportFactory = new \Thrift\Factory\TFramedTransportFactory();
        break;
    default:
        $serverTransportFactory = new \Thrift\Factory\TTransportFactory();
}

$serverTransport = new \Thrift\Server\TServerSocket('localhost', $port);
$handler = new Handler();
$processor = new ThriftTest\ThriftTestProcessor($handler);

$server = new \Thrift\Server\TSimpleServer(
    $processor,
    $serverTransport,
    $serverTransportFactory,
    $serverTransportFactory,
    new \Thrift\Factory\TBinaryProtocolFactory(),
    new \Thrift\Factory\TBinaryProtocolFactory()
);

echo "Starting the Test server...\n";
$server->serve();
