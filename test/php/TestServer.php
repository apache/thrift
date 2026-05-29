<?php

error_reporting(E_ALL);

require_once __DIR__ . '/../../vendor/autoload.php';
require_once __DIR__ . '/protocols.php';

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
      --protocol=arg (binary)      protocol: binary, compact, json, accel
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

$port = (int) ($opts['port'] ?? 9090);
$transport = $opts['transport'] ?? 'buffered';
$protocol = $opts['protocol'] ?? 'binary';

// HTTP transport: delegate to HttpRouter (its CLI-launcher branch execs into
// `php -S` with HttpRouter itself as the per-request handler).
if ($transport === 'http') {
    require __DIR__ . '/HttpRouter.php';
    return;
}

$loader = new \Thrift\ClassLoader\ThriftClassLoader();
$loader->registerDefinition('ThriftTest', __DIR__ . '/gen-php-classmap');
$loader->register();

require_once __DIR__ . '/Handler.php';

$serverTransportFactory = match ($transport) {
    'framed' => new \Thrift\Factory\TFramedTransportFactory(),
    default => new \Thrift\Factory\TTransportFactory(),
};

$protocolFactory = thrift_test_protocol_factory($protocol);

// `localhost` may resolve to an IPv6-only listener in newer PHP/runtime combinations,
// while some cross-test clients still connect via 127.0.0.1. Bind explicitly to IPv4.
$serverTransport = new \Thrift\Server\TServerSocket('127.0.0.1', $port);
$processor = new \ThriftTest\ThriftTestProcessor(new \Handler());

$server = new \Thrift\Server\TSimpleServer(
    $processor,
    $serverTransport,
    $serverTransportFactory,
    $serverTransportFactory,
    $protocolFactory,
    $protocolFactory,
);

echo "Starting the Test server...\n";
$server->serve();
