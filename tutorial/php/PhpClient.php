#!/usr/bin/env php
<?php

$GLOBALS['THRIFT_ROOT'] = '../../lib/php/src';

require_once $GLOBALS['THRIFT_ROOT'].'/Thrift.php';
require_once $GLOBALS['THRIFT_ROOT'].'/protocol/TBinaryProtocol.php';
require_once $GLOBALS['THRIFT_ROOT'].'/transport/TSocket.php';
require_once $GLOBALS['THRIFT_ROOT'].'/transport/THttpClient.php';
require_once $GLOBALS['THRIFT_ROOT'].'/transport/TBufferedTransport.php';

/**
 * Suppress errors in here, which happen because we have not installed into
 * $GLOBALS['THRIFT_ROOT'].'/packages/tutorial' like we are supposed to!
 *
 * Normally we would only have to include Calculator.php which would properly
 * include the other files from their packages/ folder locations, but we
 * include everything here due to the bogus path setup.
 */
error_reporting(E_NONE);
$GEN_DIR = '../gen-php';
require_once $GEN_DIR.'/SharedService.php';
require_once $GEN_DIR.'/shared_types.php';
require_once $GEN_DIR.'/Calculator.php';
require_once $GEN_DIR.'/tutorial_types.php';
error_reporting(E_ALL);

try {
  if (array_search('--http', $argv)) {
    $socket = new THttpClient('localhost', 8080, '/php/PhpServer.php');
  } else {
    $socket = new TSocket('localhost', 9090);
  }
  $transport = new TBufferedTransport($socket, 1024, 1024);
  $protocol = new TBinaryProtocol($transport);
  $client = new CalculatorClient($protocol);

  $transport->open();

  $client->ping();
  print "ping()\n";

  $sum = $client->add(1,1);
  print "1+1=$sum\n";

  $work = new tutorial_Work();

  $work->op = tutorial_Operation::DIVIDE;
  $work->num1 = 1;
  $work->num2 = 0;

  try {
    $client->calculate(1, $work);
    print "Whoa! We can divide by zero?\n";
  } catch (tutorial_InvalidOperation $io) {
    print "InvalidOperation: $io->why\n";
  }

  $work->op = tutorial_Operation::SUBTRACT;
  $work->num1 = 15;
  $work->num2 = 10;
  $diff = $client->calculate(1, $work);
  print "15-10=$diff\n";

  $log = $client->getStruct(1);
  print "Log: $log->value\n";

  $transport->close();

} catch (TException $tx) {
  print 'TException: '.$tx->getMessage()."\n";
}

?>
