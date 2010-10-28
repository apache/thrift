#!/usr/bin/env php
<?php
/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

/*
 * This is not a stand-alone server.  It should be run as a normal
 * php web script (like through Apache's mod_php) or as a cgi script
 * (like with the included runserver.py).  You can connect to it with
 * THttpClient in any language that supports it.  The PHP tutorial client
 * will work if you pass it the argument "--http".
 */

if (php_sapi_name() == 'cli') {
  ini_set("display_errors", "stderr");
}

$GLOBALS['THRIFT_ROOT'] = realpath(dirname(__FILE__).'/../..').'/lib/php/src';

require_once $GLOBALS['THRIFT_ROOT'].'/Thrift.php';
require_once $GLOBALS['THRIFT_ROOT'].'/protocol/TBinaryProtocol.php';
require_once $GLOBALS['THRIFT_ROOT'].'/transport/TPhpStream.php';
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
$GEN_DIR = realpath(dirname(__FILE__).'/..').'/gen-php';
require_once $GEN_DIR.'/SharedService.php';
require_once $GEN_DIR.'/shared_types.php';
require_once $GEN_DIR.'/Calculator.php';
require_once $GEN_DIR.'/tutorial_types.php';
error_reporting(E_ALL);

class CalculatorHandler implements CalculatorIf {
  protected $log = array();

  public function ping() {
    error_log("ping()");
  }

  public function add($num1, $num2) {
    error_log("add({$num1}, {$num2})");
    return $num1 + $num2;
  }

  public function calculate($logid, $w) {
    error_log("calculate({$logid}, {{$w->op}, {$w->num1}, {$w->num2}})");
    switch ($w->op) {
      case tutorial_Operation::ADD:
        $val = $w->num1 + $w->num2;
        break;
      case tutorial_Operation::SUBTRACT:
        $val = $w->num1 - $w->num2;
        break;
      case tutorial_Operation::MULTIPLY:
        $val = $w->num1 * $w->num2;
        break;
      case tutorial_Operation::DIVIDE:
        if ($w->num2 == 0) {
          $io = new tutorial_InvalidOperation();
          $io->what = $w->op;
          $io->why = "Cannot divide by 0";
          throw $io;
        }
        $val = $w->num1 / $w->num2;
        break;
      default:
        $io = new tutorial_InvalidOperation();
        $io->what = $w->op;
        $io->why = "Invalid Operation";
        throw $io;
    }

    $log = new SharedStruct();
    $log->key = $logid;
    $log->value = (string)$val;
    $this->log[$logid] = $log;

    return $val;
  }

  public function getStruct($key) {
    error_log("getStruct({$key})");
    // This actually doesn't work because the PHP interpreter is
    // restarted for every request.
    //return $this->log[$key];
    return new SharedStruct(array("key" => $key, "value" => "PHP is stateless!"));
  }

  public function zip() {
    error_log("zip()");
  }

};

header('Content-Type', 'application/x-thrift');
if (php_sapi_name() == 'cli') {
  echo "\r\n";
}

$handler = new CalculatorHandler();
$processor = new CalculatorProcessor($handler);

$transport = new TBufferedTransport(new TPhpStream(TPhpStream::MODE_R | TPhpStream::MODE_W));
$protocol = new TBinaryProtocol($transport, true, true);

$transport->open();
$processor->process($protocol, $protocol);
$transport->close();
