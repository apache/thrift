<?php

/** Set global THRIFT ROOT automatically via inclusion here */
if (!isset($GLOBALS['THRIFT_ROOT'])) {
  $GLOBALS['THRIFT_ROOT'] = dirname(__FILE__);
}
include_once $GLOBALS['THRIFT_ROOT'].'/protocol/TProtocol.php';

?>
