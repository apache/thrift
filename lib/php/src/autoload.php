<?php

/**
 * Copyright (c) 2006- Facebook
 * Distributed under the Thrift Software License
 *
 * See accompanying file LICENSE or visit the Thrift site at:
 * http://developers.facebook.com/thrift/
 *
 * @package thrift
 * @author Mark Slee <mcslee@facebook.com>
 */

/**
 * Include this file if you wish to use autoload with your PHP generated Thrift
 * code. The generated code will *not* include any defined Thrift classes by
 * default, except for the service interfaces. The generated code will populate
 * values into $GLOBALS['THRIFT_AUTOLOAD'] which can be used by the autoload
 * method below. If you have your own autoload system already in place, rename your
 * __autoload function to something else and then do:
 * $GLOBALS['AUTOLOAD_HOOKS'][] = 'my_autoload_func';
 *
 * Generate this code using the --gen php:autoload Thrift generator flag.
 */

$GLOBALS['THRIFT_AUTOLOAD'] = array();
$GLOBALS['AUTOLOAD_HOOKS'] = array();

if (!function_exists('__autoload')) {
  function __autoload($class) {
    global $THRIFT_AUTOLOAD;
    $classl = strtolower($class);
    if (isset($THRIFT_AUTOLOAD[$classl])) {
      include_once $GLOBALS['THRIFT_ROOT'].'/packages/'.$THRIFT_AUTOLOAD[$classl];
    } else if (!empty($GLOBALS['AUTOLOAD_HOOKS'])) {
      foreach ($GLOBALS['AUTOLOAD_HOOKS'] as $hook) {
        $hook($class);
      }
    }
  }
}
