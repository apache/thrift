<?php

/**
 * Copyright (c) 2006- Facebook
 * Distributed under the Thrift Software License
 *
 * See accompanying file LICENSE or visit the Thrift site at:
 * http://developers.facebook.com/thrift/
 *
 * @package thrift
 * @author Aditya Agarwal <aditya@facebook.com>
 */

/**
 * Abstract Class providing null implementation for FacebookService
 * methods.
 */
class FacebookBase implements FacebookServiceIf {
  protected $name_ = '';

  public function __construct($name) {
    $this->name_ = $name;
  }

  public function getName() {
    return $this->name_;
  }

  public function getVersion() { 
    return ''; 
  }

  public function getStatus() { 
    return null; 
  } 
  
  public function getStatusDetails() { 
    return '';
  }
 
  public function getCounters() { 
    return array();
  } 

  public function getCounter($key) { 
    return null;
  } 

  public function setOption($key, $value) { 
    return;
  } 

  public function getOption($key) { 
    return ''; 
  } 

  public function getOptions() { 
    return array();
  } 

  public function aliveSince() { 
    return 0;
  } 

  public function getCpuProfile($duration) { 
    return ''; 
  }

  public function getLimitedReflection() { 
    return array();
  } 

  public function reinitialize() { 
    return;
  }

  public function shutdown() { 
    return;
  }

}

