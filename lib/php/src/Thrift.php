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
 * Data types that can be sent via Thrift
 */
class TType {
  const STOP   = 0;
  const VOID   = 1;
  const BOOL   = 2;
  const BYTE   = 3;
  const I08    = 3;
  const DOUBLE = 4;
  const I16    = 6;
  const I32    = 8;
  const I64    = 10;
  const STRING = 11;
  const UTF7   = 11;
  const STRUCT = 12;
  const MAP    = 13;
  const SET    = 14;
  const LST    = 15;    // N.B. cannot use LIST keyword in PHP!
  const UTF8   = 16;
  const UTF16  = 17;
}

/**
 * Message types for RPC
 */
class TMessageType {
  const CALL  = 1;
  const REPLY = 2;
  const EXCEPTION = 3;
}

class TException extends Exception {
  function __construct($message=null, $code=0) {
    parent::__construct($message, $code);
  }
}

class TApplicationException extends TException {
  
  const UNKNOWN = 0;
  const UNKNOWN_METHOD = 1;
  const INVALID_MESSAGE_TYPE = 2;
  const WRONG_METHOD_NAME = 3;
  const BAD_SEQUENCE_ID = 4;
  const MISSING_RESULT = 5;

  function __construct($message=null, $code=0) {
    parent::__construct($message, $code);
  }

  public function read($input) {
    $xfer = 0;
    $fname = null;
    $ftype = 0;
    $fid = 0;
    $xfer += $input->readStructBegin($fname);
    while (true)
    {
      $xfer += $input->readFieldBegin($fname, $ftype, $fid);
      if ($ftype == TType::STOP) {
        break;
      }
      switch ($fid)
      {
        case 1:
          if ($ftype == TType::STRING) {
            $xfer += $input->readString($this->message);
          } else {
            $xfer += $input->skip($ftype);
          }
          break;
        case 2:
          if ($ftype == TType::I32) {
            $xfer += $input->readI32($this->code);
          } else {
            $xfer += $input->skip($ftype);
          }
          break;
        default:
          $xfer += $input->skip($ftype);
          break;
      }
      $xfer += $input->readFieldEnd();
    }
    $xfer += $input->readStructEnd();
    return $xfer;
  }

  public function write($output) {
    $xfer = 0;
    $xfer += $output->writeStructBegin('TApplicationException');
    if ($this->getMessage()) {
      $xfer += $output->writeFieldBegin('message', TType::STRING, 1);
      $xfer += $output->writeString($this->getMessage());
      $xfer += $output->writeFieldEnd();
    }
    if ($this->getCode()) {
      $xfer += $output->writeFieldBegin('type', TType::I32, 2);
      $xfer += $output->writeI32($this->getCode());
      $xfer += $output->writeFieldEnd();
    }
    $xfer += $output->writeFieldStop();
    $xfer += $output->writeStructEnd();
    return $xfer;
  }
}

/**
 * Set global THRIFT ROOT automatically via inclusion here
 */
if (!isset($GLOBALS['THRIFT_ROOT'])) {
  $GLOBALS['THRIFT_ROOT'] = dirname(__FILE__);
}
include_once $GLOBALS['THRIFT_ROOT'].'/protocol/TProtocol.php';
include_once $GLOBALS['THRIFT_ROOT'].'/transport/TTransport.php';

?>
