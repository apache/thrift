<?php

/** For transport operations */
require_once THRIFT_ROOT.'/transport/TTransport.php';

/**
 * Binary implementation of the Thrift protocol.
 *
 * @package thrift.protocol
 * @author Mark Slee <mcslee@facebook.com>
 */
class TBinaryProtocol extends TProtocol {

  public function writeStructBegin($out, $name) {
    return 0;
  }

  public function writeStructEnd($out) {
    return 0;
  }

  public function writeFieldBegin($out, $fieldName, $fieldType, $fieldId) {
    return
      $this->writeByte($out, $fieldType) +
      $this->writeI32($out, $fieldId);
  }

  public function writeFieldEnd($out) {
    return 0;
  } 

  public function writeFieldStop($out) {
    return
      $this->writeByte($out, TType::STOP);
  }

  public function writeMapBegin($out, $keyType, $valType, $size) {
    return
      $this->writeByte($out, $keyType) +
      $this->writeByte($out, $valType) +
      $this->writeI32($out, $size);
  }

  public function writeMapEnd($out) {
    return 0;
  }

  public function writeListBegin($out, $elemType, $size) {
    return
      $this->writeByte($out, $elemType) +
      $this->writeI32($out, $size);
  }

  public function writeListEnd($out) {
    return 0;
  }

  public function writeSetBegin($out, $elemType, $size) {
    return
      $this->writeByte($out, $elemType) +
      $this->writeI32($out, $size);
  }

  public function writeSetEnd($out) {
    return 0;
  }

  public function writeByte($out, $byte) {
    $data = pack('c', $byte);
    $out->write($data, 1);
    return 1;
  }

  public function writeI32($out, $i32) {
    $data = pack('l', $i32);
  //if (!defined('BIG_ENDIAN')) {
      $data = strrev($data);
  //}
    $out->write($data, 4);
    return 4;
  }

  public function writeI64($out, $i64) {
    $hi = $i64 >> 32;
    $lo = $i64 & 0xFFFFFFFF;
    if (!defined('BIG_ENDIAN')) {
      $data = pack('N2', $hi, $lo);
    } else {
      $data = pack('N2', $lo, $hi);
    }
    $out->write($data, 8);
    return 8;
  }

  public function writeString($out, $str) {
    $len = strlen($str);
    $result = $this->writeI32($out, $len);
    $out->write($str, $len);
    return $result + $len;
  }

  public function readStructBegin($in, &$name) {
    $name = '';
    return 0;
  }

  public function readStructEnd($in) {
    return 0;
  }

  public function readFieldBegin($in, &$name, &$fieldType, &$fieldId) {
    $result = $this->readByte($in, $fieldType);
    if ($fieldType == TType::STOP) {
      $fieldId = 0;
      return $result;
    }
    $result += $this->readI32($in, $fieldId);
    return $result;
  }

  public function readFieldEnd($in) {
    return 0;
  }

  public function readMapBegin($in, &$keyType, &$valType, &$size) {
    $result = $this->readByte($in, $keyType);
    $result += $this->readByte($in, $valType);
    $result += $this->readI32($in, $size);
    return $result;
  }

  public function readMapEnd($in) {
    return 0;
  }

  public function readListBegin($in, &$elemType, &$size) {
    $result = $this->readByte($in, $elemType);
    $result += $this->readI32($in, $size);
    return $result;
  }

  public function readListEnd($in) {
    return 0;
  }

  public function readSetBegin($in, &$elemType, &$size) {
    $result = $this->readByte($in, $elemType);
    $result += $this->readI32($in, $size);
    return $result;
  }

  public function readSetEnd($in) {
    return 0;
  }

  public function readByte($in, &$byte) {
    $data = $in->readAll(1);
    $arr = unpack('c', $data);
    $byte = $arr[1];
    return 1;
  }

  public function readI32($in, &$i32) {
    $data = $in->readAll(4);
    if (!defined('BIG_ENDIAN')) {
      $data = strrev($data);
    }
    $arr = unpack('l', $data);
    $i32 = $arr[1];
    return 4;
  }

  public function readI64($in, &$i64) {
    $data = $in->readAll(8);
    $arr = unpack('N2', $data);

    // Check for a negative
    if ($arr[1] & 0x80000000) {
      $arr[1] = $arr[1] ^ 0xFFFFFFFF;
      $arr[2] = $arr[2] ^ 0xFFFFFFFF;
      $i64 = 0 - $arr[1]*4294967296 - $arr[2] - 1;
    } else {
      $i64 = $arr[1]*4294967296 + $arr[2];
    }
    return 8;
  }

  public function readString($in, &$str) {
    $result = $this->readI32($in, $len);
    $str = $in->readAll($len);
    return $result + $len;
  }
}

?>