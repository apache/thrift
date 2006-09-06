<?php

/** For transport operations */
require_once $GLOBALS['THRIFT_ROOT'].'/transport/TTransport.php';

/**
 * Binary implementation of the Thrift protocol.
 *
 * @package thrift.protocol
 * @author Mark Slee <mcslee@facebook.com>
 */
class TBinaryProtocol extends TProtocol {

  public function writeMessageBegin($out, $name, $type, $seqid) {
    return 
      $this->writeString($out, $name) +
      $this->writeByte($out, $type) +
      $this->writeI32($out, $seqid);
  }

  public function writeMessageEnd($out) {
    return 0;
  }

  public function writeStructBegin($out, $name) {
    return 0;
  }

  public function writeStructEnd($out) {
    return 0;
  }

  public function writeFieldBegin($out, $fieldName, $fieldType, $fieldId) {
    return
      $this->writeByte($out, $fieldType) +
      $this->writeI16($out, $fieldId);
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

  public function writeBool($out, $value) {
    $data = pack('c', $value ? 1 : 0);
    $out->write($data, 1);
    return 1;
  }

  public function writeByte($out, $value) {
    $data = pack('c', $value);
    $out->write($data, 1);
    return 1;
  }

  public function writeI16($out, $value) {
    $data = pack('n', $value);
    $out->write($data, 2);
    return 2;
  }

  public function writeI32($out, $value) {
    $data = pack('N', $value);
    $out->write($data, 4);
    return 4;
  }

  public function writeI64($out, $value) {
    // If we are on a 32bit architecture we have to explicitly deal with
    // 64-bit twos-complement arithmetic since PHP wants to treat all ints
    // as signed and any int over 2^31 - 1 as a float   
    if (PHP_INT_SIZE == 4) {
      $neg = $value < 0;

      if ($neg) {
	$value *= -1;
      }
   
      $hi = (int)($value / 4294967296);
      $lo = (int)$value;
    
      if ($neg) {
	$hi = ~$hi;
	$lo = ~$lo;
	if (($lo & (int)0xffffffff) == (int)0xffffffff) {
	  $lo = 0;
	  $hi++;
	} else {
	  $lo++;
	}
      }
      $data = pack('N2', $hi, $lo);
    
    } else {
      $hi = $value >> 32;
      $lo = $value & 0xFFFFFFFF;
      $data = pack('N2', $hi, $lo);
    }

    $out->write($data, 8);
    return 8;
  }

  public function writeDouble($out, $value) {
    $data = pack('d', $value);
    $out->write(strrev($data), 8);
    return 8;
  }

  public function writeString($out, $value) {
    $len = strlen($value);
    $result = $this->writeI32($out, $len);
    $out->write($value, $len);
    return $result + $len;
  }

  public function readMessageBegin($in, &$name, &$type, &$seqid) {
    return 
      $this->readString($in, $name) +
      $this->readByte($in, $type) +
      $this->readI32($in, $seqid);
  }

  public function readMessageEnd($out) {
    return 0;
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
    $result += $this->readI16($in, $fieldId);
    return $result;
  }

  public function readFieldEnd($in) {
    return 0;
  }

  public function readMapBegin($in, &$keyType, &$valType, &$size) {
    return
      $this->readByte($in, $keyType) +
      $this->readByte($in, $valType) +
      $this->readI32($in, $size);
  }

  public function readMapEnd($in) {
    return 0;
  }

  public function readListBegin($in, &$elemType, &$size) {
    return
      $this->readByte($in, $elemType) +
      $this->readI32($in, $size);
  }

  public function readListEnd($in) {
    return 0;
  }

  public function readSetBegin($in, &$elemType, &$size) {
    return
      $this->readByte($in, $elemType) +
      $this->readI32($in, $size);
  }

  public function readSetEnd($in) {
    return 0;
  }

  public function readBool($in, &$value) {
    $data = $in->readAll(1);
    $arr = unpack('c', $data);
    $value = $arr[1] == 1;
    return 1;
  }

  public function readByte($in, &$value) {
    $data = $in->readAll(1);
    $arr = unpack('c', $data);
    $value = $arr[1];
    return 1;
  }

  public function readI16($in, &$value) {
    $data = $in->readAll(2);
    $arr = unpack('n', $data);
    $value = $arr[1];
    if ($value > 0x7fff) {
      $value = 0 - (($value - 1) ^ 0xffff);
    }
    return 2;
  }

  public function readI32($in, &$value) {
    $data = $in->readAll(4);
    $arr = unpack('N', $data);
    $value = $arr[1];
    if ($value > 0x7fffffff) {
      $value = 0 - (($value - 1) ^ 0xffffffff);
    }
    return 4;
  }

  public function readI64($in, &$value) {
    $data = $in->readAll(8);

    $arr = unpack('N2', $data);
    
    // If we are on a 32bit architecture we have to explicitly deal with
    // 64-bit twos-complement arithmetic since PHP wants to treat all ints
    // as signed and any int over 2^31 - 1 as a float
    if (PHP_INT_SIZE == 4) {

      $hi = $arr[1];
      $lo = $arr[2];
      $isNeg = $hi  < 0;
    
      // Check for a negative
      if ($isNeg) {
	$hi = ~$hi & (int)0xffffffff;
	$lo = ~$lo & (int)0xffffffff;

	if ($lo == (int)0xffffffff) {
	  $hi++;
	  $lo = 0;
	} else {
	  $lo++;
	}
      }

      // Force 32bit words in excess of 2G to pe positive - we deal wigh sign
      // explicitly below
      
      if ($hi & (int)0x80000000) {
	$hi &= (int)0x7fffffff;
	$hi += 0x80000000;
      }
      
      if ($lo & (int)0x80000000) {
	$lo &= (int)0x7fffffff;
	$lo += 0x80000000;
      }
    
      $value = $hi * 4294967296 + $lo;

      if ($isNeg) {
	$value = 0 - $value;
      }
    } else {

      // Check for a negative
      if ($arr[1] & 0x80000000) {
	$arr[1] = $arr[1] ^ 0xFFFFFFFF;
	$arr[2] = $arr[2] ^ 0xFFFFFFFF;
	$value = 0 - $arr[1]*4294967296 - $arr[2] - 1;
      } else {
	$value = $arr[1]*4294967296 + $arr[2];
      }
    }
    
    return 8;
  }

  public function readDouble($in, &$value) {
    $data = strrev($in->readAll(8));
    $arr = unpack('d', $data);
    $value = $arr[1];
    return 8;
  }

  public function readString($in, &$value) {
    $result = $this->readI32($in, $len);
    $value = $in->readAll($len);
    return $result + $len;
  }
}

?>
