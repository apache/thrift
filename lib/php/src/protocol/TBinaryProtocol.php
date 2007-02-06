<?php 

/** For transport operations */
include_once $GLOBALS['THRIFT_ROOT'].'/transport/TTransport.php';

/**
 * Binary implementation of the Thrift protocol.
 *
 * @package thrift.protocol
 * @author Mark Slee <mcslee@facebook.com>
 */
class TBinaryProtocol extends TProtocol {

  public function __construct($trans) {
    parent::__construct($trans);
  }

  public function writeMessageBegin($name, $type, $seqid) {
    return 
      $this->writeString($name) +
      $this->writeByte($type) +
      $this->writeI32($seqid);
  }

  public function writeMessageEnd() {
    return 0;
  }

  public function writeStructBegin($name) {
    return 0;
  }

  public function writeStructEnd() {
    return 0;
  }

  public function writeFieldBegin($fieldName, $fieldType, $fieldId) {
    return
      $this->writeByte($fieldType) +
      $this->writeI16($fieldId);
  }

  public function writeFieldEnd() {
    return 0;
  } 

  public function writeFieldStop() {
    return
      $this->writeByte(TType::STOP);
  }

  public function writeMapBegin($keyType, $valType, $size) {
    return
      $this->writeByte($keyType) +
      $this->writeByte($valType) +
      $this->writeI32($size);
  }

  public function writeMapEnd() {
    return 0;
  }

  public function writeListBegin($elemType, $size) {
    return
      $this->writeByte($elemType) +
      $this->writeI32($size);
  }

  public function writeListEnd() {
    return 0;
  }

  public function writeSetBegin($elemType, $size) {
    return
      $this->writeByte($elemType) +
      $this->writeI32($size);
  }

  public function writeSetEnd() {
    return 0;
  }

  public function writeBool($value) {
    $data = pack('c', $value ? 1 : 0);
    $this->trans_->write($data, 1);
    return 1;
  }

  public function writeByte($value) {
    $data = pack('c', $value);
    $this->trans_->write($data, 1);
    return 1;
  }

  public function writeI16($value) {
    $data = pack('n', $value);
    $this->trans_->write($data, 2);
    return 2;
  }

  public function writeI32($value) {
    $data = pack('N', $value);
    $this->trans_->write($data, 4);
    return 4;
  }

  public function writeI64($value) {
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

    $this->trans_->write($data, 8);
    return 8;
  }

  public function writeDouble($value) {
    $data = pack('d', $value);
    $this->trans_->write(strrev($data), 8);
    return 8;
  }

  public function writeString($value) {
    $len = strlen($value);
    $result = $this->writeI32($len);
    if ($len) {
      $this->trans_->write($value, $len);
    }
    return $result + $len;
  }

  public function readMessageBegin(&$name, &$type, &$seqid) {
    return 
      $this->readString($name) +
      $this->readByte($type) +
      $this->readI32($seqid);
  }

  public function readMessageEnd() {
    return 0;
  }

  public function readStructBegin(&$name) {
    $name = '';
    return 0;
  }

  public function readStructEnd() {
    return 0;
  }

  public function readFieldBegin(&$name, &$fieldType, &$fieldId) {
    $result = $this->readByte($fieldType);
    if ($fieldType == TType::STOP) {
      $fieldId = 0;
      return $result;
    }
    $result += $this->readI16($fieldId);
    return $result;
  }

  public function readFieldEnd() {
    return 0;
  }

  public function readMapBegin(&$keyType, &$valType, &$size) {
    return
      $this->readByte($keyType) +
      $this->readByte($valType) +
      $this->readI32($size);
  }

  public function readMapEnd() {
    return 0;
  }

  public function readListBegin(&$elemType, &$size) {
    return
      $this->readByte($elemType) +
      $this->readI32($size);
  }

  public function readListEnd() {
    return 0;
  }

  public function readSetBegin(&$elemType, &$size) {
    return
      $this->readByte($elemType) +
      $this->readI32($size);
  }

  public function readSetEnd() {
    return 0;
  }

  public function readBool(&$value) {
    $data = $this->trans_->readAll(1);
    $arr = unpack('c', $data);
    $value = $arr[1] == 1;
    return 1;
  }

  public function readByte(&$value) {
    $data = $this->trans_->readAll(1);
    $arr = unpack('c', $data);
    $value = $arr[1];
    return 1;
  }

  public function readI16(&$value) {
    $data = $this->trans_->readAll(2);
    $arr = unpack('n', $data);
    $value = $arr[1];
    if ($value > 0x7fff) {
      $value = 0 - (($value - 1) ^ 0xffff);
    }
    return 2;
  }

  public function readI32(&$value) {
    $data = $this->trans_->readAll(4);
    $arr = unpack('N', $data);
    $value = $arr[1];
    if ($value > 0x7fffffff) {
      $value = 0 - (($value - 1) ^ 0xffffffff);
    }
    return 4;
  }

  public function readI64(&$value) {
    $data = $this->trans_->readAll(8);

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

  public function readDouble(&$value) {
    $data = strrev($this->trans_->readAll(8));
    $arr = unpack('d', $data);
    $value = $arr[1];
    return 8;
  }

  public function readString(&$value) {
    $result = $this->readI32($len);
    if ($len) {
      $value = $this->trans_->readAll($len);
    } else {
      $value = '';
    }
    return $result + $len;
  }
}

/**
 * Binary Protocol Factory
 */
class TBinaryProtocolFactory implements TProtocolFactory {
  public function getProtocol($trans) {
    return new TBinaryProtocol($trans);
  }
}

?>
