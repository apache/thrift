<?php

/** Types */
require_once THRIFT_ROOT.'/protocol/TType.php';

/**
 * Protocol module.
 *
 * @package thrift.protocol
 * @author Mark Slee <mcslee@facebook.com>
 */
abstract class TProtocol {

  /**
   * Writes a struct header.
   *
   * @param TTransport $out  Output transport
   * @param string     $name Struct name
   * @throws TException on write error
   * @return int How many bytes written
   */
  public abstract function writeStructBegin($out, $name);


  /**
   * Close a struct.
   *
   * @param TTransport $out Output transport
   * @throws TException on write error
   * @return int How many bytes written
   */
  public abstract function writeStructEnd($out);

  /*
   * Starts a field.
   *
   * @param TTransport $out  Output transport
   * @param string     $name Field name
   * @param int        $type Field type
   * @param int        $fid  Field id
   * @throws TException on write error
   * @return int How many bytes written
   */
  public abstract function writeFieldBegin($out, $fieldName, $fieldType, $fieldId);

  public abstract function writeFieldEnd($out);

  public abstract function writeFieldStop($out);

  public abstract function writeMapBegin($out, $keyType, $valType, $size);

  public abstract function writeMapEnd($out);
  
  public abstract function writeListBegin($out, $elemType, $size);
  
  public abstract function writeListEnd($out);

  public abstract function writeSetBegin($out, $elemType, $size);

  public abstract function writeSetEnd($out);
  
  public abstract function writeByte($out, $byte);
  
  public abstract function writeI32($out, $i32);

  public abstract function writeI64($out, $i64);

  public abstract function writeString($out, $str);


  public abstract function readStructBegin($in, &$name);
  
  public abstract function readStructEnd($in);

  public abstract function readFieldBegin($in, &$name, &$fieldType, &$fieldId);

  public abstract function readFieldEnd($in);

  public abstract function readMapBegin($in, &$keyType, &$valType, &$size);

  public abstract function readMapEnd($in);

  public abstract function readListBegin($in, &$elemType, &$size);
  
  public abstract function readListEnd($in);

  public abstract function readSetBegin($in, &$elemType, &$size);
  
  public abstract function readSetEnd($in);

  public abstract function readByte($in, &$byte);
  
  public abstract function readI32($in, &$i32);

  public abstract function readI64($in, &$i64);

  public abstract function readString($in, &$str);

  public function skip($in, $type) {
    switch ($type) {
    case TType::BYTE:
      return $this->readByte($in, $byte);
    case TType::I32:
      return $this->readI32($in, $i32);
    case TType::I64:
      return $this->readI64($in, $i64);
    case TType::STRING:
      return $this->readString($in, $str);
    case TType::STRUCT:
      {
        $result = $this->readStructBegin($in, $name);
        while (true) {
          $result += $this->readFieldBegin($in, $name, $ftype, $fid);
          if ($ftype == TType::STOP) {
            break;
          }
          $result += $this->skip($in, $ftype);
          $result += $this->readFieldEnd($in);
        }
        $result += $this->readStructEnd($in);
        return $result;
      }
    case TType::MAP:
      {
        $result = $this->readMapBegin($in, $keyType, $valType, $size);
        for ($i = 0; $i < $size; $i++) {
          $result += $this->skip($in, $keyType);
          $result += $this->skip($in, $valType);
        }
        $result += $this->readMapEnd($in);
        return $result;
      }
    case TType::SET:
      {
        $result = $this->readSetBegin($in, $elemType, $size);
        for ($i = 0; $i < $size; $i++) {
          $result += $this->skip($in, $elemType);
        }
        $result += $this->readSetEnd($in);
        return $result;
      }
    case TType::LST:
      {
        $result = $this->readListBegin($in, $elemType, $size);
        for ($i = 0; $i < $size; $i++) {
          $result += $this->skip($in, $elemType);
        }
        $result += $this->readListEnd($in);
        return $result;
      }
    default:
      return 0;
    }
  }
}

?>
