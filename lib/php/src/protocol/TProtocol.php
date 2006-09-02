<?php

/**
 * For Type Constants
 */
require_once $GLOBALS['THRIFT_ROOT'].'/protocol/TType.php';


/**
 * Protocol module.
 *
 * @package thrift.protocol
 * @author Mark Slee <mcslee@facebook.com>
 */
abstract class TProtocol {

  /** 
   * Writes the message header
   *
   * @param TTransport $out Output transport
   * @param string $name Function name
   * @param int $type message type TMessageType::CALL or TMessageType::REPLY
   * @param int $seqid The sequence id of this message
   */
  public abstract function writeMessageBegin($out, $name, $type, $seqid);

  /**
   * Close the message
   *
   * @param TTransport $out Output transport
   */
  public abstract function writeMessageEnd($out);

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
  
  public abstract function writeBool($out, $bool);

  public abstract function writeByte($out, $byte);
  
  public abstract function writeI16($out, $i16);

  public abstract function writeI32($out, $i32);

  public abstract function writeI64($out, $i64);

  public abstract function writeString($out, $str);


  /**
   * Reads the message header
   *
   * @param TTransport $out Output transport
   * @param string $name Function name
   * @param int $type message type TMessageType::CALL or TMessageType::REPLY
   * @parem int $seqid The sequence id of this message
   */
  public abstract function readMessageBegin($out, &$name, &$type, &$seqid);

  /**
   * Read the close of message
   *
   * @param TTransport $out Output transport
   */
  public abstract function readMessageEnd($out);

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

  public abstract function readBool($in, &$bool);
  
  public abstract function readByte($in, &$byte);
  
  public abstract function readI16($in, &$i16);

  public abstract function readI32($in, &$i32);

  public abstract function readI64($in, &$i64);

  public abstract function readString($in, &$str);

  /**
   * The skip function is a utility to parse over unrecognized date without
   * causing corruption.
   *
   * @param TTransport $in Input transport
   * @param TType $type What type is it
   */
  public function skip($in, $type) {
    switch ($type) {
    case TType::BOOL:
      return $this->readBool($in, $bool);
    case TType::BYTE:
      return $this->readByte($in, $byte);
    case TType::I16;
      return $this->readI16($in, $i16);
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
