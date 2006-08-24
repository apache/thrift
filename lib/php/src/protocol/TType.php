<?php

/**
 * Constants for Thrift data types.
 *
 * @package thrift.protocol
 * @author Mark Slee <mcslee@facebook.com>
 */
class TType {
  const STOP = 0;
  const VOID = 1;
  const BOOL = 2;
  const BYTE = 3;
  const U08 = 3;
  const I08 = 4;
  const U16 = 5;
  const I16 = 6;
  const U32 = 7;
  const I32 = 8;
  const U64 = 9;
  const I64 = 10;
  const STRING = 11;
  const UTF7 = 11;
  const STRUCT = 12;
  const MAP = 13;
  const SET = 14;
  const LST = 15; // cannot use LIST keyword in PHP!
  const UTF8 = 16;
  const UTF16 = 17;
}

class TMessageType {
      const CALL  = 1;
      const REPLY  = 2;
}
