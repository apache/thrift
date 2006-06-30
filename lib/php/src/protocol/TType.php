<?php

/**
 * Constants for Thrift data types.
 *
 * @package thrift.protocol
 * @author Mark Slee <mcslee@facebook.com>
 */
class TType {
  const STOP = 1;
  const BYTE = 2;
  const I32 = 6;
  const I64 = 8;
  const STRING = 9;
  const STRUCT = 10;
  const MAP = 11;
  const SET = 12;
  const LST = 13; // cannot use LIST keyword in PHP!
}
