#ifndef T_BINARY_PROTOCOL_H
#define T_BINARY_PROTOCOL_H

#include "protocol/TProtocol.h"

namespace facebook { namespace thrift { namespace protocol { 

/**
 * The default binary protocol for thrift. Writes all data in a very basic
 * binary format, essentially just spitting out the raw bytes.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TBinaryProtocol : public TProtocol {
 public:
  TBinaryProtocol() {}
  ~TBinaryProtocol() {}

  /**
   * Writing functions.
   */

  uint32_t writeStructBegin   (TTransport*    out,
                               const std::string& name)   const;

  uint32_t writeStructEnd     (TTransport*    out)        const;

  uint32_t writeFieldBegin    (TTransport*    out,
                               const std::string& name,
                               const TType    fieldType,
                               const uint16_t fieldId)    const;

  uint32_t writeFieldEnd      (TTransport*    out)        const;

  uint32_t writeFieldStop     (TTransport*    out)        const;
                                       
  uint32_t writeMapBegin      (TTransport*    out,
                               const TType    keyType,
                               const TType    valType,
                               const int32_t  size)       const;

  uint32_t writeMapEnd        (TTransport*    out)        const;

  uint32_t writeListBegin     (TTransport*    out,
                               const TType    elemType,
                               const int32_t  size)       const;

  uint32_t writeListEnd       (TTransport*    out)        const;

  uint32_t writeSetBegin      (TTransport*    out,
                               const TType    elemType,
                               const int32_t  size)       const;

  uint32_t writeSetEnd        (TTransport*    out)        const;

  uint32_t writeByte          (TTransport*    out,
                               const uint8_t  byte)       const;

  uint32_t writeU32           (TTransport*    out,
                               const uint32_t u32)        const;

  uint32_t writeI32           (TTransport*    out,
                               const int32_t  i32)        const;

  uint32_t writeU64           (TTransport*    out,
                               const uint64_t u64)        const;

  uint32_t writeI64           (TTransport*    out,
                               const int64_t  i64)        const;

  uint32_t writeString        (TTransport*    out,
                               const std::string& str)    const;

  /**
   * Reading functions
   */

  uint32_t readStructBegin    (TTransport*    in,
                               std::string& name)         const;

  uint32_t readStructEnd      (TTransport*    in)         const;

  uint32_t readFieldBegin     (TTransport*    in,
                               std::string&   name,
                               TType&         fieldType,
                               uint16_t&      fieldId)    const;
  
  uint32_t readFieldEnd       (TTransport*    in)         const;
 
  uint32_t readMapBegin       (TTransport*    in,
                               TType&         keyType,
                               TType&         valType,
                               int32_t&       size)       const;

  uint32_t readMapEnd         (TTransport*    in)         const;

  uint32_t readListBegin      (TTransport*    in,
                               TType&         elemType,
                               int32_t&       size)       const;
  
  uint32_t readListEnd        (TTransport*    in)         const;

  uint32_t readSetBegin       (TTransport*    in,
                               TType&         elemType,
                               int32_t&       size)       const;

  uint32_t readSetEnd         (TTransport*    in)         const;

  uint32_t readByte           (TTransport*    in,
                               uint8_t&       byte)       const;

  uint32_t readU32            (TTransport*    in,
                               uint32_t&      u32)        const;

  uint32_t readI32            (TTransport*    in,
                               int32_t&       i32)        const;

  uint32_t readU64            (TTransport*    in,
                               uint64_t&      u64)        const;

  uint32_t readI64            (TTransport*    in,
                               int64_t&       i64)        const;

  uint32_t readString         (TTransport*    in,
                               std::string&   str)        const;

};

}}} // facebook::thrift::protocol

#endif

