#ifndef T_PROTOCOL_H
#define T_PROTOCOL_H

#include <netinet/in.h>
#include <sys/types.h>
#include <string>
#include <map>

#include "transport/TTransport.h"

namespace facebook { namespace thrift { namespace protocol { 

using namespace facebook::thrift::transport;

#define ntohll(x) (((uint64_t)(ntohl((int)((x << 32) >> 32))) << 32) | (uint32_t)ntohl(((int)(x >> 32))))

#define htonll(x) ntohll(x)

/** Forward declaration for TProtocol */
struct TBuf;

/**
 * Enumerated definition of the types that the Thrift protocol supports.
 * Take special note of the T_END type which is used specifically to mark
 * the end of a sequence of fields.
 */
enum TType {
  T_STOP       = 1,
  T_BYTE       = 2,
  T_U16        = 3,
  T_I16        = 4,
  T_U32        = 5,
  T_I32        = 6,
  T_U64        = 7,
  T_I64        = 8,
  T_STRING     = 9,
  T_STRUCT     = 10,
  T_MAP        = 11,
  T_SET        = 12,
  T_LIST       = 13
};

/**
 * Abstract class for a thrift protocol driver. These are all the methods that
 * a protocol must implement. Essentially, there must be some way of reading
 * and writing all the base types, plus a mechanism for writing out structs
 * with indexed fields. Also notice that all methods are strictly const. This
 * is by design. Protcol impelementations may NOT keep state, because the
 * same TProtocol object may be used simultaneously by multiple threads. This
 * theoretically introduces some limititations into the possible protocol
 * formats, but with the benefit of performance, clarity, and simplicity.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TProtocol {
 public:
  virtual ~TProtocol() {}

  /**
   * Writing functions.
   */

  virtual uint32_t writeStructBegin   (TTransport*    out,
                                       const std::string& name)   const = 0;

  virtual uint32_t writeStructEnd     (TTransport*    out)        const = 0;

  virtual uint32_t writeFieldBegin    (TTransport*    out,
                                       const std::string& name,
                                       const TType    fieldType,
                                       const uint16_t fieldId)    const = 0;

  virtual uint32_t writeFieldEnd      (TTransport*    out)        const = 0;

  virtual uint32_t writeFieldStop     (TTransport*    out)        const = 0;
                                      
  virtual uint32_t writeMapBegin      (TTransport*    out,
                                       const TType    keyType,
                                       const TType    valType,
                                       const int32_t  size)       const = 0;

  virtual uint32_t writeMapEnd        (TTransport*    out)        const = 0;

  virtual uint32_t writeListBegin     (TTransport*    out,
                                       const TType    elemType,
                                       const int32_t  size)       const = 0;

  virtual uint32_t writeListEnd       (TTransport*    out)        const = 0;

  virtual uint32_t writeSetBegin      (TTransport*    out,
                                       const TType    elemType,
                                       const int32_t  size)       const = 0;

  virtual uint32_t writeSetEnd        (TTransport*    out)        const = 0;

  virtual uint32_t writeByte          (TTransport*    out,
                                       const uint8_t  byte)       const = 0;

  virtual uint32_t writeU32           (TTransport*    out,
                                       const uint32_t u32)        const = 0;

  virtual uint32_t writeI32           (TTransport*    out,
                                       const int32_t  i32)        const = 0;

  virtual uint32_t writeU64           (TTransport*    out,
                                       const uint64_t u64)        const = 0;

  virtual uint32_t writeI64           (TTransport*    out,
                                       const int64_t  i64)        const = 0;

  virtual uint32_t writeString        (TTransport*    out,
                                       const std::string& str)    const = 0;

  /**
   * Reading functions
   */

  virtual uint32_t readStructBegin    (TTransport*    in,
                                       std::string& name)         const = 0;

  virtual uint32_t readStructEnd      (TTransport*    in)         const = 0;

  virtual uint32_t readFieldBegin     (TTransport*    in,
                                       std::string&   name,
                                       TType&         fieldType,
                                       uint16_t&      fieldId)    const = 0;
  
  virtual uint32_t readFieldEnd       (TTransport*    in)         const = 0;
 
  virtual uint32_t readMapBegin       (TTransport*    in,
                                       TType&         keyType,
                                       TType&         valType,
                                       int32_t&       size)       const = 0;

  virtual uint32_t readMapEnd         (TTransport*    in)         const = 0;

  virtual uint32_t readListBegin      (TTransport*    in,
                                       TType&         elemType,
                                       int32_t&       size)       const = 0;

  virtual uint32_t readListEnd        (TTransport*    in)         const = 0;

  virtual uint32_t readSetBegin       (TTransport*    in,
                                       TType&         elemType,
                                       int32_t&       size)       const = 0;

  virtual uint32_t readSetEnd         (TTransport*    in)         const = 0;

  virtual uint32_t readByte           (TTransport*    in,
                                       uint8_t&       byte)       const = 0;

  virtual uint32_t readU32            (TTransport*    in,
                                       uint32_t&      u32)        const = 0;

  virtual uint32_t readI32            (TTransport*    in,
                                       int32_t&       i32)        const = 0;

  virtual uint32_t readU64            (TTransport*    in,
                                       uint64_t&      u64)        const = 0;

  virtual uint32_t readI64            (TTransport*    in,
                                       int64_t&       i64)        const = 0;

  virtual uint32_t readString         (TTransport*    in,
                                       std::string&   str)        const = 0;

  /**
   * Method to arbitrarily skip over data.
   */
  uint32_t skip(TTransport* in, TType type) const {
    switch (type) {
    case T_BYTE:
      {
        uint8_t byte;
        return readByte(in, byte);
      }
    case T_U32:
      {
        uint32_t u32;
        return readU32(in, u32);
      }
    case T_I32:
      {
        int32_t i32;
        return readI32(in, i32);
      }
    case T_U64:
      {
        uint64_t u64;
        return readU64(in, u64);
      }
    case T_I64:
      {
        int64_t i64;
        return readI64(in, i64);
      }
    case T_STRING:
      {
        std::string str;
        return readString(in, str);
      }
    case T_STRUCT:
      {
        uint32_t result = 0;
        std::string name;
        uint16_t fid;
        TType ftype;
        result += readStructBegin(in, name);
        while (true) {
          result += readFieldBegin(in, name, ftype, fid);
          if (ftype == T_STOP) {
            break;
          }
          result += skip(in, ftype);
          result += readFieldEnd(in);
        }
        result += readStructEnd(in);
        return result;
      }
    case T_MAP:
      {
        uint32_t result = 0;
        TType keyType;
        TType valType;
        int32_t i, size;
        result += readMapBegin(in, keyType, valType, size);
        for (i = 0; i < size; i++) {
          result += skip(in, keyType);
          result += skip(in, valType);
        }
        result += readMapEnd(in);
        return result;
      }
    case T_SET:
      {
        uint32_t result = 0;
        TType elemType;
        int32_t i, size;
        result += readSetBegin(in, elemType, size);
        for (i = 0; i < size; i++) {
          result += skip(in, elemType);
        }
        result += readSetEnd(in);
        return result;
      }
    case T_LIST:
      {
        uint32_t result = 0;
        TType elemType;
        int32_t i, size;
        result += readListBegin(in, elemType, size);
        for (i = 0; i < size; i++) {
          result += skip(in, elemType);
        }
        result += readListEnd(in);
        return result;
      }
    default:
      return 0;
    }
  }

 protected:
  TProtocol() {}
};

}}} // facebook::thrift::protocol

#endif

