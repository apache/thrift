#ifndef T_BINARY_PROTOCOL_H
#define T_BINARY_PROTOCOL_H

#include "protocol/TProtocol.h"

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

  std::string
    readFunction(TBuf& buf) const;
  std::string
    writeFunction(const std::string& name, const std::string& args) const;

  std::map<uint32_t, TBuf>
    readStruct(TBuf& buf) const;
  std::string
    writeStruct(const std::map<uint32_t,std::string>& s) const;

  std::string readString  (TBuf& buf) const;
  uint8_t     readByte    (TBuf& buf) const;
  uint32_t    readU32     (TBuf& buf) const;
  int32_t     readI32     (TBuf& buf) const;
  uint64_t    readU64     (TBuf& buf) const;
  int64_t     readI64     (TBuf& buf) const;

  std::string writeString (const std::string& str) const;
  std::string writeByte   (const uint8_t  byte)    const;
  std::string writeU32    (const uint32_t u32)     const;
  std::string writeI32    (const int32_t  i32)     const;
  std::string writeU64    (const uint64_t u64)     const;
  std::string writeI64    (const int64_t  i64)     const;
};

#endif
