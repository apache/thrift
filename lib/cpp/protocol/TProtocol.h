#ifndef T_PROTOCOL_H
#define T_PROTOCOL_H

#include <sys/types.h>
#include <string>
#include <map>

/** Forward declaration for TProtocol */
struct TBuf;

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
   * Function call serialization.
   */

  virtual std::string
    readFunction(TBuf& buf) const = 0;
  virtual std::string
    writeFunction(const std::string& name, const std::string& args) const = 0;

  /**
   * Struct serialization.
   */

  virtual std::map<uint32_t, TBuf>
    readStruct(TBuf& buf) const = 0;
  virtual std::string
    writeStruct(const std::map<uint32_t,std::string>& s) const = 0;

  /**
   * Basic data type deserialization. Note that these read methods do not
   * take a const reference to the TBuf object. They SHOULD change the TBuf
   * object so that it reflects the buffer AFTER the basic data type has
   * been consumed such that data may continue being read serially from the
   * buffer.
   */

  virtual std::string readString  (TBuf& buf) const = 0;
  virtual uint8_t     readByte    (TBuf& buf) const = 0;
  virtual uint32_t    readU32     (TBuf& buf) const = 0;
  virtual int32_t     readI32     (TBuf& buf) const = 0;
  virtual uint64_t    readU64     (TBuf& buf) const = 0;
  virtual int64_t     readI64     (TBuf& buf) const = 0;

  virtual std::string writeString (const std::string& str) const = 0;
  virtual std::string writeByte   (const uint8_t  byte)    const = 0;
  virtual std::string writeU32    (const uint32_t u32)     const = 0;
  virtual std::string writeI32    (const int32_t  i32)     const = 0;
  virtual std::string writeU64    (const uint64_t u64)     const = 0;
  virtual std::string writeI64    (const int64_t  i64)     const = 0;

 protected:
  TProtocol() {}
};

/**
 * Wrapper around raw data that allows us to track the length of a data
 * buffer. It is the responsibility of a robust TProtocol implementation
 * to ensure that any reads that are done from data do NOT overrun the
 * memory address at data+len. It is also a convention that TBuf objects
 * do NOT own the memory pointed to by data. They are merely wrappers
 * around buffers that have been allocated elsewhere. Therefore, the user
 * should never allocate memory before putting it into a TBuf nor should
 * they free the data pointed to by a TBuf.
 */
struct TBuf {
  TBuf(const TBuf& that) : data(that.data), len(that.len) {}
  TBuf(const uint8_t* d, uint32_t l) : data(d), len(l) {}
  const uint8_t* data;
  uint32_t len;
};

#endif
