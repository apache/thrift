#include "TBinaryProtocol.h"

using std::string;

namespace facebook { namespace thrift { namespace protocol { 

uint32_t TBinaryProtocol::writeMessageBegin(shared_ptr<TTransport> out,
					    const std::string name,
					    const TMessageType messageType,
					    const uint32_t seqid) const {
  return 
    writeString(out, name) + 
    writeByte(out, (uint8_t)messageType) +
    writeU32(out, seqid);
}

uint32_t TBinaryProtocol::writeMessageEnd(shared_ptr<TTransport> out) const {
  return 0;
}

uint32_t TBinaryProtocol::writeStructBegin(shared_ptr<TTransport> out,
                                           const string& name) const {
  return 0;
}

uint32_t TBinaryProtocol::writeStructEnd(shared_ptr<TTransport> out) const {
  return 0;
}

uint32_t TBinaryProtocol::writeFieldBegin(shared_ptr<TTransport> out,
                                          const string& name,
                                          const TType fieldType,
                                          const int16_t fieldId) const {
  return
    writeByte(out, (uint8_t)fieldType) +
    writeI16(out, fieldId);
}

uint32_t TBinaryProtocol::writeFieldEnd(shared_ptr<TTransport> out) const {
  return 0;
}

uint32_t TBinaryProtocol::writeFieldStop(shared_ptr<TTransport> out) const {
  return
    writeByte(out, (uint8_t)T_STOP);
}  
                               
uint32_t TBinaryProtocol::writeMapBegin(shared_ptr<TTransport> out,
                                        const TType keyType,
                                        const TType valType,
                                        const uint32_t size) const {
  return
    writeByte(out, (uint8_t)keyType) +
    writeByte(out, (uint8_t)valType) +
    writeU32(out, (uint32_t)size);
}

uint32_t TBinaryProtocol::writeMapEnd(shared_ptr<TTransport> out) const {
  return 0;
}

uint32_t TBinaryProtocol::writeListBegin(shared_ptr<TTransport> out,
                                         const TType elemType,
                                         const uint32_t size) const {
  return
    writeByte(out, (uint8_t) elemType) +
    writeU32(out, (int32_t)size);
}

uint32_t TBinaryProtocol::writeListEnd(shared_ptr<TTransport> out) const {
  return 0;
}

uint32_t TBinaryProtocol::writeSetBegin(shared_ptr<TTransport> out,
                                        const TType elemType,
                                        const uint32_t size) const {
  return
    writeByte(out, (uint8_t)elemType) +
    writeU32(out, (int32_t)size);
}

uint32_t TBinaryProtocol::writeSetEnd(shared_ptr<TTransport> out) const {
  return 0;
}

uint32_t TBinaryProtocol::writeBool(shared_ptr<TTransport> out,
                                    const bool value) const {
  uint8_t tmp =  value ? 1 : 0;
  out->write(&tmp, 1);
  return 1;
}

uint32_t TBinaryProtocol::writeByte(shared_ptr<TTransport> out,
                                    const uint8_t byte) const {
  out->write(&byte, 1);
  return 1;
}

uint32_t TBinaryProtocol::writeU16(shared_ptr<TTransport> out,
                                   const uint16_t u16) const {
  uint16_t net = (uint16_t)htons(u16);
  out->write((uint8_t*)&net, 2);
  return 2;
}

uint32_t TBinaryProtocol::writeI16(shared_ptr<TTransport> out,
                                   const int16_t i16) const {
  int16_t net = (int16_t)htons(i16);
  out->write((uint8_t*)&net, 2);
  return 2;
}

uint32_t TBinaryProtocol::writeU32(shared_ptr<TTransport> out,
                                   const uint32_t u32) const {
  uint32_t net = (uint32_t)htonl(u32);
  out->write((uint8_t*)&net, 4);
  return 4;
}

uint32_t TBinaryProtocol::writeI32(shared_ptr<TTransport> out,
                                   const int32_t i32) const {
  int32_t net = (int32_t)htonl(i32);
  out->write((uint8_t*)&net, 4);
  return 4;
}

uint32_t TBinaryProtocol::writeU64(shared_ptr<TTransport> out,
                                   const uint64_t u64) const {
  uint64_t net = (uint64_t)htonll(u64);
  out->write((uint8_t*)&net, 8);
  return 8;
}

uint32_t TBinaryProtocol::writeI64(shared_ptr<TTransport> out,
                                   const int64_t i64) const {
  int64_t net = (int64_t)htonll(i64);
  out->write((uint8_t*)&net, 8);
  return 8;
}

uint32_t TBinaryProtocol::writeString(shared_ptr<TTransport> out,
                                      const string& str) const {
  uint32_t result = writeI32(out, str.size());
  out->write((uint8_t*)str.data(), str.size());
  return result + str.size();
}

/**
 * Reading functions
 */

uint32_t TBinaryProtocol::readMessageBegin(shared_ptr<TTransport> in,
					   std::string& name,
					   TMessageType& messageType,
					   uint32_t& seqid) const {

  uint32_t result = 0;
  uint8_t type;
  result+= readString(in, name);
  result+=  readByte(in, type);
  messageType = (TMessageType)type;
  result+= readU32(in, seqid);
  return result;
}

uint32_t TBinaryProtocol::readMessageEnd(shared_ptr<TTransport> in)  const{
  return 0;
}

uint32_t TBinaryProtocol::readStructBegin(shared_ptr<TTransport> in,
                                          string& name) const {
  name = "";
  return 0;
}

uint32_t TBinaryProtocol::readStructEnd(shared_ptr<TTransport> in) const {
  return 0;
}

uint32_t TBinaryProtocol::readFieldBegin(shared_ptr<TTransport> in,
                                         string& name,
                                         TType& fieldType,
                                         int16_t& fieldId) const {
  uint32_t result = 0;
  uint8_t type;
  result += readByte(in, type);
  fieldType = (TType)type;
  if (fieldType == T_STOP) {
    fieldId = 0;
    return result;
  }
  result += readI16(in, fieldId);
  return result;
}
  
uint32_t TBinaryProtocol::readFieldEnd(shared_ptr<TTransport> in) const {
  return 0;
}
 
uint32_t TBinaryProtocol::readMapBegin(shared_ptr<TTransport> in,
                                       TType& keyType,
                                       TType& valType,
                                       uint32_t& size) const {
  uint8_t k, v;
  uint32_t result = 0;
  result += readByte(in, k);
  keyType = (TType)k;
  result += readByte(in, v);
  valType = (TType)v;
  result += readU32(in, size);
  return result;
}

uint32_t TBinaryProtocol::readMapEnd(shared_ptr<TTransport> in) const {
  return 0;
}

uint32_t TBinaryProtocol::readListBegin(shared_ptr<TTransport> in,
                                        TType& elemType,
                                        uint32_t& size) const {
  uint8_t e;
  uint32_t result = 0;
  result += readByte(in, e);
  elemType = (TType)e;
  result += readU32(in, size);
  return result;
}

uint32_t TBinaryProtocol::readListEnd(shared_ptr<TTransport> in) const {
  return 0;
}

uint32_t TBinaryProtocol::readSetBegin(shared_ptr<TTransport> in,
                                       TType& elemType,
                                       uint32_t& size) const {
  uint8_t e;
  uint32_t result = 0;
  result += readByte(in, e);
  elemType = (TType)e;
  result += readU32(in, size);
  return result;
}

uint32_t TBinaryProtocol::readSetEnd(shared_ptr<TTransport> in) const {
  return 0;
}

uint32_t TBinaryProtocol::readBool(shared_ptr<TTransport> in,
                                   bool& value) const {
  uint8_t b[1];
  in->readAll(b, 1);
  value = *(uint8_t*)b != 0;
  return 1;
}

uint32_t TBinaryProtocol::readByte(shared_ptr<TTransport> in,
                                   uint8_t& byte) const {
  uint8_t b[1];
  in->readAll(b, 1);
  byte = *(uint8_t*)b;
  return 1;
}

uint32_t TBinaryProtocol::readU16(shared_ptr<TTransport> in,
                                  uint16_t& u16) const {
  uint8_t b[2];
  in->readAll(b, 2);
  u16 = *(uint16_t*)b;
  u16 = (uint16_t)ntohs(u16);
  return 2;
}

uint32_t TBinaryProtocol::readI16(shared_ptr<TTransport> in,
                                  int16_t& i16) const {
  uint8_t b[2];
  in->readAll(b, 2);
  i16 = *(int16_t*)b;
  i16 = (int16_t)ntohs(i16);
  return 2;
}

uint32_t TBinaryProtocol::readU32(shared_ptr<TTransport> in,
                                  uint32_t& u32) const {
  uint8_t b[4];
  in->readAll(b, 4);
  u32 = *(uint32_t*)b;
  u32 = (uint32_t)ntohl(u32);
  return 4;
}

uint32_t TBinaryProtocol::readI32(shared_ptr<TTransport> in,
                                  int32_t& i32) const {
  uint8_t b[4];
  in->readAll(b, 4);
  i32 = *(int32_t*)b;
  i32 = (int32_t)ntohl(i32);
  return 4;
}

uint32_t TBinaryProtocol::readU64(shared_ptr<TTransport> in,
                                  uint64_t& u64) const {
  uint8_t b[8];
  in->readAll(b, 8);
  u64 = *(uint64_t*)b;
  u64 = (uint64_t)ntohll(u64);
  return 8;
}

uint32_t TBinaryProtocol::readI64(shared_ptr<TTransport> in,
                                  int64_t& i64) const {
  uint8_t b[8];
  in->readAll(b, 8);
  i64 = *(int64_t*)b;
  i64 = (int64_t)ntohll(i64);
  return 8;
}

uint32_t TBinaryProtocol::readString(shared_ptr<TTransport> in,
                                     string& str) const {
  uint32_t result;
  uint32_t size;
  result = readU32(in, size);

  // Use the heap here to prevent stack overflow for v. large strings
  uint8_t *b = new uint8_t[size];
  in->readAll(b, size);
  str = string((char*)b, size);
  delete [] b;

  return result + (uint32_t)size;
}
}}} // facebook::thrift::protocol
