#include "TBinaryProtocol.h"

using std::string;

namespace facebook { namespace thrift { namespace protocol { 

uint32_t TBinaryProtocol::writeMessageBegin(shared_ptr<TTransport> out,
					    const std::string name,
					    const TMessageType messageType,
					    const int32_t seqid) const {
  return 
    writeString(out, name) + 
    writeByte(out, (int8_t)messageType) +
    writeI32(out, seqid);
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
    writeByte(out, (int8_t)fieldType) +
    writeI16(out, fieldId);
}

uint32_t TBinaryProtocol::writeFieldEnd(shared_ptr<TTransport> out) const {
  return 0;
}

uint32_t TBinaryProtocol::writeFieldStop(shared_ptr<TTransport> out) const {
  return
    writeByte(out, (int8_t)T_STOP);
}  
                               
uint32_t TBinaryProtocol::writeMapBegin(shared_ptr<TTransport> out,
                                        const TType keyType,
                                        const TType valType,
                                        const uint32_t size) const {
  return
    writeByte(out, (int8_t)keyType) +
    writeByte(out, (int8_t)valType) +
    writeI32(out, (int32_t)size);
}

uint32_t TBinaryProtocol::writeMapEnd(shared_ptr<TTransport> out) const {
  return 0;
}

uint32_t TBinaryProtocol::writeListBegin(shared_ptr<TTransport> out,
                                         const TType elemType,
                                         const uint32_t size) const {
  return
    writeByte(out, (int8_t) elemType) +
    writeI32(out, (int32_t)size);
}

uint32_t TBinaryProtocol::writeListEnd(shared_ptr<TTransport> out) const {
  return 0;
}

uint32_t TBinaryProtocol::writeSetBegin(shared_ptr<TTransport> out,
                                        const TType elemType,
                                        const uint32_t size) const {
  return
    writeByte(out, (int8_t)elemType) +
    writeI32(out, (int32_t)size);
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
                                    const int8_t byte) const {
  out->write((uint8_t*)&byte, 1);
  return 1;
}

uint32_t TBinaryProtocol::writeI16(shared_ptr<TTransport> out,
                                   const int16_t i16) const {
  int16_t net = (int16_t)htons(i16);
  out->write((uint8_t*)&net, 2);
  return 2;
}

uint32_t TBinaryProtocol::writeI32(shared_ptr<TTransport> out,
                                   const int32_t i32) const {
  int32_t net = (int32_t)htonl(i32);
  out->write((uint8_t*)&net, 4);
  return 4;
}

uint32_t TBinaryProtocol::writeI64(shared_ptr<TTransport> out,
                                   const int64_t i64) const {
  int64_t net = (int64_t)htonll(i64);
  out->write((uint8_t*)&net, 8);
  return 8;
}
  
uint32_t TBinaryProtocol::writeDouble(shared_ptr<TTransport> out,
                                      const double dub) const {
  uint8_t b[8];
  uint8_t* d = (uint8_t*)&dub;
  b[0] = d[7];
  b[1] = d[6];
  b[2] = d[5];
  b[3] = d[4];
  b[4] = d[3];
  b[5] = d[2];
  b[6] = d[1];
  b[7] = d[0];
  out->write((uint8_t*)b, 8);
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
					   int32_t& seqid) const {

  uint32_t result = 0;
  int8_t type;
  result+= readString(in, name);
  result+=  readByte(in, type);
  messageType = (TMessageType)type;
  result+= readI32(in, seqid);
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
  int8_t type;
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
  int8_t k, v;
  uint32_t result = 0;
  int32_t sizei;
  result += readByte(in, k);
  keyType = (TType)k;
  result += readByte(in, v);
  valType = (TType)v;
  result += readI32(in, sizei);
  // TODO(mcslee): check for negative size
  size = (uint32_t)sizei;
  return result;
}

uint32_t TBinaryProtocol::readMapEnd(shared_ptr<TTransport> in) const {
  return 0;
}

uint32_t TBinaryProtocol::readListBegin(shared_ptr<TTransport> in,
                                        TType& elemType,
                                        uint32_t& size) const {
  int8_t e;
  uint32_t result = 0;
  int32_t sizei;
  result += readByte(in, e);
  elemType = (TType)e;
  result += readI32(in, sizei);
  // TODO(mcslee): check for negative size
  size = (uint32_t)sizei;
  return result;
}

uint32_t TBinaryProtocol::readListEnd(shared_ptr<TTransport> in) const {
  return 0;
}

uint32_t TBinaryProtocol::readSetBegin(shared_ptr<TTransport> in,
                                       TType& elemType,
                                       uint32_t& size) const {
  int8_t e;
  uint32_t result = 0;
  int32_t sizei;
  result += readByte(in, e);
  elemType = (TType)e;
  result += readI32(in, sizei);
  // TODO(mcslee): check for negative size
  size = (uint32_t)sizei;
  return result;
}

uint32_t TBinaryProtocol::readSetEnd(shared_ptr<TTransport> in) const {
  return 0;
}

uint32_t TBinaryProtocol::readBool(shared_ptr<TTransport> in,
                                   bool& value) const {
  uint8_t b[1];
  in->readAll(b, 1);
  value = *(int8_t*)b != 0;
  return 1;
}

uint32_t TBinaryProtocol::readByte(shared_ptr<TTransport> in,
                                   int8_t& byte) const {
  uint8_t b[1];
  in->readAll(b, 1);
  byte = *(int8_t*)b;
  return 1;
}

uint32_t TBinaryProtocol::readI16(shared_ptr<TTransport> in,
                                  int16_t& i16) const {
  uint8_t b[2];
  in->readAll(b, 2);
  i16 = *(int16_t*)b;
  i16 = (int16_t)ntohs(i16);
  return 2;
}

uint32_t TBinaryProtocol::readI32(shared_ptr<TTransport> in,
                                  int32_t& i32) const {
  uint8_t b[4];
  in->readAll(b, 4);
  i32 = *(int32_t*)b;
  i32 = (int32_t)ntohl(i32);
  return 4;
}

uint32_t TBinaryProtocol::readI64(shared_ptr<TTransport> in,
                                  int64_t& i64) const {
  uint8_t b[8];
  in->readAll(b, 8);
  i64 = *(int64_t*)b;
  i64 = (int64_t)ntohll(i64);
  return 8;
}

uint32_t TBinaryProtocol::readDouble(shared_ptr<TTransport> in,
                                     double& dub) const {
  uint8_t b[8];
  uint8_t d[8];
  in->readAll(b, 8);
  d[0] = b[7];
  d[1] = b[6];
  d[2] = b[5];
  d[3] = b[4];
  d[4] = b[3];
  d[5] = b[2];
  d[6] = b[1];
  d[7] = b[0];
  dub = *(double*)d;
  return 8;
}

uint32_t TBinaryProtocol::readString(shared_ptr<TTransport> in,
                                     string& str) const {
  uint32_t result;
  int32_t size;
  result = readI32(in, size);

  // TODO(mcslee): check for negative size

  // Use the heap here to prevent stack overflow for v. large strings
  uint8_t *b = new uint8_t[size];
  in->readAll(b, size);
  str = string((char*)b, size);
  delete [] b;

  return result + (uint32_t)size;
}

}}} // facebook::thrift::protocol
