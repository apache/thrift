#include "protocol/TBinaryProtocol.h"
using namespace std;

string TBinaryProtocol::readFunction(TBuf& buf) const {
  // Let readString increment the buffer position
  return readString(buf);
}

string TBinaryProtocol::writeFunction(const string& name,
                                      const string& args) const{
  return writeString(name) + args;
}

map<uint32_t, TBuf> TBinaryProtocol::readStruct(TBuf& buf) const {
  map<uint32_t, TBuf> fieldMap;
  
  if (buf.len < 4) {
    return fieldMap;
  }
  uint32_t total_size = readU32(buf);
  if (buf.len < total_size) {
    // Data looks corrupt, we don't have that much, we will try to read what
    // we can but be sure not to go over
    total_size = buf.len;
  }

  // Field headers are 8 bytes, 4 byte fid + 4 byte length
  while (total_size > 0 && buf.len > 8) {
    uint32_t fid  = readU32(buf);
    uint32_t flen = readU32(buf);
    if (flen > buf.len) {
      // flen corrupt, there isn't that much data left
      break;
    }
    fieldMap.insert(make_pair(fid, TBuf(buf.data, flen)));
    buf.data += flen;
    buf.len  -= flen;
    total_size -= 8 + flen;
  }

  return fieldMap;
}

string TBinaryProtocol::writeStruct(const map<uint32_t,string>& s) const {
  string result = "";
  map<uint32_t,string>::const_iterator s_iter;
  for (s_iter = s.begin(); s_iter != s.end(); ++s_iter) {
    result += writeU32(s_iter->first);
    result += writeU32(s_iter->second.size());
    result += s_iter->second;
  }
  return writeU32(result.size()) + result;
}

string TBinaryProtocol::readString(TBuf& buf) const {
  uint32_t len = readU32(buf);
  if (len == 0) {
    return "";
  }
  string result((const char*)(buf.data), len);
  buf.data += len;
  buf.len  -= len;
  return result;
}

uint8_t TBinaryProtocol::readByte(TBuf& buf) const {
  if (buf.len == 0) {
    return 0;
  }
  uint8_t result = (uint8_t)buf.data[0];
  buf.data += 1;
  buf.len  -= 1;
  return result;
}

uint32_t TBinaryProtocol::readU32(TBuf& buf) const {
  if (buf.len < 4) {
    return 0;
  }
  uint32_t result = *(uint32_t*)buf.data;
  buf.data += 4;
  buf.len  -= 4;
  return result;
}

int32_t TBinaryProtocol::readI32(TBuf& buf) const {
  if (buf.len < 4) {
    return 0;
  }
  int32_t result = *(int32_t*)buf.data;
  buf.data += 4;
  buf.len  -= 4;
  return result; 
}

uint64_t TBinaryProtocol::readU64(TBuf& buf) const {
  if (buf.len < 8) {
    return 0;
  }
  uint64_t result = *(uint64_t*)buf.data;
  buf.data += 8;
  buf.len  -= 8;
  return result;
}

int64_t TBinaryProtocol::readI64(TBuf& buf) const {
  if (buf.len < 8) {
    return 0;
  }
  int64_t result = *(int64_t*)buf.data;
  buf.data += 8;
  buf.len  -= 8;
  return result;
}

string TBinaryProtocol::writeString(const string& str) const {
  uint32_t size = str.size();
  string result = string((const char*)&size, 4);
  return result + str;
}

string TBinaryProtocol::writeByte(const uint8_t byte) const {
  return string((const char*)&byte, 1);
}

string TBinaryProtocol::writeU32(const uint32_t u32) const {
  return string((const char*)&u32, 4);
}

string TBinaryProtocol::writeI32(int32_t i32) const {
  return string((const char*)&i32, 4);
}

string TBinaryProtocol::writeU64(uint64_t u64) const {
  return string((const char*)&u64, 8);
}

string TBinaryProtocol::writeI64(int64_t i64) const {
  return string((const char*)&i64, 8);
}
