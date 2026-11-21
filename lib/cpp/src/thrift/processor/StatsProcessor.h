/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#ifndef STATSPROCESSOR_H
#define STATSPROCESSOR_H

#include <cstdio>
#include <map>
#include <memory>
#include <string>
#include <thrift/TProcessor.h>
#include <thrift/concurrency/Mutex.h>
#include <thrift/protocol/TProtocol.h>
#include <thrift/transport/TTransport.h>

namespace apache {
namespace thrift {
namespace processor {

/*
 * Class for keeping track of function call statistics and printing them if desired
 *
 * One instance may serve many connections at once. It counts the calls of at most
 * maxFrequencyNames method names one by one; the calls of any further name count
 * together under "(other)".
 */
class StatsProcessor : public apache::thrift::TProcessor {
public:
  StatsProcessor(bool print, bool frequency, size_t maxFrequencyNames = 1024)
    : print_(print), frequency_(frequency), maxFrequencyNames_(maxFrequencyNames) {}
  virtual ~StatsProcessor(){};

  virtual bool process(std::shared_ptr<apache::thrift::protocol::TProtocol> piprot,
                       std::shared_ptr<apache::thrift::protocol::TProtocol> poprot,
                       void* serverContext) {
    (void)poprot;
    (void)serverContext;

    std::string fname;
    apache::thrift::protocol::TMessageType mtype;
    int32_t seqid;

    piprot->readMessageBegin(fname, mtype, seqid);
    if (mtype != apache::thrift::protocol::T_CALL && mtype != apache::thrift::protocol::T_ONEWAY) {
      if (print_) {
        printf("Unknown message type\n");
      }
      throw apache::thrift::TException("Unexpected message type");
    }
    if (print_) {
      printf("%s (", fname.c_str());
    }
    if (frequency_) {
      count(fname);
    }

    apache::thrift::protocol::TType ftype;
    int16_t fid;

    while (true) {
      piprot->readFieldBegin(fname, ftype, fid);
      if (ftype == apache::thrift::protocol::T_STOP) {
        break;
      }

      printAndPassToBuffer(*piprot, ftype);
      if (print_) {
        printf(", ");
      }
    }

    if (print_) {
      printf("\b\b)\n");
    }
    return true;
  }

  // A copy, since other connections may be counting at the same time.
  std::map<std::string, int64_t> get_frequency_map() {
    concurrency::Guard guard(mutex_);
    return frequency_map_;
  }

protected:
  void printAndPassToBuffer(apache::thrift::protocol::TProtocol& piprot,
                            apache::thrift::protocol::TType ftype) {
    apache::thrift::protocol::TInputRecursionTracker tracker(piprot);
    switch (ftype) {
    case apache::thrift::protocol::T_BOOL: {
      bool boolv;
      piprot.readBool(boolv);
      if (print_) {
        printf("%d", boolv);
      }
    } break;
    case apache::thrift::protocol::T_BYTE: {
      int8_t bytev;
      piprot.readByte(bytev);
      if (print_) {
        printf("%d", bytev);
      }
    } break;
    case apache::thrift::protocol::T_I16: {
      int16_t i16;
      piprot.readI16(i16);
      if (print_) {
        printf("%d", i16);
      }
    } break;
    case apache::thrift::protocol::T_I32: {
      int32_t i32;
      piprot.readI32(i32);
      if (print_) {
        printf("%d", i32);
      }
    } break;
    case apache::thrift::protocol::T_I64: {
      int64_t i64;
      piprot.readI64(i64);
      if (print_) {
        printf("%ld", i64);
      }
    } break;
    case apache::thrift::protocol::T_DOUBLE: {
      double dub;
      piprot.readDouble(dub);
      if (print_) {
        printf("%f", dub);
      }
    } break;
    case apache::thrift::protocol::T_STRING: {
      std::string str;
      piprot.readString(str);
      if (print_) {
        printf("%s", str.c_str());
      }
    } break;
    case apache::thrift::protocol::T_STRUCT: {
      std::string name;
      int16_t fid;
      apache::thrift::protocol::TType ftype;
      piprot.readStructBegin(name);
      if (print_) {
        printf("<");
      }
      while (true) {
        piprot.readFieldBegin(name, ftype, fid);
        if (ftype == apache::thrift::protocol::T_STOP) {
          break;
        }
        printAndPassToBuffer(piprot, ftype);
        if (print_) {
          printf(",");
        }
        piprot.readFieldEnd();
      }
      piprot.readStructEnd();
      if (print_) {
        printf("\b>");
      }
    } break;
    case apache::thrift::protocol::T_MAP: {
      apache::thrift::protocol::TType keyType;
      apache::thrift::protocol::TType valType;
      uint32_t i, size;
      piprot.readMapBegin(keyType, valType, size);
      if (print_) {
        printf("{");
      }
      for (i = 0; i < size; i++) {
        printAndPassToBuffer(piprot, keyType);
        if (print_) {
          printf("=>");
        }
        printAndPassToBuffer(piprot, valType);
        if (print_) {
          printf(",");
        }
      }
      piprot.readMapEnd();
      if (print_) {
        printf("\b}");
      }
    } break;
    case apache::thrift::protocol::T_SET: {
      apache::thrift::protocol::TType elemType;
      uint32_t i, size;
      piprot.readSetBegin(elemType, size);
      if (print_) {
        printf("{");
      }
      for (i = 0; i < size; i++) {
        printAndPassToBuffer(piprot, elemType);
        if (print_) {
          printf(",");
        }
      }
      piprot.readSetEnd();
      if (print_) {
        printf("\b}");
      }
    } break;
    case apache::thrift::protocol::T_LIST: {
      apache::thrift::protocol::TType elemType;
      uint32_t i, size;
      piprot.readListBegin(elemType, size);
      if (print_) {
        printf("[");
      }
      for (i = 0; i < size; i++) {
        printAndPassToBuffer(piprot, elemType);
        if (print_) {
          printf(",");
        }
      }
      piprot.readListEnd();
      if (print_) {
        printf("\b]");
      }
    } break;
    default:
      break;
    }
  }

  std::map<std::string, int64_t> frequency_map_;

  bool print_;
  bool frequency_;

private:
  void count(const std::string& fname) {
    concurrency::Guard guard(mutex_);
    auto entry = frequency_map_.find(fname);
    if (entry != frequency_map_.end()) {
      entry->second++;
    } else if (frequency_map_.size() < maxFrequencyNames_) {
      frequency_map_[fname] = 1;
    } else {
      frequency_map_["(other)"]++;
    }
  }

  size_t maxFrequencyNames_;
  concurrency::Mutex mutex_;
};
}
}
} // apache::thrift::processor

#endif
