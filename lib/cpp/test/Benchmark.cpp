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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <iostream>
#include <cmath>
#include "thrift/transport/TBufferTransports.h"
#include "thrift/protocol/TBinaryProtocol.h"
#include "gen-cpp/DebugProtoTest_types.h"
#include <time.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

class Timer {
public:
  timeval vStart;

  Timer() {
    gettimeofday(&vStart, 0);
  }
  void start() {
    gettimeofday(&vStart, 0);
  }

  double frame() {
    timeval vEnd;
    gettimeofday(&vEnd, 0);
    double dstart = vStart.tv_sec + ((double)vStart.tv_usec / 1000000.0);
    double dend = vEnd.tv_sec + ((double)vEnd.tv_usec / 1000000.0);
    return dend - dstart;
  }

};

int main() {
  using namespace std;
  using namespace thrift::test::debug;
  using namespace apache::thrift::transport;
  using namespace apache::thrift::protocol;
  using namespace boost;

  OneOfEach ooe;
  ooe.im_true   = true;
  ooe.im_false  = false;
  ooe.a_bite    = 0x7f;
  ooe.integer16 = 27000;
  ooe.integer32 = 1<<24;
  ooe.integer64 = (uint64_t)6000 * 1000 * 1000;
  ooe.double_precision = M_PI;
  ooe.some_characters  = "JSON THIS! \"\1";
  ooe.zomg_unicode     = "\xd7\n\a\t";
  ooe.base64 = "\1\2\3\255";

  boost::shared_ptr<TMemoryBuffer> buf(new TMemoryBuffer());

  int num = 1000000;

  {
    Timer timer;

    for (int i = 0; i < num; i ++) {
      buf->resetBuffer();
      TBinaryProtocolT<TBufferBase> prot(buf);
      ooe.write(&prot);
    }
    cout << "Write: " << num / (1000 * timer.frame()) << " kHz" << endl;
  }

  uint8_t* data;
  uint32_t datasize;

  buf->getBuffer(&data, &datasize);

  {

    Timer timer;

    for (int i = 0; i < num; i ++) {
      OneOfEach ooe2;
      boost::shared_ptr<TMemoryBuffer> buf2(new TMemoryBuffer(data, datasize));
      //buf2->resetBuffer(data, datasize);
      TBinaryProtocolT<TBufferBase> prot(buf2);
      ooe2.read(&prot);

      //cout << apache::thrift::ThriftDebugString(ooe2) << endl << endl;
    }
    cout << " Read: " << num / (1000 * timer.frame()) << " kHz" << endl;
  }


  return 0;
}
