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

#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>
#include <protocol/TBinaryProtocol.h>
#include <transport/TTransportUtils.h>
#include <transport/TSocket.h>

#include <boost/shared_ptr.hpp>
#include "ThriftTest.h"

#define __STDC_FORMAT_MACROS
#include <inttypes.h>

using namespace boost;
using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;
using namespace thrift::test;

//extern uint32_t g_socket_syscalls;

// Current time, microseconds since the epoch
uint64_t now()
{
  long long ret;
  struct timeval tv;

  gettimeofday(&tv, NULL);
  ret = tv.tv_sec;
  ret = ret*1000*1000 + tv.tv_usec;
  return ret;
}

int main(int argc, char** argv) {
  string host = "localhost";
  int port = 9090;
  int numTests = 1;
  bool framed = false;

  for (int i = 0; i < argc; ++i) {
    if (strcmp(argv[i], "-h") == 0) {
      char* pch = strtok(argv[++i], ":");
      if (pch != NULL) {
        host = string(pch);
      }
      pch = strtok(NULL, ":");
      if (pch != NULL) {
        port = atoi(pch);
      }
    } else if (strcmp(argv[i], "-n") == 0) {
      numTests = atoi(argv[++i]);
    } else if (strcmp(argv[i], "-f") == 0) {
      framed = true;
    }
  }


  shared_ptr<TTransport> transport;

  shared_ptr<TSocket> socket(new TSocket(host, port));

  if (framed) {
    shared_ptr<TFramedTransport> framedSocket(new TFramedTransport(socket));
    transport = framedSocket;
  } else {
    shared_ptr<TBufferedTransport> bufferedSocket(new TBufferedTransport(socket));
    transport = bufferedSocket;
  }

  shared_ptr<TBinaryProtocol> protocol(new TBinaryProtocol(transport));
  ThriftTestClient testClient(protocol);

  uint64_t time_min = 0;
  uint64_t time_max = 0;
  uint64_t time_tot = 0;

  int test = 0;
  for (test = 0; test < numTests; ++test) {

    try {
      transport->open();
    } catch (TTransportException& ttx) {
      printf("Connect failed: %s\n", ttx.what());
      continue;
    }

    /**
     * CONNECT TEST
     */
    printf("Test #%d, connect %s:%d\n", test+1, host.c_str(), port);

    uint64_t start = now();

    /**
     * VOID TEST
     */
    try {
      printf("testVoid()");
      testClient.testVoid();
      printf(" = void\n");
    } catch (TApplicationException tax) {
      printf("%s\n", tax.what());
    }

    /**
     * STRING TEST
     */
    printf("testString(\"Test\")");
    string s;
    testClient.testString(s, "Test");
    printf(" = \"%s\"\n", s.c_str());

    /**
     * BYTE TEST
     */
    printf("testByte(1)");
    uint8_t u8 = testClient.testByte(1);
    printf(" = %d\n", (int)u8);

    /**
     * I32 TEST
     */
    printf("testI32(-1)");
    int32_t i32 = testClient.testI32(-1);
    printf(" = %d\n", i32);

    /**
     * I64 TEST
     */
    printf("testI64(-34359738368)");
    int64_t i64 = testClient.testI64(-34359738368LL);
    printf(" = %"PRId64"\n", i64);

    /**
     * DOUBLE TEST
     */
    printf("testDouble(-5.2098523)");
    double dub = testClient.testDouble(-5.2098523);
    printf(" = %lf\n", dub);

    /**
     * STRUCT TEST
     */
    printf("testStruct({\"Zero\", 1, -3, -5})");
    Xtruct out;
    out.string_thing = "Zero";
    out.byte_thing = 1;
    out.i32_thing = -3;
    out.i64_thing = -5;
    Xtruct in;
    testClient.testStruct(in, out);
    printf(" = {\"%s\", %d, %d, %"PRId64"}\n",
           in.string_thing.c_str(),
           (int)in.byte_thing,
           in.i32_thing,
           in.i64_thing);

    /**
     * NESTED STRUCT TEST
     */
    printf("testNest({1, {\"Zero\", 1, -3, -5}), 5}");
    Xtruct2 out2;
    out2.byte_thing = 1;
    out2.struct_thing = out;
    out2.i32_thing = 5;
    Xtruct2 in2;
    testClient.testNest(in2, out2);
    in = in2.struct_thing;
    printf(" = {%d, {\"%s\", %d, %d, %"PRId64"}, %d}\n",
           in2.byte_thing,
           in.string_thing.c_str(),
           (int)in.byte_thing,
           in.i32_thing,
           in.i64_thing,
           in2.i32_thing);

    /**
     * MAP TEST
     */
    map<int32_t,int32_t> mapout;
    for (int32_t i = 0; i < 5; ++i) {
      mapout.insert(make_pair(i, i-10));
    }
    printf("testMap({");
    map<int32_t, int32_t>::const_iterator m_iter;
    bool first = true;
    for (m_iter = mapout.begin(); m_iter != mapout.end(); ++m_iter) {
      if (first) {
        first = false;
      } else {
        printf(", ");
      }
      printf("%d => %d", m_iter->first, m_iter->second);
    }
    printf("})");
    map<int32_t,int32_t> mapin;
    testClient.testMap(mapin, mapout);
    printf(" = {");
    first = true;
    for (m_iter = mapin.begin(); m_iter != mapin.end(); ++m_iter) {
      if (first) {
        first = false;
      } else {
        printf(", ");
      }
      printf("%d => %d", m_iter->first, m_iter->second);
    }
    printf("}\n");

    /**
     * SET TEST
     */
    set<int32_t> setout;
    for (int32_t i = -2; i < 3; ++i) {
      setout.insert(i);
    }
    printf("testSet({");
    set<int32_t>::const_iterator s_iter;
    first = true;
    for (s_iter = setout.begin(); s_iter != setout.end(); ++s_iter) {
      if (first) {
        first = false;
      } else {
        printf(", ");
      }
      printf("%d", *s_iter);
    }
    printf("})");
    set<int32_t> setin;
    testClient.testSet(setin, setout);
    printf(" = {");
    first = true;
    for (s_iter = setin.begin(); s_iter != setin.end(); ++s_iter) {
      if (first) {
        first = false;
      } else {
        printf(", ");
      }
      printf("%d", *s_iter);
    }
    printf("}\n");

    /**
     * LIST TEST
     */
    vector<int32_t> listout;
    for (int32_t i = -2; i < 3; ++i) {
      listout.push_back(i);
    }
    printf("testList({");
    vector<int32_t>::const_iterator l_iter;
    first = true;
    for (l_iter = listout.begin(); l_iter != listout.end(); ++l_iter) {
      if (first) {
        first = false;
      } else {
        printf(", ");
      }
      printf("%d", *l_iter);
    }
    printf("})");
    vector<int32_t> listin;
    testClient.testList(listin, listout);
    printf(" = {");
    first = true;
    for (l_iter = listin.begin(); l_iter != listin.end(); ++l_iter) {
      if (first) {
        first = false;
      } else {
        printf(", ");
      }
      printf("%d", *l_iter);
    }
    printf("}\n");

    /**
     * ENUM TEST
     */
    printf("testEnum(ONE)");
    Numberz ret = testClient.testEnum(ONE);
    printf(" = %d\n", ret);

    printf("testEnum(TWO)");
    ret = testClient.testEnum(TWO);
    printf(" = %d\n", ret);

    printf("testEnum(THREE)");
    ret = testClient.testEnum(THREE);
    printf(" = %d\n", ret);

    printf("testEnum(FIVE)");
    ret = testClient.testEnum(FIVE);
    printf(" = %d\n", ret);

    printf("testEnum(EIGHT)");
    ret = testClient.testEnum(EIGHT);
    printf(" = %d\n", ret);

    /**
     * TYPEDEF TEST
     */
    printf("testTypedef(309858235082523)");
    UserId uid = testClient.testTypedef(309858235082523LL);
    printf(" = %"PRId64"\n", uid);

    /**
     * NESTED MAP TEST
     */
    printf("testMapMap(1)");
    map<int32_t, map<int32_t, int32_t> > mm;
    testClient.testMapMap(mm, 1);
    printf(" = {");
    map<int32_t, map<int32_t, int32_t> >::const_iterator mi;
    for (mi = mm.begin(); mi != mm.end(); ++mi) {
      printf("%d => {", mi->first);
      map<int32_t, int32_t>::const_iterator mi2;
      for (mi2 = mi->second.begin(); mi2 != mi->second.end(); ++mi2) {
        printf("%d => %d, ", mi2->first, mi2->second);
      }
      printf("}, ");
    }
    printf("}\n");

    /**
     * INSANITY TEST
     */
    Insanity insane;
    insane.userMap.insert(make_pair(FIVE, 5000));
    Xtruct truck;
    truck.string_thing = "Truck";
    truck.byte_thing = 8;
    truck.i32_thing = 8;
    truck.i64_thing = 8;
    insane.xtructs.push_back(truck);
    printf("testInsanity()");
    map<UserId, map<Numberz,Insanity> > whoa;
    testClient.testInsanity(whoa, insane);
    printf(" = {");
    map<UserId, map<Numberz,Insanity> >::const_iterator i_iter;
    for (i_iter = whoa.begin(); i_iter != whoa.end(); ++i_iter) {
      printf("%"PRId64" => {", i_iter->first);
      map<Numberz,Insanity>::const_iterator i2_iter;
      for (i2_iter = i_iter->second.begin();
           i2_iter != i_iter->second.end();
           ++i2_iter) {
        printf("%d => {", i2_iter->first);
        map<Numberz, UserId> userMap = i2_iter->second.userMap;
        map<Numberz, UserId>::const_iterator um;
        printf("{");
        for (um = userMap.begin(); um != userMap.end(); ++um) {
          printf("%d => %"PRId64", ", um->first, um->second);
        }
        printf("}, ");

        vector<Xtruct> xtructs = i2_iter->second.xtructs;
        vector<Xtruct>::const_iterator x;
        printf("{");
        for (x = xtructs.begin(); x != xtructs.end(); ++x) {
          printf("{\"%s\", %d, %d, %"PRId64"}, ",
                 x->string_thing.c_str(),
                 (int)x->byte_thing,
                 x->i32_thing,
                 x->i64_thing);
        }
        printf("}");

        printf("}, ");
      }
      printf("}, ");
    }
    printf("}\n");

    /* test exception */

    try {
      printf("testClient.testException(\"Xception\") =>");
      testClient.testException("Xception");
      printf("  void\nFAILURE\n");

    } catch(Xception& e) {
      printf("  {%u, \"%s\"}\n", e.errorCode, e.message.c_str());
    }

    try {
      printf("testClient.testException(\"success\") =>");
      testClient.testException("success");
      printf("  void\n");
    } catch(...) {
      printf("  exception\nFAILURE\n");
    }

    /* test multi exception */

    try {
      printf("testClient.testMultiException(\"Xception\", \"test 1\") =>");
      Xtruct result;
      testClient.testMultiException(result, "Xception", "test 1");
      printf("  result\nFAILURE\n");
    } catch(Xception& e) {
      printf("  {%u, \"%s\"}\n", e.errorCode, e.message.c_str());
    }

    try {
      printf("testClient.testMultiException(\"Xception2\", \"test 2\") =>");
      Xtruct result;
      testClient.testMultiException(result, "Xception2", "test 2");
      printf("  result\nFAILURE\n");

    } catch(Xception2& e) {
      printf("  {%u, {\"%s\"}}\n", e.errorCode, e.struct_thing.string_thing.c_str());
    }

    try {
      printf("testClient.testMultiException(\"success\", \"test 3\") =>");
      Xtruct result;
      testClient.testMultiException(result, "success", "test 3");
      printf("  {{\"%s\"}}\n", result.string_thing.c_str());
    } catch(...) {
      printf("  exception\nFAILURE\n");
    }

    /* test oneway void */
    {
        printf("testClient.testOneway(3) =>");
        uint64_t startOneway = now();
        testClient.testOneway(3);
        uint64_t elapsed = now() - startOneway;
        if (elapsed > 200 * 1000) { // 0.2 seconds
            printf("  FAILURE - took %.2f ms\n", (double)elapsed/1000.0);
        } else {
            printf("  success - took %.2f ms\n", (double)elapsed/1000.0);
        }
    }

    /**
     * redo a simple test after the oneway to make sure we aren't "off by one" --
     * if the server treated oneway void like normal void, this next test will
     * fail since it will get the void confirmation rather than the correct
     * result. In this circumstance, the client will throw the exception:
     *
     *   TApplicationException: Wrong method namea
     */
    /**
     * I32 TEST
     */
    printf("re-test testI32(-1)");
    i32 = testClient.testI32(-1);
    printf(" = %d\n", i32);


    uint64_t stop = now();
    uint64_t tot = stop-start;

    printf("Total time: %"PRIu64" us\n", stop-start);

    time_tot += tot;
    if (time_min == 0 || tot < time_min) {
      time_min = tot;
    }
    if (tot > time_max) {
      time_max = tot;
    }

    transport->close();
  }

  //  printf("\nSocket syscalls: %u", g_socket_syscalls);
  printf("\nAll tests done.\n");

  uint64_t time_avg = time_tot / numTests;

  printf("Min time: %"PRIu64" us\n", time_min);
  printf("Max time: %"PRIu64" us\n", time_max);
  printf("Avg time: %"PRIu64" us\n", time_avg);

  return 0;
}
