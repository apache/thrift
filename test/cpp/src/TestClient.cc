#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>
#include <protocol/TBinaryProtocol.h>
#include <transport/TBufferedTransport.h>
#include <transport/TSocket.h>

#include <boost/shared_ptr.hpp>
#include "ThriftTest.h"

using namespace boost;
using namespace std;
using namespace facebook::thrift;
using namespace facebook::thrift::protocol;
using namespace facebook::thrift::transport;
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

  if (argc > 1) {
    host = argv[1];
  }
  if (argc > 2) {
    port = atoi(argv[2]);
  }
  if (argc > 3) {
    numTests = atoi(argv[3]);
  }

  shared_ptr<TSocket> socket(new TSocket(host, port));
  shared_ptr<TBufferedTransport> bufferedSocket(new TBufferedTransport(socket));
  shared_ptr<TBinaryProtocol> binaryProtocol(new TBinaryProtocol());
  ThriftTestClient testClient(bufferedSocket, binaryProtocol);

  uint64_t time_min = 0;
  uint64_t time_max = 0;
  uint64_t time_tot = 0;
 
  int test = 0;
  for (test = 0; test < numTests; ++test) {

    try {
      bufferedSocket->open();
    } catch (TTransportException& ttx) {
      printf("Connect failed: %s\n", ttx.getMessage().c_str());
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
    printf("testVoid()");
    testClient.testVoid();
    printf(" = void\n");
    
    /**
     * STRING TEST
     */
    printf("testString(\"Test\")");
    string s = testClient.testString("Test");
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
    printf(" = %ld\n", i64);

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
    Xtruct in = testClient.testStruct(out);
    printf(" = {\"%s\", %d, %d, %ld}\n",
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
    Xtruct2 in2 = testClient.testNest(out2);
    in = in2.struct_thing;
    printf(" = {%d, {\"%s\", %d, %d, %ld}, %d}\n",
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
    map<int32_t,int32_t> mapin = testClient.testMap(mapout);
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
    set<int32_t> setin = testClient.testSet(setout);
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
    list<int32_t> listout;
    for (int32_t i = -2; i < 3; ++i) {
      listout.push_back(i);
    }
    printf("testList({");
    list<int32_t>::const_iterator l_iter;
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
    list<int32_t> listin = testClient.testList(listout);
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
    printf(" = %ld\n", uid);

    /**
     * NESTED MAP TEST
     */
    printf("testMapMap(1)");
    map<int32_t, map<int32_t, int32_t> > mm = testClient.testMapMap(1);
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
    map<UserId, map<Numberz,Insanity> > whoa = testClient.testInsanity(insane);
    printf(" = {");
    map<UserId, map<Numberz,Insanity> >::const_iterator i_iter;
    for (i_iter = whoa.begin(); i_iter != whoa.end(); ++i_iter) {
      printf("%ld => {", i_iter->first);
      map<Numberz,Insanity>::const_iterator i2_iter;
      for (i2_iter = i_iter->second.begin();
           i2_iter != i_iter->second.end();
           ++i2_iter) {
        printf("%d => {", i2_iter->first);
        map<Numberz, UserId> userMap = i2_iter->second.userMap;
        map<Numberz, UserId>::const_iterator um;
        printf("{");
        for (um = userMap.begin(); um != userMap.end(); ++um) {
          printf("%d => %ld, ", um->first, um->second);
        }
        printf("}, ");

        list<Xtruct> xtructs = i2_iter->second.xtructs;
        list<Xtruct>::const_iterator x;
        printf("{");
        for (x = xtructs.begin(); x != xtructs.end(); ++x) {
          printf("{\"%s\", %d, %d, %ld}, ",
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
      
    }  catch(Xception& e) {
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
      Xtruct result = testClient.testMultiException("Xception", "test 1");
      printf("  result\nFAILURE\n");
    } catch(Xception& e) {
      printf("  {%u, \"%s\"}\n", e.errorCode, e.message.c_str());
    }

    try {
      printf("testClient.testMultiException(\"Xception2\", \"test 2\") =>");
      Xtruct result = testClient.testMultiException("Xception2", "test 2");
      printf("  result\nFAILURE\n");
      
    } catch(Xception2& e) {
      printf("  {%u, {\"%s\"}}\n", e.errorCode, e.struct_thing.string_thing.c_str());
    }
    
    try {
      printf("testClient.testMultiException(\"success\", \"test 3\") =>");
      Xtruct result = testClient.testMultiException("success", "test 3");
      printf("  {{\"%s\"}}\n", result.string_thing.c_str());
    } catch(...) {
      printf("  exception\nFAILURE\n");
    }
    
    uint64_t stop = now();
    uint64_t tot = stop-start;

    printf("Total time: %lu us\n", stop-start);
    
    time_tot += tot;
    if (time_min == 0 || tot < time_min) {
      time_min = tot;
    }
    if (tot > time_max) {
      time_max = tot;
    }

    bufferedSocket->close();
  }

  //  printf("\nSocket syscalls: %u", g_socket_syscalls);
  printf("\nAll tests done.\n");

  uint64_t time_avg = time_tot / numTests;

  printf("Min time: %lu us\n", time_min);
  printf("Max time: %lu us\n", time_max);
  printf("Avg time: %lu us\n", time_avg);

  return 0;
}
