#include <thrift/transport/TSimpleFileTransport.h>
#include<iostream>
#include<cstring>
#define BOOST_TEST_MODULE example
#include<boost/test/included/unit_test.hpp>

using namespace apache::thrift::transport;
using namespace std;

BOOST_AUTO_TEST_SUITE(TSimpleFileTransportTest)
BOOST_AUTO_TEST_CASE(TSimpleFileTransport_write_and_read)
{
  TSimpleFileTransport trans_out("data",false,true);
  uint8_t write_value[]="foo";
  trans_out.write(write_value,3);
  trans_out.close();

  TSimpleFileTransport trans_in("data",true,false);
  uint8_t read_value[3]={0};
  trans_in.read(read_value,3);
  string expected = "foo";
  string actual = string(&read_value[0], &read_value[3]); 
  BOOST_CHECK_EQUAL(actual,expected);
  trans_in.close();
  remove("./data");
}
BOOST_AUTO_TEST_SUITE_END()
