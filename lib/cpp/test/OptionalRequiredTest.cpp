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
 *
 * Contains some contributions under the Thrift Software License.
 * Please see doc/old-thrift-license.txt in the Thrift distribution for
 * details.
 */

#include <cassert>
#include <map>
#include <iostream>
#include <thrift/protocol/TDebugProtocol.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TBufferTransports.h>
#include "gen-cpp/OptionalRequiredTest_types.h"

using std::cout;
using std::endl;
using std::map;
using std::string;
using namespace thrift::test;
using namespace apache::thrift;
using namespace apache::thrift::transport;
using namespace apache::thrift::protocol;


/*
template<typename Struct>
void trywrite(const Struct& s, bool should_work) {
  bool worked;
  try {
    TBinaryProtocol protocol(boost::shared_ptr<TTransport>(new TMemoryBuffer));
    s.write(&protocol);
    worked = true;
  } catch (TProtocolException & ex) {
    worked = false;
  }
  assert(worked == should_work);
}
*/

template <typename Struct1, typename Struct2>
void write_to_read(const Struct1 & w, Struct2 & r) {
  TBinaryProtocol protocol(boost::shared_ptr<TTransport>(new TMemoryBuffer));
  w.write(&protocol);
  r.read(&protocol);
}


int main() {

  cout << "This old school struct should have three fields." << endl;
  {
    OldSchool o;
    cout << ThriftDebugString(o) << endl;
  }
  cout << endl;

  cout << "Setting a value before setting isset." << endl;
  {
    Simple s;
    cout << ThriftDebugString(s) << endl;
    s.im_optional = 10;
    cout << ThriftDebugString(s) << endl;
    s.__isset.im_optional = true;
    cout << ThriftDebugString(s) << endl;
  }
  cout << endl;

  cout << "Setting isset before setting a value." << endl;
  {
    Simple s;
    cout << ThriftDebugString(s) << endl;
    s.__isset.im_optional = true;
    cout << ThriftDebugString(s) << endl;
    s.im_optional = 10;
    cout << ThriftDebugString(s) << endl;
  }
  cout << endl;

  // Write-to-read with optional fields.
  {
    Simple s1, s2, s3;
    s1.im_optional = 10;
    assert(!s1.__isset.im_default);
  //assert(!s1.__isset.im_required);  // Compile error.
    assert(!s1.__isset.im_optional);

    write_to_read(s1, s2);

    assert( s2.__isset.im_default);
  //assert( s2.__isset.im_required);  // Compile error.
    assert(!s2.__isset.im_optional);
    assert(s3.im_optional == 0);

    s1.__isset.im_optional = true;
    write_to_read(s1, s3);

    assert( s3.__isset.im_default);
  //assert( s3.__isset.im_required);  // Compile error.
    assert( s3.__isset.im_optional);
    assert(s3.im_optional == 10);
  }

  // Writing between optional and default.
  {
    Tricky1 t1;
    Tricky2 t2;

    t2.im_optional = 10;
    write_to_read(t2, t1);
    write_to_read(t1, t2);
    assert(!t1.__isset.im_default);
    assert( t2.__isset.im_optional);
    assert(t1.im_default == t2.im_optional);
    assert(t1.im_default == 0);
  }

  // Writing between default and required.
  {
    Tricky1 t1;
    Tricky3 t3;
    write_to_read(t1, t3);
    write_to_read(t3, t1);
    assert(t1.__isset.im_default);
  }

  // Writing between optional and required.
  {
    Tricky2 t2;
    Tricky3 t3;
    t2.__isset.im_optional = true;
    write_to_read(t2, t3);
    write_to_read(t3, t2);
  }

  // Mu-hu-ha-ha-ha!
  {
    Tricky2 t2;
    Tricky3 t3;
    try {
      write_to_read(t2, t3);
      abort();
    }
    catch (const TProtocolException&) {}

    write_to_read(t3, t2);
    assert(t2.__isset.im_optional);
  }

  cout << "Complex struct, simple test." << endl;
  {
    Complex c;
    cout << ThriftDebugString(c) << endl;
  }


  {
    Tricky1 t1;
    Tricky2 t2;
    // Compile error.
    //(void)(t1 == t2);
  }

  {
    OldSchool o1, o2, o3;
    assert(o1 == o2);
    o1.im_int = o2.im_int = 10;
    assert(o1 == o2);
    o1.__isset.im_int = true;
    o2.__isset.im_int = false;
    assert(o1 == o2);
    o1.im_int = 20;
    o1.__isset.im_int = false;
    assert(o1 != o2);
    o1.im_int = 10;
    assert(o1 == o2);
    o1.im_str = o2.im_str = "foo";
    assert(o1 == o2);
    o1.__isset.im_str = o2.__isset.im_str = true;
    assert(o1 == o2);
    map<int32_t,string> mymap;
    mymap[1] = "bar";
    mymap[2] = "baz";
    o1.im_big.push_back(map<int32_t,string>());
    assert(o1 != o2);
    o2.im_big.push_back(map<int32_t,string>());
    assert(o1 == o2);
    o2.im_big.push_back(mymap);
    assert(o1 != o2);
    o1.im_big.push_back(mymap);
    assert(o1 == o2);

    TBinaryProtocol protocol(boost::shared_ptr<TTransport>(new TMemoryBuffer));
    o1.write(&protocol);

    o1.im_big.push_back(mymap);
    mymap[3] = "qux";
    o2.im_big.push_back(mymap);
    assert(o1 != o2);
    o1.im_big.back()[3] = "qux";
    assert(o1 == o2);

    o3.read(&protocol);
    o3.im_big.push_back(mymap);
    assert(o1 == o3);

    //cout << ThriftDebugString(o3) << endl;
  }

  {
    Tricky2 t1, t2;
    assert(t1.__isset.im_optional == false);
    assert(t2.__isset.im_optional == false);
    assert(t1 == t2);
    t1.im_optional = 5;
    assert(t1 == t2);
    t2.im_optional = 5;
    assert(t1 == t2);
    t1.__isset.im_optional = true;
    assert(t1 != t2);
    t2.__isset.im_optional = true;
    assert(t1 == t2);
    t1.im_optional = 10;
    assert(t1 != t2);
    t2.__isset.im_optional = false;
    assert(t1 != t2);
  }

  {
    OptionalDefault t1, t2;
    cout << ThriftDebugString(t1) << endl;
    assert(t1.__isset.opt_int == true);
    assert(t1.__isset.opt_str == true);
    assert(t1.opt_int == t2.opt_int);
    assert(t1.opt_str == t2.opt_str);

    write_to_read(t1, t2);
    cout << ThriftDebugString(t2) << endl;
    assert(t2.__isset.opt_int == true);
    assert(t2.__isset.opt_str == true);
    assert(t1.opt_int == t2.opt_int);
    assert(t1.opt_str == t2.opt_str);
  }

  return 0;
}
