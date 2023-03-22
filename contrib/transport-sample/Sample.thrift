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

/*
 transport-sample thrift IDL file .
 Execute thriftme.bat under Windows to generate the cpp stubs from this IDL.
 */

// See thrift/tutorial/tutorial.thrift and shared.thrift for more extensive examples.


namespace cpp Sample
namespace java Sample
namespace perl Sample

//This struct is not used in the sample. Shown here for illustrative purposes only.
//
struct SampleStruct
{
  1: i32 key
  2: string value
}


//A service contains the RPC(s).
//
service SampleService
{
  string HelloThere(1:string HelloString),
  void ServerDoSomething(),

  //Client calls this to tell server which port to connect back on.
  void ClientSideListenPort(1:i16 Port),
  //Named pipe version
  void ClientSidePipeName(1:string name),
}

//Sample RPC on the 'client' side that the master server can call.
service SampleCallback
{
  void pingclient(),
}
