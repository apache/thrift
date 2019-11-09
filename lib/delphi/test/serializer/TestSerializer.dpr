(*
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
 *)

program TestSerializer;

{$APPTYPE CONSOLE}

uses
  Classes,
  Windows,
  SysUtils,
  Generics.Collections,
  Thrift in '..\..\src\Thrift.pas',
  Thrift.Exception in '..\..\src\Thrift.Exception.pas',
  Thrift.Socket in '..\..\src\Thrift.Socket.pas',
  Thrift.Transport in '..\..\src\Thrift.Transport.pas',
  Thrift.Protocol in '..\..\src\Thrift.Protocol.pas',
  Thrift.Protocol.JSON in '..\..\src\Thrift.Protocol.JSON.pas',
  Thrift.Protocol.Compact in '..\..\src\Thrift.Protocol.Compact.pas',
  Thrift.Collections in '..\..\src\Thrift.Collections.pas',
  Thrift.Configuration in '..\..\src\Thrift.Configuration.pas',
  Thrift.Server in '..\..\src\Thrift.Server.pas',
  Thrift.Utils in '..\..\src\Thrift.Utils.pas',
  Thrift.Serializer in '..\..\src\Thrift.Serializer.pas',
  Thrift.Stream in '..\..\src\Thrift.Stream.pas',
  Thrift.WinHTTP in '..\..\src\Thrift.WinHTTP.pas',
  Thrift.TypeRegistry in '..\..\src\Thrift.TypeRegistry.pas',
  System_,
  DebugProtoTest,
  TestSerializer.Tests in 'TestSerializer.Tests.pas',
  TestSerializer.Data in 'TestSerializer.Data.pas';


var test : TTestSerializer;
begin
  test := TTestSerializer.Create;
  try
    test.RunTests;
  finally
    test.Free;
  end;
end.

