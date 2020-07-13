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

program TestTypeRegistry;

{$APPTYPE CONSOLE}

uses
  Classes, Windows, SysUtils, Generics.Collections, TypInfo,
  Thrift in '..\..\src\Thrift.pas',
  Thrift.Transport in '..\..\src\Thrift.Transport.pas',
  Thrift.Exception in '..\..\src\Thrift.Exception.pas',
  Thrift.Socket in '..\..\src\Thrift.Socket.pas',
  Thrift.Protocol in '..\..\src\Thrift.Protocol.pas',
  Thrift.Protocol.JSON in '..\..\src\Thrift.Protocol.JSON.pas',
  Thrift.Collections in '..\..\src\Thrift.Collections.pas',
  Thrift.Configuration in '..\..\src\Thrift.Configuration.pas',
  Thrift.Server in '..\..\src\Thrift.Server.pas',
  Thrift.Utils in '..\..\src\Thrift.Utils.pas',
  Thrift.Serializer in '..\..\src\Thrift.Serializer.pas',
  Thrift.Stream in '..\..\src\Thrift.Stream.pas',
  Thrift.WinHTTP in '..\..\src\Thrift.WinHTTP.pas',
  Thrift.TypeRegistry in '..\..\src\Thrift.TypeRegistry.pas',
  Thrift.Test, // in 'gen-delphi\Thrift.Test.pas',
  Test.TypeRegistry,
  Test.EnumToString;


begin
  try
    Test.TypeRegistry.RunTest;
    Test.EnumToString.RunTest;

    Writeln('Completed.');
  except
    on e:Exception do Writeln(e.ClassName,': ',e.Message);
  end;
end.

