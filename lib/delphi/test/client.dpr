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


program client;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  TestClient in 'TestClient.pas',
  Thrift.Test in 'gen-delphi\Thrift.Test.pas',
  Thrift in '..\..\..\lib\delphi\src\Thrift.pas',
  Thrift.Transport in '..\..\..\lib\delphi\src\Thrift.Transport.pas',
  Thrift.Protocol in '..\..\..\lib\delphi\src\Thrift.Protocol.pas',
  Thrift.Collections in '..\..\..\lib\delphi\src\Thrift.Collections.pas',
  Thrift.Server in '..\..\..\lib\delphi\src\Thrift.Server.pas',
  Thrift.Stream in '..\..\..\lib\delphi\src\Thrift.Stream.pas',
  Thrift.Console in '..\..\..\lib\delphi\src\Thrift.Console.pas',
  Thrift.Utils in '..\..\..\lib\delphi\src\Thrift.Utils.pas';

var
  nParamCount : Integer;
  args : array of string;
  i : Integer;
  arg : string;
  s : string;

begin
  try
    nParamCount := ParamCount;
    SetLength( args, nParamCount);
    for i := 1 to nParamCount do
    begin
      arg := ParamStr( i );
      args[i-1] := arg;
    end;
    TTestClient.Execute( args );
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

