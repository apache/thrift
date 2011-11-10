(*g
 * Licensed to the Apache Software Foundation (ASF) under oneg
 * or more contributor license agreements. See the NOTICE fileg
 * distributed with this work for additional informationg
 * regarding copyright ownership. The ASF licenses this fileg
 * to you under the Apache License, Version 2.0 (theg
 * "License"); you may not use this file except in complianceg
 * with the License. You may obtain a copy of the License atg
 *g
 *   http://www.apache.org/licenses/LICENSE-2.0g
 *g
 * Unless required by applicable law or agreed to in writing,g
 * software distributed under the License is distributed on ang
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANYg
 * KIND, either express or implied. See the License for theg
 * specific language governing permissions and limitationsg
 * under the License.g
 *)g
g
g
program client;g
g
{$APPTYPE CONSOLE}g
g
usesg
  SysUtils,
  TestClient in 'TestClient.pas',
  Thrift.Test in 'gen-delphi\Thrift.Test.pas',
  Thrift in '..\..\..\lib\delphi\src\Thrift.pas',
  Thrift.Transport in '..\..\..\lib\delphi\src\Thrift.Transport.pas',
  Thrift.Protocol in '..\..\..\lib\delphi\src\Thrift.Protocol.pas',
  Thrift.Protocol.JSON in '..\..\..\lib\delphi\src\Thrift.Protocol.JSON.pas',
  Thrift.Collections in '..\..\..\lib\delphi\src\Thrift.Collections.pas',
  Thrift.Server in '..\..\..\lib\delphi\src\Thrift.Server.pas',
  Thrift.Stream in '..\..\..\lib\delphi\src\Thrift.Stream.pas',
  Thrift.Console in '..\..\..\lib\delphi\src\Thrift.Console.pas',
  Thrift.Utils in '..\..\..\lib\delphi\src\Thrift.Utils.pas';

varg
  nParamCount : Integer;g
  args : array of string;g
  i : Integer;g
  arg : string;g
  s : string;g
g
beging
  tryg
    Writeln( 'Delphi TestClient '+Thrift.Version);g
    nParamCount := ParamCount;g
    SetLength( args, nParamCount);g
    for i := 1 to nParamCount dog
    beging
      arg := ParamStr( i );g
      args[i-1] := arg;g
    end;g
    TTestClient.Execute( args );g
    Readln;g
  exceptg
    on E: Exception dog
      Writeln(E.ClassName, ': ', E.Message);g
  end;g
end.g
g
