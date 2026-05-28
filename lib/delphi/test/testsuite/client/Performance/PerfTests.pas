// Licensed to the Apache Software Foundation(ASF) under one
// or more contributor license agreements.See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.
unit PerfTests;

interface

uses
  Windows, Classes, SysUtils,
  Thrift.Collections,
  Thrift.Configuration,
  Thrift.Test,
  Thrift.Protocol,
  Thrift.Protocol.JSON,
  Thrift.Protocol.Compact,
  Thrift.Transport,
  Thrift.Stream,
  ConsoleHelper,
  TestConstants,
  UnitTestTransportFactory,
  DataFactory;

type
  TPerformanceTests = class
  strict private
    FTestdata  : ICrazyNesting;
    FMemBuffer : TMemoryStream;
    FTransport : ITransport;
    FConfig    : IThriftConfiguration;

    procedure ProtocolPeformanceTest;
    procedure RunTest( const ptyp : TKnownProtocol; const layered : TLayeredTransport);
    function  GenericProtocolFactory(const ptyp : TKnownProtocol; const layered : TLayeredTransport; const forWrite : Boolean) : IProtocol;
    function  GetProtocolTransportName(const ptyp : TKnownProtocol; const layered : TLayeredTransport) : string;
  public
    class function  Execute : Integer;
  end;


implementation


// not available in all versions, so make sure we have this one imported
function IsDebuggerPresent: BOOL; stdcall; external KERNEL32 name 'IsDebuggerPresent';


class function TPerformanceTests.Execute : Integer;
var instance : TPerformanceTests;
begin
  instance := TPerformanceTests.Create;
  instance.ProtocolPeformanceTest;

  // debug only
  if IsDebuggerPresent then begin
     Console.Write('Hit ENTER ...');
     ReadLn;
  end;

  result := 0;
end;


procedure TPerformanceTests.ProtocolPeformanceTest;
var layered : TLayeredTransport;
begin
  Console.WriteLine('Setting up for ProtocolPeformanceTest ...');
  FTestdata := TestDataFactory.CreateCrazyNesting();

  for layered := Low(TLayeredTransport) to High(TLayeredTransport) do begin
    RunTest( TKnownProtocol.prot_Binary,  layered);
    RunTest( TKnownProtocol.prot_Compact, layered);
    RunTest( TKnownProtocol.prot_JSON,    layered);
  end;
end;


procedure TPerformanceTests.RunTest( const ptyp : TKnownProtocol; const layered : TLayeredTransport);
var freq, start, stop : Int64;
    proto : IProtocol;
    restored : ICrazyNesting;
begin
  QueryPerformanceFrequency( freq);

  FConfig := TThriftConfigurationImpl.Create;

  proto := GenericProtocolFactory( ptyp, layered, TRUE);
  QueryPerformanceCounter( start);
  FTestdata.Write(proto);
  FTransport.Flush;
  QueryPerformanceCounter( stop);
  Console.WriteLine( Format('RunTest(%s): write = %d msec', [
                     GetProtocolTransportName(ptyp,layered),
                     Round(1000.0*(stop-start)/freq)
                     ]));

  restored := TCrazyNestingImpl.Create;
  proto := GenericProtocolFactory( ptyp, layered, FALSE);
  QueryPerformanceCounter( start);
  restored.Read(proto);
  QueryPerformanceCounter( stop);
  Console.WriteLine( Format('RunTest(%s): read = %d msec', [
                     GetProtocolTransportName(ptyp,layered),
                     Round(1000.0*(stop-start)/freq)
                     ]));
end;


function TPerformanceTests.GenericProtocolFactory(const ptyp : TKnownProtocol; const layered : TLayeredTransport; const forWrite : Boolean) : IProtocol;
var stack : TTransportProtocolStack;
begin
  stack := MakeTransportProtocolStack( ptyp, layered, FConfig, forWrite, FMemBuffer);
  FTransport := stack.Trans;
  result := stack.Proto;
end;


function TPerformanceTests.GetProtocolTransportName(const ptyp : TKnownProtocol; const layered : TLayeredTransport) : string;
begin
  case layered of
    trns_Framed   :  result := ' + framed';
    trns_Buffered :  result := ' + buffered';
  else
    result := '';
  end;

  case ptyp of
    prot_Binary  :  result := 'binary' + result;
    prot_Compact :  result := 'compact' + result;
    prot_JSON    :  result := 'JSON' + result;
  else
    ASSERT(FALSE);
  end;
end;


end.

