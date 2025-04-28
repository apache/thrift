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

unit TestLogger;

{$I ../src/Thrift.Defines.inc}

interface

uses
  Classes, Windows, SysUtils, Math, ActiveX, ComObj,
  {$IFDEF SupportsAsync} System.Threading, {$ENDIF}
  DateUtils,
  Generics.Collections,
  TestConstants,
  ConsoleHelper,
  Thrift,
  Thrift.Protocol.Compact,
  Thrift.Protocol.JSON,
  Thrift.Protocol,
  Thrift.Transport.Pipes,
  Thrift.Transport.WinHTTP,
  Thrift.Transport.MsxmlHTTP,
  Thrift.Transport,
  Thrift.Stream,
  Thrift.Test,
  Thrift.WinHTTP,
  Thrift.Utils,
  Thrift.Configuration,
  Thrift.Collections;


type
  TClientTestGroup = (
    test_Unknown,
    test_BaseTypes,
    test_Structs,
    test_Containers,
    test_Exceptions
    // new values here
  );
  TClientTestGroups = set of TClientTestGroup;


  ITestLogger = interface
    ['{26693ED5-1469-48AD-B1F3-04281B053DD4}']
    procedure StartTestGroup( const aGroup : string; const aTest : TClientTestGroup);
    procedure Expect( aTestResult : Boolean; const aTestInfo : string);
    procedure QueryTestStats( out failed, executed : TClientTestGroups);
    procedure ReportResults;
  end;


  // test reporting helper
  TTestLoggerImpl = class( TInterfacedObject, ITestLogger)
  strict private
    FTestGroup : string;
    FCurrentTest : TClientTestGroup;
    FSuccesses : Integer;
    FErrors : TStringList;
    FFailed : TClientTestGroups;
    FExecuted : TClientTestGroups;

  strict protected
    // ITestLogger = interface
    procedure StartTestGroup( const aGroup : string; const aTest : TClientTestGroup);
    procedure Expect( aTestResult : Boolean; const aTestInfo : string);
    procedure QueryTestStats( out failed, executed : TClientTestGroups);
    procedure ReportResults;

  public
    constructor Create;
    destructor Destroy;  override;

  end;


implementation


constructor TTestLoggerImpl.Create;
begin
  inherited Create;
  FCurrentTest := test_Unknown;

  // error list: keep correct order, allow for duplicates
  FErrors := TStringList.Create;
  FErrors.Sorted := FALSE;
  FErrors.Duplicates := dupAccept;
end;


destructor TTestLoggerImpl.Destroy;
begin
  try
    FreeAndNil( FErrors);
  finally
    inherited Destroy;
  end;
end;


procedure TTestLoggerImpl.StartTestGroup( const aGroup : string; const aTest : TClientTestGroup);
begin
  FTestGroup := aGroup;
  FCurrentTest := aTest;

  Include( FExecuted, aTest);

  if FTestGroup <> '' then begin
    Console.WriteLine('');
    Console.WriteLine( aGroup+' tests');
    Console.WriteLine( StringOfChar('-',60));
  end;
end;


procedure TTestLoggerImpl.Expect( aTestResult : Boolean; const aTestInfo : string);
begin
  if aTestResult  then begin
    Inc(FSuccesses);
    Console.WriteLine( aTestInfo+': passed');
  end
  else begin
    FErrors.Add( FTestGroup+': '+aTestInfo);
    Include( FFailed, FCurrentTest);
    Console.WriteLine( aTestInfo+': *** FAILED ***');

    // We have a failed test!
    // -> issue DebugBreak ONLY if a debugger is attached,
    // -> unhandled DebugBreaks would cause Windows to terminate the app otherwise
    if IsDebuggerPresent
    then {$IFDEF CPUX64} DebugBreak {$ELSE} asm int 3 end {$ENDIF};
  end;
end;


procedure TTestLoggerImpl.QueryTestStats( out failed, executed : TClientTestGroups);
begin
  failed := FFailed;
  executed := FExecuted;
end;



procedure TTestLoggerImpl.ReportResults;
var nTotal : Integer;
    sLine : string;
begin
  // prevent us from stupid DIV/0 errors
  nTotal := FSuccesses + FErrors.Count;
  if nTotal = 0 then begin
    Console.WriteLine('No results logged');
    Exit;
  end;

  Console.WriteLine('');
  Console.WriteLine( StringOfChar('=',60));
  Console.WriteLine( IntToStr(nTotal)+' tests performed');
  Console.WriteLine( IntToStr(FSuccesses)+' tests succeeded ('+IntToStr(round(100*FSuccesses/nTotal))+'%)');
  Console.WriteLine( IntToStr(FErrors.Count)+' tests failed ('+IntToStr(round(100*FErrors.Count/nTotal))+'%)');
  Console.WriteLine( StringOfChar('=',60));
  if FErrors.Count > 0 then begin
    Console.WriteLine('FAILED TESTS:');
    for sLine in FErrors do Console.WriteLine('- '+sLine);
    Console.WriteLine( StringOfChar('=',60));
    InterlockedIncrement( ExitCode);  // return <> 0 on errors
  end;
  Console.WriteLine('');
end;






end.
