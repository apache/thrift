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

unit UnitTests;

{$I ../src/Thrift.Defines.inc}

interface

uses
  Classes, Windows, SysUtils, Math, ActiveX, ComObj,
  {$IFDEF SupportsAsync} System.Threading, {$ENDIF}
  DateUtils,
  Generics.Collections,
  TestConstants,
  TestLogger,
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
  TQuickUnitTests = class sealed
  strict private
    FLogger : ITestLogger;

  strict protected
    // Helper
    procedure StartTestGroup( const aGroup : string; const aTest : TClientTestGroup); inline;
    procedure Expect( aTestResult : Boolean; const aTestInfo : string); inline;

    // Test impl
    procedure JSONProtocolReadWriteTest;
    procedure HashSetTest;
    {$IFDEF Win64}
    procedure UseInterlockedExchangeAdd64;
    {$ENDIF}

    // main execution part
    constructor Create( const logger : ITestLogger); reintroduce;
    procedure Execute;  overload;
  public
    destructor Destroy; override;

    class procedure Execute( const logger : ITestLogger); overload; static;
  end;


implementation


constructor TQuickUnitTests.Create( const logger : ITestLogger);
begin
  inherited Create;
  FLogger := logger;
end;


destructor TQuickUnitTests.Destroy;
begin
  try
    FLogger := nil; //-> Release
  finally
    inherited Destroy;
  end;
end;


class procedure TQuickUnitTests.Execute( const logger : ITestLogger);
var instance : TQuickUnitTests;
begin
  instance := TQuickUnitTests.Create(logger);
  try
    instance.Execute;
  finally
    instance.Free;
  end;
end;


procedure TQuickUnitTests.Execute;
begin
  {$IFDEF Win64}
  UseInterlockedExchangeAdd64;
  {$ENDIF}

  JSONProtocolReadWriteTest;
  HashSetTest;
end;


procedure TQuickUnitTests.StartTestGroup( const aGroup : string; const aTest : TClientTestGroup);
begin
  FLogger.StartTestGroup( aGroup, aTest);
end;


procedure TQuickUnitTests.Expect( aTestResult : Boolean; const aTestInfo : string);
begin
  FLogger.Expect( aTestResult, aTestInfo);
end;


{$IFDEF Win64}
procedure TQuickUnitTests.UseInterlockedExchangeAdd64;
var a,b : Int64;
begin
  a := 1;
  b := 2;
  Thrift.Utils.InterlockedExchangeAdd64( a,b);
  Expect( a = 3, 'InterlockedExchangeAdd64');
end;
{$ENDIF}


procedure TQuickUnitTests.JSONProtocolReadWriteTest;
// Tests only then read/write procedures of the JSON protocol
// All tests succeed, if we can read what we wrote before
// Note that passing this test does not imply, that our JSON is really compatible to what
// other clients or servers expect as the real JSON. This is beyond the scope of this test.
var prot   : IProtocol;
    stm    : TStringStream;
    list   : TThriftList;
    config : IThriftConfiguration;
    binary, binRead, emptyBinary : TBytes;
    i,iErr : Integer;
const
  TEST_SHORT   = ShortInt( $FE);
  TEST_SMALL   = SmallInt( $FEDC);
  TEST_LONG    = LongInt( $FEDCBA98);
  TEST_I64     = Int64( $FEDCBA9876543210);
  TEST_DOUBLE  = -1.234e-56;
  DELTA_DOUBLE = TEST_DOUBLE * 1e-14;
  TEST_STRING  = 'abc-'#$00E4#$00f6#$00fc; // german umlauts (en-us: "funny chars")
  // Test THRIFT-2336 and THRIFT-3404 with U+1D11E (G Clef symbol) and 'Русское Название';
  G_CLEF_AND_CYRILLIC_TEXT = #$1d11e' '#$0420#$0443#$0441#$0441#$043a#$043e#$0435' '#$041d#$0430#$0437#$0432#$0430#$043d#$0438#$0435;
  G_CLEF_AND_CYRILLIC_JSON = '"\ud834\udd1e \u0420\u0443\u0441\u0441\u043a\u043e\u0435 \u041d\u0430\u0437\u0432\u0430\u043d\u0438\u0435"';
  // test both possible solidus encodings
  SOLIDUS_JSON_DATA = '"one/two\/three"';
  SOLIDUS_EXCPECTED = 'one/two/three';
begin
  stm  := TStringStream.Create;
  try
    FLogger.StartTestGroup( 'JsonProtocolTest', test_Unknown);

    config := TThriftConfigurationImpl.Create;

    // prepare binary data
    binary := PrepareBinaryData( FALSE, TTestSize.Normal);
    SetLength( emptyBinary, 0); // empty binary data block

    // output setup
    prot := TJSONProtocolImpl.Create(
              TStreamTransportImpl.Create(
                nil, TThriftStreamAdapterDelphi.Create( stm, FALSE), config));

    // write
    Init( list, TType.String_, 9);
    prot.WriteListBegin( list);
    prot.WriteBool( TRUE);
    prot.WriteBool( FALSE);
    prot.WriteByte( TEST_SHORT);
    prot.WriteI16( TEST_SMALL);
    prot.WriteI32( TEST_LONG);
    prot.WriteI64( TEST_I64);
    prot.WriteDouble( TEST_DOUBLE);
    prot.WriteString( TEST_STRING);
    prot.WriteBinary( binary);
    prot.WriteString( '');  // empty string
    prot.WriteBinary( emptyBinary); // empty binary data block
    prot.WriteListEnd;

    // input setup
    Expect( stm.Position = stm.Size, 'Stream position/length after write');
    stm.Position := 0;
    prot := TJSONProtocolImpl.Create(
              TStreamTransportImpl.Create(
                TThriftStreamAdapterDelphi.Create( stm, FALSE), nil, config));

    // read and compare
    list := prot.ReadListBegin;
    Expect( list.ElementType = TType.String_, 'list element type');
    Expect( list.Count = 9, 'list element count');
    Expect( prot.ReadBool, 'WriteBool/ReadBool: TRUE');
    Expect( not prot.ReadBool, 'WriteBool/ReadBool: FALSE');
    Expect( prot.ReadByte   = TEST_SHORT,  'WriteByte/ReadByte');
    Expect( prot.ReadI16    = TEST_SMALL,  'WriteI16/ReadI16');
    Expect( prot.ReadI32    = TEST_LONG,   'WriteI32/ReadI32');
    Expect( prot.ReadI64    = TEST_I64,    'WriteI64/ReadI64');
    Expect( abs(prot.ReadDouble-TEST_DOUBLE) < abs(DELTA_DOUBLE), 'WriteDouble/ReadDouble');
    Expect( prot.ReadString = TEST_STRING, 'WriteString/ReadString');
    binRead := prot.ReadBinary;
    Expect( Length(prot.ReadString) = 0, 'WriteString/ReadString (empty string)');
    Expect( Length(prot.ReadBinary) = 0, 'empty WriteBinary/ReadBinary (empty data block)');
    prot.ReadListEnd;

    // test binary data
    Expect( Length(binary) = Length(binRead), 'Binary data length check');
    iErr := -1;
    for i := Low(binary) to High(binary) do begin
      if binary[i] <> binRead[i] then begin
        iErr := i;
        Break;
      end;
    end;
    if iErr < 0
    then Expect( TRUE,  'Binary data check ('+IntToStr(Length(binary))+' Bytes)')
    else Expect( FALSE, 'Binary data check at offset '+IntToStr(iErr));

    Expect( stm.Position = stm.Size, 'Stream position after read');


    // Solidus can be encoded in two ways. Make sure we can read both
    stm.Position := 0;
    stm.Size     := 0;
    stm.WriteString(SOLIDUS_JSON_DATA);
    stm.Position := 0;
    prot := TJSONProtocolImpl.Create(
              TStreamTransportImpl.Create(
                TThriftStreamAdapterDelphi.Create( stm, FALSE), nil, config));
    Expect( prot.ReadString = SOLIDUS_EXCPECTED, 'Solidus encoding');


    // Widechars should work too. Do they?
    // After writing, we ensure that we are able to read it back
    // We can't assume hex-encoding, since (nearly) any Unicode char is valid JSON
    stm.Position := 0;
    stm.Size     := 0;
    prot := TJSONProtocolImpl.Create(
              TStreamTransportImpl.Create(
                nil, TThriftStreamAdapterDelphi.Create( stm, FALSE), config));
    prot.WriteString( G_CLEF_AND_CYRILLIC_TEXT);
    stm.Position := 0;
    prot := TJSONProtocolImpl.Create(
              TStreamTransportImpl.Create(
                TThriftStreamAdapterDelphi.Create( stm, FALSE), nil, config));
    FLogger.Expect( prot.ReadString = G_CLEF_AND_CYRILLIC_TEXT, 'Writing JSON with chars > 8 bit');

    // Widechars should work with hex-encoding too. Do they?
    stm.Position := 0;
    stm.Size     := 0;
    stm.WriteString( G_CLEF_AND_CYRILLIC_JSON);
    stm.Position := 0;
    prot := TJSONProtocolImpl.Create(
              TStreamTransportImpl.Create(
                TThriftStreamAdapterDelphi.Create( stm, FALSE), nil, config));
    FLogger.Expect( prot.ReadString = G_CLEF_AND_CYRILLIC_TEXT, 'Reading JSON with chars > 8 bit');


  finally
    stm.Free;
    prot := nil;  //-> Release
    FLogger.StartTestGroup( '', test_Unknown);  // no more tests here
  end;
end;


procedure TQuickUnitTests.HashSetTest;
var container : IThriftHashSet<Integer>;
    testdata : array of Integer;
    i : Integer;
const
  TEST_COUNT = 4096;
begin
  StartTestGroup( 'IThriftHashSet<T> implementation', test_Containers);

  // prepare test data
  SetLength( testdata, 5);
  testdata[0] := -2;
  testdata[1] := 0;
  testdata[2] := 42;
  testdata[3] := MaxInt;
  testdata[4] := Low(Integer);

  // first insert
  container := TThriftHashSetImpl<Integer>.Create;
  for i in testdata do begin
    Expect( container.Add( i), 'add first '+IntToStr(i));
    Expect( container.Contains( i), 'contains '+IntToStr(i));
  end;
  Expect( container.Count = Length(testdata), 'container size');

  // insert again
  for i in testdata do begin
    Expect( not container.Add( i), 'add second '+IntToStr(i));
    Expect( container.Contains( i), 'contains '+IntToStr(i));
  end;
  Expect( container.Count = Length(testdata), 'container size');

  // remove
  for i in testdata do begin
    Expect( container.Remove( i), 'first remove '+IntToStr(i));
    Expect( not container.Contains( i), 'not contains '+IntToStr(i));
  end;
  Expect( container.Count = 0, 'container size');

  // remove again
  for i in testdata do begin
    Expect( not container.Remove( i), 'second remove '+IntToStr(i));
    Expect( not container.Contains( i), 'not contains '+IntToStr(i));
  end;
  Expect( container.Count = 0, 'container size');

  // append and clear
  for i := 0 to TEST_COUNT-1 do begin
    container.Add(-i);
    container.Add(+i);
  end;
  Expect( container.Count = 2*TEST_COUNT-1, 'container size check');
  Expect( not container.Contains( -TEST_COUNT), 'element not contained');
  Expect( not container.Contains( TEST_COUNT), 'element not contained');
  container.Clear;
  Expect( container.Count = 0, 'count=0 after clear');
end;








end.
