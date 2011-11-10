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

unit TestClient;

interface

uses
  Windows, SysUtils, Classes,
  DateUtils,
  Generics.Collections,
  TestConstants,
  Thrift.Protocol.JSON,
  Thrift.Protocol,
  Thrift.Transport,
  Thrift.Stream,
  Thrift.Test,
  Thrift.Collections,
  Thrift.Console;

type
  TThreadConsole = class
  private
    FThread : TThread;
  public
    procedure Write( const S : string);
    procedure WriteLine( const S : string);
    constructor Create( AThread: TThread);
  end;

  TClientThread = class( TThread )
  private
    FTransport : ITransport;
    FProtocol : IProtocol;
    FNumIteration : Integer;
    FConsole : TThreadConsole;

    FErrors, FSuccesses : Integer;
    procedure Expect( aTestResult : Boolean; const aTestInfo : string);
    
    procedure ClientTest;
    procedure JSONProtocolReadWriteTest;
  protected
    procedure Execute; override;
  public
    constructor Create(ATransport: ITransport; AProtocol : IProtocol; ANumIteration: Integer);
    destructor Destroy; override;
  end;

  TTestClient = class
  private
    class var
      FNumIteration : Integer;
      FNumThread : Integer;
  public
    class procedure Execute( const args: array of string);
  end;

implementation


{ TTestClient }

class procedure TTestClient.Execute(const args: array of string);
var
  i : Integer;
  host : string;
  port : Integer;
  url : string;
  bBuffered : Boolean;
  bFramed : Boolean;
  s : string;
  n : Integer;
  threads : array of TThread;
  dtStart : TDateTime;
  test : Integer;
  thread : TThread;
  trans : ITransport;
  prot : IProtocol;
  streamtrans : IStreamTransport;
  http : IHTTPClient;
  protType, p : TKnownProtocol;
begin
  bBuffered := False;;
  bFramed := False;
  protType := prot_Binary;
  try
    host := 'localhost';
    port := 9090;
    url := '';
    i := 0;
    try
      while ( i < Length(args) ) do
      begin
        try
          if ( args[i] = '-h') then
          begin
            Inc( i );
            s := args[i];
            n := Pos( ':', s);
            if ( n > 0 ) then
            begin
              host := Copy( s, 1, n - 1);
              port := StrToInt( Copy( s, n + 1, MaxInt));
            end else
            begin
              host := s;
            end;
          end else
          if (args[i] = '-u') then
          begin
            Inc( i );
            url := args[i];
          end else
          if (args[i] = '-n') then
          begin
            Inc( i );
            FNumIteration := StrToInt( args[i] );
          end else
          if (args[i] = '-b') then
          begin
            bBuffered := True;
            Console.WriteLine('Using buffered transport');
          end else
          if (args[i] = '-f' ) or ( args[i] = '-framed') then
          begin
            bFramed := True;
            Console.WriteLine('Using framed transport');
          end else
          if (args[i] = '-t') then
          begin
            Inc( i );
            FNumThread := StrToInt( args[i] );
          end else
          if (args[i] = '-prot') then  // -prot JSON|binary
          begin
            Inc( i );
            s := args[i];
            for p:= Low(TKnownProtocol) to High(TKnownProtocol) do begin
              if SameText( s, KNOWN_PROTOCOLS[p]) then begin
                protType := p;
                Console.WriteLine('Using '+KNOWN_PROTOCOLS[protType]+' protocol');
                Break;
              end;
            end;
          end;
        finally
          Inc( i );
        end;
      end;
    except
      on E: Exception do
      begin
        Console.WriteLine( E.Message );
      end;
    end;

    SetLength( threads, FNumThread);
    dtStart := Now;

    for test := 0 to FNumThread - 1 do
    begin
      if url = '' then
      begin
        streamtrans := TSocketImpl.Create( host, port );
        trans := streamtrans;
        if bBuffered then
        begin
          trans := TBufferedTransportImpl.Create( streamtrans );
        end;

        if bFramed then
        begin
          trans := TFramedTransportImpl.Create(  trans );
        end;
      end else
      begin
        http := THTTPClientImpl.Create( url );
        trans := http;
      end;

      // create protocol instance, default to BinaryProtocol
      case protType of
        prot_Binary:  prot := TBinaryProtocolImpl.Create( trans);
        prot_JSON  :  prot := TJSONProtocolImpl.Create( trans);
      else
        ASSERT( FALSE);  // unhandled case!
        prot := TBinaryProtocolImpl.Create( trans);  // use default
      end;

      thread := TClientThread.Create( trans, prot, FNumIteration);
      threads[test] := thread;
{$WARN SYMBOL_DEPRECATED OFF}
      thread.Resume;
{$WARN SYMBOL_DEPRECATED ON}
    end;

    for test := 0 to FNumThread - 1 do
    begin
      threads[test].WaitFor;
    end;

    for test := 0 to FNumThread - 1 do
    begin
      threads[test].Free;
    end;

    Console.Write('Total time: ' + IntToStr( MilliSecondsBetween(Now, dtStart)));

  except
    on E: Exception do
    begin
      Console.WriteLine( E.Message + ' ST: ' + E.StackTrace );
    end;
  end;

  Console.WriteLine('');
  Console.WriteLine('done!');
end;

{ TClientThread }

procedure TClientThread.ClientTest;
var
  client : TThriftTest.Iface;
  s : string;
  i8 : ShortInt;
  i32 : Integer;
  i64 : Int64;
  dub : Double;
  o : IXtruct;
  o2 : IXtruct2;
  i : IXtruct;
  i2 : IXtruct2;
  mapout : IThriftDictionary<Integer,Integer>;
  mapin : IThriftDictionary<Integer,Integer>;
  j : Integer;
  first : Boolean;
  key : Integer;
  listout : IThriftList<Integer>;
  listin : IThriftList<Integer>;
  setout : IHashSet<Integer>;
  setin : IHashSet<Integer>;
  ret : TNumberz;
  uid : Int64;
  mm : IThriftDictionary<Integer, IThriftDictionary<Integer, Integer>>;
  m2 : IThriftDictionary<Integer, Integer>;
  k2 : Integer;
  insane : IInsanity;
  truck : IXtruct;
  whoa : IThriftDictionary<Int64, IThriftDictionary<TNumberz, IInsanity>>;
  key64 : Int64;
  val : IThriftDictionary<TNumberz, IInsanity>;
  k2_2 : TNumberz;
  k3 : TNumberz;
  v2 : IInsanity;
  userMap : IThriftDictionary<TNumberz, Int64>;
  xtructs : IThriftList<IXtruct>;
  x : IXtruct;
  arg0 : ShortInt;
  arg1 : Integer;
  arg2 : Int64;
  multiDict : IThriftDictionary<SmallInt, string>;
  arg4 : TNumberz;
  arg5 : Int64;
  StartTick : Cardinal;
  k : Integer;
  proc : TThreadProcedure;

begin
  client := TThriftTest.TClient.Create( FProtocol);
  try
    if not FTransport.IsOpen then
    begin
      FTransport.Open;
    end;
  except
    on E: Exception do
    begin
      Console.WriteLine( E.Message );
      Exit;
    end;
  end;

  Console.Write('testException()');
  try
    client.testException('Xception');
  except
    on E: TXception do
    begin
      Console.WriteLine( ' = ' + IntToStr(E.ErrorCode) + ', ' + E.Message_ );
    end;
  end;

  Console.Write('testVoid()');
  client.testVoid();
  Console.WriteLine(' = void');

  Console.Write('testString(''Test'')');
  s := client.testString('Test');
  Console.WriteLine(' := ''' + s + '''');

  Console.Write('testByte(1)');
  i8 := client.testByte(1);
  Console.WriteLine(' := ' + IntToStr( i8 ));

  Console.Write('testI32(-1)');
  i32 := client.testI32(-1);
  Console.WriteLine(' := ' + IntToStr(i32));

  Console.Write('testI64(-34359738368)');
  i64 := client.testI64(-34359738368);
  Console.WriteLine(' := ' + IntToStr( i64));

  Console.Write('testDouble(5.325098235)');
  dub := client.testDouble(5.325098235);
  Console.WriteLine(' := ' + FloatToStr( dub));

  Console.Write('testStruct({''Zero'', 1, -3, -5})');
  o := TXtructImpl.Create;
  o.String_thing := 'Zero';
  o.Byte_thing := 1;
  o.I32_thing := -3;
  o.I64_thing := -5;
  i := client.testStruct(o);
  Console.WriteLine(' := {''' +
    i.String_thing + ''', ' +
    IntToStr( i.Byte_thing) + ', ' +
    IntToStr( i.I32_thing) + ', ' +
    IntToStr( i.I64_thing) + '}');

  Console.Write('testNest({1, {''Zero'', 1, -3, -5}, 5})');
  o2 := TXtruct2Impl.Create;
  o2.Byte_thing := 1;
  o2.Struct_thing := o;
  o2.I32_thing := 5;
  i2 := client.testNest(o2);
  i := i2.Struct_thing;
  Console.WriteLine(' := {' + IntToStr( i2.Byte_thing) + ', {''' +
    i.String_thing + ''', ' +
    IntToStr( i.Byte_thing) + ', ' +
    IntToStr( i.I32_thing) + ', ' +
    IntToStr( i.I64_thing) + '}, ' +
    IntToStr( i2.I32_thing) + '}');


  mapout := TThriftDictionaryImpl<Integer,Integer>.Create;

  for j := 0 to 4 do
  begin
    mapout.AddOrSetValue( j, j - 10);
  end;
  Console.Write('testMap({');
  first := True;
  for key in mapout.Keys do
  begin
    if first then
    begin
      first := False;
    end else
    begin
      Console.Write( ', ' );
    end;
    Console.Write( IntToStr( key) + ' => ' + IntToStr( mapout[key]));
  end;
  Console.Write('})');

  mapin := client.testMap( mapout );
  Console.Write(' = {');
  first := True;
  for key in mapin.Keys do
  begin
    if first then
    begin
      first := False;
    end else
    begin
      Console.Write( ', ' );
    end;
    Console.Write( IntToStr( key) + ' => ' + IntToStr( mapin[key]));
  end;
  Console.WriteLine('}');

  setout := THashSetImpl<Integer>.Create;
  for j := -2 to 2 do
  begin
    setout.Add( j );
  end;
  Console.Write('testSet({');
  first := True;
  for j in setout do
  begin
    if first then
    begin
      first := False;
    end else
    begin
      Console.Write(', ');
    end;
    Console.Write(IntToStr( j));
  end;
  Console.Write('})');

  Console.Write(' = {');

  first := True;
  setin := client.testSet(setout);
  for j in setin do
  begin
    if first then
    begin
      first := False;
    end else
    begin
      Console.Write(', ');
    end;
    Console.Write(IntToStr( j));
  end;
  Console.WriteLine('}');

  Console.Write('testEnum(ONE)');
  ret := client.testEnum(TNumberz.ONE);
  Console.WriteLine(' = ' + IntToStr( Integer( ret)));

  Console.Write('testEnum(TWO)');
  ret := client.testEnum(TNumberz.TWO);
  Console.WriteLine(' = ' + IntToStr( Integer( ret)));

  Console.Write('testEnum(THREE)');
  ret := client.testEnum(TNumberz.THREE);
  Console.WriteLine(' = ' + IntToStr( Integer( ret)));

  Console.Write('testEnum(FIVE)');
  ret := client.testEnum(TNumberz.FIVE);
  Console.WriteLine(' = ' + IntToStr( Integer( ret)));

  Console.Write('testEnum(EIGHT)');
  ret := client.testEnum(TNumberz.EIGHT);
  Console.WriteLine(' = ' + IntToStr( Integer( ret)));

  Console.Write('testTypedef(309858235082523)');
  uid := client.testTypedef(309858235082523);
  Console.WriteLine(' = ' + IntToStr( uid));

  Console.Write('testMapMap(1)');
  mm := client.testMapMap(1);
  Console.Write(' = {');
  for key in mm.Keys do
  begin
    Console.Write( IntToStr( key) + ' => {');
    m2 := mm[key];
    for  k2 in m2.Keys do
    begin
      Console.Write( IntToStr( k2) + ' => ' + IntToStr( m2[k2]) + ', ');
    end;
    Console.Write('}, ');
  end;
  Console.WriteLine('}');

  insane := TInsanityImpl.Create;
  insane.UserMap := TThriftDictionaryImpl<TNumberz, Int64>.Create;
  insane.UserMap.AddOrSetValue( TNumberz.FIVE, 5000);
  truck := TXtructImpl.Create;
  truck.String_thing := 'Truck';
  truck.Byte_thing := 8;
  truck.I32_thing := 8;
  truck.I64_thing := 8;
  insane.Xtructs := TThriftListImpl<IXtruct>.Create;
  insane.Xtructs.Add( truck );
  Console.Write('testInsanity()');
  whoa := client.testInsanity( insane );
  Console.Write(' = {');
  for key64 in whoa.Keys do
  begin
    val := whoa[key64];
    Console.Write( IntToStr( key64) + ' => {');
    for k2_2 in val.Keys do
    begin
      v2 := val[k2_2];
      Console.Write( IntToStr( Integer( k2_2)) + ' => {');
      userMap := v2.UserMap;
      Console.Write('{');
      if userMap <> nil then
      begin
        for k3 in userMap.Keys do
        begin
          Console.Write( IntToStr( Integer( k3)) + ' => ' + IntToStr( userMap[k3]) + ', ');
        end;
      end else
      begin
        Console.Write('null');
      end;
      Console.Write('}, ');
      xtructs := v2.Xtructs;
      Console.Write('{');

      if xtructs <> nil then
      begin
        for x in xtructs do
        begin
          Console.Write('{"' + x.String_thing + '", ' +
            IntToStr( x.Byte_thing) + ', ' +
            IntToStr( x.I32_thing) + ', ' +
            IntToStr( x.I32_thing) + '}, ');
        end;
      end else
      begin
        Console.Write('null');
      end;
      Console.Write('}');
      Console.Write('}, ');
    end;
    Console.Write('}, ');
  end;
  Console.WriteLine('}');

  arg0 := 1;
  arg1 := 2;
  arg2 := High(Int64);

  multiDict := TThriftDictionaryImpl<SmallInt, string>.Create;
  multiDict.AddOrSetValue( 1, 'one');

  arg4 := TNumberz.FIVE;
  arg5 := 5000000;
  Console.WriteLine('Test Multi(' + IntToStr( arg0) + ',' +
    IntToStr( arg1) + ',' + IntToStr( arg2) + ',' +
    multiDict.ToString + ',' + IntToStr( Integer( arg4)) + ',' +
      IntToStr( arg5) + ')');

  Console.WriteLine('Test Oneway(1)');
  client.testOneway(1);

  Console.Write('Test Calltime()');
  StartTick := GetTIckCount;

  for k := 0 to 1000 - 1 do
  begin
    client.testVoid();
  end;
  Console.WriteLine(' = ' + FloatToStr( (GetTickCount - StartTick) / 1000 ) + ' ms a testVoid() call' );

end;


procedure TClientThread.JSONProtocolReadWriteTest;
// Tests only then read/write procedures of the JSON protocol
// All tests succeed, if we can read what we wrote before
// Note that passing this test does not imply, that our JSON is really compatible to what
// other clients or servers expect as the real JSON. This is beyond the scope of this test.
var prot   : IProtocol;
    stm    : TStringStream;
    list   : IList;
    binary, binRead : TBytes;
    i,iErr : Integer;
const
  TEST_SHORT   = ShortInt( $FE);
  TEST_SMALL   = SmallInt( $FEDC);
  TEST_LONG    = LongInt( $FEDCBA98);
  TEST_I64     = Int64( $FEDCBA9876543210);
  TEST_DOUBLE  = -1.234e-56;
  DELTA_DOUBLE = TEST_DOUBLE * 1e-14;
  TEST_STRING  = 'abc-'#$00E4#$00f6#$00fc; // german umlauts (en-us: "funny chars")
begin
  stm  := TStringStream.Create;
  try
    // prepare binary data
    SetLength( binary, $100);
    for i := Low(binary) to High(binary) do binary[i] := i;

    // output setup
    prot := TJSONProtocolImpl.Create(
              TStreamTransportImpl.Create(
                nil, TThriftStreamAdapterDelphi.Create( stm, FALSE)));

    // write
    prot.WriteListBegin( TListImpl.Create( TType.String_, 9));
    prot.WriteBool( TRUE);
    prot.WriteBool( FALSE);
    prot.WriteByte( TEST_SHORT);
    prot.WriteI16( TEST_SMALL);
    prot.WriteI32( TEST_LONG);
    prot.WriteI64( TEST_I64);
    prot.WriteDouble( TEST_DOUBLE);
    prot.WriteString( TEST_STRING);
    prot.WriteBinary( binary);
    prot.WriteListEnd;

    // input setup
    Expect( stm.Position = stm.Size, 'Stream position/length after write');
    stm.Position := 0;
    prot := TJSONProtocolImpl.Create(
              TStreamTransportImpl.Create(
                TThriftStreamAdapterDelphi.Create( stm, FALSE), nil));

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

  finally
    stm.Free;
    prot := nil;  //-> Release
  end;
end;


procedure TClientThread.Expect( aTestResult : Boolean; const aTestInfo : string);
begin
  if aTestResult  then begin
    Inc(FSuccesses);
    Console.WriteLine( aTestInfo+' = OK');
  end
  else begin
    Inc(FErrors);
    Console.WriteLine( aTestInfo+' = FAILED');
    ASSERT( FALSE);  // we have a failed test!
  end;
end;


constructor TClientThread.Create(ATransport: ITransport; AProtocol : IProtocol; ANumIteration: Integer);
begin
  inherited Create( True );
  FNumIteration := ANumIteration;
  FTransport := ATransport;
  FProtocol := AProtocol;
  FConsole := TThreadConsole.Create( Self );
end;

destructor TClientThread.Destroy;
begin
  FConsole.Free;
  inherited;
end;

procedure TClientThread.Execute;
var
  i : Integer;
  proc : TThreadProcedure;
begin
  for i := 0 to FNumIteration - 1 do
  begin
    ClientTest;
    JSONProtocolReadWriteTest;
  end;

  proc := procedure
  begin
    if FTransport <> nil then
    begin
      FTransport.Close;
      FTransport := nil;
    end;
  end;

  Synchronize( proc );
end;

{ TThreadConsole }

constructor TThreadConsole.Create(AThread: TThread);
begin
  FThread := AThread;
end;

procedure TThreadConsole.Write(const S: string);
var
  proc : TThreadProcedure;
begin
  proc := procedure
  begin
    Console.Write( S );
  end;
  TThread.Synchronize( FThread, proc);
end;

procedure TThreadConsole.WriteLine(const S: string);
var
  proc : TThreadProcedure;
begin
  proc := procedure
  begin
    Console.WriteLine( S );
  end;
  TThread.Synchronize( FThread, proc);
end;

initialization
begin
  TTestClient.FNumIteration := 1;
  TTestClient.FNumThread := 1;
end;

end.
