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

unit TestServer;

interface

uses
  SysUtils,
  Generics.Collections,
  Thrift.Console,
  Thrift.Server,
  Thrift.Transport,
  Thrift.Protocol,
  Thrift.Protocol.JSON,
  Thrift.Collections,
  Thrift.Utils,
  Thrift.Test,
  Thrift,
  TestConstants,
  Contnrs;

type
  TTestServer = class
  public
    type

      ITestHandler = interface( TThriftTest.Iface )
        procedure SetServer( AServer : IServer );
      end;

      TTestHandlerImpl = class( TInterfacedObject, ITestHandler )
      private
        FServer : IServer;
      protected
        procedure testVoid();
        function testString(thing: string): string;
        function testByte(thing: ShortInt): ShortInt;
        function testI32(thing: Integer): Integer;
        function testI64(thing: Int64): Int64;
        function testDouble(thing: Double): Double;
        function testStruct(thing: IXtruct): IXtruct;
        function testNest(thing: IXtruct2): IXtruct2;
        function testMap(thing: IThriftDictionary<Integer, Integer>): IThriftDictionary<Integer, Integer>;
        function testStringMap(thing: IThriftDictionary<string, string>): IThriftDictionary<string, string>;
        function testSet(thing: IHashSet<Integer>): IHashSet<Integer>;
        function testList(thing: IThriftList<Integer>): IThriftList<Integer>;
        function testEnum(thing: TNumberz): TNumberz;
        function testTypedef(thing: Int64): Int64;
        function testMapMap(hello: Integer): IThriftDictionary<Integer, IThriftDictionary<Integer, Integer>>;
        function testInsanity(argument: IInsanity): IThriftDictionary<Int64, IThriftDictionary<TNumberz, IInsanity>>;
        function testMulti(arg0: ShortInt; arg1: Integer; arg2: Int64; arg3: IThriftDictionary<SmallInt, string>; arg4: TNumberz; arg5: Int64): IXtruct;
        procedure testException(arg: string);
        function testMultiException(arg0: string; arg1: string): IXtruct;
        procedure testOneway(secondsToSleep: Integer);

         procedure testStop;

        procedure SetServer( AServer : IServer );
      end;

      class procedure Execute( args: array of string);
  end;

implementation

{ TTestServer.TTestHandlerImpl }

procedure TTestServer.TTestHandlerImpl.SetServer(AServer: IServer);
begin
  FServer := AServer;
end;

function TTestServer.TTestHandlerImpl.testByte(thing: ShortInt): ShortInt;
begin
  Console.WriteLine('testByte("' + IntToStr( thing) + '")');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testDouble(thing: Double): Double;
begin
  Console.WriteLine('testDouble("' + FloatToStr( thing ) + '")');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testEnum(thing: TNumberz): TNumberz;
begin
  Console.WriteLine('testEnum(' + IntToStr( Integer( thing)) + ')');
  Result := thing;
end;

procedure TTestServer.TTestHandlerImpl.testException(arg: string);
var
  x : TXception;
begin
  Console.WriteLine('testException(' + arg + ')');
  if ( arg = 'Xception') then
  begin
    x := TXception.Create;
    x.ErrorCode := 1001;
    x.Message_ := 'This is an Xception';
    x.UpdateMessageProperty;
    raise x;
  end;
end;

function TTestServer.TTestHandlerImpl.testI32(thing: Integer): Integer;
begin
  Console.WriteLine('testI32("' + IntToStr( thing) + '")');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testI64(thing: Int64): Int64;
begin
  Console.WriteLine('testI64("' + IntToStr( thing) + '")');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testInsanity(
  argument: IInsanity): IThriftDictionary<Int64, IThriftDictionary<TNumberz, IInsanity>>;
var
  hello, goodbye : IXtruct;
  crazy : IInsanity;
  looney : IInsanity;
  first_map : IThriftDictionary<TNumberz, IInsanity>;
  second_map : IThriftDictionary<TNumberz, IInsanity>;
  insane : IThriftDictionary<Int64, IThriftDictionary<TNumberz, IInsanity>>;

begin

  Console.WriteLine('testInsanity()');
  hello := TXtructImpl.Create;
  hello.String_thing := 'hello';
  hello.Byte_thing := 2;
  hello.I32_thing := 2;
  hello.I64_thing := 2;

  goodbye := TXtructImpl.Create;
  goodbye.String_thing := 'Goodbye4';
  goodbye.Byte_thing := 4;
  goodbye.I32_thing := 4;
  goodbye.I64_thing := 4;

  crazy := TInsanityImpl.Create;
  crazy.UserMap := TThriftDictionaryImpl<TNumberz, Int64>.Create;
  crazy.UserMap.AddOrSetValue( TNumberz.EIGHT, 8);
  crazy.Xtructs := TThriftListImpl<IXtruct>.Create;
  crazy.Xtructs.Add(goodbye);

  looney := TInsanityImpl.Create;
  crazy.UserMap.AddOrSetValue( TNumberz.FIVE, 5);
  crazy.Xtructs.Add(hello);

  first_map := TThriftDictionaryImpl<TNumberz, IInsanity>.Create;
  second_map := TThriftDictionaryImpl<TNumberz, IInsanity>.Create;

  first_map.AddOrSetValue( TNumberz.SIX, crazy);
  first_map.AddOrSetValue( TNumberz.THREE, crazy);

  second_map.AddOrSetValue( TNumberz.SIX, looney);

  insane := TThriftDictionaryImpl<Int64, IThriftDictionary<TNumberz, IInsanity>>.Create;

  insane.AddOrSetValue( 1, first_map);
  insane.AddOrSetValue( 2, second_map);

  Result := insane;
end;

function TTestServer.TTestHandlerImpl.testList(
  thing: IThriftList<Integer>): IThriftList<Integer>;
var
  first : Boolean;
  elem : Integer;
begin
  Console.Write('testList({');
  first := True;
  for elem in thing do
  begin
    if first then
    begin
      first := False;
    end else
    begin
      Console.Write(', ');
    end;
    Console.Write( IntToStr( elem));
  end;
  Console.WriteLine('})');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testMap(
  thing: IThriftDictionary<Integer, Integer>): IThriftDictionary<Integer, Integer>;
var
  first : Boolean;
  key : Integer;
begin
  Console.Write('testMap({');
  first := True;
  for key in thing.Keys do
  begin
    if (first) then
    begin
      first := false;
    end else
    begin
      Console.Write(', ');
    end;
    Console.Write(IntToStr(key) + ' => ' + IntToStr( thing[key]));
  end;
  Console.WriteLine('})');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.TestMapMap(
  hello: Integer): IThriftDictionary<Integer, IThriftDictionary<Integer, Integer>>;
var
  mapmap : IThriftDictionary<Integer, IThriftDictionary<Integer, Integer>>;
  pos : IThriftDictionary<Integer, Integer>;
  neg : IThriftDictionary<Integer, Integer>;
  i : Integer;
begin
  Console.WriteLine('testMapMap(' + IntToStr( hello) + ')');
  mapmap := TThriftDictionaryImpl<Integer, IThriftDictionary<Integer, Integer>>.Create;
  pos := TThriftDictionaryImpl<Integer, Integer>.Create;
  neg := TThriftDictionaryImpl<Integer, Integer>.Create;

  for i := 1 to 4 do
  begin
    pos.AddOrSetValue( i, i);
    neg.AddOrSetValue( -i, -i);
  end;

  mapmap.AddOrSetValue(4, pos);
  mapmap.AddOrSetValue( -4, neg);

  Result := mapmap;
end;

function TTestServer.TTestHandlerImpl.testMulti(arg0: ShortInt; arg1: Integer;
  arg2: Int64; arg3: IThriftDictionary<SmallInt, string>; arg4: TNumberz;
  arg5: Int64): IXtruct;
var
  hello : IXtruct;
begin
  Console.WriteLine('testMulti()');
  hello := TXtructImpl.Create;
  hello.String_thing := 'Hello2';
  hello.Byte_thing := arg0;
  hello.I32_thing := arg1;
  hello.I64_thing := arg2;
  Result := hello;
end;

function TTestServer.TTestHandlerImpl.testMultiException(arg0,
  arg1: string): IXtruct;
var
  x : TXception;
  x2 : TXception2;
begin
  Console.WriteLine('testMultiException(' + arg0 + ', ' + arg1 + ')');
  if ( arg0 = 'Xception') then
  begin
    x := TXception.Create;
    x.ErrorCode := 1001;
    x.Message_ := 'This is an Xception';
    x.UpdateMessageProperty;
    raise x;
  end else
  if ( arg0 = 'Xception2') then
  begin
    x2 := TXception2.Create;
    x2.ErrorCode := 2002;
    x2.Struct_thing := TXtructImpl.Create;
    x2.Struct_thing.String_thing := 'This is an Xception2';
    x2.UpdateMessageProperty;
    raise x2;
  end;

  Result := TXtructImpl.Create;
  Result.String_thing := arg1;
end;

function TTestServer.TTestHandlerImpl.testNest(thing: IXtruct2): IXtruct2;
var
  temp : IXtruct;
begin
  temp := thing.Struct_thing;
  Console.WriteLine('testNest({' +
         IntToStr( thing.Byte_thing) + ', {' +
         '"' + temp.String_thing + '", ' +
         IntToStr( temp.Byte_thing) + ', ' +
         IntToStr( temp.I32_thing) + ', ' +
         IntToStr( temp.I64_thing) + '}, ' +
         IntToStr( temp.I32_thing) + '})');
  Result := thing;
end;

procedure TTestServer.TTestHandlerImpl.testOneway(secondsToSleep: Integer);
begin
  Console.WriteLine('testOneway(' + IntToStr( secondsToSleep )+ '), sleeping...');
  Sleep(secondsToSleep * 1000);
  Console.WriteLine('testOneway finished');
end;

function TTestServer.TTestHandlerImpl.testSet(
  thing: IHashSet<Integer>):IHashSet<Integer>;
var
  first : Boolean;
  elem : Integer;
begin
  Console.Write('testSet({');
  first := True;

  for elem in thing do
  begin
    if first then
    begin
      first := False;
    end else
    begin
      Console.Write( ', ');
    end;
    Console.Write( IntToStr( elem));
  end;
  Console.WriteLine('})');
  Result := thing;
end;

procedure TTestServer.TTestHandlerImpl.testStop;
begin
  if FServer <> nil then
  begin
    FServer.Stop;
  end;
end;

function TTestServer.TTestHandlerImpl.testString(thing: string): string;
begin
  Console.WriteLine('teststring("' + thing + '")');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testStringMap(
  thing: IThriftDictionary<string, string>): IThriftDictionary<string, string>;
begin

end;

function TTestServer.TTestHandlerImpl.testTypedef(thing: Int64): Int64;
begin
  Console.WriteLine('testTypedef(' + IntToStr( thing) + ')');
  Result := thing;
end;

procedure TTestServer.TTestHandlerImpl.TestVoid;
begin
  Console.WriteLine('testVoid()');
end;

function TTestServer.TTestHandlerImpl.testStruct(thing: IXtruct): IXtruct;
begin
  Console.WriteLine('testStruct({' +
    '"' + thing.String_thing + '", ' +
      IntToStr( thing.Byte_thing) + ', ' +
      IntToStr( thing.I32_thing) + ', ' +
      IntToStr( thing.I64_thing));
  Result := thing;
end;

{ TTestServer }

class procedure TTestServer.Execute(args: array of string);
var
  UseBufferedSockets : Boolean;
  UseFramed : Boolean;
  Port : Integer;
  testHandler : ITestHandler;
  testProcessor : IProcessor;
  ServerSocket : IServerTransport;
  ServerEngine : IServer;
  TransportFactory : ITransportFactory;
  ProtocolFactory : IProtocolFactory;
  i : Integer;
  s : string;
  protType, p : TKnownProtocol;
begin
  try
    UseBufferedSockets := False;
    UseFramed := False;
    protType := prot_Binary;
    Port := 9090;

    i := 0;
    while ( i < Length(args) ) do begin
      s := args[i];
      Inc(i);

      if StrToIntDef( s, -1) > 0 then
      begin
        Port :=  StrToIntDef( s, Port);
      end else
      if ( s = 'raw' ) then
      begin
        // as default
      end else
      if ( s = 'buffered' ) then
      begin
        UseBufferedSockets := True;
      end else
      if ( s = 'framed' ) then
      begin
        UseFramed := True;
      end else
      if (s = '-prot') then  // -prot JSON|binary
      begin
        s := args[i];
        Inc( i );
        for p:= Low(TKnownProtocol) to High(TKnownProtocol) do begin
          if SameText( s, KNOWN_PROTOCOLS[p]) then begin
            protType := p;
            Break;
          end;
        end;
      end else
      begin
        // Fall back to the older boolean syntax
        UseBufferedSockets := StrToBoolDef( args[1], UseBufferedSockets);
      end
    end;

    // create protocol factory, default to BinaryProtocol
    case protType of
      prot_Binary:  ProtocolFactory := TBinaryProtocolImpl.TFactory.Create;
      prot_JSON  :  ProtocolFactory := TJSONProtocolImpl.TFactory.Create;
    else
      ASSERT( FALSE);  // unhandled case!
      ProtocolFactory := TBinaryProtocolImpl.TFactory.Create;
    end;

    testHandler := TTestHandlerImpl.Create;

    testProcessor := TThriftTest.TProcessorImpl.Create( testHandler );
    ServerSocket := TServerSocketImpl.Create( Port, 0, UseBufferedSockets );

    if UseFramed
    then TransportFactory := TFramedTransportImpl.TFactory.Create
    else TransportFactory := TTransportFactoryImpl.Create;

    ServerEngine := TSimpleServer.Create( testProcessor,
                                          ServerSocket,
                                          TransportFactory,
                                          ProtocolFactory);

    testHandler.SetServer( ServerEngine);

    Console.WriteLine('Starting the server on port ' + IntToStr( Port) +
      IfValue(UseBufferedSockets, ' with buffered socket', '') +
      IfValue(useFramed, ' with framed transport', '') +
      ' using '+KNOWN_PROTOCOLS[protType]+' protocol' +
      '...');

    serverEngine.Serve;
    testHandler.SetServer( nil);

  except
    on E: Exception do
    begin
      Console.Write( E.Message);
    end;
  end;
  Console.WriteLine( 'done.');
end;

end.
