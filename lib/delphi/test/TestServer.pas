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

{$WARN SYMBOL_PLATFORM OFF}

{.$DEFINE RunEndless}   // activate to interactively stress-test the server stop routines via Ctrl+C

interface

uses
  Windows, SysUtils,
  Generics.Collections,
  Thrift.Console,
  Thrift.Server,
  Thrift.Transport,
  Thrift.Transport.Pipes,
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
        procedure SetServer( const AServer : IServer );
        procedure TestStop;
      end;

      TTestHandlerImpl = class( TInterfacedObject, ITestHandler )
      private
        FServer : IServer;
      protected
        procedure testVoid();
        function testString(const thing: string): string;
        function testByte(thing: ShortInt): ShortInt;
        function testI32(thing: Integer): Integer;
        function testI64(const thing: Int64): Int64;
        function testDouble(const thing: Double): Double;
        function testStruct(const thing: IXtruct): IXtruct;
        function testNest(const thing: IXtruct2): IXtruct2;
        function testMap(const thing: IThriftDictionary<Integer, Integer>): IThriftDictionary<Integer, Integer>;
        function testStringMap(const thing: IThriftDictionary<string, string>): IThriftDictionary<string, string>;
        function testSet(const thing: IHashSet<Integer>): IHashSet<Integer>;
        function testList(const thing: IThriftList<Integer>): IThriftList<Integer>;
        function testEnum(thing: TNumberz): TNumberz;
        function testTypedef(const thing: Int64): Int64;
        function testMapMap(hello: Integer): IThriftDictionary<Integer, IThriftDictionary<Integer, Integer>>;
        function testInsanity(const argument: IInsanity): IThriftDictionary<Int64, IThriftDictionary<TNumberz, IInsanity>>;
        function testMulti(arg0: ShortInt; arg1: Integer; const arg2: Int64; const arg3: IThriftDictionary<SmallInt, string>; arg4: TNumberz; const arg5: Int64): IXtruct;
        procedure testException(const arg: string);
        function testMultiException(const arg0: string; const arg1: string): IXtruct;
        procedure testOneway(secondsToSleep: Integer);

        procedure TestStop;
        procedure SetServer( const AServer : IServer );
      end;

      class procedure LaunchAnonPipeChild( const app : string; const transport : IAnonymousPipeServerTransport);
      class procedure Execute( const args: array of string);
  end;

implementation


var g_Handler : TTestServer.ITestHandler = nil;


function MyConsoleEventHandler( dwCtrlType : DWORD) : BOOL;  stdcall;
// Note that this Handler procedure is called from another thread
var handler : TTestServer.ITestHandler;
begin
  result := TRUE;
  try
    case dwCtrlType of
      CTRL_C_EVENT        :  Console.WriteLine( 'Ctrl+C pressed');
      CTRL_BREAK_EVENT    :  Console.WriteLine( 'Ctrl+Break pressed');
      CTRL_CLOSE_EVENT    :  Console.WriteLine( 'Received CloseTask signal');
      CTRL_LOGOFF_EVENT   :  Console.WriteLine( 'Received LogOff signal');
      CTRL_SHUTDOWN_EVENT :  Console.WriteLine( 'Received Shutdown signal');
    else
      Console.WriteLine( 'Received console event #'+IntToStr(Integer(dwCtrlType)));
    end;

    handler := g_Handler;
    if handler <> nil then handler.TestStop;

  except
    // catch all
  end;
end;


{ TTestServer.TTestHandlerImpl }

procedure TTestServer.TTestHandlerImpl.SetServer( const AServer: IServer);
begin
  FServer := AServer;
end;

function TTestServer.TTestHandlerImpl.testByte(thing: ShortInt): ShortInt;
begin
  Console.WriteLine('testByte("' + IntToStr( thing) + '")');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testDouble( const thing: Double): Double;
begin
  Console.WriteLine('testDouble("' + FloatToStr( thing ) + '")');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testEnum(thing: TNumberz): TNumberz;
begin
  Console.WriteLine('testEnum(' + IntToStr( Integer( thing)) + ')');
  Result := thing;
end;

procedure TTestServer.TTestHandlerImpl.testException(const arg: string);
begin
  Console.WriteLine('testException(' + arg + ')');
  if ( arg = 'Xception') then
  begin
    raise TXception.Create( 1001, arg);
  end;

  if (arg = 'TException') then
  begin
    raise TException.Create('');
  end;

  // else do not throw anything
end;

function TTestServer.TTestHandlerImpl.testI32(thing: Integer): Integer;
begin
  Console.WriteLine('testI32("' + IntToStr( thing) + '")');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testI64( const thing: Int64): Int64;
begin
  Console.WriteLine('testI64("' + IntToStr( thing) + '")');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testInsanity(
  const argument: IInsanity): IThriftDictionary<Int64, IThriftDictionary<TNumberz, IInsanity>>;
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
  hello.String_thing := 'Hello2';
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

  first_map.AddOrSetValue( TNumberz.TWO, crazy);
  first_map.AddOrSetValue( TNumberz.THREE, crazy);

  second_map.AddOrSetValue( TNumberz.SIX, looney);

  insane := TThriftDictionaryImpl<Int64, IThriftDictionary<TNumberz, IInsanity>>.Create;

  insane.AddOrSetValue( 1, first_map);
  insane.AddOrSetValue( 2, second_map);

  Result := insane;
end;

function TTestServer.TTestHandlerImpl.testList(
  const thing: IThriftList<Integer>): IThriftList<Integer>;
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
  const thing: IThriftDictionary<Integer, Integer>): IThriftDictionary<Integer, Integer>;
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
  const arg2: Int64; const arg3: IThriftDictionary<SmallInt, string>;
  arg4: TNumberz; const arg5: Int64): IXtruct;
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

function TTestServer.TTestHandlerImpl.testMultiException( const arg0, arg1: string): IXtruct;
var
  x2 : TXception2;
begin
  Console.WriteLine('testMultiException(' + arg0 + ', ' + arg1 + ')');
  if ( arg0 = 'Xception') then
  begin
    raise TXception.Create( 1001, 'This is an Xception');  // test the new rich CTOR 
  end else
  if ( arg0 = 'Xception2') then
  begin
    x2 := TXception2.Create;  // the old way still works too?
    x2.ErrorCode := 2002;
    x2.Struct_thing := TXtructImpl.Create;
    x2.Struct_thing.String_thing := 'This is an Xception2';
    x2.UpdateMessageProperty;
    raise x2;
  end;

  Result := TXtructImpl.Create;
  Result.String_thing := arg1;
end;

function TTestServer.TTestHandlerImpl.testNest( const thing: IXtruct2): IXtruct2;
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
  const thing: IHashSet<Integer>):IHashSet<Integer>;
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

function TTestServer.TTestHandlerImpl.testString( const thing: string): string;
begin
  Console.WriteLine('teststring("' + thing + '")');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testStringMap(
  const thing: IThriftDictionary<string, string>): IThriftDictionary<string, string>;
var
  first : Boolean;
  key : string;
begin
  Console.Write('testStringMap({');
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
    Console.Write(key + ' => ' + thing[key]);
  end;
  Console.WriteLine('})');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testTypedef( const thing: Int64): Int64;
begin
  Console.WriteLine('testTypedef(' + IntToStr( thing) + ')');
  Result := thing;
end;

procedure TTestServer.TTestHandlerImpl.TestVoid;
begin
  Console.WriteLine('testVoid()');
end;

function TTestServer.TTestHandlerImpl.testStruct( const thing: IXtruct): IXtruct;
begin
  Console.WriteLine('testStruct({' +
    '"' + thing.String_thing + '", ' +
      IntToStr( thing.Byte_thing) + ', ' +
      IntToStr( thing.I32_thing) + ', ' +
      IntToStr( thing.I64_thing));
  Result := thing;
end;


{ TTestServer }


class procedure TTestServer.LaunchAnonPipeChild( const app : string; const transport : IAnonymousPipeServerTransport);
//Launch child process and pass R/W anonymous pipe handles on cmd line.
//This is a simple example and does not include elevation or other
//advanced features.
var pi : PROCESS_INFORMATION;
		si : STARTUPINFO;
		sArg, sHandles, sCmdLine : string;
    i : Integer;
begin
  GetStartupInfo( si);  //set startupinfo for the spawned process

  // preformat handles args
  sHandles := Format( '%d %d',
                    [ Integer(transport.ClientAnonRead),
                      Integer(transport.ClientAnonWrite)]);

  // pass all settings to client
  sCmdLine := app;
  for i := 1 to ParamCount do begin
    sArg := ParamStr(i);

    // add anonymous handles and quote strings where appropriate
    if sArg = '-anon'
    then sArg := sArg +' '+ sHandles
    else begin
      if Pos(' ',sArg) > 0
      then sArg := '"'+sArg+'"';
    end;;

    sCmdLine := sCmdLine +' '+ sArg;
  end;

  // spawn the child process
  Console.WriteLine('Starting client '+sCmdLine);
  Win32Check( CreateProcess( nil, PChar(sCmdLine), nil,nil,TRUE,0,nil,nil,si,pi));

  CloseHandle( pi.hThread);
	CloseHandle( pi.hProcess);
end;


class procedure TTestServer.Execute( const args: array of string);
var
  UseBufferedSockets : Boolean;
  UseFramed : Boolean;
  Port : Integer;
  AnonPipe : Boolean;
  sPipeName : string;
  testHandler : ITestHandler;
  testProcessor : IProcessor;
  ServerTrans : IServerTransport;
  ServerEngine : IServer;
  anonymouspipe : IAnonymousPipeServerTransport;
  namedpipe : INamedPipeServerTransport;
  TransportFactory : ITransportFactory;
  ProtocolFactory : IProtocolFactory;
  i : Integer;
  s : string;
  protType, p : TKnownProtocol;
const
  // pipe timeouts to be used
  DEBUG_TIMEOUT   = 30 * 1000;
  RELEASE_TIMEOUT = 0;  // server-side default
  TIMEOUT         = RELEASE_TIMEOUT;
begin
  try
    UseBufferedSockets := False;
    UseFramed := False;
    AnonPipe := FALSE;
    protType := prot_Binary;
    Port := 9090;
    sPipeName := '';

    i := 0;
    while ( i < Length(args) ) do begin
      s := args[i];
      Inc(i);

      if StrToIntDef( s, -1) > 0 then
      begin
        Port :=  StrToIntDef( s, Port);
      end
      else if ( s = 'raw' ) then
      begin
        // as default
      end
      else if ( s = 'buffered' ) then
      begin
        UseBufferedSockets := True;
      end
      else if ( s = 'framed' ) then
      begin
        UseFramed := True;
      end
      else if (s = '-pipe') then
      begin
        sPipeName := args[i];  // -pipe <name>
        Inc( i );
      end
      else if (s = '-anon') then
      begin
        AnonPipe := TRUE;
      end
      else if (s = '-prot') then  // -prot JSON|binary
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


    Console.WriteLine('Server configuration: ');

    // create protocol factory, default to BinaryProtocol
    case protType of
      prot_Binary:  ProtocolFactory := TBinaryProtocolImpl.TFactory.Create( BINARY_STRICT_READ, BINARY_STRICT_WRITE);
      prot_JSON  :  ProtocolFactory := TJSONProtocolImpl.TFactory.Create;
    else
      ASSERT( FALSE);  // unhandled case!
      ProtocolFactory := TBinaryProtocolImpl.TFactory.Create( BINARY_STRICT_READ, BINARY_STRICT_WRITE);
    end;
    ASSERT( ProtocolFactory <> nil);
    Console.WriteLine('- '+KNOWN_PROTOCOLS[protType]+' protocol');


    if sPipeName <> '' then begin
      Console.WriteLine('- named pipe ('+sPipeName+')');
      namedpipe   := TNamedPipeServerTransportImpl.Create( sPipeName, 4096, PIPE_UNLIMITED_INSTANCES, TIMEOUT);
      servertrans := namedpipe;
    end
    else if AnonPipe then begin
      Console.WriteLine('- anonymous pipes');
      anonymouspipe := TAnonymousPipeServerTransportImpl.Create;
      servertrans   := anonymouspipe;
    end
    else begin
      Console.WriteLine('- sockets (port '+IntToStr(port)+')');
      if UseBufferedSockets then Console.WriteLine('- buffered sockets');
      servertrans := TServerSocketImpl.Create( Port, 0, UseBufferedSockets);
    end;
    ASSERT( servertrans <> nil);

    if UseFramed then begin
      Console.WriteLine('- framed transport');
      TransportFactory := TFramedTransportImpl.TFactory.Create
    end
    else begin
      TransportFactory := TTransportFactoryImpl.Create;
    end;
    ASSERT( TransportFactory <> nil);

    testHandler   := TTestHandlerImpl.Create;
    testProcessor := TThriftTest.TProcessorImpl.Create( testHandler );

    ServerEngine := TSimpleServer.Create( testProcessor,
                                          ServerTrans,
                                          TransportFactory,
                                          ProtocolFactory);

    testHandler.SetServer( ServerEngine);

    // start the client now when we have the anon handles, but before the server starts
    if AnonPipe
    then LaunchAnonPipeChild( ExtractFilePath(ParamStr(0))+'client.exe', anonymouspipe);

    // install Ctrl+C handler before the server starts
    g_Handler := testHandler;
    SetConsoleCtrlHandler( @MyConsoleEventHandler, TRUE);

    Console.WriteLine('');
    repeat
      Console.WriteLine('Starting the server ...');
      serverEngine.Serve;
    until {$IFDEF RunEndless} FALSE {$ELSE} TRUE {$ENDIF};

    testHandler.SetServer( nil);
    g_Handler := nil;

  except
    on E: Exception do
    begin
      Console.Write( E.Message);
    end;
  end;
  Console.WriteLine( 'done.');
end;


end.
