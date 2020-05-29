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

{$I ../src/Thrift.Defines.inc}
{$WARN SYMBOL_PLATFORM OFF}

{.$DEFINE RunEndless}   // activate to interactively stress-test the server stop routines via Ctrl+C

interface

uses
  Windows, SysUtils,
  Generics.Collections,
  Thrift.Server,
  Thrift.Transport,
  Thrift.Transport.Pipes,
  Thrift.Protocol,
  Thrift.Protocol.JSON,
  Thrift.Protocol.Compact,
  Thrift.Collections,
  Thrift.Configuration,
  Thrift.Utils,
  Thrift.Test,
  Thrift,
  TestConstants,
  TestServerEvents,
  ConsoleHelper,
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
      strict private
        FServer : IServer;
      strict protected
        procedure testVoid();
        function testBool(thing: Boolean): Boolean;
        function testString(const thing: string): string;
        function testByte(thing: ShortInt): ShortInt;
        function testI32(thing: Integer): Integer;
        function testI64(const thing: Int64): Int64;
        function testDouble(const thing: Double): Double;
        function testBinary(const thing: TBytes): TBytes;
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

      class procedure PrintCmdLineHelp;
      class procedure InvalidArgs;
      class function  IsSwitch( const aArgument, aSwitch : string; out sValue : string) : Boolean;

      class procedure LaunchAnonPipeChild( const app : string; const transport : IAnonymousPipeServerTransport);
      class procedure Execute( const arguments : array of string);
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

function TTestServer.TTestHandlerImpl.testBinary(const thing: TBytes): TBytes;
begin
  Console.WriteLine('testBinary('+IntToStr(Length(thing)) + ' bytes)');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testEnum(thing: TNumberz): TNumberz;
begin
  Console.WriteLine('testEnum(' + EnumUtils<TNumberz>.ToString(Ord(thing)) + ')');
  Result := thing;
end;

procedure TTestServer.TTestHandlerImpl.testException(const arg: string);
begin
  Console.WriteLine('testException(' + arg + ')');
  if ( arg = 'Xception') then begin
    raise TXception.Create( 1001, arg);
  end;

  if (arg = 'TException') then begin
    raise TException.Create('TException');
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
  looney : IInsanity;
  first_map : IThriftDictionary<TNumberz, IInsanity>;
  second_map : IThriftDictionary<TNumberz, IInsanity>;
  insane : IThriftDictionary<Int64, IThriftDictionary<TNumberz, IInsanity>>;

begin
  Console.Write('testInsanity(');
  if argument <> nil then Console.Write(argument.ToString);
  Console.WriteLine(')');


  (**
   * So you think you've got this all worked, out eh?
   *
   * Creates a the returned map with these values and prints it out:
   *   { 1 => { 2 => argument,
   *            3 => argument,
   *          },
   *     2 => { 6 => <empty Insanity struct>, },
   *   }
   * @return map<UserId, map<Numberz,Insanity>> - a map with the above values
   *)

  first_map := TThriftDictionaryImpl<TNumberz, IInsanity>.Create;
  second_map := TThriftDictionaryImpl<TNumberz, IInsanity>.Create;

  first_map.AddOrSetValue( TNumberz.TWO, argument);
  first_map.AddOrSetValue( TNumberz.THREE, argument);

  looney := TInsanityImpl.Create;
  second_map.AddOrSetValue( TNumberz.SIX, looney);

  insane := TThriftDictionaryImpl<Int64, IThriftDictionary<TNumberz, IInsanity>>.Create;

  insane.AddOrSetValue( 1, first_map);
  insane.AddOrSetValue( 2, second_map);

  Result := insane;
end;

function TTestServer.TTestHandlerImpl.testList( const thing: IThriftList<Integer>): IThriftList<Integer>;
begin
  Console.Write('testList(');
  if thing <> nil then Console.Write(thing.ToString);
  Console.WriteLine(')');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testMap(
  const thing: IThriftDictionary<Integer, Integer>): IThriftDictionary<Integer, Integer>;
begin
  Console.Write('testMap(');
  if thing <> nil then Console.Write(thing.ToString);
  Console.WriteLine(')');
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
  if ( arg0 = 'Xception') then begin
    raise TXception.Create( 1001, 'This is an Xception');  // test the new rich CTOR
  end;

  if ( arg0 = 'Xception2') then begin
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
begin
  Console.Write('testNest(');
  if thing <> nil then Console.Write(thing.ToString);
  Console.WriteLine(')');

  Result := thing;
end;

procedure TTestServer.TTestHandlerImpl.testOneway(secondsToSleep: Integer);
begin
  Console.WriteLine('testOneway(' + IntToStr( secondsToSleep )+ '), sleeping...');
  Sleep(secondsToSleep * 1000);
  Console.WriteLine('testOneway finished');
end;

function TTestServer.TTestHandlerImpl.testSet( const thing: IHashSet<Integer>):IHashSet<Integer>;
begin
  Console.Write('testSet(');
  if thing <> nil then Console.Write(thing.ToString);
  Console.WriteLine(')');;

  Result := thing;
end;

procedure TTestServer.TTestHandlerImpl.testStop;
begin
  if FServer <> nil then begin
    FServer.Stop;
  end;
end;

function TTestServer.TTestHandlerImpl.testBool(thing: Boolean): Boolean;
begin
  Console.WriteLine('testBool(' + BoolToStr(thing,true) + ')');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testString( const thing: string): string;
begin
  Console.WriteLine('teststring("' + thing + '")');
  Result := thing;
end;

function TTestServer.TTestHandlerImpl.testStringMap(
  const thing: IThriftDictionary<string, string>): IThriftDictionary<string, string>;
begin
  Console.Write('testStringMap(');
  if thing <> nil then Console.Write(thing.ToString);
  Console.WriteLine(')');

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
  Console.Write('testStruct(');
  if thing <> nil then Console.Write(thing.ToString);
  Console.WriteLine(')');

  Result := thing;
end;


{ TTestServer }


class procedure TTestServer.PrintCmdLineHelp;
const HELPTEXT = ' [options]'#10
               + #10
               + 'Allowed options:'#10
               + '  -h | --help                   Produces this help message'#10
               + '  --port=arg (9090)             Port number to connect'#10
               + '  --pipe=arg                    Windows Named Pipe (e.g. MyThriftPipe)'#10
               + '  --anon-pipes                  Windows Anonymous Pipes server, auto-starts client.exe'#10
               + '  --server-type=arg (simple)    Type of server (simple, thread-pool, threaded, nonblocking)'#10
               + '  --transport=arg (sockets)     Transport: buffered, framed, anonpipe'#10
               + '  --protocol=arg (binary)       Protocol: binary, compact, json'#10
               + '  --ssl                         Encrypted Transport using SSL'#10
               + '  --processor-events            Enable processor-events'#10
               + '  -n=num | --workers=num (4)    Number of thread-pool server workers'#10
               ;
begin
  Console.WriteLine( ChangeFileExt(ExtractFileName(ParamStr(0)),'') + HELPTEXT);
end;

class procedure TTestServer.InvalidArgs;
begin
  Console.WriteLine( 'Invalid args.');
  Console.WriteLine( ChangeFileExt(ExtractFileName(ParamStr(0)),'') + ' -h for more information');
  Abort;
end;

class function TTestServer.IsSwitch( const aArgument, aSwitch : string; out sValue : string) : Boolean;
begin
  sValue := '';
  result := (Copy( aArgument, 1, Length(aSwitch)) = aSwitch);
  if result then begin
    if (Copy( aArgument, 1, Length(aSwitch)+1) = (aSwitch+'='))
    then sValue := Copy( aArgument, Length(aSwitch)+2, MAXINT);
  end;
end;

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
    if sArg = '--anon-pipes'
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


class procedure TTestServer.Execute( const arguments : array of string);
var
  Port : Integer;
  ServerEvents : Boolean;
  sPipeName : string;
  testHandler : ITestHandler;
  testProcessor : IProcessor;
  ServerTrans : IServerTransport;
  ServerEngine : IServer;
  anonymouspipe : IAnonymousPipeServerTransport;
  namedpipe : INamedPipeServerTransport;
  TransportFactory : ITransportFactory;
  ProtocolFactory : IProtocolFactory;
  iArg, numWorker : Integer;
  sArg, sValue : string;
  protType : TKnownProtocol;
  servertype : TServerType;
  endpoint : TEndpointTransport;
  layered : TLayeredTransports;
  UseSSL : Boolean; // include where appropriate (TLayeredTransport?)
begin
  try
    ServerEvents := FALSE;
    protType := prot_Binary;
    servertype := srv_Simple;
    endpoint := trns_Sockets;
    layered := [];
    UseSSL := FALSE;
    Port := 9090;
    sPipeName := '';
    numWorker := 4;

    iArg := 0;
    while iArg < Length(arguments) do begin
      sArg := arguments[iArg];
      Inc(iArg);

      // Allowed options:
      if IsSwitch( sArg, '-h', sValue)
      or IsSwitch( sArg, '--help', sValue)
      then begin
        // -h | --help               produce help message
        PrintCmdLineHelp;
        Exit;
      end
      else if IsSwitch( sArg, '--port', sValue) then begin
        // --port arg (=9090)          Port number to listen
        Port := StrToIntDef( sValue, Port);
      end
      else if IsSwitch( sArg, '--anon-pipes', sValue) then begin
        endpoint := trns_AnonPipes;
      end
      else if IsSwitch( sArg, '--pipe', sValue) then begin
        // --pipe arg                   Windows Named Pipe (e.g. MyThriftPipe)
        endpoint := trns_NamedPipes;
        sPipeName := sValue;  // --pipe <name>
      end
      else if IsSwitch( sArg, '--server-type', sValue) then begin
        // --server-type arg (=simple) type of server,
        // arg = "simple", "thread-pool", "threaded", or "nonblocking"
        if      sValue = 'simple'      then servertype := srv_Simple
        else if sValue = 'thread-pool' then servertype := srv_Threadpool
        else if sValue = 'threaded'    then servertype := srv_Threaded
        else if sValue = 'nonblocking' then servertype := srv_Nonblocking
        else InvalidArgs;
      end
      else if IsSwitch( sArg, '--transport', sValue) then begin
        // --transport arg (=buffered) transport: buffered, framed, http
        if      sValue = 'buffered' then Include( layered, trns_Buffered)
        else if sValue = 'framed'   then Include( layered, trns_Framed)
        else if sValue = 'http'     then endpoint := trns_MsxmlHttp
        else if sValue = 'winhttp'  then endpoint := trns_WinHttp
        else if sValue = 'anonpipe' then endpoint := trns_AnonPipes
        else InvalidArgs;
      end
      else if IsSwitch( sArg, '--protocol', sValue) then begin
        // --protocol arg (=binary)    protocol: binary, compact, json
        if      sValue = 'binary'   then protType := prot_Binary
        else if sValue = 'compact'  then protType := prot_Compact
        else if sValue = 'json'     then protType := prot_JSON
        else InvalidArgs;
      end
      else if IsSwitch( sArg, '--ssl', sValue) then begin
        // --ssl     Encrypted Transport using SSL
        UseSSL := TRUE;
      end
      else if IsSwitch( sArg, '--processor-events', sValue) then begin
         // --processor-events          processor-events
        ServerEvents := TRUE;
      end
      else if IsSwitch( sArg, '-n', sValue) or IsSwitch( sArg, '--workers', sValue) then begin
        // -n [ --workers ] arg (=4)   Number of thread pools workers.
        // Only valid for thread-pool server type
        numWorker := StrToIntDef(sValue,4);
      end
      else begin
        InvalidArgs;
      end;
    end;


    Console.WriteLine('Server configuration: ');

    // create protocol factory, default to BinaryProtocol
    case protType of
      prot_Binary  :  ProtocolFactory := TBinaryProtocolImpl.TFactory.Create( BINARY_STRICT_READ, BINARY_STRICT_WRITE);
      prot_JSON    :  ProtocolFactory := TJSONProtocolImpl.TFactory.Create;
      prot_Compact :  ProtocolFactory := TCompactProtocolImpl.TFactory.Create;
    else
      raise Exception.Create('Unhandled protocol');
    end;
    ASSERT( ProtocolFactory <> nil);
    Console.WriteLine('- '+THRIFT_PROTOCOLS[protType]+' protocol');

    case endpoint of

      trns_Sockets : begin
        Console.WriteLine('- sockets (port '+IntToStr(port)+')');
        if (trns_Buffered in layered) then Console.WriteLine('- buffered');
        servertrans := TServerSocketImpl.Create( Port, DEFAULT_THRIFT_TIMEOUT, (trns_Buffered in layered));
      end;

      trns_MsxmlHttp,
      trns_WinHttp : begin
        raise Exception.Create('HTTP server transport not implemented');
      end;

      trns_NamedPipes : begin
        Console.WriteLine('- named pipe ('+sPipeName+')');
        namedpipe   := TNamedPipeServerTransportImpl.Create( sPipeName, 4096, PIPE_UNLIMITED_INSTANCES, INFINITE);
        servertrans := namedpipe;
      end;

      trns_AnonPipes : begin
        Console.WriteLine('- anonymous pipes');
        anonymouspipe := TAnonymousPipeServerTransportImpl.Create;
        servertrans   := anonymouspipe;
      end

    else
      raise Exception.Create('Unhandled endpoint transport');
    end;
    ASSERT( servertrans <> nil);

    if UseSSL then begin
      raise Exception.Create('SSL not implemented');
    end;

    if (trns_Framed in layered) then begin
      Console.WriteLine('- framed transport');
      TransportFactory := TFramedTransportImpl.TFactory.Create;
    end
    else begin
      TransportFactory := TTransportFactoryImpl.Create;
    end;
    ASSERT( TransportFactory <> nil);

    testHandler   := TTestHandlerImpl.Create;
    testProcessor := TThriftTest.TProcessorImpl.Create( testHandler );

    case servertype of
      srv_Simple      : begin
        ServerEngine := TSimpleServer.Create( testProcessor, ServerTrans, TransportFactory, ProtocolFactory);
      end;

      srv_Nonblocking : begin
        raise Exception.Create(SERVER_TYPES[servertype]+' server not implemented');
      end;

      srv_Threadpool,
      srv_Threaded: begin
        if numWorker > 1 then {use here};
        raise Exception.Create(SERVER_TYPES[servertype]+' server not implemented');
      end;

    else
      raise Exception.Create('Unhandled server type');
    end;
    ASSERT( ServerEngine <> nil);

    testHandler.SetServer( ServerEngine);

    // test events?
    if ServerEvents then begin
      Console.WriteLine('- server events test enabled');
      ServerEngine.ServerEvents := TServerEventsImpl.Create;
    end;

    // start the client now when we have the anon handles, but before the server starts
    if endpoint = trns_AnonPipes
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
    on E: EAbort do raise;
    on E: Exception do begin
      Console.WriteLine( E.Message + #10 + E.StackTrace );
    end;
  end;
  Console.WriteLine( 'done.');
end;


end.
