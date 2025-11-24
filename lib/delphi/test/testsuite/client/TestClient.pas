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

{$I ../src/Thrift.Defines.inc}

{.$DEFINE StressTest}   // activate to stress-test the server with frequent connects/disconnects
{.$DEFINE PerfTest}     // activate the performance test
{$DEFINE Exceptions}    // activate the exceptions test (or disable while debugging)

{$if CompilerVersion >= 28}
{$DEFINE SupportsAsync}
{$ifend}

{$WARN SYMBOL_PLATFORM OFF}  // Win32Check

interface

uses
  Classes, Windows, SysUtils, Math, ActiveX, ComObj,
  {$IFDEF SupportsAsync} System.Threading, {$ENDIF}
  DateUtils,
  Generics.Collections,
  TestConstants,
  TestLogger,
  ConsoleHelper,
  PerfTests,
  UnitTests,
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
  TClientThread = class;

  TThreadConsole = class(TThriftConsole)
  strict private
    FThread : TClientThread;
    FLogThreadID : Boolean;
  public
    constructor Create( const aThread: TClientThread; const aLogThreadID : Boolean);

    procedure Write( const S: string); override;
    procedure WriteLine( const S: string); override;
  end;

  TTestSetup = record
    protType  : TKnownProtocol;
    endpoint  : TEndpointTransport;
    layered   : TLayeredTransports;
    useSSL    : Boolean; // include where appropriate (TLayeredTransport?)
    host      : string;
    port      : Integer;
    sPipeName : string;
    hAnonRead, hAnonWrite : THandle;
  end;

  TClientThread = class( TThread )
  strict private
    FSetup : TTestSetup;
    FTransport : ITransport;
    FProtocol : IProtocol;
    FNumIterations : Integer;

    FThreadNo : Integer;
    FConsole : TThreadConsole;
    FLogger : ITestLogger;

    procedure ClientTest;
    {$IFDEF SupportsAsync}
    procedure ClientAsyncTest;
    {$ENDIF}

    procedure InitializeProtocolTransportStack;
    procedure ShutdownProtocolTransportStack;
    function  InitializeHttpTransport( const aTimeoutSetting : Integer; const aConfig : IThriftConfiguration = nil) : IHTTPClient;

    {$IFDEF StressTest}
    procedure StressTest(const client : TThriftTest.Iface);
    {$ENDIF}

    procedure StartTestGroup( const aGroup : string; const aTest : TClientTestGroup); inline;
    procedure Expect( aTestResult : Boolean; const aTestInfo : string); inline;
    function CalculateExitCode : Byte;

  strict protected
    procedure Execute; override;
    property Console : TThreadConsole read FConsole;

  public
    constructor Create( const aSetup : TTestSetup; const aNumIteration, aThreadNo: Integer; const aLogThreadID : Boolean);
    destructor Destroy; override;

    property ThreadNo : Integer read FThreadNo;
  end;

  TTestClient = class
  private
    class var
      FNumIterations : Integer;
      FNumThreads : Integer;

    class procedure PrintCmdLineHelp;
    class procedure InvalidArgs;
  public
    class function Execute( const arguments: array of string) : Byte;
  end;


implementation

const
   EXITCODE_SUCCESS           = $00;  // no errors bits set
   //
   EXITCODE_FAILBIT_BASETYPES  = $01;
   EXITCODE_FAILBIT_STRUCTS    = $02;
   EXITCODE_FAILBIT_CONTAINERS = $04;
   EXITCODE_FAILBIT_EXCEPTIONS = $08;

   MAP_FAILURES_TO_EXITCODE_BITS : array[TClientTestGroup] of Byte = (
     EXITCODE_SUCCESS,  // no bits here
     EXITCODE_FAILBIT_BASETYPES,
     EXITCODE_FAILBIT_STRUCTS,
     EXITCODE_FAILBIT_CONTAINERS,
     EXITCODE_FAILBIT_EXCEPTIONS
   );



function BoolToString( b : Boolean) : string;
// overrides global BoolToString()
begin
  if b
  then result := 'true'
  else result := 'false';
end;

// not available in all versions, so make sure we have this one imported
function IsDebuggerPresent: BOOL; stdcall; external KERNEL32 name 'IsDebuggerPresent';

{ TTestClient }

class procedure TTestClient.PrintCmdLineHelp;
const HELPTEXT = ' [options]'#10
               + #10
               + 'Allowed options:'#10
               + '  -h | --help                   Produces this help message'#10
               + '  --host=arg (localhost)        Host to connect'#10
               + '  --port=arg (9090)             Port number to connect'#10
               + '  --pipe=arg                    Windows Named Pipe (e.g. MyThriftPipe)'#10
               + '  --anon-pipes hRead hWrite     Windows Anonymous Pipes pair (handles)'#10
               + '  --transport=arg (sockets)     Transport: buffered, framed, http, winhttp'#10
               + '  --protocol=arg (binary)       Protocol: binary, compact, json'#10
               + '  --ssl                         Encrypted Transport using SSL'#10
               + '  -n=num | --testloops=num (1)  Number of Tests'#10
               + '  -t=num | --threads=num (1)    Number of Test threads'#10
               + '  --performance                 Run the built-in performance test (no other arguments)'#10
               ;
begin
  Writeln( ChangeFileExt(ExtractFileName(ParamStr(0)),'') + HELPTEXT);
end;

class procedure TTestClient.InvalidArgs;
begin
  Console.WriteLine( 'Invalid args.');
  Console.WriteLine( ChangeFileExt(ExtractFileName(ParamStr(0)),'') + ' -h for more information');
  Abort;
end;

class function TTestClient.Execute(const arguments: array of string) : Byte;

  function IsSwitch( const aArgument, aSwitch : string; out sValue : string) : Boolean;
  begin
    sValue := '';
    result := (Copy( aArgument, 1, Length(aSwitch)) = aSwitch);
    if result then begin
      if (Copy( aArgument, 1, Length(aSwitch)+1) = (aSwitch+'='))
      then sValue := Copy( aArgument, Length(aSwitch)+2, MAXINT);
    end;
  end;

var
  iArg : Integer;
  threadExitCode : Byte;
  sArg, sValue : string;
  threads : array of TThread;
  dtStart : TDateTime;
  test : Integer;
  thread : TThread;
  setup : TTestSetup;
begin
  // init record
  with setup do begin
    protType   := prot_Binary;
    endpoint   := trns_Sockets;
    layered    := [];
    useSSL     := FALSE;
    host       := 'localhost';
    port       := 9090;
    sPipeName  := '';
    hAnonRead  := INVALID_HANDLE_VALUE;
    hAnonWrite := INVALID_HANDLE_VALUE;
  end;

  try
    iArg := 0;
    while iArg < Length(arguments) do begin
      sArg := arguments[iArg];
      Inc(iArg);

      if IsSwitch( sArg, '-h', sValue)
      or IsSwitch( sArg, '--help', sValue)
      then begin
        // -h [ --help ]               produce help message
        PrintCmdLineHelp;
        result := $FF;   // all tests failed
        Exit;
      end
      else if IsSwitch( sArg, '--host', sValue) then begin
        // --host arg (=localhost)     Host to connect
        setup.host := sValue;
      end
      else if IsSwitch( sArg, '--port', sValue) then begin
        // --port arg (=9090)          Port number to connect
        setup.port := StrToIntDef(sValue,0);
        if setup.port <= 0 then InvalidArgs;
      end
      else if IsSwitch( sArg, '--domain-socket', sValue) then begin
        // --domain-socket arg         Domain Socket (e.g. /tmp/ThriftTest.thrift), instead of host and port
        raise Exception.Create('domain-socket not supported');
      end
        // --pipe arg                 Windows Named Pipe (e.g. MyThriftPipe)
      else if IsSwitch( sArg, '--pipe', sValue) then begin
        // --pipe arg                 Windows Named Pipe (e.g. MyThriftPipe)
        setup.endpoint := trns_NamedPipes;
        setup.sPipeName := sValue;
        Console.WriteLine('Using named pipe ('+setup.sPipeName+')');
      end
      else if IsSwitch( sArg, '--anon-pipes', sValue) then begin
        // --anon-pipes hRead hWrite   Windows Anonymous Pipes pair (handles)
        setup.endpoint := trns_AnonPipes;
        setup.hAnonRead := THandle( StrToIntDef( arguments[iArg], Integer(INVALID_HANDLE_VALUE)));
        Inc(iArg);
        setup.hAnonWrite := THandle( StrToIntDef( arguments[iArg], Integer(INVALID_HANDLE_VALUE)));
        Inc(iArg);
        Console.WriteLine('Using anonymous pipes ('+IntToStr(Integer(setup.hAnonRead))+' and '+IntToStr(Integer(setup.hAnonWrite))+')');
      end
      else if IsSwitch( sArg, '--transport', sValue) then begin
        // --transport arg (=sockets)  Transport: buffered, framed, http, winhttp, evhttp
        if      sValue = 'buffered' then Include( setup.layered, trns_Buffered)
        else if sValue = 'framed'   then Include( setup.layered, trns_Framed)
        else if sValue = 'http'     then setup.endpoint := trns_MsXmlHttp
        else if sValue = 'winhttp'  then setup.endpoint := trns_WinHttp
        else if sValue = 'evhttp'   then setup.endpoint := trns_EvHttp  // recognized, but not supported
        else InvalidArgs;
      end
      else if IsSwitch( sArg, '--protocol', sValue) then begin
        // --protocol arg (=binary)    Protocol: binary, compact, json
        if      sValue = 'binary'   then setup.protType := prot_Binary
        else if sValue = 'compact'  then setup.protType := prot_Compact
        else if sValue = 'json'     then setup.protType := prot_JSON
        else InvalidArgs;
      end
      else if IsSwitch( sArg, '--ssl', sValue) then begin
        // --ssl                       Encrypted Transport using SSL
        setup.useSSL := TRUE;

      end
      else if IsSwitch( sArg, '-n', sValue) or IsSwitch( sArg, '--testloops', sValue) then begin
        // -n [ --testloops ] arg (=1) Number of Tests
        FNumIterations := StrToIntDef( sValue, 0);
        if FNumIterations <= 0
        then InvalidArgs;

      end
      else if IsSwitch( sArg, '-t', sValue) or IsSwitch( sArg, '--threads', sValue) then begin
        // -t [ --threads ] arg (=1)   Number of Test threads
        FNumThreads := StrToIntDef( sValue, 0);
        if FNumThreads <= 0
        then InvalidArgs;
      end
      else if IsSwitch( sArg, '--performance', sValue) then begin
        result := TPerformanceTests.Execute;
        Exit;
      end
      else begin
        InvalidArgs;
      end;
    end;


    // In the anonymous pipes mode the client is launched by the test server
    // -> behave nicely and allow for attaching a debugger to this process
    if (setup.endpoint = trns_AnonPipes) and not IsDebuggerPresent
    then MessageBox( 0, 'Attach Debugger and/or click OK to continue.',
                        'Thrift TestClient (Delphi)',
                        MB_OK or MB_ICONEXCLAMATION);

    SetLength( threads, FNumThreads);
    dtStart := Now;

    // layered transports are not really meant to be stacked upon each other
    if (trns_Framed in setup.layered) then begin
      Console.WriteLine('Using framed transport');
    end
    else if (trns_Buffered in setup.layered) then begin
      Console.WriteLine('Using buffered transport');
    end;

    Console.WriteLine(THRIFT_PROTOCOLS[setup.protType]+' protocol');

    if FNumThreads <> 1
    then Console.WriteLine(IntToStr(FNumThreads)+' client threads');

    if FNumIterations <> 1
    then Console.WriteLine(IntToStr(FNumIterations)+' iterations');

    for test := 0 to FNumThreads - 1 do begin
      thread := TClientThread.Create( setup, FNumIterations, test, FNumThreads<>1);
      threads[test] := thread;
      thread.Start;
    end;

    result := 0;
    for test := 0 to FNumThreads - 1 do begin
      threadExitCode := threads[test].WaitFor;
      result := result or threadExitCode;
      threads[test].Free;
      threads[test] := nil;
    end;

    Console.Write('Total time: ' + IntToStr( MilliSecondsBetween(Now, dtStart)));

  except
    on E: EAbort do raise;
    on E: Exception do begin
      Console.WriteLine( E.Message + #10 + E.StackTrace);
      raise;
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
  binOut,binIn : TBytes;
  guidIn, guidOut : TGuid;
  dub : Double;
  o : IXtruct;
  o2 : IXtruct2;
  i : IXtruct;
  i2 : IXtruct2;
  mapout : IThriftDictionary<Integer,Integer>;
  mapin : IThriftDictionary<Integer,Integer>;
  strmapout : IThriftDictionary<string,string>;
  strmapin : IThriftDictionary<string,string>;
  j : Integer;
  first : Boolean;
  key : Integer;
  strkey : string;
  listout : IThriftList<Integer>;
  listin : IThriftList<Integer>;
  setout : IThriftHashSet<Integer>;
  setin : IThriftHashSet<Integer>;
  ret : TNumberz;
  uid : Int64;
  mm : IThriftDictionary<Integer, IThriftDictionary<Integer, Integer>>;
  pos : IThriftDictionary<Integer, Integer>;
  neg : IThriftDictionary<Integer, Integer>;
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
  arg3 : IThriftDictionary<SmallInt, string>;
  arg4 : TNumberz;
  arg5 : Int64;
  {$IFDEF PerfTest}
  StartTick : Cardinal;
  k : Integer;
  {$ENDIF}
  hello, goodbye : IXtruct;
  crazy : IInsanity;
  looney : IInsanity;
  first_map : IThriftDictionary<TNumberz, IInsanity>;
  second_map : IThriftDictionary<TNumberz, IInsanity>;
  pair : TPair<TNumberz, TUserId>;
  testsize : TTestSize;
begin
  client := TThriftTest.TClient.Create( FProtocol);
  FTransport.Open;

  {$IFDEF StressTest}
  StressTest( client);
  {$ENDIF StressTest}

  {$IFDEF Exceptions}
  // in-depth exception test
  // (1) do we get an exception at all?
  // (2) do we get the right exception?
  // (3) does the exception contain the expected data?
  StartTestGroup( 'testException', test_Exceptions);
  // case 1: exception type declared in IDL at the function call
  try
    client.testException('Xception');
    Expect( FALSE, 'testException(''Xception''): must trow an exception');
  except
    on e:TXception do begin
      Expect( e.ErrorCode = 1001,       'error code');
      Expect( e.Message_  = 'Xception', 'error message');
      Console.WriteLine( ' = ' + IntToStr(e.ErrorCode) + ', ' + e.Message_ );
    end;
    on e:TTransportException do Expect( FALSE, 'Unexpected : "'+e.ToString+'"');
    on e:Exception do Expect( FALSE, 'Unexpected exception "'+e.ClassName+'": '+e.Message);
  end;

  // re-open connection if needed
  if not FTransport.IsOpen
  then FTransport.Open;

  // case 2: exception type NOT declared in IDL at the function call
  // this will close the connection
  try
    client.testException('TException');
    Expect( FALSE, 'testException(''TException''): must trow an exception');
  except
    on e:TTransportException do begin
      Console.WriteLine( e.ClassName+' = '+e.Message); // this is what we get
    end;
    on e:TApplicationException do begin
      Console.WriteLine( e.ClassName+' = '+e.Message); // this is what we get
    end;
    on e:TException do Expect( FALSE, 'Unexpected exception "'+e.ClassName+'": '+e.Message);
    on e:Exception do Expect( FALSE, 'Unexpected exception "'+e.ClassName+'": '+e.Message);
  end;


  if FTransport.IsOpen then FTransport.Close;
  FTransport.Open;   // re-open connection, server has already closed


  // case 3: no exception
  try
    client.testException('something');
    Expect( TRUE, 'testException(''something''): must not trow an exception');
  except
    on e:TTransportException do Expect( FALSE, 'Unexpected : "'+e.ToString+'"');
    on e:Exception do Expect( FALSE, 'Unexpected exception "'+e.ClassName+'": '+e.Message);
  end;
  {$ENDIF Exceptions}

  // re-open connection if needed
  if not FTransport.IsOpen
  then FTransport.Open;

  // simple things
  StartTestGroup( 'simple Thrift calls', test_BaseTypes);
  client.testVoid();
  Expect( TRUE, 'testVoid()');  // success := no exception

  s := BoolToString( client.testBool(TRUE));
  Expect( s = BoolToString(TRUE),  'testBool(TRUE) = '+s);
  s := BoolToString( client.testBool(FALSE));
  Expect( s = BoolToString(FALSE),  'testBool(FALSE) = '+s);

  s := client.testString('Test');
  Expect( s = 'Test', 'testString(''Test'') = "'+s+'"');

  s := client.testString('');  // empty string
  Expect( s = '', 'testString('''') = "'+s+'"');

  s := client.testString(HUGE_TEST_STRING);
  Expect( length(s) = length(HUGE_TEST_STRING),
          'testString( length(HUGE_TEST_STRING) = '+IntToStr(Length(HUGE_TEST_STRING))+') '
         +'=> length(result) = '+IntToStr(Length(s)));

  i8 := client.testByte(1);
  Expect( i8 = 1, 'testByte(1) = ' + IntToStr( i8 ));

  i32 := client.testI32(-1);
  Expect( i32 = -1, 'testI32(-1) = ' + IntToStr(i32));

  Console.WriteLine('testI64(-34359738368)');
  i64 := client.testI64(-34359738368);
  Expect( i64 = -34359738368, 'testI64(-34359738368) = ' + IntToStr( i64));

  guidOut := StringToGUID('{00112233-4455-6677-8899-AABBCCDDEEFF}');
  Console.WriteLine('testUuid('+GUIDToString(guidOut)+')');
  try
    guidIn := client.testUuid(guidOut);
    Expect( IsEqualGUID(guidIn, guidOut), 'testUuid('+GUIDToString(guidOut)+') = '+GUIDToString(guidIn));
  except
    on e:TApplicationException do Console.WriteLine('testUuid(): '+e.Message);
    on e:Exception do Expect( FALSE, 'testUuid(): Unexpected exception "'+e.ClassName+'": '+e.Message);
  end;

  // random binary small
  for testsize := Low(TTestSize) to High(TTestSize) do begin
    binOut := PrepareBinaryData( TRUE, testsize);
    Console.WriteLine('testBinary('+IntToStr(Length(binOut))+' bytes)');
    try
      binIn := client.testBinary(binOut);
      Expect( Length(binOut) = Length(binIn), 'testBinary('+IntToStr(Length(binOut))+' bytes): '+IntToStr(Length(binIn))+' bytes received');
      i32 := Min( Length(binOut), Length(binIn));
      Expect( CompareMem( binOut, binIn, i32), 'testBinary('+IntToStr(Length(binOut))+' bytes): validating received data');
    except
      on e:TApplicationException do Console.WriteLine('testBinary(): '+e.Message);
      on e:Exception do Expect( FALSE, 'testBinary(): Unexpected exception "'+e.ClassName+'": '+e.Message);
    end;
  end;

  Console.WriteLine('testDouble(5.325098235)');
  dub := client.testDouble(5.325098235);
  Expect( abs(dub-5.325098235) < 1e-14, 'testDouble(5.325098235) = ' + FloatToStr( dub));

  // structs
  StartTestGroup( 'testStruct', test_Structs);
  Console.WriteLine('testStruct({''Zero'', 1, -3, -5})');
  o := TXtructImpl.Create;
  o.String_thing := 'Zero';
  o.Byte_thing := 1;
  o.I32_thing := -3;
  o.I64_thing := -5;
  i := client.testStruct(o);
  Expect( i.String_thing = 'Zero', 'i.String_thing = "'+i.String_thing+'"');
  Expect( i.Byte_thing = 1, 'i.Byte_thing = '+IntToStr(i.Byte_thing));
  Expect( i.I32_thing = -3, 'i.I32_thing = '+IntToStr(i.I32_thing));
  Expect( i.I64_thing = -5, 'i.I64_thing = '+IntToStr(i.I64_thing));
  Expect( i.__isset_String_thing, 'i.__isset_String_thing = '+BoolToString(i.__isset_String_thing));
  Expect( i.__isset_Byte_thing, 'i.__isset_Byte_thing = '+BoolToString(i.__isset_Byte_thing));
  Expect( i.__isset_I32_thing, 'i.__isset_I32_thing = '+BoolToString(i.__isset_I32_thing));
  Expect( i.__isset_I64_thing, 'i.__isset_I64_thing = '+BoolToString(i.__isset_I64_thing));

  // nested structs
  StartTestGroup( 'testNest', test_Structs);
  Console.WriteLine('testNest({1, {''Zero'', 1, -3, -5}, 5})');
  o2 := TXtruct2Impl.Create;
  o2.Byte_thing := 1;
  o2.Struct_thing := o;
  o2.I32_thing := 5;
  i2 := client.testNest(o2);
  i := i2.Struct_thing;
  Expect( i.String_thing = 'Zero', 'i.String_thing = "'+i.String_thing+'"');
  Expect( i.Byte_thing = 1,  'i.Byte_thing = '+IntToStr(i.Byte_thing));
  Expect( i.I32_thing = -3,  'i.I32_thing = '+IntToStr(i.I32_thing));
  Expect( i.I64_thing = -5,  'i.I64_thing = '+IntToStr(i.I64_thing));
  Expect( i2.Byte_thing = 1, 'i2.Byte_thing = '+IntToStr(i2.Byte_thing));
  Expect( i2.I32_thing = 5,  'i2.I32_thing = '+IntToStr(i2.I32_thing));
  Expect( i.__isset_String_thing, 'i.__isset_String_thing = '+BoolToString(i.__isset_String_thing));
  Expect( i.__isset_Byte_thing,  'i.__isset_Byte_thing = '+BoolToString(i.__isset_Byte_thing));
  Expect( i.__isset_I32_thing,  'i.__isset_I32_thing = '+BoolToString(i.__isset_I32_thing));
  Expect( i.__isset_I64_thing,  'i.__isset_I64_thing = '+BoolToString(i.__isset_I64_thing));
  Expect( i2.__isset_Byte_thing, 'i2.__isset_Byte_thing');
  Expect( i2.__isset_I32_thing,  'i2.__isset_I32_thing');

  // map<type1,type2>: A map of strictly unique keys to values.
  // Translates to an STL map, Java HashMap, PHP associative array, Python/Ruby dictionary, etc.
  StartTestGroup( 'testMap', test_Containers);
  mapout := TThriftDictionaryImpl<Integer,Integer>.Create;
  for j := 0 to 4 do
  begin
    mapout.AddOrSetValue( j, j - 10);
  end;
  Console.Write('testMap({');
  first := True;
  for key in mapout.Keys do
  begin
    if first
    then first := False
    else Console.Write( ', ' );
    Console.Write( IntToStr( key) + ' => ' + IntToStr( mapout[key]));
  end;
  Console.WriteLine('})');

  mapin := client.testMap( mapout );
  Expect( mapin.Count = mapout.Count, 'testMap: mapin.Count = mapout.Count');
  for j := 0 to 4 do
  begin
    Expect( mapout.ContainsKey(j), 'testMap: mapout.ContainsKey('+IntToStr(j)+') = '+BoolToString(mapout.ContainsKey(j)));
  end;
  for key in mapin.Keys do
  begin
    Expect( mapin[key] = mapout[key], 'testMap: '+IntToStr(key) + ' => ' + IntToStr( mapin[key]));
    Expect( mapin[key] = key - 10, 'testMap: mapin['+IntToStr(key)+'] = '+IntToStr( mapin[key]));
  end;


  // map<type1,type2>: A map of strictly unique keys to values.
  // Translates to an STL map, Java HashMap, PHP associative array, Python/Ruby dictionary, etc.
  StartTestGroup( 'testStringMap', test_Containers);
  strmapout := TThriftDictionaryImpl<string,string>.Create;
  for j := 0 to 4 do
  begin
    strmapout.AddOrSetValue( IntToStr(j), IntToStr(j - 10));
  end;
  Console.Write('testStringMap({');
  first := True;
  for strkey in strmapout.Keys do
  begin
    if first
    then first := False
    else Console.Write( ', ' );
    Console.Write( strkey + ' => ' + strmapout[strkey]);
  end;
  Console.WriteLine('})');

  strmapin := client.testStringMap( strmapout );
  Expect( strmapin.Count = strmapout.Count, 'testStringMap: strmapin.Count = strmapout.Count');
  for j := 0 to 4 do
  begin
    Expect( strmapout.ContainsKey(IntToStr(j)),
            'testStringMap: strmapout.ContainsKey('+IntToStr(j)+') = '
            + BoolToString(strmapout.ContainsKey(IntToStr(j))));
  end;
  for strkey in strmapin.Keys do
  begin
    Expect( strmapin[strkey] = strmapout[strkey], 'testStringMap: '+strkey + ' => ' + strmapin[strkey]);
    Expect( strmapin[strkey] = IntToStr( StrToInt(strkey) - 10), 'testStringMap: strmapin['+strkey+'] = '+strmapin[strkey]);
  end;


  // set<type>: An unordered set of unique elements.
  // Translates to an STL set, Java HashSet, set in Python, etc.
  // Note: PHP does not support sets, so it is treated similar to a List
  StartTestGroup( 'testSet', test_Containers);
  setout := TThriftHashSetImpl<Integer>.Create;
  for j := -2 to 2 do
  begin
    setout.Add( j );
  end;
  Console.Write('testSet({');
  first := True;
  for j in setout do
  begin
    if first
    then first := False
    else Console.Write(', ');
    Console.Write(IntToStr( j));
  end;
  Console.WriteLine('})');

  setin := client.testSet(setout);
  Expect( setin.Count = setout.Count, 'testSet: setin.Count = setout.Count');
  Expect( setin.Count = 5, 'testSet: setin.Count = '+IntToStr(setin.Count));
  for j := -2 to 2 do // unordered, we can't rely on the order => test for known elements only
  begin
    Expect( setin.Contains(j), 'testSet: setin.Contains('+IntToStr(j)+') => '+BoolToString(setin.Contains(j)));
  end;

  // list<type>: An ordered list of elements.
  // Translates to an STL vector, Java ArrayList, native arrays in scripting languages, etc.
  StartTestGroup( 'testList', test_Containers);
  listout := TThriftListImpl<Integer>.Create;
  listout.Add( +1);
  listout.Add( -2);
  listout.Add( +3);
  listout.Add( -4);
  listout.Add( 0);
  Console.Write('testList({');
  first := True;
  for j in listout do
    begin
    if first
    then first := False
    else Console.Write(', ');
    Console.Write(IntToStr( j));
  end;
  Console.WriteLine('})');

  listin := client.testList(listout);
  Expect( listin.Count = listout.Count, 'testList: listin.Count = listout.Count');
  Expect( listin.Count = 5, 'testList: listin.Count = '+IntToStr(listin.Count));
  Expect( listin[0] = +1, 'listin[0] = '+IntToStr( listin[0]));
  Expect( listin[1] = -2, 'listin[1] = '+IntToStr( listin[1]));
  Expect( listin[2] = +3, 'listin[2] = '+IntToStr( listin[2]));
  Expect( listin[3] = -4, 'listin[3] = '+IntToStr( listin[3]));
  Expect( listin[4] = 0,  'listin[4] = '+IntToStr( listin[4]));

  // enums
  ret := client.testEnum(TNumberz.ONE);
  Expect( ret = TNumberz.ONE, 'testEnum(ONE) = '+IntToStr(Ord(ret)));

  ret := client.testEnum(TNumberz.TWO);
  Expect( ret = TNumberz.TWO, 'testEnum(TWO) = '+IntToStr(Ord(ret)));

  ret := client.testEnum(TNumberz.THREE);
  Expect( ret = TNumberz.THREE, 'testEnum(THREE) = '+IntToStr(Ord(ret)));

  ret := client.testEnum(TNumberz.FIVE);
  Expect( ret = TNumberz.FIVE, 'testEnum(FIVE) = '+IntToStr(Ord(ret)));

  ret := client.testEnum(TNumberz.EIGHT);
  Expect( ret = TNumberz.EIGHT, 'testEnum(EIGHT) = '+IntToStr(Ord(ret)));


  // typedef
  uid := client.testTypedef(309858235082523);
  Expect( uid = 309858235082523, 'testTypedef(309858235082523) = '+IntToStr(uid));


  // maps of maps
  StartTestGroup( 'testMapMap(1)', test_Containers);
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

  // verify result data
  Expect( mm.Count = 2, 'mm.Count = '+IntToStr(mm.Count));
  pos := mm[4];
  neg := mm[-4];
  for j := 1 to 4 do
  begin
    Expect( pos[j]  = j,  'pos[j]  = '+IntToStr(pos[j]));
    Expect( neg[-j] = -j, 'neg[-j] = '+IntToStr(neg[-j]));
  end;



  // insanity
  StartTestGroup( 'testInsanity', test_Structs);
  insane := TInsanityImpl.Create;
  insane.UserMap := TThriftDictionaryImpl<TNumberz, Int64>.Create;
  insane.UserMap.AddOrSetValue( TNumberz.FIVE, 5000);
  truck := TXtructImpl.Create;
  truck.String_thing := 'Truck';
  truck.Byte_thing := -8;  // byte is signed
  truck.I32_thing := 32;
  truck.I64_thing := 64;
  insane.Xtructs := TThriftListImpl<IXtruct>.Create;
  insane.Xtructs.Add( truck );
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

  // verify result data
  Expect( whoa.Count = 2, 'whoa.Count = '+IntToStr(whoa.Count));
  //
  first_map  := whoa[1];
  second_map := whoa[2];
  Expect( first_map.Count = 2, 'first_map.Count = '+IntToStr(first_map.Count));
  Expect( second_map.Count = 1, 'second_map.Count = '+IntToStr(second_map.Count));
  //
  looney := second_map[TNumberz.SIX];
  Expect( Assigned(looney), 'Assigned(looney) = '+BoolToString(Assigned(looney)));
  Expect( not looney.__isset_UserMap, 'looney.__isset_UserMap = '+BoolToString(looney.__isset_UserMap));
  Expect( not looney.__isset_Xtructs, 'looney.__isset_Xtructs = '+BoolToString(looney.__isset_Xtructs));
  //
  for ret in [TNumberz.TWO, TNumberz.THREE] do begin
    crazy := first_map[ret];
    Console.WriteLine('first_map['+intToStr(Ord(ret))+']');

    Expect( crazy.__isset_UserMap, 'crazy.__isset_UserMap = '+BoolToString(crazy.__isset_UserMap));
    Expect( crazy.__isset_Xtructs, 'crazy.__isset_Xtructs = '+BoolToString(crazy.__isset_Xtructs));

    Expect( crazy.UserMap.Count = insane.UserMap.Count, 'crazy.UserMap.Count = '+IntToStr(crazy.UserMap.Count));
    for pair in insane.UserMap do begin
      Expect( crazy.UserMap[pair.Key] = pair.Value, 'crazy.UserMap['+IntToStr(Ord(pair.key))+'] = '+IntToStr(crazy.UserMap[pair.Key]));
    end;

    Expect( crazy.Xtructs.Count = insane.Xtructs.Count, 'crazy.Xtructs.Count = '+IntToStr(crazy.Xtructs.Count));
    for arg0 := 0 to insane.Xtructs.Count-1 do begin
      hello   := insane.Xtructs[arg0];
      goodbye := crazy.Xtructs[arg0];
      Expect( goodbye.String_thing = hello.String_thing, 'goodbye.String_thing = '+goodbye.String_thing);
      Expect( goodbye.Byte_thing = hello.Byte_thing, 'goodbye.Byte_thing = '+IntToStr(goodbye.Byte_thing));
      Expect( goodbye.I32_thing = hello.I32_thing, 'goodbye.I32_thing = '+IntToStr(goodbye.I32_thing));
      Expect( goodbye.I64_thing = hello.I64_thing, 'goodbye.I64_thing = '+IntToStr(goodbye.I64_thing));
    end;
  end;


  // multi args
  StartTestGroup( 'testMulti', test_BaseTypes);
  arg0 := 1;
  arg1 := 2;
  arg2 := High(Int64);
  arg3 := TThriftDictionaryImpl<SmallInt, string>.Create;
  arg3.AddOrSetValue( 1, 'one');
  arg4 := TNumberz.FIVE;
  arg5 := 5000000;
  Console.WriteLine('Test Multi(' + IntToStr( arg0) + ',' +
    IntToStr( arg1) + ',' + IntToStr( arg2) + ',' +
    arg3.ToString + ',' + IntToStr( Integer( arg4)) + ',' +
      IntToStr( arg5) + ')');

  i := client.testMulti( arg0, arg1, arg2, arg3, arg4, arg5);
  Expect( i.String_thing = 'Hello2', 'testMulti: i.String_thing = "'+i.String_thing+'"');
  Expect( i.Byte_thing = arg0, 'testMulti: i.Byte_thing = '+IntToStr(i.Byte_thing));
  Expect( i.I32_thing = arg1, 'testMulti: i.I32_thing = '+IntToStr(i.I32_thing));
  Expect( i.I64_thing = arg2, 'testMulti: i.I64_thing = '+IntToStr(i.I64_thing));
  Expect( i.__isset_String_thing, 'testMulti: i.__isset_String_thing = '+BoolToString(i.__isset_String_thing));
  Expect( i.__isset_Byte_thing, 'testMulti: i.__isset_Byte_thing = '+BoolToString(i.__isset_Byte_thing));
  Expect( i.__isset_I32_thing, 'testMulti: i.__isset_I32_thing = '+BoolToString(i.__isset_I32_thing));
  Expect( i.__isset_I64_thing, 'testMulti: i.__isset_I64_thing = '+BoolToString(i.__isset_I64_thing));

  // multi exception
  StartTestGroup( 'testMultiException(1)', test_Exceptions);
  try
    i := client.testMultiException( 'need more pizza', 'run out of beer');
    Expect( i.String_thing = 'run out of beer', 'i.String_thing = "' +i.String_thing+ '"');
    Expect( i.__isset_String_thing, 'i.__isset_String_thing = '+BoolToString(i.__isset_String_thing));
    { this is not necessarily true, these fields are default-serialized
    Expect( not i.__isset_Byte_thing, 'i.__isset_Byte_thing = '+BoolToString(i.__isset_Byte_thing));
    Expect( not i.__isset_I32_thing, 'i.__isset_I32_thing = '+BoolToString(i.__isset_I32_thing));
    Expect( not i.__isset_I64_thing, 'i.__isset_I64_thing = '+BoolToString(i.__isset_I64_thing));
    }
  except
    on e:Exception do Expect( FALSE, 'Unexpected exception "'+e.ClassName+'": '+e.Message);
  end;

  StartTestGroup( 'testMultiException(Xception)', test_Exceptions);
  try
    i := client.testMultiException( 'Xception', 'second test');
    Expect( FALSE, 'testMultiException(''Xception''): must trow an exception');
  except
    on x:TXception do begin
      Expect( x.__isset_ErrorCode, 'x.__isset_ErrorCode = '+BoolToString(x.__isset_ErrorCode));
      Expect( x.__isset_Message,  'x.__isset_Message = '+BoolToString(x.__isset_Message));
      Expect( x.ErrorCode = 1001, 'x.ErrorCode = '+IntToStr(x.ErrorCode));
      Expect( x.Message_ = 'This is an Xception', 'x.Message = "'+x.Message_+'"');
    end;
    on e:Exception do Expect( FALSE, 'Unexpected exception "'+e.ClassName+'": '+e.Message);
  end;

  StartTestGroup( 'testMultiException(Xception2)', test_Exceptions);
  try
    i := client.testMultiException( 'Xception2', 'third test');
    Expect( FALSE, 'testMultiException(''Xception2''): must trow an exception');
  except
    on x:TXception2 do begin
      Expect( x.__isset_ErrorCode, 'x.__isset_ErrorCode = '+BoolToString(x.__isset_ErrorCode));
      Expect( x.__isset_Struct_thing,  'x.__isset_Struct_thing = '+BoolToString(x.__isset_Struct_thing));
      Expect( x.ErrorCode = 2002, 'x.ErrorCode = '+IntToStr(x.ErrorCode));
      Expect( x.Struct_thing.String_thing = 'This is an Xception2', 'x.Struct_thing.String_thing = "'+x.Struct_thing.String_thing+'"');
      Expect( x.Struct_thing.__isset_String_thing, 'x.Struct_thing.__isset_String_thing = '+BoolToString(x.Struct_thing.__isset_String_thing));
      { this is not necessarily true, these fields are default-serialized
      Expect( not x.Struct_thing.__isset_Byte_thing, 'x.Struct_thing.__isset_Byte_thing = '+BoolToString(x.Struct_thing.__isset_Byte_thing));
      Expect( not x.Struct_thing.__isset_I32_thing, 'x.Struct_thing.__isset_I32_thing = '+BoolToString(x.Struct_thing.__isset_I32_thing));
      Expect( not x.Struct_thing.__isset_I64_thing, 'x.Struct_thing.__isset_I64_thing = '+BoolToString(x.Struct_thing.__isset_I64_thing));
      }
    end;
    on e:Exception do Expect( FALSE, 'Unexpected exception "'+e.ClassName+'": '+e.Message);
  end;


  // oneway functions
  StartTestGroup( 'Test Oneway(1)', test_Unknown);
  client.testOneway(1);
  Expect( TRUE, 'Test Oneway(1)');  // success := no exception

  // call time
  {$IFDEF PerfTest}
  StartTestGroup( 'Test Calltime()');
  StartTick := GetTickCount;
  for k := 0 to 1000 - 1 do
  begin
    client.testVoid();
  end;
  Console.WriteLine(' = ' + FloatToStr( (GetTickCount - StartTick) / 1000 ) + ' ms a testVoid() call' );
  {$ENDIF PerfTest}

  // no more tests here
  StartTestGroup( '', test_Unknown);
end;


{$IFDEF SupportsAsync}
procedure TClientThread.ClientAsyncTest;
var
  client : TThriftTest.IAsync;
  s : string;
  i8 : ShortInt;
begin
  StartTestGroup( 'Async Tests', test_Unknown);
  client := TThriftTest.TClient.Create( FProtocol);
  FTransport.Open;

  // oneway void functions
  client.testOnewayAsync(1).Wait;
  Expect( TRUE, 'Test Oneway(1)');  // success := no exception

  // normal functions
  s := client.testStringAsync(HUGE_TEST_STRING).Value;
  Expect( length(s) = length(HUGE_TEST_STRING),
          'testString( length(HUGE_TEST_STRING) = '+IntToStr(Length(HUGE_TEST_STRING))+') '
         +'=> length(result) = '+IntToStr(Length(s)));

  i8 := client.testByte(1).Value;
  Expect( i8 = 1, 'testByte(1) = ' + IntToStr( i8 ));
end;
{$ENDIF}


{$IFDEF StressTest}
procedure TClientThread.StressTest(const client : TThriftTest.Iface);
begin
  while TRUE do begin
    try
      if not FTransport.IsOpen then FTransport.Open;   // re-open connection, server has already closed
      try
        client.testString('Test');
        Write('.');
      finally
        if FTransport.IsOpen then FTransport.Close;
      end;
    except
      on e:Exception do Writeln(#10+e.message);
    end;
  end;
end;
{$ENDIF}


procedure TClientThread.StartTestGroup( const aGroup : string; const aTest : TClientTestGroup);
begin
  FLogger.StartTestGroup( aGroup, aTest);
end;


procedure TClientThread.Expect( aTestResult : Boolean; const aTestInfo : string);
begin
  FLogger.Expect( aTestResult, aTestInfo);
end;


function TClientThread.CalculateExitCode : Byte;
var test : TClientTestGroup;
    failed, executed : TClientTestGroups;
begin
  result := EXITCODE_SUCCESS;
  FLogger.QueryTestStats( failed, executed);
  for test := Low(TClientTestGroup) to High(TClientTestGroup) do begin
    if (test in failed) or not (test in executed)
    then result := result or MAP_FAILURES_TO_EXITCODE_BITS[test];
  end;
end;


constructor TClientThread.Create( const aSetup : TTestSetup; const aNumIteration, aThreadNo: Integer; const aLogThreadID : Boolean);
begin
  FSetup := aSetup;
  FThreadNo := aThreadNo;
  FNumIterations := aNumIteration;

  FConsole := TThreadConsole.Create( Self, aLogThreadID);
  FLogger := TTestLoggerImpl.Create;

  inherited Create( TRUE);
end;

destructor TClientThread.Destroy;
begin
  FreeAndNil( FConsole);
  FLogger := nil; //-> Release
  inherited;
end;

procedure TClientThread.Execute;
var
  i : Integer;
begin
  // perform all tests
  try
    // builtin (quick) unit tests on one thread only
    if ThreadNo = 0
    then TQuickUnitTests.Execute(FLogger);

    // must be run in the context of the thread
    InitializeProtocolTransportStack;
    try
      for i := 0 to FNumIterations - 1 do begin
        ClientTest;
        {$IFDEF SupportsAsync}
        ClientAsyncTest;
        {$ENDIF}
      end;

      // report the outcome
      FLogger.ReportResults;
      SetReturnValue( CalculateExitCode);

    finally
      ShutdownProtocolTransportStack;
    end;

  except
    on e:Exception do Expect( FALSE, 'unexpected exception: "'+e.message+'"');
  end;
end;


function TClientThread.InitializeHttpTransport( const aTimeoutSetting : Integer; const aConfig : IThriftConfiguration) : IHTTPClient;
var sUrl    : string;
    comps   : URL_COMPONENTS;
    dwChars : DWORD;
begin
  ASSERT( FSetup.endpoint in [trns_MsxmlHttp, trns_WinHttp]);

  if FSetup.useSSL
  then sUrl := 'https://'
  else sUrl := 'http://';

  sUrl := sUrl + FSetup.host;

  // add the port number if necessary and at the right place
  FillChar( comps, SizeOf(comps), 0);
  comps.dwStructSize := SizeOf(comps);
  comps.dwSchemeLength    := MAXINT;
  comps.dwHostNameLength  := MAXINT;
  comps.dwUserNameLength  := MAXINT;
  comps.dwPasswordLength  := MAXINT;
  comps.dwUrlPathLength   := MAXINT;
  comps.dwExtraInfoLength := MAXINT;
  Win32Check( WinHttpCrackUrl( PChar(sUrl), Length(sUrl), 0, comps));
  case FSetup.port of
    80  : if FSetup.useSSL then comps.nPort := FSetup.port;
    443 : if not FSetup.useSSL then comps.nPort := FSetup.port;
  else
    if FSetup.port > 0 then comps.nPort := FSetup.port;
  end;
  dwChars := Length(sUrl) + 64;
  SetLength( sUrl, dwChars);
  Win32Check( WinHttpCreateUrl( comps, 0, @sUrl[1], dwChars));
  SetLength( sUrl, dwChars);


  Console.WriteLine('Target URL: '+sUrl);
  case FSetup.endpoint of
    trns_MsxmlHttp :  result := TMsxmlHTTPClientImpl.Create( sUrl, aConfig);
    trns_WinHttp   :  result := TWinHTTPClientImpl.Create(   sUrl, aConfig);
  else
    raise Exception.Create(ENDPOINT_TRANSPORTS[FSetup.endpoint]+' unhandled case');
  end;

  result.DnsResolveTimeout := aTimeoutSetting;
  result.ConnectionTimeout := aTimeoutSetting;
  result.SendTimeout       := aTimeoutSetting;
  result.ReadTimeout       := aTimeoutSetting;
end;


procedure TClientThread.InitializeProtocolTransportStack;
var streamtrans : IStreamTransport;
    canSSL : Boolean;
const
  DEBUG_TIMEOUT   = 30 * 1000;
  RELEASE_TIMEOUT = DEFAULT_THRIFT_TIMEOUT;
  PIPE_TIMEOUT    = RELEASE_TIMEOUT;
  HTTP_TIMEOUTS   = 10 * 1000;
begin
  // needed for HTTP clients as they utilize the MSXML COM components
  OleCheck( CoInitialize( nil));

  canSSL := FALSE;
  case FSetup.endpoint of
    trns_Sockets: begin
      Console.WriteLine('Using sockets ('+FSetup.host+' port '+IntToStr(FSetup.port)+')');
      streamtrans := TSocketImpl.Create( FSetup.host, FSetup.port);
      FTransport := streamtrans;
    end;

    trns_MsxmlHttp,
    trns_WinHttp: begin
      Console.WriteLine('Using HTTPClient');
      FTransport := InitializeHttpTransport( HTTP_TIMEOUTS);
      canSSL := TRUE;
    end;

    trns_EvHttp: begin
      raise Exception.Create(ENDPOINT_TRANSPORTS[FSetup.endpoint]+' transport not implemented');
    end;

    trns_NamedPipes: begin
      streamtrans := TNamedPipeTransportClientEndImpl.Create( FSetup.sPipeName, 0, nil, PIPE_TIMEOUT, PIPE_TIMEOUT);
      FTransport := streamtrans;
    end;

    trns_AnonPipes: begin
      streamtrans := TAnonymousPipeTransportImpl.Create( FSetup.hAnonRead, FSetup.hAnonWrite, FALSE, PIPE_TIMEOUT);
      FTransport := streamtrans;
    end;

  else
    raise Exception.Create('Unhandled endpoint transport');
  end;
  ASSERT( FTransport <> nil);

  // layered transports are not really meant to be stacked upon each other
  if (trns_Framed in FSetup.layered) then begin
    FTransport := TFramedTransportImpl.Create( FTransport);
  end
  else if (trns_Buffered in FSetup.layered) and (streamtrans <> nil) then begin
    FTransport := TBufferedTransportImpl.Create( streamtrans, 32);  // small buffer to test read()
  end;

  if FSetup.useSSL and not canSSL then begin
    raise Exception.Create('SSL/TLS not implemented');
  end;

  // create protocol instance, default to BinaryProtocol
  FProtocol := PROTOCOL_CLASSES[FSetup.protType].Create(FTransport);
  ASSERT( (FTransport <> nil) and (FProtocol <> nil));
end;


procedure TClientThread.ShutdownProtocolTransportStack;
begin
  try
    FProtocol := nil;

    if FTransport <> nil then begin
      FTransport.Close;
      FTransport := nil;
    end;

  finally
    CoUninitialize;
  end;
end;


{ TThreadConsole }

constructor TThreadConsole.Create( const aThread: TClientThread; const aLogThreadID : Boolean);
begin
  inherited Create;
  FThread := AThread;
  FLogThreadID := aLogThreadID;
end;

procedure TThreadConsole.Write(const S: string);
begin
  if FLogThreadID
  then ConsoleHelper.Console.Write( IntToStr(FThread.ThreadNo)+'> '+S)
  else ConsoleHelper.Console.Write( S);
end;

procedure TThreadConsole.WriteLine(const S: string);
begin
  if FLogThreadID
  then ConsoleHelper.Console.WriteLine( IntToStr(FThread.ThreadNo)+'> '+S)
  else ConsoleHelper.Console.WriteLine( S);
end;


initialization
  TTestClient.FNumIterations := 1;
  TTestClient.FNumThreads := 1;

end.
