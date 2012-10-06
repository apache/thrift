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
unit Thrift.Transport.Pipes;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, SysUtils, Math, AccCtrl, AclAPI,
  Thrift.Transport,
  Thrift.Console,
  Thrift.Stream;

const
  DEFAULT_THRIFT_PIPE_TIMEOUT = 5 * 1000; // ms


type
  IPipe = interface( IStreamTransport)
    ['{5E05CC85-434F-428F-BFB2-856A168B5558}']
  end;


  TPipeStreamImpl = class( TThriftStreamImpl)
  private
    FPipe : THandle;
    FOwner : Boolean;
    FPipeName : string;
    FTimeout : DWORD;
    FShareMode: DWORD;
    FSecurityAttribs : PSecurityAttributes;

  protected
    procedure Write( const buffer: TBytes; offset: Integer; count: Integer); override;
    function  Read( var buffer: TBytes; offset: Integer; count: Integer): Integer; override;
    procedure Open; override;
    procedure Close; override;
    procedure Flush; override;

    function IsOpen: Boolean; override;
    function ToArray: TBytes; override;
  public
    constructor Create( const aPipeHandle : THandle; aOwnsHandle : Boolean);  overload;
    constructor Create( const aPipeName : string;
                        const aShareMode: DWORD = 0;
                        const aSecurityAttributes: PSecurityAttributes = nil;
                        const aTimeOut : DWORD = DEFAULT_THRIFT_PIPE_TIMEOUT);  overload;
    destructor Destroy;  override;
  end;


  TNamedPipeImpl = class( TStreamTransportImpl, IPipe)
  public
    FOwner : Boolean;

    // Constructs a new pipe object.
    constructor Create(); overload;
    // Named pipe constructors
    constructor Create( aPipe : THandle; aOwnsHandle : Boolean); overload;
    constructor Create( const aPipeName : string;
                        const aShareMode: DWORD = 0;
                        const aSecurityAttributes: PSecurityAttributes = nil;
                        const aTimeOut : DWORD = DEFAULT_THRIFT_PIPE_TIMEOUT);  overload;

    // ITransport
    function  GetIsOpen: Boolean; override;
    procedure Open; override;
    procedure Close; override;
  end;


  TAnonymousPipeImpl = class( TStreamTransportImpl, IPipe)
  public
    FOwner : Boolean;

    // Constructs a new pipe object.
    constructor Create(); overload;
    // Anonymous pipe constructor
    constructor Create( const aPipeRead, aPipeWrite : THandle; aOwnsHandles : Boolean); overload;

    // ITransport
    function  GetIsOpen: Boolean; override;
    procedure Open; override;
    procedure Close; override;
  end;


  IPipeServer = interface( IServerTransport)
    ['{7AEE6793-47B9-4E49-981A-C39E9108E9AD}']
    // Server side anonymous pipe ends
    function Handle : THandle;
    function WriteHandle : THandle;
    // Client side anonymous pipe ends
    function ClientAnonRead : THandle;
    function ClientAnonWrite  : THandle;
  end;


  TServerPipeImpl = class( TServerTransportImpl, IPipeServer)
  private
    FPipeName     : string;
    FMaxConns     : DWORD;
    FBufSize      : DWORD;
    FAnonymous    : Boolean;

    FHandle,
    FWriteHandle : THandle;

    //Client side anonymous pipe handles
    FClientAnonRead,
    FClientAnonWrite  : THandle;

  protected
    function AcceptImpl: ITransport; override;

    function CreateNamedPipe : Boolean;
    function CreateAnonPipe : Boolean;

    // IPipeServer
    function Handle : THandle;
    function WriteHandle : THandle;
    function ClientAnonRead : THandle;
    function ClientAnonWrite  : THandle;

  public
    // Constructors
    constructor Create();  overload;
    // Named Pipe
    constructor Create( aPipename : string);  overload;
    constructor Create( aPipename : string; aBufsize : Cardinal);  overload;
    constructor Create( aPipename : string; aBufsize, aMaxConns : Cardinal);  overload;
    // Anonymous pipe
    constructor Create( aBufsize : Cardinal);  overload;

    procedure Listen; override;
    procedure Close; override;
  end;


const
  TPIPE_SERVER_MAX_CONNS_DEFAULT = 10;


implementation


{ TPipeStreamImpl }


constructor TPipeStreamImpl.Create( const aPipeHandle : THandle; aOwnsHandle : Boolean);
begin
  FPipe            := aPipeHandle;
  FOwner           := aOwnsHandle;
  FPipeName        := '';
  FTimeout         := DEFAULT_THRIFT_PIPE_TIMEOUT;
  FShareMode       := 0;
  FSecurityAttribs := nil;
end;


constructor TPipeStreamImpl.Create( const aPipeName : string; const aShareMode: DWORD;
                                    const aSecurityAttributes: PSecurityAttributes;
                                    const aTimeOut : DWORD);
begin
  FPipe            := INVALID_HANDLE_VALUE;
  FOwner           := TRUE;
  FPipeName        := aPipeName;
  FTimeout         := aTimeOut;
  FShareMode       := aShareMode;
  FSecurityAttribs := aSecurityAttributes;

  if Copy(FPipeName,1,2) <> '\\'
  then FPipeName := '\\.\pipe\' + FPipeName;  // assume localhost
end;


destructor TPipeStreamImpl.Destroy;
begin
  try
    Close;
  finally
    inherited Destroy;
  end;
end;


procedure TPipeStreamImpl.Close;
begin
  if IsOpen then try
    if FOwner
    then CloseHandle( FPipe);
  finally
    FPipe := INVALID_HANDLE_VALUE;
  end;
end;


procedure TPipeStreamImpl.Flush;
begin
  // nothing to do
end;


function TPipeStreamImpl.IsOpen: Boolean;
begin
  result := (FPipe <> INVALID_HANDLE_VALUE);
end;


procedure TPipeStreamImpl.Open;
var retries  : Integer;
    hPipe    : THandle;
    dwMode   : DWORD;
const INTERVAL = 500; // ms
begin
  if IsOpen then Exit;

  // open that thingy
  retries  := Max( 1, Round( 1.0 * FTimeout / INTERVAL));
  hPipe    := INVALID_HANDLE_VALUE;
  while TRUE do begin
    hPipe := CreateFile( PChar( FPipeName),
                         GENERIC_READ or GENERIC_WRITE,
                         FShareMode,        // sharing
                         FSecurityAttribs,  // security attributes
                         OPEN_EXISTING,     // opens existing pipe
                         0,                 // default attributes
                         0);                // no template file

    if hPipe <> INVALID_HANDLE_VALUE
    then Break;

    Dec( retries);
    if (retries > 0) or (FTimeout = INFINITE)
    then Sleep( INTERVAL)
    else raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                           'Unable to open pipe');
  end;

  // pipe connected; change to message-read mode.
  dwMode := PIPE_READMODE_MESSAGE;
  if not SetNamedPipeHandleState( hPipe, dwMode, nil, nil) then begin
    Close;
    raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                      'SetNamedPipeHandleState failed');
  end;

  // everything fine
  FPipe := hPipe;
end;


procedure TPipeStreamImpl.Write(const buffer: TBytes; offset, count: Integer);
var cbWritten : DWORD;
begin
  if not IsOpen
  then raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                         'Called write on non-open pipe');

  if not WriteFile( FPipe, buffer[offset], count, cbWritten, nil)
  then raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                         'Write to pipe failed');
end;


function TPipeStreamImpl.Read( var buffer: TBytes; offset, count: Integer): Integer;
var cbRead  : DWORD;
    bytes, retries  : LongInt;
    bOk     : Boolean;
const INTERVAL = 10;  // ms
begin
  if not IsOpen
  then raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                         'Called read on non-open pipe');

  // MSDN: Handle can be a handle to a named pipe instance,
  // or it can be a handle to the read end of an anonymous pipe,
  // The handle must have GENERIC_READ access to the pipe.
  if FTimeOut <> INFINITE then begin
    retries := Max( 1, Round( 1.0 * FTimeOut / INTERVAL));
    while TRUE do begin
      if  IsOpen
      and PeekNamedPipe( FPipe, nil, 0, nil, @bytes, nil)
      and (bytes > 0)
      then Break;  // there are data

      Dec( retries);
      if retries > 0
      then Sleep( INTERVAL)
      else raise TTransportException.Create( TTransportException.TExceptionType.TimedOut,
                                             'Pipe read timed out');
    end;
  end;

  // read the data (or block INFINITE-ly)
  bOk := ReadFile( FPipe, buffer[offset], count, cbRead, nil);
  if (not bOk) and (GetLastError() <> ERROR_MORE_DATA)
  then result := 0 // No more data, possibly because client disconnected.
  else result := cbRead;
end;


function TPipeStreamImpl.ToArray: TBytes;
var bytes : LongInt;
begin
  SetLength( result, 0);
  bytes := 0;

  if  IsOpen
  and PeekNamedPipe( FPipe, nil, 0, nil, @bytes, nil)
  and (bytes > 0)
  then begin
    SetLength( result, bytes);
    Read( result, 0, bytes);
  end;
end;


{ TNamedPipeImpl }


constructor TNamedPipeImpl.Create();
// Constructs a new pipe object / provides defaults
begin
  inherited Create( nil, nil);
  FOwner := FALSE;
end;


constructor TNamedPipeImpl.Create( const aPipeName : string; const aShareMode: DWORD;
                                   const aSecurityAttributes: PSecurityAttributes;
                                   const aTimeOut : DWORD);
// Named pipe constructor
begin
  Create();
  FInputStream  := TPipeStreamImpl.Create( aPipeName, aShareMode, aSecurityAttributes, aTimeOut);
  FOutputStream := FInputStream;  // true for named pipes
  FOwner        := TRUE;
end;


constructor TNamedPipeImpl.Create( aPipe : THandle; aOwnsHandle : Boolean);
// Named pipe constructor
begin
  Create();
  FInputStream  := TPipeStreamImpl.Create( aPipe, aOwnsHandle);
  FOutputStream := FInputStream;  // true for named pipes
  FOwner        := aOwnsHandle;
end;


function TNamedPipeImpl.GetIsOpen: Boolean;
begin
  result := (FInputStream <> nil);
end;


procedure TNamedPipeImpl.Open;
begin
  if FOwner then begin
    FInputStream.Open;
    if (FOutputStream <> nil) and (FOutputStream <> FInputStream)
    then FOutputStream.Open;
  end;
end;


procedure TNamedPipeImpl.Close;
begin
  if FOwner then begin
    FInputStream.Close;
    if (FOutputStream <> nil) and (FOutputStream <> FInputStream)
    then FOutputStream.Close;
  end;
end;


{ TAnonymousPipeImpl }


constructor TAnonymousPipeImpl.Create();
// Constructs a new pipe object / provides defaults
begin
  inherited Create( nil, nil);
  FOwner := FALSE;
end;


constructor TAnonymousPipeImpl.Create( const aPipeRead, aPipeWrite : THandle; aOwnsHandles : Boolean);
// Anonymous pipe constructor
begin
  Create();
  FInputStream  := TPipeStreamImpl.Create( aPipeRead, aOwnsHandles);
  FOutputStream := TPipeStreamImpl.Create( aPipeWrite, aOwnsHandles);
  FOwner        := aOwnsHandles;
end;


function TAnonymousPipeImpl.GetIsOpen: Boolean;
begin
  result := (FInputStream <> nil) or (FOutputStream <> nil);
end;


procedure TAnonymousPipeImpl.Open;
begin
  if FOwner then begin
    FInputStream.Open;
    if (FOutputStream <> nil) and (FOutputStream <> FInputStream)
    then FOutputStream.Open;
  end;
end;


procedure TAnonymousPipeImpl.Close;
begin
  if FOwner then begin
    FInputStream.Close;
    if (FOutputStream <> nil) and (FOutputStream <> FInputStream)
    then FOutputStream.Close;
  end;
end;


{ TServerPipeImpl }


constructor TServerPipeImpl.Create( aPipename : string; aBufsize, aMaxConns : Cardinal);
// Named Pipe CTOR
begin
  inherited Create;
  FPipeName := aPipename;
  FBufsize  := aBufSize;
  FMaxConns := Max( 1, Min( 255, aMaxConns));  // restrict to 1-255 connections
  FAnonymous := FALSE;
  FHandle := INVALID_HANDLE_VALUE;
  FWriteHandle := INVALID_HANDLE_VALUE;
  FClientAnonRead := INVALID_HANDLE_VALUE;
  FClientAnonWrite := INVALID_HANDLE_VALUE;

  if Copy(FPipeName,1,2) <> '\\'
  then FPipeName := '\\.\pipe\' + FPipeName;  // assume localhost
end;


constructor TServerPipeImpl.Create( aPipename : string; aBufsize : Cardinal);
// Named Pipe CTOR
begin
  Create( aPipename, aBufSize, TPIPE_SERVER_MAX_CONNS_DEFAULT);
end;


constructor TServerPipeImpl.Create( aPipename : string);
// Named Pipe CTOR
begin
  Create( aPipename, 1024, TPIPE_SERVER_MAX_CONNS_DEFAULT);
end;


constructor TServerPipeImpl.Create( aBufsize : Cardinal);
// Anonymous pipe CTOR
begin
  inherited Create;
  FPipeName := '';
  FBufsize  := aBufSize;
  FMaxConns := 1;
  FAnonymous := TRUE;
  FHandle := INVALID_HANDLE_VALUE;
  FWriteHandle := INVALID_HANDLE_VALUE;
  FClientAnonRead := INVALID_HANDLE_VALUE;
  FClientAnonWrite := INVALID_HANDLE_VALUE;

  // The anonymous pipe needs to be created first so that the server can
  // pass the handles on to the client before the serve (acceptImpl)
  // blocking call.
  if not CreateAnonPipe
  then raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                         ClassName+'.Create() failed');
end;


constructor TServerPipeImpl.Create();
// Anonymous pipe CTOR
begin
  Create( 1024);
end;


function TServerPipeImpl.AcceptImpl: ITransport;
var buf    : Byte;
    br     : DWORD;
    connectRet : Boolean;
begin
  if FAnonymous then begin   //Anonymous Pipe

    // This 0-byte read serves merely as a blocking call.
    if not ReadFile( FHandle, buf, 0, br, nil)
    and (GetLastError() <> ERROR_MORE_DATA)
    then raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                           'TPipeServer unable to initiate pipe communication');
	  result := TAnonymousPipeImpl.Create( FHandle, FWriteHandle, FALSE);

  end
  else begin  //Named Pipe

    while TRUE do begin
      if not CreateNamedPipe()
      then raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                             'TPipeServer CreateNamedPipe failed');

      // Wait for the client to connect; if it succeeds, the
      // function returns a nonzero value. If the function returns
      // zero, GetLastError should return ERROR_PIPE_CONNECTED.
      if ConnectNamedPipe( FHandle,nil)
      then connectRet := TRUE
      else connectRet := (GetLastError() = ERROR_PIPE_CONNECTED);

      if connectRet
      then Break;

      Close;
      raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                        'TPipeServer: client connection failed');
    end;

	  result := TNamedPipeImpl.Create( FHandle, TRUE);
  end;
end;


procedure TServerPipeImpl.Listen;
begin
  // not much to do here
end;


procedure TServerPipeImpl.Close;
begin
  if not FAnonymous then begin

    if FHandle <> INVALID_HANDLE_VALUE then begin
      DisconnectNamedPipe( FHandle);
      CloseHandle( FHandle);
      FHandle := INVALID_HANDLE_VALUE;
    end;

  end
  else begin

    if FHandle <> INVALID_HANDLE_VALUE then begin
      CloseHandle( FHandle);
      FHandle := INVALID_HANDLE_VALUE;
    end;
    if FWriteHandle <> INVALID_HANDLE_VALUE then begin
      CloseHandle( FWriteHandle);
      FWriteHandle := INVALID_HANDLE_VALUE;
    end;
    if FClientAnonRead <> INVALID_HANDLE_VALUE then begin
      CloseHandle( FClientAnonRead);
      FClientAnonRead := INVALID_HANDLE_VALUE;
    end;
    if FClientAnonWrite <> INVALID_HANDLE_VALUE then begin
      CloseHandle( FClientAnonWrite);
      FClientAnonWrite := INVALID_HANDLE_VALUE;
    end;
  end;
end;


function TServerPipeImpl.Handle : THandle;
begin
  result := FHandle;
end;


function TServerPipeImpl.WriteHandle : THandle;
begin
  result := FWriteHandle;
end;


function TServerPipeImpl.ClientAnonRead : THandle;
begin
  result := FClientAnonRead;
end;


function TServerPipeImpl.ClientAnonWrite  : THandle;
begin
  result := FClientAnonWrite;
end;


function TServerPipeImpl.CreateNamedPipe : Boolean;
var SIDAuthWorld : SID_IDENTIFIER_AUTHORITY ;
    everyone_sid : PSID;
    ea           : EXPLICIT_ACCESS;
    acl          : PACL;
    sd           : PSECURITY_DESCRIPTOR;
    sa           : SECURITY_ATTRIBUTES;
    hPipe : THandle;
const
  SECURITY_WORLD_SID_AUTHORITY  : TSIDIdentifierAuthority = (Value : (0,0,0,0,0,1));
  SECURITY_WORLD_RID = $00000000;
begin
  // Windows - set security to allow non-elevated apps
  // to access pipes created by elevated apps.
  SIDAuthWorld := SECURITY_WORLD_SID_AUTHORITY;
  everyone_sid := nil;
  AllocateAndInitializeSid( SIDAuthWorld, 1, SECURITY_WORLD_RID, 0, 0, 0, 0, 0, 0, 0, everyone_sid);

  ZeroMemory( @ea, SizeOf(ea));
  ea.grfAccessPermissions := SPECIFIC_RIGHTS_ALL or STANDARD_RIGHTS_ALL;
  ea.grfAccessMode        := SET_ACCESS;
  ea.grfInheritance       := NO_INHERITANCE;
  ea.Trustee.TrusteeForm  := TRUSTEE_IS_SID;
  ea.Trustee.TrusteeType  := TRUSTEE_IS_WELL_KNOWN_GROUP;
  ea.Trustee.ptstrName    := PChar(everyone_sid);

  acl := nil;
  SetEntriesInAcl( 1, @ea, nil, acl);

  sd := PSECURITY_DESCRIPTOR( LocalAlloc( LPTR,SECURITY_DESCRIPTOR_MIN_LENGTH));
  Win32Check( InitializeSecurityDescriptor( sd, SECURITY_DESCRIPTOR_REVISION));
  Win32Check( SetSecurityDescriptorDacl( sd, TRUE, acl, FALSE));

  sa.nLength := SizeOf(sa);
  sa.lpSecurityDescriptor := sd;
  sa.bInheritHandle       := FALSE;

  // Create an instance of the named pipe
  hPipe := Windows.CreateNamedPipe( PChar( FPipeName),        // pipe name
                                    PIPE_ACCESS_DUPLEX,       // read/write access
                                    PIPE_TYPE_MESSAGE or      // message type pipe
                                    PIPE_READMODE_MESSAGE,    // message-read mode
                                    FMaxConns,                // max. instances
                                    FBufSize,                 // output buffer size
                                    FBufSize,                 // input buffer size
                                    0,                        // client time-out
                                    @sa);                     // default security attribute

  if( hPipe = INVALID_HANDLE_VALUE) then begin
    FHandle := INVALID_HANDLE_VALUE;
    raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                      'CreateNamedPipe() failed ' + IntToStr(GetLastError));
  end;

  FHandle := hPipe;
  result  := TRUE;
end;


function TServerPipeImpl.CreateAnonPipe : Boolean;
var sd           : PSECURITY_DESCRIPTOR;
    sa           : SECURITY_ATTRIBUTES; //TSecurityAttributes;
    hCAR, hPipeW, hCAW, hPipe : THandle;
begin
  result := FALSE;

  sd := PSECURITY_DESCRIPTOR( LocalAlloc( LPTR,SECURITY_DESCRIPTOR_MIN_LENGTH));
  Win32Check( InitializeSecurityDescriptor( sd, SECURITY_DESCRIPTOR_REVISION));
  Win32Check( SetSecurityDescriptorDacl( sd, TRUE, nil, FALSE));

  sa.nLength := sizeof( sa);
  sa.lpSecurityDescriptor := sd;
  sa.bInheritHandle       := TRUE; //allow passing handle to child

  if not CreatePipe( hCAR, hPipeW, @sa, FBufSize) then begin   //create stdin pipe
    Console.WriteLine( 'TPipeServer CreatePipe (anon) failed, '+SysErrorMessage(GetLastError));
    Exit;
  end;

  if not CreatePipe( hPipe, hCAW, @sa, FBufSize) then begin  //create stdout pipe
    Console.WriteLine( 'TPipeServer CreatePipe (anon) failed, '+SysErrorMessage(GetLastError));
    CloseHandle( hCAR);
    CloseHandle( hPipeW);
    Exit;
  end;

  FClientAnonRead  := hCAR;
  FClientAnonWrite := hCAW;
  FHandle          := hPipe;
  FWriteHandle     := hPipeW;

  result := TRUE;
end;



end.



