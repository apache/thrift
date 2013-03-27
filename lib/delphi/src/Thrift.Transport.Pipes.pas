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
  Windows, SysUtils, Math, AccCtrl, AclAPI, SyncObjs,
  Thrift.Transport,
  Thrift.Stream;

const
  DEFAULT_THRIFT_PIPE_TIMEOUT = 5 * 1000; // ms


type
  //--- Pipe Streams ---


  TPipeStreamBase = class( TThriftStreamImpl)
  strict protected
    FPipe    : THandle;
    FTimeout : DWORD;

    procedure Write( const buffer: TBytes; offset: Integer; count: Integer); override;
    function  Read( var buffer: TBytes; offset: Integer; count: Integer): Integer; override;
    //procedure Open; override; - see derived classes
    procedure Close; override;
    procedure Flush; override;

    function IsOpen: Boolean; override;
    function ToArray: TBytes; override;
  public
    constructor Create( const aTimeOut : DWORD = DEFAULT_THRIFT_PIPE_TIMEOUT);
    destructor Destroy;  override;
  end;


  TNamedPipeStreamImpl = class sealed( TPipeStreamBase)
  private
    FPipeName  : string;
    FShareMode : DWORD;
    FSecurityAttribs : PSecurityAttributes;

  protected
    procedure Open; override;

  public
    constructor Create( const aPipeName : string;
                        const aShareMode: DWORD = 0;
                        const aSecurityAttributes: PSecurityAttributes = nil;
                        const aTimeOut : DWORD = DEFAULT_THRIFT_PIPE_TIMEOUT);  overload;
  end;


  THandlePipeStreamImpl = class sealed( TPipeStreamBase)
  private
    FSrcHandle : THandle;

  protected
    procedure Open; override;

  public
    constructor Create( const aPipeHandle : THandle; aOwnsHandle : Boolean);  overload;
    destructor Destroy;  override;
  end;


  //--- Pipe Transports ---


  IPipeTransport = interface( IStreamTransport)
    ['{5E05CC85-434F-428F-BFB2-856A168B5558}']
  end;


  TPipeTransportBase = class( TStreamTransportImpl, IPipeTransport)
  public
    // ITransport
    function  GetIsOpen: Boolean; override;
    procedure Open; override;
    procedure Close; override;
  end;


  TNamedPipeTransportClientEndImpl = class( TPipeTransportBase)
  public
    // Named pipe constructors
    constructor Create( aPipe : THandle; aOwnsHandle : Boolean); overload;
    constructor Create( const aPipeName : string;
                        const aShareMode: DWORD = 0;
                        const aSecurityAttributes: PSecurityAttributes = nil;
                        const aTimeOut : DWORD = DEFAULT_THRIFT_PIPE_TIMEOUT);  overload;
  end;


  TNamedPipeTransportServerEndImpl = class( TNamedPipeTransportClientEndImpl)
  strict private
    FHandle : THandle;
  public
    // ITransport
    procedure Close; override;
    constructor Create( aPipe : THandle; aOwnsHandle : Boolean); reintroduce;
  end;


  TAnonymousPipeTransportImpl = class( TPipeTransportBase)
  public
    // Anonymous pipe constructor
    constructor Create( const aPipeRead, aPipeWrite : THandle; aOwnsHandles : Boolean); overload;
  end;


  //--- Server Transports ---


  IAnonymousPipeServerTransport = interface( IServerTransport)
    ['{7AEE6793-47B9-4E49-981A-C39E9108E9AD}']
    // Server side anonymous pipe ends
    function ReadHandle : THandle;
    function WriteHandle : THandle;
    // Client side anonymous pipe ends
    function ClientAnonRead : THandle;
    function ClientAnonWrite  : THandle;
  end;


  INamedPipeServerTransport = interface( IServerTransport)
    ['{9DF9EE48-D065-40AF-8F67-D33037D3D960}']
    function Handle : THandle;
  end;


  TPipeServerTransportBase = class( TServerTransportImpl)
  protected
    FStopServer   : Boolean;
    procedure InternalClose; virtual; abstract;
  public
    procedure Listen; override;
    procedure Close; override;
  end;


  TAnonymousPipeServerTransportImpl = class( TPipeServerTransportBase, IAnonymousPipeServerTransport)
  private
    FBufSize      : DWORD;

    // Server side anonymous pipe handles
    FReadHandle,
    FWriteHandle : THandle;

    //Client side anonymous pipe handles
    FClientAnonRead,
    FClientAnonWrite  : THandle;

  protected
    function AcceptImpl: ITransport; override;

    function CreateAnonPipe : Boolean;

    // IAnonymousPipeServerTransport
    function ReadHandle : THandle;
    function WriteHandle : THandle;
    function ClientAnonRead : THandle;
    function ClientAnonWrite  : THandle;

    procedure InternalClose; override;

  public
    constructor Create( aBufsize : Cardinal = 4096);
  end;


  TNamedPipeServerTransportImpl = class( TPipeServerTransportBase, INamedPipeServerTransport)
  private
    FPipeName     : string;
    FMaxConns     : DWORD;
    FBufSize      : DWORD;
    FTimeout      : DWORD;
    FHandle       : THandle;
    FConnected    : Boolean;

  protected
    function AcceptImpl: ITransport; override;
    function CreateNamedPipe : THandle;
    function CreateTransportInstance : ITransport;

    // INamedPipeServerTransport
    function Handle : THandle;
    procedure InternalClose; override;

  public
    constructor Create( aPipename : string; aBufsize : Cardinal = 4096;
                        aMaxConns : Cardinal = PIPE_UNLIMITED_INSTANCES;
                        aTimeOut : Cardinal = 0);
  end;


implementation


procedure ClosePipeHandle( var hPipe : THandle);
begin
  if hPipe <> INVALID_HANDLE_VALUE
  then try
    CloseHandle( hPipe);
  finally
    hPipe := INVALID_HANDLE_VALUE;
  end;
end;


function DuplicatePipeHandle( const hSource : THandle) : THandle;
begin
  if not DuplicateHandle( GetCurrentProcess, hSource,
                          GetCurrentProcess, @result,
                          0, FALSE, DUPLICATE_SAME_ACCESS)
  then raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                         'DuplicateHandle: '+SysErrorMessage(GetLastError));
end;



{ TPipeStreamBase }


constructor TPipeStreamBase.Create( const aTimeOut : DWORD = DEFAULT_THRIFT_PIPE_TIMEOUT);
begin
  inherited Create;
  FPipe    := INVALID_HANDLE_VALUE;
  FTimeout := aTimeOut;
end;


destructor TPipeStreamBase.Destroy;
begin
  try
    Close;
  finally
    inherited Destroy;
  end;
end;


procedure TPipeStreamBase.Close;
begin
  ClosePipeHandle( FPipe);
end;


procedure TPipeStreamBase.Flush;
begin
  // nothing to do
end;


function TPipeStreamBase.IsOpen: Boolean;
begin
  result := (FPipe <> INVALID_HANDLE_VALUE);
end;


procedure TPipeStreamBase.Write(const buffer: TBytes; offset, count: Integer);
var cbWritten : DWORD;
begin
  if not IsOpen
  then raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                         'Called write on non-open pipe');

  if not WriteFile( FPipe, buffer[offset], count, cbWritten, nil)
  then raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                         'Write to pipe failed');
end;


function TPipeStreamBase.Read( var buffer: TBytes; offset, count: Integer): Integer;
var cbRead, dwErr  : DWORD;
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

      dwErr := GetLastError;
      if (dwErr = ERROR_INVALID_HANDLE)
      or (dwErr = ERROR_BROKEN_PIPE)
      or (dwErr = ERROR_PIPE_NOT_CONNECTED)
      then begin
        result := 0;  // other side closed the pipe
        Exit;
      end;

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


function TPipeStreamBase.ToArray: TBytes;
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


{ TNamedPipeStreamImpl }


constructor TNamedPipeStreamImpl.Create( const aPipeName : string; const aShareMode: DWORD;
                                         const aSecurityAttributes: PSecurityAttributes;
                                         const aTimeOut : DWORD);
begin
  inherited Create( aTimeout);

  FPipeName        := aPipeName;
  FShareMode       := aShareMode;
  FSecurityAttribs := aSecurityAttributes;

  if Copy(FPipeName,1,2) <> '\\'
  then FPipeName := '\\.\pipe\' + FPipeName;  // assume localhost
end;


procedure TNamedPipeStreamImpl.Open;
var hPipe    : THandle;
    dwMode   : DWORD;
begin
  if IsOpen then Exit;

  // open that thingy

  if not WaitNamedPipe( PChar(FPipeName), FTimeout)
  then raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                         'Unable to open pipe, '+SysErrorMessage(GetLastError));

  hPipe := CreateFile( PChar( FPipeName),
                       GENERIC_READ or GENERIC_WRITE,
                       FShareMode,        // sharing
                       FSecurityAttribs,  // security attributes
                       OPEN_EXISTING,     // opens existing pipe
                       0,                 // default attributes
                       0);                // no template file

  if hPipe = INVALID_HANDLE_VALUE
  then raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                         'Unable to open pipe, '+SysErrorMessage(GetLastError));

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


{ THandlePipeStreamImpl }


constructor THandlePipeStreamImpl.Create( const aPipeHandle : THandle; aOwnsHandle : Boolean);
begin
  inherited Create( DEFAULT_THRIFT_PIPE_TIMEOUT);

  if aOwnsHandle
  then FSrcHandle := aPipeHandle
  else FSrcHandle := DuplicatePipeHandle( aPipeHandle);

  Open;
end;


destructor THandlePipeStreamImpl.Destroy;
begin
  try
    ClosePipeHandle( FSrcHandle);
  finally
    inherited Destroy;
  end;
end;


procedure THandlePipeStreamImpl.Open;
begin
  if not IsOpen
  then FPipe := DuplicatePipeHandle( FSrcHandle);
end;


{ TPipeTransportBase }


function TPipeTransportBase.GetIsOpen: Boolean;
begin
  result := (FInputStream <> nil)  and (FInputStream.IsOpen)
        and (FOutputStream <> nil) and (FOutputStream.IsOpen);
end;


procedure TPipeTransportBase.Open;
begin
  FInputStream.Open;
  FOutputStream.Open;
end;


procedure TPipeTransportBase.Close;
begin
  FInputStream.Close;
  FOutputStream.Close;
end;


{ TNamedPipeTransportClientEndImpl }


constructor TNamedPipeTransportClientEndImpl.Create( const aPipeName : string; const aShareMode: DWORD;
                                   const aSecurityAttributes: PSecurityAttributes;
                                   const aTimeOut : DWORD);
// Named pipe constructor
begin
  inherited Create( nil, nil);
  FInputStream  := TNamedPipeStreamImpl.Create( aPipeName, aShareMode, aSecurityAttributes, aTimeOut);
  FOutputStream := FInputStream;  // true for named pipes
end;


constructor TNamedPipeTransportClientEndImpl.Create( aPipe : THandle; aOwnsHandle : Boolean);
// Named pipe constructor
begin
  inherited Create( nil, nil);
  FInputStream  := THandlePipeStreamImpl.Create( aPipe, aOwnsHandle);
  FOutputStream := FInputStream;  // true for named pipes
end;


{ TNamedPipeTransportServerEndImpl }


constructor TNamedPipeTransportServerEndImpl.Create( aPipe : THandle; aOwnsHandle : Boolean);
// Named pipe constructor
begin
  FHandle := DuplicatePipeHandle( aPipe);
  inherited Create( aPipe, aOwnsHandle);
end;


procedure TNamedPipeTransportServerEndImpl.Close;
begin
  FlushFileBuffers( FHandle);
  DisconnectNamedPipe( FHandle);  // force client off the pipe
  ClosePipeHandle( FHandle);

  inherited Close;
end;


{ TAnonymousPipeTransportImpl }


constructor TAnonymousPipeTransportImpl.Create( const aPipeRead, aPipeWrite : THandle; aOwnsHandles : Boolean);
// Anonymous pipe constructor
begin
  inherited Create( nil, nil);
  FInputStream  := THandlePipeStreamImpl.Create( aPipeRead, aOwnsHandles);
  FOutputStream := THandlePipeStreamImpl.Create( aPipeWrite, aOwnsHandles);
end;


{ TPipeServerTransportBase }


procedure TPipeServerTransportBase.Listen;
begin
  FStopServer := FALSE;
end;


procedure TPipeServerTransportBase.Close;
begin
  FStopServer := TRUE;
  InternalClose;
end;


{ TAnonymousPipeServerTransportImpl }


constructor TAnonymousPipeServerTransportImpl.Create( aBufsize : Cardinal);
// Anonymous pipe CTOR
begin
  inherited Create;
  FBufsize  := aBufSize;
  FReadHandle := INVALID_HANDLE_VALUE;
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


function TAnonymousPipeServerTransportImpl.AcceptImpl: ITransport;
var buf    : Byte;
    br     : DWORD;
begin
  // This 0-byte read serves merely as a blocking call.
  if not ReadFile( FReadHandle, buf, 0, br, nil)
  and (GetLastError() <> ERROR_MORE_DATA)
  then raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                         'TServerPipe unable to initiate pipe communication');

  // create the transport impl
  result := TAnonymousPipeTransportImpl.Create( FReadHandle, FWriteHandle, FALSE);
end;


procedure TAnonymousPipeServerTransportImpl.InternalClose;
begin
  ClosePipeHandle( FReadHandle);
  ClosePipeHandle( FWriteHandle);
  ClosePipeHandle( FClientAnonRead);
  ClosePipeHandle( FClientAnonWrite);
end;


function TAnonymousPipeServerTransportImpl.ReadHandle : THandle;
begin
  result := FReadHandle;
end;


function TAnonymousPipeServerTransportImpl.WriteHandle : THandle;
begin
  result := FWriteHandle;
end;


function TAnonymousPipeServerTransportImpl.ClientAnonRead : THandle;
begin
  result := FClientAnonRead;
end;


function TAnonymousPipeServerTransportImpl.ClientAnonWrite  : THandle;
begin
  result := FClientAnonWrite;
end;


function TAnonymousPipeServerTransportImpl.CreateAnonPipe : Boolean;
var sd           : PSECURITY_DESCRIPTOR;
    sa           : SECURITY_ATTRIBUTES; //TSecurityAttributes;
    hCAR, hPipeW, hCAW, hPipe : THandle;
begin
  result := FALSE;

  sd := PSECURITY_DESCRIPTOR( LocalAlloc( LPTR,SECURITY_DESCRIPTOR_MIN_LENGTH));
  try
    Win32Check( InitializeSecurityDescriptor( sd, SECURITY_DESCRIPTOR_REVISION));
    Win32Check( SetSecurityDescriptorDacl( sd, TRUE, nil, FALSE));

    sa.nLength := sizeof( sa);
    sa.lpSecurityDescriptor := sd;
    sa.bInheritHandle       := TRUE; //allow passing handle to child

    if not CreatePipe( hCAR, hPipeW, @sa, FBufSize) then begin   //create stdin pipe
      raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                        'TServerPipe CreatePipe (anon) failed, '+SysErrorMessage(GetLastError));
      Exit;
    end;

    if not CreatePipe( hPipe, hCAW, @sa, FBufSize) then begin  //create stdout pipe
      CloseHandle( hCAR);
      CloseHandle( hPipeW);
      raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                        'TServerPipe CreatePipe (anon) failed, '+SysErrorMessage(GetLastError));
      Exit;
    end;

    FClientAnonRead  := hCAR;
    FClientAnonWrite := hCAW;
    FReadHandle      := hPipe;
    FWriteHandle     := hPipeW;

    result := TRUE;
  
  finally
    if sd <> nil then LocalFree( Cardinal(sd));
  end;
end;


{ TNamedPipeServerTransportImpl }


constructor TNamedPipeServerTransportImpl.Create( aPipename : string; aBufsize, aMaxConns, aTimeOut : Cardinal);
// Named Pipe CTOR
begin
  inherited Create;
  FPipeName  := aPipename;
  FBufsize   := aBufSize;
  FMaxConns  := Max( 1, Min( PIPE_UNLIMITED_INSTANCES, aMaxConns));
  FHandle    := INVALID_HANDLE_VALUE;
  FTimeout   := aTimeOut;
  FConnected := FALSE;

  if Copy(FPipeName,1,2) <> '\\'
  then FPipeName := '\\.\pipe\' + FPipeName;  // assume localhost
end;


function TNamedPipeServerTransportImpl.AcceptImpl: ITransport;
var dwError, dwWait, dwDummy : DWORD;
    overlapped : TOverlapped;
    event      : TEvent;
begin
  FillChar( overlapped, SizeOf(overlapped), 0);
  event := TEvent.Create( nil, TRUE, FALSE, '');  // always ManualReset, see MSDN
  try
    overlapped.hEvent := event.Handle;

    ASSERT( not FConnected);
    while not FConnected do begin
      InternalClose;
      if FStopServer then Abort;
      CreateNamedPipe;

      // Wait for the client to connect; if it succeeds, the
      // function returns a nonzero value. If the function returns
      // zero, GetLastError should return ERROR_PIPE_CONNECTED.
      if ConnectNamedPipe( Handle, @overlapped)
      then FConnected := TRUE
      else begin
        // ConnectNamedPipe() returns FALSE for OverlappedIO, even if connected.
        // We have to check GetLastError() explicitly to find out
        dwError := GetLastError;
        case dwError of
          ERROR_PIPE_CONNECTED : begin
            FConnected := TRUE;  // special case: pipe immediately connected
          end;

          ERROR_IO_PENDING : begin
            dwWait := WaitForSingleObject( overlapped.hEvent, DEFAULT_THRIFT_PIPE_TIMEOUT);
            FConnected := (dwWait = WAIT_OBJECT_0)
                      and GetOverlappedResult( Handle, overlapped, dwDummy, TRUE);
          end;

        else
          InternalClose;
          raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                            'Client connection failed');
        end;
      end;
    end;

    // create the transport impl
    result := CreateTransportInstance;

  finally
    event.Free;
  end;
end;


function TNamedPipeServerTransportImpl.CreateTransportInstance : ITransport;
// create the transport impl
var hPipe : THandle;
begin
  hPipe := THandle( InterlockedExchangePointer( Pointer(FHandle), Pointer(INVALID_HANDLE_VALUE)));
  try
    FConnected := FALSE;
    result := TNamedPipeTransportServerEndImpl.Create( hPipe, TRUE);
  except
    ClosePipeHandle(hPipe);
    raise;
  end;
end;


procedure TNamedPipeServerTransportImpl.InternalClose;
var hPipe : THandle;
begin
  hPipe := THandle( InterlockedExchangePointer( Pointer(FHandle), Pointer(INVALID_HANDLE_VALUE)));
  if hPipe = INVALID_HANDLE_VALUE then Exit;

  try
    if FConnected
    then FlushFileBuffers( hPipe)
    else CancelIo( hPipe);
    DisconnectNamedPipe( hPipe);
  finally
    ClosePipeHandle( hPipe);
    FConnected := FALSE;
  end;
end;


function TNamedPipeServerTransportImpl.Handle : THandle;
begin
  {$IFDEF WIN64}
  result := THandle( InterlockedExchangeAdd64( Integer(FHandle), 0));
  {$ELSE}
  result := THandle( InterlockedExchangeAdd( Integer(FHandle), 0));
  {$ENDIF}
end;


function TNamedPipeServerTransportImpl.CreateNamedPipe : THandle;
var SIDAuthWorld : SID_IDENTIFIER_AUTHORITY ;
    everyone_sid : PSID;
    ea           : EXPLICIT_ACCESS;
    acl          : PACL;
    sd           : PSECURITY_DESCRIPTOR;
    sa           : SECURITY_ATTRIBUTES;
const
  SECURITY_WORLD_SID_AUTHORITY  : TSIDIdentifierAuthority = (Value : (0,0,0,0,0,1));
  SECURITY_WORLD_RID = $00000000;
begin
  sd := nil;
  everyone_sid := nil;
  try
    ASSERT( (FHandle = INVALID_HANDLE_VALUE) and not FConnected);

    // Windows - set security to allow non-elevated apps
    // to access pipes created by elevated apps.
    SIDAuthWorld := SECURITY_WORLD_SID_AUTHORITY;
    AllocateAndInitializeSid( SIDAuthWorld, 1, SECURITY_WORLD_RID, 0, 0, 0, 0, 0, 0, 0, everyone_sid);

    ZeroMemory( @ea, SizeOf(ea));
    ea.grfAccessPermissions := GENERIC_ALL; //SPECIFIC_RIGHTS_ALL or STANDARD_RIGHTS_ALL;
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
    result := Windows.CreateNamedPipe( PChar( FPipeName),        // pipe name
                                       PIPE_ACCESS_DUPLEX or     // read/write access
                                       FILE_FLAG_OVERLAPPED,     // async mode
                                       PIPE_TYPE_MESSAGE or      // message type pipe
                                       PIPE_READMODE_MESSAGE,    // message-read mode
                                       FMaxConns,                // max. instances
                                       FBufSize,                 // output buffer size
                                       FBufSize,                 // input buffer size
                                       FTimeout,                 // time-out, see MSDN
                                       @sa);                     // default security attribute

    if( result <> INVALID_HANDLE_VALUE)
    then InterlockedExchangePointer( Pointer(FHandle), Pointer(result))
    else raise TTransportException.Create( TTransportException.TExceptionType.NotOpen,
                                           'CreateNamedPipe() failed ' + IntToStr(GetLastError));

  finally
    if sd <> nil then LocalFree( Cardinal( sd));
    if acl <> nil then LocalFree( Cardinal( acl));
    if everyone_sid <> nil then FreeSid(everyone_sid);
  end;
end;



end.



