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
unit Thrift.Transport;

{$I Thrift.Defines.inc}
{$SCOPEDENUMS ON}

interface

uses
  Classes,
  SysUtils,
  Math,
  Generics.Collections,
  {$IFDEF OLD_UNIT_NAMES}
    WinSock, Sockets,
  {$ELSE}
    Winapi.WinSock,
    {$IFDEF OLD_SOCKETS}
      Web.Win.Sockets,
    {$ELSE}
      Thrift.Socket,
    {$ENDIF}
  {$ENDIF}
  Thrift.Configuration,
  Thrift.Collections,
  Thrift.Exception,
  Thrift.Utils,
  Thrift.WinHTTP,
  Thrift.Stream;

const
  DEFAULT_MAX_MESSAGE_SIZE = 100 * 1024 * 1024; // 100 MB
  DEFAULT_THRIFT_TIMEOUT = 5 * 1000; // ms

type
  IStreamTransport = interface;

  ITransport = interface
    ['{52F81383-F880-492F-8AA7-A66B85B93D6B}']
    function GetIsOpen: Boolean;
    property IsOpen: Boolean read GetIsOpen;
    function Peek: Boolean;
    procedure Open;
    procedure Close;

    function Read(var buf: TBytes; off: Integer; len: Integer): Integer; overload;
    function Read(const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer): Integer; overload;
    function ReadAll(var buf: TBytes; off: Integer; len: Integer): Integer; overload;
    function ReadAll(const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer): Integer; overload;
    procedure Write( const buf: TBytes); overload;
    procedure Write( const buf: TBytes; off: Integer; len: Integer); overload;
    procedure Write( const pBuf : Pointer; off, len : Integer); overload;
    procedure Write( const pBuf : Pointer; len : Integer); overload;
    procedure Flush;

    function  Configuration : IThriftConfiguration;
    function  MaxMessageSize : Integer;
    procedure ResetConsumedMessageSize( const knownSize : Int64 = -1);
    procedure CheckReadBytesAvailable( const numBytes : Int64);
    procedure UpdateKnownMessageSize( const size : Int64);
  end;

  TTransportBase = class abstract( TInterfacedObject)
  strict protected
    function GetIsOpen: Boolean; virtual; abstract;
    property IsOpen: Boolean read GetIsOpen;
    function Peek: Boolean; virtual;
    procedure Open(); virtual; abstract;
    procedure Close(); virtual; abstract;

    function Read(var buf: TBytes; off: Integer; len: Integer): Integer; overload; inline;
    function Read(const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer): Integer; overload; virtual; abstract;
    function ReadAll(var buf: TBytes; off: Integer; len: Integer): Integer;  overload; inline;
    function ReadAll(const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer): Integer; overload; virtual;
    procedure Write( const buf: TBytes); overload; inline;
    procedure Write( const buf: TBytes; off: Integer; len: Integer); overload; inline;
    procedure Write( const pBuf : Pointer; len : Integer); overload; inline;
    procedure Write( const pBuf : Pointer; off, len : Integer); overload; virtual; abstract;
    procedure Flush; virtual;

    function  Configuration : IThriftConfiguration; virtual; abstract;
    procedure UpdateKnownMessageSize( const size : Int64); virtual; abstract;
  end;

  // base class for all endpoint transports, e.g. sockets, pipes or HTTP
  TEndpointTransportBase = class abstract( TTransportBase, ITransport)
  strict private
    FRemainingMessageSize : Int64;
    FKnownMessageSize : Int64;
    FConfiguration : IThriftConfiguration;
  strict protected
    function  Configuration : IThriftConfiguration; override;
    function  MaxMessageSize : Integer;
    property  RemainingMessageSize : Int64 read FRemainingMessageSize;
    property  KnownMessageSize : Int64 read FKnownMessageSize;
    procedure ResetConsumedMessageSize( const newSize : Int64 = -1);
    procedure UpdateKnownMessageSize(const size : Int64); override;
    procedure CheckReadBytesAvailable(const numBytes : Int64); inline;
    procedure CountConsumedMessageBytes(const numBytes : Int64); inline;
  public
    constructor Create( const aConfig : IThriftConfiguration);  reintroduce;
  end;

  // base class for all layered transports, e.g. framed
  TLayeredTransportBase<T : ITransport> = class abstract( TTransportBase, ITransport)
  strict private
    FTransport : T;
  strict protected
    property  InnerTransport : T read FTransport;
    function  GetUnderlyingTransport: ITransport;
    function  Configuration : IThriftConfiguration; override;
    procedure UpdateKnownMessageSize( const size : Int64); override;
    function  MaxMessageSize : Integer;  inline;
    procedure ResetConsumedMessageSize( const knownSize : Int64 = -1);  inline;
    procedure CheckReadBytesAvailable( const numBytes : Int64);   virtual;
  public
    constructor Create( const aTransport: T); reintroduce;
    property UnderlyingTransport: ITransport read GetUnderlyingTransport;
  end;

  TTransportException = class abstract( TException)
  public
    type
      TExceptionType = (
        Unknown,
        NotOpen,
        AlreadyOpen,
        TimedOut,
        EndOfFile,
        BadArgs,
        Interrupted,
        CorruptedData
      );
  strict protected
    constructor HiddenCreate(const Msg: string);
    class function GetType: TExceptionType;  virtual; abstract;
  public
    class function Create( aType: TExceptionType): TTransportException; overload; deprecated 'Use specialized TTransportException types (or regenerate from IDL)';
    class function Create( const msg: string): TTransportException; reintroduce; overload; deprecated 'Use specialized TTransportException types (or regenerate from IDL)';
    class function Create( aType: TExceptionType; const msg: string): TTransportException; overload; deprecated 'Use specialized TTransportException types (or regenerate from IDL)';
    property Type_: TExceptionType read GetType;
  end;

  // Needed to remove deprecation warning
  TTransportExceptionSpecialized = class abstract (TTransportException)
  public
    constructor Create(const Msg: string);
  end;

  TTransportExceptionUnknown = class (TTransportExceptionSpecialized)
  strict protected
    class function GetType: TTransportException.TExceptionType;  override;
  end;

  TTransportExceptionNotOpen = class (TTransportExceptionSpecialized)
  strict protected
    class function GetType: TTransportException.TExceptionType;  override;
  end;

  TTransportExceptionAlreadyOpen = class (TTransportExceptionSpecialized)
  strict protected
    class function GetType: TTransportException.TExceptionType;  override;
  end;

  TTransportExceptionTimedOut = class (TTransportExceptionSpecialized)
  strict protected
    class function GetType: TTransportException.TExceptionType;  override;
  end;

  TTransportExceptionEndOfFile = class (TTransportExceptionSpecialized)
  strict protected
    class function GetType: TTransportException.TExceptionType;  override;
  end;

  TTransportExceptionBadArgs = class (TTransportExceptionSpecialized)
  strict protected
    class function GetType: TTransportException.TExceptionType;  override;
  end;

  TTransportExceptionInterrupted = class (TTransportExceptionSpecialized)
  strict protected
    class function GetType: TTransportException.TExceptionType;  override;
  end;

  TTransportExceptionCorruptedData = class (TTransportExceptionSpecialized)
  protected
    class function GetType: TTransportException.TExceptionType;  override;
  end;

  TSecureProtocol = (
    SSL_2, SSL_3, TLS_1,   // outdated, for compatibilty only
    TLS_1_1, TLS_1_2       // secure (as of today)
  );

  TSecureProtocols = set of TSecureProtocol;

  IHTTPClient = interface( ITransport )
    ['{7BF615DD-8680-4004-A5B2-88947BA3BA3D}']
    procedure SetDnsResolveTimeout(const Value: Integer);
    function GetDnsResolveTimeout: Integer;
    procedure SetConnectionTimeout(const Value: Integer);
    function GetConnectionTimeout: Integer;
    procedure SetSendTimeout(const Value: Integer);
    function GetSendTimeout: Integer;
    procedure SetReadTimeout(const Value: Integer);
    function GetReadTimeout: Integer;
    function GetCustomHeaders: IThriftDictionary<string,string>;
    procedure SendRequest;
    function GetSecureProtocols : TSecureProtocols;
    procedure SetSecureProtocols( const value : TSecureProtocols);

    property DnsResolveTimeout: Integer read GetDnsResolveTimeout write SetDnsResolveTimeout;
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;
    property SendTimeout: Integer read GetSendTimeout write SetSendTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property CustomHeaders: IThriftDictionary<string,string> read GetCustomHeaders;
    property SecureProtocols : TSecureProtocols read GetSecureProtocols write SetSecureProtocols;
  end;

  IServerTransport = interface
    ['{FA01363F-6B40-482F-971E-4A085535EFC8}']
    procedure Listen;
    procedure Close;
    function Accept( const fnAccepting: TProc): ITransport;
    function Configuration : IThriftConfiguration;
  end;

  TServerTransportImpl = class( TInterfacedObject, IServerTransport)
  strict private
    FConfig : IThriftConfiguration;
  strict protected
    function  Configuration : IThriftConfiguration;
    procedure Listen; virtual; abstract;
    procedure Close; virtual; abstract;
    function  Accept( const fnAccepting: TProc): ITransport;  virtual; abstract;
  public
    constructor Create( const aConfig : IThriftConfiguration);
  end;

  ITransportFactory = interface
    ['{DD809446-000F-49E1-9BFF-E0D0DC76A9D7}']
    function GetTransport( const aTransport: ITransport): ITransport;
  end;

  TTransportFactoryImpl = class ( TInterfacedObject, ITransportFactory)
  strict protected
    function GetTransport( const aTransport: ITransport): ITransport; virtual;
  end;


  TTcpSocketStreamImpl = class( TThriftStreamImpl)
{$IFDEF OLD_SOCKETS}
  strict private type
    TWaitForData = ( wfd_HaveData, wfd_Timeout, wfd_Error);
  strict private
    FTcpClient : TCustomIpClient;
    FTimeout : Integer;
    function Select( ReadReady, WriteReady, ExceptFlag: PBoolean;
                     TimeOut: Integer; var wsaError : Integer): Integer;
    function WaitForData( TimeOut : Integer; pBuf : Pointer; DesiredBytes: Integer;
                          var wsaError, bytesReady : Integer): TWaitForData;
{$ELSE}
    FTcpClient: TSocket;
  strict protected const
    SLEEP_TIME = 200;
{$ENDIF}
  strict protected
    procedure Write( const pBuf : Pointer; offset, count: Integer); override;
    function Read( const pBuf : Pointer; const buflen : Integer; offset: Integer; count: Integer): Integer; override;
    procedure Open; override;
    procedure Close; override;
    procedure Flush; override;

    function IsOpen: Boolean; override;
    function ToArray: TBytes; override;
  public
{$IFDEF OLD_SOCKETS}
    constructor Create( const aTcpClient: TCustomIpClient; const aTimeout : Integer = DEFAULT_THRIFT_TIMEOUT);
{$ELSE}
    constructor Create( const aTcpClient: TSocket; const aTimeout : Longword = DEFAULT_THRIFT_TIMEOUT);
{$ENDIF}
  end;

  IStreamTransport = interface( ITransport )
    ['{A8479B47-2A3E-4421-A9A0-D5A9EDCC634A}']
    function GetInputStream: IThriftStream;
    function GetOutputStream: IThriftStream;
    property InputStream : IThriftStream read GetInputStream;
    property OutputStream : IThriftStream read GetOutputStream;
  end;

  TStreamTransportImpl = class( TEndpointTransportBase, IStreamTransport)
  strict protected
    FInputStream : IThriftStream;
    FOutputStream : IThriftStream;
  strict protected
    function GetIsOpen: Boolean; override;

    function GetInputStream: IThriftStream;
    function GetOutputStream: IThriftStream;

  strict protected
    procedure Open; override;
    procedure Close; override;
    procedure Flush; override;
    function  Read( const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer): Integer; override;
    procedure Write( const pBuf : Pointer; off, len : Integer); override;
  public
    constructor Create( const aInputStream, aOutputStream : IThriftStream; const aConfig : IThriftConfiguration = nil);  reintroduce;
    destructor Destroy; override;

    property InputStream : IThriftStream read GetInputStream;
    property OutputStream : IThriftStream read GetOutputStream;
  end;

  TBufferedStreamImpl = class( TThriftStreamImpl)
  strict private
    FStream : IThriftStream;
    FBufSize : Integer;
    FReadBuffer : TMemoryStream;
    FWriteBuffer : TMemoryStream;
  strict protected
    procedure Write( const pBuf : Pointer; offset: Integer; count: Integer); override;
    function Read( const pBuf : Pointer; const buflen : Integer; offset: Integer; count: Integer): Integer; override;
    procedure Open;  override;
    procedure Close; override;
    procedure Flush; override;
    function IsOpen: Boolean; override;
    function ToArray: TBytes; override;
    function Size : Int64; override;
    function Position : Int64; override;
  public
    constructor Create( const aStream: IThriftStream; const aBufSize : Integer);
    destructor Destroy; override;
  end;

  TServerSocketImpl = class( TServerTransportImpl)
  strict private
{$IFDEF OLD_SOCKETS}
    FServer : TTcpServer;
    FPort : Integer;
    FClientTimeout : Integer;
{$ELSE}
    FServer: TServerSocket;
{$ENDIF}
    FUseBufferedSocket : Boolean;
    FOwnsServer : Boolean;

  strict protected
    function Accept( const fnAccepting: TProc) : ITransport; override;

  public
    {$IFDEF OLD_SOCKETS}
    constructor Create( const aServer: TTcpServer; const aClientTimeout : Integer = DEFAULT_THRIFT_TIMEOUT; const aConfig : IThriftConfiguration = nil);  overload;
    constructor Create( const aPort: Integer; const aClientTimeout: Integer = DEFAULT_THRIFT_TIMEOUT; aUseBufferedSockets: Boolean = FALSE; const aConfig : IThriftConfiguration = nil);  overload;
    {$ELSE}
    constructor Create( const aServer: TServerSocket; const aClientTimeout: Longword = DEFAULT_THRIFT_TIMEOUT; const aConfig : IThriftConfiguration = nil);  overload;
    constructor Create( const aPort: Integer; const aClientTimeout: Longword = DEFAULT_THRIFT_TIMEOUT; aUseBufferedSockets: Boolean = FALSE; const aConfig : IThriftConfiguration = nil);  overload;
    {$ENDIF}

    destructor Destroy; override;
    procedure Listen; override;
    procedure Close; override;
  end;

  TBufferedTransportImpl = class( TLayeredTransportBase<IStreamTransport>)
  strict private
    FInputBuffer : IThriftStream;
    FOutputBuffer : IThriftStream;
    FBufSize : Integer;

    procedure InitBuffers;
  strict protected
    function GetIsOpen: Boolean; override;
    procedure Flush; override;
  public
    type
      TFactory = class( TTransportFactoryImpl )
      public
        function GetTransport( const aTransport: ITransport): ITransport; override;
      end;

    constructor Create( const aTransport : IStreamTransport; const aBufSize: Integer = 1024);
    procedure Open(); override;
    procedure Close(); override;
    function  Read( const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer): Integer; override;
    procedure Write( const pBuf : Pointer; off, len : Integer); override;
    procedure CheckReadBytesAvailable( const value : Int64); override;
    property IsOpen: Boolean read GetIsOpen;
  end;

  TSocketImpl = class(TStreamTransportImpl)
  strict private
{$IFDEF OLD_SOCKETS}
    FClient : TCustomIpClient;
{$ELSE}
    FClient: TSocket;
{$ENDIF}
    FOwnsClient : Boolean;
    FHost : string;
    FPort : Integer;
{$IFDEF OLD_SOCKETS}
    FTimeout : Integer;
{$ELSE}
    FTimeout : Longword;
{$ENDIF}

    procedure InitSocket;
  strict protected
    function GetIsOpen: Boolean; override;
  public
{$IFDEF OLD_SOCKETS}
    constructor Create( const aClient : TCustomIpClient; const aOwnsClient : Boolean; const aTimeout: Integer = DEFAULT_THRIFT_TIMEOUT; const aConfig : IThriftConfiguration = nil); overload;
    constructor Create( const aHost: string; const aPort: Integer; const aTimeout: Integer = DEFAULT_THRIFT_TIMEOUT; const aConfig : IThriftConfiguration = nil); overload;
{$ELSE}
    constructor Create(const aClient: TSocket; const aOwnsClient: Boolean; const aConfig : IThriftConfiguration = nil); overload;
    constructor Create( const aHost: string; const aPort: Integer; const aTimeout: Longword = DEFAULT_THRIFT_TIMEOUT; const aConfig : IThriftConfiguration = nil); overload;
{$ENDIF}
    destructor Destroy; override;

    procedure Open; override;
    procedure Close; override;
{$IFDEF OLD_SOCKETS}
    property TcpClient: TCustomIpClient read FClient;
{$ELSE}
    property TcpClient: TSocket read FClient;
{$ENDIF}
    property Host : string read FHost;
    property Port: Integer read FPort;
  end;

  TFramedTransportImpl = class( TLayeredTransportBase<ITransport>)
  strict protected type
    TFramedHeader = Int32;
  strict protected
    FWriteBuffer : TMemoryStream;
    FReadBuffer : TMemoryStream;

    procedure InitWriteBuffer;
    procedure ReadFrame;

    procedure Open(); override;
    function  GetIsOpen: Boolean; override;

    procedure Close(); override;
    function  Read( const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer): Integer; override;
    procedure Write( const pBuf : Pointer; off, len : Integer); override;
    procedure CheckReadBytesAvailable( const value : Int64);  override;
    procedure Flush; override;

  public
    type
      TFactory = class( TTransportFactoryImpl )
      public
        function GetTransport( const aTransport: ITransport): ITransport; override;
      end;

    constructor Create( const aTransport: ITransport); overload;
    destructor Destroy; override;
  end;


const
  DEFAULT_THRIFT_SECUREPROTOCOLS = [ TSecureProtocol.TLS_1_1, TSecureProtocol.TLS_1_2];

implementation


{ TTransportBase }

procedure TTransportBase.Flush;
begin
  // nothing to do
end;

function TTransportBase.Peek: Boolean;
begin
  Result := IsOpen;
end;

function TTransportBase.Read(var buf: TBytes; off: Integer; len: Integer): Integer;
begin
  if Length(buf) > 0
  then result := Read( @buf[0], Length(buf), off, len)
  else result := 0;
end;

function TTransportBase.ReadAll(var buf: TBytes; off: Integer; len: Integer): Integer;
begin
  if Length(buf) > 0
  then result := ReadAll( @buf[0], Length(buf), off, len)
  else result := 0;
end;

function TTransportBase.ReadAll(const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer): Integer;
var ret : Integer;
begin
  result := 0;
  while result < len do begin
    ret := Read( pBuf, buflen, off + result, len - result);
    if ret > 0
    then Inc( result, ret)
    else raise TTransportExceptionNotOpen.Create( 'Cannot read, Remote side has closed' );
  end;
end;

procedure TTransportBase.Write( const buf: TBytes);
begin
  if Length(buf) > 0
  then Write( @buf[0], 0, Length(buf));
end;

procedure TTransportBase.Write( const buf: TBytes; off: Integer; len: Integer);
begin
  if Length(buf) > 0
  then Write( @buf[0], off, len);
end;

procedure TTransportBase.Write( const pBuf : Pointer; len : Integer);
begin
  Self.Write( pBuf, 0, len);
end;


{ TEndpointTransportBase }

constructor TEndpointTransportBase.Create( const aConfig : IThriftConfiguration);
begin
  inherited Create;

  if aConfig <> nil
  then FConfiguration := aConfig
  else FConfiguration := TThriftConfigurationImpl.Create;

  ResetConsumedMessageSize;
end;


function TEndpointTransportBase.Configuration : IThriftConfiguration;
begin
  result := FConfiguration;
end;


function TEndpointTransportBase.MaxMessageSize : Integer;
begin
  ASSERT( Configuration <> nil);
  result := Configuration.MaxMessageSize;
end;


procedure TEndpointTransportBase.ResetConsumedMessageSize( const newSize : Int64);
// Resets RemainingMessageSize to the configured maximum
begin
  // full reset
  if newSize < 0 then begin
    FKnownMessageSize := MaxMessageSize;
    FRemainingMessageSize := MaxMessageSize;
    Exit;
  end;

  // update only: message size can shrink, but not grow
  ASSERT( KnownMessageSize <= MaxMessageSize);
  if newSize > KnownMessageSize
  then raise TTransportExceptionEndOfFile.Create('MaxMessageSize reached');

  FKnownMessageSize := newSize;
  FRemainingMessageSize := newSize;
end;


procedure TEndpointTransportBase.UpdateKnownMessageSize( const size : Int64);
// Updates RemainingMessageSize to reflect then known real message size (e.g. framed transport).
// Will throw if we already consumed too many bytes.
var consumed : Int64;
begin
  consumed := KnownMessageSize - RemainingMessageSize;
  ResetConsumedMessageSize(size);
  CountConsumedMessageBytes(consumed);
end;


procedure TEndpointTransportBase.CheckReadBytesAvailable( const numBytes : Int64);
// Throws if there are not enough bytes in the input stream to satisfy a read of numBytes bytes of data
begin
  if RemainingMessageSize < numBytes
  then raise TTransportExceptionEndOfFile.Create('MaxMessageSize reached');
end;


procedure TEndpointTransportBase.CountConsumedMessageBytes( const numBytes : Int64);
// Consumes numBytes from the RemainingMessageSize.
begin
  if (RemainingMessageSize >= numBytes)
  then Dec( FRemainingMessageSize, numBytes)
  else begin
    FRemainingMessageSize := 0;
    raise TTransportExceptionEndOfFile.Create('MaxMessageSize reached');
  end;
end;

{ TLayeredTransportBase }

constructor TLayeredTransportBase<T>.Create( const aTransport: T);
begin
  inherited Create;
  FTransport := aTransport;
end;

function TLayeredTransportBase<T>.GetUnderlyingTransport: ITransport;
begin
  result := InnerTransport;
end;

function TLayeredTransportBase<T>.Configuration : IThriftConfiguration;
begin
  result := InnerTransport.Configuration;
end;

procedure TLayeredTransportBase<T>.UpdateKnownMessageSize( const size : Int64);
begin
  InnerTransport.UpdateKnownMessageSize( size);
end;


function TLayeredTransportBase<T>.MaxMessageSize : Integer;
begin
  result := InnerTransport.MaxMessageSize;
end;


procedure TLayeredTransportBase<T>.ResetConsumedMessageSize( const knownSize : Int64 = -1);
begin
  InnerTransport.ResetConsumedMessageSize( knownSize);
end;


procedure TLayeredTransportBase<T>.CheckReadBytesAvailable( const numBytes : Int64);
begin
  InnerTransport.CheckReadBytesAvailable( numBytes);
end;



{ TTransportException }

constructor TTransportException.HiddenCreate(const Msg: string);
begin
  inherited Create(Msg);
end;

class function TTransportException.Create(aType: TExceptionType): TTransportException;
begin
  //no inherited;
{$WARN SYMBOL_DEPRECATED OFF}
  Result := Create(aType, '')
{$WARN SYMBOL_DEPRECATED DEFAULT}
end;

class function TTransportException.Create(aType: TExceptionType; const msg: string): TTransportException;
begin
  case aType of
    TExceptionType.NotOpen:     Result := TTransportExceptionNotOpen.Create(msg);
    TExceptionType.AlreadyOpen: Result := TTransportExceptionAlreadyOpen.Create(msg);
    TExceptionType.TimedOut:    Result := TTransportExceptionTimedOut.Create(msg);
    TExceptionType.EndOfFile:   Result := TTransportExceptionEndOfFile.Create(msg);
    TExceptionType.BadArgs:     Result := TTransportExceptionBadArgs.Create(msg);
    TExceptionType.Interrupted: Result := TTransportExceptionInterrupted.Create(msg);
  else
    ASSERT( TExceptionType.Unknown = aType);
    Result := TTransportExceptionUnknown.Create(msg);
  end;
end;

class function TTransportException.Create(const msg: string): TTransportException;
begin
  Result := TTransportExceptionUnknown.Create(Msg);
end;

{ TTransportExceptionSpecialized }

constructor TTransportExceptionSpecialized.Create(const Msg: string);
begin
  inherited HiddenCreate(Msg);
end;

{ specialized TTransportExceptions }

class function TTransportExceptionUnknown.GetType: TTransportException.TExceptionType;
begin
  result := TExceptionType.Unknown;
end;

class function TTransportExceptionNotOpen.GetType: TTransportException.TExceptionType;
begin
  result := TExceptionType.NotOpen;
end;

class function TTransportExceptionAlreadyOpen.GetType: TTransportException.TExceptionType;
begin
  result := TExceptionType.AlreadyOpen;
end;

class function TTransportExceptionTimedOut.GetType: TTransportException.TExceptionType;
begin
  result := TExceptionType.TimedOut;
end;

class function TTransportExceptionEndOfFile.GetType: TTransportException.TExceptionType;
begin
  result := TExceptionType.EndOfFile;
end;

class function TTransportExceptionBadArgs.GetType: TTransportException.TExceptionType;
begin
  result := TExceptionType.BadArgs;
end;

class function TTransportExceptionInterrupted.GetType: TTransportException.TExceptionType;
begin
  result := TExceptionType.Interrupted;
end;

class function TTransportExceptionCorruptedData.GetType: TTransportException.TExceptionType;
begin
  result := TExceptionType.CorruptedData;
end;

{ TTransportFactoryImpl }

function TTransportFactoryImpl.GetTransport( const aTransport: ITransport): ITransport;
begin
  Result := aTransport;
end;


{ TServerTransportImpl }

constructor TServerTransportImpl.Create( const aConfig : IThriftConfiguration);
begin
  inherited Create;
  if aConfig <> nil
  then FConfig := aConfig
  else FConfig := TThriftConfigurationImpl.Create;
end;

function TServerTransportImpl.Configuration : IThriftConfiguration;
begin
  result := FConfig;
end;

{ TServerSocket }

{$IFDEF OLD_SOCKETS}
constructor TServerSocketImpl.Create( const aServer: TTcpServer; const aClientTimeout : Integer; const aConfig : IThriftConfiguration);
{$ELSE}
constructor TServerSocketImpl.Create( const aServer: TServerSocket; const aClientTimeout: Longword; const aConfig : IThriftConfiguration);
{$ENDIF}
begin
  inherited Create( aConfig);
  FServer := aServer;


{$IFDEF OLD_SOCKETS}
  FClientTimeout := aClientTimeout;
{$ELSE}
  FServer.RecvTimeout := aClientTimeout;
  FServer.SendTimeout := aClientTimeout;
{$ENDIF}
end;


{$IFDEF OLD_SOCKETS}
constructor TServerSocketImpl.Create( const aPort: Integer; const aClientTimeout: Integer; aUseBufferedSockets: Boolean; const aConfig : IThriftConfiguration);
{$ELSE}
constructor TServerSocketImpl.Create( const aPort: Integer; const aClientTimeout: Longword; aUseBufferedSockets: Boolean; const aConfig : IThriftConfiguration);
{$ENDIF}
begin
  inherited Create( aConfig);

{$IFDEF OLD_SOCKETS}
  FPort := aPort;
  FClientTimeout := aClientTimeout;

  FOwnsServer := True;
  FServer := TTcpServer.Create( nil );
  FServer.BlockMode := bmBlocking;
  {$IF CompilerVersion >= 21.0}
  FServer.LocalPort := AnsiString( IntToStr( FPort));
  {$ELSE}
  FServer.LocalPort := IntToStr( FPort);
  {$IFEND}
{$ELSE}
  FOwnsServer := True;
  FServer := TServerSocket.Create(aPort, aClientTimeout, aClientTimeout);
{$ENDIF}

  FUseBufferedSocket := aUseBufferedSockets;
end;

destructor TServerSocketImpl.Destroy;
begin
  if FOwnsServer then begin
    FServer.Free;
    FServer := nil;
  end;
  inherited;
end;

function TServerSocketImpl.Accept( const fnAccepting: TProc): ITransport;
var
{$IFDEF OLD_SOCKETS}
  client : TCustomIpClient;
{$ELSE}
  client: TSocket;
{$ENDIF}
  trans  : IStreamTransport;
begin
  if FServer = nil then begin
    raise TTransportExceptionNotOpen.Create('No underlying server socket.');
  end;

{$IFDEF OLD_SOCKETS}
  client := nil;
  try
    client := TCustomIpClient.Create(nil);

    if Assigned(fnAccepting)
    then fnAccepting();

    if not FServer.Accept( client) then begin
      client.Free;
      Result := nil;
      Exit;
    end;

    if client = nil then begin
      Result := nil;
      Exit;
    end;

    trans := TSocketImpl.Create( client, TRUE, FClientTimeout, Configuration);
    client := nil;  // trans owns it now

    if FUseBufferedSocket
    then result := TBufferedTransportImpl.Create( trans)
    else result := trans;

  except
    on E: Exception do begin
      client.Free;
      raise TTransportExceptionUnknown.Create(E.ToString);
    end;
  end;
{$ELSE}
  if Assigned(fnAccepting) then
    fnAccepting();

  client := FServer.Accept;
  try
    trans := TSocketImpl.Create(client, TRUE, Configuration);
    client := nil;

    if FUseBufferedSocket then
      Result := TBufferedTransportImpl.Create(trans)
    else
      Result := trans;
  except
    client.Free;
    raise;
  end;
{$ENDIF}
end;

procedure TServerSocketImpl.Listen;
begin
  if FServer <> nil then
  begin
{$IFDEF OLD_SOCKETS}
    try
      FServer.Active := True;
    except
      on E: Exception
      do raise TTransportExceptionUnknown.Create('Could not accept on listening socket: ' + E.Message);
    end;
{$ELSE}
    FServer.Listen;
{$ENDIF}
  end;
end;

procedure TServerSocketImpl.Close;
begin
  if FServer <> nil then
{$IFDEF OLD_SOCKETS}
    try
      FServer.Active := False;
    except
      on E: Exception
      do raise TTransportExceptionUnknown.Create('Error on closing socket : ' + E.Message);
    end;
{$ELSE}
    FServer.Close;
{$ENDIF}
end;

{ TSocket }

{$IFDEF OLD_SOCKETS}
constructor TSocketImpl.Create( const aClient : TCustomIpClient; const aOwnsClient : Boolean; const aTimeout: Integer; const aConfig : IThriftConfiguration);
{$ELSE}
constructor TSocketImpl.Create(const aClient: TSocket; const aOwnsClient: Boolean; const aConfig : IThriftConfiguration);
{$ENDIF}
var stream : IThriftStream;
begin
  FClient := aClient;
  FOwnsClient := aOwnsClient;

{$IFDEF OLD_SOCKETS}
  FTimeout := aTimeout;
{$ELSE}
  FTimeout := aClient.RecvTimeout;
{$ENDIF}

  stream := TTcpSocketStreamImpl.Create( FClient, FTimeout);
  inherited Create( stream, stream, aConfig);
end;


{$IFDEF OLD_SOCKETS}
constructor TSocketImpl.Create(const aHost: string; const aPort, aTimeout: Integer; const aConfig : IThriftConfiguration);
{$ELSE}
constructor TSocketImpl.Create(const aHost: string; const aPort : Integer; const aTimeout: Longword; const aConfig : IThriftConfiguration);
{$ENDIF}
begin
  inherited Create(nil,nil, aConfig);
  FHost := aHost;
  FPort := aPort;
  FTimeout := aTimeout;
  InitSocket;
end;

destructor TSocketImpl.Destroy;
begin
  if FOwnsClient
  then FreeAndNil( FClient);
  inherited;
end;

procedure TSocketImpl.Close;
begin
  inherited Close;

  FInputStream := nil;
  FOutputStream := nil;

  if FOwnsClient
  then FreeAndNil( FClient)
  else FClient := nil;
end;

function TSocketImpl.GetIsOpen: Boolean;
begin
{$IFDEF OLD_SOCKETS}
  Result := (FClient <> nil) and FClient.Connected;
{$ELSE}
  Result := (FClient <> nil) and FClient.IsOpen
{$ENDIF}
end;

procedure TSocketImpl.InitSocket;
var
  stream : IThriftStream;
begin
  if FOwnsClient
  then FreeAndNil( FClient)
  else FClient := nil;

{$IFDEF OLD_SOCKETS}
  FClient := TTcpClient.Create( nil);
{$ELSE}
  FClient := TSocket.Create(FHost, FPort);
{$ENDIF}
  FOwnsClient := True;

  stream := TTcpSocketStreamImpl.Create( FClient, FTimeout);
  FInputStream := stream;
  FOutputStream := stream;
end;

procedure TSocketImpl.Open;
begin
  if IsOpen then begin
    raise TTransportExceptionAlreadyOpen.Create('Socket already connected');
  end;

  if FHost = '' then begin
    raise TTransportExceptionNotOpen.Create('Cannot open null host');
  end;

  if Port <= 0 then begin
    raise TTransportExceptionNotOpen.Create('Cannot open without port');
  end;

  if FClient = nil
  then InitSocket;

{$IFDEF OLD_SOCKETS}
  FClient.RemoteHost := TSocketHost( Host);
  FClient.RemotePort := TSocketPort( IntToStr( Port));
  FClient.Connect;
{$ELSE}
  FClient.Open;
{$ENDIF}

  FInputStream := TTcpSocketStreamImpl.Create( FClient, FTimeout);
  FOutputStream := FInputStream;
end;

{ TBufferedStream }

procedure TBufferedStreamImpl.Close;
begin
  Flush;
  FStream := nil;

  FReadBuffer.Free;
  FReadBuffer := nil;

  FWriteBuffer.Free;
  FWriteBuffer := nil;
end;

constructor TBufferedStreamImpl.Create( const aStream: IThriftStream; const aBufSize : Integer);
begin
  inherited Create;
  FStream := aStream;
  FBufSize := aBufSize;
  FReadBuffer := TMemoryStream.Create;
  FWriteBuffer := TMemoryStream.Create;
end;

destructor TBufferedStreamImpl.Destroy;
begin
  Close;
  inherited;
end;

procedure TBufferedStreamImpl.Flush;
var
  buf : TBytes;
  len : Integer;
begin
  if IsOpen then begin
    len := FWriteBuffer.Size;
    if len > 0 then begin
      SetLength( buf, len );
      FWriteBuffer.Position := 0;
      FWriteBuffer.Read( Pointer(@buf[0])^, len );
      FStream.Write( buf, 0, len );
    end;
    FWriteBuffer.Clear;
  end;
end;

function TBufferedStreamImpl.IsOpen: Boolean;
begin
  Result := (FWriteBuffer <> nil)
        and (FReadBuffer <> nil)
        and (FStream <> nil)
        and FStream.IsOpen;
end;

procedure TBufferedStreamImpl.Open;
begin
  FStream.Open;
end;

function TBufferedStreamImpl.Read( const pBuf : Pointer; const buflen : Integer; offset: Integer; count: Integer): Integer;
var
  nRead : Integer;
  tempbuf : TBytes;
  pTmp : PByte;
begin
  inherited;
  Result := 0;

  if IsOpen then begin
    while count > 0 do begin

      if FReadBuffer.Position >= FReadBuffer.Size then begin
        FReadBuffer.Clear;
        SetLength( tempbuf, FBufSize);
        nRead := FStream.Read( tempbuf, 0, FBufSize );
        if nRead = 0 then Break; // avoid infinite loop

        FReadBuffer.WriteBuffer( Pointer(@tempbuf[0])^, nRead );
        FReadBuffer.Position := 0;
      end;

      if FReadBuffer.Position < FReadBuffer.Size then begin
        nRead := Min( FReadBuffer.Size - FReadBuffer.Position, count);
        pTmp  := pBuf;
        Inc( pTmp, offset);
        Inc( Result, FReadBuffer.Read( pTmp^, nRead));
        Dec( count, nRead);
        Inc( offset, nRead);
      end;
    end;
  end;
end;


function TBufferedStreamImpl.ToArray: TBytes;
var len : Integer;
begin
  if IsOpen
  then len := FReadBuffer.Size
  else len := 0;

  SetLength( Result, len);

  if len > 0 then begin
    FReadBuffer.Position := 0;
    FReadBuffer.Read( Pointer(@Result[0])^, len );
  end;
end;

procedure TBufferedStreamImpl.Write( const pBuf : Pointer; offset: Integer; count: Integer);
var pTmp : PByte;
begin
  inherited;
  if count > 0 then begin
    if IsOpen then begin
      pTmp := pBuf;
      Inc( pTmp, offset);
      FWriteBuffer.Write( pTmp^, count );
      if FWriteBuffer.Size > FBufSize then begin
        Flush;
      end;
    end;
  end;
end;


function TBufferedStreamImpl.Size : Int64;
begin
  result := FReadBuffer.Size;
end;


function TBufferedStreamImpl.Position : Int64;
begin
  result := FReadBuffer.Position;
end;


{ TStreamTransportImpl }

constructor TStreamTransportImpl.Create( const aInputStream, aOutputStream : IThriftStream; const aConfig : IThriftConfiguration);
begin
  inherited Create( aConfig);
  FInputStream := aInputStream;
  FOutputStream := aOutputStream;
end;

destructor TStreamTransportImpl.Destroy;
begin
  FInputStream := nil;
  FOutputStream := nil;
  inherited;
end;

procedure TStreamTransportImpl.Close;
begin
  FInputStream := nil;
  FOutputStream := nil;
end;

procedure TStreamTransportImpl.Flush;
begin
  if FOutputStream = nil then begin
    raise TTransportExceptionNotOpen.Create('Cannot flush null outputstream' );
  end;

  FOutputStream.Flush;
end;

function TStreamTransportImpl.GetInputStream: IThriftStream;
begin
  Result := FInputStream;
end;

function TStreamTransportImpl.GetIsOpen: Boolean;
begin
  Result := True;
end;

function TStreamTransportImpl.GetOutputStream: IThriftStream;
begin
  Result := FOutputStream;
end;

procedure TStreamTransportImpl.Open;
begin
  // nothing to do
end;

function TStreamTransportImpl.Read( const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer): Integer;
begin
  if FInputStream = nil
  then raise TTransportExceptionNotOpen.Create('Cannot read from null inputstream' );

  Result := FInputStream.Read( pBuf,buflen, off, len );
  CountConsumedMessageBytes( result);
end;

procedure TStreamTransportImpl.Write( const pBuf : Pointer; off, len : Integer);
begin
  if FOutputStream = nil
  then raise TTransportExceptionNotOpen.Create('Cannot write to null outputstream' );

  FOutputStream.Write( pBuf, off, len );
end;

{ TBufferedTransportImpl }

constructor TBufferedTransportImpl.Create( const aTransport : IStreamTransport; const aBufSize: Integer);
begin
  ASSERT( aTransport <> nil);
  inherited Create( aTransport);
  FBufSize := aBufSize;
  InitBuffers;
end;

procedure TBufferedTransportImpl.Close;
begin
  InnerTransport.Close;
  FInputBuffer := nil;
  FOutputBuffer := nil;
end;

procedure TBufferedTransportImpl.Flush;
begin
  if FOutputBuffer <> nil then begin
    FOutputBuffer.Flush;
  end;
end;

function TBufferedTransportImpl.GetIsOpen: Boolean;
begin
  Result := InnerTransport.IsOpen;
end;

procedure TBufferedTransportImpl.InitBuffers;
begin
  if InnerTransport.InputStream <> nil then begin
    FInputBuffer := TBufferedStreamImpl.Create( InnerTransport.InputStream, FBufSize );
  end;
  if InnerTransport.OutputStream <> nil then begin
    FOutputBuffer := TBufferedStreamImpl.Create( InnerTransport.OutputStream, FBufSize );
  end;
end;

procedure TBufferedTransportImpl.Open;
begin
  InnerTransport.Open;
  InitBuffers;  // we need to get the buffers to match FTransport substreams again
end;

function TBufferedTransportImpl.Read( const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer): Integer;
begin
  if FInputBuffer <> nil
  then Result := FInputBuffer.Read( pBuf,buflen, off, len )
  else Result := 0;
end;

procedure TBufferedTransportImpl.Write( const pBuf : Pointer; off, len : Integer);
begin
  if FOutputBuffer <> nil then begin
    FOutputBuffer.Write( pBuf, off, len );
  end;
end;

procedure TBufferedTransportImpl.CheckReadBytesAvailable( const value : Int64);
var buffered, need : Int64;
begin
  need := value;

  // buffered bytes
  buffered := FInputBuffer.Size - FInputBuffer.Position;
  if buffered < need
  then InnerTransport.CheckReadBytesAvailable( need - buffered);
end;


{ TBufferedTransportImpl.TFactory }

function TBufferedTransportImpl.TFactory.GetTransport( const aTransport: ITransport): ITransport;
begin
  Result := TFramedTransportImpl.Create( aTransport);
end;


{ TFramedTransportImpl }

constructor TFramedTransportImpl.Create( const aTransport: ITransport);
begin
  ASSERT( aTransport <> nil);
  inherited Create( aTransport);

  InitWriteBuffer;
end;

destructor TFramedTransportImpl.Destroy;
begin
  FWriteBuffer.Free;
  FWriteBuffer := nil;
  FReadBuffer.Free;
  FReadBuffer := nil;
  inherited;
end;

procedure TFramedTransportImpl.Close;
begin
  InnerTransport.Close;
end;

procedure TFramedTransportImpl.Flush;
var
  buf : TBytes;
  len : Integer;
  data_len : Int64;
begin
  if not IsOpen
  then raise TTransportExceptionNotOpen.Create('not open');

  len := FWriteBuffer.Size;
  SetLength( buf, len);
  if len > 0 then begin
    System.Move( FWriteBuffer.Memory^, buf[0], len );
  end;

  data_len := len - SizeOf(TFramedHeader);
  if (0 > data_len) or (data_len > Configuration.MaxFrameSize)
  then raise TTransportExceptionUnknown.Create('TFramedTransport.Flush: invalid frame size ('+IntToStr(data_len)+')')
  else UpdateKnownMessageSize( len);

  InitWriteBuffer;

  buf[0] := Byte($FF and (data_len shr 24));
  buf[1] := Byte($FF and (data_len shr 16));
  buf[2] := Byte($FF and (data_len shr 8));
  buf[3] := Byte($FF and data_len);

  InnerTransport.Write( buf, 0, len );
  InnerTransport.Flush;
end;

function TFramedTransportImpl.GetIsOpen: Boolean;
begin
  Result := InnerTransport.IsOpen;
end;

type
  TAccessMemoryStream = class(TMemoryStream)
  end;

procedure TFramedTransportImpl.InitWriteBuffer;
const DUMMY_HEADER : TFramedHeader = 0;
begin
  FreeAndNil( FWriteBuffer);
  FWriteBuffer := TMemoryStream.Create;
  TAccessMemoryStream(FWriteBuffer).Capacity := 1024;
  FWriteBuffer.Write( DUMMY_HEADER, SizeOf(DUMMY_HEADER));
end;

procedure TFramedTransportImpl.Open;
begin
  InnerTransport.Open;
end;

function TFramedTransportImpl.Read( const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer): Integer;
var pTmp : PByte;
begin
  if len > (buflen-off)
  then len := buflen-off;

  pTmp := pBuf;
  Inc( pTmp, off);

  if (FReadBuffer <> nil) and (len > 0) then begin
    result := FReadBuffer.Read( pTmp^, len);
    if result > 0 then Exit;
  end;

  ReadFrame;
  if len > 0
  then Result := FReadBuffer.Read( pTmp^, len)
  else Result := 0;
end;

procedure TFramedTransportImpl.ReadFrame;
var
  i32rd : packed array[0..SizeOf(TFramedHeader)-1] of Byte;
  size : Integer;
  buff : TBytes;
begin
  InnerTransport.ReadAll( @i32rd[0], SizeOf(i32rd), 0, SizeOf(i32rd));
  size :=
    ((i32rd[0] and $FF) shl 24) or
    ((i32rd[1] and $FF) shl 16) or
    ((i32rd[2] and $FF) shl 8) or
     (i32rd[3] and $FF);

  if size < 0 then begin
    Close();
    raise TTransportExceptionCorruptedData.Create('Read a negative frame size ('+IntToStr(size)+')');
  end;

  if Int64(size) > Int64(Configuration.MaxFrameSize) then begin
    Close();
    raise TTransportExceptionCorruptedData.Create('Frame size ('+IntToStr(size)+') larger than allowed maximum ('+IntToStr(Configuration.MaxFrameSize)+')');
  end;

  UpdateKnownMessageSize(size + SizeOf(size));

  SetLength( buff, size );
  InnerTransport.ReadAll( buff, 0, size );

  FreeAndNil( FReadBuffer);
  FReadBuffer := TMemoryStream.Create;
  if Length(buff) > 0
  then FReadBuffer.Write( Pointer(@buff[0])^, size );
  FReadBuffer.Position := 0;
end;

procedure TFramedTransportImpl.Write( const pBuf : Pointer; off, len : Integer);
var pTmp : PByte;
begin
  if len > 0 then begin
    pTmp := pBuf;
    Inc( pTmp, off);

    FWriteBuffer.Write( pTmp^, len );
  end;
end;


procedure TFramedTransportImpl.CheckReadBytesAvailable( const value : Int64);
var buffered, need : Int64;
begin
  need := value;

  // buffered bytes
  buffered := FReadBuffer.Size - FReadBuffer.Position;
  if buffered < need
  then InnerTransport.CheckReadBytesAvailable( need - buffered);
end;


{ TFramedTransport.TFactory }

function TFramedTransportImpl.TFactory.GetTransport( const aTransport: ITransport): ITransport;
begin
  Result := TFramedTransportImpl.Create( aTransport);
end;

{ TTcpSocketStreamImpl }

procedure TTcpSocketStreamImpl.Close;
begin
  FTcpClient.Close;
end;

{$IFDEF OLD_SOCKETS}
constructor TTcpSocketStreamImpl.Create( const aTcpClient: TCustomIpClient; const aTimeout : Integer);
begin
  inherited Create;
  FTcpClient := aTcpClient;
  FTimeout := aTimeout;
end;
{$ELSE}
constructor TTcpSocketStreamImpl.Create( const aTcpClient: TSocket; const aTimeout : Longword);
begin
  inherited Create;
  FTcpClient := aTcpClient;
  if aTimeout = 0 then
    FTcpClient.RecvTimeout := SLEEP_TIME
  else
    FTcpClient.RecvTimeout := aTimeout;
  FTcpClient.SendTimeout := aTimeout;
end;
{$ENDIF}

procedure TTcpSocketStreamImpl.Flush;
begin
  // nothing to do
end;


function TTcpSocketStreamImpl.IsOpen: Boolean;
begin
{$IFDEF OLD_SOCKETS}
  Result := FTcpClient.Active;
{$ELSE}
  Result := FTcpClient.IsOpen;
{$ENDIF}
end;

procedure TTcpSocketStreamImpl.Open;
begin
  FTcpClient.Open;
end;


{$IFDEF OLD_SOCKETS}
function TTcpSocketStreamImpl.Select( ReadReady, WriteReady, ExceptFlag: PBoolean;
                                      TimeOut: Integer; var wsaError : Integer): Integer;
var
  ReadFds: TFDset;
  ReadFdsptr: PFDset;
  WriteFds: TFDset;
  WriteFdsptr: PFDset;
  ExceptFds: TFDset;
  ExceptFdsptr: PFDset;
  tv: timeval;
  Timeptr: PTimeval;
  socket : TSocket;
begin
  if not FTcpClient.Active then begin
    wsaError := WSAEINVAL;
    Exit( SOCKET_ERROR);
  end;

  socket := FTcpClient.Handle;

  if Assigned(ReadReady) then begin
    ReadFdsptr := @ReadFds;
    FD_ZERO(ReadFds);
    FD_SET(socket, ReadFds);
  end
  else begin
    ReadFdsptr := nil;
  end;

  if Assigned(WriteReady) then begin
    WriteFdsptr := @WriteFds;
    FD_ZERO(WriteFds);
    FD_SET(socket, WriteFds);
  end
  else begin
    WriteFdsptr := nil;
  end;

  if Assigned(ExceptFlag) then begin
    ExceptFdsptr := @ExceptFds;
    FD_ZERO(ExceptFds);
    FD_SET(socket, ExceptFds);
  end
  else begin
    ExceptFdsptr := nil;
  end;

  if TimeOut >= 0 then begin
    tv.tv_sec := TimeOut div 1000;
    tv.tv_usec :=  1000 * (TimeOut mod 1000);
    Timeptr := @tv;
  end
  else begin
    Timeptr := nil;  // wait forever
  end;

  wsaError := 0;
  try
    {$IFDEF MSWINDOWS}
      {$IFDEF OLD_UNIT_NAMES}
      result := WinSock.select(        socket + 1, ReadFdsptr, WriteFdsptr, ExceptFdsptr, Timeptr);
      {$ELSE}
      result := Winapi.WinSock.select( socket + 1, ReadFdsptr, WriteFdsptr, ExceptFdsptr, Timeptr);
      {$ENDIF}
    {$ENDIF}
    {$IFDEF LINUX}
      result := Libc.select(           socket + 1, ReadFdsptr, WriteFdsptr, ExceptFdsptr, Timeptr);
    {$ENDIF}

    if result = SOCKET_ERROR
    then wsaError := WSAGetLastError;

  except
    result := SOCKET_ERROR;
  end;

  if Assigned(ReadReady) then
   ReadReady^ := FD_ISSET(socket, ReadFds);
   
  if Assigned(WriteReady) then
    WriteReady^ := FD_ISSET(socket, WriteFds);
  
  if Assigned(ExceptFlag) then
    ExceptFlag^ := FD_ISSET(socket, ExceptFds);
end;
{$ENDIF}

{$IFDEF OLD_SOCKETS}
function TTcpSocketStreamImpl.WaitForData( TimeOut : Integer; pBuf : Pointer;
                                           DesiredBytes : Integer;
                                           var wsaError, bytesReady : Integer): TWaitForData;
var bCanRead, bError : Boolean;
    retval : Integer;
const 
  MSG_PEEK = {$IFDEF OLD_UNIT_NAMES} WinSock.MSG_PEEK  {$ELSE} Winapi.WinSock.MSG_PEEK  {$ENDIF};
begin
  bytesReady := 0;

  // The select function returns the total number of socket handles that are ready
  // and contained in the fd_set structures, zero if the time limit expired,
  // or SOCKET_ERROR if an error occurred. If the return value is SOCKET_ERROR,
  // WSAGetLastError can be used to retrieve a specific error code.
  retval := Self.Select( @bCanRead, nil, @bError, TimeOut, wsaError);
  if retval = SOCKET_ERROR
  then Exit( TWaitForData.wfd_Error);
  if (retval = 0) or not bCanRead
  then Exit( TWaitForData.wfd_Timeout);

  // recv() returns the number of bytes received, or -1 if an error occurred.
  // The return value will be 0 when the peer has performed an orderly shutdown.
  
  retval := recv( FTcpClient.Handle, pBuf^, DesiredBytes, MSG_PEEK);
  if retval <= 0
  then Exit( TWaitForData.wfd_Error);

  // at least we have some data
  bytesReady := Min( retval, DesiredBytes);
  result := TWaitForData.wfd_HaveData;
end;
{$ENDIF}

{$IFDEF OLD_SOCKETS}
function TTcpSocketStreamImpl.Read( const pBuf : Pointer; const buflen : Integer; offset: Integer; count: Integer): Integer;
// old sockets version
var wfd : TWaitForData;
    wsaError,
    msecs : Integer;
    nBytes : Integer;
    pTmp : PByte;
begin
  inherited;

  if FTimeout > 0
  then msecs := FTimeout
  else msecs := DEFAULT_THRIFT_TIMEOUT;

  result := 0;
  pTmp   := pBuf;
  Inc( pTmp, offset);
  while (count > 0) and (result = 0) do begin

    while TRUE do begin
      wfd := WaitForData( msecs, pTmp, count, wsaError, nBytes);
      case wfd of
        TWaitForData.wfd_Error    :  Exit;
        TWaitForData.wfd_HaveData :  Break;
        TWaitForData.wfd_Timeout  :  begin
          if (FTimeout = 0)
          then Exit
          else raise TTransportExceptionTimedOut.Create(SysErrorMessage(Cardinal(wsaError)));
        end;
      else
        ASSERT( FALSE);
      end;
    end;

    // reduce the timeout once we got data
    if FTimeout > 0
    then msecs := FTimeout div 10
    else msecs := DEFAULT_THRIFT_TIMEOUT div 10;
    msecs := Max( msecs, 200);

    ASSERT( nBytes <= count);
    nBytes := FTcpClient.ReceiveBuf( pTmp^, nBytes);
    Inc( pTmp, nBytes);
    Dec( count, nBytes);
    Inc( result, nBytes);
  end;
end;

function TTcpSocketStreamImpl.ToArray: TBytes;
// old sockets version
var len : Integer;
begin
  len := 0;
  if IsOpen then begin
    len := FTcpClient.BytesReceived;
  end;

  SetLength( Result, len );

  if len > 0 then begin
    FTcpClient.ReceiveBuf( Pointer(@Result[0])^, len);
  end;
end;

procedure TTcpSocketStreamImpl.Write( const pBuf : Pointer; offset, count: Integer);
// old sockets version
var bCanWrite, bError : Boolean;
    retval, wsaError : Integer;
    pTmp : PByte;
begin
  inherited;

  if not FTcpClient.Active
  then raise TTransportExceptionNotOpen.Create('not open');

  // The select function returns the total number of socket handles that are ready
  // and contained in the fd_set structures, zero if the time limit expired,
  // or SOCKET_ERROR if an error occurred. If the return value is SOCKET_ERROR,
  // WSAGetLastError can be used to retrieve a specific error code.
  retval := Self.Select( nil, @bCanWrite, @bError, FTimeOut, wsaError);
  if retval = SOCKET_ERROR
  then raise TTransportExceptionUnknown.Create(SysErrorMessage(Cardinal(wsaError)));

  if (retval = 0)
  then raise TTransportExceptionTimedOut.Create('timed out');

  if bError or not bCanWrite
  then raise TTransportExceptionUnknown.Create('unknown error');

  pTmp := pBuf;
  Inc( pTmp, offset);
  FTcpClient.SendBuf( pTmp^, count);
end;

{$ELSE}

function TTcpSocketStreamImpl.Read( const pBuf : Pointer; const buflen : Integer; offset: Integer; count: Integer): Integer;
// new sockets version
var nBytes : Integer;
    pTmp : PByte;
begin
  inherited;

  result := 0;
  pTmp   := pBuf;
  Inc( pTmp, offset);
  while count > 0 do begin
    nBytes := FTcpClient.Read( pTmp^, count);
    if nBytes = 0 then Exit;
    Inc( pTmp, nBytes);
    Dec( count, nBytes);
    Inc( result, nBytes);
  end;
end;

function TTcpSocketStreamImpl.ToArray: TBytes;
// new sockets version
var len : Integer;
begin
  len := 0;
  try
    if FTcpClient.Peek then
      repeat
        SetLength(Result, Length(Result) + 1024);
        len := FTcpClient.Read(Result[Length(Result) - 1024], 1024);
      until len < 1024;
  except
    on TTransportException do begin { don't allow default exceptions } end;
    else raise;
  end;
  if len > 0 then
    SetLength(Result, Length(Result) - 1024 + len);
end;

procedure TTcpSocketStreamImpl.Write( const pBuf : Pointer; offset, count: Integer);
// new sockets version
var pTmp : PByte;
begin
  inherited;

  if not FTcpClient.IsOpen
  then raise TTransportExceptionNotOpen.Create('not open');

  pTmp := pBuf;
  Inc( pTmp, offset);
  FTcpClient.Write( pTmp^, count);
end;

{$ENDIF}


end.
