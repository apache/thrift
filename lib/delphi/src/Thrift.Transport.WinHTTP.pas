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
unit Thrift.Transport.WinHTTP;

{$I Thrift.Defines.inc}
{$SCOPEDENUMS ON}

interface

uses
  Classes,
  SysUtils,
  Math,
  Generics.Collections,
  Thrift.Collections,
  Thrift.Configuration,
  Thrift.Transport,
  Thrift.Exception,
  Thrift.Utils,
  Thrift.WinHTTP,
  Thrift.Stream;

type
  TWinHTTPClientImpl = class( TEndpointTransportBase, IHTTPClient)
  strict private
    FUri : string;
    FInputStream : IThriftStream;
    FOutputMemoryStream : TMemoryStream;
    FDnsResolveTimeout : Integer;
    FConnectionTimeout : Integer;
    FSendTimeout : Integer;
    FReadTimeout : Integer;
    FCustomHeaders : IThriftDictionary<string,string>;
    FSecureProtocols : TSecureProtocols;

    function CreateRequest: IWinHTTPRequest;
    function SecureProtocolsAsWinHTTPFlags : Cardinal;

  strict private
    type
      TErrorInfo = ( SplitUrl, WinHTTPSession, WinHTTPConnection, WinHTTPRequest, RequestSetup, AutoProxy );

      THTTPResponseStream = class( TThriftStreamImpl)
      strict private
        FRequest : IWinHTTPRequest;
      strict protected
        procedure Write( const pBuf : Pointer; offset: Integer; count: Integer); override;
        function Read( const pBuf : Pointer; const buflen : Integer; offset: Integer; count: Integer): Integer; override;
        procedure Open; override;
        procedure Close; override;
        procedure Flush; override;
        function IsOpen: Boolean; override;
        function ToArray: TBytes; override;
      public
        constructor Create( const aRequest : IWinHTTPRequest);
        destructor Destroy; override;
      end;

  strict protected
    function GetIsOpen: Boolean; override;
    procedure Open(); override;
    procedure Close(); override;
    function  Read( const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer): Integer; override;
    procedure Write( const pBuf : Pointer; off, len : Integer); override;
    procedure Flush; override;

    procedure SetDnsResolveTimeout(const Value: Integer);
    function GetDnsResolveTimeout: Integer;
    procedure SetConnectionTimeout(const Value: Integer);
    function GetConnectionTimeout: Integer;
    procedure SetSendTimeout(const Value: Integer);
    function GetSendTimeout: Integer;
    procedure SetReadTimeout(const Value: Integer);
    function GetReadTimeout: Integer;
    function GetSecureProtocols : TSecureProtocols;
    procedure SetSecureProtocols( const value : TSecureProtocols);

    function GetCustomHeaders: IThriftDictionary<string,string>;
    procedure SendRequest;

    property DnsResolveTimeout: Integer read GetDnsResolveTimeout write SetDnsResolveTimeout;
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;
    property SendTimeout: Integer read GetSendTimeout write SetSendTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property CustomHeaders: IThriftDictionary<string,string> read GetCustomHeaders;
  public
    constructor Create( const aUri: string; const aConfig : IThriftConfiguration = nil);
    destructor Destroy; override;
  end;

implementation

const
  WINHTTP_CONNECTION_TIMEOUT = 60 * 1000;
  WINHTTP_SENDRECV_TIMEOUT   = 30 * 1000;


{ TWinHTTPClientImpl }

constructor TWinHTTPClientImpl.Create( const aUri: string; const aConfig : IThriftConfiguration);
begin
  inherited Create( aConfig);
  FUri := AUri;

  // defaults according to MSDN
  FDnsResolveTimeout := 0; // no timeout
  FConnectionTimeout := WINHTTP_CONNECTION_TIMEOUT;
  FSendTimeout       := WINHTTP_SENDRECV_TIMEOUT;
  FReadTimeout       := WINHTTP_SENDRECV_TIMEOUT;

  FSecureProtocols := DEFAULT_THRIFT_SECUREPROTOCOLS;

  FCustomHeaders := TThriftDictionaryImpl<string,string>.Create;
  FOutputMemoryStream := TMemoryStream.Create;
end;

destructor TWinHTTPClientImpl.Destroy;
begin
  Close;
  FreeAndNil( FOutputMemoryStream);
  inherited;
end;

function TWinHTTPClientImpl.CreateRequest: IWinHTTPRequest;
var
  pair    : TPair<string,string>;
  session : IWinHTTPSession;
  connect : IWinHTTPConnection;
  url     : IWinHTTPUrl;
  sPath   : string;
  info    : TErrorInfo;
begin
  info := TErrorInfo.SplitUrl;
  try
    url := TWinHTTPUrlImpl.Create( FUri);

    info := TErrorInfo.WinHTTPSession;
    session := TWinHTTPSessionImpl.Create('Apache Thrift Delphi WinHTTP');
    session.EnableSecureProtocols( SecureProtocolsAsWinHTTPFlags);

    info := TErrorInfo.WinHTTPConnection;
    connect := session.Connect( url.HostName, url.Port);

    info := TErrorInfo.WinHTTPRequest;
    sPath   := url.UrlPath + url.ExtraInfo;
    result  := connect.OpenRequest( (url.Scheme = 'https'), 'POST', sPath, THRIFT_MIMETYPE);

    // setting a timeout value to 0 (zero) means "no timeout" for that setting
    info := TErrorInfo.RequestSetup;
    result.SetTimeouts( DnsResolveTimeout, ConnectionTimeout, SendTimeout, ReadTimeout);

    // headers
    result.AddRequestHeader( 'Content-Type: '+THRIFT_MIMETYPE, WINHTTP_ADDREQ_FLAG_ADD);
    for pair in FCustomHeaders do begin
      Result.AddRequestHeader( pair.Key +': '+ pair.Value, WINHTTP_ADDREQ_FLAG_ADD);
    end;

    // enable automatic gzip,deflate decompression
    result.EnableAutomaticContentDecompression(TRUE);

    // AutoProxy support
    info := TErrorInfo.AutoProxy;
    result.TryAutoProxy( FUri);
  except
    on e:TException do raise;
    on e:Exception do raise TTransportExceptionUnknown.Create( e.Message+' (at '+EnumUtils<TErrorInfo>.ToString(Ord(info))+')');
  end;
end;


function TWinHTTPClientImpl.SecureProtocolsAsWinHTTPFlags : Cardinal;
const
  PROTOCOL_MAPPING : array[TSecureProtocol] of Cardinal = (
    WINHTTP_FLAG_SECURE_PROTOCOL_SSL2,
    WINHTTP_FLAG_SECURE_PROTOCOL_SSL3,
    WINHTTP_FLAG_SECURE_PROTOCOL_TLS1,
    WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1,
    WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2
  );
var
  prot : TSecureProtocol;
  protos : TSecureProtocols;
begin
  result := 0;
  protos := GetSecureProtocols;
  for prot := Low(TSecureProtocol) to High(TSecureProtocol) do begin
    if prot in protos
    then result := result or PROTOCOL_MAPPING[prot];
  end;
end;


function TWinHTTPClientImpl.GetDnsResolveTimeout: Integer;
begin
  Result := FDnsResolveTimeout;
end;

procedure TWinHTTPClientImpl.SetDnsResolveTimeout(const Value: Integer);
begin
  FDnsResolveTimeout := Value;
end;

function TWinHTTPClientImpl.GetConnectionTimeout: Integer;
begin
  Result := FConnectionTimeout;
end;

procedure TWinHTTPClientImpl.SetConnectionTimeout(const Value: Integer);
begin
  FConnectionTimeout := Value;
end;

function TWinHTTPClientImpl.GetSendTimeout: Integer;
begin
  Result := FSendTimeout;
end;

procedure TWinHTTPClientImpl.SetSendTimeout(const Value: Integer);
begin
  FSendTimeout := Value;
end;

function TWinHTTPClientImpl.GetReadTimeout: Integer;
begin
  Result := FReadTimeout;
end;

procedure TWinHTTPClientImpl.SetReadTimeout(const Value: Integer);
begin
  FReadTimeout := Value;
end;

function TWinHTTPClientImpl.GetSecureProtocols : TSecureProtocols;
begin
  Result := FSecureProtocols;
end;

procedure TWinHTTPClientImpl.SetSecureProtocols( const value : TSecureProtocols);
begin
  FSecureProtocols := Value;
end;

function TWinHTTPClientImpl.GetCustomHeaders: IThriftDictionary<string,string>;
begin
  Result := FCustomHeaders;
end;

function TWinHTTPClientImpl.GetIsOpen: Boolean;
begin
  Result := Assigned( FOutputMemoryStream);
end;

procedure TWinHTTPClientImpl.Open;
begin
  FreeAndNil( FOutputMemoryStream);
  FOutputMemoryStream := TMemoryStream.Create;
end;

procedure TWinHTTPClientImpl.Close;
begin
  FInputStream := nil;
  FreeAndNil( FOutputMemoryStream);
end;

procedure TWinHTTPClientImpl.Flush;
begin
  try
    SendRequest;
  finally
    FreeAndNil( FOutputMemoryStream);
    FOutputMemoryStream := TMemoryStream.Create;
    ASSERT( FOutputMemoryStream <> nil);
  end;
end;

function TWinHTTPClientImpl.Read( const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer): Integer;
begin
  if FInputStream = nil then begin
    raise TTransportExceptionNotOpen.Create('No request has been sent');
  end;

  try
    Result := FInputStream.Read( pBuf, buflen, off, len);
    CountConsumedMessageBytes( result);
  except
    on E: Exception
    do raise TTransportExceptionUnknown.Create(E.Message);
  end;
end;

procedure TWinHTTPClientImpl.SendRequest;
var
  http  : IWinHTTPRequest;
  pData : PByte;
  len   : Integer;
  error, dwSize : Cardinal;
  sMsg  : string;
begin
  http := CreateRequest;

  pData := FOutputMemoryStream.Memory;
  len   := FOutputMemoryStream.Size;

  // send all data immediately, since we have it in memory
  if not http.SendRequest( pData, len, 0) then begin
    error := Cardinal( GetLastError);
    sMsg  := 'WinHTTP send error '+IntToStr(Int64(error))+' '+WinHttpSysErrorMessage(error);
    raise TTransportExceptionUnknown.Create(sMsg);
  end;

  // end request and start receiving
  if not http.FlushAndReceiveResponse then begin
    error := Cardinal( GetLastError);
    sMsg  := 'WinHTTP recv error '+IntToStr(Int64(error))+' '+WinHttpSysErrorMessage(error);
    if error = ERROR_WINHTTP_TIMEOUT
    then raise TTransportExceptionTimedOut.Create( sMsg)
    else raise TTransportExceptionInterrupted.Create( sMsg);
  end;

  // we're about to receive a new message, so reset everyting
  ResetConsumedMessageSize(-1);
  FInputStream := THTTPResponseStream.Create( http);
  if http.QueryTotalResponseSize( dwSize)  // FALSE indicates "no info available"
  then UpdateKnownMessageSize( dwSize);
end;

procedure TWinHTTPClientImpl.Write( const pBuf : Pointer; off, len : Integer);
var pTmp : PByte;
begin
  pTmp := pBuf;
  Inc(pTmp,off);
  FOutputMemoryStream.Write( pTmp^, len);
end;


{ TWinHTTPClientImpl.THTTPResponseStream }

constructor TWinHTTPClientImpl.THTTPResponseStream.Create( const aRequest : IWinHTTPRequest);
begin
  inherited Create;
  FRequest := aRequest;
end;

destructor TWinHTTPClientImpl.THTTPResponseStream.Destroy;
begin
  try
    Close;
  finally
    inherited Destroy;
  end;
end;

procedure TWinHTTPClientImpl.THTTPResponseStream.Close;
begin
  FRequest := nil;
end;

procedure TWinHTTPClientImpl.THTTPResponseStream.Flush;
begin
  raise ENotImplemented(ClassName+'.Flush');
end;

function TWinHTTPClientImpl.THTTPResponseStream.IsOpen: Boolean;
begin
  Result := FRequest <> nil;
end;

procedure TWinHTTPClientImpl.THTTPResponseStream.Open;
begin
  // nothing to do
end;

procedure TWinHTTPClientImpl.THTTPResponseStream.Write(const pBuf : Pointer; offset, count: Integer);
begin
  inherited;  // check pointers
  raise ENotImplemented(ClassName+'.Write');
end;

function TWinHTTPClientImpl.THTTPResponseStream.Read(const pBuf : Pointer; const buflen : Integer; offset, count: Integer): Integer;
var pTmp : PByte;
begin
  inherited;  // check pointers

  if count >= buflen-offset
  then count := buflen-offset;

  if count > 0 then begin
    pTmp   := pBuf;
    Inc( pTmp, offset);
    Result := FRequest.ReadData( pTmp, count);
    ASSERT( Result >= 0);
  end
  else Result := 0;
end;

function TWinHTTPClientImpl.THTTPResponseStream.ToArray: TBytes;
begin
  raise ENotImplemented(ClassName+'.ToArray');
end;


end.
