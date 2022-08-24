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
unit Thrift.Transport.MsxmlHTTP;

{$I Thrift.Defines.inc}
{$SCOPEDENUMS ON}

interface

uses
  Classes,
  SysUtils,
  Math,
  Generics.Collections,
  {$IFDEF OLD_UNIT_NAMES}
    ActiveX, msxml,
  {$ELSE}
    Winapi.ActiveX, Winapi.msxml,
  {$ENDIF}
  Thrift.Collections,
  Thrift.Configuration,
  Thrift.Transport,
  Thrift.Exception,
  Thrift.Utils,
  Thrift.Stream;

type
  TMsxmlHTTPClientImpl = class( TEndpointTransportBase, IHTTPClient)
  strict private
    FUri : string;
    FInputStream : IThriftStream;
    FOutputStream : IThriftStream;
    FDnsResolveTimeout : Integer;
    FConnectionTimeout : Integer;
    FSendTimeout : Integer;
    FReadTimeout : Integer;
    FCustomHeaders : IThriftDictionary<string,string>;

    function CreateRequest: IXMLHTTPRequest;
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
    constructor Create( const aUri: string; const aConfig : IThriftConfiguration);  reintroduce;
    destructor Destroy; override;
  end;


implementation

const
  XMLHTTP_CONNECTION_TIMEOUT = 60 * 1000;
  XMLHTTP_SENDRECV_TIMEOUT   = 30 * 1000;

{ TMsxmlHTTPClientImpl }

constructor TMsxmlHTTPClientImpl.Create( const aUri: string; const aConfig : IThriftConfiguration);
begin
  inherited Create( aConfig);
  FUri := aUri;

  // defaults according to MSDN
  FDnsResolveTimeout := 0; // no timeout
  FConnectionTimeout := XMLHTTP_CONNECTION_TIMEOUT;
  FSendTimeout       := XMLHTTP_SENDRECV_TIMEOUT;
  FReadTimeout       := XMLHTTP_SENDRECV_TIMEOUT;

  FCustomHeaders := TThriftDictionaryImpl<string,string>.Create;
  FOutputStream := TThriftStreamAdapterDelphi.Create( TThriftMemoryStream.Create, True);
end;

function TMsxmlHTTPClientImpl.CreateRequest: IXMLHTTPRequest;
var
  pair : TPair<string,string>;
  srvHttp : IServerXMLHTTPRequest;
begin
  {$IF CompilerVersion >= 21.0}
  Result := CoServerXMLHTTP.Create;
  {$ELSE}
  Result := CoXMLHTTPRequest.Create;
  {$IFEND}

  // setting a timeout value to 0 (zero) means "no timeout" for that setting
  if Supports( result, IServerXMLHTTPRequest, srvHttp)
  then srvHttp.setTimeouts( DnsResolveTimeout, ConnectionTimeout, SendTimeout, ReadTimeout);

  Result.open('POST', FUri, False, '', '');
  Result.setRequestHeader( 'Content-Type', THRIFT_MIMETYPE);
  Result.setRequestHeader( 'Accept', THRIFT_MIMETYPE);
  Result.setRequestHeader( 'User-Agent', 'ApacheThriftDelphi/msxml');

  for pair in FCustomHeaders do begin
    Result.setRequestHeader( pair.Key, pair.Value );
  end;
end;

destructor TMsxmlHTTPClientImpl.Destroy;
begin
  Close;
  inherited;
end;

function TMsxmlHTTPClientImpl.GetDnsResolveTimeout: Integer;
begin
  Result := FDnsResolveTimeout;
end;

procedure TMsxmlHTTPClientImpl.SetDnsResolveTimeout(const Value: Integer);
begin
  FDnsResolveTimeout := Value;
end;

function TMsxmlHTTPClientImpl.GetConnectionTimeout: Integer;
begin
  Result := FConnectionTimeout;
end;

procedure TMsxmlHTTPClientImpl.SetConnectionTimeout(const Value: Integer);
begin
  FConnectionTimeout := Value;
end;

function TMsxmlHTTPClientImpl.GetSendTimeout: Integer;
begin
  Result := FSendTimeout;
end;

procedure TMsxmlHTTPClientImpl.SetSendTimeout(const Value: Integer);
begin
  FSendTimeout := Value;
end;

function TMsxmlHTTPClientImpl.GetReadTimeout: Integer;
begin
  Result := FReadTimeout;
end;

procedure TMsxmlHTTPClientImpl.SetReadTimeout(const Value: Integer);
begin
  FReadTimeout := Value;
end;

function TMsxmlHTTPClientImpl.GetSecureProtocols : TSecureProtocols;
begin
  Result := [];
end;

procedure TMsxmlHTTPClientImpl.SetSecureProtocols( const value : TSecureProtocols);
begin
  raise TTransportExceptionBadArgs.Create('SetSecureProtocols: Not supported with '+ClassName);
end;

function TMsxmlHTTPClientImpl.GetCustomHeaders: IThriftDictionary<string,string>;
begin
  Result := FCustomHeaders;
end;

function TMsxmlHTTPClientImpl.GetIsOpen: Boolean;
begin
  Result := Assigned(FOutputStream);
end;

procedure TMsxmlHTTPClientImpl.Open;
begin
  FOutputStream := TThriftStreamAdapterDelphi.Create( TThriftMemoryStream.Create, True);
end;

procedure TMsxmlHTTPClientImpl.Close;
begin
  FInputStream := nil;
  FOutputStream := nil;
end;

procedure TMsxmlHTTPClientImpl.Flush;
begin
  try
    SendRequest;
  finally
    FOutputStream := nil;
    FOutputStream := TThriftStreamAdapterDelphi.Create( TThriftMemoryStream.Create, True);
    ASSERT( FOutputStream <> nil);
  end;
end;

function TMsxmlHTTPClientImpl.Read( const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer): Integer;
begin
  if FInputStream = nil then begin
    raise TTransportExceptionNotOpen.Create('No request has been sent');
  end;

  try
    Result := FInputStream.Read( pBuf, buflen, off, len);
  except
    on E: Exception
    do raise TTransportExceptionUnknown.Create(E.Message);
  end;
end;

procedure TMsxmlHTTPClientImpl.SendRequest;
var
  xmlhttp : IXMLHTTPRequest;
  ms : TThriftMemoryStream;
  a : TBytes;
  len : Integer;
begin
  xmlhttp := CreateRequest;

  ms := TThriftMemoryStream.Create;
  try
    a := FOutputStream.ToArray;
    len := Length(a);
    if len > 0 then begin
      ms.WriteBuffer( Pointer(@a[0])^, len);
    end;
    ms.Position := 0;
    xmlhttp.send( IUnknown( TStreamAdapter.Create( ms, soReference )));
    FInputStream := nil;
    FInputStream := TThriftStreamAdapterCOM.Create( IUnknown( xmlhttp.responseStream) as IStream);
    ResetConsumedMessageSize;
    UpdateKnownMessageSize( FInputStream.Size);
  finally
    ms.Free;
  end;
end;

procedure TMsxmlHTTPClientImpl.Write( const pBuf : Pointer; off, len : Integer);
begin
  if FOutputStream <> nil
  then FOutputStream.Write( pBuf, off, len)
  else raise TTransportExceptionNotOpen.Create('Transport closed');
end;



end.

