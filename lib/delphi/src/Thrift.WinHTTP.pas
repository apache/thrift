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
unit Thrift.WinHTTP;

{$I Thrift.Defines.inc}
{$SCOPEDENUMS ON}

// packing according to winhttp.h
{$IFDEF Win64} {$ALIGN 8} {$ELSE} {$ALIGN 4} {$ENDIF}

interface

uses
  Windows,
  Classes,
  SysUtils,
  Math,
  Generics.Collections;


type
  HINTERNET = type Pointer;
  INTERNET_PORT = type WORD;
  INTERNET_SCHEME = type Integer;
  LPLPCWSTR = ^LPCWSTR;

  LPURL_COMPONENTS = ^URL_COMPONENTS;
  URL_COMPONENTS = record
    dwStructSize      : DWORD;              // set to SizeOf(URL_COMPONENTS)
    lpszScheme        : LPWSTR;             // scheme name
    dwSchemeLength    : DWORD;
    nScheme           : INTERNET_SCHEME;    // enumerated scheme type
    lpszHostName      : LPWSTR;             // host name
    dwHostNameLength  : DWORD;
    nPort             : INTERNET_PORT;      // port number
    lpszUserName      : LPWSTR;             // user name
    dwUserNameLength  : DWORD;
    lpszPassword      : LPWSTR;             // password
    dwPasswordLength  : DWORD;
    lpszUrlPath       : LPWSTR;             // URL-path
    dwUrlPathLength   : DWORD;
    lpszExtraInfo     : LPWSTR;             // extra information
    dwExtraInfoLength : DWORD;
  end;

  URL_COMPONENTSW = URL_COMPONENTS;
  LPURL_COMPONENTSW = LPURL_COMPONENTS;


function WinHttpCloseHandle( aHandle : HINTERNET) : BOOL;  stdcall;

function WinHttpOpen( const pszAgentW       : LPCWSTR;
                      const dwAccessType    : DWORD;
                      const pszProxyW       : LPCWSTR;
                      const pszProxyBypassW : LPCWSTR;
                      const dwFlags         : DWORD
                      ) : HINTERNET;  stdcall;

function WinHttpConnect( const hSession : HINTERNET;
                         const pswzServerName : LPCWSTR;
                         const nServerPort : INTERNET_PORT;
                         const dwReserved : DWORD
                         ) : HINTERNET;  stdcall;

function WinHttpOpenRequest( const hConnect : HINTERNET;
                             const pwszVerb, pwszObjectName, pwszVersion, pwszReferrer : LPCWSTR;
                             const ppwszAcceptTypes : LPLPCWSTR;
                             const dwFlags : DWORD
                             ) : HINTERNET;  stdcall;

function WinHttpSetTimeouts( const hRequestOrSession : HINTERNET;
                             const aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout : Int32
                             ) : BOOL;  stdcall;

function WinHttpAddRequestHeaders( const hRequest : HINTERNET;
                                   const pwszHeaders : LPCWSTR;
                                   const dwHeadersLengthInChars : DWORD;
                                   const dwModifiers : DWORD
                                   ) : BOOL;  stdcall;

function WinHttpSendRequest( const hRequest : HINTERNET;
                             const lpszHeaders : LPCWSTR;
                             const dwHeadersLength : DWORD;
                             const lpOptional : Pointer;
                             const dwOptionalLength : DWORD;
                             const dwTotalLength : DWORD;
                             const pContext : Pointer
                             ) : BOOL;  stdcall;

function WinHttpWriteData( const hRequest : HINTERNET;
                           const pBuf : Pointer;
                           const dwBytesToWrite : DWORD;
                           out dwBytesWritten : DWORD
                           ) : BOOL;  stdcall;

function WinHttpReceiveResponse( const hRequest : HINTERNET; const lpReserved : Pointer) : BOOL;  stdcall;

function WinHttpQueryHeaders( const hRequest     : HINTERNET;
                              const dwInfoLevel  : DWORD;
                              const pwszName     : LPCWSTR;
                              const lpBuffer     : Pointer;
                              var dwBufferLength : DWORD;
                              var dwIndex        : DWORD
                              ) : BOOL;  stdcall;

function WinHttpQueryDataAvailable( const hRequest     : HINTERNET;
                                    var dwNumberOfBytesAvailable : DWORD
                                    ) : BOOL;  stdcall;

function WinHttpReadData( const hRequest      : HINTERNET;
                          const lpBuffer      : Pointer;
                          const dwBytesToRead : DWORD;
                          out dwBytesRead     : DWORD
                          ) : BOOL;  stdcall;

function WinHttpCrackUrl( const pwszUrl : LPCWSTR;
                          const dwUrlLength : DWORD;
                          const dwFlags : DWORD;
                          var urlComponents : URL_COMPONENTS
                          ) : BOOL;  stdcall;

function WinHttpCreateUrl( const UrlComponents : URL_COMPONENTS;
                           const dwFlags : DWORD;
                           const pwszUrl : LPCWSTR;
                           var pdwUrlLength : DWORD
                          ) : BOOL;  stdcall;


const
  // WinHttpOpen dwAccessType values
  WINHTTP_ACCESS_TYPE_DEFAULT_PROXY = 0;
  WINHTTP_ACCESS_TYPE_NO_PROXY      = 1;
  WINHTTP_ACCESS_TYPE_NAMED_PROXY   = 3;

  // flags for WinHttpOpen():
  WINHTTP_FLAG_ASYNC = $10000000;  // want async session, requires WinHttpSetStatusCallback() usage

  // ports
  INTERNET_DEFAULT_PORT = 0;           // use the protocol-specific default (80 or 443)

  // flags for WinHttpOpenRequest():
  WINHTTP_FLAG_SECURE                = $00800000;  // use SSL if applicable (HTTPS)
  WINHTTP_FLAG_ESCAPE_PERCENT        = $00000004;  // if escaping enabled, escape percent as well
  WINHTTP_FLAG_NULL_CODEPAGE         = $00000008;  // assume all symbols are ASCII, use fast convertion
  WINHTTP_FLAG_BYPASS_PROXY_CACHE    = $00000100; // add "pragma: no-cache" request header
  WINHTTP_FLAG_REFRESH               = WINHTTP_FLAG_BYPASS_PROXY_CACHE;
  WINHTTP_FLAG_ESCAPE_DISABLE        = $00000040;  // disable escaping
  WINHTTP_FLAG_ESCAPE_DISABLE_QUERY  = $00000080;  // if escaping enabled escape path part, but do not escape query

  // flags for WinHttpSendRequest():
  WINHTTP_NO_ADDITIONAL_HEADERS   = nil;
  WINHTTP_NO_REQUEST_DATA         = nil;

  // WinHttpAddRequestHeaders() dwModifiers
  WINHTTP_ADDREQ_INDEX_MASK = $0000FFFF;
  WINHTTP_ADDREQ_FLAGS_MASK = $FFFF0000;

  WINHTTP_ADDREQ_FLAG_ADD_IF_NEW                = $10000000;
  WINHTTP_ADDREQ_FLAG_ADD                       = $20000000;
  WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA       = $40000000;
  WINHTTP_ADDREQ_FLAG_COALESCE_WITH_SEMICOLON   = $01000000;
  WINHTTP_ADDREQ_FLAG_COALESCE                  = WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA;
  WINHTTP_ADDREQ_FLAG_REPLACE                   = $80000000;

  // URL functions
  ICU_NO_ENCODE          = $20000000;  // Don't convert unsafe characters to escape sequence
  ICU_DECODE             = $10000000;  // Convert %XX escape sequences to characters
  ICU_NO_META            = $08000000;  // Don't convert .. etc. meta path sequences
  ICU_ENCODE_SPACES_ONLY = $04000000;  // Encode spaces only
  ICU_BROWSER_MODE       = $02000000;  // Special encode/decode rules for browser
  ICU_ENCODE_PERCENT     = $00001000;  // Encode any percent (ASCII25)

  ICU_ESCAPE             = $80000000;  // (un)escape URL characters
  ICU_ESCAPE_AUTHORITY   = $00002000;  // causes InternetCreateUrlA to escape chars in authority components (user, pwd, host)
  ICU_REJECT_USERPWD     = $00004000;  // rejects usrls whick have username/pwd sections

  INTERNET_SCHEME_HTTP  = INTERNET_SCHEME(1);
  INTERNET_SCHEME_HTTPS = INTERNET_SCHEME(2);

const
  WINHTTP_ERROR_BASE                      = 12000;
  ERROR_WINHTTP_OUT_OF_HANDLES            = WINHTTP_ERROR_BASE + 1;
  ERROR_WINHTTP_TIMEOUT                   = WINHTTP_ERROR_BASE + 2;
  ERROR_WINHTTP_INTERNAL_ERROR            = WINHTTP_ERROR_BASE + 4;
  ERROR_WINHTTP_INVALID_URL               = WINHTTP_ERROR_BASE + 5;
  ERROR_WINHTTP_UNRECOGNIZED_SCHEME       = WINHTTP_ERROR_BASE + 6;
  ERROR_WINHTTP_NAME_NOT_RESOLVED         = WINHTTP_ERROR_BASE + 7;
  ERROR_WINHTTP_INVALID_OPTION            = WINHTTP_ERROR_BASE + 9;
  ERROR_WINHTTP_OPTION_NOT_SETTABLE       = WINHTTP_ERROR_BASE + 11;
  ERROR_WINHTTP_SHUTDOWN                  = WINHTTP_ERROR_BASE + 12;
  ERROR_WINHTTP_LOGIN_FAILURE             = WINHTTP_ERROR_BASE + 15;
  ERROR_WINHTTP_OPERATION_CANCELLED       = WINHTTP_ERROR_BASE + 17;
  ERROR_WINHTTP_INCORRECT_HANDLE_TYPE     = WINHTTP_ERROR_BASE + 18;
  ERROR_WINHTTP_INCORRECT_HANDLE_STATE    = WINHTTP_ERROR_BASE + 19;
  ERROR_WINHTTP_CANNOT_CONNECT            = WINHTTP_ERROR_BASE + 29;
  ERROR_WINHTTP_CONNECTION_ERROR          = WINHTTP_ERROR_BASE + 30;
  ERROR_WINHTTP_RESEND_REQUEST            = WINHTTP_ERROR_BASE + 32;
  ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED   = WINHTTP_ERROR_BASE + 44;
  ERROR_WINHTTP_CANNOT_CALL_BEFORE_OPEN	  = WINHTTP_ERROR_BASE + 100;
  ERROR_WINHTTP_CANNOT_CALL_BEFORE_SEND	  = WINHTTP_ERROR_BASE + 101;
  ERROR_WINHTTP_CANNOT_CALL_AFTER_SEND	  = WINHTTP_ERROR_BASE + 102;
  ERROR_WINHTTP_CANNOT_CALL_AFTER_OPEN	  = WINHTTP_ERROR_BASE + 103;
  ERROR_WINHTTP_HEADER_NOT_FOUND          = WINHTTP_ERROR_BASE + 150;
  ERROR_WINHTTP_INVALID_SERVER_RESPONSE   = WINHTTP_ERROR_BASE + 152;
  ERROR_WINHTTP_INVALID_HEADER            = WINHTTP_ERROR_BASE + 153;
  ERROR_WINHTTP_INVALID_QUERY_REQUEST     = WINHTTP_ERROR_BASE + 154;
  ERROR_WINHTTP_HEADER_ALREADY_EXISTS     = WINHTTP_ERROR_BASE + 155;
  ERROR_WINHTTP_REDIRECT_FAILED           = WINHTTP_ERROR_BASE + 156;
  ERROR_WINHTTP_AUTO_PROXY_SERVICE_ERROR  = WINHTTP_ERROR_BASE + 178;
  ERROR_WINHTTP_BAD_AUTO_PROXY_SCRIPT     = WINHTTP_ERROR_BASE + 166;
  ERROR_WINHTTP_UNABLE_TO_DOWNLOAD_SCRIPT = WINHTTP_ERROR_BASE + 167;
  ERROR_WINHTTP_NOT_INITIALIZED           = WINHTTP_ERROR_BASE + 172;
  ERROR_WINHTTP_SECURE_FAILURE            = WINHTTP_ERROR_BASE + 175;

  // Certificate security errors. Additional information is provided
  // via the WINHTTP_CALLBACK_STATUS_SECURE_FAILURE callback notification.
  ERROR_WINHTTP_SECURE_CERT_DATE_INVALID    = WINHTTP_ERROR_BASE + 37;
  ERROR_WINHTTP_SECURE_CERT_CN_INVALID      = WINHTTP_ERROR_BASE + 38;
  ERROR_WINHTTP_SECURE_INVALID_CA           = WINHTTP_ERROR_BASE + 45;
  ERROR_WINHTTP_SECURE_CERT_REV_FAILED      = WINHTTP_ERROR_BASE + 57;
  ERROR_WINHTTP_SECURE_CHANNEL_ERROR        = WINHTTP_ERROR_BASE + 157;
  ERROR_WINHTTP_SECURE_INVALID_CERT         = WINHTTP_ERROR_BASE + 169;
  ERROR_WINHTTP_SECURE_CERT_REVOKED         = WINHTTP_ERROR_BASE + 170;
  ERROR_WINHTTP_SECURE_CERT_WRONG_USAGE     = WINHTTP_ERROR_BASE + 179;

  ERROR_WINHTTP_AUTODETECTION_FAILED                  = WINHTTP_ERROR_BASE + 180;
  ERROR_WINHTTP_HEADER_COUNT_EXCEEDED                 = WINHTTP_ERROR_BASE + 181;
  ERROR_WINHTTP_HEADER_SIZE_OVERFLOW                  = WINHTTP_ERROR_BASE + 182;
  ERROR_WINHTTP_CHUNKED_ENCODING_HEADER_SIZE_OVERFLOW = WINHTTP_ERROR_BASE + 183;
  ERROR_WINHTTP_RESPONSE_DRAIN_OVERFLOW               = WINHTTP_ERROR_BASE + 184;
  ERROR_WINHTTP_CLIENT_CERT_NO_PRIVATE_KEY            = WINHTTP_ERROR_BASE + 185;
  ERROR_WINHTTP_CLIENT_CERT_NO_ACCESS_PRIVATE_KEY     = WINHTTP_ERROR_BASE + 186;


const
  WINHTTP_THRIFT_DEFAULTS = WINHTTP_FLAG_NULL_CODEPAGE
                         or WINHTTP_FLAG_BYPASS_PROXY_CACHE
                         or WINHTTP_FLAG_ESCAPE_DISABLE;


type
  IWinHTTPRequest = interface
    ['{35C6D9D4-FDCE-42C6-B84C-9294E6FB904C}']
    function  Handle : HINTERNET;
    function  AddRequestHeader( const aHeader : string; const addflag : DWORD = WINHTTP_ADDREQ_FLAG_ADD) : Boolean;
    function  SetTimeouts( const aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout : Int32) : Boolean;
    function  SendRequest( const pBuf : Pointer; const dwBytes : DWORD; const dwExtra : DWORD = 0) : Boolean;
    function  WriteExtraData( const pBuf : Pointer; const dwBytes : DWORD) : DWORD;
    function  FlushAndReceiveResponse : Boolean;
    function  ReadData( const dwRead : DWORD) : TBytes;  overload;
    function  ReadData( const pBuf : Pointer; const dwRead : DWORD) : DWORD;  overload;
  end;

  IWinHTTPConnection = interface
    ['{1C4F78B5-1525-4788-B638-A0E41BCF4D43}']
    function  Handle : HINTERNET;
    function  OpenRequest( const secure : Boolean; const aVerb, aObjName, aAcceptTypes : UnicodeString) : IWinHTTPRequest;
  end;

  IWinHTTPSession = interface
    ['{B6F8BD98-0605-4A9E-B671-4CB191D74A5E}']
    function  Handle : HINTERNET;
    function  Connect( const aHostName : UnicodeString; const aPort : INTERNET_PORT = INTERNET_DEFAULT_PORT) : IWinHTTPConnection;
    function  SetTimeouts( const aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout : Int32) : Boolean;
  end;

  IWinHTTPUrl = interface
    ['{78BE977C-4171-4AF5-A250-FD2890205E63}']
    // url parts getter
    function  GetScheme : UnicodeString;
    function  GetNumScheme : INTERNET_SCHEME;
    function  GetHostName  : UnicodeString;
    function  GetPort : INTERNET_PORT;
    function  GetUserName  : UnicodeString;
    function  GetPassword  : UnicodeString;
    function  GetUrlPath   : UnicodeString;
    function  GetExtraInfo : UnicodeString;

    // url parts setter
    procedure SetScheme( const value : UnicodeString);
    procedure SetHostName ( const value : UnicodeString);
    procedure SetPort( const value : INTERNET_PORT);
    procedure SetUserName( const value : UnicodeString);
    procedure SetPassword( const value : UnicodeString);
    procedure SetUrlPath( const value : UnicodeString);
    procedure SetExtraInfo( const value : UnicodeString);

    // url as a whole
    function  BuildUrl : UnicodeString;
    procedure CrackUrl( const value : UnicodeString);

    // url parts
    property  Scheme    : UnicodeString   read GetScheme    write SetScheme;
    property  NumScheme : INTERNET_SCHEME read GetNumScheme; // readonly
    property  HostName  : UnicodeString   read GetHostName  write SetHostName;
    property  Port      : INTERNET_PORT   read GetPort      write SetPort;
    property  UserName  : UnicodeString   read GetUserName  write SetUserName;
    property  Password  : UnicodeString   read GetPassword  write SetPassword;
    property  UrlPath   : UnicodeString   read GetUrlPath   write SetUrlPath;
    property  ExtraInfo : UnicodeString   read GetExtraInfo write SetExtraInfo;

    // url as a whole
    property  CompleteURL : UnicodeString read BuildUrl write CrackUrl;
  end;




type
  TWinHTTPHandleObjectImpl = class( TInterfacedObject)
  strict protected
    FHandle : HINTERNET;
    function  Handle : HINTERNET;
  public
    constructor Create( const aHandle : HINTERNET);
    destructor Destroy;  override;
  end;


  TWinHTTPSessionImpl = class( TWinHTTPHandleObjectImpl, IWinHTTPSession)
  strict protected

    // IWinHTTPSession
    function  Connect( const aHostName : UnicodeString; const aPort : INTERNET_PORT = INTERNET_DEFAULT_PORT) : IWinHTTPConnection;
    function  SetTimeouts( const aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout : Int32) : Boolean;

  public
    constructor Create( const aAgent : UnicodeString;
                        const aAccessType : DWORD          = WINHTTP_ACCESS_TYPE_DEFAULT_PROXY;
                        const aProxy : UnicodeString       = '';
                        const aProxyBypass : UnicodeString = '';
                        const aFlags : DWORD               = 0);
    destructor Destroy;  override;
  end;


  TWinHTTPConnectionImpl = class( TWinHTTPHandleObjectImpl, IWinHTTPConnection)
  strict protected
    FSession : IWinHTTPSession;

    // IWinHTTPConnection
    function  OpenRequest( const secure : Boolean; const aVerb, aObjName, aAcceptTypes : UnicodeString) : IWinHTTPRequest;

  public
    constructor Create( const aSession : IWinHTTPSession; const aHostName : UnicodeString; const aPort : INTERNET_PORT);
    destructor Destroy;  override;
  end;


  TAcceptTypesArray = array of string;

  TWinHTTPRequestImpl = class( TWinHTTPHandleObjectImpl, IWinHTTPRequest)
  strict protected
    FConnection : IWinHTTPConnection;

    // IWinHTTPRequest
    function  AddRequestHeader( const aHeader : string; const addflag : DWORD = WINHTTP_ADDREQ_FLAG_ADD) : Boolean;
    function  SetTimeouts( const aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout : Int32) : Boolean;
    function  SendRequest( const pBuf : Pointer; const dwBytes : DWORD; const dwExtra : DWORD = 0) : Boolean;
    function  WriteExtraData( const pBuf : Pointer; const dwBytes : DWORD) : DWORD;
    function  FlushAndReceiveResponse : Boolean;
    function  ReadData( const dwRead : DWORD) : TBytes;  overload;
    function  ReadData( const pBuf : Pointer; const dwRead : DWORD) : DWORD;  overload;

  public
    constructor Create( const aConnection : IWinHTTPConnection;
                        const aVerb, aObjName : UnicodeString;
                        const aVersion : UnicodeString = '';
                        const aReferrer : UnicodeString = '';
                        const aAcceptTypes : UnicodeString = '*/*';
                        const aFlags : DWORD = WINHTTP_THRIFT_DEFAULTS
                        );

    destructor Destroy;  override;
  end;


  TWinHTTPUrlImpl = class( TInterfacedObject, IWinHTTPUrl)
  strict private
    FScheme    : UnicodeString;
    FNumScheme : INTERNET_SCHEME;
    FHostName  : UnicodeString;
    FPort      : INTERNET_PORT;
    FUserName  : UnicodeString;
    FPassword  : UnicodeString;
    FUrlPath   : UnicodeString;
    FExtraInfo : UnicodeString;

  strict protected
    // url parts getter
    function  GetScheme    : UnicodeString;
    function  GetNumScheme : INTERNET_SCHEME;
    function  GetHostName  : UnicodeString;
    function  GetPort      : INTERNET_PORT;
    function  GetUserName  : UnicodeString;
    function  GetPassword  : UnicodeString;
    function  GetUrlPath   : UnicodeString;
    function  GetExtraInfo : UnicodeString;

    // url parts setter
    procedure SetScheme(    const value : UnicodeString);
    procedure SetHostName ( const value : UnicodeString);
    procedure SetPort(      const value : INTERNET_PORT);
    procedure SetUserName(  const value : UnicodeString);
    procedure SetPassword(  const value : UnicodeString);
    procedure SetUrlPath(   const value : UnicodeString);
    procedure SetExtraInfo( const value : UnicodeString);

    // url as a whole
    function  BuildUrl : UnicodeString;
    procedure CrackUrl( const value : UnicodeString);

  public
    constructor Create( const aUri : UnicodeString);
    destructor Destroy;  override;
  end;


  EWinHTTPException = class(Exception);

implementation

const WINHTTP_DLL = 'WinHTTP.dll';

function WinHttpCloseHandle;  stdcall; external WINHTTP_DLL;
function WinHttpOpen;  stdcall; external WINHTTP_DLL;
function WinHttpConnect;  stdcall; external WINHTTP_DLL;
function WinHttpOpenRequest; stdcall; external WINHTTP_DLL;
function WinHttpSendRequest; stdcall; external WINHTTP_DLL;
function WinHttpSetTimeouts; stdcall; external WINHTTP_DLL;
function WinHttpAddRequestHeaders; stdcall; external WINHTTP_DLL;
function WinHttpWriteData; stdcall; external WINHTTP_DLL;
function WinHttpReceiveResponse; stdcall; external WINHTTP_DLL;
function WinHttpQueryHeaders; stdcall; external WINHTTP_DLL;
function WinHttpQueryDataAvailable; stdcall; external WINHTTP_DLL;
function WinHttpReadData; stdcall; external WINHTTP_DLL;
function WinHttpCrackUrl; stdcall; external WINHTTP_DLL;
function WinHttpCreateUrl; stdcall; external WINHTTP_DLL;


{ TWinHTTPHandleObjectImpl }

constructor TWinHTTPHandleObjectImpl.Create( const aHandle : HINTERNET);
begin
  inherited Create;
  FHandle := aHandle;

  if FHandle = nil
  then raise EWinHTTPException.Create('Invalid handle');
end;


destructor TWinHTTPHandleObjectImpl.Destroy;
begin
  try
    if Assigned(FHandle) then begin
      WinHttpCloseHandle(FHandle);
      FHandle := nil;
    end;

  finally
    inherited Destroy;
  end;
end;


function TWinHTTPHandleObjectImpl.Handle : HINTERNET;
begin
  result := FHandle;
end;


{ TWinHTTPSessionImpl }


constructor TWinHTTPSessionImpl.Create( const aAgent : UnicodeString; const aAccessType : DWORD;
                                        const aProxy, aProxyBypass : UnicodeString; const aFlags : DWORD);
var handle : HINTERNET;
begin
  handle := WinHttpOpen( PWideChar(aAgent), aAccessType,
                         PWideChar(Pointer(aProxy)),        // may be nil
                         PWideChar(Pointer(aProxyBypass)),  // may be nil
                         aFlags);
  inherited Create( handle);
end;


destructor TWinHTTPSessionImpl.Destroy;
begin
  inherited Destroy;
  // add code here
end;


function TWinHTTPSessionImpl.Connect( const aHostName : UnicodeString; const aPort : INTERNET_PORT) : IWinHTTPConnection;
begin
  result := TWinHTTPConnectionImpl.Create( Self, aHostName, aPort);
end;


function TWinHTTPSessionImpl.SetTimeouts( const aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout : Int32) : Boolean;
begin
  result := WinHttpSetTimeouts( FHandle, aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout);
end;


{ TWinHTTPConnectionImpl }

constructor TWinHTTPConnectionImpl.Create( const aSession : IWinHTTPSession; const aHostName : UnicodeString; const aPort : INTERNET_PORT);
var handle : HINTERNET;
begin
  FSession := aSession;
  handle   := WinHttpConnect( FSession.Handle, PWideChar(aHostName), aPort, 0);
  inherited Create( handle);
end;


destructor TWinHTTPConnectionImpl.Destroy;
begin
  inherited Destroy;
  FSession := nil;
end;


function TWinHTTPConnectionImpl.OpenRequest( const secure : Boolean; const aVerb, aObjName, aAcceptTypes : UnicodeString) : IWinHTTPRequest;
var dwFlags : DWORD;
begin
  dwFlags := WINHTTP_THRIFT_DEFAULTS;
  if secure
  then dwFlags := dwFlags or WINHTTP_FLAG_SECURE
  else dwFlags := dwFlags and not WINHTTP_FLAG_SECURE;

  result := TWinHTTPRequestImpl.Create( Self, aVerb, aObjName, '', '', aAcceptTypes, dwFlags);
end;


{ TWinHTTPRequestImpl }

constructor TWinHTTPRequestImpl.Create( const aConnection : IWinHTTPConnection;
                                        const aVerb, aObjName, aVersion, aReferrer : UnicodeString;
                                        const aAcceptTypes : UnicodeString;
                                        const aFlags : DWORD
                                        );
var handle : HINTERNET;
    accept : array[0..1] of PWideChar;
begin
  FConnection := aConnection;

  accept[0] := PWideChar(aAcceptTypes);
  accept[1] := nil;

  handle      := WinHttpOpenRequest( FConnection.Handle,
                                     PWideChar(UpperCase(aVerb)),
                                     PWideChar(aObjName),
                                     PWideChar(aVersion),
                                     PWideChar(aReferrer),
                                     @accept,
                                     aFlags);
  inherited Create( handle);
end;


destructor TWinHTTPRequestImpl.Destroy;
begin
  inherited Destroy;
  FConnection := nil;
end;


function TWinHTTPRequestImpl.SetTimeouts( const aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout : Int32) : Boolean;
begin
  result := WinHttpSetTimeouts( FHandle, aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout);
end;


function TWinHTTPRequestImpl.AddRequestHeader( const aHeader : string; const addflag : DWORD) : Boolean;
begin
  result := WinHttpAddRequestHeaders( FHandle, PWideChar(aHeader), DWORD(-1), addflag);
end;


function TWinHTTPRequestImpl.SendRequest( const pBuf : Pointer; const dwBytes, dwExtra : DWORD) : Boolean;
begin
  result := WinHttpSendRequest( FHandle,
                                WINHTTP_NO_ADDITIONAL_HEADERS, 0,
                                pBuf, dwBytes,      // number of bytes in pBuf
                                dwBytes + dwExtra,  // becomes the Content-Length
                                nil);               // context for async operations
end;


function TWinHTTPRequestImpl.WriteExtraData( const pBuf : Pointer; const dwBytes : DWORD) : DWORD;
begin
  if not WinHttpWriteData( FHandle, pBuf, dwBytes, result)
  then result := 0;
end;


function TWinHTTPRequestImpl.FlushAndReceiveResponse : Boolean;
begin
  result := WinHttpReceiveResponse( FHandle, nil);
end;


function TWinHTTPRequestImpl.ReadData( const dwRead : DWORD) : TBytes;
var dwAvailable, dwReceived : DWORD;
begin
  if WinHttpQueryDataAvailable( FHandle, dwAvailable)
  then dwAvailable := Min( dwRead, dwAvailable)
  else dwAvailable := 0;

  SetLength( result, dwAvailable);
  if dwAvailable = 0 then Exit;

  if WinHttpReadData( FHandle, @result[0], Length(result), dwReceived)
  then SetLength( result, dwReceived)
  else SetLength( result, 0);
end;


function TWinHTTPRequestImpl.ReadData( const pBuf : Pointer; const dwRead : DWORD) : DWORD;
var dwAvailable : DWORD;
begin
  if WinHttpQueryDataAvailable( FHandle, dwAvailable)
  then dwAvailable := Min( dwRead, dwAvailable)
  else dwAvailable := 0;

  if (dwAvailable = 0)
  or not WinHttpReadData( FHandle, pBuf, dwAvailable, result)
  then result := 0;
end;


{ TWinHTTPUrlImpl }

constructor TWinHTTPUrlImpl.Create(const aUri: UnicodeString);
begin
  inherited Create;
  CrackUrl( aUri)
end;


destructor TWinHTTPUrlImpl.Destroy;
begin
  inherited Destroy;
end;


procedure TWinHTTPUrlImpl.CrackURL( const value : UnicodeString);
const FLAGS = 0; // no special operations, leave components as-is
var components : URL_COMPONENTS;
begin
  FillChar(components, SizeOf(components), 0);
  components.dwStructSize := SizeOf(components);

  if value <> '' then begin
    { For the WinHttpCrackUrl function, [...] if the pointer member is NULL but the
      length member is not zero, both the pointer and length members are returned. }
    components.dwSchemeLength    := DWORD(-1);
    components.dwHostNameLength  := DWORD(-1);
    components.dwUserNameLength  := DWORD(-1);
    components.dwPasswordLength  := DWORD(-1);
    components.dwUrlPathLength   := DWORD(-1);
    components.dwExtraInfoLength := DWORD(-1);

    WinHttpCrackUrl( PWideChar(value), Length(value), FLAGS, components);
  end;

  FNumScheme := components.nScheme;
  FPort      := components.nPort;
  SetString( FScheme,    components.lpszScheme,    components.dwSchemeLength);
  SetString( FHostName,  components.lpszHostName,  components.dwHostNameLength);
  SetString( FUserName,  components.lpszUserName,  components.dwUserNameLength);
  SetString( FPassword,  components.lpszPassword,  components.dwPasswordLength);
  SetString( FUrlPath,   components.lpszUrlPath,   components.dwUrlPathLength);
  SetString( FExtraInfo, components.lpszExtraInfo, components.dwExtraInfoLength);
end;


function TWinHTTPUrlImpl.BuildUrl : UnicodeString;
const FLAGS = 0; // no special operations, leave components as-is
var components : URL_COMPONENTS;
    dwChars : DWORD;
begin
  FillChar(components, SizeOf(components), 0);
  components.dwStructSize      := SizeOf(components);
  components.lpszScheme        := PWideChar(FScheme);
  components.dwSchemeLength    := Length(FScheme);
  components.lpszHostName      := PWideChar(FHostName);
  components.dwHostNameLength  := Length(FHostName);
  components.nPort             := FPort;
  components.lpszUserName      := PWideChar(FUserName);
  components.dwUserNameLength  := Length(FUserName);
  components.lpszPassword      := PWideChar(FPassword);
  components.dwPasswordLength  := Length(FPassword);
  components.lpszUrlPath       := PWideChar(FUrlPath);
  components.dwUrlPathLength   := Length(FUrlPath);
  components.lpszExtraInfo     := PWideChar(FExtraInfo);
  components.dwExtraInfoLength := Length(FExtraInfo);

  WinHttpCreateUrl( components, FLAGS, nil, dwChars);
  if dwChars = 0
  then result := ''
  else begin
    SetLength( result, dwChars + 1);
    WinHttpCreateUrl( components, FLAGS, @result[1], dwChars);
    SetLength( result, dwChars);  // cut off terminating #0
  end;
end;


function TWinHTTPUrlImpl.GetExtraInfo: UnicodeString;
begin
  result := FExtraInfo;
end;

function TWinHTTPUrlImpl.GetHostName: UnicodeString;
begin
  result := FHostName;
end;

function TWinHTTPUrlImpl.GetNumScheme: INTERNET_SCHEME;
begin
  result := FNumScheme;
end;

function TWinHTTPUrlImpl.GetPassword: UnicodeString;
begin
  result := FPassword;
end;

function TWinHTTPUrlImpl.GetPort: INTERNET_PORT;
begin
  result := FPort;
end;

function TWinHTTPUrlImpl.GetScheme: UnicodeString;
begin
  result := FScheme;
end;

function TWinHTTPUrlImpl.GetUrlPath: UnicodeString;
begin
  result := FUrlPath;
end;

function TWinHTTPUrlImpl.GetUserName: UnicodeString;
begin
  result := FUserName;
end;

procedure TWinHTTPUrlImpl.SetExtraInfo(const value: UnicodeString);
begin
  FExtraInfo := value;
end;

procedure TWinHTTPUrlImpl.SetHostName(const value: UnicodeString);
begin
  FHostName := value;
end;

procedure TWinHTTPUrlImpl.SetPassword(const value: UnicodeString);
begin
  FPassword := value;
end;

procedure TWinHTTPUrlImpl.SetPort(const value: INTERNET_PORT);
begin
  FPort := value;
end;

procedure TWinHTTPUrlImpl.SetScheme(const value: UnicodeString);
begin
  FScheme := value;
end;

procedure TWinHTTPUrlImpl.SetUrlPath(const value: UnicodeString);
begin
  FUrlPath := value;
end;

procedure TWinHTTPUrlImpl.SetUserName(const value: UnicodeString);
begin
  FUserName := value;
end;


end.

