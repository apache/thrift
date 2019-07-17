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


  // When retrieving proxy data, an application must free the lpszProxy and
  // lpszProxyBypass strings contained in this structure (if they are non-NULL)
  // using the GlobalFree function.
  LPWINHTTP_PROXY_INFO = ^WINHTTP_PROXY_INFO;
  WINHTTP_PROXY_INFO = record
    dwAccessType    : DWORD;      // see WINHTTP_ACCESS_* types below
    lpszProxy       : LPWSTR;     // proxy server list
    lpszProxyBypass : LPWSTR;     // proxy bypass list
  end;

  LPWINHTTP_PROXY_INFOW = ^WINHTTP_PROXY_INFOW;
  WINHTTP_PROXY_INFOW   = WINHTTP_PROXY_INFO;


  WINHTTP_AUTOPROXY_OPTIONS = record
    dwFlags                : DWORD;
    dwAutoDetectFlags      : DWORD;
    lpszAutoConfigUrl      : LPCWSTR;
    lpvReserved            : LPVOID;
    dwReserved             : DWORD;
    fAutoLogonIfChallenged : BOOL;
  end;


  WINHTTP_CURRENT_USER_IE_PROXY_CONFIG = record
    fAutoDetect       : BOOL;
    lpszAutoConfigUrl : LPWSTR;
    lpszProxy         : LPWSTR;
    lpszProxyBypass   : LPWSTR;
  end;




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

function WinHttpQueryOption( const hInternet : HINTERNET;
                             const dwOption : DWORD;
                             const pBuffer : Pointer;
                             var dwBufferLength : DWORD) : BOOL;  stdcall;

function WinHttpSetOption( const hInternet : HINTERNET;
                           const dwOption : DWORD;
                           const pBuffer : Pointer;
                           const dwBufferLength : DWORD) : BOOL;  stdcall;

function WinHttpSetTimeouts( const hRequestOrSession : HINTERNET;
                             const aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout : Int32
                             ) : BOOL;  stdcall;

function WinHttpAddRequestHeaders( const hRequest : HINTERNET;
                                   const pwszHeaders : LPCWSTR;
                                   const dwHeadersLengthInChars : DWORD;
                                   const dwModifiers : DWORD
                                   ) : BOOL;  stdcall;

function WinHttpGetProxyForUrl( const hSession  : HINTERNET;
                                const lpcwszUrl : LPCWSTR;
                                const options   : WINHTTP_AUTOPROXY_OPTIONS;
                                const info      : WINHTTP_PROXY_INFO
                                ) : BOOL;  stdcall;

function WinHttpGetIEProxyConfigForCurrentUser( var config : WINHTTP_CURRENT_USER_IE_PROXY_CONFIG
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

  WINHTTP_NO_CLIENT_CERT_CONTEXT = nil;

  // options manifests for WinHttp{Query|Set}Option
  WINHTTP_OPTION_CALLBACK = 1;
  WINHTTP_OPTION_RESOLVE_TIMEOUT = 2;
  WINHTTP_OPTION_CONNECT_TIMEOUT = 3;
  WINHTTP_OPTION_CONNECT_RETRIES = 4;
  WINHTTP_OPTION_SEND_TIMEOUT = 5;
  WINHTTP_OPTION_RECEIVE_TIMEOUT = 6;
  WINHTTP_OPTION_RECEIVE_RESPONSE_TIMEOUT = 7;
  WINHTTP_OPTION_HANDLE_TYPE = 9;
  WINHTTP_OPTION_READ_BUFFER_SIZE = 12;
  WINHTTP_OPTION_WRITE_BUFFER_SIZE = 13;
  WINHTTP_OPTION_PARENT_HANDLE = 21;
  WINHTTP_OPTION_EXTENDED_ERROR = 24;
  WINHTTP_OPTION_SECURITY_FLAGS = 31;
  WINHTTP_OPTION_SECURITY_CERTIFICATE_STRUCT = 32;
  WINHTTP_OPTION_URL = 34;
  WINHTTP_OPTION_SECURITY_KEY_BITNESS = 36;
  WINHTTP_OPTION_PROXY = 38;
  WINHTTP_OPTION_USER_AGENT = 41;
  WINHTTP_OPTION_CONTEXT_VALUE = 45;
  WINHTTP_OPTION_CLIENT_CERT_CONTEXT = 47;
  WINHTTP_OPTION_REQUEST_PRIORITY = 58;
  WINHTTP_OPTION_HTTP_VERSION = 59;
  WINHTTP_OPTION_DISABLE_FEATURE = 63;
  WINHTTP_OPTION_CODEPAGE = 68;
  WINHTTP_OPTION_MAX_CONNS_PER_SERVER = 73;
  WINHTTP_OPTION_MAX_CONNS_PER_1_0_SERVER = 74;
  WINHTTP_OPTION_AUTOLOGON_POLICY = 77;
  WINHTTP_OPTION_SERVER_CERT_CONTEXT = 78;
  WINHTTP_OPTION_ENABLE_FEATURE = 79;
  WINHTTP_OPTION_WORKER_THREAD_COUNT = 80;
  WINHTTP_OPTION_PASSPORT_COBRANDING_TEXT = 81;
  WINHTTP_OPTION_PASSPORT_COBRANDING_URL = 82;
  WINHTTP_OPTION_CONFIGURE_PASSPORT_AUTH = 83;
  WINHTTP_OPTION_SECURE_PROTOCOLS = 84;
  WINHTTP_OPTION_ENABLETRACING = 85;
  WINHTTP_OPTION_PASSPORT_SIGN_OUT = 86;
  WINHTTP_OPTION_PASSPORT_RETURN_URL = 87;
  WINHTTP_OPTION_REDIRECT_POLICY = 88;
  WINHTTP_OPTION_MAX_HTTP_AUTOMATIC_REDIRECTS = 89;
  WINHTTP_OPTION_MAX_HTTP_STATUS_CONTINUE = 90;
  WINHTTP_OPTION_MAX_RESPONSE_HEADER_SIZE = 91;
  WINHTTP_OPTION_MAX_RESPONSE_DRAIN_SIZE = 92;
  WINHTTP_OPTION_CONNECTION_INFO = 93;
  WINHTTP_OPTION_CLIENT_CERT_ISSUER_LIST = 94;
  WINHTTP_OPTION_SPN = 96;
  WINHTTP_OPTION_GLOBAL_PROXY_CREDS = 97;
  WINHTTP_OPTION_GLOBAL_SERVER_CREDS = 98;
  WINHTTP_OPTION_UNLOAD_NOTIFY_EVENT = 99;
  WINHTTP_OPTION_REJECT_USERPWD_IN_URL = 100;
  WINHTTP_OPTION_USE_GLOBAL_SERVER_CREDENTIALS = 101;
  WINHTTP_OPTION_RECEIVE_PROXY_CONNECT_RESPONSE = 103;
  WINHTTP_OPTION_IS_PROXY_CONNECT_RESPONSE = 104;
  WINHTTP_OPTION_SERVER_SPN_USED = 106;
  WINHTTP_OPTION_PROXY_SPN_USED = 107;
  WINHTTP_OPTION_SERVER_CBT = 108;
  // options for newer WinHTTP versions
  WINHTTP_OPTION_DECOMPRESSION = 118;
  //
  WINHTTP_FIRST_OPTION = WINHTTP_OPTION_CALLBACK;
  //WINHTTP_LAST_OPTION = WINHTTP_OPTION_SERVER_CBT;

  WINHTTP_OPTION_USERNAME = $1000;
  WINHTTP_OPTION_PASSWORD = $1001;
  WINHTTP_OPTION_PROXY_USERNAME = $1002;
  WINHTTP_OPTION_PROXY_PASSWORD = $1003;

  // manifest value for WINHTTP_OPTION_MAX_CONNS_PER_SERVER and WINHTTP_OPTION_MAX_CONNS_PER_1_0_SERVER
  WINHTTP_CONNS_PER_SERVER_UNLIMITED    = $FFFFFFFF;

  // values for WINHTTP_OPTION_AUTOLOGON_POLICY
  WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM  = 0;
  WINHTTP_AUTOLOGON_SECURITY_LEVEL_LOW     = 1;
  WINHTTP_AUTOLOGON_SECURITY_LEVEL_HIGH    = 2;

  WINHTTP_AUTOLOGON_SECURITY_LEVEL_DEFAULT = WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM;

  // values for WINHTTP_OPTION_REDIRECT_POLICY
  WINHTTP_OPTION_REDIRECT_POLICY_NEVER                  = 0;
  WINHTTP_OPTION_REDIRECT_POLICY_DISALLOW_HTTPS_TO_HTTP = 1;
  WINHTTP_OPTION_REDIRECT_POLICY_ALWAYS                 = 2;

  WINHTTP_OPTION_REDIRECT_POLICY_LAST      = WINHTTP_OPTION_REDIRECT_POLICY_ALWAYS;
  WINHTTP_OPTION_REDIRECT_POLICY_DEFAULT   = WINHTTP_OPTION_REDIRECT_POLICY_DISALLOW_HTTPS_TO_HTTP;

  WINHTTP_DISABLE_PASSPORT_AUTH      = $00000000;
  WINHTTP_ENABLE_PASSPORT_AUTH       = $10000000;
  WINHTTP_DISABLE_PASSPORT_KEYRING   = $20000000;
  WINHTTP_ENABLE_PASSPORT_KEYRING    = $40000000;

  // values for WINHTTP_OPTION_DISABLE_FEATURE
  WINHTTP_DISABLE_COOKIES            = $00000001;
  WINHTTP_DISABLE_REDIRECTS          = $00000002;
  WINHTTP_DISABLE_AUTHENTICATION     = $00000004;
  WINHTTP_DISABLE_KEEP_ALIVE         = $00000008;

  // values for WINHTTP_OPTION_ENABLE_FEATURE
  WINHTTP_ENABLE_SSL_REVOCATION            = $00000001;
  WINHTTP_ENABLE_SSL_REVERT_IMPERSONATION  = $00000002;

  // values for WINHTTP_OPTION_SPN
  WINHTTP_DISABLE_SPN_SERVER_PORT    = $00000000;
  WINHTTP_ENABLE_SPN_SERVER_PORT     = $00000001;
  WINHTTP_OPTION_SPN_MASK            = WINHTTP_ENABLE_SPN_SERVER_PORT;

  // winhttp handle types
  WINHTTP_HANDLE_TYPE_SESSION  = 1;
  WINHTTP_HANDLE_TYPE_CONNECT  = 2;
  WINHTTP_HANDLE_TYPE_REQUEST  = 3;

  // values for auth schemes
  WINHTTP_AUTH_SCHEME_BASIC      = $00000001;
  WINHTTP_AUTH_SCHEME_NTLM       = $00000002;
  WINHTTP_AUTH_SCHEME_PASSPORT   = $00000004;
  WINHTTP_AUTH_SCHEME_DIGEST     = $00000008;
  WINHTTP_AUTH_SCHEME_NEGOTIATE  = $00000010;

  // WinHttp supported Authentication Targets
  WINHTTP_AUTH_TARGET_SERVER     = $00000000;
  WINHTTP_AUTH_TARGET_PROXY      = $00000001;

  // options for WINHTTP_OPTION_DECOMPRESSION
  WINHTTP_DECOMPRESSION_FLAG_GZIP    = $00000001;
  WINHTTP_DECOMPRESSION_FLAG_DEFLATE = $00000002;
  WINHTTP_DECOMPRESSION_FLAG_ALL     = WINHTTP_DECOMPRESSION_FLAG_GZIP
                                    or WINHTTP_DECOMPRESSION_FLAG_DEFLATE;

  // values for WINHTTP_OPTION_SECURITY_FLAGS

  // query only
  SECURITY_FLAG_SECURE           = $00000001; // can query only
  SECURITY_FLAG_STRENGTH_WEAK    = $10000000;
  SECURITY_FLAG_STRENGTH_MEDIUM  = $40000000;
  SECURITY_FLAG_STRENGTH_STRONG  = $20000000;

  // Secure connection error status flags
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_REV_FAILED         = $00000001;
  WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CERT            = $00000002;
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_REVOKED            = $00000004;
  WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CA              = $00000008;
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_CN_INVALID         = $00000010;
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_DATE_INVALID       = $00000020;
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_WRONG_USAGE        = $00000040;
  WINHTTP_CALLBACK_STATUS_FLAG_SECURITY_CHANNEL_ERROR  = $80000000;

  WINHTTP_FLAG_SECURE_PROTOCOL_SSL2   = $00000008;
  WINHTTP_FLAG_SECURE_PROTOCOL_SSL3   = $00000020;
  WINHTTP_FLAG_SECURE_PROTOCOL_TLS1   = $00000080;
  WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 = $00000200;
  WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 = $00000800;

  // Note: SECURE_PROTOCOL_ALL does not include TLS1.1 and higher!
  WINHTTP_FLAG_SECURE_PROTOCOL_ALL    = WINHTTP_FLAG_SECURE_PROTOCOL_SSL2
                                     or WINHTTP_FLAG_SECURE_PROTOCOL_SSL3
                                     or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1;

  // AutoProxy
  WINHTTP_AUTOPROXY_AUTO_DETECT           = $00000001;
  WINHTTP_AUTOPROXY_CONFIG_URL            = $00000002;
  WINHTTP_AUTOPROXY_HOST_KEEPCASE         = $00000004;
  WINHTTP_AUTOPROXY_HOST_LOWERCASE        = $00000008;
  WINHTTP_AUTOPROXY_RUN_INPROCESS         = $00010000;
  WINHTTP_AUTOPROXY_RUN_OUTPROCESS_ONLY   = $00020000;

  // Flags for dwAutoDetectFlags
  WINHTTP_AUTO_DETECT_TYPE_DHCP           = $00000001;
  WINHTTP_AUTO_DETECT_TYPE_DNS_A          = $00000002;

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

  WINHTTP_ERROR_LAST                                  = WINHTTP_ERROR_BASE + 186;


const
  WINHTTP_THRIFT_DEFAULTS = WINHTTP_FLAG_NULL_CODEPAGE
                         or WINHTTP_FLAG_BYPASS_PROXY_CACHE
                         or WINHTTP_FLAG_ESCAPE_DISABLE;



type
  IWinHTTPSession = interface;
  IWinHTTPConnection = interface;

  IWinHTTPRequest = interface
    ['{F65952F2-2F3B-47DC-B524-F1694E6D2AD7}']
    function  Handle : HINTERNET;
    function  Connection : IWinHTTPConnection;
    function  AddRequestHeader( const aHeader : string; const addflag : DWORD = WINHTTP_ADDREQ_FLAG_ADD) : Boolean;
    function  SetTimeouts( const aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout : Int32) : Boolean;
    procedure TryAutoProxy( const aUrl : string);
    procedure EnableAutomaticContentDecompression( const aEnable : Boolean);
    function  SendRequest( const pBuf : Pointer; const dwBytes : DWORD; const dwExtra : DWORD = 0) : Boolean;
    function  WriteExtraData( const pBuf : Pointer; const dwBytes : DWORD) : DWORD;
    function  FlushAndReceiveResponse : Boolean;
    function  ReadData( const dwRead : DWORD) : TBytes;  overload;
    function  ReadData( const pBuf : Pointer; const dwRead : DWORD) : DWORD;  overload;
  end;

  IWinHTTPConnection = interface
    ['{ED5BCA49-84D6-4CFE-BF18-3238B1FF2AFB}']
    function  Handle : HINTERNET;
    function  Session : IWinHTTPSession;
    function  OpenRequest( const secure : Boolean; const aVerb, aObjName, aAcceptTypes : UnicodeString) : IWinHTTPRequest;
  end;

  IWinHTTPSession = interface
    ['{261ADCB7-5465-4407-8840-468C17F009F0}']
    function  Handle : HINTERNET;
    function  Connect( const aHostName : UnicodeString; const aPort : INTERNET_PORT = INTERNET_DEFAULT_PORT) : IWinHTTPConnection;
    function  SetTimeouts( const aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout : Int32) : Boolean;
    function  EnableSecureProtocols( const aFlagSet : DWORD) : Boolean;
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
    function  EnableSecureProtocols( const aFlagSet : DWORD) : Boolean;
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
    function  Session : IWinHTTPSession;

  public
    constructor Create( const aSession : IWinHTTPSession; const aHostName : UnicodeString; const aPort : INTERNET_PORT);
    destructor Destroy;  override;
  end;


  TAcceptTypesArray = array of string;

  TWinHTTPRequestImpl = class( TWinHTTPHandleObjectImpl, IWinHTTPRequest)
  strict protected
    FConnection : IWinHTTPConnection;

    // IWinHTTPRequest
    function  Connection : IWinHTTPConnection;
    function  AddRequestHeader( const aHeader : string; const addflag : DWORD = WINHTTP_ADDREQ_FLAG_ADD) : Boolean;
    function  SetTimeouts( const aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout : Int32) : Boolean;
    procedure TryAutoProxy( const aUrl : string);
    procedure EnableAutomaticContentDecompression( const aEnable : Boolean);
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


  WINHTTP_PROXY_INFO_Helper = record helper for WINHTTP_PROXY_INFO
    procedure Initialize;
    procedure FreeAllocatedResources;
  end;


  WINHTTP_CURRENT_USER_IE_PROXY_CONFIG_Helper = record helper for WINHTTP_CURRENT_USER_IE_PROXY_CONFIG
    procedure Initialize;
    procedure FreeAllocatedResources;
  end;


  EWinHTTPException = class(Exception);

{ helper functions }

function WinHttpSysErrorMessage( const error : Cardinal): string;
procedure RaiseLastWinHttpError;


implementation

const WINHTTP_DLL = 'WinHTTP.dll';

function WinHttpCloseHandle;  stdcall; external WINHTTP_DLL;
function WinHttpOpen;  stdcall; external WINHTTP_DLL;
function WinHttpConnect;  stdcall; external WINHTTP_DLL;
function WinHttpOpenRequest; stdcall; external WINHTTP_DLL;
function WinHttpSendRequest; stdcall; external WINHTTP_DLL;
function WinHttpSetTimeouts; stdcall; external WINHTTP_DLL;
function WinHttpQueryOption; stdcall; external WINHTTP_DLL;
function WinHttpSetOption; stdcall; external WINHTTP_DLL;
function WinHttpAddRequestHeaders; stdcall; external WINHTTP_DLL;
function WinHttpGetProxyForUrl; stdcall; external WINHTTP_DLL;
function WinHttpGetIEProxyConfigForCurrentUser; stdcall; external WINHTTP_DLL;
function WinHttpWriteData; stdcall; external WINHTTP_DLL;
function WinHttpReceiveResponse; stdcall; external WINHTTP_DLL;
function WinHttpQueryHeaders; stdcall; external WINHTTP_DLL;
function WinHttpQueryDataAvailable; stdcall; external WINHTTP_DLL;
function WinHttpReadData; stdcall; external WINHTTP_DLL;
function WinHttpCrackUrl; stdcall; external WINHTTP_DLL;
function WinHttpCreateUrl; stdcall; external WINHTTP_DLL;


{ helper functions }

function WinHttpSysErrorMessage( const error : Cardinal): string;
const FLAGS = FORMAT_MESSAGE_ALLOCATE_BUFFER
           or FORMAT_MESSAGE_IGNORE_INSERTS
           or FORMAT_MESSAGE_FROM_SYSTEM
           or FORMAT_MESSAGE_FROM_HMODULE;
var pBuffer : PChar;
    nChars : Cardinal;
begin
  if (error < WINHTTP_ERROR_BASE)
  or (error > WINHTTP_ERROR_LAST)
  then Exit( SysUtils.SysErrorMessage( error));

  pBuffer := nil;
  try
    nChars := FormatMessage( FLAGS,
                             Pointer( GetModuleHandle( WINHTTP_DLL)),
                             error,
                             MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // default language
                             @pBuffer, 0,
                             nil);
    SetString( result, pBuffer, nChars);
  finally
    LocalFree( NativeUInt( pBuffer));
  end;
end;


procedure RaiseLastWinHttpError;
var error : Cardinal;
    sMsg  : string;
begin
  error := Cardinal( GetLastError);
  if error <> NOERROR then begin
    sMSg := IntToStr(Integer(error))+' '+WinHttpSysErrorMessage(error);
    raise EWinHTTPException.Create( sMsg);
  end;
end;



{ misc. record helper }


procedure GlobalFreeAndNil( var p : LPWSTR);
begin
  if p <> nil then begin
    GlobalFree( HGLOBAL( p));
    p := nil;
  end;
end;


procedure WINHTTP_PROXY_INFO_Helper.Initialize;
begin
  FillChar( Self, SizeOf(Self), 0);
end;


procedure WINHTTP_PROXY_INFO_Helper.FreeAllocatedResources;
// The caller must free the lpszProxy and lpszProxyBypass strings
// if they are non-NULL. Use GlobalFree to free the strings.
begin
  GlobalFreeAndNil( lpszProxy);
  GlobalFreeAndNil( lpszProxyBypass);
  Initialize;
end;


procedure WINHTTP_CURRENT_USER_IE_PROXY_CONFIG_Helper.Initialize;
begin
  FillChar( Self, SizeOf(Self), 0);
end;


procedure WINHTTP_CURRENT_USER_IE_PROXY_CONFIG_Helper.FreeAllocatedResources;
// The caller must free the lpszProxy, lpszProxyBypass and lpszAutoConfigUrl strings
// if they are non-NULL. Use GlobalFree to free the strings.
begin
  GlobalFreeAndNil( lpszProxy);
  GlobalFreeAndNil( lpszProxyBypass);
  GlobalFreeAndNil( lpszAutoConfigUrl);
  Initialize;
end;


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
  if handle = nil then RaiseLastWinHttpError;
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


function TWinHTTPSessionImpl.EnableSecureProtocols( const aFlagSet : DWORD) : Boolean;
var dwSize : DWORD;
begin
  dwSize := SizeOf(aFlagSet);
  result := WinHttpSetOption( Handle, WINHTTP_OPTION_SECURE_PROTOCOLS, @aFlagset, dwSize);
end;


{ TWinHTTPConnectionImpl }

constructor TWinHTTPConnectionImpl.Create( const aSession : IWinHTTPSession; const aHostName : UnicodeString; const aPort : INTERNET_PORT);
var handle : HINTERNET;
begin
  FSession := aSession;
  handle   := WinHttpConnect( FSession.Handle, PWideChar(aHostName), aPort, 0);
  if handle = nil then RaiseLastWinHttpError;
  inherited Create( handle);
end;


destructor TWinHTTPConnectionImpl.Destroy;
begin
  inherited Destroy;
  FSession := nil;
end;


function TWinHTTPConnectionImpl.Session : IWinHTTPSession;
begin
  result := FSession;
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
  if handle = nil then RaiseLastWinHttpError;
  inherited Create( handle);
end;


destructor TWinHTTPRequestImpl.Destroy;
begin
  inherited Destroy;
  FConnection := nil;
end;


function TWinHTTPRequestImpl.Connection : IWinHTTPConnection;
begin
  result := FConnection;
end;


function TWinHTTPRequestImpl.SetTimeouts( const aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout : Int32) : Boolean;
begin
  result := WinHttpSetTimeouts( FHandle, aResolveTimeout, aConnectTimeout, aSendTimeout, aReceiveTimeout);
end;


function TWinHTTPRequestImpl.AddRequestHeader( const aHeader : string; const addflag : DWORD) : Boolean;
begin
  result := WinHttpAddRequestHeaders( FHandle, PWideChar(aHeader), DWORD(-1), addflag);
end;


procedure TWinHTTPRequestImpl.TryAutoProxy( const aUrl : string);
// From MSDN:
//    AutoProxy support is not fully integrated into the HTTP stack in WinHTTP.
//    Before sending a request, the application must call WinHttpGetProxyForUrl
//    to obtain the name of a proxy server and then call WinHttpSetOption using
//    WINHTTP_OPTION_PROXY to set the proxy configuration on the WinHTTP request
//    handle created by WinHttpOpenRequest.
//    See https://docs.microsoft.com/en-us/windows/desktop/winhttp/winhttp-autoproxy-api
var
  options : WINHTTP_AUTOPROXY_OPTIONS;
  proxy   : WINHTTP_PROXY_INFO;
  ieProxy : WINHTTP_CURRENT_USER_IE_PROXY_CONFIG;
  dwSize  : DWORD;
begin
  // try AutoProxy via PAC first
  proxy.Initialize;
  try
    FillChar( options, SizeOf(options), 0);
    options.dwFlags                := WINHTTP_AUTOPROXY_AUTO_DETECT;
    options.dwAutoDetectFlags      := WINHTTP_AUTO_DETECT_TYPE_DHCP or WINHTTP_AUTO_DETECT_TYPE_DNS_A;
    options.fAutoLogonIfChallenged := TRUE;
    if WinHttpGetProxyForUrl( FConnection.Session.Handle, PChar(aUrl), options, proxy) then begin
      dwSize  := SizeOf(proxy);
      WinHttpSetOption( Handle, WINHTTP_OPTION_PROXY, @proxy, dwSize);
      Exit;
    end;

  finally
    proxy.FreeAllocatedResources;
  end;

  // Use IE settings as a fallback, useful in client (i.e. non-server) environments
  ieProxy.Initialize;
  try
    if WinHttpGetIEProxyConfigForCurrentUser( ieProxy)
    then begin

      // lpszAutoConfigUrl = "Use automatic proxy configuration"
      if ieProxy.lpszAutoConfigUrl <> nil then begin
        options.lpszAutoConfigUrl := ieProxy.lpszAutoConfigUrl;
        options.dwFlags := options.dwFlags or WINHTTP_AUTOPROXY_CONFIG_URL;

        proxy.Initialize;
        try
          if WinHttpGetProxyForUrl( FConnection.Session.Handle, PChar(aUrl), options, proxy) then begin
            dwSize := SizeOf(proxy);
            WinHttpSetOption( Handle, WINHTTP_OPTION_PROXY, @proxy, dwSize);
            Exit;
          end;
        finally
          proxy.FreeAllocatedResources;
        end;
      end;

      // lpszProxy = "use a proxy server"
      if ieProxy.lpszProxy <> nil then begin
        proxy.Initialize;
        try
          proxy.dwAccessType    := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
          proxy.lpszProxy       := ieProxy.lpszProxy;
          proxy.lpszProxyBypass := ieProxy.lpszProxyBypass;
          dwSize := SizeOf(proxy);
          WinHttpSetOption( Handle, WINHTTP_OPTION_PROXY, @proxy, dwSize);
          Exit;
        finally
          proxy.Initialize; // not FreeAllocatedResources, we only hold pointer copies!
        end;
      end;

    end;

  finally
    ieProxy.FreeAllocatedResources;
  end;
end;


procedure TWinHTTPRequestImpl.EnableAutomaticContentDecompression( const aEnable : Boolean);
// Enable automatic gzip,deflate decompression on systems that support this option
// From the docs: WinHTTP will automatically set an appropriate Accept-Encoding header,
// overriding any value supplied by the caller -> we don't have to do this
// Available on Win 8.1 or higher
var value : DWORD;
begin
  if aEnable
  then value := WINHTTP_DECOMPRESSION_FLAG_ALL
  else value := 0;

  // ignore returned value, the option is not supported with older WinHTTP versions
  WinHttpSetOption( Handle, WINHTTP_OPTION_DECOMPRESSION, @value, SizeOf(DWORD));
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


initialization
  OutputDebugString( PChar( SysErrorMessage( 12002)));

end.


