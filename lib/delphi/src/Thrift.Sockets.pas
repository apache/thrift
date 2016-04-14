//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Thrift.Sockets;

{$WARN SYMBOL_DEPRECATED OFF}

interface

{$IFDEF MSWINDOWS}
uses
{$IF CompilerVersion < 23.0}
  Windows, WinSock, SysUtils, Classes, Types;
{$ELSE}
  Winapi.Windows, Winapi.WinSock, System.SysUtils, System.Classes, System.Types;
{$IFEND}

type
  TSocketDomain = (pfUnspec, pfUnix, pfInet,
                pfImpLink, pfPup, pfChaos,
                pfIpx, pfNs, pfIso,
                pfOsi, pfEcma, pfDataKit,
                pfCcitt, pfSna, pfDecNet,
                pfDli, pfLat, pfHylink,
                pfAppleTalk, pfVoiceView, pfFireFox,
                pfUnknown1, pfBan, pfMax);
{$ENDIF}
{$IFDEF LINUX}
uses Libc, SysUtils, Classes;

type
  TSocketDomain = (pfUnspec, pfLocal, pfUnix,
                pfFile, pfInet, pfAx25,
                pfIpx, pfAppleTalk, pfNetRom,
                pfBridge, pfAtmPvc, pfX25,
                pfInet6, pfRose, pfDecNet,
                pfNetbeui, pfSecurity, pfKey,
                pfNetLink, pfRoute, pfPacket,
                pfAsh, pfEcoNet, pfAtmSvc,
                pfSna, pfIrda, pfMax);
{$ENDIF}

Const
  CRLF = #13#10;

type

{ TBaseSocket }

  TSocketProtocol = Word;
  TServerSocketBlockMode = (bmBlocking, bmNonBlocking, bmThreadBlocking);
  TSocketBlockMode = bmBlocking..bmNonBlocking;
  TSocketType = (stStream, stDgram, stRaw, stRdm, stSeqPacket);
  TSocketNotifyEvent = procedure (Sender: TObject) of object;
  TSocketDataEvent = procedure (Sender: TObject; Buf: PAnsiChar; var DataLen: Integer) of object;
  TSocketErrorEvent = procedure (Sender: TObject; SocketError: Integer) of object;
  ESocketError = class(Exception);

  TBaseSocket = class(TComponent)
  private
    FActive: Boolean;
    FBlockMode: TSocketBlockMode;
    FBytesReceived: Cardinal;
    FBytesSent: Cardinal;
    FDomain: TSocketDomain;
    FProtocol: TSocketProtocol;
    FSocket: TSocket;
    FSockType: TSocketType;
    FOnCreateHandle: TSocketNotifyEvent;
    FOnDestroyHandle: TSocketNotifyEvent;
    FOnError: TSocketErrorEvent;
    FOnReceive: TSocketDataEvent;
    FOnSend: TSocketDataEvent;

    procedure SetActive(Value: Boolean);
    procedure SetBlockMode(Value: TSocketBlockMode);
    procedure SetDomain(Value: TSocketDomain);
    procedure SetProtocol(Value: TSocketProtocol);
    procedure SetSockType(Value: TSocketType);

  protected
    procedure DoCreateHandle; dynamic;
    procedure DoDestroyHandle; dynamic;
    procedure DoHandleError; dynamic;
    procedure DoReceive(Buf: PAnsiChar; var DataLen: Integer); virtual;
    procedure DoSend(Buf: PAnsiChar; var DataLen: Integer); virtual;
    function ErrorCheck(rc: Integer): Integer; virtual;
    procedure Loaded; override;
    procedure SetBytesReceived(Value: Cardinal);
    procedure SetBytesSent(Value: Cardinal);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open; virtual;
    procedure Close; virtual;
    function MapDomain(sd: TSocketDomain): Integer;
    function MapSockType(st: TSocketType): Integer;
    function PeekBuf(var Buf; BufSize: Integer): Integer;
    function ReceiveBuf(var Buf; BufSize: Integer; Flags: Integer = 0): Integer;
    function Receiveln(const eol: AnsiString = CRLF): AnsiString;
    function Select(ReadReady, WriteReady, ExceptFlag: PBoolean; TimeOut: Integer = 0): Boolean;
    function SendBuf(var Buf; BufSize: Integer; Flags: Integer = 0): Integer;
    function Sendln(s: AnsiString; const eol: AnsiString = CRLF): Integer;
    function SendStream(AStream: TStream): Integer;
    function WaitForData(TimeOut: Integer = 0): Boolean;

    property Active: Boolean read FActive write SetActive default False;
    property BlockMode: TSocketBlockMode read FBlockMode write SetBlockMode default bmBlocking;
    property BytesReceived: Cardinal read FBytesReceived;
    property BytesSent: Cardinal read FBytesSent;
    property Domain: TSocketDomain read FDomain write SetDomain default pfUnspec;
    property Handle: TSocket read FSocket;
    property Protocol: TSocketProtocol read FProtocol write SetProtocol;
    property SockType: TSocketType read FSockType write SetSockType default stStream;
    property OnCreateHandle: TSocketNotifyEvent read FOnCreateHandle write FOnCreateHandle;
    property OnDestroyHandle: TSocketNotifyEvent read FOnDestroyHandle write FOnDestroyHandle;
    property OnError: TSocketErrorEvent read FOnError write FOnError;
    property OnReceive: TSocketDataEvent read FOnReceive write FOnReceive;
    property OnSend: TSocketDataEvent read FOnSend write FOnSend;
  end;

{ TIpSocket }

  TIPHeader = packed record
    iph_verlen: byte;           // Version and length
    iph_tos: byte;              // Type of service
    iph_length: word;           // Total datagram length
    iph_id: word;               // Identification
    iph_offset: word;           // Flags, fragment offset
    iph_ttl: byte;              // Time to live
    iph_protocol: byte;         // Protocol
    iph_xsum: word;             // Header checksum
    iph_src: longword;          // Source address
    iph_dest: longword;         // Destination address
  end;

  TSocketHost = type AnsiString;
  TSocketPort = type AnsiString;

{$IFDEF LINUX}
(*$HPPEMIT '#include <sys/socket.h>'*)
{$ENDIF}

  TIpSocket = class(TBaseSocket)
  private
    FLocalHost: TSocketHost;
    FLocalPort: TSocketPort;
    FRemoteHost: TSocketHost;
    FRemotePort: TSocketPort;

    procedure SetLocalHost(Value: TSocketHost);
    procedure SetLocalPort(Value: TSocketPort);
    procedure SetRemoteHost(Value: TSocketHost);
    procedure SetRemotePort(Value: TSocketPort);

  protected
    function Bind: Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    function GetSocketAddr(h: TSocketHost; p: TSocketPort): TSockAddr;
    function LookupHostName(const ipaddr: AnsiString): TSocketHost;
    function LookupHostAddr(const hn: AnsiString): TSocketHost;
    function LookupPort(const sn: AnsiString; pn: PAnsiChar = nil): word;
    function LookupProtocol(const pn: AnsiString): TSocketProtocol;
    function LocalDomainName: AnsiString;
    function LocalHostName: TSocketHost;
    function LocalHostAddr: TSocketHost;

    function ReceiveFrom(var buf; bufsize: Integer; var FromAddr: TSockAddr; var len: Integer; flags: Integer = 0): Integer;
    function SendTo(var buf; bufsize: Integer; ToAddr: TSockAddr; flags: Integer = 0): Integer;

    property LocalHost: TSocketHost read FLocalHost write SetLocalHost;
    property LocalPort: TSocketPort read FLocalPort write SetLocalPort;
    property RemoteHost: TSocketHost read FRemoteHost write SetRemoteHost;
    property RemotePort: TSocketPort read FRemotePort write SetRemotePort;
    property Domain default pfInet;
  end;

{ TCustomIpClient }

  TClientSocketThread = class;

  TCustomIpClient = class(TIpSocket)
  private
    FConnected: Boolean;
    FOnConnect: TSocketNotifyEvent;
    FOnDisconnect: TSocketNotifyEvent;

  protected
    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Open; override;
    procedure Close; override;
    function Connect: Boolean;
    procedure Disconnect;
    function GetThreadObject: TClientSocketThread;

    property Connected: Boolean read FConnected;
    property OnConnect: TSocketNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TSocketNotifyEvent read FOnDisconnect write FOnDisConnect;
  end;

{ TRawSocket }

  TRawSocket = class(TIpSocket)
  public
    constructor Create(AOwner: TComponent); override;
    property SockType default stRaw;

  end;

{ TUdpSocket }

{$IF CompilerVersion > 23.0}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$IFEND}
  TUdpSocket = class(TCustomIpClient)
  public
    constructor Create(AOwner: TComponent); override;

  published
    property Active;
    property BlockMode;
    property LocalHost;
    property LocalPort;
    property RemoteHost;
    property RemotePort;
    property OnCreateHandle;
    property OnDestroyHandle;
    property OnConnect;
    property OnDisconnect;
    property OnReceive;
    property OnSend;
    property OnError;
  end;

{ TTcpClient }

{$IF CompilerVersion > 23.0}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$IFEND}
  TTcpClient = class(TCustomIpClient)
  published
    property Active;
    property BlockMode;
    property Connected;
    property RemoteHost;
    property RemotePort;
    property OnCreateHandle;
    property OnDestroyHandle;
    property OnConnect;
    property OnDisconnect;
    property OnReceive;
    property OnSend;
    property OnError;
  end;

{ TClientSocketThread }

  TServerSocketThread = class;

  TClientSocketThread = class(TThread)
  private
    FClientSocket: TCustomIpClient;
    FServerSocketThread: TServerSocketThread;

  protected
    procedure SyncProc; virtual;

  public
    constructor Create(ServerSocketThread: TServerSocketThread);
    destructor Destroy; override;
    procedure Execute; override;
    procedure ExecuteSyncProc;
    property ClientSocket: TCustomIpClient read FClientSocket;
    property ServerSocketThread: TServerSocketThread read FServerSocketThread;
  end;

{ TServerSocketThread }

  TCustomTcpServer = class;
  TGetThreadEvent = procedure (Sender: TObject; var ClientSocketThread: TClientSocketThread) of object;

  TServerSocketThread = class(TThread)
  private
    FPoolIndex: Integer;
    FServerSocket: TCustomTcpServer;
    FThreadCacheSize: Integer;
    FThreadPool: TList;
    FOnGetThread: TGetThreadEvent;
    procedure SetThreadCacheSize(Value: Integer);

  protected
    function AddClientSocketThread: TClientSocketThread;
    function CreateThread: TClientSocketThread; virtual;
    function FetchClientSocketThread: TClientSocketThread;
    procedure RemoveClientSocketThread(ClientSocketThread: TClientSocketThread);

  public
    constructor Create(AServerSocket: TCustomTcpServer);
    destructor Destroy; override;
    procedure ClearThreadPool;
    procedure Execute; override;
    property ServerSocket: TCustomTcpServer read FServerSocket;
    property ThreadCacheSize: Integer read FThreadCacheSize write SetThreadCacheSize default 10;
    property ThreadPool: TList read FThreadPool;
    property OnGetThread: TGetThreadEvent read FOnGetThread write FOnGetThread;
  end;

{ TCustomTcpServer }

  TSocketAcceptEvent = procedure (Sender: TObject; ClientSocket: TCustomIpClient) of object;

  TCustomTcpServer = class(TIpSocket)
  private
    FServerBlockMode: TServerSocketBlockMode;
    FListening: Boolean;
    FServerSocketThread: TServerSocketThread;
{$IFDEF LINUX}
    FThreadLock: TRTLCriticalSection;
{$ENDIF}
    FOnAccept: TSocketAcceptEvent;
    FOnGetThread: TGetThreadEvent;
    FOnListening: TNotifyEvent;

    procedure GetThread(Sender: TObject; var ClientSocketThread: TClientSocketThread);
    function GetServerSocketThread: TServerSocketThread;
    procedure SetServerSocketThread(Value: TServerSocketThread);
    procedure SetServerBlockMode(Value: TServerSocketBlockMode);

  protected
    procedure DoAccept(ClientSocket: TCustomIpClient); virtual;
    function Listen(backlog: Integer = SOMAXCONN): Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open; override;
    procedure Close; override;
    function Accept: Boolean; overload;
    function Accept(var ClientSocket: TCustomIpClient): Boolean; overload;
    function WaitForConnection: Boolean;

    property BlockMode: TServerSocketBlockMode read FServerBlockMode write SetServerBlockMode default bmThreadBlocking;
    property Listening: Boolean read FListening;
    property ServerSocketThread: TServerSocketThread read GetServerSocketThread write SetServerSocketThread;
    property OnAccept: TSocketAcceptEvent read FOnAccept write FOnAccept;
    property OnGetThread: TGetThreadEvent read FOnGetThread write FOnGetThread;
    property OnListening: TNotifyEvent read FOnListening write FOnListening;
  end;

{$IF CompilerVersion > 23.0}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$IFEND}
  TTcpServer = class(TCustomTcpServer)
  published
    property Active;
    property BlockMode;
    property LocalHost;
    property LocalPort;
    property OnAccept;
    property OnGetThread;
    property OnListening;
    property OnCreateHandle;
    property OnDestroyHandle;
  end;

implementation

threadvar
  ThreadObject: TClientSocketThread;

{$IFDEF MSWINDOWS}
const
  Xlat_Domain: array[TSocketDomain] of Integer
                = (PF_UNSPEC, PF_UNIX, PF_INET,
                   PF_IMPLINK, PF_PUP, PF_CHAOS,
                   PF_IPX, PF_NS, PF_ISO,
                   PF_OSI, PF_ECMA, PF_DATAKIT,
                   PF_CCITT, PF_SNA, PF_DECnet,
                   PF_DLI, PF_LAT, PF_HYLINK,
                   PF_APPLETALK, PF_VOICEVIEW, PF_FIREFOX,
                   PF_UNKNOWN1, PF_BAN, PF_MAX);
type
  __socket_type = Integer;
{$ENDIF}

{$IFDEF LINUX}
const
  Xlat_Domain: array[TSocketDomain] of Integer
  		= (PF_UNSPEC, PF_LOCAL, PF_UNIX,
                   PF_FILE, PF_INET, PF_AX25,
                   PF_IPX, PF_APPLETALK, PF_NETROM,
                   PF_BRIDGE, PF_ATMPVC, PF_X25,
                   PF_INET6, PF_ROSE, PF_DECnet,
                   PF_NETBEUI, PF_SECURITY, PF_KEY,
                   PF_NETLINK, PF_ROUTE, PF_PACKET,
                   PF_ASH, PF_ECONET, PF_ATMSVC,
                   PF_SNA, PF_IRDA, PF_MAX);
{$ENDIF}

const
  Xlat_SocketType: array[TSocketType] of __socket_type
                = (SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_RDM, SOCK_SEQPACKET);

{ TBaseSocket }

constructor TBaseSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive  := False;
  FBlockMode := bmBlocking;
  FBytesReceived := 0;
  FBytesSent := 0;
  FDomain := pfUnspec;
  FProtocol := IPPROTO_IP;
  FSocket := INVALID_SOCKET;
  FSockType := stStream;
  FOnCreateHandle := nil;
  FOnDestroyHandle := nil;
  FOnError := nil;
  FOnReceive := nil;
  FOnSend := nil;
{$IFDEF MSWINDOWS}
  RPR;
{$ENDIF}
{$IFDEF LINUX}
  RPR;
{$ENDIF}
end;

destructor TBaseSocket.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TBaseSocket.Open;
{$IFDEF MSWINDOWS}
var
  NonBlock: Integer;
{$ENDIF}
begin
  if not FActive then
  begin
    FSocket := ErrorCheck(socket(Integer(Xlat_Domain[FDomain]), Integer(Xlat_SocketType[FSockType]), FProtocol));
    FActive := FSocket <> INVALID_SOCKET;
    if FActive then
    begin
      if FBlockMode = bmNonBlocking then
      begin
  {$IFDEF MSWINDOWS}
        NonBlock := 1;
        ErrorCheck(ioctlsocket(FSocket, FIONBIO, NonBlock));
  {$ENDIF}
  {$IFDEF LINUX}
        ErrorCheck(fcntl(FSocket, F_SETFL, O_NONBLOCK));
  {$ENDIF}
      end;
      FBytesReceived := 0;
      FBytesSent := 0;
      DoCreateHandle;
    end;
  end;
end;

procedure TBaseSocket.Close;
begin
  if FActive then
  begin
{$IFDEF MSWINDOWS}
    ErrorCheck(closesocket(FSocket));
{$ENDIF}
{$IFDEF LINUX}
    ErrorCheck(Libc.__close(FSocket));
{$ENDIF}
    FSocket := INVALID_SOCKET;
    FActive := False;
    DoDestroyHandle;
  end;
end;

function TBaseSocket.MapDomain(sd: TSocketDomain): Integer;
begin
  Result := Integer(Xlat_Domain[sd]);
end;

function TBaseSocket.MapSockType(st: TSocketType): Integer;
begin
  Result := Integer(Xlat_SocketType[st]);
end;

function TBaseSocket.PeekBuf(var Buf; BufSize: Integer): Integer;
begin
  Result := ErrorCheck(recv(FSocket, buf, bufsize, MSG_PEEK));
end;

function TBaseSocket.ReceiveBuf(var Buf; BufSize: Integer; Flags: Integer): Integer;
begin
  Result := ErrorCheck(recv(FSocket, Buf, BufSize, Flags));
  if Result <> SOCKET_ERROR then
    DoReceive(PAnsiChar(@Buf), Result);
end;

function TBaseSocket.Receiveln(const eol: AnsiString): AnsiString;
var
  len: Integer;
  buf: array[0..511] of AnsiChar;
  eolptr: PAnsiChar;
begin
  Result := '';
  eolptr := nil;
  repeat
    len := PeekBuf(buf, sizeof(buf) - 1);
    if len > 0 then
    begin
      buf[len] := #0;
      eolptr := strpos(buf, PAnsiChar(eol));
      if eolptr <> nil then
        len := eolptr - buf + length(eol);
      ReceiveBuf(buf, len);
      if eolptr <> nil then
        len := len - length(eol);
      buf[len] := #0;
      Result := Result + buf;
    end;
  until (len < 1) or (eolptr <> nil);
end;

function TBaseSocket.Select(ReadReady, WriteReady, ExceptFlag: PBoolean; TimeOut: Integer): Boolean;
var
  ReadFds: TFDset;
  ReadFdsptr: PFDset;
  WriteFds: TFDset;
  WriteFdsptr: PFDset;
  ExceptFds: TFDset;
  ExceptFdsptr: PFDset;
  tv: timeval;
  Timeptr: PTimeval;
begin
  Result := False;
  if Active then
  begin
    if Assigned(ReadReady) then
    begin
      ReadFdsptr := @ReadFds;
      FD_ZERO(ReadFds);
      FD_SET(FSocket, ReadFds);
    end
    else
      ReadFdsptr := nil;
    if Assigned(WriteReady) then
    begin
      WriteFdsptr := @WriteFds;
      FD_ZERO(WriteFds);
      FD_SET(FSocket, WriteFds);
    end
    else
      WriteFdsptr := nil;
    if Assigned(ExceptFlag) then
    begin
      ExceptFdsptr := @ExceptFds;
      FD_ZERO(ExceptFds);
      FD_SET(FSocket, ExceptFds);
    end
    else
      ExceptFdsptr := nil;
    if TimeOut >= 0 then
    begin
      tv.tv_sec := TimeOut div 1000;
      tv.tv_usec :=  1000 * (TimeOut mod 1000);
      Timeptr := @tv;
    end
    else
      Timeptr := nil;
    Try
{$IFDEF MSWINDOWS}
    {$IF CompilerVersion < 23.0}
      Result := ErrorCheck(WinSock.select(FSocket + 1, ReadFdsptr, WriteFdsptr, ExceptFdsptr, Timeptr)) > 0;
    {$ELSE}
      Result := ErrorCheck(Winapi.WinSock.select(FSocket + 1, ReadFdsptr, WriteFdsptr, ExceptFdsptr, Timeptr)) > 0;
    {$IFEND}
{$ENDIF}
{$IFDEF LINUX}
      Result := ErrorCheck(Libc.select(FSocket + 1, ReadFdsptr, WriteFdsptr, ExceptFdsptr, Timeptr)) > 0;
{$ENDIF}
    except
      Result := False;
    end;
    if Assigned(ReadReady) then
      ReadReady^ := FD_ISSET(FSocket, ReadFds);
    if Assigned(WriteReady) then
      WriteReady^ := FD_ISSET(FSocket, WriteFds);
    if Assigned(ExceptFlag) then
      ExceptFlag^ := FD_ISSET(FSocket, ExceptFds);
  end;
end;

function TBaseSocket.SendBuf(var Buf; BufSize: Integer; Flags: Integer): Integer;
begin
  DoSend(PAnsiChar(@Buf), BufSize);
  Result := ErrorCheck(Send(FSocket, Buf, BufSize, Flags));
  if Result <> SOCKET_ERROR then
    inc(FBytesSent, Result);
end;

function TBaseSocket.Sendln(s: AnsiString; const eol: AnsiString): Integer;
begin
  s := s + eol;
  Result := SendBuf(PAnsiChar(s)^, length(s), 0);
end;

function TBaseSocket.SendStream(AStream: TStream): Integer;
var
  BufLen: Integer;
  Buffer: array[0..511] of Byte;
begin
  Result := 0;
  if Assigned(AStream) then
  begin
    repeat
      BufLen := AStream.Read(Buffer, SizeOf(Buffer));
    until (BufLen = 0) or (SendBuf(Buffer, BufLen) = SOCKET_ERROR);
  end;
end;

function TBaseSocket.WaitForData(TimeOut: Integer): Boolean;
var
  ReadReady, ExceptFlag: Boolean;
  DataByte: Byte;
begin
  Result := False;
  // Select also returns True when connection is broken.
  if Select(@ReadReady, nil, @ExceptFlag, TimeOut) then
    Result := ReadReady and not ExceptFlag and
      (PeekBuf(DataByte, sizeof(DataByte)) = 1);
end;

procedure TBaseSocket.DoHandleError;
var
  SocketError: Integer;
begin
{$IFDEF MSWINDOWS}
  SocketError := WSAGetLastError;
{$ENDIF}
{$IFDEF LINUX}
  SocketError := errno;
{$ENDIF}
  if Assigned(FOnError) then
    OnError(Self, SocketError);
end;

procedure TBaseSocket.DoCreateHandle;
begin
  if FActive and Assigned(FOnCreateHandle) then
    OnCreateHandle(self);
end;

procedure TBaseSocket.DoDestroyHandle;
begin
  if Assigned(FOnDestroyHandle) then
    OnDestroyHandle(self);
end;

procedure TBaseSocket.DoReceive(Buf: PAnsiChar; var DataLen: Integer);
begin
  if Assigned(FOnReceive) then
    OnReceive(Self, Buf, DataLen);
  inc(FBytesReceived, DataLen);
end;

procedure TBaseSocket.DoSend(Buf: PAnsiChar; var DataLen: Integer);
begin
  if Assigned(FOnSend) then
    OnSend(Self, Buf, DataLen);
end;

function TBaseSocket.ErrorCheck(rc: Integer): Integer;
begin
  Result := rc;
  if rc = SOCKET_ERROR then
    DoHandleError;
end;

procedure TBaseSocket.Loaded;
begin
  inherited Loaded;
  if FActive and not (csDesigning in ComponentState) then
  begin
    FActive := False;
    Open;
  end;
end;

procedure TBaseSocket.SetBytesReceived(Value: Cardinal);
begin
  FBytesReceived := Value;
end;

procedure TBaseSocket.SetBytesSent(Value: Cardinal);
begin
  FBytesSent := Value
end;

procedure TBaseSocket.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      if Value then Open
      else Close
    else FActive := Value;
  end;
end;

procedure TBaseSocket.SetDomain(Value: TSocketDomain);
begin
  if Value <> FDomain then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      Close;
    FDomain := Value;
  end;
end;

procedure TBaseSocket.SetSockType(Value: TSocketType);
begin
  if Value <> FSockType then
  begin
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      Close;
    FSockType := Value;
  end;
end;

procedure TBaseSocket.SetProtocol(Value: TSocketProtocol);
begin
  if Value <> FProtocol then
  begin
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      Close;
    FProtocol := Value;
  end;
end;

procedure TBaseSocket.SetBlockMode(Value: TSocketBlockMode);
begin
  if Value <> FBlockMode then
  begin
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      Close;
    FBlockMode := Value;
  end;
end;

{ TIpSocket }

constructor TIpSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDomain := pfInet;
  FProtocol := IPPROTO_IP;
  FLocalHost := '';
  FLocalPort := '';
  FRemoteHost := '';
  FRemotePort := '';
end;

function TIpSocket.GetSocketAddr(h: TSocketHost; p: TSocketPort): TSockAddr;
var
  lsHostAddr: AnsiString;
begin
  Result.sin_family := AF_INET;
  lsHostAddr := AnsiString(LookupHostAddr(AnsiString(h)));
  Result.sin_addr.s_addr := inet_addr(PAnsiChar(lsHostAddr));
  Result.sin_port := htons(LookupPort(AnsiString(p)));
end;

function TIpSocket.LookupHostName(const ipaddr: AnsiString): TSocketHost;
var
  h: PHostEnt;
  addr: TSockAddr;
begin
  Result := '';
  addr.sin_addr.s_addr := inet_addr(pansichar(ipaddr));
  if addr.sin_addr.s_addr <> Integer(INADDR_NONE) then
  begin
    h := gethostbyaddr(@addr.sin_addr.s_addr, sizeof(addr), AF_INET);
    if h <> nil then
      Result := (h^.h_name);
  end;
end;

function TIpSocket.LookupHostAddr(const hn: AnsiString): TSocketHost;
var
  h: PHostEnt;
begin
  Result := '';
  if hn <> '' then
  begin
    if hn[1] in ['0'..'9'] then
    begin
      if inet_addr(pansichar(hn)) <> Integer(INADDR_NONE) then
        Result := (hn);
    end
    else
    begin
      h := gethostbyname(pansichar(hn));
      if h <> nil then
        with h^ do
        Result := AnsiString(format('%d.%d.%d.%d', [ord(h_addr^[0]), ord(h_addr^[1]),
      		  ord(h_addr^[2]), ord(h_addr^[3])]));
    end;
  end
  else Result := '0.0.0.0';
end;

function TIpSocket.LookupPort(const sn: AnsiString; pn: PAnsiChar): word;
var
  se: PServent;
begin
  Result := 0;
  if sn <> '' then
  begin
    se := getservbyname(pansichar(sn), pn);
    if se <> nil then
      Result := ntohs(se^.s_port)
    else
      Result := StrToInt(string(sn));
  end;
end;

function TIpSocket.LookupProtocol(const pn: AnsiString): TSocketProtocol;
var
  pe: PProtoent;
begin
  Result := 0;
  pe := getprotobyname(pansichar(pn));
  if pe <> nil then
    Result := pe^.p_proto;
end;

function TIpSocket.LocalDomainName: AnsiString;
var
{$IFDEF MSWINDOWS}
  dname: PAnsiChar;
{$ENDIF}
{$IFDEF LINUX}
  dname: array[0..255] of char;
{$ENDIF}
begin
  Result := '';
{$IFDEF MSWINDOWS}
  dname := strpos(PAnsiChar(LookupHostName(AnsiString(LocalHostAddr))), '.');
  if dname <> nil then
    Result := dname + 1;
{$ENDIF}
{$IFDEF LINUX}
  if ErrorCheck(getdomainname(dname, sizeof(dname))) = 0 then
    Result := dname;
{$ENDIF}
end;

function TIpSocket.LocalHostName: TSocketHost;
var
  name: array[0..255] of ansichar;
begin
  Result := '';
  if ErrorCheck(gethostname(name, sizeof(name))) = 0 then
    Result := (name);
end;

function TIpSocket.LocalHostAddr: TSocketHost;
begin
  Result := LookupHostAddr(AnsiString(LocalHostName));
end;

function TIpSocket.Bind: Boolean;
var
  addr: TSockAddr;
begin
  Result := False;
  if Active then
  begin
    addr := GetSocketAddr(FLocalHost, FLocalPort);
{$IFDEF MSWINDOWS}
  {$IF CompilerVersion < 23.0}
    Result := ErrorCheck(WinSock.bind(FSocket, addr, sizeof(addr))) = 0;
  {$ELSE}
    Result := ErrorCheck(Winapi.WinSock.bind(FSocket, addr, sizeof(addr))) = 0;
  {$IFEND}
{$ENDIF}
{$IFDEF LINUX}
    Result := ErrorCheck(Libc.bind(FSocket, addr, sizeof(addr))) = 0;
{$ENDIF}
  end;
end;

function TIpSocket.ReceiveFrom(var buf; bufsize: Integer; var FromAddr: TSockAddr; var len: Integer; flags: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  {$IF CompilerVersion < 23.0}
    Result := ErrorCheck(WinSock.recvfrom(FSocket, buf, bufsize, flags, FromAddr, len));
  {$ELSE}
    Result := ErrorCheck(Winapi.WinSock.recvfrom(FSocket, buf, bufsize, flags, FromAddr, len));
  {$IFEND}
{$ENDIF}
{$IFDEF LINUX}
  Result := ErrorCheck(Libc.recvfrom(FSocket, buf, bufsize, flags, @FromAddr, @len));
{$ENDIF}
  if Result <> SOCKET_ERROR then
    DoReceive(PAnsiChar(@Buf), Result);
end;

function TIpSocket.SendTo(var buf; bufsize: Integer; ToAddr: TSockAddr; flags: Integer): Integer;
begin
  DoSend(PAnsiChar(@Buf), BufSize);
{$IFDEF MSWINDOWS}
  {$IF CompilerVersion < 23.0}
    Result := ErrorCheck(WinSock.sendto(FSocket, buf, bufsize, flags, ToAddr, sizeof(ToAddr)));
  {$ELSE}
    Result := ErrorCheck(Winapi.WinSock.sendto(FSocket, buf, bufsize, flags, ToAddr, sizeof(ToAddr)));
  {$IFEND}
{$ENDIF}
{$IFDEF LINUX}
  Result := ErrorCheck(Libc.sendto(FSocket, buf, bufsize, flags, ToAddr, sizeof(ToAddr)));
{$ENDIF}
  if Result <> SOCKET_ERROR then
    SetBytesSent(BytesSent + Cardinal(Result));
end;

procedure TIpSocket.SetLocalHost(Value : TSocketHost);
begin
  if Value <> FLocalHost then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      Close;
    FLocalHost := Value;
  end;
end;

procedure TIpSocket.SetLocalPort(Value: TSocketPort);
begin
  if Value <> FLocalPort then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      Close;
    FLocalPort := Value;
  end;
end;

procedure TIpSocket.SetRemoteHost(Value : TSocketHost);
begin
  if Value <> FRemoteHost then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      Close;
    FRemoteHost := Value;
  end;
end;

procedure TIpSocket.SetRemotePort(Value: TSocketPort);
begin
  if Value <> FRemotePort then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      Close;
    FRemotePort := Value;
  end;
end;

{ TCustomIpClient }

constructor TCustomIpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnected := False;
  FOnConnect := nil;
  FOnDisconnect := nil;
end;

procedure TCustomIpClient.Open;
var
  addr: TSockAddr;
begin
  inherited Open;
  if Active and not FConnected then
  begin
    addr := GetSocketAddr(FRemoteHost, FRemotePort);
{$IFDEF MSWINDOWS}
  {$IF CompilerVersion < 23.0}
    FConnected := ErrorCheck(WinSock.connect(FSocket, addr, sizeof(addr))) = 0;
  {$ELSE}
    FConnected := ErrorCheck(Winapi.WinSock.connect(FSocket, addr, sizeof(addr))) = 0;
  {$IFEND}
{$ENDIF}
{$IFDEF LINUX}
    FConnected := ErrorCheck(Libc.connect(FSocket, addr, sizeof(addr))) = 0;
    if not FConnected then   // Workaround on bug in Red Hat 6.2
      Close;
{$ENDIF}
    if FConnected then
      DoConnect;
  end;
end;

procedure TCustomIpClient.Close;
begin
  if FConnected then
  begin
{$IFDEF MSWINDOWS}
    ErrorCheck(shutdown(FSocket, SD_BOTH));
{$ENDIF}
{$IFDEF LINUX}
    ErrorCheck(shutdown(FSocket, SHUT_RDWR));
{$ENDIF}
    FConnected := False;
    DoDisconnect;
  end;
  inherited Close;
end;

function TCustomIpClient.Connect: Boolean;
begin
  Open;
  Result := FConnected;
end;

procedure TCustomIpClient.Disconnect;
begin
  Close;
end;

function TCustomIpClient.GetThreadObject: TClientSocketThread;
begin
  Result := ThreadObject;
end;

procedure TCustomIpClient.DoConnect;
begin
  if Assigned(FOnConnect) then
    OnConnect(self);
end;

procedure TCustomIpClient.DoDisconnect;
begin
  if Assigned(FOnDisconnect) then
    OnDisconnect(self);
end;

{ TRawSocket }

constructor TRawSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSockType := stRaw;
  FProtocol := IPPROTO_RAW;
end;

{ TUdpSocket }

constructor TUdpSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SockType := stDgram;
  Protocol := IPPROTO_UDP;
end;

{ TClientSocketThread }

constructor TClientSocketThread.Create(ServerSocketThread: TServerSocketThread);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FServerSocketThread := ServerSocketThread;
end;

destructor TClientSocketThread.Destroy;
begin
  if Assigned(FServerSocketThread) then
    FServerSocketThread.RemoveClientSocketThread(Self);
  inherited Destroy;
end;

procedure TClientSocketThread.Execute;
begin
  ThreadObject := Self;
  while not Terminated do
  begin
    if Assigned(FServerSocketThread) and not FServerSocketThread.Terminated and
       Assigned(FServerSocketThread.ServerSocket) then
    begin
      FClientSocket := TCustomIpClient.Create(nil);
      try
        FServerSocketThread.ServerSocket.Accept(FClientSocket);
      finally
        FClientSocket.Free;
        FClientSocket := nil;
      end;
    end;
    if not Terminated then
      Suspend;
  end;
end;

procedure TClientSocketThread.ExecuteSyncProc;
begin
  Synchronize(SyncProc);
end;

procedure TClientSocketThread.SyncProc;
begin
  // override this method and put code there
  // to be executed in the main clx thread,
  // then call ExecuteSyncProc.
end;

{ TServerSocketThread }

constructor TServerSocketThread.Create(AServerSocket: TCustomTcpServer);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FServerSocket := AServerSocket;
  FThreadCacheSize := 10;
  FThreadPool := TList.Create;
end;

destructor TServerSocketThread.Destroy;
begin
  ClearThreadPool;
  while FThreadPool.Count > 0 do;
  FThreadPool.Free;
  inherited Destroy;
end;

procedure TServerSocketThread.ClearThreadPool;
var
  I: Integer;
begin
  for I := FThreadPool.Count - 1 downto 0 do
  with TClientSocketThread(FThreadPool[I]) do
  begin
    Terminate;
    if Suspended then
      Resume;
  end;
end;

procedure TServerSocketThread.Execute;
var
  T: TClientSocketThread;
begin
  while not Terminated and Assigned(FServerSocket) and FServerSocket.Listening do
  begin
    if FServerSocket.WaitForConnection then
      if not Terminated then
      begin
        T := FetchClientSocketThread;
        if not Assigned(T) then
          T := AddClientSocketThread;
        if Assigned(T) then
          T.Resume;
        Sleep(0);
      end;
  end;
end;

procedure TServerSocketThread.SetThreadCacheSize(Value: Integer);
begin
  FThreadCacheSize := Value;
end;

function TServerSocketThread.AddClientSocketThread: TClientSocketThread;
begin
  Result := nil;
  if Assigned(FServerSocket) and (FThreadPool.Count < FThreadCacheSize) then
  begin
    if Assigned(FOnGetThread) then
      FOnGetThread(Self, Result);
    if not Assigned(Result) then
      Result := CreateThread;
    if Assigned(Result) then
      FThreadPool.Add(Result);
  end
end;

function TServerSocketThread.CreateThread: TClientSocketThread;
begin
  Result := TClientSocketThread.Create(Self);
end;

function TServerSocketThread.FetchClientSocketThread: TClientSocketThread;
var
  IndexRef: Integer;
begin
  Result := nil;
  if Assigned(FServerSocket) and (FThreadPool.Count > 0) then
  begin
    IndexRef := FPoolIndex;
    repeat
      FPoolIndex := (FPoolIndex + 1) mod FThreadPool.Count;
      Result := FThreadPool[FPoolIndex];
    until (FPoolIndex = IndexRef) or Result.Suspended;
    if not Result.Suspended then
      Result := nil;
  end;
end;

procedure TServerSocketThread.RemoveClientSocketThread(ClientSocketThread: TClientSocketThread);
begin
  FThreadPool.Remove(ClientSocketThread);
end;

{ TCustomTcpServer }

constructor TCustomTcpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListening := False;
  FServerSocketThread := nil;
{$IFDEF LINUX}
  InitializeCriticalSection(FThreadLock);
{$ENDIF}
  FOnAccept := nil;
  FOnGetThread := nil;
  FOnListening := nil;
  BlockMode := bmThreadBlocking;
end;

destructor TCustomTcpServer.Destroy;
begin
{$IFDEF LINUX}
  DeleteCriticalSection(FThreadLock);
{$ENDIF}
  inherited Destroy;
end;

procedure TCustomTcpServer.Open;
begin
  inherited Open;

  if Bind then
    if Listen then
      if BlockMode = bmThreadBlocking then
      begin
        GetServerSocketThread;
        if Assigned(FServerSocketThread) and FServerSocketThread.Suspended then
          FServerSocketThread.Resume;
      end;
end;

procedure TCustomTcpServer.Close;
begin
  if (BlockMode = bmThreadBlocking) and Assigned(FServerSocketThread) then
  begin
    FServerSocketThread.Terminate;
    FServerSocketThread := nil;
{$IFDEF LINUX}
    EnterCriticalSection(FThreadLock);
    try
    finally
      LeaveCriticalSection(FThreadLock);
    end;
{$ENDIF}
  end;
  FListening := False;
  inherited Close;
end;

function TCustomTcpServer.Accept: Boolean;
var
  ClientSocket: TCustomIpClient;
begin
  ClientSocket := TCustomIpClient.Create(nil);
  try
    Result := Accept(ClientSocket);
  finally
    ClientSocket.Free;
  end;
end;

function TCustomTcpServer.Accept(var ClientSocket: TCustomIpClient): Boolean;
var
  sock: TSocket;
  addr: TSockAddr;
  len: Integer;
begin
  Result := False;
  len := sizeof(addr);
  Fillchar(addr, sizeof(addr), 0);
  try
{$IFDEF MSWINDOWS}
  {$IF CompilerVersion < 23.0}
    Sock := ErrorCheck(WinSock.accept(FSocket, @addr, @len));
  {$ELSE}
    Sock := ErrorCheck(Winapi.WinSock.accept(FSocket, @addr, @len));
  {$IFEND}
{$ENDIF}
{$IFDEF LINUX}
    Sock := ErrorCheck(Libc.accept(FSocket, @addr, @len));
{$ENDIF}
  except
    Sock := INVALID_SOCKET;
  end;
  if Sock <> INVALID_SOCKET then
  begin
    Result := True;
    ClientSocket.FActive := True;
    ClientSocket.FConnected := True;
    ClientSocket.FSocket := Sock;
    ClientSocket.FDomain := FDomain;
    ClientSocket.SockType := FSockType;
    ClientSocket.FProtocol := FProtocol;
    ClientSocket.FBlockMode := FBlockMode;
    ClientSocket.FRemoteHost := (inet_ntoa(addr.sin_addr));
    ClientSocket.FRemotePort := AnsiString(IntToStr(ntohs(addr.sin_port)));
    DoAccept(ClientSocket);
  end;
end;

procedure TCustomTcpServer.GetThread(Sender: TObject; var ClientSocketThread: TClientSocketThread);
begin
  if Assigned(FOnGetThread) then
    FOnGetThread(Self, ClientSocketThread);
end;

function TCustomTcpServer.GetServerSocketThread: TServerSocketThread;
begin
  if not Assigned(FServerSocketThread) then
    FServerSocketThread := TServerSocketThread.Create(Self);
  if Assigned(FServerSocketThread) then
    FServerSocketThread.OnGetThread := GetThread;
  Result := FServerSocketThread;
end;

procedure TCustomTcpServer.SetServerSocketThread(Value: TServerSocketThread);
begin
  if Assigned(FServerSocketThread) then
  begin
    FServerSocketThread.Terminate;
    Close;
  end;
  FServerSocketThread := Value;
end;

procedure TCustomTcpServer.DoAccept(ClientSocket: TCustomIpClient);
begin
  if Assigned(FOnAccept) then
    FOnAccept(Self, ClientSocket);
end;

function TCustomTcpServer.Listen(backlog: Integer): Boolean;
begin
  if Active and not FListening then
  begin
{$IFDEF MSWINDOWS}
  {$IF CompilerVersion < 23.0}
    FListening := ErrorCheck(WinSock.listen(FSocket, backlog)) = 0;
  {$ELSE}
    FListening := ErrorCheck(Winapi.WinSock.listen(FSocket, backlog)) = 0;
  {$IFEND}
{$ENDIF}
{$IFDEF LINUX}
    FListening := ErrorCheck(Libc.listen(FSocket, backlog)) = 0;
{$ENDIF}
  end;
  Result := FListening;
end;

procedure TCustomTcpServer.SetServerBlockMode(Value: TServerSocketBlockMode);
begin
  if Value <> FServerBlockMode then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      Close;
    FServerBlockMode := Value;
    if Value = bmThreadBlocking then
      inherited BlockMode := bmBlocking
    else
      inherited BlockMode := Value;
  end;
end;

function TCustomTcpServer.WaitForConnection: Boolean;
var
  ReadReady, ExceptFlag: Boolean;
begin
  Result := False;
{$IFDEF LINUX}
  if BlockMode = bmThreadBlocking then
  begin
    // Hack to avoid server thread block forever in linux
    EnterCriticalSection(FThreadLock);
    try
      if Select(@ReadReady, nil, @ExceptFlag, 1000) then
        Result := ReadReady and not ExceptFlag;
    finally
      LeaveCriticalSection(FThreadLock);
    end;
  end
  else
{$ENDIF}
    if Select(@ReadReady, nil, @ExceptFlag, -1) then
      Result := ReadReady and not ExceptFlag;
end;

{$IFDEF MSWINDOWS}
var
  WSAData: TWSAData;

procedure Startup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0101, WSAData);
  if ErrorCode <> 0 then
    raise ESocketError.Create('WSAStartup');
end;

procedure Cleanup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    raise ESocketError.Create('WSACleanup');
end;

initialization
  Startup;

finalization
  Cleanup;
{$ENDIF}

end.
