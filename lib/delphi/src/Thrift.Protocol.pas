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

{$SCOPEDENUMS ON}
{$IFOPT M+} {$DEFINE TYPEINFO_WAS_ON} {$ELSE} {$UNDEF TYPEINFO_WAS_ON} {$ENDIF}

unit Thrift.Protocol;

interface

uses
  Classes,
  SysUtils,
  Contnrs,
  Math,
  Thrift.Exception,
  Thrift.Stream,
  Thrift.Utils,
  Thrift.Collections,
  Thrift.Configuration,
  Thrift.Transport;

type

  TType = (
    Stop = 0,
    Void = 1,
    Bool_ = 2,
    Byte_ = 3,
    Double_ = 4,
    I16 = 6,
    I32 = 8,
    I64 = 10,
    String_ = 11,
    Struct = 12,
    Map = 13,
    Set_ = 14,
    List = 15,
    Uuid = 16
  );

  TMessageType = (
    Call = 1,
    Reply = 2,
    Exception = 3,
    Oneway = 4
  );

const
  VALID_TTYPES = [
    TType.Stop, TType.Void,
    TType.Bool_, TType.Byte_, TType.Double_, TType.I16, TType.I32, TType.I64, TType.String_, TType.Uuid,
    TType.Struct, TType.Map, TType.Set_, TType.List
  ];

  VALID_MESSAGETYPES = [Low(TMessageType)..High(TMessageType)];

type
  IProtocol = interface;

  TThriftMessage = record
    Name: string;
    Type_: TMessageType;
    SeqID: Integer;
  end;

  TThriftStruct = record
    Name: string;
  end;

  TThriftField = record
    Name: string;
    Type_: TType;
    Id: SmallInt;
  end;

  TThriftList = record
    ElementType: TType;
    Count: Integer;
  end;

  TThriftMap = record
    KeyType: TType;
    ValueType: TType;
    Count: Integer;
  end;

  TThriftSet = record
    ElementType: TType;
    Count: Integer;
  end;


  IProtocolFactory = interface
    ['{7CD64A10-4E9F-4E99-93BF-708A31F4A67B}']
    function GetProtocol( const trans: ITransport): IProtocol;
  end;

  TProtocolException = class abstract( TException)
  public
    type TExceptionType = (
      UNKNOWN = 0,
      INVALID_DATA = 1,
      NEGATIVE_SIZE = 2,
      SIZE_LIMIT = 3,
      BAD_VERSION = 4,
      NOT_IMPLEMENTED = 5,
      DEPTH_LIMIT = 6
    );
  strict protected
    constructor HiddenCreate(const Msg: string);
    class function GetType: TExceptionType;  virtual; abstract;
  public
    // purposefully hide inherited constructor
    class function Create(const Msg: string): TProtocolException; overload; deprecated 'Use specialized TProtocolException types (or regenerate from IDL)';
    class function Create: TProtocolException; overload; deprecated 'Use specialized TProtocolException types (or regenerate from IDL)';
    class function Create( aType: TExceptionType): TProtocolException; overload; deprecated 'Use specialized TProtocolException types (or regenerate from IDL)';
    class function Create( aType: TExceptionType; const msg: string): TProtocolException; overload; deprecated 'Use specialized TProtocolException types (or regenerate from IDL)';
    property Type_: TExceptionType read GetType;
  end;

  // Needed to remove deprecation warning
  TProtocolExceptionSpecialized = class abstract (TProtocolException)
  public
    constructor Create(const Msg: string);
  end;

  TProtocolExceptionUnknown = class (TProtocolExceptionSpecialized)
  strict protected
    class function GetType: TProtocolException.TExceptionType;  override;
  end;

  TProtocolExceptionInvalidData = class (TProtocolExceptionSpecialized)
  strict protected
    class function GetType: TProtocolException.TExceptionType;  override;
  end;

  TProtocolExceptionNegativeSize = class (TProtocolExceptionSpecialized)
  strict protected
    class function GetType: TProtocolException.TExceptionType;  override;
  end;

  TProtocolExceptionSizeLimit = class (TProtocolExceptionSpecialized)
  strict protected
    class function GetType: TProtocolException.TExceptionType;  override;
  end;

  TProtocolExceptionBadVersion = class (TProtocolExceptionSpecialized)
  strict protected
    class function GetType: TProtocolException.TExceptionType;  override;
  end;

  TProtocolExceptionNotImplemented = class (TProtocolExceptionSpecialized)
  strict protected
    class function GetType: TProtocolException.TExceptionType;  override;
  end;

  TProtocolExceptionDepthLimit = class (TProtocolExceptionSpecialized)
  strict protected
    class function GetType: TProtocolException.TExceptionType;  override;
  end;



  TProtocolUtil = class
  public
    class procedure Skip( prot: IProtocol; type_: TType);
  end;

  IProtocolRecursionTracker = interface
    ['{29CA033F-BB56-49B1-9EE3-31B1E82FC7A5}']
    // no members yet
  end;

  TProtocolRecursionTrackerImpl = class abstract( TInterfacedObject, IProtocolRecursionTracker)
  strict protected
    FProtocol : IProtocol;
  public
    constructor Create( prot : IProtocol);
    destructor Destroy; override;
  end;

  IThriftBytes = interface; // forward

  {$TYPEINFO ON}
  TThriftBytes = packed record  // can't use SysUtils.TBytes because it has no typinfo -> E2134
    data : System.TArray<System.Byte>;

    class operator Implicit(aRec : SysUtils.TBytes) : TThriftBytes;
    class operator Implicit(aRec : TThriftBytes) : SysUtils.TBytes;
    function Length : Integer;
  end;
  {$IFNDEF TYPEINFO_WAS_ON} {$TYPEINFO OFF} {$ENDIF}


  IProtocol = interface
    ['{6067A28E-15BF-4C9D-9A6F-D991BB3DCB85}']
    function GetTransport: ITransport;
    procedure WriteMessageBegin( const msg: TThriftMessage);
    procedure WriteMessageEnd;
    procedure WriteStructBegin( const struc: TThriftStruct);
    procedure WriteStructEnd;
    procedure WriteFieldBegin( const field: TThriftField);
    procedure WriteFieldEnd;
    procedure WriteFieldStop;
    procedure WriteMapBegin( const map: TThriftMap);
    procedure WriteMapEnd;
    procedure WriteListBegin( const list: TThriftList);
    procedure WriteListEnd();
    procedure WriteSetBegin( const set_: TThriftSet );
    procedure WriteSetEnd();
    procedure WriteBool( b: Boolean);
    procedure WriteByte( b: ShortInt);
    procedure WriteI16( i16: SmallInt);
    procedure WriteI32( i32: Integer);
    procedure WriteI64( const i64: Int64);
    procedure WriteDouble( const d: Double);
    procedure WriteString( const s: string );
    procedure WriteAnsiString( const s: AnsiString);  deprecated 'AnsiString routines are deprecated, see THRIFT-5750';
    procedure WriteBinary( const b: TBytes); overload;
    procedure WriteBinary( const b: IThriftBytes); overload;
    procedure WriteUuid( const uuid: TGuid);

    function ReadMessageBegin: TThriftMessage;
    procedure ReadMessageEnd();
    function ReadStructBegin: TThriftStruct;
    procedure ReadStructEnd;
    function ReadFieldBegin: TThriftField;
    procedure ReadFieldEnd();
    function ReadMapBegin: TThriftMap;
    procedure ReadMapEnd();
    function ReadListBegin: TThriftList;
    procedure ReadListEnd();
    function ReadSetBegin: TThriftSet;
    procedure ReadSetEnd();
    function ReadBool: Boolean;
    function ReadByte: ShortInt;
    function ReadI16: SmallInt;
    function ReadI32: Integer;
    function ReadI64: Int64;
    function ReadDouble:Double;
    function ReadBinary: TBytes;  // IMPORTANT: this is NOT safe across module boundaries
    function ReadBinaryCOM : IThriftBytes;
    function ReadUuid: TGuid;
    function ReadString: string;
    function ReadAnsiString: AnsiString;  deprecated 'AnsiString routines are deprecated, see THRIFT-5750';

    function  NextRecursionLevel : IProtocolRecursionTracker;
    procedure IncrementRecursionDepth;
    procedure DecrementRecursionDepth;
    function  GetMinSerializedSize( const aType : TType) : Integer;

    property Transport: ITransport read GetTransport;
    function Configuration : IThriftConfiguration;
  end;

  TProtocolImplClass = class of TProtocolImpl;

  TProtocolImpl = class abstract( TInterfacedObject, IProtocol)
  strict protected
    FTrans : ITransport;
    FRecursionLimit : Integer;
    FRecursionDepth : Integer;

    function  NextRecursionLevel : IProtocolRecursionTracker;
    procedure IncrementRecursionDepth;
    procedure DecrementRecursionDepth;

    function  GetMinSerializedSize( const aType : TType) : Integer;  virtual; abstract;
    procedure CheckReadBytesAvailable( const value : TThriftList);  overload; inline;
    procedure CheckReadBytesAvailable( const value : TThriftSet);  overload; inline;
    procedure CheckReadBytesAvailable( const value : TThriftMap);  overload; inline;

    procedure Reset;  virtual;
    function  GetTransport: ITransport;
    function  Configuration : IThriftConfiguration;

    procedure WriteMessageBegin( const msg: TThriftMessage); virtual; abstract;
    procedure WriteMessageEnd; virtual; abstract;
    procedure WriteStructBegin( const struc: TThriftStruct); virtual; abstract;
    procedure WriteStructEnd; virtual; abstract;
    procedure WriteFieldBegin( const field: TThriftField); virtual; abstract;
    procedure WriteFieldEnd; virtual; abstract;
    procedure WriteFieldStop; virtual; abstract;
    procedure WriteMapBegin( const map: TThriftMap); virtual; abstract;
    procedure WriteMapEnd; virtual; abstract;
    procedure WriteListBegin( const list: TThriftList); virtual; abstract;
    procedure WriteListEnd(); virtual; abstract;
    procedure WriteSetBegin( const set_: TThriftSet ); virtual; abstract;
    procedure WriteSetEnd(); virtual; abstract;
    procedure WriteBool( b: Boolean); virtual; abstract;
    procedure WriteByte( b: ShortInt); virtual; abstract;
    procedure WriteI16( i16: SmallInt); virtual; abstract;
    procedure WriteI32( i32: Integer); virtual; abstract;
    procedure WriteI64( const i64: Int64); virtual; abstract;
    procedure WriteDouble( const d: Double); virtual; abstract;
    procedure WriteString( const s: string ); virtual;
    procedure WriteBinary( const b: TBytes); overload; virtual; abstract;
    procedure WriteUuid( const b: TGuid); virtual; abstract;

    function ReadMessageBegin: TThriftMessage; virtual; abstract;
    procedure ReadMessageEnd(); virtual; abstract;
    function ReadStructBegin: TThriftStruct; virtual; abstract;
    procedure ReadStructEnd; virtual; abstract;
    function ReadFieldBegin: TThriftField; virtual; abstract;
    procedure ReadFieldEnd(); virtual; abstract;
    function ReadMapBegin: TThriftMap; virtual; abstract;
    procedure ReadMapEnd(); virtual; abstract;
    function ReadListBegin: TThriftList; virtual; abstract;
    procedure ReadListEnd(); virtual; abstract;
    function ReadSetBegin: TThriftSet; virtual; abstract;
    procedure ReadSetEnd(); virtual; abstract;
    function ReadBool: Boolean; virtual; abstract;
    function ReadByte: ShortInt; virtual; abstract;
    function ReadI16: SmallInt; virtual; abstract;
    function ReadI32: Integer; virtual; abstract;
    function ReadI64: Int64; virtual; abstract;
    function ReadDouble:Double; virtual; abstract;
    function ReadBinary: TBytes; virtual; abstract;
    function ReadUuid: TGuid; virtual; abstract;
    function ReadString: string; virtual;

    // provide generic implementation for all derived classes
    procedure WriteBinary( const bytes : IThriftBytes); overload; virtual;
    function ReadBinaryCOM : IThriftBytes;  virtual;

    property  Transport: ITransport read GetTransport;

  private
    // THRIFT-5750 unit visible, but no longer protected - awaiting final removal
    // - Note that you can implement whavetever you want in your derived class, but no longer inherit
    // - The function can still be called via IProtocol until final removal
    function ReadAnsiString: AnsiString; virtual;  //deprecated;
    procedure WriteAnsiString( const s: AnsiString); virtual; //deprecated;

  public
    constructor Create( const aTransport : ITransport); virtual;
  end;

  {.$TYPEINFO ON}  // big NO -> may cause E2134 due to Delphis stupidity on enums vs TypeInfo
  {$RTTI EXPLICIT METHODS([vcPublic, vcPublished]) PROPERTIES([vcPublic, vcPublished])}
  IBase = interface( ISupportsToString)
    ['{AFF6CECA-5200-4540-950E-9B89E0C1C00C}']
    procedure Read( const prot: IProtocol);
    procedure Write( const prot: IProtocol);
  end;

  {$TYPEINFO ON}
  {$RTTI EXPLICIT METHODS([vcPublic, vcPublished]) PROPERTIES([vcPublic, vcPublished])}
  IBaseWithTypeInfo = interface( IBase) end;

  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
  {$IFNDEF TYPEINFO_WAS_ON} {$TYPEINFO OFF} {$ENDIF}


  IThriftBytes = interface( ISupportsToString)
    ['{CDBEF7E8-BEF2-4A0A-983A-F334E3FF0016}']
    function  GetCount : Integer;
    procedure SetCount(const value : Integer);

    // WARNING: This returns a direct pointer to the underlying data structure
    function  QueryRawDataPtr : Pointer;

    property Count : Integer read GetCount write SetCount;
  end;


  TThriftBytesImpl = class( TInterfacedObject, IThriftBytes, ISupportsToString)
  strict private
    FData : TBytes;

  strict protected
    function  GetCount : Integer;
    procedure SetCount(const value : Integer);
    function  QueryRawDataPtr : Pointer;

  public
    constructor Create; overload;
    constructor Create( const bytes : TBytes); overload;
    constructor Create( var bytes : TBytes; const aTakeOwnership : Boolean = FALSE); overload;
    constructor Create( const pData : Pointer; const nCount : Integer); overload;

    function ToString : string; override;
  end;


  TBinaryProtocolImpl = class( TProtocolImpl )
  strict protected
    const
      VERSION_MASK : Cardinal = $ffff0000;
      VERSION_1 : Cardinal = $80010000;
  strict protected
    FStrictRead : Boolean;
    FStrictWrite : Boolean;
    function GetMinSerializedSize( const aType : TType) : Integer;  override;

  strict private
    function ReadAll( const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer ): Integer;  inline;
    function ReadStringBody( size: Integer): string;

  public
    type
      TFactory = class( TInterfacedObject, IProtocolFactory)
      strict protected
        FStrictRead : Boolean;
        FStrictWrite : Boolean;
        function GetProtocol( const trans: ITransport): IProtocol;
      public
        constructor Create( const aStrictRead : Boolean = FALSE; const aStrictWrite: Boolean = TRUE); reintroduce;
      end;

    constructor Create( const trans: ITransport); overload; override;
    constructor Create( const trans: ITransport; strictRead, strictWrite: Boolean); reintroduce; overload;

    procedure WriteMessageBegin( const msg: TThriftMessage); override;
    procedure WriteMessageEnd; override;
    procedure WriteStructBegin( const struc: TThriftStruct); override;
    procedure WriteStructEnd; override;
    procedure WriteFieldBegin( const field: TThriftField); override;
    procedure WriteFieldEnd; override;
    procedure WriteFieldStop; override;
    procedure WriteMapBegin( const map: TThriftMap); override;
    procedure WriteMapEnd; override;
    procedure WriteListBegin( const list: TThriftList); override;
    procedure WriteListEnd(); override;
    procedure WriteSetBegin( const set_: TThriftSet ); override;
    procedure WriteSetEnd(); override;
    procedure WriteBool( b: Boolean); override;
    procedure WriteByte( b: ShortInt); override;
    procedure WriteI16( i16: SmallInt); override;
    procedure WriteI32( i32: Integer); override;
    procedure WriteI64( const i64: Int64); override;
    procedure WriteDouble( const d: Double); override;
    procedure WriteBinary( const b: TBytes); override;
    procedure WriteBinary( const bytes : IThriftBytes); overload; override;
    procedure WriteUuid( const uuid: TGuid); override;

    function ReadMessageBegin: TThriftMessage; override;
    procedure ReadMessageEnd(); override;
    function ReadStructBegin: TThriftStruct; override;
    procedure ReadStructEnd; override;
    function ReadFieldBegin: TThriftField; override;
    procedure ReadFieldEnd(); override;
    function ReadMapBegin: TThriftMap; override;
    procedure ReadMapEnd(); override;
    function ReadListBegin: TThriftList; override;
    procedure ReadListEnd(); override;
    function ReadSetBegin: TThriftSet; override;
    procedure ReadSetEnd(); override;
    function ReadBool: Boolean; override;
    function ReadByte: ShortInt; override;
    function ReadI16: SmallInt; override;
    function ReadI32: Integer; override;
    function ReadI64: Int64; override;
    function ReadDouble:Double; override;
    function ReadBinary: TBytes; override;
    function ReadUuid: TGuid; override;

  end;


  { TProtocolDecorator forwards all requests to an enclosed TProtocol instance,
    providing a way to author concise concrete decorator subclasses. The decorator
    does not (and should not) modify the behaviour of the enclosed TProtocol

    See p.175 of Design Patterns (by Gamma et al.)
  }
  TProtocolDecorator = class( TProtocolImpl)
  strict private
    FWrappedProtocol : IProtocol;

  strict protected
    function GetMinSerializedSize( const aType : TType) : Integer;  override;

  public
    // Encloses the specified protocol.
    // All operations will be forward to the given protocol.  Must be non-null.
    constructor Create( const aProtocol : IProtocol);  reintroduce;

    procedure WriteMessageBegin( const msg: TThriftMessage); override;
    procedure WriteMessageEnd; override;
    procedure WriteStructBegin( const struc: TThriftStruct); override;
    procedure WriteStructEnd; override;
    procedure WriteFieldBegin( const field: TThriftField); override;
    procedure WriteFieldEnd; override;
    procedure WriteFieldStop; override;
    procedure WriteMapBegin( const map: TThriftMap); override;
    procedure WriteMapEnd; override;
    procedure WriteListBegin( const list: TThriftList); override;
    procedure WriteListEnd(); override;
    procedure WriteSetBegin( const set_: TThriftSet ); override;
    procedure WriteSetEnd(); override;
    procedure WriteBool( b: Boolean); override;
    procedure WriteByte( b: ShortInt); override;
    procedure WriteI16( i16: SmallInt); override;
    procedure WriteI32( i32: Integer); override;
    procedure WriteI64( const i64: Int64); override;
    procedure WriteDouble( const d: Double); override;
    procedure WriteString( const s: string ); override;
    procedure WriteBinary( const b: TBytes); override;
    procedure WriteBinary( const bytes : IThriftBytes); overload; override;
    procedure WriteUuid( const uuid: TGuid); override;

    function ReadMessageBegin: TThriftMessage; override;
    procedure ReadMessageEnd(); override;
    function ReadStructBegin: TThriftStruct; override;
    procedure ReadStructEnd; override;
    function ReadFieldBegin: TThriftField; override;
    procedure ReadFieldEnd(); override;
    function ReadMapBegin: TThriftMap; override;
    procedure ReadMapEnd(); override;
    function ReadListBegin: TThriftList; override;
    procedure ReadListEnd(); override;
    function ReadSetBegin: TThriftSet; override;
    procedure ReadSetEnd(); override;
    function ReadBool: Boolean; override;
    function ReadByte: ShortInt; override;
    function ReadI16: SmallInt; override;
    function ReadI32: Integer; override;
    function ReadI64: Int64; override;
    function ReadDouble:Double; override;
    function ReadBinary: TBytes; override;
    function ReadUuid: TGuid; override;
    function ReadString: string; override;

  private
    // THRIFT-5750 unit visible, but no longer protected - awaiting final removal
    // - Note that you can implement whavetever you want in your derived class, but no longer inherit
    // - The function can still be called via IProtocol until final removal
    {$WARN SYMBOL_DEPRECATED OFF}
    function ReadAnsiString: AnsiString; override;  deprecated;
    procedure WriteAnsiString( const s: AnsiString); override;  deprecated;
    {$WARN SYMBOL_DEPRECATED DEFAULT}
  end;


type
  IRequestEvents = interface
    ['{F926A26A-5B00-4560-86FA-2CAE3BA73DAF}']
    // Called before reading arguments.
    procedure PreRead;
    // Called between reading arguments and calling the handler.
    procedure PostRead;
    // Called between calling the handler and writing the response.
    procedure PreWrite;
    // Called after writing the response.
    procedure PostWrite;
    // Called when an oneway (async) function call completes successfully.
    procedure OnewayComplete;
    // Called if the handler throws an undeclared exception.
    procedure UnhandledError( const e : Exception);
    // Called when a client has finished request-handling to clean up
    procedure CleanupContext;
  end;


  IProcessorEvents = interface
    ['{A8661119-657C-447D-93C5-512E36162A45}']
    // Called when a client is about to call the processor.
    procedure Processing( const transport : ITransport);
    // Called on any service function invocation
    function  CreateRequestContext( const aFunctionName : string) : IRequestEvents;
    // Called when a client has finished request-handling to clean up
    procedure CleanupContext;
  end;


  IProcessor = interface
    ['{7BAE92A5-46DA-4F13-B6EA-0EABE233EE5F}']
    function Process( const iprot :IProtocol; const oprot: IProtocol; const events : IProcessorEvents = nil): Boolean;
  end;


procedure Init( var rec : TThriftMessage; const AName: string = ''; const AMessageType: TMessageType = Low(TMessageType); const ASeqID: Integer = 0);  overload;  inline;
procedure Init( var rec : TThriftStruct;  const AName: string = '');  overload;  inline;
procedure Init( var rec : TThriftField;   const AName: string = ''; const AType: TType = Low(TType); const AID: SmallInt = 0);  overload;  inline;
procedure Init( var rec : TThriftMap;     const AKeyType: TType = Low(TType); const AValueType: TType = Low(TType); const ACount: Integer = 0); overload;  inline;
procedure Init( var rec : TThriftSet;     const AElementType: TType = Low(TType); const ACount: Integer = 0); overload;  inline;
procedure Init( var rec : TThriftList;    const AElementType: TType = Low(TType); const ACount: Integer = 0); overload;  inline;


implementation

function ConvertInt64ToDouble( const n: Int64): Double;  inline;
begin
  ASSERT( SizeOf(n) = SizeOf(Result));
  System.Move( n, Result, SizeOf(Result));
end;

function ConvertDoubleToInt64( const d: Double): Int64;  inline;
begin
  ASSERT( SizeOf(d) = SizeOf(Result));
  System.Move( d, Result, SizeOf(Result));
end;


//--- TThriftBytes ----------------------------------------------------------------------


class operator TThriftBytes.Implicit(aRec : SysUtils.TBytes) : TThriftBytes;
begin
  ASSERT( @result.data = @result);         // must be first field
  ASSERT( SizeOf(aRec) = SizeOf(result));  // must be the only field
  result := TThriftBytes(aRec);
end;


class operator TThriftBytes.Implicit(aRec : TThriftBytes) : SysUtils.TBytes;
begin
  ASSERT( @aRec.data = @aRec);             // must be first field
  ASSERT( SizeOf(aRec) = SizeOf(result));  // must be the only field
  result := SysUtils.TBytes(aRec.data);
end;


function TThriftBytes.Length : Integer;
begin
  result := System.Length(data);
end;


{ TProtocolRecursionTrackerImpl }

constructor TProtocolRecursionTrackerImpl.Create( prot : IProtocol);
begin
  inherited Create;

  // storing the pointer *after* the (successful) increment is important here
  prot.IncrementRecursionDepth;
  FProtocol := prot;
end;

destructor TProtocolRecursionTrackerImpl.Destroy;
begin
  try
    // we have to release the reference iff the pointer has been stored
    if FProtocol <> nil then begin
      FProtocol.DecrementRecursionDepth;
      FProtocol := nil;
    end;
  finally
    inherited Destroy;
  end;
end;

{ TProtocolImpl }

constructor TProtocolImpl.Create( const aTransport : ITransport);
begin
  inherited Create;
  FTrans := aTransport;
  FRecursionLimit := aTransport.Configuration.RecursionLimit;
  FRecursionDepth := 0;
end;

function TProtocolImpl.NextRecursionLevel : IProtocolRecursionTracker;
begin
  result := TProtocolRecursionTrackerImpl.Create(Self);
end;

procedure TProtocolImpl.IncrementRecursionDepth;
begin
  if FRecursionDepth < FRecursionLimit
  then Inc(FRecursionDepth)
  else raise TProtocolExceptionDepthLimit.Create('Depth limit exceeded');
end;

procedure TProtocolImpl.DecrementRecursionDepth;
begin
  Dec(FRecursionDepth)
end;

function TProtocolImpl.GetTransport: ITransport;
begin
  Result := FTrans;
end;

function TProtocolImpl.Configuration : IThriftConfiguration;
begin
  Result := FTrans.Configuration;
end;

procedure TProtocolImpl.Reset;
begin
  FTrans.ResetConsumedMessageSize;
end;

function TProtocolImpl.ReadAnsiString: AnsiString;
var
  b : TBytes;
  len : Integer;
begin
  Result := '';
  b := ReadBinary;
  len := Length( b );
  if len > 0 then begin
    SetLength( Result, len);
    System.Move( b[0], Pointer(Result)^, len );
  end;
end;

function TProtocolImpl.ReadString: string;
begin
  Result := TEncoding.UTF8.GetString( ReadBinary );
end;

procedure TProtocolImpl.WriteAnsiString(const s: AnsiString);
var
  b : TBytes;
  len : Integer;
begin
  len := Length(s);
  SetLength( b, len);
  if len > 0 then begin
    System.Move( Pointer(s)^, b[0], len );
  end;
  WriteBinary( b );
end;

procedure TProtocolImpl.WriteString(const s: string);
var
  b : TBytes;
begin
  b := TEncoding.UTF8.GetBytes(s);
  WriteBinary( b );
end;


procedure TProtocolImpl.CheckReadBytesAvailable( const value : TThriftList);
begin
  FTrans.CheckReadBytesAvailable( value.Count * GetMinSerializedSize(value.ElementType));
end;


procedure TProtocolImpl.CheckReadBytesAvailable( const value : TThriftSet);
begin
  FTrans.CheckReadBytesAvailable( value.Count * GetMinSerializedSize(value.ElementType));
end;


procedure TProtocolImpl.CheckReadBytesAvailable( const value : TThriftMap);
var nPairSize : Integer;
begin
  nPairSize := GetMinSerializedSize(value.KeyType) + GetMinSerializedSize(value.ValueType);
  FTrans.CheckReadBytesAvailable( value.Count * nPairSize);
end;


procedure TProtocolImpl.WriteBinary( const bytes : IThriftBytes);
// This implementation works, but is rather inefficient due to the extra memory allocation
// Consider overwriting this for your transport implementation
var tmp : TBytes;
begin
  SetLength( tmp, bytes.Count);
  if Length(tmp) > 0
  then Move( bytes.QueryRawDataPtr^, tmp[0], Length(tmp));
  WriteBinary( tmp);
end;


function TProtocolImpl.ReadBinaryCOM : IThriftBytes;
var bytes : TBytes;
begin
  bytes := ReadBinary;
  result := TThriftBytesImpl.Create(bytes,TRUE);
end;


{ TThriftBytesImpl }

constructor TThriftBytesImpl.Create;
begin
  inherited Create;
  ASSERT( Length(FData) = 0);
end;


constructor TThriftBytesImpl.Create( const bytes : TBytes);
begin
  FData := bytes; // copies the data
end;


constructor TThriftBytesImpl.Create( var bytes : TBytes; const aTakeOwnership : Boolean);

  procedure SwapPointer( var one, two);
  var
    pOne : Pointer absolute one;
    pTwo : Pointer absolute two;
    pTmp : Pointer;
  begin
    pTmp := pOne;
    pOne := pTwo;
    pTwo := pTmp;
  end;

begin
  inherited Create;
  ASSERT( Length(FData) = 0);

  if aTakeOwnership
  then SwapPointer( FData, bytes)
  else FData := bytes; // copies the data
end;


constructor TThriftBytesImpl.Create( const pData : Pointer; const nCount : Integer);
begin
  SetLength(FData, Max(nCount,0));
  if Length(FData) > 0 then Move( pData^, FData[0], Length(FData));
end;


function TThriftBytesImpl.ToString : string;
var sb : TThriftStringBuilder;
begin
  sb := TThriftStringBuilder.Create();
  try
    sb.Append('Bin: ');
    sb.Append( FData);

    result := sb.ToString;
  finally
    sb.Free;
  end;
end;


function TThriftBytesImpl.GetCount : Integer;
begin
  result := Length(FData);
end;


procedure TThriftBytesImpl.SetCount(const value : Integer);
begin
  SetLength( FData, value);
end;


function TThriftBytesImpl.QueryRawDataPtr : Pointer;
begin
  result := FData;
end;

{ TProtocolUtil }

class procedure TProtocolUtil.Skip( prot: IProtocol; type_: TType);
var field : TThriftField;
    map   : TThriftMap;
    set_  : TThriftSet;
    list  : TThriftList;
    i     : Integer;
    tracker : IProtocolRecursionTracker;
begin
  tracker := prot.NextRecursionLevel;
  case type_ of
    // simple types
    TType.Bool_   :  prot.ReadBool();
    TType.Byte_   :  prot.ReadByte();
    TType.I16     :  prot.ReadI16();
    TType.I32     :  prot.ReadI32();
    TType.I64     :  prot.ReadI64();
    TType.Double_ :  prot.ReadDouble();
    TType.String_ :  prot.ReadBinary();// Don't try to decode the string, just skip it.
    TType.Uuid    :  prot.ReadUuid();

    // structured types
    TType.Struct :  begin
      prot.ReadStructBegin();
      while TRUE do begin
        field := prot.ReadFieldBegin();
        if (field.Type_ = TType.Stop) then Break;
        Skip(prot, field.Type_);
        prot.ReadFieldEnd();
      end;
      prot.ReadStructEnd();
    end;

    TType.Map :  begin
      map := prot.ReadMapBegin();
      for i := 0 to map.Count-1 do begin
        Skip(prot, map.KeyType);
        Skip(prot, map.ValueType);
      end;
      prot.ReadMapEnd();
    end;

    TType.Set_ :  begin
      set_ := prot.ReadSetBegin();
      for i := 0 to set_.Count-1
      do Skip( prot, set_.ElementType);
      prot.ReadSetEnd();
    end;

    TType.List :  begin
      list := prot.ReadListBegin();
      for i := 0 to list.Count-1
      do Skip( prot, list.ElementType);
      prot.ReadListEnd();
    end;

  else
    raise TProtocolExceptionInvalidData.Create('Unexpected type '+IntToStr(Ord(type_)));
  end;
end;


{ TBinaryProtocolImpl }

constructor TBinaryProtocolImpl.Create( const trans: ITransport);
begin
  // call the real CTOR
  Self.Create( trans, FALSE, TRUE);
end;

constructor TBinaryProtocolImpl.Create( const trans: ITransport; strictRead, strictWrite: Boolean);
begin
  inherited Create( trans);
  FStrictRead := strictRead;
  FStrictWrite := strictWrite;
end;

function TBinaryProtocolImpl.ReadAll( const pBuf : Pointer; const buflen : Integer; off: Integer; len: Integer ): Integer;
begin
  Result := FTrans.ReadAll( pBuf, buflen, off, len );
end;

function TBinaryProtocolImpl.ReadBinary: TBytes;
var
  size : Integer;
  buf : TBytes;
begin
  size := ReadI32;
  FTrans.CheckReadBytesAvailable( size);
  SetLength( buf, size);
  FTrans.ReadAll( buf, 0, size);
  Result := buf;
end;

function TBinaryProtocolImpl.ReadUuid : TGuid;
var network : TGuid;  // in network order (Big Endian)
begin
  ASSERT( SizeOf(result) = 16);
  FTrans.ReadAll( @network, SizeOf(network), 0, SizeOf(network));
  result := GuidUtils.SwapByteOrder(network);
end;

function TBinaryProtocolImpl.ReadBool: Boolean;
begin
  Result := (ReadByte = 1);
end;

function TBinaryProtocolImpl.ReadByte: ShortInt;
begin
  ReadAll( @result, SizeOf(result), 0, 1);
end;

function TBinaryProtocolImpl.ReadDouble: Double;
begin
  Result := ConvertInt64ToDouble( ReadI64 )
end;

function TBinaryProtocolImpl.ReadFieldBegin: TThriftField;
begin
  Init( result, '', TType( ReadByte), 0);
  if ( result.Type_ <> TType.Stop ) then begin
    result.Id := ReadI16;
  end;
end;

procedure TBinaryProtocolImpl.ReadFieldEnd;
begin

end;

function TBinaryProtocolImpl.ReadI16: SmallInt;
var i16in : packed array[0..1] of Byte;
begin
  ReadAll( @i16in, Sizeof(i16in), 0, 2);
  Result := SmallInt(((i16in[0] and $FF) shl 8) or (i16in[1] and $FF));
end;

function TBinaryProtocolImpl.ReadI32: Integer;
var i32in : packed array[0..3] of Byte;
begin
  ReadAll( @i32in, SizeOf(i32in), 0, 4);

  Result := Integer(
    ((i32in[0] and $FF) shl 24) or
    ((i32in[1] and $FF) shl 16) or
    ((i32in[2] and $FF) shl 8) or
     (i32in[3] and $FF));

end;

function TBinaryProtocolImpl.ReadI64: Int64;
var i64in : packed array[0..7] of Byte;
begin
  ReadAll( @i64in, SizeOf(i64in), 0, 8);
  Result :=
    (Int64( i64in[0] and $FF) shl 56) or
    (Int64( i64in[1] and $FF) shl 48) or
    (Int64( i64in[2] and $FF) shl 40) or
    (Int64( i64in[3] and $FF) shl 32) or
    (Int64( i64in[4] and $FF) shl 24) or
    (Int64( i64in[5] and $FF) shl 16) or
    (Int64( i64in[6] and $FF) shl 8) or
    (Int64( i64in[7] and $FF));
end;

function TBinaryProtocolImpl.ReadListBegin: TThriftList;
begin
  result.ElementType := TType(ReadByte);
  result.Count       := ReadI32;
  CheckReadBytesAvailable(result);
end;

procedure TBinaryProtocolImpl.ReadListEnd;
begin

end;

function TBinaryProtocolImpl.ReadMapBegin: TThriftMap;
begin
  result.KeyType   := TType(ReadByte);
  result.ValueType := TType(ReadByte);
  result.Count     := ReadI32;
  CheckReadBytesAvailable(result);
end;

procedure TBinaryProtocolImpl.ReadMapEnd;
begin

end;

function TBinaryProtocolImpl.ReadMessageBegin: TThriftMessage;
var
  size : Integer;
  version : Integer;
begin
  Reset;
  Init( result);

  size := ReadI32;
  if (size < 0) then begin
    version := size and Integer( VERSION_MASK);
    if ( version <> Integer( VERSION_1)) then begin
      raise TProtocolExceptionBadVersion.Create('Bad version in ReadMessageBegin: ' + IntToStr(version) );
    end;
    result.Type_ := TMessageType( size and $000000ff);
    result.Name := ReadString;
    result.SeqID := ReadI32;
    Exit;
  end;

  try
    if FStrictRead
    then raise TProtocolExceptionBadVersion.Create('Missing version in readMessageBegin, old client?' );

    result.Name := ReadStringBody( size );
    result.Type_ := TMessageType( ReadByte );
    result.SeqID := ReadI32;
  except
    if CharUtils.IsHtmlDoctype(size)
    then raise TProtocolExceptionInvalidData.Create('Remote end sends HTML instead of data')
    else raise; // something else
  end;
end;

procedure TBinaryProtocolImpl.ReadMessageEnd;
begin
  inherited;

end;

function TBinaryProtocolImpl.ReadSetBegin: TThriftSet;
begin
  result.ElementType := TType(ReadByte);
  result.Count       := ReadI32;
  CheckReadBytesAvailable(result);
end;

procedure TBinaryProtocolImpl.ReadSetEnd;
begin

end;

function TBinaryProtocolImpl.ReadStringBody( size: Integer): string;
var buf : TBytes;
begin
  FTrans.CheckReadBytesAvailable( size);
  SetLength( buf, size);
  FTrans.ReadAll( buf, 0, size );
  Result := TEncoding.UTF8.GetString( buf);
end;

function TBinaryProtocolImpl.ReadStructBegin: TThriftStruct;
begin
  Init( Result);
end;

procedure TBinaryProtocolImpl.ReadStructEnd;
begin
  inherited;

end;

procedure TBinaryProtocolImpl.WriteBinary( const b: TBytes);
var iLen : Integer;
begin
  iLen := Length(b);
  WriteI32( iLen);
  if iLen > 0 then FTrans.Write(b, 0, iLen);
end;

procedure TBinaryProtocolImpl.WriteBinary( const bytes : IThriftBytes);
var iLen : Integer;
begin
  iLen := bytes.Count;
  WriteI32( iLen);
  if iLen > 0 then FTrans.Write( bytes.QueryRawDataPtr, 0, iLen);
end;

procedure TBinaryProtocolImpl.WriteUuid( const uuid: TGuid);
var network : TGuid;  // in network order (Big Endian)
begin
  ASSERT( SizeOf(uuid) = 16);
  network := GuidUtils.SwapByteOrder(uuid);
  Transport.Write( @network, 0, SizeOf(network));
end;

procedure TBinaryProtocolImpl.WriteBool(b: Boolean);
begin
  if b then begin
    WriteByte( 1 );
  end else begin
    WriteByte( 0 );
  end;
end;

procedure TBinaryProtocolImpl.WriteByte(b: ShortInt);
begin
  FTrans.Write( @b, 0, 1);
end;

procedure TBinaryProtocolImpl.WriteDouble( const d: Double);
begin
  WriteI64(ConvertDoubleToInt64(d));
end;

procedure TBinaryProtocolImpl.WriteFieldBegin( const field: TThriftField);
begin
  WriteByte(ShortInt(field.Type_));
  WriteI16(field.ID);
end;

procedure TBinaryProtocolImpl.WriteFieldEnd;
begin

end;

procedure TBinaryProtocolImpl.WriteFieldStop;
begin
  WriteByte(ShortInt(TType.Stop));
end;

procedure TBinaryProtocolImpl.WriteI16(i16: SmallInt);
var i16out : packed array[0..1] of Byte;
begin
  i16out[0] := Byte($FF and (i16 shr 8));
  i16out[1] := Byte($FF and i16);
  FTrans.Write( @i16out, 0, 2);
end;

procedure TBinaryProtocolImpl.WriteI32(i32: Integer);
var i32out : packed array[0..3] of Byte;
begin
  i32out[0] := Byte($FF and (i32 shr 24));
  i32out[1] := Byte($FF and (i32 shr 16));
  i32out[2] := Byte($FF and (i32 shr 8));
  i32out[3] := Byte($FF and i32);
  FTrans.Write( @i32out, 0, 4);
end;

procedure TBinaryProtocolImpl.WriteI64( const i64: Int64);
var i64out : packed array[0..7] of Byte;
begin
  i64out[0] := Byte($FF and (i64 shr 56));
  i64out[1] := Byte($FF and (i64 shr 48));
  i64out[2] := Byte($FF and (i64 shr 40));
  i64out[3] := Byte($FF and (i64 shr 32));
  i64out[4] := Byte($FF and (i64 shr 24));
  i64out[5] := Byte($FF and (i64 shr 16));
  i64out[6] := Byte($FF and (i64 shr 8));
  i64out[7] := Byte($FF and i64);
  FTrans.Write( @i64out, 0, 8);
end;

procedure TBinaryProtocolImpl.WriteListBegin( const list: TThriftList);
begin
  WriteByte(ShortInt(list.ElementType));
  WriteI32(list.Count);
end;

procedure TBinaryProtocolImpl.WriteListEnd;
begin

end;

procedure TBinaryProtocolImpl.WriteMapBegin( const map: TThriftMap);
begin
  WriteByte(ShortInt(map.KeyType));
  WriteByte(ShortInt(map.ValueType));
  WriteI32(map.Count);
end;

procedure TBinaryProtocolImpl.WriteMapEnd;
begin

end;

procedure TBinaryProtocolImpl.WriteMessageBegin( const msg: TThriftMessage);
var version : Cardinal;
begin
  Reset;
  if FStrictWrite then begin
    version := VERSION_1 or Cardinal( msg.Type_);
    WriteI32( Integer( version) );
    WriteString( msg.Name);
    WriteI32( msg.SeqID);
  end else begin
    WriteString( msg.Name);
    WriteByte(ShortInt( msg.Type_));
    WriteI32( msg.SeqID);
  end;
end;

procedure TBinaryProtocolImpl.WriteMessageEnd;
begin

end;

procedure TBinaryProtocolImpl.WriteSetBegin( const set_: TThriftSet);
begin
  WriteByte(ShortInt(set_.ElementType));
  WriteI32(set_.Count);
end;

procedure TBinaryProtocolImpl.WriteSetEnd;
begin

end;

procedure TBinaryProtocolImpl.WriteStructBegin( const struc: TThriftStruct);
begin

end;

procedure TBinaryProtocolImpl.WriteStructEnd;
begin

end;

function TBinaryProtocolImpl.GetMinSerializedSize( const aType : TType) : Integer;
// Return the minimum number of bytes a type will consume on the wire
begin
  case aType of
    TType.Stop:    result := 0;
    TType.Void:    result := 0;
    TType.Bool_:   result := SizeOf(Byte);
    TType.Byte_:   result := SizeOf(Byte);
    TType.Double_: result := SizeOf(Double);
    TType.I16:     result := SizeOf(Int16);
    TType.I32:     result := SizeOf(Int32);
    TType.I64:     result := SizeOf(Int64);
    TType.String_: result := SizeOf(Int32);  // string length
    TType.Struct:  result := 0;  // empty struct
    TType.Map:     result := SizeOf(Int32);  // element count
    TType.Set_:    result := SizeOf(Int32);  // element count
    TType.List:    result := SizeOf(Int32);  // element count
    TType.Uuid:    result := SizeOf(TGuid);
  else
    raise TTransportExceptionBadArgs.Create('Unhandled type code');
  end;
end;


{ TProtocolException }

constructor TProtocolException.HiddenCreate(const Msg: string);
begin
  inherited Create(Msg);
end;

class function TProtocolException.Create(const Msg: string): TProtocolException;
begin
  Result := TProtocolExceptionUnknown.Create(Msg);
end;

class function TProtocolException.Create: TProtocolException;
begin
  Result := TProtocolExceptionUnknown.Create('');
end;

class function TProtocolException.Create(aType: TExceptionType): TProtocolException;
begin
{$WARN SYMBOL_DEPRECATED OFF}
  Result := Create(aType, '');
{$WARN SYMBOL_DEPRECATED DEFAULT}
end;

class function TProtocolException.Create(aType: TExceptionType; const msg: string): TProtocolException;
begin
  case aType of
    TExceptionType.INVALID_DATA:    Result := TProtocolExceptionInvalidData.Create(msg);
    TExceptionType.NEGATIVE_SIZE:   Result := TProtocolExceptionNegativeSize.Create(msg);
    TExceptionType.SIZE_LIMIT:      Result := TProtocolExceptionSizeLimit.Create(msg);
    TExceptionType.BAD_VERSION:     Result := TProtocolExceptionBadVersion.Create(msg);
    TExceptionType.NOT_IMPLEMENTED: Result := TProtocolExceptionNotImplemented.Create(msg);
    TExceptionType.DEPTH_LIMIT:     Result := TProtocolExceptionDepthLimit.Create(msg);
  else
    ASSERT( TExceptionType.UNKNOWN = aType);
    Result := TProtocolExceptionUnknown.Create(msg);
  end;
end;

{ TProtocolExceptionSpecialized }

constructor TProtocolExceptionSpecialized.Create(const Msg: string);
begin
  inherited HiddenCreate(Msg);
end;

{ specialized TProtocolExceptions }

class function TProtocolExceptionUnknown.GetType: TProtocolException.TExceptionType;
begin
  result := TExceptionType.UNKNOWN;
end;

class function TProtocolExceptionInvalidData.GetType: TProtocolException.TExceptionType;
begin
  result := TExceptionType.INVALID_DATA;
end;

class function TProtocolExceptionNegativeSize.GetType: TProtocolException.TExceptionType;
begin
  result := TExceptionType.NEGATIVE_SIZE;
end;

class function TProtocolExceptionSizeLimit.GetType: TProtocolException.TExceptionType;
begin
  result := TExceptionType.SIZE_LIMIT;
end;

class function TProtocolExceptionBadVersion.GetType: TProtocolException.TExceptionType;
begin
  result := TExceptionType.BAD_VERSION;
end;

class function TProtocolExceptionNotImplemented.GetType: TProtocolException.TExceptionType;
begin
  result := TExceptionType.NOT_IMPLEMENTED;
end;

class function TProtocolExceptionDepthLimit.GetType: TProtocolException.TExceptionType;
begin
  result := TExceptionType.DEPTH_LIMIT;
end;

{ TBinaryProtocolImpl.TFactory }

constructor TBinaryProtocolImpl.TFactory.Create( const aStrictRead, aStrictWrite: Boolean);
begin
  inherited Create;
  FStrictRead := AStrictRead;
  FStrictWrite := AStrictWrite;
end;

function TBinaryProtocolImpl.TFactory.GetProtocol( const trans: ITransport): IProtocol;
begin
  Result := TBinaryProtocolImpl.Create( trans, FStrictRead, FStrictWrite);
end;


{ TProtocolDecorator }

constructor TProtocolDecorator.Create( const aProtocol : IProtocol);
begin
  ASSERT( aProtocol <> nil);
  inherited Create( aProtocol.Transport);
  FWrappedProtocol := aProtocol;
end;


procedure TProtocolDecorator.WriteMessageBegin( const msg: TThriftMessage);
begin
  FWrappedProtocol.WriteMessageBegin( msg);
end;


procedure TProtocolDecorator.WriteMessageEnd;
begin
  FWrappedProtocol.WriteMessageEnd;
end;


procedure TProtocolDecorator.WriteStructBegin( const struc: TThriftStruct);
begin
  FWrappedProtocol.WriteStructBegin( struc);
end;


procedure TProtocolDecorator.WriteStructEnd;
begin
  FWrappedProtocol.WriteStructEnd;
end;


procedure TProtocolDecorator.WriteFieldBegin( const field: TThriftField);
begin
  FWrappedProtocol.WriteFieldBegin( field);
end;


procedure TProtocolDecorator.WriteFieldEnd;
begin
  FWrappedProtocol.WriteFieldEnd;
end;


procedure TProtocolDecorator.WriteFieldStop;
begin
  FWrappedProtocol.WriteFieldStop;
end;


procedure TProtocolDecorator.WriteMapBegin( const map: TThriftMap);
begin
  FWrappedProtocol.WriteMapBegin( map);
end;


procedure TProtocolDecorator.WriteMapEnd;
begin
  FWrappedProtocol.WriteMapEnd;
end;


procedure TProtocolDecorator.WriteListBegin( const list: TThriftList);
begin
  FWrappedProtocol.WriteListBegin( list);
end;


procedure TProtocolDecorator.WriteListEnd();
begin
  FWrappedProtocol.WriteListEnd();
end;


procedure TProtocolDecorator.WriteSetBegin( const set_: TThriftSet );
begin
  FWrappedProtocol.WriteSetBegin( set_);
end;


procedure TProtocolDecorator.WriteSetEnd();
begin
  FWrappedProtocol.WriteSetEnd();
end;


procedure TProtocolDecorator.WriteBool( b: Boolean);
begin
  FWrappedProtocol.WriteBool( b);
end;


procedure TProtocolDecorator.WriteByte( b: ShortInt);
begin
  FWrappedProtocol.WriteByte( b);
end;


procedure TProtocolDecorator.WriteI16( i16: SmallInt);
begin
  FWrappedProtocol.WriteI16( i16);
end;


procedure TProtocolDecorator.WriteI32( i32: Integer);
begin
  FWrappedProtocol.WriteI32( i32);
end;


procedure TProtocolDecorator.WriteI64( const i64: Int64);
begin
  FWrappedProtocol.WriteI64( i64);
end;


procedure TProtocolDecorator.WriteDouble( const d: Double);
begin
  FWrappedProtocol.WriteDouble( d);
end;


procedure TProtocolDecorator.WriteString( const s: string );
begin
  FWrappedProtocol.WriteString( s);
end;


procedure TProtocolDecorator.WriteAnsiString( const s: AnsiString);
begin
  {$WARN SYMBOL_DEPRECATED OFF}
  FWrappedProtocol.WriteAnsiString( s);
  {$WARN SYMBOL_DEPRECATED DEFAULT}
end;


procedure TProtocolDecorator.WriteBinary( const b: TBytes);
begin
  FWrappedProtocol.WriteBinary( b);
end;


procedure TProtocolDecorator.WriteBinary( const bytes : IThriftBytes);
begin
  FWrappedProtocol.WriteBinary( bytes);
end;


procedure TProtocolDecorator.WriteUuid( const uuid: TGuid);
begin
  FWrappedProtocol.WriteUuid( uuid);
end;


function TProtocolDecorator.ReadMessageBegin: TThriftMessage;
begin
  result := FWrappedProtocol.ReadMessageBegin;
end;


procedure TProtocolDecorator.ReadMessageEnd();
begin
  FWrappedProtocol.ReadMessageEnd();
end;


function TProtocolDecorator.ReadStructBegin: TThriftStruct;
begin
  result := FWrappedProtocol.ReadStructBegin;
end;


procedure TProtocolDecorator.ReadStructEnd;
begin
  FWrappedProtocol.ReadStructEnd;
end;


function TProtocolDecorator.ReadFieldBegin: TThriftField;
begin
  result := FWrappedProtocol.ReadFieldBegin;
end;


procedure TProtocolDecorator.ReadFieldEnd();
begin
  FWrappedProtocol.ReadFieldEnd();
end;


function TProtocolDecorator.ReadMapBegin: TThriftMap;
begin
  result := FWrappedProtocol.ReadMapBegin;
end;


procedure TProtocolDecorator.ReadMapEnd();
begin
  FWrappedProtocol.ReadMapEnd();
end;


function TProtocolDecorator.ReadListBegin: TThriftList;
begin
  result := FWrappedProtocol.ReadListBegin;
end;


procedure TProtocolDecorator.ReadListEnd();
begin
  FWrappedProtocol.ReadListEnd();
end;


function TProtocolDecorator.ReadSetBegin: TThriftSet;
begin
  result := FWrappedProtocol.ReadSetBegin;
end;


procedure TProtocolDecorator.ReadSetEnd();
begin
  FWrappedProtocol.ReadSetEnd();
end;


function TProtocolDecorator.ReadBool: Boolean;
begin
  result := FWrappedProtocol.ReadBool;
end;


function TProtocolDecorator.ReadByte: ShortInt;
begin
  result := FWrappedProtocol.ReadByte;
end;


function TProtocolDecorator.ReadI16: SmallInt;
begin
  result := FWrappedProtocol.ReadI16;
end;


function TProtocolDecorator.ReadI32: Integer;
begin
  result := FWrappedProtocol.ReadI32;
end;


function TProtocolDecorator.ReadI64: Int64;
begin
  result := FWrappedProtocol.ReadI64;
end;


function TProtocolDecorator.ReadDouble:Double;
begin
  result := FWrappedProtocol.ReadDouble;
end;


function TProtocolDecorator.ReadBinary: TBytes;
begin
  result := FWrappedProtocol.ReadBinary;
end;


function TProtocolDecorator.ReadUuid: TGuid;
begin
  result := FWrappedProtocol.ReadUuid;
end;


function TProtocolDecorator.ReadString: string;
begin
  result := FWrappedProtocol.ReadString;
end;


function TProtocolDecorator.ReadAnsiString: AnsiString;
begin
  {$WARN SYMBOL_DEPRECATED OFF}
  result := FWrappedProtocol.ReadAnsiString;
  {$WARN SYMBOL_DEPRECATED DEFAULT}
end;


function TProtocolDecorator.GetMinSerializedSize( const aType : TType) : Integer;
begin
  result := FWrappedProtocol.GetMinSerializedSize(aType);
end;


{ Init helper functions }

procedure Init( var rec : TThriftMessage; const AName: string; const AMessageType: TMessageType; const ASeqID: Integer);
begin
  rec.Name := AName;
  rec.Type_ := AMessageType;
  rec.SeqID := ASeqID;
end;


procedure Init( var rec : TThriftStruct; const AName: string = '');
begin
  rec.Name := AName;
end;


procedure Init( var rec : TThriftField; const AName: string; const AType: TType; const AID: SmallInt);
begin
  rec.Name := AName;
  rec.Type_ := AType;
  rec.Id := AId;
end;


procedure Init( var rec : TThriftMap; const AKeyType, AValueType: TType; const ACount: Integer);
begin
  rec.ValueType := AValueType;
  rec.KeyType := AKeyType;
  rec.Count := ACount;
end;


procedure Init( var rec : TThriftSet; const AElementType: TType; const ACount: Integer);
begin
  rec.Count := ACount;
  rec.ElementType := AElementType;
end;


procedure Init( var rec : TThriftList; const AElementType: TType; const ACount: Integer);
begin
  rec.Count := ACount;
  rec.ElementType := AElementType;
end;



end.

