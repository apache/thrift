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

unit Thrift.Utils;

interface

{$I Thrift.Defines.inc}

uses
  {$IFDEF OLD_UNIT_NAMES}
  Classes, Windows, SysUtils, Character, SyncObjs, TypInfo, Rtti;
  {$ELSE}
  System.Classes, Winapi.Windows, System.SysUtils, System.Character,
  System.SyncObjs, System.TypInfo, System.Rtti;
  {$ENDIF}

type
  ISupportsToString = interface
    ['{AF71C350-E0CD-4E94-B77C-0310DC8227FF}']
    function ToString : string;
  end;


  IOverlappedHelper = interface
    ['{A1832EFA-2E02-4884-8F09-F0A0277157FA}']
    function Overlapped : TOverlapped;
    function OverlappedPtr : POverlapped;
    function WaitHandle : THandle;
    function WaitFor(dwTimeout: DWORD) : DWORD;
  end;

  TOverlappedHelperImpl = class( TInterfacedObject, IOverlappedHelper)
  strict protected
    FOverlapped : TOverlapped;
    FEvent      : TEvent;

    // IOverlappedHelper
    function Overlapped : TOverlapped;
    function OverlappedPtr : POverlapped;
    function WaitHandle : THandle;
    function WaitFor(dwTimeout: DWORD) : DWORD;
  public
    constructor Create;
    destructor Destroy;  override;
  end;


  TThriftStringBuilder = class( TStringBuilder)
  public
    function Append(const Value: TBytes): TStringBuilder; overload;
    function Append(const Value: ISupportsToString): TStringBuilder; overload;
  end;


  Base64Utils = class sealed
  public
    class function Encode( const src : TBytes; srcOff, len : Integer; dst : TBytes; dstOff : Integer) : Integer; static;
    class function Decode( const src : TBytes; srcOff, len : Integer; dst : TBytes; dstOff : Integer) : Integer; static;
  end;


  CharUtils = class sealed
  public
    class function IsHighSurrogate( const c : Char) : Boolean; static; inline;
    class function IsLowSurrogate( const c : Char) : Boolean; static; inline;

    class function IsHtmlDoctype( const fourBytes : Integer) : Boolean; static;
  end;


  IntegerUtils = class sealed
  strict private
    class procedure SwapBytes( var one, two : Byte); static; inline;
    class procedure Swap2( const pValue : Pointer); static;
    class procedure Swap4( const pValue : Pointer); static;
    class procedure Swap8( const pValue : Pointer); static;
  public
    class procedure SwapByteOrder( const pValue : Pointer; const size : Integer); overload; static;
  end;


  // problem: inheritance possible for class helpers ONLY but not with record helpers
  // workaround: use static class method instead of record helper :-(
  GuidUtils = class sealed
  public
    // new stuff
    class function SwapByteOrder( const aGuid : TGuid) : TGuid; static;

    {$IFDEF Debug}
    class procedure SelfTest; static;
    {$ENDIF}
  end;


  EnumUtils<T> = class sealed
  public
    class function ToString(const value : Integer) : string;  reintroduce; static; inline;
  end;


  StringUtils<T> = class sealed
  public
    class function ToString(const value : T) : string;  reintroduce; static; inline;
  end;


const
  THRIFT_MIMETYPE = 'application/x-thrift';

{$IFDEF Win64}
function InterlockedExchangeAdd64( var Addend : Int64; Value : Int64) : Int64;
{$ENDIF}


implementation

{ TOverlappedHelperImpl }

constructor TOverlappedHelperImpl.Create;
begin
  inherited Create;
  FillChar( FOverlapped, SizeOf(FOverlapped), 0);
  FEvent := TEvent.Create( nil, TRUE, FALSE, '');  // always ManualReset, see MSDN
  FOverlapped.hEvent := FEvent.Handle;
end;



destructor TOverlappedHelperImpl.Destroy;
begin
  try
    FOverlapped.hEvent := 0;
    FreeAndNil( FEvent);

  finally
    inherited Destroy;
  end;

end;


function TOverlappedHelperImpl.Overlapped : TOverlapped;
begin
  result := FOverlapped;
end;


function TOverlappedHelperImpl.OverlappedPtr : POverlapped;
begin
  result := @FOverlapped;
end;


function TOverlappedHelperImpl.WaitHandle : THandle;
begin
  result := FOverlapped.hEvent;
end;


function TOverlappedHelperImpl.WaitFor( dwTimeout : DWORD) : DWORD;
begin
  result := WaitForSingleObject( FOverlapped.hEvent, dwTimeout);
end;


{ Base64Utils }

class function Base64Utils.Encode( const src : TBytes; srcOff, len : Integer; dst : TBytes; dstOff : Integer) : Integer;
const ENCODE_TABLE : PAnsiChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
begin
  ASSERT( len in [1..3]);
  dst[dstOff] := Byte( ENCODE_TABLE[ (src[srcOff] shr 2) and $3F]);
  case len of
    3 : begin
      Inc(dstOff);
      dst[dstOff] := Byte( ENCODE_TABLE[ ((src[srcOff] shl 4) and $30) or ((src[srcOff + 1] shr 4) and $0F)]);
      Inc(dstOff);
      dst[dstOff] := Byte( ENCODE_TABLE[ ((src[srcOff + 1] shl 2) and $3C) or ((src[srcOff + 2] shr 6) and $03)]);
      Inc(dstOff);
      dst[dstOff] := Byte( ENCODE_TABLE[ src[srcOff + 2] and $3F]);
      result := 4;
    end;

    2 : begin
      Inc(dstOff);
      dst[dstOff] := Byte( ENCODE_TABLE[ ((src[srcOff] shl 4) and $30) or ((src[srcOff + 1] shr 4) and $0F)]);
      Inc(dstOff);
      dst[dstOff] := Byte( ENCODE_TABLE[ (src[srcOff + 1] shl 2) and $3C]);
      result := 3;
    end;

    1 : begin
      Inc(dstOff);
      dst[dstOff] := Byte( ENCODE_TABLE[ (src[srcOff] shl 4) and $30]);
      result := 2;
    end;

  else
    ASSERT( FALSE);
    result := 0;  // because invalid call
  end;
end;


class function Base64Utils.Decode( const src : TBytes; srcOff, len : Integer; dst : TBytes; dstOff : Integer) : Integer;
const DECODE_TABLE : array[0..$FF] of Integer
                   = ( -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,62,-1,-1,-1,63,
                       52,53,54,55,56,57,58,59,60,61,-1,-1,-1,-1,-1,-1,
                       -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,
                       15,16,17,18,19,20,21,22,23,24,25,-1,-1,-1,-1,-1,
                       -1,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                       41,42,43,44,45,46,47,48,49,50,51,-1,-1,-1,-1,-1,
                       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1  );
begin
  ASSERT( len in [1..4]);
  result := 1;
  dst[dstOff] := ((DECODE_TABLE[src[srcOff]     and $0FF] shl 2)
              or  (DECODE_TABLE[src[srcOff + 1] and $0FF] shr 4));

  if (len > 2) then begin
    Inc( result);
    Inc( dstOff);
    dst[dstOff] := (((DECODE_TABLE[src[srcOff + 1] and $0FF] shl 4) and $F0)
                or   (DECODE_TABLE[src[srcOff + 2] and $0FF] shr 2));

    if (len > 3) then begin
      Inc( result);
      Inc( dstOff);
      dst[dstOff] := (((DECODE_TABLE[src[srcOff + 2] and $0FF] shl 6) and $C0)
                  or    DECODE_TABLE[src[srcOff + 3] and $0FF]);
    end;
  end;
end;


class function CharUtils.IsHighSurrogate( const c : Char) : Boolean;
begin
  {$IF CompilerVersion < 25.0}
    {$IFDEF OLD_UNIT_NAMES}
    result := Character.IsHighSurrogate(c);
    {$ELSE}
    result := System.Character.IsHighSurrogate(c);
    {$ENDIF}
  {$ELSE}
  result := c.IsHighSurrogate();
  {$IFEND}
end;


class function CharUtils.IsLowSurrogate( const c : Char) : Boolean;
begin
  {$IF CompilerVersion < 25.0}
    {$IFDEF OLD_UNIT_NAMES}
    result := Character.IsLowSurrogate(c);
    {$ELSE}
    result := System.Character.IsLowSurrogate(c);
    {$ENDIF}
  {$ELSE}
  result := c.IsLowSurrogate();
  {$IFEND}
end;


class function CharUtils.IsHtmlDoctype( const fourBytes : Integer) : Boolean;
var pc : PAnsiChar;
const HTML_BEGIN : PAnsiChar = 'OD!<';  // first 4 bytes of '<!DOCTYPE ' in LE byte order
begin
  pc := @fourBytes;

  if UpCase(pc^) <> HTML_BEGIN[0]
  then Exit(FALSE);

  Inc( pc);
  if UpCase(pc^) <> HTML_BEGIN[1]
  then Exit(FALSE);


  Inc( pc);
  if UpCase(pc^) <> HTML_BEGIN[2]
  then Exit(FALSE);

  Inc( pc);
  result := (UpCase(pc^) = HTML_BEGIN[3]);
end;

{ IntegerUtils }


class procedure IntegerUtils.SwapBytes( var one, two : Byte);
var tmp : Byte;
begin
  tmp := one;
  one := two;
  two := tmp;
end;


class procedure IntegerUtils.Swap2( const pValue : Pointer);
var pData : PByteArray absolute pValue;
begin
  SwapBytes( pData^[0], pData^[1]);
end;


class procedure IntegerUtils.Swap4( const pValue : Pointer);
var pData : PByteArray absolute pValue;
begin
  SwapBytes( pData^[0], pData^[3]);
  SwapBytes( pData^[1], pData^[2]);
end;


class procedure IntegerUtils.Swap8( const pValue : Pointer);
var pData : PByteArray absolute pValue;
begin
  SwapBytes( pData^[0], pData^[7]);
  SwapBytes( pData^[1], pData^[6]);
  SwapBytes( pData^[2], pData^[5]);
  SwapBytes( pData^[3], pData^[4]);
end;


class procedure IntegerUtils.SwapByteOrder( const pValue : Pointer; const size : Integer);
begin
  case size of
    2 : Swap2( pValue);
    4 : Swap4( pValue);
    8 : Swap8( pValue);
  else
    raise EArgumentException.Create('Unexpected size');
  end;
end;


{ GuidUtils }


class function GuidUtils.SwapByteOrder( const aGuid : TGuid) : TGuid;
// convert to/from network byte order
// - https://www.ietf.org/rfc/rfc4122.txt
// - https://stackoverflow.com/questions/10850075/guid-uuid-compatibility-issue-between-net-and-linux
// - https://lists.gnu.org/archive/html/bug-parted/2002-01/msg00099.html
begin
  result := aGuid;

  IntegerUtils.SwapByteOrder( @result.D1, SizeOf(result.D1));
  IntegerUtils.SwapByteOrder( @result.D2, SizeOf(result.D2));
  IntegerUtils.SwapByteOrder( @result.D3, SizeOf(result.D3));
  //result.D4 = array of byte -> implicitly correct
end;


{$IFDEF Debug}
class procedure GuidUtils.SelfTest;
var guid   : TGuid;
    pBytes : PByteArray;
    i, expected : Integer;
const TEST_GUID : TGuid = '{00112233-4455-6677-8899-aabbccddeeff}';
begin
  // host to network
  guid := TEST_GUID;
  guid := GuidUtils.SwapByteOrder(guid);

  // validate network order
  pBytes := @guid;
  for i := 0 to $F do begin
    expected := i * $11;
    ASSERT( pBytes^[i] = expected);
  end;

  // network to host and final validation
  guid := GuidUtils.SwapByteOrder(guid);
  ASSERT( IsEqualGuid( guid, TEST_GUID));

  // prevent collisions with SysUtils.TGuidHelper
  guid := TGuid.NewGuid;
end;
{$ENDIF}



{$IFDEF Win64}

function InterlockedCompareExchange64( var Target : Int64; Exchange, Comparand : Int64) : Int64;  inline;
begin
  {$IFDEF OLD_UNIT_NAMES}
  result := Windows.InterlockedCompareExchange64( Target, Exchange, Comparand);
  {$ELSE}
  result := WinApi.Windows.InterlockedCompareExchange64( Target, Exchange, Comparand);
  {$ENDIF}
end;


function InterlockedExchangeAdd64( var Addend : Int64; Value : Int64) : Int64;
var old : Int64;
begin
  repeat
    Old := Addend;
  until (InterlockedCompareExchange64( Addend, Old + Value, Old) = Old);
  result := Old;
end;

{$ENDIF}


{ EnumUtils<T> }

class function EnumUtils<T>.ToString(const value : Integer) : string;
var pType : PTypeInfo;
begin
  pType := PTypeInfo(TypeInfo(T));
  if Assigned(pType)
  and (pType^.Kind = tkEnumeration)
  {$IF CompilerVersion >= 23.0}   // TODO: Range correct? What we know is that XE does not offer it, but Rio has it
  and (pType^.TypeData^.MaxValue >= value)
  and (pType^.TypeData^.MinValue <= value)
  {$ELSE}
  and FALSE  // THRIFT-5048: pType^.TypeData^ member not supported -> prevent GetEnumName() from reading outside the legal range
  {$IFEND}
  then result := GetEnumName( PTypeInfo(pType), value)
  else result := IntToStr(Ord(value));
end;


{ StringUtils<T> }

class function StringUtils<T>.ToString(const value : T) : string;
type PInterface = ^IInterface;
var pType : PTypeInfo;
    stos  : ISupportsToString;
    pIntf : PInterface;  // Workaround: Rio does not allow the direct typecast
begin
  pType := PTypeInfo(TypeInfo(T));
  if Assigned(pType) then begin
    case pType^.Kind of

      tkInterface : begin
        pIntf := PInterface(@value);
        if Supports( pIntf^, ISupportsToString, stos) then begin
          result := stos.toString;
          Exit;
        end;
      end;

      tkEnumeration : begin
        case SizeOf(value) of
          1 : begin result := EnumUtils<T>.ToString( PShortInt(@value)^);  Exit; end;
          2 : begin result := EnumUtils<T>.ToString( PSmallInt(@value)^);  Exit; end;
          4 : begin result := EnumUtils<T>.ToString( PLongInt(@value)^);  Exit; end;
        else
          ASSERT(FALSE); // in theory, this should not happen
        end;
      end;

    end;
  end;

  result := TValue.From<T>(value).ToString;
end;


{ TThriftStringBuilder }

function TThriftStringBuilder.Append(const Value: TBytes): TStringBuilder;
begin
  Result := Append( string( RawByteString(Value)) );
end;

function TThriftStringBuilder.Append( const Value: ISupportsToString): TStringBuilder;
begin
  Result := Append( Value.ToString );
end;


begin
  {$IFDEF Debug}
  GuidUtils.SelfTest;
  {$ENDIF}
end.
