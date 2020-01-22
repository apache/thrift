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

unit Thrift.Stream;

{$I Thrift.Defines.inc}

interface

uses
  Classes,
  SysUtils,
  SysConst,
  RTLConsts,
  {$IFDEF OLD_UNIT_NAMES}
  ActiveX,
  {$ELSE}
  Winapi.ActiveX,
  {$ENDIF}
  Thrift.Utils;

type
  IThriftStream = interface
    ['{3A61A8A6-3639-4B91-A260-EFCA23944F3A}']
    procedure Write( const buffer: TBytes; offset: Integer; count: Integer);  overload;
    procedure Write( const pBuf : Pointer; offset: Integer; count: Integer);  overload;
    function Read( var buffer: TBytes; offset: Integer; count: Integer): Integer;  overload;
    function Read( const pBuf : Pointer; const buflen : Integer; offset: Integer; count: Integer): Integer;  overload;
    procedure Open;
    procedure Close;
    procedure Flush;
    function IsOpen: Boolean;
    function ToArray: TBytes;
    function Size : Int64;
    function Position : Int64;
  end;


  TThriftStreamImpl = class abstract( TInterfacedObject, IThriftStream)
  strict private
    procedure CheckSizeAndOffset( const pBuf : Pointer; const buflen : Integer; offset: Integer; count: Integer);  overload;
  strict protected
    // IThriftStream
    procedure Write( const buffer: TBytes; offset: Integer; count: Integer); overload; inline;
    procedure Write( const pBuf : Pointer; offset: Integer; count: Integer);  overload; virtual;
    function Read( var buffer: TBytes; offset: Integer; count: Integer): Integer; overload; inline;
    function Read( const pBuf : Pointer; const buflen : Integer; offset: Integer; count: Integer): Integer; overload; virtual;
    procedure Open; virtual; abstract;
    procedure Close; virtual; abstract;
    procedure Flush; virtual; abstract;
    function IsOpen: Boolean; virtual; abstract;
    function ToArray: TBytes; virtual; abstract;
    function Size : Int64; virtual;
    function Position : Int64;  virtual;
  end;

  TThriftStreamAdapterDelphi = class( TThriftStreamImpl)
  strict private
    FStream : TStream;
    FOwnsStream : Boolean;
  strict protected
    // IThriftStream
    procedure Write( const pBuf : Pointer; offset: Integer; count: Integer); override;
    function Read( const pBuf : Pointer; const buflen : Integer; offset: Integer; count: Integer): Integer; override;
    procedure Open; override;
    procedure Close; override;
    procedure Flush; override;
    function IsOpen: Boolean; override;
    function ToArray: TBytes; override;
    function Size : Int64; override;
    function Position : Int64;  override;
  public
    constructor Create( const aStream: TStream; aOwnsStream : Boolean);
    destructor Destroy; override;
  end;

  TThriftStreamAdapterCOM = class( TThriftStreamImpl)
  strict private
    FStream : IStream;
  strict protected
    // IThriftStream
    procedure Write( const pBuf : Pointer; offset: Integer; count: Integer); override;
    function Read( const pBuf : Pointer; const buflen : Integer; offset: Integer; count: Integer): Integer; override;
    procedure Open; override;
    procedure Close; override;
    procedure Flush; override;
    function IsOpen: Boolean; override;
    function ToArray: TBytes; override;
    function Size : Int64; override;
    function Position : Int64;  override;
  public
    constructor Create( const aStream: IStream);
  end;

implementation

uses Thrift.Transport;

{ TThriftStreamAdapterCOM }

procedure TThriftStreamAdapterCOM.Close;
begin
  FStream := nil;
end;

constructor TThriftStreamAdapterCOM.Create( const aStream: IStream);
begin
  inherited Create;
  FStream := aStream;
end;

procedure TThriftStreamAdapterCOM.Flush;
begin
  if IsOpen then begin
    if FStream <> nil then begin
      FStream.Commit( STGC_DEFAULT );
    end;
  end;
end;

function TThriftStreamAdapterCOM.Size : Int64;
var statstg: TStatStg;
begin
  FillChar( statstg, SizeOf( statstg), 0);
  if  IsOpen
  and Succeeded( FStream.Stat( statstg, STATFLAG_NONAME ))
  then result := statstg.cbSize
  else result := 0;
end;

function TThriftStreamAdapterCOM.Position : Int64;
var newpos : {$IF CompilerVersion >= 29.0} UInt64 {$ELSE} Int64  {$IFEND};
begin
  if SUCCEEDED( FStream.Seek( 0, STREAM_SEEK_CUR, newpos))
  then result := Int64(newpos)  
  else raise TTransportExceptionEndOfFile.Create('Seek() error');
end;

function TThriftStreamAdapterCOM.IsOpen: Boolean;
begin
  Result := FStream <> nil;
end;

procedure TThriftStreamAdapterCOM.Open;
begin
  // nothing to do
end;

function TThriftStreamAdapterCOM.Read( const pBuf : Pointer; const buflen : Integer; offset: Integer; count: Integer): Integer;
var pTmp : PByte;
begin
  inherited;

  if count >= buflen-offset
  then count := buflen-offset;

  Result := 0;
  if FStream <> nil then begin
    if count > 0 then begin
      pTmp := pBuf;
      Inc( pTmp, offset);
      FStream.Read( pTmp, count, @Result);
    end;
  end;
end;

function TThriftStreamAdapterCOM.ToArray: TBytes;
var
  len : Int64;
  NewPos : {$IF CompilerVersion >= 29.0} UInt64 {$ELSE} Int64  {$IFEND};
  cbRead : Integer;
begin
  len := Self.Size;
  SetLength( Result, len );

  if len > 0 then begin
    if Succeeded( FStream.Seek( 0, STREAM_SEEK_SET, NewPos) ) then begin
      FStream.Read( @Result[0], len, @cbRead);
    end;
  end;
end;

procedure TThriftStreamAdapterCOM.Write( const pBuf: Pointer; offset: Integer; count: Integer);
var nWritten : Integer;
    pTmp : PByte;
begin
  inherited;
  if IsOpen then begin
    if count > 0 then begin
      pTmp := pBuf;
      Inc( pTmp, offset);
      FStream.Write( pTmp, count, @nWritten);
    end;
  end;
end;

{ TThriftStreamImpl }

procedure TThriftStreamImpl.CheckSizeAndOffset( const pBuf : Pointer; const buflen : Integer; offset: Integer; count: Integer);
begin
  if count > 0 then begin
    if (offset < 0) or ( offset >= buflen) then begin
      raise ERangeError.Create( SBitsIndexError );
    end;
    if count > buflen then begin
      raise ERangeError.Create( SBitsIndexError );
    end;
  end;
end;

function TThriftStreamImpl.Read(var buffer: TBytes; offset, count: Integer): Integer;
begin
  if Length(buffer) > 0
  then Result := Read( @buffer[0], Length(buffer), offset, count)
  else Result := 0;
end;

function TThriftStreamImpl.Read( const pBuf : Pointer; const buflen : Integer; offset: Integer; count: Integer): Integer;
begin
  Result := 0;
  CheckSizeAndOffset( pBuf, buflen, offset, count );
end;

procedure TThriftStreamImpl.Write(const buffer: TBytes; offset, count: Integer);
begin
  if Length(buffer) > 0
  then Write( @buffer[0], offset, count);
end;

procedure TThriftStreamImpl.Write( const pBuf : Pointer; offset: Integer; count: Integer);
begin
  CheckSizeAndOffset( pBuf, offset+count, offset, count);
end;

function TThriftStreamImpl.Size : Int64;
begin
  ASSERT(FALSE);
  raise ENotImplemented.Create(ClassName+'.Size');
end;

function TThriftStreamImpl.Position : Int64;
begin
  ASSERT(FALSE);
  raise ENotImplemented.Create(ClassName+'.Position');
end;


{ TThriftStreamAdapterDelphi }

constructor TThriftStreamAdapterDelphi.Create( const aStream: TStream; aOwnsStream: Boolean);
begin
  inherited Create;
  FStream := aStream;
  FOwnsStream := aOwnsStream;
end;

destructor TThriftStreamAdapterDelphi.Destroy;
begin
  if FOwnsStream
  then Close;

  inherited;
end;

procedure TThriftStreamAdapterDelphi.Close;
begin
  FStream.Free;
  FStream := nil;
  FOwnsStream := False;
end;

procedure TThriftStreamAdapterDelphi.Flush;
begin
  // nothing to do
end;

function TThriftStreamAdapterDelphi.Size : Int64;
begin
  result := FStream.Size;
end;

function TThriftStreamAdapterDelphi.Position : Int64;
begin
  result := FStream.Position;
end;

function TThriftStreamAdapterDelphi.IsOpen: Boolean;
begin
  Result := FStream <> nil;
end;

procedure TThriftStreamAdapterDelphi.Open;
begin
  // nothing to do
end;

function TThriftStreamAdapterDelphi.Read(const pBuf : Pointer; const buflen : Integer; offset, count: Integer): Integer;
var pTmp : PByte;
begin
  inherited;

  if count >= buflen-offset
  then count := buflen-offset;

  if count > 0 then begin
    pTmp := pBuf;
    Inc( pTmp, offset);
    Result := FStream.Read( pTmp^, count)
  end
  else Result := 0;
end;

function TThriftStreamAdapterDelphi.ToArray: TBytes;
var
  OrgPos : Integer;
  len : Integer;
begin
  if FStream <> nil
  then len := FStream.Size
  else len := 0;

  SetLength( Result, len );

  if len > 0 then
  begin
    OrgPos := FStream.Position;
    try
      FStream.Position := 0;
      FStream.ReadBuffer( Pointer(@Result[0])^, len );
    finally
      FStream.Position := OrgPos;
    end;
  end
end;

procedure TThriftStreamAdapterDelphi.Write(const pBuf : Pointer; offset, count: Integer);
var pTmp : PByte;
begin
  inherited;
  if count > 0 then begin
    pTmp := pBuf;
    Inc( pTmp, offset);
    FStream.Write( pTmp^, count)
  end;
end;

end.
