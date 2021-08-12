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

unit Thrift;

interface

uses
  SysUtils,
  Thrift.Utils,
  Thrift.Exception,
  Thrift.Protocol;

const
  Version = '0.14.1';

type
  TException = Thrift.Exception.TException; // compatibility alias

  TApplicationExceptionSpecializedClass = class of TApplicationExceptionSpecialized;

  TApplicationException = class( TException, IBase, ISupportsToString)
  public
    type
{$SCOPEDENUMS ON}
      TExceptionType = (
        Unknown,
        UnknownMethod,
        InvalidMessageType,
        WrongMethodName,
        BadSequenceID,
        MissingResult,
        InternalError,
        ProtocolError,
        InvalidTransform,
        InvalidProtocol,
        UnsupportedClientType
      );
{$SCOPEDENUMS OFF}
  strict private
    FExceptionType : TExceptionType;

  strict protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

  strict protected
    constructor HiddenCreate(const Msg: string);
    class function GetSpecializedExceptionType(AType: TExceptionType): TApplicationExceptionSpecializedClass;

  public
    // purposefully hide inherited constructor
    class function Create(const Msg: string): TApplicationException; overload; deprecated 'Use specialized TApplicationException types (or regenerate from IDL)';
    class function Create: TApplicationException; overload; deprecated 'Use specialized TApplicationException types (or regenerate from IDL)';
    class function Create( AType: TExceptionType): TApplicationException; overload; deprecated 'Use specialized TApplicationException types (or regenerate from IDL)';
    class function Create( AType: TExceptionType; const msg: string): TApplicationException; overload; deprecated 'Use specialized TApplicationException types (or regenerate from IDL)';

    function Type_: TExceptionType; virtual;

    procedure IBase_Read( const iprot: IProtocol);
    procedure IBase.Read = IBase_Read;

    class function Read( const iprot: IProtocol): TApplicationException;
    procedure Write( const oprot: IProtocol );
  end;

  // Needed to remove deprecation warning
  TApplicationExceptionSpecialized = class abstract (TApplicationException)
  strict protected
    class function GetType: TApplicationException.TExceptionType;  virtual; abstract;
  public
    constructor Create(const Msg: string);
    function Type_: TApplicationException.TExceptionType; override;
  end;

  TApplicationExceptionUnknown = class (TApplicationExceptionSpecialized)
  strict protected
    class function GetType: TApplicationException.TExceptionType;  override;
  end;

  TApplicationExceptionUnknownMethod = class (TApplicationExceptionSpecialized)
  strict protected
    class function GetType: TApplicationException.TExceptionType;  override;
  end;

  TApplicationExceptionInvalidMessageType = class (TApplicationExceptionSpecialized)
  strict protected
    class function GetType: TApplicationException.TExceptionType;  override;
  end;

  TApplicationExceptionWrongMethodName = class (TApplicationExceptionSpecialized)
  strict protected
    class function GetType: TApplicationException.TExceptionType;  override;
  end;

  TApplicationExceptionBadSequenceID = class (TApplicationExceptionSpecialized)
  strict protected
    class function GetType: TApplicationException.TExceptionType;  override;
  end;

  TApplicationExceptionMissingResult = class (TApplicationExceptionSpecialized)
  strict protected
    class function GetType: TApplicationException.TExceptionType;  override;
  end;

  TApplicationExceptionInternalError = class (TApplicationExceptionSpecialized)
  strict protected
    class function GetType: TApplicationException.TExceptionType;  override;
  end;

  TApplicationExceptionProtocolError = class (TApplicationExceptionSpecialized)
  strict protected
    class function GetType: TApplicationException.TExceptionType;  override;
  end;

  TApplicationExceptionInvalidTransform = class (TApplicationExceptionSpecialized)
  strict protected
    class function GetType: TApplicationException.TExceptionType;  override;
  end;

  TApplicationExceptionInvalidProtocol = class (TApplicationExceptionSpecialized)
  strict protected
    class function GetType: TApplicationException.TExceptionType;  override;
  end;

  TApplicationExceptionUnsupportedClientType = class (TApplicationExceptionSpecialized)
  strict protected
    class function GetType: TApplicationException.TExceptionType;  override;
  end;



implementation

{ TApplicationException }

constructor TApplicationException.HiddenCreate(const Msg: string);
begin
  inherited Create(Msg);
end;

class function TApplicationException.Create(const Msg: string): TApplicationException;
begin
  Result := TApplicationExceptionUnknown.Create(Msg);
end;

class function TApplicationException.Create: TApplicationException;
begin
  Result := TApplicationExceptionUnknown.Create('');
end;

class function TApplicationException.Create( AType: TExceptionType): TApplicationException;
begin
{$WARN SYMBOL_DEPRECATED OFF}
  Result := Create(AType, '');
{$WARN SYMBOL_DEPRECATED DEFAULT}
end;

class function TApplicationException.Create( AType: TExceptionType; const msg: string): TApplicationException;
begin
  Result := GetSpecializedExceptionType(AType).Create(msg);
end;


function TApplicationException.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj)
  then result := S_OK
  else result := E_NOINTERFACE;
end;

function TApplicationException._AddRef: Integer;
begin
  result := -1;    // not refcounted
end;

function TApplicationException._Release: Integer;
begin
  result := -1;    // not refcounted
end;


function TApplicationException.Type_: TExceptionType;
begin
  result := FExceptionType;
end;


class function TApplicationException.GetSpecializedExceptionType(AType: TExceptionType): TApplicationExceptionSpecializedClass;
begin
  case AType of
    TExceptionType.UnknownMethod:         Result := TApplicationExceptionUnknownMethod;
    TExceptionType.InvalidMessageType:    Result := TApplicationExceptionInvalidMessageType;
    TExceptionType.WrongMethodName:       Result := TApplicationExceptionWrongMethodName;
    TExceptionType.BadSequenceID:         Result := TApplicationExceptionBadSequenceID;
    TExceptionType.MissingResult:         Result := TApplicationExceptionMissingResult;
    TExceptionType.InternalError:         Result := TApplicationExceptionInternalError;
    TExceptionType.ProtocolError:         Result := TApplicationExceptionProtocolError;
    TExceptionType.InvalidTransform:      Result := TApplicationExceptionInvalidTransform;
    TExceptionType.InvalidProtocol:       Result := TApplicationExceptionInvalidProtocol;
    TExceptionType.UnsupportedClientType: Result := TApplicationExceptionUnsupportedClientType;
  else
    ASSERT( TExceptionType.Unknown = aType);
    Result := TApplicationExceptionUnknown;
  end;
end;


procedure TApplicationException.IBase_Read( const iprot: IProtocol);
var
  field : TThriftField;
  struc : TThriftStruct;
begin
  struc := iprot.ReadStructBegin;
  while ( True ) do
  begin
    field := iprot.ReadFieldBegin;
    if ( field.Type_ = TType.Stop) then begin
      Break;
    end;

    case field.Id of
      1 : begin
        if ( field.Type_ = TType.String_) then begin
          Exception(Self).Message := iprot.ReadString;
        end else begin
          TProtocolUtil.Skip( iprot, field.Type_ );
        end;
      end;

      2 : begin
        if ( field.Type_ = TType.I32) then begin
          FExceptionType := TExceptionType( iprot.ReadI32 );
        end else begin
          TProtocolUtil.Skip( iprot, field.Type_ );
        end;
      end else begin
        TProtocolUtil.Skip( iprot, field.Type_);
      end;
    end;
    iprot.ReadFieldEnd;
  end;
  iprot.ReadStructEnd;
end;


class function TApplicationException.Read( const iprot: IProtocol): TApplicationException;
var instance : TApplicationException;
    base : IBase;
begin
  instance := TApplicationException.CreateFmt('',[]);
  try
    if Supports( instance, IBase, base) then try
      base.Read(iprot);
    finally
      base := nil;  // clear ref before free
    end;

    result := GetSpecializedExceptionType(instance.Type_).Create( Exception(instance).Message);
  finally
    instance.Free;
  end;
end;

procedure TApplicationException.Write( const oprot: IProtocol);
var
  struc : TThriftStruct;
  field : TThriftField;
begin
  Init(struc, 'TApplicationException');
  Init(field);

  oprot.WriteStructBegin( struc );
  if Message <> '' then begin
    field.Name := 'message';
    field.Type_ := TType.String_;
    field.Id := 1;
    oprot.WriteFieldBegin( field );
    oprot.WriteString( Message );
    oprot.WriteFieldEnd;
  end;

  field.Name := 'type';
  field.Type_ := TType.I32;
  field.Id := 2;
  oprot.WriteFieldBegin(field);
  oprot.WriteI32(Integer(Type_));
  oprot.WriteFieldEnd();
  oprot.WriteFieldStop();
  oprot.WriteStructEnd();
end;

{ TApplicationExceptionSpecialized }

constructor TApplicationExceptionSpecialized.Create(const Msg: string);
begin
  inherited HiddenCreate(Msg);
end;

function TApplicationExceptionSpecialized.Type_: TApplicationException.TExceptionType;
begin
  result := GetType;
end;


{ specialized TApplicationExceptions }

class function TApplicationExceptionUnknownMethod.GetType : TApplicationException.TExceptionType;
begin
  result := TExceptionType.UnknownMethod;
end;

class function TApplicationExceptionInvalidMessageType.GetType : TApplicationException.TExceptionType;
begin
  result := TExceptionType.InvalidMessageType;
end;

class function TApplicationExceptionWrongMethodName.GetType : TApplicationException.TExceptionType;
begin
  result := TExceptionType.WrongMethodName;
end;

class function TApplicationExceptionBadSequenceID.GetType : TApplicationException.TExceptionType;
begin
  result := TExceptionType.BadSequenceID;
end;

class function TApplicationExceptionMissingResult.GetType : TApplicationException.TExceptionType;
begin
  result := TExceptionType.MissingResult;
end;

class function TApplicationExceptionInternalError.GetType : TApplicationException.TExceptionType;
begin
  result := TExceptionType.InternalError;
end;

class function TApplicationExceptionProtocolError.GetType : TApplicationException.TExceptionType;
begin
  result := TExceptionType.ProtocolError;
end;

class function TApplicationExceptionInvalidTransform.GetType : TApplicationException.TExceptionType;
begin
  result := TExceptionType.InvalidTransform;
end;

class function TApplicationExceptionInvalidProtocol.GetType : TApplicationException.TExceptionType;
begin
  result := TExceptionType.InvalidProtocol;
end;

class function TApplicationExceptionUnsupportedClientType.GetType : TApplicationException.TExceptionType;
begin
  result := TExceptionType.UnsupportedClientType;
end;

class function TApplicationExceptionUnknown.GetType : TApplicationException.TExceptionType;
begin
  result := TExceptionType.Unknown;
end;


end.
