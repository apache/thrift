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
  SysUtils, Thrift.Protocol;

const
  Version = '0.9.2';

type
  TApplicationException = class( SysUtils.Exception )
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
  private
    FType : TExceptionType;
  public
    constructor Create; overload;
    constructor Create( AType: TExceptionType); overload;
    constructor Create( AType: TExceptionType; const msg: string); overload;

    class function Read( const iprot: IProtocol): TApplicationException;
    procedure Write( const oprot: IProtocol );
  end;

  // base class for IDL-generated exceptions
  TException = class( SysUtils.Exception)
  public
    function Message : string;        // hide inherited property: allow read, but prevent accidental writes
    procedure UpdateMessageProperty;  // update inherited message property with toString()
  end;

implementation

{ TException }

function TException.Message;
// allow read (exception summary), but prevent accidental writes
// read will return the exception summary
begin
  result := Self.ToString;
end;

procedure TException.UpdateMessageProperty;
// Update the inherited Message property to better conform to standard behaviour.
// Nice benefit: The IDE is now able to show the exception message again.
begin
  inherited Message := Self.ToString;  // produces a summary text
end;

{ TApplicationException }

constructor TApplicationException.Create;
begin
  inherited Create( '' );
end;

constructor TApplicationException.Create(AType: TExceptionType;
  const msg: string);
begin
  inherited Create( msg );
  FType := AType;
end;

constructor TApplicationException.Create(AType: TExceptionType);
begin
  inherited Create('');
  FType := AType;
end;

class function TApplicationException.Read( const iprot: IProtocol): TApplicationException;
var
  field : IField;
  msg : string;
  typ : TExceptionType;
  struc : IStruct;
begin
  msg := '';
  typ := TExceptionType.Unknown;
  struc := iprot.ReadStructBegin;
  while ( True ) do
  begin
    field := iprot.ReadFieldBegin;
    if ( field.Type_ = TType.Stop) then
    begin
      Break;
    end;

    case field.Id of
      1 : begin
        if ( field.Type_ = TType.String_) then
        begin
          msg := iprot.ReadString;
        end else
        begin
          TProtocolUtil.Skip( iprot, field.Type_ );
        end;
      end;

      2 : begin
        if ( field.Type_ = TType.I32) then
        begin
          typ := TExceptionType( iprot.ReadI32 );
        end else
        begin
          TProtocolUtil.Skip( iprot, field.Type_ );
        end;
      end else
      begin
        TProtocolUtil.Skip( iprot, field.Type_);
      end;
    end;
    iprot.ReadFieldEnd;
  end;
  iprot.ReadStructEnd;
  Result := TApplicationException.Create( typ, msg );
end;

procedure TApplicationException.Write( const oprot: IProtocol);
var
  struc : IStruct;
  field : IField;

begin
  struc := TStructImpl.Create( 'TApplicationException' );
  field := TFieldImpl.Create;

  oprot.WriteStructBegin( struc );
  if Message <> '' then
  begin
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
  oprot.WriteI32(Integer(FType));
  oprot.WriteFieldEnd();
  oprot.WriteFieldStop();
  oprot.WriteStructEnd();
end;

end.
