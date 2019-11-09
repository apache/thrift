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

unit Thrift.Configuration;

interface

uses
  SysUtils, Generics.Collections, Generics.Defaults,
  Thrift.Utils;

const
  DEFAULT_RECURSION_LIMIT = 64;
  DEFAULT_MAX_MESSAGE_SIZE = 100 * 1024 * 1024; // 100 MB
  DEFAULT_THRIFT_TIMEOUT = 5 * 1000; // ms

type
  // configuration clients only need to query data
  IThriftConfiguration = interface
    ['{DB8AEF29-3BDE-41B6-8927-A1E767AEC543}']
    function  GetRecursionLimit : Cardinal;
    function  GetMaxMessageSize : Cardinal;

    property  RecursionLimit : Cardinal read GetRecursionLimit;
    property  MaxMessageSize : Cardinal read GetMaxMessageSize;

    // different transports have different timeout defaults for good reasons
    function  ConnectionTimeout( const aTransportDefault : Cardinal) : Cardinal;
    function  ReadWriteTimeout( const aTransportDefault : Cardinal) : Cardinal;
  end;

  // write access for configuration providers
  IConfigureThrift = interface
    ['{7D5E5B29-E99A-4811-9F55-E1E13F8A3125}']
    function  GetRecursionLimit : Cardinal;
    procedure SetRecursionLimit( const value : Cardinal);
    function  GetMaxMessageSize : Cardinal;
    procedure SetMaxMessageSize( const value : Cardinal);
    function  GetConnectionTimeout : TThriftNullable<Cardinal>;
    procedure SetConnectionTimeout( const value : TThriftNullable<Cardinal>);
    function  GetReadWriteTimeout : TThriftNullable<Cardinal>;
    procedure SetReadWriteTimeout( const value : TThriftNullable<Cardinal>);

    property  RecursionLimit : Cardinal read GetRecursionLimit write SetRecursionLimit;
    property  MaxMessageSize : Cardinal read GetMaxMessageSize write SetMaxMessageSize;
    property  ConnectionTimeout : TThriftNullable<Cardinal> read GetConnectionTimeout write SetConnectionTimeout;
    property  ReadWriteTimeout : TThriftNullable<Cardinal> read GetReadWriteTimeout write SetReadWriteTimeout;

    // helper to access the IThriftConfiguration interface, internally this only calls supports()
    function  GetConfiguration : IThriftConfiguration;
  end;


  TConfigurationImpl = class( TInterfacedObject, IThriftConfiguration, IConfigureThrift)
  strict protected
    // IThriftConfiguration
    FRecursionLimit : Cardinal;
    FMaxMessageSize : Cardinal;
    FConnectionTimeout : TThriftNullable<Cardinal>;
    FReadWriteTimeout : TThriftNullable<Cardinal>;

    // IThriftConfiguration
    function  GetRecursionLimit : Cardinal;
    function  GetMaxMessageSize : Cardinal;
    // different transports have different timeout defaults for good reasons
    function  ConnectionTimeout( const aTransportDefault : Cardinal) : Cardinal;
    function  ReadWriteTimeout( const aTransportDefault : Cardinal) : Cardinal;

    // IConfigureThrift
    procedure SetRecursionLimit( const aValue : Cardinal);
    procedure SetMaxMessageSize( const aValue : Cardinal);
    function  GetConnectionTimeout : TThriftNullable<Cardinal>;
    procedure SetConnectionTimeout( const value : TThriftNullable<Cardinal>);
    function  GetReadWriteTimeout : TThriftNullable<Cardinal>;
    procedure SetReadWriteTimeout( const value : TThriftNullable<Cardinal>);
    function  GetConfiguration : IThriftConfiguration;

  public
    constructor Create;

    class function DefaultConfiguration : IThriftConfiguration;
  end;

implementation



{ TConfigurationImpl }

constructor TConfigurationImpl.Create;
begin
  inherited Create;

  FRecursionLimit := DEFAULT_RECURSION_LIMIT;
  FMaxMessageSize := DEFAULT_MAX_MESSAGE_SIZE;
  FConnectionTimeout := TThriftNullable<Cardinal>.Empty;  // not set
  FReadWriteTimeout := TThriftNullable<Cardinal>.Empty;  // not set
end;


class function TConfigurationImpl.DefaultConfiguration : IThriftConfiguration;
begin
  result := TConfigurationImpl.Create;
  // defaults go here
end;


function TConfigurationImpl.GetConfiguration : IThriftConfiguration;
begin
  result := Self;
end;


function TConfigurationImpl.ConnectionTimeout( const aTransportDefault : Cardinal) : Cardinal;
begin
  if not FConnectionTimeout.GetValue(result)
  then result := aTransportDefault;
end;

function TConfigurationImpl.GetConnectionTimeout : TThriftNullable<Cardinal>;
begin
  result := FConnectionTimeout;
end;

procedure TConfigurationImpl.SetConnectionTimeout( const value : TThriftNullable<Cardinal>);
begin
  FConnectionTimeout := value;
end;

function TConfigurationImpl.ReadWriteTimeout( const aTransportDefault : Cardinal) : Cardinal;
begin
  if not FReadWriteTimeout.GetValue(result)
  then result := aTransportDefault;
end;

function TConfigurationImpl.GetReadWriteTimeout : TThriftNullable<Cardinal>;
begin
  result := FReadWriteTimeout;
end;

procedure TConfigurationImpl.SetReadWriteTimeout(const value: TThriftNullable<Cardinal>);
begin
  FReadWriteTimeout := value;
end;

function TConfigurationImpl.GetMaxMessageSize: Cardinal;
begin
  result := FMaxMessageSize;
end;

procedure TConfigurationImpl.SetMaxMessageSize(const aValue: Cardinal);
begin
  FMaxMessageSize := aValue;
end;

function TConfigurationImpl.GetRecursionLimit: Cardinal;
begin
  result := FRecursionLimit;
end;

procedure TConfigurationImpl.SetRecursionLimit(const aValue: Cardinal);
begin
  FRecursionLimit := aValue;
end;


end.
