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
  SysUtils, Generics.Collections, Generics.Defaults;

const
  DEFAULT_RECURSION_LIMIT  = 64;
  DEFAULT_MAX_MESSAGE_SIZE = 100 * 1024 * 1024; // 100 MB
  DEFAULT_MAX_FRAME_SIZE   = 16384000;      // this value is used consistently across all Thrift libraries

  DEFAULT_THRIFT_TIMEOUT = 5 * 1000; // ms

type
  IThriftConfiguration = interface
    ['{ADD75449-1A67-4B78-9B75-502A1E338CFC}']
    function  GetRecursionLimit : Cardinal;
    procedure SetRecursionLimit( const value : Cardinal);
    function  GetMaxFrameSize : Cardinal;
    procedure SetMaxFrameSize( const value : Cardinal);
    function  GetMaxMessageSize : Cardinal;
    procedure SetMaxMessageSize( const value : Cardinal);

    property  RecursionLimit : Cardinal read GetRecursionLimit write SetRecursionLimit;
    property  MaxFrameSize : Cardinal   read GetMaxFrameSize   write SetMaxFrameSize;
    property  MaxMessageSize : Cardinal read GetMaxMessageSize write SetMaxMessageSize;
  end;


  TThriftConfigurationImpl = class( TInterfacedObject, IThriftConfiguration)
  strict protected
    FRecursionLimit : Cardinal;
    FMaxFrameSize   : Cardinal;
    FMaxMessageSize : Cardinal;

    // IThriftConfiguration
    function  GetRecursionLimit : Cardinal;
    procedure SetRecursionLimit( const value : Cardinal);
    function  GetMaxFrameSize : Cardinal;
    procedure SetMaxFrameSize( const value : Cardinal);
    function  GetMaxMessageSize : Cardinal;
    procedure SetMaxMessageSize( const value : Cardinal);

  public
    constructor Create;
  end;


implementation


{ TThriftConfigurationImpl }


constructor TThriftConfigurationImpl.Create;
begin
  inherited Create;

  FRecursionLimit := DEFAULT_RECURSION_LIMIT;
  FMaxFrameSize   := DEFAULT_MAX_FRAME_SIZE;
  FMaxMessageSize := DEFAULT_MAX_MESSAGE_SIZE;
end;


function TThriftConfigurationImpl.GetRecursionLimit: Cardinal;
begin
  result := FRecursionLimit;
end;


procedure TThriftConfigurationImpl.SetRecursionLimit(const value: Cardinal);
begin
  FRecursionLimit := value;
end;


function TThriftConfigurationImpl.GetMaxFrameSize: Cardinal;
begin
  result := FMaxFrameSize;
end;


procedure TThriftConfigurationImpl.SetMaxFrameSize(const value: Cardinal);
begin
  FMaxFrameSize := value;
end;


function TThriftConfigurationImpl.GetMaxMessageSize: Cardinal;
begin
  result := FMaxMessageSize;
end;


procedure TThriftConfigurationImpl.SetMaxMessageSize(const value: Cardinal);
begin
  FMaxMessageSize := value;
end;


end.
