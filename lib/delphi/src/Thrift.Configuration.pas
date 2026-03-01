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
    ['{666F7848-744A-4746-BDD5-43DC9B1D5520}']
    function  GetRecursionLimit : Integer;
    procedure SetRecursionLimit( const value : Integer);
    function  GetMaxFrameSize : Integer;
    procedure SetMaxFrameSize( const value : Integer);
    function  GetMaxMessageSize : Integer;
    procedure SetMaxMessageSize( const value : Integer);

    property  RecursionLimit : Integer read GetRecursionLimit write SetRecursionLimit;
    property  MaxFrameSize : Integer   read GetMaxFrameSize   write SetMaxFrameSize;
    property  MaxMessageSize : Integer read GetMaxMessageSize write SetMaxMessageSize;
  end;


  TThriftConfigurationImpl = class( TInterfacedObject, IThriftConfiguration)
  strict private
    class procedure ValidateLimitArgument( const value : Integer); inline;

  strict protected
    FRecursionLimit : Cardinal;
    FMaxFrameSize   : Cardinal;
    FMaxMessageSize : Cardinal;

    // IThriftConfiguration
    function  GetRecursionLimit : Integer;
    procedure SetRecursionLimit( const value : Integer);
    function  GetMaxFrameSize : Integer;
    procedure SetMaxFrameSize( const value : Integer);
    function  GetMaxMessageSize : Integer;
    procedure SetMaxMessageSize( const value : Integer);

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


class procedure TThriftConfigurationImpl.ValidateLimitArgument( const value : Integer);
begin
  if value <= 0  // zero makes not much sense either
  then raise EArgumentOutOfRangeException.Create('Value must be positive');
end;


function TThriftConfigurationImpl.GetRecursionLimit: Integer;
begin
  result := FRecursionLimit and MAXINT;
  ASSERT( result > 0);
end;


procedure TThriftConfigurationImpl.SetRecursionLimit(const value: Integer);
begin
  ValidateLimitArgument( value);
  ASSERT( value > 0);
  FRecursionLimit := value and MAXINT;
end;


function TThriftConfigurationImpl.GetMaxFrameSize: Integer;
begin
  result := FMaxFrameSize and MAXINT;
  ASSERT( result > 0);
end;


procedure TThriftConfigurationImpl.SetMaxFrameSize(const value: Integer);
begin
  ValidateLimitArgument( value);
  ASSERT( value > 0);
  FMaxFrameSize := value and MAXINT;
end;


function TThriftConfigurationImpl.GetMaxMessageSize: Integer;
begin
  result := FMaxMessageSize and MAXINT;
  ASSERT( result > 0);
end;


procedure TThriftConfigurationImpl.SetMaxMessageSize(const value: Integer);
begin
  ValidateLimitArgument( value);
  ASSERT( value > 0);
  FMaxMessageSize := value and MAXINT;
end;


end.
