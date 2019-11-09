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
unit Thrift.Serializer;

{$I Thrift.Defines.inc}

interface

uses
  {$IFDEF OLD_UNIT_NAMES}
  Classes, Windows, SysUtils,
  {$ELSE}
  System.Classes, Winapi.Windows, System.SysUtils,
  {$ENDIF}
  Thrift.Configuration,
  Thrift.Protocol,
  Thrift.Transport,
  Thrift.Stream;


type
  // Generic utility for easily serializing objects into a byte array or Stream.
  TSerializer = class
  strict private
    FStream    : TMemoryStream;
    FTransport : ITransport;
    FProtocol  : IProtocol;

  public
    constructor Create( const aProtFact  : IProtocolFactory = nil;    // defaults to TBinaryProtocol
                        const aTransFact : ITransportFactory = nil;
                        const aConfig   : IThriftConfiguration = nil);

    // DTOR
    destructor Destroy;  override;

    // Serialize the Thrift object.
    function  Serialize( const input : IBase) : TBytes;  overload;
    procedure Serialize( const input : IBase; const aStm : TStream);  overload;
  end;


  // Generic utility for easily deserializing objects from byte array or Stream.
  TDeserializer = class
  strict private
    FStream    : TMemoryStream;
    FTransport : ITransport;
    FProtocol  : IProtocol;

  public
    constructor Create( const aProtFact  : IProtocolFactory = nil;    // defaults to TBinaryProtocol
                        const aTransFact : ITransportFactory = nil;
                        const aConfig   : IThriftConfiguration = nil);

    // DTOR
    destructor Destroy;  override;

    // Deserialize the Thrift object data.
    procedure Deserialize( const input : TBytes; const target : IBase);  overload;
    procedure Deserialize( const input : TStream; const target : IBase);  overload;
  end;



implementation


{ TSerializer }


constructor TSerializer.Create( const aProtFact  : IProtocolFactory;
                                const aTransFact : ITransportFactory;
                                const aConfig   : IThriftConfiguration);
var adapter : IThriftStream;
    protfact : IProtocolFactory;
begin
  inherited Create;

  FStream    := TMemoryStream.Create;
  adapter    := TThriftStreamAdapterDelphi.Create( FStream, FALSE);

  FTransport := TStreamTransportImpl.Create( nil, adapter, aConfig);
  if aTransfact <> nil then FTransport := aTransfact.GetTransport( FTransport);

  if aProtFact <> nil
  then protfact := aProtFact
  else protfact := TBinaryProtocolImpl.TFactory.Create;
  FProtocol := protfact.GetProtocol( FTransport);

  if not FTransport.IsOpen
  then FTransport.Open;
end;


destructor TSerializer.Destroy;
begin
  try
    FProtocol  := nil;
    FTransport := nil;
    FreeAndNil( FStream);
  finally
    inherited Destroy;
  end;
end;


function TSerializer.Serialize( const input : IBase) : TBytes;
// Serialize the Thrift object into a byte array. The process is simple,
// just clear the byte array output, write the object into it, and grab the
// raw bytes.
var iBytes : Int64;
begin
  try
    FStream.Size := 0;
    input.Write( FProtocol);
    FTransport.Flush;

    SetLength( result, FStream.Size);
    iBytes := Length(result);
    if iBytes > 0
    then Move( FStream.Memory^, result[0], iBytes);
  finally
    FStream.Size := 0;  // free any allocated memory
  end;
end;


procedure TSerializer.Serialize( const input : IBase; const aStm : TStream);
// Serialize the Thrift object into a byte array. The process is simple,
// just clear the byte array output, write the object into it, and grab the
// raw bytes.
const COPY_ENTIRE_STREAM = 0;
begin
  try
    FStream.Size := 0;
    input.Write( FProtocol);
    FTransport.Flush;

    aStm.CopyFrom( FStream, COPY_ENTIRE_STREAM);
  finally
    FStream.Size := 0;  // free any allocated memory
  end;
end;


{ TDeserializer }


constructor TDeserializer.Create( const aProtFact  : IProtocolFactory;
                                  const aTransFact : ITransportFactory;
                                  const aConfig   : IThriftConfiguration);
var adapter : IThriftStream;
    protfact : IProtocolFactory;
begin
  inherited Create;

  FStream    := TMemoryStream.Create;
  adapter    := TThriftStreamAdapterDelphi.Create( FStream, FALSE);

  FTransport := TStreamTransportImpl.Create( adapter, nil, aConfig);
  if aTransfact <> nil then FTransport := aTransfact.GetTransport( FTransport);

  if aProtFact <> nil
  then protfact := aProtFact
  else protfact := TBinaryProtocolImpl.TFactory.Create;
  FProtocol := protfact.GetProtocol( FTransport);

  if not FTransport.IsOpen
  then FTransport.Open;
end;


destructor TDeserializer.Destroy;
begin
  try
    FProtocol  := nil;
    FTransport := nil;
    FreeAndNil( FStream);
  finally
    inherited Destroy;
  end;
end;


procedure TDeserializer.Deserialize( const input : TBytes; const target : IBase);
// Deserialize the Thrift object data from the byte array.
var iBytes : Int64;
begin
  try
    iBytes := Length(input);
    FStream.Size := iBytes;
    if iBytes > 0
    then Move( input[0], FStream.Memory^, iBytes);

    target.Read( FProtocol);
  finally
    FStream.Size := 0;  // free any allocated memory
  end;
end;


procedure TDeserializer.Deserialize( const input : TStream; const target : IBase);
// Deserialize the Thrift object data from the byte array.
const COPY_ENTIRE_STREAM = 0;
var before : Int64;
begin
  try
    before := FStream.Position;
    FStream.CopyFrom( input, COPY_ENTIRE_STREAM);
    FStream.Position := before;
    target.Read( FProtocol);
  finally
    FStream.Size := 0;  // free any allocated memory
  end;
end;


end.

