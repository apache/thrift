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

program TestSerializer;

{$APPTYPE CONSOLE}

uses
  Classes, Windows, SysUtils, Generics.Collections,
  Thrift in '..\..\src\Thrift.pas',
  Thrift.Exception in '..\..\src\Thrift.Exception.pas',
  Thrift.Socket in '..\..\src\Thrift.Socket.pas',
  Thrift.Transport in '..\..\src\Thrift.Transport.pas',
  Thrift.Protocol in '..\..\src\Thrift.Protocol.pas',
  Thrift.Protocol.JSON in '..\..\src\Thrift.Protocol.JSON.pas',
  Thrift.Protocol.Compact in '..\..\src\Thrift.Protocol.Compact.pas',
  Thrift.Collections in '..\..\src\Thrift.Collections.pas',
  Thrift.Server in '..\..\src\Thrift.Server.pas',
  Thrift.Utils in '..\..\src\Thrift.Utils.pas',
  Thrift.Serializer in '..\..\src\Thrift.Serializer.pas',
  Thrift.Stream in '..\..\src\Thrift.Stream.pas',
  Thrift.WinHTTP in '..\..\src\Thrift.WinHTTP.pas',
  Thrift.TypeRegistry in '..\..\src\Thrift.TypeRegistry.pas',
  System_,
  DebugProtoTest,
  TestSerializer.Data;



type
  TTestSerializer = class //extends TestCase {
  private type
    TMethod = (
      mt_Bytes,
      mt_Stream
    );

  private
    FProtocols : TList< IProtocolFactory>;

    class function  Serialize(const input : IBase; const factory : IProtocolFactory) : TBytes;  overload;
    class procedure Serialize(const input : IBase; const factory : IProtocolFactory; const aStream : TStream);  overload;
    class procedure Deserialize( const input : TBytes; const target : IBase; const factory : IProtocolFactory);  overload;
    class procedure Deserialize( const input : TStream; const target : IBase; const factory : IProtocolFactory);  overload;

    procedure Test_Serializer_Deserializer;
    procedure Test_OneOfEach(     const method : TMethod; const factory : IProtocolFactory; const stream : TFileStream);
    procedure Test_CompactStruct( const method : TMethod; const factory : IProtocolFactory; const stream : TFileStream);

  public
    constructor Create;
    destructor Destroy;  override;

    procedure RunTests;
  end;



{ TTestSerializer }

constructor TTestSerializer.Create;
begin
  inherited Create;
  FProtocols := TList< IProtocolFactory>.Create;
  FProtocols.Add( TBinaryProtocolImpl.TFactory.Create);
  FProtocols.Add( TCompactProtocolImpl.TFactory.Create);
  FProtocols.Add( TJSONProtocolImpl.TFactory.Create);
end;


destructor TTestSerializer.Destroy;
begin
  try
    FreeAndNil( FProtocols);
  finally
    inherited Destroy;
  end;
end;


procedure TTestSerializer.Test_OneOfEach( const method : TMethod; const factory : IProtocolFactory; const stream : TFileStream);
var tested, correct : IOneOfEach;
    bytes   : TBytes;
    i : Integer;
begin
  // write
  tested := Fixtures.CreateOneOfEach;
  case method of
    mt_Bytes:  bytes := Serialize( tested, factory);
    mt_Stream: begin
      stream.Size := 0;
      Serialize( tested, factory, stream);
    end
  else
    ASSERT( FALSE);
  end;

  // init + read
  tested := TOneOfEachImpl.Create;
  case method of
    mt_Bytes:  Deserialize( bytes, tested, factory);
    mt_Stream: begin
      stream.Position := 0;
      Deserialize( stream, tested, factory);
    end
  else
    ASSERT( FALSE);
  end;

  // check
  correct := Fixtures.CreateOneOfEach;
  ASSERT( tested.Im_true = correct.Im_true);
  ASSERT( tested.Im_false = correct.Im_false);
  ASSERT( tested.A_bite = correct.A_bite);
  ASSERT( tested.Integer16 = correct.Integer16);
  ASSERT( tested.Integer32 = correct.Integer32);
  ASSERT( tested.Integer64 = correct.Integer64);
  ASSERT( Abs( tested.Double_precision - correct.Double_precision) < 1E-12);
  ASSERT( tested.Some_characters = correct.Some_characters);
  ASSERT( tested.Zomg_unicode = correct.Zomg_unicode);
  ASSERT( tested.What_who = correct.What_who);

  ASSERT( Length(tested.Base64) = Length(correct.Base64));
  ASSERT( CompareMem( @tested.Base64[0], @correct.Base64[0], Length(correct.Base64)));

  ASSERT( tested.Byte_list.Count = correct.Byte_list.Count);
  for i := 0 to tested.Byte_list.Count-1
  do ASSERT( tested.Byte_list[i] = correct.Byte_list[i]);

  ASSERT( tested.I16_list.Count = correct.I16_list.Count);
  for i := 0 to tested.I16_list.Count-1
  do ASSERT( tested.I16_list[i] = correct.I16_list[i]);

  ASSERT( tested.I64_list.Count = correct.I64_list.Count);
  for i := 0 to tested.I64_list.Count-1
  do ASSERT( tested.I64_list[i] = correct.I64_list[i]);
end;


procedure TTestSerializer.Test_CompactStruct( const method : TMethod; const factory : IProtocolFactory; const stream : TFileStream);
var tested, correct : ICompactProtoTestStruct;
    bytes   : TBytes;
begin
  // write
  tested := Fixtures.CreateCompactProtoTestStruct;
  case method of
    mt_Bytes:  bytes := Serialize( tested, factory);
    mt_Stream: begin
      stream.Size := 0;
      Serialize( tested, factory, stream);
    end
  else
    ASSERT( FALSE);
  end;

  // init + read
  correct := TCompactProtoTestStructImpl.Create;
  case method of
    mt_Bytes:  Deserialize( bytes, tested, factory);
    mt_Stream: begin
      stream.Position := 0;
      Deserialize( stream, tested, factory);
    end
  else
    ASSERT( FALSE);
  end;

  // check
  correct := Fixtures.CreateCompactProtoTestStruct;
  ASSERT( correct.Field500  = tested.Field500);
  ASSERT( correct.Field5000  = tested.Field5000);
  ASSERT( correct.Field20000 = tested.Field20000);
end;


procedure TTestSerializer.Test_Serializer_Deserializer;
var factory : IProtocolFactory;
    stream  : TFileStream;
    method  : TMethod;
begin
  stream  := TFileStream.Create( 'TestSerializer.dat', fmCreate);
  try

    for method in [Low(TMethod)..High(TMethod)] do begin
      for factory in FProtocols do begin

        Test_OneOfEach(     method, factory, stream);
        Test_CompactStruct( method, factory, stream);
      end;
    end;

  finally
    stream.Free;
  end;
end;


procedure TTestSerializer.RunTests;
begin
  try
    Test_Serializer_Deserializer;
  except
    on e:Exception do begin
      Writeln( e.Message);
      Write('Hit ENTER to close ... '); Readln;
    end;
  end;
end;


class function TTestSerializer.Serialize(const input : IBase; const factory : IProtocolFactory) : TBytes;
var serial : TSerializer;
begin
  serial := TSerializer.Create( factory);
  try
    result := serial.Serialize( input);
  finally
    serial.Free;
  end;
end;


class procedure TTestSerializer.Serialize(const input : IBase; const factory : IProtocolFactory; const aStream : TStream);
var serial : TSerializer;
begin
  serial := TSerializer.Create( factory);
  try
    serial.Serialize( input, aStream);
  finally
    serial.Free;
  end;
end;


class procedure TTestSerializer.Deserialize( const input : TBytes; const target : IBase; const factory : IProtocolFactory);
var serial : TDeserializer;
begin
  serial := TDeserializer.Create( factory);
  try
    serial.Deserialize( input, target);
  finally
    serial.Free;
  end;
end;

class procedure TTestSerializer.Deserialize( const input : TStream; const target : IBase; const factory : IProtocolFactory);
var serial : TDeserializer;
begin
  serial := TDeserializer.Create( factory);
  try
    serial.Deserialize( input, target);
  finally
    serial.Free;
  end;
end;


var test : TTestSerializer;
begin
  test := TTestSerializer.Create;
  try
    test.RunTests;
  finally
    test.Free;
  end;
end.

