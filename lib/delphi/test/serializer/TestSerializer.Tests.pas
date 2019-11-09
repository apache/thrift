unit TestSerializer.Tests;
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

interface

uses
  Classes,
  Windows,
  SysUtils,
  Generics.Collections,
  Thrift,
  Thrift.Exception,
  Thrift.Socket,
  Thrift.Transport,
  Thrift.Protocol,
  Thrift.Protocol.JSON,
  Thrift.Protocol.Compact,
  Thrift.Collections,
  Thrift.Configuration,
  Thrift.Server,
  Thrift.Utils,
  Thrift.Serializer,
  Thrift.Stream,
  Thrift.WinHTTP,
  Thrift.TypeRegistry,
  System_,
  DebugProtoTest,
  TestSerializer.Data;


type
  TFactoryPair = record
    prot : IProtocolFactory;
    trans : ITransportFactory;
  end;

  TTestSerializer = class //extends TestCase {
  private type
    TMethod = (
      mt_Bytes,
      mt_Stream
    );

  private
    FProtocols : TList< TFactoryPair>;
    procedure AddFactoryCombination( const aProto : IProtocolFactory; const aTrans : ITransportFactory);
    class function UserFriendlyName( const factory : TFactoryPair) : string;  overload;
    class function UserFriendlyName( const method : TMethod) : string;  overload;

    class function  Serialize(const input : IBase; const factory : TFactoryPair) : TBytes;  overload;
    class procedure Serialize(const input : IBase; const factory : TFactoryPair; const aStream : TStream);  overload;

    class procedure Deserialize( const input : TBytes; const target : IBase; const factory : TFactoryPair);  overload;
    class procedure Deserialize( const input : TStream; const target : IBase; const factory : TFactoryPair);  overload;

    class procedure ValidateReadToEnd( const input : TBytes; const serial : TDeserializer);  overload;
    class procedure ValidateReadToEnd( const input : TStream; const serial : TDeserializer);  overload;

    procedure Test_Serializer_Deserializer;
    procedure Test_OneOfEach(     const method : TMethod; const factory : TFactoryPair; const stream : TFileStream);
    procedure Test_CompactStruct( const method : TMethod; const factory : TFactoryPair; const stream : TFileStream);

  public
    constructor Create;
    destructor Destroy;  override;

    procedure RunTests;
  end;


implementation


{ TTestSerializer }

constructor TTestSerializer.Create;
begin
  inherited Create;
  FProtocols := TList< TFactoryPair>.Create;

  AddFactoryCombination( TBinaryProtocolImpl.TFactory.Create, nil);
  AddFactoryCombination( TCompactProtocolImpl.TFactory.Create, nil);
  AddFactoryCombination( TJSONProtocolImpl.TFactory.Create, nil);

  AddFactoryCombination( TBinaryProtocolImpl.TFactory.Create, TFramedTransportImpl.TFactory.Create);
  AddFactoryCombination( TCompactProtocolImpl.TFactory.Create, TFramedTransportImpl.TFactory.Create);
  AddFactoryCombination( TJSONProtocolImpl.TFactory.Create, TFramedTransportImpl.TFactory.Create);

  AddFactoryCombination( TBinaryProtocolImpl.TFactory.Create, TBufferedTransportImpl.TFactory.Create);
  AddFactoryCombination( TCompactProtocolImpl.TFactory.Create, TBufferedTransportImpl.TFactory.Create);
  AddFactoryCombination( TJSONProtocolImpl.TFactory.Create, TBufferedTransportImpl.TFactory.Create);
end;


destructor TTestSerializer.Destroy;
begin
  try
    FreeAndNil( FProtocols);
  finally
    inherited Destroy;
  end;
end;


procedure TTestSerializer.AddFactoryCombination( const aProto : IProtocolFactory; const aTrans : ITransportFactory);
var rec : TFactoryPair;
begin
  rec.prot  := aProto;
  rec.trans := aTrans;
  FProtocols.Add( rec);
end;


procedure TTestSerializer.Test_OneOfEach( const method : TMethod; const factory : TFactoryPair; const stream : TFileStream);
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


procedure TTestSerializer.Test_CompactStruct( const method : TMethod; const factory : TFactoryPair; const stream : TFileStream);
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
var factory : TFactoryPair;
    stream  : TFileStream;
    method  : TMethod;
begin
  stream  := TFileStream.Create( 'TestSerializer.dat', fmCreate);
  try
    for method in [Low(TMethod)..High(TMethod)] do begin
      Writeln( UserFriendlyName(method));

      for factory in FProtocols do begin
        Writeln('- '+UserFriendlyName(factory));

        Test_OneOfEach(     method, factory, stream);
        Test_CompactStruct( method, factory, stream);
      end;

      Writeln;
    end;

  finally
    stream.Free;
  end;
end;


class function TTestSerializer.UserFriendlyName( const factory : TFactoryPair) : string;
begin
  result := Copy( (factory.prot as TObject).ClassName, 2, MAXINT);

  if factory.trans <> nil
  then result := Copy( (factory.trans as TObject).ClassName, 2, MAXINT) +' '+ result;

  result := StringReplace( result, 'Impl', '', [rfReplaceAll]);
  result := StringReplace( result, 'Transport.TFactory', '', [rfReplaceAll]);
  result := StringReplace( result, 'Protocol.TFactory', '', [rfReplaceAll]);
end;


class function TTestSerializer.UserFriendlyName( const method : TMethod) : string;
begin
  result := EnumUtils<TMethod>.ToString(Ord(method));
  result := StringReplace( result, 'mt_', '', [rfReplaceAll]);
end;


procedure TTestSerializer.RunTests;
begin
  try
    Test_Serializer_Deserializer;
  except
    on e:Exception do begin
      Writeln( e.ClassName+': '+ e.Message);
      Write('Hit ENTER to close ... '); Readln;
    end;
  end;
end;


class function TTestSerializer.Serialize(const input : IBase; const factory : TFactoryPair) : TBytes;
var serial : TSerializer;
    config : IThriftConfiguration;
begin
  config := TThriftConfigurationImpl.Create;
  config.MaxMessageSize := 0;   // we don't read anything here

  serial := TSerializer.Create( factory.prot, factory.trans, config);
  try
    result := serial.Serialize( input);
  finally
    serial.Free;
  end;
end;


class procedure TTestSerializer.Serialize(const input : IBase; const factory : TFactoryPair; const aStream : TStream);
var serial : TSerializer;
    config : IThriftConfiguration;
begin
  config := TThriftConfigurationImpl.Create;
  config.MaxMessageSize := 0;   // we don't read anything here

  serial := TSerializer.Create( factory.prot, factory.trans, config);
  try
    serial.Serialize( input, aStream);
  finally
    serial.Free;
  end;
end;


class procedure TTestSerializer.Deserialize( const input : TBytes; const target : IBase; const factory : TFactoryPair);
var serial : TDeserializer;
    config : IThriftConfiguration;
begin
  config := TThriftConfigurationImpl.Create;
  config.MaxMessageSize := Length(input);

  serial := TDeserializer.Create( factory.prot, factory.trans, config);
  try
    serial.Deserialize( input, target);
    ValidateReadToEnd( input, serial);
  finally
    serial.Free;
  end;
end;


class procedure TTestSerializer.Deserialize( const input : TStream; const target : IBase; const factory : TFactoryPair);
var serial : TDeserializer;
    config : IThriftConfiguration;
begin
  config := TThriftConfigurationImpl.Create;
  config.MaxMessageSize := input.Size;

  serial := TDeserializer.Create( factory.prot, factory.trans, config);
  try
    serial.Deserialize( input, target);
    ValidateReadToEnd( input, serial);
  finally
    serial.Free;
  end;
end;


class procedure TTestSerializer.ValidateReadToEnd( const input : TBytes; const serial : TDeserializer);
// we should not have any more byte to read
var dummy : IBase;
begin
  try
    dummy := TOneOfEachImpl.Create;
    serial.Deserialize( input, dummy);
    raise EInOutError.Create('Expected exception not thrown?');
  except
    on e:TTransportExceptionEndOfFile do {expected};
    on e:Exception do raise; // unexpected
  end;
end;


class procedure TTestSerializer.ValidateReadToEnd( const input : TStream; const serial : TDeserializer);
// we should not have any more byte to read
var dummy : IBase;
begin
  try
    input.Position := 0;
    dummy := TOneOfEachImpl.Create;
    serial.Deserialize( input, dummy);
    raise EInOutError.Create('Expected exception not thrown?');
  except
    on e:TTransportExceptionEndOfFile do {expected};
    on e:Exception do raise; // unexpected
  end;
end;

end.
