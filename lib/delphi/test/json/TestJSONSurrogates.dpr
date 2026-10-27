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

program TestJSONSurrogates;

{$APPTYPE CONSOLE}

// Unit tests for the UTF-16 surrogate pairing checks on \uXXXX escapes
// in TJSONProtocolImpl.ReadJSONString.
//
// The transport stack is
//     memory stream -> TStreamTransportImpl -> TJSONProtocolImpl
// The surrogate checks do not depend on the transport composition.

uses
  SysUtils,
  Classes,
  Thrift.Configuration,
  Thrift.Transport,
  Thrift.Protocol,
  Thrift.Protocol.JSON,
  Thrift.Stream;

var
  gFailures : Integer = 0;


function BuildJSONProtocol( const data : AnsiString) : IProtocol;
// memory stream -> TStreamTransportImpl -> TJSONProtocolImpl
var mem    : TMemoryStream;
    stm    : IThriftStream;
    config : IThriftConfiguration;
    strans : IStreamTransport;
begin
  mem := TMemoryStream.Create;
  if Length(data) > 0
  then mem.WriteBuffer( data[1], Length(data));
  mem.Position := 0;
  stm := TThriftStreamAdapterDelphi.Create( mem, TRUE);  // adapter owns the stream

  config := TThriftConfigurationImpl.Create;
  strans := TStreamTransportImpl.Create( stm, nil, config);
  result := TJSONProtocolImpl.Create( strans);
end;


function HexBytes( const bytes : TBytes) : string;
var i : Integer;
begin
  result := '';
  for i := 0 to Length(bytes)-1 do begin
    if i > 0 then result := result + ' ';
    result := result + IntToHex( bytes[i], 2);
  end;
end;


procedure ExpectAccept( const title : string; const data : AnsiString; const expected : TBytes);
// success == the string is read and its UTF-8 bytes match the expected ones
var proto  : IProtocol;
    actual : TBytes;
begin
  proto := BuildJSONProtocol( data);
  try
    actual := TEncoding.UTF8.GetBytes( proto.ReadString);
    if HexBytes( actual) = HexBytes( expected)
    then Writeln('PASS  '+title+' (bytes '+HexBytes( actual)+')')
    else begin
      Inc( gFailures);
      Writeln('FAIL  '+title+' (expected bytes '+HexBytes( expected)+', got '+HexBytes( actual)+')');
    end;
  except
    on e : Exception do begin
      Inc( gFailures);
      Writeln('FAIL  '+title+' (unexpected '+e.ClassName+': '+e.Message+')');
    end;
  end;
end;


procedure ExpectInvalidData( const title : string; const data : AnsiString);
// success == a TProtocolExceptionInvalidData is raised.
// A clean return or any other exception type means the input was not rejected as invalid data.
var proto : IProtocol;
begin
  proto := BuildJSONProtocol( data);
  try
    proto.ReadString;
    Inc( gFailures);
    Writeln('FAIL  '+title+' (expected TProtocolExceptionInvalidData, none raised)');
  except
    on e : TProtocolExceptionInvalidData do
      Writeln('PASS  '+title+' (rejected: '+e.ClassName+')');
    on e : Exception do begin
      Inc( gFailures);
      Writeln('FAIL  '+title+' (expected TProtocolExceptionInvalidData, got '+e.ClassName+': '+e.Message+')');
    end;
  end;
end;


begin
  try
    Writeln('TJSONProtocol surrogate escapes');
    Writeln('------------------------------------------------------------------------');

    // 1. a valid pair is accepted and decodes to U+1F600
    ExpectAccept( 'valid surrogate pair is accepted',
                  '"\ud83d\ude00"', TBytes.Create( $F0, $9F, $98, $80));

    // 2. low surrogate without a preceding high surrogate
    ExpectInvalidData( 'unpaired low surrogate is rejected',
                       '"\udc00"');

    // 3. same, surrounded by regular chars
    ExpectInvalidData( 'unpaired low surrogate between regular chars is rejected',
                       '"a\udc00b"');

    // 4. high surrogate at the end of the string
    ExpectInvalidData( 'unpaired high surrogate at end of string is rejected',
                       '"\ud800"');

    // 5. high surrogate followed by another high surrogate
    ExpectInvalidData( 'high surrogate followed by high surrogate is rejected',
                       '"\ud800\ud800"');

    Writeln('------------------------------------------------------------------------');
    if gFailures = 0
    then Writeln('RESULT: all checks passed')
    else Writeln('RESULT: '+IntToStr(gFailures)+' check(s) failed');

  except
    on e : Exception do begin
      Inc( gFailures);
      Writeln('FATAL '+e.ClassName+': '+e.Message);
    end;
  end;

  ExitCode := gFailures;
end.
