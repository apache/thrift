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

program TestJSONSizeLimit;

{$APPTYPE CONSOLE}

// Unit tests for the per-field size bound in TJSONProtocolImpl.ReadJSONString
// and TJSONProtocolImpl.ReadJSONNumericChars.
//
// The transport stack is built explicitly as
//     memory stream -> TStreamTransportImpl -> TBufferedTransportImpl -> TJSONProtocolImpl
// on purpose: the buffered composition reads from its own input buffer and does
// not draw down the endpoint transport's remaining-message-size counter, so it is
// the only composition on which the missing per-field bound is observable. On a
// plain stream/socket transport the endpoint counter already rejects the oversized
// input, which would mask whether the reader itself enforces the bound.

uses
  SysUtils,
  Classes,
  Thrift.Configuration,
  Thrift.Transport,
  Thrift.Protocol,
  Thrift.Protocol.JSON,
  Thrift.Stream;

type
  TReadKind = ( rkString, rkNumber);

var
  gFailures : Integer = 0;


function BuildBufferedJSONProtocol( const data : TBytes; const maxSize : Integer) : IProtocol;
// memory stream -> TStreamTransportImpl -> TBufferedTransportImpl -> TJSONProtocolImpl
var mem    : TMemoryStream;
    stm    : IThriftStream;
    config : IThriftConfiguration;
    strans : IStreamTransport;
    btrans : ITransport;
begin
  mem := TMemoryStream.Create;
  if Length(data) > 0
  then mem.WriteBuffer( data[0], Length(data));
  mem.Position := 0;
  stm := TThriftStreamAdapterDelphi.Create( mem, TRUE);  // adapter owns the stream

  config := TThriftConfigurationImpl.Create;
  config.MaxMessageSize := maxSize;

  strans := TStreamTransportImpl.Create( stm, nil, config);
  btrans := TBufferedTransportImpl.Create( strans);
  result := TJSONProtocolImpl.Create( btrans);
end;


procedure PerformRead( const proto : IProtocol; const kind : TReadKind);
begin
  case kind of
    rkString : proto.ReadString;
    rkNumber : proto.ReadI64;
  else
    raise Exception.Create('unhandled read kind');
  end;
end;


procedure ExpectAccept( const title : string; const data : TBytes; const maxSize : Integer; const kind : TReadKind);
var proto : IProtocol;
begin
  proto := BuildBufferedJSONProtocol( data, maxSize);
  try
    PerformRead( proto, kind);
    Writeln('PASS  '+title+' (read completed, no exception)');
  except
    on e : Exception do begin
      Inc( gFailures);
      Writeln('FAIL  '+title+' (unexpected '+e.ClassName+': '+e.Message+')');
    end;
  end;
end;


procedure ExpectSizeReject( const title : string; const data : TBytes; const maxSize : Integer; const kind : TReadKind);
// success == a TTransportException (the message-size bound) is raised.
// A clean return means the reader did not enforce the bound (the pre-fix behaviour).
// Any non-transport exception means something other than the size bound stopped us,
// which also means the size bound did not fire.
var proto : IProtocol;
begin
  proto := BuildBufferedJSONProtocol( data, maxSize);
  try
    PerformRead( proto, kind);
    Inc( gFailures);
    Writeln('FAIL  '+title+' (expected TTransportException, none raised)');
  except
    on e : TTransportException do
      Writeln('PASS  '+title+' (rejected: '+e.ClassName+')');
    on e : Exception do begin
      Inc( gFailures);
      Writeln('FAIL  '+title+' (size bound did not fire; got '+e.ClassName+': '+e.Message+')');
    end;
  end;
end;


function RepeatByte( const b : Byte; const count : Integer) : TBytes;
var i : Integer;
begin
  SetLength( result, count);
  for i := 0 to count-1 do result[i] := b;
end;


function QuotedString( const bodyLen : Integer) : TBytes;
// a JSON string literal: '"' + bodyLen x 'A' + '"'
var i : Integer;
begin
  SetLength( result, bodyLen + 2);
  result[0] := Byte('"');
  for i := 1 to bodyLen do result[i] := Byte('A');
  result[bodyLen+1] := Byte('"');
end;


function NumericLiteral( const digits : Integer) : TBytes;
// a JSON numeric run followed by a non-numeric terminator, so the *unpatched*
// numeric loop returns cleanly on the terminator (rather than on stream
// exhaustion) and the test isolates the size bound.
var i : Integer;
begin
  SetLength( result, digits + 1);
  for i := 0 to digits-1 do result[i] := Byte('1');
  result[digits] := Byte(' ');  // non-numeric terminator
end;


begin
  try
    Writeln('TJSONProtocol per-field size bound on the buffered transport composition');
    Writeln('------------------------------------------------------------------------');

    // 1. within limit is still accepted
    ExpectAccept( 'string within limit (512 bytes, max 4096)',
                  QuotedString( 512), 4096, rkString);

    // 2. string over limit is rejected
    ExpectSizeReject( 'string over limit (4096 bytes, max 1024)',
                      QuotedString( 4096), 1024, rkString);

    // 3. number over limit is rejected
    ExpectSizeReject( 'number over limit (4096 digits, max 1024)',
                      NumericLiteral( 4096), 1024, rkNumber);

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
