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

program TestNegativeSizes;

{$APPTYPE CONSOLE}

// Negative string and container sizes read from the wire must be rejected by
// CheckReadBytesAvailable() on every transport composition:
//     (a) memory stream -> TStreamTransportImpl(config)
//     (b) memory stream -> TStreamTransportImpl(config) -> TBufferedTransportImpl
//     (c) memory stream -> TStreamTransportImpl(config) -> TFramedTransportImpl
//
// The endpoint transport rejects a negative size. The layered transports must
// pass such a size on to it instead of treating it as already satisfied by the
// bytes they hold. Each read runs with and without trailing bytes, so that the
// layered read buffer is empty in one run and not empty in the other.

uses
  SysUtils,
  Classes,
  Thrift.Configuration,
  Thrift.Transport,
  Thrift.Stream,
  Thrift.Protocol,
  Thrift.Protocol.Compact;

type
  TStack = (stPlain, stBuffered, stFramed);
  TRead = (rdBinaryString, rdBinaryList, rdCompactString);

const
  STACK_NAMES : array[TStack] of string = ('plain', 'buffered', 'framed');
  READ_NAMES : array[TRead] of string = ('binary string', 'binary list', 'compact string');
  TRAILING_BYTES = 8;

var
  gFailures : Integer = 0;


function Payload( const read : TRead) : TBytes;
begin
  case read of
    rdBinaryString  : result := TBytes.Create( $FF, $FF, $FF, $FF);            // length -1
    rdBinaryList    : result := TBytes.Create( $08, $FF, $FF, $FF, $FF);       // i32 elements, count -1
    rdCompactString : result := TBytes.Create( $FF, $FF, $FF, $FF, $0F);       // varint, Integer(...) = -1
  else
    raise Exception.Create('unhandled read');
  end;
end;


function BuildTransport( const stack : TStack; const data : TBytes) : ITransport;
var mem    : TMemoryStream;
    stm    : IThriftStream;
    config : IThriftConfiguration;
    strans : IStreamTransport;
    header : TBytes;
    size   : Integer;
begin
  mem := TMemoryStream.Create;
  if stack = stFramed then begin
    size := Length(data);
    header := TBytes.Create( Byte(size shr 24), Byte(size shr 16), Byte(size shr 8), Byte(size));
    mem.WriteBuffer( header[0], Length(header));
  end;
  if Length(data) > 0 then mem.WriteBuffer( data[0], Length(data));
  mem.Position := 0;
  stm := TThriftStreamAdapterDelphi.Create( mem, TRUE);  // adapter owns the stream

  config := TThriftConfigurationImpl.Create;
  config.MaxMessageSize := 4096;

  strans := TStreamTransportImpl.Create( stm, nil, config);
  case stack of
    stPlain    : result := strans;
    stBuffered : result := TBufferedTransportImpl.Create( strans, 512);
    stFramed   : result := TFramedTransportImpl.Create( strans);
  else
    raise Exception.Create('unhandled stack');
  end;
end;


function DoRead( const read : TRead; const trans : ITransport) : string;
// performs the read and describes its result, if it returns at all
var bytes : TBytes;
    list  : TThriftList;
begin
  case read of
    rdBinaryString : begin
      bytes := TBinaryProtocolImpl.Create( trans).ReadBinary;
      result := 'returned '+IntToStr(Length(bytes))+' bytes';
    end;
    rdBinaryList : begin
      list := TBinaryProtocolImpl.Create( trans).ReadListBegin;
      result := 'returned count '+IntToStr(list.Count);
    end;
    rdCompactString : begin
      bytes := TCompactProtocolImpl.Create( trans).ReadBinary;
      result := 'returned '+IntToStr(Length(bytes))+' bytes';
    end;
  else
    raise Exception.Create('unhandled read');
  end;
end;


procedure ExpectRejected( const stack : TStack; const read : TRead; const trailing : Integer);
var data    : TBytes;
    title   : string;
    outcome : string;
    ok      : Boolean;
begin
  data := Payload( read);
  SetLength( data, Length(data) + trailing);  // new bytes are zero

  title := 'negative '+READ_NAMES[read]+' size is rejected on '+STACK_NAMES[stack]
         + ' ('+IntToStr(trailing)+' trailing bytes)';
  try
    outcome := 'no exception, '+DoRead( read, BuildTransport( stack, data));
    ok := FALSE;
  except
    on e : TTransportExceptionMessageSizeLimit do begin
      outcome := 'rejected: '+e.ClassName;
      ok := TRUE;
    end;
    on e : Exception do begin
      outcome := 'expected TTransportExceptionMessageSizeLimit, got '+e.ClassName+': '+e.Message;
      ok := FALSE;
    end;
  end;

  if ok
  then Writeln('PASS  '+title+' ('+outcome+')')
  else begin
    Inc( gFailures);
    Writeln('FAIL  '+title+' ('+outcome+')');
  end;
end;


var stack : TStack;
    read  : TRead;
begin
  try
    Writeln('Negative read sizes on the transport compositions');
    Writeln('------------------------------------------------------------------------');

    for stack := Low(TStack) to High(TStack) do begin
      for read := Low(TRead) to High(TRead) do begin
        ExpectRejected( stack, read, 0);
        ExpectRejected( stack, read, TRAILING_BYTES);
      end;
    end;

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
