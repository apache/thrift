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

program TestBufferedMsgSizeLimit;

{$APPTYPE CONSOLE}

// Transport-level tests for the cumulative MaxMessageSize bound on the buffered
// transport composition:
//     memory stream -> TStreamTransportImpl(config) -> TBufferedTransportImpl
//
// The endpoint transport (TStreamTransportImpl) draws its message-size budget
// down inside its own Read(). A buffered transport that refills its read buffer
// straight from the endpoint's raw input stream bypasses that Read(), so the
// budget is never consumed and a peer can read an unbounded *total* as long as
// each individual read stays under the limit. These tests read past the limit
// on the buffered composition and assert the budget is enforced.

uses
  SysUtils,
  Classes,
  Math,
  Thrift.Configuration,
  Thrift.Transport,
  Thrift.Stream;

var
  gFailures : Integer = 0;


function BuildBufferedTransport( const totalBytes, maxSize, bufSize : Integer) : ITransport;
// memory stream (totalBytes) -> TStreamTransportImpl(maxSize) -> TBufferedTransportImpl(bufSize)
var mem    : TMemoryStream;
    stm    : IThriftStream;
    config : IThriftConfiguration;
    strans : IStreamTransport;
    data   : TBytes;
    i      : Integer;
begin
  SetLength( data, totalBytes);
  for i := 0 to totalBytes-1 do data[i] := Byte( Ord('A') + (i mod 26));

  mem := TMemoryStream.Create;
  if totalBytes > 0 then mem.WriteBuffer( data[0], totalBytes);
  mem.Position := 0;
  stm := TThriftStreamAdapterDelphi.Create( mem, TRUE);  // adapter owns the stream

  config := TThriftConfigurationImpl.Create;
  config.MaxMessageSize := maxSize;

  strans := TStreamTransportImpl.Create( stm, nil, config);
  result := TBufferedTransportImpl.Create( strans, bufSize);
end;


function TryReadTotal( const trans : ITransport; const wantTotal, chunk : Integer; out bytesRead : Integer) : Boolean;
// Reads wantTotal bytes in `chunk`-sized ReadAll calls. Returns TRUE if a
// TTransportException was raised (and how far we got in bytesRead), FALSE if the
// whole wantTotal was read with no exception. wantTotal never exceeds the stream
// length, so a raise here is the size budget, not end-of-file.
var buf : TBytes;
    n   : Integer;
begin
  SetLength( buf, chunk);
  bytesRead := 0;
  result := FALSE;
  try
    while bytesRead < wantTotal do begin
      n := Min( chunk, wantTotal - bytesRead);
      trans.ReadAll( buf, 0, n);
      Inc( bytesRead, n);
    end;
  except
    on e : TTransportException do result := TRUE;
  end;
end;


procedure ExpectAcceptedWithinLimit( const title : string; const totalBytes, maxSize : Integer);
var trans : ITransport;
    got   : Integer;
    threw : Boolean;
begin
  trans := BuildBufferedTransport( totalBytes, maxSize, 512);
  threw := TryReadTotal( trans, totalBytes, 128, got);
  if (not threw) and (got = totalBytes)
  then Writeln('PASS  '+title+' (read '+IntToStr(got)+' bytes, no exception)')
  else begin
    Inc( gFailures);
    Writeln('FAIL  '+title+' (threw='+BoolToStr(threw,TRUE)+', read '+IntToStr(got)+'/'+IntToStr(totalBytes)+')');
  end;
end;


procedure ExpectRejectedOverLimit( const title : string; const totalBytes, maxSize : Integer);
var trans : ITransport;
    got   : Integer;
    threw : Boolean;
begin
  trans := BuildBufferedTransport( totalBytes, maxSize, 512);
  threw := TryReadTotal( trans, totalBytes, 128, got);
  if threw and (got < totalBytes) and (got <= maxSize + 512)
  then Writeln('PASS  '+title+' (rejected after '+IntToStr(got)+' bytes, limit '+IntToStr(maxSize)+')')
  else begin
    Inc( gFailures);
    Writeln('FAIL  '+title+' (expected size-limit rejection; threw='+BoolToStr(threw,TRUE)+', read '+IntToStr(got)+'/'+IntToStr(totalBytes)+')');
  end;
end;


begin
  try
    Writeln('Cumulative MaxMessageSize bound on the buffered transport composition');
    Writeln('--------------------------------------------------------------------');

    // 1. total within the limit is accepted
    ExpectAcceptedWithinLimit( 'total within limit (1024 bytes, max 4096)', 1024, 4096);

    // 2. total exactly at the limit is accepted (no off-by-one false-reject)
    ExpectAcceptedWithinLimit( 'total exactly at limit (1024 bytes, max 1024)', 1024, 1024);

    // 3. total over the limit is rejected (each read is small; the cumulative total is not)
    ExpectRejectedOverLimit( 'total over limit (4096 bytes, max 1024)', 4096, 1024);

    Writeln('--------------------------------------------------------------------');
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
