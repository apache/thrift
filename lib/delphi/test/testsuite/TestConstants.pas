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

unit TestConstants;

interface

uses SysUtils,
     Thrift.Protocol, Thrift.Protocol.Compact, Thrift.Protocol.JSON;

type
  TKnownProtocol = (
    prot_Binary,  // default binary protocol
    prot_JSON,    // JSON protocol
    prot_Compact
  );

  TServerType = (
    srv_Simple,
    srv_Nonblocking,
    srv_Threadpool,
    srv_Threaded
  );

  TEndpointTransport = (
    trns_Sockets,
    trns_MsxmlHttp,
    trns_WinHttp,
    trns_NamedPipes,
    trns_AnonPipes,
    trns_EvHttp  // as listed on http://thrift.apache.org/test
  );

  TLayeredTransport = (
    trns_None,
    trns_Buffered,
    trns_Framed
  );

  TLayeredTransports = set of TLayeredTransport;

  {$SCOPEDENUMS ON}
  TTestSize = (
    Empty,           // Edge case: the zero-length empty binary
    Normal,          // Fairly small array of usual size (256 bytes)
    ByteArrayTest,   // THRIFT-4454 Large writes/reads may cause range check errors in debug mode
    PipeWriteLimit,  // THRIFT-4372 Pipe write operations across a network are limited to 65,535 bytes per write.
    FifteenMB        // quite a bit of data, but still below the default max frame size
  );
  {$SCOPEDENUMS OFF}



const
  PROTOCOL_CLASSES : array[TKnownProtocol] of TProtocolImplClass = (
    TBinaryProtocolImpl,
    TJSONProtocolImpl,
    TCompactProtocolImpl
  );

const
  SERVER_TYPES : array[TServerType] of string
                  = ('Simple', 'Nonblocking', 'Threadpool', 'Threaded');

  THRIFT_PROTOCOLS : array[TKnownProtocol] of string
                  = ('Binary', 'JSON', 'Compact');

  LAYERED_TRANSPORTS : array[TLayeredTransport] of string
                  = ('None', 'Buffered', 'Framed');

  ENDPOINT_TRANSPORTS : array[TEndpointTransport] of string
                  = ('Sockets', 'Http', 'WinHttp', 'Named Pipes','Anon Pipes', 'EvHttp');

  HUGE_TEST_STRING = 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. '
                   + 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy '
                   + 'eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam '
                   + 'voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit '
                   + 'amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam '
                   + 'nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed '
                   + 'diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet '
                   + 'clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. ';


function BytesToHex( const bytes : TBytes) : string;

function PrepareBinaryData( aRandomDist : Boolean; aSize : TTestSize) : TBytes;


implementation


function BytesToHex( const bytes : TBytes) : string;
var i : Integer;
begin
  result := '';
  for i := Low(bytes) to High(bytes) do begin
    result := result + IntToHex(bytes[i],2);
  end;
end;


function PrepareBinaryData( aRandomDist : Boolean; aSize : TTestSize) : TBytes;
var i : Integer;
begin
  case aSize of
    TTestSize.Empty          : SetLength( result, 0);
    TTestSize.Normal         : SetLength( result, $100);
    TTestSize.ByteArrayTest  : SetLength( result, SizeOf(TByteArray) + 128);
    TTestSize.PipeWriteLimit : SetLength( result, 65535 + 128);
    TTestSize.FifteenMB      : SetLength( result, 15 * 1024 * 1024);
  else
    raise EArgumentException.Create('aSize');
  end;

  ASSERT( Low(result) = 0);
  if Length(result) = 0 then Exit;

  // linear distribution, unless random is requested
  if not aRandomDist then begin
    for i := Low(result) to High(result) do begin
      result[i] := i mod $100;
    end;
    Exit;
  end;

  // random distribution of all 256 values
  FillChar( result[0], Length(result) * SizeOf(result[0]), $0);
  for i := Low(result) to High(result) do begin
    result[i] := Byte( Random($100));
  end;
end;




end.
