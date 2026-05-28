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

unit UnitTestTransportFactory;

interface

uses
  Windows, Classes, SysUtils,
  Thrift.Collections,
  Thrift.Configuration,
  Thrift.Test,
  Thrift.Protocol,
  Thrift.Protocol.JSON,
  Thrift.Protocol.Compact,
  Thrift.Transport,
  Thrift.Stream,
  ConsoleHelper,
  TestConstants;

type
  TTransportProtocolStack = record
    Proto : IProtocol;
    Trans : ITransport;
  end;


function MakeTransportProtocolStack( const ptyp : TKnownProtocol;
                                     const layered : TLayeredTransport;
                                     const aConfig : IThriftConfiguration;
                                     const forWrite : Boolean;
                                     var buffer : TMemoryStream) : TTransportProtocolStack;


implementation


function MakeTransportProtocolStack( const ptyp : TKnownProtocol; const layered : TLayeredTransport;
                                     const aConfig : IThriftConfiguration; const forWrite : Boolean;
                                     var buffer : TMemoryStream) : TTransportProtocolStack;
var newBuf : TMemoryStream;
    stream : IThriftStream;
    trans  : IStreamTransport;
const COPY_ENTIRE_STREAM = 0;
begin
  // read happens after write here, so let's take over the written bytes
  newBuf := TMemoryStream.Create;
  if Assigned(buffer) and not forWrite
  then newBuf.CopyFrom( buffer, COPY_ENTIRE_STREAM);
  // NEVER FreeAndNil(buffer);  -> stream owned by the old transport = refcounted
  buffer := newBuf;
  buffer.Position := 0;

  //  layered transports anyone?
  stream := TThriftStreamAdapterDelphi.Create( newBuf, TRUE);
  if forWrite
  then trans := TStreamTransportImpl.Create( nil, stream, aConfig)
  else trans := TStreamTransportImpl.Create( stream, nil, aConfig);
  case layered of
    trns_Framed   :  result.Trans := TFramedTransportImpl.Create( trans);
    trns_Buffered :  result.Trans := TBufferedTransportImpl.Create( trans);
  else
    result.Trans := trans;
  end;

  if not result.Trans.IsOpen
  then result.Trans.Open;

  case ptyp of
    prot_Binary  :  result.Proto := TBinaryProtocolImpl.Create(trans);
    prot_Compact :  result.Proto := TCompactProtocolImpl.Create(trans);
    prot_JSON    :  result.Proto := TJSONProtocolImpl.Create(trans);
  else
    ASSERT(FALSE);
    raise Exception.Create('Unrecognized protocol type');
  end;
end;




end.

