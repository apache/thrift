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

program TestJSONDoubles;

{$APPTYPE CONSOLE}

// Unit tests for NaN and the infinities in TJSONProtocolImpl.WriteJSONDouble
// and TJSONProtocolImpl.ReadJSONDouble. The strings on the wire are the ones
// defined in the header comment of lib/cpp/src/thrift/protocol/TJSONProtocol.h.
//
// The transport stack is
//     memory stream -> TStreamTransportImpl -> TJSONProtocolImpl
// The handling of doubles does not depend on the transport composition.

uses
  SysUtils,
  Classes,
  Math,
  Thrift.Configuration,
  Thrift.Transport,
  Thrift.Protocol,
  Thrift.Protocol.JSON,
  Thrift.Stream;

type
  TPosition = ( posListElement, posMapKey);

const
  POSITION_NAME : array[TPosition] of string = ( 'list element', 'map key');

  INVARIANT_CULTURE : TFormatSettings
                    = ( ThousandSeparator: ',';
                        DecimalSeparator: '.');

var
  gFailures : Integer = 0;


function DoubleToStr( const value : Double) : string;
// for the output only, the same on every machine
begin
  result := FloatToStr( value, INVARIANT_CULTURE);
end;


function SameDouble( const a, b : Double) : Boolean;
begin
  if IsNaN(a) or IsNaN(b)
  then result := IsNaN(a) and IsNaN(b)
  else result := (a = b);
end;


function ToJSON( const token : AnsiString; const where : TPosition) : AnsiString;
// the complete JSON text with a single double, given as the text on the wire
begin
  case where of
    posListElement : result := '["dbl",1,'+token+']';
    posMapKey      : result := '["dbl","i32",1,{'+token+':1}]';
  else
    raise Exception.Create('unhandled position');
  end;
end;


function BuildReader( const data : AnsiString) : IProtocol;
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


function BuildWriter( const mem : TMemoryStream) : IProtocol;
// TJSONProtocolImpl -> TStreamTransportImpl -> memory stream
var stm    : IThriftStream;
    config : IThriftConfiguration;
    strans : IStreamTransport;
begin
  stm := TThriftStreamAdapterDelphi.Create( mem, FALSE);  // caller owns the stream

  config := TThriftConfigurationImpl.Create;
  strans := TStreamTransportImpl.Create( nil, stm, config);
  result := TJSONProtocolImpl.Create( strans);
end;


function ReadDoubleAt( const data : AnsiString; const where : TPosition) : Double;
var proto : IProtocol;
begin
  proto := BuildReader( data);
  case where of
    posListElement : begin
      proto.ReadListBegin;
      result := proto.ReadDouble;
      proto.ReadListEnd;
    end;
    posMapKey : begin
      proto.ReadMapBegin;
      result := proto.ReadDouble;
      proto.ReadI32;
      proto.ReadMapEnd;
    end;
  else
    raise Exception.Create('unhandled position');
  end;
end;


function WriteDoubleAt( const value : Double; const where : TPosition) : AnsiString;
var mem   : TMemoryStream;
    proto : IProtocol;
    list  : TThriftList;
    map   : TThriftMap;
begin
  mem := TMemoryStream.Create;
  try
    proto := BuildWriter( mem);
    case where of
      posListElement : begin
        Init( list, TType.Double_, 1);
        proto.WriteListBegin( list);
        proto.WriteDouble( value);
        proto.WriteListEnd;
      end;
      posMapKey : begin
        Init( map, TType.Double_, TType.I32, 1);
        proto.WriteMapBegin( map);
        proto.WriteDouble( value);
        proto.WriteI32( 1);
        proto.WriteMapEnd;
      end;
    else
      raise Exception.Create('unhandled position');
    end;
    proto := nil;

    SetString( result, PAnsiChar(mem.Memory), mem.Size);
  finally
    mem.Free;
  end;
end;


procedure ExpectWrite( const value : Double; const token : AnsiString; const where : TPosition);
// success == the JSON text has exactly this token at the given position
var title    : string;
    actual   : AnsiString;
    expected : AnsiString;
begin
  title := 'write '+string(token)+' as '+POSITION_NAME[where];
  try
    expected := ToJSON( token, where);
    actual   := WriteDoubleAt( value, where);
    if actual = expected
    then Writeln('PASS  '+title)
    else begin
      Inc( gFailures);
      Writeln('FAIL  '+title+' (expected '+string(expected)+', got '+string(actual)+')');
    end;
  except
    on e : Exception do begin
      Inc( gFailures);
      Writeln('FAIL  '+title+' (unexpected '+e.ClassName+': '+e.Message+')');
    end;
  end;
end;


procedure ExpectRead( const token : AnsiString; const expected : Double; const where : TPosition);
// success == the token at the given position reads as the expected value
var title  : string;
    actual : Double;
begin
  title := 'read '+string(token)+' as '+POSITION_NAME[where];
  try
    actual := ReadDoubleAt( ToJSON( token, where), where);
    if SameDouble( actual, expected)
    then Writeln('PASS  '+title)
    else begin
      Inc( gFailures);
      Writeln('FAIL  '+title+' (expected '+DoubleToStr(expected)+', got '+DoubleToStr(actual)+')');
    end;
  except
    on e : Exception do begin
      Inc( gFailures);
      Writeln('FAIL  '+title+' (unexpected '+e.ClassName+': '+e.Message+')');
    end;
  end;
end;


procedure ExpectRoundTrip( const value : Double; const where : TPosition);
// success == what we write reads back as the same value
var title  : string;
    actual : Double;
begin
  title := 'round trip of '+DoubleToStr(value)+' as '+POSITION_NAME[where];
  try
    actual := ReadDoubleAt( WriteDoubleAt( value, where), where);
    if SameDouble( actual, value)
    then Writeln('PASS  '+title)
    else begin
      Inc( gFailures);
      Writeln('FAIL  '+title+' (got '+DoubleToStr(actual)+')');
    end;
  except
    on e : Exception do begin
      Inc( gFailures);
      Writeln('FAIL  '+title+' (unexpected '+e.ClassName+': '+e.Message+')');
    end;
  end;
end;


procedure ExpectInvalidData( const token : AnsiString; const where : TPosition);
// success == a TProtocolExceptionInvalidData is raised.
// A clean return or any other exception type means the input was not rejected as invalid data.
var title : string;
begin
  title := 'read '+string(token)+' as '+POSITION_NAME[where];
  try
    ReadDoubleAt( ToJSON( token, where), where);
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


procedure TestNonFinite( const where : TPosition);
begin
  // 1. writing uses the strings of the wire format
  ExpectWrite( NaN,         '"NaN"',       where);
  ExpectWrite( Infinity,    '"Infinity"',  where);
  ExpectWrite( NegInfinity, '"-Infinity"', where);

  // 2. reading takes them, whatever StrToFloat() makes of them
  ExpectRead( '"NaN"',       NaN,         where);
  ExpectRead( '"Infinity"',  Infinity,    where);
  ExpectRead( '"-Infinity"', NegInfinity, where);

  // 3. released versions of this library write FloatToStr() output, peers may still send it
  ExpectRead( '"NAN"',  NaN,         where);
  ExpectRead( '"INF"',  Infinity,    where);
  ExpectRead( '"-INF"', NegInfinity, where);

  // 4. the case of the letters does not matter
  ExpectRead( '"nan"',  NaN,         where);
  ExpectRead( '"inf"',  Infinity,    where);
  ExpectRead( '"-inf"', NegInfinity, where);

  // 5. what we write, we can read
  ExpectRoundTrip( NaN,         where);
  ExpectRoundTrip( Infinity,    where);
  ExpectRoundTrip( NegInfinity, where);

  // 6. quoted text that is no number is invalid data, whatever exception StrToFloat() raises
  ExpectInvalidData( '"abc"',       where);
  ExpectInvalidData( '"Infinityx"', where);
end;


procedure TestFinite;
begin
  // 7. a finite number is quoted in a map key only, and read accordingly
  ExpectWrite( 1.5, '1.5',   posListElement);
  ExpectWrite( 1.5, '"1.5"', posMapKey);
  ExpectRead( '1.5',   1.5, posListElement);
  ExpectRead( '"1.5"', 1.5, posMapKey);
  ExpectRoundTrip( 1.5, posListElement);
  ExpectRoundTrip( 1.5, posMapKey);

  // 8. a quoted number is not expected outside of a map key
  ExpectInvalidData( '"1.5"', posListElement);

  // 9. unquoted text that is no number is invalid data, too
  ExpectInvalidData( '1.5.5', posListElement);
end;


begin
  // the wire format must not depend on the locale of the user
  {$IF CompilerVersion >= 22.0}
  FormatSettings.DecimalSeparator  := ',';
  FormatSettings.ThousandSeparator := '.';
  {$ELSE}
  DecimalSeparator  := ',';
  ThousandSeparator := '.';
  {$IFEND}

  try
    Writeln('TJSONProtocol doubles');
    Writeln('------------------------------------------------------------------------');

    TestNonFinite( posListElement);
    TestNonFinite( posMapKey);
    TestFinite;

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
