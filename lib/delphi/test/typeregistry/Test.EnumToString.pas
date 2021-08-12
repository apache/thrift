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

unit Test.EnumToString;

interface

uses
  Classes, SysUtils,
  Thrift.Utils,
  DebugProtoTest;


procedure RunTest;


implementation

{$SCOPEDENUMS ON}

type
  TIrregularEnum = (  // has gaps and/or does not start at zero
    FiveHundretOne = 501,
    FiveHundretTwo = 502,
    FiveHundretFive = 505
  );

  TRegularEnum = (  // starts at zero, no gaps, no duplicates
    One,
    Two,
    Three
  );


procedure IrregularEnumToString;
// TIrregularEnum does not run from 0 to N, so we don't have RTTI for it
// Search for "E2134: Type has no typeinfo" message to get the details
// Unfortunately, this also means that StringUtils<T>.ToString() does not work for enums w/o RTTI
var value : Integer;
    sA,sB : string;
begin
  for value := Pred(Ord(Low(TIrregularEnum))) to Succ(Ord(High(TIrregularEnum))) do begin
    sA := EnumUtils<TIrregularEnum>.ToString(Ord(value));               // much more reliable
    sB := StringUtils<TIrregularEnum>.ToString(TIrregularEnum(value));  // does not really work
    WriteLn( '- TIrregularEnum('+IntToStr(value)+'): EnumUtils => ',sA,', StringUtils => ', sB);
  end;
end;


procedure RegularEnumToString;
// Regular enums have RTTI and work like a charm
var value : Integer;
    sA,sB : string;
begin
  for value := Pred(Ord(Low(TRegularEnum))) to Succ(Ord(High(TRegularEnum))) do begin
    sA := EnumUtils<TRegularEnum>.ToString(Ord(value));
    sB := StringUtils<TRegularEnum>.ToString(TRegularEnum(value));
    if sA = sB  // both are expected to work with regular enums
    then WriteLn( '- TRegularEnum('+IntToStr(value)+'): ',sA,' = ', sB)
    else raise Exception.Create( 'Test failed: '+sA+' <> '+sB);
  end;
end;


procedure RunTest;
begin
  Writeln('Testing enum utils ...');

  RegularEnumToString;
  IrregularEnumToString;

  Writeln;
end;


end.

