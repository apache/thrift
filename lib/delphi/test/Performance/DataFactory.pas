// Licensed to the Apache Software Foundation(ASF) under one
// or more contributor license agreements.See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.
unit DataFactory;

interface

uses
  SysUtils,
  Thrift.Collections,
  Thrift.Test;

type
  TestDataFactory = class
  strict protected
    class function CreateSetField(const count : Integer) : IHashSet< IInsanity>;  static;
    class function CreateInsanity(const count : Integer) : IInsanity; static;
    class function CreateBytesArray(const count : Integer) : TBytes; static;
    class function CreateXtructs(const count : Integer) : IThriftList< IXtruct>; static;
    class function CreateXtruct(const count : Integer) : IXtruct; static;
    class function CreateListField(const count : Integer) : IThriftList< IThriftDictionary< IHashSet< Integer>, IThriftDictionary< Integer, IHashSet< IThriftList< IThriftDictionary< IInsanity, string>>>>>>; static;
    class function CreateUserMap(const count : Integer) : IThriftDictionary< TNumberz, Int64>; static;
    class function CreateListFieldData(const count : Integer) : IThriftDictionary< IHashSet< Integer>, IThriftDictionary< Integer, IHashSet< IThriftList< IThriftDictionary< IInsanity, string>>>>>; static;
    class function CreateIntHashSet(const count : Integer) : IHashSet< Integer>; static;
    class function CreateListFieldDataDict(const count : Integer) : IThriftDictionary< Integer, IHashSet< IThriftList< IThriftDictionary< IInsanity, string>>>>; static;
    class function CreateListFieldDataDictValue(const count : Integer) :  IHashSet< IThriftList< IThriftDictionary< IInsanity, string>>>; static;
    class function CreateListFieldDataDictValueList(const count : Integer) : IThriftList< IThriftDictionary< IInsanity, string>>; static;
    class function CreateListFieldDataDictValueListDict(const count : Integer) : IThriftDictionary< IInsanity, string>; static;
  public
    class function CreateCrazyNesting(const count : Integer = 10) : ICrazyNesting; static;
  end;

implementation


class function TestDataFactory.CreateCrazyNesting(const count : Integer = 10) : ICrazyNesting;
begin
  if (count <= 0)
  then Exit(nil);

  result := TCrazyNestingImpl.Create;
  result.Binary_field := CreateBytesArray(count);
  result.List_field := CreateListField(count);
  result.Set_field := CreateSetField(count);
  result.String_field := Format('data level %d', [count]);
end;

class function TestDataFactory.CreateSetField(const count : Integer) : IHashSet< IInsanity>;
var i : Integer;
begin
  result := THashSetImpl< IInsanity>.Create;
  for i := 0 to count-1 do begin
    result.Add(CreateInsanity(count));
  end;
end;

class function TestDataFactory.CreateInsanity(const count : Integer) : IInsanity;
begin
  result := TInsanityImpl.Create;
  result.UserMap := CreateUserMap(count);
  result.Xtructs := CreateXtructs(count);
end;

class function TestDataFactory.CreateXtructs(const count : Integer) : IThriftList< IXtruct>;
var i : Integer;
begin
  result := TThriftListImpl< IXtruct>.Create;
  for i := 0 to count-1 do begin
    result.Add(CreateXtruct(count));
  end;
end;

class function TestDataFactory.CreateXtruct(const count : Integer) : IXtruct;
begin
  result := TXtructImpl.Create;
  result.Byte_thing := SmallInt(count mod 128);
  result.I32_thing := count;
  result.I64_thing := count;
  result.String_thing := Format('data level %d', [count]);
end;

class function TestDataFactory.CreateUserMap(const count : Integer) : IThriftDictionary< TNumberz, Int64>;
begin
  result := TThriftDictionaryImpl< TNumberz, Int64>.Create;
  result.Add(TNumberz.ONE, count);
  result.Add(TNumberz.TWO, count);
  result.Add(TNumberz.THREE, count);
  result.Add(TNumberz.FIVE, count);
  result.Add(TNumberz.SIX, count);
  result.Add(TNumberz.EIGHT, count);
end;

class function TestDataFactory.CreateListField(const count : Integer) : IThriftList< IThriftDictionary< IHashSet< Integer>, IThriftDictionary< Integer, IHashSet< IThriftList< IThriftDictionary< IInsanity, string>>>>>>;
var i : Integer;
begin
  result := TThriftListImpl< IThriftDictionary< IHashSet< Integer>, IThriftDictionary< Integer, IHashSet< IThriftList< IThriftDictionary< IInsanity, string>>>>>>.Create;
  for i := 0 to count-1 do begin
    result.Add(CreateListFieldData(count));
  end;
end;

class function TestDataFactory.CreateListFieldData(const count : Integer) : IThriftDictionary< IHashSet< Integer>, IThriftDictionary< Integer, IHashSet< IThriftList< IThriftDictionary< IInsanity, string>>>>>;
var i : Integer;
begin
  result := TThriftDictionaryImpl< IHashSet< Integer>, IThriftDictionary< Integer, IHashSet< IThriftList< IThriftDictionary< IInsanity, string>>>>>.Create;
  for i := 0 to count-1 do begin
    result.Add( CreateIntHashSet(count), CreateListFieldDataDict(count));
  end;
end;

class function TestDataFactory.CreateIntHashSet(const count : Integer) : IHashSet< Integer>;
var i : Integer;
begin
  result := THashSetImpl< Integer>.Create;
  for i := 0 to count-1 do begin
    result.Add(i);
  end;
end;

class function TestDataFactory.CreateListFieldDataDict(const count : Integer) : IThriftDictionary< Integer, IHashSet< IThriftList< IThriftDictionary< IInsanity, string>>>>;
var i : Integer;
begin
  result := TThriftDictionaryImpl< Integer, IHashSet< IThriftList< IThriftDictionary< IInsanity, string>>>>.Create;
  for i := 0 to count-1 do begin
    result.Add(i, CreateListFieldDataDictValue(count));
  end;
end;

class function TestDataFactory.CreateListFieldDataDictValue(const count : Integer) :  IHashSet< IThriftList< IThriftDictionary< IInsanity, string>>>;
var i : Integer;
begin
  result := THashSetImpl< IThriftList< IThriftDictionary< IInsanity, string>>>.Create;
  for i := 0 to count-1 do begin
    result.Add( CreateListFieldDataDictValueList(count));
  end;
end;

class function TestDataFactory.CreateListFieldDataDictValueList(const count : Integer) : IThriftList< IThriftDictionary< IInsanity, string>>;
var i : Integer;
begin
  result := TThriftListImpl< IThriftDictionary< IInsanity, string>>.Create;
  for i := 0 to count-1 do begin
    result.Add(CreateListFieldDataDictValueListDict(count));
  end;
end;

class function TestDataFactory.CreateListFieldDataDictValueListDict(const count : Integer) : IThriftDictionary< IInsanity, string>;
begin
  result := TThriftDictionaryImpl< IInsanity, string>.Create;
  result.Add(CreateInsanity(count), Format('data level %d', [count]));
end;

class function TestDataFactory.CreateBytesArray(const count : Integer) : TBytes;
var i : Integer;
begin
  SetLength( result, count);
  for i := 0 to count-1 do begin
    result[i] := i mod $FF;
  end;
end;

end.

