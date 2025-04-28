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

unit Thrift.Collections;

interface

uses
  SysUtils, Generics.Collections, Generics.Defaults, Thrift.Utils;

type

{$IF CompilerVersion < 21.0}
  TArray<T> = array of T;
{$IFEND}

  IThriftContainer = interface( ISupportsToString)
    ['{E05C0F9D-A4F5-491D-AADA-C926B4BDB6E4}']
  end;


  IThriftDictionary<TKey,TValue> = interface(IThriftContainer)
    ['{25EDD506-F9D1-4008-A40F-5940364B7E46}']
    function GetEnumerator: TEnumerator<TPair<TKey,TValue>>;

    function GetKeys: TDictionary<TKey,TValue>.TKeyCollection;
    function GetValues: TDictionary<TKey,TValue>.TValueCollection;
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);
    function GetCount: Integer;

    procedure Add(const Key: TKey; const Value: TValue);
    procedure Remove(const Key: TKey);
{$IF CompilerVersion >= 21.0}
    function ExtractPair(const Key: TKey): TPair<TKey,TValue>;
{$IFEND}
    procedure Clear;
    procedure TrimExcess;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure AddOrSetValue(const Key: TKey; const Value: TValue);
    function ContainsKey(const Key: TKey): Boolean;
    function ContainsValue(const Value: TValue): Boolean;
    function ToArray: TArray<TPair<TKey,TValue>>;

    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property Keys: TDictionary<TKey,TValue>.TKeyCollection read GetKeys;
    property Values: TDictionary<TKey,TValue>.TValueCollection read GetValues;
  end;

  TThriftDictionaryImpl<TKey,TValue> = class( TInterfacedObject, IThriftDictionary<TKey,TValue>, IThriftContainer, ISupportsToString)
  strict private
    FDictionary : TDictionary<TKey,TValue>;
  strict protected
    function GetEnumerator: TEnumerator<TPair<TKey,TValue>>;

    function GetKeys: TDictionary<TKey,TValue>.TKeyCollection;
    function GetValues: TDictionary<TKey,TValue>.TValueCollection;
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);
    function GetCount: Integer;

    procedure Add(const Key: TKey; const Value: TValue);
    procedure Remove(const Key: TKey);
{$IF CompilerVersion >= 21.0}
    function ExtractPair(const Key: TKey): TPair<TKey,TValue>;
{$IFEND}
    procedure Clear;
    procedure TrimExcess;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure AddOrSetValue(const Key: TKey; const Value: TValue);
    function ContainsKey(const Key: TKey): Boolean;
    function ContainsValue(const Value: TValue): Boolean;
    function ToArray: TArray<TPair<TKey,TValue>>;
    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property Keys: TDictionary<TKey,TValue>.TKeyCollection read GetKeys;
    property Values: TDictionary<TKey,TValue>.TValueCollection read GetValues;
  public
    constructor Create( const aCapacity: Integer = 0);  overload;
    constructor Create( const aCapacity: Integer; const aComparer : IEqualityComparer<TKey>);  overload;
    destructor Destroy; override;
    function ToString : string;  override;
  end;

  IThriftList<T> = interface(IThriftContainer)
    ['{29BEEE31-9CB4-401B-AA04-5148A75F473B}']
    function GetEnumerator: TEnumerator<T>;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    function GetCount: Integer;
    procedure SetCount(Value: Integer);
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
    function Add(const Value: T): Integer;
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Collection: IEnumerable<T>); overload;
    procedure AddRange(Collection: TEnumerable<T>); overload;
    procedure Insert(Index: Integer; const Value: T);
    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    procedure InsertRange(Index: Integer; const Collection: IEnumerable<T>); overload;
    procedure InsertRange(Index: Integer; const Collection: TEnumerable<T>); overload;
    function Remove(const Value: T): Integer;
    procedure Delete(Index: Integer);
    procedure DeleteRange(AIndex, ACount: Integer);
    function Extract(const Value: T): T;
{$IF CompilerVersion >= 21.0}
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    function First: T;
    function Last: T;
{$IFEND}
    procedure Clear;
    function Contains(const Value: T): Boolean;
    function IndexOf(const Value: T): Integer;
    function LastIndexOf(const Value: T): Integer;
    procedure Reverse;
    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    function BinarySearch(const Item: T; out Index: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean; overload;
    procedure TrimExcess;
    function ToArray: TArray<T>;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;

  TThriftListImpl<T> = class( TInterfacedObject, IThriftList<T>, IThriftContainer, ISupportsToString)
  strict private
    FList : TList<T>;
  strict protected
    function GetEnumerator: TEnumerator<T>;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    function GetCount: Integer;
    procedure SetCount(Value: Integer);
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
    function Add(const Value: T): Integer;
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Collection: IEnumerable<T>); overload;
    procedure AddRange(Collection: TEnumerable<T>); overload;
    procedure Insert(Index: Integer; const Value: T);
    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    procedure InsertRange(Index: Integer; const Collection: IEnumerable<T>); overload;
    procedure InsertRange(Index: Integer; const Collection: TEnumerable<T>); overload;
    function Remove(const Value: T): Integer;
    procedure Delete(Index: Integer);
    procedure DeleteRange(AIndex, ACount: Integer);
    function Extract(const Value: T): T;
{$IF CompilerVersion >= 21.0}
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    function First: T;
    function Last: T;
{$IFEND}
    procedure Clear;
    function Contains(const Value: T): Boolean;
    function IndexOf(const Value: T): Integer;
    function LastIndexOf(const Value: T): Integer;
    procedure Reverse;
    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    function BinarySearch(const Item: T; out Index: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean; overload;
    procedure TrimExcess;
    function ToArray: TArray<T>;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  public
    constructor Create( const aCapacity: Integer = 0);
    destructor Destroy; override;
    function ToString : string;  override;
  end;

  IThriftHashSet<T> = interface(IThriftContainer)
    ['{733E2B57-C374-4359-BBD5-2B9CD8DF737C}']
    function GetEnumerator: TEnumerator<T>;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    function Add( const item: T) : Boolean;
    procedure Clear;
    function Contains( const item: T): Boolean;
    function Remove( const item: T): Boolean;
  end;

  // compatibility
  IHashSet<T> = interface( IThriftHashSet<T>)
    ['{C3CF557F-21D9-4524-B899-D3145B0389BB}']
  end deprecated 'use IThriftHashSet<T>';


  {$WARN SYMBOL_DEPRECATED OFF}
  TThriftHashSetImpl<T> = class( TInterfacedObject, IHashSet<T>, IThriftHashSet<T>, IThriftContainer, ISupportsToString)
  {$WARN SYMBOL_DEPRECATED DEFAULT}
  strict private
    FDictionary : TDictionary<T,Byte>;  // there is no THashSet<T> in older Delphi versions
  strict protected
    function GetEnumerator: TEnumerator<T>;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    function Add( const item: T) : Boolean;
    procedure Clear;
    function Contains( const item: T): Boolean;
    function Remove( const item: T): Boolean;
  public
    constructor Create( const aCapacity: Integer = 0);  overload;
    constructor Create( const aCapacity: Integer; const aComparer : IEqualityComparer<T>);  overload;
    destructor Destroy; override;
    function ToString : string;  override;
  end;

  // compatibility
  THashSetImpl<T> = class( TThriftHashSetImpl<T>)
  end deprecated 'use TThriftHashSetImpl<T>';

implementation

{ TThriftHashSetImpl<T>. }

function TThriftHashSetImpl<T>.Add( const item: T) : Boolean;
begin
  result := not FDictionary.ContainsKey(item);
  if result then FDictionary.Add( item, 0);
end;

procedure TThriftHashSetImpl<T>.Clear;
begin
  FDictionary.Clear;
end;

function TThriftHashSetImpl<T>.Contains( const item: T): Boolean;
begin
  Result := FDictionary.ContainsKey(item);
end;

constructor TThriftHashSetImpl<T>.Create( const aCapacity: Integer);
begin
  inherited Create;
  FDictionary := TDictionary<T,Byte>.Create( aCapacity);
end;

constructor TThriftHashSetImpl<T>.Create( const aCapacity: Integer; const aComparer : IEqualityComparer<T>);
begin
  inherited Create;
  FDictionary := TDictionary<T,Byte>.Create( aCapacity, aComparer);
end;


destructor TThriftHashSetImpl<T>.Destroy;
begin
  FDictionary.Free;
  inherited Destroy;
end;


function TThriftHashSetImpl<T>.GetCount: Integer;
begin
  Result := FDictionary.Count;
end;

function TThriftHashSetImpl<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := FDictionary.Keys.GetEnumerator;
end;

function TThriftHashSetImpl<T>.Remove( const item: T): Boolean;
begin
  Result := FDictionary.ContainsKey( item);
  if Result then FDictionary.Remove( item );
end;

function TThriftHashSetImpl<T>.ToString : string;
var elm : T;
    sb : TThriftStringBuilder;
    first : Boolean;
begin
  sb := TThriftStringBuilder.Create('{');
  try
    first := TRUE;
    for elm in FDictionary.Keys do begin
      if first
      then first := FALSE
      else sb.Append(', ');

      sb.Append( StringUtils<T>.ToString(elm));
    end;
    sb.Append('}');
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

{ TThriftDictionaryImpl<TKey, TValue> }

procedure TThriftDictionaryImpl<TKey, TValue>.Add(const Key: TKey;
  const Value: TValue);
begin
  FDictionary.Add( Key, Value);
end;

procedure TThriftDictionaryImpl<TKey, TValue>.AddOrSetValue(const Key: TKey;
  const Value: TValue);
begin
  FDictionary.AddOrSetValue( Key, Value);
end;

procedure TThriftDictionaryImpl<TKey, TValue>.Clear;
begin
  FDictionary.Clear;
end;

function TThriftDictionaryImpl<TKey, TValue>.ContainsKey(
  const Key: TKey): Boolean;
begin
  Result := FDictionary.ContainsKey( Key );
end;

function TThriftDictionaryImpl<TKey, TValue>.ContainsValue(
  const Value: TValue): Boolean;
begin
  Result := FDictionary.ContainsValue( Value );
end;

constructor TThriftDictionaryImpl<TKey, TValue>.Create(const aCapacity: Integer);
begin
  inherited Create;
  FDictionary := TDictionary<TKey,TValue>.Create( aCapacity);
end;

constructor TThriftDictionaryImpl<TKey, TValue>.Create(const aCapacity: Integer; const aComparer : IEqualityComparer<TKey>);
begin
  inherited Create;
  FDictionary := TDictionary<TKey,TValue>.Create( aCapacity, aComparer);
end;

destructor TThriftDictionaryImpl<TKey, TValue>.Destroy;
begin
  FDictionary.Free;
  inherited;
end;

{$IF CompilerVersion >= 21.0}
function TThriftDictionaryImpl<TKey, TValue>.ExtractPair( const Key: TKey): TPair<TKey, TValue>;
begin
  Result := FDictionary.ExtractPair( Key);
end;
{$IFEND}

function TThriftDictionaryImpl<TKey, TValue>.GetCount: Integer;
begin
  Result := FDictionary.Count;
end;

function TThriftDictionaryImpl<TKey, TValue>.GetEnumerator: TEnumerator<TPair<TKey, TValue>>;
begin
  Result := FDictionary.GetEnumerator;
end;

function TThriftDictionaryImpl<TKey, TValue>.GetItem(const Key: TKey): TValue;
begin
  Result := FDictionary.Items[Key];
end;

function TThriftDictionaryImpl<TKey, TValue>.GetKeys: TDictionary<TKey, TValue>.TKeyCollection;
begin
  Result := FDictionary.Keys;
end;

function TThriftDictionaryImpl<TKey, TValue>.GetValues: TDictionary<TKey, TValue>.TValueCollection;
begin
  Result := FDictionary.Values;
end;

procedure TThriftDictionaryImpl<TKey, TValue>.Remove(const Key: TKey);
begin
  FDictionary.Remove( Key );
end;

procedure TThriftDictionaryImpl<TKey, TValue>.SetItem(const Key: TKey;
  const Value: TValue);
begin
  FDictionary.AddOrSetValue( Key, Value);
end;

function TThriftDictionaryImpl<TKey, TValue>.ToArray: TArray<TPair<TKey, TValue>>;
{$IF CompilerVersion < 22.0}
var
  x : TPair<TKey, TValue>;
  i : Integer;
{$IFEND}
begin
{$IF CompilerVersion < 22.0}
  SetLength(Result, Count);
  i := 0;
  for x in FDictionaly do
  begin
    Result[i] := x;
    Inc( i );
  end;
{$ELSE}
  Result := FDictionary.ToArray;
{$IFEND}
end;

function TThriftDictionaryImpl<TKey, TValue>.ToString : string;
var pair : TPair<TKey, TValue>;
    sb : TThriftStringBuilder;
    first : Boolean;
begin
  sb := TThriftStringBuilder.Create('{');
  try
    first := TRUE;
    for pair in FDictionary do begin
      if first
      then first := FALSE
      else sb.Append(', ');

      sb.Append( '(');
      sb.Append( StringUtils<TKey>.ToString(pair.Key));
      sb.Append(' => ');
      sb.Append( StringUtils<TValue>.ToString(pair.Value));
      sb.Append(')');
    end;
    sb.Append('}');
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

procedure TThriftDictionaryImpl<TKey, TValue>.TrimExcess;
begin
  FDictionary.TrimExcess;
end;

function TThriftDictionaryImpl<TKey, TValue>.TryGetValue(const Key: TKey;
  out Value: TValue): Boolean;
begin
  Result := FDictionary.TryGetValue( Key, Value);
end;

{ TThriftListImpl<T> }

function TThriftListImpl<T>.Add(const Value: T): Integer;
begin
  Result := FList.Add( Value );
end;

procedure TThriftListImpl<T>.AddRange(Collection: TEnumerable<T>);
begin
  FList.AddRange( Collection );
end;

procedure TThriftListImpl<T>.AddRange(const Collection: IEnumerable<T>);
begin
  FList.AddRange( Collection );
end;

procedure TThriftListImpl<T>.AddRange(const Values: array of T);
begin
  FList.AddRange( Values );
end;

function TThriftListImpl<T>.BinarySearch(const Item: T;
  out Index: Integer): Boolean;
begin
  Result := FList.BinarySearch( Item, Index);
end;

function TThriftListImpl<T>.BinarySearch(const Item: T; out Index: Integer;
  const AComparer: IComparer<T>): Boolean;
begin
  Result := FList.BinarySearch( Item, Index, AComparer);
end;

procedure TThriftListImpl<T>.Clear;
begin
  FList.Clear;
end;

function TThriftListImpl<T>.Contains(const Value: T): Boolean;
begin
  Result := FList.Contains( Value );
end;

constructor TThriftListImpl<T>.Create( const aCapacity: Integer);
begin
  inherited Create;
  FList := TList<T>.Create;

  if aCapacity > 0
  then FList.Capacity := aCapacity;
end;

procedure TThriftListImpl<T>.Delete(Index: Integer);
begin
  FList.Delete( Index )
end;

procedure TThriftListImpl<T>.DeleteRange(AIndex, ACount: Integer);
begin
  FList.DeleteRange( AIndex, ACount)
end;

destructor TThriftListImpl<T>.Destroy;
begin
  FList.Free;
  inherited;
end;

{$IF CompilerVersion >= 21.0}
procedure TThriftListImpl<T>.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange( Index1, Index2 )
end;
{$IFEND}

function TThriftListImpl<T>.Extract(const Value: T): T;
begin
  Result := FList.Extract( Value )
end;

{$IF CompilerVersion >= 21.0}
function TThriftListImpl<T>.First: T;
begin
  Result := FList.First;
end;
{$IFEND}

function TThriftListImpl<T>.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TThriftListImpl<T>.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TThriftListImpl<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := FList.GetEnumerator;
end;

function TThriftListImpl<T>.GetItem(Index: Integer): T;
begin
  Result := FList[Index];
end;

function TThriftListImpl<T>.IndexOf(const Value: T): Integer;
begin
  Result := FList.IndexOf( Value );
end;

procedure TThriftListImpl<T>.Insert(Index: Integer; const Value: T);
begin
  FList.Insert( Index, Value);
end;

procedure TThriftListImpl<T>.InsertRange(Index: Integer;
  const Collection: TEnumerable<T>);
begin
  FList.InsertRange( Index, Collection );
end;

procedure TThriftListImpl<T>.InsertRange(Index: Integer;
  const Values: array of T);
begin
  FList.InsertRange( Index, Values);
end;

procedure TThriftListImpl<T>.InsertRange(Index: Integer;
  const Collection: IEnumerable<T>);
begin
  FList.InsertRange( Index, Collection );
end;

{$IF CompilerVersion >= 21.0}
function TThriftListImpl<T>.Last: T;
begin
  Result := FList.Last;
end;
{$IFEND}

function TThriftListImpl<T>.LastIndexOf(const Value: T): Integer;
begin
  Result := FList.LastIndexOf( Value );
end;

{$IF CompilerVersion >= 21.0}
procedure TThriftListImpl<T>.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move( CurIndex,  NewIndex);
end;
{$IFEND}

function TThriftListImpl<T>.Remove(const Value: T): Integer;
begin
  Result := FList.Remove( Value );
end;

procedure TThriftListImpl<T>.Reverse;
begin
  FList.Reverse;
end;

procedure TThriftListImpl<T>.SetCapacity(Value: Integer);
begin
  FList.Capacity := Value;
end;

procedure TThriftListImpl<T>.SetCount(Value: Integer);
begin
  FList.Count := Value;
end;

procedure TThriftListImpl<T>.SetItem(Index: Integer; const Value: T);
begin
  FList[Index] := Value;
end;

procedure TThriftListImpl<T>.Sort;
begin
  FList.Sort;
end;

procedure TThriftListImpl<T>.Sort(const AComparer: IComparer<T>);
begin
  FList.Sort(AComparer);
end;

function TThriftListImpl<T>.ToArray: TArray<T>;
{$IF CompilerVersion < 22.0}
var
  x : T;
  i : Integer;
{$IFEND}
begin
{$IF CompilerVersion < 22.0}
  SetLength(Result, Count);
  i := 0;
  for x in FList do
  begin
    Result[i] := x;
    Inc( i );
  end;
{$ELSE}
  Result := FList.ToArray;
{$IFEND}
end;

function TThriftListImpl<T>.ToString : string;
var elm : T;
    sb : TThriftStringBuilder;
    first : Boolean;
begin
  sb := TThriftStringBuilder.Create('{');
  try
    first := TRUE;
    for elm in FList do begin
      if first
      then first := FALSE
      else sb.Append(', ');

      sb.Append( StringUtils<T>.ToString(elm));
    end;
    sb.Append('}');
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

procedure TThriftListImpl<T>.TrimExcess;
begin
  FList.TrimExcess;
end;

end.
