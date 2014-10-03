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

unit Thrift.Utils;

interface

uses
  Classes, Windows, SysUtils, SyncObjs;

type
  IOverlappedHelper = interface
    ['{A1832EFA-2E02-4884-8F09-F0A0277157FA}']
    function Overlapped : TOverlapped;
    function OverlappedPtr : POverlapped;
    function WaitHandle : THandle;
    function WaitFor(dwTimeout: DWORD) : DWORD;
  end;

  TOverlappedHelperImpl = class( TInterfacedObject, IOverlappedHelper)
  strict protected
    FOverlapped : TOverlapped;
    FEvent      : TEvent;

    // IOverlappedHelper
    function Overlapped : TOverlapped;
    function OverlappedPtr : POverlapped;
    function WaitHandle : THandle;
    function WaitFor(dwTimeout: DWORD) : DWORD;
  public
    constructor Create;
    destructor Destroy;  override;
  end;



function IfValue(B: Boolean; const TrueValue, FalseValue: WideString): string;

implementation


function IfValue(B: Boolean; const TrueValue, FalseValue: WideString): string;
begin
  if B then
    Result := TrueValue
  else
    Result := FalseValue;
end;


{ TOverlappedHelperImpl }

constructor TOverlappedHelperImpl.Create;
begin
  inherited Create;
  FillChar( FOverlapped, SizeOf(FOverlapped), 0);
  FEvent := TEvent.Create( nil, TRUE, FALSE, '');  // always ManualReset, see MSDN
  FOverlapped.hEvent := FEvent.Handle;
end;



destructor TOverlappedHelperImpl.Destroy;
begin
  try
    FOverlapped.hEvent := 0;
    FreeAndNil( FEvent);

  finally
    inherited Destroy;
  end;

end;


function TOverlappedHelperImpl.Overlapped : TOverlapped;
begin
  result := FOverlapped;
end;


function TOverlappedHelperImpl.OverlappedPtr : POverlapped;
begin
  result := @FOverlapped;
end;


function TOverlappedHelperImpl.WaitHandle : THandle;
begin
  result := FOverlapped.hEvent;
end;


function TOverlappedHelperImpl.WaitFor( dwTimeout : DWORD) : DWORD;
begin
  result := WaitForSingleObject( FOverlapped.hEvent, dwTimeout);
end;




end.
