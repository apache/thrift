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

 unit Thrift.Server;

{$I Thrift.Defines.inc}
{$I-}  // prevent annoying errors with default log delegate and no console

interface

uses
  {$IFDEF OLD_UNIT_NAMES}
  Windows, SysUtils,
  {$ELSE}
  Winapi.Windows, System.SysUtils,
  {$ENDIF}
  Thrift,
  Thrift.Protocol,
  Thrift.Transport,
  Thrift.Configuration;

type
  IServerEvents = interface
    ['{9E2A99C5-EE85-40B2-9A52-2D1722B18176}']
    // Called before the server begins.
    procedure PreServe;
    // Called when the server transport is ready to accept requests
    procedure PreAccept;
    // Called when a new client has connected and the server is about to being processing.
    function  CreateProcessingContext( const input, output : IProtocol) : IProcessorEvents;
  end;


  IServer = interface
    ['{ADC46F2D-8199-4D1C-96D2-87FD54351723}']
    procedure Serve;
    procedure Stop;

    function GetServerEvents : IServerEvents;
    procedure SetServerEvents( const value : IServerEvents);

    property ServerEvents : IServerEvents read GetServerEvents write SetServerEvents;
  end;

  TServerImpl = class abstract( TInterfacedObject, IServer )
  public
    type
      TLogDelegate = reference to procedure( const str: string);
  strict protected
    FProcessor : IProcessor;
    FServerTransport : IServerTransport;
    FInputTransportFactory : ITransportFactory;
    FOutputTransportFactory : ITransportFactory;
    FInputProtocolFactory : IProtocolFactory;
    FOutputProtocolFactory : IProtocolFactory;
    FLogDelegate : TLogDelegate;
    FServerEvents : IServerEvents;
    FConfiguration : IThriftConfiguration;

    class procedure DefaultLogDelegate( const str: string);

    function GetServerEvents : IServerEvents;
    procedure SetServerEvents( const value : IServerEvents);

    procedure Serve; virtual; abstract;
    procedure Stop; virtual; abstract;
  public
    constructor Create(
      const aProcessor :IProcessor;
      const aServerTransport: IServerTransport;
      const aInputTransportFactory : ITransportFactory;
      const aOutputTransportFactory : ITransportFactory;
      const aInputProtocolFactory : IProtocolFactory;
      const aOutputProtocolFactory : IProtocolFactory;
      const aConfig : IThriftConfiguration;
      const aLogDelegate : TLogDelegate
      ); overload;

    constructor Create(
      const aProcessor: IProcessor;
      const aServerTransport: IServerTransport;
      const aTransportFactory: ITransportFactory = nil;
      const aProtocolFactory: IProtocolFactory = nil;
      const aConfig : IThriftConfiguration = nil;
      const aLogDel: TServerImpl.TLogDelegate = nil
      ); overload;
  end;


  TSimpleServer = class( TServerImpl)
  private
    FStop : Boolean;
  public
    procedure Serve; override;
    procedure Stop; override;
  end;


implementation

{ TServerImpl }

constructor TServerImpl.Create( const aProcessor: IProcessor;
                                const aServerTransport: IServerTransport;
                                const aInputTransportFactory, aOutputTransportFactory: ITransportFactory;
                                const aInputProtocolFactory, aOutputProtocolFactory: IProtocolFactory;
                                const aConfig : IThriftConfiguration;
                                const aLogDelegate : TLogDelegate);
begin
  inherited Create;
  FProcessor := aProcessor;
  FServerTransport := aServerTransport;

  if aConfig <> nil
  then FConfiguration := aConfig
  else FConfiguration := TThriftConfigurationImpl.Create;

  if aInputTransportFactory <> nil
  then FInputTransportFactory := aInputTransportFactory
  else FInputTransportFactory := TTransportFactoryImpl.Create;

  if aOutputTransportFactory <> nil
  then FOutputTransportFactory := aOutputTransportFactory
  else FOutputTransportFactory := TTransportFactoryImpl.Create;

  if aInputProtocolFactory <> nil
  then FInputProtocolFactory := aInputProtocolFactory
  else FInputProtocolFactory := TBinaryProtocolImpl.TFactory.Create;

  if aOutputProtocolFactory <> nil
  then FOutputProtocolFactory := aOutputProtocolFactory
  else FOutputProtocolFactory := TBinaryProtocolImpl.TFactory.Create;

  if Assigned(aLogDelegate)
  then FLogDelegate := aLogDelegate
  else FLogDelegate := DefaultLogDelegate;
end;


constructor TServerImpl.Create( const aProcessor: IProcessor;
                                const aServerTransport: IServerTransport;
                                const aTransportFactory: ITransportFactory;
                                const aProtocolFactory: IProtocolFactory;
                                const aConfig : IThriftConfiguration;
                                const aLogDel: TServerImpl.TLogDelegate);
begin
  Create( aProcessor, aServerTransport,
          aTransportFactory, aTransportFactory,
          aProtocolFactory, aProtocolFactory,
          aConfig, aLogDel);
end;


class procedure TServerImpl.DefaultLogDelegate( const str: string);
begin
  try
    Writeln( str);
    if IoResult <> 0 then OutputDebugString(PChar(str));
  except
    OutputDebugString(PChar(str));
  end;
end;



function TServerImpl.GetServerEvents : IServerEvents;
begin
  result := FServerEvents;
end;


procedure TServerImpl.SetServerEvents( const value : IServerEvents);
begin
  // if you need more than one, provide a specialized IServerEvents implementation
  FServerEvents := value;
end;


{ TSimpleServer }

procedure TSimpleServer.Serve;
var
  client : ITransport;
  InputTransport : ITransport;
  OutputTransport : ITransport;
  InputProtocol : IProtocol;
  OutputProtocol : IProtocol;
  context : IProcessorEvents;
begin
  try
    FServerTransport.Listen;
  except
    on E: Exception do
    begin
      FLogDelegate( E.ToString);
    end;
  end;

  if FServerEvents <> nil
  then FServerEvents.PreServe;

  client := nil;
  while (not FStop) do
  begin
    try
      // clean up any old instances before waiting for clients
      InputTransport := nil;
      OutputTransport := nil;
      InputProtocol := nil;
      OutputProtocol := nil;

      // close any old connections before before waiting for new clients
      if client <> nil then try
        try
          client.Close;
        finally
          client := nil;
        end;
      except
        // catch all, we can't do much about it at this point
      end;

      client := FServerTransport.Accept( procedure
                                         begin
                                           if FServerEvents <> nil
                                           then FServerEvents.PreAccept;
                                         end);

      if client = nil then begin
        if FStop
        then Abort  // silent exception
        else raise TTransportExceptionUnknown.Create('ServerTransport.Accept() may not return NULL');
      end;

      FLogDelegate( 'Client Connected!');

      InputTransport := FInputTransportFactory.GetTransport( client );
      OutputTransport := FOutputTransportFactory.GetTransport( client );
      InputProtocol := FInputProtocolFactory.GetProtocol( InputTransport );
      OutputProtocol := FOutputProtocolFactory.GetProtocol( OutputTransport );

      if FServerEvents <> nil
      then context := FServerEvents.CreateProcessingContext( InputProtocol, OutputProtocol)
      else context := nil;

      while not FStop do begin
        if context <> nil
        then context.Processing( client);
        if not FProcessor.Process( InputProtocol, OutputProtocol, context)
        then Break;
      end;

    except
      on E: TTransportException do begin
        if FStop
        then FLogDelegate('TSimpleServer was shutting down, caught ' + E.ToString)
        else FLogDelegate( E.ToString);
      end;
      on E: Exception do begin
        FLogDelegate( E.ToString);
      end;
    end;

    if context <> nil then begin
      context.CleanupContext;
      context := nil;
    end;

    if InputTransport <> nil then
    begin
      InputTransport.Close;
    end;
    if OutputTransport <> nil then
    begin
      OutputTransport.Close;
    end;
  end;

  if FStop then
  begin
    try
      FServerTransport.Close;
    except
      on E: TTransportException do
      begin
        FLogDelegate('TServerTranport failed on close: ' + E.Message);
      end;
    end;
    FStop := False;
  end;
end;

procedure TSimpleServer.Stop;
begin
  FStop := True;
  FServerTransport.Close;
end;

end.
