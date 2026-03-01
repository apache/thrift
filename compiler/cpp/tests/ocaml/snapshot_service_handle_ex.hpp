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

const char* snapshot(R"""(
open Thrift
open Service_types

(* HELPER FUNCTIONS AND STRUCTURES *)

class ping_args =
object (self)
  method copy =
      let _new = Oo.copy self in
    _new
  method write (oprot : Protocol.t) =
    oprot#writeStructBegin "ping_args";
    oprot#writeFieldStop;
    oprot#writeStructEnd
end
let rec read_ping_args (iprot : Protocol.t) =
  let _str2 = new ping_args in
    ignore(iprot#readStructBegin);
    (try while true do
        let (_,_t3,_id4) = iprot#readFieldBegin in
        if _t3 = Protocol.T_STOP then
          raise Break
        else ();
        (match _id4 with 
          | _ -> iprot#skip _t3);
        iprot#readFieldEnd;
      done; ()
    with Break -> ());
    iprot#readStructEnd;
    _str2

class ping_result =
object (self)
  val mutable _serverError : Errors_types.serverError option = None
  method get_serverError = _serverError
  method grab_serverError = match _serverError with None->raise (Field_empty "ping_result.serverError") | Some _x5 -> _x5
  method set_serverError _x5 = _serverError <- Some _x5
  method unset_serverError = _serverError <- None
  method reset_serverError = _serverError <- None

  method copy =
      let _new = Oo.copy self in
      if _serverError <> None then
        _new#set_serverError self#grab_serverError#copy;
    _new
  method write (oprot : Protocol.t) =
    oprot#writeStructBegin "ping_result";
    (match _serverError with None -> () | Some _v -> 
      oprot#writeFieldBegin("serverError",Protocol.T_STRUCT,1);
      _v#write(oprot);
      oprot#writeFieldEnd
    );
    oprot#writeFieldStop;
    oprot#writeStructEnd
end
let rec read_ping_result (iprot : Protocol.t) =
  let _str8 = new ping_result in
    ignore(iprot#readStructBegin);
    (try while true do
        let (_,_t9,_id10) = iprot#readFieldBegin in
        if _t9 = Protocol.T_STOP then
          raise Break
        else ();
        (match _id10 with 
          | 1 -> (if _t9 = Protocol.T_STRUCT then
              _str8#set_serverError (Errors_types.read_serverError iprot)
            else
              iprot#skip _t9)
          | _ -> iprot#skip _t9);
        iprot#readFieldEnd;
      done; ()
    with Break -> ());
    iprot#readStructEnd;
    _str8

class virtual iface =
object (self)
  method virtual ping : unit
end

class client (iprot : Protocol.t) (oprot : Protocol.t) =
object (self)
  val mutable seqid = 0
  method ping  = 
    self#send_ping;
    self#recv_ping
  method private send_ping  = 
    oprot#writeMessageBegin ("ping", Protocol.CALL, seqid);
    let args = new ping_args in
      args#write oprot;
      oprot#writeMessageEnd;
      oprot#getTransport#flush
  method private recv_ping  =
    let (fname, mtype, rseqid) = iprot#readMessageBegin in
      (if mtype = Protocol.EXCEPTION then
        let x = Application_Exn.read iprot in
          (iprot#readMessageEnd;           raise (Application_Exn.E x))
      else ());
      let result = read_ping_result iprot in
        iprot#readMessageEnd;
        (match result#get_serverError with None -> () | Some _v ->
          raise (Errors_types.ServerError _v));
        ()
end

class processor (handler : iface) =
object (self)
  inherit Processor.t

  val processMap = Hashtbl.create 1
  method process iprot oprot =
    let (name, typ, seqid)  = iprot#readMessageBegin in
      if Hashtbl.mem processMap name then
        (Hashtbl.find processMap name) (seqid, iprot, oprot)
      else (
        iprot#skip(Protocol.T_STRUCT);
        iprot#readMessageEnd;
        let x = Application_Exn.create Application_Exn.UNKNOWN_METHOD ("Unknown function "^name) in
          oprot#writeMessageBegin(name, Protocol.EXCEPTION, seqid);
          x#write oprot;
          oprot#writeMessageEnd;
          oprot#getTransport#flush
      );
      true
  method private process_ping (seqid, iprot, oprot) =
    let _ = read_ping_args iprot in
      iprot#readMessageEnd;
      let result = new ping_result in
        (try
          (handler#ping);
        with
          | Errors_types.ServerError serverError -> 
              result#set_serverError serverError
        );
        oprot#writeMessageBegin ("ping", Protocol.REPLY, seqid);
        result#write oprot;
        oprot#writeMessageEnd;
        oprot#getTransport#flush
  initializer
    Hashtbl.add processMap "ping" self#process_ping;
end

)""");
