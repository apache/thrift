(*
 Licensed to the Apache Software Foundation (ASF) under one
 or more contributor license agreements. See the NOTICE file
 distributed with this work for additional information
 regarding copyright ownership. The ASF licenses this file
 to you under the Apache License, Version 2.0 (the
 "License"); you may not use this file except in compliance
 with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing,
 software distributed under the License is distributed on an
 "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 KIND, either express or implied. See the License for the
 specific language governing permissions and limitations
 under the License.
*)

(* Recursion-depth guard test for the OCaml library (THRIFT-6051).

   This exercises the real depth guard that lives in [Thrift.Protocol.t]
   (increment_recursion_depth / decrement_recursion_depth, limit 64) through
   full struct/union/exception read+write round-trips, NOT by calling those
   two methods in isolation.

   The recursive types and their read/write functions below are written by
   hand to mirror exactly what the OCaml generator now emits (the same
   "increment_recursion_depth; Fun.protect ~finally:decrement (fun () -> ...)"
   wrapper around every struct reader and writer). Generated code from
   test/Recursive.thrift cannot be used directly here for two pre-existing,
   unrelated reasons: the generator emits mutually recursive classes
   (CoRec/CoUnion/CoError pairs) as separate "class" declarations instead of
   "class ... and ...", and TBinaryProtocol.ml does not compile on modern
   OCaml (uses removed mutable-string APIs). Both are out of scope for
   THRIFT-6051 and flagged separately.

   In the OCaml generator unions and exceptions are emitted through the very
   same code path as structs (generate_ocaml_struct_definition), so a single
   guarded reader/writer template covers all three. The three node types below
   make that coverage explicit at runtime. *)

open Thrift

(* Minimal no-op transport: the in-memory protocol below never touches it. *)
class null_transport =
object
  inherit Transport.t
  method isOpen = true
  method opn = ()
  method close = ()
  method read _buf _off _len = 0
  method write _buf _off _len = ()
  method flush = ()
end

(* A tiny in-memory protocol that records the structural events as tokens in a
   FIFO queue on write and replays them on read. It implements only the methods
   the recursive types below use; the rest fail loudly if ever reached. This
   stands in for TBinaryProtocol, which does not compile on modern OCaml. *)
type token =
  | StructBegin of string
  | StructEnd
  | FieldBegin of string * Protocol.t_type * int
  | FieldEnd
  | FieldStop
  | I16 of int

class mem_protocol trans =
object (self)
  inherit Protocol.t trans

  val tokens : token Queue.t = Queue.create ()
  method private put t = Queue.add t tokens
  method private get = Queue.take tokens

  (* functional, structural methods *)
  method writeStructBegin name = self#put (StructBegin name)
  method writeStructEnd = self#put StructEnd
  method writeFieldBegin (name, t, id) = self#put (FieldBegin (name, t, id))
  method writeFieldEnd = self#put FieldEnd
  method writeFieldStop = self#put FieldStop
  method writeI16 i = self#put (I16 i)

  method readStructBegin =
    (match self#get with StructBegin n -> n | _ -> failwith "expected StructBegin")
  method readStructEnd =
    (match self#get with StructEnd -> () | _ -> failwith "expected StructEnd")
  method readFieldBegin =
    (match self#get with
     | FieldBegin (n, t, id) -> (n, t, id)
     | FieldStop -> ("", Protocol.T_STOP, 0)
     | _ -> failwith "expected FieldBegin or FieldStop")
  method readFieldEnd =
    (match self#get with FieldEnd -> () | _ -> failwith "expected FieldEnd")
  method readI16 =
    (match self#get with I16 i -> i | _ -> failwith "expected I16")

  (* unused by this test *)
  method writeMessageBegin _ = failwith "unused"
  method writeMessageEnd = failwith "unused"
  method writeMapBegin _ = failwith "unused"
  method writeMapEnd = failwith "unused"
  method writeListBegin _ = failwith "unused"
  method writeListEnd = failwith "unused"
  method writeSetBegin _ = failwith "unused"
  method writeSetEnd = failwith "unused"
  method writeBool _ = failwith "unused"
  method writeByte _ = failwith "unused"
  method writeI32 _ = failwith "unused"
  method writeI64 _ = failwith "unused"
  method writeDouble _ = failwith "unused"
  method writeString _ = failwith "unused"
  method writeBinary _ = failwith "unused"
  method readMessageBegin = failwith "unused"
  method readMessageEnd = failwith "unused"
  method readMapBegin = failwith "unused"
  method readMapEnd = failwith "unused"
  method readListBegin = failwith "unused"
  method readListEnd = failwith "unused"
  method readSetBegin = failwith "unused"
  method readSetEnd = failwith "unused"
  method readBool = failwith "unused"
  method readByte = failwith "unused"
  method readI32 = failwith "unused"
  method readI64 = failwith "unused"
  method readDouble = failwith "unused"
  method readString = failwith "unused"
  method readBinary = failwith "unused"
end

let passed = ref 0
let failed = ref 0

let check label cond =
  if cond then begin
    Printf.printf "PASS: %s\n%!" label;
    incr passed
  end else begin
    Printf.printf "FAIL: %s\n%!" label;
    incr failed
  end

(* ---- Recursive STRUCT (self-recursive, mirrors generated struct codegen) ---- *)
class rec_struct =
object (_self)
  val mutable _other : rec_struct option = None
  val mutable _item : int = 0
  method get_other = _other
  method set_other (x : rec_struct) = _other <- Some x
  method set_item i = _item <- i
  method write (oprot : Protocol.t) =
    oprot#increment_recursion_depth;
    Fun.protect ~finally:(fun () -> oprot#decrement_recursion_depth) (fun () ->
      oprot#writeStructBegin "RecStruct";
      (match _other with None -> () | Some _v ->
        oprot#writeFieldBegin ("other", Protocol.T_STRUCT, 1);
        _v#write oprot;
        oprot#writeFieldEnd);
      oprot#writeFieldBegin ("item", Protocol.T_I16, 2);
      oprot#writeI16 _item;
      oprot#writeFieldEnd;
      oprot#writeFieldStop;
      oprot#writeStructEnd)
end

let rec read_rec_struct (iprot : Protocol.t) =
  let str = new rec_struct in
  iprot#increment_recursion_depth;
  Fun.protect ~finally:(fun () -> iprot#decrement_recursion_depth) (fun () ->
    ignore (iprot#readStructBegin);
    (try while true do
      let (_, t, id) = iprot#readFieldBegin in
      if t = Protocol.T_STOP then raise Break;
      (match id with
       | 1 -> str#set_other (read_rec_struct iprot)
       | 2 -> str#set_item iprot#readI16
       | _ -> iprot#skip t);
      iprot#readFieldEnd
    done with Break -> ());
    iprot#readStructEnd);
  str

let rec build_struct n =
  let s = new rec_struct in
  s#set_item n;
  if n > 1 then s#set_other (build_struct (n - 1));
  s

let rec depth_struct s =
  match s#get_other with None -> 1 | Some o -> 1 + depth_struct o

(* Craft an over-limit payload with raw (unguarded) protocol calls, so the
   depth limit can only trip on the read side. *)
let rec craft_struct (oprot : Protocol.t) n =
  oprot#writeStructBegin "RecStruct";
  if n > 1 then begin
    oprot#writeFieldBegin ("other", Protocol.T_STRUCT, 1);
    craft_struct oprot (n - 1);
    oprot#writeFieldEnd
  end;
  oprot#writeFieldStop;
  oprot#writeStructEnd

(* ---- Recursive UNION (OCaml emits unions through the struct path) ---- *)
class rec_union =
object (_self)
  val mutable _other : rec_union option = None
  method get_other = _other
  method set_other (x : rec_union) = _other <- Some x
  method write (oprot : Protocol.t) =
    oprot#increment_recursion_depth;
    Fun.protect ~finally:(fun () -> oprot#decrement_recursion_depth) (fun () ->
      oprot#writeStructBegin "RecUnion";
      (match _other with None -> () | Some _v ->
        oprot#writeFieldBegin ("other", Protocol.T_STRUCT, 1);
        _v#write oprot;
        oprot#writeFieldEnd);
      oprot#writeFieldStop;
      oprot#writeStructEnd)
end

let rec read_rec_union (iprot : Protocol.t) =
  let str = new rec_union in
  iprot#increment_recursion_depth;
  Fun.protect ~finally:(fun () -> iprot#decrement_recursion_depth) (fun () ->
    ignore (iprot#readStructBegin);
    (try while true do
      let (_, t, id) = iprot#readFieldBegin in
      if t = Protocol.T_STOP then raise Break;
      (match id with
       | 1 -> str#set_other (read_rec_union iprot)
       | _ -> iprot#skip t);
      iprot#readFieldEnd
    done with Break -> ());
    iprot#readStructEnd);
  str

let rec build_union n =
  let u = new rec_union in
  if n > 1 then u#set_other (build_union (n - 1));
  u

let rec depth_union u =
  match u#get_other with None -> 1 | Some o -> 1 + depth_union o

let rec craft_union (oprot : Protocol.t) n =
  oprot#writeStructBegin "RecUnion";
  if n > 1 then begin
    oprot#writeFieldBegin ("other", Protocol.T_STRUCT, 1);
    craft_union oprot (n - 1);
    oprot#writeFieldEnd
  end;
  oprot#writeFieldStop;
  oprot#writeStructEnd

(* ---- Recursive EXCEPTION (OCaml emits a class plus an "exception E of cls") ---- *)
class rec_error =
object (_self)
  val mutable _other : rec_error option = None
  method get_other = _other
  method set_other (x : rec_error) = _other <- Some x
  method write (oprot : Protocol.t) =
    oprot#increment_recursion_depth;
    Fun.protect ~finally:(fun () -> oprot#decrement_recursion_depth) (fun () ->
      oprot#writeStructBegin "RecError";
      (match _other with None -> () | Some _v ->
        oprot#writeFieldBegin ("other", Protocol.T_STRUCT, 1);
        _v#write oprot;
        oprot#writeFieldEnd);
      oprot#writeFieldStop;
      oprot#writeStructEnd)
end
exception RecError of rec_error

let rec read_rec_error (iprot : Protocol.t) =
  let str = new rec_error in
  iprot#increment_recursion_depth;
  Fun.protect ~finally:(fun () -> iprot#decrement_recursion_depth) (fun () ->
    ignore (iprot#readStructBegin);
    (try while true do
      let (_, t, id) = iprot#readFieldBegin in
      if t = Protocol.T_STOP then raise Break;
      (match id with
       | 1 -> str#set_other (read_rec_error iprot)
       | _ -> iprot#skip t);
      iprot#readFieldEnd
    done with Break -> ());
    iprot#readStructEnd);
  str

let rec build_error n =
  let e = new rec_error in
  if n > 1 then e#set_other (build_error (n - 1));
  e

let rec depth_error e =
  match e#get_other with None -> 1 | Some o -> 1 + depth_error o

let rec craft_error (oprot : Protocol.t) n =
  oprot#writeStructBegin "RecError";
  if n > 1 then begin
    oprot#writeFieldBegin ("other", Protocol.T_STRUCT, 1);
    craft_error oprot (n - 1);
    oprot#writeFieldEnd
  end;
  oprot#writeFieldStop;
  oprot#writeStructEnd

let limit = 64

let is_depth_limit = function Protocol.E (Protocol.DEPTH_LIMIT, _) -> true | _ -> false

(* Run the full read/write round-trip matrix for one node category. *)
let run_suite label ~build ~write ~read ~depth_of ~craft =
  (* 1. A chain exactly at the limit round-trips and preserves its depth. *)
  (let proto = new mem_protocol (new null_transport) in
   try
     write (build limit) proto;
     let got = depth_of (read proto) in
     check (Printf.sprintf "%s: %d-deep write/read round-trip preserves depth" label limit)
       (got = limit)
   with e ->
     Printf.printf "  (unexpected: %s)\n%!" (Printexc.to_string e);
     check (Printf.sprintf "%s: %d-deep write/read round-trip preserves depth" label limit) false);

  (* 2. Writing one level past the limit raises DEPTH_LIMIT. *)
  (let proto = new mem_protocol (new null_transport) in
   try
     write (build (limit + 1)) proto;
     check (Printf.sprintf "%s: write %d-deep raises DEPTH_LIMIT" label (limit + 1)) false
   with e ->
     check (Printf.sprintf "%s: write %d-deep raises DEPTH_LIMIT" label (limit + 1))
       (is_depth_limit e));

  (* 3. Reading a crafted over-limit payload raises DEPTH_LIMIT. *)
  (let proto = new mem_protocol (new null_transport) in
   craft proto (limit + 1);
   try
     ignore (read proto);
     check (Printf.sprintf "%s: read %d-deep raises DEPTH_LIMIT" label (limit + 1)) false
   with e ->
     check (Printf.sprintf "%s: read %d-deep raises DEPTH_LIMIT" label (limit + 1))
       (is_depth_limit e))

let () =
  run_suite "struct"
    ~build:build_struct
    ~write:(fun o p -> o#write p)
    ~read:read_rec_struct
    ~depth_of:depth_struct
    ~craft:craft_struct;
  run_suite "union"
    ~build:build_union
    ~write:(fun o p -> o#write p)
    ~read:read_rec_union
    ~depth_of:depth_union
    ~craft:craft_union;
  run_suite "exception"
    ~build:build_error
    ~write:(fun o p -> o#write p)
    ~read:read_rec_error
    ~depth_of:depth_error
    ~craft:craft_error;

  (* The exception wrapper itself is exercised so the generated
     "exception E of cls" shape is covered as well. *)
  (try raise (RecError (build_error 1)) with RecError _ -> check "exception wrapper raises/catches" true);

  Printf.printf "\nResults: %d passed, %d failed\n" !passed !failed;
  if !failed > 0 then exit 1
