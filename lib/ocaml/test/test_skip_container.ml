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

(* Skip-cost test for the OCaml library.

   skip (Thrift.Protocol.t) iterates a container over the element count the
   header declares, calling skip once per element. For that to stay bounded by
   the bytes received, every element type skip meets must read at least one
   byte, descend a nested value, or be refused. Wire types 16 and 17
   (T_UTF8/T_UTF16) used to return with no read, so a list/set/map header
   declaring a huge count let skip iterate that whole count while touching
   nothing on the transport -- cost detached from the wire, and multiplied
   again by every sibling field in a struct (the depth guard bounds nesting,
   not the per-container count nor the number of siblings).

   Like test_recursion_depth.ml this drives the real skip in Thrift.ml through
   an in-memory protocol; TBinaryProtocol.ml does not compile on modern OCaml,
   so its matching container-count cap is covered by reading, not here. The
   element reads counted below stand in for transport reads: a type that reads
   nothing leaves leaf_reads at zero. *)

open Thrift

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

(* An in-memory protocol whose container headers are scripted: readListBegin /
   readSetBegin / readMapBegin answer (elem, size) for the elem and size this
   object was built with, and readFieldBegin hands out [nfields] fields of type
   T_LIST before T_STOP (so a struct of sibling containers can be crafted).
   skip is wrapped to count how many times it is entered, and every leaf read
   bumps leaf_reads. *)
class scripted ~elem ~size ~nfields trans =
object (self)
  inherit Protocol.t trans as super

  val mutable skip_calls = 0
  val mutable leaf_reads = 0
  val mutable fields_left = nfields
  method skip_calls = skip_calls
  method leaf_reads = leaf_reads

  method! skip typ =
    skip_calls <- skip_calls + 1;
    super#skip typ

  method readListBegin = (elem, size)
  method readListEnd = ()
  method readSetBegin = (elem, size)
  method readSetEnd = ()
  method readMapBegin = (elem, elem, size)
  method readMapEnd = ()
  method readStructBegin = ""
  method readStructEnd = ()
  method readFieldBegin =
    if fields_left > 0 then begin
      fields_left <- fields_left - 1;
      ("", Protocol.T_LIST, fields_left + 1)
    end else ("", Protocol.T_STOP, 0)
  method readFieldEnd = ()

  method readBool = leaf_reads <- leaf_reads + 1; false
  method readByte = leaf_reads <- leaf_reads + 1; 0
  method readI16 = leaf_reads <- leaf_reads + 1; 0
  method readI32 = leaf_reads <- leaf_reads + 1; 0l
  method readI64 = leaf_reads <- leaf_reads + 1; 0L
  method readDouble = leaf_reads <- leaf_reads + 1; 0.0
  method readString = leaf_reads <- leaf_reads + 1; ""
  method readBinary = leaf_reads <- leaf_reads + 1; ""

  method readMessageBegin = failwith "unused"
  method readMessageEnd = failwith "unused"
  method writeMessageBegin _ = failwith "unused"
  method writeMessageEnd = failwith "unused"
  method writeStructBegin _ = failwith "unused"
  method writeStructEnd = failwith "unused"
  method writeFieldBegin _ = failwith "unused"
  method writeFieldEnd = failwith "unused"
  method writeFieldStop = failwith "unused"
  method writeMapBegin _ = failwith "unused"
  method writeMapEnd = failwith "unused"
  method writeListBegin _ = failwith "unused"
  method writeListEnd = failwith "unused"
  method writeSetBegin _ = failwith "unused"
  method writeSetEnd = failwith "unused"
  method writeBool _ = failwith "unused"
  method writeByte _ = failwith "unused"
  method writeI16 _ = failwith "unused"
  method writeI32 _ = failwith "unused"
  method writeI64 _ = failwith "unused"
  method writeDouble _ = failwith "unused"
  method writeString _ = failwith "unused"
  method writeBinary _ = failwith "unused"
end

let passed = ref 0
let failed = ref 0
let check label cond =
  if cond then begin Printf.printf "PASS: %s\n%!" label; incr passed end
  else begin Printf.printf "FAIL: %s\n%!" label; incr failed end

let is_invalid_data = function
  | Protocol.E (Protocol.INVALID_DATA, _) -> true | _ -> false

(* Skip [container] of [size] elements of type [elem]; report the outcome. *)
let run ~elem ~size ~nfields container =
  let p = new scripted ~elem ~size ~nfields (new null_transport) in
  let t0 = Unix.gettimeofday () in
  let outcome =
    try p#skip container; `Returned
    with e -> `Raised e in
  let dt = Unix.gettimeofday () -. t0 in
  (outcome, p#skip_calls, p#leaf_reads, dt)

let ms dt = dt *. 1000.

let () =
  (* 1. list<UTF8>, huge declared count: a zero-read element type is refused on
        the first element instead of spinning the whole count. *)
  let (o, calls, reads, dt) = run ~elem:Protocol.T_UTF8 ~size:1000000 ~nfields:0 Protocol.T_LIST in
  Printf.printf "list<UTF8> size=1000000: skip_calls=%d leaf_reads=%d elapsed_ms=%.1f\n%!" calls reads (ms dt);
  check "list<UTF8> is refused with INVALID_DATA"
    (match o with `Raised e -> is_invalid_data e | _ -> false);
  check "list<UTF8> stops at the first element (skip_calls = 2)" (calls = 2);
  check "list<UTF8> reads nothing past the header (leaf_reads = 0)" (reads = 0);

  (* 2. set<UTF16>: the other zero-read wire type, the other container. *)
  let (o, calls, _, _) = run ~elem:Protocol.T_UTF16 ~size:1000000 ~nfields:0 Protocol.T_SET in
  check "set<UTF16> is refused with INVALID_DATA"
    (match o with `Raised e -> is_invalid_data e | _ -> false);
  check "set<UTF16> stops at the first element (skip_calls = 2)" (calls = 2);

  (* 3. map<UTF8,UTF8>: refused on the first key. *)
  let (o, calls, _, _) = run ~elem:Protocol.T_UTF8 ~size:1000000 ~nfields:0 Protocol.T_MAP in
  check "map<UTF8,UTF8> is refused with INVALID_DATA"
    (match o with `Raised e -> is_invalid_data e | _ -> false);
  check "map<UTF8,UTF8> stops at the first key (skip_calls = 2)" (calls = 2);

  (* 4. Breadth: a struct of 64 sibling list<UTF8> fields. The depth guard only
        sees depth 3; without refusing the element type, cost is 64 x size. *)
  let (o, calls, _, dt) = run ~elem:Protocol.T_UTF8 ~size:1000000 ~nfields:64 Protocol.T_STRUCT in
  Printf.printf "struct of 64 list<UTF8> size=1000000: skip_calls=%d elapsed_ms=%.1f\n%!" calls (ms dt);
  check "struct of sibling list<UTF8> is refused on the first element"
    (match o with `Raised e -> is_invalid_data e | _ -> false);
  check "struct breadth stops early (skip_calls = 3, not 64 million)" (calls = 3);

  (* 5. Control: a normal list<byte> still skips in full, one read per element. *)
  let (o, calls, reads, _) = run ~elem:Protocol.T_BYTE ~size:1000 ~nfields:0 Protocol.T_LIST in
  check "list<byte> still skips to completion" (o = `Returned);
  check "list<byte> skips every element (skip_calls = 1001)" (calls = 1001);
  check "list<byte> reads one byte per element (leaf_reads = 1000)" (reads = 1000);

  (* 6. Control: list<i32> likewise. *)
  let (o, _, reads, _) = run ~elem:Protocol.T_I32 ~size:100 ~nfields:0 Protocol.T_LIST in
  check "list<i32> still skips to completion" (o = `Returned);
  check "list<i32> reads one i32 per element (leaf_reads = 100)" (reads = 100);

  Printf.printf "\nResults: %d passed, %d failed\n" !passed !failed;
  if !failed > 0 then exit 1
