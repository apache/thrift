// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

pub mod base_one;
pub mod base_two;
pub mod midlayer;
pub mod recursive;
pub mod ultimate;

#[cfg(test)]
mod tests {

    use std::default::Default;

    use super::*;

    #[test]
    fn must_be_able_to_use_constructor() {
        let _ = midlayer::Meal::new(Some(base_one::Noodle::default()), None);
    }

    #[test]
    fn must_be_able_to_use_constructor_with_no_fields() {
        let _ = midlayer::Meal::new(None, None);
    }

    #[test]
    fn must_be_able_to_use_constructor_without_option_wrap() {
        let _ = midlayer::Meal::new(base_one::Noodle::default(), None);
    }

    #[test]
    fn must_be_able_to_use_defaults() {
        let _ = midlayer::Meal {
            noodle: Some(base_one::Noodle::default()),
            ..Default::default()
        };
    }

    #[test]
    fn unknown_union_variant_in_struct_field_is_treated_as_none() {
        use std::io::Cursor;

        use thrift::protocol::{
            TBinaryInputProtocol, TBinaryOutputProtocol, TFieldIdentifier, TOutputProtocol,
            TSerializable, TStructIdentifier, TType,
        };

        // Serialize AidKit with an unknown union variant (id=99), verify it deserializes as None.

        let mut write_buf: Vec<u8> = Vec::new();
        {
            let cursor = Cursor::new(&mut write_buf);
            let mut prot = TBinaryOutputProtocol::new(cursor, false);

            prot.write_struct_begin(&TStructIdentifier {
                name: "AidKit".to_owned(),
            })
            .unwrap();
            prot.write_field_begin(&TFieldIdentifier {
                name: None,
                field_type: TType::Struct,
                id: Some(1),
            })
            .unwrap();
            prot.write_struct_begin(&TStructIdentifier {
                name: "MeasuringAids".to_owned(),
            })
            .unwrap();
            prot.write_field_begin(&TFieldIdentifier {
                name: None,
                field_type: TType::I32,
                id: Some(99),
            })
            .unwrap();
            prot.write_i32(42).unwrap();
            prot.write_field_end().unwrap();
            prot.write_field_stop().unwrap();
            prot.write_struct_end().unwrap();
            prot.write_field_end().unwrap();
            prot.write_field_stop().unwrap();
            prot.write_struct_end().unwrap();
        }

        let read_cursor = Cursor::new(write_buf);
        let mut rprot = TBinaryInputProtocol::new(read_cursor, false);
        let kit = base_one::AidKit::read_from_in_protocol(&mut rprot)
            .expect("forward-compat deserialization should succeed");

        assert!(
            kit.aid.is_none(),
            "unknown union variant should result in None field (forward compat)"
        );
    }

    #[test]
    fn union_with_known_and_unknown_fields_deserializes_to_known_variant() {
        use std::io::Cursor;

        use thrift::protocol::{
            TBinaryInputProtocol, TBinaryOutputProtocol, TFieldIdentifier, TOutputProtocol,
            TSerializable, TStructIdentifier, TType,
        };

        // Regression test for total_field_count: a newer server may send a union
        // with a recognised variant (id=2, MeasuringCup) plus an extra unknown
        // field (id=99).  The deserializer must return the known variant rather
        // than failing with "received multiple fields for union".
        let mut buf: Vec<u8> = Vec::new();
        {
            let mut prot = TBinaryOutputProtocol::new(Cursor::new(&mut buf), false);
            prot.write_struct_begin(&TStructIdentifier {
                name: "MeasuringAids".to_owned(),
            })
            .unwrap();
            // Known variant: MeasuringCup (field id=2)
            prot.write_field_begin(&TFieldIdentifier {
                name: None,
                field_type: TType::Struct,
                id: Some(2),
            })
            .unwrap();
            prot.write_struct_begin(&TStructIdentifier {
                name: "MeasuringCup".to_owned(),
            })
            .unwrap();
            prot.write_field_begin(&TFieldIdentifier {
                name: None,
                field_type: TType::Double,
                id: Some(1),
            })
            .unwrap();
            prot.write_double(250.0).unwrap();
            prot.write_field_end().unwrap();
            prot.write_field_stop().unwrap();
            prot.write_struct_end().unwrap();
            prot.write_field_end().unwrap();
            // Unknown field from a newer schema (field id=99)
            prot.write_field_begin(&TFieldIdentifier {
                name: None,
                field_type: TType::I32,
                id: Some(99),
            })
            .unwrap();
            prot.write_i32(0).unwrap();
            prot.write_field_end().unwrap();
            prot.write_field_stop().unwrap();
            prot.write_struct_end().unwrap();
        }

        let aids = base_one::MeasuringAids::read_from_in_protocol(&mut TBinaryInputProtocol::new(
            Cursor::new(buf),
            false,
        ))
        .expect("union with a known variant plus an unknown field must deserialize successfully");

        assert!(
            matches!(aids, base_one::MeasuringAids::Cup(_)),
            "known variant (Cup) must be returned when an unknown field is also present"
        );
    }

    #[test]
    fn nested_union_unknown_variant_does_not_corrupt_stream() {
        use std::io::Cursor;

        use thrift::protocol::{
            TBinaryInputProtocol, TBinaryOutputProtocol, TFieldIdentifier, TOutputProtocol,
            TSerializable, TStructIdentifier, TType,
        };

        // Regression test for stream corruption: when suppress_unknown catches
        // an UnknownUnionVariant error from a union field whose variant value is
        // itself a union, the outer union's Stop byte must have been consumed
        // before the error propagates.  If it hasn't, the enclosing struct loop
        // reads that orphaned 0x00 as its own Stop and silently drops every
        // subsequent field.
        let mut buf: Vec<u8> = Vec::new();
        {
            let mut prot = TBinaryOutputProtocol::new(Cursor::new(&mut buf), false);
            // InstrumentBox
            prot.write_struct_begin(&TStructIdentifier {
                name: "InstrumentBox".to_owned(),
            })
            .unwrap();
            // field 1: InstrumentUnion
            prot.write_field_begin(&TFieldIdentifier {
                name: None,
                field_type: TType::Struct,
                id: Some(1),
            })
            .unwrap();
            // InstrumentUnion: known variant 1 whose value (MeasuringAids) carries
            // only an unknown sub-variant, so MeasuringAids::read returns
            // UnknownUnionVariant, which propagates via ? before InstrumentUnion
            // has consumed its own Stop byte.
            prot.write_struct_begin(&TStructIdentifier {
                name: "InstrumentUnion".to_owned(),
            })
            .unwrap();
            prot.write_field_begin(&TFieldIdentifier {
                name: None,
                field_type: TType::Struct,
                id: Some(1),
            })
            .unwrap();
            prot.write_struct_begin(&TStructIdentifier {
                name: "MeasuringAids".to_owned(),
            })
            .unwrap();
            prot.write_field_begin(&TFieldIdentifier {
                name: None,
                field_type: TType::I32,
                id: Some(99),
            })
            .unwrap();
            prot.write_i32(0).unwrap();
            prot.write_field_end().unwrap();
            prot.write_field_stop().unwrap();
            prot.write_struct_end().unwrap();
            prot.write_field_end().unwrap();
            prot.write_field_stop().unwrap(); // InstrumentUnion's Stop
            prot.write_struct_end().unwrap();
            prot.write_field_end().unwrap();
            // field 2: tag=42 — must survive even if field 1 triggers suppress_unknown
            prot.write_field_begin(&TFieldIdentifier {
                name: None,
                field_type: TType::I32,
                id: Some(2),
            })
            .unwrap();
            prot.write_i32(42).unwrap();
            prot.write_field_end().unwrap();
            prot.write_field_stop().unwrap();
            prot.write_struct_end().unwrap();
        }

        let ibox = base_one::InstrumentBox::read_from_in_protocol(&mut TBinaryInputProtocol::new(
            Cursor::new(buf),
            false,
        ))
        .expect("struct with a nested unknown union variant must deserialize without error");

        assert!(
            ibox.instrument.is_none(),
            "outer union with nested unknown variant must become None"
        );
        assert_eq!(
            ibox.tag,
            Some(42),
            "field following a suppressed union must not be silently dropped due to stream corruption"
        );
    }

    // ---- Recursion depth limiting (THRIFT-6057) ----
    //
    // These exercise the generated read/write recursion guard (a thread-local
    // DepthGuard acquired at the top of every struct/union read_from_in_protocol
    // and write_to_out_protocol) through full round-trips over the recursive
    // types in test/Recursive.thrift, not by poking the guard directly.
    //
    // NB: both skip() and the struct-read guard raise ProtocolErrorKind::DepthLimit,
    // so a crafted over-limit payload must use the *known* recursive field (id 1,
    // a struct) which the generated reader matches-and-recurses into -- an unknown
    // field would be skip()-ed and trip the unrelated skip guard. The over-limit
    // assertions therefore also rely on the master baseline (no DepthGuard emitted)
    // to confirm the new guard -- not skip -- is what fires.

    const LIMIT: usize = 64;

    fn build_co_rec(depth: usize) -> recursive::CoRec {
        if depth <= 1 {
            recursive::CoRec { other: None }
        } else {
            recursive::CoRec {
                other: Some(Box::new(build_co_rec2(depth - 1))),
            }
        }
    }

    fn build_co_rec2(depth: usize) -> recursive::CoRec2 {
        if depth <= 1 {
            recursive::CoRec2 { other: None }
        } else {
            recursive::CoRec2 {
                other: Some(build_co_rec(depth - 1)),
            }
        }
    }

    fn build_co_error(depth: usize) -> recursive::CoError {
        if depth <= 1 {
            recursive::CoError { other: None }
        } else {
            recursive::CoError {
                other: Some(Box::new(build_co_error2(depth - 1))),
            }
        }
    }

    fn build_co_error2(depth: usize) -> recursive::CoError2 {
        if depth <= 1 {
            recursive::CoError2 { other: None }
        } else {
            recursive::CoError2 {
                other: Some(build_co_error(depth - 1)),
            }
        }
    }

    // Write a `depth`-deep nesting of structs that each carry the recursive
    // field (id 1, a struct) using raw protocol calls, so no write-side guard is
    // involved -- reading it back through a generated reader is what must trip
    // the read-side guard.
    fn write_nested_chain(prot: &mut dyn thrift::protocol::TOutputProtocol, depth: usize) {
        use thrift::protocol::{TFieldIdentifier, TOutputProtocol, TStructIdentifier, TType};
        prot.write_struct_begin(&TStructIdentifier {
            name: "Rec".to_owned(),
        })
        .unwrap();
        if depth > 1 {
            prot.write_field_begin(&TFieldIdentifier {
                name: None,
                field_type: TType::Struct,
                id: Some(1),
            })
            .unwrap();
            write_nested_chain(prot, depth - 1);
            prot.write_field_end().unwrap();
        }
        prot.write_field_stop().unwrap();
        prot.write_struct_end().unwrap();
    }

    fn assert_depth_limit<T: std::fmt::Debug>(result: thrift::Result<T>) {
        match result {
            Err(thrift::Error::Protocol(pe)) => assert_eq!(
                pe.kind,
                thrift::ProtocolErrorKind::DepthLimit,
                "expected DepthLimit, got {:?}",
                pe
            ),
            other => panic!("expected DepthLimit protocol error, got {:?}", other),
        }
    }

    #[test]
    fn recursion_depth_struct_round_trip_and_limit() {
        use std::io::Cursor;
        use thrift::protocol::{TBinaryInputProtocol, TBinaryOutputProtocol, TSerializable};

        // A chain exactly at the limit round-trips and is value-preserving.
        let original = build_co_rec(LIMIT);
        let mut buf: Vec<u8> = Vec::new();
        {
            let mut o = TBinaryOutputProtocol::new(Cursor::new(&mut buf), false);
            original
                .write_to_out_protocol(&mut o)
                .expect("at-limit write must succeed");
        }
        let read_back = recursive::CoRec::read_from_in_protocol(&mut TBinaryInputProtocol::new(
            Cursor::new(buf),
            false,
        ))
        .expect("at-limit read must succeed");
        assert_eq!(original, read_back);

        // One level past the limit: the write is rejected (counter restored on drop).
        let mut o = TBinaryOutputProtocol::new(Cursor::new(Vec::new()), false);
        assert_depth_limit(build_co_rec(LIMIT + 1).write_to_out_protocol(&mut o));

        // One level past the limit: a crafted payload is rejected on read.
        let mut deep: Vec<u8> = Vec::new();
        {
            // craft with an unbounded writer so the bound is exercised only on read
            let mut o = TBinaryOutputProtocol::with_config(
                Cursor::new(&mut deep),
                false,
                thrift::TConfiguration::no_limits(),
            );
            write_nested_chain(&mut o, LIMIT + 1);
        }
        assert_depth_limit(recursive::CoRec::read_from_in_protocol(
            &mut TBinaryInputProtocol::new(Cursor::new(deep), false),
        ));
    }

    #[test]
    fn recursion_depth_exception_round_trip_and_limit() {
        use std::io::Cursor;
        use thrift::protocol::{TBinaryInputProtocol, TBinaryOutputProtocol, TSerializable};

        let original = build_co_error(LIMIT);
        let mut buf: Vec<u8> = Vec::new();
        {
            let mut o = TBinaryOutputProtocol::new(Cursor::new(&mut buf), false);
            original
                .write_to_out_protocol(&mut o)
                .expect("at-limit write must succeed");
        }
        let read_back = recursive::CoError::read_from_in_protocol(&mut TBinaryInputProtocol::new(
            Cursor::new(buf),
            false,
        ))
        .expect("at-limit read must succeed");
        assert_eq!(original, read_back);

        let mut o = TBinaryOutputProtocol::new(Cursor::new(Vec::new()), false);
        assert_depth_limit(build_co_error(LIMIT + 1).write_to_out_protocol(&mut o));

        let mut deep: Vec<u8> = Vec::new();
        {
            // craft with an unbounded writer so the bound is exercised only on read
            let mut o = TBinaryOutputProtocol::with_config(
                Cursor::new(&mut deep),
                false,
                thrift::TConfiguration::no_limits(),
            );
            write_nested_chain(&mut o, LIMIT + 1);
        }
        assert_depth_limit(recursive::CoError::read_from_in_protocol(
            &mut TBinaryInputProtocol::new(Cursor::new(deep), false),
        ));
    }

    #[test]
    fn recursion_depth_union_read_limit() {
        use std::io::Cursor;
        use thrift::protocol::{TBinaryInputProtocol, TBinaryOutputProtocol, TSerializable};

        // CoUnion has only the recursive variant (no leaf), so a finite value
        // cannot be constructed/written; exercise the union read guard with a
        // crafted over-limit payload.
        let mut deep: Vec<u8> = Vec::new();
        {
            // craft with an unbounded writer so the bound is exercised only on read
            let mut o = TBinaryOutputProtocol::with_config(
                Cursor::new(&mut deep),
                false,
                thrift::TConfiguration::no_limits(),
            );
            write_nested_chain(&mut o, LIMIT + 1);
        }
        assert_depth_limit(recursive::CoUnion::read_from_in_protocol(
            &mut TBinaryInputProtocol::new(Cursor::new(deep), false),
        ));
    }

    // The bound lives in the protocol's struct read/write, so it applies to every
    // generated type uniformly; the binary tests above cover struct/union/exception
    // routing. This confirms the compact protocol enforces the same bound.
    #[test]
    fn recursion_depth_compact_round_trip_and_limit() {
        use std::io::Cursor;
        use thrift::protocol::{TCompactInputProtocol, TCompactOutputProtocol, TSerializable};

        let original = build_co_rec(LIMIT);
        let mut buf: Vec<u8> = Vec::new();
        {
            let mut o = TCompactOutputProtocol::new(Cursor::new(&mut buf));
            original
                .write_to_out_protocol(&mut o)
                .expect("at-limit write must succeed");
        }
        let read_back = recursive::CoRec::read_from_in_protocol(&mut TCompactInputProtocol::new(
            Cursor::new(buf),
        ))
        .expect("at-limit read must succeed");
        assert_eq!(original, read_back);

        let mut o = TCompactOutputProtocol::new(Cursor::new(Vec::new()));
        assert_depth_limit(build_co_rec(LIMIT + 1).write_to_out_protocol(&mut o));

        let mut deep: Vec<u8> = Vec::new();
        {
            // craft with an unbounded writer so the bound is exercised only on read
            let mut o = TCompactOutputProtocol::with_config(
                Cursor::new(&mut deep),
                thrift::TConfiguration::no_limits(),
            );
            write_nested_chain(&mut o, LIMIT + 1);
        }
        assert_depth_limit(recursive::CoRec::read_from_in_protocol(
            &mut TCompactInputProtocol::new(Cursor::new(deep)),
        ));
    }
}
