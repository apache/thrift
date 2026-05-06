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
}
