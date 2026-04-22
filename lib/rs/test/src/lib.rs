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
            TStructIdentifier, TType,
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
            // field 1: optional aid (union => Struct on wire)
            prot.write_field_begin(&TFieldIdentifier {
                name: None,
                field_type: TType::Struct,
                id: Some(1),
            })
            .unwrap();
            // inner union-as-struct
            prot.write_struct_begin(&TStructIdentifier {
                name: "MeasuringAids".to_owned(),
            })
            .unwrap();
            // unknown variant: use an id that doesn't exist (99), type i32
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
            // end outer field
            prot.write_field_end().unwrap();
            prot.write_field_stop().unwrap();
            prot.write_struct_end().unwrap();
        }

        // Read it back using generated code
        let read_cursor = Cursor::new(write_buf);
        let mut rprot = TBinaryInputProtocol::new(read_cursor, false);
        let kit = base_one::AidKit::read_from_in_protocol(&mut rprot)
            .expect("forward-compat deserialization should succeed");

        assert!(
            kit.aid.is_none(),
            "unknown union variant should result in None field (forward compat)"
        );
    }
}
