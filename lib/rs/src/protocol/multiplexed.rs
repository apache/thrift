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

use super::{TFieldIdentifier, TListIdentifier, TMapIdentifier, TMessageIdentifier, TMessageType,
            TOutputProtocol, TSetIdentifier, TStructIdentifier};

/// `TOutputProtocol` that prefixes the service name to all outgoing Thrift
/// messages.
///
/// A `TMultiplexedOutputProtocol` should be used when multiple Thrift services
/// send messages over a single I/O channel. By prefixing service identifiers
/// to outgoing messages receivers are able to demux them and route them to the
/// appropriate service processor. Rust receivers must use a `TMultiplexedProcessor`
/// to process incoming messages, while other languages must use their
/// corresponding multiplexed processor implementations.
///
/// For example, given a service `TestService` and a service call `test_call`,
/// this implementation would identify messages as originating from
/// `TestService:test_call`.
///
/// # Examples
///
/// Create and use a `TMultiplexedOutputProtocol`.
///
/// ```no_run
/// use std::cell::RefCell;
/// use std::rc::Rc;
/// use thrift::protocol::{TMessageIdentifier, TMessageType, TOutputProtocol};
/// use thrift::protocol::{TBinaryOutputProtocol, TMultiplexedOutputProtocol};
/// use thrift::transport::{TTcpTransport, TTransport};
///
/// let mut transport = TTcpTransport::new();
/// transport.open("localhost:9090").unwrap();
/// let transport = Rc::new(RefCell::new(Box::new(transport) as Box<TTransport>));
///
/// let o_prot = TBinaryOutputProtocol::new(transport, true);
/// let mut o_prot = TMultiplexedOutputProtocol::new("service_name", Box::new(o_prot));
///
/// let ident = TMessageIdentifier::new("svc_call", TMessageType::Call, 1);
/// o_prot.write_message_begin(&ident).unwrap();
/// ```
pub struct TMultiplexedOutputProtocol {
    service_name: String,
    inner: Box<TOutputProtocol>,
}

impl TMultiplexedOutputProtocol {
    /// Create a `TMultiplexedOutputProtocol` that identifies outgoing messages
    /// as originating from a service named `service_name` and sends them over
    /// the `wrapped` `TOutputProtocol`. Outgoing messages are encoded and sent
    /// by `wrapped`, not by this instance.
    pub fn new(service_name: &str, wrapped: Box<TOutputProtocol>) -> TMultiplexedOutputProtocol {
        TMultiplexedOutputProtocol {
            service_name: service_name.to_owned(),
            inner: wrapped,
        }
    }
}

// FIXME: avoid passthrough methods
impl TOutputProtocol for TMultiplexedOutputProtocol {
    fn write_message_begin(&mut self, identifier: &TMessageIdentifier) -> ::Result<()> {
        match identifier.message_type { // FIXME: is there a better way to override identifier here?
            TMessageType::Call | TMessageType::OneWay => {
                let identifier = TMessageIdentifier {
                    name: format!("{}:{}", self.service_name, identifier.name),
                    ..*identifier
                };
                self.inner.write_message_begin(&identifier)
            }
            _ => self.inner.write_message_begin(identifier),
        }
    }

    fn write_message_end(&mut self) -> ::Result<()> {
        self.inner.write_message_end()
    }

    fn write_struct_begin(&mut self, identifier: &TStructIdentifier) -> ::Result<()> {
        self.inner.write_struct_begin(identifier)
    }

    fn write_struct_end(&mut self) -> ::Result<()> {
        self.inner.write_struct_end()
    }

    fn write_field_begin(&mut self, identifier: &TFieldIdentifier) -> ::Result<()> {
        self.inner.write_field_begin(identifier)
    }

    fn write_field_end(&mut self) -> ::Result<()> {
        self.inner.write_field_end()
    }

    fn write_field_stop(&mut self) -> ::Result<()> {
        self.inner.write_field_stop()
    }

    fn write_bytes(&mut self, b: &[u8]) -> ::Result<()> {
        self.inner.write_bytes(b)
    }

    fn write_bool(&mut self, b: bool) -> ::Result<()> {
        self.inner.write_bool(b)
    }

    fn write_i8(&mut self, i: i8) -> ::Result<()> {
        self.inner.write_i8(i)
    }

    fn write_i16(&mut self, i: i16) -> ::Result<()> {
        self.inner.write_i16(i)
    }

    fn write_i32(&mut self, i: i32) -> ::Result<()> {
        self.inner.write_i32(i)
    }

    fn write_i64(&mut self, i: i64) -> ::Result<()> {
        self.inner.write_i64(i)
    }

    fn write_double(&mut self, d: f64) -> ::Result<()> {
        self.inner.write_double(d)
    }

    fn write_string(&mut self, s: &str) -> ::Result<()> {
        self.inner.write_string(s)
    }

    fn write_list_begin(&mut self, identifier: &TListIdentifier) -> ::Result<()> {
        self.inner.write_list_begin(identifier)
    }

    fn write_list_end(&mut self) -> ::Result<()> {
        self.inner.write_list_end()
    }

    fn write_set_begin(&mut self, identifier: &TSetIdentifier) -> ::Result<()> {
        self.inner.write_set_begin(identifier)
    }

    fn write_set_end(&mut self) -> ::Result<()> {
        self.inner.write_set_end()
    }

    fn write_map_begin(&mut self, identifier: &TMapIdentifier) -> ::Result<()> {
        self.inner.write_map_begin(identifier)
    }

    fn write_map_end(&mut self) -> ::Result<()> {
        self.inner.write_map_end()
    }

    fn flush(&mut self) -> ::Result<()> {
        self.inner.flush()
    }

    // utility
    //

    fn write_byte(&mut self, b: u8) -> ::Result<()> {
        self.inner.write_byte(b)
    }
}

#[cfg(test)]
mod tests {

    use std::cell::RefCell;
    use std::rc::Rc;

    use ::protocol::{TBinaryOutputProtocol, TMessageIdentifier, TMessageType, TOutputProtocol};
    use ::transport::{TPassThruTransport, TTransport};
    use ::transport::mem::TBufferTransport;

    use super::*;

    #[test]
    fn must_write_message_begin_with_prefixed_service_name() {
        let (trans, mut o_prot) = test_objects();

        let ident = TMessageIdentifier::new("bar", TMessageType::Call, 2);
        assert_success!(o_prot.write_message_begin(&ident));

        let expected: [u8; 19] =
            [0x80, 0x01 /* protocol identifier */, 0x00, 0x01 /* message type */, 0x00,
             0x00, 0x00, 0x07, 0x66, 0x6F, 0x6F /* "foo" */, 0x3A /* ":" */, 0x62, 0x61,
             0x72 /* "bar" */, 0x00, 0x00, 0x00, 0x02 /* sequence number */];

        assert_eq!(&trans.borrow().write_buffer_to_vec(), &expected);
    }

    fn test_objects() -> (Rc<RefCell<Box<TBufferTransport>>>, TMultiplexedOutputProtocol) {
        let mem = Rc::new(RefCell::new(Box::new(TBufferTransport::with_capacity(40, 40))));

        let inner: Box<TTransport> = Box::new(TPassThruTransport { inner: mem.clone() });
        let inner = Rc::new(RefCell::new(inner));

        let o_prot = TBinaryOutputProtocol::new(inner.clone(), true);
        let o_prot = TMultiplexedOutputProtocol::new("foo", Box::new(o_prot));

        (mem, o_prot)
    }
}
