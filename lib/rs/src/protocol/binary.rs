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

use std::convert;
use byteorder::{BigEndian, ByteOrder, ReadBytesExt, WriteBytesExt};
use try_from;

use ::{Error, Result};
use ::transport::TTransport;
use super::{TFieldIdentifier, TType, TMessageIdentifier, TMessageType, TProtocol, TStructIdentifier};

/// Identifies the serialized message as conforming to Thrift binary protocol version 1.
const BINARY_PROTOCOL_VERSION_1: [u8; 2] = [0x80, 0x01];

/// Implementation of the Thrift binary protocol.
pub struct TBinaryProtocol {
    /// Set to `true` if the strict binary protocol is to be used.
    pub strict: bool,

    /// Underlying transport used to read protocol bytes from, and write protocol bytes to.
    pub transport: TTransport,
}

impl TProtocol for TBinaryProtocol {

    //
    // write methods follow
    //

    fn write_message_begin(&mut self, identifier: &TMessageIdentifier) -> Result<()> {
        if self.strict {
            try!(self.write_bytes(&BINARY_PROTOCOL_VERSION_1)); // FIXME: how can I avoid 3 calls here?
            try!(self.write_byte(0x00));
            try!(self.write_byte(identifier.message_type)); // FIXME: previously I did u8::from; how does this syntax work?
            try!(self.write_string(&identifier.name));
            self.write_i32(identifier.sequence_number)
        } else {
            try!(self.write_string(&identifier.name));
            try!(self.write_byte(identifier.message_type));
            self.write_i32(identifier.sequence_number)
        }
    }

    fn write_message_end(&mut self) -> Result<()> {
        Ok(()) // nothing written to mark message end
    }

    fn write_struct_begin(&mut self, _: &TStructIdentifier) -> Result<()> {
        Ok(()) // nothing written to mark struct beginning
    }

    fn write_struct_end(&mut self) -> Result<()> {
        Ok(()) // nothing written to mark struct end
    }

    fn write_field_begin(&mut self, identifier: &TFieldIdentifier) -> Result<()> {
        try!(self.write_byte(identifier.field_type));
        self.write_i16(identifier.id)
    }

    fn write_field_end(&mut self) -> Result<()> {
        Ok(()) // nothing written to mark field end
    }

    fn write_byte<I: convert::Into<u8>>(&mut self, b: I) -> Result<()> {
        self.transport.write_u8(b.into()).map_err(convert::From::from)
    }

    fn write_bytes(&mut self, b: &[u8]) -> Result<()> {
        try!(self.write_i32(b.len() as i32));
        self.write_transport(b)
    }

    fn write_bool(&mut self, b: bool) -> Result<()> {
        if b {
            self.write_i8(1)
        } else {
            self.write_i8(0)
        }
    }

    fn write_i8(&mut self, i: i8) -> Result<()> {
        self.transport.write_i8(i).map_err(convert::From::from)
    }

    fn write_i16(&mut self, i: i16) -> Result<()> {
        self.transport.write_i16::<BigEndian>(i).map_err(convert::From::from)
    }

    fn write_i32(&mut self, i: i32) -> Result<()> {
        self.transport.write_i32::<BigEndian>(i).map_err(convert::From::from)
    }

    fn write_i64(&mut self, i: i64) -> Result<()> {
        self.transport.write_i64::<BigEndian>(i).map_err(convert::From::from)
    }

    fn write_double(&mut self, d: f64) -> Result<()> {
        self.transport.write_f64::<BigEndian>(d).map_err(convert::From::from)
    }

    fn write_string(&mut self, s: &str) -> Result<()> {
        self.write_bytes(s.as_bytes())
    }

    fn flush(&mut self) -> Result<()> {
        self.transport.flush().map_err(convert::From::from)
    }

    //
    // read methods follow
    //

    fn read_message_begin(&mut self) -> Result<TMessageIdentifier> {
        let mut first_bytes = Vec::with_capacity(4);
        try!(self.transport.read_exact(&mut first_bytes));

        // the thrift version header is intentionally negative
        // so the first check we'll do is see if the sign bit is set
        // and if so - assume it's the protocol-version header
        if first_bytes[0] >= 8 {
            // apparently we got a protocol-version header - check
            // it, and if it matches, read the rest of the fields
            if first_bytes[0..1] != BINARY_PROTOCOL_VERSION_1 {
                Err(Error::InvalidThriftMessageHeader)
            } else {
                let message_type: TMessageType = try!(try_from::TryFrom::try_from(first_bytes[3]));
                let name = try!(self.read_string());
                let sequence_number = try!(self.read_i32());
                Ok(TMessageIdentifier { name: name, message_type: message_type, sequence_number: sequence_number })
            }
        } else {
            // apparently we didn't get a protocol-version header,
            // which happens if the sender is not using the strict protocol
            if self.strict {
                // we're in strict mode however, and that always
                // requires the protocol-version header to be written first
                Err(Error::InvalidThriftMessageHeader) // FIXME: this is a bad version header
            } else {
                // in the non-strict version the first message field
                // is the message name. strings (byte arrays) are length-prefixed,
                // so we've just read the length in the first 4 bytes
                let name_size = BigEndian::read_i32(&first_bytes) as usize;
                let mut name_buf: Vec<u8> = Vec::with_capacity(name_size);
                try!(self.transport.read_exact(&mut name_buf));
                let name = try!(String::from_utf8(name_buf));

                // read the rest of the fields
                let message_type: TMessageType = try!(self.read_byte().and_then(try_from::TryFrom::try_from));
                let sequence_number = try!(self.read_i32());
                Ok(TMessageIdentifier { name: name, message_type: message_type, sequence_number: sequence_number })
            }
        }
    }

    fn read_message_end(&mut self) -> Result<()> {
        Ok(()) // nothing has to be read for message end
    }

    fn read_struct_begin(&mut self) -> Result<Option<TStructIdentifier>> {
        Ok(None) // nothing has to be read for struct begin
    }

    fn read_struct_end(&mut self) -> Result<()> {
        Ok(()) // nothing has to be read for struct end
    }

    fn read_field_begin(&mut self) -> Result<TFieldIdentifier> {
        let field_type_byte = try!(self.read_byte());
        let field_type = try!(try_from::TryFrom::try_from(field_type_byte));
        let id = try!(match field_type {
            TType::Stop => Ok(0),
            _ => self.read_i16()
        });
        Ok(TFieldIdentifier { name: None, field_type: field_type, id: id })
    }

    fn read_field_end(&mut self) -> Result<()> {
        Ok(()) // nothing has to be read for field end
    }

    fn read_byte(&mut self) -> Result<u8> {
        self.transport.read_u8().map_err(convert::From::from)
    }

    fn read_bytes(&mut self) -> Result<Vec<u8>> {
        let num_bytes = try!(self.transport.read_i32::<BigEndian>()) as usize;
        let mut buf: Vec<u8> = Vec::with_capacity(num_bytes); // FIXME: how do I specify u8 as part of call?
        self.transport.read_exact(&mut buf).map(|_| buf).map_err(convert::From::from)
    }

    fn read_bool(&mut self) -> Result<bool> {
        let b = try!(self.read_i8());
        match b {
            0 => Ok(false),
            1 => Ok(true),
            v => Err(Error::InvalidBooleanValue(v))
        }
    }

    fn read_i8(&mut self) -> Result<i8> {
        self.transport.read_i8().map_err(convert::From::from)
    }

    fn read_i16(&mut self) -> Result<i16> {
        self.transport.read_i16::<BigEndian>().map_err(convert::From::from)
    }

    fn read_i32(&mut self) -> Result<i32> {
        self.transport.read_i32::<BigEndian>().map_err(convert::From::from)
    }

    fn read_i64(&mut self) -> Result<i64> {
        self.transport.read_i64::<BigEndian>().map_err(convert::From::from)
    }

    fn read_double(&mut self) -> Result<f64> {
        self.transport.read_f64::<BigEndian>().map_err(convert::From::from)
    }

    fn read_string(&mut self) -> Result<String> {
        let bytes = try!(self.read_bytes());
        String::from_utf8(bytes).map_err(convert::From::from)
    }
}

impl TBinaryProtocol {
    fn write_transport(&mut self, buf: &[u8]) -> Result<()> {
        self.transport.write(buf).map(|_| ()).map_err(convert::From::from)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}
