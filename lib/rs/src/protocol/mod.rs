/*
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
 */

use transport::Transport;
use TResult;
use ThriftErr;

pub mod binary_protocol;

#[derive(Copy, Eq, PartialEq, Debug)]
pub enum Type {
    TStop = 0x00,
    TVoid = 0x01,
    TBool = 0x02,
    TByte = 0x03,
    TDouble = 0x04,
    TI16 = 0x06,
    TI32 = 0x08,
    TI64 = 0x0a,
    TString = 0x0b,
    TStruct = 0x0c,
    TMap = 0x0d,
    TSet = 0x0e,
    TList = 0x0f
}

impl Type {
    pub fn from_num(num: u64) -> Option<Type> {
        match num {
            0x00 => Some(Type::TStop),
            0x01 => Some(Type::TVoid),
            0x02 => Some(Type::TBool),
            0x03 => Some(Type::TByte),
            0x04 => Some(Type::TDouble),
            0x06 => Some(Type::TI16),
            0x08 => Some(Type::TI32),
            0x0a => Some(Type::TI64),
            0x0b => Some(Type::TString),
            0x0c => Some(Type::TStruct),
            0x0d => Some(Type::TMap),
            0x0e => Some(Type::TSet),
            0x0f => Some(Type::TList),
            _ => None,
        }
    }
}

#[derive(Copy, Eq, PartialEq, Debug)]
pub enum MessageType {
    MtCall = 0x01,
    MtReply = 0x02,
    MtException = 0x03,
}

impl MessageType {
    pub fn from_num(num: u64) -> Option<MessageType> {
        match num {
            0x01 => Some(MessageType::MtCall),
            0x02 => Some(MessageType::MtReply),
            0x03 => Some(MessageType::MtException),
            _ => None,
        }
    }
}

pub trait Writeable {
    fn write(&self, iprot: &Protocol, transport: &mut Transport) -> TResult<()>;
}

pub trait Readable {
    fn read(&mut self, iprot: &Protocol, transport: &mut Transport) -> TResult<()>;
}

pub trait Protocol {
    fn write_message_begin(
        &self,
        transport: &mut Transport,
        name: &str,
        message_type: MessageType,
        sequence_id: i32
    ) -> TResult<()>;
    fn write_message_end(&self, transport: &mut Transport) -> TResult<()>;

    fn write_struct_begin(&self, transport: &mut Transport, name: &str) -> TResult<()>;
    fn write_struct_end(&self, transport: &mut Transport) -> TResult<()>;

    fn write_field_begin(
        &self,
        transport: &mut Transport,
        name: &str,
        field_type: Type,
        field_id: i16
    ) -> TResult<()>;
    fn write_field_end(&self, transport: &mut Transport) -> TResult<()>;
    fn write_field_stop(&self, transport: &mut Transport) -> TResult<()>;

    fn write_map_begin(
        &self,
        transport: &mut Transport,
        key_type: Type,
        value_type: Type,
        size: usize
    ) -> TResult<()>;
    fn write_map_end(&self, transport: &mut Transport) -> TResult<()>;

    fn write_list_begin(&self, transport: &mut Transport, elem_type: Type, size: usize) -> TResult<()>;
    fn write_list_end(&self, transport: &mut Transport) -> TResult<()>;

    fn write_set_begin(&self, transport: &mut Transport, elem_type: Type, size: usize) -> TResult<()>;
    fn write_set_end(&self, transport: &mut Transport) -> TResult<()>;

    fn write_bool(&self, transport: &mut Transport, value: bool) -> TResult<()>;
    fn write_byte(&self, transport: &mut Transport, value: i8) -> TResult<()>;
    fn write_i16(&self, transport: &mut Transport, value: i16) -> TResult<()>;
    fn write_i32(&self, transport: &mut Transport, value: i32) -> TResult<()>;
    fn write_i64(&self, transport: &mut Transport, value: i64) -> TResult<()>;
    fn write_double(&self, transport: &mut Transport, value: f64) -> TResult<()>;
    fn write_str(&self, transport: &mut Transport, value: &str) -> TResult<()>;
    fn write_string(&self, transport: &mut Transport, value: &String) -> TResult<()>;
    fn write_binary(&self, transport: &mut Transport, value: &[u8]) -> TResult<()>;

    fn read_message_begin(&self, transport: &mut Transport) -> TResult<(String, MessageType, i32)>;
    fn read_message_end(&self, transport: &mut Transport) -> TResult<()>;

    fn read_struct_begin(&self, transport: &mut Transport) -> TResult<String>;
    fn read_struct_end(&self, transport: &mut Transport) -> TResult<()>;

    fn read_field_begin(&self, transport: &mut Transport) -> TResult<(String, Type, i16)>;
    fn read_field_end(&self, transport: &mut Transport) -> TResult<()>;

    fn read_map_begin(&self, transport: &mut Transport) -> TResult<(Type, Type, i32)>;
    fn read_map_end(&self, transport: &mut Transport) -> TResult<()>;

    fn read_list_begin(&self, transport: &mut Transport) -> TResult<(Type, i32)>;
    fn read_list_end(&self, transport: &mut Transport) -> TResult<()>;

    fn read_set_begin(&self, transport: &mut Transport) -> TResult<(Type, i32)>;
    fn read_set_end(&self, transport: &mut Transport) -> TResult<()>;

    fn read_bool(&self, transport: &mut Transport) -> TResult<bool>;
    fn read_byte(&self, transport: &mut Transport) -> TResult<i8>;
    fn read_i16(&self, transport: &mut Transport) -> TResult<i16>;
    fn read_i32(&self, transport: &mut Transport) -> TResult<i32>;
    fn read_i64(&self, transport: &mut Transport) -> TResult<i64>;
    fn read_double(&self, transport: &mut Transport) -> TResult<f64>;
    fn read_string(&self, transport: &mut Transport) -> TResult<String>;
    fn read_binary(&self, transport: &mut Transport) -> TResult<Vec<u8>>;

    fn skip(&self, transport: &mut Transport, type_: Type) -> TResult<()>;
}

pub trait FromNum {
    fn from_num(num: i32) -> Option<Self>;
}

pub struct ProtocolHelpers;

impl ProtocolHelpers {

    pub fn read_enum<T: FromNum>(iprot: &Protocol, transport: &mut Transport) -> TResult<T> {
        let i = try!(iprot.read_i32(transport));
        match <T as FromNum>::from_num(i) {
            Some(v) => Ok(v),
            None => Err(ThriftErr::ProtocolError),
        }
    }

    pub fn send<W: Writeable>(protocol: &Protocol, 
                          transport: &mut Transport,
                          name: &str, 
                          _type: MessageType, 
                          args: &W) -> TResult<()> {

        let cseqid: i32 = 0;
        try!(protocol.write_message_begin(transport, name, _type, cseqid));
        try!(args.write(protocol, transport));
        try!(protocol.write_message_end(transport));
        //self.transport.write_end();
        try!(transport.flush());

        Ok(())
    }

    pub fn receive<R: Readable>(protocol: &Protocol, 
                            transport: &mut Transport, 
                            op: &'static str, 
                            result: &mut R) -> TResult<()> {

        match try!(protocol.read_message_begin(transport)) {
            (_, MessageType::MtException, _) => {
                println!("got exception");
                // TODO
                //let x = ApplicationException;
                //x.read(&mut protocol)
                //protocol.read_message_end();
                //transport.read_end();
                //throw x     
                Err(ThriftErr::Exception)
            }
            (fname, MessageType::MtReply, _) => {
                if &fname[..] == op {
                    try!(result.read(protocol, transport));
                    try!(protocol.read_message_end(transport));
                    Ok(())
                 }
                else {
                  // FIXME: shall we err in this case?
                    try!(protocol.skip(transport, Type::TStruct));
                    try!(protocol.read_message_end(transport));
                    Err(ThriftErr::ProtocolError)
                }
            }
            (_, _, _) => {
                try!(protocol.skip(transport, Type::TStruct));
                try!(protocol.read_message_end(transport));
                Err(ThriftErr::ProtocolError)
            }
        }
    }

}
