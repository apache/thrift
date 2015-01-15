use protocol;
use protocol::{ MessageType, Protocol, Type };
use transport::Transport;
use std::num::FromPrimitive;

static BINARY_PROTOCOL_VERSION_1: u16 = 0x8001;

pub struct BinaryProtocol;

impl BinaryProtocol {
  fn write_type(&self, transport: &mut Transport, type_: Type) {
    self.write_byte(transport, type_ as i8);
  }

  fn read_type(&self, transport: &mut Transport) -> Type {
    let raw = self.read_byte(transport);
    match FromPrimitive::from_i8(raw) {
      Some(type_) => type_,
      None => panic!("unknown type {}", raw),
    }
  }
}

impl Protocol for BinaryProtocol {
  fn write_message_begin(
    &self, transport: &mut Transport,
    name: &str,
    message_type: MessageType,
    sequence_id: i32
  ) {
    let version = BINARY_PROTOCOL_VERSION_1 as i32 | message_type as i32;
    self.write_i32(transport, version);
    self.write_string(transport, name);
    self.write_i32(transport, sequence_id);
  }

  fn write_message_end(&self, _transport: &mut Transport) { }

  fn write_struct_begin(&self, _transport: &mut Transport, _name: &str) { }

  fn write_struct_end(&self, _transport: &mut Transport) { }

  fn write_field_begin(
    &self, transport: &mut Transport,
    _name: &str,
    field_type: Type,
    field_id: i16
  ) {
    self.write_type(transport, field_type);
    self.write_i16(transport, field_id);
  }

  fn write_field_end(&self, _transport: &mut Transport) { }

  fn write_field_stop(&self, transport: &mut Transport) {
    self.write_byte(transport, protocol::Type::TStop as i8);
  }

  fn write_map_begin(&self, transport: &mut Transport, key_type: Type, value_type: Type, size: i32) {
    self.write_type(transport, key_type);
    self.write_type(transport, value_type);
    self.write_i32(transport, size);
  }

  fn write_map_end(&self, _transport: &mut Transport) { }

  fn write_list_begin(&self, transport: &mut Transport, elem_type: Type, size: i32) {
    self.write_type(transport, elem_type);
    self.write_i32(transport, size);
  }

  fn write_list_end(&self, _transport: &mut Transport) { }

  fn write_set_begin(&self, transport: &mut Transport, elem_type: Type, size: i32) {
    self.write_type(transport, elem_type);
    self.write_i32(transport, size);
  }

  fn write_set_end(&self, _transport: &mut Transport) { }

  fn write_bool(&self, transport: &mut Transport, value: bool) {
    self.write_byte(transport, value as i8);
  }

  fn write_byte(&self, transport: &mut Transport, value: i8) {
    match transport.write_i8(value) {
      Ok(_) => (),
      Err(e) => panic!(e),
    }
  }

  fn write_i16(&self, transport: &mut Transport, value: i16) {
    match transport.write_be_i16(value) {
      Ok(_) => (),
      Err(e) => panic!(e),
    }
  }

  fn write_i32(&self, transport: &mut Transport, value: i32) {
    match transport.write_be_i32(value) {
      Ok(_) => (),
      Err(e) => panic!(e),
    }
  }

  fn write_i64(&self, transport: &mut Transport, value: i64) {
    match transport.write_be_i64(value) {
      Ok(_) => (),
      Err(e) => panic!(e),
    }
  }

  fn write_double(&self, transport: &mut Transport, value: f64) {
    match transport.write_be_f64(value) {
      Ok(_) => (),
      Err(e) => panic!(e),
    }
  }

  fn write_string(&self, transport: &mut Transport, value: &str) {
    self.write_binary(transport, value.as_bytes());
  }

  fn write_binary(&self, transport: &mut Transport, value: &[u8]) {
    self.write_i32(transport, value.len() as i32);
    match transport.write(value) {
      Ok(_) => (),
      Err(e) => panic!(e),
    }
  }

  fn read_message_begin(&self, transport: &mut Transport) -> (String, MessageType, i32) {
    let header = self.read_i32(transport);
    let version = (header >> 16) as u16;
    if version != BINARY_PROTOCOL_VERSION_1 {
      panic!("unknown protocol version: {:x}", version);
    };
    let name = self.read_string(transport);
    let raw_type = header & 0xff;
    let message_type = match FromPrimitive::from_i32(raw_type) {
      Some(t) => t,
      None => panic!("unknown message type {:x}", raw_type),
    };
    let sequence_id = self.read_i32(transport);
    (name, message_type, sequence_id)
  }

  fn read_message_end(&self, _transport: &mut Transport) { }

  fn read_struct_begin(&self, _transport: &mut Transport) -> String { String::from_str("") }

  fn read_struct_end(&self, _transport: &mut Transport) { }

  fn read_field_begin(&self, transport: &mut Transport) -> (String, Type, i16) {
    let field_type = self.read_type(transport);
    let field_id = match field_type {
      protocol::Type::TStop => 0,
      _ => self.read_i16(transport),
    };
    (String::from_str(""), field_type, field_id)
  }

  fn read_field_end(&self, _transport: &mut Transport) { }

  fn read_map_begin(&self, transport: &mut Transport) -> (Type, Type, i32) {
    let key_type = self.read_type(transport);
    let value_type = self.read_type(transport);
    let size = self.read_i32(transport);
    (key_type, value_type, size)
  }

  fn read_map_end(&self, _transport: &mut Transport) { }

  fn read_list_begin(&self, transport: &mut Transport) -> (Type, i32) {
    let elem_type = self.read_type(transport);
    let size = self.read_i32(transport);
    (elem_type, size)
  }

  fn read_list_end(&self, _transport: &mut Transport) { }

  fn read_set_begin(&self, transport: &mut Transport) -> (Type, i32) {
    let elem_type = self.read_type(transport);
    let size = self.read_i32(transport);
    (elem_type, size)
  }

  fn read_set_end(&self, _transport: &mut Transport) { }

  fn read_bool(&self, transport: &mut Transport) -> bool {
    match self.read_byte(transport) {
      0 => false,
      _ => true,
    }
  }

  fn read_byte(&self, transport: &mut Transport) -> i8 {
    transport.read_i8().unwrap()
  }

  fn read_i16(&self, transport: &mut Transport) -> i16 {
    transport.read_be_i16().unwrap()
  }

  fn read_i32(&self, transport: &mut Transport) -> i32 {
    transport.read_be_i32().unwrap()
  }

  fn read_i64(&self, transport: &mut Transport) -> i64 {
    transport.read_be_i64().unwrap()
  }

  fn read_double(&self, transport: &mut Transport) -> f64 {
    transport.read_be_f64().unwrap()
  }

  fn read_string(&self, transport: &mut Transport) -> String {
    String::from_utf8(self.read_binary(transport)).unwrap()
  }

  fn read_binary(&self, transport: &mut Transport) -> Vec<u8> {
    let len = self.read_i32(transport) as usize;
    transport.read_exact(len).unwrap()
  }
}

#[cfg(test)]
pub mod test;
