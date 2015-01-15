use transport::Transport;

pub mod binary_protocol;

#[derive(Eq, PartialEq, FromPrimitive, Show)]
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

#[derive(Eq, PartialEq, FromPrimitive, Show)]
pub enum MessageType {
  MtCall = 0x01,
  MtReply = 0x02,
  MtException = 0x03,
}

pub trait Protocol {
  fn write_message_begin(
    &self,
    transport: &mut Transport,
    name: &str,
    message_type: MessageType,
    sequence_id: i32
  );
  fn write_message_end(&self, transport: &mut Transport);

  fn write_struct_begin(&self, transport: &mut Transport, name: &str);
  fn write_struct_end(&self, transport: &mut Transport);

  fn write_field_begin(
    &self,
    transport: &mut Transport,
    name: &str,
    field_type: Type,
    field_id: i16
  );
  fn write_field_end(&self, transport: &mut Transport);
  fn write_field_stop(&self, transport: &mut Transport);

  fn write_map_begin(
    &self,
    transport: &mut Transport,
    key_type: Type,
    value_type: Type,
    size: i32
  );
  fn write_map_end(&self, transport: &mut Transport);

  fn write_list_begin(&self, transport: &mut Transport, elem_type: Type, size: i32);
  fn write_list_end(&self, transport: &mut Transport);

  fn write_set_begin(&self, transport: &mut Transport, elem_type: Type, size: i32);
  fn write_set_end(&self, transport: &mut Transport);

  fn write_bool(&self, transport: &mut Transport, value: bool);
  fn write_byte(&self, transport: &mut Transport, value: i8);
  fn write_i16(&self, transport: &mut Transport, value: i16);
  fn write_i32(&self, transport: &mut Transport, value: i32);
  fn write_i64(&self, transport: &mut Transport, value: i64);
  fn write_double(&self, transport: &mut Transport, value: f64);
  fn write_string(&self, transport: &mut Transport, value: &str);
  fn write_binary(&self, transport: &mut Transport, value: &[u8]);

  fn read_message_begin(&self, transport: &mut Transport) -> (String, MessageType, i32);
  fn read_message_end(&self, transport: &mut Transport);

  fn read_struct_begin(&self, transport: &mut Transport) -> String;
  fn read_struct_end(&self, transport: &mut Transport);

  fn read_field_begin(&self, transport: &mut Transport) -> (String, Type, i16);
  fn read_field_end(&self, transport: &mut Transport);

  fn read_map_begin(&self, transport: &mut Transport) -> (Type, Type, i32);
  fn read_map_end(&self, transport: &mut Transport);

  fn read_list_begin(&self, transport: &mut Transport) -> (Type, i32);
  fn read_list_end(&self, transport: &mut Transport);

  fn read_set_begin(&self, transport: &mut Transport) -> (Type, i32);
  fn read_set_end(&self, transport: &mut Transport);

  fn read_bool(&self, transport: &mut Transport) -> bool;
  fn read_byte(&self, transport: &mut Transport) -> i8;
  fn read_i16(&self, transport: &mut Transport) -> i16;
  fn read_i32(&self, transport: &mut Transport) -> i32;
  fn read_i64(&self, transport: &mut Transport) -> i64;
  fn read_double(&self, transport: &mut Transport) -> f64;
  fn read_string(&self, transport: &mut Transport) -> String;
  fn read_binary(&self, transport: &mut Transport) -> Vec<u8>;
}
