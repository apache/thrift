use super::BinaryProtocol;

use protocol;
use protocol::Protocol;
use transport::test::FakeTransport;

#[test]
pub fn read_bool() {
  let transport = &mut FakeTransport::new(vec!(0x00, 0x01, 0xff));
  let protocol = BinaryProtocol;
  assert_eq!(protocol.read_bool(transport), false);
  assert_eq!(protocol.read_bool(transport), true);
  assert_eq!(protocol.read_bool(transport), true);
}

#[test]
pub fn read_byte() {
  let transport = &mut FakeTransport::new(vec!(0xa4, 0x27));
  let protocol = BinaryProtocol;
  assert_eq!(protocol.read_byte(transport), -0x5c);
  assert_eq!(protocol.read_byte(transport), 0x27);
}

#[test]
pub fn read_i16() {
  let transport = &mut FakeTransport::new(vec!(0xf2, 0xf8, 0xa1, 0x40));
  let protocol = BinaryProtocol;
  assert_eq!(protocol.read_i16(transport), -0x0d08);
  assert_eq!(protocol.read_i16(transport), -0x5ec0);
}

#[test]
pub fn read_i32() {
  let transport = &mut FakeTransport::new(vec!(0x27, 0xd0, 0x39, 0x49, 0xe5, 0xd8, 0xfe, 0x8b));
  let protocol = BinaryProtocol;
  assert_eq!(protocol.read_i32(transport), 0x27d03949);
  assert_eq!(protocol.read_i32(transport), -0x1a270175);
}

#[test]
pub fn read_i64() {
  let transport = &mut FakeTransport::new(vec!(
    0x27, 0xd0, 0x39, 0x49, 0xe5, 0xd8, 0xfe, 0x8b,
    0xa7, 0x2e, 0x82, 0xea, 0xd1, 0x28, 0x0b, 0xe2,
  ));
  let protocol = BinaryProtocol;
  assert_eq!(protocol.read_i64(transport), 0x27d03949e5d8fe8b);
  assert_eq!(protocol.read_i64(transport), -0x58d17d152ed7f41e);
}

#[test]
pub fn read_double() {
  let transport = &mut FakeTransport::new(vec!(
    0x40, 0xa9, 0x5e, 0xaf, 0x39, 0x4b, 0x7b, 0x29,
    0xbf, 0xe9, 0x3a, 0xe4, 0x21, 0xd3, 0x0e, 0x85,
  ));
  let protocol = BinaryProtocol;
  assert_eq!(protocol.read_double(transport), 3247.342234);
  assert_eq!(protocol.read_double(transport), -0.78843886);
}

#[test]
pub fn read_string() {
  let transport = &mut FakeTransport::new(vec!(
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x04, 0x41, 0x73, 0x64, 0x66,
    0x00, 0x00, 0x00, 0x0d, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0x57, 0x6f, 0x72, 0x6c, 0x64, 0x21,
  ));
  let protocol = BinaryProtocol;
  assert_eq!(protocol.read_string(transport), String::from_str(""));
  assert_eq!(protocol.read_string(transport), String::from_str("Asdf"));
  assert_eq!(protocol.read_string(transport), String::from_str("Hello, World!"));
}

#[test]
pub fn read_binary() {
  let transport = &mut FakeTransport::new(vec!(
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x04, 0x41, 0x73, 0x64, 0x66,
    0x00, 0x00, 0x00, 0x0d, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0x57, 0x6f, 0x72, 0x6c, 0x64, 0x21,
  ));
  let protocol = BinaryProtocol;
  assert_eq!(protocol.read_binary(transport), vec!());
  assert_eq!(protocol.read_binary(transport), vec!(0x41, 0x73, 0x64, 0x66));
  assert_eq!(protocol.read_binary(transport), vec!(0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0x57, 0x6f, 0x72, 0x6c, 0x64, 0x21));
}

#[test]
pub fn read_set_begin() {
  let transport = &mut FakeTransport::new(vec!(0x0b, 0x00, 0x00, 0x01, 0x0f));
  let protocol = BinaryProtocol;
  assert_eq!(protocol.read_set_begin(transport), (protocol::Type::TString, 0x0000010f));
}

#[test]
pub fn read_list_begin() {
  let transport = &mut FakeTransport::new(vec!(0x0b, 0x00, 0x00, 0x01, 0x0f));
  let protocol = BinaryProtocol;
  assert_eq!(protocol.read_list_begin(transport), (protocol::Type::TString, 0x0000010f));
}

#[test]
pub fn read_map_begin() {
  let transport = &mut FakeTransport::new(vec!(0x0b, 0x08, 0x00, 0x00, 0x01, 0x0f));
  let protocol = BinaryProtocol;
  assert_eq!(protocol.read_map_begin(transport), (protocol::Type::TString, protocol::Type::TI32, 0x0000010f));
}

#[test]
pub fn read_field_begin() {
  let transport = &mut FakeTransport::new(vec!(0x00, 0x0d, 0x14, 0x0e));
  let protocol = BinaryProtocol;
  assert_eq!(protocol.read_field_begin(transport), (String::from_str(""), protocol::Type::TStop, 0));
  assert_eq!(protocol.read_field_begin(transport), (String::from_str(""), protocol::Type::TMap, 0x140e));
}

#[test]
pub fn read_message_begin() {
  let transport = &mut FakeTransport::new(vec!(
    0x80, 0x01, 0x00, 0x01,
    0x00, 0x00, 0x00, 0x03, 0x66, 0x6f, 0x6f,
    0x00, 0x02, 0x47, 0x1e
  ));
  let protocol = BinaryProtocol;
  assert_eq!(protocol.read_message_begin(transport), (String::from_str("foo"), protocol::MessageType::MtCall, 0x0002471e));
}
