use async_trait::async_trait;

use crate::{ProtocolError, ProtocolErrorKind};

use super::*;

#[async_trait]
pub trait TInputStreamProtocol: Send + Sync {
    /// Read the beginning of a Thrift message.
    async fn read_message_begin(&mut self) -> crate::Result<TMessageIdentifier>;
    /// Read the end of a Thrift message.
    async fn read_message_end(&mut self) -> crate::Result<()>;
    /// Read the beginning of a Thrift struct.
    async fn read_struct_begin(&mut self) -> crate::Result<Option<TStructIdentifier>>;
    /// Read the end of a Thrift struct.
    async fn read_struct_end(&mut self) -> crate::Result<()>;
    /// Read the beginning of a Thrift struct field.
    async fn read_field_begin(&mut self) -> crate::Result<TFieldIdentifier>;
    /// Read the end of a Thrift struct field.
    async fn read_field_end(&mut self) -> crate::Result<()>;
    /// Read a bool.
    async fn read_bool(&mut self) -> crate::Result<bool>;
    /// Read a fixed-length byte array.
    async fn read_bytes(&mut self) -> crate::Result<Vec<u8>>;
    /// Read a word.
    async fn read_i8(&mut self) -> crate::Result<i8>;
    /// Read a 16-bit signed integer.
    async fn read_i16(&mut self) -> crate::Result<i16>;
    /// Read a 32-bit signed integer.
    async fn read_i32(&mut self) -> crate::Result<i32>;
    /// Read a 64-bit signed integer.
    async fn read_i64(&mut self) -> crate::Result<i64>;
    /// Read a 64-bit float.
    async fn read_double(&mut self) -> crate::Result<f64>;
    /// Read a fixed-length string (not null terminated).
    async fn read_string(&mut self) -> crate::Result<String>;
    /// Read the beginning of a list.
    async fn read_list_begin(&mut self) -> crate::Result<TListIdentifier>;
    /// Read the end of a list.
    async fn read_list_end(&mut self) -> crate::Result<()>;
    /// Read the beginning of a set.
    async fn read_set_begin(&mut self) -> crate::Result<TSetIdentifier>;
    /// Read the end of a set.
    async fn read_set_end(&mut self) -> crate::Result<()>;
    /// Read the beginning of a map.
    async fn read_map_begin(&mut self) -> crate::Result<TMapIdentifier>;
    /// Read the end of a map.
    async fn read_map_end(&mut self) -> crate::Result<()>;

    /// Skip a field with type `field_type` recursively until the default
    /// maximum skip depth is reached.
    async fn skip(&mut self, field_type: TType) -> crate::Result<()> {
        self.skip_till_depth(field_type, MAXIMUM_SKIP_DEPTH).await
    }

    /// Skip a field with type `field_type` recursively up to `depth` levels.
    async fn skip_till_depth(&mut self, field_type: TType, depth: i8) -> crate::Result<()> {
        if depth == 0 {
            return Err(crate::Error::Protocol(ProtocolError {
                kind: ProtocolErrorKind::DepthLimit,
                message: format!("cannot parse past {:?}", field_type),
            }));
        }

        match field_type {
            TType::Bool => self.read_bool().await.map(|_| ()),
            TType::I08 => self.read_i8().await.map(|_| ()),
            TType::I16 => self.read_i16().await.map(|_| ()),
            TType::I32 => self.read_i32().await.map(|_| ()),
            TType::I64 => self.read_i64().await.map(|_| ()),
            TType::Double => self.read_double().await.map(|_| ()),
            TType::String => self.read_string().await.map(|_| ()),
            TType::Struct => {
                self.read_struct_begin().await?;
                loop {
                    let field_ident = self.read_field_begin().await?;
                    if field_ident.field_type == TType::Stop {
                        break;
                    }
                    self.skip_till_depth(field_ident.field_type, depth - 1)
                        .await?;
                }
                self.read_struct_end().await
            }
            TType::List => {
                let list_ident = self.read_list_begin().await?;
                for _ in 0..list_ident.size {
                    self.skip_till_depth(list_ident.element_type, depth - 1)
                        .await?;
                }
                self.read_list_end().await
            }
            TType::Set => {
                let set_ident = self.read_set_begin().await?;
                for _ in 0..set_ident.size {
                    self.skip_till_depth(set_ident.element_type, depth - 1)
                        .await?;
                }
                self.read_set_end().await
            }
            TType::Map => {
                let map_ident = self.read_map_begin().await?;
                for _ in 0..map_ident.size {
                    let key_type = map_ident
                        .key_type
                        .expect("non-zero sized map should contain key type");
                    let val_type = map_ident
                        .value_type
                        .expect("non-zero sized map should contain value type");
                    self.skip_till_depth(key_type, depth - 1).await?;
                    self.skip_till_depth(val_type, depth - 1).await?;
                }
                self.read_map_end().await
            }
            u => Err(crate::Error::Protocol(ProtocolError {
                kind: ProtocolErrorKind::Unknown,
                message: format!("cannot skip field type {:?}", &u),
            })),
        }
    }

    // utility (DO NOT USE IN GENERATED CODE!!!!)
    //

    /// Read an unsigned byte.
    ///
    /// This method should **never** be used in generated code.
    async fn read_byte(&mut self) -> crate::Result<u8>;
}
