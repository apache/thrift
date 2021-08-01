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

use std::convert::{From, TryFrom};
use std::io;

use async_trait::async_trait;
use futures::{AsyncRead, AsyncReadExt, AsyncSeek};
use integer_encoding::VarIntAsyncReader;

use super::compact::{
    collection_u8_to_type, u8_to_type, COMPACT_PROTOCOL_ID, COMPACT_VERSION, COMPACT_VERSION_MASK,
};
use super::{
    TFieldIdentifier, TInputStreamProtocol, TListIdentifier, TMapIdentifier, TMessageIdentifier,
    TMessageType,
};
use super::{TSetIdentifier, TStructIdentifier, TType};

#[derive(Debug)]
pub struct TCompactInputStreamProtocol<T: Send> {
    // Identifier of the last field deserialized for a struct.
    last_read_field_id: i16,
    // Stack of the last read field ids (a new entry is added each time a nested struct is read).
    read_field_id_stack: Vec<i16>,
    // Boolean value for a field.
    // Saved because boolean fields and their value are encoded in a single byte,
    // and reading the field only occurs after the field id is read.
    pending_read_bool_value: Option<bool>,
    // Underlying transport used for byte-level operations.
    transport: T,
}

impl<T: VarIntAsyncReader + AsyncRead + Unpin + Send> TCompactInputStreamProtocol<T> {
    /// Create a `TCompactInputProtocol` that reads bytes from `transport`.
    pub fn new(transport: T) -> Self {
        Self {
            last_read_field_id: 0,
            read_field_id_stack: Vec::new(),
            pending_read_bool_value: None,
            transport,
        }
    }

    async fn read_list_set_begin(&mut self) -> crate::Result<(TType, i32)> {
        let header = self.read_byte().await?;
        let element_type = collection_u8_to_type(header & 0x0F)?;

        let element_count;
        let possible_element_count = (header & 0xF0) >> 4;
        if possible_element_count != 15 {
            // high bits set high if count and type encoded separately
            element_count = possible_element_count as i32;
        } else {
            element_count = self.transport.read_varint_async::<u32>().await? as i32;
        }

        Ok((element_type, element_count))
    }
}

#[async_trait]
impl<T: VarIntAsyncReader + AsyncRead + Unpin + Send> TInputStreamProtocol
    for TCompactInputStreamProtocol<T>
{
    async fn read_message_begin(&mut self) -> crate::Result<TMessageIdentifier> {
        let compact_id = self.read_byte().await?;
        if compact_id != COMPACT_PROTOCOL_ID {
            Err(crate::Error::Protocol(crate::ProtocolError {
                kind: crate::ProtocolErrorKind::BadVersion,
                message: format!("invalid compact protocol header {:?}", compact_id),
            }))
        } else {
            Ok(())
        }?;

        let type_and_byte = self.read_byte().await?;
        let received_version = type_and_byte & COMPACT_VERSION_MASK;
        if received_version != COMPACT_VERSION {
            Err(crate::Error::Protocol(crate::ProtocolError {
                kind: crate::ProtocolErrorKind::BadVersion,
                message: format!(
                    "cannot process compact protocol version {:?}",
                    received_version
                ),
            }))
        } else {
            Ok(())
        }?;

        // NOTE: unsigned right shift will pad with 0s
        let message_type: TMessageType = TMessageType::try_from(type_and_byte >> 5)?;
        // writing side wrote signed sequence number as u32 to avoid zigzag encoding
        let sequence_number = self.transport.read_varint_async::<u32>().await? as i32;
        let service_call_name = self.read_string().await?;

        self.last_read_field_id = 0;

        Ok(TMessageIdentifier::new(
            service_call_name,
            message_type,
            sequence_number,
        ))
    }

    async fn read_message_end(&mut self) -> crate::Result<()> {
        Ok(())
    }

    async fn read_struct_begin(&mut self) -> crate::Result<Option<TStructIdentifier>> {
        self.read_field_id_stack.push(self.last_read_field_id);
        self.last_read_field_id = 0;
        Ok(None)
    }

    async fn read_struct_end(&mut self) -> crate::Result<()> {
        self.last_read_field_id = self
            .read_field_id_stack
            .pop()
            .expect("should have previous field ids");
        Ok(())
    }

    async fn read_field_begin(&mut self) -> crate::Result<TFieldIdentifier> {
        // we can read at least one byte, which is:
        // - the type
        // - the field delta and the type
        let field_type = self.read_byte().await?;
        let field_delta = (field_type & 0xF0) >> 4;
        let field_type = match field_type & 0x0F {
            0x01 => {
                self.pending_read_bool_value = Some(true);
                Ok(TType::Bool)
            }
            0x02 => {
                self.pending_read_bool_value = Some(false);
                Ok(TType::Bool)
            }
            ttu8 => u8_to_type(ttu8),
        }?;

        match field_type {
            TType::Stop => Ok(
                TFieldIdentifier::new::<Option<String>, String, Option<i16>>(
                    None,
                    TType::Stop,
                    None,
                ),
            ),
            _ => {
                if field_delta != 0 {
                    self.last_read_field_id += field_delta as i16;
                } else {
                    self.last_read_field_id = self.read_i16().await?;
                };

                Ok(TFieldIdentifier {
                    name: None,
                    field_type,
                    id: Some(self.last_read_field_id),
                })
            }
        }
    }

    async fn read_field_end(&mut self) -> crate::Result<()> {
        Ok(())
    }

    async fn read_bool(&mut self) -> crate::Result<bool> {
        match self.pending_read_bool_value.take() {
            Some(b) => Ok(b),
            None => {
                let b = self.read_byte().await?;
                match b {
                    0x01 => Ok(true),
                    0x02 => Ok(false),
                    unkn => Err(crate::Error::Protocol(crate::ProtocolError {
                        kind: crate::ProtocolErrorKind::InvalidData,
                        message: format!("cannot convert {} into bool", unkn),
                    })),
                }
            }
        }
    }

    async fn read_bytes(&mut self) -> crate::Result<Vec<u8>> {
        let len = self.transport.read_varint_async::<u32>().await?;
        let mut buf = vec![0u8; len as usize];
        self.transport
            .read_exact(&mut buf)
            .await
            .map_err(From::from)
            .map(|_| buf)
    }

    async fn read_i8(&mut self) -> crate::Result<i8> {
        self.read_byte().await.map(|i| i as i8)
    }

    async fn read_i16(&mut self) -> crate::Result<i16> {
        self.transport
            .read_varint_async::<i16>()
            .await
            .map_err(From::from)
    }

    async fn read_i32(&mut self) -> crate::Result<i32> {
        self.transport
            .read_varint_async::<i32>()
            .await
            .map_err(From::from)
    }

    async fn read_i64(&mut self) -> crate::Result<i64> {
        self.transport
            .read_varint_async::<i64>()
            .await
            .map_err(From::from)
    }

    async fn read_double(&mut self) -> crate::Result<f64> {
        let mut buf = [0; 8];
        self.transport.read_exact(&mut buf).await?;
        let r = f64::from_le_bytes(buf);
        Ok(r)
    }

    async fn read_string(&mut self) -> crate::Result<String> {
        let bytes = self.read_bytes().await?;
        String::from_utf8(bytes).map_err(From::from)
    }

    async fn read_list_begin(&mut self) -> crate::Result<TListIdentifier> {
        let (element_type, element_count) = self.read_list_set_begin().await?;
        Ok(TListIdentifier::new(element_type, element_count))
    }

    async fn read_list_end(&mut self) -> crate::Result<()> {
        Ok(())
    }

    async fn read_set_begin(&mut self) -> crate::Result<TSetIdentifier> {
        let (element_type, element_count) = self.read_list_set_begin().await?;
        Ok(TSetIdentifier::new(element_type, element_count))
    }

    async fn read_set_end(&mut self) -> crate::Result<()> {
        Ok(())
    }

    async fn read_map_begin(&mut self) -> crate::Result<TMapIdentifier> {
        let element_count = self.transport.read_varint_async::<u32>().await? as i32;
        if element_count == 0 {
            Ok(TMapIdentifier::new(None, None, 0))
        } else {
            let type_header = self.read_byte().await?;
            let key_type = collection_u8_to_type((type_header & 0xF0) >> 4)?;
            let val_type = collection_u8_to_type(type_header & 0x0F)?;
            Ok(TMapIdentifier::new(key_type, val_type, element_count))
        }
    }

    async fn read_map_end(&mut self) -> crate::Result<()> {
        Ok(())
    }

    // utility
    //

    async fn read_byte(&mut self) -> crate::Result<u8> {
        let mut buf = [0u8; 1];
        self.transport
            .read_exact(&mut buf)
            .await
            .map_err(From::from)
            .map(|_| buf[0])
    }
}

impl<T> AsyncSeek for TCompactInputStreamProtocol<T>
where
    T: AsyncSeek + Unpin + Send,
{
    fn poll_seek(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        pos: io::SeekFrom,
    ) -> std::task::Poll<io::Result<u64>> {
        std::pin::Pin::new(&mut self.transport).poll_seek(cx, pos)
    }
}
