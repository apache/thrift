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

/// Configuration for Thrift protocols.
#[derive(Debug, Clone)]
pub struct TConfiguration {
    max_message_size: Option<usize>,
    max_frame_size: Option<usize>,
    max_recursion_depth: Option<usize>,
    max_container_size: Option<usize>,
    max_string_size: Option<usize>,
}

impl TConfiguration {
    // this value is used consistently across all Thrift libraries
    pub const DEFAULT_MAX_MESSAGE_SIZE: usize = 100 * 1024 * 1024;

    // this value is used consistently across all Thrift libraries
    pub const DEFAULT_MAX_FRAME_SIZE: usize = 16_384_000;

    pub const DEFAULT_RECURSION_LIMIT: usize = 64;

    pub const DEFAULT_CONTAINER_LIMIT: Option<usize> = None;

    pub const DEFAULT_STRING_LIMIT: usize = 100 * 1024 * 1024;

    pub fn no_limits() -> Self {
        Self {
            max_message_size: None,
            max_frame_size: None,
            max_recursion_depth: None,
            max_container_size: None,
            max_string_size: None,
        }
    }

    pub fn max_message_size(&self) -> Option<usize> {
        self.max_message_size
    }

    pub fn max_frame_size(&self) -> Option<usize> {
        self.max_frame_size
    }

    pub fn max_recursion_depth(&self) -> Option<usize> {
        self.max_recursion_depth
    }

    pub fn max_container_size(&self) -> Option<usize> {
        self.max_container_size
    }

    pub fn max_string_size(&self) -> Option<usize> {
        self.max_string_size
    }

    pub fn builder() -> TConfigurationBuilder {
        TConfigurationBuilder::default()
    }
}

impl Default for TConfiguration {
    fn default() -> Self {
        Self {
            max_message_size: Some(Self::DEFAULT_MAX_MESSAGE_SIZE),
            max_frame_size: Some(Self::DEFAULT_MAX_FRAME_SIZE),
            max_recursion_depth: Some(Self::DEFAULT_RECURSION_LIMIT),
            max_container_size: Self::DEFAULT_CONTAINER_LIMIT,
            max_string_size: Some(Self::DEFAULT_STRING_LIMIT),
        }
    }
}

#[derive(Debug, Default)]
pub struct TConfigurationBuilder {
    config: TConfiguration,
}

impl TConfigurationBuilder {
    pub fn max_message_size(mut self, limit: Option<usize>) -> Self {
        self.config.max_message_size = limit;
        self
    }

    pub fn max_frame_size(mut self, limit: Option<usize>) -> Self {
        self.config.max_frame_size = limit;
        self
    }

    pub fn max_recursion_depth(mut self, limit: Option<usize>) -> Self {
        self.config.max_recursion_depth = limit;
        self
    }

    pub fn max_container_size(mut self, limit: Option<usize>) -> Self {
        self.config.max_container_size = limit;
        self
    }

    pub fn max_string_size(mut self, limit: Option<usize>) -> Self {
        self.config.max_string_size = limit;
        self
    }

    pub fn build(self) -> crate::Result<TConfiguration> {
        if let (Some(frame_size), Some(message_size)) =
            (self.config.max_frame_size, self.config.max_message_size)
        {
            if frame_size > message_size {
                // FIXME: This should probably be a different error type.
                return Err(crate::Error::Application(crate::ApplicationError::new(
                    crate::ApplicationErrorKind::Unknown,
                    format!(
                        "Invalid configuration: max_frame_size ({}) cannot exceed max_message_size ({})",
                        frame_size, message_size
                    ),
                )));
            }
        }

        Ok(self.config)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_custom_configuration_builder() {
        let config = TConfiguration::builder()
            .max_message_size(Some(1024))
            .max_frame_size(Some(512))
            .max_recursion_depth(Some(10))
            .max_container_size(Some(100))
            .max_string_size(Some(256))
            .build()
            .unwrap();

        assert_eq!(config.max_message_size(), Some(1024));
        assert_eq!(config.max_frame_size(), Some(512));
        assert_eq!(config.max_recursion_depth(), Some(10));
        assert_eq!(config.max_container_size(), Some(100));
        assert_eq!(config.max_string_size(), Some(256));
    }

    #[test]
    fn test_invalid_configuration() {
        // Test that builder catches invalid configurations
        let result = TConfiguration::builder()
            .max_frame_size(Some(1000))
            .max_message_size(Some(500)) // frame size > message size is invalid
            .build();

        assert!(result.is_err());
        match result {
            Err(crate::Error::Application(e)) => {
                assert!(e.message.contains("max_frame_size"));
                assert!(e.message.contains("cannot exceed max_message_size"));
            }
            _ => panic!("Expected Application error"),
        }
    }
}
