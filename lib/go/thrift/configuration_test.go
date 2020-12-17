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

package thrift

import (
	"crypto/tls"
	"testing"
	"time"
)

func TestTConfiguration(t *testing.T) {
	invalidProtoID := THeaderProtocolID(-1)
	if invalidProtoID.Validate() == nil {
		t.Fatalf("Expected %v to be an invalid THeaderProtocolID, it passes the validation", invalidProtoID)
	}

	tlsConfig := &tls.Config{
		Time: time.Now,
	}

	for _, c := range []struct {
		label                  string
		cfg                    *TConfiguration
		expectedMessageSize    int32
		expectedFrameSize      int32
		expectedConnectTimeout time.Duration
		expectedSocketTimeout  time.Duration
		expectedTLSConfig      *tls.Config
		expectedBinaryRead     bool
		expectedBinaryWrite    bool
		expectedProtoID        THeaderProtocolID
	}{
		{
			label:                  "nil",
			cfg:                    nil,
			expectedMessageSize:    DEFAULT_MAX_MESSAGE_SIZE,
			expectedFrameSize:      DEFAULT_MAX_FRAME_SIZE,
			expectedConnectTimeout: DEFAULT_CONNECT_TIMEOUT,
			expectedSocketTimeout:  DEFAULT_SOCKET_TIMEOUT,
			expectedTLSConfig:      nil,
			expectedBinaryRead:     DEFAULT_TBINARY_STRICT_READ,
			expectedBinaryWrite:    DEFAULT_TBINARY_STRICT_WRITE,
			expectedProtoID:        THeaderProtocolDefault,
		},
		{
			label:                  "empty",
			cfg:                    &TConfiguration{},
			expectedMessageSize:    DEFAULT_MAX_MESSAGE_SIZE,
			expectedFrameSize:      DEFAULT_MAX_FRAME_SIZE,
			expectedConnectTimeout: DEFAULT_CONNECT_TIMEOUT,
			expectedSocketTimeout:  DEFAULT_SOCKET_TIMEOUT,
			expectedTLSConfig:      nil,
			expectedBinaryRead:     DEFAULT_TBINARY_STRICT_READ,
			expectedBinaryWrite:    DEFAULT_TBINARY_STRICT_WRITE,
			expectedProtoID:        THeaderProtocolDefault,
		},
		{
			label: "normal",
			cfg: &TConfiguration{
				MaxMessageSize:     1024,
				MaxFrameSize:       1024,
				ConnectTimeout:     time.Millisecond,
				SocketTimeout:      time.Millisecond * 2,
				TLSConfig:          tlsConfig,
				TBinaryStrictRead:  BoolPtr(true),
				TBinaryStrictWrite: BoolPtr(false),
				THeaderProtocolID:  THeaderProtocolIDPtrMust(THeaderProtocolCompact),
			},
			expectedMessageSize:    1024,
			expectedFrameSize:      1024,
			expectedConnectTimeout: time.Millisecond,
			expectedSocketTimeout:  time.Millisecond * 2,
			expectedTLSConfig:      tlsConfig,
			expectedBinaryRead:     true,
			expectedBinaryWrite:    false,
			expectedProtoID:        THeaderProtocolCompact,
		},
		{
			label: "message<frame",
			cfg: &TConfiguration{
				MaxMessageSize: 1024,
				MaxFrameSize:   4096,
			},
			expectedMessageSize:    1024,
			expectedFrameSize:      1024,
			expectedConnectTimeout: DEFAULT_CONNECT_TIMEOUT,
			expectedSocketTimeout:  DEFAULT_SOCKET_TIMEOUT,
			expectedTLSConfig:      nil,
			expectedBinaryRead:     DEFAULT_TBINARY_STRICT_READ,
			expectedBinaryWrite:    DEFAULT_TBINARY_STRICT_WRITE,
			expectedProtoID:        THeaderProtocolDefault,
		},
		{
			label: "frame<message",
			cfg: &TConfiguration{
				MaxMessageSize: 4096,
				MaxFrameSize:   1024,
			},
			expectedMessageSize:    4096,
			expectedFrameSize:      1024,
			expectedConnectTimeout: DEFAULT_CONNECT_TIMEOUT,
			expectedSocketTimeout:  DEFAULT_SOCKET_TIMEOUT,
			expectedTLSConfig:      nil,
			expectedBinaryRead:     DEFAULT_TBINARY_STRICT_READ,
			expectedBinaryWrite:    DEFAULT_TBINARY_STRICT_WRITE,
			expectedProtoID:        THeaderProtocolDefault,
		},
		{
			label: "negative-message-size",
			cfg: &TConfiguration{
				MaxMessageSize: -1,
			},
			expectedMessageSize:    DEFAULT_MAX_MESSAGE_SIZE,
			expectedFrameSize:      DEFAULT_MAX_FRAME_SIZE,
			expectedConnectTimeout: DEFAULT_CONNECT_TIMEOUT,
			expectedSocketTimeout:  DEFAULT_SOCKET_TIMEOUT,
			expectedTLSConfig:      nil,
			expectedBinaryRead:     DEFAULT_TBINARY_STRICT_READ,
			expectedBinaryWrite:    DEFAULT_TBINARY_STRICT_WRITE,
			expectedProtoID:        THeaderProtocolDefault,
		},
		{
			label: "negative-frame-size",
			cfg: &TConfiguration{
				MaxFrameSize: -1,
			},
			expectedMessageSize:    DEFAULT_MAX_MESSAGE_SIZE,
			expectedFrameSize:      DEFAULT_MAX_FRAME_SIZE,
			expectedConnectTimeout: DEFAULT_CONNECT_TIMEOUT,
			expectedSocketTimeout:  DEFAULT_SOCKET_TIMEOUT,
			expectedTLSConfig:      nil,
			expectedBinaryRead:     DEFAULT_TBINARY_STRICT_READ,
			expectedBinaryWrite:    DEFAULT_TBINARY_STRICT_WRITE,
			expectedProtoID:        THeaderProtocolDefault,
		},
		{
			label: "negative-connect-timeout",
			cfg: &TConfiguration{
				ConnectTimeout: -1,
				SocketTimeout:  time.Millisecond,
			},
			expectedMessageSize:    DEFAULT_MAX_MESSAGE_SIZE,
			expectedFrameSize:      DEFAULT_MAX_FRAME_SIZE,
			expectedConnectTimeout: DEFAULT_CONNECT_TIMEOUT,
			expectedSocketTimeout:  time.Millisecond,
			expectedTLSConfig:      nil,
			expectedBinaryRead:     DEFAULT_TBINARY_STRICT_READ,
			expectedBinaryWrite:    DEFAULT_TBINARY_STRICT_WRITE,
			expectedProtoID:        THeaderProtocolDefault,
		},
		{
			label: "negative-socket-timeout",
			cfg: &TConfiguration{
				SocketTimeout: -1,
			},
			expectedMessageSize:    DEFAULT_MAX_MESSAGE_SIZE,
			expectedFrameSize:      DEFAULT_MAX_FRAME_SIZE,
			expectedConnectTimeout: DEFAULT_CONNECT_TIMEOUT,
			expectedSocketTimeout:  DEFAULT_SOCKET_TIMEOUT,
			expectedTLSConfig:      nil,
			expectedBinaryRead:     DEFAULT_TBINARY_STRICT_READ,
			expectedBinaryWrite:    DEFAULT_TBINARY_STRICT_WRITE,
			expectedProtoID:        THeaderProtocolDefault,
		},
		{
			label: "invalid-proto-id",
			cfg: &TConfiguration{
				THeaderProtocolID: &invalidProtoID,
			},
			expectedMessageSize:    DEFAULT_MAX_MESSAGE_SIZE,
			expectedFrameSize:      DEFAULT_MAX_FRAME_SIZE,
			expectedConnectTimeout: DEFAULT_CONNECT_TIMEOUT,
			expectedSocketTimeout:  DEFAULT_SOCKET_TIMEOUT,
			expectedTLSConfig:      nil,
			expectedBinaryRead:     DEFAULT_TBINARY_STRICT_READ,
			expectedBinaryWrite:    DEFAULT_TBINARY_STRICT_WRITE,
			expectedProtoID:        THeaderProtocolDefault,
		},
	} {
		t.Run(c.label, func(t *testing.T) {
			t.Run("GetMaxMessageSize", func(t *testing.T) {
				actual := c.cfg.GetMaxMessageSize()
				if actual != c.expectedMessageSize {
					t.Errorf(
						"Expected %v, got %v",
						c.expectedMessageSize,
						actual,
					)
				}
			})
			t.Run("GetMaxFrameSize", func(t *testing.T) {
				actual := c.cfg.GetMaxFrameSize()
				if actual != c.expectedFrameSize {
					t.Errorf(
						"Expected %v, got %v",
						c.expectedFrameSize,
						actual,
					)
				}
			})
			t.Run("GetConnectTimeout", func(t *testing.T) {
				actual := c.cfg.GetConnectTimeout()
				if actual != c.expectedConnectTimeout {
					t.Errorf(
						"Expected %v, got %v",
						c.expectedConnectTimeout,
						actual,
					)
				}
			})
			t.Run("GetSocketTimeout", func(t *testing.T) {
				actual := c.cfg.GetSocketTimeout()
				if actual != c.expectedSocketTimeout {
					t.Errorf(
						"Expected %v, got %v",
						c.expectedSocketTimeout,
						actual,
					)
				}
			})
			t.Run("GetTLSConfig", func(t *testing.T) {
				actual := c.cfg.GetTLSConfig()
				if actual != c.expectedTLSConfig {
					t.Errorf(
						"Expected %p(%#v), got %p(%#v)",
						c.expectedTLSConfig,
						c.expectedTLSConfig,
						actual,
						actual,
					)
				}
			})
			t.Run("GetTBinaryStrictRead", func(t *testing.T) {
				actual := c.cfg.GetTBinaryStrictRead()
				if actual != c.expectedBinaryRead {
					t.Errorf(
						"Expected %v, got %v",
						c.expectedBinaryRead,
						actual,
					)
				}
			})
			t.Run("GetTBinaryStrictWrite", func(t *testing.T) {
				actual := c.cfg.GetTBinaryStrictWrite()
				if actual != c.expectedBinaryWrite {
					t.Errorf(
						"Expected %v, got %v",
						c.expectedBinaryWrite,
						actual,
					)
				}
			})
			t.Run("GetTHeaderProtocolID", func(t *testing.T) {
				actual := c.cfg.GetTHeaderProtocolID()
				if actual != c.expectedProtoID {
					t.Errorf(
						"Expected %v, got %v",
						c.expectedProtoID,
						actual,
					)
				}
			})
		})
	}
}

func TestTHeaderProtocolIDPtr(t *testing.T) {
	var invalidProtoID = THeaderProtocolID(-1)
	if invalidProtoID.Validate() == nil {
		t.Fatalf("Expected %v to be an invalid THeaderProtocolID, it passes the validation", invalidProtoID)
	}

	ptr, err := THeaderProtocolIDPtr(invalidProtoID)
	if err == nil {
		t.Error("Expected error on invalid proto id, got nil")
	}
	if ptr == nil {
		t.Fatal("Expected non-nil pointer on invalid proto id, got nil")
	}
	if *ptr != THeaderProtocolDefault {
		t.Errorf("Expected pointer to %v, got %v", THeaderProtocolDefault, *ptr)
	}
}

func TestTHeaderProtocolIDPtrMust(t *testing.T) {
	const expected = THeaderProtocolCompact
	ptr := THeaderProtocolIDPtrMust(expected)
	if *ptr != expected {
		t.Errorf("Expected pointer to %v, got %v", expected, *ptr)
	}
}

func TestTHeaderProtocolIDPtrMustPanic(t *testing.T) {
	var invalidProtoID = THeaderProtocolID(-1)
	if invalidProtoID.Validate() == nil {
		t.Fatalf("Expected %v to be an invalid THeaderProtocolID, it passes the validation", invalidProtoID)
	}

	defer func() {
		if recovered := recover(); recovered == nil {
			t.Error("Expected panic on invalid proto id, did not happen.")
		}
	}()

	THeaderProtocolIDPtrMust(invalidProtoID)
}

func TestPropagateTConfiguration(t *testing.T) {
	cfg := &TConfiguration{}
	// Just make sure it won't cause panics on some nil
	// TProtocol/TTransport/TProtocolFactory/TTransportFactory values.
	PropagateTConfiguration(nil, cfg)
	var proto TProtocol
	PropagateTConfiguration(proto, cfg)
	var protoFactory TProtocolFactory
	PropagateTConfiguration(protoFactory, cfg)
	var trans TTransport
	PropagateTConfiguration(trans, cfg)
	var transFactory TTransportFactory
	PropagateTConfiguration(transFactory, cfg)
}
