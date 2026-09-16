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
	"bytes"
	"compress/zlib"
	"context"
	"testing"
)

func TestZlibTransport(t *testing.T) {
	trans, err := NewTZlibTransport(NewTMemoryBuffer(), zlib.BestCompression)
	if err != nil {
		t.Fatal(err)
	}
	TransportTest(t, trans, trans)
}

type DummyTransportFactory struct{}

func (p *DummyTransportFactory) GetTransport(trans TTransport) (TTransport, error) {
	return NewTMemoryBuffer(), nil
}

func TestZlibFactoryTransportWithFactory(t *testing.T) {
	factory := NewTZlibTransportFactoryWithFactory(
		zlib.BestCompression,
		&DummyTransportFactory{},
	)
	buffer := NewTMemoryBuffer()
	trans, err := factory.GetTransport(buffer)
	if err != nil {
		t.Fatal(err)
	}
	TransportTest(t, trans, trans)
}

func TestZlibFactoryTransportWithoutFactory(t *testing.T) {
	factory := NewTZlibTransportFactoryWithFactory(zlib.BestCompression, nil)
	buffer := NewTMemoryBuffer()
	trans, err := factory.GetTransport(buffer)
	if err != nil {
		t.Fatal(err)
	}
	TransportTest(t, trans, trans)
}

func TestZlibTransportMessageSizeLimit(t *testing.T) {
	const dataSize = 4096
	data := bytes.Repeat([]byte{'a'}, dataSize)

	// Write and flush (not close) so TMemoryBuffer.Close() doesn't wipe the data.
	writeBuf := NewTMemoryBuffer()
	writer, err := NewTZlibTransport(writeBuf, zlib.BestCompression)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := writer.Write(data); err != nil {
		t.Fatal(err)
	}
	if err := writer.Flush(context.Background()); err != nil {
		t.Fatal(err)
	}
	compressed := make([]byte, writeBuf.Len())
	copy(compressed, writeBuf.Bytes())

	readBuf := NewTMemoryBuffer()
	readBuf.Write(compressed)
	reader, err := NewTZlibTransport(readBuf, zlib.BestCompression)
	if err != nil {
		t.Fatal(err)
	}
	reader.SetTConfiguration(&TConfiguration{MaxMessageSize: 1024})

	_, err = reader.Read(make([]byte, dataSize))
	if err == nil {
		t.Fatal("expected SIZE_LIMIT error, got nil")
	}
	protoEx, ok := err.(TProtocolException)
	if !ok || protoEx.TypeId() != SIZE_LIMIT {
		t.Fatalf("expected SIZE_LIMIT TProtocolException, got %T: %v", err, err)
	}
}

// zlibCompressed returns data compressed the way TZlibTransport writes it.
func zlibCompressed(t *testing.T, data []byte) []byte {
	t.Helper()
	// Flush rather than close, so that TMemoryBuffer.Close() does not wipe the data.
	buf := NewTMemoryBuffer()
	writer, err := NewTZlibTransport(buf, zlib.BestCompression)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := writer.Write(data); err != nil {
		t.Fatal(err)
	}
	if err := writer.Flush(context.Background()); err != nil {
		t.Fatal(err)
	}
	return bytes.Clone(buf.Bytes())
}

// confRecordingTransport is a memory buffer that remembers the configuration
// it was handed.
type confRecordingTransport struct {
	*TMemoryBuffer
	conf *TConfiguration
}

func (c *confRecordingTransport) SetTConfiguration(conf *TConfiguration) {
	c.conf = conf
}

// confRecordingFactory hands out confRecordingTransports and remembers the
// configuration it was handed.
type confRecordingFactory struct {
	conf      *TConfiguration
	transport *confRecordingTransport
}

func (f *confRecordingFactory) GetTransport(TTransport) (TTransport, error) {
	f.transport = &confRecordingTransport{TMemoryBuffer: NewTMemoryBuffer()}
	return f.transport, nil
}

func (f *confRecordingFactory) SetTConfiguration(conf *TConfiguration) {
	f.conf = conf
}

func TestZlibFactoryTransportConf(t *testing.T) {
	factory := NewTZlibTransportFactoryConf(zlib.BestCompression, nil, &TConfiguration{})
	trans, err := factory.GetTransport(NewTMemoryBuffer())
	if err != nil {
		t.Fatal(err)
	}
	TransportTest(t, trans, trans)
}

// A limit configured on the factory is the one the transports it makes apply
// to what they decompress.
func TestZlibFactoryTransportConfMessageSizeLimit(t *testing.T) {
	const dataSize = 4096
	compressed := zlibCompressed(t, bytes.Repeat([]byte{'a'}, dataSize))

	factory := NewTZlibTransportFactoryConf(
		zlib.BestCompression,
		nil,
		&TConfiguration{MaxMessageSize: 1024},
	)
	buf := NewTMemoryBuffer()
	buf.Write(compressed)
	trans, err := factory.GetTransport(buf)
	if err != nil {
		t.Fatal(err)
	}

	// Read directly: io.ReadFull drops an error returned together with the
	// last bytes it asked for.
	for i := 0; err == nil && i < dataSize; i++ {
		_, err = trans.Read(make([]byte, dataSize))
	}
	protoEx, ok := err.(TProtocolException)
	if !ok || protoEx.TypeId() != SIZE_LIMIT {
		t.Fatalf("expected SIZE_LIMIT TProtocolException, got %T: %v", err, err)
	}
}

// The configuration reaches the wrapped factory, the transport handed to it,
// the transport it makes, and the transport the zlib factory makes around that.
func TestZlibFactoryTransportConfPropagation(t *testing.T) {
	conf := &TConfiguration{MaxMessageSize: 1024}
	inner := &confRecordingFactory{}
	factory := NewTZlibTransportFactoryConf(zlib.BestCompression, inner, conf)
	if inner.conf != conf {
		t.Errorf("wrapped factory has configuration %v, want %v", inner.conf, conf)
	}

	base := &confRecordingTransport{TMemoryBuffer: NewTMemoryBuffer()}
	trans, err := factory.GetTransport(base)
	if err != nil {
		t.Fatal(err)
	}
	if base.conf != conf {
		t.Errorf("transport handed to the factory has configuration %v, want %v", base.conf, conf)
	}
	if inner.transport.conf != conf {
		t.Errorf("wrapped transport has configuration %v, want %v", inner.transport.conf, conf)
	}
	if got := trans.(*TZlibTransport).conf; got != conf {
		t.Errorf("zlib transport has configuration %v, want %v", got, conf)
	}

	updated := &TConfiguration{MaxMessageSize: 2048}
	PropagateTConfiguration(factory, updated)
	if inner.conf != updated {
		t.Errorf("wrapped factory has configuration %v after the update, want %v", inner.conf, updated)
	}
	trans, err = factory.GetTransport(NewTMemoryBuffer())
	if err != nil {
		t.Fatal(err)
	}
	if got := trans.(*TZlibTransport).conf; got != updated {
		t.Errorf("zlib transport has configuration %v after the update, want %v", got, updated)
	}
}

// The factories that take no configuration leave the transports they make at
// the defaults, as before.
func TestZlibFactoryTransportWithoutConf(t *testing.T) {
	for name, factory := range map[string]*TZlibTransportFactory{
		"level":       NewTZlibTransportFactory(zlib.BestCompression),
		"withFactory": NewTZlibTransportFactoryWithFactory(zlib.BestCompression, nil),
	} {
		t.Run(name, func(t *testing.T) {
			trans, err := factory.GetTransport(NewTMemoryBuffer())
			if err != nil {
				t.Fatal(err)
			}
			if got := trans.(*TZlibTransport).conf; got != nil {
				t.Errorf("zlib transport has configuration %v, want none", got)
			}
		})
	}
}
