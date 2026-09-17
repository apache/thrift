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
	"context"
	"encoding/binary"
	"errors"
	"fmt"
	"math"
	"testing"
)

var containerProtocols = []struct {
	name string
	new  func(TTransport, *TConfiguration) TProtocol
}{
	{"binary", func(t TTransport, c *TConfiguration) TProtocol { return NewTBinaryProtocolConf(t, c) }},
	{"compact", func(t TTransport, c *TConfiguration) TProtocol { return NewTCompactProtocolConf(t, c) }},
	{"json", func(t TTransport, c *TConfiguration) TProtocol {
		p := NewTJSONProtocol(t)
		p.SetTConfiguration(c)
		return p
	}},
	{"simplejson", func(t TTransport, c *TConfiguration) TProtocol { return NewTSimpleJSONProtocolConf(t, c) }},
	{"header", func(t TTransport, c *TConfiguration) TProtocol { return NewTHeaderProtocolConf(t, c) }},
}

// writeContainer writes a list, set or map of count byte elements, all of
// them, so that the bytes behind the count do not limit it. With message set,
// the container is wrapped in a message.
func writeContainer(t *testing.T, p TProtocol, kind string, count int, message bool) {
	t.Helper()
	ctx := context.Background()
	var err error
	if message {
		err = p.WriteMessageBegin(ctx, "m", CALL, 1)
	}
	if err == nil {
		switch kind {
		case "list":
			err = p.WriteListBegin(ctx, BYTE, count)
		case "set":
			err = p.WriteSetBegin(ctx, BYTE, count)
		case "map":
			err = p.WriteMapBegin(ctx, BYTE, BYTE, count)
		}
	}
	for i := 0; err == nil && i < count; i++ {
		err = p.WriteByte(ctx, 1)
		if err == nil && kind == "map" {
			err = p.WriteByte(ctx, 2)
		}
	}
	if err == nil {
		switch kind {
		case "list":
			err = p.WriteListEnd(ctx)
		case "set":
			err = p.WriteSetEnd(ctx)
		case "map":
			err = p.WriteMapEnd(ctx)
		}
	}
	if err == nil && message {
		err = p.WriteMessageEnd(ctx)
	}
	if err == nil {
		err = p.Flush(ctx)
	}
	if err != nil {
		t.Fatal(err)
	}
}

func readContainerBegin(p TProtocol, kind string, message bool) (size int, err error) {
	ctx := context.Background()
	if message {
		if _, _, _, err = p.ReadMessageBegin(ctx); err != nil {
			return 0, err
		}
	}
	switch kind {
	case "list":
		_, size, err = p.ReadListBegin(ctx)
	case "set":
		_, size, err = p.ReadSetBegin(ctx)
	default:
		_, _, size, err = p.ReadMapBegin(ctx)
	}
	return size, err
}

// MaxContainerSize bounds the element count of each list, set and map a
// protocol reads, however many bytes follow the count. THeaderProtocol builds
// a new inner protocol for each message it reads, so the containers are read
// both on their own and inside a message.
func TestMaxContainerSize(t *testing.T) {
	const limit = 10
	cfg := &TConfiguration{MaxContainerSize: limit}
	for _, p := range containerProtocols {
		for _, message := range []bool{false, true} {
			for _, kind := range []string{"list", "set", "map"} {
				for _, count := range []int{limit, limit + 1} {
					t.Run(fmt.Sprintf("%s/message=%v/%s/%d", p.name, message, kind, count), func(t *testing.T) {
						buf := NewTMemoryBuffer()
						writeContainer(t, p.new(buf, nil), kind, count, message)
						size, err := readContainerBegin(p.new(buf, cfg), kind, message)
						if count <= limit {
							if err != nil || size != count {
								t.Fatalf("count %d at the limit: got size %d, error %v", count, size, err)
							}
							return
						}
						var te TProtocolException
						if !errors.As(err, &te) || te.TypeId() != SIZE_LIMIT {
							t.Fatalf("count %d over the limit of %d: got size %d, error %v", count, limit, size, err)
						}
					})
				}
			}
		}
	}
}

// Without MaxContainerSize a count is not limited on its own.
func TestMaxContainerSizeUnset(t *testing.T) {
	const count = 1000
	for _, c := range []struct {
		name string
		cfg  *TConfiguration
	}{
		{"nil", nil},
		{"zero", &TConfiguration{}},
		{"negative", &TConfiguration{MaxContainerSize: -1}},
	} {
		for _, p := range containerProtocols {
			t.Run(p.name+"/"+c.name, func(t *testing.T) {
				buf := NewTMemoryBuffer()
				writeContainer(t, p.new(buf, nil), "list", count, true)
				size, err := readContainerBegin(p.new(buf, c.cfg), "list", true)
				if err != nil || size != count {
					t.Fatalf("got size %d, error %v", size, err)
				}
			})
		}
	}
}

func TestGetMaxContainerSize(t *testing.T) {
	for _, c := range []struct {
		cfg  *TConfiguration
		want int32
	}{
		{nil, math.MaxInt32},
		{&TConfiguration{}, math.MaxInt32},
		{&TConfiguration{MaxContainerSize: -5}, math.MaxInt32},
		{&TConfiguration{MaxContainerSize: 7}, 7},
	} {
		if got := c.cfg.GetMaxContainerSize(); got != c.want {
			t.Errorf("%v: GetMaxContainerSize() = %d, want %d", c.cfg, got, c.want)
		}
	}
}

// THeaderTransport holds the transform count of a frame header to
// MaxContainerSize as well.
func TestTHeaderTransportTransformCountMaxContainerSize(t *testing.T) {
	headers := NewTMemoryBuffer()
	hp := NewTCompactProtocol(headers)
	// protocol id, transform count, the transform ids
	for _, v := range []int32{int32(THeaderProtocolCompact), 2, int32(TransformNone), int32(TransformNone)} {
		if _, err := hp.writeVarint32(v); err != nil {
			t.Fatal(err)
		}
	}
	if padding := 4 - headers.Len()%4; padding < 4 {
		headers.Write(make([]byte, padding))
	}
	frame := NewTMemoryBuffer()
	meta := headerMeta{MagicFlags: THeaderHeaderMagic, HeaderLength: uint16(headers.Len() / 4)}
	if err := binary.Write(frame, binary.BigEndian, meta); err != nil {
		t.Fatal(err)
	}
	frame.Write(headers.Bytes())

	for _, limit := range []int32{2, 1} {
		trans := NewTMemoryBuffer()
		binary.Write(trans, binary.BigEndian, uint32(frame.Len()))
		trans.Write(frame.Bytes())
		err := NewTHeaderTransportConf(trans, &TConfiguration{MaxContainerSize: limit}).ReadFrame(context.Background())
		if limit == 2 {
			if err != nil {
				t.Errorf("2 transforms, limit 2: %v", err)
			}
			continue
		}
		var te TProtocolException
		if !errors.As(err, &te) || te.TypeId() != SIZE_LIMIT {
			t.Errorf("2 transforms, limit 1: got %v, want a SIZE_LIMIT TProtocolException", err)
		}
	}
}

func TestCheckContainerSizeForProtocolLimit(t *testing.T) {
	cfg := &TConfiguration{MaxContainerSize: 5}
	if err := checkContainerSizeForProtocol(5, 1, UnknownRemainingBytes, cfg); err != nil {
		t.Errorf("count at the limit: %v", err)
	}
	err := checkContainerSizeForProtocol(6, 1, UnknownRemainingBytes, cfg)
	var te TProtocolException
	if !errors.As(err, &te) || te.TypeId() != SIZE_LIMIT {
		t.Errorf("count over the limit: got %v, want a SIZE_LIMIT TProtocolException", err)
	}
}
