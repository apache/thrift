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
	"errors"
	"fmt"
	"sync"
	"sync/atomic"
	"testing"
	"testing/quick"
)

type ProtocolFactory interface {
	GetProtocol(t TTransport) TProtocol
}

func compareStructs(m, m1 MyTestStruct) error {
	switch {
	case m.On != m1.On:
		return errors.New("Boolean not equal")
	case m.B != m1.B:
		return errors.New("Byte not equal")
	case m.Int16 != m1.Int16:
		return errors.New("Int16 not equal")
	case m.Int32 != m1.Int32:
		return errors.New("Int32 not equal")
	case m.Int64 != m1.Int64:
		return errors.New("Int64 not equal")
	case m.D != m1.D:
		return errors.New("Double not equal")
	case m.St != m1.St:
		return errors.New("String not equal")

	case len(m.Bin) != len(m1.Bin):
		return errors.New("Binary size not equal")
	case len(m.Bin) == len(m1.Bin):
		for i := range m.Bin {
			if m.Bin[i] != m1.Bin[i] {
				return errors.New("Binary not equal")
			}
		}
	case len(m.StringMap) != len(m1.StringMap):
		return errors.New("StringMap size not equal")
	case len(m.StringList) != len(m1.StringList):
		return errors.New("StringList size not equal")
	case len(m.StringSet) != len(m1.StringSet):
		return errors.New("StringSet size not equal")

	case m.E != m1.E:
		return errors.New("MyTestEnum not equal")

	default:
		return nil

	}
	return nil
}

type serializer interface {
	WriteString(context.Context, TStruct) (string, error)
}

type deserializer interface {
	ReadString(context.Context, TStruct, string) error
}

func plainSerializer(pf ProtocolFactory) serializer {
	t := NewTSerializer()
	t.Protocol = pf.GetProtocol(t.Transport)
	return t
}

func poolSerializer(pf ProtocolFactory) serializer {
	return NewTSerializerPool(
		func() *TSerializer {
			return plainSerializer(pf).(*TSerializer)
		},
	)
}

func plainDeserializer(pf ProtocolFactory) deserializer {
	d := NewTDeserializer()
	d.Protocol = pf.GetProtocol(d.Transport)
	return d
}

func poolDeserializer(pf ProtocolFactory) deserializer {
	return NewTDeserializerPool(
		func() *TDeserializer {
			return plainDeserializer(pf).(*TDeserializer)
		},
	)
}

type constructors struct {
	Label        string
	Serializer   func(pf ProtocolFactory) serializer
	Deserializer func(pf ProtocolFactory) deserializer
}

var implementations = []constructors{
	{
		Label:        "plain",
		Serializer:   plainSerializer,
		Deserializer: plainDeserializer,
	},
	{
		Label:        "pool",
		Serializer:   poolSerializer,
		Deserializer: poolDeserializer,
	},
}

func ProtocolTest1(t *testing.T, pf ProtocolFactory) {
	for _, impl := range implementations {
		t.Run(
			impl.Label,
			func(test *testing.T) {
				t := impl.Serializer(pf)
				var m = MyTestStruct{}
				m.On = true
				m.B = int8(0)
				m.Int16 = 1
				m.Int32 = 2
				m.Int64 = 3
				m.D = 4.1
				m.St = "Test"
				m.Bin = make([]byte, 10)
				m.StringMap = make(map[string]string, 5)
				m.StringList = make([]string, 5)
				m.StringSet = make(map[string]struct{}, 5)
				m.E = 2

				s, err := t.WriteString(context.Background(), &m)
				if err != nil {
					test.Fatalf("Unable to Serialize struct: %v", err)

				}

				t1 := impl.Deserializer(pf)
				var m1 MyTestStruct
				if err = t1.ReadString(context.Background(), &m1, s); err != nil {
					test.Fatalf("Unable to Deserialize struct: %v", err)

				}

				if err := compareStructs(m, m1); err != nil {
					test.Error(err)
				}
			},
		)
	}
}

func ProtocolTest2(t *testing.T, pf ProtocolFactory) {
	for _, impl := range implementations {
		t.Run(
			impl.Label,
			func(test *testing.T) {
				t := impl.Serializer(pf)
				var m = MyTestStruct{}
				m.On = false
				m.B = int8(0)
				m.Int16 = 1
				m.Int32 = 2
				m.Int64 = 3
				m.D = 4.1
				m.St = "Test"
				m.Bin = make([]byte, 10)
				m.StringMap = make(map[string]string, 5)
				m.StringList = make([]string, 5)
				m.StringSet = make(map[string]struct{}, 5)
				m.E = 2

				s, err := t.WriteString(context.Background(), &m)
				if err != nil {
					test.Fatalf("Unable to Serialize struct: %v", err)

				}

				t1 := impl.Deserializer(pf)
				var m1 MyTestStruct
				if err = t1.ReadString(context.Background(), &m1, s); err != nil {
					test.Fatalf("Unable to Deserialize struct: %v", err)

				}

				if err := compareStructs(m, m1); err != nil {
					test.Error(err)
				}
			},
		)
	}
}

func TestSerializer(t *testing.T) {

	var protocol_factories map[string]ProtocolFactory
	protocol_factories = make(map[string]ProtocolFactory)
	protocol_factories["Binary"] = NewTBinaryProtocolFactoryDefault()
	protocol_factories["Compact"] = NewTCompactProtocolFactory()
	//protocol_factories["SimpleJSON"] = NewTSimpleJSONProtocolFactory() - write only, can't be read back by design
	protocol_factories["JSON"] = NewTJSONProtocolFactory()

	tests := make(map[string]func(*testing.T, ProtocolFactory))
	tests["Test 1"] = ProtocolTest1
	tests["Test 2"] = ProtocolTest2
	//tests["Test 3"] = ProtocolTest3 // Example of how to add additional tests

	for name, pf := range protocol_factories {
		t.Run(
			name,
			func(t *testing.T) {
				for label, f := range tests {
					t.Run(
						label,
						func(t *testing.T) {
							f(t, pf)
						},
					)
				}
			},
		)
	}

}

func TestSerializerPoolAsync(t *testing.T) {
	var wg sync.WaitGroup
	var counter int64
	s := NewTSerializerPool(NewTSerializer)
	d := NewTDeserializerPool(NewTDeserializer)
	f := func(i int64) bool {
		wg.Add(1)
		go func() {
			defer wg.Done()
			t.Run(
				fmt.Sprintf("#%d-%d", atomic.AddInt64(&counter, 1), i),
				func(t *testing.T) {
					m := MyTestStruct{
						Int64: i,
					}
					str, err := s.WriteString(context.Background(), &m)
					if err != nil {
						t.Fatal("serialize:", err)
					}
					var m1 MyTestStruct
					if err = d.ReadString(context.Background(), &m1, str); err != nil {
						t.Fatal("deserialize:", err)

					}

					if err := compareStructs(m, m1); err != nil {
						t.Error(err)
					}
				},
			)
		}()
		return true
	}
	quick.Check(f, nil)
	wg.Wait()
}

func BenchmarkSerializer(b *testing.B) {
	sharedSerializer := NewTSerializer()
	poolSerializer := NewTSerializerPool(NewTSerializer)
	sharedDeserializer := NewTDeserializer()
	poolDeserializer := NewTDeserializerPool(NewTDeserializer)

	cases := []struct {
		Label        string
		Serializer   func() serializer
		Deserializer func() deserializer
	}{
		{
			// Baseline uses shared plain serializer/deserializer
			Label: "baseline",
			Serializer: func() serializer {
				return sharedSerializer
			},
			Deserializer: func() deserializer {
				return sharedDeserializer
			},
		},
		{
			// Plain creates new serializer/deserializer on every run,
			// as that's how it's used in real world
			Label: "plain",
			Serializer: func() serializer {
				return NewTSerializer()
			},
			Deserializer: func() deserializer {
				return NewTDeserializer()
			},
		},
		{
			// Pool uses the shared pool serializer/deserializer
			Label: "pool",
			Serializer: func() serializer {
				return poolSerializer
			},
			Deserializer: func() deserializer {
				return poolDeserializer
			},
		},
	}

	for _, c := range cases {
		b.Run(
			c.Label,
			func(b *testing.B) {
				for i := 0; i < b.N; i++ {
					s := c.Serializer()
					m := MyTestStruct{}
					str, _ := s.WriteString(context.Background(), &m)
					var m1 MyTestStruct
					d := c.Deserializer()
					d.ReadString(context.Background(), &m1, str)
				}
			},
		)
	}
}
