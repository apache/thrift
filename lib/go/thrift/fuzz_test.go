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

// Native Go fuzz targets for the protocol and transport read paths.
//
// These need no generated code: a reader that calls ReadMessageBegin and then
// Skip walks the same library code that generated struct-read code does, since
// Skip dispatches on the wire-supplied type and reads containers at
// wire-supplied sizes. The targets in lib/go/test/fuzz complement these by
// reading an actual generated struct.
//
// Run as ordinary tests -- go test ./thrift, which is what "make check" does --
// they replay the seed corpus below plus anything under
// testdata/fuzz/<Target>/, so a fixed input stays fixed. To fuzz for real:
//
//	go test ./thrift -run '^$' -fuzz FuzzReadBinary -fuzztime 2m
//
// A failing input is written to testdata/fuzz/<Target>/ and should be committed
// with the fix.

package thrift

import (
	"context"
	"testing"
)

// fuzzConf is what a server gets by default, with the ceilings pulled down so
// that a made-up size costs the fuzzer time rather than memory.
func fuzzConf() *TConfiguration {
	return &TConfiguration{
		MaxMessageSize: 1 << 18,
		MaxFrameSize:   1 << 18,
	}
}

func fuzzBuffer(data []byte) *TMemoryBuffer {
	trans := NewTMemoryBufferLen(len(data))
	trans.Write(data)
	return trans
}

// fuzzReadMessage reads an enveloped message and walks its payload.
func fuzzReadMessage(p TProtocol) {
	ctx := context.Background()
	if _, _, _, err := p.ReadMessageBegin(ctx); err != nil {
		return
	}
	if err := SkipDefaultDepth(ctx, p, STRUCT); err != nil {
		return
	}
	p.ReadMessageEnd(ctx)
}

// fuzzReadStruct reads a bare struct, the way TDeserializer does.
func fuzzReadStruct(p TProtocol) {
	SkipDefaultDepth(context.Background(), p, STRUCT)
}

// fuzzSeeds covers the shapes worth starting from: nothing, a well formed call
// in each dialect, and the sizes and types that have to be rejected rather than
// believed.
func fuzzSeeds(f *testing.F) {
	f.Add([]byte{})
	// Strict binary "ping" call with an empty argument struct.
	f.Add([]byte{0x80, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x04, 'p', 'i', 'n', 'g', 0x00, 0x00, 0x00, 0x00, 0x00})
	// Non-strict binary: bare name length, no version word.
	f.Add([]byte{0x00, 0x00, 0x00, 0x04, 'p', 'i', 'n', 'g', 0x01, 0x00, 0x00, 0x00, 0x00, 0x00})
	// Compact "ping" call.
	f.Add([]byte{0x82, 0x21, 0x01, 0x04, 'p', 'i', 'n', 'g', 0x00})
	// JSON and simple-JSON envelopes.
	f.Add([]byte(`[1,"ping",1,0,{}]`))
	f.Add([]byte(`[1,"ping",1,0,{"1":{"str":"x"}}]`))
	// A header frame: frame size, header magic, flags, sequence id, header
	// length, then no transforms and no info headers.
	f.Add([]byte{0x00, 0x00, 0x00, 0x0e, 0x0f, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00})
	// A string field claiming a negative length.
	f.Add([]byte{0x0b, 0x00, 0x01, 0xff, 0xff, 0xff, 0xff, 0x00})
	// A string field claiming more bytes than the message can hold.
	f.Add([]byte{0x0b, 0x00, 0x01, 0x7f, 0xff, 0xff, 0xff, 0x00})
	// A list header claiming a huge element count.
	f.Add([]byte{0x0f, 0x00, 0x01, 0x08, 0x7f, 0xff, 0xff, 0xff, 0x00})
	// A map header with mismatched key/value types and a large count.
	f.Add([]byte{0x0d, 0x00, 0x01, 0x0b, 0x0c, 0x7f, 0xff, 0xff, 0xff, 0x00})
	// Nested structs, to walk into the depth limit.
	f.Add([]byte{0x0c, 0x00, 0x01, 0x0c, 0x00, 0x01, 0x0c, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00})
	// A frame header on its own, with nothing behind it.
	f.Add([]byte{0x7f, 0xff, 0xff, 0xff})
}

func FuzzReadBinary(f *testing.F) {
	fuzzSeeds(f)
	f.Fuzz(func(t *testing.T, data []byte) {
		fuzzReadMessage(NewTBinaryProtocolConf(fuzzBuffer(data), fuzzConf()))
	})
}

func FuzzReadBinaryNonStrict(f *testing.F) {
	fuzzSeeds(f)
	f.Fuzz(func(t *testing.T, data []byte) {
		conf := fuzzConf()
		strict := false
		conf.TBinaryStrictRead = &strict
		fuzzReadMessage(NewTBinaryProtocolConf(fuzzBuffer(data), conf))
	})
}

func FuzzReadBinaryStruct(f *testing.F) {
	fuzzSeeds(f)
	f.Fuzz(func(t *testing.T, data []byte) {
		fuzzReadStruct(NewTBinaryProtocolConf(fuzzBuffer(data), fuzzConf()))
	})
}

func FuzzReadCompact(f *testing.F) {
	fuzzSeeds(f)
	f.Fuzz(func(t *testing.T, data []byte) {
		fuzzReadMessage(NewTCompactProtocolConf(fuzzBuffer(data), fuzzConf()))
	})
}

func FuzzReadCompactStruct(f *testing.F) {
	fuzzSeeds(f)
	f.Fuzz(func(t *testing.T, data []byte) {
		fuzzReadStruct(NewTCompactProtocolConf(fuzzBuffer(data), fuzzConf()))
	})
}

func FuzzReadJSON(f *testing.F) {
	fuzzSeeds(f)
	f.Fuzz(func(t *testing.T, data []byte) {
		fuzzReadMessage(NewTJSONProtocol(fuzzBuffer(data)))
	})
}

func FuzzReadJSONStruct(f *testing.F) {
	fuzzSeeds(f)
	f.Fuzz(func(t *testing.T, data []byte) {
		fuzzReadStruct(NewTJSONProtocol(fuzzBuffer(data)))
	})
}

func FuzzReadSimpleJSON(f *testing.F) {
	fuzzSeeds(f)
	f.Fuzz(func(t *testing.T, data []byte) {
		fuzzReadMessage(NewTSimpleJSONProtocol(fuzzBuffer(data)))
	})
}

// FuzzReadHeader covers THeaderProtocol, which decides the client type from the
// first bytes and, for a header frame, parses the transform list, the info
// headers and the padding before any payload is read.
func FuzzReadHeader(f *testing.F) {
	fuzzSeeds(f)
	f.Fuzz(func(t *testing.T, data []byte) {
		fuzzReadMessage(NewTHeaderProtocolConf(fuzzBuffer(data), fuzzConf()))
	})
}

// FuzzReadFramed covers the frame length taken off the wire by
// TFramedTransport, underneath a protocol that then reads the frame.
func FuzzReadFramed(f *testing.F) {
	fuzzSeeds(f)
	f.Fuzz(func(t *testing.T, data []byte) {
		trans := NewTFramedTransportConf(fuzzBuffer(data), fuzzConf())
		fuzzReadMessage(NewTBinaryProtocolConf(trans, fuzzConf()))
	})
}

// FuzzServerDispatch covers the server side hop: client type detection, then a
// processor that dispatches on the peer-supplied method name, and the reply
// written back for a name it does not know.
func FuzzServerDispatch(f *testing.F) {
	fuzzSeeds(f)
	f.Fuzz(func(t *testing.T, data []byte) {
		in := NewTHeaderProtocolConf(fuzzBuffer(data), fuzzConf())
		out := NewTHeaderProtocolConf(NewTMemoryBuffer(), fuzzConf())
		processor := NewTMultiplexedProcessor()
		processor.RegisterProcessor("svc", &fuzzProcessor{})
		processor.RegisterDefault(&fuzzProcessor{})
		// A single buffer can hold more than one message; stop at the first
		// one the processor rejects.
		for range 4 {
			if ok, err := processor.Process(context.Background(), in, out); err != nil || !ok {
				return
			}
		}
	})
}

// fuzzProcessor stands in for generated dispatch code: read the arguments,
// write an empty reply.
type fuzzProcessor struct {
	funcs map[string]TProcessorFunction
}

func (p *fuzzProcessor) ProcessorMap() map[string]TProcessorFunction {
	return p.funcs
}

func (p *fuzzProcessor) AddToProcessorMap(name string, fn TProcessorFunction) {
	if p.funcs == nil {
		p.funcs = make(map[string]TProcessorFunction)
	}
	p.funcs[name] = fn
}

func (p *fuzzProcessor) Process(ctx context.Context, in, out TProtocol) (bool, TException) {
	name, _, seqID, err := in.ReadMessageBegin(ctx)
	if err != nil {
		return false, NewTProtocolException(err)
	}
	if err := SkipDefaultDepth(ctx, in, STRUCT); err != nil {
		in.ReadMessageEnd(ctx)
		return false, NewTProtocolException(err)
	}
	if err := in.ReadMessageEnd(ctx); err != nil {
		return false, NewTProtocolException(err)
	}
	if err := out.WriteMessageBegin(ctx, name, REPLY, seqID); err != nil {
		return false, NewTProtocolException(err)
	}
	out.WriteStructBegin(ctx, "result")
	out.WriteFieldStop(ctx)
	out.WriteStructEnd(ctx)
	out.WriteMessageEnd(ctx)
	out.Flush(ctx)
	return true, nil
}

// FuzzParseTuuid covers the uuid text parser, which TSimpleJSONProtocol feeds
// straight from the wire.
func FuzzParseTuuid(f *testing.F) {
	f.Add("6ba7b810-9dad-11d1-80b4-00c04fd430c8")
	f.Add("")
	f.Add("------------------------------------")
	f.Add("6ba7b810-9dad-11d1-80b4-00c04fd430")
	f.Fuzz(func(t *testing.T, s string) {
		ParseTuuid(s)
	})
}
