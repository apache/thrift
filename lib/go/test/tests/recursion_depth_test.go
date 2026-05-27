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

package tests

// These tests exercise the recursion-depth limit through the *generated* struct
// read/write path (RecTree.Read / RecTree.Write) -- the real path a hostile
// payload would hit -- rather than the depth counter (CheckRecursionDepth) in
// isolation. The whole point of the fix is that the generator threads the
// per-context tracker into every nested .Read/.Write call, so only a round-trip
// over generated code actually proves it.
//
// RecTree comes from test/Recursive.thrift. A linear chain of nested RecTree
// structs adds exactly one struct level per node, so a chain of N nodes reaches
// recursion depth N. The limit is thrift.DEFAULT_RECURSION_DEPTH.

import (
	"context"
	"errors"
	"testing"

	"github.com/apache/thrift/lib/go/test/gopath/src/recursive"
	"github.com/apache/thrift/lib/go/thrift"
)

var recursionProtocols = []struct {
	name string
	make func(thrift.TTransport) thrift.TProtocol
}{
	{"binary", func(t thrift.TTransport) thrift.TProtocol { return thrift.NewTBinaryProtocolConf(t, nil) }},
	{"compact", func(t thrift.TTransport) thrift.TProtocol { return thrift.NewTCompactProtocolConf(t, nil) }},
	{"json", func(t thrift.TTransport) thrift.TProtocol { return thrift.NewTJSONProtocol(t) }},
}

// buildChain returns a linearly nested RecTree that is depth struct levels deep.
func buildChain(depth int) *recursive.RecTree {
	node := &recursive.RecTree{Item: int16(depth)}
	if depth > 1 {
		node.Children = []*recursive.RecTree{buildChain(depth - 1)}
	}
	return node
}

// chainDepth counts the struct nesting depth of a linear RecTree chain.
func chainDepth(t *recursive.RecTree) int {
	n := 0
	for t != nil {
		n++
		if len(t.Children) == 0 {
			break
		}
		t = t.Children[0]
	}
	return n
}

// writeRawChain emits, via raw protocol calls (which carry no depth guard), the
// wire image of a RecTree chain depth levels deep. This lets the read test feed
// an over-limit payload that the guarded writer would itself refuse to produce.
func writeRawChain(ctx context.Context, oprot thrift.TProtocol, depth int) error {
	if err := oprot.WriteStructBegin(ctx, "RecTree"); err != nil {
		return err
	}
	if err := oprot.WriteFieldBegin(ctx, "children", thrift.LIST, 1); err != nil {
		return err
	}
	if depth > 1 {
		if err := oprot.WriteListBegin(ctx, thrift.STRUCT, 1); err != nil {
			return err
		}
		if err := writeRawChain(ctx, oprot, depth-1); err != nil {
			return err
		}
	} else {
		if err := oprot.WriteListBegin(ctx, thrift.STRUCT, 0); err != nil {
			return err
		}
	}
	if err := oprot.WriteListEnd(ctx); err != nil {
		return err
	}
	if err := oprot.WriteFieldEnd(ctx); err != nil {
		return err
	}
	if err := oprot.WriteFieldBegin(ctx, "item", thrift.I16, 2); err != nil {
		return err
	}
	if err := oprot.WriteI16(ctx, int16(depth)); err != nil {
		return err
	}
	if err := oprot.WriteFieldEnd(ctx); err != nil {
		return err
	}
	if err := oprot.WriteFieldStop(ctx); err != nil {
		return err
	}
	return oprot.WriteStructEnd(ctx)
}

func isDepthLimit(err error) bool {
	var pe thrift.TProtocolException
	return errors.As(err, &pe) && pe.TypeId() == thrift.DEPTH_LIMIT
}

// A chain exactly at the limit must round-trip cleanly through generated
// Write and Read (off-by-one guard, and proof the counter unwinds on the way
// back out so the read does not inherit the writer's depth).
func TestRecursionDepthRoundTripAtLimit(t *testing.T) {
	ctx := context.Background()
	limit := thrift.DEFAULT_RECURSION_DEPTH
	for _, proto := range recursionProtocols {
		t.Run(proto.name, func(t *testing.T) {
			trans := thrift.NewTMemoryBuffer()
			wp := proto.make(trans)
			if err := buildChain(limit).Write(ctx, wp); err != nil {
				t.Fatalf("write at limit (%d) failed: %v", limit, err)
			}
			if err := wp.Flush(ctx); err != nil {
				t.Fatalf("flush failed: %v", err)
			}
			out := &recursive.RecTree{}
			if err := out.Read(ctx, proto.make(trans)); err != nil {
				t.Fatalf("read at limit (%d) failed: %v", limit, err)
			}
			if got := chainDepth(out); got != limit {
				t.Fatalf("round-trip depth mismatch: got %d, want %d", got, limit)
			}
		})
	}
}

// Writing a chain one level over the limit must be rejected with DEPTH_LIMIT.
func TestRecursionDepthWriteOverLimit(t *testing.T) {
	ctx := context.Background()
	limit := thrift.DEFAULT_RECURSION_DEPTH
	for _, proto := range recursionProtocols {
		t.Run(proto.name, func(t *testing.T) {
			trans := thrift.NewTMemoryBuffer()
			p := proto.make(trans)
			err := buildChain(limit+1).Write(ctx, p)
			if !isDepthLimit(err) {
				t.Fatalf("write over limit (%d): want DEPTH_LIMIT, got %v", limit+1, err)
			}
		})
	}
}

// Reading a payload one level over the limit must be rejected with DEPTH_LIMIT.
// The payload is hand-serialized so the over-limit bytes exist on the wire,
// mimicking a hostile message from the network.
func TestRecursionDepthReadOverLimit(t *testing.T) {
	ctx := context.Background()
	limit := thrift.DEFAULT_RECURSION_DEPTH
	for _, proto := range recursionProtocols {
		t.Run(proto.name, func(t *testing.T) {
			trans := thrift.NewTMemoryBuffer()
			wp := proto.make(trans)
			if err := writeRawChain(ctx, wp, limit+1); err != nil {
				t.Fatalf("crafting over-limit payload failed: %v", err)
			}
			if err := wp.Flush(ctx); err != nil {
				t.Fatalf("flush failed: %v", err)
			}
			err := (&recursive.RecTree{}).Read(ctx, proto.make(trans))
			if !isDepthLimit(err) {
				t.Fatalf("read over limit (%d): want DEPTH_LIMIT, got %v", limit+1, err)
			}
		})
	}
}

// A wide but shallow tree whose total number of struct read/writes far exceeds
// the limit must still round-trip. This only holds if the depth counter is
// decremented for every sibling, i.e. it tracks nesting depth, not a running
// total of structs seen.
func TestRecursionDepthWideStructureRoundTrips(t *testing.T) {
	ctx := context.Background()
	width := thrift.DEFAULT_RECURSION_DEPTH * 3
	for _, proto := range recursionProtocols {
		t.Run(proto.name, func(t *testing.T) {
			root := &recursive.RecTree{Item: 0, Children: make([]*recursive.RecTree, 0, width)}
			for i := 0; i < width; i++ {
				root.Children = append(root.Children, &recursive.RecTree{Item: int16(i)})
			}
			trans := thrift.NewTMemoryBuffer()
			wp := proto.make(trans)
			if err := root.Write(ctx, wp); err != nil {
				t.Fatalf("write wide structure failed: %v", err)
			}
			if err := wp.Flush(ctx); err != nil {
				t.Fatalf("flush failed: %v", err)
			}
			out := &recursive.RecTree{}
			if err := out.Read(ctx, proto.make(trans)); err != nil {
				t.Fatalf("read wide structure failed: %v", err)
			}
			if len(out.Children) != width {
				t.Fatalf("wide structure child count mismatch: got %d, want %d", len(out.Children), width)
			}
		})
	}
}

// The same bound must apply to recursive exceptions. CoError <-> CoError2 (from
// test/Recursive.thrift) form a mutually recursive exception chain through the
// "other" field (id 1, type STRUCT). Exceptions are read/written through the
// same generated path as structs, so the guard reaches them too.
//
// Unlike the list-based RecTree, the chain terminates in a struct-typed field,
// and Go serializes a non-optional struct field even when nil, so a built chain
// of N nodes writes one level deeper than N. These tests therefore use generous
// depth margins rather than the exact limit boundary (which the struct cases
// above already pin): within-limit exceptions must round-trip, over-limit ones
// must be rejected in both directions.

// buildErrorChain returns a CoError/CoError2 chain that is depth nodes deep.
func buildErrorChain(depth int) *recursive.CoError {
	if depth <= 1 {
		return &recursive.CoError{}
	}
	return &recursive.CoError{Other: buildError2Chain(depth - 1)}
}

func buildError2Chain(depth int) *recursive.CoError2 {
	if depth <= 1 {
		return &recursive.CoError2{}
	}
	return &recursive.CoError2{Other: buildErrorChain(depth - 1)}
}

// writeRawErrorChain emits, via raw protocol calls (no depth guard), the wire
// image of a CoError chain depth levels deep, using the real recursive field
// (id 1, type STRUCT) so the reader recurses through the guarded generated Read
// rather than Skip.
func writeRawErrorChain(ctx context.Context, oprot thrift.TProtocol, depth int) error {
	if err := oprot.WriteStructBegin(ctx, "CoError"); err != nil {
		return err
	}
	if depth > 1 {
		if err := oprot.WriteFieldBegin(ctx, "other", thrift.STRUCT, 1); err != nil {
			return err
		}
		if err := writeRawErrorChain(ctx, oprot, depth-1); err != nil {
			return err
		}
		if err := oprot.WriteFieldEnd(ctx); err != nil {
			return err
		}
	}
	if err := oprot.WriteFieldStop(ctx); err != nil {
		return err
	}
	return oprot.WriteStructEnd(ctx)
}

// A recursive exception comfortably within the limit must round-trip. The depth
// (well above limit/2) also confirms the bound is the full limit, not a halved one.
func TestRecursionDepthExceptionWithinLimitRoundTrips(t *testing.T) {
	ctx := context.Background()
	depth := thrift.DEFAULT_RECURSION_DEPTH - 16
	for _, proto := range recursionProtocols {
		t.Run(proto.name, func(t *testing.T) {
			trans := thrift.NewTMemoryBuffer()
			wp := proto.make(trans)
			if err := buildErrorChain(depth).Write(ctx, wp); err != nil {
				t.Fatalf("write exception within limit (%d) failed: %v", depth, err)
			}
			if err := wp.Flush(ctx); err != nil {
				t.Fatalf("flush failed: %v", err)
			}
			out := &recursive.CoError{}
			if err := out.Read(ctx, proto.make(trans)); err != nil {
				t.Fatalf("read exception within limit (%d) failed: %v", depth, err)
			}
			if out.Other == nil {
				t.Fatalf("round-trip lost the nested exception chain")
			}
		})
	}
}

func TestRecursionDepthExceptionWriteOverLimit(t *testing.T) {
	ctx := context.Background()
	depth := thrift.DEFAULT_RECURSION_DEPTH * 2
	for _, proto := range recursionProtocols {
		t.Run(proto.name, func(t *testing.T) {
			trans := thrift.NewTMemoryBuffer()
			p := proto.make(trans)
			err := buildErrorChain(depth).Write(ctx, p)
			if !isDepthLimit(err) {
				t.Fatalf("write exception over limit (%d): want DEPTH_LIMIT, got %v", depth, err)
			}
		})
	}
}

func TestRecursionDepthExceptionReadOverLimit(t *testing.T) {
	ctx := context.Background()
	depth := thrift.DEFAULT_RECURSION_DEPTH * 2
	for _, proto := range recursionProtocols {
		t.Run(proto.name, func(t *testing.T) {
			trans := thrift.NewTMemoryBuffer()
			wp := proto.make(trans)
			if err := writeRawErrorChain(ctx, wp, depth); err != nil {
				t.Fatalf("crafting over-limit exception payload failed: %v", err)
			}
			if err := wp.Flush(ctx); err != nil {
				t.Fatalf("flush failed: %v", err)
			}
			err := (&recursive.CoError{}).Read(ctx, proto.make(trans))
			if !isDepthLimit(err) {
				t.Fatalf("read exception over limit (%d): want DEPTH_LIMIT, got %v", depth, err)
			}
		})
	}
}
