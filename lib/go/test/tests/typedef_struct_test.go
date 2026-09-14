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

import (
	"context"
	"testing"

	"github.com/apache/thrift/lib/go/test/gopath/src/typedefincludetest"
	"github.com/apache/thrift/lib/go/test/gopath/src/typedefstructtest"
	"github.com/apache/thrift/lib/go/thrift"
)

// A typedef of a struct is generated as a Go alias, so it names the struct
// itself and keeps its method set. These assignments do not compile against a
// defined type. See https://issues.apache.org/jira/browse/THRIFT-6197.
var (
	_ *typedefstructtest.InnerStruct = (*typedefstructtest.InnerAlias)(nil)
	_ *typedefstructtest.InnerStruct = (*typedefstructtest.NestedAlias)(nil)
	_ *typedefstructtest.ExcStruct   = (*typedefstructtest.ExcAlias)(nil)
	_ *typedefstructtest.InnerStruct = (*typedefincludetest.IncludedAlias)(nil)
)

// A field whose type is declared after its first use keeps that type rather
// than the type it resolves to. THRIFT-5601, THRIFT-5489.
var _ *typedefstructtest.ForwardTypedef = typedefstructtest.NewUsesForwardTypedef().Value

// A struct declared after its first use is still reached through a pointer, so
// the getters chain. Losing that is what made THRIFT-5685 revert the first
// attempt at THRIFT-5601. Never called; it is here for the compiler.
func forwardStructGetterChain(p *typedefstructtest.UsesForwardStruct) int64 {
	return p.GetForward().GetPayload()
}

var _ = forwardStructGetterChain

// A typedef of a container names the container, not its element type.
var (
	_ typedefstructtest.InnerList = []*typedefstructtest.InnerStruct(nil)
	_ typedefstructtest.InnerMap  = map[string]*typedefstructtest.InnerStruct(nil)
)

// A typedef'd struct is passed through the service interface the same way the
// struct would be. THRIFT-3491.
type aliasHandler struct{}

func (aliasHandler) Echo(ctx context.Context, value *typedefstructtest.InnerAlias) (*typedefstructtest.InnerAlias, error) {
	return value, nil
}

var _ typedefstructtest.AliasService = aliasHandler{}

func newOuter() *typedefstructtest.Outer {
	s := typedefstructtest.NewOuter()
	s.Inner = &typedefstructtest.InnerAlias{S: "inner"}
	s.Inners = []*typedefstructtest.InnerAlias{
		{S: "first"},
		{S: "second"},
	}
	s.ByName = map[string]*typedefstructtest.InnerAlias{
		"key": {S: "value"},
	}
	s.Nested = &typedefstructtest.NestedAlias{S: "nested"}
	s.ListAlias = typedefstructtest.InnerList{{S: "listed"}}
	s.MapAlias = typedefstructtest.InnerMap{"mapped": {S: "mapped"}}
	return s
}

func TestTypedefStructRoundTrip(t *testing.T) {
	for label, factory := range map[string]thrift.TProtocolFactory{
		"binary":  thrift.NewTBinaryProtocolFactoryConf(nil),
		"compact": thrift.NewTCompactProtocolFactoryConf(nil),
		"json":    thrift.NewTJSONProtocolFactory(),
	} {
		t.Run(label, func(t *testing.T) {
			ctx := context.Background()
			src := newOuter()

			serializer := thrift.NewTSerializer()
			serializer.Protocol = factory.GetProtocol(serializer.Transport)
			data, err := serializer.Write(ctx, src)
			if err != nil {
				t.Fatalf("write: %v", err)
			}

			dst := typedefstructtest.NewOuter()
			des := thrift.NewTDeserializer()
			des.Protocol = factory.GetProtocol(des.Transport)
			if err := des.Read(ctx, dst, data); err != nil {
				t.Fatalf("read: %v", err)
			}

			if !src.Equals(dst) {
				t.Errorf("round trip mismatch:\n src=%v\n dst=%v", src, dst)
			}
			if got, want := dst.GetInner().S, "inner"; got != want {
				t.Errorf("inner got %q want %q", got, want)
			}
		})
	}
}

func TestTypedefStructFromIncludedFile(t *testing.T) {
	ctx := context.Background()
	src := typedefincludetest.NewHoldsIncluded()
	src.Value = &typedefincludetest.IncludedAlias{S: "from another file"}
	src.Values = []*typedefincludetest.IncludedAlias{{S: "in a list"}}

	serializer := thrift.NewTSerializer()
	data, err := serializer.Write(ctx, src)
	if err != nil {
		t.Fatalf("write: %v", err)
	}

	dst := typedefincludetest.NewHoldsIncluded()
	if err := thrift.NewTDeserializer().Read(ctx, dst, data); err != nil {
		t.Fatalf("read: %v", err)
	}
	if !src.Equals(dst) {
		t.Errorf("round trip mismatch:\n src=%v\n dst=%v", src, dst)
	}
}

func TestForwardTypedefRoundTrip(t *testing.T) {
	ctx := context.Background()
	src := typedefstructtest.NewUsesForwardTypedef()
	value := typedefstructtest.ForwardTypedef(42)
	src.Value = &value
	src.Values = []typedefstructtest.ForwardTypedef{1, 2, 3}

	serializer := thrift.NewTSerializer()
	data, err := serializer.Write(ctx, src)
	if err != nil {
		t.Fatalf("write: %v", err)
	}

	dst := typedefstructtest.NewUsesForwardTypedef()
	if err := thrift.NewTDeserializer().Read(ctx, dst, data); err != nil {
		t.Fatalf("read: %v", err)
	}
	if got, want := dst.GetValue(), value; got != want {
		t.Errorf("value got %v want %v", got, want)
	}
	if !src.Equals(dst) {
		t.Errorf("round trip mismatch:\n src=%v\n dst=%v", src, dst)
	}
}
