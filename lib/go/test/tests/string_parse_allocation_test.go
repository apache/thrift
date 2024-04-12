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
	"fmt"
	"strings"
	"testing"

	"github.com/apache/thrift/lib/go/test/gopath/src/stringparseallocationtest"
	"github.com/apache/thrift/lib/go/thrift"
)

func TestSimpleJsonStringParse_Allocations(t *testing.T) {
	byteAllocationLimit := 100 * 1024 // 100 KB
	res := testing.Benchmark(BenchmarkSimpleJsonStringParse_Allocations)
	if res.AllocedBytesPerOp() > int64(byteAllocationLimit) {
		t.Errorf("Total memory allocation size too high: %d (> %d)", res.AllocedBytesPerOp(), byteAllocationLimit)
	}
}

func BenchmarkSimpleJsonStringParse_Allocations(b *testing.B) {
	b.ReportAllocs()
	b.StopTimer()
	numEscapedQuotes := 1000
	var sb strings.Builder
	for i := 0; i < numEscapedQuotes; i++ {
		sb.WriteString(`\"`)
	}

	testString := fmt.Sprintf(`{"1": {"str": "this is a test with %d of escaped quotes %s"}}`, numEscapedQuotes, sb.String())
	stringStruct := stringparseallocationtest.NewStringStruct()
	transport := thrift.NewTMemoryBuffer()
	p := thrift.NewTJSONProtocol(transport)

	for i := 0; i < b.N; i++ {
		transport.Reset()
		transport.WriteString(testString)
		transport.Flush(context.Background())
		b.StartTimer()
		_ = stringStruct.Read(context.Background(), p)
		b.StopTimer()
	}
}
