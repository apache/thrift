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
	"math"
	"strings"
	"testing"
)

func TestReadWriteBinaryProtocol(t *testing.T) {
	ReadWriteProtocolTest(t, NewTBinaryProtocolFactoryDefault())
}

const (
	safeReadBytesSource = `
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer sit amet
tincidunt nibh. Phasellus vel convallis libero, sit amet posuere quam. Nullam
blandit velit at nibh fringilla, sed egestas erat dapibus. Sed hendrerit
tincidunt accumsan. Curabitur consectetur bibendum dui nec hendrerit. Fusce quis
turpis nec magna efficitur volutpat a ut nibh. Vestibulum odio risus, tristique
a nisi et, congue mattis mi. Vivamus a nunc justo. Mauris molestie sagittis
magna, hendrerit auctor lectus egestas non. Phasellus pretium, odio sit amet
bibendum feugiat, velit nunc luctus erat, ac bibendum mi dui molestie nulla.
Nullam fermentum magna eu elit vehicula tincidunt. Etiam ornare laoreet
dignissim. Ut sed nunc ac neque vulputate fermentum. Morbi volutpat dapibus
magna, at porttitor quam facilisis a. Donec eget fermentum risus. Aliquam erat
volutpat.

Phasellus molestie id ante vel iaculis. Fusce eget quam nec quam viverra laoreet
vitae a dui. Mauris blandit blandit dui, iaculis interdum diam mollis at. Morbi
vel sem et.
`
	safeReadBytesSourceLen = len(safeReadBytesSource)
)

func TestSafeReadBytes(t *testing.T) {
	srcData := []byte(safeReadBytesSource)

	for _, c := range []struct {
		label     string
		askedSize int32
		dataSize  int
	}{
		{
			label:     "normal",
			askedSize: 100,
			dataSize:  100,
		},
		{
			label:     "max-askedSize",
			askedSize: math.MaxInt32,
			dataSize:  safeReadBytesSourceLen,
		},
	} {
		t.Run(c.label, func(t *testing.T) {
			data := bytes.NewReader(srcData[:c.dataSize])
			buf, err := safeReadBytes(c.askedSize, data)
			if len(buf) != c.dataSize {
				t.Errorf(
					"Expected to read %d bytes, got %d",
					c.dataSize,
					len(buf),
				)
			}
			if !strings.HasPrefix(safeReadBytesSource, string(buf)) {
				t.Errorf("Unexpected read data: %q", buf)
			}
			if int32(c.dataSize) < c.askedSize {
				// We expect error in this case
				if err == nil {
					t.Errorf(
						"Expected error when dataSize %d < askedSize %d, got nil",
						c.dataSize,
						c.askedSize,
					)
				}
			} else {
				// We expect no error in this case
				if err != nil {
					t.Errorf(
						"Expected no error when dataSize %d >= askedSize %d, got: %v",
						c.dataSize,
						c.askedSize,
						err,
					)
				}
			}
		})
	}
}

func generateSafeReadBytesBenchmark(askedSize int32, dataSize int) func(b *testing.B) {
	return func(b *testing.B) {
		data := make([]byte, dataSize)
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			safeReadBytes(askedSize, bytes.NewReader(data))
		}
	}
}

func TestSafeReadBytesAlloc(t *testing.T) {
	if testing.Short() {
		// NOTE: Since this test runs a benchmark test, it takes at
		// least 1 second.
		//
		// In general we try to avoid unit tests taking that long to run,
		// but it's to verify a security issue so we made an exception
		// here:
		// https://issues.apache.org/jira/browse/THRIFT-5322
		t.Skip("skipping test in short mode.")
	}

	const (
		askedSize = int32(math.MaxInt32)
		dataSize  = 4096
	)

	// The purpose of this test is that in the case a string header says
	// that it has a string askedSize bytes long, the implementation should
	// not just allocate askedSize bytes upfront. So when there're actually
	// not enough data to be read (dataSize), the actual allocated bytes
	// should be somewhere between dataSize and askedSize.
	//
	// Different approachs could have different memory overheads, so this
	// target is arbitrary in nature. But when dataSize is small enough
	// compare to askedSize, half the askedSize is a good and safe target.
	const target = int64(askedSize) / 2

	bm := testing.Benchmark(generateSafeReadBytesBenchmark(askedSize, dataSize))
	actual := bm.AllocedBytesPerOp()
	if actual > target {
		t.Errorf(
			"Expected allocated bytes per op to be <= %d, got %d",
			target,
			actual,
		)
	} else {
		t.Logf("Allocated bytes: %d B/op", actual)
	}
}

func BenchmarkSafeReadBytes(b *testing.B) {
	for _, c := range []struct {
		label     string
		askedSize int32
		dataSize  int
	}{
		{
			label:     "normal",
			askedSize: 100,
			dataSize:  100,
		},
		{
			label:     "max-askedSize",
			askedSize: math.MaxInt32,
			dataSize:  4096,
		},
	} {
		b.Run(c.label, generateSafeReadBytesBenchmark(c.askedSize, c.dataSize))
	}
}
