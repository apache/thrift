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
	"compress/zlib"
	"fmt"
	"maps"
	"slices"
	"testing"
)

func TestZlibWriterPools(t *testing.T) {
	// make sure we have the writer pools created at the given levels
	for _, level := range []int{
		zlib.HuffmanOnly,
		zlib.DefaultCompression,
		zlib.NoCompression,
		zlib.BestSpeed,
		zlib.BestCompression,
	} {
		t.Run(fmt.Sprintf("%d", level), func(t *testing.T) {
			_, ok := zlibWriterPools[level]
			if !ok {
				t.Errorf("level %d does not exist in the writer pools", level)
			}
		})
	}
	if t.Failed() {
		levels := slices.Collect(maps.Keys(zlibWriterPools))
		slices.Sort(levels)
		t.Log("zlib writer pools:", levels)
	}
}
