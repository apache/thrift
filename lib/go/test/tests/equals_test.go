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
	"equalstest"
	"strconv"
	"testing"
)

func TestEquals(t *testing.T) {
	// test basic field
	basicTgt, basicSrc := genBasicFoo(), genBasicFoo()
	if !basicTgt.Equals(basicSrc) {
		t.Error("BasicEqualsFoo.Equals() test failed")
	}
	basicSrc.EnumFoo = equalstest.EnumFoo_e2
	if basicTgt.Equals(basicSrc) {
		t.Error("BasicEqualsFoo.Equals() test failed")
	}
	basicSrc = genBasicFoo()
	basicSrc.OptBoolFoo = nil
	if basicTgt.Equals(basicSrc) || basicSrc.Equals(basicTgt) {
		t.Error("BasicEqualsFoo.Equals() test failed")
	}
	if !(&equalstest.BasicEqualsFoo{}).Equals(&equalstest.BasicEqualsFoo{}) {
		t.Error("BasicEqualsFoo.Equals() test failed")
	}
	// test struct field
	structTgt, structSrc := genStructFoo(), genStructFoo()
	if !structTgt.Equals(structSrc) {
		t.Error("StructEqualsFoo.Equals() test failed")
	}
	structSrc.OptStructFoo.EnumFoo = equalstest.EnumFoo_e2
	if structTgt.Equals(structSrc) {
		t.Error("StructEqualsFoo.Equals() test failed")
	}
	structSrc = genStructFoo()
	structSrc.OptStructFoo = nil
	if structTgt.Equals(structSrc) || structSrc.Equals(structTgt) {
		t.Error("StructEqualsFoo.Equals() test failed")
	}
	if !(&equalstest.StructEqualsFoo{}).Equals(&equalstest.StructEqualsFoo{}) {
		t.Error("StructEqualsFoo.Equals() test failed")
	}
	// test list field
	listTgt, listSrc := genListFoo(), genListFoo()
	if !listTgt.Equals(listSrc) {
		t.Error("ListEqualsFoo.Equals() test failed")
	}
	listSrc.OptI64StringMapListFoo[0][1] = "0"
	if listTgt.Equals(listSrc) {
		t.Error("ListEqualsFoo.Equals() test failed")
	}
	listSrc = genListFoo()
	listSrc.OptI64StringMapListFoo = nil
	if listTgt.Equals(listSrc) || listSrc.Equals(listTgt) {
		t.Error("ListEqualsFoo.Equals() test failed")
	}
	if !(&equalstest.ListEqualsFoo{}).Equals(&equalstest.ListEqualsFoo{}) {
		t.Error("ListEqualsFoo.Equals() test failed")
	}
	// test set field
	setTgt, setSrc := genSetFoo(), genSetFoo()
	if !setTgt.Equals(setSrc) {
		t.Error("SetEqualsFoo.Equals() test failed")
	}
	setSrc.OptI64StringMapSetFoo[0][1] = "0"
	if setTgt.Equals(setSrc) {
		t.Error("SetEqualsFoo.Equals() test failed")
	}
	setSrc = genSetFoo()
	setSrc.OptI64StringMapSetFoo = nil
	if setTgt.Equals(setSrc) || setSrc.Equals(setTgt) {
		t.Error("SetEqualsFoo.Equals() test failed")
	}
	if !(&equalstest.SetEqualsFoo{}).Equals(&equalstest.SetEqualsFoo{}) {
		t.Error("SetEqualsFoo.Equals() test failed")
	}
	// test map field
	mapTgt, mapSrc := genMapFoo(), genMapFoo()
	if !mapTgt.Equals(mapSrc) {
		t.Error("MapEqualsFoo.Equals() test failed")
	}
	mapSrc.OptI64I64StringMapMapFoo[1][1] = "0"
	if mapTgt.Equals(mapSrc) {
		t.Error("MapEqualsFoo.Equals() test failed")
	}
	mapSrc = genMapFoo()
	mapSrc.OptI64I64StringMapMapFoo = nil
	if mapTgt.Equals(mapSrc) || mapSrc.Equals(mapTgt) {
		t.Error("MapEqualsFoo.Equals() test failed")
	}
	if !(&equalstest.MapEqualsFoo{}).Equals(&equalstest.MapEqualsFoo{}) {
		t.Error("MapEqualsFoo.Equals() test failed")
	}
}

func genBasicFoo() *equalstest.BasicEqualsFoo {
	return &equalstest.BasicEqualsFoo{
		BoolFoo:      true,
		OptBoolFoo:   &(&struct{ x bool }{true}).x,
		I8Foo:        1,
		OptI8Foo:     &(&struct{ x int8 }{1}).x,
		I16Foo:       2,
		OptI16Foo:    &(&struct{ x int16 }{2}).x,
		I32Foo:       3,
		OptI32Foo:    &(&struct{ x int32 }{3}).x,
		I64Foo:       4,
		OptI64Foo:    &(&struct{ x int64 }{4}).x,
		DoubleFoo:    5,
		OptDoubleFoo: &(&struct{ x float64 }{5}).x,
		StrFoo:       "6",
		OptStrFoo:    &(&struct{ x string }{"6"}).x,
		BinFoo:       []byte("7"),
		OptBinFoo:    []byte("7"),
		EnumFoo:      equalstest.EnumFoo_e1,
		OptEnumFoo:   equalstest.EnumFooPtr(equalstest.EnumFoo_e1),
		MyByteFoo:    equalstest.Mybyte(8),
		OptMyByteFoo: equalstest.MybytePtr(8),
		MyStrFoo:     equalstest.Mystr("9"),
		OptMyStrFoo:  equalstest.MystrPtr(equalstest.Mystr("9")),
		MyBinFoo:     equalstest.Mybin("10"),
		OptMyBinFoo:  equalstest.Mybin("10"),
	}
}

func genStructFoo() *equalstest.StructEqualsFoo {
	return &equalstest.StructEqualsFoo{
		StructFoo:    genBasicFoo(),
		OptStructFoo: genBasicFoo(),
	}
}

func genListFoo() *equalstest.ListEqualsFoo {
	return &equalstest.ListEqualsFoo{
		I64ListFoo:             genInt64Slice(6),
		OptI64ListFoo:          genInt64Slice(6),
		StrListFoo:             genStringSlice(6),
		OptStrListFoo:          genStringSlice(6),
		BinListFoo:             genBytesSlice(6),
		OptBinListFoo:          genBytesSlice(6),
		StructListFoo:          []*equalstest.BasicEqualsFoo{genBasicFoo(), {}},
		OptStructListFoo:       []*equalstest.BasicEqualsFoo{genBasicFoo(), {}},
		I64ListListFoo:         [][]int64{genInt64Slice(6), genInt64Slice(5), genInt64Slice(4), genInt64Slice(3), genInt64Slice(2), genInt64Slice(1)},
		OptI64ListListFoo:      [][]int64{genInt64Slice(6), genInt64Slice(5), genInt64Slice(4), genInt64Slice(3), genInt64Slice(2), genInt64Slice(1)},
		I64SetListFoo:          [][]int64{genInt64Slice(6), genInt64Slice(5), genInt64Slice(4), genInt64Slice(3), genInt64Slice(2), genInt64Slice(1)},
		OptI64SetListFoo:       [][]int64{genInt64Slice(6), genInt64Slice(5), genInt64Slice(4), genInt64Slice(3), genInt64Slice(2), genInt64Slice(1)},
		I64StringMapListFoo:    []map[int64]string{{6: "6"}, {5: "5"}, {4: "4"}, {3: "3"}, {2: "2"}, {1: "1"}},
		OptI64StringMapListFoo: []map[int64]string{{6: "6"}, {5: "5"}, {4: "4"}, {3: "3"}, {2: "2"}, {1: "1"}},
		MyByteListFoo:          []equalstest.Mybyte{6, 5, 4, 3, 2, 1},
		OptMyByteListFoo:       []equalstest.Mybyte{6, 5, 4, 3, 2, 1},
		MyStrListFoo:           []equalstest.Mystr{equalstest.Mystr("6"), equalstest.Mystr("5"), equalstest.Mystr("4"), equalstest.Mystr("3"), equalstest.Mystr("2"), equalstest.Mystr("1")},
		OptMyStrListFoo:        []equalstest.Mystr{equalstest.Mystr("6"), equalstest.Mystr("5"), equalstest.Mystr("4"), equalstest.Mystr("3"), equalstest.Mystr("2"), equalstest.Mystr("1")},
		MyBinListFoo:           []equalstest.Mybin{equalstest.Mybin("6"), equalstest.Mybin("5"), equalstest.Mybin("4"), equalstest.Mybin("3"), equalstest.Mybin("2"), equalstest.Mybin("1")},
		OptMyBinListFoo:        []equalstest.Mybin{equalstest.Mybin("6"), equalstest.Mybin("5"), equalstest.Mybin("4"), equalstest.Mybin("3"), equalstest.Mybin("2"), equalstest.Mybin("1")},
	}
}

func genSetFoo() *equalstest.SetEqualsFoo {
	return &equalstest.SetEqualsFoo{
		I64SetFoo:             genInt64Slice(6),
		OptI64SetFoo:          genInt64Slice(6),
		StrSetFoo:             genStringSlice(6),
		OptStrSetFoo:          genStringSlice(6),
		BinSetFoo:             genBytesSlice(6),
		OptBinSetFoo:          genBytesSlice(6),
		StructSetFoo:          []*equalstest.BasicEqualsFoo{genBasicFoo(), {}},
		OptStructSetFoo:       []*equalstest.BasicEqualsFoo{genBasicFoo(), {}},
		I64ListSetFoo:         [][]int64{genInt64Slice(6), genInt64Slice(5), genInt64Slice(4), genInt64Slice(3), genInt64Slice(2), genInt64Slice(1)},
		OptI64ListSetFoo:      [][]int64{genInt64Slice(6), genInt64Slice(5), genInt64Slice(4), genInt64Slice(3), genInt64Slice(2), genInt64Slice(1)},
		I64SetSetFoo:          [][]int64{genInt64Slice(6), genInt64Slice(5), genInt64Slice(4), genInt64Slice(3), genInt64Slice(2), genInt64Slice(1)},
		OptI64SetSetFoo:       [][]int64{genInt64Slice(6), genInt64Slice(5), genInt64Slice(4), genInt64Slice(3), genInt64Slice(2), genInt64Slice(1)},
		I64StringMapSetFoo:    []map[int64]string{{6: "6"}, {5: "5"}, {4: "4"}, {3: "3"}, {2: "2"}, {1: "1"}},
		OptI64StringMapSetFoo: []map[int64]string{{6: "6"}, {5: "5"}, {4: "4"}, {3: "3"}, {2: "2"}, {1: "1"}},
		MyByteSetFoo:          []equalstest.Mybyte{6, 5, 4, 3, 2, 1},
		OptMyByteSetFoo:       []equalstest.Mybyte{6, 5, 4, 3, 2, 1},
		MyStrSetFoo:           []equalstest.Mystr{equalstest.Mystr("6"), equalstest.Mystr("5"), equalstest.Mystr("4"), equalstest.Mystr("3"), equalstest.Mystr("2"), equalstest.Mystr("1")},
		OptMyStrSetFoo:        []equalstest.Mystr{equalstest.Mystr("6"), equalstest.Mystr("5"), equalstest.Mystr("4"), equalstest.Mystr("3"), equalstest.Mystr("2"), equalstest.Mystr("1")},
		MyBinSetFoo:           []equalstest.Mybin{equalstest.Mybin("6"), equalstest.Mybin("5"), equalstest.Mybin("4"), equalstest.Mybin("3"), equalstest.Mybin("2"), equalstest.Mybin("1")},
		OptMyBinSetFoo:        []equalstest.Mybin{equalstest.Mybin("6"), equalstest.Mybin("5"), equalstest.Mybin("4"), equalstest.Mybin("3"), equalstest.Mybin("2"), equalstest.Mybin("1")},
	}
}

var (
	structMapKey0 = genBasicFoo()
	structMapKey1 = &equalstest.BasicEqualsFoo{}
)

func genMapFoo() *equalstest.MapEqualsFoo {
	return &equalstest.MapEqualsFoo{
		I64StrMapFoo:             genInt64StringMap(6),
		OptI64StrMapFoo:          genInt64StringMap(6),
		StrI64MapFoo:             map[string]int64{"6": 6, "5": 5, "4": 4, "3": 3, "2": 2, "1": 1},
		OptStrI64MapFoo:          map[string]int64{"6": 6, "5": 5, "4": 4, "3": 3, "2": 2, "1": 1},
		StructBinMapFoo:          map[*equalstest.BasicEqualsFoo][]byte{structMapKey0: []byte("0"), structMapKey1: []byte("1")},
		OptStructBinMapFoo:       map[*equalstest.BasicEqualsFoo][]byte{structMapKey0: []byte("0"), structMapKey1: []byte("1")},
		BinStructMapFoo:          map[string]*equalstest.BasicEqualsFoo{"1": genBasicFoo(), "0": {}},
		OptBinStructMapFoo:       map[string]*equalstest.BasicEqualsFoo{"1": genBasicFoo(), "0": {}},
		I64I64ListMapFoo:         map[int64][]int64{6: genInt64Slice(6), 5: genInt64Slice(5), 4: genInt64Slice(4), 3: genInt64Slice(3), 2: genInt64Slice(2), 1: genInt64Slice(1)},
		OptI64I64ListMapFoo:      map[int64][]int64{6: genInt64Slice(6), 5: genInt64Slice(5), 4: genInt64Slice(4), 3: genInt64Slice(3), 2: genInt64Slice(2), 1: genInt64Slice(1)},
		I64I64SetMapFoo:          map[int64][]int64{6: genInt64Slice(6), 5: genInt64Slice(5), 4: genInt64Slice(4), 3: genInt64Slice(3), 2: genInt64Slice(2), 1: genInt64Slice(1)},
		OptI64I64SetMapFoo:       map[int64][]int64{6: genInt64Slice(6), 5: genInt64Slice(5), 4: genInt64Slice(4), 3: genInt64Slice(3), 2: genInt64Slice(2), 1: genInt64Slice(1)},
		I64I64StringMapMapFoo:    map[int64]map[int64]string{6: genInt64StringMap(6), 5: genInt64StringMap(5), 4: genInt64StringMap(4), 3: genInt64StringMap(3), 2: genInt64StringMap(2), 1: genInt64StringMap(1)},
		OptI64I64StringMapMapFoo: map[int64]map[int64]string{6: genInt64StringMap(6), 5: genInt64StringMap(5), 4: genInt64StringMap(4), 3: genInt64StringMap(3), 2: genInt64StringMap(2), 1: genInt64StringMap(1)},
		MyStrMyBinMapFoo:         map[equalstest.Mystr]equalstest.Mybin{equalstest.Mystr("1"): equalstest.Mybin("1"), equalstest.Mystr("0"): equalstest.Mybin("0")},
		OptMyStrMyBinMapFoo:      map[equalstest.Mystr]equalstest.Mybin{equalstest.Mystr("1"): equalstest.Mybin("1"), equalstest.Mystr("0"): equalstest.Mybin("0")},
		Int64MyByteMapFoo:        map[int64]equalstest.Mybyte{6: equalstest.Mybyte(6), 5: equalstest.Mybyte(5), 4: equalstest.Mybyte(4), 3: equalstest.Mybyte(3), 2: equalstest.Mybyte(2), 1: equalstest.Mybyte(1)},
		OptInt64MyByteMapFoo:     map[int64]equalstest.Mybyte{6: equalstest.Mybyte(6), 5: equalstest.Mybyte(5), 4: equalstest.Mybyte(4), 3: equalstest.Mybyte(3), 2: equalstest.Mybyte(2), 1: equalstest.Mybyte(1)},
		MyByteInt64MapFoo:        map[equalstest.Mybyte]int64{equalstest.Mybyte(6): 6, equalstest.Mybyte(5): 5, equalstest.Mybyte(4): 4, equalstest.Mybyte(3): 3, equalstest.Mybyte(2): 2, equalstest.Mybyte(1): 1},
		OptMyByteInt64MapFoo:     map[equalstest.Mybyte]int64{equalstest.Mybyte(6): 6, equalstest.Mybyte(5): 5, equalstest.Mybyte(4): 4, equalstest.Mybyte(3): 3, equalstest.Mybyte(2): 2, equalstest.Mybyte(1): 1},
	}
}

func genInt64Slice(length int) []int64 {
	ret := make([]int64, length)
	for i := 0; i < length; i++ {
		ret[i] = int64(length - i)
	}
	return ret
}

func genStringSlice(length int) []string {
	ret := make([]string, length)
	for i := 0; i < length; i++ {
		ret[i] = strconv.Itoa(length - i)
	}
	return ret
}

func genBytesSlice(length int) [][]byte {
	ret := make([][]byte, length)
	for i := 0; i < length; i++ {
		ret[i] = []byte(strconv.Itoa(length - i))
	}
	return ret
}

func genInt64StringMap(length int) map[int64]string {
	ret := make(map[int64]string, length)
	for i := 0; i < length; i++ {
		ret[int64(i)] = strconv.Itoa(i)
	}
	return ret
}
