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

package main

import (
	"common"
	"flag"
	"gen/thrifttest"
	t "log"
	"reflect"
	"thrift"
)

var host = flag.String("host", "localhost", "Host to connect")
var port = flag.Int64("port", 9090, "Port number to connect")
var domain_socket = flag.String("domain-socket", "", "Domain Socket (e.g. /tmp/thrifttest.thrift), instead of host and port")
var transport = flag.String("transport", "buffered", "Transport: buffered, framed, http")
var protocol = flag.String("protocol", "binary", "Protocol: binary, compact, json")
var ssl = flag.Bool("ssl", false, "Encrypted Transport using SSL")
var testloops = flag.Int("testloops", 1, "Number of Tests")

func main() {
	flag.Parse()
	client, err := common.StartClient(*host, *port, *domain_socket, *transport, *protocol, *ssl)
	if err != nil {
		t.Fatalf("Unable to start client: ", err)
	}
	for i := 0; i < *testloops; i++ {
		callEverything(client)
	}
}

var rmapmap = map[int32]map[int32]int32{
	-4: map[int32]int32{-4: -4, -3: -3, -2: -2, -1: -1},
	4:  map[int32]int32{4: 4, 3: 3, 2: 2, 1: 1},
}

var xxs = &thrifttest.Xtruct{
	StringThing: "Hello2",
	ByteThing:   42,
	I32Thing:    4242,
	I64Thing:    424242,
}

var xcept = &thrifttest.Xception{ErrorCode: 1001, Message: "Xception"}

func callEverything(client *thrifttest.ThriftTestClient) {
	var err error
	if err = client.TestVoid(); err != nil {
		t.Fatalf("Unexpected error in TestVoid() call: ", err)
	}

	thing, err := client.TestString("thing")
	if err != nil {
		t.Fatalf("Unexpected error in TestString() call: ", err)
	}
	if thing != "thing" {
		t.Fatalf("Unexpected TestString() result, expected 'thing' got '%s' ", thing)
	}

	b, err := client.TestByte(42)
	if err != nil {
		t.Fatalf("Unexpected error in TestByte() call: ", err)
	}
	if b != 42 {
		t.Fatalf("Unexpected TestByte() result expected 42, got %d ", b)
	}

	i32, err := client.TestI32(4242)
	if err != nil {
		t.Fatalf("Unexpected error in TestI32() call: ", err)
	}
	if i32 != 4242 {
		t.Fatalf("Unexpected TestI32() result expected 4242, got %d ", i32)
	}

	i64, err := client.TestI64(424242)
	if err != nil {
		t.Fatalf("Unexpected error in TestI64() call: ", err)
	}
	if i64 != 424242 {
		t.Fatalf("Unexpected TestI64() result expected 424242, got %d ", i64)
	}

	d, err := client.TestDouble(42.42)
	if err != nil {
		t.Fatalf("Unexpected error in TestDouble() call: ", err)
	}
	if d != 42.42 {
		t.Fatalf("Unexpected TestDouble() result expected 42.42, got %f ", d)
	}

	xs := thrifttest.NewXtruct()
	xs.StringThing = "thing"
	xs.ByteThing = 42
	xs.I32Thing = 4242
	xs.I64Thing = 424242
	xsret, err := client.TestStruct(xs)
	if err != nil {
		t.Fatalf("Unexpected error in TestStruct() call: ", err)
	}
	if *xs != *xsret {
		t.Fatalf("Unexpected TestStruct() result expected %#v, got %#v ", xs, xsret)
	}

	x2 := thrifttest.NewXtruct2()
	x2.StructThing = xs
	x2ret, err := client.TestNest(x2)
	if err != nil {
		t.Fatalf("Unexpected error in TestNest() call: ", err)
	}
	if !reflect.DeepEqual(x2, x2ret) {
		t.Fatalf("Unexpected TestNest() result expected %#v, got %#v ", x2, x2ret)
	}

	m := map[int32]int32{1: 2, 3: 4, 5: 42}
	mret, err := client.TestMap(m)
	if err != nil {
		t.Fatalf("Unexpected error in TestMap() call: ", err)
	}
	if !reflect.DeepEqual(m, mret) {
		t.Fatalf("Unexpected TestMap() result expected %#v, got %#v ", m, mret)
	}

	sm := map[string]string{"a": "2", "b": "blah", "some": "thing"}
	smret, err := client.TestStringMap(sm)
	if err != nil {
		t.Fatalf("Unexpected error in TestStringMap() call: ", err)
	}
	if !reflect.DeepEqual(sm, smret) {
		t.Fatalf("Unexpected TestStringMap() result expected %#v, got %#v ", sm, smret)
	}

	s := map[int32]bool{1: true, 2: true, 42: true}
	sret, err := client.TestSet(s)
	if err != nil {
		t.Fatalf("Unexpected error in TestSet() call: ", err)
	}
	if !reflect.DeepEqual(s, sret) {
		t.Fatalf("Unexpected TestSet() result expected %#v, got %#v ", s, sret)
	}

	l := []int32{1, 2, 42}
	lret, err := client.TestList(l)
	if err != nil {
		t.Fatalf("Unexpected error in TestList() call: ", err)
	}
	if !reflect.DeepEqual(l, lret) {
		t.Fatalf("Unexpected TestSet() result expected %#v, got %#v ", l, lret)
	}

	eret, err := client.TestEnum(thrifttest.Numberz_TWO)
	if err != nil {
		t.Fatalf("Unexpected error in TestEnum() call: ", err)
	}
	if eret != thrifttest.Numberz_TWO {
		t.Fatalf("Unexpected TestEnum() result expected %#v, got %#v ", thrifttest.Numberz_TWO, eret)
	}

	tret, err := client.TestTypedef(thrifttest.UserId(42))
	if err != nil {
		t.Fatalf("Unexpected error in TestTypedef() call: ", err)
	}
	if tret != thrifttest.UserId(42) {
		t.Fatalf("Unexpected TestTypedef() result expected %#v, got %#v ", thrifttest.UserId(42), tret)
	}

	mapmap, err := client.TestMapMap(42)
	if err != nil {
		t.Fatalf("Unexpected error in TestMapmap() call: ", err)
	}
	if !reflect.DeepEqual(mapmap, rmapmap) {
		t.Fatalf("Unexpected TestMapmap() result expected %#v, got %#v ", rmapmap, mapmap)
	}

	xxsret, err := client.TestMulti(42, 4242, 424242, map[int16]string{1: "blah", 2: "thing"}, thrifttest.Numberz_EIGHT, thrifttest.UserId(24))
	if err != nil {
		t.Fatalf("Unexpected error in TestMulti() call: ", err)
	}
	if !reflect.DeepEqual(xxs, xxsret) {
		t.Fatalf("Unexpected TestMulti() result expected %#v, got %#v ", xxs, xxsret)
	}

	err = client.TestException("Xception")
	if err == nil {
		t.Fatalf("Expecting exception in TestException() call")
	}
	if !reflect.DeepEqual(err, xcept) {
		t.Fatalf("Unexpected TestException() result expected %#v, got %#v ", xcept, err)
	}

	// TODO: connection is being closed on this
	err = client.TestException("TException")
	tex, ok := err.(thrift.TApplicationException)
	if err == nil || !ok || tex.TypeId() != thrift.INTERNAL_ERROR {
		t.Fatalf("Unexpected TestException() result expected ApplicationError, got %#v ", err)
	}

	ign, err := client.TestMultiException("Xception", "ignoreme")
	if ign != nil || err == nil {
		t.Fatalf("Expecting exception in TestMultiException() call")
	}
	if !reflect.DeepEqual(err, &thrifttest.Xception{ErrorCode: 1001, Message: "This is an Xception"}) {
		t.Fatalf("Unexpected TestMultiException() %#v ", err)
	}

	ign, err = client.TestMultiException("Xception2", "ignoreme")
	if ign != nil || err == nil {
		t.Fatalf("Expecting exception in TestMultiException() call")
	}
	expecting := &thrifttest.Xception2{ErrorCode: 2002, StructThing: &thrifttest.Xtruct{StringThing: "This is an Xception2"}}

	if !reflect.DeepEqual(err, expecting) {
		t.Fatalf("Unexpected TestMultiException() %#v ", err)
	}

	err = client.TestOneway(2)
	if err != nil {
		t.Fatalf("Unexpected error in TestOneway() call: ", err)
	}

	//Make sure the connection still alive
	if err = client.TestVoid(); err != nil {
		t.Fatalf("Unexpected error in TestVoid() call: ", err)
	}
}
