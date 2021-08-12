// +build !windows

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
	"io"
	"net"
	"testing"
	"time"
)

func TestSocketConnUnix(t *testing.T) {
	const (
		interval = time.Millisecond * 10
		first    = "hello"
		second   = "world"
	)

	ln, err := serverSocketConn(
		t,
		func(tb testing.TB, sc *socketConn) {
			defer sc.Close()

			time.Sleep(interval)
			if !writeFully(tb, sc, first) {
				return
			}
			time.Sleep(interval)
			writeFully(tb, sc, second)
		},
	)
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()

	sc, err := createSocketConnFromReturn(net.Dial("tcp", ln.Addr().String()))
	if err != nil {
		t.Fatal(err)
	}
	buf := make([]byte, 1024)

	if !sc.IsOpen() {
		t.Error("Expected sc to report open, got false")
	}
	n, err := sc.Read(buf)
	if err != nil {
		t.Fatal(err)
	}
	read := string(buf[:n])
	if read != first {
		t.Errorf("Expected read %q, got %q", first, read)
	}

	if !sc.IsOpen() {
		t.Error("Expected sc to report open, got false")
	}
	// Do connection check again twice after server already wrote new data,
	// make sure we don't cause any data loss with the check.
	time.Sleep(interval * 10)
	if !sc.IsOpen() {
		t.Error("Expected sc to report open, got false")
	}
	if !sc.IsOpen() {
		t.Error("Expected sc to report open, got false")
	}
	n, err = sc.Read(buf)
	if err != nil {
		t.Fatal(err)
	}
	read = string(buf[:n])
	if read != second {
		t.Errorf("Expected read %q, got %q", second, read)
	}

	// Now it's supposed to be closed on the server side
	if err := sc.read0(); err != io.EOF {
		t.Errorf("Expected to get EOF on read0, got %v", err)
	}
	if sc.IsOpen() {
		t.Error("Expected sc to report not open, got true")
	}
}
