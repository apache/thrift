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
	"errors"
	"io"
	"net"
	"sync/atomic"
	"testing"
	"time"

	"github.com/apache/thrift/lib/go/test/gopath/src/servicestest"
	"github.com/apache/thrift/lib/go/thrift"
)

type zlibTestHandler struct {
	servicestest.AServ

	tb   testing.TB
	text string
}

func (z zlibTestHandler) Stringfunc_1int_1s(ctx context.Context, i int64, s string) (string, error) {
	if s != z.text {
		z.tb.Errorf("string arg got %q want %q", s, z.text)
	}
	return z.text, nil
}

type countingProxy struct {
	// Need to fill when constructing
	tb         testing.TB
	remoteAddr net.Addr

	// internal states
	listener      net.Listener
	clientWritten atomic.Int64
	serverWritten atomic.Int64
}

func (cp *countingProxy) getAndResetCounters() (req, resp int64) {
	req = cp.clientWritten.Swap(0)
	resp = cp.serverWritten.Swap(0)
	return req, resp
}

func (cp *countingProxy) serve() {
	cp.tb.Helper()

	listener, err := net.Listen("tcp", ":0")
	if err != nil {
		cp.tb.Fatalf("Failed to listen proxy: %v", err)
	}
	go func() {
		for {
			client, err := listener.Accept()
			if err != nil {
				if !errors.Is(err, io.EOF) && !errors.Is(err, net.ErrClosed) {
					cp.tb.Errorf("proxy accept error: %v", err)
				}
				return
			}
			server, err := net.Dial(cp.remoteAddr.Network(), cp.remoteAddr.String())
			if err != nil {
				cp.tb.Logf("proxy failed to dial server %v: %v", cp.remoteAddr, err)
			}
			proxy := func(read, write net.Conn, count *atomic.Int64) {
				var buf [1024]byte
				for {
					n, err := read.Read(buf[:])
					if n > 0 {
						count.Add(int64(n))
						if _, err := write.Write(buf[:n]); err != nil {
							cp.tb.Errorf("proxy write error: %v", err)
						}
					}
					if err != nil {
						if !errors.Is(err, io.EOF) && !errors.Is(err, net.ErrClosed) {
							cp.tb.Errorf("proxy read error: %v", err)
						}
						read.Close()
						write.Close()
						return
					}
				}
			}
			// Read from client
			go proxy(client, server, &cp.clientWritten)
			// Read from server
			go proxy(server, client, &cp.serverWritten)
		}
	}()
	cp.listener = listener
}

func TestTHeaderZlibClient(t *testing.T) {
	// Some text that zlib should be able to compress
	const text = `Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.`

	socket, err := thrift.NewTServerSocket(":0")
	if err != nil {
		t.Fatalf("Failed to create server socket: %v", err)
	}
	// Call listen to reserve a port and check for any issues early
	if err := socket.Listen(); err != nil {
		t.Fatalf("Failed to listen server socket: %v", err)
	}
	server := thrift.NewTSimpleServer4(
		servicestest.NewAServProcessor(zlibTestHandler{
			tb:   t,
			text: text,
		}),
		socket,
		thrift.NewTHeaderTransportFactoryConf(nil, nil),
		thrift.NewTHeaderProtocolFactoryConf(nil),
	)
	go server.Serve()
	// give the server a little time to start serving
	time.Sleep(10 * time.Millisecond)
	t.Cleanup(func() {
		server.Stop()
	})
	t.Logf("server running on %v", socket.Addr())

	proxy := countingProxy{
		tb:         t,
		remoteAddr: socket.Addr(),
	}
	proxy.serve()
	t.Cleanup(func() {
		proxy.listener.Close()
	})
	t.Logf("proxy running on %v", proxy.listener.Addr())

	clientRoundtrip := func(cfg *thrift.TConfiguration) {
		t.Helper()

		socket := thrift.NewTSocketConf(proxy.listener.Addr().String(), cfg)
		if err := socket.Open(); err != nil {
			t.Errorf("failed to open socket: %v", err)
			return
		}
		defer socket.Close()
		protoFactory := thrift.NewTHeaderProtocolFactoryConf(cfg)
		client := thrift.NewTStandardClient(
			protoFactory.GetProtocol(socket),
			protoFactory.GetProtocol(socket),
		)
		c := servicestest.NewAServClient(client)
		got, err := c.Stringfunc_1int_1s(context.Background(), 0, text)
		if err != nil {
			t.Errorf("Stringfunc_1int_1s call failed: %v", err)
			return
		}
		if got != text {
			t.Errorf("Stringfunc_1int_1s got %q want %q", got, text)
		}
	}

	clientRoundtrip(nil)
	nozlibReq, nozlibResp := proxy.getAndResetCounters()
	t.Logf("nozlib request size: %d, response size: %d", nozlibReq, nozlibResp)

	clientRoundtrip(&thrift.TConfiguration{
		THeaderTransforms: []thrift.THeaderTransformID{thrift.TransformZlib},
	})
	zlibReq, zlibResp := proxy.getAndResetCounters()
	t.Logf("zlib request size: %d, response size: %d", zlibReq, zlibResp)

	if zlibReq >= nozlibReq {
		t.Errorf("zlib request size %d >= nozlib request size %d", zlibReq, nozlibReq)
	}
	if zlibResp >= nozlibResp {
		t.Errorf("zlib response size %d >= nozlib response size %d", zlibResp, nozlibResp)
	}

	clientRoundtrip(nil)
	nozlibReq2, nozlibResp2 := proxy.getAndResetCounters()
	t.Logf("nozlib2 request size: %d, response size: %d", nozlibReq, nozlibResp)

	if nozlibReq2 != nozlibReq {
		t.Errorf("nozlib request 2 size %d != nozlib request size %d", nozlibReq2, nozlibReq)
	}
	if nozlibResp2 != nozlibResp {
		t.Errorf("nozlib response 2 size %d != nozlib response size %d", nozlibResp2, nozlibResp)
	}
}
