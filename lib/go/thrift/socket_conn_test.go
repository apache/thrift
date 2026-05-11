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
	"errors"
	"io"
	"net"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"
)

type serverSocketConnCallback func(testing.TB, *socketConn)

func serverSocketConn(tb testing.TB, f serverSocketConnCallback) (net.Listener, error) {
	tb.Helper()

	ln, err := net.Listen("tcp", "localhost:0")
	if err != nil {
		return nil, err
	}
	go func() {
		for {
			sc, err := createSocketConnFromReturn(ln.Accept())
			if err != nil {
				// This is usually caused by Listener being
				// closed, not really an error.
				return
			}
			go f(tb, sc)
		}
	}()
	return ln, nil
}

func writeFully(tb testing.TB, w io.Writer, s string) bool {
	tb.Helper()

	n, err := io.Copy(w, strings.NewReader(s))
	if err != nil {
		tb.Errorf("Failed to write %q: %v", s, err)
		return false
	}
	if int(n) < len(s) {
		tb.Errorf("Only wrote %d out of %q", n, s)
		return false
	}
	return true
}

func TestSocketConn(t *testing.T) {
	const (
		interval = time.Millisecond * 10
		first    = "hello"
		second   = "world"
	)

	ln, err := serverSocketConn(
		t,
		func(tb testing.TB, sc *socketConn) {
			defer sc.Close()

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

	n, err := sc.Read(buf)
	if err != nil {
		t.Fatal(err)
	}
	read := string(buf[:n])
	if read != first {
		t.Errorf("Expected read %q, got %q", first, read)
	}

	n, err = sc.Read(buf)
	if err != nil {
		t.Fatal(err)
	}
	read = string(buf[:n])
	if read != second {
		t.Errorf("Expected read %q, got %q", second, read)
	}
}

func TestSocketConnNilSafe(t *testing.T) {
	sc := (*socketConn)(nil)
	if sc.isValid() {
		t.Error("Expected false for nil.isValid(), got true")
	}
	if sc.IsOpen() {
		t.Error("Expected false for nil.IsOpen(), got true")
	}
}

func TestSocketConnClose(t *testing.T) {
	t.Run("concurrent calls", func(t *testing.T) {
		c := &mockCloseCounterConn{}
		sc := &socketConn{
			Conn: c,
		}

		var g sync.WaitGroup
		for range 4 {
			g.Go(func() { sc.Close() })
		}
		g.Wait()

		cnt := c.closeCounter.Load()
		if cnt != 1 {
			t.Errorf("Expected conn.Close() to be called once, got %d calls", cnt)
		}
	})

	t.Run("socketConn nilSafe", func(t *testing.T) {
		sc := (*socketConn)(nil)

		err := sc.Close()

		if !errors.Is(err, net.ErrClosed) {
			t.Errorf("Expected %v, got %v ", net.ErrClosed, err)
		}
	})

	t.Run("netConn nilSafe", func(t *testing.T) {
		sc := &socketConn{
			Conn: nil,
		}

		err := sc.Close()

		if !errors.Is(err, net.ErrClosed) {
			t.Errorf("Expected %v, got %v ", net.ErrClosed, err)
		}
	})
}

type mockCloseCounterConn struct {
	closeCounter atomic.Int32
}

func (m *mockCloseCounterConn) Read(b []byte) (n int, err error) {
	return 0, nil
}

func (m *mockCloseCounterConn) Write(b []byte) (n int, err error) {
	return 0, nil
}

func (m *mockCloseCounterConn) Close() error {
	m.closeCounter.Add(1)
	return nil
}

func (m *mockCloseCounterConn) LocalAddr() net.Addr {
	return nil
}

func (m *mockCloseCounterConn) RemoteAddr() net.Addr {
	return nil
}

func (m *mockCloseCounterConn) SetDeadline(t time.Time) error {
	return nil
}

func (m *mockCloseCounterConn) SetReadDeadline(t time.Time) error {
	return nil
}

func (m *mockCloseCounterConn) SetWriteDeadline(t time.Time) error {
	return nil
}
