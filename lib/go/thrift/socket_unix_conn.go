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
	"syscall"
)

func (sc *socketConn) read0() error {
	return sc.checkConn()
}

func (sc *socketConn) checkConn() error {
	syscallConn, ok := sc.Conn.(syscall.Conn)
	if !ok {
		// No way to check, return nil
		return nil
	}
	rc, err := syscallConn.SyscallConn()
	if err != nil {
		return err
	}

	var n int
	var buf [1]byte

	if readErr := rc.Read(func(fd uintptr) bool {
		n, err = syscall.Read(int(fd), buf[:])
		return true
	}); readErr != nil {
		return readErr
	}

	if err == syscall.EAGAIN || err == syscall.EWOULDBLOCK {
		// This means the connection is still open but we don't have
		// anything to read right now.
		return nil
	}

	if n > 0 {
		// We got 1 byte,
		// put it to sc's buf for the next real read to use.
		sc.buf.Write(buf[:])
		return nil
	}

	if err != nil {
		return err
	}

	// At this point, it means the other side already closed the connection.
	return io.EOF
}
