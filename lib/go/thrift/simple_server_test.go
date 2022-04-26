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
	"context"
	"errors"
	"net"
	"runtime"
	"sync"
	"testing"
	"time"
)

const networkWaitDuration = 10 * time.Millisecond

type mockServerTransport struct {
	ListenFunc    func() error
	AcceptFunc    func() (TTransport, error)
	CloseFunc     func() error
	InterruptFunc func() error
}

func (m *mockServerTransport) Listen() error {
	return m.ListenFunc()
}

func (m *mockServerTransport) Accept() (TTransport, error) {
	return m.AcceptFunc()
}

func (m *mockServerTransport) Close() error {
	return m.CloseFunc()
}

func (m *mockServerTransport) Interrupt() error {
	return m.InterruptFunc()
}

type mockTTransport struct {
	TTransport
}

func (m *mockTTransport) Close() error {
	return nil
}

func TestMultipleStop(t *testing.T) {
	proc := &mockProcessor{
		ProcessFunc: func(in, out TProtocol) (bool, TException) {
			return false, nil
		},
	}

	var interruptCalled bool
	c := make(chan struct{})
	trans := &mockServerTransport{
		ListenFunc: func() error {
			return nil
		},
		AcceptFunc: func() (TTransport, error) {
			<-c
			return nil, nil
		},
		CloseFunc: func() error {
			c <- struct{}{}
			return nil
		},
		InterruptFunc: func() error {
			interruptCalled = true
			return nil
		},
	}

	serv := NewTSimpleServer2(proc, trans)
	go serv.Serve()
	serv.Stop()
	if !interruptCalled {
		t.Error("first server transport should have been interrupted")
	}

	serv = NewTSimpleServer2(proc, trans)
	interruptCalled = false
	go serv.Serve()
	serv.Stop()
	if !interruptCalled {
		t.Error("second server transport should have been interrupted")
	}
}

func TestWaitRace(t *testing.T) {
	proc := &mockProcessor{
		ProcessFunc: func(in, out TProtocol) (bool, TException) {
			return false, nil
		},
	}

	trans := &mockServerTransport{
		ListenFunc: func() error {
			return nil
		},
		AcceptFunc: func() (TTransport, error) {
			return &mockTTransport{}, nil
		},
		CloseFunc: func() error {
			return nil
		},
		InterruptFunc: func() error {
			return nil
		},
	}

	serv := NewTSimpleServer2(proc, trans)
	go serv.Serve()
	runtime.Gosched()
	serv.Stop()
}

func TestNoHangDuringStopFromDanglingLockAcquireDuringAcceptLoop(t *testing.T) {
	proc := &mockProcessor{
		ProcessFunc: func(in, out TProtocol) (bool, TException) {
			return false, nil
		},
	}

	trans := &mockServerTransport{
		ListenFunc: func() error {
			return nil
		},
		AcceptFunc: func() (TTransport, error) {
			return nil, errors.New("no sir")
		},
		CloseFunc: func() error {
			return nil
		},
		InterruptFunc: func() error {
			return nil
		},
	}

	serv := NewTSimpleServer2(proc, trans)
	go serv.Serve()
	runtime.Gosched()
	serv.Stop()
}

func TestNoHangDuringStopFromClientNoDataSendDuringAcceptLoop(t *testing.T) {
	ln, err := net.Listen("tcp", "localhost:0")

	if err != nil {
		t.Fatalf("Failed to listen: %v", err)
	}

	proc := &mockProcessor{
		ProcessFunc: func(in, out TProtocol) (bool, TException) {
			in.ReadMessageBegin(context.Background())
			return false, nil
		},
	}

	trans := &mockServerTransport{
		ListenFunc: func() error {
			return nil
		},
		AcceptFunc: func() (TTransport, error) {
			conn, err := ln.Accept()
			if err != nil {
				return nil, err
			}

			return NewTSocketFromConnConf(conn, nil), nil
		},
		CloseFunc: func() error {
			return nil
		},
		InterruptFunc: func() error {
			return ln.Close()
		},
	}

	serv := NewTSimpleServer2(proc, trans)
	go serv.Serve()
	time.Sleep(networkWaitDuration)

	netConn, err := net.Dial("tcp", ln.Addr().String())
	if err != nil || netConn == nil {
		t.Fatal("error when dial server")
	}
	time.Sleep(networkWaitDuration)

	serverStopTimeout := 50 * time.Millisecond
	backupServerStopTimeout := ServerStopTimeout
	t.Cleanup(func() {
		ServerStopTimeout = backupServerStopTimeout
	})
	ServerStopTimeout = serverStopTimeout

	st := time.Now()
	err = serv.Stop()
	if err != nil {
		t.Errorf("error when stop server:%v", err)
	}

	if elapsed := time.Since(st); elapsed < serverStopTimeout {
		t.Errorf("stop cost less time than server stop timeout, server stop timeout:%v,cost time:%v", ServerStopTimeout, elapsed)
	}
}

func TestStopTimeoutWithSocketTimeout(t *testing.T) {
	ln, err := net.Listen("tcp", "localhost:0")

	if err != nil {
		t.Fatalf("Failed to listen: %v", err)
	}

	proc := &mockProcessor{
		ProcessFunc: func(in, out TProtocol) (bool, TException) {
			in.ReadMessageBegin(context.Background())
			return false, nil
		},
	}

	conf := &TConfiguration{SocketTimeout: 5 * time.Millisecond}
	wg := &sync.WaitGroup{}
	trans := &mockServerTransport{
		ListenFunc: func() error {
			return nil
		},
		AcceptFunc: func() (TTransport, error) {
			conn, err := ln.Accept()
			if err != nil {
				return nil, err
			}
			defer wg.Done()
			return NewTSocketFromConnConf(conn, conf), nil
		},
		CloseFunc: func() error {
			return nil
		},
		InterruptFunc: func() error {
			return ln.Close()
		},
	}

	serv := NewTSimpleServer2(proc, trans)
	go serv.Serve()
	time.Sleep(networkWaitDuration)

	wg.Add(1)
	netConn, err := net.Dial("tcp", ln.Addr().String())
	if err != nil || netConn == nil {
		t.Fatal("error when dial server")
	}
	wg.Wait()

	expectedStopTimeout := time.Second
	backupServerStopTimeout := ServerStopTimeout
	t.Cleanup(func() {
		ServerStopTimeout = backupServerStopTimeout
	})
	ServerStopTimeout = expectedStopTimeout

	st := time.Now()
	err = serv.Stop()
	if elapsed := time.Since(st); elapsed > expectedStopTimeout/2 {
		t.Errorf("stop cost more time than socket timeout, socket timeout:%v,server stop timeout:%v,cost time:%v", conf.SocketTimeout, ServerStopTimeout, elapsed)
	}

	if err != nil {
		t.Fatalf("error when stop server:%v", err)
	}
}
