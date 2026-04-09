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
	"net"
	"sync"
	"time"
)

type TServerSocket struct {
	// TServerSocketListenerFactory abstracts how listeners are created.
	factory       func(net.Addr) (net.Listener, error)
	addr          net.Addr
	clientTimeout time.Duration

	mu          sync.RWMutex
	listener    net.Listener
	interrupted bool
}

// --- Constructors ---

func NewTServerSocket(listenAddr string) (*TServerSocket, error) {
	return NewTServerSocketTimeout(listenAddr, 0)
}

func NewTServerSocketTimeout(listenAddr string, clientTimeout time.Duration) (*TServerSocket, error) {
	addr, err := net.ResolveTCPAddr("tcp", listenAddr)
	if err != nil {
		return nil, err
	}

	return NewTServerSocketFromAddrTimeout(addr, clientTimeout), nil
}

func NewTServerSocketFromAddrTimeout(addr net.Addr, clientTimeout time.Duration) *TServerSocket {
	factory := func(addr net.Addr) (net.Listener, error) {
		return net.Listen(addr.Network(), addr.String())
	}

	return NewTServerSocketFromFactoryTimeout(factory, addr, clientTimeout)
}

// Allows full customization (TLS, mocks, unix sockets, windows named pipes, etc.)
func NewTServerSocketFromFactoryTimeout(factory func(addr net.Addr) (listener net.Listener, err error), addr net.Addr, clientTimeout time.Duration) *TServerSocket {
	return &TServerSocket{
		factory:       factory,
		addr:          addr,
		clientTimeout: clientTimeout,
	}
}

// --- Core methods ---

func (p *TServerSocket) try_listen(raise bool) error {
	p.mu.Lock()
	defer p.mu.Unlock()

	if p.listener != nil {
		if (raise) {
		        return NewTTransportException(ALREADY_OPEN, "Server socket already open")
		}
		return nil
	}

	_, l, err := p.factory(true)
	if err != nil {
		return err
	}

	p.listener = l
	p.interrupted = false
	return nil
}

func (p *TServerSocket) Open() error {
	return p.try_listen(true /* raise error if listening */)
}

func (p *TServerSocket) Listen() error {
	return p.try_listen(false /* do not raise error if listening */)
}

func (p *TServerSocket) Accept() (TTransport, error) {
	p.mu.RLock()
	interrupted := p.interrupted
	listener := p.listener
	p.mu.RUnlock()

	if interrupted {
		return nil, errTransportInterrupted
	}

	if listener == nil {
		return nil, NewTTransportException(NOT_OPEN, "No underlying server socket")
	}

	conn, err := listener.Accept()
	if err != nil {
		return nil, NewTTransportExceptionFromError(err)
	}
	return NewTSocketFromConnTimeout(conn, p.clientTimeout), nil
}

// --- State helpers ---

func (p *TServerSocket) IsListening() bool {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return p.listener != nil
}

func (p *TServerSocket) Addr() net.Addr {
	p.mu.RLock()
	defer p.mu.RUnlock()

	if p.listener != nil {
		return p.listener.Addr()
	}
	addr, _, _ := p.factory(false)
	return addr
}

// --- Shutdown / control ---

func (p *TServerSocket) try_close(interrupt bool) error {
	p.mu.Lock()
	defer p.mu.Unlock()
	if (interrupt){
		p.interrupted = true
	}

	var err error = nil
	if p.listener != nil {
		err = p.listener.Close()
		p.listener = nil
	}
	return err
}


func (p *TServerSocket) Close() error {
	return p.try_close(false /* do not set interrupted flag */)
}

func (p *TServerSocket) Interrupt() error {
	return p.try_close(true /* set interrupted flag */)
}
