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
	"encoding/binary"
	"io"
)

type TFramedTransport struct {
	transport   TTransport
	writeBuffer *bytes.Buffer
	readBuffer  *bytes.Buffer
}

type tFramedTransportFactory struct {
	factory TTransportFactory
}

func NewTFramedTransportFactory(factory TTransportFactory) TTransportFactory {
	return &tFramedTransportFactory{factory: factory}
}

func (p *tFramedTransportFactory) GetTransport(base TTransport) TTransport {
	return NewTFramedTransport(p.factory.GetTransport(base))
}

func NewTFramedTransport(transport TTransport) *TFramedTransport {
	writeBuf := make([]byte, 0, 1024)
	readBuf := make([]byte, 0, 1024)
	return &TFramedTransport{transport: transport, writeBuffer: bytes.NewBuffer(writeBuf), readBuffer: bytes.NewBuffer(readBuf)}
}

func (p *TFramedTransport) Open() error {
	return p.transport.Open()
}

func (p *TFramedTransport) IsOpen() bool {
	return p.transport.IsOpen()
}

func (p *TFramedTransport) Peek() bool {
	return p.transport.Peek()
}

func (p *TFramedTransport) Close() error {
	return p.transport.Close()
}

func (p *TFramedTransport) Read(buf []byte) (int, error) {
	if p.readBuffer.Len() > 0 {
		got, err := p.readBuffer.Read(buf)
		if got > 0 {
			return got, NewTTransportExceptionFromError(err)
		}
	}

	// Read another frame of data
	p.readFrame()

	got, err := p.readBuffer.Read(buf)
	return got, NewTTransportExceptionFromError(err)
}

func (p *TFramedTransport) Write(buf []byte) (int, error) {
	n, err := p.writeBuffer.Write(buf)
	return n, NewTTransportExceptionFromError(err)
}

func (p *TFramedTransport) Flush() error {
	size := p.writeBuffer.Len()
	buf := []byte{0, 0, 0, 0}
	binary.BigEndian.PutUint32(buf, uint32(size))
	_, err := p.transport.Write(buf)
	if err != nil {
		return NewTTransportExceptionFromError(err)
	}
	if size > 0 {
		if n, err := p.writeBuffer.WriteTo(p.transport); err != nil {
			print("Error while flushing write buffer of size ", size, " to transport, only wrote ", n, " bytes: ", err.Error(), "\n")
			return NewTTransportExceptionFromError(err)
		}
	}
	err = p.transport.Flush()
	return NewTTransportExceptionFromError(err)
}

func (p *TFramedTransport) readFrame() (int, error) {
	buf := []byte{0, 0, 0, 0}
	if _, err := io.ReadFull(p.transport, buf); err != nil {
		return 0, err
	}
	size := int(binary.BigEndian.Uint32(buf))
	if size < 0 {
		return 0, NewTTransportException(UNKNOWN_TRANSPORT_EXCEPTION, "Read a negative frame size ("+string(size)+")")
	}
	if size == 0 {
		return 0, nil
	}
	buf2 := make([]byte, size)
	if n, err := io.ReadFull(p.transport, buf2); err != nil {
		return n, err
	}
	p.readBuffer = bytes.NewBuffer(buf2)
	return size, nil
}
