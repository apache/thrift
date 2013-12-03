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

type TBufferedTransportFactory struct {
	size int
}

type TBuffer struct {
	buffer     []byte
	pos, limit int
}

type TBufferedTransport struct {
	tp   TTransport
	rbuf *TBuffer
	wbuf *TBuffer
}

func (p *TBufferedTransportFactory) GetTransport(trans TTransport) TTransport {
	return NewTBufferedTransport(trans, p.size)
}

func NewTBufferedTransportFactory(bufferSize int) *TBufferedTransportFactory {
	return &TBufferedTransportFactory{size: bufferSize}
}

func NewTBufferedTransport(trans TTransport, bufferSize int) *TBufferedTransport {
	rb := &TBuffer{buffer: make([]byte, bufferSize)}
	wb := &TBuffer{buffer: make([]byte, bufferSize), limit: bufferSize}
	return &TBufferedTransport{tp: trans, rbuf: rb, wbuf: wb}
}

func (p *TBufferedTransport) IsOpen() bool {
	return p.tp.IsOpen()
}

func (p *TBufferedTransport) Open() (err error) {
	return p.tp.Open()
}

func (p *TBufferedTransport) Close() (err error) {
	return p.tp.Close()
}

func (p *TBufferedTransport) Read(buf []byte) (n int, err error) {
	rbuf := p.rbuf
	if rbuf.pos == rbuf.limit { // no more data to read from buffer
		rbuf.pos = 0
		// read data, fill buffer
		rbuf.limit, err = p.tp.Read(rbuf.buffer)
		if err != nil {
			return 0, err
		}
	}
	n = copy(buf, rbuf.buffer[rbuf.pos:rbuf.limit])
	rbuf.pos += n
	return n, nil
}

func (p *TBufferedTransport) Write(buf []byte) (n int, err error) {
	wbuf := p.wbuf
	remaining := len(buf)

	for remaining > 0 {
		if wbuf.pos+remaining > wbuf.limit { // buffer is full, flush buffer
			if err := p.Flush(); err != nil {
				return n, err
			}
		}
		copied := copy(wbuf.buffer[wbuf.pos:], buf[n:])

		wbuf.pos += copied
		n += copied
		remaining -= copied
	}

	return n, nil
}

func (p *TBufferedTransport) Flush() error {
	start := 0
	wbuf := p.wbuf
	for start < wbuf.pos {
		n, err := p.tp.Write(wbuf.buffer[start:wbuf.pos])
		if err != nil {
			return err
		}
		start += n
	}

	wbuf.pos = 0
	return p.tp.Flush()
}

func (p *TBufferedTransport) Peek() bool {
	return p.rbuf.pos < p.rbuf.limit || p.tp.Peek()
}
