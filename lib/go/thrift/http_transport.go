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
	"fmt"
	"io"
	"strconv"
	"time"
)

const (
	BAD_STATUS   = 5
	NO_MORE_DATA = 6
)

var (
	// The golang language spec doesn't allow array constants,
	// so these constants are listed as vars here.
	THttpTransportStatusLineSep  = []byte{' '}
	THttpTransportHeaderColon    = []byte{':'}
	THttpTransportChunkSemicolon = []byte{';'}
	THttpTransportCRLF           = []byte{'\r', '\n'}
)

var (
	THttpTransportBufferSize = 1024
)

type THttpTransport struct {
	transport TTransport

	readBuffer      *bytes.Buffer
	writeBuffer     *bytes.Buffer
	readHeadersDone bool
	contentLength   int
	chunked         bool
}

func NewTHttpTransport(transport TTransport) *THttpTransport {
	return &THttpTransport{
		transport:   transport,
		readBuffer:  bytes.NewBuffer(make([]byte, 0, THttpTransportBufferSize)),
		writeBuffer: bytes.NewBuffer(make([]byte, 0, THttpTransportBufferSize)),
	}
}

func (p *THttpTransport) init() {
	p.readBuffer.Reset()
	p.writeBuffer.Reset()
	p.readHeadersDone = false
	p.contentLength = 0
	p.chunked = false
}

func (p *THttpTransport) Open() error {
	return p.transport.Open()
}

func (p *THttpTransport) IsOpen() bool {
	return p.transport.IsOpen()
}

func (p *THttpTransport) Close() error {
	p.init()
	return p.transport.Close()
}

func (p *THttpTransport) Read(buf []byte) (l int, err error) {
	n, _ := p.readBuffer.Read(buf)
	if n > 0 {
		return n, nil
	}
	p.readBuffer.Reset()
	got, err := p.readMoteData()
	if got == 0 {
		return 0, err
	}
	n, _ = p.readBuffer.Read(buf)
	return n, err
}

func (p *THttpTransport) readMoteData() (int, error) {
	if !p.readHeadersDone {
		p.readHeaders()
	}
	var size int
	var err error
	if p.chunked {
		size, err = p.readChunked()
	} else {
		size, err = p.readContent(p.contentLength)
	}
	return size, err
}

func (p *THttpTransport) readHeaders() error {
	// initialize headers status variables
	p.contentLength = 0
	p.chunked = false

	readStatusLineDone := false
	for {
		// read until headers are finished
		line, err := p.readLine()
		if err != nil {
			return err
		}
		if len(line) == 0 {
			if readStatusLineDone {
				p.readHeadersDone = true
				return nil
			} else {
				// Must have been an HTTP 100, keep going for another status line
				readStatusLineDone = false
			}
		} else {
			if !readStatusLineDone {
				if err := p.parseStatusLine(line); err != nil {
					return err
				}
				readStatusLineDone = true
			} else {
				p.parseHeader(line)
			}
		}
	}
}

func (p *THttpTransport) refill() error {
	buf := make([]byte, THttpTransportBufferSize)
	n, err := p.transport.Read(buf)
	if n == 0 {
		return NewTTransportException(NO_MORE_DATA, "Could not read more data")
	}
	read := buf[:n]
	p.readBuffer.Write(read)
	if err == nil || err == io.EOF {
		return nil
	}
	return err
}

func (p *THttpTransport) readLine() ([]byte, error) {
	for {
		rbuf := p.readBuffer.Bytes()
		i := bytes.Index(rbuf, THttpTransportCRLF)
		if i != -1 {
			// length is equal to first index of CRLF
			line := p.readBuffer.Next(i)
			// skip CRLF
			p.readBuffer.Next(len(THttpTransportCRLF))
			return line, nil
		} else {
			if err := p.refill(); err != nil {
				return nil, err
			}
		}
	}
}

func (p *THttpTransport) parseStatusLine(line []byte) error {
	s := bytes.Split(line, THttpTransportStatusLineSep)
	if len(s) != 3 {
		return NewTTransportException(BAD_STATUS, "Bad Status: "+string(line))
	}
	method := string(s[0])
	path := s[1]
	http := s[2]
	if len(path) == 0 || len(http) == 0 {
		return NewTTransportException(BAD_STATUS, "Bad Status: "+string(line))
	}
	switch method {
	case "POST":
		// POST method ok, looking for content.
		return nil
	case "OPTIONS":
		// preflight OPTIONS method, we don't need further content.
		header := fmt.Sprintf(
			"HTTP/1.1 200 OK%s"+
				"Date: %s%s"+
				"Access-Control-Allow-Origin: *%s"+
				"Access-Control-Allow-Methods: POST, OPTIONS%s"+
				"Access-Control-Allow-Headers: Content-Type%s"+
				"%s",
			THttpTransportCRLF,
			getTimeRFC1123(), THttpTransportCRLF,
			THttpTransportCRLF,
			THttpTransportCRLF,
			THttpTransportCRLF,
			THttpTransportCRLF)
		// Flush the write buffer and reset header variables
		if err := p.flushWriteBuffer(&header); err != nil {
			return err
		}
		p.readHeadersDone = false
		return nil
	default:
		return NewTTransportException(BAD_STATUS, "Bad Status (unsupported method): "+string(line))
	}
}

func (p *THttpTransport) parseHeader(line []byte) error {
	s := bytes.Split(line, THttpTransportHeaderColon)
	if len(s) != 2 {
		return nil
	}
	key, value := string(s[0]), string(s[1])
	switch key {
	case "Content-Length":
		l, err := strconv.Atoi(value)
		if err != nil {
			return err
		}
		p.chunked = false
		p.contentLength = l
		return nil
	case "Transfer-Encoding":
		if value == "chunked" {
			p.chunked = true
		}
	}
	return nil
}

func getTimeRFC1123() string {
	return time.Now().Format(time.RFC1123)
}

func (p *THttpTransport) readChunked() (int, error) {
	var length int
	line, err := p.readLine()
	if err != nil {
		return 0, err
	}
	chunkSize, err := p.parseChunkSize(line)
	if err != nil {
		return 0, err
	}
	if chunkSize == 0 {
		if err = p.readChunkedFooters(); err != nil {
			return 0, err
		}
	} else {
		// read chunk data
		length, err = p.readContent(chunkSize)
		if err != nil {
			return 0, err
		}
		// read trailing CRLF after chunk
		if _, err = p.readLine(); err != nil {
			return length, err
		}
	}
	return length, nil
}

func (p *THttpTransport) parseChunkSize(line []byte) (int, error) {
	s := bytes.SplitN(line, THttpTransportChunkSemicolon, 1)
	sizeStr := string(s[0])
	return strconv.Atoi(sizeStr)
}

func (p *THttpTransport) readChunkedFooters() error {
	// End of all chunks, read footer lines untils a blank line
	for {
		line, err := p.readLine()
		if err != nil {
			return err
		}
		if len(line) == 0 {
			break
		}
	}
	return nil
}

func (p *THttpTransport) readContent(size int) (int, error) {
	for {
		if size < len(p.readBuffer.Bytes()) {
			// have enough data in read buffer
			break
		}
		if len(p.readBuffer.Bytes()) == 0 {
			// We have given all the data, reset buffer
			p.readBuffer.Reset()
		}
		if err := p.refill(); err != nil {
			return 0, nil
		}
	}
	return size, nil
}

func (p *THttpTransport) Write(buf []byte) (int, error) {
	return p.writeBuffer.Write(buf)
}

func (p *THttpTransport) flushWriteBuffer(header *string) error {
	// Write back the header, data and then flush and reset write buffer
	if _, err := p.transport.Write([]byte(*header)); err != nil {
		return err
	}
	if _, err := p.transport.Write(p.writeBuffer.Bytes()); err != nil {
		return err
	}
	if err := p.transport.Flush(); err != nil {
		return err
	}
	p.writeBuffer.Reset()
	return nil
}

func (p *THttpTransport) Flush() error {
	header := fmt.Sprintf(
		"HTTP/1.1 200 OK%s"+
			"Date: %s%s"+
			"Server: Thrift%s"+
			"Access-Control-Allow-Origin: *%s"+
			"Content-Type: application/x-thrift%s"+
			"Content-Length: %d%s"+
			"Connetion: Keep-Alive%s"+
			"%s",
		THttpTransportCRLF,
		getTimeRFC1123(), THttpTransportCRLF,
		THttpTransportCRLF,
		THttpTransportCRLF,
		THttpTransportCRLF,
		len(p.writeBuffer.Bytes()), THttpTransportCRLF,
		THttpTransportCRLF,
		THttpTransportCRLF,
	)
	if err := p.flushWriteBuffer(&header); err != nil {
		return err
	}
	return nil
}

type tHttpTransportFactory struct {
}

func NewTHttpTransportFactory() TTransportFactory {
	return &tHttpTransportFactory{}
}

func (p *tHttpTransportFactory) GetTransport(base TTransport) TTransport {
	return NewTHttpTransport(base)
}
