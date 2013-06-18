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
	"net/http"
	"net/url"
	"strconv"
)

type THttpClient struct {
	response           *http.Response
	url                *url.URL
	requestBuffer      *bytes.Buffer
	nsecConnectTimeout int64
	nsecReadTimeout    int64
}

type THttpClientTransportFactory struct {
	url    string
	isPost bool
}

func (p *THttpClientTransportFactory) GetTransport(trans TTransport) TTransport {
	if trans != nil {
		t, ok := trans.(*THttpClient)
		if ok && t.url != nil {
			if t.requestBuffer != nil {
				t2, _ := NewTHttpPostClient(t.url.String())
				return t2
			}
			t2, _ := NewTHttpClient(t.url.String())
			return t2
		}
	}
	if p.isPost {
		s, _ := NewTHttpPostClient(p.url)
		return s
	}
	s, _ := NewTHttpClient(p.url)
	return s
}

func NewTHttpClientTransportFactory(url string) *THttpClientTransportFactory {
	return &THttpClientTransportFactory{url: url, isPost: false}
}

func NewTHttpPostClientTransportFactory(url string) *THttpClientTransportFactory {
	return &THttpClientTransportFactory{url: url, isPost: true}
}

func NewTHttpClient(urlstr string) (TTransport, error) {
	parsedURL, err := url.Parse(urlstr)
	if err != nil {
		return nil, err
	}
	response, err := http.Get(urlstr)
	if err != nil {
		return nil, err
	}
	return &THttpClient{response: response, url: parsedURL}, nil
}

func NewTHttpPostClient(urlstr string) (TTransport, error) {
	parsedURL, err := url.Parse(urlstr)
	if err != nil {
		return nil, err
	}
	buf := make([]byte, 0, 1024)
	return &THttpClient{url: parsedURL, requestBuffer: bytes.NewBuffer(buf)}, nil
}

func (p *THttpClient) Open() error {
	// do nothing
	return nil
}

func (p *THttpClient) IsOpen() bool {
	return p.response != nil || p.requestBuffer != nil
}

func (p *THttpClient) Peek() bool {
	return p.IsOpen()
}

func (p *THttpClient) Close() error {
	if p.response != nil && p.response.Body != nil {
		err := p.response.Body.Close()
		p.response = nil
		return err
	}
	if p.requestBuffer != nil {
		p.requestBuffer.Reset()
		p.requestBuffer = nil
	}
	return nil
}

func (p *THttpClient) Read(buf []byte) (int, error) {
	if p.response == nil {
		return 0, NewTTransportException(NOT_OPEN, "Response buffer is empty, no request.")
	}
	n, err := p.response.Body.Read(buf)
	return n, NewTTransportExceptionFromError(err)
}

func (p *THttpClient) Write(buf []byte) (int, error) {
	n, err := p.requestBuffer.Write(buf)
	return n, err
}

func (p *THttpClient) Flush() error {
	response, err := http.Post(p.url.String(), "application/x-thrift", p.requestBuffer)
	if err != nil {
		return NewTTransportExceptionFromError(err)
	}
	if response.StatusCode != http.StatusOK {
		// TODO(pomack) log bad response
		return NewTTransportException(UNKNOWN_TRANSPORT_EXCEPTION, "HTTP Response code: "+strconv.Itoa(response.StatusCode))
	}
	p.response = response
	return nil
}
