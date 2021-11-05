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
	"context"
	"net/http"
	"testing"
)

func TestHttpClient(t *testing.T) {
	l, addr := HttpClientSetupForTest(t)
	if l != nil {
		defer l.Close()
	}
	trans, err := NewTHttpPostClient("http://" + addr.String())
	if err != nil {
		l.Close()
		t.Fatalf("Unable to connect to %s: %v", addr.String(), err)
	}
	TransportTest(t, trans, trans)

	t.Run("nilBuffer", func(t *testing.T) {
		_ = trans.Close()
		if _, err = trans.Write([]byte{1, 2, 3, 4}); err == nil {
			t.Fatal("writing to a closed transport did not result in an error")
		}
	})
}

func TestHttpClientHeaders(t *testing.T) {
	l, addr := HttpClientSetupForTest(t)
	if l != nil {
		defer l.Close()
	}
	trans, err := NewTHttpPostClient("http://" + addr.String())
	if err != nil {
		l.Close()
		t.Fatalf("Unable to connect to %s: %v", addr.String(), err)
	}
	TransportHeaderTest(t, trans, trans)
}

func TestHttpCustomClient(t *testing.T) {
	l, addr := HttpClientSetupForTest(t)
	if l != nil {
		defer l.Close()
	}

	httpTransport := &customHttpTransport{}

	trans, err := NewTHttpPostClientWithOptions("http://"+addr.String(), THttpClientOptions{
		Client: &http.Client{
			Transport: httpTransport,
		},
	})
	if err != nil {
		l.Close()
		t.Fatalf("Unable to connect to %s: %v", addr.String(), err)
	}
	TransportHeaderTest(t, trans, trans)

	if !httpTransport.hit {
		t.Fatalf("Custom client was not used")
	}
}

func TestHttpCustomClientPackageScope(t *testing.T) {
	l, addr := HttpClientSetupForTest(t)
	if l != nil {
		defer l.Close()
	}
	httpTransport := &customHttpTransport{}
	DefaultHttpClient = &http.Client{
		Transport: httpTransport,
	}

	trans, err := NewTHttpPostClient("http://" + addr.String())
	if err != nil {
		l.Close()
		t.Fatalf("Unable to connect to %s: %v", addr.String(), err)
	}
	TransportHeaderTest(t, trans, trans)

	if !httpTransport.hit {
		t.Fatalf("Custom client was not used")
	}
}

func TestHTTPClientFlushesRequestBufferOnErrors(t *testing.T) {
	var (
		write1 = []byte("write 1")
		write2 = []byte("write 2")
	)

	l, addr := HttpClientSetupForTest(t)
	if l != nil {
		defer l.Close()
	}
	trans, err := NewTHttpPostClient("http://" + addr.String())
	if err != nil {
		t.Fatalf("Unable to connect to %s: %v", addr.String(), err)
	}
	defer trans.Close()

	_, err = trans.Write(write1)
	if err != nil {
		t.Fatalf("Failed to write to transport: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	err = trans.Flush(ctx)
	if err == nil {
		t.Fatal("Expected flush error")
	}

	_, err = trans.Write(write2)
	if err != nil {
		t.Fatalf("Failed to write to transport: %v", err)
	}
	err = trans.Flush(context.Background())
	if err != nil {
		t.Fatalf("Failed to flush: %v", err)
	}

	data := make([]byte, 1024)
	n, err := trans.Read(data)
	if err != nil {
		t.Fatalf("Failed to read: %v", err)
	}

	data = data[:n]
	if !bytes.Equal(data, write2) {
		t.Fatalf("Received unexpected data: %q, expected: %q", data, write2)
	}
}

type customHttpTransport struct {
	hit bool
}

func (c *customHttpTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	c.hit = true
	return http.DefaultTransport.RoundTrip(req)
}
