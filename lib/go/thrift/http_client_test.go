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
	"io"
	"net/http"
	"net/http/httptest"
	"strconv"
	"strings"
	"sync/atomic"
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

// newRedirectTarget starts a plain http server that counts the requests it
// receives and answers each of them with the body "target".
func newRedirectTarget(t *testing.T) (*httptest.Server, *atomic.Int32) {
	t.Helper()
	var hits atomic.Int32
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		hits.Add(1)
		io.Copy(io.Discard, r.Body)
		w.Write([]byte("target"))
	}))
	t.Cleanup(ts.Close)
	return ts, &hits
}

// redirectTo answers every request with the given status and a Location
// header pointing at location.
func redirectTo(status int, location string) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		io.Copy(io.Discard, r.Body)
		http.Redirect(w, r, location, status)
	})
}

// flushRequest writes a request through trans and flushes it.
func flushRequest(t *testing.T, trans TTransport) error {
	t.Helper()
	if _, err := trans.Write([]byte("request")); err != nil {
		t.Fatalf("Failed to write to transport: %v", err)
	}
	return trans.Flush(context.Background())
}

// readResponse reads the response of the last flushed request.
func readResponse(t *testing.T, trans TTransport) string {
	t.Helper()
	data := make([]byte, 64)
	n, err := trans.Read(data)
	if err != nil {
		t.Fatalf("Failed to read: %v", err)
	}
	return string(data[:n])
}

func TestHttpClientRedirectFromHttpsToHttpNotFollowed(t *testing.T) {
	for _, status := range []int{http.StatusFound, http.StatusTemporaryRedirect, http.StatusPermanentRedirect} {
		t.Run(strconv.Itoa(status), func(t *testing.T) {
			target, hits := newRedirectTarget(t)
			origin := httptest.NewTLSServer(redirectTo(status, target.URL))
			defer origin.Close()

			client := origin.Client()
			trans, err := NewTHttpClientWithOptions(origin.URL, THttpClientOptions{Client: client})
			if err != nil {
				t.Fatalf("Unable to create transport for %s: %v", origin.URL, err)
			}
			defer trans.Close()

			err = flushRequest(t, trans)
			if err == nil {
				t.Fatal("Flush followed a redirect from https to http")
			}
			if want := "HTTP Response code: " + strconv.Itoa(status); !strings.Contains(err.Error(), want) {
				t.Errorf("Flush error %q does not contain %q", err, want)
			}
			if n := hits.Load(); n != 0 {
				t.Errorf("Redirect target received %d requests, expected 0", n)
			}
			if client.CheckRedirect != nil {
				t.Error("Flush changed the CheckRedirect of the client passed in")
			}
		})
	}
}

func TestHttpClientRedirectFromHttpsToHttpDefaultClient(t *testing.T) {
	target, hits := newRedirectTarget(t)
	origin := httptest.NewTLSServer(redirectTo(http.StatusTemporaryRedirect, target.URL))
	defer origin.Close()

	saved := DefaultHttpClient
	defer func() { DefaultHttpClient = saved }()
	DefaultHttpClient = origin.Client()

	trans, err := NewTHttpClient(origin.URL)
	if err != nil {
		t.Fatalf("Unable to create transport for %s: %v", origin.URL, err)
	}
	defer trans.Close()

	if err := flushRequest(t, trans); err == nil {
		t.Fatal("Flush followed a redirect from https to http")
	}
	if n := hits.Load(); n != 0 {
		t.Errorf("Redirect target received %d requests, expected 0", n)
	}
	if DefaultHttpClient.CheckRedirect != nil {
		t.Error("Flush changed the CheckRedirect of DefaultHttpClient")
	}
}

func TestHttpClientRedirectsFollowed(t *testing.T) {
	plainTarget, plainHits := newRedirectTarget(t)
	var tlsHits atomic.Int32
	tlsTarget := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		tlsHits.Add(1)
		io.Copy(io.Discard, r.Body)
		w.Write([]byte("target"))
	}))
	defer tlsTarget.Close()

	for _, tc := range []struct {
		name   string
		origin *httptest.Server
		hits   *atomic.Int32
	}{
		{"http to http", httptest.NewServer(redirectTo(http.StatusTemporaryRedirect, plainTarget.URL)), plainHits},
		{"http to https", httptest.NewServer(redirectTo(http.StatusTemporaryRedirect, tlsTarget.URL)), &tlsHits},
		{"https to https", httptest.NewTLSServer(redirectTo(http.StatusTemporaryRedirect, tlsTarget.URL)), &tlsHits},
	} {
		t.Run(tc.name, func(t *testing.T) {
			defer tc.origin.Close()
			before := tc.hits.Load()

			// tlsTarget.Client() trusts the test certificate, which all
			// httptest TLS servers share, and has no CheckRedirect.
			client := tlsTarget.Client()
			trans, err := NewTHttpClientWithOptions(tc.origin.URL, THttpClientOptions{Client: client})
			if err != nil {
				t.Fatalf("Unable to create transport for %s: %v", tc.origin.URL, err)
			}
			defer trans.Close()

			if err := flushRequest(t, trans); err != nil {
				t.Fatalf("Failed to flush: %v", err)
			}
			if n := tc.hits.Load() - before; n != 1 {
				t.Errorf("Redirect target received %d requests, expected 1", n)
			}
			if got := readResponse(t, trans); got != "target" {
				t.Errorf("Received unexpected data: %q, expected: %q", got, "target")
			}
		})
	}
}

func TestHttpClientRedirectCustomCheckRedirect(t *testing.T) {
	target, hits := newRedirectTarget(t)
	origin := httptest.NewTLSServer(redirectTo(http.StatusTemporaryRedirect, target.URL))
	defer origin.Close()

	var calls atomic.Int32
	client := origin.Client()
	client.CheckRedirect = func(req *http.Request, via []*http.Request) error {
		calls.Add(1)
		return nil
	}
	trans, err := NewTHttpClientWithOptions(origin.URL, THttpClientOptions{Client: client})
	if err != nil {
		t.Fatalf("Unable to create transport for %s: %v", origin.URL, err)
	}
	defer trans.Close()

	if err := flushRequest(t, trans); err != nil {
		t.Fatalf("Failed to flush: %v", err)
	}
	if n := calls.Load(); n != 1 {
		t.Errorf("CheckRedirect of the client passed in was called %d times, expected 1", n)
	}
	if n := hits.Load(); n != 1 {
		t.Errorf("Redirect target received %d requests, expected 1", n)
	}
	if got := readResponse(t, trans); got != "target" {
		t.Errorf("Received unexpected data: %q, expected: %q", got, "target")
	}
}

func TestHttpClientRedirectLimit(t *testing.T) {
	var hits atomic.Int32
	var origin *httptest.Server
	origin = httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		hits.Add(1)
		io.Copy(io.Discard, r.Body)
		http.Redirect(w, r, origin.URL, http.StatusTemporaryRedirect)
	}))
	defer origin.Close()

	trans, err := NewTHttpClientWithOptions(origin.URL, THttpClientOptions{Client: &http.Client{}})
	if err != nil {
		t.Fatalf("Unable to create transport for %s: %v", origin.URL, err)
	}
	defer trans.Close()

	err = flushRequest(t, trans)
	if err == nil {
		t.Fatal("Flush did not stop following redirects")
	}
	if want := "stopped after 10 redirects"; !strings.Contains(err.Error(), want) {
		t.Errorf("Flush error %q does not contain %q", err, want)
	}
	if n := hits.Load(); n != 10 {
		t.Errorf("Server received %d requests, expected 10", n)
	}
}
