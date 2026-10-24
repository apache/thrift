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

// TestHttpClientRedirects checks that a redirect is followed within the origin
// of the configured URL and reported as its status code when it points
// anywhere else, unless the client carries a CheckRedirect policy of its own.
func TestHttpClientRedirects(t *testing.T) {
	const header, value = "X-Thrift-Test", "transport-header-value"
	const payload = "request-body"

	// Every request that reaches the other origin, for the assertions.
	type request struct {
		method, header, body string
	}
	elsewhere := make(chan request, 16)
	other := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		body, _ := io.ReadAll(r.Body)
		elsewhere <- request{r.Method, r.Header.Get(header), string(body)}
		w.Write([]byte("elsewhere"))
	}))
	defer other.Close()

	mux := http.NewServeMux()
	mux.HandleFunc("/elsewhere", func(w http.ResponseWriter, r *http.Request) {
		http.Redirect(w, r, other.URL+"/", http.StatusTemporaryRedirect)
	})
	mux.HandleFunc("/moved", func(w http.ResponseWriter, r *http.Request) {
		http.Redirect(w, r, "/here", http.StatusTemporaryRedirect)
	})
	mux.HandleFunc("/here", func(w http.ResponseWriter, r *http.Request) {
		io.Copy(w, r.Body)
	})
	origin := httptest.NewServer(mux)
	defer origin.Close()

	// post sends payload with the test header to url and returns the first
	// len(want) bytes of the response, or the error Flush reported.
	post := func(t *testing.T, url string, options THttpClientOptions, want string) (string, error) {
		t.Helper()
		trans, err := NewTHttpClientWithOptions(url, options)
		if err != nil {
			t.Fatalf("Unable to create a client for %s: %v", url, err)
		}
		defer trans.Close()
		client := trans.(*THttpClient)
		client.SetHeader(header, value)
		if _, err := client.Write([]byte(payload)); err != nil {
			t.Fatalf("Unable to write: %v", err)
		}
		if err := client.Flush(context.Background()); err != nil {
			return "", err
		}
		body := make([]byte, len(want))
		if _, err := io.ReadFull(client, body); err != nil {
			t.Fatalf("Unable to read the response: %v", err)
		}
		return string(body), nil
	}

	t.Run("otherOrigin", func(t *testing.T) {
		for name, options := range map[string]THttpClientOptions{
			"defaultClient": {},
			"customClient":  {Client: &http.Client{}},
		} {
			t.Run(name, func(t *testing.T) {
				_, err := post(t, origin.URL+"/elsewhere", options, "")
				if err == nil {
					t.Fatal("Flush did not report the redirect to another origin")
				}
				if _, ok := err.(TTransportException); !ok {
					t.Fatalf("Flush returned %T, want TTransportException: %v", err, err)
				}
				select {
				case got := <-elsewhere:
					t.Fatalf("The other origin received %v", got)
				default:
				}
			})
		}
	})

	t.Run("sameOrigin", func(t *testing.T) {
		body, err := post(t, origin.URL+"/moved", THttpClientOptions{}, payload)
		if err != nil {
			t.Fatalf("Flush did not follow the redirect within the origin: %v", err)
		}
		if body != payload {
			t.Fatalf("Read %q after the redirect, want %q", body, payload)
		}
	})

	t.Run("ownPolicy", func(t *testing.T) {
		follow := func(*http.Request, []*http.Request) error { return nil }
		body, err := post(t, origin.URL+"/elsewhere", THttpClientOptions{
			Client: &http.Client{CheckRedirect: follow},
		}, "elsewhere")
		if err != nil {
			t.Fatalf("Flush did not apply the client's own redirect policy: %v", err)
		}
		if body != "elsewhere" {
			t.Fatalf("Read %q, want the other origin's response", body)
		}
		want := request{http.MethodPost, value, payload}
		if got := <-elsewhere; got != want {
			t.Fatalf("The other origin received %v, want %v", got, want)
		}
	})
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
