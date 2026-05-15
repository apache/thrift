//go:build !windows

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
	"crypto/rand"
	"crypto/rsa"
	"crypto/tls"
	"crypto/x509"
	"crypto/x509/pkix"
	"io"
	"net"
	"testing"
	"time"
)

func TestSocketConnUnix(t *testing.T) {

	t.Run("plain", func(t *testing.T) {
		testSocketConn(t, nil)
	})
	t.Run("tls", func(t *testing.T) {
		tlsCert := randomTLSCertificate(t)
		testSocketConn(t, tlsCert)
	})
}

func testSocketConn(t *testing.T, tlsCert *tls.Certificate) {
	const (
		interval = time.Millisecond * 10
		first    = "hello"
		second   = "world"
	)

	ln, err := serverSocketConn(
		t,
		func(tb testing.TB, sc *socketConn) {
			defer sc.Close()

			time.Sleep(interval)
			if !writeFully(tb, sc, first) {
				return
			}
			time.Sleep(interval)
			writeFully(tb, sc, second)
		},
		tlsCert,
	)
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()

	conn, err := net.Dial("tcp", ln.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	if tlsCert != nil {
		conn = tls.Client(conn, &tls.Config{
			InsecureSkipVerify: true,
		})
	}
	sc := wrapSocketConn(conn)

	buf := make([]byte, 1024)

	if !sc.IsOpen() {
		t.Error("Expected sc to report open, got false")
	}
	n, err := sc.Read(buf)
	if err != nil {
		t.Fatal(err)
	}
	read := string(buf[:n])
	if read != first {
		t.Errorf("Expected read %q, got %q", first, read)
	}

	if !sc.IsOpen() {
		t.Error("Expected sc to report open, got false")
	}
	// Do connection check again twice after server already wrote new data,
	// make sure we don't cause any data loss with the check.
	time.Sleep(interval * 10)
	if !sc.IsOpen() {
		t.Error("Expected sc to report open, got false")
	}
	if !sc.IsOpen() {
		t.Error("Expected sc to report open, got false")
	}
	n, err = sc.Read(buf)
	if err != nil {
		t.Fatal(err)
	}
	read = string(buf[:n])
	if read != second {
		t.Errorf("Expected read %q, got %q", second, read)
	}

	// Now it's supposed to be closed on the server side
	if err := sc.read0(); err != io.EOF {
		t.Errorf("Expected to get EOF on read0, got %v", err)
	}
	if sc.IsOpen() {
		t.Error("Expected sc to report not open, got true")
	}
}

func randomTLSCertificate(t *testing.T) *tls.Certificate {
	t.Helper()

	privateKey, err := rsa.GenerateKey(rand.Reader, 2048)
	if err != nil {
		t.Fatalf("generate private key: %v", err)
	}

	template := x509.Certificate{
		Subject: pkix.Name{
			CommonName: "localhost",
		},
		NotBefore: time.Now().Add(-time.Minute),
		NotAfter:  time.Now().Add(time.Hour),

		BasicConstraintsValid: true,

		DNSNames: []string{"localhost"},
		IPAddresses: []net.IP{
			net.ParseIP("127.0.0.1"),
			net.ParseIP("::1"),
		},
	}

	derBytes, err := x509.CreateCertificate(
		rand.Reader,
		&template,
		&template,
		&privateKey.PublicKey,
		privateKey,
	)
	if err != nil {
		t.Fatalf("create certificate: %v", err)
	}

	cert := tls.Certificate{
		Certificate: [][]byte{derBytes},
		PrivateKey:  privateKey,
	}

	return &cert
}
