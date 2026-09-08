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
	"crypto/tls"
	"testing"
)

// When the caller leaves MinVersion unset, the socket constructor must not
// lower it below what crypto/tls would choose on its own. crypto/tls treats
// an unset (0) MinVersion as its own secure default, so the constructor
// leaves it unset rather than pinning an older floor.
func TestTSSLSocketConfDoesNotLowerTLSFloor(t *testing.T) {
	conf := &TConfiguration{TLSConfig: &tls.Config{}}
	NewTSSLSocketConf("localhost:9090", conf)

	if got := conf.GetTLSConfig().MinVersion; got == tls.VersionTLS10 {
		t.Errorf("MinVersion was pinned to TLS 1.0 (%#04x); expected it left to the crypto/tls default", got)
	}
	if got := conf.GetTLSConfig().MinVersion; got != 0 {
		t.Errorf("an unset MinVersion was changed to %#04x; expected it left unset (0)", got)
	}
}

// A MinVersion the caller did set must be preserved exactly.
func TestTSSLSocketConfKeepsCallerMinVersion(t *testing.T) {
	conf := &TConfiguration{TLSConfig: &tls.Config{MinVersion: tls.VersionTLS13}}
	NewTSSLSocketConf("localhost:9090", conf)

	if got := conf.GetTLSConfig().MinVersion; got != tls.VersionTLS13 {
		t.Errorf("a caller-set MinVersion was overwritten: got %#04x, want %#04x", got, tls.VersionTLS13)
	}
}

// The server socket constructor must not lower the floor either.
func TestTSSLServerSocketDoesNotLowerTLSFloor(t *testing.T) {
	cfg := &tls.Config{}
	if _, err := NewTSSLServerSocketTimeout("localhost:0", cfg, 0); err != nil {
		t.Fatal(err)
	}

	if got := cfg.MinVersion; got == tls.VersionTLS10 {
		t.Errorf("MinVersion was pinned to TLS 1.0 (%#04x); expected it left to the crypto/tls default", got)
	}
	if got := cfg.MinVersion; got != 0 {
		t.Errorf("an unset MinVersion was changed to %#04x; expected it left unset (0)", got)
	}
}
