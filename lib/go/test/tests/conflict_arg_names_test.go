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

package tests

import (
	"context"
	"testing"

	"github.com/apache/thrift/lib/go/test/gopath/src/conflictargnamestest"
	"github.com/apache/thrift/lib/go/thrift"
)

// We just want to make sure that the conflictargnamestest package compiles.
var _ = conflictargnamestest.GoUnusedProtection__

// The client puts every argument into the request, also those named like
// identifiers of the generated client method itself.
func TestConflictArgNamesClientSendsArgs(t *testing.T) {
	var sent thrift.TStruct
	client := conflictargnamestest.NewConflictArgNamesTestClient(thrift.WrappedTClient{
		Wrapped: func(_ context.Context, _ string, args, _ thrift.TStruct) (thrift.ResponseMeta, error) {
			sent = args
			return thrift.ResponseMeta{}, nil
		},
	})
	if err := client.TestNameConflicts(context.Background(), "result", "meta", "r", "err", "p"); err != nil {
		t.Fatalf("TestNameConflicts: %v", err)
	}

	args, ok := sent.(*conflictargnamestest.ConflictArgNamesTestTestNameConflictsArgs)
	if !ok {
		t.Fatalf("client sent %T, want *conflictargnamestest.ConflictArgNamesTestTestNameConflictsArgs", sent)
	}
	for _, arg := range []struct{ name, got string }{
		{"result", args.Result_},
		{"meta", args.Meta},
		{"r", args.R},
		{"err", args.Err},
		{"p", args.P},
	} {
		if arg.got != arg.name {
			t.Errorf("argument %s arrived as %q, want %q", arg.name, arg.got, arg.name)
		}
	}
}
