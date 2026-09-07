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
	"go/parser"
	"go/token"
	"os"
	"strconv"
	"strings"
	"testing"
)

// The library is linked into every program that speaks thrift, so none of its
// non-test files may import testing: doing so registers the test flags and
// pulls the testing machinery into production binaries.
func TestLibraryDoesNotImportTesting(t *testing.T) {
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatal(err)
	}

	fset := token.NewFileSet()
	for _, entry := range entries {
		name := entry.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		f, err := parser.ParseFile(fset, name, nil, parser.ImportsOnly)
		if err != nil {
			t.Errorf("%s: %v", name, err)
			continue
		}
		for _, imp := range f.Imports {
			path, err := strconv.Unquote(imp.Path.Value)
			if err != nil {
				t.Errorf("%s: %v", name, err)
				continue
			}
			if path == "testing" || strings.HasPrefix(path, "testing/") {
				t.Errorf("%s imports %q", name, path)
			}
		}
	}
}

// recordingTB stands in for testing.TB without being one.
type recordingTB struct {
	msgs []string
}

func (r *recordingTB) Errorf(format string, args ...any) {
	r.msgs = append(r.msgs, format)
}

func TestTestLoggerAcceptsAnyErrorf(t *testing.T) {
	rec := new(recordingTB)
	TestLogger(rec)("boom")
	if len(rec.msgs) != 1 {
		t.Fatalf("expected one call, got %d", len(rec.msgs))
	}
}

func TestTestLoggerAcceptsTestingTB(t *testing.T) {
	// The point of the case is that it compiles: a testing.TB must keep
	// working as the argument.
	var tb testing.TB = t
	if TestLogger(tb) == nil {
		t.Error("expected a logger")
	}
}
