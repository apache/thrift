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
	"log/slog"
	"strings"
	"testing"

	"github.com/apache/thrift/lib/go/test/gopath/src/forwardtypetest"
	"github.com/apache/thrift/lib/go/thrift"
)

func dropTime(groups []string, a slog.Attr) slog.Attr {
	if len(groups) == 0 && a.Key == slog.TimeKey {
		return slog.Attr{}
	}
	return a
}

func TestSlogTStructWrapperJSON(t *testing.T) {
	for _, c := range []struct {
		label string
		value thrift.TStruct
		want  string
	}{
		{
			label: "struct",
			value: &forwardtypetest.Struct{
				Foo: &forwardtypetest.Exc{
					Code: thrift.Int32Ptr(10),
				},
			},
			want: `{"level":"INFO","msg":"bar","struct":{"type":"*forwardtypetest.Struct","value":{"foo":{"code":10}}}}`,
		},
		{
			label: "exception",
			value: &forwardtypetest.Exc{
				Code: thrift.Int32Ptr(10),
			},
			want: `{"level":"INFO","msg":"bar","struct":{"type":"*forwardtypetest.Exc","value":{"code":10}}}`,
		},
		{
			label: "nil-struct",
			value: (*forwardtypetest.Struct)(nil),
			want:  `{"level":"INFO","msg":"bar","struct":null}`,
		},
		{
			label: "nil-exception",
			value: (*forwardtypetest.Exc)(nil),
			want:  `{"level":"INFO","msg":"bar","struct":null}`,
		},
	} {
		t.Run(c.label, func(t *testing.T) {
			var sb strings.Builder
			logger := slog.New(slog.NewJSONHandler(&sb, &slog.HandlerOptions{
				AddSource:   false,
				ReplaceAttr: dropTime,
			}))

			logger.Info("bar", "struct", c.value)
			if got := strings.TrimSuffix(sb.String(), "\n"); got != c.want {
				t.Errorf("got %q want %q", got, c.want)
			}
		})
	}
}

func TestSlogTStructWrapperText(t *testing.T) {
	for _, c := range []struct {
		label string
		value thrift.TStruct
		want  string
	}{
		{
			label: "struct",
			value: &forwardtypetest.Struct{
				Foo: &forwardtypetest.Exc{
					Code: thrift.Int32Ptr(10),
				},
			},
			want: `level=INFO msg=bar struct="*forwardtypetest.Struct{\"foo\":{\"code\":10}}"`,
		},
		{
			label: "exception",
			value: &forwardtypetest.Exc{
				Code: thrift.Int32Ptr(10),
			},
			want: `level=INFO msg=bar struct="*forwardtypetest.Exc{\"code\":10}"`,
		},
		{
			label: "nil-struct",
			value: (*forwardtypetest.Struct)(nil),
			want:  `level=INFO msg=bar struct=<nil>`,
		},
		{
			label: "nil-exception",
			value: (*forwardtypetest.Exc)(nil),
			want:  `level=INFO msg=bar struct=<nil>`,
		},
	} {
		t.Run(c.label, func(t *testing.T) {
			var sb strings.Builder
			logger := slog.New(slog.NewTextHandler(&sb, &slog.HandlerOptions{
				AddSource:   false,
				ReplaceAttr: dropTime,
			}))

			logger.Info("bar", "struct", c.value)
			if got := strings.TrimSuffix(sb.String(), "\n"); got != c.want {
				t.Errorf("got %q want %q", got, c.want)
			}
		})
	}
}
