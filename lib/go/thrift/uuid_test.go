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
	"fmt"
	"testing"
	"testing/quick"
)

func TestHexToByte(t *testing.T) {
	for _, c := range []struct {
		s    string
		b    byte
		fail bool
	}{
		{
			s: "ff",
			b: 0xff,
		},
		{
			s: "FF",
			b: 0xff,
		},
		{
			s: "00",
			b: 0,
		},
		{
			s: "77",
			b: 0x77,
		},
		{
			s: "aC",
			b: 0xac,
		},
		{
			s:    "xx",
			fail: true,
		},
		{
			s:    "x0",
			fail: true,
		},
		{
			s:    "fx",
			fail: true,
		},
	} {
		t.Run(c.s, func(t *testing.T) {
			b, ok := hexToByte(c.s[0], c.s[1])
			if ok != !c.fail {
				t.Errorf("Want failure, got %x, %v", b, ok)
			}
			if !c.fail && b != c.b {
				t.Errorf("Want %x, got %x", c.b, b)
			}
		})
	}
}

func TestUUIDString(t *testing.T) {
	for _, c := range []struct {
		uuid Tuuid
		want string
	}{
		{
			uuid: Tuuid{},
			want: "00000000-0000-0000-0000-000000000000",
		},
		{
			uuid: Tuuid{
				0x6b, 0xa7, 0xb8, 0x10,
				0x9d, 0xad,
				0x11, 0xd1,
				0x80, 0xb4,
				0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
			},
			want: "6ba7b810-9dad-11d1-80b4-00c04fd430c8",
		},
		{
			uuid: Tuuid{
				0x6b, 0xa7, 0xB8, 0x11,
				0x9d, 0xAd,
				0x11, 0xd1,
				0x80, 0xb4,
				0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
			},
			want: "6ba7b811-9dad-11d1-80b4-00c04fd430c8",
		},
		{
			uuid: Tuuid{
				0x6b, 0xa7, 0xb8, 0x12,
				0x9d, 0xad,
				0x11, 0xd1,
				0x80, 0xb4,
				0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
			},
			want: "6ba7b812-9dad-11d1-80b4-00c04fd430c8",
		},
		{
			uuid: Tuuid{
				0x6b, 0xa7, 0xb8, 0x14,
				0x9d, 0xad,
				0x11, 0xd1,
				0x80, 0xb4,
				0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
			},
			want: "6ba7b814-9dad-11d1-80b4-00c04fd430c8",
		},
	} {
		t.Run(fmt.Sprintf("% 02x", c.uuid[:]), func(t *testing.T) {
			got := c.uuid.String()
			if got != c.want {
				t.Errorf("got %q, want %q", got, c.want)
			}
		})
	}
}

func TestUUIDParse(t *testing.T) {
	for _, c := range []struct {
		uuid string
		want Tuuid
		err  bool
	}{
		{
			uuid: "00000000-0000-0000-0000-000000000000",
			want: Tuuid{
				0x00, 0x00, 0x00, 0x00,
				0x00, 0x00,
				0x00, 0x00,
				0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
		},
		{
			uuid: "6BA7B810-9DAD-11D1-80B4-00C04FD430C8",
			want: Tuuid{
				0x6B, 0xA7, 0xB8, 0x10,
				0x9D, 0xAD,
				0x11, 0xD1,
				0x80, 0xB4,
				0x00, 0xC0, 0x4F, 0xD4, 0x30, 0xC8,
			},
		},
		{
			uuid: "6ba7B811-9dAd-11d1-80b4-00c04fd430c8",
			want: Tuuid{
				0x6b, 0xa7, 0xB8, 0x11,
				0x9d, 0xAd,
				0x11, 0xd1,
				0x80, 0xb4,
				0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
			},
		},
		{
			uuid: "6ba7b812-9dad-11d1-80b4-00c04fd430c8",
			want: Tuuid{
				0x6b, 0xa7, 0xb8, 0x12,
				0x9d, 0xad,
				0x11, 0xd1,
				0x80, 0xb4,
				0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
			},
		},
		{
			uuid: "6ba7b814-9dad-11d1-80b4-00c04fd430c8",
			want: Tuuid{
				0x6b, 0xa7, 0xb8, 0x14,
				0x9d, 0xad,
				0x11, 0xd1,
				0x80, 0xb4,
				0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
			},
		},
		{
			uuid: "00000000000000000000000000000000",
			err:  true, // not in canonical form
		},
		{
			uuid: "6ba7b810-9d-ad11d1-80b4-00c04fd430c8",
			err:  true, // wrong position of hyphens
		},
		{
			uuid: "urn:uuid:6ba7b811-9dad-11d1-80b4-00c04fd430c8",
			err:  true, // urn form is not supported
		},
		{
			uuid: "{6ba7b812-9dad-11d1-80b4-00c04fd430c8}",
			err:  true, // guid with braces form is not supported
		},
		{
			uuid: "6xa7b814-9dad-11d1-80b4-00c04fd430c8",
			err:  true, // non-hex numbers
		},
	} {
		t.Run(c.uuid, func(t *testing.T) {
			uuid, err := ParseTuuid(c.uuid)
			if c.err {
				if err == nil {
					t.Errorf("Got %v, want error", uuid)
				}
			} else {
				if err != nil {
					t.Errorf("Failed to parse: %v", err)
				}
				if uuid != c.want {
					t.Errorf("Got %v, want %v", uuid, c.want)
				}
			}
		})
	}
}

func TestUUIDQuick(t *testing.T) {
	f := func(u Tuuid) bool {
		s := u.String()
		parsed, err := ParseTuuid(s)
		if err != nil {
			t.Error(err)
		}
		if parsed != u {
			t.Errorf("Parsed %v want %v", parsed, u)
		}
		return !t.Failed()
	}
	if err := quick.Check(f, nil); err != nil {
		t.Error(err)
	}
}

func BenchmarkUUIDParse(b *testing.B) {
	for _, s := range []string{
		"00000000-0000-0000-0000-000000000000",
		"6ba7b810-9dad-11d1-80b4-00c04fd430c8",
		"6ba7b811-9dad-11d1-80b4-00c04fd430c8",
		"6ba7b812-9dad-11d1-80b4-00c04fd430c8",
		"6ba7b814-9dad-11d1-80b4-00c04fd430c8",
	} {
		b.Run(s, func(b *testing.B) {
			b.ReportAllocs()
			if _, err := ParseTuuid(s); err != nil {
				b.Fatalf("Unable to parse %q: %v", s, err)
			}
			b.ResetTimer()
			b.RunParallel(func(pb *testing.PB) {
				for pb.Next() {
					ParseTuuid(s)
				}
			})
		})
	}
}

func BenchmarkUUIDString(b *testing.B) {
	for _, u := range []Tuuid{
		{},
		Must(ParseTuuid("6ba7b810-9dad-11d1-80b4-00c04fd430c8")),
		Must(ParseTuuid("6ba7b811-9dad-11d1-80b4-00c04fd430c8")),
		Must(ParseTuuid("6ba7b812-9dad-11d1-80b4-00c04fd430c8")),
		Must(ParseTuuid("6ba7b814-9dad-11d1-80b4-00c04fd430c8")),
	} {
		b.Run(u.String(), func(b *testing.B) {
			b.ReportAllocs()
			b.RunParallel(func(pb *testing.PB) {
				for pb.Next() {
					_ = u.String()
				}
			})
		})
	}
}
