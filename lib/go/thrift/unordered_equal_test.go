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

import "testing"

func intEq(x, y int) bool { return x == y }

func TestUnorderedEqual(t *testing.T) {
	for name, c := range map[string]struct {
		a, b []int
		want bool
	}{
		"both nil":             {nil, nil, true},
		"nil and empty":        {nil, []int{}, true},
		"same order":           {[]int{1, 2, 3}, []int{1, 2, 3}, true},
		"reversed":             {[]int{1, 2, 3}, []int{3, 2, 1}, true},
		"rotated":              {[]int{1, 2, 3}, []int{2, 3, 1}, true},
		"single element":       {[]int{1}, []int{1}, true},
		"different element":    {[]int{1, 2}, []int{1, 3}, false},
		"shorter":              {[]int{1, 2}, []int{1}, false},
		"longer":               {[]int{1}, []int{1, 2}, false},
		"disjoint":             {[]int{1, 2}, []int{3, 4}, false},
		"same multiset":        {[]int{1, 1, 2}, []int{2, 1, 1}, true},
		"different multiplic.": {[]int{1, 1, 2}, []int{1, 2, 2}, false},
		"all duplicates":       {[]int{7, 7, 7}, []int{7, 7, 7}, true},
	} {
		t.Run(name, func(t *testing.T) {
			if got := UnorderedEqual(c.a, c.b, intEq); got != c.want {
				t.Errorf("UnorderedEqual(%v, %v) = %v, want %v", c.a, c.b, got, c.want)
			}
			// The relation must not depend on which side is which.
			if got := UnorderedEqual(c.b, c.a, intEq); got != c.want {
				t.Errorf("UnorderedEqual(%v, %v) = %v, want %v (not symmetric)", c.b, c.a, got, c.want)
			}
		})
	}
}

// Elements that cannot be Go map keys take the same path as any other.
func TestUnorderedEqualNonComparable(t *testing.T) {
	eq := func(x, y []string) bool {
		if len(x) != len(y) {
			return false
		}
		for i := range x {
			if x[i] != y[i] {
				return false
			}
		}
		return true
	}
	a := [][]string{{"a"}, {"b", "c"}}
	b := [][]string{{"b", "c"}, {"a"}}
	if !UnorderedEqual(a, b, eq) {
		t.Error("reordered slice elements should compare equal")
	}
	if UnorderedEqual(a, [][]string{{"a"}, {"b"}}, eq) {
		t.Error("differing slice elements should compare unequal")
	}
}

// Comparing values that agree on order must not allocate.
func TestUnorderedEqualOrderedPathDoesNotAllocate(t *testing.T) {
	a := []int{1, 2, 3, 4, 5, 6, 7, 8}
	b := []int{1, 2, 3, 4, 5, 6, 7, 8}
	if n := testing.AllocsPerRun(100, func() {
		if !UnorderedEqual(a, b, intEq) {
			t.Fatal("expected equal")
		}
	}); n != 0 {
		t.Errorf("ordered comparison allocated %v times per run, want 0", n)
	}
}

func BenchmarkUnorderedEqualSameOrder(b *testing.B) {
	x := make([]int, 1000)
	y := make([]int, 1000)
	for i := range x {
		x[i], y[i] = i, i
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if !UnorderedEqual(x, y, intEq) {
			b.Fatal("expected equal")
		}
	}
}

func BenchmarkUnorderedEqualReordered(b *testing.B) {
	x := make([]int, 1000)
	y := make([]int, 1000)
	for i := range x {
		x[i] = i
		y[len(y)-1-i] = i
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if !UnorderedEqual(x, y, intEq) {
			b.Fatal("expected equal")
		}
	}
}
