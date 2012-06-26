package thrift

import (
  "testing"
)

func TestMapSetI32(t *testing.T) {
  tm := NewTMap(I32, BOOL, 1)
  tm.Set(15, true)
  tm.Set(32, false)
  if !tm.Contains(15) {
    t.Errorf("Expected a key of 15, but not found in set")
  }
  if !tm.Contains(32) {
    t.Errorf("Expected a key of 32, but not found in set")
  }
  if tm.Contains(37) {
    t.Errorf("Expected not to find a key of 37, but found in set")
  }
  if v, found := tm.Get(15); !v.(bool) || !found {
    t.Errorf("Expected key of 15 => true, true, but was %v, %v", v, found)
  }
  if v, found := tm.Get(32); v.(bool) || !found {
    t.Errorf("Expected key of 32 => false, true, but was %v, %v", v, found)
  }
  if v, found := tm.Get(37); found {
    t.Errorf("Expected key of 37 => false, false, but was %v, %v", v, found)
  }
}

func TestMapSetString(t *testing.T) {
  tm := NewTMap(STRING, BOOL, 1)
  tm.Set("a", true)
  tm.Set("b", false)
  if !tm.Contains("a") {
    t.Errorf("Expected a key of \"a\", but not found in set")
  }
  if !tm.Contains("b") {
    t.Errorf("Expected a key of \"b\", but not found in set")
  }
  if tm.Contains("c") {
    t.Errorf("Expected not to find a key of \"c\", but found in set")
  }
  if v, found := tm.Get("a"); !v.(bool) || !found {
    t.Errorf("Expected key of \"a\" => true, true, but was %v, %v", v, found)
  }
  if v, found := tm.Get("b"); v.(bool) || !found {
    t.Errorf("Expected key of \"b\" => false, true, but was %v, %v", v, found)
  }
  if v, found := tm.Get("c"); found {
    t.Errorf("Expected key of \"c\" => false, false, but was %v, %v", v, found)
  }
}

func TestMapSetBinary(t *testing.T) {
  tm := NewTMap(BINARY, BOOL, 1)
  tm.Set([]byte("a"), true)
  tm.Set([]byte("b"), false)
  if !tm.Contains([]byte("a")) {
    t.Errorf("Expected a key of []byte(\"a\"), but not found in set")
  }
  if !tm.Contains([]byte("b")) {
    t.Errorf("Expected a key of []byte(\"b\"), but not found in set")
  }
  if tm.Contains([]byte("c")) {
    t.Errorf("Expected not to find a key of []byte(\"c\"), but found in set")
  }
  if v, found := tm.Get([]byte("a")); !v.(bool) || !found {
    t.Errorf("Expected key of []byte(\"a\") => true, true, but was %v, %v", v, found)
  }
  if v, found := tm.Get([]byte("b")); v.(bool) || !found {
    t.Errorf("Expected key of []byte(\"b\") => false, true, but was %v, %v", v, found)
  }
  if v, found := tm.Get([]byte("c")); found {
    t.Errorf("Expected key of []byte(\"c\") => false, false, but was %v, %v", v, found)
  }
}
