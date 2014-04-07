package tests

import (
	"BinaryKeyTest"
	"testing"
)

func TestBinaryMapKeyGeneratesString(t *testing.T) {
	s := BinaryKeyTest.NewTestStruct()
	//This will only compile if BinToString has type of map[string]string
	s.BinToString = make(map[string]string)
}
