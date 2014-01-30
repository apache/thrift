package thrift

import (
	"fmt"
	"io"

	"testing"
)

type timeout struct{ timedout bool }

func (t *timeout) Timeout() bool {
	return t.timedout
}

func (t *timeout) Error() string {
	return fmt.Sprintf("Timeout: %v", t.timedout)
}

func TestTExceptionTimeout(t *testing.T) {
	timeout := &timeout{true}
	exception := NewTTransportExceptionFromError(timeout)
	if timeout.Error() != exception.Error() {
		t.Fatalf("Error did not match: expected %q, got %q", timeout.Error(), exception.Error())
	}

	if exception.TypeId() != TIMED_OUT {
		t.Fatalf("TypeId was not TIMED_OUT: expected %v, got %v", TIMED_OUT, exception.TypeId())
	}
}

func TestTExceptionEOF(t *testing.T) {
	exception := NewTTransportExceptionFromError(io.EOF)
	if io.EOF.Error() != exception.Error() {
		t.Fatalf("Error did not match: expected %q, got %q", io.EOF.Error(), exception.Error())
	}

	if exception.TypeId() != END_OF_FILE {
		t.Fatalf("TypeId was not END_OF_FILE: expected %v, got %v", END_OF_FILE, exception.TypeId())
	}
}
