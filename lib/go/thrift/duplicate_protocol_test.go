package thrift

import (
	"bytes"
	"context"
	"encoding/json"
	"testing"
)

func TestDuplicateToJSONReadMakesEqualJSON(t *testing.T) {
	delegateBuf := NewTMemoryBuffer()
	iprot := NewTJSONProtocolFactory().GetProtocol(delegateBuf)

	s := NewMyTestStruct()
	ctx := context.Background()
	s.Write(ctx, iprot)

	iprot.Flush(ctx)
	jsonBytesWritten := delegateBuf.Bytes()

	if err := json.Unmarshal(jsonBytesWritten, NewMyTestStruct()); err != nil {
		t.Errorf("error unmarshaling written bytes: %s", err)
	}

	duplicateBuf := NewTMemoryBuffer()
	dupProto := NewTJSONProtocolFactory().GetProtocol(duplicateBuf)
	proto := &TDuplicateToProtocol{
		Delegate:    NewTJSONProtocolFactory().GetProtocol(delegateBuf),
		DuplicateTo: dupProto,
	}

	if err := s.Read(ctx, proto); err != nil {
		t.Errorf("error reading struct from DuplicateTo: %s", err)
	}
	dupProto.Flush(ctx)

	jsonBytesRead := duplicateBuf.Bytes()

	if !bytes.Equal(jsonBytesWritten, jsonBytesRead) {
		t.Errorf(`bytes read into duplicate do not equal bytes written
		read:
		%s
		written:
		%s
		`, jsonBytesRead, jsonBytesWritten)
	}

	dup := NewMyTestStruct()
	if err := dup.Read(ctx, dupProto); err != nil {
		t.Errorf("error reading struct from duplicated protocol: %s", err)
	}
}
