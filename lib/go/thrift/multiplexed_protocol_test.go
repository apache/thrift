package thrift

import (
	"context"
	"strings"
	"testing"
)

func TestMultiplexedProcessorMap(t *testing.T) {
	name := "test"
	processorName := "foo"
	processor := &TMultiplexedProcessor{}
	processor.RegisterDefault(&mockWrappableProcessor{
		ProcessorFuncs: map[string]TProcessorFunction{
			name: WrappedTProcessorFunction{
				Wrapped: func(ctx context.Context, seqId int32, in, out TProtocol) (bool, TException) {
					return true, nil
				},
			},
		},
	})
	processor.RegisterProcessor(processorName, &mockWrappableProcessor{
		ProcessorFuncs: map[string]TProcessorFunction{
			name: WrappedTProcessorFunction{
				Wrapped: func(ctx context.Context, seqId int32, in, out TProtocol) (bool, TException) {
					return true, nil
				},
			},
		},
	})

	processorMap := processor.ProcessorMap()
	if len(processorMap) != 2 {
		t.Fatalf("Wrong processor map size %#v", processorMap)
	}
	for k := range processorMap {
		components := strings.SplitN(k, MULTIPLEXED_SEPARATOR, 2)
		if len(components) == 1 {
			if components[0] != name {
				t.Fatalf("Wrong name for default processor func, expected %q, got %q", name, components[0])
			}
		} else if len(components) == 2 {
			if components[0] != processorName {
				t.Errorf("Wrong processor name, expected %q, got %q", processorName, components[0])
			}
			if components[1] != name {
				t.Errorf("Wrong name for processor func, expected %q, got %q", name, components[1])
			}
		} else {
			t.Fatalf("Wrong number of components %#v", components)
		}
	}
}
