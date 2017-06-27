package thrift

import (
	"fmt"
	"strings"

	"golang.org/x/net/context"
)

/*
TMultiplexedProcessor2 is a TProcessor allowing
a single TServer to provide multiple services with
context support in TProcessor.

To do so, you instantiate the processor and then register additional
processors with it, as shown in the following example:

var processor = thrift.NewTMultiplexedProcessor2()

firstProcessor :=
processor.RegisterProcessor("FirstService", firstProcessor)

processor.registerProcessor(
  "Calculator",
  Calculator.NewCalculatorProcessor(&CalculatorHandler{}),
)

processor.registerProcessor(
  "WeatherReport",
  WeatherReport.NewWeatherReportProcessor(&WeatherReportHandler{}),
)

serverTransport, err := thrift.NewTServerSocketTimeout(addr, TIMEOUT)
if err != nil {
  t.Fatal("Unable to create server socket", err)
}
server := thrift.NewTSimpleServer2(processor, serverTransport)
server.Serve();
*/

type TMultiplexedProcessor2 struct {
	serviceProcessorMap map[string]TProcessor2
	DefaultProcessor    TProcessor2
}

func NewTMultiplexedProcessor2() *TMultiplexedProcessor2 {
	return &TMultiplexedProcessor2{
		serviceProcessorMap: make(map[string]TProcessor2),
	}
}

func (t *TMultiplexedProcessor2) RegisterDefault(processor TProcessor2) {
	t.DefaultProcessor = processor
}

func (t *TMultiplexedProcessor2) RegisterProcessor(name string, processor TProcessor2) {
	if t.serviceProcessorMap == nil {
		t.serviceProcessorMap = make(map[string]TProcessor2)
	}
	t.serviceProcessorMap[name] = processor
}

func (t *TMultiplexedProcessor2) Process(ctx context.Context, in, out TProtocol) (bool, TException) {
	name, typeId, seqid, err := in.ReadMessageBegin()
	if err != nil {
		return false, err
	}
	if typeId != CALL && typeId != ONEWAY {
		return false, fmt.Errorf("Unexpected message type %v", typeId)
	}
	//extract the service name
	v := strings.SplitN(name, MULTIPLEXED_SEPARATOR, 2)
	if len(v) != 2 {
		if t.DefaultProcessor != nil {
			smb := NewStoredMessageProtocol(in, name, typeId, seqid)
			return t.DefaultProcessor.Process(ctx, smb, out)
		}
		return false, fmt.Errorf("Service name not found in message name: %s.  Did you forget to use a TMultiplexProtocol in your client?", name)
	}
	actualProcessor, ok := t.serviceProcessorMap[v[0]]
	if !ok {
		return false, fmt.Errorf("Service name not found: %s.  Did you forget to call registerProcessor()?", v[0])
	}
	smb := NewStoredMessageProtocol(in, v[1], typeId, seqid)
	return actualProcessor.Process(ctx, smb, out)
}
