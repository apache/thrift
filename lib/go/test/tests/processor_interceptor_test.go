package tests

import (
	"context"
	"processorinterceptortest"
	"testing"
	"thrift"
	"time"
)

const requestArgument = "Test message"
const handlerResult = " is processed, return processed result"
const expectedHandlerResult = requestArgument + handlerResult
const expectedInterceptedHandlerResult = expectedHandlerResult + " was intercepted"

func NewResultInterceptor() thrift.HandlerInterceptor {
	return func(ctx context.Context, methodName string, arg interface{}, handlerFunc thrift.HandlerFunc) (result interface{}, err error) {
		handlerResult, handlerError := handlerFunc(ctx, arg)

		return handlerResult.(string) + " was intercepted", handlerError
	}
}

type DummyHandler struct {
}

func (h *DummyHandler) DummyRequest(arg1 string) (string, error) {
	return expectedHandlerResult, nil
}

func TestProcessorInterceptor(t *testing.T) {
	processor := processorinterceptortest.NewProcessorInterceptor(new(DummyHandler), thrift.NewHandlerInterceptorOption(NewResultInterceptor()))
	protocolFactory := thrift.NewTBinaryProtocolFactoryDefault()
	transportFactory := thrift.NewTTransportFactory()
	transportFactory = thrift.NewTFramedTransportFactory(transportFactory)
	addr := FindAvailableTCPServerPort()
	serverTransport, err := thrift.NewTServerSocketTimeout(addr.String(), TIMEOUT)
	if err != nil {
		t.Fatal("Unable to create server socket", err)
	}
	server = thrift.NewTSimpleServer4(processor, serverTransport, transportFactory, protocolFactory)

	defer server.Stop()
	go server.Serve()
	time.Sleep(10 * time.Millisecond)

	transport, err := createTransport(addr)
	if err != nil {
		t.Fatal(err)
	}
	defer transport.Close()
	protocol := protocolFactory.GetProtocol(thrift.NewTBinaryProtocolTransport(transport))

	client := processorinterceptortest.NewProcessorInterceptorClient(thrift.NewTStandardClient(protocol, protocol))

	ret, err := client.DummyRequest(requestArgument)

	if err != nil {
		t.Fatal("Unable to call server:", err)
	} else if ret != expectedInterceptedHandlerResult {
		t.Fatal("Unexpected result from server: ", ret)
	}
}
