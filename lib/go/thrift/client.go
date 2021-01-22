package thrift

import (
	"context"
	"fmt"
)

type TClient interface {
	Call(ctx context.Context, method string, args, result TStruct) error
}

// TStandardClientUnwrapper is an optional interface TClient implementations can
// choose to implement, to unwrap the underlying *TStandardClient.
//
// Both TStandardClient and WrappedTClient implement it.
type TStandardClientUnwrapper interface {
	TClient

	// Returns the underlying *TStandardClient, if any.
	UnwrapTStandardClient() *TStandardClient
}

type TStandardClient struct {
	seqId        int32
	iprot, oprot TProtocol
}

// TStandardClient implements TClient, and uses the standard message format for Thrift.
// It is not safe for concurrent use.
func NewTStandardClient(inputProtocol, outputProtocol TProtocol) *TStandardClient {
	return &TStandardClient{
		iprot: inputProtocol,
		oprot: outputProtocol,
	}
}

func (p *TStandardClient) Send(ctx context.Context, oprot TProtocol, seqId int32, method string, args TStruct) error {
	// Set headers from context object on THeaderProtocol
	if headerProt, ok := oprot.(*THeaderProtocol); ok {
		headerProt.ClearWriteHeaders()
		for _, key := range GetWriteHeaderList(ctx) {
			if value, ok := GetHeader(ctx, key); ok {
				headerProt.SetWriteHeader(key, value)
			}
		}
	}

	if err := oprot.WriteMessageBegin(ctx, method, CALL, seqId); err != nil {
		return err
	}
	if err := args.Write(ctx, oprot); err != nil {
		return err
	}
	if err := oprot.WriteMessageEnd(ctx); err != nil {
		return err
	}
	return oprot.Flush(ctx)
}

func (p *TStandardClient) Recv(ctx context.Context, iprot TProtocol, seqId int32, method string, result TStruct) error {
	rMethod, rTypeId, rSeqId, err := iprot.ReadMessageBegin(ctx)
	if err != nil {
		return err
	}

	if method != rMethod {
		return NewTApplicationException(WRONG_METHOD_NAME, fmt.Sprintf("%s: wrong method name", method))
	} else if seqId != rSeqId {
		return NewTApplicationException(BAD_SEQUENCE_ID, fmt.Sprintf("%s: out of order sequence response", method))
	} else if rTypeId == EXCEPTION {
		var exception tApplicationException
		if err := exception.Read(ctx, iprot); err != nil {
			return err
		}

		if err := iprot.ReadMessageEnd(ctx); err != nil {
			return err
		}

		return &exception
	} else if rTypeId != REPLY {
		return NewTApplicationException(INVALID_MESSAGE_TYPE_EXCEPTION, fmt.Sprintf("%s: invalid message type", method))
	}

	if err := result.Read(ctx, iprot); err != nil {
		return err
	}

	return iprot.ReadMessageEnd(ctx)
}

func (p *TStandardClient) Call(ctx context.Context, method string, args, result TStruct) error {
	p.seqId++
	seqId := p.seqId

	if err := p.Send(ctx, p.oprot, seqId, method, args); err != nil {
		return err
	}

	// method is oneway
	if result == nil {
		return nil
	}

	return p.Recv(ctx, p.iprot, seqId, method, result)
}

// UnwrapTStandardClient implements TStandardClientUnwrapper by returning self.
func (p *TStandardClient) UnwrapTStandardClient() *TStandardClient {
	return p
}

var _ TStandardClientUnwrapper = (*TStandardClient)(nil)
