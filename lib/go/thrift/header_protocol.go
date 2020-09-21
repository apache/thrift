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
	"context"
)

// THeaderProtocol is a thrift protocol that implements THeader:
// https://github.com/apache/thrift/blob/master/doc/specs/HeaderFormat.md
//
// It supports either binary or compact protocol as the wrapped protocol.
//
// Most of the THeader handlings are happening inside THeaderTransport.
type THeaderProtocol struct {
	transport *THeaderTransport

	// Will be initialized on first read/write.
	protocol TProtocol
}

// NewTHeaderProtocol creates a new THeaderProtocol from the underlying
// transport with default protocol ID.
//
// The passed in transport will be wrapped with THeaderTransport.
//
// Note that THeaderTransport handles frame and zlib by itself,
// so the underlying transport should be a raw socket transports (TSocket or TSSLSocket),
// instead of rich transports like TZlibTransport or TFramedTransport.
func NewTHeaderProtocol(trans TTransport) *THeaderProtocol {
	p, err := newTHeaderProtocolWithProtocolID(trans, THeaderProtocolDefault)
	if err != nil {
		// Since we used THeaderProtocolDefault this should never happen,
		// but put a sanity check here just in case.
		panic(err)
	}
	return p
}

func newTHeaderProtocolWithProtocolID(trans TTransport, protoID THeaderProtocolID) (*THeaderProtocol, error) {
	t, err := NewTHeaderTransportWithProtocolID(trans, protoID)
	if err != nil {
		return nil, err
	}
	p, err := t.protocolID.GetProtocol(t)
	if err != nil {
		return nil, err
	}
	return &THeaderProtocol{
		transport: t,
		protocol:  p,
	}, nil
}

type tHeaderProtocolFactory struct {
	protoID THeaderProtocolID
}

func (f tHeaderProtocolFactory) GetProtocol(trans TTransport) TProtocol {
	p, err := newTHeaderProtocolWithProtocolID(trans, f.protoID)
	if err != nil {
		// Currently there's no way for external users to construct a
		// valid factory with invalid protoID, so this should never
		// happen. But put a sanity check here just in case in the
		// future a bug made that possible.
		panic(err)
	}
	return p
}

// NewTHeaderProtocolFactory creates a factory for THeader with default protocol
// ID.
//
// It's a wrapper for NewTHeaderProtocol
func NewTHeaderProtocolFactory() TProtocolFactory {
	return tHeaderProtocolFactory{
		protoID: THeaderProtocolDefault,
	}
}

// NewTHeaderProtocolFactoryWithProtocolID creates a factory for THeader with
// given protocol ID.
func NewTHeaderProtocolFactoryWithProtocolID(protoID THeaderProtocolID) (TProtocolFactory, error) {
	if err := protoID.Validate(); err != nil {
		return nil, err
	}
	return tHeaderProtocolFactory{
		protoID: protoID,
	}, nil
}

// Transport returns the underlying transport.
//
// It's guaranteed to be of type *THeaderTransport.
func (p *THeaderProtocol) Transport() TTransport {
	return p.transport
}

// GetReadHeaders returns the THeaderMap read from transport.
func (p *THeaderProtocol) GetReadHeaders() THeaderMap {
	return p.transport.GetReadHeaders()
}

// SetWriteHeader sets a header for write.
func (p *THeaderProtocol) SetWriteHeader(key, value string) {
	p.transport.SetWriteHeader(key, value)
}

// ClearWriteHeaders clears all write headers previously set.
func (p *THeaderProtocol) ClearWriteHeaders() {
	p.transport.ClearWriteHeaders()
}

// AddTransform add a transform for writing.
func (p *THeaderProtocol) AddTransform(transform THeaderTransformID) error {
	return p.transport.AddTransform(transform)
}

func (p *THeaderProtocol) Flush(ctx context.Context) error {
	return p.transport.Flush(ctx)
}

func (p *THeaderProtocol) WriteMessageBegin(ctx context.Context, name string, typeID TMessageType, seqID int32) error {
	newProto, err := p.transport.Protocol().GetProtocol(p.transport)
	if err != nil {
		return err
	}
	p.protocol = newProto
	p.transport.SequenceID = seqID
	return p.protocol.WriteMessageBegin(ctx, name, typeID, seqID)
}

func (p *THeaderProtocol) WriteMessageEnd(ctx context.Context) error {
	if err := p.protocol.WriteMessageEnd(ctx); err != nil {
		return err
	}
	return p.transport.Flush(ctx)
}

func (p *THeaderProtocol) WriteStructBegin(ctx context.Context, name string) error {
	return p.protocol.WriteStructBegin(ctx, name)
}

func (p *THeaderProtocol) WriteStructEnd(ctx context.Context) error {
	return p.protocol.WriteStructEnd(ctx)
}

func (p *THeaderProtocol) WriteFieldBegin(ctx context.Context, name string, typeID TType, id int16) error {
	return p.protocol.WriteFieldBegin(ctx, name, typeID, id)
}

func (p *THeaderProtocol) WriteFieldEnd(ctx context.Context) error {
	return p.protocol.WriteFieldEnd(ctx)
}

func (p *THeaderProtocol) WriteFieldStop(ctx context.Context) error {
	return p.protocol.WriteFieldStop(ctx)
}

func (p *THeaderProtocol) WriteMapBegin(ctx context.Context, keyType TType, valueType TType, size int) error {
	return p.protocol.WriteMapBegin(ctx, keyType, valueType, size)
}

func (p *THeaderProtocol) WriteMapEnd(ctx context.Context) error {
	return p.protocol.WriteMapEnd(ctx)
}

func (p *THeaderProtocol) WriteListBegin(ctx context.Context, elemType TType, size int) error {
	return p.protocol.WriteListBegin(ctx, elemType, size)
}

func (p *THeaderProtocol) WriteListEnd(ctx context.Context) error {
	return p.protocol.WriteListEnd(ctx)
}

func (p *THeaderProtocol) WriteSetBegin(ctx context.Context, elemType TType, size int) error {
	return p.protocol.WriteSetBegin(ctx, elemType, size)
}

func (p *THeaderProtocol) WriteSetEnd(ctx context.Context) error {
	return p.protocol.WriteSetEnd(ctx)
}

func (p *THeaderProtocol) WriteBool(ctx context.Context, value bool) error {
	return p.protocol.WriteBool(ctx, value)
}

func (p *THeaderProtocol) WriteByte(ctx context.Context, value int8) error {
	return p.protocol.WriteByte(ctx, value)
}

func (p *THeaderProtocol) WriteI16(ctx context.Context, value int16) error {
	return p.protocol.WriteI16(ctx, value)
}

func (p *THeaderProtocol) WriteI32(ctx context.Context, value int32) error {
	return p.protocol.WriteI32(ctx, value)
}

func (p *THeaderProtocol) WriteI64(ctx context.Context, value int64) error {
	return p.protocol.WriteI64(ctx, value)
}

func (p *THeaderProtocol) WriteDouble(ctx context.Context, value float64) error {
	return p.protocol.WriteDouble(ctx, value)
}

func (p *THeaderProtocol) WriteString(ctx context.Context, value string) error {
	return p.protocol.WriteString(ctx, value)
}

func (p *THeaderProtocol) WriteBinary(ctx context.Context, value []byte) error {
	return p.protocol.WriteBinary(ctx, value)
}

// ReadFrame calls underlying THeaderTransport's ReadFrame function.
func (p *THeaderProtocol) ReadFrame(ctx context.Context) error {
	return p.transport.ReadFrame(ctx)
}

func (p *THeaderProtocol) ReadMessageBegin(ctx context.Context) (name string, typeID TMessageType, seqID int32, err error) {
	if err = p.transport.ReadFrame(ctx); err != nil {
		return
	}

	var newProto TProtocol
	newProto, err = p.transport.Protocol().GetProtocol(p.transport)
	if err != nil {
		tAppExc, ok := err.(TApplicationException)
		if !ok {
			return
		}
		if e := p.protocol.WriteMessageBegin(ctx, "", EXCEPTION, seqID); e != nil {
			return
		}
		if e := tAppExc.Write(ctx, p.protocol); e != nil {
			return
		}
		if e := p.protocol.WriteMessageEnd(ctx); e != nil {
			return
		}
		if e := p.transport.Flush(ctx); e != nil {
			return
		}
		return
	}
	p.protocol = newProto

	return p.protocol.ReadMessageBegin(ctx)
}

func (p *THeaderProtocol) ReadMessageEnd(ctx context.Context) error {
	return p.protocol.ReadMessageEnd(ctx)
}

func (p *THeaderProtocol) ReadStructBegin(ctx context.Context) (name string, err error) {
	return p.protocol.ReadStructBegin(ctx)
}

func (p *THeaderProtocol) ReadStructEnd(ctx context.Context) error {
	return p.protocol.ReadStructEnd(ctx)
}

func (p *THeaderProtocol) ReadFieldBegin(ctx context.Context) (name string, typeID TType, id int16, err error) {
	return p.protocol.ReadFieldBegin(ctx)
}

func (p *THeaderProtocol) ReadFieldEnd(ctx context.Context) error {
	return p.protocol.ReadFieldEnd(ctx)
}

func (p *THeaderProtocol) ReadMapBegin(ctx context.Context) (keyType TType, valueType TType, size int, err error) {
	return p.protocol.ReadMapBegin(ctx)
}

func (p *THeaderProtocol) ReadMapEnd(ctx context.Context) error {
	return p.protocol.ReadMapEnd(ctx)
}

func (p *THeaderProtocol) ReadListBegin(ctx context.Context) (elemType TType, size int, err error) {
	return p.protocol.ReadListBegin(ctx)
}

func (p *THeaderProtocol) ReadListEnd(ctx context.Context) error {
	return p.protocol.ReadListEnd(ctx)
}

func (p *THeaderProtocol) ReadSetBegin(ctx context.Context) (elemType TType, size int, err error) {
	return p.protocol.ReadSetBegin(ctx)
}

func (p *THeaderProtocol) ReadSetEnd(ctx context.Context) error {
	return p.protocol.ReadSetEnd(ctx)
}

func (p *THeaderProtocol) ReadBool(ctx context.Context) (value bool, err error) {
	return p.protocol.ReadBool(ctx)
}

func (p *THeaderProtocol) ReadByte(ctx context.Context) (value int8, err error) {
	return p.protocol.ReadByte(ctx)
}

func (p *THeaderProtocol) ReadI16(ctx context.Context) (value int16, err error) {
	return p.protocol.ReadI16(ctx)
}

func (p *THeaderProtocol) ReadI32(ctx context.Context) (value int32, err error) {
	return p.protocol.ReadI32(ctx)
}

func (p *THeaderProtocol) ReadI64(ctx context.Context) (value int64, err error) {
	return p.protocol.ReadI64(ctx)
}

func (p *THeaderProtocol) ReadDouble(ctx context.Context) (value float64, err error) {
	return p.protocol.ReadDouble(ctx)
}

func (p *THeaderProtocol) ReadString(ctx context.Context) (value string, err error) {
	return p.protocol.ReadString(ctx)
}

func (p *THeaderProtocol) ReadBinary(ctx context.Context) (value []byte, err error) {
	return p.protocol.ReadBinary(ctx)
}

func (p *THeaderProtocol) Skip(ctx context.Context, fieldType TType) error {
	return p.protocol.Skip(ctx, fieldType)
}

// GetResponseHeadersFromClient is a helper function to get the read THeaderMap
// from the last response received from the given client.
//
// If the last response was not sent over THeader protocol,
// a nil map will be returned.
func GetResponseHeadersFromClient(c TClient) THeaderMap {
	if sc, ok := c.(*TStandardClient); ok {
		if hp, ok := sc.iprot.(*THeaderProtocol); ok {
			return hp.transport.readHeaders
		}
	}
	return nil
}
