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
	"fmt"
)

type TDebugProtocol struct {
	Delegate  TProtocol
	LogPrefix string
	Logger    Logger
}

type TDebugProtocolFactory struct {
	Underlying TProtocolFactory
	LogPrefix  string
	Logger     Logger
}

// NewTDebugProtocolFactory creates a TDebugProtocolFactory.
//
// Deprecated: Please use NewTDebugProtocolFactoryWithLogger or the struct
// itself instead. This version will use the default logger from standard
// library.
func NewTDebugProtocolFactory(underlying TProtocolFactory, logPrefix string) *TDebugProtocolFactory {
	return &TDebugProtocolFactory{
		Underlying: underlying,
		LogPrefix:  logPrefix,
		Logger:     StdLogger(nil),
	}
}

// NewTDebugProtocolFactoryWithLogger creates a TDebugProtocolFactory.
func NewTDebugProtocolFactoryWithLogger(underlying TProtocolFactory, logPrefix string, logger Logger) *TDebugProtocolFactory {
	return &TDebugProtocolFactory{
		Underlying: underlying,
		LogPrefix:  logPrefix,
		Logger:     logger,
	}
}

func (t *TDebugProtocolFactory) GetProtocol(trans TTransport) TProtocol {
	return &TDebugProtocol{
		Delegate:  t.Underlying.GetProtocol(trans),
		LogPrefix: t.LogPrefix,
		Logger:    fallbackLogger(t.Logger),
	}
}

func (tdp *TDebugProtocol) logf(format string, v ...interface{}) {
	fallbackLogger(tdp.Logger)(fmt.Sprintf(format, v...))
}

func (tdp *TDebugProtocol) WriteMessageBegin(ctx context.Context, name string, typeId TMessageType, seqid int32) error {
	err := tdp.Delegate.WriteMessageBegin(ctx, name, typeId, seqid)
	tdp.logf("%sWriteMessageBegin(name=%#v, typeId=%#v, seqid=%#v) => %#v", tdp.LogPrefix, name, typeId, seqid, err)
	return err
}
func (tdp *TDebugProtocol) WriteMessageEnd(ctx context.Context) error {
	err := tdp.Delegate.WriteMessageEnd(ctx)
	tdp.logf("%sWriteMessageEnd() => %#v", tdp.LogPrefix, err)
	return err
}
func (tdp *TDebugProtocol) WriteStructBegin(ctx context.Context, name string) error {
	err := tdp.Delegate.WriteStructBegin(ctx, name)
	tdp.logf("%sWriteStructBegin(name=%#v) => %#v", tdp.LogPrefix, name, err)
	return err
}
func (tdp *TDebugProtocol) WriteStructEnd(ctx context.Context) error {
	err := tdp.Delegate.WriteStructEnd(ctx)
	tdp.logf("%sWriteStructEnd() => %#v", tdp.LogPrefix, err)
	return err
}
func (tdp *TDebugProtocol) WriteFieldBegin(ctx context.Context, name string, typeId TType, id int16) error {
	err := tdp.Delegate.WriteFieldBegin(ctx, name, typeId, id)
	tdp.logf("%sWriteFieldBegin(name=%#v, typeId=%#v, id%#v) => %#v", tdp.LogPrefix, name, typeId, id, err)
	return err
}
func (tdp *TDebugProtocol) WriteFieldEnd(ctx context.Context) error {
	err := tdp.Delegate.WriteFieldEnd(ctx)
	tdp.logf("%sWriteFieldEnd() => %#v", tdp.LogPrefix, err)
	return err
}
func (tdp *TDebugProtocol) WriteFieldStop(ctx context.Context) error {
	err := tdp.Delegate.WriteFieldStop(ctx)
	tdp.logf("%sWriteFieldStop() => %#v", tdp.LogPrefix, err)
	return err
}
func (tdp *TDebugProtocol) WriteMapBegin(ctx context.Context, keyType TType, valueType TType, size int) error {
	err := tdp.Delegate.WriteMapBegin(ctx, keyType, valueType, size)
	tdp.logf("%sWriteMapBegin(keyType=%#v, valueType=%#v, size=%#v) => %#v", tdp.LogPrefix, keyType, valueType, size, err)
	return err
}
func (tdp *TDebugProtocol) WriteMapEnd(ctx context.Context) error {
	err := tdp.Delegate.WriteMapEnd(ctx)
	tdp.logf("%sWriteMapEnd() => %#v", tdp.LogPrefix, err)
	return err
}
func (tdp *TDebugProtocol) WriteListBegin(ctx context.Context, elemType TType, size int) error {
	err := tdp.Delegate.WriteListBegin(ctx, elemType, size)
	tdp.logf("%sWriteListBegin(elemType=%#v, size=%#v) => %#v", tdp.LogPrefix, elemType, size, err)
	return err
}
func (tdp *TDebugProtocol) WriteListEnd(ctx context.Context) error {
	err := tdp.Delegate.WriteListEnd(ctx)
	tdp.logf("%sWriteListEnd() => %#v", tdp.LogPrefix, err)
	return err
}
func (tdp *TDebugProtocol) WriteSetBegin(ctx context.Context, elemType TType, size int) error {
	err := tdp.Delegate.WriteSetBegin(ctx, elemType, size)
	tdp.logf("%sWriteSetBegin(elemType=%#v, size=%#v) => %#v", tdp.LogPrefix, elemType, size, err)
	return err
}
func (tdp *TDebugProtocol) WriteSetEnd(ctx context.Context) error {
	err := tdp.Delegate.WriteSetEnd(ctx)
	tdp.logf("%sWriteSetEnd() => %#v", tdp.LogPrefix, err)
	return err
}
func (tdp *TDebugProtocol) WriteBool(ctx context.Context, value bool) error {
	err := tdp.Delegate.WriteBool(ctx, value)
	tdp.logf("%sWriteBool(value=%#v) => %#v", tdp.LogPrefix, value, err)
	return err
}
func (tdp *TDebugProtocol) WriteByte(ctx context.Context, value int8) error {
	err := tdp.Delegate.WriteByte(ctx, value)
	tdp.logf("%sWriteByte(value=%#v) => %#v", tdp.LogPrefix, value, err)
	return err
}
func (tdp *TDebugProtocol) WriteI16(ctx context.Context, value int16) error {
	err := tdp.Delegate.WriteI16(ctx, value)
	tdp.logf("%sWriteI16(value=%#v) => %#v", tdp.LogPrefix, value, err)
	return err
}
func (tdp *TDebugProtocol) WriteI32(ctx context.Context, value int32) error {
	err := tdp.Delegate.WriteI32(ctx, value)
	tdp.logf("%sWriteI32(value=%#v) => %#v", tdp.LogPrefix, value, err)
	return err
}
func (tdp *TDebugProtocol) WriteI64(ctx context.Context, value int64) error {
	err := tdp.Delegate.WriteI64(ctx, value)
	tdp.logf("%sWriteI64(value=%#v) => %#v", tdp.LogPrefix, value, err)
	return err
}
func (tdp *TDebugProtocol) WriteDouble(ctx context.Context, value float64) error {
	err := tdp.Delegate.WriteDouble(ctx, value)
	tdp.logf("%sWriteDouble(value=%#v) => %#v", tdp.LogPrefix, value, err)
	return err
}
func (tdp *TDebugProtocol) WriteString(ctx context.Context, value string) error {
	err := tdp.Delegate.WriteString(ctx, value)
	tdp.logf("%sWriteString(value=%#v) => %#v", tdp.LogPrefix, value, err)
	return err
}
func (tdp *TDebugProtocol) WriteBinary(ctx context.Context, value []byte) error {
	err := tdp.Delegate.WriteBinary(ctx, value)
	tdp.logf("%sWriteBinary(value=%#v) => %#v", tdp.LogPrefix, value, err)
	return err
}

func (tdp *TDebugProtocol) ReadMessageBegin(ctx context.Context) (name string, typeId TMessageType, seqid int32, err error) {
	name, typeId, seqid, err = tdp.Delegate.ReadMessageBegin(ctx)
	tdp.logf("%sReadMessageBegin() (name=%#v, typeId=%#v, seqid=%#v, err=%#v)", tdp.LogPrefix, name, typeId, seqid, err)
	return
}
func (tdp *TDebugProtocol) ReadMessageEnd(ctx context.Context) (err error) {
	err = tdp.Delegate.ReadMessageEnd(ctx)
	tdp.logf("%sReadMessageEnd() err=%#v", tdp.LogPrefix, err)
	return
}
func (tdp *TDebugProtocol) ReadStructBegin(ctx context.Context) (name string, err error) {
	name, err = tdp.Delegate.ReadStructBegin(ctx)
	tdp.logf("%sReadStructBegin() (name%#v, err=%#v)", tdp.LogPrefix, name, err)
	return
}
func (tdp *TDebugProtocol) ReadStructEnd(ctx context.Context) (err error) {
	err = tdp.Delegate.ReadStructEnd(ctx)
	tdp.logf("%sReadStructEnd() err=%#v", tdp.LogPrefix, err)
	return
}
func (tdp *TDebugProtocol) ReadFieldBegin(ctx context.Context) (name string, typeId TType, id int16, err error) {
	name, typeId, id, err = tdp.Delegate.ReadFieldBegin(ctx)
	tdp.logf("%sReadFieldBegin() (name=%#v, typeId=%#v, id=%#v, err=%#v)", tdp.LogPrefix, name, typeId, id, err)
	return
}
func (tdp *TDebugProtocol) ReadFieldEnd(ctx context.Context) (err error) {
	err = tdp.Delegate.ReadFieldEnd(ctx)
	tdp.logf("%sReadFieldEnd() err=%#v", tdp.LogPrefix, err)
	return
}
func (tdp *TDebugProtocol) ReadMapBegin(ctx context.Context) (keyType TType, valueType TType, size int, err error) {
	keyType, valueType, size, err = tdp.Delegate.ReadMapBegin(ctx)
	tdp.logf("%sReadMapBegin() (keyType=%#v, valueType=%#v, size=%#v, err=%#v)", tdp.LogPrefix, keyType, valueType, size, err)
	return
}
func (tdp *TDebugProtocol) ReadMapEnd(ctx context.Context) (err error) {
	err = tdp.Delegate.ReadMapEnd(ctx)
	tdp.logf("%sReadMapEnd() err=%#v", tdp.LogPrefix, err)
	return
}
func (tdp *TDebugProtocol) ReadListBegin(ctx context.Context) (elemType TType, size int, err error) {
	elemType, size, err = tdp.Delegate.ReadListBegin(ctx)
	tdp.logf("%sReadListBegin() (elemType=%#v, size=%#v, err=%#v)", tdp.LogPrefix, elemType, size, err)
	return
}
func (tdp *TDebugProtocol) ReadListEnd(ctx context.Context) (err error) {
	err = tdp.Delegate.ReadListEnd(ctx)
	tdp.logf("%sReadListEnd() err=%#v", tdp.LogPrefix, err)
	return
}
func (tdp *TDebugProtocol) ReadSetBegin(ctx context.Context) (elemType TType, size int, err error) {
	elemType, size, err = tdp.Delegate.ReadSetBegin(ctx)
	tdp.logf("%sReadSetBegin() (elemType=%#v, size=%#v, err=%#v)", tdp.LogPrefix, elemType, size, err)
	return
}
func (tdp *TDebugProtocol) ReadSetEnd(ctx context.Context) (err error) {
	err = tdp.Delegate.ReadSetEnd(ctx)
	tdp.logf("%sReadSetEnd() err=%#v", tdp.LogPrefix, err)
	return
}
func (tdp *TDebugProtocol) ReadBool(ctx context.Context) (value bool, err error) {
	value, err = tdp.Delegate.ReadBool(ctx)
	tdp.logf("%sReadBool() (value=%#v, err=%#v)", tdp.LogPrefix, value, err)
	return
}
func (tdp *TDebugProtocol) ReadByte(ctx context.Context) (value int8, err error) {
	value, err = tdp.Delegate.ReadByte(ctx)
	tdp.logf("%sReadByte() (value=%#v, err=%#v)", tdp.LogPrefix, value, err)
	return
}
func (tdp *TDebugProtocol) ReadI16(ctx context.Context) (value int16, err error) {
	value, err = tdp.Delegate.ReadI16(ctx)
	tdp.logf("%sReadI16() (value=%#v, err=%#v)", tdp.LogPrefix, value, err)
	return
}
func (tdp *TDebugProtocol) ReadI32(ctx context.Context) (value int32, err error) {
	value, err = tdp.Delegate.ReadI32(ctx)
	tdp.logf("%sReadI32() (value=%#v, err=%#v)", tdp.LogPrefix, value, err)
	return
}
func (tdp *TDebugProtocol) ReadI64(ctx context.Context) (value int64, err error) {
	value, err = tdp.Delegate.ReadI64(ctx)
	tdp.logf("%sReadI64() (value=%#v, err=%#v)", tdp.LogPrefix, value, err)
	return
}
func (tdp *TDebugProtocol) ReadDouble(ctx context.Context) (value float64, err error) {
	value, err = tdp.Delegate.ReadDouble(ctx)
	tdp.logf("%sReadDouble() (value=%#v, err=%#v)", tdp.LogPrefix, value, err)
	return
}
func (tdp *TDebugProtocol) ReadString(ctx context.Context) (value string, err error) {
	value, err = tdp.Delegate.ReadString(ctx)
	tdp.logf("%sReadString() (value=%#v, err=%#v)", tdp.LogPrefix, value, err)
	return
}
func (tdp *TDebugProtocol) ReadBinary(ctx context.Context) (value []byte, err error) {
	value, err = tdp.Delegate.ReadBinary(ctx)
	tdp.logf("%sReadBinary() (value=%#v, err=%#v)", tdp.LogPrefix, value, err)
	return
}
func (tdp *TDebugProtocol) Skip(ctx context.Context, fieldType TType) (err error) {
	err = tdp.Delegate.Skip(ctx, fieldType)
	tdp.logf("%sSkip(fieldType=%#v) (err=%#v)", tdp.LogPrefix, fieldType, err)
	return
}
func (tdp *TDebugProtocol) Flush(ctx context.Context) (err error) {
	err = tdp.Delegate.Flush(ctx)
	tdp.logf("%sFlush() (err=%#v)", tdp.LogPrefix, err)
	return
}

func (tdp *TDebugProtocol) Transport() TTransport {
	return tdp.Delegate.Transport()
}
