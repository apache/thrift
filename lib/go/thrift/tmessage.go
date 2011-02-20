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

/**
 * Helper class that encapsulates struct metadata.
 *
 */
type TMessage interface {
  Name() string
  TypeId() TMessageType
  SeqId() int
  Equals(other TMessage) bool
}
type tMessage struct {
  name   string
  typeId TMessageType
  seqid  int
}

func NewTMessageDefault() TMessage {
  return NewTMessage("", STOP, 0)
}

func NewTMessage(n string, t TMessageType, s int) TMessage {
  return &tMessage{name: n, typeId: t, seqid: s}
}

func (p *tMessage) Name() string {
  return p.name
}

func (p *tMessage) TypeId() TMessageType {
  return p.typeId
}

func (p *tMessage) SeqId() int {
  return p.seqid
}

func (p *tMessage) String() string {
  return "<TMessage name:'" + p.name + "' type: " + string(p.typeId) + " seqid:" + string(p.seqid) + ">"
}

func (p *tMessage) Equals(other TMessage) bool {
  return p.name == other.Name() && p.typeId == other.TypeId() && p.seqid == other.SeqId()
}

var EMPTY_MESSAGE TMessage

func init() {
  EMPTY_MESSAGE = NewTMessageDefault()
}
