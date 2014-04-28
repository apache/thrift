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
	"bytes"
	"reflect"
	"testing"
)

func TestEnsureTransportsAreRich(t *testing.T) {
	buf := bytes.NewBuffer(make([]byte, 0, 1024))

	transports := []TTransportFactory{
		NewTMemoryBufferTransportFactory(1024),
		NewStreamTransportFactory(buf, buf, true),
		NewTFramedTransportFactory(NewTMemoryBufferTransportFactory(1024)),
		NewTHttpPostClientTransportFactory("http://127.0.0.1"),
	}
	for _, tf := range transports {
		trans := tf.GetTransport(nil)
		_, ok := trans.(TRichTransport)
		if !ok {
			t.Errorf("Transport %s does not implement TRichTransport interface", reflect.ValueOf(trans))
		}
	}
}
