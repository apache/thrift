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

// The default processor2 factory just returns a singleton
// instance.
// The TProcessorFactory2 is a context version of the orignal.
type TProcessorFactory2 interface {
	GetProcessor(trans TTransport) TProcessor2
}

type tProcessorFactory2 struct {
	processor TProcessor2
}

func NewTProcessorFactory2(p TProcessor2) TProcessorFactory2 {
	return &tProcessorFactory2{processor: p}
}

func (p *tProcessorFactory2) GetProcessor(trans TTransport) TProcessor2 {
	return p.processor
}

/**
 * The default processor factory2 just returns a singleton
 * instance.
 */
type TProcessorFunctionFactory2 interface {
	GetProcessorFunction(trans TTransport) TProcessorFunction2
}

type tProcessorFunctionFactory2 struct {
	processor TProcessorFunction2
}

func NewTProcessorFunctionFactory2(p TProcessorFunction2) TProcessorFunctionFactory2 {
	return &tProcessorFunctionFactory2{processor: p}
}

func (p *tProcessorFunctionFactory2) GetProcessorFunction(trans TTransport) TProcessorFunction2 {
	return p.processor
}
