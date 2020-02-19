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

type HandlerFunc func(ctx context.Context, arg interface{}) (result interface{}, err error)
type HandlerInterceptor func(ctx context.Context, methodName string, arg interface{}, handlerFunc HandlerFunc) (result interface{}, err error)

func NullInterceptor() HandlerInterceptor {
	return func(ctx context.Context, methodName string, arg interface{}, handlerFunc HandlerFunc) (result interface{}, err error) {
		return handlerFunc(ctx, arg)
	}
}

func ChainedHandlerInterceptor(interceptors ...HandlerInterceptor) HandlerInterceptor {
	interceptorCount := len(interceptors)

	if interceptorCount == 0 {
		return nil
	}

	chainer := func(currentInterceptor HandlerInterceptor, nextInterceptor HandlerInterceptor) HandlerInterceptor {
		return func(ctx context.Context, methodName string, arg interface{}, handlerFunc HandlerFunc) (result interface{}, err error) {
			return currentInterceptor(ctx, methodName, arg, func(ctx context.Context, arg interface{}) (result interface{}, err error) {
				return nextInterceptor(ctx, methodName, arg, handlerFunc)
			})
		}
	}

	interceptorCount--

	chainedHandlerInterceptor := interceptors[interceptorCount]

	for i := interceptorCount - 1; i >= 0; i-- {
		chainedHandlerInterceptor = chainer(interceptors[i], chainedHandlerInterceptor)
	}

	return chainedHandlerInterceptor
}
