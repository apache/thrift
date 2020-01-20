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
		return NullInterceptor()
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
		chainedHandlerInterceptor = chainer(chainedHandlerInterceptor, interceptors[i])
	}

	return chainedHandlerInterceptor
}
