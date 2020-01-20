package thrift

type ProcessorOption interface {
	Apply(processorOptions *ProcessorOptions)
}

type funcProcessorOption struct {
	f func(*ProcessorOptions)
}

func (f *funcProcessorOption) Apply(processorOptions *ProcessorOptions) {
	f.f(processorOptions)
}

func newFuncProcessOption(f func(*ProcessorOptions)) *funcProcessorOption {
	return &funcProcessorOption{f: f}
}

type ProcessorOptions struct {
	Interceptor HandlerInterceptor
}

var DefaultProcessorOptions = ProcessorOptions {
	Interceptor: NullInterceptor(),
}

func NewHandlerInterceptorOption(interceptor HandlerInterceptor) ProcessorOption {
	return newFuncProcessOption(func(options *ProcessorOptions) {
		options.Interceptor = interceptor
	})
}
