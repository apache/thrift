package thrift

type TSerializer struct {
	Transport TTransport
	Protocol  TProtocol
}

type TStruct interface {
	Write(p TProtocol) error
	Read(p TProtocol) error
}

func NewTSerializer() *TSerializer {
	var transport TTransport
	transport = NewTMemoryBufferLen(1024)

	protocol := NewTBinaryProtocolFactoryDefault().GetProtocol(transport)

	return &TSerializer{
		transport,
		protocol}
}

func (t *TSerializer) WriteString(msg TStruct) (s string, err error) {
	s = ""
	err = nil

	if err = msg.Write(t.Protocol); err != nil {
		return
	}

	if err = t.Protocol.Flush(); err != nil {
		return
	}
	if err = t.Transport.Flush(); err != nil {
		return
	}

	var buf []byte
	var place int
	buf = make([]byte, 1024)
	if place, err = t.Transport.Read(buf); err != nil {
		return
	}

	s = string(buf[:place])
	return
}

func (t *TSerializer) Write(msg TStruct) (b []byte, err error) {
	err = nil

	if err = msg.Write(t.Protocol); err != nil {
		return
	}

	if err = t.Protocol.Flush(); err != nil {
		return
	}

	if err = t.Transport.Flush(); err != nil {
		return
	}

	var buf []byte
	var place int
	buf = make([]byte, 1024)
	if place, err = t.Transport.Read(buf); err != nil {
		return
	}

	b = buf[:place]
	return
}
