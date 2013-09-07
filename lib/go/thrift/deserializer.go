package thrift

type TDeserializer struct {
	Transport TTransport
	Protocol  TProtocol
}

func NewTDeserializer() *TDeserializer {
	var transport TTransport
	transport = NewTMemoryBufferLen(1024)

	protocol := NewTBinaryProtocolFactoryDefault().GetProtocol(transport)

	return &TDeserializer{
		transport,
		protocol}
}

func (t *TDeserializer) ReadString(msg TStruct, s string) (err error) {
	err = nil
	if _, err = t.Transport.Write([]byte(s)); err != nil {
		return
	}
	if err = msg.Read(t.Protocol); err != nil {
		return
	}
	return
}

func (t *TDeserializer) Read(msg TStruct, b []byte) (err error) {
	err = nil
	if _, err = t.Transport.Write(b); err != nil {
		return
	}
	if err = msg.Read(t.Protocol); err != nil {
		return
	}
	return
}
