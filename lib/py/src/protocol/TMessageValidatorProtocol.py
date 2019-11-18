class TMessageValidatorProtocolFactory(object):
    """Factory transport that builds framed transports"""
    def __init__(self, protocol_factory, validation_mode, operation_mode):
        self.protocol_factory = protocol_factory
        self._validation_mode = validation_mode

    def getProtocol(self, trans):
        protocol = TMessageValidatorProtocol(self.protocol_factory.getProtocol(trans), self._validation_mode, self._validation_mode)
        return protocol


class TMessageValidatorProtocol(TProtocolDecorator.TProtocolDecorator):
    _MAGIC_NUMBER = 21474347
    class ValidationMode:
        KEEP_READING = 0
        THROW_EXCEPTION = 1
    class OperationMode:
        SEQID_SLAVE = 0
        SEQID_MASTER = 1
    def __init__(self, protocol, validation_mode, operation_mode):
        self._validation_mode = validation_mode
        self._operation_mode = operation_mode
        self.trans = protocol.trans
        self._rand_seq_id = random.randint(-2147483648, 2147483647)

    def _readMagicNumber(self):
        result = 0
        while True:
            result = result << 8 | ord(self.trans.read(1))
            if result == TMessageValidatorProtocol._MAGIC_NUMBER:
                break

    def writeMagicNumber(self):
        buff = struct.pack("!i", TMessageValidatorProtocol._MAGIC_NUMBER)
        self.trans.write(buff)

    def writeMessageBegin(self, name, m_type, seqid):
        if self._operation_mode == TMessageValidatorProtocol.OperationMode.SEQID_MASTER:
            self._rand_seq_id = self._rand_seq_id + 1
            seqid = self._rand_seq_id
        elif self._operation_mode == TMessageValidatorProtocol.OperationMode.SEQID_SLAVE:
            pass
        else:
            raise TProtocolException(
                type=TProtocolException.NOT_IMPLEMENTED, message='Invalid operation mode selected')
        self.writeMagicNumber()
        super(TMessageValidatorProtocol, self).writeMessageBegin(name, m_type, seqid)

    def readMessageBegin(self):
        self._readMagicNumber()
        (name, msg_type, seqid) = super(TMessageValidatorProtocol, self).readMessageBegin()
        if self._operation_mode == TMessageValidatorProtocol.OperationMode.SEQID_MASTER:
            while self._rand_seq_id != seqid:
                if self._validation_mode == TMessageValidatorProtocol.ValidationMode.KEEP_READING:
                    self._readMagicNumber()
                    (name, msg_type, seqid) = super(TMessageValidatorProtocol, self).readMessageBegin()
                elif self._validation_mode == TMessageValidatorProtocol.ValidationMode.THROW_EXCEPTION:
                    raise TProtocolException(type=TProtocolException.BAD_VERSION, message='Received an unexpected seq id')

        return (name, msg_type, seqid)
