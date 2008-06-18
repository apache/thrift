module TProcessor
  def initialize(handler)
    @handler = handler
  end

  def process(iprot, oprot)
    name, type, seqid  = iprot.readMessageBegin()
    if respond_to?("process_#{name}")
      send("process_#{name}", seqid, iprot, oprot)
      return true
    else
      iprot.skip(TType::STRUCT)
      iprot.readMessageEnd()
      x = TApplicationException.new(TApplicationException::UNKNOWN_METHOD, 'Unknown function '+name)
      oprot.writeMessageBegin(name, TMessageType::EXCEPTION, seqid)
        x.write(oprot)
      oprot.writeMessageEnd()
      oprot.trans.flush()
      return
    end
  end

  def read_args(iprot, args_class)
    args = args_class.new
    args.read(iprot)
    iprot.readMessageEnd
    args
  end

  def write_result(result, oprot, name, seqid)
    oprot.writeMessageBegin(name, TMessageType::REPLY, seqid)
    result.write(oprot)
    oprot.writeMessageEnd()
    oprot.trans.flush()
  end
end
