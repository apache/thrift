module ThriftClient
  def initialize(iprot, oprot=nil)
    @iprot = iprot
    @oprot = oprot || iprot
    @seqid = 0
  end

  def send_message(name, args_class, args = {})
    @oprot.writeMessageBegin(name, TMessageType::CALL, @seqid)
    data = args_class.new
    args.each do |k, v|
      data.send("#{k.to_s}=", v)
    end
    data.write(@oprot)
    @oprot.writeMessageEnd()
    @oprot.trans.flush()
  end

  def receive_message(result_klass)
    fname, mtype, rseqid = @iprot.readMessageBegin()
    handle_exception(mtype)
    result = result_klass.new
    result.read(@iprot)
    @iprot.readMessageEnd()
    return result
  end

  def handle_exception(mtype)
    if mtype == TMessageType::EXCEPTION
      x = TApplicationException.new()
      x.read(@iprot)
      @iprot.readMessageEnd()
      raise x
    end
  end
end
