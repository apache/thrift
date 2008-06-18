module Thrift
  module Processor
    def initialize(handler)
      @handler = handler
    end

    def process(iprot, oprot)
      name, type, seqid  = iprot.readMessageBegin()
      if respond_to?("process_#{name}")
        send("process_#{name}", seqid, iprot, oprot)
        return true
      else
        iprot.skip(Types::STRUCT)
        iprot.readMessageEnd()
        x = ApplicationException.new(ApplicationException::UNKNOWN_METHOD, 'Unknown function '+name)
        oprot.writeMessageBegin(name, MessageTypes::EXCEPTION, seqid)
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
      oprot.writeMessageBegin(name, MessageTypes::REPLY, seqid)
      result.write(oprot)
      oprot.writeMessageEnd()
      oprot.trans.flush()
    end
  end
end
