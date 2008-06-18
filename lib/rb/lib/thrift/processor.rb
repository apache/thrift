module Thrift
  module Processor
    def initialize(handler)
      @handler = handler
    end

    def process(iprot, oprot)
      name, type, seqid  = iprot.read_message_begin
      if respond_to?("process_#{name}")
        send("process_#{name}", seqid, iprot, oprot)
        true
      else
        iprot.skip(Types::STRUCT)
        iprot.read_message_end
        x = ApplicationException.new(ApplicationException::UNKNOWN_METHOD, 'Unknown function '+name)
        oprot.write_message_begin(name, MessageTypes::EXCEPTION, seqid)
        x.write(oprot)
        oprot.write_message_end
        oprot.trans.flush
        false
      end
    end

    def read_args(iprot, args_class)
      args = args_class.new
      args.read(iprot)
      iprot.read_message_end
      args
    end

    def write_result(result, oprot, name, seqid)
      oprot.write_message_begin(name, MessageTypes::REPLY, seqid)
      result.write(oprot)
      oprot.write_message_end
      oprot.trans.flush
    end
  end
  deprecate_module! :TProcessor => Processor
end
