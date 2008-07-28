module Thrift
  module Client
    def initialize(iprot, oprot=nil)
      @iprot = iprot
      @oprot = oprot || iprot
      @seqid = 0
    end

    def send_message(name, args_class, args = {})
      @oprot.write_message_begin(name, MessageTypes::CALL, @seqid)
      data = args_class.new
      args.each do |k, v|
        data.send("#{k.to_s}=", v)
      end
      begin
        data.write(@oprot)
      rescue StandardError => e
        @oprot.trans.close
        raise e
      end
      @oprot.write_message_end
      @oprot.trans.flush
    end

    def receive_message(result_klass)
      fname, mtype, rseqid = @iprot.read_message_begin
      handle_exception(mtype)
      result = result_klass.new
      result.read(@iprot)
      @iprot.read_message_end
      result
    end

    def handle_exception(mtype)
      if mtype == MessageTypes::EXCEPTION
        x = ApplicationException.new
        x.read(@iprot)
        @iprot.read_message_end
        raise x
      end
    end
  end
  deprecate_module! :ThriftClient => Client
end
