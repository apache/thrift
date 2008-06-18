module Thrift
  class Exception < StandardError
    def initialize(message)
      super
      @message = message
    end

    attr_reader :message
  end
  deprecate_class! :TException => Exception

  class ApplicationException < Exception

    UNKNOWN = 0
    UNKNOWN_METHOD = 1
    INVALID_MESSAGE_TYPE = 2
    WRONG_METHOD_NAME = 3
    BAD_SEQUENCE_ID = 4
    MISSING_RESULT = 5

    attr_reader :type

    def initialize(type=UNKNOWN, message=nil)
      super
      @type = type
    end

    def read(iprot)
      iprot.read_struct_begin()
      while true
        fname, ftype, fid = iprot.read_field_begin()
        if (ftype === Types::STOP)
          break
        end
        if (fid == 1)
          if (ftype === Types::STRING)
            @message = iprot.read_string();
          else
            iprot.skip(ftype)
          end
        elsif (fid == 2)
          if (ftype === Types::I32)
            @type = iprot.read_i32();
          else
            iprot.skip(ftype)
          end
        else
          iprot.skip(ftype)
        end
        iprot.read_field_end()
      end
      iprot.read_struct_end()
    end

    def write(oprot)
      oprot.write_struct_begin('Thrift::ApplicationException')
      if (@message != nil)
        oprot.write_field_begin('message', Types::STRING, 1)
        oprot.write_string(@message)
        oprot.write_field_end()
      end
      if (@type != nil)
        oprot.write_field_begin('type', Types::I32, 2)
        oprot.write_i32(@type)
        oprot.write_field_end()
      end
      oprot.write_field_stop()
      oprot.write_struct_end()
    end

  end
  deprecate_class! :TApplicationException => ApplicationException
end