
require 'thrift'

module Thrift

	class MemoryBufferTransportFactory
		def createTransport
			MemoryBufferTransport.new
		end
	end

	class FastMemoryBufferTransportFactory
		def createTransport
			if defined? FastMemoryBufferTransport then
				FastMemoryBufferTransport.new
			else
				puts "Falling back to MemoryBufferTransport from FastMemoryBufferTransport";
				MemoryBufferTransport.new
			end
		end	
	end
end
