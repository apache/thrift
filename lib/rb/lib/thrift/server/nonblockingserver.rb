require 'thrift/server'
require 'sync'
# thrift/server already imports fastthread/thread

module Thrift
  # this class expects to always use a FramedTransport for reading messages
  #--
  # this isn't very pretty, but we're working around the fact that FramedTransport
  # and the processors are all written in a synchronous manner.
  # So lets read data off the wire ourselves, check if we have a full frame, and
  # only then hand it to the transport to parse
  #
  # we inherit from ThreadPoolServer for the initialize/rescuable_serve methods
  class NonblockingServer < ThreadPoolServer
    def initialize(processor, serverTransport, transportFactory=nil, protocolFactory=nil, num=20)
      super
      @sync = Sync.new
    end

    def serve
      @server_thread = Thread.current
      @serverTransport.listen

      begin
        connections = {}
        running_connections = {}
        # the swapping_connections stuff is to ensure the thread doesn't
        # put the connection back into the regular list, then have the server
        # thread process it again, then have the first thread remove it from
        # the running_connections list
        swapping_connections = {}
        thread_group = ThreadGroup.new
        loop do
          break if @shutdown
          handles = [@serverTransport.handle]
          @sync.synchronize(Sync::SH) do
            handles.concat connections.keys
          end
          rd, = select(handles)
          next if rd.nil?
          rd.each do |socket|
            if socket == @serverTransport.handle
              client = @serverTransport.accept
              buffer = ''
              outtrans = @transportFactory.get_transport(client)
              outprot = @protocolFactory.get_protocol(outtrans)
              @sync.synchronize(Sync::EX) do
                connections[client.handle] = [client, buffer, outtrans, outprot]
              end
            else
              client, buffer, outtrans, outprot = nil # for scope
              @sync.synchronize(Sync::SH) do
                client, buffer, outtrans, outprot = connections[socket]
              end
              if socket.eof?
                client.close
                @sync.synchronize(Sync::EX) do
                  connections.delete(socket)
                end
              else
                buffer << client.read(4096, true)
                if has_full_frame?(buffer)
                  @sync.synchronize(Sync::EX) do
                    running_connections[socket] = connections.delete(socket)
                  end
                  @thread_q.push :token
                  t = Thread.new(Thread.current) do |master|
                    begin
                      membuf = MemoryBuffer.new(buffer)
                      intrans = @transportFactory.get_transport(membuf)
                      inprot = @protocolFactory.get_protocol(intrans)
                      @processor.process(inprot, outprot)
                      if @shutdown
                        client.close
                        @sync.synchronize(Sync::EX) do
                          running_connections.delete(socket)
                        end
                      else
                        @sync.synchronize(Sync::EX) do
                          swapping_connections[socket] = running_connections.delete(socket)
                        end
                      end
                    rescue => e
                      outtrans.close
                      @exception_q.push e
                    ensure
                      should_wakeup = false
                      @sync.synchronize(Sync::EX) do
                        running_connections.delete(socket)
                        if swapping_connections.include? socket
                          connections[socket] = swapping_connections.delete(socket)
                          should_wakeup = true
                        end
                      end
                      master.wakeup if should_wakeup
                      intrans.close
                      @thread_q.pop
                    end
                  end
                  thread_group.add t
                end
              end
            end
          end
        end
        if @shutdown
          @serverTransport.close
          handles = []
          @sync.synchronize(Sync::SH) do
            handles = connections
            handles.merge! running_connections
            handles.merge! swapping_connections
          end
          handles.values.each do |client, buffer, outtrans, outprot|
            # can't close completely or we'll break active messages
            # but lets at least stop accepting input
            client.handle.close_read
          end
          start = Time.now.to_f
          until thread_group.list.empty?
            if @shutdown_timeout
              now = Time.now.to_f
              cur_timeout = @shutdown_timeout - (now - start)
              break if cur_timeout <= 0
              thread_group.list.first.join(cur_timeout)
            else
              thread_group.list.first.join
            end
          end
          thread_group.list.each { |t| t.kill } if @shutdown_kill
          # now kill connections completely if they still exists
          handles.values.each do |client, buffer, outtrans, outprot|
            client.close
          end
        end
      ensure
        @serverTransport.close
      end
    end

    # Stop accepting new messages and wait for active messages to finish
    # If the given timeout passes without the active messages finishing,
    # control will exit from #serve and leave the remaining threads active.
    # If you pass true for kill, the remaining threads will be reaped instead.
    # A false timeout means wait indefinitely
    def shutdown(timeout = nil, kill = false)
      @shutdown_timeout = timeout
      @shutdown_kill = kill
      @shutdown = true
      @server_thread.wakeup
    end

    private

    def has_full_frame?(buf)
      return no unless buf.length >= 4
      size = buf.unpack('N').first
      size + 4 <= buf.length
    end
  end
end
