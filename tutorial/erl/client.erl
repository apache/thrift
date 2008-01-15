-module(client).

-include("thrift.hrl").
-include("transport/tSocket.hrl").
-include("protocol/tBinaryProtocol.hrl").

-include("calculator_thrift.hrl").

-export([t/0]).

t() ->
    Host = "dev020",
    Port = 9999,

    _Sock = oop:start_new(tSocket, [Host, Port]),
    Trans = oop:start_new(tBufferedTransport, [_Sock]),
    Prot  = oop:start_new(tBinaryProtocol, [Trans]),

    ?R0(Trans, effectful_open),

    Client = calculator_thrift:new(Prot),

    calculator_thrift:ping(Client),
    p("ping"),

 %%
 %%   sum = client.add(1,1)
 %%   print "1+1=", sum, "\n"
 %%
 %%   sum = client.add(1,4)
 %%   print "1+4=", sum, "\n"
 %%
 %%   work = Work.new()
 %%
 %%   work.op = Operation::SUBTRACT
 %%   work.num1 = 15
 %%   work.num2 = 10
 %%   diff = client.calculate(1, work)
 %%   print "15-10=", diff, "\n"
 %%
 %%   log = client.getStruct(1)
 %%   print "Log: ", log.value, "\n"
 %%
 %%   begin
 %%     work.op = Operation::DIVIDE
 %%     work.num1 = 1
 %%     work.num2 = 0
 %%     quot = client.calculate(1, work)
 %%     puts "Whoa, we can divide by 0 now?"
 %%   rescue InvalidOperation => io
 %%     print "InvalidOperation: ", io.why, "\n"
 %%   end
 %%
 %%   client.zip()
 %%   print "zip\n"
 %%
 %%   transport.close()
 %%
 %% rescue TException => tx
 %%   print 'TException: ', tx.message, "\n"
 %% end
 %%
 %%
 %%
    %% XXX if we don't close, the connection doesn't time out on the other side
    ?R0(Trans, effectful_close),

    ok.
