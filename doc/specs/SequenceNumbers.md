# Sequence Number #

Apache Thrift built sequence numbers into every protocol exchange to allow
for clients that may submit multiple outstanding requests on a single transport
connection.  This is typically done by asynchronous clients.

The following rules apply to sequence numbers:

1. A sequence number is a signed 32-bit integer.  Negative values are allowed.
1. Sequence numbers `MUST` be unique across all outstanding requests on a
   given transport connection.  There is no requirement for unique numbers
   between different transport connections even if they are from the same client.
1. A server `MUST` reply to a client with the same sequence number that was
   used in the request.  This includes any exception-based reply.
1. A client `MAY` use sequence numbers if it needs them for proper operation.
1. A client `SHOULD` set the sequence number to zero if it does not rely
   on them.
1. Wrapped protocols (such as THeaderProtocol) `SHOULD` use the same sequence
   number on the wrapping as is used on the payload protocol.

Servers will not inspect or make any logic choices based on the sequence number
sent by the client.  The server's only job is to process the request and reply
with the same sequence number.
