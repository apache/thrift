#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

# SimpleServer serves one connection at a time. A failure while serving one
# connection - a request that cannot be dispatched, a transport that fails to
# close, or a reply to a client that has gone - must end only that connection;
# the server must go on accepting.

use strict;
use warnings;

use POSIX ();
use Test::More tests => 11;

use Thrift;
use Thrift::Type;
use Thrift::MessageType;
use Thrift::BinaryProtocol;
use Thrift::MemoryBuffer;
use Thrift::MultiplexedProcessor;
use Thrift::Server;

# A registered service whose only job is to record that it was reached.
package CountingService;
our $served = 0;
sub new { return bless {}, shift }
sub process {
    my ($self, $input, $output) = @_;
    my ($name, $type, $seqid);
    $input->readMessageBegin(\$name, \$type, \$seqid);
    $input->skip(Thrift::TType::STRUCT);
    $input->readMessageEnd();
    $served++;
    return 1;
}

# A registered service whose handler dies with a plain reference rather than an
# exception object or a message.
package RefDyingService;
sub new { return bless {}, shift }
sub process { die { code => 400, reason => 'refused' } }

# An accepted connection: reads come from the request it was built with, writes
# are collected apart from it, and close() is counted. A connection may be asked
# to fail on close, the way a socket close on a broken connection can.
package FakeConnection;
sub new {
    my ($class, $request, %opt) = @_;
    my $in = Thrift::MemoryBuffer->new();
    $in->resetBuffer($request);
    return bless {
        in         => $in,
        out        => Thrift::MemoryBuffer->new(),
        closed     => 0,
        close_dies => $opt{close_dies},
    }, $class;
}
sub isOpen  { return 1 }
sub open    { }
sub flush   { }
sub read    { my ($self, $len) = @_; return $self->{in}->read($len) }
sub readAll { my ($self, $len) = @_; return $self->{in}->readAll($len) }
sub write   { my ($self, $buf) = @_; return $self->{out}->write($buf) }
sub close {
    my $self = shift;
    $self->{closed}++;
    if ($self->{close_dies} and $self->{close_dies} eq 'ref') {
        die [ 'could not close the connection' ];
    }
    if ($self->{close_dies}) {
        die Thrift::TTransportException->new('Thrift::Socket: could not close the connection',
            Thrift::TTransportException::UNKNOWN);
    }
}

# Hands out the prepared connections, one per accept(), then reports the end of
# input with undef the way a listening socket does when it is shut down.
package FakeServerTransport;
use parent -norequire, 'Thrift::ServerTransport';
sub new {
    my ($class, $connections) = @_;
    return bless { connections => $connections, accepted => 0 }, $class;
}
sub listen { }
sub close  { }
sub accept {
    my $self = shift;
    return undef unless @{$self->{connections}};
    $self->{accepted}++;
    return shift @{$self->{connections}};
}

package main;

# A multiplexed call as MultiplexedProtocol puts it on the wire: the service
# name, a separator and the message name, followed by an empty argument struct.
sub multiplexed_call {
    my ($service, $method) = @_;
    my $buffer = Thrift::MemoryBuffer->new();
    my $protocol = Thrift::BinaryProtocol->new($buffer);
    $protocol->writeMessageBegin($service . Thrift::MultiplexedProtocol::SEPARATOR . $method,
        Thrift::TMessageType::CALL, 1);
    $protocol->writeStructBegin('args');
    $protocol->writeFieldStop();
    $protocol->writeStructEnd();
    $protocol->writeMessageEnd();
    return $buffer->getBuffer();
}

my $processor = Thrift::MultiplexedProcessor->new();
$processor->registerProcessor('GoodService', CountingService->new());

# First a call whose service name is not registered - and is spelled so that it
# appears in the "service not found" message the multiplexer raises - then two
# ordinary calls, the second of which also fails when its connection is closed.
my @connections = (
    FakeConnection->new(multiplexed_call('TTransportException', 'doWork')),
    FakeConnection->new(multiplexed_call('GoodService', 'doWork')),
    FakeConnection->new(multiplexed_call('GoodService', 'doWork'), close_dies => 1),
    FakeConnection->new(multiplexed_call('GoodService', 'doWork')),
);
my $server_transport = FakeServerTransport->new([@connections]);

my $server = Thrift::SimpleServer->new(
    $processor, $server_transport,
    Thrift::TransportFactory->new(), Thrift::TransportFactory->new(),
    Thrift::BinaryProtocolFactory->new(), Thrift::BinaryProtocolFactory->new(),
);

$CountingService::served = 0;
my $error;
{
    local $SIG{__WARN__} = sub { };   # the per-connection end of input is logged; keep it quiet
    eval { $server->serve(); };
    $error = $@;
}

is($error, '', 'serve() returns after connections fail instead of propagating the failure');
is($server_transport->{accepted}, 4, 'the server accepts every connection');
is($CountingService::served, 3, 'every registered call is served');
is($connections[2]->{closed} > 0, 1, 'a connection that fails to close is still closed');

# A connection can also end with something that is neither an exception object
# nor a message: a handler that dies with a plain reference, or a close that
# does. That too ends only that connection.
{
    my $processor = Thrift::MultiplexedProcessor->new();
    $processor->registerProcessor('GoodService', CountingService->new());
    $processor->registerProcessor('RefService', RefDyingService->new());

    my @connections = (
        FakeConnection->new(multiplexed_call('RefService', 'doWork')),
        FakeConnection->new(multiplexed_call('GoodService', 'doWork'), close_dies => 'ref'),
        FakeConnection->new(multiplexed_call('GoodService', 'doWork')),
    );
    my $server_transport = FakeServerTransport->new([@connections]);

    my $server = Thrift::SimpleServer->new(
        $processor, $server_transport,
        Thrift::TransportFactory->new(), Thrift::TransportFactory->new(),
        Thrift::BinaryProtocolFactory->new(), Thrift::BinaryProtocolFactory->new(),
    );

    $CountingService::served = 0;
    my $error;
    {
        local $SIG{__WARN__} = sub { };
        eval { $server->serve(); };
        $error = $@;
    }

    is($error, '', 'serve() returns after a connection ends with a plain reference');
    is($server_transport->{accepted}, 3, 'the server accepts every connection after a plain-reference failure');
    is($CountingService::served, 2, 'the calls after a plain-reference failure are served');
    is($connections[1]->{closed} > 0, 1, 'a connection whose close dies with a plain reference is still closed');
}

# A registered service that answers each call.
package ReplyingService;
sub new { return bless {}, shift }
sub process {
    my ($self, $input, $output) = @_;
    my ($name, $type, $seqid);
    $input->readMessageBegin(\$name, \$type, \$seqid);
    $input->skip(Thrift::TType::STRUCT);
    $input->readMessageEnd();
    $output->writeMessageBegin($name, Thrift::TMessageType::REPLY, $seqid);
    $output->writeStructBegin('result');
    $output->writeFieldStop();
    $output->writeStructEnd();
    $output->writeMessageEnd();
    $output->getTransport()->flush();
    $CountingService::served++;
    return 1;
}

# A connection whose client has gone. Writing to it raises SIGPIPE, as a write
# to a TLS socket whose peer has reset the connection does; where that signal
# is ignored, the write fails the way such a write then does.
package GoneConnection;
use parent -norequire, 'FakeConnection';
sub write {
    kill 'PIPE', $$;
    die Thrift::TTransportException->new('Thrift::SSLSocket: Could not write',
        Thrift::TTransportException::END_OF_FILE);
}

package main;

# Serves a call to a client that has gone and then an ordinary one, in a child
# process so that the test can see how the server ends. The child's exit code
# is what $report returns. Returns the child's wait status.
sub serve_gone_client_in_child {
    my ($report, $setup) = @_;
    my $pid = fork();
    die "fork: $!" unless defined $pid;
    if ($pid == 0) {
        $setup->() if $setup;
        my $processor = Thrift::MultiplexedProcessor->new();
        $processor->registerProcessor('ReplyService', ReplyingService->new());
        my $server_transport = FakeServerTransport->new([
            GoneConnection->new(multiplexed_call('ReplyService', 'doWork')),
            FakeConnection->new(multiplexed_call('ReplyService', 'doWork')),
        ]);
        my $server = Thrift::SimpleServer->new(
            $processor, $server_transport,
            Thrift::TransportFactory->new(), Thrift::TransportFactory->new(),
            Thrift::BinaryProtocolFactory->new(), Thrift::BinaryProtocolFactory->new(),
        );
        $CountingService::served = 0;
        local $SIG{__WARN__} = sub { };
        eval { $server->serve(); };
        POSIX::_exit($report->());
    }
    waitpid($pid, 0);
    return $?;
}

{
    my $status = serve_gone_client_in_child(sub { $CountingService::served });
    is($status & 127, 0, 'a reply that raises SIGPIPE does not end the server process');
    is($status >> 8, 1, 'the call after it is served');
}

# A SIGPIPE handler of the application's own stays in place while it serves.
{
    my $pipes = 0;
    my $status = serve_gone_client_in_child(sub { 10 * $pipes + $CountingService::served },
                                            sub { $SIG{PIPE} = sub { $pipes++ } });
    is($status, (10 * 1 + 1) << 8, "the application's own SIGPIPE handler is called, and the next call served");
}
