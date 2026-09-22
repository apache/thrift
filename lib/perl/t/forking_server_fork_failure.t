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

# ForkingServer forks a process per connection. When a fork fails there is no
# child to serve the connection, so the parent must close it and keep accepting
# rather than run the child path - which ends by exiting - in the parent itself.

use strict;
use warnings;

BEGIN {
    # Make fork() report failure, but only while the flag is set, so the test
    # can still fork its own worker with CORE::fork. Installed before
    # Thrift::Server is compiled so the server's fork() sees the override.
    *CORE::GLOBAL::fork = sub {
        return $main::simulate_fork_failure ? undef : CORE::fork();
    };
}

use Config;
use Test::More;

use Thrift;
use Thrift::Type;
use Thrift::MessageType;
use Thrift::BinaryProtocol;
use Thrift::Transport;
use Thrift::MemoryBuffer;
use Thrift::Server;

if (!$Config{d_fork}) {
    plan skip_all => 'this platform has no fork';
}
plan tests => 2;

# A processor whose dispatch reads from the connection, so that on an empty
# connection it ends with a transport error the way a real one would.
package DummyProcessor;
sub new { return bless {}, shift }
sub process {
    my ($self, $input, $output) = @_;
    my ($name, $type, $seqid);
    $input->readMessageBegin(\$name, \$type, \$seqid);
    return 1;
}

# An accepted connection with no request waiting on it.
package EmptyConnection;
sub new {
    my $class = shift;
    my $in = Thrift::MemoryBuffer->new();
    $in->resetBuffer('');
    return bless { in => $in }, $class;
}
sub isOpen  { return 1 }
sub open    { }
sub flush   { }
sub close   { }
sub read    { my ($self, $len) = @_; return $self->{in}->read($len) }
sub readAll { my ($self, $len) = @_; return $self->{in}->readAll($len) }
sub write   { }

# Hands out one connection, then breaks the accept loop the way a shut-down
# listening socket would, by raising instead of blocking forever.
package OneShotServerTransport;
use parent -norequire, 'Thrift::ServerTransport';
sub new {
    my ($class, $connection) = @_;
    return bless { connection => $connection, accepted => 0 }, $class;
}
sub listen { }
sub close  { }
sub accept {
    my $self = shift;
    if (defined $self->{connection}) {
        $self->{accepted}++;
        my $connection = $self->{connection};
        $self->{connection} = undef;
        return $connection;
    }
    die "OneShotServerTransport: no more connections\n";
}

package main;

my $transport = OneShotServerTransport->new(EmptyConnection->new());
my $server = Thrift::ForkingServer->new(
    DummyProcessor->new(), $transport,
    Thrift::TransportFactory->new(), Thrift::TransportFactory->new(),
    Thrift::BinaryProtocolFactory->new(), Thrift::BinaryProtocolFactory->new(),
);

# Run the server in a worker process: if the fork-failure path ends the process
# (by running the child path in the parent) the worker exits non-zero; if the
# server keeps accepting it runs out of connections and the worker exits zero.
my $worker = CORE::fork();
if (defined $worker && $worker == 0) {
    $main::simulate_fork_failure = 1;
    local $SIG{__WARN__} = sub { };
    eval { $server->serve(); };
    exit 0;
}

ok(defined $worker && $worker > 0, 'forked a worker process to run the server');
waitpid($worker, 0);
is($?, 0, 'the server keeps accepting after a fork failure and exits cleanly');
