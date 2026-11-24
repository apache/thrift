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
# Thrift::Socket::write() waits for room in the socket at most sendTimeout
# milliseconds, also when the data does not fit into the room there is: a
# peer that stops reading ends the write with TIMED_OUT.
#

use strict;
use warnings;

use IO::Socket::INET;
use POSIX ();
use Socket qw(SOL_SOCKET SO_RCVBUF);
use Test::More tests => 2;
use Time::HiRes qw(time);

use Thrift::Socket;
use Thrift::Transport;

# A peer that accepts the connection and never reads from it. Its small
# receive buffer keeps what the connection can hold small.
my $listener = IO::Socket::INET->new(LocalAddr => '127.0.0.1', LocalPort => 0, Listen => 1, ReuseAddr => 1)
    or die "listen: $!";
setsockopt($listener, SOL_SOCKET, SO_RCVBUF, 4096) or die "setsockopt: $!";
my $pid = fork();
die "fork: $!" unless defined $pid;
if ($pid == 0) {
    my $peer = $listener->accept();
    sleep 30;
    POSIX::_exit(0);
}

my $socket = Thrift::Socket->new('127.0.0.1', $listener->sockport);
$socket->setSendTimeout(500);
$socket->open();

my $error;
my $start = time;
eval {
    local $SIG{ALRM} = sub { die "still writing after 10 seconds\n" };
    alarm 10;
    $socket->write('x' x 16_000_000);
    alarm 0;
    1;
} or do {
    alarm 0;
    $error = $@;
};
my $elapsed = time - $start;
kill 'KILL', $pid;
waitpid($pid, 0);

ok(ref($error) && $error->isa('Thrift::TTransportException')
       && $error->{code} == Thrift::TTransportException::TIMED_OUT,
   'a write the peer does not read ends with TIMED_OUT')
    or diag(ref($error) ? $error->{message} : $error // 'the write returned');
ok($elapsed < 5, sprintf('it ends soon after the send timeout (%.1f s)', $elapsed));
