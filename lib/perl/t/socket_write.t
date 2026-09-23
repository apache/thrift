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
# Thrift::Socket::write() sends a buffer in as many calls as the socket needs.
# A TLS socket takes one record per call. These tests run write() over
# stand-in sockets that take a few bytes per call, through the __send() of
# Thrift::Socket and of Thrift::SSLSocket.
#

use strict;
use warnings;

use Config;
use Symbol qw(gensym);
use Test::More tests => 12;

use Thrift::Socket;

# The checks below take the address of a string with pack('p'), which warns
# when it is handed a temporary. Treat any warning as a failure.
$SIG{__WARN__} = sub { die "unexpected warning: $_[0]" };

# Unpacks a pointer, as pack('p') packs it, into a number.
my $POINTER = $Config{ptrsize} == $Config{uvsize} ? 'J' : 'L';

#
# Stands in for the connection under a socket. Each call accepts the next of a
# list of sizes and keeps the bytes it accepted. It also counts the calls whose
# data does not start where the previous call's accepted bytes ended, in the
# same memory: calls that were handed a copy of the unsent bytes.
#
package PartialSink;

sub new
{
    my ($class, @sizes) = @_;
    return bless { sizes => \@sizes, calls => 0, copied => 0, next => undef, received => '' }, $class;
}

# Takes the data as $_[0] and uses it in place, so that nothing here copies it.
# $len and $offset are those of syswrite().
sub take
{
    my $self = shift;
    my (undef, $len, $offset) = @_;
    $offset = 0 unless defined $offset;
    $len = length($_[0]) - $offset unless defined $len;

    my $start = main::address_of($_[0]) + $offset;
    my $n = $self->{sizes}[$self->{calls}++ % @{$self->{sizes}}];
    $n = $len if $n > $len;
    $self->{copied}++ if defined $self->{next} && $start != $self->{next};
    $self->{next} = $start + $n;
    $self->{received} .= substr($_[0], $offset, $n);
    return $n;
}

#
# What Thrift::Socket sends to: its __send() calls send() on the socket.
#
package SinkSocket;
use base qw(PartialSink);

sub send
{
    my $self = shift;
    return $self->take($_[0]);
}

#
# What Thrift::SSLSocket writes to: its __send() calls syswrite() on the
# socket, which IO::Socket::SSL ties to reach its own syswrite().
#
package SinkHandle;

sub TIEHANDLE
{
    my ($class, $sink) = @_;
    return bless { sink => $sink }, $class;
}

sub WRITE
{
    my $self = shift;
    return $self->{sink}->take(@_);
}

#
# Stands in for the IO::Select that a socket keeps: always ready to write.
#
package WritableSelect;

sub new
{
    my ($class, $sock) = @_;
    return bless { sock => $sock }, $class;
}

sub can_write
{
    my $self = shift;
    return ($self->{sock});
}

package main;

# Where the bytes of a string are in memory. Takes the string as $_[0], so
# finding out does not copy it.
sub address_of
{
    return unpack($POINTER, pack('p', $_[0]));
}

# Bytes that differ from one position to the next, so a byte sent twice,
# skipped or out of order fails the comparison.
sub sample
{
    my ($n, $seed) = @_;
    return join('', map { chr(($_ * 31 + $seed * 7) % 256) } 1 .. $n);
}

# A socket of the given class that writes to the given stand-in.
sub socket_over
{
    my ($class, $sock) = @_;
    my $socket = $class->new();
    $socket->{handle} = WritableSelect->new($sock);
    return $socket;
}

# A Thrift::Socket, and the sink under it that accepts the given sizes.
sub plain_socket
{
    my $sink = SinkSocket->new(@_);
    return (socket_over('Thrift::Socket', $sink), $sink);
}

# A Thrift::SSLSocket, and the sink under it that accepts the given sizes.
sub tls_socket
{
    my $sink = PartialSink->new(@_);
    my $handle = gensym();
    tie *$handle, 'SinkHandle', $sink;
    return (socket_over('Thrift::SSLSocket', $handle), $sink);
}

# The sizes are all odd. A copy of the data is in newly allocated memory, which
# is aligned to at least two bytes, so it cannot start an odd number of bytes
# after the previous call's data. Only data that write() left where it was
# starts exactly where the previous call's accepted bytes ended.
#
# write() works on a copy of the caller's string that shares its bytes until it
# is first changed, so the first time write() drops sent bytes, the rest are
# copied once. After that they stay where they are: at most one call counts.
my @sendSizes = (1, 3, 7, 13, 999, 1001, 4097, 16383);
my $size = 300001;

{
    my $message = sample($size, 1);
    my ($socket, $sink) = plain_socket(@sendSizes);
    $socket->write($message);
    ok($sink->{received} eq $message, "Thrift::Socket: $sink->{calls} partial sends deliver the message in order");
    cmp_ok($sink->{copied}, '<=', 1,
        "Thrift::Socket: at most one of $sink->{calls} partial sends is handed a copy of the unsent bytes");
    ok($message eq sample($size, 1), 'Thrift::Socket: write() leaves the caller\'s buffer as it was');
}

SKIP: {
    skip 'IO::Socket::SSL is required', 3
        unless eval { local $SIG{__WARN__} = 'DEFAULT'; require Thrift::SSLSocket; 1 };

    my $message = sample($size, 2);
    my ($socket, $sink) = tls_socket(@sendSizes);
    $socket->write($message);
    ok($sink->{received} eq $message, "Thrift::SSLSocket: $sink->{calls} partial sends deliver the message in order");
    cmp_ok($sink->{copied}, '<=', 1,
        "Thrift::SSLSocket: at most one of $sink->{calls} partial sends is handed a copy of the unsent bytes");
    ok($message eq sample($size, 2), 'Thrift::SSLSocket: write() leaves the caller\'s buffer as it was');
}

# Consecutive writes arrive one after the other, and an empty write sends
# nothing.
{
    my ($socket, $sink) = plain_socket(@sendSizes);
    my @messages = (sample(70001, 3), sample(5, 4), sample(20001, 5));
    $socket->write($_) for @messages;
    ok($sink->{received} eq join('', @messages), 'consecutive writes arrive in order');

    my $calls = $sink->{calls};
    $socket->write('');
    is($sink->{calls}, $calls, 'an empty write makes no send call');
}

# A send that takes everything at once is a single call.
{
    my ($socket, $sink) = plain_socket($size);
    $socket->write(sample($size, 6));
    is($sink->{calls}, 1, 'a send that takes the whole buffer is the only call');
}

# A send that accepts nothing fails the write, which reports how much was left.
{
    my ($socket, $sink) = plain_socket(5, 0);
    eval { $socket->write(sample(100, 7)); };
    my $error = $@;
    ok(ref($error) && $error->isa('Thrift::TTransportException')
        && $error->{code} == Thrift::TTransportException::END_OF_FILE,
        'a send that accepts nothing fails with END_OF_FILE');
    like(ref($error) ? $error->{message} : '', qr/Could not write 95 bytes/,
        'the error reports the bytes that were not sent');
    is($sink->{received}, sample(5, 7), 'the bytes accepted before that were sent in order');
}
