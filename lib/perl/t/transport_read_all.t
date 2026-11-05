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
# readAll() asks read() for the bytes it still needs until it has them all. A
# read() that returns nothing leaves it no way to get there, and the read() of
# Thrift::FramedTransport returns nothing for an empty frame. These run
# readAll() over a transport with nothing more to give, and over frames with
# empty ones among them. Each readAll() runs under an alarm, so a loop that
# does not end fails here rather than hanging the suite.
#

use strict;
use warnings;

use Test::More tests => 10;

use Thrift::FramedTransport;
use Thrift::MemoryBuffer;
use Thrift::Transport;

#
# A transport with nothing more to give: read() returns no bytes, however
# often it is asked.
#
package DryTransport;
use base('Thrift::Transport');

sub new
{
    my $classname = shift;
    return bless({ reads => 0 }, $classname);
}

sub read
{
    my $self = shift;
    $self->{reads}++;
    return '';
}

package main;

# Runs readAll($len) on the transport under an alarm. Returns what readAll()
# returned and what it died with, which is "timed out\n" when the alarm went
# off first.
sub read_all_within
{
    my ($transport, $len) = @_;
    my $data = eval {
        local $SIG{ALRM} = sub { die "timed out\n" };
        alarm(5);
        my $got = $transport->readAll($len);
        alarm(0);
        $got;
    };
    my $err = $@;
    alarm(0);
    return ($data, $err);
}

# The code of a transport exception, or undef for anything else.
sub code_of
{
    my $err = shift;
    return ref($err) && $err->isa('Thrift::TTransportException') ? $err->{code} : undef;
}

# A framed transport that reads the given frames.
sub framed_over
{
    my @frames = @_;
    my $inner = Thrift::MemoryBuffer->new();
    $inner->resetBuffer(join('', map { pack('N', length($_)) . $_ } @frames));
    return Thrift::FramedTransport->new($inner);
}

# A read() that returns nothing is the end of the stream: asking again cannot
# bring anything either.
{
    my $dry = DryTransport->new();
    my ($data, $err) = read_all_within($dry, 4);
    isnt("$err", "timed out\n", 'readAll() ends when read() returns nothing');
    isa_ok($err, 'Thrift::TTransportException', 'what it ends with');
    is(code_of($err), Thrift::TTransportException::END_OF_FILE, 'it reports the end of the stream');
    is($dry->{reads}, 1, 'it does not repeat a read that returned nothing');
}

# An empty frame is not the end of the stream: readAll() of a framed transport
# moves on to the frame after it.
{
    my ($data, $err) = read_all_within(framed_over('abc', '', 'def'), 6);
    is($data, 'abcdef', 'readAll() reads across an empty frame between two others')
        or diag($err);
}

{
    my $framed = framed_over('abc', '', 'def');
    my ($first, $err1) = read_all_within($framed, 3);
    my ($second, $err2) = read_all_within($framed, 3);
    is($first, 'abc', 'readAll() reads the frame before an empty one')
        or diag($err1);
    is($second, 'def', 'readAll() that starts at an empty frame reads the frame after it')
        or diag($err2);
}

{
    my ($data, $err) = read_all_within(framed_over('ab', '', '', 'cd'), 4);
    is($data, 'abcd', 'readAll() reads across two empty frames in a row')
        or diag($err);
}

# Moving on past empty frames still ends where the frames do ...
{
    my ($data, $err) = read_all_within(framed_over('ab', ''), 4);
    is(code_of($err), Thrift::TTransportException::END_OF_FILE,
        'readAll() reports the end of the stream after the last frame')
        or diag($err);
}

# ... and where the transport under the frames has nothing more to give.
{
    my ($data, $err) = read_all_within(Thrift::FramedTransport->new(DryTransport->new()), 4);
    is(code_of($err), Thrift::TTransportException::END_OF_FILE,
        'readAll() of frames over a transport whose read() returns nothing reports the end of the stream')
        or diag($err);
}
