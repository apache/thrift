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
# Thrift::BinaryProtocol reads a string's length off the wire and asks the
# transport for that many bytes. These tests hold it to a maximum string size,
# and check that a refused length is never asked of the transport.
#

use strict;
use warnings;

use Test::More tests => 14;
use Test::Exception;

use Thrift::BinaryProtocol;
use Thrift::MemoryBuffer;

package RecordingBuffer;
use base('Thrift::MemoryBuffer');

sub new {
    my $class = shift;
    my $self  = Thrift::MemoryBuffer::new($class, @_);
    $self->{requested} = [];
    return $self;
}

sub readAll {
    my ($self, $len) = @_;
    push @{$self->{requested}}, $len;
    return $self->SUPER::readAll($len);
}

package main;

my $LIMIT = 16384000;
my $HUGE  = 0x7fffffff;

sub protocol_over {
    my ($bytes, @args) = @_;
    my $trans = RecordingBuffer->new();
    $trans->resetBuffer($bytes);
    return ($trans, Thrift::BinaryProtocol->new($trans, @args));
}

sub asked_for {
    my ($trans, $len) = @_;
    return scalar grep { $_ >= $len } @{$trans->{requested}};
}

is(eval { Thrift::BinaryProtocol->DEFAULT_MAX_STRING_SIZE }, $LIMIT,
   'the default maximum string size is exported');

{
    my ($trans, $proto) = protocol_over(pack('N', $HUGE));
    my $s;
    throws_ok { $proto->readString(\$s) } qr/larger than the maximum/,
        'a string over the default maximum is refused';
    is($@->{code}, Thrift::TProtocolException::SIZE_LIMIT, 'with the SIZE_LIMIT code');
    ok(!asked_for($trans, $HUGE), 'and its length is never asked of the transport');
}

{
    my ($trans, $proto) = protocol_over(pack('N', $LIMIT + 1));
    my $s;
    throws_ok { $proto->readString(\$s) } qr/larger than the maximum/,
        'one byte over the default maximum is refused';
    ($trans, $proto) = protocol_over(pack('N', $LIMIT));
    dies_ok { $proto->readString(\$s) } 'a string at the maximum is read (and runs out here)';
    ok(asked_for($trans, $LIMIT), 'a string at the maximum is asked of the transport');
}

{
    # Without a version header the first word is the method name's length. Any
    # word with bits in 0xffff0000 is taken for a version instead, so an
    # old-style name is shorter than 65536 bytes by construction, and only a
    # caller-set maximum below that can refuse one.
    my ($trans, $proto) = protocol_over(pack('N', 101) . ('x' x 101) . pack('c', 1) . pack('N', 7), 100);
    my ($name, $type, $seqid);
    throws_ok { $proto->readMessageBegin(\$name, \$type, \$seqid) } qr/larger than the maximum/,
        'an old-style message name over a caller-set maximum is refused';
    ok(!asked_for($trans, 101), 'and is never asked of the transport');
}

{
    my ($trans, $proto) = protocol_over(pack('N', 101) . ('x' x 101), 100);
    my $s;
    throws_ok { $proto->readString(\$s) } qr/larger than the maximum/,
        'a caller-set maximum is honoured';
    ($trans, $proto) = protocol_over(pack('N', 100) . ('x' x 100), 100);
    $proto->readString(\$s);
    is($s, 'x' x 100, 'a string at a caller-set maximum still reads');
}

{
    my ($trans, $proto) = protocol_over(pack('N', $LIMIT + 1), 0);
    my $s;
    dies_ok { $proto->readString(\$s) } 'a maximum of 0 reads any length (and runs out here)';
    ok(asked_for($trans, $LIMIT + 1), 'so the declared length is asked of the transport');
}

is(Thrift::BinaryProtocolFactory->new(100)->getProtocol(RecordingBuffer->new())->{maxStringSize}, 100,
   'the factory passes its maximum to the protocol');
