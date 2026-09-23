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

use strict;
use warnings;

use Test::More tests => 6;

use Thrift::BinaryProtocol;
use Thrift::FramedTransport;
use Thrift::MemoryBuffer;
use Thrift::Type;

#
# A scalar that adds up the length of every value assigned to it.
#
package StoredBytes;

sub TIESCALAR
{
    my ($class, $value) = @_;
    return bless { value => $value, stored => 0 }, $class;
}

sub FETCH
{
    my $self = shift;
    return $self->{value};
}

sub STORE
{
    my ($self, $value) = @_;
    $self->{stored} += length($value);
    $self->{value} = $value;
    return;
}

package main;

# Bytes that differ from one position to the next, so a byte returned twice,
# skipped or out of order fails the comparison.
sub sample
{
    my ($n, $seed) = @_;
    return join('', map { chr(($_ * 31 + $seed * 7) % 256) } 1 .. $n);
}

# A framed transport that reads the given frames.
sub framed_over
{
    my @frames = @_;
    my $inner = Thrift::MemoryBuffer->new();
    $inner->resetBuffer(join('', map { pack('N', length($_)) . $_ } @frames));
    return Thrift::FramedTransport->new($inner);
}

my @frameSizes = (1, 5, 0, 300, 4096, 17, 70000, 3);
my @frames = map { sample($frameSizes[$_], $_) } 0 .. $#frameSizes;
my @readSizes = (1, 2, 3, 7, 64, 1000, 5000, 13);

# read() returns at most what is left of the current frame, and starts on the
# next frame once that one is used up. An empty frame reads as ''.
{
    my $framed = framed_over(@frames);
    my ($frame, $pos, $reads, $wrong) = (-1, 0, 0, 0);
    while ($frame < $#frames || $pos < length($frames[$frame])) {
        if ($frame < 0 || $pos == length($frames[$frame])) {
            $frame++;
            $pos = 0;
        }
        my $len  = $readSizes[$reads++ % @readSizes];
        my $want = substr($frames[$frame], $pos, $len);
        $wrong++ unless $framed->read($len) eq $want;
        $pos += length($want);
    }
    is($wrong, 0, "$reads reads of assorted sizes return each frame's bytes in order");
}

# readAll() carries on into the next frame until it has what it asked for.
{
    my $framed = framed_over(@frames);
    my $expected = join('', @frames);
    my ($got, $calls) = ('', 0);
    while (length($got) < length($expected)) {
        my $len  = 3 * $readSizes[$calls++ % @readSizes];
        my $left = length($expected) - length($got);
        $got .= $framed->readAll($len < $left ? $len : $left);
    }
    ok($got eq $expected, "$calls readAll() calls across frame boundaries return the bytes in order");
}

# A list skipped inside a frame leaves the protocol at the field after it.
{
    my $buffer = Thrift::MemoryBuffer->new();
    my $out = Thrift::FramedTransport->new($buffer);
    my $proto = Thrift::BinaryProtocol->new($out);
    $proto->writeListBegin(Thrift::TType::BYTE, 3000);
    $proto->writeByte($_ % 100) for 1 .. 3000;
    $proto->writeListEnd();
    $proto->writeI32(123456789);
    $proto->writeString('after the list');
    $out->flush();

    $proto = Thrift::BinaryProtocol->new(Thrift::FramedTransport->new($buffer));
    my ($marker, $text);
    $proto->skip(Thrift::TType::LIST);
    $proto->readI32(\$marker);
    $proto->readString(\$text);
    is($marker, 123456789, 'a list skipped inside a frame leaves the protocol at the next field');
    is($text, 'after the list', 'the string after the list is read in full');
}

# Reading a frame one byte at a time copies the frame into the read buffer
# once, however many reads it takes.
{
    my $size = 20000;
    my $frame = sample($size, 99);
    my $framed = framed_over($frame);
    my $buffer = tie $framed->{rBuf}, 'StoredBytes', $framed->{rBuf};

    my $got = '';
    $got .= $framed->read(1) for 1 .. $size;
    ok($got eq $frame, 'one-byte reads return the whole frame');
    cmp_ok($buffer->{stored}, '<=', 2 * $size,
        "one-byte reads of a $size-byte frame store at most twice its size in the read buffer");
}
