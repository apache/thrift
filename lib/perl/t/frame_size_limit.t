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
use Test::Exception;

use Thrift::FramedTransport;
use Thrift::MemoryBuffer;

# A frame whose declared length is over the configured maximum is refused
# before the body is read, so only the four length bytes need to be present.
{
    my $limit = 100;
    my $declared = $limit + 1;
    my $inner = Thrift::MemoryBuffer->new();
    $inner->resetBuffer(pack('N', $declared));
    my $framed = Thrift::FramedTransport->new($inner, 1, 1, $limit);

    throws_ok { $framed->read(5) }
        qr/\Q$declared\E.*\Q$limit\E/,
        'a frame over the configured maximum is rejected, naming both sizes';
}

# The default maximum is in force when no limit is passed.
{
    my $declared = Thrift::FramedTransport::DEFAULT_MAX_FRAME_SIZE + 1;
    my $inner = Thrift::MemoryBuffer->new();
    $inner->resetBuffer(pack('N', $declared));
    my $framed = Thrift::FramedTransport->new($inner);

    throws_ok { $framed->read(5) }
        qr/larger than the maximum/,
        'a frame over the default maximum is rejected';
}

# The rejection carries the SIZE_LIMIT code.
{
    my $inner = Thrift::MemoryBuffer->new();
    $inner->resetBuffer(pack('N', 11));
    my $framed = Thrift::FramedTransport->new($inner, 1, 1, 10);

    eval { $framed->read(1) };
    my $err = $@;
    isa_ok($err, 'Thrift::TTransportException');
    is($err->{code}, Thrift::TTransportException::SIZE_LIMIT,
        'the exception carries the SIZE_LIMIT code');
}

# A frame exactly at the maximum is read in full.
{
    my $limit = 100;
    my $body = 'x' x $limit;
    my $inner = Thrift::MemoryBuffer->new();
    $inner->resetBuffer(pack('N', $limit) . $body);
    my $framed = Thrift::FramedTransport->new($inner, 1, 1, $limit);

    my $out;
    lives_ok { $out = $framed->read($limit) }
        'a frame exactly at the maximum is accepted';
    is($out, $body, 'the whole frame body is returned');
}
