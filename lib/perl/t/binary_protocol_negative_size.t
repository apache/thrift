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

use Test::More tests => 4;
use Test::Exception;

use Thrift::BinaryProtocol;
use Thrift::MemoryBuffer;

my $trans = Thrift::MemoryBuffer->new();
my $proto = Thrift::BinaryProtocol->new($trans);

# 0xffffffff is -1 as a signed int32 (big-endian)
my $NEG_SIZE = pack('N', 0xffffffff);

{
    $trans->resetBuffer(pack('c', 8) . pack('c', 11) . $NEG_SIZE);
    my ($kt, $vt, $sz);
    throws_ok { $proto->readMapBegin(\$kt, \$vt, \$sz) }
        qr/Negative size/,
        'readMapBegin rejects negative size';
}

{
    $trans->resetBuffer(pack('c', 11) . $NEG_SIZE);
    my ($et, $sz);
    throws_ok { $proto->readListBegin(\$et, \$sz) }
        qr/Negative size/,
        'readListBegin rejects negative size';
}

{
    $trans->resetBuffer(pack('c', 11) . $NEG_SIZE);
    my ($et, $sz);
    throws_ok { $proto->readSetBegin(\$et, \$sz) }
        qr/Negative size/,
        'readSetBegin rejects negative size';
}

{
    $trans->resetBuffer($NEG_SIZE);
    my $str;
    throws_ok { $proto->readString(\$str) }
        qr/Negative size/,
        'readString rejects negative size';
}
