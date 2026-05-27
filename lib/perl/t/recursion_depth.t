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

# Drives the recursion-depth guard through the *generated* read()/write()
# code over a recursive struct (RecTree), union (RecUnion) and exception
# (RecError). This exercises the real serialization path, not the protocol
# counter in isolation.

use strict;
use warnings;

use Test::More;
use Test::Exception;

use Thrift;
use Thrift::BinaryProtocol;
use Thrift::MemoryBuffer;

use RecursionDepth::Types;

use constant LIMIT => Thrift::Protocol::DEFAULT_RECURSION_DEPTH;

# Build a chain of $depth nested nodes. Every node sets exactly one field:
# the inner nodes carry a single child, the deepest node carries the leaf
# scalar -- a valid shape for both the struct and the union.
sub make_chain {
    my ($class, $leaf_field, $depth) = @_;
    my $node = $class->new();
    if ($depth > 1) {
        $node->{children} = [make_chain($class, $leaf_field, $depth - 1)];
    }
    else {
        $node->{$leaf_field} = 1;
    }
    return $node;
}

sub chain_depth {
    my ($node) = @_;
    my $depth = 1;
    if (defined $node->{children} && scalar @{$node->{children}}) {
        $depth += chain_depth($node->{children}->[0]);
    }
    return $depth;
}

# Serialize an over-limit payload using raw protocol primitives so that the
# reader recurses through the guarded struct path (field id 1 = list<self>),
# rather than through the separate (unbounded) skip() path.
sub write_deep {
    my ($proto, $struct_name, $depth) = @_;
    $proto->writeStructBegin($struct_name);
    if ($depth > 1) {
        $proto->writeFieldBegin('children', Thrift::TType::LIST, 1);
        $proto->writeListBegin(Thrift::TType::STRUCT, 1);
        write_deep($proto, $struct_name, $depth - 1);
        $proto->writeListEnd();
        $proto->writeFieldEnd();
    }
    $proto->writeFieldStop();
    $proto->writeStructEnd();
}

# Run $code and return the exception it threw (or undef).
sub caught {
    my ($code) = @_;
    my $err;
    eval { $code->(); 1 } or $err = $@;
    return $err;
}

my @cases = (
    {kind => 'struct',    class => 'RecursionDepth::RecTree',  name => 'RecTree',  leaf => 'item'},
    {kind => 'union',     class => 'RecursionDepth::RecUnion', name => 'RecUnion', leaf => 'leaf'},
    {kind => 'exception', class => 'RecursionDepth::RecError', name => 'RecError', leaf => 'leaf'},
);

for my $case (@cases) {
    my ($kind, $class, $name, $leaf) = @{$case}{qw(kind class name leaf)};

    # 1. A chain exactly at the limit round-trips. This also proves the
    #    generator and protocol guards do not double-count (a chain of 64
    #    would be rejected at 32 if they did).
    {
        my $buffer = Thrift::MemoryBuffer->new();
        my $proto  = Thrift::BinaryProtocol->new($buffer);
        my $chain  = make_chain($class, $leaf, LIMIT);

        lives_ok { $chain->write($proto) }
            "$kind: writing a chain at the depth limit succeeds";

        my $decoded = $class->new();
        lives_ok { $decoded->read($proto) }
            "$kind: reading a chain at the depth limit succeeds";
        is(chain_depth($decoded), LIMIT,
            "$kind: round-trips to the original depth (${name})");
    }

    # 2. Writing past the limit is rejected with DEPTH_LIMIT.
    {
        my $buffer = Thrift::MemoryBuffer->new();
        my $proto  = Thrift::BinaryProtocol->new($buffer);
        my $chain  = make_chain($class, $leaf, LIMIT + 5);

        my $err = caught(sub { $chain->write($proto) });
        ok(ref($err) && $err->isa('Thrift::TProtocolException'),
            "$kind: writing past the limit throws TProtocolException");
        is(ref($err) ? $err->{code} : undef, Thrift::TProtocolException::DEPTH_LIMIT,
            "$kind: ... with the DEPTH_LIMIT code");
    }

    # 3. Reading an over-limit payload is rejected with DEPTH_LIMIT.
    {
        my $buffer = Thrift::MemoryBuffer->new();
        my $proto  = Thrift::BinaryProtocol->new($buffer);
        write_deep($proto, $name, LIMIT + 5);

        my $decoded = $class->new();
        my $err = caught(sub { $decoded->read($proto) });
        ok(ref($err) && $err->isa('Thrift::TProtocolException'),
            "$kind: reading past the limit throws TProtocolException");
        is(ref($err) ? $err->{code} : undef, Thrift::TProtocolException::DEPTH_LIMIT,
            "$kind: ... with the DEPTH_LIMIT code");
    }
}

done_testing();
