use Test::More tests => 6;

use strict;
use warnings;

use Data::Dumper;

use Thrift::BinaryProtocol;
use Thrift::MemoryBuffer;

use ThriftTest::Types;


my $transport = Thrift::MemoryBuffer->new();
my $protocol = Thrift::BinaryProtocol->new($transport);

my $a = ThriftTest::Xtruct->new();
$a->i32_thing(10);
$a->i64_thing(30);
$a->string_thing('Hello, world!');
$a->write($protocol);

my $b = ThriftTest::Xtruct->new();
$b->read($protocol);
is($b->i32_thing, $a->i32_thing);
is($b->i64_thing, $a->i64_thing);
is($b->string_thing, $a->string_thing);

$b->write($protocol);
my $c = ThriftTest::Xtruct->new();
$c->read($protocol);
is($c->i32_thing, $a->i32_thing);
is($c->i64_thing, $a->i64_thing);
is($c->string_thing, $a->string_thing);
