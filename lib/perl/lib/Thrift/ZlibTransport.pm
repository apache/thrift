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

use 5.10.0;
use strict;
use warnings;
use Compress::Zlib;

use Thrift;
use Thrift::Exception;
use Thrift::Transport;

package Thrift::ZlibTransport;
use base('Thrift::Transport');
use version 0.77; our $VERSION = version->declare("$Thrift::VERSION");

sub new
{
    my $classname = shift;

    my $trans = shift;

    my $level = shift || 9;

    my $self = {
        trans    => $trans,
        level    => $level,
        rbuf     => '',
        deflater => scalar Compress::Zlib::deflateInit(-Level => $level),
        inflater => scalar Compress::Zlib::inflateInit(),
    };

    return bless($self, $classname);
}

sub isOpen
{
    my $self = shift;

    return $self->{trans}->isOpen;
}

sub open
{
    my $self = shift;

    return $self->{trans}->open;
}

sub listen
{
    my $self = shift;

    return $self->{trans}->listen;
}

sub accept
{
    my $self = shift;

    return $self->{trans}->accept;
}

sub close
{
    my $self = shift;

    return $self->{trans}->close;
}

sub read
{
    my $self = shift;
    my $len  = shift;

    while (length($self->{rbuf}) < $len) {
        my $in = $self->{trans}->read($len);
        my ($out, $status) = $self->{inflater}->inflate($in);
        if ($status != Compress::Zlib::Z_OK and $status != Compress::Zlib::Z_STREAM_END) {
            die Thrift::TTransportException->new("ZlibTransport: Failed to inflate data. Status: $status",
                    Thrift::TTransportException::END_OF_FILE);
        }
        $self->{rbuf} .= $out;
    }

    return substr($self->{rbuf}, 0, $len, '');
}

sub write
{
    my $self = shift;
    my $buf  = shift;

    my ($out, $status) = $self->{deflater}->deflate($buf);
    if ($status != Compress::Zlib::Z_OK) {
        die Thrift::TTransportException->new("ZlibTransport: Failed to deflate data. Status: $status",
                Thrift::TTransportException::END_OF_FILE);
    }

    return $self->{trans}->write($out);
}

sub flush
{
    my $self = shift;

    my ($out, $status) = $self->{deflater}->flush(Compress::Zlib::Z_SYNC_FLUSH);
    if ($status != Compress::Zlib::Z_OK) {
        die Thrift::TTransportException->new("ZlibTransport: Failed to flush buffer. Status: $status",
                Thrift::TTransportException::END_OF_FILE);
    }
    $self->{trans}->write($out);

    return $self->{trans}->flush;
}

sub getTransport
{
    my $self  = shift;
    my $trans = shift;

    return Thrift::ZlibTransport->new($trans);
}

1;
