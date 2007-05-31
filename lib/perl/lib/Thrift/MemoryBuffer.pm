#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/
#
#  package - thrift.transport.memorybuffer
#  author  - T Jake Luciani <jakers@gmail.com>
#  author  - Mark Slee      <mcslee@facebook.com>
#

require 5.6.0;
use strict;
use warnings;

use Thrift;
use Thrift::Transport;

package Thrift::MemoryBuffer;
use base('Thrift::Transport');

sub new
{
    my $classname = shift;

    my $bufferSize= shift || 1024;

    my $self = {
        buffer    => '',
        bufferSize=> $bufferSize,
        wPos      => 0,
        rPos      => 0,
    };

    return bless($self,$classname);
}

sub isOpen
{
    return 1;
}

sub open
{

}

sub close
{

}

sub peek
{
    my $self = shift;
    return($self->{rPos} < $self->{wPos});
}


sub getBuffer
{
    my $self = shift;
    return $self->{buffer};
}

sub resetBuffer
{
    my $self = shift;

    my $new_buffer  = shift || '';

    $self->{buffer}     = $new_buffer;
    $self->{bufferSize} = length($new_buffer);
    $self->{wPos}       = length($new_buffer);
    $self->{rPos}       = 0;
}

sub available
{
    my $self = shift;
    return ($self->{wPos} - $self->{rPos});
}

sub read
{
    my $self = shift;
    my $len  = shift;
    my $ret;

    my $avail = ($self->{wPos} - $self->{rPos});
    return '' if $avail == 0;

    #how much to give
    my $give = $len;
    $give = $avail if $avail < $len;

    $ret = substr($self->{buffer},$self->{rPos},$give);

    $self->{rPos} += $give;

    return $ret;
}

sub write
{
    my $self = shift;
    my $buf  = shift;

    $self->{buffer} .= $buf;
    $self->{wPos}   += length($buf);
}

sub flush
{

}

1;
