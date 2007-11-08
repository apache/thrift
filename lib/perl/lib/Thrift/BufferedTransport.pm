#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/
#
#  package - thrift.transport.buffered
#  author  - T Jake Luciani <jakers@gmail.com>
#  author  - Mark Slee      <mcslee@facebook.com>
#

require 5.6.0;
use strict;
use warnings;

use Thrift;
use Thrift::Transport;

package Thrift::BufferedTransport;
use base('Thrift::Transport');

sub new
{
    my $classname = shift;
    my $transport = shift;
    my $rBufSize  = shift || 512;
    my $wBufSize  = shift || 512;

    my $self = {
        transport => $transport,
        rBufSize  => $rBufSize,
        wBufSize  => $wBufSize,
        wBuf      => '',
        rBuf      => '',
    };

    return bless($self,$classname);
}

sub isOpen
{
    my $self = shift;

    return $self->{transport}->isOpen();
}

sub open
{
    my $self = shift;
    $self->{transport}->open();
}

sub close()
{
    my $self = shift;
    $self->{transport}->close();
}

sub readAll
{
    my $self = shift;
    my $len  = shift;

    return $self->{transport}->readAll($len);
}

sub read
{
    my $self = shift;
    my $len  = shift;
    my $ret;

    # Methinks Perl is already buffering these for us
    return $self->{transport}->read($len);
}

sub write
{
    my $self = shift;
    my $buf  = shift;

    $self->{wBuf} .= $buf;
    if (length($self->{wBuf}) >= $self->{wBufSize}) {
        $self->{transport}->write($self->{wBuf});
        $self->{wBuf} = '';
    }
}

sub flush
{
    my $self = shift;

    if (length($self->{wBuf}) > 0) {
        $self->{transport}->write($self->{wBuf});
        $self->{wBuf} = '';
    }
    $self->{transport}->flush();
}


1;
