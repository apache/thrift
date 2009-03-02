#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/
#
#  package - thrift.socket
#  author  - T Jake Luciani <jakers@gmail.com>
#  author  - Mark Slee      <mcslee@facebook.com>
#

require 5.6.0;
use strict;
use warnings;

use Thrift;
use Thrift::Transport;

use IO::Socket::INET;
use IO::Select;

package Thrift::Socket;

use base('Thrift::Transport');

sub new
{
    my $classname = shift;
    my $host      = shift || "localhost";
    my $port      = shift || 9090;
    my $debugHandler = shift;

    my $self = {
        host         => $host,
        port         => $port,
        debugHandler => $debugHandler,
        debug        => 0,
        sendTimeout  => 100,
        recvTimeout  => 750,
        handle       => undef,
    };

    return bless($self,$classname);
}


sub setSendTimeout
{
    my $self    = shift;
    my $timeout = shift;

    $self->{sendTimeout} = $timeout;
}

sub setRecvTimeout
{
    my $self    = shift;
    my $timeout = shift;

    $self->{recvTimeout} = $timeout;
}


#
#Sets debugging output on or off
#
# @param bool $debug
#
sub setDebug
{
    my $self  = shift;
    my $debug = shift;

    $self->{debug} = $debug;
}

#
# Tests whether this is open
#
# @return bool true if the socket is open
#
sub isOpen
{
    my $self = shift;

    if( defined $self->{handle} ){
        return ($self->{handle}->handles())[0]->connected;
    }

    return 0;
}

#
# Connects the socket.
#
sub open
{
    my $self = shift;

    my $sock = IO::Socket::INET->new(PeerAddr => $self->{host},
                                            PeerPort => $self->{port},
                                            Proto    => 'tcp',
                                            Timeout  => $self->{sendTimeout}/1000)
        || do {
            my $error = 'TSocket: Could not connect to '.$self->{host}.':'.$self->{port}.' ('.$!.')';

            if ($self->{debug}) {
                $self->{debugHandler}->($error);
            }

            die new Thrift::TException($error);

        };


    $self->{handle} = new IO::Select( $sock );
}

#
# Closes the socket.
#
sub close
{
    my $self = shift;

    if( defined $self->{handle} ){
        close( ($self->{handle}->handles())[0] );
    }
}

#
# Uses stream get contents to do the reading
#
# @param int $len How many bytes
# @return string Binary data
#
sub readAll
{
    my $self = shift;
    my $len  = shift;


    return unless defined $self->{handle};

    my $pre = "";
    while (1) {

        #check for timeout
        my @sockets = $self->{handle}->can_read( $self->{recvTimeout} / 1000 );

        if(@sockets == 0){
            die new Thrift::TException('TSocket: timed out reading '.$len.' bytes from '.
                                       $self->{host}.':'.$self->{port});
        }

        my $sock = $sockets[0];

        my ($buf,$sz);
        $sock->recv($buf, $len);

        if (!defined $buf || $buf eq '') {

            die new Thrift::TException('TSocket: Could not read '.$len.' bytes from '.
                               $self->{host}.':'.$self->{port});

        } elsif (($sz = length($buf)) < $len) {

            $pre .= $buf;
            $len -= $sz;

        } else {
            return $pre.$buf;
        }
    }
}

#
# Read from the socket
#
# @param int $len How many bytes
# @return string Binary data
#
sub read
{
    my $self = shift;
    my $len  = shift;

    return unless defined $self->{handle};

    #check for timeout
    my @sockets = $self->{handle}->can_read( $self->{sendTimeout} / 1000 );

    if(@sockets == 0){
        die new Thrift::TException('TSocket: timed out reading '.$len.' bytes from '.
                                   $self->{host}.':'.$self->{port});
    }

    my $sock = $sockets[0];

    my ($buf,$sz);
    $sock->recv($buf, $len);

    if (!defined $buf || $buf eq '') {

        die new TException('TSocket: Could not read '.$len.' bytes from '.
                           $self->{host}.':'.$self->{port});

    }

    return $buf;
}


#
# Write to the socket.
#
# @param string $buf The data to write
#
sub write
{
    my $self = shift;
    my $buf  = shift;


    return unless defined $self->{handle};

    while (length($buf) > 0) {


        #check for timeout
        my @sockets = $self->{handle}->can_write( $self->{recvTimeout} / 1000 );

        if(@sockets == 0){
            die new Thrift::TException('TSocket: timed out writing to bytes from '.
                                       $self->{host}.':'.$self->{port});
        }

        my $sock = $sockets[0];

        my $got = $sock->send($buf);

        if (!defined $got || $got == 0 ) {
            die new Thrift::TException('TSocket: Could not write '.length($buf).' bytes '.
                                 $self->{host}.':'.$self->{host});
        }

        $buf = substr($buf, $got);
    }
}

#
# Flush output to the socket.
#
sub flush
{
    my $self = shift;

    return unless defined $self->{handle};

    my $ret  = ($self->{handle}->handles())[0]->flush;
}

1;
