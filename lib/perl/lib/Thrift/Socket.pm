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
    my $classname    = shift;
    my $host         = shift || "localhost";
    my $port         = shift || 9090;
    my $debugHandler = shift;

    my $self = {
        host         => $host,
        port         => $port,
        debugHandler => $debugHandler,
        debug        => 0,
        sendTimeout  => 10000,
        recvTimeout  => 10000,
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
        CORE::close( ($self->{handle}->handles())[0] );
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
    my @sockets = $self->{handle}->can_read( $self->{recvTimeout} / 1000 );

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
        my @sockets = $self->{handle}->can_write( $self->{sendTimeout} / 1000 );

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


#
# Build a ServerSocket from the ServerTransport base class
#
package  Thrift::ServerSocket;

use base qw( Thrift::Socket Thrift::ServerTransport );

use constant LISTEN_QUEUE_SIZE => 128;

sub new
{
    my $classname   = shift;
    my $port        = shift;

    my $self        = $classname->SUPER::new(undef, $port, undef);
    return bless($self,$classname);
}

sub listen
{
    my $self = shift;

    # Listen to a new socket
    my $sock = IO::Socket::INET->new(LocalAddr => undef, # any addr
                                     LocalPort => $self->{port},
                                     Proto     => 'tcp',
                                     Listen    => LISTEN_QUEUE_SIZE,
                                     ReuseAddr => 1)
        || do {
            my $error = 'TServerSocket: Could not bind to ' .
                        $self->{host} . ':' . $self->{port} . ' (' . $! . ')';

            if ($self->{debug}) {
                $self->{debugHandler}->($error);
            }

            die new Thrift::TException($error);
        };

    $self->{handle} = $sock;
}

sub accept
{
    my $self = shift;

    if ( exists $self->{handle} and defined $self->{handle} )
    {
        my $client        = $self->{handle}->accept();
        my $result        = new Thrift::Socket;
        $result->{handle} = new IO::Select($client);
        return $result;
    }

    return 0;
}


1;
