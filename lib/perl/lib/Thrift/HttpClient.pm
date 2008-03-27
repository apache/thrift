#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/
#
#  package - thrift.transport.http
#  based on socket transport implementation and java version of HttpClient
#  author - Igor Afanasyev <igor.afanasyev@gmail.com>
#

require 5.6.0;
use strict;
use warnings;

use Thrift;
use Thrift::Transport;

use HTTP::Request;
use LWP::UserAgent;
use IO::String;

package Thrift::HttpClient;

use base('Thrift::Transport');

sub new
{
    my $classname = shift;
    my $url       = shift || 'http://localhost:9090';
    my $debugHandler = shift;

    my $out = IO::String->new;
    binmode($out);

    my $self = {
        url          => $url,
        out          => $out,
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
    return 1;
}

sub open {}

#
# Cleans up the buffer.
#
sub close
{
    my $self = shift;
    if (defined($self->{io})) {
      close($self->{io});
      $self->{io} = undef;
    }
}

#
# Guarantees that the full amount of data is read.
#
# @return string The data, of exact length
# @throws TTransportException if cannot read data
#
sub readAll
{
    my $self = shift;
    my $len  = shift;

    my $buf = $self->read($len);

    if (!defined($buf)) {
      die new Thrift::TException('TSocket: Could not read '.$len.' bytes from input buffer');
    }
    return $buf;
}

#
# Read and return string
#
sub read
{
    my $self = shift;
    my $len  = shift;

    my $buf;

    my $in = $self->{in};

    if (!defined($in)) {
      die new Thrift::TException("Response buffer is empty, no request.");
    }
    eval {
      my $ret = sysread($in, $buf, $len);
      if (! defined($ret)) {
        die new Thrift::TException("No more data available.");
      }
    }; if($@){
      die new Thrift::TException($@);
    }

    return $buf;
}

#
# Write string
#
sub write
{
    my $self = shift;
    my $buf  = shift;
    $self->{out}->print($buf);
}

#
# Flush output (do the actual HTTP/HTTPS request)
#
sub flush
{
    my $self = shift;

    my $ua = LWP::UserAgent->new('timeout' => ($self->{sendTimeout} / 1000),
      'agent' => 'Perl/THttpClient'
     );
    $ua->default_header('Accept' => 'application/x-thrift');
    $ua->default_header('Content-Type' => 'application/x-thrift');
    $ua->cookie_jar({}); # hash to remember cookies between redirects

    my $out = $self->{out};
    $out->setpos(0); # rewind
    my $buf = join('', <$out>);

    my $request = new HTTP::Request(POST => $self->{url}, undef, $buf);
    my $response = $ua->request($request);
    my $content_ref = $response->content_ref;

    my $in = IO::String->new($content_ref);
    binmode($in);
    $self->{in} = $in;
    $in->setpos(0); # rewind

    # reset write buffer
    $out = IO::String->new;
    binmode($out);
    $self->{out} = $out;
}

1;
