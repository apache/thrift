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

# Thrift::SSLSocket authenticates the server it connects to.
#
# Each case runs a real TLS handshake against a local listener whose
# certificate we generate here, so what is asserted is the outcome of the
# handshake rather than the contents of the options hash.
#
# Two of the six are regression guards that also pass before the change; they
# are marked as such where they appear.

use strict;
use warnings;

use Test::More;

BEGIN {
    eval { require IO::Socket::SSL; require IO::Socket::SSL::Utils; 1 }
        or plan skip_all => 'IO::Socket::SSL and IO::Socket::SSL::Utils are required';
    eval { require File::Temp; 1 }
        or plan skip_all => 'File::Temp is required';
    plan skip_all => 'fork() is required' unless $Config::Config{d_fork} || $^O !~ /Win32/;
}

use IO::Socket::SSL;
use IO::Socket::SSL::Utils;
use File::Temp qw(tempdir);

use Thrift::SSLSocket;

plan tests => 6;

my $dir = tempdir(CLEANUP => 1);

# A private CA, a certificate it issued for "localhost", and one it issued for
# a name the client will not be connecting to.
my ($cacert, $cakey) = CERT_create(CA => 1, subject => { CN => 'Thrift Test CA' });
my ($goodcert, $goodkey) = CERT_create(
    issuer          => [$cacert, $cakey],
    subject         => { CN => 'localhost' },
    subjectAltNames => [ [ DNS => 'localhost' ], [ IP => '127.0.0.1' ] ]);
my ($othercert, $otherkey) = CERT_create(
    issuer          => [$cacert, $cakey],
    subject         => { CN => 'wrong.example' },
    subjectAltNames => [ [ DNS => 'wrong.example' ] ]);

PEM_cert2file($cacert,    "$dir/ca.pem");
PEM_cert2file($goodcert,  "$dir/good.crt");
PEM_key2file ($goodkey,   "$dir/good.key");
PEM_cert2file($othercert, "$dir/other.crt");
PEM_key2file ($otherkey,  "$dir/other.key");

# Serves $count TLS connections from a child process and returns the port.
sub start_server {
    my ($cert, $key, $count) = @_;

    my $listener = IO::Socket::SSL->new(
        LocalAddr       => '127.0.0.1',
        LocalPort       => 0,
        Listen          => 5,
        ReuseAddr       => 1,
        SSL_cert_file   => $cert,
        SSL_key_file    => $key,
        # A server does not ask its clients for a certificate; that is the
        # ordinary posture and is not what this test is about.
        SSL_verify_mode => IO::Socket::SSL::SSL_VERIFY_NONE,
    ) or die "could not listen: $!";

    my $port = $listener->sockport();
    my $pid  = fork();
    die "fork failed: $!" unless defined $pid;

    if (!$pid) {
        # The handshake is expected to fail in most cases here; accept() simply
        # returns and we move on to the next connection.
        for (1 .. $count) { $listener->accept(); }
        exit 0;
    }

    $listener->close();
    return ($port, $pid);
}

sub stop_server {
    my ($pid) = @_;
    kill 'TERM', $pid;
    waitpid($pid, 0);
}

# Returns (connected, error-text).
sub connects {
    my (%opts) = @_;
    my $socket = Thrift::SSLSocket->new({ %opts });
    my $ok = eval { $socket->open(); 1 };
    my $err = $ok ? '' : "$@";
    eval { $socket->close() } if $ok;
    return ($ok, $err);
}

{
    my ($port, $pid) = start_server("$dir/good.crt", "$dir/good.key", 3);

    # The certificate is well-formed and its name matches; what it is not is
    # issued by anything the client trusts.
    my ($ok, $err) = connects(host => 'localhost', port => $port);
    ok(!$ok, 'a server whose certificate chain cannot be verified is refused');
    like($err, qr/certificate verify failed|verify|SSL/i,
        '... and the refusal names the verification failure, not a connect error');

    # The same server, now with its CA supplied: this must keep working, or the
    # first assertion would be satisfied by refusing everything.
    ($ok, $err) = connects(host => 'localhost', port => $port, ca => "$dir/ca.pem");
    ok($ok, 'a server whose certificate the configured CA issued is accepted')
        or diag("unexpected refusal: $err");

    # Opting out is still possible for deployments that need it.
    ($ok, $err) = connects(host => 'localhost', port => $port, verify => 0);
    ok($ok, 'verify => 0 restores the unauthenticated handshake')
        or diag("unexpected refusal: $err");

    stop_server($pid);
}

{
    # Issued by the CA the client trusts, but for a different name.
    #
    # Both assertions below are regression guards, not demonstrations: they
    # pass against the unmodified library too, because supplying "ca" was
    # already enough to switch verification on, and IO::Socket::SSL checks the
    # host name whenever it verifies. What they pin is that the change does not
    # lose that.
    my ($port, $pid) = start_server("$dir/other.crt", "$dir/other.key", 2);

    my ($ok, $err) = connects(host => 'localhost', port => $port, ca => "$dir/ca.pem");
    ok(!$ok, 'a certificate issued for another host name is refused');
    like($err, qr/hostname|host name|verify|SSL/i,
        '... and the refusal names the host-name check');

    stop_server($pid);
}
