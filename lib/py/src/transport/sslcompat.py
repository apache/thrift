#
# licensed to the apache software foundation (asf) under one
# or more contributor license agreements. see the notice file
# distributed with this work for additional information
# regarding copyright ownership. the asf licenses this file
# to you under the apache license, version 2.0 (the
# "license"); you may not use this file except in compliance
# with the license. you may obtain a copy of the license at
#
#   http://www.apache.org/licenses/license-2.0
#
# unless required by applicable law or agreed to in writing,
# software distributed under the license is distributed on an
# "as is" basis, without warranties or conditions of any
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

import logging
import sys

from thrift.transport.TTransport import TTransportException

logger = logging.getLogger(__name__)


def legacy_validate_callback(cert, hostname):
    """legacy method to validate the peer's SSL certificate, and to check
    the commonName of the certificate to ensure it matches the hostname we
    used to make this connection.  Does not support subjectAltName records
    in certificates.

    raises TTransportException if the certificate fails validation.
    """
    if 'subject' not in cert:
        raise TTransportException(
            TTransportException.NOT_OPEN,
            'No SSL certificate found from %s' % hostname)
    fields = cert['subject']
    for field in fields:
        # ensure structure we get back is what we expect
        if not isinstance(field, tuple):
            continue
        cert_pair = field[0]
        if len(cert_pair) < 2:
            continue
        cert_key, cert_value = cert_pair[0:2]
        if cert_key != 'commonName':
            continue
        certhost = cert_value
        # this check should be performed by some sort of Access Manager
        if certhost == hostname:
            # success, cert commonName matches desired hostname
            return
        else:
            raise TTransportException(
                TTransportException.UNKNOWN,
                'Hostname we connected to "%s" doesn\'t match certificate '
                'provided commonName "%s"' % (hostname, certhost))
    raise TTransportException(
        TTransportException.UNKNOWN,
        'Could not validate SSL certificate from host "%s".  Cert=%s'
        % (hostname, cert))


def match_peer_ipaddress(cert, hostname):
    """Check that a peer's certificate covers the IP address it connected from.

    Stands in for ssl.match_hostname, removed in Python 3.12, on the one path
    that still needs a check in Python rather than in OpenSSL: a server
    validating a client certificate against the peer address.

    Matching a *name* is deliberately not attempted. That follows RFC 6125 and
    belongs to OpenSSL, which does it during the handshake whenever the context
    has check_hostname set. Reaching this function with a name therefore means
    the check is not happening anywhere, and saying so is the only honest
    answer -- returning success because the check cannot be performed is what
    this replaces.

    raises TTransportException if the certificate does not cover the address.
    """
    import ipaddress

    if not cert:
        raise TTransportException(
            TTransportException.NOT_OPEN,
            'No SSL certificate found from %s' % hostname)

    try:
        peer = ipaddress.ip_address(hostname)
    except ValueError:
        raise TTransportException(
            TTransportException.NOT_OPEN,
            'Cannot verify the name "%s" against the peer certificate: this '
            'Python has no ssl.match_hostname, so name checking must be left '
            'to OpenSSL by enabling check_hostname on the SSL context.'
            % (hostname,))

    for field in cert.get('subjectAltName', ()):
        if not isinstance(field, tuple) or len(field) < 2:
            continue
        kind, value = field[0:2]
        if kind != 'IP Address':
            continue
        try:
            if ipaddress.ip_address(value) == peer:
                return
        except ValueError:
            continue

    raise TTransportException(
        TTransportException.UNKNOWN,
        'Peer address "%s" is not covered by the certificate it presented'
        % (hostname,))


def _optional_dependencies():
    try:
        import ipaddress  # noqa
        logger.debug('ipaddress module is available')
        ipaddr = True
    except ImportError:
        logger.warning('ipaddress module is unavailable')
        ipaddr = False

    if sys.hexversion < 0x030500F0:
        try:
            from backports.ssl_match_hostname import match_hostname, __version__ as ver
            ver = list(map(int, ver.split('.')))
            logger.debug('backports.ssl_match_hostname module is available')
            match = match_hostname
            if ver[0] * 10 + ver[1] >= 35:
                return ipaddr, match
            else:
                logger.warning('backports.ssl_match_hostname module is too old')
                ipaddr = False
        except ImportError:
            logger.warning('backports.ssl_match_hostname is unavailable')
            ipaddr = False
    try:
        from ssl import match_hostname
        logger.debug('ssl.match_hostname is available')
        match = match_hostname
    except ImportError:
        # https://docs.python.org/3/whatsnew/3.12.html:
        # "Remove the ssl.match_hostname() function. It was deprecated in Python
        # 3.7. OpenSSL performs hostname matching since Python 3.7, Python no
        # longer uses the ssl.match_hostname() function.""
        #
        # OpenSSL performs it only when the context has check_hostname set,
        # which TSSLSocket now does for the contexts it builds. What is left
        # for this function is TSSLServerSocket, which matches a client
        # certificate against the address the connection arrived from -- an IP
        # address, never a name -- so that is what the replacement covers.
        if sys.version_info[0] > 3 or (sys.version_info[0] == 3 and sys.version_info[1] >= 12):
            match = match_peer_ipaddress
        else:
            logger.warning('using legacy validation callback')
            match = legacy_validate_callback
    return ipaddr, match


_match_has_ipaddress, _match_hostname = _optional_dependencies()
