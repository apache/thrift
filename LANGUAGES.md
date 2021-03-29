# Apache Thrift Language Support #

Guidance For: 0.13.0 | 
[0.12.0](https://github.com/apache/thrift/blob/v0.12.0/LANGUAGES.md) | 
[0.11.0](https://github.com/apache/thrift/blob/0.11.0/LANGUAGES.md)

Thrift supports many programming languages and has an impressive test suite that
exercises most of the languages, protocols, and transports.  Each build exercises
a matrix of thousands of possible combinations.  Each language typically has a
minimum required version as well as support libraries - some mandatory and some
optional.  The information provided below will help you assess whether you can
use Apache Thrift with your project.  Obviously this is a complex matrix to
maintain and may not be correct in all cases - if you spot an error please inform
the developers using the mailing list, or better yet,
[Edit on GitHub](https://github.com/apache/thrift/edit/master/LANGUAGES.md).

Apache Thrift currently uses two build systems.  The `autoconf` build system is
the most complete and builds all supported languages, however it does not support
Windows..  The `cmake` build system works on Linux and Windows, and has been
designated by the project to replace `autoconf` however this transition will
take quite some time to complete.  During that transition, the cmake build will
not support all languages.

The Language/Library Levels indicate the minimum and maximum versions that are
used in the [continuous integration environments](build/docker/README.md)
(Appveyor, Travis) for Apache Thrift.  Other language levels may be supported
for each language, however tested less thoroughly; check the README file inside
each lib directory for additional details.  Note: while a language may contain
support for protocols, transports, and servers, the extent to which each is tested
as part of the overall build process varies.  The definitive integration test for
the project is called the "cross" test which executes a test matrix with clients
and servers communicating across languages.

Thrift's core transport (supported by all languages) is TSocket.
Thrift's core protocol is TBinary, supported by all languages except for JavaScript.

<table style="font-size: 60%; padding: 1px;">
<thead>
<tr>
<th rowspan=2>Language</th>
<th rowspan=2 align=center>Since</th>
<th colspan=2 align=center>Build Systems</th>
<th colspan=2 align=center>Lang/Lib Levels (Tested)</th>
<th colspan=6 align=center>Low-Level Transports</th>
<th colspan=4 align=center>Transport Wrappers</th>
<th colspan=4 align=center>Protocols</th>
<th colspan=5 align=center>Servers</th>
<th rowspan=2>Open Issues</th>
</tr>
<tr>
<!-- Build Systems ---------><th>autoconf</th><th>cmake</th>
<!-- Lang/Lib Levels -------><th>Min</th><th>Max</th>
<!-- Low-Level Transports --><th><a href="https://en.wikipedia.org/wiki/Unix_domain_socket">Domain</a></th><th>&nbsp;File&nbsp;</th><th>Memory</th><th>&nbsp;Pipe&nbsp;</th><th>Socket</th><th>&nbsp;TLS&nbsp;</th>
<!-- Transport Wrappers ----><th>Framed</th><th>Header</th><th>&nbsp;http&nbsp;</th><th>&nbsp;zlib&nbsp;</th>
<!-- Protocols -------------><th><a href="doc/specs/thrift-binary-protocol.md">Binary</a></th><th><a href="doc/specs/thrift-compact-protocol.md">Compact</a></th><th>&nbsp;JSON&nbsp;</th><th>Multiplex</th>
<!-- Servers ---------------><th>Forking</th><th>Nonblocking</th><th>Simple</th><th>Threaded</th><th>ThreadPool</th>
</tr>
</thead>
<tbody>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/as3/README.md">ActionScript</a></td>
<!-- Since -----------------><td>0.3.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Language Levels -------><td colspan=2>FLEX SDK 4.6</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22AS3%20-%20Compiler%22%2C%20%22AS3%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">ActionScript</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/c_glib/README.md">C (glib)</a></td>
<!-- Since -----------------><td>0.6.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Language Levels -------><td>2.48.2</td><td>2.56.4</td>
<!-- Low-Level Transports --><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22C%20glib%20-%20Compiler%22%2C%20%22C%20glib%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">C (glib)</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/cpp/README.md">C++</a></td>
<!-- Since -----------------><td>0.2.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Language Levels -------><td colspan=2>C++11</td>
<!-- Low-Level Transports --><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22C%2B%2B%20-%20Compiler%22%2C%20%22C%2B%2B%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">C++</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/cl/README.md">Common LISP</a></td>
<!-- Since -----------------><td>0.12.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>SBCL 1.4.x</td><td>SBCL 1.5.3</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Common%20LISP%20-%20Compiler%22%2C%20%22Common%20LISP%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Common LISP</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/d/README.md">Dlang</a></td>
<!-- Since -----------------><td>0.9.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>2.075.1</td><td>2.087.0</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22D%20-%20Compiler%22%2C%20%22D%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">D</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/dart/README.md">Dart</a></td>
<!-- Since -----------------><td>0.10.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>2.0.0</td><td>2.4.0</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Dart%20-%20Compiler%22%2C%20%22Dart%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Dart</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/delphi/README.md">Delphi</a></td>
<!-- Since -----------------><td>0.8.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>2010</td><td>unknown</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Delphi%20-%20Compiler%22%2C%20%22Delphi%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Delphi</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/netstd/README.md">.NET Standard</a></td>
<!-- Since -----------------><td>0.13.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td colspan=2>.NET 4.5+, .NET Standard 2.x</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22netstd%20-%20Compiler%22%2C%20%22netstd%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">.NET Standard</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/erl/README.md">Erlang</a></td>
<!-- Since -----------------><td>0.3.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>18.3</td><td>22.0</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Erlang%20-%20Compiler%22%2C%20%22Erlang%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Erlang</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/go/README.md">Go</a></td>
<!-- Since -----------------><td>0.7.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>1.14.14</td><td>1.15.7</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Go%20-%20Compiler%22%2C%20%22Go%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Go</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/hs/README.md">Haskell</a></td>
<!-- Since -----------------><td>0.5.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Language Levels -------><td>7.10.3</td><td>8.0.2</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Haskell%20-%20Compiler%22%2C%20%22Haskell%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Haskell</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/haxe/README.md">Haxe</a></td>
<!-- Since -----------------><td>0.9.3</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>3.2.1</td><td>3.4.4</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Haxe%20-%20Compiler%22%2C%20%22Haxe%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Haxe</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/java/README.md">Java (SE)</a></td>
<!-- Since -----------------><td>0.2.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Language Levels -------><td>1.8.0_151</td><td>11.0.3</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Java%20-%20Compiler%22%2C%20%22Java%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Java SE</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/javame/README.md">Java (ME)</a></td>
<!-- Since -----------------><td>0.5.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td colspan=2>unknown</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22JavaME%20-%20Compiler%22%2C%20%22JavaME%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Java ME</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/js/README.md">Javascript</a></td>
<!-- Since -----------------><td>0.3.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>ES5</td><td>ES6</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Javascript%20-%20Compiler%22%2C%20%22Javascript%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Javascript</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/lua/README.md">Lua</a></td>
<!-- Since -----------------><td>0.9.2</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>5.1.5</td><td>5.2.4</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Lua%20-%20Compiler%22%2C%20%22Lua%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Lua</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/nodejs/README.md">node.js</a></td>
<!-- Since -----------------><td>0.6.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>10.x</td><td>10.x</td>
<!-- Low-Level Transports --><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Node.js%20-%20Compiler%22%2C%20%22Node.js%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">node.js</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/nodets/README.md">node.ts</a></td>
<!-- Since -----------------><td>0.12.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>3.1.6</td><td></td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22TypeScript%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">node.ts</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/ocaml/README.md">OCaml</a></td>
<!-- Since -----------------><td>0.2.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td colspan=2>4.04.0</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22OCaml%20-%20Compiler%22%2C%20%22OCaml%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">OCaml</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/perl/README.md">Perl</a></td>
<!-- Since -----------------><td>0.2.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>5.22.1</td><td>5.26.1</td>
<!-- Low-Level Transports --><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Perl%20-%20Compiler%22%2C%20%22Perl%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Perl</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/php/README.md">PHP</a></td>
<!-- Since -----------------><td>0.2.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>7.0.22</td><td>7.2.19</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22PHP%20-%20Compiler%22%2C%20%22PHP%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">PHP</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/py/README.md">Python</a></td>
<!-- Since -----------------><td>0.2.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Language Levels -------><td>2.7.12, 3.5.2</td><td>2.7.15, 3.6.8</td>
<!-- Low-Level Transports --><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Python%20-%20Compiler%22%2C%20%22Python%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Python</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/rb/README.md">Ruby</a></td>
<!-- Since -----------------><td>0.2.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>2.3.1p112</td><td>2.5.1p57</td>
<!-- Low-Level Transports --><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Ruby%20-%20Compiler%22%2C%20%22Ruby%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Ruby</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/rs/README.md">Rust</a></td>
<!-- Since -----------------><td>0.11.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>1.40.0</td><td>1.xx.x</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Rust%20-%20Compiler%22%2C%20%22Rust%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Rust</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/st/README.md">Smalltalk</a></td>
<!-- Since -----------------><td>0.2.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td colspan=2>unknown</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Smalltalk%20-%20Compiler%22%2C%20%22Smalltalk%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Smalltalk</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/swift/README.md">Swift</a></td>
<!-- Since -----------------><td>0.12.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td colspan=2>4.2.1</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Swift%20-%20Compiler%22%2C%20%22Swift%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Swift</a></td>
</tr>
</tbody>
<tfoot>
<tr>
<th rowspan=2>Language</th>
<th rowspan=2 align=center>Since</th>
<!-- Build Systems ---------><th>autoconf</th><th>cmake</th>
<!-- Lang/Lib Levels -------><th>Min</th><th>Max</th>
<!-- Low-Level Transports --><th><a href="https://en.wikipedia.org/wiki/Unix_domain_socket">Domain</a></th></th><th>&nbsp;File&nbsp;</th><th>Memory</th><th>&nbsp;Pipe&nbsp;</th><th>Socket</th><th>&nbsp;TLS&nbsp;</th>
<!-- Transport Wrappers ----><th>Framed</th><th>Header</th><th>&nbsp;http&nbsp;</th><th>&nbsp;zlib&nbsp;</th>
<!-- Protocols -------------><th><a href="doc/specs/thrift-binary-protocol.md">Binary</a></th><th><a href="doc/specs/thrift-compact-protocol.md">Compact</a></th><th>&nbsp;JSON&nbsp;</th><th>Multiplex</th>
<!-- Servers ---------------><th>Forking</th><th>Nonblocking</th><th>Simple</th><th>Threaded</th><th>ThreadPool</th>
<th rowspan=2>Open Issues</th>
</tr>
<tr>
<th colspan=2 align=center>Build Systems</th>
<th colspan=2 align=center>Lang/Lib Levels (Tested)</th>
<th colspan=6 align=center>Low-Level Transports</th>
<th colspan=4 align=center>Transport Wrappers</th>
<th colspan=4 align=center>Protocols</th>
<th colspan=5 align=center>Servers</th>
</tr>
</tfoot>
</ft
table>
