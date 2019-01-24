# Apache Thrift Language Support #

Last Modified: 2018-12-17

Guidance For: 0.12.0 or later

Thrift supports many programming languages and has an impressive test suite that exercises most of the languages, protocols, and transports that represents a matrix of thousands of possible combinations.  Each language typically has a minimum required version as well as support libraries - some mandatory and some optional.  All of this information is provided below to help you assess whether you can use Apache Thrift with your project.  Obviously this is a complex matrix to maintain and may not be correct in all cases - if you spot an error please inform the developers using the mailing list.

Apache Thrift has a choice of two build systems.  The `autoconf` build system is the most complete build and is used to build all supported languages.  The `cmake` build system has been designated by the project to replace `autoconf` however this transition will take quite some time to complete.

The Language/Library Levels indicate the minimum and maximum versions that are used in the [continuous integration environments](build/docker/README.md) (Appveyor, Travis) for Apache Thrift.  Other language levels may be supported for each language, however tested less thoroughly; check the README file inside each lib directory for additional details.  Note that while a language may contain support for protocols, transports, and servers, the extent to which each is tested as part of the overall build process varies.  The definitive integration test for the project is called the "cross" test which executes a test matrix with clients and servers communicating across languages.

<table style="font-size: 60%; padding: 1px;">
<thead>
<tr>
<th rowspan=2>Language</th>
<th rowspan=2 align=center>Since</th>
<th colspan=2 align=center>Build Systems</th>
<th colspan=2 align=center>Lang/Lib Levels (Tested)</th>
<th colspan=6 align=center>Low-Level Transports</th>
<th colspan=3 align=center>Transport Wrappers</th>
<th colspan=4 align=center>Protocols</th>
<th colspan=5 align=center>Servers</th>
<th rowspan=2>Open Issues</th>
</tr>
<tr>
<!-- Build Systems ---------><th>autoconf</th><th>cmake</th>
<!-- Lang/Lib Levels -------><th>Min</th><th>Max</th>
<!-- Low-Level Transports --><th><a href="https://en.wikipedia.org/wiki/Unix_domain_socket">Domain</a></th><th>&nbsp;File&nbsp;</th><th>Memory</th><th>&nbsp;Pipe&nbsp;</th><th>Socket</th><th>&nbsp;TLS&nbsp;</th>
<!-- Transport Wrappers ----><th>Framed</th><th>&nbsp;http&nbsp;</th><th>&nbsp;zlib&nbsp;</th>
<!-- Protocols -------------><th><a href="doc/specs/thrift-binary-protocol.md">Binary</a></th><th><a href="doc/specs/thrift-compact-protocol.md">Compact</a></th><th>&nbsp;JSON&nbsp;</th><th>Multiplex</th>
<!-- Servers ---------------><th>Forking</th><th>Nonblocking</th><th>Simple</th><th>Threaded</th><th>ThreadPool</th>
</tr>
</thead>
<tbody>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/as3/README.md">ActionScript</a></td>
<!-- Since -----------------><td>0.3.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td colspan=2>FLEX SDK 4.6</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22AS3%20-%20Compiler%22%2C%20%22AS3%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">ActionScript</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/c_glib/README.md">C (glib)</a></td>
<!-- Since -----------------><td>0.6.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Language Levels -------><td>2.48.2</td><td>2.56.0</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22C%20glib%20-%20Compiler%22%2C%20%22C%20glib%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">C (glib)</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/cpp/README.md">C++</a></td>
<!-- Since -----------------><td>0.2.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Language Levels -------><td>C++98 (through 0.12.0)</td><td>C++11 (after 0.12.0)</td>
<!-- Low-Level Transports --><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22C%2B%2B%20-%20Compiler%22%2C%20%22C%2B%2B%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">C++</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/csharp/README.md">C#</a></td>
<!-- Since -----------------><td>0.2.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>.NET&nbsp;3.5 / mono&nbsp;3.2.8.0</td><td>.NET&nbsp;4.6.1 / mono&nbsp;4.6.2.7</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22C%23%20-%20Compiler%22%2C%20%22C%23%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">C# (.NET)</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/cocoa/README.md">Cocoa</a></td>
<!-- Since -----------------><td>0.2.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td colspan=2>unknown</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Cocoa%20-%20Compiler%22%2C%20%22Cocoa%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Cocoa</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/cl/README.md">Common LISP</a></td>
<!-- Since -----------------><td>0.12.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>SBCL 1.4.5</td><td>SBCL 1.4.15</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Common%20LISP%20-%20Compiler%22%2C%20%22Common%20LISP%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Common LISP</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/d/README.md">Dlang</a></td>
<!-- Since -----------------><td>0.9.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>2.075.1</td><td>2.083.2</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22D%20-%20Compiler%22%2C%20%22D%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">D</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/dart/README.md">Dart</a></td>
<!-- Since -----------------><td>0.10.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>1.22.1</td><td>1.24.3</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
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
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Delphi%20-%20Compiler%22%2C%20%22Delphi%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Delphi</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/netcore/README.md">.NET Core</a></td>
<!-- Since -----------------><td>0.11.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>2.1.4</td><td>2.2.101</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22.NETCore%20-%20Compiler%22%2C%20%22.NETCore%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">.NET Core</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/erl/README.md">Erlang</a></td>
<!-- Since -----------------><td>0.3.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>18.3</td><td>20.0.4</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Erlang%20-%20Compiler%22%2C%20%22Erlang%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Erlang</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/go/README.md">Go</a></td>
<!-- Since -----------------><td>0.7.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>1.7.6</td><td>1.11.4</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
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
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
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
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Haxe%20-%20Compiler%22%2C%20%22Haxe%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Haxe</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/java/README.md">Java (SE)</a></td>
<!-- Since -----------------><td>0.2.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Language Levels -------><td>1.8.0_151</td><td>1.8.0_191</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
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
<!-- Transport Wrappers ----><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22JavaME%20-%20Compiler%22%2C%20%22JavaME%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Java ME</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/js/README.md">Javascript</a></td>
<!-- Since -----------------><td>0.3.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td colspan=2>unknown</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
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
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Lua%20-%20Compiler%22%2C%20%22Lua%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Lua</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/nodejs/README.md">node.js</a></td>
<!-- Since -----------------><td>0.6.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>6.x</td><td>8.x</td>
<!-- Low-Level Transports --><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
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
<!-- Transport Wrappers ----><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
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
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
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
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Perl%20-%20Compiler%22%2C%20%22Perl%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Perl</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/php/README.md">PHP</a></td>
<!-- Since -----------------><td>0.2.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>7.0.22</td><td>7.2.10</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22PHP%20-%20Compiler%22%2C%20%22PHP%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">PHP</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/py/README.md">Python</a></td>
<!-- Since -----------------><td>0.2.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Language Levels -------><td>2.7.12, 3.5.2</td><td>2.7.15rc1, 3.6.7</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
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
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Protocols -------------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<!-- Servers ---------------><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td>
<td align=left><a href="https://issues.apache.org/jira/issues/?jql=project%20%3D%20THRIFT%20AND%20component%20in%20(%22Ruby%20-%20Compiler%22%2C%20%22Ruby%20-%20Library%22)%20and%20status%20not%20in%20(fixed%2C%20resolved%2C%20closed)">Ruby</a></td>
</tr>
<tr align=center>
<td align=left><a href="https://github.com/apache/thrift/blob/master/lib/rs/README.md">Rust</a></td>
<!-- Since -----------------><td>0.11.0</td>
<!-- Build Systems ---------><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Language Levels -------><td>1.17.0</td><td>1.30.0</td>
<!-- Low-Level Transports --><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
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
<!-- Transport Wrappers ----><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td><td><img src="doc/images/cred.png" alt=""/></td>
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
<!-- Transport Wrappers ----><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cgrn.png" alt="Yes"/></td><td><img src="doc/images/cred.png" alt=""/></td>
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
<!-- Transport Wrappers ----><th>Framed</th><th>&nbsp;http&nbsp;</th><th>&nbsp;zlib&nbsp;</th>
<!-- Protocols -------------><th><a href="doc/specs/thrift-binary-protocol.md">Binary</a></th><th><a href="doc/specs/thrift-compact-protocol.md">Compact</a></th><th>&nbsp;JSON&nbsp;</th><th>Multiplex</th>
<!-- Servers ---------------><th>Forking</th><th>Nonblocking</th><th>Simple</th><th>Threaded</th><th>ThreadPool</th>
<th rowspan=2>Open Issues</th>
</tr>
<tr>
<th colspan=2 align=center>Build Systems</th>
<th colspan=2 align=center>Lang/Lib Levels (Tested)</th>
<th colspan=6 align=center>Low-Level Transports</th>
<th colspan=3 align=center>Transport Wrappers</th>
<th colspan=4 align=center>Protocols</th>
<th colspan=5 align=center>Servers</th>
</tr>
</tfoot>
</ft
table>
