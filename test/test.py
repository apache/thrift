#!/usr/bin/env python

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

from __future__ import division
import time
import socket
import subprocess
import sys
import os
import signal
import json
import platform
import shutil
import threading
from optparse import OptionParser

parser = OptionParser()
parser.add_option("--port", type="int", dest="port", default=9090,
    help="port number for server to listen on")
parser.add_option('-v', '--verbose', action="store_const",
    dest="verbose", const=2,
    help="verbose output")
parser.add_option('-q', '--quiet', action="store_const",
    dest="verbose", const=0,
    help="minimal output")
parser.add_option("--server", type="string", dest="servers", default="",
    help="list of servers to test seperated by commas, eg:- --server=cpp,java")
parser.add_option("--client", type="string", dest="clients", default="",
    help="list of clients to test seperated by commas, eg:- --client=cpp,java")
parser.set_defaults(verbose=1)
options, args = parser.parse_args()

if options.servers == "":
  serversList = []
else:
  serversList = options.servers.split(",")
if options.clients == "":
  clientsList = []
else:
  clientsList = options.clients.split(",")

def relfile(fname):
    return os.path.join(os.path.dirname(__file__), fname)

def getSocketArgs(socket_type):
  if socket_type == 'ip':
    return ""
  elif socket_type == 'ip-ssl':
    return "--ssl"
  elif socket_type == 'domain':
    return "--domain-socket=/tmp/ThriftTest.thrift"

def runServiceTest(test_name, server_lib, server_executable, server_extra_args, client_lib,  client_executable, client_extra_args, server_protocol, client_protocol, transport, port, use_zlib, socket_type):
  # Build command line arguments
  server_args = []
  cli_args = []
  if server_lib == 'java':
    server_args.append(server_executable[0])
    server_args.append(server_executable[1])
    server_args.append(relfile(server_executable[2]))
    server_args.extend(['-Dtestargs','\"'])
  else:
    server_args = [relfile(server_executable)]
  if client_lib == 'java':
    cli_args.append(client_executable[0])
    cli_args.append(client_executable[1])
    cli_args.append(relfile(client_executable[2]))
    cli_args.extend(['-Dtestargs','\"'])
  else:
    cli_args = [relfile(client_executable)]

  server_args.append('--protocol=%s' % server_protocol)
  cli_args.append('--protocol=%s' % client_protocol)

  for which in (server_args, cli_args):
    which.append('--transport=%s' % transport)
    which.append('--port=%d' % port) # default to 9090
    if use_zlib:
      which.append('--zlib')
    if socket_type == 'ip-ssl':
      which.append('--ssl')
    elif socket_type == 'domain':
      which.append('--domain-socket=/tmp/ThriftTest.thrift')
#    if options.verbose == 0:
#      which.append('-q')
#    if options.verbose == 2:
#      which.append('-v')
  if server_lib == 'java':
    server_args.append('\"')
  if client_lib == 'java':
    cli_args.append('\"')

  server_args.extend(server_extra_args)
  cli_args.extend(client_extra_args)

  server_log=open(relfile("log/" + test_name + "_server.log"),"a")
  client_log=open(relfile("log/" + test_name + "_client.log"),"a")

  try:
    if options.verbose > 0:
      print 'Testing server: %s' % (' '.join(server_args))
      serverproc = subprocess.Popen(server_args, stdout=server_log, stderr=server_log)
    else:
      serverproc = subprocess.Popen(server_args, stdout=server_log, stderr=server_log)
  except OSError as e:
    return "OS error({0}): {1}".format(e.errno, e.strerror)

  def ensureServerAlive():
    if serverproc.poll() is not None:
      return 'Server subprocess died, args: %s' % (' '.join(server_args))

  # Wait for the server to start accepting connections on the given port.
  sock = socket.socket()
  sleep_time = 0.1  # Seconds
  max_attempts = 100
  try:
    attempt = 0

    if socket_type != 'domain':
      while sock.connect_ex(('127.0.0.1', port)) != 0:
        attempt += 1
        if attempt >= max_attempts:
          return "TestServer not ready on port %d after %.2f seconds" % (port, sleep_time * attempt)
        ensureServerAlive()
        time.sleep(sleep_time)
  finally:
    sock.close()

  try:
    o = []
    def target():
      try:
        if options.verbose > 0:
          print 'Testing client: %s' % (' '.join(cli_args))
          process = subprocess.Popen(cli_args, stdout=client_log, stderr=client_log)
          o.append(process)
          process.communicate()
        else:
          process = subprocess.Popen(cli_args, stdout=client_log, stderr=client_log)
          o.append(process)
          process.communicate()
      except OSError as e:
        return "OS error({0}): {1}".format(e.errno, e.strerror)
      except:
        return "Unexpected error:", sys.exc_info()[0]
    thread = threading.Thread(target=target)
    thread.start()

    thread.join(10)
    if thread.is_alive():
      print 'Terminating process'
      o[0].terminate()
      thread.join()
    if(len(o)==0):
      return "Client subprocess failed, args: %s" % (' '.join(cli_args))
    ret = o[0].returncode
    if ret != 0:
      return "Client subprocess failed, retcode=%d, args: %s" % (ret, ' '.join(cli_args))
      #raise Exception("Client subprocess failed, retcode=%d, args: %s" % (ret, ' '.join(cli_args)))
  finally:
    # check that server didn't die
    #ensureServerAlive()
    extra_sleep = 0
    if extra_sleep > 0 and options.verbose > 0:
      print ('Giving (protocol=%s,zlib=%s,ssl=%s) an extra %d seconds for child'
             'processes to terminate via alarm'
             % (protocol, use_zlib, use_ssl, extra_sleep))
      time.sleep(extra_sleep)
    os.kill(serverproc.pid, signal.SIGTERM)
    #serverproc.wait()
  client_log.flush()
  server_log.flush()
  client_log.close()
  server_log.close()

test_count = 0
failed = 0
hard_fail_count = 0
platform = platform.system()
if os.path.exists(relfile('log')): shutil.rmtree(relfile('log'))
os.makedirs(relfile('log'))
if os.path.exists(relfile('results.json')): os.remove(relfile('results.json'))
results_json = open(relfile("results.json"),"a")
results_json.write("[\n")

with open(relfile('tests.json')) as data_file:
    data = json.load(data_file)

#subprocess.call("export NODE_PATH=../lib/nodejs/test:../lib/nodejs/lib:${NODE_PATH}")
count = 0
for server in data["server"]:
  if (server["lib"] in serversList or len(serversList) == 0) and platform in server["platform"]:
    server_executable = server["executable"]
    server_extra_args = ""
    server_lib = server["lib"]
    if "extra_args" in server:
      server_extra_args = server["extra_args"]
    for protocol in server["protocols"]:
      for transport in server["transports"]:
        for sock in server["sockets"]:
          for client in data["client"]:
            if (client["lib"] in clientsList or len(clientsList) == 0) and platform in client["platform"]:
              client_executable = client["executable"]
              client_extra_args = ""
              client_lib = client["lib"]
              if "extra_args" in client:
                client_extra_args = client["extra_args"]
              if protocol in client["protocols"]:
                if transport in client["transports"]:
                  if sock in client["sockets"]:
                    if count != 0:
                      results_json.write(",\n")
                    count = 1
                    results_json.write("\t[\n\t\t\"" + server_lib + "\",\n\t\t\"" + client_lib + "\",\n\t\t\"" + protocol + "\",\n\t\t\"" + transport + "-" + sock + "\",\n" )
                    test_name = server_lib + "_" + client_lib + "_" + protocol + "_" + transport + "_" + sock
                    ret = runServiceTest(test_name, server_lib, server_executable, server_extra_args, client_lib, client_executable, client_extra_args, protocol, protocol, transport, options.port, 0, sock)
                    if ret != None:
                      failed += 1
                      if client["exit"] == "hard" and server["exit"] == "hard":
                        hard_fail_count +=1
                      print "Error: %s" % ret
                      print "Using"
                      print (' Server: %s --protocol=%s --transport=%s %s %s'
                        % (server_executable, protocol, transport, getSocketArgs(sock), ' '.join(server_extra_args)))
                      print (' Client: %s --protocol=%s --transport=%s %s %s'
                        % (client_executable, protocol, transport, getSocketArgs(sock), ''.join(client_extra_args)))
                      results_json.write("\t\t\"failure\",\n")
                    else:
                      results_json.write("\t\t\"success\",\n")
                    results_json.write("\t\t{\n\t\t\t\"Client\":\"log/" + test_name + "_client.log\",\n\t\t\t\"Server\":\"log/" + test_name + "_server.log\"\n\t\t}\n\t]")
                    test_count += 1
              if protocol == 'binary' and 'accel' in client["protocols"]:
                if transport in client["transports"]:
                  if sock in client["sockets"]:
                    if count != 0:
                      results_json.write(",\n")
                    count = 1
                    results_json.write("\t[\n\t\t\"" + server_lib + "\",\n\t\t\"" + client_lib + "\",\n\t\t\"accel-binary\",\n\t\t\"" + transport + "-" + sock + "\",\n" )
                    test_name = server_lib + "_" + client_lib + "_accel-binary_" + transport + "_" + sock
                    ret = runServiceTest(test_name, server_lib,server_executable, server_extra_args, client_lib, client_executable, client_extra_args, protocol, 'accel', transport, options.port, 0, sock)

                    if ret != None:
                      failed += 1
                      if client["exit"] == "hard" and server["exit"] == "hard":
                        hard_fail_count +=1
                      print "Error: %s" % ret
                      print "Using"
                      print (' Server: %s --protocol=%s --transport=%s %s %s'
                        % (server_executable, protocol, transport, getSocketArgs(sock), ' '.join(server_extra_args)))
                      print (' Client: %s --protocol=%s --transport=%s %s %s'
                        % (client_executable, protocol, transport , getSocketArgs(sock), ''.join(client_extra_args)))
                      results_json.write("\t\t\"failure\",\n")
                    else:
                      results_json.write("\t\t\"success\",\n")
                    results_json.write("\t\t{\n\t\t\t\"Client\":\"log/" + test_name + "_client.log\",\n\t\t\t\"Server\":\"log/" + test_name + "_server.log\"\n\t\t}\n\t]")
                    test_count += 1
              if protocol == 'accel' and 'binary' in client["protocols"]:
                if transport in client["transports"]:
                  if sock in client["sockets"]:
                    if count != 0:
                      results_json.write(",\n")
                    count = 1
                    results_json.write("\t[\n\t\t\"" + server_lib + "\",\n\t\t\"" + client_lib + "\",\n\t\t\"binary-accel\",\n\t\t\"" + transport + "-" + sock + "\",\n" )
                    test_name = server_lib + "_" + client_lib + "_binary-accel_" + transport + "_" + sock
                    ret = runServiceTest(test_name, server_lib,server_executable, server_extra_args, client_lib, client_executable, client_extra_args, protocol, 'binary', transport, options.port, 0, sock)
                    if ret != None:
                      failed += 1
                      if client["exit"] == "hard" and server["exit"] == "hard":
                        hard_fail_count +=1
                      print "Error: %s" % ret
                      print "Using"
                      print (' Server: %s --protocol=%s --transport=%s %s %s'
                        % (server_executable, protocol, transport + sock, getSocketArgs(sock), ' '.join(server_extra_args)))
                      print (' Client: %s --protocol=%s --transport=%s %s %s'
                        % (client_executable, protocol, transport + sock, getSocketArgs(sock), ''.join(client_extra_args)))
                      results_json.write("\t\t\"failure\",\n")
                    else:
                      results_json.write("\t\t\"success\",\n")
                    results_json.write("\t\t{\n\t\t\t\"Client\":\"log/" + test_name + "_client.log\",\n\t\t\t\"Server\":\"log/" + test_name + "_server.log\"\n\t\t}\n\t]")
                    test_count += 1
results_json.write("\n]")
results_json.flush()
results_json.close()
print '%s failed of %s tests in total' % (failed, test_count)
sys.exit(hard_fail_count)