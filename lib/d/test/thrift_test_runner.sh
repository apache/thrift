#!/bin/bash
# Runs the D ThriftTest client and servers for all combinations of transport,
# protocol, SSL-mode and server type.
# Pass -k to keep going after failed tests.

protocols="binary compact json"
transports="buffered framed http raw"
servers="simple taskpool threaded"
framed_only_servers="nonblocking pooledNonblocking"

# Don't leave any server instances behind when interrupted (e.g. by Ctrl+C)
# or terminated.
trap "kill $(jobs -p) 2>/dev/null" INT TERM

for protocol in $protocols; do
  for ssl in "" " --ssl"; do
    for transport in $transports; do
      for server in $servers $framed_only_servers; do
        case $framed_only_servers in
          *$server*) if [ $transport != "framed" ] || [ $ssl != "" ]; then continue; fi;;
        esac

        args="--transport=$transport --protocol=$protocol$ssl"
        ./thrift_test_server $args --server-type=$server > /dev/null &
        server_pid=$!

        # Give the server some time to get up and check if it runs (yes, this
        # is a huge kludge, should add a connect timeout to test client).
        client_rc=-1
        sleep 0.01
        kill -0 $server_pid 2>/dev/null
        if [ $? -eq 0 ]; then
          ./thrift_test_client $args --numTests=10 > /dev/null
          client_rc=$?

          # Temporarily redirect stderr to null to avoid job control messages,
          # restore it afterwards.
          exec 3>&2
          exec 2>/dev/null
          kill $server_pid
          exec 3>&2
        fi

        # Get the server exit code (wait should immediately return).
        wait $server_pid
        server_rc=$?

        if [ $client_rc -ne 0 -o $server_rc -eq 1 ]; then
          echo -e "\nTests failed for: $args --server-type=$server"
          failed="true"
          if [ "$1" != "-k" ]; then
            exit 1
          fi
        else
           echo -n "."
        fi
      done
    done
  done
done

echo
if [ -z "$failed" ]; then
  echo "All tests passed."
fi
