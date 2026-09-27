#!/usr/bin/env bash

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

# Runs the tutorial client against the tutorial server, once for each transport
# and with the other protocols, the layered transports and multiplexing.
# Every run starts two clients at once (-mc:2). Build the tutorial first.

DOTNET="${DOTNET:-dotnet}"
PORT=9090
PIPE="${TMPDIR:-/tmp}/CoreFxPipe_.test"
CLIENTS=2

CASES=(
  "-tr:tcp"
  "-tr:tcp -pr:compact -bf:buffered"
  "-tr:tcp -pr:json -bf:framed"
  "-tr:tcp -multiplex"
  "-tr:tcptls"
  "-tr:namedpipe"
  "-tr:http"
)

cd "$(dirname "$0")" || exit 1

SERVER=""
for f in Server/bin/Release/net*/Server.dll; do
  [ -f "$f" ] && SERVER="$f"
done
CLIENT=""
for f in Client/bin/Release/net*/Client.dll; do
  [ -f "$f" ] && CLIENT="$f"
done
if [ -z "$SERVER" ] || [ -z "$CLIENT" ]; then
  echo "Server.dll or Client.dll not found, build the tutorial first"
  exit 1
fi

TIMEOUT=""
if command -v timeout > /dev/null 2>&1; then
  TIMEOUT="timeout 60"
fi

LOGDIR=$(mktemp -d) || exit 1
SLOG="$LOGDIR/server.log"
CLOG="$LOGDIR/client.log"
SPID=""

# is a server listening where the given case expects one?
server_ready() {
  case "$1" in
    *namedpipe*) [ -S "$PIPE" ] ;;
    *) (exec 3<> "/dev/tcp/127.0.0.1/$PORT") 2> /dev/null ;;
  esac
}

stop_server() {
  if [ -n "$SPID" ]; then
    kill "$SPID" 2> /dev/null
    for _ in {1..20}; do
      kill -0 "$SPID" 2> /dev/null || break
      sleep 0.5
    done
    kill -9 "$SPID" 2> /dev/null
    wait "$SPID" 2> /dev/null
    SPID=""
  fi
}

show_logs() {
  echo "  --- server output (last 30 lines) ---"
  tail -n 30 "$SLOG" | sed 's/^/  /'
  echo "  --- client output (last 30 lines) ---"
  tail -n 30 "$CLOG" | sed 's/^/  /'
}

cleanup() {
  stop_server
  rm -f "$SLOG" "$CLOG"
  rmdir "$LOGDIR"
}
trap cleanup EXIT

run_case() {
  local args="$1"
  local rc

  if server_ready "$args"; then
    echo "FAIL  $args: port $PORT or $PIPE is already in use, is another server running?"
    return 1
  fi

  : > "$CLOG"
  # shellcheck disable=SC2086
  "$DOTNET" "$SERVER" $args < /dev/null > "$SLOG" 2>&1 &
  SPID=$!

  for _ in {1..60}; do
    server_ready "$args" && break
    kill -0 "$SPID" 2> /dev/null || break
    sleep 0.5
  done
  if ! server_ready "$args"; then
    echo "FAIL  $args: the server did not start"
    stop_server
    show_logs
    return 1
  fi

  # shellcheck disable=SC2086
  $TIMEOUT "$DOTNET" "$CLIENT" $args "-mc:$CLIENTS" < /dev/null > "$CLOG" 2>&1
  rc=$?
  stop_server
  case "$args" in
    *namedpipe*) rm -f "$PIPE" ;;
  esac

  if [ "$rc" -ne 0 ]; then
    echo "FAIL  $args -mc:$CLIENTS: the client exited with $rc"
    show_logs
    return 1
  fi
  echo "PASS  $args -mc:$CLIENTS"
}

FAILED=0
for args in "${CASES[@]}"; do
  run_case "$args" || FAILED=$((FAILED + 1))
done

echo "$((${#CASES[@]} - FAILED)) of ${#CASES[@]} cases passed"
[ "$FAILED" -eq 0 ]
