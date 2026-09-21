//
// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.
//

// Drives socket_send() and tcp_send() from usocket.c with scripted stand-ins
// for send() and select(): partial writes, EAGAIN, EINTR, a wait that times
// out and a hard error. For each script it checks that every window handed to
// send() lies inside the caller's buffer, that the bytes send() accepted are
// the payload exactly and in order, and what the call reports.
//
//   ./test/test_socket_send

#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>

// The headers above declare the real send() and select(). From here on the
// two names refer to the stand-ins below, inside usocket.c as well.
static ssize_t scripted_send(int fd, const void *buf, size_t len, int flags);
static int scripted_select(int nfds, fd_set *readfds, fd_set *writefds,
                           fd_set *exceptfds, struct timeval *timeout);
#define send scripted_send
#define select scripted_select
#include "../src/usocket.c"
#undef send
#undef select

#define PAYLOAD_SIZE 20000
#define GUARD_SIZE 64

enum { ACCEPT_ALL, ACCEPT_SOME, FAIL };

typedef struct {
  int kind;
  int value; // bytes accepted for ACCEPT_SOME, errno for FAIL
} step;

// The payload sits between two guard areas, so that a window reaching past
// either end of it is still readable here and shows up in the comparison.
static char arena[GUARD_SIZE + PAYLOAD_SIZE + GUARD_SIZE];
static char *const payload = arena + GUARD_SIZE;
static size_t payload_len;

// send() results, in order. Once they run out the last one repeats.
static const step *script;
static size_t script_len, script_pos;
static int select_result;

// Everything the stand-in accepted, in the order it accepted it.
static char wire[4 * PAYLOAD_SIZE];
static size_t wire_len;
static int windows_outside;

static ssize_t scripted_send(int fd, const void *buf, size_t len, int flags) {
  uintptr_t at = (uintptr_t)buf;
  uintptr_t lo = (uintptr_t)payload, hi = lo + payload_len;
  uintptr_t arena_lo = (uintptr_t)arena, arena_hi = arena_lo + sizeof(arena);
  step s;
  size_t n;
  (void)fd;
  (void)flags;

  if (at < lo || at > hi || len > hi - at) {
    windows_outside++;
  }
  if (at < arena_lo || at > arena_hi || len > arena_hi - at) {
    errno = EFAULT;
    return -1;
  }

  s = script[script_pos < script_len ? script_pos : script_len - 1];
  script_pos++;
  if (s.kind == FAIL) {
    errno = s.value;
    return -1;
  }
  n = len;
  if (s.kind == ACCEPT_SOME && (size_t)s.value < n) {
    n = (size_t)s.value;
  }
  if (n > sizeof(wire) - wire_len) {
    n = sizeof(wire) - wire_len;
  }
  memcpy(wire + wire_len, buf, n);
  wire_len += n;
  return (ssize_t)n;
}

static int scripted_select(int nfds, fd_set *readfds, fd_set *writefds,
                           fd_set *exceptfds, struct timeval *timeout) {
  (void)nfds;
  (void)readfds;
  (void)writefds;
  (void)exceptfds;
  (void)timeout;
  return select_result;
}

static int failures = 0;

static void check(int ok, const char *what) {
  printf("%s - %s\n", ok ? "ok" : "not ok", what);
  if (!ok) {
    failures++;
  }
}

static void start(const step *s, size_t steps, size_t len, int writable) {
  size_t i;
  for (i = 0; i < sizeof(arena); i++) {
    arena[i] = (char)0xCC;
  }
  for (i = 0; i < len; i++) {
    payload[i] = (char)((i * 31 + 7) % 251);
  }
  payload_len = len;
  script = s;
  script_len = steps;
  script_pos = 0;
  select_result = writable ? 1 : 0;
  wire_len = 0;
  windows_outside = 0;
}

// What reached the wire must be the start of the payload, byte for byte.
// complete: all of it; otherwise: a part of it, so the call must not succeed.
static void check_wire(const char *name, int complete) {
  char what[256];
  snprintf(what, sizeof(what),
           "%s: every window given to send() lies inside the buffer "
           "(%d outside)", name, windows_outside);
  check(windows_outside == 0, what);
  if (complete) {
    snprintf(what, sizeof(what),
             "%s: the bytes sent are the %zu of the payload, in order "
             "(%zu sent)", name, payload_len, wire_len);
    check(wire_len == payload_len && memcmp(wire, payload, wire_len) == 0,
          what);
  } else {
    snprintf(what, sizeof(what),
             "%s: the bytes sent are the start of the payload, in order "
             "(%zu of %zu sent)", name, wire_len, payload_len);
    check(wire_len < payload_len && memcmp(wire, payload, wire_len) == 0,
          what);
  }
}

static void send_one(const char *name, const step *s, size_t steps,
                     size_t len, int writable, T_ERRCODE want) {
  t_socket sock = 5;
  T_ERRCODE got;
  char what[256];
  start(s, steps, len, writable);
  got = socket_send(&sock, payload, len, 1000);
  check_wire(name, want == SUCCESS);
  snprintf(what, sizeof(what), "%s: socket_send() returns %d (got %d)",
           name, want, got);
  check(got == want, what);
}

static void send_all(const char *name, const step *s, size_t steps,
                     size_t len, int complete) {
  t_socket sock = 5;
  const char *got;
  char what[256];
  start(s, steps, len, 1);
  got = tcp_send(&sock, payload, len, 1000);
  check_wire(name, complete);
  snprintf(what, sizeof(what), "%s: tcp_send() %s (got \"%s\")", name,
           complete ? "succeeds" : "reports an error",
           got ? got : "success");
  check(complete ? got == NULL : got != NULL, what);
}

#define STEPS(s) s, sizeof(s) / sizeof(s[0])

int main(void) {
  static const step all_at_once[] = {{ACCEPT_ALL, 0}};
  static const step again_then_rest[] = {{FAIL, EAGAIN}, {ACCEPT_ALL, 0}};
  static const step intr_then_rest[] = {{FAIL, EINTR}, {ACCEPT_ALL, 0}};
  static const step two_parts_then_rest[] = {
    {ACCEPT_SOME, 100}, {ACCEPT_SOME, 50}, {ACCEPT_ALL, 0}};
  static const step mixed[] = {
    {ACCEPT_SOME, 100}, {FAIL, EINTR}, {FAIL, EAGAIN}, {ACCEPT_SOME, 7},
    {ACCEPT_ALL, 0}};
  static const step always_some[] = {{ACCEPT_SOME, 10}};
  static const step again[] = {{FAIL, EAGAIN}};
  static const step reset[] = {{FAIL, ECONNRESET}};
  static const step long_mixed[] = {
    {ACCEPT_SOME, 3000}, {FAIL, EAGAIN}, {FAIL, EINTR}, {ACCEPT_ALL, 0},
    {ACCEPT_SOME, 1}, {FAIL, EINTR}, {ACCEPT_ALL, 0}, {ACCEPT_ALL, 0}};

  send_one("all at once", STEPS(all_at_once), 1000, 1, SUCCESS);
  send_one("EAGAIN, then the rest", STEPS(again_then_rest), 1000, 1, SUCCESS);
  send_one("EINTR, then the rest", STEPS(intr_then_rest), 1000, 1, SUCCESS);
  send_one("two partial sends, then the rest", STEPS(two_parts_then_rest),
           1000, 1, SUCCESS);
  send_one("partial, EINTR, EAGAIN, partial, the rest", STEPS(mixed), 1000, 1,
           SUCCESS);
  send_one("partial sends until the retries run out", STEPS(always_some),
           1000, 1, TIMEOUT);
  send_one("EAGAIN and the wait times out", STEPS(again), 1000, 0, TIMEOUT);
  send_one("a hard error", STEPS(reset), 1000, 1, ECONNRESET);

  send_all("tcp_send, several steps with partial sends", STEPS(long_mixed),
           PAYLOAD_SIZE, 1);
  send_all("tcp_send, partial sends until the retries run out",
           STEPS(always_some), PAYLOAD_SIZE, 0);

  printf("\n%d failure(s)\n", failures);
  return failures == 0 ? 0 : 1;
}
