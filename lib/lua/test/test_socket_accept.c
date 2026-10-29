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

// Drives socket_accept() and tcp_accept() from usocket.c with scripted
// stand-ins for accept() and select(): a connection that is already waiting,
// one that arrives while socket_accept() waits for it, EINTR and ECONNABORTED
// on the way, a wake-up that finds no connection, a wait that times out and a
// hard error. For each script it checks what the call reports, the descriptor
// it hands back, and how often it called accept() and select().
//
//   ./test/test_socket_accept

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
#include <stdio.h>

// The headers above declare the real accept() and select(). From here on the
// two names refer to the stand-ins below, inside usocket.c as well.
static int scripted_accept(int fd, struct sockaddr *addr, socklen_t *addrlen);
static int scripted_select(int nfds, fd_set *readfds, fd_set *writefds,
                           fd_set *exceptfds, struct timeval *timeout);
#define accept scripted_accept
#define select scripted_select
#include "../src/usocket.c"
#undef accept
#undef select

// One accept() result: a descriptor, or (fd 0) a failure with errno err.
typedef struct {
  int fd;
  int err;
} accepted;

// One select() result: the socket is readable, or the wait timed out.
enum { READABLE = 1, TIMED_OUT = 0 };

// The results, in order. Past the end of its script, accept() fails with
// EMFILE and select() times out, so a call that keeps going ends.
static const accepted *accept_script;
static size_t accept_steps, accept_calls;
static const int *select_script;
static size_t select_steps, select_calls;

static int scripted_accept(int fd, struct sockaddr *addr, socklen_t *addrlen) {
  accepted a;
  (void)fd;
  (void)addr;
  (void)addrlen;
  if (accept_calls >= accept_steps) {
    accept_calls++;
    errno = EMFILE;
    return -1;
  }
  a = accept_script[accept_calls++];
  if (a.fd > 0) {
    return a.fd;
  }
  errno = a.err;
  return -1;
}

static int scripted_select(int nfds, fd_set *readfds, fd_set *writefds,
                           fd_set *exceptfds, struct timeval *timeout) {
  (void)nfds;
  (void)readfds;
  (void)writefds;
  (void)exceptfds;
  (void)timeout;
  if (select_calls >= select_steps) {
    select_calls++;
    return TIMED_OUT;
  }
  return select_script[select_calls++];
}

static int failures = 0;

static void check(int ok, const char *what) {
  printf("%s - %s\n", ok ? "ok" : "not ok", what);
  if (!ok) {
    failures++;
  }
}

static void start(const accepted *a, size_t a_steps, const int *s,
                  size_t s_steps) {
  accept_script = a;
  accept_steps = a_steps;
  accept_calls = 0;
  select_script = s;
  select_steps = s_steps;
  select_calls = 0;
}

static void check_calls(const char *name, size_t accepts, size_t selects) {
  char what[256];
  snprintf(what, sizeof(what),
           "%s: ... after %zu accept() and %zu select() calls "
           "(made %zu and %zu)",
           name, accepts, selects, accept_calls, select_calls);
  check(accept_calls == accepts && select_calls == selects, what);
}

// socket_accept() with a 1000 ms timeout, so that the wait goes to select().
static void accept_one(const char *name, const accepted *a, size_t a_steps,
                       const int *s, size_t s_steps, T_ERRCODE want,
                       int want_client, size_t accepts, size_t selects) {
  t_socket sock = 5;
  t_socket client = -1;
  T_ERRCODE got;
  char what[256];
  start(a, a_steps, s, s_steps);
  got = socket_accept(&sock, &client, NULL, NULL, 1000);
  snprintf(what, sizeof(what), "%s: socket_accept() returns %d (got %d)",
           name, want, got);
  check(got == want, what);
  if (want == SUCCESS) {
    snprintf(what, sizeof(what),
             "%s: ... and hands back descriptor %d (got %d)", name,
             want_client, client);
    check(client == want_client, what);
  }
  check_calls(name, accepts, selects);
}

#define STEPS(s) s, sizeof(s) / sizeof(s[0])
#define NO_STEPS NULL, 0

int main(void) {
  static const accepted at_once[] = {{7, 0}};
  static const accepted again_then_one[] = {{0, EAGAIN}, {7, 0}};
  static const accepted intr_then_one[] = {{0, EINTR}, {7, 0}};
  static const accepted aborted_then_one[] = {{0, ECONNABORTED}, {8, 0}};
  static const accepted again_twice_then_one[] = {
    {0, EAGAIN}, {0, EAGAIN}, {9, 0}};
  static const accepted again[] = {{0, EAGAIN}};
  static const accepted again_twice[] = {{0, EAGAIN}, {0, EAGAIN}};
  static const accepted too_many_files[] = {{0, EMFILE}};
  static const accepted again_then_no_buffers[] = {{0, EAGAIN}, {0, ENOBUFS}};
  static const int readable[] = {READABLE};
  static const int readable_twice[] = {READABLE, READABLE};
  static const int timed_out[] = {TIMED_OUT};
  static const int readable_then_timed_out[] = {READABLE, TIMED_OUT};
  t_socket sock = 5;
  t_socket client = -1;
  const char *err;
  char what[256];

  accept_one("a connection that is already waiting", STEPS(at_once), NO_STEPS,
             SUCCESS, 7, 1, 0);
  accept_one("a connection that arrives during the wait",
             STEPS(again_then_one), STEPS(readable), SUCCESS, 7, 2, 1);
  accept_one("EINTR, then a connection", STEPS(intr_then_one), NO_STEPS,
             SUCCESS, 7, 2, 0);
  accept_one("ECONNABORTED, then a connection after the wait",
             STEPS(aborted_then_one), STEPS(readable), SUCCESS, 8, 2, 1);
  accept_one("a wake-up that finds no connection, then one",
             STEPS(again_twice_then_one), STEPS(readable_twice), SUCCESS, 9,
             3, 2);
  accept_one("the wait times out", STEPS(again), STEPS(timed_out), TIMEOUT, 0,
             1, 1);
  accept_one("a wake-up that finds no connection, then the wait times out",
             STEPS(again_twice), STEPS(readable_then_timed_out), TIMEOUT, 0, 2,
             2);
  accept_one("a hard error", STEPS(too_many_files), NO_STEPS, EMFILE, 0, 1, 0);
  accept_one("a hard error after the wait", STEPS(again_then_no_buffers),
             STEPS(readable), ENOBUFS, 0, 2, 1);

  // What the Lua accept() gets for a connection that arrives during the wait.
  start(STEPS(again_then_one), STEPS(readable));
  err = tcp_accept(&sock, &client, 1000);
  snprintf(what, sizeof(what),
           "tcp_accept, a connection that arrives during the wait: reports "
           "no error and hands back descriptor 7 (got \"%s\", %d)",
           err ? err : "no error", client);
  check(err == NULL && client == 7, what);
  check_calls("tcp_accept, a connection that arrives during the wait", 2, 1);

  printf("\n%d failure(s)\n", failures);
  return failures == 0 ? 0 : 1;
}
