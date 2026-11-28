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

// Drives socket_setnonblocking() and socket_setblocking() from usocket.c with
// a scripted stand-in for fcntl(): when F_GETFL fails, the call has to report
// that error and leave the flags alone, instead of writing back a flag word
// built from the failure value.
//
//   ./test/test_socket_flags

#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>

// From here on fcntl() refers to the stand-in below, inside usocket.c as well.
static int scripted_fcntl(int fd, int cmd, ...);
#define fcntl scripted_fcntl
#include "../src/usocket.c"
#undef fcntl

static int failures;

static void check(int ok, const char *what) {
  printf("%s - %s\n", ok ? "ok" : "not ok", what);
  if (!ok) {
    failures++;
  }
}

// F_GETFL either fails with get_errno (when it is not 0) or reports
// current_flags; F_SETFL always succeeds, and records what it was given.
static int get_errno;
static int current_flags;
static int set_calls, last_set_flags;

static int scripted_fcntl(int fd, int cmd, ...) {
  va_list ap;
  (void)fd;
  if (cmd == F_GETFL) {
    if (get_errno) {
      errno = get_errno;
      return -1;
    }
    return current_flags;
  }
  va_start(ap, cmd);
  last_set_flags = va_arg(ap, int);
  va_end(ap);
  set_calls++;
  return 0;
}

static void start(int flags, int getfl_errno) {
  current_flags = flags;
  get_errno = getfl_errno;
  set_calls = 0;
  last_set_flags = 0;
}

int main(void) {
  t_socket sock = 5;
  T_ERRCODE got;
  char what[256];

  start(O_RDWR, 0);
  got = socket_setnonblocking(&sock);
  check(got == SUCCESS && last_set_flags == (O_RDWR | O_NONBLOCK),
        "setnonblocking sets O_NONBLOCK and keeps the other flags");

  start(O_RDWR | O_NONBLOCK, 0);
  got = socket_setblocking(&sock);
  check(got == SUCCESS && last_set_flags == O_RDWR,
        "setblocking clears O_NONBLOCK and keeps the other flags");

  start(0, EIO);
  got = socket_setnonblocking(&sock);
  snprintf(what, sizeof(what),
           "setnonblocking reports a failed F_GETFL (got %d) and does not "
           "call F_SETFL (%d calls)", got, set_calls);
  check(got == EIO && set_calls == 0, what);

  start(0, EIO);
  got = socket_setblocking(&sock);
  snprintf(what, sizeof(what),
           "setblocking reports a failed F_GETFL (got %d) and does not call "
           "F_SETFL (%d calls)", got, set_calls);
  check(got == EIO && set_calls == 0, what);

  printf("\n%d failure(s)\n", failures);
  return failures == 0 ? 0 : 1;
}
