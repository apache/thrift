/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#include <thrift/windows/GetTimeOfDay.h>
#include <thrift/thrift-config.h>

// win32
#if defined(__MINGW32__)
  #include <sys/time.h>
#endif

#if !defined(__MINGW32__)
struct timezone {
  int tz_minuteswest; /* minutes W of Greenwich */
  int tz_dsttime;     /* type of dst correction */
};
#endif

#if defined(__MINGW32__)
int thrift_gettimeofday(struct timeval* tv, struct timezone* tz) {
  return gettimeofday(tv,tz);
}
#else
#define WIN32_LEAN_AND_MEAN
#include <Winsock2.h>
#include <cstdint>
#include <sstream>
#include <thrift/transport/TTransportException.h>

// This code started from a "FREE implementation" posted to Stack Overflow at:
// https://stackoverflow.com/questions/10905892/equivalent-of-gettimeday-for-windows
// added: assert
// added: error handling
// added: C++ style casts
int thrift_gettimeofday(struct timeval * tp, struct timezone * tzp)
{
    // We don't fill it in so prove nobody is looking for the data
    assert(tzp == NULL);

    // Note: some broken versions only have 8 trailing zero's, the correct epoch has 9 trailing zero's
    // This magic number is the number of 100 nanosecond intervals since January 1, 1601 (UTC)
    // until 00:00:00 January 1, 1970 
    static const uint64_t EPOCH = static_cast<uint64_t>(116444736000000000ULL);

    SYSTEMTIME  system_time;
    FILETIME    file_time;
    uint64_t    time;

    GetSystemTime( &system_time );
    if (!SystemTimeToFileTime( &system_time, &file_time )) {
      DWORD lastError = GetLastError();
      std::stringstream ss;
      ss << "SystemTimeToFileTime failed: 0x" << std::hex << lastError;
      using apache::thrift::transport::TTransportException;
      throw TTransportException(TTransportException::INTERNAL_ERROR, ss.str());
    }
    time =  static_cast<uint64_t>(file_time.dwLowDateTime )      ;
    time += static_cast<uint64_t>(file_time.dwHighDateTime) << 32;

    tp->tv_sec  = static_cast<long>((time - EPOCH) / 10000000L);
    tp->tv_usec = static_cast<long>(system_time.wMilliseconds * 1000);
    return 0;
}
#endif

int thrift_sleep(unsigned int seconds) {
  ::Sleep(seconds * 1000);
  return 0;
}
int thrift_usleep(unsigned int microseconds) {
  unsigned int milliseconds = (microseconds + 999) / 1000;
  ::Sleep(milliseconds);
  return 0;
}

char* thrift_ctime_r(const time_t* _clock, char* _buf) {
  strcpy(_buf, ctime(_clock));
  return _buf;
}
