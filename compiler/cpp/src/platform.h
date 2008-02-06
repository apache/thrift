// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

/**
 * define for mkdir,since the method signature
 * is different for the non-POSIX MinGW
 */

#ifdef MINGW
#include <io.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif

#if defined MINGW
#define MKDIR(x) mkdir(x)
#else
#define MKDIR(x) mkdir(x, S_IRWXU | S_IRWXG | S_IRWXO)
#endif
