#ifndef _THRIFT_H
#define _THRIFT_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <glib.h>

/* this macro is called to satisfy -Wall hardcore compilation */
#ifndef THRIFT_UNUSED_VAR
# define THRIFT_UNUSED_VAR(x) ((void) x)
#endif

void thrift_hash_table_get_keys (gpointer key, gpointer value,
                                 gpointer user_data);

#endif // #ifndef _THRIFT_THRIFT_H
