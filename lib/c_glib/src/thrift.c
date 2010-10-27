#include "thrift.h"

/**
 * GHashTable callback to add keys to a GList.
 */
void
thrift_hash_table_get_keys (gpointer key, gpointer value, gpointer user_data)
{
  THRIFT_UNUSED_VAR (value);
  GList **list = (GList **) user_data;
  *list = g_list_append (*list, key);
}

