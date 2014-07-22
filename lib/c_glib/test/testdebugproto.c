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

#include <assert.h>
#include <math.h>
#include <glib-object.h>

#ifndef M_PI
#define M_PI 3.1415926535897932385
#endif

#include <thrift/c_glib/protocol/thrift_protocol.h>
#include <thrift/c_glib/protocol/thrift_binary_protocol.h>

#include "gen-c_glib/t_test_debug_proto_test_types.h"
#include "gen-c_glib/t_test_srv.h"
#include "gen-c_glib/t_test_inherited.h"

static void
test_structs(void)
{
  TTestOneOfEach *ooe = NULL;
  TTestNesting *n = NULL;
  TTestHolyMoley *hm = NULL;
  gchar *random = g_strdup("random string");
  gchar *nothing = g_strdup("nothing");

  ooe = g_object_new (T_TEST_TYPE_ONE_OF_EACH, NULL);
  ooe->im_true = TRUE;
  ooe->im_false = FALSE;
  ooe->a_bite = 0xd6;
  ooe->integer16 = 27000;
  ooe->integer32 = 1<<24;
  ooe->integer64 = (guint64) 6000 * 1000 * 1000;
  ooe->double_precision = M_PI;
  ooe->some_characters = g_strdup("Debug THIS!");
  ooe->zomg_unicode = g_strdup("\xd7\n\a\t");

  n = g_object_new (T_TEST_TYPE_NESTING, NULL);
  if (n->my_ooe != NULL)
    g_object_unref(n->my_ooe);

  n->my_ooe = ooe;
  n->my_ooe->integer16 = 16;
  n->my_ooe->integer32 = 32;
  n->my_ooe->integer64 = 64;
  n->my_ooe->double_precision = (sqrt(5.0) + 1) / 2;
  n->my_ooe->some_characters = g_strdup(":R (me going \"rrrr\")");
  n->my_ooe->zomg_unicode = g_strdup("\xd3\x80\xe2\x85\xae\xce\x9d\x20");
  n->my_bonk->type = 31337;
  n->my_bonk->message = g_strdup("I am a bonk... xor!");

  hm = g_object_new (T_TEST_TYPE_HOLY_MOLEY, NULL);
  g_ptr_array_add (hm->big, ooe);
  g_ptr_array_add (hm->big, g_object_ref(n->my_ooe));
  ((TTestOneOfEach *) g_ptr_array_index (hm->big, 0))->a_bite = 0x22;
  ((TTestOneOfEach *) g_ptr_array_index (hm->big, 1))->a_bite = 0x33;

  g_hash_table_insert (hm->contain, random, random);

  TTestBonk *bonk = NULL;
  bonk = g_object_new (T_TEST_TYPE_BONK, NULL);
  GPtrArray *bonks = g_ptr_array_new_with_free_func (g_object_unref);
  g_ptr_array_add (bonks, bonk);
  g_hash_table_insert (hm->bonks, nothing, bonks);

  g_object_unref (hm);

  return 0;
}

static void
test_service_inheritance (void)
{
  ThriftProtocol *protocol;
  TTestInheritedClient *inherited_client;
  GObject *input_protocol, *output_protocol;

  protocol = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL, NULL);
  inherited_client = g_object_new (T_TEST_TYPE_INHERITED_CLIENT,
                                   NULL);

  /* TTestInheritedClient inherits from TTestSrvClient */
  assert (g_type_is_a (T_TEST_TYPE_INHERITED_CLIENT,
                       T_TEST_TYPE_SRV_CLIENT));

  /* TTestInheritedClient implements TTestSrvClient's interface */
  assert (g_type_is_a (T_TEST_TYPE_INHERITED_CLIENT,
                       T_TEST_TYPE_SRV_IF));

  /* TTestInheritedClient's inherited properties can be set and
   * retrieved */
  g_object_set (inherited_client,
                "input_protocol", protocol,
                "output_protocol", protocol,
                NULL);

  g_object_get (inherited_client,
                "input_protocol", &input_protocol,
                "output_protocol", &output_protocol,
                NULL);

  assert (input_protocol == G_OBJECT(protocol));
  assert (output_protocol == G_OBJECT(protocol));

  g_object_unref (output_protocol);
  g_object_unref (input_protocol);
  g_object_unref (inherited_client);
  g_object_unref (protocol);
}

int
main(int argc, char *argv[])
{
  g_type_init();
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testdebugproto/DebugProto/Structs", test_structs);
  g_test_add_func ("/testdebugproto/DebugProto/ServiceInheritance",
                   test_service_inheritance);

  return g_test_run ();
}


