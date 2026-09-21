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

#include <glib.h>
#include <glib-object.h>

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/thrift_configuration.h>
#include <thrift/c_glib/thrift_struct.h>
#include <thrift/c_glib/protocol/thrift_binary_protocol.h>
#include <thrift/c_glib/protocol/thrift_protocol.h>
#include <thrift/c_glib/protocol/thrift_stored_message_protocol.h>
#include <thrift/c_glib/transport/thrift_memory_buffer.h>
#include <thrift/c_glib/transport/thrift_transport.h>

#include "gen-c_glib/t_test_recursion_limit_test_types.h"

/* Struct reads through the generated code must honour the recursion limit
   configured on the protocol's transport. */

#define TEST_RECURSION_LIMIT 8
#define TEST_BUFFER_SIZE 4096

/* the generated RecTree reader, wrapped so the tests can count how many
   levels of a message were actually read */
static gint32 (*rec_tree_read) (ThriftStruct *object, ThriftProtocol *protocol,
                                GError **error);
static gint levels_read = 0;

static gint32
counting_rec_tree_read (ThriftStruct *object, ThriftProtocol *protocol,
                        GError **error)
{
  levels_read++;
  return rec_tree_read (object, protocol, error);
}

/* a binary protocol over a memory buffer, with the given recursion limit
   configured on the buffer, or none if recursion_limit is negative */
static ThriftProtocol *
new_protocol (gint recursion_limit)
{
  ThriftConfiguration *configuration = NULL;
  ThriftMemoryBuffer *tbuffer = NULL;
  ThriftProtocol *protocol = NULL;

  if (recursion_limit >= 0)
  {
    configuration = g_object_new (THRIFT_TYPE_CONFIGURATION,
                                  "recursion_limit", recursion_limit, NULL);
  }
  tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER,
                          "buf_size", TEST_BUFFER_SIZE,
                          "configuration", configuration, NULL);
  protocol = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL,
                           "transport", tbuffer, NULL);

  g_object_unref (tbuffer);
  if (configuration != NULL)
  {
    g_object_unref (configuration);
  }
  return protocol;
}

/* writes a well-formed RecTree nested depth levels deep, each level holding
   the next one as its only child */
static void
write_nested_tree (ThriftProtocol *protocol, gint depth)
{
  TTestRecTree *root = g_object_new (T_TEST_TYPE_REC_TREE, NULL);
  TTestRecTree *level = root;
  GError *error = NULL;
  gint i;

  for (i = 1; i < depth; i++)
  {
    TTestRecTree *child = g_object_new (T_TEST_TYPE_REC_TREE, NULL);
    child->item = i;
    g_ptr_array_add (level->children, child);
    level = child;
  }

  g_assert (thrift_struct_write (THRIFT_STRUCT (root), protocol, &error) > 0);
  g_assert (error == NULL);
  g_object_unref (root);
}

/* the number of levels in a tree read back from a nested one */
static gint
nested_tree_depth (TTestRecTree *tree)
{
  gint depth = 1;

  while (tree->children->len > 0)
  {
    tree = g_ptr_array_index (tree->children, 0);
    g_assert_cmpint (tree->item, ==, depth);
    depth++;
  }
  return depth;
}

/* discards what a refused read left behind in the transport */
static void
discard_unread (ThriftProtocol *protocol)
{
  gchar scratch[TEST_BUFFER_SIZE];

  while (thrift_transport_read (protocol->transport, scratch, sizeof (scratch),
                                NULL) > 0)
  {
    /* keep reading until the buffer is empty */
  }
}

/* reads a RecTree, counting the levels read */
static gint32
read_tree (ThriftProtocol *protocol, TTestRecTree **tree, GError **error)
{
  *tree = g_object_new (T_TEST_TYPE_REC_TREE, NULL);
  levels_read = 0;
  return thrift_struct_read (THRIFT_STRUCT (*tree), protocol, error);
}

static void
test_read_nested_past_limit (void)
{
  ThriftProtocol *protocol = new_protocol (TEST_RECURSION_LIMIT);
  TTestRecTree *tree = NULL;
  GError *error = NULL;

  write_nested_tree (protocol, TEST_RECURSION_LIMIT + 10);

  /* the read stops at the first level past the limit rather than following
     the message all the way down */
  g_assert_cmpint (read_tree (protocol, &tree, &error), ==, -1);
  g_assert (error != NULL);
  g_assert (error->domain == THRIFT_PROTOCOL_ERROR);
  g_assert_cmpint (error->code, ==, THRIFT_PROTOCOL_ERROR_DEPTH_LIMIT);
  g_assert_cmpint (levels_read, ==, TEST_RECURSION_LIMIT);

  g_error_free (error);
  g_object_unref (tree);
  g_object_unref (protocol);
}

static void
test_read_nested_to_limit (void)
{
  ThriftProtocol *protocol = new_protocol (TEST_RECURSION_LIMIT);
  TTestRecTree *tree = NULL;
  GError *error = NULL;

  /* a message nested exactly as deep as the limit reads in full */
  write_nested_tree (protocol, TEST_RECURSION_LIMIT);
  g_assert_cmpint (read_tree (protocol, &tree, &error), >, 0);
  g_assert (error == NULL);
  g_assert_cmpint (levels_read, ==, TEST_RECURSION_LIMIT);
  g_assert_cmpint (nested_tree_depth (tree), ==, TEST_RECURSION_LIMIT);
  g_object_unref (tree);

  /* one level more is refused */
  write_nested_tree (protocol, TEST_RECURSION_LIMIT + 1);
  g_assert_cmpint (read_tree (protocol, &tree, &error), ==, -1);
  g_assert (error != NULL);
  g_assert (error->domain == THRIFT_PROTOCOL_ERROR);
  g_assert_cmpint (error->code, ==, THRIFT_PROTOCOL_ERROR_DEPTH_LIMIT);
  g_assert_cmpint (levels_read, ==, TEST_RECURSION_LIMIT);

  g_error_free (error);
  g_object_unref (tree);
  g_object_unref (protocol);
}

static void
test_read_wide_tree (void)
{
  ThriftProtocol *protocol = new_protocol (TEST_RECURSION_LIMIT);
  TTestRecTree *root = g_object_new (T_TEST_TYPE_REC_TREE, NULL);
  TTestRecTree *tree = NULL;
  GError *error = NULL;
  const gint width = TEST_RECURSION_LIMIT * 3;
  gint i;

  /* siblings sit at the same depth, so any number of them read fine */
  for (i = 0; i < width; i++)
  {
    g_ptr_array_add (root->children,
                     g_object_new (T_TEST_TYPE_REC_TREE, NULL));
  }
  g_assert (thrift_struct_write (THRIFT_STRUCT (root), protocol, &error) > 0);
  g_assert (error == NULL);

  g_assert_cmpint (read_tree (protocol, &tree, &error), >, 0);
  g_assert (error == NULL);
  g_assert_cmpint (levels_read, ==, width + 1);
  g_assert_cmpint (tree->children->len, ==, width);

  g_object_unref (tree);
  g_object_unref (root);
  g_object_unref (protocol);
}

static void
test_read_after_refused_read (void)
{
  ThriftProtocol *protocol = new_protocol (TEST_RECURSION_LIMIT);
  TTestRecTree *tree = NULL;
  GError *error = NULL;

  write_nested_tree (protocol, TEST_RECURSION_LIMIT + 10);
  g_assert_cmpint (read_tree (protocol, &tree, &error), ==, -1);
  g_assert (error != NULL);
  g_assert_cmpint (error->code, ==, THRIFT_PROTOCOL_ERROR_DEPTH_LIMIT);
  g_clear_error (&error);
  g_object_unref (tree);
  discard_unread (protocol);

  /* the refused read leaves no depth behind on the protocol: the next
     message may again be nested right up to the limit */
  write_nested_tree (protocol, TEST_RECURSION_LIMIT);
  g_assert_cmpint (read_tree (protocol, &tree, &error), >, 0);
  g_assert (error == NULL);
  g_assert_cmpint (levels_read, ==, TEST_RECURSION_LIMIT);
  g_assert_cmpint (nested_tree_depth (tree), ==, TEST_RECURSION_LIMIT);

  g_object_unref (tree);
  g_object_unref (protocol);
}

static void
test_read_through_protocol_decorator (void)
{
  /* no configuration, so the wrapped protocol and the decorator both work to
     the default limit */
  ThriftProtocol *protocol = new_protocol (-1);
  ThriftProtocol *decorator = NULL;
  TTestRecTree *tree = NULL;
  GError *error = NULL;

  decorator = g_object_new (THRIFT_TYPE_STORED_MESSAGE_PROTOCOL,
                            "protocol", protocol,
                            "name", "echoTree",
                            "type", T_CALL,
                            "seqid", 1,
                            NULL);

  /* each level is counted once, on the protocol the struct is read from */
  write_nested_tree (protocol, DEFAULT_RECURSION_DEPTH);
  g_assert_cmpint (read_tree (decorator, &tree, &error), >, 0);
  g_assert (error == NULL);
  g_assert_cmpint (levels_read, ==, DEFAULT_RECURSION_DEPTH);
  g_assert_cmpint (nested_tree_depth (tree), ==, DEFAULT_RECURSION_DEPTH);
  g_object_unref (tree);

  write_nested_tree (protocol, DEFAULT_RECURSION_DEPTH + 1);
  g_assert_cmpint (read_tree (decorator, &tree, &error), ==, -1);
  g_assert (error != NULL);
  g_assert (error->domain == THRIFT_PROTOCOL_ERROR);
  g_assert_cmpint (error->code, ==, THRIFT_PROTOCOL_ERROR_DEPTH_LIMIT);
  g_assert_cmpint (levels_read, ==, DEFAULT_RECURSION_DEPTH);

  g_error_free (error);
  g_object_unref (tree);
  g_object_unref (decorator);
  g_object_unref (protocol);
}

int
main (int argc, char *argv[])
{
  gpointer rec_tree_class;
  int result;

#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init ();
#endif

  g_test_init (&argc, &argv, NULL);

  rec_tree_class = g_type_class_ref (T_TEST_TYPE_REC_TREE);
  rec_tree_read = THRIFT_STRUCT_CLASS (rec_tree_class)->read;
  THRIFT_STRUCT_CLASS (rec_tree_class)->read = counting_rec_tree_read;

  g_test_add_func ("/testrecursionlimit/ReadNestedPastLimit",
                   test_read_nested_past_limit);
  g_test_add_func ("/testrecursionlimit/ReadNestedToLimit",
                   test_read_nested_to_limit);
  g_test_add_func ("/testrecursionlimit/ReadWideTree",
                   test_read_wide_tree);
  g_test_add_func ("/testrecursionlimit/ReadAfterRefusedRead",
                   test_read_after_refused_read);
  g_test_add_func ("/testrecursionlimit/ReadThroughProtocolDecorator",
                   test_read_through_protocol_decorator);

  result = g_test_run ();

  g_type_class_unref (rec_tree_class);
  return result;
}
