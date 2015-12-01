#include <thrift/c_glib/protocol/thrift_binary_protocol.h>
#include <thrift/c_glib/protocol/thrift_protocol.h>
#include <thrift/c_glib/transport/thrift_memory_buffer.h>
#include <thrift/c_glib/transport/thrift_transport.h>
#include "gen-c_glib/t_test_debug_proto_test_types.h"

static void struct_read_write_length_should_equal() {
  GError* error = NULL;
  ThriftTransport* transport
      = THRIFT_TRANSPORT(g_object_new(THRIFT_TYPE_MEMORY_BUFFER, "buf_size", 2048, NULL));
  ThriftProtocol* protocol
      = THRIFT_PROTOCOL(g_object_new(THRIFT_TYPE_BINARY_PROTOCOL, "transport", transport, NULL));
  TTestBonk* src = g_object_new(T_TEST_TYPE_BONK, NULL);
  TTestBonk* dst = g_object_new(T_TEST_TYPE_BONK, NULL);
  TTestBonkClass* cls = T_TEST_BONK_GET_CLASS(src);

  int write_len = THRIFT_STRUCT_CLASS(cls)->write(THRIFT_STRUCT(src), protocol, &error);
  g_assert(!error);
  g_assert(write_len > 0);

  int read_len = THRIFT_STRUCT_CLASS(cls)->read(THRIFT_STRUCT(dst), protocol, &error);
  g_assert(!error);
  g_assert_cmpint(write_len, ==, read_len);

  g_object_unref(dst);
  g_object_unref(src);
  g_object_unref(protocol);
  g_object_unref(transport);
}

int main(int argc, char* argv[]) {
#if (!GLIB_CHECK_VERSION(2, 36, 0))
  g_type_init();
#endif
  g_test_init(&argc, &argv, NULL);

  g_test_add_func("/testserialization/StructReadWriteLengthShouldEqual",
                  struct_read_write_length_should_equal);
  return g_test_run();
}
