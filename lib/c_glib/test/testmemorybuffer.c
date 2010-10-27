#include <assert.h>
#include <netdb.h>

#include "transport/thrift_transport.h"
#include "transport/thrift_socket.h"
#include "transport/thrift_server_transport.h"
#include "transport/thrift_server_socket.h"

#define TEST_DATA { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j' }

#include "../src/transport/thrift_memory_buffer.c"

/* test object creation and destruction */
static void
test_create_and_destroy(void)
{
  GObject *object = NULL;
  object = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, NULL);
  assert (object != NULL);
  g_object_unref (object);
}

static void
test_open_and_close(void)
{
  ThriftMemoryBuffer *tbuffer = NULL;

  /* create a ThriftMemoryBuffer */
  tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, NULL);

  /* this shouldn't work */
  assert (thrift_memory_buffer_open (THRIFT_TRANSPORT (tbuffer), NULL) == TRUE);
  assert (thrift_memory_buffer_is_open (THRIFT_TRANSPORT (tbuffer)) == TRUE);
  assert (thrift_memory_buffer_close (THRIFT_TRANSPORT (tbuffer), NULL) == TRUE);
  g_object_unref (tbuffer);
}

static void
test_read_and_write(void)
{
  ThriftMemoryBuffer *tbuffer = NULL;
  guchar buf[10] = TEST_DATA;
  guchar read[10];
  GError *error = NULL;

  tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf_size", 5, NULL);
  assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer),
                                      (gpointer) buf,
                                      10, &error) == FALSE);
  assert (error != NULL);
  g_error_free (error);
  error = NULL;
  g_object_unref (tbuffer);

  tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf_size", 15, NULL);
  assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer),
                                      (gpointer) buf, 10, &error) == TRUE);
  assert (error == NULL);

  assert (thrift_memory_buffer_read (THRIFT_TRANSPORT (tbuffer),
                                     &read, 10, &error) > 0);
  assert (error == NULL);
}

int
main(void)
{
  g_type_init();
  test_create_and_destroy();
  test_open_and_close();
  test_read_and_write();

  return 0;
}

