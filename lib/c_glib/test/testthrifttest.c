#include <assert.h>
#include <netdb.h>

#include "transport/thrift_server_transport.h"
#include "transport/thrift_server_socket.h"

static const char TEST_ADDRESS[] = "localhost";
static const int TEST_PORT = 64444;

static void thrift_server (const int port);

static void
thrift_server (const int port)
{
  ThriftServerSocket *tsocket = g_object_new (THRIFT_TYPE_SERVER_SOCKET,
                                              "port", port, NULL);

  g_object_unref (tsocket);
}

int
main(void)
{
  g_type_init ();
  thrift_server (TEST_PORT);
  return 0;
}

