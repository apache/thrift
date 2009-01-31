#include <cstdlib>
#include <stdexcept>
#include <Thrift.h>
#include <transport/TTransportUtils.h>
#include <transport/TBufferTransports.h>
using namespace std;
using boost::shared_ptr;
using apache::thrift::transport::TTransportException;
using apache::thrift::transport::TPipedTransport;
using apache::thrift::transport::TMemoryBuffer;

int main() {
  shared_ptr<TMemoryBuffer> underlying(new TMemoryBuffer);
  shared_ptr<TMemoryBuffer> pipe(new TMemoryBuffer);
  shared_ptr<TPipedTransport> trans(new TPipedTransport(underlying, pipe));

  uint8_t buffer[4];

  underlying->write((uint8_t*)"abcd", 4);
  trans->readAll(buffer, 2);
  assert( string((char*)buffer, 2) == "ab" );
  trans->readEnd();
  assert( pipe->getBufferAsString() == "ab" );
  pipe->resetBuffer();
  underlying->write((uint8_t*)"ef", 2);
  trans->readAll(buffer, 2);
  assert( string((char*)buffer, 2) == "cd" );
  trans->readAll(buffer, 2);
  assert( string((char*)buffer, 2) == "ef" );
  trans->readEnd();
  assert( pipe->getBufferAsString() == "cdef" );

  return 0;

}
