#include <cstdlib>
#include <stdexcept>
#include <Thrift.h>
#include <transport/TFDTransport.h>
using apache::thrift::transport::TTransportException;
using apache::thrift::transport::TFDTransport;

class DummyException : std::exception {
};

int main() {
  {
    TFDTransport t(256, TFDTransport::NO_CLOSE_ON_DESTROY);
  }

  try {
    {
      TFDTransport t(256, TFDTransport::CLOSE_ON_DESTROY);
    }
    std::abort();
  } catch (TTransportException) {
  }

  try {
    {
      TFDTransport t(256, TFDTransport::CLOSE_ON_DESTROY);
      throw DummyException();
    }
    std::abort();
  } catch (TTransportException&) {
    abort();
  } catch (DummyException&) {
  }

  return 0;

}
