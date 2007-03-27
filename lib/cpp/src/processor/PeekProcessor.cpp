#include "PeekProcessor.h"

namespace facebook { namespace thrift { namespace processor { 

PeekProcessor::PeekProcessor() {
  memoryBuffer_.reset(new facebook::thrift::transport::TMemoryBuffer());
}
PeekProcessor::~PeekProcessor() {}

void PeekProcessor::initialize(boost::shared_ptr<facebook::thrift::TProcessor> actualProcessor,
                    boost::shared_ptr<facebook::thrift::protocol::TProtocolFactory> protocolFactory,
                    boost::shared_ptr<facebook::thrift::transport::TPipedTransportFactory> transportFactory) {
  actualProcessor_ = actualProcessor;
  pipedProtocol_ = protocolFactory->getProtocol(memoryBuffer_);
  transportFactory_ = transportFactory;
  transportFactory_->initializeTargetTransport(memoryBuffer_);
}

boost::shared_ptr<facebook::thrift::transport::TTransport> PeekProcessor::getPipedTransport(boost::shared_ptr<facebook::thrift::transport::TTransport> in) {
  return transportFactory_->getTransport(in);
}

bool PeekProcessor::process(boost::shared_ptr<facebook::thrift::protocol::TProtocol> in, 
                            boost::shared_ptr<facebook::thrift::protocol::TProtocol> out) {

  std::string fname;
  facebook::thrift::protocol::TMessageType mtype;
  int32_t seqid;
  in->readMessageBegin(fname, mtype, seqid);

  if (mtype != facebook::thrift::protocol::T_CALL) {
    throw facebook::thrift::TException("Unexpected message type");
  }

  // Peek at the name
  peekName(fname);

  facebook::thrift::protocol::TType ftype;
  int16_t fid;
  while (true) {
    in->readFieldBegin(fname, ftype, fid);
    if (ftype == facebook::thrift::protocol::T_STOP) {
      break;
    }

    // Peek at the variable
    peek(in, ftype, fid);
  }
  in->readMessageEnd();
  in->getTransport()->readEnd();

  // Done peeking at variables
  peekEnd();

  //
  // All the data is now in memoryBuffer_ and ready to be processed
  //

  // Let's first take a peek at the full data in memory
  uint8_t* buffer;
  uint32_t size;
  memoryBuffer_->getBuffer(&buffer, &size);
  peekBuffer(buffer, size);
  
  bool ret = actualProcessor_->process(pipedProtocol_, out);
  memoryBuffer_->resetBuffer();
  return ret;
}

void PeekProcessor::peekName(const std::string& fname) {
}

void PeekProcessor::peekBuffer(uint8_t* buffer, uint32_t size) {
}

void PeekProcessor::peek(boost::shared_ptr<facebook::thrift::protocol::TProtocol> in, 
                  facebook::thrift::protocol::TType ftype,
                  int16_t fid) {
  in->skip(ftype);
}

void PeekProcessor::peekEnd() {}

}}}
