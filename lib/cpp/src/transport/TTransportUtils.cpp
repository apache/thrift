// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include <transport/TTransportUtils.h>

using std::string;

namespace apache { namespace thrift { namespace transport {

uint32_t TPipedTransport::read(uint8_t* buf, uint32_t len) {
  uint32_t need = len;

  // We don't have enough data yet
  if (rLen_-rPos_ < need) {
    // Copy out whatever we have
    if (rLen_-rPos_ > 0) {
      memcpy(buf, rBuf_+rPos_, rLen_-rPos_);
      need -= rLen_-rPos_;
      buf += rLen_-rPos_;
      rPos_ = rLen_;
    }

    // Double the size of the underlying buffer if it is full
    if (rLen_ == rBufSize_) {
      rBufSize_ *=2;
      rBuf_ = (uint8_t *)std::realloc(rBuf_, sizeof(uint8_t) * rBufSize_);
    }

    // try to fill up the buffer
    rLen_ += srcTrans_->read(rBuf_+rPos_, rBufSize_ - rPos_);
  }


  // Hand over whatever we have
  uint32_t give = need;
  if (rLen_-rPos_ < give) {
    give = rLen_-rPos_;
  }
  if (give > 0) {
    memcpy(buf, rBuf_+rPos_, give);
    rPos_ += give;
    need -= give;
  }

  return (len - need);
}

void TPipedTransport::write(const uint8_t* buf, uint32_t len) {
  if (len == 0) {
    return;
  }

  // Make the buffer as big as it needs to be
  if ((len + wLen_) >= wBufSize_) {
    uint32_t newBufSize = wBufSize_*2;
    while ((len + wLen_) >= newBufSize) {
      newBufSize *= 2;
    }
    wBuf_ = (uint8_t *)std::realloc(wBuf_, sizeof(uint8_t) * newBufSize);
    wBufSize_ = newBufSize;
  }

  // Copy into the buffer
  memcpy(wBuf_ + wLen_, buf, len);
  wLen_ += len;
}

void TPipedTransport::flush()  {
  // Write out any data waiting in the write buffer
  if (wLen_ > 0) {
    srcTrans_->write(wBuf_, wLen_);
    wLen_ = 0;
  }

  // Flush the underlying transport
  srcTrans_->flush();
}

TPipedFileReaderTransport::TPipedFileReaderTransport(boost::shared_ptr<TFileReaderTransport> srcTrans, boost::shared_ptr<TTransport> dstTrans)
  : TPipedTransport(srcTrans, dstTrans),
    srcTrans_(srcTrans) {
}

TPipedFileReaderTransport::~TPipedFileReaderTransport() {
}

bool TPipedFileReaderTransport::isOpen() {
  return TPipedTransport::isOpen();
}

bool TPipedFileReaderTransport::peek() {
  return TPipedTransport::peek();
}

void TPipedFileReaderTransport::open() {
  TPipedTransport::open();
}

void TPipedFileReaderTransport::close() {
  TPipedTransport::close();
}

uint32_t TPipedFileReaderTransport::read(uint8_t* buf, uint32_t len) {
  return TPipedTransport::read(buf, len);
}

uint32_t TPipedFileReaderTransport::readAll(uint8_t* buf, uint32_t len) {
  uint32_t have = 0;
  uint32_t get = 0;

  while (have < len) {
    get = read(buf+have, len-have);
    if (get <= 0) {
      throw TEOFException();
    }
    have += get;
  }

  return have;
}

void TPipedFileReaderTransport::readEnd() {
  TPipedTransport::readEnd();
}

void TPipedFileReaderTransport::write(const uint8_t* buf, uint32_t len) {
  TPipedTransport::write(buf, len);
}

void TPipedFileReaderTransport::writeEnd() {
  TPipedTransport::writeEnd();
}

void TPipedFileReaderTransport::flush() {
  TPipedTransport::flush();
}

int32_t TPipedFileReaderTransport::getReadTimeout() {
  return srcTrans_->getReadTimeout();
}

void TPipedFileReaderTransport::setReadTimeout(int32_t readTimeout) {
  srcTrans_->setReadTimeout(readTimeout);
}

uint32_t TPipedFileReaderTransport::getNumChunks() {
  return srcTrans_->getNumChunks();
}

uint32_t TPipedFileReaderTransport::getCurChunk() {
  return srcTrans_->getCurChunk();
}

void TPipedFileReaderTransport::seekToChunk(int32_t chunk) {
  srcTrans_->seekToChunk(chunk);
}

void TPipedFileReaderTransport::seekToEnd() {
  srcTrans_->seekToEnd();
}

}}} // apache::thrift::transport
