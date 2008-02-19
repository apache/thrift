// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include <transport/TTransportUtils.h>

using std::string;

namespace facebook { namespace thrift { namespace transport {

uint32_t TBufferedTransport::read(uint8_t* buf, uint32_t len) {
  uint32_t need = len;

  // We don't have enough data yet
  if (rLen_-rPos_ < need) {
    // Copy out whatever we have
    if (rLen_-rPos_ > 0) {
      memcpy(buf, rBuf_+rPos_, rLen_-rPos_);
      need -= rLen_-rPos_;
      buf += rLen_-rPos_;
    }
    // Get more from underlying transport up to buffer size
    rLen_ = transport_->read(rBuf_, rBufSize_);
    rPos_ = 0;
  }

  // Hand over whatever we have
  uint32_t give = need;
  if (rLen_-rPos_ < give) {
    give = rLen_-rPos_;
  }
  memcpy(buf, rBuf_+rPos_, give);
  rPos_ += give;
  need -= give;
  return (len - need);
}

void TBufferedTransport::write(const uint8_t* buf, uint32_t len) {
  if (len == 0) {
    return;
  }

  uint32_t pos = 0;

  while ((len-pos) + wLen_ >= wBufSize_) {
    uint32_t copy = wBufSize_ - wLen_;
    memcpy(wBuf_ + wLen_, buf + pos, copy);

    transport_->write(wBuf_, wBufSize_);
    pos += copy;
    wLen_ = 0;
  }

  if ((len - pos) > 0) {
    memcpy(wBuf_ + wLen_, buf + pos, len - pos);
    wLen_ += len - pos;
  }
}

const uint8_t* TBufferedTransport::borrow(uint8_t* buf, uint32_t* len) {
  // The number of additional bytes we need from the underlying transport.
  // Could be zero or negative.
  uint32_t need = *len - (rLen_-rPos_);

  // If we have enough data, just hand over a pointer.
  if (need <= 0) {
    *len = rLen_-rPos_;
    return rBuf_+rPos_;
  }

  // If the request is bigger than our buffer, we are hosed.
  if (*len > rBufSize_) {
    return NULL;
  }

  // If we have less than half our buffer available,
  // or we need more space than is in the buffer,
  // shift the data we have down to the start.
  if ((rLen_ > rBufSize_/2) || (rLen_+need > rBufSize_)) {
    memmove(rBuf_, rBuf_+rPos_, rLen_-rPos_);
    rLen_ -= rPos_;
    rPos_ = 0;
  }

  // First try to fill up the buffer.
  uint32_t got = transport_->read(rBuf_+rLen_, rBufSize_-rLen_);
  rLen_ += got;
  need -= got;

  // If that fails, readAll until we get what we need.
  if (need > 0) {
    rLen_ += transport_->readAll(rBuf_+rLen_, need);
  }

  *len = rLen_-rPos_;
  return rBuf_+rPos_;
}

void TBufferedTransport::consume(uint32_t len) {
  if (rLen_-rPos_ >= len) {
    rPos_ += len;
  } else {
    throw TTransportException(TTransportException::BAD_ARGS,
                              "consume did not follow a borrow.");
  }
}

void TBufferedTransport::flush()  {
  // Write out any data waiting in the write buffer
  if (wLen_ > 0) {
    transport_->write(wBuf_, wLen_);
    wLen_ = 0;
  }

  // Flush the underlying transport
  transport_->flush();
}

uint32_t TFramedTransport::read(uint8_t* buf, uint32_t len) {
  if (!read_) {
    return transport_->read(buf, len);
  }

  uint32_t need = len;

  // We don't have enough data yet
  if (rLen_-rPos_ < need) {
    // Copy out whatever we have
    if (rLen_-rPos_ > 0) {
      memcpy(buf, rBuf_+rPos_, rLen_-rPos_);
      need -= rLen_-rPos_;
      buf += rLen_-rPos_;
    }

    // Read another chunk
    readFrame();
  }

  // Hand over whatever we have
  uint32_t give = need;
  if (rLen_-rPos_ < give) {
    give = rLen_-rPos_;
  }
  memcpy(buf, rBuf_+rPos_, give);
  rPos_ += give;
  need -= give;
  return (len - need);
}

void TFramedTransport::readFrame() {
  // Get rid of the old frame
  if (rBuf_ != NULL) {
    delete [] rBuf_;
    rBuf_ = NULL;
  }

  // Read in the next chunk size
  int32_t sz;
  transport_->readAll((uint8_t*)&sz, 4);
  sz = (int32_t)ntohl(sz);

  if (sz < 0) {
    throw TTransportException("Frame size has negative value");
  }

  // Read the frame payload, reset markers
  rBuf_ = new uint8_t[sz];
  transport_->readAll(rBuf_, sz);
  rPos_ = 0;
  rLen_ = sz;
}

void TFramedTransport::write(const uint8_t* buf, uint32_t len) {
  if (len == 0) {
    return;
  }

  // Shortcut out if not write mode
  if (!write_) {
    transport_->write(buf, len);
    return;
  }

  // Need to grow the buffer
  if (len + wLen_ >= wBufSize_) {

    // Double buffer size until sufficient
    while (wBufSize_ < len + wLen_) {
      wBufSize_ *= 2;
    }

    // Allocate new buffer
    uint8_t* wBuf2 = new uint8_t[wBufSize_];

    // Copy the old buffer to the new one
    memcpy(wBuf2, wBuf_, wLen_);

    // Now point buf to the new one
    delete [] wBuf_;
    wBuf_ = wBuf2;
  }

  // Copy data into buffer
  memcpy(wBuf_ + wLen_, buf, len);
  wLen_ += len;
}

void TFramedTransport::flush()  {
  if (!write_) {
    transport_->flush();
    return;
  }

  // Write frame size
  int32_t sz = wLen_;
  sz = (int32_t)htonl(sz);

  transport_->write((const uint8_t*)&sz, 4);

  // Write frame body
  if (wLen_ > 0) {
    transport_->write(wBuf_, wLen_);
  }

  // All done
  wLen_ = 0;

  // Flush the underlying
  transport_->flush();
}

const uint8_t* TFramedTransport::borrow(uint8_t* buf, uint32_t* len) {
  // Don't try to be clever with shifting buffers.
  // If we have enough data, give a pointer to it,
  // otherwise let the protcol use its slow path.
  if (read_ && (rLen_-rPos_ >= *len)) {
    *len = rLen_-rPos_;
    return rBuf_+rPos_;
  }
  return NULL;
}

void TFramedTransport::consume(uint32_t len) {
  if (rLen_-rPos_ >= len) {
    rPos_ += len;
  } else {
    throw TTransportException(TTransportException::BAD_ARGS,
                              "consume did not follow a borrow.");
  }
}

uint32_t TMemoryBuffer::read(uint8_t* buf, uint32_t len) {
  // Check avaible data for reading
  uint32_t avail = wPos_ - rPos_;
  if (avail == 0) {
    return 0;
  }

  // Decide how much to give
  uint32_t give = len;
  if (avail < len) {
    give = avail;
  }

  // Copy into buffer and increment rPos_
  memcpy(buf, buffer_ + rPos_, give);
  rPos_ += give;

  return give;
}

uint32_t TMemoryBuffer::readAppendToString(std::string& str, uint32_t len) {
  // Don't get some stupid assertion failure.
  if (buffer_ == NULL) {
    return 0;
  }

  // Check avaible data for reading
  uint32_t avail = wPos_ - rPos_;
  if (avail == 0) {
    return 0;
  }

  // Device how much to give
  uint32_t give = len;
  if (avail < len) {
    give = avail;
  }

  // Reserve memory, copy into string, and increment rPos_
  str.reserve(str.length()+give);
  str.append((char*)buffer_ + rPos_, give);
  rPos_ += give;

  return give;
}

void TMemoryBuffer::write(const uint8_t* buf, uint32_t len) {
  // Check available space
  uint32_t avail = bufferSize_ - wPos_;

  // Grow the buffer
  if (len > avail) {
    if (!owner_) {
      throw TTransportException("Insufficient space in external MemoryBuffer");
    }
    while (len > avail) {
      bufferSize_ *= 2;
      avail = bufferSize_ - wPos_;
    }
    buffer_ = (uint8_t*)std::realloc(buffer_, bufferSize_);
    if (buffer_ == NULL) {
      throw TTransportException("Out of memory.");
    }
  }

  // Copy into the buffer and increment wPos_
  memcpy(buffer_ + wPos_, buf, len);
  wPos_ += len;
}

const uint8_t* TMemoryBuffer::borrow(uint8_t* buf, uint32_t* len) {
  if (wPos_-rPos_ >= *len) {
    *len = wPos_-rPos_;
    return buffer_ + rPos_;
  }
  return NULL;
}

void TMemoryBuffer::consume(uint32_t len) {
  if (wPos_-rPos_ >= len) {
    rPos_ += len;
  } else {
    throw TTransportException(TTransportException::BAD_ARGS,
                              "consume did not follow a borrow.");
  }
}

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

}}} // facebook::thrift::transport
