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

#include <cassert>
#include <algorithm>

#include <transport/TBufferTransports.h>

using std::string;

namespace apache { namespace thrift { namespace transport {


uint32_t TBufferedTransport::readSlow(uint8_t* buf, uint32_t len) {
  uint32_t want = len;
  uint32_t have = rBound_ - rBase_;

  // We should only take the slow path if we can't satisfy the read
  // with the data already in the buffer.
  assert(have < want);

  // Copy out whatever we have.
  if (have > 0) {
    memcpy(buf, rBase_, have);
    want -= have;
    buf += have;
  }
  // Get more from underlying transport up to buffer size.
  // Note that this makes a lot of sense if len < rBufSize_
  // and almost no sense otherwise.  TODO(dreiss): Fix that
  // case (possibly including some readv hotness).
  setReadBuffer(rBuf_.get(), transport_->read(rBuf_.get(), rBufSize_));

  // Hand over whatever we have.
  uint32_t give = std::min(want, static_cast<uint32_t>(rBound_ - rBase_));
  memcpy(buf, rBase_, give);
  rBase_ += give;
  want -= give;

  return (len - want);
}

void TBufferedTransport::writeSlow(const uint8_t* buf, uint32_t len) {
  uint32_t have_bytes = wBase_ - wBuf_.get();
  uint32_t space = wBound_ - wBase_;
  // We should only take the slow path if we can't accomodate the write
  // with the free space already in the buffer.
  assert(wBound_ - wBase_ < static_cast<ptrdiff_t>(len));

  // Now here's the tricky question: should we copy data from buf into our
  // internal buffer and write it from there, or should we just write out
  // the current internal buffer in one syscall and write out buf in another.
  // If our currently buffered data plus buf is at least double our buffer
  // size, we will have to do two syscalls no matter what (except in the
  // degenerate case when our buffer is empty), so there is no use copying.
  // Otherwise, there is sort of a sliding scale.  If we have N-1 bytes
  // buffered and need to write 2, it would be crazy to do two syscalls.
  // On the other hand, if we have 2 bytes buffered and are writing 2N-3,
  // we can save a syscall in the short term by loading up our buffer, writing
  // it out, and copying the rest of the bytes into our buffer.  Of course,
  // if we get another 2-byte write, we haven't saved any syscalls at all,
  // and have just copied nearly 2N bytes for nothing.  Finding a perfect
  // policy would require predicting the size of future writes, so we're just
  // going to always eschew syscalls if we have less than 2N bytes to write.

  // The case where we have to do two syscalls.
  // This case also covers the case where the buffer is empty,
  // but it is clearer (I think) to think of it as two separate cases.
  if ((have_bytes + len >= 2*wBufSize_) || (have_bytes == 0)) {
    // TODO(dreiss): writev
    if (have_bytes > 0) {
      transport_->write(wBuf_.get(), have_bytes);
    }
    transport_->write(buf, len);
    wBase_ = wBuf_.get();
    return;
  }

  // Fill up our internal buffer for a write.
  memcpy(wBase_, buf, space);
  buf += space;
  len -= space;
  transport_->write(wBuf_.get(), wBufSize_);

  // Copy the rest into our buffer.
  assert(len < wBufSize_);
  memcpy(wBuf_.get(), buf, len);
  wBase_ = wBuf_.get() + len;
  return;
}

const uint8_t* TBufferedTransport::borrowSlow(uint8_t* buf, uint32_t* len) {
  // If the request is bigger than our buffer, we are hosed.
  if (*len > rBufSize_) {
    return NULL;
  }

  // The number of bytes of data we have already.
  uint32_t have = rBound_ - rBase_;
  // The number of additional bytes we need from the underlying transport.
  int32_t need = *len - have;
  // The space from the start of the buffer to the end of our data.
  uint32_t offset = rBound_ - rBuf_.get();
  assert(need > 0);

  // If we have less than half our buffer space available, shift the data
  // we have down to the start.  If the borrow is big compared to our buffer,
  // this could be kind of a waste, but if the borrow is small, it frees up
  // space at the end of our buffer to do a bigger single read from the
  // underlying transport.  Also, if our needs extend past the end of the
  // buffer, we have to do a copy no matter what.
  if ((offset > rBufSize_/2) || (offset + need > rBufSize_)) {
    memmove(rBuf_.get(), rBase_, have);
    setReadBuffer(rBuf_.get(), have);
  }

  // First try to fill up the buffer.
  uint32_t got = transport_->read(rBound_, rBufSize_ - have);
  rBound_ += got;
  need -= got;

  // If that fails, readAll until we get what we need.
  if (need > 0) {
    rBound_ += transport_->readAll(rBound_, need);
  }

  *len = rBound_ - rBase_;
  return rBase_;
}

void TBufferedTransport::flush()  {
  // Write out any data waiting in the write buffer.
  uint32_t have_bytes = wBase_ - wBuf_.get();
  if (have_bytes > 0) {
    // Note that we reset wBase_ prior to the underlying write
    // to ensure we're in a sane state (i.e. internal buffer cleaned)
    // if the underlying write throws up an exception
    wBase_ = wBuf_.get();
    transport_->write(wBuf_.get(), have_bytes);
  }

  // Flush the underlying transport.
  transport_->flush();
}


uint32_t TFramedTransport::readSlow(uint8_t* buf, uint32_t len) {
  uint32_t want = len;
  uint32_t have = rBound_ - rBase_;

  // We should only take the slow path if we can't satisfy the read
  // with the data already in the buffer.
  assert(have < want);

  // Copy out whatever we have.
  if (have > 0) {
    memcpy(buf, rBase_, have);
    want -= have;
    buf += have;
  }

  // Read another frame.
  readFrame();

  // TODO(dreiss): Should we warn when reads cross frames?

  // Hand over whatever we have.
  uint32_t give = std::min(want, static_cast<uint32_t>(rBound_ - rBase_));
  memcpy(buf, rBase_, give);
  rBase_ += give;
  want -= give;

  return (len - want);
}

void TFramedTransport::readFrame() {
  // TODO(dreiss): Think about using readv here, even though it would
  // result in (gasp) read-ahead.

  // Read the size of the next frame.
  int32_t sz;
  transport_->readAll((uint8_t*)&sz, sizeof(sz));
  sz = ntohl(sz);

  if (sz < 0) {
    throw TTransportException("Frame size has negative value");
  }

  // Read the frame payload, and reset markers.
  if (sz > static_cast<int32_t>(rBufSize_)) {
    rBuf_.reset(new uint8_t[sz]);
    rBufSize_ = sz;
  }
  transport_->readAll(rBuf_.get(), sz);
  setReadBuffer(rBuf_.get(), sz);
}

void TFramedTransport::writeSlow(const uint8_t* buf, uint32_t len) {
  // Double buffer size until sufficient.
  uint32_t have = wBase_ - wBuf_.get();
  while (wBufSize_ < len + have) {
    wBufSize_ *= 2;
  }

  // TODO(dreiss): Consider modifying this class to use malloc/free
  // so we can use realloc here.

  // Allocate new buffer.
  uint8_t* new_buf = new uint8_t[wBufSize_];

  // Copy the old buffer to the new one.
  memcpy(new_buf, wBuf_.get(), have);

  // Now point buf to the new one.
  wBuf_.reset(new_buf);
  wBase_ = wBuf_.get() + have;
  wBound_ = wBuf_.get() + wBufSize_;

  // Copy the data into the new buffer.
  memcpy(wBase_, buf, len);
  wBase_ += len;
}

void TFramedTransport::flush()  {
  int32_t sz_hbo, sz_nbo;
  assert(wBufSize_ > sizeof(sz_nbo));

  // Slip the frame size into the start of the buffer.
  sz_hbo = wBase_ - (wBuf_.get() + sizeof(sz_nbo));
  sz_nbo = (int32_t)htonl((uint32_t)(sz_hbo));
  memcpy(wBuf_.get(), (uint8_t*)&sz_nbo, sizeof(sz_nbo));

  if (sz_hbo > 0) {
    // Note that we reset wBase_ (with a pad for the frame size)
    // prior to the underlying write to ensure we're in a sane state
    // (i.e. internal buffer cleaned) if the underlying write throws
    // up an exception
    wBase_ = wBuf_.get() + sizeof(sz_nbo);

    // Write size and frame body.
    transport_->write(wBuf_.get(), sizeof(sz_nbo)+sz_hbo);
  }

  // Flush the underlying transport.
  transport_->flush();
}

const uint8_t* TFramedTransport::borrowSlow(uint8_t* buf, uint32_t* len) {
  // Don't try to be clever with shifting buffers.
  // If the fast path failed let the protocol use its slow path.
  // Besides, who is going to try to borrow across messages?
  return NULL;
}


void TMemoryBuffer::computeRead(uint32_t len, uint8_t** out_start, uint32_t* out_give) {
  // Correct rBound_ so we can use the fast path in the future.
  rBound_ = wBase_;

  // Decide how much to give.
  uint32_t give = std::min(len, available_read());

  *out_start = rBase_;
  *out_give = give;

  // Preincrement rBase_ so the caller doesn't have to.
  rBase_ += give;
}

uint32_t TMemoryBuffer::readSlow(uint8_t* buf, uint32_t len) {
  uint8_t* start;
  uint32_t give;
  computeRead(len, &start, &give);

  // Copy into the provided buffer.
  memcpy(buf, start, give);

  return give;
}

uint32_t TMemoryBuffer::readAppendToString(std::string& str, uint32_t len) {
  // Don't get some stupid assertion failure.
  if (buffer_ == NULL) {
    return 0;
  }

  uint8_t* start;
  uint32_t give;
  computeRead(len, &start, &give);

  // Append to the provided string.
  str.append((char*)start, give);

  return give;
}

void TMemoryBuffer::ensureCanWrite(uint32_t len) {
  // Check available space
  uint32_t avail = available_write();
  if (len <= avail) {
    return;
  }

  if (!owner_) {
    throw TTransportException("Insufficient space in external MemoryBuffer");
  }

  // Grow the buffer as necessary.
  while (len > avail) {
    bufferSize_ *= 2;
    wBound_ = buffer_ + bufferSize_;
    avail = available_write();
  }

  // Allocate into a new pointer so we don't bork ours if it fails.
  void* new_buffer = std::realloc(buffer_, bufferSize_);
  if (new_buffer == NULL) {
    throw TTransportException("Out of memory.");
  }

  ptrdiff_t offset = (uint8_t*)new_buffer - buffer_;
  buffer_ += offset;
  rBase_ += offset;
  rBound_ += offset;
  wBase_ += offset;
  wBound_ += offset;
}

void TMemoryBuffer::writeSlow(const uint8_t* buf, uint32_t len) {
  ensureCanWrite(len);

  // Copy into the buffer and increment wBase_.
  memcpy(wBase_, buf, len);
  wBase_ += len;
}

void TMemoryBuffer::wroteBytes(uint32_t len) {
  uint32_t avail = available_write();
  if (len > avail) {
    throw TTransportException("Client wrote more bytes than size of buffer.");
  }
  wBase_ += len;
}

const uint8_t* TMemoryBuffer::borrowSlow(uint8_t* buf, uint32_t* len) {
  rBound_ = wBase_;
  if (available_read() >= *len) {
    *len = available_read();
    return rBase_;
  }
  return NULL;
}

}}} // apache::thrift::transport
