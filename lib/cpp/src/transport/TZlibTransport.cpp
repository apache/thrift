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
#include <cstring>
#include <algorithm>
#include <transport/TZlibTransport.h>
#include <zlib.h>

using std::string;

namespace apache { namespace thrift { namespace transport {

// Don't call this outside of the constructor.
void TZlibTransport::initZlib() {
  int rv;
  bool r_init = false;
  try {
    rstream_ = new z_stream;
    wstream_ = new z_stream;

    rstream_->zalloc = Z_NULL;
    wstream_->zalloc = Z_NULL;
    rstream_->zfree  = Z_NULL;
    wstream_->zfree  = Z_NULL;
    rstream_->opaque = Z_NULL;
    wstream_->opaque = Z_NULL;

    rstream_->next_in   = crbuf_;
    wstream_->next_in   = uwbuf_;
    rstream_->next_out  = urbuf_;
    wstream_->next_out  = cwbuf_;
    rstream_->avail_in  = 0;
    wstream_->avail_in  = 0;
    rstream_->avail_out = urbuf_size_;
    wstream_->avail_out = cwbuf_size_;

    rv = inflateInit(rstream_);
    checkZlibRv(rv, rstream_->msg);

    // Have to set this flag so we know whether to de-initialize.
    r_init = true;

    rv = deflateInit(wstream_, Z_DEFAULT_COMPRESSION);
    checkZlibRv(rv, wstream_->msg);
  }

  catch (...) {
    if (r_init) {
      rv = inflateEnd(rstream_);
      checkZlibRvNothrow(rv, rstream_->msg);
    }
    // There is no way we can get here if wstream_ was initialized.

    throw;
  }
}

inline void TZlibTransport::checkZlibRv(int status, const char* message) {
  if (status != Z_OK) {
    throw TZlibTransportException(status, message);
  }
}

inline void TZlibTransport::checkZlibRvNothrow(int status, const char* message) {
  if (status != Z_OK) {
    string output = "TZlibTransport: zlib failure in destructor: " +
      TZlibTransportException::errorMessage(status, message);
    GlobalOutput(output.c_str());
  }
}

TZlibTransport::~TZlibTransport() {
  int rv;
  rv = inflateEnd(rstream_);
  checkZlibRvNothrow(rv, rstream_->msg);
  rv = deflateEnd(wstream_);
  checkZlibRvNothrow(rv, wstream_->msg);

  delete[] urbuf_;
  delete[] crbuf_;
  delete[] uwbuf_;
  delete[] cwbuf_;
  delete rstream_;
  delete wstream_;
}

bool TZlibTransport::isOpen() {
  return (readAvail() > 0) || transport_->isOpen();
}

// READING STRATEGY
//
// We have two buffers for reading: one containing the compressed data (crbuf_)
// and one containing the uncompressed data (urbuf_).  When read is called,
// we repeat the following steps until we have satisfied the request:
// - Copy data from urbuf_ into the caller's buffer.
// - If we had enough, return.
// - If urbuf_ is empty, read some data into it from the underlying transport.
// - Inflate data from crbuf_ into urbuf_.
//
// In standalone objects, we set input_ended_ to true when inflate returns
// Z_STREAM_END.  This allows to make sure that a checksum was verified.

inline int TZlibTransport::readAvail() {
  return urbuf_size_ - rstream_->avail_out - urpos_;
}

uint32_t TZlibTransport::read(uint8_t* buf, uint32_t len) {
  int need = len;

  // TODO(dreiss): Skip urbuf on big reads.

  while (true) {
    // Copy out whatever we have available, then give them the min of
    // what we have and what they want, then advance indices.
    int give = std::min(readAvail(), need);
    memcpy(buf, urbuf_ + urpos_, give);
    need -= give;
    buf += give;
    urpos_ += give;

    // If they were satisfied, we are done.
    if (need == 0) {
      return len;
    }

    // If we get to this point, we need to get some more data.

    // If zlib has reported the end of a stream, we can't really do any more.
    if (input_ended_) {
      return len - need;
    }

    // The uncompressed read buffer is empty, so reset the stream fields.
    rstream_->next_out  = urbuf_;
    rstream_->avail_out = urbuf_size_;
    urpos_ = 0;

    // If we don't have any more compressed data available,
    // read some from the underlying transport.
    if (rstream_->avail_in == 0) {
      uint32_t got = transport_->read(crbuf_, crbuf_size_);
      if (got == 0) {
        return len - need;
      }
      rstream_->next_in  = crbuf_;
      rstream_->avail_in = got;
    }

    // We have some compressed data now.  Uncompress it.
    int zlib_rv = inflate(rstream_, Z_SYNC_FLUSH);

    if (zlib_rv == Z_STREAM_END) {
      if (standalone_) {
        input_ended_ = true;
      }
    } else {
      checkZlibRv(zlib_rv, rstream_->msg);
    }

    // Okay.  The read buffer should have whatever we can give it now.
    // Loop back to the start and try to give some more.
  }
}


// WRITING STRATEGY
//
// We buffer up small writes before sending them to zlib, so our logic is:
// - Is the write big?
//   - Send the buffer to zlib.
//   - Send this data to zlib.
// - Is the write small?
//   - Is there insufficient space in the buffer for it?
//     - Send the buffer to zlib.
//   - Copy the data to the buffer.
//
// We have two buffers for writing also: the uncompressed buffer (mentioned
// above) and the compressed buffer.  When sending data to zlib we loop over
// the following until the source (uncompressed buffer or big write) is empty:
// - Is there no more space in the compressed buffer?
//   - Write the compressed buffer to the underlying transport.
// - Deflate from the source into the compressed buffer.

void TZlibTransport::write(const uint8_t* buf, uint32_t len) {
  // zlib's "deflate" function has enough logic in it that I think
  // we're better off (performance-wise) buffering up small writes.
  if ((int)len > MIN_DIRECT_DEFLATE_SIZE) {
    flushToZlib(uwbuf_, uwpos_);
    uwpos_ = 0;
    flushToZlib(buf, len);
  } else if (len > 0) {
    if (uwbuf_size_ - uwpos_ < (int)len) {
      flushToZlib(uwbuf_, uwpos_);
      uwpos_ = 0;
    }
    memcpy(uwbuf_ + uwpos_, buf, len);
    uwpos_ += len;
  }
}

void TZlibTransport::flush()  {
  flushToZlib(uwbuf_, uwpos_, true);
  assert((int)wstream_->avail_out != cwbuf_size_);
  transport_->write(cwbuf_, cwbuf_size_ - wstream_->avail_out);
  transport_->flush();
}

void TZlibTransport::flushToZlib(const uint8_t* buf, int len, bool finish) {
  int flush = (finish ? Z_FINISH : Z_NO_FLUSH);

  wstream_->next_in  = const_cast<uint8_t*>(buf);
  wstream_->avail_in = len;

  while (wstream_->avail_in > 0 || finish) {
    // If our ouput buffer is full, flush to the underlying transport.
    if (wstream_->avail_out == 0) {
      transport_->write(cwbuf_, cwbuf_size_);
      wstream_->next_out  = cwbuf_;
      wstream_->avail_out = cwbuf_size_;
    }

    int zlib_rv = deflate(wstream_, flush);

    if (finish && zlib_rv == Z_STREAM_END) {
      assert(wstream_->avail_in == 0);
      break;
    }

    checkZlibRv(zlib_rv, wstream_->msg);
  }
}

const uint8_t* TZlibTransport::borrow(uint8_t* buf, uint32_t* len) {
  // Don't try to be clever with shifting buffers.
  // If we have enough data, give a pointer to it,
  // otherwise let the protcol use its slow path.
  if (readAvail() >= (int)*len) {
    *len = (uint32_t)readAvail();
    return urbuf_ + urpos_;
  }
  return NULL;
}

void TZlibTransport::consume(uint32_t len) {
  if (readAvail() >= (int)len) {
    urpos_ += len;
  } else {
    throw TTransportException(TTransportException::BAD_ARGS,
                              "consume did not follow a borrow.");
  }
}

void TZlibTransport::verifyChecksum() {
  if (!standalone_) {
    throw TTransportException(
        TTransportException::BAD_ARGS,
        "TZLibTransport can only verify checksums for standalone objects.");
  }

  if (!input_ended_) {
    // This should only be called when reading is complete,
    // but it's possible that the whole checksum has not been fed to zlib yet.
    // We try to read an extra byte here to force zlib to finish the stream.
    // It might not always be easy to "unread" this byte,
    // but we throw an exception if we get it, which is not really
    // a recoverable error, so it doesn't matter.
    uint8_t buf[1];
    uint32_t got = this->read(buf, sizeof(buf));
    if (got || !input_ended_) {
      throw TTransportException(
          TTransportException::CORRUPTED_DATA,
          "Zlib stream not complete.");
    }
  }

  // If the checksum had been bad, we would have gotten an error while
  // inflating.
}


}}} // apache::thrift::transport
