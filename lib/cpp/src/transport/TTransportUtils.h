// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _THRIFT_TRANSPORT_TTRANSPORTUTILS_H_
#define _THRIFT_TRANSPORT_TTRANSPORTUTILS_H_ 1

#include <cstdlib>
#include <cstring>
#include <string>
#include <algorithm>
#include <transport/TTransport.h>
#include <transport/TFileTransport.h>

namespace facebook { namespace thrift { namespace transport {

/**
 * The null transport is a dummy transport that doesn't actually do anything.
 * It's sort of an analogy to /dev/null, you can never read anything from it
 * and it will let you write anything you want to it, though it won't actually
 * go anywhere.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TNullTransport : public TTransport {
 public:
  TNullTransport() {}

  ~TNullTransport() {}

  bool isOpen() {
    return true;
  }

  void open() {}

  void write(const uint8_t* /* buf */, uint32_t /* len */) {
    return;
  }

};


/**
 * Buffered transport. For reads it will read more data than is requested
 * and will serve future data out of a local buffer. For writes, data is
 * stored to an in memory buffer before being written out.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TBufferedTransport : public TTransport {
 public:
  TBufferedTransport(boost::shared_ptr<TTransport> transport) :
    transport_(transport),
    rBufSize_(512), rPos_(0), rLen_(0),
    wBufSize_(512), wLen_(0) {
    rBuf_ = new uint8_t[rBufSize_];
    wBuf_ = new uint8_t[wBufSize_];
  }

  TBufferedTransport(boost::shared_ptr<TTransport> transport, uint32_t sz) :
    transport_(transport),
    rBufSize_(sz), rPos_(0), rLen_(0),
    wBufSize_(sz), wLen_(0) {
    rBuf_ = new uint8_t[rBufSize_];
    wBuf_ = new uint8_t[wBufSize_];
  }

  TBufferedTransport(boost::shared_ptr<TTransport> transport, uint32_t rsz, uint32_t wsz) :
    transport_(transport),
    rBufSize_(rsz), rPos_(0), rLen_(0),
    wBufSize_(wsz), wLen_(0) {
    rBuf_ = new uint8_t[rBufSize_];
    wBuf_ = new uint8_t[wBufSize_];
  }

  ~TBufferedTransport() {
    delete [] rBuf_;
    delete [] wBuf_;
  }

  bool isOpen() {
    return transport_->isOpen();
  }

  bool peek() {
    if (rPos_ >= rLen_) {
      rLen_ = transport_->read(rBuf_, rBufSize_);
      rPos_ = 0;
    }
    return (rLen_ > rPos_);
  }

  void open() {
    transport_->open();
  }

  void close() {
    flush();
    transport_->close();
  }

  uint32_t read(uint8_t* buf, uint32_t len);

  void write(const uint8_t* buf, uint32_t len);

  void flush();

  /**
   * The following behavior is currently implemented by TBufferedTransport,
   * but that may change in a future version:
   * 1/ If len is at most rBufSize_, borrow will never return NULL.
   *    Depending on the underlying transport, it could throw an exception
   *    or hang forever.
   * 2/ Some borrow requests may copy bytes internally.  However,
   *    if len is at most rBufSize_/2, none of the copied bytes
   *    will ever have to be copied again.  For optimial performance,
   *    stay under this limit.
   */
  const uint8_t* borrow(uint8_t* buf, uint32_t* len);

  void consume(uint32_t len);

  boost::shared_ptr<TTransport> getUnderlyingTransport() {
    return transport_;
  }

 protected:
  boost::shared_ptr<TTransport> transport_;
  uint8_t* rBuf_;
  uint32_t rBufSize_;
  uint32_t rPos_;
  uint32_t rLen_;

  uint8_t* wBuf_;
  uint32_t wBufSize_;
  uint32_t wLen_;
};

/**
 * Wraps a transport into a buffered one.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TBufferedTransportFactory : public TTransportFactory {
 public:
  TBufferedTransportFactory() {}

  virtual ~TBufferedTransportFactory() {}

  /**
   * Wraps the transport into a buffered one.
   */
  virtual boost::shared_ptr<TTransport> getTransport(boost::shared_ptr<TTransport> trans) {
    return boost::shared_ptr<TTransport>(new TBufferedTransport(trans));
  }

};

/**
 * Framed transport. All writes go into an in-memory buffer until flush is
 * called, at which point the transport writes the length of the entire
 * binary chunk followed by the data payload. This allows the receiver on the
 * other end to always do fixed-length reads.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TFramedTransport : public TTransport {
 public:
  TFramedTransport(boost::shared_ptr<TTransport> transport) :
    transport_(transport),
    rPos_(0),
    rLen_(0),
    read_(true),
    wBufSize_(512),
    wLen_(0),
    write_(true) {
    rBuf_ = NULL;
    wBuf_ = new uint8_t[wBufSize_];
  }

  TFramedTransport(boost::shared_ptr<TTransport> transport, uint32_t sz) :
    transport_(transport),
    rPos_(0),
    rLen_(0),
    read_(true),
    wBufSize_(sz),
    wLen_(0),
    write_(true) {
    rBuf_ = NULL;
    wBuf_ = new uint8_t[wBufSize_];
  }

  ~TFramedTransport() {
    if (rBuf_ != NULL) {
      delete [] rBuf_;
    }
    if (wBuf_ != NULL) {
      delete [] wBuf_;
    }
  }

  void setRead(bool read) {
    read_ = read;
  }

  void setWrite(bool write) {
    write_ = write;
  }

  void open() {
    transport_->open();
  }

  bool isOpen() {
    return transport_->isOpen();
  }

  bool peek() {
    if (rPos_ < rLen_) {
      return true;
    }
    return transport_->peek();
  }

  void close() {
    if (wLen_ > 0) {
      flush();
    }
    transport_->close();
  }

  uint32_t read(uint8_t* buf, uint32_t len);

  void write(const uint8_t* buf, uint32_t len);

  void flush();

  const uint8_t* borrow(uint8_t* buf, uint32_t* len);

  void consume(uint32_t len);

  boost::shared_ptr<TTransport> getUnderlyingTransport() {
    return transport_;
  }

 protected:
  boost::shared_ptr<TTransport> transport_;
  uint8_t* rBuf_;
  uint32_t rPos_;
  uint32_t rLen_;
  bool read_;

  uint8_t* wBuf_;
  uint32_t wBufSize_;
  uint32_t wLen_;
  bool write_;

  /**
   * Reads a frame of input from the underlying stream.
   */
  void readFrame();
};

/**
 * Wraps a transport into a framed one.
 *
 * @author Dave Simpson <dave@powerset.com>
 */
class TFramedTransportFactory : public TTransportFactory {
 public:
  TFramedTransportFactory() {}

  virtual ~TFramedTransportFactory() {}

  /**
   * Wraps the transport into a framed one.
   */
  virtual boost::shared_ptr<TTransport> getTransport(boost::shared_ptr<TTransport> trans) {
    return boost::shared_ptr<TTransport>(new TFramedTransport(trans));
  }

};


/**
 * A memory buffer is a tranpsort that simply reads from and writes to an
 * in memory buffer. Anytime you call write on it, the data is simply placed
 * into a buffer, and anytime you call read, data is read from that buffer.
 *
 * The buffers are allocated using C constructs malloc,realloc, and the size
 * doubles as necessary.
 *
 * @author Mark Slee <mcslee@facebook.com>
 * @author David Reiss <dreiss@facebook.com>
 */
class TMemoryBuffer : public TTransport {
 private:

  // Common initialization done by all constructors.
  void initCommon(uint8_t* buf, uint32_t size, bool owner, uint32_t wPos) {
    if (buf == NULL && size != 0) {
      assert(owner);
      buf = (uint8_t*)std::malloc(size);
      if (buf == NULL) {
        throw TTransportException("Out of memory");
      }
    }

    buffer_ = buf;
    bufferSize_ = size;
    owner_ = owner;
    wPos_ = wPos;
    rPos_ = 0;
  }

  // make sure there's at least 'len' bytes available for writing
  void ensureCanWrite(uint32_t len);

 public:
  static const uint32_t defaultSize = 1024;

  /**
   * This enum specifies how a TMemoryBuffer should treat
   * memory passed to it via constructors or resetBuffer.
   *
   * OBSERVE:
   *   TMemoryBuffer will simply store a pointer to the memory.
   *   It is the callers responsibility to ensure that the pointer
   *   remains valid for the lifetime of the TMemoryBuffer,
   *   and that it is properly cleaned up.
   *   Note that no data can be written to observed buffers.
   *
   * COPY:
   *   TMemoryBuffer will make an internal copy of the buffer.
   *   The caller has no responsibilities.
   *
   * TAKE_OWNERSHIP:
   *   TMemoryBuffer will become the "owner" of the buffer,
   *   and will be responsible for freeing it.
   *   The membory must have been allocated with malloc.
   */
  enum MemoryPolicy {
    OBSERVE = 1,
    COPY = 2,
    TAKE_OWNERSHIP = 3,
  };

  /**
   * Construct a TMemoryBuffer with a default-sized buffer,
   * owned by the TMemoryBuffer object.
   */
  TMemoryBuffer() {
    initCommon(NULL, defaultSize, true, 0);
  }

  /**
   * Construct a TMemoryBuffer with a buffer of a specified size,
   * owned by the TMemoryBuffer object.
   *
   * @param sz  The initial size of the buffer.
   */
  TMemoryBuffer(uint32_t sz) {
    initCommon(NULL, sz, true, 0);
  }

  /**
   * Construct a TMemoryBuffer with buf as its initial contents.
   *
   * @param buf    The initial contents of the buffer.
   *               Note that, while buf is a non-const pointer,
   *               TMemoryBuffer will not write to it if policy == OBSERVE,
   *               so it is safe to const_cast<uint8_t*>(whatever).
   * @param sz     The size of @c buf.
   * @param policy See @link MemoryPolicy @endlink .
   */
  TMemoryBuffer(uint8_t* buf, uint32_t sz, MemoryPolicy policy = OBSERVE) {
    if (buf == NULL && sz != 0) {
      throw TTransportException(TTransportException::BAD_ARGS,
                                "TMemoryBuffer given null buffer with non-zero size.");
    }

    switch (policy) {
      case OBSERVE:
      case TAKE_OWNERSHIP:
        initCommon(buf, sz, policy == TAKE_OWNERSHIP, sz);
        break;
      case COPY:
        initCommon(NULL, sz, true, 0);
        this->write(buf, sz);
        break;
      default:
        throw TTransportException(TTransportException::BAD_ARGS,
                                  "Invalid MemoryPolicy for TMemoryBuffer");
    }
  }

  ~TMemoryBuffer() {
    if (owner_) {
      std::free(buffer_);
      buffer_ = NULL;
    }
  }

  bool isOpen() {
    return true;
  }

  bool peek() {
    return (rPos_ < wPos_);
  }

  void open() {}

  void close() {}

  // TODO(dreiss): Make bufPtr const.
  void getBuffer(uint8_t** bufPtr, uint32_t* sz) {
    *bufPtr = buffer_;
    *sz = wPos_;
  }

  std::string getBufferAsString() {
    if (buffer_ == NULL) {
      return "";
    }
    return std::string((char*)buffer_, (std::string::size_type)wPos_);
  }

  void appendBufferToString(std::string& str) {
    if (buffer_ == NULL) {
      return;
    }
    str.append((char*)buffer_, wPos_);
  }

  void resetBuffer() {
    wPos_ = 0;
    rPos_ = 0;
    // It isn't safe to write into a buffer we don't own.
    if (!owner_) {
      bufferSize_ = 0;
    }
  }

  /// See constructor documentation.
  void resetBuffer(uint8_t* buf, uint32_t sz, MemoryPolicy policy = OBSERVE) {
    // Use a variant of the copy-and-swap trick for assignment operators.
    // This is sub-optimal in terms of performance for two reasons:
    //   1/ The constructing and swapping of the (small) values
    //      in the temporary object takes some time, and is not necessary.
    //   2/ If policy == COPY, we allocate the new buffer before
    //      freeing the old one, precluding the possibility of
    //      reusing that memory.
    // I doubt that either of these problems could be optimized away,
    // but the second is probably no a common case, and the first is minor.
    // I don't expect resetBuffer to be a common operation, so I'm willing to
    // bite the performance bullet to make the method this simple.

    // Construct the new buffer.
    TMemoryBuffer new_buffer(buf, sz, policy);
    // Move it into ourself.
    this->swap(new_buffer);
    // Our old self gets destroyed.
  }

  uint32_t read(uint8_t* buf, uint32_t len);

  std::string readAsString(uint32_t len) {
    std::string str;
    (void)readAppendToString(str, len);
    return str;
  }

  uint32_t readAppendToString(std::string& str, uint32_t len);

  void readEnd() {
    if (rPos_ == wPos_) {
      resetBuffer();
    }
  }

  void write(const uint8_t* buf, uint32_t len);

  uint32_t available() const {
    return wPos_ - rPos_;
  }

  const uint8_t* borrow(uint8_t* buf, uint32_t* len);

  void consume(uint32_t len);

  void swap(TMemoryBuffer& that) {
    using std::swap;
    swap(buffer_,     that.buffer_);
    swap(bufferSize_, that.bufferSize_);
    swap(wPos_,       that.wPos_);
    swap(owner_,      that.owner_);
  }

  // Returns a pointer to where the client can write data to append to
  // the TMemoryBuffer, and ensures the buffer is big enough to accomodate a
  // write of the provided length.  The returned pointer is very convenient for
  // passing to read(), recv(), or similar. You must call wroteBytes() as soon
  // as data is written or the buffer will not be aware that data has changed.
  uint8_t* getWritePtr(uint32_t len) {
    ensureCanWrite(len);
    return buffer_ + wPos_;
  }

  // Informs the buffer that the client has written 'len' bytes into storage
  // that had been provided by getWritePtr().
  void wroteBytes(uint32_t len);

 private:
  // Data buffer
  uint8_t* buffer_;

  // Allocated buffer size
  uint32_t bufferSize_;

  // Where the write is at
  uint32_t wPos_;

  // Where the reader is at
  uint32_t rPos_;

  // Is this object the owner of the buffer?
  bool owner_;

  // Don't forget to update constrctors, initCommon, and swap if
  // you add new members.
};

/**
 * TPipedTransport. This transport allows piping of a request from one
 * transport to another either when readEnd() or writeEnd(). The typical
 * use case for this is to log a request or a reply to disk.
 * The underlying buffer expands to a keep a copy of the entire
 * request/response.
 *
 * @author Aditya Agarwal <aditya@facebook.com>
 */
class TPipedTransport : virtual public TTransport {
 public:
  TPipedTransport(boost::shared_ptr<TTransport> srcTrans,
                  boost::shared_ptr<TTransport> dstTrans) :
    srcTrans_(srcTrans),
    dstTrans_(dstTrans),
    rBufSize_(512), rPos_(0), rLen_(0),
    wBufSize_(512), wLen_(0) {

    // default is to to pipe the request when readEnd() is called
    pipeOnRead_ = true;
    pipeOnWrite_ = false;

    rBuf_ = (uint8_t*) std::malloc(sizeof(uint8_t) * rBufSize_);
    wBuf_ = (uint8_t*) std::malloc(sizeof(uint8_t) * wBufSize_);
  }

  TPipedTransport(boost::shared_ptr<TTransport> srcTrans,
                  boost::shared_ptr<TTransport> dstTrans,
                  uint32_t sz) :
    srcTrans_(srcTrans),
    dstTrans_(dstTrans),
    rBufSize_(512), rPos_(0), rLen_(0),
    wBufSize_(sz), wLen_(0) {

    rBuf_ = (uint8_t*) std::malloc(sizeof(uint8_t) * rBufSize_);
    wBuf_ = (uint8_t*) std::malloc(sizeof(uint8_t) * wBufSize_);
  }

  ~TPipedTransport() {
    std::free(rBuf_);
    std::free(wBuf_);
  }

  bool isOpen() {
    return srcTrans_->isOpen();
  }

  bool peek() {
    if (rPos_ >= rLen_) {
      // Double the size of the underlying buffer if it is full
      if (rLen_ == rBufSize_) {
        rBufSize_ *=2;
        rBuf_ = (uint8_t *)std::realloc(rBuf_, sizeof(uint8_t) * rBufSize_);
      }

      // try to fill up the buffer
      rLen_ += srcTrans_->read(rBuf_+rPos_, rBufSize_ - rPos_);
    }
    return (rLen_ > rPos_);
  }


  void open() {
    srcTrans_->open();
  }

  void close() {
    srcTrans_->close();
  }

  void setPipeOnRead(bool pipeVal) {
    pipeOnRead_ = pipeVal;
  }

  void setPipeOnWrite(bool pipeVal) {
    pipeOnWrite_ = pipeVal;
  }

  uint32_t read(uint8_t* buf, uint32_t len);

  void readEnd() {

    if (pipeOnRead_) {
      dstTrans_->write(rBuf_, rPos_);
      dstTrans_->flush();
    }

    srcTrans_->readEnd();

    // If requests are being pipelined, copy down our read-ahead data,
    // then reset our state.
    int read_ahead = rLen_ - rPos_;
    memcpy(rBuf_, rBuf_ + rPos_, read_ahead);
    rPos_ = 0;
    rLen_ = read_ahead;
  }

  void write(const uint8_t* buf, uint32_t len);

  void writeEnd() {
    if (pipeOnWrite_) {
      dstTrans_->write(wBuf_, wLen_);
      dstTrans_->flush();
    }
  }

  void flush();

  boost::shared_ptr<TTransport> getTargetTransport() {
    return dstTrans_;
  }

 protected:
  boost::shared_ptr<TTransport> srcTrans_;
  boost::shared_ptr<TTransport> dstTrans_;

  uint8_t* rBuf_;
  uint32_t rBufSize_;
  uint32_t rPos_;
  uint32_t rLen_;

  uint8_t* wBuf_;
  uint32_t wBufSize_;
  uint32_t wLen_;

  bool pipeOnRead_;
  bool pipeOnWrite_;
};


/**
 * Wraps a transport into a pipedTransport instance.
 *
 * @author Aditya Agarwal <aditya@facebook.com>
 */
class TPipedTransportFactory : public TTransportFactory {
 public:
  TPipedTransportFactory() {}
  TPipedTransportFactory(boost::shared_ptr<TTransport> dstTrans) {
    initializeTargetTransport(dstTrans);
  }
  virtual ~TPipedTransportFactory() {}

  /**
   * Wraps the base transport into a piped transport.
   */
  virtual boost::shared_ptr<TTransport> getTransport(boost::shared_ptr<TTransport> srcTrans) {
    return boost::shared_ptr<TTransport>(new TPipedTransport(srcTrans, dstTrans_));
  }

  virtual void initializeTargetTransport(boost::shared_ptr<TTransport> dstTrans) {
    if (dstTrans_.get() == NULL) {
      dstTrans_ = dstTrans;
    } else {
      throw TException("Target transport already initialized");
    }
  }

 protected:
  boost::shared_ptr<TTransport> dstTrans_;
};

/**
 * TPipedFileTransport. This is just like a TTransport, except that
 * it is a templatized class, so that clients who rely on a specific
 * TTransport can still access the original transport.
 *
 * @author James Wang <jwang@facebook.com>
 */
class TPipedFileReaderTransport : public TPipedTransport,
                                  public TFileReaderTransport {
 public:
  TPipedFileReaderTransport(boost::shared_ptr<TFileReaderTransport> srcTrans, boost::shared_ptr<TTransport> dstTrans);

  ~TPipedFileReaderTransport();

  // TTransport functions
  bool isOpen();
  bool peek();
  void open();
  void close();
  uint32_t read(uint8_t* buf, uint32_t len);
  uint32_t readAll(uint8_t* buf, uint32_t len);
  void readEnd();
  void write(const uint8_t* buf, uint32_t len);
  void writeEnd();
  void flush();

  // TFileReaderTransport functions
  int32_t getReadTimeout();
  void setReadTimeout(int32_t readTimeout);
  uint32_t getNumChunks();
  uint32_t getCurChunk();
  void seekToChunk(int32_t chunk);
  void seekToEnd();

 protected:
  // shouldn't be used
  TPipedFileReaderTransport();
  boost::shared_ptr<TFileReaderTransport> srcTrans_;
};

/**
 * Creates a TPipedFileReaderTransport from a filepath and a destination transport
 *
 * @author James Wang <jwang@facebook.com>
 */
class TPipedFileReaderTransportFactory : public TPipedTransportFactory {
 public:
  TPipedFileReaderTransportFactory() {}
  TPipedFileReaderTransportFactory(boost::shared_ptr<TTransport> dstTrans)
    : TPipedTransportFactory(dstTrans)
  {}
  virtual ~TPipedFileReaderTransportFactory() {}

  boost::shared_ptr<TTransport> getTransport(boost::shared_ptr<TTransport> srcTrans) {
    boost::shared_ptr<TFileReaderTransport> pFileReaderTransport = boost::dynamic_pointer_cast<TFileReaderTransport>(srcTrans);
    if (pFileReaderTransport.get() != NULL) {
      return getFileReaderTransport(pFileReaderTransport);
    } else {
      return boost::shared_ptr<TTransport>();
    }
  }

  boost::shared_ptr<TFileReaderTransport> getFileReaderTransport(boost::shared_ptr<TFileReaderTransport> srcTrans) {
    return boost::shared_ptr<TFileReaderTransport>(new TPipedFileReaderTransport(srcTrans, dstTrans_));
  }
};

}}} // facebook::thrift::transport

#endif // #ifndef _THRIFT_TRANSPORT_TTRANSPORTUTILS_H_
