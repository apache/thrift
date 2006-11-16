#ifndef _THRIFT_TRANSPORT_TTRANSPORTUTILS_H_
#define _THRIFT_TRANSPORT_TTRANSPORTUTILS_H_ 1

#include <transport/TTransport.h>

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

  void write(const std::string& s) {}
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
    transport_->close();
  }

  uint32_t read(uint8_t* buf, uint32_t len);
  
  void write(const uint8_t* buf, uint32_t len);

  void flush();

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
  virtual std::pair<boost::shared_ptr<TTransport>, boost::shared_ptr<TTransport> > getIOTransports(boost::shared_ptr<TTransport> trans) {
    boost::shared_ptr<TTransport> buffered(new TBufferedTransport(trans));
    return std::make_pair(buffered, buffered);
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
    transport_->close();
  }
 
  uint32_t read(uint8_t* buf, uint32_t len);

  void write(const uint8_t* buf, uint32_t len);

  void flush();

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
 * A memory buffer is a tranpsort that simply reads from and writes to an
 * in memory buffer. Anytime you call write on it, the data is simply placed
 * into a buffer, and anytime you call read, data is read from that buffer.
 *
 * The buffers are allocated using C constructs malloc,realloc, and the size
 * doubles as necessary.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TMemoryBuffer : public TTransport {
 public:
  TMemoryBuffer() {
    owner_ = true;
    bufferSize_ = 1024;
    buffer_ = (uint8_t*)malloc(bufferSize_);
    if (buffer_ == NULL) {
      throw TTransportException("Out of memory");
    }
    wPos_ = 0;
    rPos_ = 0;
  }

  TMemoryBuffer(uint32_t sz) {
    owner_ = true;
    bufferSize_ = sz;
    buffer_ = (uint8_t*)malloc(bufferSize_);
    if (buffer_ == NULL) {
      throw TTransportException("Out of memory");
    }
    wPos_ = 0;
    rPos_ = 0;
  }

  TMemoryBuffer(uint8_t* buf, int sz) {
    owner_ = false;
    buffer_ = buf;
    bufferSize_ = sz;
    wPos_ = sz;
    rPos_ = 0;
  }

  ~TMemoryBuffer() {
    if (owner_) {
      if (buffer_ != NULL) {
        free(buffer_);
        buffer_ = NULL;
      }
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

  void getBuffer(uint8_t** bufPtr, uint32_t* sz) {
    *bufPtr = buffer_;
    *sz = wPos_;
  }

  void resetBuffer() {
    wPos_ = 0;
    rPos_ = 0;
  }

  void resetBuffer(uint8_t* buf, uint32_t sz) {
    if (owner_) {
      if (buffer_ != NULL) {
        free(buffer_);
      }
    }
    owner_ = false;
    buffer_ = buf;
    bufferSize_ = sz;
    wPos_ = sz;
    rPos_ = 0;
  }

  uint32_t read(uint8_t* buf, uint32_t len);

  void write(const uint8_t* buf, uint32_t len);

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

};

}}} // facebook::thrift::transport


#endif // #ifndef _THRIFT_TRANSPORT_TTRANSPORTUTILS_H_
