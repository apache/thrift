// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _THRIFT_TRANSPORT_TTRANSPORTUTILS_H_
#define _THRIFT_TRANSPORT_TTRANSPORTUTILS_H_ 1

#include <string>
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

  void write(const uint8_t* buf, uint32_t len) {
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

  uint32_t available() {
    return wPos_ - rPos_;
  }

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

/**
 * TPipedTransport. This transport allows piping of a request from one 
 * transport to another either when readEnd() or writeEnd(). The typicAL
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

    rBuf_ = (uint8_t*) malloc(sizeof(uint8_t) * rBufSize_);
    wBuf_ = (uint8_t*) malloc(sizeof(uint8_t) * wBufSize_);
  }
    
  TPipedTransport(boost::shared_ptr<TTransport> srcTrans, 
                  boost::shared_ptr<TTransport> dstTrans, 
                  uint32_t sz) :
    srcTrans_(srcTrans),
    dstTrans_(dstTrans),
    rBufSize_(512), rPos_(0), rLen_(0),
    wBufSize_(sz), wLen_(0) {

    rBuf_ = (uint8_t*) malloc(sizeof(uint8_t) * rBufSize_);
    wBuf_ = (uint8_t*) malloc(sizeof(uint8_t) * wBufSize_);
  }

  ~TPipedTransport() {
    free(rBuf_);
    free(wBuf_);
  }

  bool isOpen() {
    return srcTrans_->isOpen();
  }
  
  bool peek() {    
    if (rPos_ >= rLen_) {
      // Double the size of the underlying buffer if it is full
      if (rLen_ == rBufSize_) {
        rBufSize_ *=2;
        rBuf_ = (uint8_t *)realloc(rBuf_, sizeof(uint8_t) * rBufSize_);
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
      dstTrans_->write(rBuf_, rLen_);
    }

    // reset state
    rLen_ = 0;
    rPos_ = 0;
  }

  void write(const uint8_t* buf, uint32_t len);

  void writeEnd() {
    if (pipeOnWrite_) {
      dstTrans_->write(wBuf_, wLen_);
    }
  }

  void flush();

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
