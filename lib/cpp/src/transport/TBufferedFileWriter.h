#ifndef _THRIFT_TRANSPORT_TBUFFEREDFILEWRITER_H_
#define _THRIFT_TRANSPORT_TBUFFEREDFILEWRITER_H_ 1

#include "TTransport.h"
#include "Thrift.h"

#include <string>

#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace transport { 

using namespace boost;
using std::string;

// Data pertaining to a single event
typedef struct eventInfo {
   uint8_t* payLoad_;
   uint32_t eventSize_;

  eventInfo():payLoad_(NULL), eventSize_(0){};
} eventInfo;


/**
 * Class that stores a circular in-memory event/message buffer and writes 
 * elements to disk when the buffer becomes full or a flush is triggered.
 *
 * @author Aditya Agarwal <aditya@facebook.com>
 */
class TBufferedFileWriter : public TTransport {
 public:
  void setFlushMaxUs(uint32_t flushMaxUs) {
    flushMaxUs_ = flushMaxUs;
  }
  uint32_t getFlushMaxUs() {
    return flushMaxUs_;
  }

  void setFlushMaxBytes(uint32_t flushMaxBytes) {
    flushMaxBytes_ = flushMaxBytes;
  }
  uint32_t getFlushMaxBytes() {
    return flushMaxBytes_;
  }

  void setChunkSize(uint32_t chunkSize) {
    chunkSize_ = chunkSize;
  }
  uint32_t getChunkSize() {
    return chunkSize_;
  }

  void setMaxEventSize(uint32_t maxEventSize) {
    maxEventSize_ = maxEventSize;
  }
  uint32_t getMaxEventSize() {
    return maxEventSize_;
  }

  TBufferedFileWriter(string filename, uint32_t sz);
  TBufferedFileWriter(string filename, uint32_t sz, int fd, long long offset);
  void init(string filename, uint32_t sz, int fd, long long offset);
  ~TBufferedFileWriter();

  void resetOutputFile(int fd, string filename, long long offset);

  void enqueueEvent(const uint8_t* buf, uint32_t eventLen, bool blockUntilFlush);
  void enqueueEvent(const eventInfo& toEnqueue, bool blockUntilFlush);
  void write(const uint8_t* buf, uint32_t len) {
    enqueueEvent(buf, len, false);
  }

  eventInfo dequeueEvent(long long deadline);
  void flush();

  // control for writer thread
  static void* startWriterThread(void* ptr) {
    (((TBufferedFileWriter*)ptr)->writerThread());
    return 0;
  }
  void writerThread();


 private:
  // circular buffer to hold data in before it is flushed. This is an array of strings. Each
  // element of the array stores a msg that needs to be written to the file
  eventInfo* buffer_;
  
  // size of string buffer
  uint32_t sz_;

  // size of chunks that file will be split up into
  uint32_t chunkSize_;

  // max number of microseconds that can pass without flushing
  uint32_t flushMaxUs_;

  // max number of bytes that can be written without flushing
  uint32_t flushMaxBytes_;

  // max event size
  uint32_t maxEventSize_;
  
  // writer thread id
  pthread_t writer_;

  // variables that determine position of head/tail of circular buffer
  int headPos_, tailPos_;

  // variables indicating whether the buffer is full or empty
  bool isFull_, isEmpty_;
  pthread_cond_t notFull_, notEmpty_;
  bool closing_;

  // To keep track of whether the buffer has been flushed
  pthread_cond_t flushed_;
  bool notFlushed_;

  // Mutex that is grabbed when enqueueing, dequeueing and flushing
  // from the circular buffer
  pthread_mutex_t mutex_;

  // File information
  string filename_;
  int fd_;

  // Offset within the file
  long long offset_;

  void openOutputFile();
  uint32_t getCurrentTime();

};

}}}

#endif // _THRIFT_TRANSPORT_TBUFFEREDFILEWRITER_H_
