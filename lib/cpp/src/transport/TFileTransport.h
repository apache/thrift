#ifndef _THRIFT_TRANSPORT_TFILETRANSPORT_H_
#define _THRIFT_TRANSPORT_TFILETRANSPORT_H_ 1

#include "TTransport.h"
#include "Thrift.h"
#include "TProcessor.h"

#include <string>
#include <stdio.h>

#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace transport { 

using namespace boost;
using std::string;

// Data pertaining to a single event
typedef struct eventInfo {
  uint8_t* eventBuff_;
  uint32_t eventSize_;
  uint32_t eventBuffPos_;

  eventInfo():eventBuff_(NULL), eventSize_(0), eventBuffPos_(0){};
  ~eventInfo() {
    if (eventBuff_) {
      delete[] eventBuff_;
    }
  }
} eventInfo;

// information about current read state
typedef struct readState {
  eventInfo* event_;

  // keep track of event size
  uint8_t   eventSizeBuff_[4];
  uint8_t   eventSizeBuffPos_;
  bool      readingSize_;

  // read buffer variables
  int32_t  bufferPtr_;
  int32_t  bufferLen_;

  // last successful dispatch point
  int32_t lastDispatchPtr_;
  
  void resetState(uint32_t lastDispatchPtr) {
    readingSize_ = true;
    eventSizeBuffPos_ = 0;
    lastDispatchPtr_ = lastDispatchPtr;
  }

  void resetAllValues() {
    resetState(0);
    bufferPtr_ = 0;
    bufferLen_ = 0;
    if (event_) {
      delete(event_);
    }
    event_ = 0;
  }

  readState() {
    event_ = 0;
    resetAllValues();
  }

  ~readState() {
    if (event_) {
      delete(event_);
    }
  }

} readState;
 
/**
 * File implementation of a transport. Reads and writes are done to a 
 * file on disk.
 *
 * @author Aditya Agarwal <aditya@facebook.com>
 */
class TFileTransport : public TTransport {
 public:
  TFileTransport(string path);
  ~TFileTransport();

  // TODO: what is the correct behaviour for this?
  // the log file is generally always open
  bool isOpen() {
    return true;
  }
  
  void write(const uint8_t* buf, uint32_t len) {
    enqueueEvent(buf, len, false);
  }
  
  void flush();

  uint32_t readAll(uint8_t* buf, uint32_t len);
  uint32_t read(uint8_t* buf, uint32_t len);

  // log-file specific functions
  void seekToChunk(int chunk);
  void seekToEnd();
  uint32_t getNumChunks();

  // for changing the output file
  void resetOutputFile(int fd, string filename, long long offset);

  // Setter/Getter functions for user-controllable options
  void setReadBuffSize(uint32_t readBuffSize) {
    if (readBuffSize) {
      readBuffSize_ = readBuffSize;
    }
  }
  uint32_t getReadBuffSize() {
    return readBuffSize_;
  }

  void setReadTimeout(int32_t readTimeout) {
    readTimeout_ = readTimeout;
  }
  int32_t getReadTimeout() {
    return readTimeout_;
  }

  void setChunkSize(uint32_t chunkSize) {
    if (chunkSize) {
      chunkSize_ = chunkSize;
    }
  }
  uint32_t getChunkSize() {
    return chunkSize_;
  }

  void setEventBufferSize(uint32_t bufferSize) {    
    if (bufferSize) {
      if (buffer_) {
        delete[] buffer_;
      }
      eventBufferSize_ = bufferSize;
      buffer_ = new eventInfo*[eventBufferSize_];
    }
  }
  uint32_t getEventBufferSize() {
    return eventBufferSize_;
  }

  void setFlushMaxUs(uint32_t flushMaxUs) {
    if (flushMaxUs) {
      flushMaxUs_ = flushMaxUs;
    }
  }
  uint32_t getFlushMaxUs() {
    return flushMaxUs_;
  }

  void setFlushMaxBytes(uint32_t flushMaxBytes) {
    if (flushMaxBytes) {
      flushMaxBytes_ = flushMaxBytes;
    }
  }
  uint32_t getFlushMaxBytes() {
    return flushMaxBytes_;
  }

  void setMaxEventSize(uint32_t maxEventSize) {
    maxEventSize_ = maxEventSize;
  }
  uint32_t getMaxEventSize() {
    return maxEventSize_;
  }

  void setMaxCorruptedEvents(uint32_t maxCorruptedEvents) {
    maxCorruptedEvents_ = maxCorruptedEvents;
  }
  uint32_t getMaxCorruptedEvents() {
    return maxCorruptedEvents_;
  }

  void setEofSleepTimeUs(uint32_t eofSleepTime) {
    if (eofSleepTime) {
      eofSleepTime_ = eofSleepTime;
    }
  }
  uint32_t getEofSleepTimeUs() {
    return eofSleepTime_;
  }

 private:
  // helper functions for writing to a file
  void enqueueEvent(const uint8_t* buf, uint32_t eventLen, bool blockUntilFlush);
  void enqueueEvent(eventInfo* toEnqueue, bool blockUntilFlush);
  eventInfo* dequeueEvent(long long deadline);

  // control for writer thread
  static void* startWriterThread(void* ptr) {
    (((TFileTransport*)ptr)->writerThread());
    return 0;
  }
  void writerThread();

  // helper functions for reading from a file
  bool readEvent();

  // Utility functions
  void openLogFile();
  uint32_t getCurrentTime();

  // Class variables
  readState readState_;
  uint8_t* readBuff_;

  eventInfo* currentEvent_;

  uint32_t readBuffSize_;
  static const uint32_t DEFAULT_READ_BUFF_SIZE = 1 * 1024 * 1024;

  int32_t readTimeout_;
  static const int32_t DEFAULT_READ_TIMEOUT_MS = 200;

  // size of chunks that file will be split up into
  uint32_t chunkSize_;
  static const uint32_t DEFAULT_CHUNK_SIZE = 16 * 1024 * 1024;

  // size of string buffer
  uint32_t eventBufferSize_;
  static const uint32_t DEFAULT_EVENT_BUFFER_SIZE = 1024;

  // circular buffer to hold data in before it is flushed. This is an array of strings. Each
  // element of the array stores a msg that needs to be written to the file
  eventInfo** buffer_;
  
  // max number of microseconds that can pass without flushing
  uint32_t flushMaxUs_;
  static const uint32_t DEFAULT_FLUSH_MAX_US = 20000;

  // max number of bytes that can be written without flushing
  uint32_t flushMaxBytes_;
  static const uint32_t DEFAULT_FLUSH_MAX_BYTES = 1000 * 1024;

  // max event size
  uint32_t maxEventSize_;
  static const uint32_t DEFAULT_MAX_EVENT_SIZE = 0;

  // max number of corrupted events per chunk
  uint32_t maxCorruptedEvents_;
  static const uint32_t DEFAULT_MAX_CORRUPTED_EVENTS = 0;
  
  // sleep duration when EOF is hit
  uint32_t eofSleepTime_;
  static const uint32_t DEFAULT_EOF_SLEEP_TIME_US = 500 * 1000;
    
  // writer thread id
  pthread_t writerThreadId_;

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
  off_t offset_;

};

// Exception thrown when EOF is hit
class TEOFException : public facebook::thrift::TTransportException {
 public:
  TEOFException():
    facebook::thrift::TTransportException(TTX_EOF) {};
};


// wrapper class to process events from a file containing thrift events
class TFileProcessor {
 public:
  /** 
   * Constructor that defaults output transport to null transport
   * 
   * @param processor processes log-file events
   * @param protocolFactory protocol factory
   * @param inputTransport file transport
   */
  TFileProcessor(shared_ptr<TProcessor> processor,
                 shared_ptr<TProtocolFactory> protocolFactory,
                 shared_ptr<TFileTransport> inputTransport);

  /** 
   * Constructor
   * 
   * @param processor processes log-file events
   * @param protocolFactory protocol factory
   * @param inputTransport input file transport
   * @param output output transport
   */    
  TFileProcessor(shared_ptr<TProcessor> processor,
                 shared_ptr<TProtocolFactory> protocolFactory,
                 shared_ptr<TFileTransport> inputTransport,
                 shared_ptr<TTransport> outputTransport);                      

  /**
   * processes events from the file
   *
   * @param numEvents number of events to process (0 for unlimited)
   * @param tail tails the file if true
   */
  void process(uint32_t numEvents, bool tail);

 private:
  shared_ptr<TProcessor> processor_;
  shared_ptr<TProtocolFactory> protocolFactory_;
  shared_ptr<TFileTransport> inputTransport_;
  shared_ptr<TTransport> outputTransport_;
};

 
}}} // facebook::thrift::transport

#endif // _THRIFT_TRANSPORT_TFILETRANSPORT_H_
