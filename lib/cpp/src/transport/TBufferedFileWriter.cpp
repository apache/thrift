#include "TBufferedFileWriter.h"
#include "TTransportUtils.h"

#include <pthread.h>
 #include <sys/time.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <iostream>

using namespace std;

namespace facebook { namespace thrift { namespace transport { 

TFileTransport::TFileTransport(string path) {
  filename_ = path;
  openLogFile();

  // set initial values to default
  readBuffSize_ = DEFAULT_READ_BUFF_SIZE;
  readTimeout_ = DEFAULT_READ_TIMEOUT_MS;
  chunkSize_ = DEFAULT_CHUNK_SIZE;
  eventBufferSize_ = DEFAULT_EVENT_BUFFER_SIZE;
  flushMaxUs_ = DEFAULT_FLUSH_MAX_US;
  flushMaxBytes_ = DEFAULT_FLUSH_MAX_BYTES;
  maxEventSize_ = DEFAULT_MAX_EVENT_SIZE;
  maxCorruptedEvents_ = DEFAULT_MAX_CORRUPTED_EVENTS;
  eofSleepTime_ = DEFAULT_EOF_SLEEP_TIME_US;

  // initialize buffer lazily
  buffer_ = 0;

  // buffer is initially empty
  isEmpty_ = true;
  isFull_  = false;

  // both head and tail are initially at 0
  headPos_ = 0;
  tailPos_ = 0;

  // initialize all the condition vars/mutexes
  pthread_mutex_init(&mutex_, NULL);
  pthread_cond_init(&notFull_, NULL);
  pthread_cond_init(&notEmpty_, NULL);
  pthread_cond_init(&flushed_, NULL);

  // not closing the file during init
  closing_ = false;

  // create writer thread on demand
  writerThreadId_ = 0;

  // read related variables
  // read buff initialized lazily
  readBuff_ = 0;
  currentEvent_ = 0;
}

void TFileTransport::resetOutputFile(int fd, string filename, long long offset) {
  filename_ = filename;
  offset_ = offset;

  // check if current file is still open
  if (fd_ > 0) {
    // TODO: should there be a flush here?
    fprintf(stderr, "error, current file (%s) not closed\n", filename_.c_str());
    if(-1 == ::close(fd_)) {
      perror("TFileTransport: error in file close");
      throw TTransportException("TFileTransport: error in file close");
    }
  }

  if (fd) {
    fd_ = fd;
  } else {
    // open file if the input fd is 0
    openLogFile();
  }
}


TFileTransport::~TFileTransport() {
  // TODO: Make sure the buffer is actually flushed
  // flush the buffer if a writer thread is active
  if (writerThreadId_ > 0) {
    // flush output buffer
    flush();

    // send a signal to write thread to end
    closing_ = true;
    pthread_join(writerThreadId_, NULL);
  }

  if (buffer_) {
    delete[] buffer_;
  }

  if (readBuff_) {
    delete readBuff_;
  }

  if (currentEvent_) {
    delete currentEvent_;
  }

  // close logfile
  if (fd_ > 0) {
    if(-1 == ::close(fd_)) {
      perror("TFileTransport: error in file close");
    }
  }
}


void TFileTransport::enqueueEvent(const uint8_t* buf, uint32_t eventLen, bool blockUntilFlush) {
  // make sure that event size is valid
  if ( (maxEventSize_ > 0) && (eventLen > maxEventSize_) ) {
    T_DEBUG("msg size is greater than max event size: %lu > %u\n", eventLen, maxEventSize_);
    return;
  }

  if (eventLen == 0) {
    T_ERROR("cannot enqueue an empty event");
    return;
  }

  eventInfo* toEnqueue = new eventInfo();
  toEnqueue->eventBuff_ = (uint8_t *)malloc((sizeof(uint8_t) * eventLen) + 4);
  // first 4 bytes is the event length
  memcpy(toEnqueue->eventBuff_, (void*)(&eventLen), 4);
  // actual event contents
  memcpy(toEnqueue->eventBuff_ + 4, buf, eventLen);
  toEnqueue->eventSize_ = eventLen + 4;

  //  T_DEBUG_L(1, "event size: %u", eventLen);
  return enqueueEvent(toEnqueue, blockUntilFlush);
}

void TFileTransport::enqueueEvent(eventInfo* toEnqueue, bool blockUntilFlush) {
  // lock mutex
  pthread_mutex_lock(&mutex_);

  // make sure that enqueue buffer is initialized and writer thread is running
  if (buffer_ == 0) {
    buffer_ = new eventInfo*[eventBufferSize_];
  }
  if (writerThreadId_ == 0) {
    if (pthread_create(&writerThreadId_, NULL, startWriterThread, (void *)this) != 0) {
      T_ERROR("Error creating write thread");
      return;
    }
  }

  // Can't enqueue while buffer is full
  while(isFull_) {
    pthread_cond_wait(&notFull_, &mutex_);
  }

  // make a copy and enqueue at tail of buffer
  buffer_[tailPos_] = toEnqueue;
  tailPos_ = (tailPos_+1) % eventBufferSize_;
  
  // mark the buffer as non-empty
  isEmpty_ = false;
  
  // circular buffer has wrapped around (and is full)
  if(tailPos_ == headPos_) {
    //    T_DEBUG("queue is full");
    isFull_ = true;
  }

  // signal anybody who's waiting for the buffer to be non-empty
  pthread_cond_signal(&notEmpty_);
  if(blockUntilFlush) {
    pthread_cond_wait(&flushed_, &mutex_);
  }

  // TODO: don't return until flushed to disk
  // this really should be a loop where it makes sure it got flushed
  // because condition variables can get triggered by the os for no reason 
  // it is probably a non-factor for the time being
  pthread_mutex_unlock(&mutex_);

}

eventInfo* TFileTransport::dequeueEvent(long long deadline) {
  //deadline time struc
  struct timespec ts;
  if(deadline) {
    ts.tv_sec = deadline/(1000*1000);
    ts.tv_nsec = (deadline%(1000*1000))*1000;
  }

  // wait for the queue to fill up
  pthread_mutex_lock(&mutex_);
  while(isEmpty_) {
    // do a timed wait on the condition variable
    if(deadline) {
      int e = pthread_cond_timedwait(&notEmpty_, &mutex_, &ts);
      if(e == ETIMEDOUT) {
        break;
      }
    }
    else {
      // just wait until the buffer gets an item
      pthread_cond_wait(&notEmpty_, &mutex_);
    }
  }

  string ret;
  bool doSignal = false;

  // could be empty if we timed out
  eventInfo* retEvent = 0;
  if(!isEmpty_) {
    retEvent = buffer_[headPos_];
    headPos_ = (headPos_+1) % eventBufferSize_;

    isFull_ = false;
    doSignal = true;

    // check if this is the last item in the buffer
    if(headPos_ == tailPos_) {
      isEmpty_ = true;
    }
  }

  // unlock the mutex and signal if required
  pthread_mutex_unlock(&mutex_);
  if(doSignal) {
    pthread_cond_signal(&notFull_);
  }

  if (!retEvent) {
    retEvent = new eventInfo();
  }
  return retEvent;
}


void TFileTransport::writerThread() {
  // open file if it is not open
  if(!fd_) {
    openLogFile();
  }

  // set the offset to the correct value (EOF)
  offset_ = lseek(fd_, 0, SEEK_END);

  // Figure out the next time by which a flush must take place
  long long nextFlush = getCurrentTime() + flushMaxUs_;
  uint32_t unflushed = 0;

  while(1) {
    // this will only be true when the destructor is being invoked
    if(closing_) {
      if(-1 == ::close(fd_)) {
        perror("TFileTransport: error in close");
        throw TTransportException("TFileTransport: error in file close");
      }
      fd_ = 0;
      return;
    }

    //long long start = now();
    eventInfo* outEvent = dequeueEvent(nextFlush);    
    if (!outEvent) {
      T_DEBUG_L(1, "Got an empty event");
      return;
    }

    // sanity check on event
    if ( (maxEventSize_ > 0) && (outEvent->eventSize_ > maxEventSize_)) {
      T_ERROR("msg size is greater than max event size: %u > %u\n", outEvent->eventSize_, maxEventSize_);
      delete(outEvent);
      continue;
    }
    //long long diff = now()-start;
    //T_DEBUG("got a dequeue of size %d after %lld ms\n", (int)s.size(), diff/1000);

    // If chunking is required, then make sure that msg does not cross chunk boundary
    if( (outEvent->eventSize_ > 0) && (chunkSize_ != 0)) {

      // event size must be less than chunk size
      if(outEvent->eventSize_ > chunkSize_) {
        T_ERROR("TFileTransport: event size(%u) is greater than chunk size(%u): skipping event",
              outEvent->eventSize_, chunkSize_);
        delete(outEvent);
        continue;
      }

      long long chunk1 = offset_/chunkSize_;
      long long chunk2 = (offset_ + outEvent->eventSize_ - 1)/chunkSize_;
      
      // if adding this event will cross a chunk boundary, pad the chunk with zeros
      if(chunk1 != chunk2) {
        int32_t padding = (int32_t)(chunk2*chunkSize_ - offset_);

        // sanity check
        if (padding <= 0) {
          T_DEBUG("Padding is empty, skipping event");
         continue;
        }
        if (padding > (int32_t)chunkSize_) {
          T_DEBUG("padding is larger than chunk size, skipping event");
          continue;
        }
        //        T_DEBUG("padding %d zeros to get to chunk %lld\n", padding, chunk2);
        uint8_t zeros[padding];
        bzero(zeros, padding);
        T_DEBUG_L(1, "Adding padding of %u bytes at %lu", padding, offset_);
        if(-1 == ::write(fd_, zeros, padding)) {
          perror("TFileTransport: error while padding zeros");
          throw TTransportException("TFileTransport: error while padding zeros");
        }
        unflushed += padding;
        offset_ += padding;
      }
    }

    // write the dequeued event to the file
    if(outEvent->eventSize_ > 0) {
      if(-1 == ::write(fd_, outEvent->eventBuff_, outEvent->eventSize_)) {
        perror("TFileTransport: error while writing event");
        throw TTransportException("TFileTransport: error while writing event");
      }

      unflushed += outEvent->eventSize_;
      offset_ += outEvent->eventSize_;
    }

    // couple of cases from which a flush could be triggered
    if((getCurrentTime() >= nextFlush && unflushed > 0) ||
       unflushed > flushMaxBytes_ ||
       (outEvent && (outEvent->eventSize_== 0)) ) {
      //T_DEBUG("flushing %d bytes to %s (%d %d, full? %d)", unflushed, filename_.c_str(), headPos_, tailPos_, isFull_);

      // sync (force flush) file to disk
      fsync(fd_);
      nextFlush = getCurrentTime() + flushMaxUs_;
      unflushed = 0;

      // notify anybody(thing?) waiting for flush completion
      pthread_mutex_lock(&mutex_);
      notFlushed_ = false;
      pthread_mutex_unlock(&mutex_);
      pthread_cond_broadcast(&flushed_);
    }
    // deallocate dequeued event
    delete(outEvent);
  }
}

void TFileTransport::flush() {
  eventInfo* flushEvent = new eventInfo();
  notFlushed_ = true;

  enqueueEvent(flushEvent, false);

  // wait for flush to take place
  pthread_mutex_lock(&mutex_);

  while(notFlushed_) {
    pthread_cond_wait(&flushed_, &mutex_);
  }

  pthread_mutex_unlock(&mutex_);
}


uint32_t TFileTransport::readAll(uint8_t* buf, uint32_t len) {
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

uint32_t TFileTransport::read(uint8_t* buf, uint32_t len) {
  // check if there an event is ready to be read
  if (!currentEvent_) {
    readEvent();
  }
  
  // did not manage to read an event from the file. This could have happened
  // if the timeout expired or there was some other error
  if (!currentEvent_) {
    return 0;
  }

  // read as much of the current event as possible
  int32_t remaining = currentEvent_->eventSize_ - currentEvent_->eventBuffPos_;
  if (remaining <= (int32_t)len) {
    memcpy(buf, 
           currentEvent_->eventBuff_ + currentEvent_->eventBuffPos_, 
           remaining);
    delete(currentEvent_);
    currentEvent_ = 0;
    return remaining;
  }
  
  // read as much as possible
  memcpy(buf, currentEvent_->eventBuff_ + currentEvent_->eventBuffPos_, len);
  currentEvent_->eventBuffPos_ += len;
  return len;
}

bool TFileTransport::readEvent() {
  int readTries = 0;

  if (!readBuff_) {
    readBuff_ = new uint8_t[readBuffSize_];    
  }

  while (1) {
    // check if there is anything in the read buffer
    if (readState_.bufferPtr_ == readState_.bufferLen_) {
      // advance the offset pointer
      offset_ += readState_.bufferLen_;
      readState_.bufferLen_ = ::read(fd_, readBuff_, readBuffSize_);
      if (readState_.bufferLen_) {
        T_DEBUG_L(1, "Amount read: %u (offset: %lu)", readState_.bufferLen_, offset_);
      }
      readState_.bufferPtr_ = 0;
      readState_.lastDispatchPtr_ = 0;
  
      // read error
      if (readState_.bufferLen_ == -1) {
        readState_.resetAllValues();
        perror("TFileTransport: error while reading from file");
        throw TTransportException("TFileTransport: error while reading from file");
      } else if (readState_.bufferLen_ == 0) {  // EOF
        // wait indefinitely if there is no timeout
        if (readTimeout_ == -1) {
          usleep(eofSleepTime_);
          continue;
        } else if (readTimeout_ == 0) {
          // reset state
          readState_.resetState(0);
          return false;
        } else if (readTimeout_ > 0) {
          // timeout already expired once
          if (readTries > 0) {
            readState_.resetState(0);
            return false;
          } else {
            usleep(readTimeout_ * 1000);
            readTries++;
            continue;
          }
        }
      }
    }
    
    readTries = 0;

    // attempt to read an event from the buffer
    while(readState_.bufferPtr_ < readState_.bufferLen_) {
      if (readState_.readingSize_) {
        if(readState_.eventSizeBuffPos_ == 0) {
          if ( (offset_ + readState_.bufferPtr_)/chunkSize_ != 
               ((offset_ + readState_.bufferPtr_ + 3)/chunkSize_)) {
            // skip one byte towards chunk boundary
            //            T_DEBUG_L(1, "Skipping a byte");
            readState_.bufferPtr_++;
            continue;
          }
        }

        readState_.eventSizeBuff_[readState_.eventSizeBuffPos_++] = 
          readBuff_[readState_.bufferPtr_++];
        bool eventCorruption = false;
        if (readState_.eventSizeBuffPos_ == 4) {
          // 0 length event indicates padding
          if (*((uint32_t *)(readState_.eventSizeBuff_)) == 0) {
            T_DEBUG_L(1, "Got padding");
            readState_.resetState(readState_.lastDispatchPtr_);
            continue;
          }
          // got a valid event
          readState_.readingSize_ = false;
          if (readState_.event_) {
            delete(readState_.event_);
          }
          readState_.event_ = new eventInfo();
          readState_.event_->eventSize_ = *((uint32_t *)(readState_.eventSizeBuff_));

          T_DEBUG_L(0, "Event size: %u", readState_.event_->eventSize_);

          // TODO
          // make sure event is valid, an error is triggered if:
          // 1. Event size is larger than user-speficied max-event size

          // 2. Event size is larger than chunk size

          // 3. size indicates that event crosses chunk boundary

        }

        if (eventCorruption) {
          // perform some kickass recovery 
        }
      } else {
        if (!readState_.event_->eventBuff_) {
          readState_.event_->eventBuff_ = new uint8_t[readState_.event_->eventSize_];
          readState_.event_->eventBuffPos_ = 0;
        }
        // take either the entire event or the remaining bytes in the buffer
        int reclaimBuffer = min((uint32_t)(readState_.bufferLen_ - readState_.bufferPtr_),
                                readState_.event_->eventSize_ - readState_.event_->eventBuffPos_);

        // copy data from read buffer into event buffer
        memcpy(readState_.event_->eventBuff_ + readState_.event_->eventBuffPos_, 
               readBuff_ + readState_.bufferPtr_,
               reclaimBuffer);
        
        // increment position ptrs
        readState_.event_->eventBuffPos_ += reclaimBuffer;
        readState_.bufferPtr_ += reclaimBuffer;
        
        //         if (reclaimBuffer > 0) {
        //           T_DEBUG_L(0, "eventBuffPost: %u", readState_.event_->eventBuffPos_);
        //           T_DEBUG_L(0, "eventSize: %u", readState_.event_->eventSize_);
        //         }

        // check if the event has been read in full
        if (readState_.event_->eventBuffPos_ == readState_.event_->eventSize_) {
          // set the completed event to the current event
          currentEvent_ = readState_.event_;
          currentEvent_->eventBuffPos_ = 0;
          
          readState_.event_ = 0;
          readState_.resetState(readState_.bufferPtr_);
          
          // exit criteria
          T_DEBUG_L(0, "Finished one event");
          return true;
        }
      }
    }
    
    
  }
}

void TFileTransport::seekToChunk(int32_t chunk) {
  if (fd_ <= 0) {
    throw TTransportException("File not open");
  }
 
  int32_t lastChunk = getNumChunks();

  // negative indicates reverse seek (from the end)
  if (chunk < 0) {
    chunk += lastChunk;
  }
  
  // cannot seek past EOF
  if (chunk > lastChunk) {
    T_DEBUG("Trying to seek past EOF. Seeking to EOF instead");
    chunk = lastChunk;
  }

  uint32_t minEndOffset = 0;
  if (chunk == lastChunk) {
    minEndOffset = lseek(fd_, 0, SEEK_END);
  }
  
  offset_ = lseek(fd_, chunk * chunkSize_, SEEK_SET);  
  readState_.resetAllValues();
  if (offset_ == -1) {
    perror("TFileTransport: lseek error in seekToChunk");
    throw TTransportException("TFileTransport: lseek error in seekToChunk");
  }

  // seek to EOF if user wanted to go to last chunk
  uint32_t oldReadTimeout = getReadTimeout();
  setReadTimeout(0);  
  if (chunk == lastChunk) {
    // keep on reading unti the last event at point of seekChunk call
    while( readEvent() && ((offset_ + readState_.bufferPtr_) < minEndOffset)) {};
  }
  setReadTimeout(oldReadTimeout);

}

void TFileTransport::seekToEnd() {
  seekToChunk(getNumChunks());
}

uint32_t TFileTransport::getNumChunks() {
  if (fd_ <= 0) {
    return 0;
  }
  struct stat f_info;
  fstat(fd_, &f_info);
  return (f_info.st_size)/chunkSize_;
}

// Utility Functions
void TFileTransport::openLogFile() {
  mode_t mode = S_IRUSR| S_IWUSR| S_IRGRP | S_IROTH;
  fd_ = ::open(filename_.c_str(), O_RDWR | O_CREAT | O_APPEND, mode);

  // make sure open call was successful
  if(fd_ == -1) {
    char errorMsg[1024];
    sprintf(errorMsg, "TFileTransport: Could not open file: %s", filename_.c_str());
    perror(errorMsg);
    throw TTransportException(errorMsg);
  }

  // opening the file in append mode causes offset_t to be at the end
  offset_ = lseek(fd_, 0, SEEK_CUR);
  T_DEBUG_L(1, "initial offset: %lu", offset_);
}

uint32_t TFileTransport::getCurrentTime() {
  long long ret;
  struct timeval tv;
  gettimeofday(&tv, NULL);
  ret = tv.tv_sec;
  ret = ret*1000*1000 + tv.tv_usec;
  return ret;
}


TFileProcessor::TFileProcessor(shared_ptr<TProcessor> processor,
                               shared_ptr<TProtocolFactory> protocolFactory,
                               shared_ptr<TFileTransport> inputTransport):
  processor_(processor), protocolFactory_(protocolFactory), 
  inputTransport_(inputTransport) {

  // default the output transport to a null transport (common case)
  outputTransport_ = shared_ptr<TNullTransport>(new TNullTransport());
}

TFileProcessor::TFileProcessor(shared_ptr<TProcessor> processor,
                               shared_ptr<TProtocolFactory> protocolFactory,
                               shared_ptr<TFileTransport> inputTransport,
                               shared_ptr<TTransport> outputTransport):
  processor_(processor), protocolFactory_(protocolFactory), 
  inputTransport_(inputTransport), outputTransport_(outputTransport) {
};

void TFileProcessor::process(uint32_t numEvents, bool tail) {
  pair<shared_ptr<TProtocol>,shared_ptr<TProtocol> > iop;
  iop = protocolFactory_->getIOProtocols(inputTransport_, outputTransport_);

  // set the read timeout to 0 if tailing is required
  int32_t oldReadTimeout = inputTransport_->getReadTimeout();
  if (tail) {
    // save old read timeout so it can be restored
    inputTransport_->setReadTimeout(0);
  } 

  uint32_t numProcessed = 0;
  while(1) {
    // bad form to use exceptions for flow control but there is really
    // no other way around it
    try {
      processor_->process(iop.first, iop.second);
      numProcessed++;
      if ( (numEvents > 0) && (numProcessed == numEvents)) {
        return;
      }
    } catch (TEOFException& teof) {
      if (!tail) {
        break;
      }
    } catch (TException te) {
      cerr << te.what() << endl;
      break;
    }
  }

  // restore old read timeout
  if (tail) {
    inputTransport_->setReadTimeout(oldReadTimeout);
  }  

}

}}} // facebook::thrift::transport
