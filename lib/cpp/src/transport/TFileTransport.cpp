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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "TFileTransport.h"
#include "TTransportUtils.h"

#include <pthread.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <sys/stat.h>

namespace apache { namespace thrift { namespace transport {

using boost::scoped_ptr;
using boost::shared_ptr;
using namespace std;
using namespace apache::thrift::protocol;

#ifndef HAVE_CLOCK_GETTIME

/**
 * Fake clock_gettime for systems like darwin
 *
 */
#define CLOCK_REALTIME 0
static int clock_gettime(int clk_id /*ignored*/, struct timespec *tp) {
  struct timeval now;

  int rv = gettimeofday(&now, NULL);
  if (rv != 0) {
    return rv;
  }

  tp->tv_sec = now.tv_sec;
  tp->tv_nsec = now.tv_usec * 1000;
  return 0;
}
#endif

TFileTransport::TFileTransport(string path, bool readOnly)
  : readState_()
  , readBuff_(NULL)
  , currentEvent_(NULL)
  , readBuffSize_(DEFAULT_READ_BUFF_SIZE)
  , readTimeout_(NO_TAIL_READ_TIMEOUT)
  , chunkSize_(DEFAULT_CHUNK_SIZE)
  , eventBufferSize_(DEFAULT_EVENT_BUFFER_SIZE)
  , flushMaxUs_(DEFAULT_FLUSH_MAX_US)
  , flushMaxBytes_(DEFAULT_FLUSH_MAX_BYTES)
  , maxEventSize_(DEFAULT_MAX_EVENT_SIZE)
  , maxCorruptedEvents_(DEFAULT_MAX_CORRUPTED_EVENTS)
  , eofSleepTime_(DEFAULT_EOF_SLEEP_TIME_US)
  , corruptedEventSleepTime_(DEFAULT_CORRUPTED_SLEEP_TIME_US)
  , writerThreadIOErrorSleepTime_(DEFAULT_WRITER_THREAD_SLEEP_TIME_US)
  , writerThreadId_(0)
  , dequeueBuffer_(NULL)
  , enqueueBuffer_(NULL)
  , closing_(false)
  , forceFlush_(false)
  , filename_(path)
  , fd_(0)
  , bufferAndThreadInitialized_(false)
  , offset_(0)
  , lastBadChunk_(0)
  , numCorruptedEventsInChunk_(0)
  , readOnly_(readOnly)
{
  // initialize all the condition vars/mutexes
  pthread_mutex_init(&mutex_, NULL);
  pthread_cond_init(&notFull_, NULL);
  pthread_cond_init(&notEmpty_, NULL);
  pthread_cond_init(&flushed_, NULL);

  openLogFile();
}

void TFileTransport::resetOutputFile(int fd, string filename, int64_t offset) {
  filename_ = filename;
  offset_ = offset;

  // check if current file is still open
  if (fd_ > 0) {
    // flush any events in the queue
    flush();
    GlobalOutput.printf("error, current file (%s) not closed", filename_.c_str());
    if (-1 == ::close(fd_)) {
      int errno_copy = errno;
      GlobalOutput.perror("TFileTransport: resetOutputFile() ::close() ", errno_copy);
      throw TTransportException(TTransportException::UNKNOWN, "TFileTransport: error in file close", errno_copy);
    } else {
      //successfully closed fd
      fd_ = 0;
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
  // flush the buffer if a writer thread is active
  if (writerThreadId_ > 0) {
    // set state to closing
    closing_ = true;

    // wake up the writer thread
    // Since closing_ is true, it will attempt to flush all data, then exit.
    pthread_cond_signal(&notEmpty_);

    pthread_join(writerThreadId_, NULL);
    writerThreadId_ = 0;
  }

  if (dequeueBuffer_) {
    delete dequeueBuffer_;
    dequeueBuffer_ = NULL;
  }

  if (enqueueBuffer_) {
    delete enqueueBuffer_;
    enqueueBuffer_ = NULL;
  }

  if (readBuff_) {
    delete[] readBuff_;
    readBuff_ = NULL;
  }

  if (currentEvent_) {
    delete currentEvent_;
    currentEvent_ = NULL;
  }

  // close logfile
  if (fd_ > 0) {
    if(-1 == ::close(fd_)) {
      GlobalOutput.perror("TFileTransport: ~TFileTransport() ::close() ", errno);
    } else {
      //successfully closed fd
      fd_ = 0;
    }
  }
}

bool TFileTransport::initBufferAndWriteThread() {
  if (bufferAndThreadInitialized_) {
    T_ERROR("%s", "Trying to double-init TFileTransport");
    return false;
  }

  if (writerThreadId_ == 0) {
    if (pthread_create(&writerThreadId_, NULL, startWriterThread, (void *)this) != 0) {
      T_ERROR("%s", "Could not create writer thread");
      return false;
    }
  }

  dequeueBuffer_ = new TFileTransportBuffer(eventBufferSize_);
  enqueueBuffer_ = new TFileTransportBuffer(eventBufferSize_);
  bufferAndThreadInitialized_ = true;

  return true;
}

void TFileTransport::write(const uint8_t* buf, uint32_t len) {
  if (readOnly_) {
    throw TTransportException("TFileTransport: attempting to write to file opened readonly");
  }

  enqueueEvent(buf, len);
}

void TFileTransport::enqueueEvent(const uint8_t* buf, uint32_t eventLen) {
  // can't enqueue more events if file is going to close
  if (closing_) {
    return;
  }

  // make sure that event size is valid
  if ( (maxEventSize_ > 0) && (eventLen > maxEventSize_) ) {
    T_ERROR("msg size is greater than max event size: %u > %u\n", eventLen, maxEventSize_);
    return;
  }

  if (eventLen == 0) {
    T_ERROR("%s", "cannot enqueue an empty event");
    return;
  }

  eventInfo* toEnqueue = new eventInfo();
  toEnqueue->eventBuff_ = (uint8_t *)std::malloc((sizeof(uint8_t) * eventLen) + 4);
  if (toEnqueue->eventBuff_ == NULL) {
    throw std::bad_alloc();
  }
  // first 4 bytes is the event length
  memcpy(toEnqueue->eventBuff_, (void*)(&eventLen), 4);
  // actual event contents
  memcpy(toEnqueue->eventBuff_ + 4, buf, eventLen);
  toEnqueue->eventSize_ = eventLen + 4;

  // lock mutex
  pthread_mutex_lock(&mutex_);

  // make sure that enqueue buffer is initialized and writer thread is running
  if (!bufferAndThreadInitialized_) {
    if (!initBufferAndWriteThread()) {
      delete toEnqueue;
      pthread_mutex_unlock(&mutex_);
      return;
    }
  }

  // Can't enqueue while buffer is full
  while (enqueueBuffer_->isFull()) {
    pthread_cond_wait(&notFull_, &mutex_);
  }

  // We shouldn't be trying to enqueue new data while a forced flush is
  // requested.  (Otherwise the writer thread might not ever be able to finish
  // the flush if more data keeps being enqueued.)
  assert(!forceFlush_);

  // add to the buffer
  if (!enqueueBuffer_->addEvent(toEnqueue)) {
    delete toEnqueue;
    pthread_mutex_unlock(&mutex_);
    return;
  }

  // signal anybody who's waiting for the buffer to be non-empty
  pthread_cond_signal(&notEmpty_);

  // this really should be a loop where it makes sure it got flushed
  // because condition variables can get triggered by the os for no reason
  // it is probably a non-factor for the time being
  pthread_mutex_unlock(&mutex_);
}

bool TFileTransport::swapEventBuffers(struct timespec* deadline) {
  pthread_mutex_lock(&mutex_);

  bool swap;
  if (!enqueueBuffer_->isEmpty()) {
    swap = true;
  } else if (closing_) {
    // even though there is no data to write,
    // return immediately if the transport is closing
    swap = false;
  } else {
    if (deadline != NULL) {
      // if we were handed a deadline time struct, do a timed wait
      pthread_cond_timedwait(&notEmpty_, &mutex_, deadline);
    } else {
      // just wait until the buffer gets an item
      pthread_cond_wait(&notEmpty_, &mutex_);
    }

    // could be empty if we timed out
    swap = enqueueBuffer_->isEmpty();
  }

  if (swap) {
    TFileTransportBuffer *temp = enqueueBuffer_;
    enqueueBuffer_ = dequeueBuffer_;
    dequeueBuffer_ = temp;
  }

  // unlock the mutex and signal if required
  pthread_mutex_unlock(&mutex_);

  if (swap) {
    pthread_cond_signal(&notFull_);
  }

  return swap;
}


void TFileTransport::writerThread() {
  bool hasIOError = false;

  // open file if it is not open
  if(!fd_) {
    try {
      openLogFile();
    } catch (...) {
      int errno_copy = errno;
      GlobalOutput.perror("TFileTransport: writerThread() openLogFile() ", errno_copy);
      fd_ = 0;
      hasIOError = true;
    }
  }

  // set the offset to the correct value (EOF)
  if (!hasIOError) {
    try {
      seekToEnd();
      // throw away any partial events
      offset_ += readState_.lastDispatchPtr_;
      ftruncate(fd_, offset_);
      readState_.resetAllValues();
    } catch (...) {
      int errno_copy = errno;
      GlobalOutput.perror("TFileTransport: writerThread() initialization ", errno_copy);
      hasIOError = true;
    }
  }

  // Figure out the next time by which a flush must take place
  struct timespec ts_next_flush;
  getNextFlushTime(&ts_next_flush);
  uint32_t unflushed = 0;

  while (1) {
    // this will only be true when the destructor is being invoked
    if (closing_) {
      if (hasIOError) {
        pthread_exit(NULL);
      }

      // Try to empty buffers before exit
      if (enqueueBuffer_->isEmpty() && dequeueBuffer_->isEmpty()) {
        fsync(fd_);
        if (-1 == ::close(fd_)) {
          int errno_copy = errno;
          GlobalOutput.perror("TFileTransport: writerThread() ::close() ", errno_copy);
        } else {
          //fd successfully closed
          fd_ = 0;
        }
        pthread_exit(NULL);
      }
    }

    if (swapEventBuffers(&ts_next_flush)) {
      eventInfo* outEvent;
      while (NULL != (outEvent = dequeueBuffer_->getNext())) {
        // Remove an event from the buffer and write it out to disk. If there is any IO error, for instance,
        // the output file is unmounted or deleted, then this event is dropped. However, the writer thread
        // will: (1) sleep for a short while; (2) try to reopen the file; (3) if successful then start writing
        // from the end.

        while (hasIOError) {
          T_ERROR("TFileTransport: writer thread going to sleep for %d microseconds due to IO errors", writerThreadIOErrorSleepTime_);
          usleep(writerThreadIOErrorSleepTime_);
          if (closing_) {
            pthread_exit(NULL);
          }
          if (!fd_) {
            ::close(fd_);
            fd_ = 0;
          }
          try {
            openLogFile();
            seekToEnd();
            unflushed = 0;
            hasIOError = false;
            T_LOG_OPER("TFileTransport: log file %s reopened by writer thread during error recovery", filename_.c_str());
          } catch (...) {
            T_ERROR("TFileTransport: unable to reopen log file %s during error recovery", filename_.c_str());
          }
        }

        // sanity check on event
        if ((maxEventSize_ > 0) && (outEvent->eventSize_ > maxEventSize_)) {
          T_ERROR("msg size is greater than max event size: %u > %u\n", outEvent->eventSize_, maxEventSize_);
          continue;
        }

        // If chunking is required, then make sure that msg does not cross chunk boundary
        if ((outEvent->eventSize_ > 0) && (chunkSize_ != 0)) {
          // event size must be less than chunk size
          if (outEvent->eventSize_ > chunkSize_) {
            T_ERROR("TFileTransport: event size(%u) > chunk size(%u): skipping event", outEvent->eventSize_, chunkSize_);
            continue;
          }

          int64_t chunk1 = offset_/chunkSize_;
          int64_t chunk2 = (offset_ + outEvent->eventSize_ - 1)/chunkSize_;

          // if adding this event will cross a chunk boundary, pad the chunk with zeros
          if (chunk1 != chunk2) {
            // refetch the offset to keep in sync
            offset_ = lseek(fd_, 0, SEEK_CUR);
            int32_t padding = (int32_t)((offset_ / chunkSize_ + 1) * chunkSize_ - offset_);

            uint8_t* zeros = new uint8_t[padding];
            memset(zeros, '\0', padding);
            boost::scoped_array<uint8_t> array(zeros);
            if (-1 == ::write(fd_, zeros, padding)) {
              int errno_copy = errno;
              GlobalOutput.perror("TFileTransport: writerThread() error while padding zeros ", errno_copy);
              hasIOError = true;
              continue;
            }
            unflushed += padding;
            offset_ += padding;
          }
        }

        // write the dequeued event to the file
        if (outEvent->eventSize_ > 0) {
          if (-1 == ::write(fd_, outEvent->eventBuff_, outEvent->eventSize_)) {
            int errno_copy = errno;
            GlobalOutput.perror("TFileTransport: error while writing event ", errno_copy);
            hasIOError = true;
            continue;
          }
          unflushed += outEvent->eventSize_;
          offset_ += outEvent->eventSize_;
        }
      }
      dequeueBuffer_->reset();
    }

    if (hasIOError) {
      continue;
    }

    // Local variable to cache the state of forceFlush_.
    //
    // We only want to check the value of forceFlush_ once each time around the
    // loop.  If we check it more than once without holding the lock the entire
    // time, it could have changed state in between.  This will result in us
    // making inconsistent decisions.
    bool forced_flush = false;
    pthread_mutex_lock(&mutex_);
    if (forceFlush_) {
      if (!enqueueBuffer_->isEmpty()) {
        // If forceFlush_ is true, we need to flush all available data.
        // If enqueueBuffer_ is not empty, go back to the start of the loop to
        // write it out.
        //
        // We know the main thread is waiting on forceFlush_ to be cleared,
        // so no new events will be added to enqueueBuffer_ until we clear
        // forceFlush_.  Therefore the next time around the loop enqueueBuffer_
        // is guaranteed to be empty.  (I.e., we're guaranteed to make progress
        // and clear forceFlush_ the next time around the loop.)
        pthread_mutex_unlock(&mutex_);
        continue;
      }
      forced_flush = true;
    }
    pthread_mutex_unlock(&mutex_);

    // determine if we need to perform an fsync
    bool flush = false;
    if (forced_flush || unflushed > flushMaxBytes_) {
      flush = true;
    } else {
      struct timespec current_time;
      clock_gettime(CLOCK_REALTIME, &current_time);
      if (current_time.tv_sec > ts_next_flush.tv_sec ||
          (current_time.tv_sec == ts_next_flush.tv_sec &&
           current_time.tv_nsec > ts_next_flush.tv_nsec)) {
        if (unflushed > 0) {
          flush = true;
        } else {
          // If there is no new data since the last fsync,
          // don't perform the fsync, but do reset the timer.
          getNextFlushTime(&ts_next_flush);
        }
      }
    }

    if (flush) {
      // sync (force flush) file to disk
      fsync(fd_);
      unflushed = 0;
      getNextFlushTime(&ts_next_flush);

      // notify anybody waiting for flush completion
      if (forced_flush) {
        pthread_mutex_lock(&mutex_);
        forceFlush_ = false;
        assert(enqueueBuffer_->isEmpty());
        assert(dequeueBuffer_->isEmpty());
        pthread_cond_broadcast(&flushed_);
        pthread_mutex_unlock(&mutex_);
      }
    }
  }
}

void TFileTransport::flush() {
  // file must be open for writing for any flushing to take place
  if (writerThreadId_ <= 0) {
    return;
  }
  // wait for flush to take place
  pthread_mutex_lock(&mutex_);

  // Indicate that we are requesting a flush
  forceFlush_ = true;
  // Wake up the writer thread so it will perform the flush immediately
  pthread_cond_signal(&notEmpty_);

  while (forceFlush_) {
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

bool TFileTransport::peek() {
  // check if there is an event ready to be read
  if (!currentEvent_) {
    currentEvent_ = readEvent();
  }

  // did not manage to read an event from the file. This could have happened
  // if the timeout expired or there was some other error
  if (!currentEvent_) {
    return false;
  }

  // check if there is anything to read
  return (currentEvent_->eventSize_ - currentEvent_->eventBuffPos_) > 0;
}

uint32_t TFileTransport::read(uint8_t* buf, uint32_t len) {
  // check if there an event is ready to be read
  if (!currentEvent_) {
    currentEvent_ = readEvent();
  }

  // did not manage to read an event from the file. This could have happened
  // if the timeout expired or there was some other error
  if (!currentEvent_) {
    return 0;
  }

  // read as much of the current event as possible
  int32_t remaining = currentEvent_->eventSize_ - currentEvent_->eventBuffPos_;
  if (remaining <= (int32_t)len) {
    // copy over anything thats remaining
    if (remaining > 0) {
      memcpy(buf,
             currentEvent_->eventBuff_ + currentEvent_->eventBuffPos_,
             remaining);
    }
    delete(currentEvent_);
    currentEvent_ = NULL;
    return remaining;
  }

  // read as much as possible
  memcpy(buf, currentEvent_->eventBuff_ + currentEvent_->eventBuffPos_, len);
  currentEvent_->eventBuffPos_ += len;
  return len;
}

// note caller is responsible for freeing returned events
eventInfo* TFileTransport::readEvent() {
  int readTries = 0;

  if (!readBuff_) {
    readBuff_ = new uint8_t[readBuffSize_];
  }

  while (1) {
    // read from the file if read buffer is exhausted
    if (readState_.bufferPtr_ == readState_.bufferLen_) {
      // advance the offset pointer
      offset_ += readState_.bufferLen_;
      readState_.bufferLen_ = ::read(fd_, readBuff_, readBuffSize_);
      //       if (readState_.bufferLen_) {
      //         T_DEBUG_L(1, "Amount read: %u (offset: %lu)", readState_.bufferLen_, offset_);
      //       }
      readState_.bufferPtr_ = 0;
      readState_.lastDispatchPtr_ = 0;

      // read error
      if (readState_.bufferLen_ == -1) {
        readState_.resetAllValues();
        GlobalOutput("TFileTransport: error while reading from file");
        throw TTransportException("TFileTransport: error while reading from file");
      } else if (readState_.bufferLen_ == 0) {  // EOF
        // wait indefinitely if there is no timeout
        if (readTimeout_ == TAIL_READ_TIMEOUT) {
          usleep(eofSleepTime_);
          continue;
        } else if (readTimeout_ == NO_TAIL_READ_TIMEOUT) {
          // reset state
          readState_.resetState(0);
          return NULL;
        } else if (readTimeout_ > 0) {
          // timeout already expired once
          if (readTries > 0) {
            readState_.resetState(0);
            return NULL;
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
        if (readState_.eventSizeBuffPos_ == 4) {
          // 0 length event indicates padding
          if (*((uint32_t *)(readState_.eventSizeBuff_)) == 0) {
            //            T_DEBUG_L(1, "Got padding");
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

          // check if the event is corrupted and perform recovery if required
          if (isEventCorrupted()) {
            performRecovery();
            // start from the top
            break;
          }
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

        // check if the event has been read in full
        if (readState_.event_->eventBuffPos_ == readState_.event_->eventSize_) {
          // set the completed event to the current event
          eventInfo* completeEvent = readState_.event_;
          completeEvent->eventBuffPos_ = 0;

          readState_.event_ = NULL;
          readState_.resetState(readState_.bufferPtr_);

          // exit criteria
          return completeEvent;
        }
      }
    }

  }
}

bool TFileTransport::isEventCorrupted() {
  // an error is triggered if:
  if ( (maxEventSize_ > 0) &&  (readState_.event_->eventSize_ > maxEventSize_)) {
    // 1. Event size is larger than user-speficied max-event size
    T_ERROR("Read corrupt event. Event size(%u) greater than max event size (%u)",
            readState_.event_->eventSize_, maxEventSize_);
    return true;
  } else if (readState_.event_->eventSize_ > chunkSize_) {
    // 2. Event size is larger than chunk size
    T_ERROR("Read corrupt event. Event size(%u) greater than chunk size (%u)",
               readState_.event_->eventSize_, chunkSize_);
    return true;
  } else if( ((offset_ + readState_.bufferPtr_ - 4)/chunkSize_) !=
             ((offset_ + readState_.bufferPtr_ + readState_.event_->eventSize_ - 1)/chunkSize_) ) {
    // 3. size indicates that event crosses chunk boundary
    T_ERROR("Read corrupt event. Event crosses chunk boundary. Event size:%u  Offset:%lu",
            readState_.event_->eventSize_,
            (offset_ + readState_.bufferPtr_ + 4));

    return true;
  }

  return false;
}

void TFileTransport::performRecovery() {
  // perform some kickass recovery
  uint32_t curChunk = getCurChunk();
  if (lastBadChunk_ == curChunk) {
    numCorruptedEventsInChunk_++;
  } else {
    lastBadChunk_ = curChunk;
    numCorruptedEventsInChunk_ = 1;
  }

  if (numCorruptedEventsInChunk_ < maxCorruptedEvents_) {
    // maybe there was an error in reading the file from disk
    // seek to the beginning of chunk and try again
    seekToChunk(curChunk);
  } else {

    // just skip ahead to the next chunk if we not already at the last chunk
    if (curChunk != (getNumChunks() - 1)) {
      seekToChunk(curChunk + 1);
    } else if (readTimeout_ == TAIL_READ_TIMEOUT) {
      // if tailing the file, wait until there is enough data to start
      // the next chunk
      while(curChunk == (getNumChunks() - 1)) {
        usleep(DEFAULT_CORRUPTED_SLEEP_TIME_US);
      }
      seekToChunk(curChunk + 1);
    } else {
      // pretty hosed at this stage, rewind the file back to the last successful
      // point and punt on the error
      readState_.resetState(readState_.lastDispatchPtr_);
      currentEvent_ = NULL;
      char errorMsg[1024];
      sprintf(errorMsg, "TFileTransport: log file corrupted at offset: %lu",
              (offset_ + readState_.lastDispatchPtr_));
              
      GlobalOutput(errorMsg);
      throw TTransportException(errorMsg);
    }
  }

}

void TFileTransport::seekToChunk(int32_t chunk) {
  if (fd_ <= 0) {
    throw TTransportException("File not open");
  }

  int32_t numChunks = getNumChunks();

  // file is empty, seeking to chunk is pointless
  if (numChunks == 0) {
    return;
  }

  // negative indicates reverse seek (from the end)
  if (chunk < 0) {
    chunk += numChunks;
  }

  // too large a value for reverse seek, just seek to beginning
  if (chunk < 0) {
    T_DEBUG("%s", "Incorrect value for reverse seek. Seeking to beginning...");
    chunk = 0;
  }

  // cannot seek past EOF
  bool seekToEnd = false;
  off_t minEndOffset = 0;
  if (chunk >= numChunks) {
    T_DEBUG("%s", "Trying to seek past EOF. Seeking to EOF instead...");
    seekToEnd = true;
    chunk = numChunks - 1;
    // this is the min offset to process events till
    minEndOffset = lseek(fd_, 0, SEEK_END);
  }

  off_t newOffset = off_t(chunk) * chunkSize_;
  offset_ = lseek(fd_, newOffset, SEEK_SET);
  readState_.resetAllValues();
  currentEvent_ = NULL;
  if (offset_ == -1) {
    GlobalOutput("TFileTransport: lseek error in seekToChunk");
    throw TTransportException("TFileTransport: lseek error in seekToChunk");
  }

  // seek to EOF if user wanted to go to last chunk
  if (seekToEnd) {
    uint32_t oldReadTimeout = getReadTimeout();
    setReadTimeout(NO_TAIL_READ_TIMEOUT);
    // keep on reading unti the last event at point of seekChunk call
    boost::scoped_ptr<eventInfo> event;
    while ((offset_ + readState_.bufferPtr_) < minEndOffset) {
      event.reset(readEvent());
      if (event.get() == NULL) {
        break;
      }
    }
    setReadTimeout(oldReadTimeout);
  }

}

void TFileTransport::seekToEnd() {
  seekToChunk(getNumChunks());
}

uint32_t TFileTransport::getNumChunks() {
  if (fd_ <= 0) {
    return 0;
  }

  struct stat f_info;
  int rv = fstat(fd_, &f_info);

  if (rv < 0) {
    int errno_copy = errno;
    throw TTransportException(TTransportException::UNKNOWN,
                              "TFileTransport::getNumChunks() (fstat)",
                              errno_copy);
  }

  if (f_info.st_size > 0) {
    return ((f_info.st_size)/chunkSize_) + 1;
  }

  // empty file has no chunks
  return 0;
}

uint32_t TFileTransport::getCurChunk() {
  return offset_/chunkSize_;
}

// Utility Functions
void TFileTransport::openLogFile() {
  mode_t mode = readOnly_ ? S_IRUSR | S_IRGRP | S_IROTH : S_IRUSR | S_IWUSR| S_IRGRP | S_IROTH;
  int flags = readOnly_ ? O_RDONLY : O_RDWR | O_CREAT | O_APPEND;
  fd_ = ::open(filename_.c_str(), flags, mode);
  offset_ = 0;

  // make sure open call was successful
  if(fd_ == -1) {
    int errno_copy = errno;
    GlobalOutput.perror("TFileTransport: openLogFile() ::open() file: " + filename_, errno_copy);
    throw TTransportException(TTransportException::NOT_OPEN, filename_, errno_copy);
  }

}

void TFileTransport::getNextFlushTime(struct timespec* ts_next_flush) {
  clock_gettime(CLOCK_REALTIME, ts_next_flush);
  ts_next_flush->tv_nsec += (flushMaxUs_ % 1000000) * 1000;
  if (ts_next_flush->tv_nsec > 1000000000) {
    ts_next_flush->tv_nsec -= 1000000000;
    ts_next_flush->tv_sec += 1;
  }
  ts_next_flush->tv_sec += flushMaxUs_ / 1000000;
}

TFileTransportBuffer::TFileTransportBuffer(uint32_t size)
  : bufferMode_(WRITE)
  , writePoint_(0)
  , readPoint_(0)
  , size_(size)
{
  buffer_ = new eventInfo*[size];
}

TFileTransportBuffer::~TFileTransportBuffer() {
  if (buffer_) {
    for (uint32_t i = 0; i < writePoint_; i++) {
      delete buffer_[i];
    }
    delete[] buffer_;
    buffer_ = NULL;
  }
}

bool TFileTransportBuffer::addEvent(eventInfo *event) {
  if (bufferMode_ == READ) {
    GlobalOutput("Trying to write to a buffer in read mode");
  }
  if (writePoint_ < size_) {
    buffer_[writePoint_++] = event;
    return true;
  } else {
    // buffer is full
    return false;
  }
}

eventInfo* TFileTransportBuffer::getNext() {
  if (bufferMode_ == WRITE) {
    bufferMode_ = READ;
  }
  if (readPoint_ < writePoint_) {
    return buffer_[readPoint_++];
  } else {
    // no more entries
    return NULL;
  }
}

void TFileTransportBuffer::reset() {
  if (bufferMode_ == WRITE || writePoint_ > readPoint_) {
    T_DEBUG("%s", "Resetting a buffer with unread entries");
  }
  // Clean up the old entries
  for (uint32_t i = 0; i < writePoint_; i++) {
    delete buffer_[i];
  }
  bufferMode_ = WRITE;
  writePoint_ = 0;
  readPoint_ = 0;
}

bool TFileTransportBuffer::isFull() {
  return writePoint_ == size_;
}

bool TFileTransportBuffer::isEmpty() {
  return writePoint_ == 0;
}

TFileProcessor::TFileProcessor(shared_ptr<TProcessor> processor,
                               shared_ptr<TProtocolFactory> protocolFactory,
                               shared_ptr<TFileReaderTransport> inputTransport):
  processor_(processor),
  inputProtocolFactory_(protocolFactory),
  outputProtocolFactory_(protocolFactory),
  inputTransport_(inputTransport) {

  // default the output transport to a null transport (common case)
  outputTransport_ = shared_ptr<TNullTransport>(new TNullTransport());
}

TFileProcessor::TFileProcessor(shared_ptr<TProcessor> processor,
                               shared_ptr<TProtocolFactory> inputProtocolFactory,
                               shared_ptr<TProtocolFactory> outputProtocolFactory,
                               shared_ptr<TFileReaderTransport> inputTransport):
  processor_(processor),
  inputProtocolFactory_(inputProtocolFactory),
  outputProtocolFactory_(outputProtocolFactory),
  inputTransport_(inputTransport) {

  // default the output transport to a null transport (common case)
  outputTransport_ = shared_ptr<TNullTransport>(new TNullTransport());
}

TFileProcessor::TFileProcessor(shared_ptr<TProcessor> processor,
                               shared_ptr<TProtocolFactory> protocolFactory,
                               shared_ptr<TFileReaderTransport> inputTransport,
                               shared_ptr<TTransport> outputTransport):
  processor_(processor),
  inputProtocolFactory_(protocolFactory),
  outputProtocolFactory_(protocolFactory),
  inputTransport_(inputTransport),
  outputTransport_(outputTransport) {}

void TFileProcessor::process(uint32_t numEvents, bool tail) {
  shared_ptr<TProtocol> inputProtocol = inputProtocolFactory_->getProtocol(inputTransport_);
  shared_ptr<TProtocol> outputProtocol = outputProtocolFactory_->getProtocol(outputTransport_);

  // set the read timeout to 0 if tailing is required
  int32_t oldReadTimeout = inputTransport_->getReadTimeout();
  if (tail) {
    // save old read timeout so it can be restored
    inputTransport_->setReadTimeout(TFileTransport::TAIL_READ_TIMEOUT);
  }

  uint32_t numProcessed = 0;
  while(1) {
    // bad form to use exceptions for flow control but there is really
    // no other way around it
    try {
      processor_->process(inputProtocol, outputProtocol, NULL);
      numProcessed++;
      if ( (numEvents > 0) && (numProcessed == numEvents)) {
        return;
      }
    } catch (TEOFException& teof) {
      if (!tail) {
        break;
      }
    } catch (TException &te) {
      cerr << te.what() << endl;
      break;
    }
  }

  // restore old read timeout
  if (tail) {
    inputTransport_->setReadTimeout(oldReadTimeout);
  }

}

void TFileProcessor::processChunk() {
  shared_ptr<TProtocol> inputProtocol = inputProtocolFactory_->getProtocol(inputTransport_);
  shared_ptr<TProtocol> outputProtocol = outputProtocolFactory_->getProtocol(outputTransport_);

  uint32_t curChunk = inputTransport_->getCurChunk();

  while(1) {
    // bad form to use exceptions for flow control but there is really
    // no other way around it
    try {
      processor_->process(inputProtocol, outputProtocol, NULL);
      if (curChunk != inputTransport_->getCurChunk()) {
        break;
      }
    } catch (TEOFException& teof) {
      break;
    } catch (TException &te) {
      cerr << te.what() << endl;
      break;
    }
  }
}

}}} // apache::thrift::transport
