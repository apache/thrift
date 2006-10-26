#include "TBufferedFileWriter.h"

#include <pthread.h>
#include <cassert>
#include <cstdlib>
#include <string>
#include <sys/time.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>

using std::string;

namespace facebook { namespace thrift { namespace transport { 

TBufferedFileWriter::TBufferedFileWriter(string filename, uint32_t sz) {
  init(filename, sz, 0, 0);
}

TBufferedFileWriter::TBufferedFileWriter(string filename, uint32_t sz, int fd, long long offset) {
  init(filename, sz, fd, offset);
}

void TBufferedFileWriter::init(string filename, uint32_t sz, int fd, long long offset) {
  // validate buffer size
  sz_ = sz;
  if (sz_ <= 0) {
    throw TTransportException("invalid input buffer size");
  }

  // set file-related variables
  fd_ = 0;
  resetOutputFile(fd, filename, offset);

  // set default values of flush related params
  flushMaxBytes_ = 1024 * 100;
  flushMaxUs_ = 20 * 1000;

  // allocate event buffer
  buffer_ = new eventInfo[sz_];

  // buffer is initially empty
  isEmpty_ = true;
  isFull_  = false;

  // both head and tail are initially at 0
  headPos_ = 0;
  tailPos_ = 0;

  // for lack of a better option, set chunk size to 0. Users can change this to whatever they want
  chunkSize_ = 0;

  // initialize all the condition vars/mutexes
  pthread_mutex_init(&mutex_, NULL);
  pthread_cond_init(&notFull_, NULL);
  pthread_cond_init(&notEmpty_, NULL);
  pthread_cond_init(&flushed_, NULL);

  // not closing the file during init
  closing_ = false;

  // spawn writer thread
  pthread_create(&writer_, NULL, startWriterThread, (void *)this);
}

void TBufferedFileWriter::resetOutputFile(int fd, string filename, long long offset) {
  filename_ = filename;
  offset_ = offset;

  // check if current file is still open
  if (fd_ > 0) {
    // TODO: unclear if this should throw an error
    fprintf(stderr, "error, current file not closed (trying to open %s)\n", filename_.c_str());
    ::close(fd_);
  }
  fd_ = fd;
}


TBufferedFileWriter::~TBufferedFileWriter() {
  // flush output buffer
  flush();

  // send a signal to write thread to end
  closing_ = true;
  pthread_join(writer_, NULL);

  delete[] buffer_;

  // TODO: should the file be closed here?
}


void TBufferedFileWriter::enqueueEvent(const uint8_t* buf, uint32_t eventLen, bool blockUntilFlush) {
  // make sure that event size is valid
  if ( (maxEventSize_ > 0) && (eventLen > maxEventSize_) ) {
    //    T_ERROR("msg size is greater than max event size: %lu > %u\n", eventLen, maxEventSize_);
    return;
  }

  if (eventLen == 0) {
    T_ERROR("cannot enqueue an empty event");
    return;
  }

  eventInfo toEnqueue;
  uint8_t* bufCopy = (uint8_t *)malloc(sizeof(uint8_t) * eventLen);
  toEnqueue.payLoad_ = bufCopy;
  toEnqueue.eventSize_ = eventLen;

  return enqueueEvent(toEnqueue, blockUntilFlush);
}

void TBufferedFileWriter::enqueueEvent(const eventInfo& toEnqueue, bool blockUntilFlush) {
  // Lock mutex
  pthread_mutex_lock(&mutex_);
  // Can't enqueue while buffer is full
  while(isFull_) {
    pthread_cond_wait(&notFull_, &mutex_);
  }

  // make a copy and enqueue at tail of buffer
  buffer_[tailPos_] = toEnqueue;
  tailPos_ = (tailPos_+1) % sz_;
  
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

eventInfo TBufferedFileWriter::dequeueEvent(long long deadline) {
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
  eventInfo retEvent;
  if(!isEmpty_) {
    retEvent = buffer_[headPos_];
    headPos_ = (headPos_+1) % sz_;

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

  return retEvent;
}


void TBufferedFileWriter::flush()
{
  eventInfo flushEvent;
  flushEvent.payLoad_ = NULL;
  flushEvent.eventSize_ = 0;

  notFlushed_ = true;

  enqueueEvent(flushEvent, false);

  // wait for flush to take place
  pthread_mutex_lock(&mutex_);

  while(notFlushed_) {
    pthread_cond_wait(&flushed_, &mutex_);
  }

  pthread_mutex_unlock(&mutex_);
}

void TBufferedFileWriter::openOutputFile() {
  mode_t mode = S_IRUSR| S_IWUSR| S_IRGRP | S_IROTH;
  fd_ = ::open(filename_.c_str(), O_WRONLY | O_CREAT | O_APPEND, mode);

  // make sure open call was successful
  if(fd_ == -1) {
    char errorMsg[1024];
    sprintf(errorMsg, "TBufferedFileWriter: Could not open file: %s", filename_.c_str());
    perror(errorMsg);
    throw TTransportException(errorMsg);
  }
}

uint32_t TBufferedFileWriter::getCurrentTime() {
  long long ret;
  struct timeval tv;
  gettimeofday(&tv, NULL);
  ret = tv.tv_sec;
  ret = ret*1000*1000 + tv.tv_usec;
  return ret;
}


void TBufferedFileWriter::writerThread() {
  // open file if it is not open
  if(!fd_) {
    openOutputFile();
  }

  // Figure out the next time by which a flush must take place
  long long nextFlush = getCurrentTime() + flushMaxUs_;
  uint32_t unflushed = 0;

  while(1) {
    // this will only be true when the destructor is being invoked
    if(closing_) {
      if(-1 == ::close(fd_)) {
        perror("TBufferedFileWriter: error in close");
      }
      throw TTransportException("error in file close");
    }

    //long long start = now();
    eventInfo outEvent = dequeueEvent(nextFlush);

    // sanity check on event
    if ( (maxEventSize_ > 0) && (outEvent.eventSize_ > maxEventSize_)) {
      T_ERROR("msg size is greater than max event size: %u > %u\n", outEvent.eventSize_, maxEventSize_);
      continue;
    }
    //long long diff = now()-start;
    //T_DEBUG("got a dequeue of size %d after %lld ms\n", (int)s.size(), diff/1000);

    // If chunking is required, then make sure that msg does not cross chunk boundary
    if( (outEvent.eventSize_ > 0) && (chunkSize_ != 0)) {

      // event size must be less than chunk size
      if(outEvent.eventSize_ > chunkSize_) {
        T_ERROR("TBufferedFileWriter: event size(%u) is greater than chunk size(%u): skipping event",
              outEvent.eventSize_, chunkSize_);
        continue;
      }

      long long chunk1 = offset_/chunkSize_;
      long long chunk2 = (offset_ + outEvent.eventSize_ - 1)/chunkSize_;
      
      // if adding this event will cross a chunk boundary, pad the chunk with zeros
      if(chunk1 != chunk2) {
        int padding = (int)(chunk2*chunkSize_ - offset_);

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
        if(-1 == ::write(fd_, zeros, padding)) {
          perror("TBufferedFileWriter: error while padding zeros");
          throw TTransportException("TBufferedFileWriter: error while padding zeros");
        }
        unflushed += padding;
        offset_ += padding;
      }
    }

    // write the dequeued event to the file
    if(outEvent.eventSize_ > 0) {
      if(-1 == ::write(fd_, outEvent.payLoad_, outEvent.eventSize_)) {
        perror("TBufferedFileWriter: error while writing event");
        // TODO: should this trigger an exception or simply continue?
        throw TTransportException("TBufferedFileWriter: error while writing event");
      }

      // deallocate payload
      free(outEvent.payLoad_);

      unflushed += outEvent.eventSize_;
      offset_ += outEvent.eventSize_;
    }

    // couple of cases from which a flush could be triggered
    if((getCurrentTime() >= nextFlush && unflushed > 0) ||
       unflushed > flushMaxBytes_ ||
       (outEvent.eventSize_ == 0) ) {
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
  }

}

}}} // facebook::thrift::transport
