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

#include <cstdlib>
#include <sstream>

#include "THttpClient.h"
#include "TSocket.h"

namespace apache { namespace thrift { namespace transport {

using namespace std;

/**
 * Http client implementation.
 *
 */

// Yeah, yeah, hacky to put these here, I know.
static const char* CRLF = "\r\n";
static const int CRLF_LEN = 2;

THttpClient::THttpClient(boost::shared_ptr<TTransport> transport, string host, string path) :
  transport_(transport),
  host_(host),
  path_(path),
  readHeaders_(true),
  chunked_(false),
  chunkedDone_(false),
  chunkSize_(0),
  contentLength_(0),
  httpBuf_(NULL),
  httpPos_(0),
  httpBufLen_(0),
  httpBufSize_(1024) {
  init();
}

THttpClient::THttpClient(string host, int port, string path) :
  host_(host),
  path_(path),
  readHeaders_(true),
  chunked_(false),
  chunkedDone_(false),
  chunkSize_(0),
  contentLength_(0),
  httpBuf_(NULL),
  httpPos_(0),
  httpBufLen_(0),
  httpBufSize_(1024) {
  transport_ = boost::shared_ptr<TTransport>(new TSocket(host, port));
  init();
}

void THttpClient::init() {
  httpBuf_ = (char*)std::malloc(httpBufSize_+1);
  if (httpBuf_ == NULL) {
    throw TTransportException("Out of memory.");
  }
  httpBuf_[httpBufLen_] = '\0';
}

THttpClient::~THttpClient() {
  if (httpBuf_ != NULL) {
    std::free(httpBuf_);
  }
}

uint32_t THttpClient::read(uint8_t* buf, uint32_t len) {
  if (readBuffer_.available_read() == 0) {
    readBuffer_.resetBuffer();
    uint32_t got = readMoreData();
    if (got == 0) {
      return 0;
    }
  }
  return readBuffer_.read(buf, len);
}

void THttpClient::readEnd() {
  // Read any pending chunked data (footers etc.)
  if (chunked_) {
    while (!chunkedDone_) {
      readChunked();
    }
  }
}

uint32_t THttpClient::readMoreData() {
  // Get more data!
  refill();

  if (readHeaders_) {
    readHeaders();
  }

  if (chunked_) {
    return readChunked();
  } else {
    return readContent(contentLength_);
  }
}

uint32_t THttpClient::readChunked() {
  uint32_t length = 0;

  char* line = readLine();
  uint32_t chunkSize = parseChunkSize(line);
  if (chunkSize == 0) {
    readChunkedFooters();
  } else {
    // Read data content
    length += readContent(chunkSize);
    // Read trailing CRLF after content
    readLine();
  }
  return length;
}

void THttpClient::readChunkedFooters() {
  // End of data, read footer lines until a blank one appears
  while (true) {
    char* line = readLine();
    if (strlen(line) == 0) {
      chunkedDone_ = true;
      break;
    }
  }
}

uint32_t THttpClient::parseChunkSize(char* line) {
  char* semi = strchr(line, ';');
  if (semi != NULL) {
    *semi = '\0';
  }
  int size = 0;
  sscanf(line, "%x", &size);
  return (uint32_t)size;
}

uint32_t THttpClient::readContent(uint32_t size) {
  uint32_t need = size;
  while (need > 0) {
    uint32_t avail = httpBufLen_ - httpPos_;
    if (avail == 0) {
      // We have given all the data, reset position to head of the buffer
      httpPos_ = 0;
      httpBufLen_ = 0;
      refill();

      // Now have available however much we read
      avail = httpBufLen_;
    }
    uint32_t give = avail;
    if (need < give) {
      give = need;
    }
    readBuffer_.write((uint8_t*)(httpBuf_+httpPos_), give);
    httpPos_ += give;
    need -= give;
  }
  return size;
}

char* THttpClient::readLine() {
  while (true) {
    char* eol = NULL;

    eol = strstr(httpBuf_+httpPos_, CRLF);

    // No CRLF yet?
    if (eol == NULL) {
      // Shift whatever we have now to front and refill
      shift();
      refill();
    } else {
      // Return pointer to next line
      *eol = '\0';
      char* line = httpBuf_+httpPos_;
      httpPos_ = (eol-httpBuf_) + CRLF_LEN;
      return line;
    }
  }

}

void THttpClient::shift() {
  if (httpBufLen_ > httpPos_) {
    // Shift down remaining data and read more
    uint32_t length = httpBufLen_ - httpPos_;
    memmove(httpBuf_, httpBuf_+httpPos_, length);
    httpBufLen_ = length;
  } else {
    httpBufLen_ = 0;
  }
  httpPos_ = 0;
  httpBuf_[httpBufLen_] = '\0';
}

void THttpClient::refill() {
  uint32_t avail = httpBufSize_ - httpBufLen_;
  if (avail <= (httpBufSize_ / 4)) {
    httpBufSize_ *= 2;
    httpBuf_ = (char*)std::realloc(httpBuf_, httpBufSize_+1);
    if (httpBuf_ == NULL) {
      throw TTransportException("Out of memory.");
    }
  }

  // Read more data
  uint32_t got = transport_->read((uint8_t*)(httpBuf_+httpBufLen_), httpBufSize_-httpBufLen_);
  httpBufLen_ += got;
  httpBuf_[httpBufLen_] = '\0';

  if (got == 0) {
    throw TTransportException("Could not refill buffer");
  }
}

void THttpClient::readHeaders() {
  // Initialize headers state variables
  contentLength_ = 0;
  chunked_ = false;
  chunkedDone_ = false;
  chunkSize_ = 0;

  // Control state flow
  bool statusLine = true;
  bool finished = false;

  // Loop until headers are finished
  while (true) {
    char* line = readLine();

    if (strlen(line) == 0) {
      if (finished) {
        readHeaders_ = false;
        return;
      } else {
        // Must have been an HTTP 100, keep going for another status line
        statusLine = true;
      }
    } else {
      if (statusLine) {
        statusLine = false;
        finished = parseStatusLine(line);
      } else {
        parseHeader(line);
      }
    }
  }
}

bool THttpClient::parseStatusLine(char* status) {
  char* http = status;

  char* code = strchr(http, ' ');
  if (code == NULL) {
    throw TTransportException(string("Bad Status: ") + status);
  }

  *code = '\0';
  while (*(code++) == ' ');

  char* msg = strchr(code, ' ');
  if (msg == NULL) {
    throw TTransportException(string("Bad Status: ") + status);
  }
  *msg = '\0';

  if (strcmp(code, "200") == 0) {
    // HTTP 200 = OK, we got the response
    return true;
  } else if (strcmp(code, "100") == 0) {
    // HTTP 100 = continue, just keep reading
    return false;
  } else {
    throw TTransportException(string("Bad Status: ") + status);
  }
}

void THttpClient::parseHeader(char* header) {
  char* colon = strchr(header, ':');
  if (colon == NULL) {
    return;
  }
  uint32_t sz = colon - header;
  char* value = colon+1;

  if (strncmp(header, "Transfer-Encoding", sz) == 0) {
    if (strstr(value, "chunked") != NULL) {
      chunked_ = true;
    }
  } else if (strncmp(header, "Content-Length", sz) == 0) {
    chunked_ = false;
    contentLength_ = atoi(value);
  }
}

void THttpClient::write(const uint8_t* buf, uint32_t len) {
  writeBuffer_.write(buf, len);
}

void THttpClient::flush() {
  // Fetch the contents of the write buffer
  uint8_t* buf;
  uint32_t len;
  writeBuffer_.getBuffer(&buf, &len);

  // Construct the HTTP header
  std::ostringstream h;
  h <<
    "POST " << path_ << " HTTP/1.1" << CRLF <<
    "Host: " << host_ << CRLF <<
    "Content-Type: application/x-thrift" << CRLF <<
    "Content-Length: " << len << CRLF <<
    "Accept: application/x-thrift" << CRLF <<
    "User-Agent: C++/THttpClient" << CRLF <<
    CRLF;
  string header = h.str();

  // Write the header, then the data, then flush
  transport_->write((const uint8_t*)header.c_str(), header.size());
  transport_->write(buf, len);
  transport_->flush();

  // Reset the buffer and header variables
  writeBuffer_.resetBuffer();
  readHeaders_ = true;
}

}}} // apache::thrift::transport
