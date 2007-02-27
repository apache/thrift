#include "THttpClient.h"
#include "TSocket.h"

namespace facebook { namespace thrift { namespace transport { 

using namespace std;

/**
 * Http client implementation.
 *
 * @author Mark Slee <mcslee@facebook.com>
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
  chunkSize_(0),
  contentLength_(0),
  httpBuf_(NULL),
  httpBufPos_(0),
  httpBufSize_(1024) {
  init();
}

THttpClient::THttpClient(string host, int port, string path) :
  host_(host),
  path_(path),
  readHeaders_(true),
  chunked_(false),
  chunkSize_(0),
  contentLength_(0),
  httpBuf_(NULL),
  httpBufPos_(0),
  httpBufSize_(1024) {
  transport_ = boost::shared_ptr<TTransport>(new TSocket(host, port));
  init();
}

void THttpClient::init() {
  httpBuf_ = (char*)malloc(httpBufSize_+1);
  if (httpBuf_ == NULL) {
    throw TTransportException("Out of memory.");
  }
  httpBuf_[httpBufPos_] = '\0';
}

THttpClient::~THttpClient() {
  if (httpBuf_ != NULL) {
    free(httpBuf_);
  }
}

uint32_t THttpClient::read(uint8_t* buf, uint32_t len) {
  if (readBuffer_.available() == 0) {
    readBuffer_.resetBuffer();
    uint32_t got = readMoreData();
    if (got == 0) {
      return 0;
    }
  }
  return readBuffer_.read(buf, len);
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
    char* read;
    read = readContent(httpBuf_, contentLength_);
    shift(read);
    return contentLength_;
  }
}

uint32_t THttpClient::readChunked() {
  uint32_t length = 0;
  char* nextLine = httpBuf_;
  while (true) {
    char* line = readLine(nextLine, &nextLine);
    uint32_t chunkSize = parseChunkSize(line);
    if (chunkSize == 0) {
      break;
    }
    // Read data content
    nextLine = readContent(nextLine, chunkSize);
    length += chunkSize;

    // Read trailing CRLF after content
    readLine(nextLine, &nextLine);
  }

  // Read footer lines until a blank one appears
  while (true) {
    char* line = readLine(nextLine, &nextLine);
    if (strlen(line) == 0) {
      break;
    }
  }

  // Shift down whatever we have left in the buf
  shift(nextLine);

  return length;
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

char* THttpClient::readContent(char* pos, uint32_t size) {
  uint32_t need = size;

  while (need > 0) {
    uint32_t avail = httpBufPos_ - (pos - httpBuf_);
    if (avail == 0) {
      // We have given all the data, reset position to head of the buffer
      pos = shift(pos);
      pos = refill();
      
      // Now have available however much we read
      avail = httpBufPos_;
    }
    uint32_t give = avail;
    if (need < give) {
      give = need;
    }
    readBuffer_.write((uint8_t*)pos, give);
    pos += give;
    need -= give;
  }
  return pos;
}
  
char* THttpClient::readLine(char* pos, char** next) {
  while (true) {
    char* eol = NULL;

    // Note, the data we read could have ended right on the CRLF pair
    if (pos != NULL) {
      eol = strstr(pos, CRLF);
    }   

    // No CRLF yet?
    if (eol == NULL) {
      // Shift whatever we have now to front and refill
      pos = shift(pos);
      pos = refill();
    } else {
      // Return pointer to next line
      *eol = '\0';
      *next = eol + CRLF_LEN;
      return pos;
    }
  }

}

char* THttpClient::shift(char* pos) {
  if (pos != NULL && httpBufPos_ > (pos - httpBuf_)) {
    // Shift down remaining data and read more
    uint32_t length = httpBufPos_ - (pos - httpBuf_);
    memmove(httpBuf_, pos, length);
    httpBufPos_ = length;       
  } else {
    httpBufPos_ = 0;
  }
  httpBuf_[httpBufPos_] = '\0';
  return httpBuf_;
}

char* THttpClient::refill() {
  uint32_t avail = httpBufSize_ - httpBufPos_;
  if (avail <= (httpBufSize_ / 4)) {
    httpBufSize_ *= 2;
    httpBuf_ = (char*)realloc(httpBuf_, httpBufSize_+1);
    if (httpBuf_ == NULL) {
      throw TTransportException("Out of memory.");
    }
  }
      
  // Read more data
  uint32_t got = transport_->read((uint8_t*)(httpBuf_+httpBufPos_), httpBufSize_-httpBufPos_);
  httpBufPos_ += got;
  httpBuf_[httpBufPos_] = '\0';
 
  if (got == 0) {
    throw TTransportException("Could not refill buffer");
  }

  return httpBuf_;
}

void THttpClient::readHeaders() {
  // Initialize headers state variables
  contentLength_ = 0;
  chunked_ = false;
  chunkSize_ = 0;

  // Control state flow
  bool statusLine = true;
  bool finished = false;

  // Initialize local pos vars
  char* nextLine = (char*)httpBuf_;

  // Loop until headers are finished
  while (true) {
    char* line = readLine(nextLine, &nextLine);

    if (strlen(line) == 0) {
      if (finished) {
        readHeaders_ = false;
        shift(nextLine);
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
  *code = '\0';

  while (*(code++) == ' ');

  char* msg = strchr(code, ' ');
  *msg = '\0';

  if (strcmp(code, "200") == 0) {
    // HTTP 200 = OK, we got the response
    return true;
  } else if (strcmp(code, "100") == 0) {
    // HTTP 100 = continue, just keep reading
    return false;
  } else {
    throw TTransportException(status);
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

}}} // facebook::thrift::transport
