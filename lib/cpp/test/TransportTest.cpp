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
#ifndef _GNU_SOURCE
#define _GNU_SOURCE // needed for getopt_long
#endif

#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <getopt.h>
#include <sstream>
#include <tr1/functional>

#include <boost/mpl/list.hpp>
#include <boost/shared_array.hpp>
#include <boost/random.hpp>
#include <boost/type_traits.hpp>
#include <boost/test/unit_test.hpp>

#include <transport/TBufferTransports.h>
#include <transport/TFDTransport.h>
#include <transport/TFileTransport.h>

using namespace apache::thrift::transport;

static boost::mt19937 rng;
static const char* tmp_dir = "/tmp";

void initrand(const int* seedptr) {
  unsigned int seed;
  if (seedptr) {
    seed = *seedptr;
  } else {
    seed = static_cast<unsigned int>(time(NULL));
  }

  rng.seed(seed);
}

class SizeGenerator {
 public:
  virtual ~SizeGenerator() {}
  virtual uint32_t nextSize() = 0;
  virtual std::string describe() const = 0;
};

class ConstantSizeGenerator : public SizeGenerator {
 public:
  ConstantSizeGenerator(uint32_t value) : value_(value) {}
  uint32_t nextSize() { return value_; }
  std::string describe() const {
    std::ostringstream desc;
    desc << value_;
    return desc.str();
  }

 private:
  uint32_t value_;
};

class RandomSizeGenerator : public SizeGenerator {
 public:
  RandomSizeGenerator(uint32_t min, uint32_t max) :
    generator_(rng, boost::uniform_int<int>(min, max)) {}

  uint32_t nextSize() { return generator_(); }

  std::string describe() const {
    std::ostringstream desc;
    desc << "rand(" << getMin() << ", " << getMax() << ")";
    return desc.str();
  }

  uint32_t getMin() const { return generator_.distribution().min(); }
  uint32_t getMax() const { return generator_.distribution().max(); }

 private:
  boost::variate_generator< boost::mt19937&, boost::uniform_int<int> >
    generator_;
};

/**
 * This class exists solely to make the TEST_RW() macro easier to use.
 * - it can be constructed implicitly from an integer
 * - it can contain either a ConstantSizeGenerator or a RandomSizeGenerator
 *   (TEST_RW can't take a SizeGenerator pointer or reference, since it needs
 *   to make a copy of the generator to bind it to the test function.)
 */
class GenericSizeGenerator : public SizeGenerator {
 public:
  GenericSizeGenerator(uint32_t value) :
    generator_(new ConstantSizeGenerator(value)) {}
  GenericSizeGenerator(uint32_t min, uint32_t max) :
    generator_(new RandomSizeGenerator(min, max)) {}

  uint32_t nextSize() { return generator_->nextSize(); }
  std::string describe() const { return generator_->describe(); }

 private:
  boost::shared_ptr<SizeGenerator> generator_;
};

/**************************************************************************
 * Classes to set up coupled transports
 **************************************************************************/

template <class Transport_>
class CoupledTransports {
 public:
  typedef Transport_ TransportType;

  CoupledTransports() : in(NULL), out(NULL) {}

  Transport_* in;
  Transport_* out;

 private:
  CoupledTransports(const CoupledTransports&);
  CoupledTransports &operator=(const CoupledTransports&);
};

class CoupledMemoryBuffers : public CoupledTransports<TMemoryBuffer> {
 public:
  CoupledMemoryBuffers() {
    in = &buf;
    out = &buf;
  }

  TMemoryBuffer buf;
};

class CoupledBufferedTransports :
  public CoupledTransports<TBufferedTransport> {
 public:
  CoupledBufferedTransports() :
    buf(new TMemoryBuffer) {
    in = new TBufferedTransport(buf);
    out = new TBufferedTransport(buf);
  }

  ~CoupledBufferedTransports() {
    delete in;
    delete out;
  }

  boost::shared_ptr<TMemoryBuffer> buf;
};

class CoupledFramedTransports : public CoupledTransports<TFramedTransport> {
 public:
  CoupledFramedTransports() :
    buf(new TMemoryBuffer) {
    in = new TFramedTransport(buf);
    out = new TFramedTransport(buf);
  }

  ~CoupledFramedTransports() {
    delete in;
    delete out;
  }

  boost::shared_ptr<TMemoryBuffer> buf;
};

class CoupledFDTransports : public CoupledTransports<TFDTransport> {
 public:
  CoupledFDTransports() {
    int pipes[2];

    if (pipe(pipes) != 0) {
      return;
    }

    in = new TFDTransport(pipes[0], TFDTransport::CLOSE_ON_DESTROY);
    out = new TFDTransport(pipes[1], TFDTransport::CLOSE_ON_DESTROY);
  }

  ~CoupledFDTransports() {
    delete in;
    delete out;
  }
};

class CoupledFileTransports : public CoupledTransports<TFileTransport> {
 public:
  CoupledFileTransports() {
    // Create a temporary file to use
    size_t filename_len = strlen(tmp_dir) + 32;
    filename = new char[filename_len];
    snprintf(filename, filename_len,
             "%s/thrift.transport_test.XXXXXX", tmp_dir);
    fd = mkstemp(filename);
    if (fd < 0) {
      return;
    }

    in = new TFileTransport(filename, true);
    out = new TFileTransport(filename);
  }

  ~CoupledFileTransports() {
    delete in;
    delete out;

    if (fd >= 0) {
      close(fd);
      unlink(filename);
    }
    delete[] filename;
  }

  char* filename;
  int fd;
};

template <class CoupledTransports_>
class CoupledTTransports : public CoupledTransports<TTransport> {
 public:
  CoupledTTransports() : transports() {
    in = transports.in;
    out = transports.out;
  }

  CoupledTransports_ transports;
};

template <class CoupledTransports_>
class CoupledBufferBases : public CoupledTransports<TBufferBase> {
 public:
  CoupledBufferBases() : transports() {
    in = transports.in;
    out = transports.out;
  }

  CoupledTransports_ transports;
};

/*
 * TODO: It would be nice to test TSocket, too.
 * Unfortunately, TSocket/TServerSocket currently don't provide a low-level
 * API that would allow us to create a connected socket pair.
 *
 * TODO: It would be nice to test TZlibTransport, too.
 * However, TZlibTransport doesn't conform to quite the same semantics as other
 * transports.  No new data can be written to a TZlibTransport after flush() is
 * called, since flush() terminates the zlib data stream.  In the future maybe
 * we should make TZlibTransport behave more like the other transports.
 */

/**************************************************************************
 * Main testing function
 **************************************************************************/

/**
 * Test interleaved write and read calls.
 *
 * Generates a buffer totalSize bytes long, then writes it to the transport,
 * and verifies the written data can be read back correctly.
 *
 * Mode of operation:
 * - call wChunkGenerator to figure out how large of a chunk to write
 *   - call wSizeGenerator to get the size for individual write() calls,
 *     and do this repeatedly until the entire chunk is written.
 * - call rChunkGenerator to figure out how large of a chunk to read
 *   - call rSizeGenerator to get the size for individual read() calls,
 *     and do this repeatedly until the entire chunk is read.
 * - repeat until the full buffer is written and read back,
 *   then compare the data read back against the original buffer
 *
 *
 * - If any of the size generators return 0, this means to use the maximum
 *   possible size.
 *
 * - If maxOutstanding is non-zero, write chunk sizes will be chosen such that
 *   there are never more than maxOutstanding bytes waiting to be read back.
 */
template <class CoupledTransports>
void test_rw(uint32_t totalSize,
             SizeGenerator& wSizeGenerator,
             SizeGenerator& rSizeGenerator,
             SizeGenerator& wChunkGenerator,
             SizeGenerator& rChunkGenerator,
             uint32_t maxOutstanding) {
  CoupledTransports transports;
  BOOST_REQUIRE(transports.in != NULL);
  BOOST_REQUIRE(transports.out != NULL);

  boost::shared_array<uint8_t> wbuf =
    boost::shared_array<uint8_t>(new uint8_t[totalSize]);
  boost::shared_array<uint8_t> rbuf =
    boost::shared_array<uint8_t>(new uint8_t[totalSize]);

  // store some data in wbuf
  for (uint32_t n = 0; n < totalSize; ++n) {
    wbuf[n] = (n & 0xff);
  }
  // clear rbuf
  memset(rbuf.get(), 0, totalSize);

  uint32_t total_written = 0;
  uint32_t total_read = 0;
  while (total_read < totalSize) {
    // Determine how large a chunk of data to write
    uint32_t wchunk_size = wChunkGenerator.nextSize();
    if (wchunk_size == 0 || wchunk_size > totalSize - total_written) {
      wchunk_size = totalSize - total_written;
    }

    // Make sure (total_written - total_read) + wchunk_size
    // is less than maxOutstanding
    if (maxOutstanding > 0 &&
        wchunk_size > maxOutstanding - (total_written - total_read)) {
      wchunk_size = maxOutstanding - (total_written - total_read);
    }

    // Write the chunk
    uint32_t chunk_written = 0;
    while (chunk_written < wchunk_size) {
      uint32_t write_size = wSizeGenerator.nextSize();
      if (write_size == 0 || write_size > wchunk_size - chunk_written) {
        write_size = wchunk_size - chunk_written;
      }

      transports.out->write(wbuf.get() + total_written, write_size);
      chunk_written += write_size;
      total_written += write_size;
    }

    // Flush the data, so it will be available in the read transport
    // Don't flush if wchunk_size is 0.  (This should only happen if
    // total_written == totalSize already, and we're only reading now.)
    if (wchunk_size > 0) {
      transports.out->flush();
    }

    // Determine how large a chunk of data to read back
    uint32_t rchunk_size = rChunkGenerator.nextSize();
    if (rchunk_size == 0 || rchunk_size > total_written - total_read) {
      rchunk_size = total_written - total_read;
    }

    // Read the chunk
    uint32_t chunk_read = 0;
    while (chunk_read < rchunk_size) {
      uint32_t read_size = rSizeGenerator.nextSize();
      if (read_size == 0 || read_size > rchunk_size - chunk_read) {
        read_size = rchunk_size - chunk_read;
      }

      int bytes_read = transports.in->read(rbuf.get() + total_read, read_size);
      BOOST_REQUIRE_MESSAGE(bytes_read > 0,
                            "read(pos=" << total_read << ", size=" <<
                            read_size << ") returned " << bytes_read <<
                            "; written so far: " << total_written << " / " <<
                            totalSize << " bytes");
      chunk_read += bytes_read;
      total_read += bytes_read;
    }
  }

  // make sure the data read back is identical to the data written
  BOOST_CHECK_EQUAL(memcmp(rbuf.get(), wbuf.get(), totalSize), 0);
}

/**************************************************************************
 * Test case generation
 *
 * Pretty ugly and annoying.  This would be much easier if we the unit test
 * framework didn't force each test to be a separate function.
 * - Writing a completely separate function definition for each of these would
 *   result in a lot of repetitive boilerplate code.
 * - Combining many tests into a single function makes it more difficult to
 *   tell precisely which tests failed.  It also means you can't get a progress
 *   update after each test, and the tests are already fairly slow.
 * - Similar registration could be acheived with BOOST_TEST_CASE_TEMPLATE,
 *   but it requires a lot of awkward MPL code, and results in useless test
 *   case names.  (The names are generated from std::type_info::name(), which
 *   is compiler-dependent.  gcc returns mangled names.)
 **************************************************************************/

#define TEST_RW(CoupledTransports, totalSize, ...) \
  do { \
    /* Add the test as specified, to test the non-virtual function calls */ \
    addTest<CoupledTransports>(BOOST_STRINGIZE(CoupledTransports), \
                               totalSize, ## __VA_ARGS__); \
    /* \
     * Also test using the transport as a TTransport*, to test \
     * the read_virt()/write_virt() calls \
     */ \
    addTest< CoupledTTransports<CoupledTransports> >( \
        BOOST_STRINGIZE(CoupledTTransports<CoupledTransports>), \
                        totalSize, ## __VA_ARGS__); \
  } while (0)

#define TEST_RW_BUF(CoupledTransports, totalSize, ...) \
  do { \
    /* Add the standard tests */ \
    TEST_RW(CoupledTransports, totalSize, ## __VA_ARGS__); \
    /* Also test using the transport as a TBufferBase* */ \
    addTest< CoupledBufferBases<CoupledTransports> >( \
        BOOST_STRINGIZE(CoupledBufferBases<CoupledTransports>), \
                        totalSize, ## __VA_ARGS__); \
  } while (0)

// We use the same tests for all of the buffered transports
// This is a helper macro so we don't have to copy-and-paste them.
#define BUFFER_TESTS(CoupledTransports) \
  TEST_RW_BUF(CoupledTransports, 1024*1024*30, 0, 0); \
  TEST_RW_BUF(CoupledTransports, 1024*1024*10, rand4k, rand4k); \
  TEST_RW_BUF(CoupledTransports, 1024*1024*10, 167, 163); \
  TEST_RW_BUF(CoupledTransports, 1024*1024, 1, 1); \
  \
  TEST_RW_BUF(CoupledTransports, 1024*1024*10, 0, 0, rand4k, rand4k); \
  TEST_RW_BUF(CoupledTransports, 1024*1024*10, \
              rand4k, rand4k, rand4k, rand4k); \
  TEST_RW_BUF(CoupledTransports, 1024*1024*10, 167, 163, rand4k, rand4k); \
  TEST_RW_BUF(CoupledTransports, 1024*1024*2, 1, 1, rand4k, rand4k);

class TransportTestGen {
 public:
  TransportTestGen(boost::unit_test::test_suite* suite) : suite_(suite) {}

  void generate() {
    GenericSizeGenerator rand4k(1, 4096);

    /*
     * We do the basically the same set of tests for each transport type,
     * although we tweak the parameters in some places.
     */

    // Buffered transport tests
    BUFFER_TESTS(CoupledMemoryBuffers)
    BUFFER_TESTS(CoupledBufferedTransports)
    BUFFER_TESTS(CoupledFramedTransports)

    // TFDTransport tests
    // Since CoupledFDTransports tests with a pipe, writes will block
    // if there is too much outstanding unread data in the pipe.
    uint32_t fd_max_outstanding = 4096;
    TEST_RW(CoupledFDTransports, 1024*1024*30, 0, 0,
            0, 0, fd_max_outstanding);
    TEST_RW(CoupledFDTransports, 1024*1024*10, rand4k, rand4k,
            0, 0, fd_max_outstanding);
    TEST_RW(CoupledFDTransports, 1024*1024*10, 167, 163,
            0, 0, fd_max_outstanding);
    TEST_RW(CoupledFDTransports, 1024*512, 1, 1,
            0, 0, fd_max_outstanding);

    TEST_RW(CoupledFDTransports, 1024*1024*10, 0, 0,
            rand4k, rand4k, fd_max_outstanding);
    TEST_RW(CoupledFDTransports, 1024*1024*10, rand4k, rand4k,
            rand4k, rand4k, fd_max_outstanding);
    TEST_RW(CoupledFDTransports, 1024*1024*10, 167, 163,
            rand4k, rand4k, fd_max_outstanding);
    TEST_RW(CoupledFDTransports, 1024*512, 1, 1,
            rand4k, rand4k, fd_max_outstanding);

    // TFileTransport tests
    // We use smaller buffer sizes here, since TFileTransport is fairly slow.
    //
    // TFileTransport can't write more than 16MB at once
    uint32_t max_write_at_once = 1024*1024*16 - 4;
    TEST_RW(CoupledFileTransports, 1024*1024*30, max_write_at_once, 0);
    TEST_RW(CoupledFileTransports, 1024*1024*5, rand4k, rand4k);
    TEST_RW(CoupledFileTransports, 1024*1024*5, 167, 163);
    TEST_RW(CoupledFileTransports, 1024*64, 1, 1);

    TEST_RW(CoupledFileTransports, 1024*1024*2, 0, 0, rand4k, rand4k);
    TEST_RW(CoupledFileTransports, 1024*1024*2,
            rand4k, rand4k, rand4k, rand4k);
    TEST_RW(CoupledFileTransports, 1024*1024*2, 167, 163, rand4k, rand4k);
    TEST_RW(CoupledFileTransports, 1024*64, 1, 1, rand4k, rand4k);
  }

 private:
  template <class CoupledTransports>
  void addTest(const char* transport_name, uint32_t totalSize,
               GenericSizeGenerator wSizeGen, GenericSizeGenerator rSizeGen,
               GenericSizeGenerator wChunkSizeGen = 0,
               GenericSizeGenerator rChunkSizeGen = 0,
               uint32_t maxOutstanding = 0,
               uint32_t expectedFailures = 0) {
    std::ostringstream name;
    name << transport_name << "::test_rw(" << totalSize << ", " <<
      wSizeGen.describe() << ", " << rSizeGen.describe() << ", " <<
      wChunkSizeGen.describe() << ", " << rChunkSizeGen.describe() << ", " <<
      maxOutstanding << ")";

    boost::unit_test::callback0<> test_func =
      std::tr1::bind(test_rw<CoupledTransports>, totalSize,
                     wSizeGen, rSizeGen, wChunkSizeGen, rChunkSizeGen,
                     maxOutstanding);
    boost::unit_test::test_case* tc =
      boost::unit_test::make_test_case(test_func, name.str());
    suite_->add(tc, expectedFailures);
  };

  boost::unit_test::test_suite* suite_;
};

/**************************************************************************
 * General Initialization
 **************************************************************************/

void print_usage(FILE* f, const char* argv0) {
  fprintf(f, "Usage: %s [boost_options] [options]\n", argv0);
  fprintf(f, "Options:\n");
  fprintf(f, "  --seed=<N>, -s <N>\n");
  fprintf(f, "  --tmp-dir=DIR, -t DIR\n");
  fprintf(f, "  --help\n");
}

void parse_args(int argc, char* argv[]) {
  int seed;
  int *seedptr = NULL;

  struct option long_opts[] = {
    { "help", false, NULL, 'h' },
    { "seed", true, NULL, 's' },
    { "tmp-dir", true, NULL, 't' },
    { NULL, 0, NULL, 0 }
  };

  while (true) {
    optopt = 1;
    int optchar = getopt_long(argc, argv, "hs:t:", long_opts, NULL);
    if (optchar == -1) {
      break;
    }

    switch (optchar) {
      case 't':
        tmp_dir = optarg;
        break;
      case 's': {
        char *endptr;
        seed = strtol(optarg, &endptr, 0);
        if (endptr == optarg || *endptr != '\0') {
          fprintf(stderr, "invalid seed value \"%s\": must be an integer\n",
                  optarg);
          exit(1);
        }
        seedptr = &seed;
        break;
      }
      case 'h':
        print_usage(stdout, argv[0]);
        exit(0);
      case '?':
        exit(1);
      default:
        // Only happens if someone adds another option to the optarg string,
        // but doesn't update the switch statement to handle it.
        fprintf(stderr, "unknown option \"-%c\"\n", optchar);
        exit(1);
    }
  }

  initrand(seedptr);
}

boost::unit_test::test_suite* init_unit_test_suite(int argc, char* argv[]) {
  // Parse arguments
  parse_args(argc, argv);

  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE("TransportTests");
  TransportTestGen transport_test_generator(suite);
  transport_test_generator.generate();

  return suite;
}
