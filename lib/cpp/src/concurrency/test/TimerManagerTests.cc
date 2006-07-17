#include <ThreadManager.h>
#include <PosixThreadFactory.h>

#include <assert.h>

namespace facebook { namespace thrift { namespace concurrency { namespace test {

/** ThreadManagerTests class */


class ThreadManagerTests {

  void init() {

    ThreadManager* threadManager =  ThreadManager::newThreadManager();

    threadManager->poolPolicy(new BasicPoolPolicy());

    threadManager->threadFactory(new PosixThreadFactory());

    threadManager->poolPolicy(new BasicPoolPolicy());
  }
};
  

}}}} // facebook::thrift::concurrency

int main(int argc, char** argv) {

  return 0;

}

