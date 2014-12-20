#define BOOST_TEST_MODULE TQTcpServerTest
#include <QTest>
#include <boost/smart_ptr.hpp>
#include <iostream>

#include <QTcpServer>
#include <QTcpSocket>
#include <QHostAddress>

#include "thrift/protocol/TBinaryProtocol.h"
#include "thrift/async/TAsyncProcessor.h"
#include "thrift/qt/TQTcpServer.h"
#include "thrift/qt/TQIODeviceTransport.h"

#include "gen-cpp/ParentService.h"

using namespace apache::thrift;

struct AsyncHandler : public test::ParentServiceCobSvIf {
  std::vector<std::string> strings;
  virtual void addString(tcxx::function<void()> cob, const std::string& s) {
    strings.push_back(s);
    cob();
  }
  virtual void getStrings(tcxx::function<void(std::vector<std::string> const& _return)> cob) {
    cob(strings);
  }

  // Overrides not used in this test
  virtual void incrementGeneration(tcxx::function<void(int32_t const& _return)> cob) {}
  virtual void getGeneration(tcxx::function<void(int32_t const& _return)> cob) {}
  virtual void getDataWait(tcxx::function<void(std::string const& _return)> cob,
                           const int32_t length) {}
  virtual void onewayWait(tcxx::function<void()> cob) {}
  virtual void exceptionWait(
      tcxx::function<void()> cob,
      tcxx::function<void(::apache::thrift::TDelayedException* _throw)> /* exn_cob */,
      const std::string& message) {}
  virtual void unexpectedExceptionWait(tcxx::function<void()> cob, const std::string& message) {}
};

class TQTcpServerTest : public QObject {
  void init() {
    // setup server
    serverSocket.reset(new QTcpServer);
    server.reset(new async::TQTcpServer(serverSocket,
                                        boost::make_shared<test::ParentServiceAsyncProcessor>(
                                            boost::make_shared<AsyncHandler>()),
                                        boost::make_shared<protocol::TBinaryProtocolFactory>()));
    QVERIFY(serverSocket->listen(QHostAddress::LocalHost));
    int port = serverSocket->serverPort();
    QVERIFY(port > 0);

    // setup client
    socket.reset(new QTcpSocket);
    client.reset(new test::ParentServiceClient(boost::make_shared<protocol::TBinaryProtocol>(
        boost::make_shared<transport::TQIODeviceTransport>(socket))));
    socket->connectToHost(QHostAddress::LocalHost, port);
    QVERIFY(socket->waitForConnected());
  }

  void cleanup() {
    socket->close();
    serverSocket->close();
  }

  void test_communicate() {
    client->addString("foo");
    client->addString("bar");

    std::vector<std::string> reply;
    client->getStrings(reply);
    QCOMPARE(reply[0], "foo");
    QCOMPARE(reply[1], "foo");
  }

  boost::shared_ptr<QTcpServer> serverSocket;
  boost::shared_ptr<async::TQTcpServer> server;
  boost::shared_ptr<QTcpSocket> socket;
  boost::shared_ptr<test::ParentServiceClient> client;
};

#if (QT_VERSION >= QT_VERSION_CHECK(5, 0, 0))
QTEST_GUILESS_MAIN(TQTcpServerTest);
#else
#undef QT_GUI_LIB
QTEST_MAIN(TQTcpServerTest);
#endif
#include "TQTcpServerTest.moc"
