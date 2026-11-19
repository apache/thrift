#define BOOST_TEST_MODULE TQTcpServerTest
#include <QTest>
#include <iostream>

#include <QTcpServer>
#include <QTcpSocket>
#include <QHostAddress>
#include <QThread>

#ifndef Q_MOC_RUN
  #include "thrift/protocol/TBinaryProtocol.h"
  #include "thrift/async/TAsyncProcessor.h"
  #include "thrift/qt/TQTcpServer.h"
  #include "thrift/qt/TQIODeviceTransport.h"

  #include "gen-cpp/ParentService.h"
#endif

using namespace apache::thrift;

struct AsyncHandler : public test::ParentServiceCobSvIf {
  std::vector<std::string> strings;
  void addString(std::function<void()> cob, const std::string& s) override {
    strings.push_back(s);
    cob();
  }
  void getStrings(std::function<void(std::vector<std::string> const& _return)> cob) override {
    cob(strings);
  }

  // Overrides not used in this test
  void incrementGeneration(std::function<void(int32_t const& _return)> cob) override {}
  void getGeneration(std::function<void(int32_t const& _return)> cob) override {}
  void getDataWait(std::function<void(std::string const& _return)> cob,
                           const int32_t length) override {}
  void onewayWait(std::function<void()> cob) override {}
  void exceptionWait(
      std::function<void()> cob,
      std::function<void(::apache::thrift::TDelayedException* _throw)> /* exn_cob */,
      const std::string& message) override {}
  void unexpectedExceptionWait(std::function<void()> cob, const std::string& message) override {}
};

class TQTcpServerTest : public QObject {
  Q_OBJECT

private slots:
  void initTestCase();
  void cleanupTestCase();
  void test_communicate();
  void test_failedRequestsLeaveServerServing();

private:
  void sendAndClose(const QByteArray& bytes);

  std::shared_ptr<QThread> serverThread;
  std::shared_ptr<async::TQTcpServer> server;
  std::shared_ptr<test::ParentServiceClient> client;
  int port = 0;
};

void TQTcpServerTest::initTestCase() {
  // setup server
  std::shared_ptr<QTcpServer> serverSocket = std::make_shared<QTcpServer>();
  server.reset(new async::TQTcpServer(serverSocket,
                                      std::make_shared<test::ParentServiceAsyncProcessor>(
                                      std::make_shared<AsyncHandler>()),
                                      std::make_shared<protocol::TBinaryProtocolFactory>()));
  QVERIFY(serverSocket->listen(QHostAddress::LocalHost));
  port = serverSocket->serverPort();
  QVERIFY(port > 0);

  //setup server thread and move server to it
  serverThread.reset(new QThread());
  serverSocket->moveToThread(serverThread.get());
  server->moveToThread(serverThread.get());
  serverThread->start();

  // setup client
  std::shared_ptr<QTcpSocket> socket = std::make_shared<QTcpSocket>();
  client.reset(new test::ParentServiceClient(std::make_shared<protocol::TBinaryProtocol>(
      std::make_shared<transport::TQIODeviceTransport>(socket))));
  socket->connectToHost(QHostAddress::LocalHost, port);
  QVERIFY(socket->waitForConnected());
}

void TQTcpServerTest::cleanupTestCase() {
  //first, stop the thread which holds the server
  serverThread->quit();
  serverThread->wait();
  // now, it is safe to delete the server
  server.reset();
  // delete thread now
  serverThread.reset();

  // cleanup client
  client.reset();
}

void TQTcpServerTest::test_communicate() {
  client->addString("foo");
  client->addString("bar");

  std::vector<std::string> reply;
  client->getStrings(reply);
  QCOMPARE(QString::fromStdString(reply[0]), QString("foo"));
  QCOMPARE(QString::fromStdString(reply[1]), QString("bar"));
}

// Opens a connection of its own, writes bytes and closes it, then gives the
// server thread time to handle the data and the close.
void TQTcpServerTest::sendAndClose(const QByteArray& bytes) {
  QTcpSocket raw;
  raw.connectToHost(QHostAddress::LocalHost, port);
  QVERIFY(raw.waitForConnected());
  raw.write(bytes);
  QVERIFY(raw.waitForBytesWritten());
  raw.disconnectFromHost();
  if (raw.state() != QAbstractSocket::UnconnectedState) {
    raw.waitForDisconnected();
  }
  QTest::qWait(200);
}

// A request the processor cannot complete ends that connection only: a reply
// sent to the server, and a call whose arguments end with the connection.
void TQTcpServerTest::test_failedRequestsLeaveServerServing() {
  // strict binary header of a REPLY: version | type, name "x", seqid 1
  const QByteArray reply(
      "\x80\x01\x00\x02"
      "\x00\x00\x00\x01"
      "x"
      "\x00\x00\x00\x01",
      13);
  sendAndClose(reply);

  // CALL addString, seqid 1, field 1 a string of 100 bytes of which 3 are sent
  const QByteArray call(
      "\x80\x01\x00\x01"
      "\x00\x00\x00\x09"
      "addString"
      "\x00\x00\x00\x01"
      "\x0b\x00\x01"
      "\x00\x00\x00\x64"
      "abc",
      31);
  sendAndClose(call);

  std::vector<std::string> reply2;
  client->getStrings(reply2);
  QCOMPARE(reply2.size(), static_cast<size_t>(2));
}

#if (QT_VERSION >= QT_VERSION_CHECK(5, 0, 0))
QTEST_GUILESS_MAIN(TQTcpServerTest);
#else
#undef QT_GUI_LIB
QTEST_MAIN(TQTcpServerTest);
#endif
#include "TQTcpServerTest.moc"
