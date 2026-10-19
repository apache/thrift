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

#include <QTest>

#include <QBuffer>
#include <QHostAddress>
#include <QTcpServer>
#include <QTcpSocket>

#include <memory>

#ifndef Q_MOC_RUN
  #include "thrift/qt/TQIODeviceTransport.h"
  #include "thrift/transport/TTransportException.h"
#endif

using namespace apache::thrift;

// readAll() asks the underlying QIODevice for the whole requested length and
// retries whenever a read returns nothing, waiting on the device in between.
// When the device can no longer deliver -- a peer that sent part of a message
// and closed, or a truncated in-process buffer -- the retry can only ever read
// zero again, so readAll() must end with END_OF_FILE rather than retry without
// progress. These tests cover both the socket case (the server/client wire
// path) and the non-socket case (an in-process QBuffer).
class TQIODeviceTransportTest : public QObject {
  Q_OBJECT

private slots:
  // Non-socket path: a QBuffer that holds fewer bytes than readAll() is asked
  // for, with nothing more to come, ends in END_OF_FILE.
  void test_readAll_on_truncated_buffer_ends() {
    auto buffer = std::make_shared<QBuffer>();
    QByteArray data;
    data.append(static_cast<char>(0x80));
    data.append(static_cast<char>(0x01));
    data.append(static_cast<char>(0x00));
    data.append(static_cast<char>(0x01));
    buffer->setData(data);
    QVERIFY(buffer->open(QIODevice::ReadOnly));

    transport::TQIODeviceTransport trans(buffer);

    uint8_t buf[8];
    bool threw = false;
    try {
      trans.readAll(buf, sizeof(buf)); // wants 8, only 4 present, no more will come
    } catch (const transport::TTransportException& e) {
      threw = true;
      QCOMPARE(static_cast<int>(e.getType()),
               static_cast<int>(transport::TTransportException::END_OF_FILE));
    }
    QVERIFY(threw);
  }

  // Socket path: a peer sends part of a message and closes. readAll() must end
  // with END_OF_FILE once the socket reaches the unconnected state.
  void test_readAll_when_peer_closes_mid_message_ends() {
    QTcpServer listener;
    QVERIFY(listener.listen(QHostAddress::LocalHost, 0));
    const quint16 port = listener.serverPort();

    QTcpSocket client;
    client.connectToHost(QHostAddress::LocalHost, port);
    QVERIFY(client.waitForConnected(2000));
    QVERIFY(listener.waitForNewConnection(2000));
    QTcpSocket* serverSide = listener.nextPendingConnection();
    QVERIFY(serverSide != nullptr);

    // Four bytes (a partial message), then close.
    const char prefix[4] = {static_cast<char>(0x80), static_cast<char>(0x01),
                            static_cast<char>(0x00), static_cast<char>(0x01)};
    QCOMPARE(static_cast<int>(client.write(prefix, sizeof(prefix))), 4);
    QVERIFY(client.waitForBytesWritten(2000));
    client.disconnectFromHost();
    client.close();

    // The listener owns serverSide (Qt parent/child); keep it alive without
    // taking ownership here.
    auto dev = std::shared_ptr<QIODevice>(serverSide, [](QIODevice*) {});
    transport::TQIODeviceTransport trans(dev);

    uint8_t buf[8];
    bool threw = false;
    try {
      trans.readAll(buf, sizeof(buf)); // wants 8, peer sent 4 and left
    } catch (const transport::TTransportException& e) {
      threw = true;
      QCOMPARE(static_cast<int>(e.getType()),
               static_cast<int>(transport::TTransportException::END_OF_FILE));
    }
    QVERIFY(threw);
  }
};

#if (QT_VERSION >= QT_VERSION_CHECK(5, 0, 0))
QTEST_GUILESS_MAIN(TQIODeviceTransportTest);
#else
#undef QT_GUI_LIB
QTEST_MAIN(TQIODeviceTransportTest);
#endif
#include "TQIODeviceTransportTest.moc"
