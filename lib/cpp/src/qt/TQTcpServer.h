#ifndef _THRIFT_TASYNC_QTCP_SERVER_H_
#define _THRIFT_TASYNC_QTCP_SERVER_H_

#include <QObject>
#include <QTcpServer>

#include <boost/shared_ptr.hpp>

#include <tr1/functional>

namespace apache { namespace thrift { namespace protocol {
class TProtocol;
class TProtocolFactory;
}}} // apache::thrift::protocol

namespace apache { namespace thrift { namespace async {

class TAsyncProcessor;

class TQTcpServer : public QObject {
 Q_OBJECT
 public:
  TQTcpServer(boost::shared_ptr<QTcpServer> server,
              boost::shared_ptr<TAsyncProcessor> processor,
              boost::shared_ptr<apache::thrift::protocol::TProtocolFactory> protocolFactory,
              QT_PREPEND_NAMESPACE(QObject)* parent = NULL);
  virtual ~TQTcpServer();

 private Q_SLOTS:
  void processIncoming();
  void beginDecode();
  void socketClosed();

 private:
  class ConnectionContext;

  void finish(boost::shared_ptr<ConnectionContext> ctx, bool healthy);

  boost::shared_ptr<QTcpServer> server_;
  boost::shared_ptr<TAsyncProcessor> processor_;
  boost::shared_ptr<apache::thrift::protocol::TProtocolFactory> pfact_;

  std::map<QT_PREPEND_NAMESPACE(QTcpSocket)*, boost::shared_ptr<ConnectionContext> > ctxMap_;
};

}}} // apache::thrift::async

#endif // #ifndef _THRIFT_TASYNC_QTCP_SERVER_H_
