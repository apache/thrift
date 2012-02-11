#ifndef _THRIFT_TASYNC_QTCP_SERVER_H_
#define _THRIFT_TASYNC_QTCP_SERVER_H_

#include <QObject>
#include <QTcpServer>

#include <boost/shared_ptr.hpp>

namespace apache { namespace thrift { namespace protocol {
class TProtocolFactory;
}}} // apache::thrift::protocol

namespace apache { namespace thrift { namespace async {

class TAsyncProcessor;

/**
 *  Server that uses Qt to listen for connections.
 *  Simply give it a QTcpServer that is listening, along with an async
 *  processor and a protocol factory, and then run the Qt event loop.
 */
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
  TQTcpServer(const TQTcpServer&);
  TQTcpServer& operator=(const TQTcpServer&);
  
  class ConnectionContext;

  void finish(boost::shared_ptr<ConnectionContext> ctx, bool healthy);

  boost::shared_ptr<QTcpServer> server_;
  boost::shared_ptr<TAsyncProcessor> processor_;
  boost::shared_ptr<apache::thrift::protocol::TProtocolFactory> pfact_;

  std::map<QT_PREPEND_NAMESPACE(QTcpSocket)*, boost::shared_ptr<ConnectionContext> > ctxMap_;
};

}}} // apache::thrift::async

#endif // #ifndef _THRIFT_TASYNC_QTCP_SERVER_H_
