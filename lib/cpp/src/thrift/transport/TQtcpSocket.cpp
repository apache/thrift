#include "TQtcpSocket.h"
namespace apache {
namespace thrift {
namespace transport {

using namespace std;
/**
            * TQtcpSocket implementation.
            *
            */
TQtcpSocket::TQtcpSocket(const string& host, int port)
    : host_(host),
      port_(port),
      path_(""),
      connTimeout_(1000),
      sendTimeout_(1000),
      recvTimeout_(1000),
      keepAlive_(false),
      lingerOn_(1),
      lingerVal_(0),
      noDelay_(1),
      tcpSocket(new QTcpSocket()){
}

TQtcpSocket::TQtcpSocket(const string& path)
    : host_(""),
      port_(0),
      path_(path),
      connTimeout_(1000),
      sendTimeout_(1000),
      recvTimeout_(1000),
      keepAlive_(false),
      lingerOn_(1),
      lingerVal_(0),
      noDelay_(1),
      tcpSocket(new QTcpSocket()){
}

TQtcpSocket::TQtcpSocket()
    : host_(""),
      port_(0),
      path_(""),
      connTimeout_(1000),
      sendTimeout_(1000),
      recvTimeout_(1000),
      keepAlive_(false),
      lingerOn_(1),
      lingerVal_(0),
      tcpSocket(new QTcpSocket()),
      noDelay_(1){
}


TQtcpSocket::~TQtcpSocket()
{
    close();
}

bool TQtcpSocket::isOpen() {
    return tcpSocket->isOpen();
}
bool TQtcpSocket::peek() {
    return !tcpSocket->peek(1).isEmpty();
}
void TQtcpSocket::open(){
    if (isOpen()) {
        return;
    }
    unix_open();
}
void TQtcpSocket::close(){
    tcpSocket->close();
}

uint32_t TQtcpSocket::read(uint8_t* buf, uint32_t len){
    tcpSocket->waitForReadyRead(recvTimeout_);
    int i = tcpSocket->read((char*)buf, len);
    if(i < 0){
        std::string errorString = "";
        int error;
        errorString = getSocketError(&error);
        throw TTransportException(TTransportException::END_OF_FILE, "read() failed " + errorString, error);
    }
    return i;
}

void TQtcpSocket::write(const uint8_t* buf, uint32_t len){
    uint32_t sent = 0;
    while (sent < len) {
        uint32_t b = write_partial(buf + sent, len - sent);
        sent += b;
    }
    return;
}
uint32_t TQtcpSocket::write_partial(const uint8_t* buf, uint32_t len){
    if (!isOpen()) {
        throw TTransportException(TTransportException::NOT_OPEN, "Called write on non-open socket");
    }

    uint32_t sent = 0;
    bool writtenWait = false;
    int written = tcpSocket->write((char*)(buf + sent), len - sent);
    if( (written < 0) || (written == 0)){
        std::string errorString = "";
        int error;
        errorString = getSocketError(&error);
        close();
        throw TTransportException(TTransportException::NOT_OPEN, "TSocket::write_partial() write()" + errorString, error);
    }
    writtenWait = tcpSocket->waitForBytesWritten(sendTimeout_);
    if( !writtenWait){
        std::string errorString = "";
        int error;
        errorString = getSocketError(&error);
        close();
        throw TTransportException(TTransportException::TIMED_OUT, "TSocket::write_partial() waitForBytesWritten()" + errorString, error);
    }
    return written;
}
std::string  TQtcpSocket::getHost(){
    return host_;
}
int TQtcpSocket::getPort(){
    return port_;
}
void TQtcpSocket::setHost(std::string host){
    host_ = host;
}

void TQtcpSocket::setPort(int port){
    port_ = port;
}
void TQtcpSocket::setNoDelay(bool noDelay){
    tcpSocket->setSocketOption(QAbstractSocket::LowDelayOption, noDelay);
    std::string errorString = "";
    int error;
    errorString = getSocketError(&error);
    if(errorString != "")
        throw TTransportException(TTransportException::UNKNOWN, "setNoDelay() failed " + errorString, error);
}
void TQtcpSocket::setConnTimeout(int ms){
    connTimeout_ = ms;
}
void TQtcpSocket::setRecvTimeout(int ms){
    recvTimeout_ = ms;
}
void TQtcpSocket::setSendTimeout(int ms){
    sendTimeout_ = ms;
}
void TQtcpSocket::setKeepAlive(bool keepAlive){
    tcpSocket->setSocketOption(QAbstractSocket::KeepAliveOption, keepAlive);
}
std::string TQtcpSocket::getSocketInfo(){
    std::string oss;
    if (host_.empty() || port_ == 0) {
        oss += "<Host: " + getPeerAddress();
        oss += " Port: " + QString::number(getPeerPort()).toStdString()/*std::to_string(getPeerPort())*/ + ">";
    }
    else {
        oss += "<Host: " + host_ + " Port: " + QString::number(port_).toStdString()/*std::to_string(port_)*/ + ">";
    }
    return oss;
}
std::string TQtcpSocket::getPeerHost(){
    peerHost_ = tcpSocket->peerName().toStdString();
    return peerHost_;
}
std::string TQtcpSocket::getPeerAddress(){
    QHostAddress hostAddr = tcpSocket->peerAddress();
    peerAddress_ = hostAddr.toString().toStdString();
    peerPort_ = tcpSocket->peerPort();
    return peerAddress_;
}
int TQtcpSocket::getPeerPort(){
    getPeerAddress();
    return peerPort_;
}
const std::string TQtcpSocket::getOrigin(){
    return getPeerHost() + ":" + QString::number(getPeerPort()).toStdString()/*std::to_string(getPeerPort())*/;
}
void TQtcpSocket::unix_open(){
    openConnection();

}
void TQtcpSocket::openConnection(){
    std::string errorString = "";
    int error;
    QString host = QString::fromStdString(host_);
    tcpSocket->connectToHost(QString::fromStdString(host_), port_);
    if (!tcpSocket->waitForConnected(connTimeout_)){
        errorString = getSocketError(&error);
        throw TTransportException(TTransportException::NOT_OPEN, "connect() failed " + errorString, error);
    }
}
    std::string TQtcpSocket::getSocketError(int *errorNumber){
        std::string errorString = "";
        QAbstractSocket::SocketError error = tcpSocket->error();
        *errorNumber = (int)error;
        switch (error)
        {
        case QAbstractSocket::ConnectionRefusedError:
            errorString = "ConnectionRefusedError";
            break;
        case QAbstractSocket::RemoteHostClosedError:
            errorString = "RemoteHostClosedError";
            break;
        case QAbstractSocket::HostNotFoundError:
            errorString = "HostNotFoundError";
            break;
        case QAbstractSocket::SocketAccessError:
            errorString = "SocketAccessError";
            break;
        case QAbstractSocket::SocketResourceError:
            errorString = "SocketResourceError";
            break;
        case QAbstractSocket::SocketTimeoutError:
            errorString = "SocketTimeoutError";
            break;
        case QAbstractSocket::DatagramTooLargeError:
            errorString = "DatagramTooLargeError";
            break;
        case QAbstractSocket::NetworkError:
            errorString = "NetworkError";
            break;
        case QAbstractSocket::AddressInUseError:
            errorString = "AddressInUseError";
            break;
        case QAbstractSocket::SocketAddressNotAvailableError:
            errorString = "SocketAddressNotAvailableError";
            break;
        case QAbstractSocket::UnsupportedSocketOperationError:
            errorString = "UnsupportedSocketOperationError";
            break;
        case QAbstractSocket::UnfinishedSocketOperationError:
            errorString = "UnfinishedSocketOperationError";
            break;
        case QAbstractSocket::ProxyAuthenticationRequiredError:
            errorString = "ProxyAuthenticationRequiredError";
            break;
        case QAbstractSocket::SslHandshakeFailedError:
            errorString = "SslHandshakeFailedError";
            break;
        case QAbstractSocket::ProxyConnectionRefusedError:
            errorString = "ProxyConnectionRefusedError";
            break;
        case QAbstractSocket::ProxyConnectionClosedError:
            errorString = "ProxyConnectionClosedError";
            break;
        case QAbstractSocket::ProxyConnectionTimeoutError:
            errorString = "ProxyConnectionTimeoutError";
            break;
        case QAbstractSocket::ProxyNotFoundError:
            errorString = "ProxyNotFoundError";
            break;
        case QAbstractSocket::ProxyProtocolError:
            errorString = "ProxyProtocolError";
            break;
        case QAbstractSocket::OperationError:
            errorString = "OperationError";
            break;
        case QAbstractSocket::SslInternalError:
            errorString = "SslInternalError";
            break;
        case QAbstractSocket::SslInvalidUserDataError:
            errorString = "SslInvalidUserDataError";
            break;
        case QAbstractSocket::TemporaryError:
            errorString = "TemporaryError";
            break;
        case QAbstractSocket::UnknownSocketError:
            errorString = "UnknownSocketError";
            break;
        default:
            break;
        }
        return errorString;
}
}
}
}
