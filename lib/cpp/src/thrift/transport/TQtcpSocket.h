#pragma once
#include <QtNetwork/QTcpSocket>
#include <QtNetwork/QHostAddress>
#include <string>

#include <thrift/transport/TTransport.h>
#include <thrift/transport/TVirtualTransport.h>
#include <thrift/transport/TServerSocket.h>
#include <thrift/transport/PlatformSocket.h>
namespace apache {
namespace thrift {
namespace transport {
class TQtcpSocket : public TVirtualTransport<TQtcpSocket>
{
public:
    /**
        * Constructs a new socket. Note that this does NOT actually connect the
        * socket.
        *
        */
    TQtcpSocket();
    /**
    * Constructs a new socket. Note that this does NOT actually connect the
    * socket.
    *
    * @param host An IP address or hostname to connect to
    * @param port The port to connect on
    */
    TQtcpSocket(const std::string& host, int port);
    /**
    * Constructs a new Unix domain socket.
    * Note that this does NOT actually connect the socket.
    *
    * @param path The Unix domain socket e.g. "/tmp/ThriftTest.binary.thrift"
    */
    TQtcpSocket(const std::string& path);

    /**
    * Destroyes the socket object, closing it if necessary.
    */
    virtual ~TQtcpSocket();
    /**
    * Whether the socket is alive.
    *
    * @return Is the socket alive?
    */
    virtual bool isOpen();

    /**
    * Calls select on the socket to see if there is more data available.
    */
    virtual bool peek();

    /**
    * Creates and opens the UNIX socket.
    *
    * @throws TTransportException If the socket could not connect
    */
    virtual void open();

    /**
    * Shuts down communications on the socket.
    */
    virtual void close();

    /**
    * Reads from the underlying socket.
    * \returns the number of bytes read or 0 indicates EOF
    * \throws TTransportException of types:
    *           INTERRUPTED means the socket was interrupted
    *                       out of a blocking call
    *           NOT_OPEN means the socket has been closed
    *           TIMED_OUT means the receive timeout expired
    *           UNKNOWN means something unexpected happened
    */
    virtual uint32_t read(uint8_t* buf, uint32_t len);

    /**
    * Writes to the underlying socket.  Loops until done or fail.
    */
    virtual void write(const uint8_t* buf, uint32_t len);

    /**
    * Writes to the underlying socket.  Does single send() and returns result.
    */
    uint32_t write_partial(const uint8_t* buf, uint32_t len);

    /**
    * Get the host that the socket is connected to
    *
    * @return string host identifier
    */
    std::string getHost();

    /**
    * Get the port that the socket is connected to
    *
    * @return int port number
    */
    int getPort();

    /**
    * Set the host that socket will connect to
    *
    * @param host host identifier
    */
    void setHost(std::string host);

    /**
    * Set the port that socket will connect to
    *
    * @param port port number
    */
    void setPort(int port);

    /**
    * Whether to enable/disable Nagle's algorithm.
    *
    * @param noDelay Whether or not to disable the algorithm.
    * @return
    */
    void setNoDelay(bool noDelay);

    /**
    * Set the connect timeout
    */
    void setConnTimeout(int ms);

    /**
    * Set the receive timeout
    */
    void setRecvTimeout(int ms);

    /**
    * Set the send timeout
    */
    void setSendTimeout(int ms);

    /**
    * Set SO_KEEPALIVE
    */
    void setKeepAlive(bool keepAlive);

    /**
    * Get socket information formatted as a string <Host: x Port: x>
    */
    std::string getSocketInfo();

    /**
    * Returns the DNS name of the host to which the socket is connected
    */
    std::string getPeerHost();

    /**
    * Returns the address of the host to which the socket is connected
    */
    std::string getPeerAddress();

    /**
    * Returns the port of the host to which the socket is connected
    **/
    int getPeerPort();

    /**
    * Sets whether to use a low minimum TCP retransmission timeout.
    */
    static void setUseLowMinRto(bool useLowMinRto);

    /**
    * Gets whether to use a low minimum TCP retransmission timeout.
    */
    static bool getUseLowMinRto();

    /**
    * Get the origin the socket is connected to
    *
    * @return string peer host identifier and port
    */
    virtual const std::string getOrigin();

protected:
    std::string getSocketError(int *errorNumber);
    void openConnection();
    /** Host to connect to */
    std::string host_;

    /** Peer hostname */
    std::string peerHost_;

    /** Peer address */
    std::string peerAddress_;

    /** Peer port */
    int peerPort_;

    /** Port number to connect on */
    int port_;

    /** UNIX domain socket path */
    std::string path_;

    /** Connect timeout in ms */
    int connTimeout_;

    /** Send timeout in ms */
    int sendTimeout_;

    /** Recv timeout in ms */
    int recvTimeout_;

    /** Keep alive on */
    bool keepAlive_;

    /** Linger on */
    bool lingerOn_;

    /** Linger val */
    int lingerVal_;

    /** Nodelay */
    bool noDelay_;

    /** Whether to use low minimum TCP retransmission timeout */
    static bool useLowMinRto_;
    QTcpSocket *tcpSocket;
    void unix_open();
};
}
}
}
