QT       += core network

CONFIG += c++11

greaterThan(QT_MAJOR_VERSION, 4): QT -= widgets

TARGET = libthrift
TEMPLATE = lib
#CONFIG += staticlib

SOURCES += \
    src/thrift/TApplicationException.cpp \
    src/thrift/TOutput.cpp \
    src/thrift/transport/TBufferTransports.cpp \
    src/thrift/transport/TFDTransport.cpp \
    src/thrift/transport/TQtcpSocket.cpp \
    src/thrift/transport/TTransportException.cpp \
    src/thrift/transport/TTransportUtils.cpp \
    src/thrift/server/TConnectedClient.cpp \
    src/thrift/protocol/TBase64Utils.cpp \
    src/thrift/protocol/TBinaryProtocol.tcc \
    src/thrift/protocol/TDebugProtocol.cpp \
    src/thrift/protocol/TJSONProtocol.cpp \
    src/thrift/protocol/TMultiplexedProtocol.cpp \
    src/thrift/processor/PeekProcessor.cpp \
    src/thrift/concurrency/StdThreadFactory.cpp \
    src/thrift/concurrency/Util.cpp \
    src/thrift/async/TAsyncChannel.cpp \
    src/thrift/protocol/TProtocol.cpp \
    src/thrift/async/TConcurrentClientSyncInfo.cpp \
    src/thrift/protocol/TCompactProtocol.tcc \



HEADERS  += \
    src/thrift/TApplicationException.h \
    src/thrift/Thrift.h \
    src/thrift/TOutput.h \
    src/thrift/TProcessor.h \
    src/thrift/protocol/TProtocol.h \
    src/thrift/transport/TBufferTransports.h \
    src/thrift/transport/TFDTransport.h \
    src/thrift/transport/TQtcpSocket.h \
    src/thrift/transport/TServerTransport.h \
    src/thrift/transport/TTransport.h \
    src/thrift/transport/TTransportException.h \
    src/thrift/transport/TTransportUtils.h \
    src/thrift/transport/TVirtualTransport.h \
    src/thrift/server/TConnectedClient.h \
    src/thrift/server/TServer.h \
    src/thrift/protocol/TBase64Utils.h \
    src/thrift/protocol/TBinaryProtocol.h \
    src/thrift/protocol/TDebugProtocol.h \
    src/thrift/protocol/TJSONProtocol.h \
    src/thrift/protocol/TMultiplexedProtocol.h \
    src/thrift/protocol/TVirtualProtocol.h \
    src/thrift/processor/PeekProcessor.h \
    src/thrift/processor/TMultiplexedProcessor.h \
    src/thrift/concurrency/Exception.h \
    src/thrift/concurrency/PlatformThreadFactory.h \
    src/thrift/concurrency/StdThreadFactory.h \
    src/thrift/concurrency/Util.h \
    src/thrift/async/TAsyncChannel.h \
    src/thrift/async/TConcurrentClientSyncInfo.h \
    src/thrift/protocol/TCompactProtocol.h


win32 {
    SOURCES += \
    src/thrift/async/TAsyncChannel.cpp \
    src/thrift/concurrency/BoostMonitor.cpp \
    src/thrift/concurrency/BoostMutex.cpp \
    src/thrift/concurrency/BoostThreadFactory.cpp \
    src/thrift/concurrency/ThreadManager.cpp \
    src/thrift/concurrency/TimerManager.cpp \
    src/thrift/server/TServerFramework.cpp \
    src/thrift/server/TSimpleServer.cpp \
    src/thrift/server/TThreadedServer.cpp \
    src/thrift/server/TThreadPoolServer.cpp \
    src/thrift/windows/GetTimeOfDay.cpp \
    src/thrift/windows/OverlappedSubmissionThread.cpp \
    src/thrift/windows/SocketPair.cpp \
    src/thrift/windows/TWinsockSingleton.cpp \
    src/thrift/windows/WinFcntl.cpp \
    src/thrift/transport/TFileTransport.cpp \
    src/thrift/transport/THttpClient.cpp \
    src/thrift/transport/THttpServer.cpp \
    src/thrift/transport/THttpTransport.cpp \
    src/thrift/transport/TPipe.cpp \
    src/thrift/transport/TPipeServer.cpp \
    src/thrift/transport/TServerSocket.cpp \
    src/thrift/transport/TSimpleFileTransport.cpp \
    src/thrift/transport/TSocket.cpp \
    src/thrift/transport/TSSLSocket.cpp

    HEADERS  += \
    src/thrift/async/TAsyncChannel.h \
    src/thrift/concurrency/BoostThreadFactory.h \
    src/thrift/server/TServerFramework.h \
    src/thrift/server/TSimpleServer.h \
    src/thrift/server/TThreadedServer.h \
    src/thrift/server/TThreadPoolServer.h \
    src/thrift/windows/config.h \
    src/thrift/windows/GetTimeOfDay.h \
    src/thrift/windows/Operators.h \
    src/thrift/windows/OverlappedSubmissionThread.h \
    src/thrift/windows/SocketPair.h \
    src/thrift/windows/TWinsockSingleton.h \
    src/thrift/windows/WinFcntl.h \
    src/thrift/transport/TFileTransport.h \
    src/thrift/transport/THttpClient.h \
    src/thrift/transport/THttpServer.h \
    src/thrift/transport/THttpTransport.h \
    src/thrift/transport/TPipe.h \
    src/thrift/transport/TPipeServer.h \
    src/thrift/transport/TServerSocket.h \
    src/thrift/transport/TServerTransport.h \
    src/thrift/transport/TSimpleFileTransport.h \
    src/thrift/transport/TSocket.h \
    src/thrift/transport/TSSLSocket.h

    INCLUDEPATH += "D:\libs\OpenSSL-Win64_1.0.1i\include"

    LIBS += -LD:\libs\boost_1_58_0_X64_VS2013\lib64-msvc-12.0 -llibboost_thread-vc120-mt-gd-1_58 -llibboost_chrono-vc120-mt-gd-1_58
    LIBS += -LD:\libs\OpenSSL-Win64_1.0.1i\lib\VC -llibeay32MDd -lssleay32MTd

    CONFIG(debug, debug|release) {
        DESTDIR = ../cpp/DebugQtWin
        OBJECTS_DIR = ../cpp/DebugQtWin/.obj
    } else {
        DESTDIR = ../cpp/releaseQtWin
        OBJECTS_DIR = ../cpp/releaseQtWin/.obj
    }
}
android {
    #SOURCES +=
    HEADERS += src/thrift/thrift-config.h \
               src/thrift/concurrency/Mutex.h \
               src/thrift/concurrency/Monitor.h

    SOURCES += src/thrift/concurrency/Mutex.cpp \
               src/thrift/concurrency/Monitor.cpp

    INCLUDEPATH += "src/thrift/android"

    CONFIG(debug, debug|release) {
        DESTDIR = ../cpp/DebugQtAndroid
        OBJECTS_DIR = ../cpp/DebugQtAndroid/.obj
    } else {
        DESTDIR = ../cpp/releaseQtAndroid
        OBJECTS_DIR = ../cpp/releaseQtAndroid/.obj
    }
}

INCLUDEPATH += "src"
INCLUDEPATH += "D:\libs\boost_1_58_0_X64_VS2013"


CONFIG += mobility
MOBILITY = 

