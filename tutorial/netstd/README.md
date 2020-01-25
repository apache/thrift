# Building of samples for different platforms 

# Requirements
- NET Core Standard 3.1 (LTS) runtime or SDK (see below for further info)

# How to build
- Download and install the latest .NET Core SDK for your platform https://dotnet.microsoft.com/download/dotnet-core
- Ensure that you have thrift.exe which supports netstd lib and it added to PATH 
- Go to current folder 
- Run **build.sh** or **build.cmd** from the root of cloned repository
- Check tests in **src/Tests** folder
- Continue with /tutorials/netstd 

# How to run 

Depending on the platform, the name of the generated executables will vary. On Linux, it is just "Client" or "Server", on Windows it is "Client.exe" and "Server.exe". In the following, we use the abbreviated form "Client" and "Server".

- build 
- go to folder (Client/Server) 
- run the generated executables: server first, then client from a second console

# Known issues
- In trace logging mode you can see some not important internal exceptions

# Running of samples 
On machines that do not have the SDK installed, you need to install the NET Core runtime first. The SDK is only needed to build programs, otherwise the runtime is sufficient.

# NetCore Server

Usage: 

    Server -help
        will diplay help information 

    Server -tr:<transport> -pr:<protocol>
        will run server with specified arguments (tcp transport and binary protocol by default)

Options:

    -tr (transport): 
        tcp - (default) tcp transport will be used (host - ""localhost"", port - 9090)
        namedpipe - namedpipe transport will be used (pipe address - "".test"")
        http - http transport will be used (http address - ""localhost:9090"")
        tcptls - tcp transport with tls will be used (host - ""localhost"", port - 9090)

    -bf (buffering): 
        none - (default) no transport factory will be used
        buffered - buffered transport factory will be used
        framed - framed transport factory will be used (this must match the client)

    -pr (protocol): 
        binary - (default) binary protocol will be used
        compact - compact protocol will be used
        json - json protocol will be used
        multiplexed - multiplexed protocol will be used

Sample:

    Server -tr:tcp

**Remarks**:

    For TcpTls mode certificate's file ThriftTest.pfx should be in directory with binaries in case of command line usage (or at project level in case of debugging from IDE).
    Password for certificate - "ThriftTest".



# NetCore Client

Usage: 

    Client -help
        will diplay help information 

    Client -tr:<transport> -pr:<protocol> -mc:<numClients>
        will run client with specified arguments (tcp transport and binary protocol by default)

Options:

    -tr (transport): 
        tcp - (default) tcp transport will be used (host - ""localhost"", port - 9090)
        namedpipe - namedpipe transport will be used (pipe address - "".test"")
        http - http transport will be used (address - ""http://localhost:9090"")        
        tcptls - tcp tls transport will be used (host - ""localhost"", port - 9090)

    -bf (buffering): 
        none - (default) no transport factory will be used
        buffered - buffered transport factory will be used
        framed - framed transport factory will be used (this must match the client)

    -pr (protocol): 
        binary - (default) binary protocol will be used
        compact - compact protocol will be used
        json - json protocol will be used
        multiplexed - multiplexed protocol will be used

    -mc (multiple clients):
        <numClients> - number of multiple clients to connect to server (max 100, default 1)

Sample:

    Client -tr:tcp -pr:binary -mc:10

Remarks:

    For TcpTls mode certificate's file ThriftTest.pfx should be in directory 
	with binaries in case of command line usage (or at project level in case of debugging from IDE).
    Password for certificate - "ThriftTest".

# How to test communication between NetCore and Python

* Generate code with the latest **thrift** utility
* Ensure that **thrift** generated folder **gen-py** with generated code for Python exists
* Create **client.py** and **server.py** from the code examples below and save them to the folder with previosly generated folder **gen-py**
* Run netstd samples (client and server) and python samples (client and server)

Remarks:

Samples of client and server code below use correct methods (operations) 
and fields (properties) according to generated contracts from *.thrift files

At Windows 10 add record **127.0.0.1 testserver** to **C:\Windows\System32\drivers\etc\hosts** file
for correct work of python server


**Python Client:**
	
```python
import sys
import glob
sys.path.append('gen-py')

from tutorial import Calculator
from tutorial.ttypes import InvalidOperation, Operation, Work

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol


def main():
    # Make socket
    transport = TSocket.TSocket('127.0.0.1', 9090)

    # Buffering is critical. Raw sockets are very slow
    transport = TTransport.TBufferedTransport(transport)

    # Wrap in a protocol
    protocol = TBinaryProtocol.TBinaryProtocol(transport)

    # Create a client to use the protocol encoder
    client = Calculator.Client(protocol)

    # Connect!
    transport.open()

    client.Ping()
    print('ping()')

    sum = client.Add(1, 1)
    print(('1+1=%d' % (sum)))

    work = Work()

    work.Op = Operation.Divide
    work.Num1 = 1
    work.Num2 = 0

    try:
        quotient = client.Calculate(1, work)
        print('Whoa? You know how to divide by zero?')
        print('FYI the answer is %d' % quotient)
    except InvalidOperation as e:
        print(('InvalidOperation: %r' % e))

    work.Op = Operation.Substract
    work.Num1 = 15
    work.Num2 = 10

    diff = client.Calculate(1, work)
    print(('15-10=%d' % (diff)))

    log = client.GetStruct(1)
    print(('Check log: %s' % (log.Value)))

    client.Zip()
    print('zip()')

    # Close!
    transport.close()

if __name__ == '__main__':
  try:
    main()
  except Thrift.TException as tx:
    print('%s' % tx.message)
```


**Python Server:**


```python
import glob
import sys
sys.path.append('gen-py')

from tutorial import Calculator
from tutorial.ttypes import InvalidOperation, Operation

from shared.ttypes import SharedStruct

from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol
from thrift.server import TServer


class CalculatorHandler:
    def __init__(self):
        self.log = {}

    def Ping(self):
        print('ping()')

    def Add(self, n1, n2):
        print('add(%d,%d)' % (n1, n2))
        return n1 + n2

    def Calculate(self, logid, work):
        print('calculate(%d, %r)' % (logid, work))

        if work.Op == Operation.Add:
            val = work.Num1 + work.Num2
        elif work.Op == Operation.Substract:
            val = work.Num1 - work.Num2
        elif work.Op == Operation.Multiply:
            val = work.Num1 * work.Num2
        elif work.Op == Operation.Divide:
            if work.Num2 == 0:
                raise InvalidOperation(work.Op, 'Cannot divide by 0')
            val = work.Num1 / work.Num2
        else:
            raise InvalidOperation(work.Op, 'Invalid operation')

        log = SharedStruct()
        log.Key = logid
        log.Value = '%d' % (val)
        self.log[logid] = log

        return val

    def GetStruct(self, key):
        print('getStruct(%d)' % (key))
        return self.log[key]

    def Zip(self):
        print('zip()')

if __name__ == '__main__':
    handler = CalculatorHandler()
    processor = Calculator.Processor(handler)
    transport = TSocket.TServerSocket(host="testserver", port=9090)
    tfactory = TTransport.TBufferedTransportFactory()
    pfactory = TBinaryProtocol.TBinaryProtocolFactory()

    server = TServer.TSimpleServer(processor, transport, tfactory, pfactory)
    print('Starting the server...')
    server.serve()
    print('done.')

    # You could do one of these for a multithreaded server
    # server = TServer.TThreadedServer(processor, transport, tfactory, pfactory)
    # server = TServer.TThreadPoolServer(processor, transport, tfactory, pfactory)
```
