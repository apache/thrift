import sys
import time
# import pdb
# sys.path.append('./gen-py')

from ThriftTest import ThriftTest
from ThriftTest.ttypes import *

from thrift.transport import TTransport
from thrift.protocol import TJSONProtocol
from thrift.server import TServer

# This version of TestHandler never prints anything since stdout is redirected
# when it is used.
class TestHandler:

    def testVoid(self):
        return

    def testString(self, str):
        return str

    def testByte(self, byte):
        return byte

    def testI16(self, i16):
        return i16

    def testI32(self, i32):
        return i32

    def testI64(self, i64):
        return i64

    def testDouble(self, dub):
        return dub

    def testBinary(self, thing):
        return thring

    def testStruct(self, thing):
        return thing

    def testException(self, arg):
        if arg == 'Xception':
            raise Xception(errorCode=1001, message=arg)
        elif arg == 'TException':
            raise TException(message='This is a TException')

    def testMultiException(self, arg0, arg1):
        if arg0 == 'Xception':
            raise Xception(errorCode=1001, message='This is an Xception')
        elif arg0 == 'Xception2':
            raise Xception2(
            errorCode=2002,
            struct_thing=Xtruct(string_thing='This is an Xception2'))
        return Xtruct(string_thing=arg1)

    def testOneway(self, seconds):
        time.sleep(seconds / 3) # be quick

    def testNest(self, thing):
        return thing

    def testMap(self, thing):
        return thing

    def testSet(self, thing):
        return thing

    def testList(self, thing):
        return thing

    def testEnum(self, thing):
        return thing

    def testTypedef(self, thing):
        return thing

    def testMapMap(self, thing):
        return {thing: {thing: thing}}

    def testInsanity(self, argument):
        return {123489: {Numberz.ONE:argument}}

    def testMulti(self, arg0, arg1, arg2, arg3, arg4, arg5):
        return Xtruct(string_thing='Hello2',
                  byte_thing=arg0, i32_thing=arg1, i64_thing=arg2)

# def fixed_pdb(Pdb=pdb.Pdb):
#     '''make Pdb() tied to original stdout'''
#     return Pdb(stdout=sys.stderr)
#
# #if __name__ == '__main__':
# pdb.Pdb = fixed_pdb
# pdb.main()

# print >> sys.stderr, 'server says hi'
# if __name__ == "__main__":
handler = TestHandler()
processor = ThriftTest.Processor(handler)
transport = TTransport.TIOStreamTransport(sys.stdin, sys.stdout)
server = TServer.TStreamServer(processor, transport,
         TTransport.TTransportFactoryBase(),
         TJSONProtocol.TJSONProtocolFactory())

server.serve()
