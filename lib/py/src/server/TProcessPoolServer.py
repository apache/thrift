#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#


import logging
from multiprocessing import  Process, Value, Condition, reduction

from TServer import TServer
from thrift.transport.TTransport import TTransportException

class TProcessPoolServer(TServer):

    """
    Server with a fixed size pool of worker subprocesses which service requests.
    Note that if you need shared state between the handlers - it's up to you!
    Written by Dvir Volk, doat.com
    """

    def __init__(self, * args):
        TServer.__init__(self, *args)
        self.numWorkers = 10
        self.workers = []
        self.isRunning = Value('b', False)
        self.stopCondition = Condition()
        self.postForkCallback = None

    def setPostForkCallback(self, callback):
        if not callable(callback):
            raise TypeError("This is not a callback!")
        self.postForkCallback = callback

    def setNumWorkers(self, num):
        """Set the number of worker threads that should be created"""
        self.numWorkers = num

    def workerProcess(self):
        """Loop around getting clients from the shared queue and process them."""

        if self.postForkCallback:
            self.postForkCallback()

        while self.isRunning.value == True:
            try:
                client = self.serverTransport.accept()
                self.serveClient(client)
            except (KeyboardInterrupt, SystemExit):
                return 0
            except Exception, x:
                logging.exception(x)

    def serveClient(self, client):
        """Process input/output from a client for as long as possible"""
        itrans = self.inputTransportFactory.getTransport(client)
        otrans = self.outputTransportFactory.getTransport(client)
        iprot = self.inputProtocolFactory.getProtocol(itrans)
        oprot = self.outputProtocolFactory.getProtocol(otrans)

        try:
            while True:
                self.processor.process(iprot, oprot)
        except TTransportException, tx:
            pass
        except Exception, x:
            logging.exception(x)

        itrans.close()
        otrans.close()


    def serve(self):
        """Start a fixed number of worker threads and put client into a queue"""

        #this is a shared state that can tell the workers to exit when set as false
        self.isRunning.value = True

        #first bind and listen to the port
        self.serverTransport.listen()

        #fork the children
        for i in range(self.numWorkers):
            try:
                w = Process(target=self.workerProcess)
                w.daemon = True
                w.start()
                self.workers.append(w)
            except Exception, x:
                logging.exception(x)

        #wait until the condition is set by stop()

        while True:

            self.stopCondition.acquire()
            try:
                self.stopCondition.wait()
                break
            except (SystemExit, KeyboardInterrupt):
		break
            except Exception, x:
                logging.exception(x)

        self.isRunning.value = False

    def stop(self):
        self.isRunning.value = False
        self.stopCondition.acquire()
        self.stopCondition.notify()
        self.stopCondition.release()

