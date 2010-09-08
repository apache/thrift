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

package org.apache.thrift.transport;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;
import javax.security.sasl.AuthorizeCallback;
import javax.security.sasl.RealmCallback;
import javax.security.sasl.Sasl;
import javax.security.sasl.SaslException;

import org.apache.thrift.TProcessor;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.server.ServerTestBase;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TSimpleServer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import junit.framework.TestCase;

public class TestTSaslTransports extends TestCase {

  private static final Logger LOGGER = LoggerFactory.getLogger(TestTSaslTransports.class);

  private static final String HOST = "localhost";
  private static final String SERVICE = "thrift-test";
  private static final String PRINCIPAL = "thrift-test-principal";
  private static final String PASSWORD = "super secret password";
  private static final String REALM = "thrift-test-realm";

  private static final String UNWRAPPED_MECHANISM = "CRAM-MD5";
  private static final Map<String, String> UNWRAPPED_PROPS = null;

  private static final String WRAPPED_MECHANISM = "DIGEST-MD5";
  private static final Map<String, String> WRAPPED_PROPS = new HashMap<String, String>();

  static {
    WRAPPED_PROPS.put(Sasl.QOP, "auth-int");
    WRAPPED_PROPS.put("com.sun.security.sasl.digest.realm", REALM);
  }

  private static final String testMessage1 = "Hello, world! Also, four "
      + "score and seven years ago our fathers brought forth on this "
      + "continent a new nation, conceived in liberty, and dedicated to the "
      + "proposition that all men are created equal.";
  
  private static final String testMessage2 = "I have a dream that one day "
      + "this nation will rise up and live out the true meaning of its creed: "
      + "'We hold these truths to be self-evident, that all men are created equal.'";


  private static class TestSaslCallbackHandler implements CallbackHandler {
    @Override
    public void handle(Callback[] callbacks) throws IOException, UnsupportedCallbackException {
      for (Callback c : callbacks) {
        if (c instanceof NameCallback) {
          ((NameCallback) c).setName(PRINCIPAL);
        } else if (c instanceof PasswordCallback) {
          ((PasswordCallback) c).setPassword(PASSWORD.toCharArray());
        } else if (c instanceof AuthorizeCallback) {
          ((AuthorizeCallback) c).setAuthorized(true);
        } else if (c instanceof RealmCallback) {
          ((RealmCallback) c).setText(REALM);
        } else {
          throw new UnsupportedCallbackException(c);
        }
      }
    }
  }

  private void testSaslOpen(final String mechanism, final Map<String, String> props)
      throws SaslException, TTransportException {
    Thread serverThread = new Thread() {
      public void run() {
        try {
          TServerSocket serverSocket = new TServerSocket(ServerTestBase.PORT);
          TTransport serverTransport = serverSocket.accept();
          TTransport saslServerTransport = new TSaslServerTransport(mechanism, SERVICE, HOST,
              props, new TestSaslCallbackHandler(), serverTransport);

          saslServerTransport.open();

          byte[] inBuf = new byte[testMessage1.getBytes().length];
          // Deliberately read less than the full buffer to ensure
          // that TSaslTransport is correctly buffering reads. This
          // will fail for the WRAPPED test, if it doesn't work.
          saslServerTransport.readAll(inBuf, 0, 5);
          saslServerTransport.readAll(inBuf, 5, 10);
          saslServerTransport.readAll(inBuf, 15, inBuf.length - 15);
          LOGGER.debug("server got: {}", new String(inBuf));
          assertEquals(new String(inBuf), testMessage1);

          LOGGER.debug("server writing: {}", testMessage2);
          saslServerTransport.write(testMessage2.getBytes());
          saslServerTransport.flush();

          serverSocket.close();
          saslServerTransport.close();
        } catch (TTransportException e) {
          fail(e.toString());
        }
      }
    };
    serverThread.start();

    try {
      Thread.sleep(1000);
    } catch (InterruptedException e) {
      // Ah well.
    }

    TSocket clientSocket = new TSocket(HOST, ServerTestBase.PORT);
    TTransport saslClientTransport = new TSaslClientTransport(mechanism,
        PRINCIPAL, SERVICE, HOST, props, new TestSaslCallbackHandler(), clientSocket);
    saslClientTransport.open();
    LOGGER.debug("client writing: {}", testMessage1);
    saslClientTransport.write(testMessage1.getBytes());
    saslClientTransport.flush();

    byte[] inBuf = new byte[testMessage2.getBytes().length];
    saslClientTransport.readAll(inBuf, 0, inBuf.length);
    LOGGER.debug("client got: {}", new String(inBuf));
    assertEquals(new String(inBuf), testMessage2);

    TTransportException expectedException = null;
    try {
      saslClientTransport.open();
    } catch (TTransportException e) {
      expectedException = e;
    }
    assertNotNull(expectedException);

    saslClientTransport.close();

    try {
      serverThread.join();
    } catch (InterruptedException e) {
      // Ah well.
    }
  }

  public void testUnwrappedOpen() throws SaslException, TTransportException {
    testSaslOpen(UNWRAPPED_MECHANISM, UNWRAPPED_PROPS);
  }

  public void testWrappedOpen() throws SaslException, TTransportException {
    testSaslOpen(WRAPPED_MECHANISM, WRAPPED_PROPS);
  }

  public void testWithServer() throws Exception {
    new TestTSaslTransportsWithServer().testIt();
  }

  private static class TestTSaslTransportsWithServer extends ServerTestBase {

    private Thread serverThread;
    private TServer server;

    @Override
    public TTransport getClientTransport(TTransport underlyingTransport) throws Exception {
      return new TSaslClientTransport(WRAPPED_MECHANISM,
          PRINCIPAL, SERVICE, HOST, WRAPPED_PROPS, new TestSaslCallbackHandler(), underlyingTransport);
    }

    @Override
    public void startServer(final TProcessor processor, final TProtocolFactory protoFactory) throws Exception {
      serverThread = new Thread() {
        public void run() {
          try {
            // Transport
            TServerSocket socket = new TServerSocket(PORT);

            TTransportFactory factory = new TSaslServerTransport.Factory(WRAPPED_MECHANISM,
                SERVICE, HOST, WRAPPED_PROPS, new TestSaslCallbackHandler());
            server = new TSimpleServer(processor, socket, factory, protoFactory);

            // Run it
            LOGGER.debug("Starting the server on port {}", PORT);
            server.serve();
          } catch (Exception e) {
            e.printStackTrace();
            fail();
          }
        }
      };
      serverThread.start();
      Thread.sleep(1000);
    }

    @Override
    public void stopServer() throws Exception {
      server.stop();
      try {
        serverThread.join();
      } catch (InterruptedException e) {}
    }

  }

}
