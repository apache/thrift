/*
 * Copyright 2019-present Facebook, Inc.
 *
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
package org.apache.thrift.test;

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.THeaderProtocol;
import org.apache.thrift.protocol.TMessage;
import org.apache.thrift.transport.THeaderException;
import org.apache.thrift.transport.THeaderTransport;
import org.apache.thrift.transport.TMemoryBuffer;
import org.apache.thrift.transport.TTransportException;
import org.junit.Test;
import thrift.test.Xtruct;

import java.io.IOException;
import java.util.HashMap;

import static org.junit.Assert.*;

public class TestTHeader {

    @Test
    public void testShouldWriteReadHeaders() throws TException {
        TMemoryBuffer buf = new TMemoryBuffer(200);
        THeaderTransport trans = new THeaderTransport(buf);
        TBinaryProtocol prot = new TBinaryProtocol(trans);
        Xtruct out = new Xtruct();

        trans.setHeader("test1", "value1");
        trans.setHeader("test2", "value2");
        trans.setProtocolId(THeaderTransport.T_BINARY_PROTOCOL);
        //trans.wri
        out.write(prot);
        trans.flush();

        Xtruct in = new Xtruct();
        in.read(prot);
        HashMap<String, String> headers = trans.getHeaders();
        assertEquals(2, headers.size());
        assertTrue(headers.containsKey("test1"));
        assertTrue(headers.containsKey("test2"));
        assertEquals("value1", headers.get("test1"));
        assertEquals("value2", headers.get("test2"));
    }

    private void testTransform(THeaderTransport.Transforms transform) throws TException {
        TMemoryBuffer buf = new TMemoryBuffer(200);
        THeaderTransport writer = new THeaderTransport(buf);
        writer.addTransform(transform);
        writer.setHeader("test1", "value1");
        String frost = "Whose woods these are I think I know";
        byte[] testBytes = frost.getBytes();
        writer.write(testBytes, 0, testBytes.length);
        writer.flush();

        THeaderTransport reader = new THeaderTransport(buf);
        byte[] receivedBytes = new byte[testBytes.length];
        reader.read(receivedBytes, 0, receivedBytes.length);
        assertArrayEquals(testBytes, receivedBytes);
        assertTrue(reader.getHeaders().containsKey("test1"));
    }

    @Test
    public void testZlibTransform() throws TException {
        testTransform(THeaderTransport.Transforms.ZLIB_TRANSFORM);
    }

    @Test
    public void testSnappyTransform() throws TException {
        try {
            testTransform(THeaderTransport.Transforms.SNAPPY_TRANSFORM);
            fail("expects THeaderException");
        } catch (THeaderException e) {
            // expected
        }
    }

    // converted to bytes from https://github.com/apache/thrift/pull/1743/files#diff-f2f546561d93f14754d666df0558dac1
    private byte[] buf = {0, 0, 0, 72, 15, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 1, 2, 6, 80, 97, 114, 101, 110, 116, 7, 115, 104, 111, 111, 98, 97, 114, 5, 84, 114, 97, 99, 101, 5, 97, 98, 99, 100, 101, 0, -128, 1, 0, 1, 0, 0, 0, 3, 97, 100, 100, 0, 0, 0, 0, 8, 0, 1, 0, 0, 0, 1, 8, 0, 2, 0, 0, 0, 1, 0};

    @Test
    public void testShouldReadHeadersFromPayload() throws IOException, TTransportException {
        TMemoryBuffer membuf = new TMemoryBuffer(200);
        membuf.write(buf);
        membuf.flush();

        THeaderTransport inTrans = new THeaderTransport(membuf);
        byte[] receivedBytes = new byte[40];
        inTrans.read(receivedBytes, 0, buf.length);
        inTrans.flush();

        assertTrue(inTrans.getHeaders().get("Parent").equals("shoobar") && inTrans.getHeaders().get("Trace").equals("abcde"));
    }

    @Test
    public void testShouldReadHeadersWhenReadingMessageBegin() throws IOException, TException {
        TMemoryBuffer membuf = new TMemoryBuffer(200);
        membuf.write(buf);
        membuf.flush();

        THeaderTransport inTrans = new THeaderTransport(membuf);
        THeaderProtocol protocol = new THeaderProtocol(inTrans);
        TMessage message = protocol.readMessageBegin();
        assertEquals(message.name, "add");
        assertEquals(inTrans.getProtocolId(), THeaderTransport.T_BINARY_PROTOCOL);
    }
}
