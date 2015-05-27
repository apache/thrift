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

import java.util.zip.Deflater;
import java.util.zip.Inflater;
import org.apache.thrift.TByteArrayOutputStream;
import org.apache.thrift.transport.TMemoryInputTransport;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;
import org.apache.thrift.transport.TTransportFactory;

/**
 * TZlibTransport deflates on write and inflates on read.
 */
public class TZlibTransport extends TTransport {
    //Class constants
    public static final int INFLATE_BUF_SIZE = 1024;
    public static final int READ_BUF_SIZE = 1024;
    public static final int INIT_WRITE_BUF_SIZE = 1024;
    //Client rw buffers and underlying transport
    private TByteArrayOutputStream writeBuffer_ = new TByteArrayOutputStream(INIT_WRITE_BUF_SIZE);
    private TMemoryInputTransport readBuffer_ = new TMemoryInputTransport(new byte[0]);
    private TTransport transport_ = null;
    //Zip objects and buffers
    private byte[] inflateBuf = new byte[INFLATE_BUF_SIZE];
    private byte[] readBuf = new byte[READ_BUF_SIZE];
    private Inflater decompresser = new Inflater(false);
    private Deflater compresser = new Deflater(Deflater.BEST_COMPRESSION, false);

    public static class Factory extends TTransportFactory {
        public Factory() {
        }

        @Override
        public TTransport getTransport(TTransport base) {
            return new TZlibTransport(base);
        }
    }

    /**
     * Constructs a new TZlibTransport instance.
     * @param  transport the underlying transport to read from and write to
     */
    public TZlibTransport(TTransport transport) {
        transport_ = transport;
    }

    /**
     * Constructs a new TZlibTransport instance.
     * @param  transport the underlying transport to read from and write to
     * @param  compressionLevel 0 for no compression, 9 for maximum compression
     */
    public TZlibTransport(TTransport transport, int compressionLevel) {
        transport_ = transport;
        compresser = new Deflater(compressionLevel, false);
    }

    @Override
    public void open() throws TTransportException {
        transport_.open();
    }

    @Override
    public boolean isOpen() {
        return transport_.isOpen();
    }

    @Override
    public void close() {
        readBuffer_.reset(new byte[0]);
        writeBuffer_.reset();
        compresser.reset();
        decompresser.reset();
        transport_.close();
    }

    @Override
    public int read(byte[] buf, int off, int len) throws TTransportException {
        int bytesRead = readBuffer_.read(buf, off, len);
        if (bytesRead > 0) {
            return bytesRead;
        }

        while (true) {
            if (readComp() > 0) {
                break;
            }
        }

        return readBuffer_.read(buf, off, len);
    }

    private int readComp() throws TTransportException {
        //If low level read buffer is exhausted, read more bytes from underlying transport
        if (decompresser.needsInput()) {
            int bytesRead = transport_.read(readBuf, 0, READ_BUF_SIZE);
            decompresser.setInput(readBuf, 0, bytesRead);
        }
        //Decompress bytes into high level client read buffer
        try {
            int InflatedBytes = decompresser.inflate(inflateBuf);
            if (InflatedBytes <= 0) {
                return 0;
            }

            byte[] old = new byte[readBuffer_.getBytesRemainingInBuffer()];
            readBuffer_.read(old, 0, readBuffer_.getBytesRemainingInBuffer());
            byte[] all = new byte[old.length + InflatedBytes];
            System.arraycopy(old, 0, all, 0, old.length);
            System.arraycopy(inflateBuf, 0, all, old.length, InflatedBytes);

            readBuffer_.reset(all);
            return all.length;
        } catch (java.util.zip.DataFormatException ex) {
            throw new TTransportException(ex);
        }
    }

    @Override
    public byte[] getBuffer() {
        return readBuffer_.getBuffer();
    }

    @Override
    public int getBufferPosition() {
        return readBuffer_.getBufferPosition();
    }

    @Override
    public int getBytesRemainingInBuffer() {
        return readBuffer_.getBytesRemainingInBuffer();
    }

    @Override
    public void consumeBuffer(int len) {
        readBuffer_.consumeBuffer(len);
    }

    @Override
    public void write(byte[] buf, int off, int len) throws TTransportException {
        writeBuffer_.write(buf, off, len);
    }

    /**
     * Compress write buffer and send it to underlying transport.
     */
    @Override
    public void flush() throws TTransportException {
        byte[] buf = writeBuffer_.get();
        writeBuffer_.reset();
        compresser.setInput(buf);

        byte[] compBuf = new byte[buf.length * 2];
        int compressedDataLength = compresser.deflate(compBuf, 0, compBuf.length, Deflater.SYNC_FLUSH);
        if (compressedDataLength >= compBuf.length) {
            throw new TTransportException("Compression error, compressed output exceeds buffer size");
        }
        if (compressedDataLength > 0) {
            transport_.write(compBuf, 0, compressedDataLength);
            transport_.flush();
        }
    }
}

