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
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Objects;
import java.util.zip.Deflater;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.Inflater;
import java.util.zip.InflaterInputStream;
import org.apache.thrift.TConfiguration;

/** TZlibTransport deflates on write and inflates on read. */
public class TZlibTransport extends TIOStreamTransport {

  private TTransport transport_ = null;

  public static class Factory extends TTransportFactory {
    public Factory() {}

    @Override
    public TTransport getTransport(TTransport base) throws TTransportException {
      return new TZlibTransport(base);
    }
  }

  /**
   * Constructs a new TZlibTransport instance.
   *
   * @param transport the underlying transport to read from and write to
   */
  public TZlibTransport(TTransport transport) throws TTransportException {
    this(transport, Deflater.BEST_COMPRESSION);
  }

  /**
   * Constructs a new TZlibTransport instance.
   *
   * @param transport the underlying transport to read from and write to
   * @param compressionLevel 0 for no compression, 9 for maximum compression
   */
  public TZlibTransport(TTransport transport, int compressionLevel) throws TTransportException {
    super(
        Objects.isNull(transport.getConfiguration())
            ? new TConfiguration()
            : transport.getConfiguration());
    transport_ = transport;
    inputStream_ = new InflaterInputStream(new TTransportInputStream(transport_), new Inflater());
    outputStream_ =
        new DeflaterOutputStream(
            new TTransportOutputStream(transport_), new Deflater(compressionLevel, false), true);
  }

  @Override
  public int read(byte[] buf, int off, int len) throws TTransportException {
    checkReadBytesAvailable(len);
    int bytesRead = super.read(buf, off, len);
    if (bytesRead > 0) {
      countConsumedMessageBytes(bytesRead);
    }
    return bytesRead;
  }

  @Override
  public boolean isOpen() {
    return transport_.isOpen();
  }

  @Override
  public void open() throws TTransportException {
    transport_.open();
  }

  /**
   * Finishes compression, flushes the output, and closes the underlying transport.
   *
   * <p>This can block while sending the compressed output. To abort pending I/O, close the
   * underlying transport directly.
   *
   * <p>If finishing the output and closing the underlying transport both fail unchecked, the
   * cleanup failure is suppressed on the first failure.
   */
  @Override
  public void close() {
    Throwable primaryFailure = null;
    try {
      super.close();
    } catch (RuntimeException | Error failure) {
      primaryFailure = failure;
      throw failure;
    } finally {
      try {
        if (transport_.isOpen()) {
          transport_.close();
        }
      } catch (RuntimeException | Error cleanupFailure) {
        if (primaryFailure == null) {
          throw cleanupFailure;
        }
        if (cleanupFailure != primaryFailure) {
          primaryFailure.addSuppressed(cleanupFailure);
        }
      }
    }
  }
}

class TTransportInputStream extends InputStream {

  private TTransport transport = null;

  public TTransportInputStream(TTransport transport) {
    this.transport = transport;
  }

  @Override
  public int read() throws IOException {
    try {
      byte[] buf = new byte[1];
      transport.read(buf, 0, 1);
      return buf[0];
    } catch (TTransportException e) {
      throw new IOException(e);
    }
  }

  @Override
  public int read(byte b[], int off, int len) throws IOException {
    try {
      return transport.read(b, off, len);
    } catch (TTransportException e) {
      throw new IOException(e);
    }
  }
}

class TTransportOutputStream extends OutputStream {

  private TTransport transport = null;

  public TTransportOutputStream(TTransport transport) {
    this.transport = transport;
  }

  @Override
  public void write(final int b) throws IOException {
    try {
      transport.write(new byte[] {(byte) b});
    } catch (TTransportException e) {
      throw new IOException(e);
    }
  }

  @Override
  public void write(byte b[], int off, int len) throws IOException {
    try {
      transport.write(b, off, len);
    } catch (TTransportException e) {
      throw new IOException(e);
    }
  }

  @Override
  public void flush() throws IOException {
    try {
      transport.flush();
    } catch (TTransportException e) {
      throw new IOException(e);
    }
  }

  @Override
  public void close() throws IOException {
    // DeflaterOutputStream.close() finishes compression before closing this adapter. Flush the
    // resulting trailer while the underlying transport is still open; its owner closes it later.
    flush();
  }
}
