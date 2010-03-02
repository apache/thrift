package org.apache.thrift.test.transport;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Arrays;

import org.apache.thrift.transport.TFramedTransport;
import org.apache.thrift.transport.TIOStreamTransport;
import org.apache.thrift.transport.TMemoryBuffer;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;

public class TFramedTransportTest {
  public static class WriteCountingTransport extends TTransport {
    private int writeCount = 0;
    private final TTransport trans;

    public WriteCountingTransport(TTransport underlying) {
      trans = underlying;
    }

    @Override
    public void close() {}

    @Override
    public boolean isOpen() {return true;}

    @Override
    public void open() throws TTransportException {}

    @Override
    public int read(byte[] buf, int off, int len) throws TTransportException {
      return 0;
    }

    @Override
    public void write(byte[] buf, int off, int len) throws TTransportException {
      writeCount ++;
      trans.write(buf, off, len);
    }
  }

  public static class ReadCountingTransport extends TTransport {
    public int readCount = 0;
    private TTransport trans;

    public ReadCountingTransport(TTransport underlying) {
      trans = underlying;
    }

    @Override
    public void close() {}

    @Override
    public boolean isOpen() {return true;}

    @Override
    public void open() throws TTransportException {}

    @Override
    public int read(byte[] buf, int off, int len) throws TTransportException {
      readCount++;
      return trans.read(buf, off, len);
    }

    @Override
    public void write(byte[] buf, int off, int len) throws TTransportException {}
  }

  public static void main(String[] args) throws TTransportException, IOException {
    testWrite();
    testRead();
    testDirectRead();
  }

  private static void testWrite() throws TTransportException, IOException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    WriteCountingTransport countingTrans = new WriteCountingTransport(new TIOStreamTransport(baos));
    TTransport trans = new TFramedTransport(countingTrans);

    trans.write(byteSequence(0,100));
    failUnless(countingTrans.writeCount == 0);
    trans.write(byteSequence(101,200));
    trans.write(byteSequence(201,255));
    failUnless(countingTrans.writeCount == 0);

    trans.flush();
    failUnless(countingTrans.writeCount == 2);

    DataInputStream din = new DataInputStream(new ByteArrayInputStream(baos.toByteArray()));
    failUnless(din.readInt() == 256);

    byte[] buf = new byte[256];
    din.read(buf, 0, 256);
    failUnless(Arrays.equals(byteSequence(0,255), buf));
  }

  private static void testRead() throws IOException, TTransportException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DataOutputStream dos = new DataOutputStream(baos);
    dos.writeInt(50);
    dos.write(byteSequence(0, 49));

    TMemoryBuffer membuf = new TMemoryBuffer(0);
    membuf.write(baos.toByteArray());

    ReadCountingTransport countTrans = new ReadCountingTransport(membuf);
    TFramedTransport trans = new TFramedTransport(countTrans);

    byte[] readBuf = new byte[10];
    trans.read(readBuf, 0, 10);
    failUnless(Arrays.equals(readBuf, byteSequence(0,9)));

    trans.read(readBuf, 0, 10);
    failUnless(Arrays.equals(readBuf, byteSequence(10,19)));

    failUnless(countTrans.readCount == 2);
  }

  private static void testDirectRead() throws IOException, TTransportException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DataOutputStream dos = new DataOutputStream(baos);
    dos.writeInt(50);
    dos.write(byteSequence(0, 49));

    TMemoryBuffer membuf = new TMemoryBuffer(0);
    membuf.write(baos.toByteArray());

    ReadCountingTransport countTrans = new ReadCountingTransport(membuf);
    TFramedTransport trans = new TFramedTransport(countTrans);

    failUnless(trans.getBytesRemainingInBuffer() == 0);

    byte[] readBuf = new byte[10];
    trans.read(readBuf, 0, 10);
    failUnless(Arrays.equals(readBuf, byteSequence(0,9)));

    failUnless(trans.getBytesRemainingInBuffer() == 40);
    failUnless(trans.getBufferPosition() == 10);

    trans.consumeBuffer(5);
    failUnless(trans.getBytesRemainingInBuffer() == 35);
    failUnless(trans.getBufferPosition() == 15);

    failUnless(countTrans.readCount == 2);
  }

  private static void failUnless(boolean b) {
    if (!b) {
      throw new RuntimeException();
    }
  }

  private static byte[] byteSequence(int start, int end) {
    byte[] result = new byte[end-start+1];
    for (int i = 0; i <= (end-start); i++) {
      result[i] = (byte)(start+i);
    }
    return result;
  }
}
