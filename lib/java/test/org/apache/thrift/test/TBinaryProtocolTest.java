package org.apache.thrift.test;

import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocolFactory;

public class TBinaryProtocolTest extends ProtocolTestBase {

  public static void main(String[] args) throws Exception {
    new TBinaryProtocolTest().main();
  }
  
  @Override
  protected TProtocolFactory getFactory() {
    return new TBinaryProtocol.Factory();
  }

}
