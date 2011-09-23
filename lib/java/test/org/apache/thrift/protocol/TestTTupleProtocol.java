package org.apache.thrift.protocol;

import java.util.HashMap;
import java.util.Map;

import org.apache.thrift.TException;
import org.apache.thrift.transport.TMemoryBuffer;
import org.apache.thrift.transport.TMemoryInputTransport;

import thrift.test.Complex;
import thrift.test.Simple;

public class TestTTupleProtocol extends ProtocolTestBase {

  @Override
  protected boolean canBeUsedNaked() {
    return false;
  }

  @Override
  protected TProtocolFactory getFactory() {
    return new TTupleProtocol.Factory();
  }
}
