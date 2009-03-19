package org.apache.thrift.test;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;

import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TIOStreamTransport;
import org.apache.thrift.transport.TTransport;

public class WriteStruct {
  public static void main(String[] args) throws Exception {
    if (args.length != 2) {
      System.out.println("usage: java -cp build/classes org.apache.thrift.test.WriteStruct filename proto_factory_class");
      System.out.println("Write out an instance of Fixtures.compactProtocolTestStruct to 'file'. Use a protocol from 'proto_factory_class'.");
    }
    
    TTransport trans = new TIOStreamTransport(new BufferedOutputStream(new FileOutputStream(args[0])));
    
    TProtocolFactory factory = (TProtocolFactory)Class.forName(args[1]).newInstance();
    
    TProtocol proto = factory.getProtocol(trans);
    
    Fixtures.compactProtoTestStruct.write(proto);
    trans.flush();
  }

}
