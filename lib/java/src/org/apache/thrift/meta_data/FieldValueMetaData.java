package org.apache.thrift.meta_data;

import org.apache.thrift.protocol.TType;

/**
 * FieldValueMetaData and collection of subclasses to store metadata about
 * the value(s) of a field
 */
public class FieldValueMetaData implements java.io.Serializable {
  public final byte type;  
 
  public FieldValueMetaData(byte type){
    this.type = type;
  }
  
  public boolean isStruct() {
    return type == TType.STRUCT; 
  }
  
  public boolean isContainer() {
    return type == TType.LIST || type == TType.MAP || type == TType.SET;
  }
}
