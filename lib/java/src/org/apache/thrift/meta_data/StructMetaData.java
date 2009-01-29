package org.apache.thrift.meta_data;

public class StructMetaData extends FieldValueMetaData {
  public final Class structClass;
  
  public StructMetaData(byte type, Class sClass){
    super(type);
    this.structClass = sClass;
  }    
}
