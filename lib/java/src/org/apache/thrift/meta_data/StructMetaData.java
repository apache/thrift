package org.apache.thrift.meta_data;

import org.apache.thrift.TBase;

public class StructMetaData extends FieldValueMetaData {
  public final Class<? extends TBase> structClass;
  
  public StructMetaData(byte type, Class<? extends TBase> sClass){
    super(type);
    this.structClass = sClass;
  }    
}
