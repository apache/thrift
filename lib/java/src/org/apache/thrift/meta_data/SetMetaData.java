package org.apache.thrift.meta_data;

public class SetMetaData extends FieldValueMetaData {
  public final FieldValueMetaData elemMetaData;
  
  public SetMetaData(byte type, FieldValueMetaData eMetaData){
    super(type);
    this.elemMetaData = eMetaData;
  }    
}
