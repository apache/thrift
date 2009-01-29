package org.apache.thrift.meta_data;

public class ListMetaData extends FieldValueMetaData {
  public final FieldValueMetaData elemMetaData;
  
  public ListMetaData(byte type, FieldValueMetaData eMetaData){
    super(type);
    this.elemMetaData = eMetaData;
  }    
}
