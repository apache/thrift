package org.apache.thrift.meta_data;

public class MapMetaData extends FieldValueMetaData {
  public final FieldValueMetaData keyMetaData;
  public final FieldValueMetaData valueMetaData;
  
  public MapMetaData(byte type, FieldValueMetaData kMetaData, FieldValueMetaData vMetaData){
    super(type);
    this.keyMetaData = kMetaData;
    this.valueMetaData = vMetaData;
  }    
}
