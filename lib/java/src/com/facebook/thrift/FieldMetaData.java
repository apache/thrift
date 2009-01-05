package com.facebook.thrift;

public class FieldMetaData implements java.io.Serializable {
  public final String fieldName;
  public FieldMetaData(String fieldName){
    this.fieldName = fieldName;
  }
}
