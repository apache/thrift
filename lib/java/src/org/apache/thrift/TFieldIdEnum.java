package org.apache.thrift;

/**
 * Interface for all generated struct Fields objects.
 */
public interface TFieldIdEnum {
  /**
   * Get the Thrift field id for the named field.
   */
  public short getThriftFieldId();

  /**
   * Get the field's name, exactly as in the IDL.
   */
  public String getFieldName();
}
