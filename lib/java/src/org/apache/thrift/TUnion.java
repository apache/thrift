package org.apache.thrift;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.thrift.protocol.TField;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolException;
import org.apache.thrift.protocol.TStruct;

public abstract class TUnion implements TBase {

  protected Object value_;
  protected int setField_;
  
  protected TUnion() {
    setField_ = 0;
    value_ = null;
  }

  protected TUnion(int setField, Object value) {
    setFieldValue(setField, value);
  }

  protected TUnion(TUnion other) {
    if (!other.getClass().equals(this.getClass())) {
      throw new ClassCastException();
    }
    setField_ = other.setField_;
    value_ = deepCopyObject(other.value_);
  }
  
  private static Object deepCopyObject(Object o) {
    if (o instanceof TBase) {
      return ((TBase)o).deepCopy();
    } else if (o instanceof byte[]) {
      byte[] other_val = (byte[])o;
      byte[] this_val = new byte[other_val.length];
      System.arraycopy(other_val, 0, this_val, 0, other_val.length);
      return this_val;
    } else if (o instanceof List) {
      return deepCopyList((List)o);
    } else if (o instanceof Set) {
      return deepCopySet((Set)o);
    } else if (o instanceof Map) {
      return deepCopyMap((Map)o);
    } else {
      return o;
    }
  }
  
  private static Map deepCopyMap(Map<Object, Object> map) {
    Map copy = new HashMap();
    for (Map.Entry<Object, Object> entry : map.entrySet()) {
      copy.put(deepCopyObject(entry.getKey()), deepCopyObject(entry.getValue()));
    }
    return copy;
  }

  private static Set deepCopySet(Set set) {
    Set copy = new HashSet();
    for (Object o : set) {
      copy.add(deepCopyObject(o));
    }
    return copy;
  }

  private static List deepCopyList(List list) {
    List copy = new ArrayList(list.size());
    for (Object o : list) {
      copy.add(deepCopyObject(o));
    }
    return copy;
  }

  public int getSetField() {
    return setField_;
  }
  
  public Object getFieldValue() {
    return value_;
  }
  
  public Object getFieldValue(int fieldId) {
    if (fieldId != setField_) {
      throw new IllegalArgumentException("Cannot get the value of field " + fieldId + " because union's set field is " + setField_);
    }
    
    return getFieldValue();
  }

  public boolean isSet() {
    return setField_ != 0;
  }
  
  public boolean isSet(int fieldId) {
    return setField_ == fieldId;
  }

  public void read(TProtocol iprot) throws TException {
    setField_ = 0;
    value_ = null;

    iprot.readStructBegin();

    TField field = iprot.readFieldBegin();

    value_ = readValue(iprot, field);
    if (value_ != null) {
      setField_ = field.id;
    }

    iprot.readFieldEnd();
    // this is so that we will eat the stop byte. we could put a check here to
    // make sure that it actually *is* the stop byte, but it's faster to do it
    // this way.
    iprot.readFieldBegin();
    iprot.readStructEnd();
  }

  public void setFieldValue(int fieldId, Object value) {
    checkType((short)fieldId, value);
    setField_ = (short)fieldId;
    value_ = value;
  }

  public void write(TProtocol oprot) throws TException {
    if (getSetField() == 0 || getFieldValue() == null) {
      throw new TProtocolException("Cannot write a TUnion with no set value!");
    }
    oprot.writeStructBegin(getStructDesc());
    oprot.writeFieldBegin(getFieldDesc(setField_));
    writeValue(oprot, (short)setField_, value_);
    oprot.writeFieldEnd();
    oprot.writeFieldStop();
    oprot.writeStructEnd();
  }

  /**
   * Implementation should be generated so that we can efficiently type check 
   * various values.
   * @param setField
   * @param value
   */
  protected abstract void checkType(short setField, Object value) throws ClassCastException;

  /**
   * Implementation should be generated to read the right stuff from the wire 
   * based on the field header. 
   * @param field
   * @return
   */
  protected abstract Object readValue(TProtocol iprot, TField field) throws TException;

  protected abstract void writeValue(TProtocol oprot, short setField, Object value) throws TException;

  protected abstract TStruct getStructDesc();

  protected abstract TField getFieldDesc(int setField);

  @Override
  public String toString() {
    Object v = getFieldValue();
    String vStr = null;
    if (v instanceof byte[]) {
      vStr = bytesToStr((byte[])v);
    } else {
      vStr = v.toString();
    }
    return "<" + this.getClass().getSimpleName() + " " + getFieldDesc(getSetField()).name + ":" + vStr + ">";
  }

  private static String bytesToStr(byte[] bytes) {
    StringBuilder sb = new StringBuilder();
    int size = Math.min(bytes.length, 128);
    for (int i = 0; i < size; i++) {
      if (i != 0) {
        sb.append(" ");
      }
      String digit = Integer.toHexString(bytes[i]);
      sb.append(digit.length() > 1 ? digit : "0" + digit);
    }
    if (bytes.length > 128) {
      sb.append(" ...");
    }
    return sb.toString();
  }
}
