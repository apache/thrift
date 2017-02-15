package org.apache.thrift.meta_data;

public class CollectionMetaData extends FieldValueMetaData {
    public final FieldValueMetaData elemMetaData;

    public CollectionMetaData(byte type, FieldValueMetaData eMetaData){
        super(type);
        this.elemMetaData = eMetaData;
    }

    public FieldValueMetaData getElementMetaData() {
        return elemMetaData;
    }
}
