# Partial Thrift Deserialization

## Overview
This document describes how partial deserialization of Thrift works. There are two main goals of this documentation:
1. Make it easier to understand the current Java implementation in this folder.
1. Be useful in implementing partial deserialization support in additional languages.

This document is divided into two high level areas. The first part explains important concepts relevant to partial deserialization. The second part describes components involved in the Java implementation in this folder.

Moreover, this blog provides some performance numbers and addtional information: https://medium.com/pinterest-engineering/improving-data-processing-efficiency-using-partial-deserialization-of-thrift-16bc3a4a38b4

## Basic Concepts

### Motivation

The main motivation behind implementing this feature is to improve performance when we need to access only a subset of fields in any Thrift object. This situation arises often when big data is stored in Thrift encoded format (for example, SequenceFile with serialized Thrift values). Many data processing jobs may access this data. However, not every job needs to access every field of each object. In such cases, if we have prior knowledge of the fields needed for a given job, we can deserialize only that subset of fields and avoid the cost deserializing the rest of the fields. There are two benefits of this approach: we save cpu cycles by not deserializing unnecessary fields and we end up reducing gc pressure. Both of the savings quickly add up when processing billions of instances in a data processing job.

### Partial deserialization

Partial deserialization involves deserializing only a subset of the fields of a serialized Thrift object while efficiently skipping over the rest. One very important benefit of partial deserialization is that the output of the deserialization process is not limited to a `TBase` derived object. It can deserialize a serialized blob into any type by using an appropriate `ThriftFieldValueProcessor`.

### Defining the subset of fields to deserialize

The subset of fields to deserialize is defined using a list of fully qualified field names. For example, consider the Thrift `struct` definition below:

```Thrift
struct SmallStruct {
   1: optional string stringValue;
   2: optional i16 i16Value;
}

struct TestStruct {
   1: optional i16 i16Field;
   2: optional list<SmallStruct> structList;
   3: optional set<SmallStruct> structSet;
   4: optional map<string, SmallStruct> structMap;
   5: optional SmallStruct structField;
}
```

For the Thrift `struct`, each of the following line shows a fully qualified field definition. Partial deserialization uses a non-empty set of such field definitions to identify the subset of fields to deserialize.

```
- i16Field
- structList.stringValue
- structSet.i16Value
- structMap.stringValue
- structField.i16Value
```

Note that the syntax of denoting paths involving map fields do not support a way to define sub-fields of the key type. However, that limitation can be addressed in future by amending the syntax in a backword compatible way.

For example, the field path `structMap.stringValue` shown above has leaf segment `stringValue` which is a field in map values.

## Components

The process of partial deserialization involves the following major components. We have listed names of the Java file(s) implementing each component for easier mapping to the source code.

### Thrift Metadata

Source files:
- ThriftField.java
- ThriftMetadata.java
- TDeserializer.java

We saw in the previous section how we can identify the subset of fields to deserialize. As the first step, we need to compile the collection of field definitions into an internal efficient data structure that we can traverse at runtime. The compilation takes place internally when one creates an instance of TDeserializer using a constructor that accepts a list of field names.

```Java
// First, create a collection of fully qualified field names.
List<String> fieldNames = Arrays.asList("i16Field", "structField.i16Value");

// Create an instance of TDeserializer that supports partial deserialization.
TDeserializer deserializer =
    new TDeserializer(TestStruct.class, fieldNames, new TBinaryProtocol.Factory());
```

At this point, we have an efficient internal representation of the fields that need to get deserialized.

### Partial Thrift Protocol

Source files:
- TProtocol.java
- TBinaryProtocol.java
- TCompactProtocol.java

This component implements efficient skipping over fields that need not be deserialized. The functionality to skip over fields has been added to the above protocols by addition of `skip*()` methods. The default implementation of each such method simply calls the corresponding `read*()` method in `TProtocol.java`. A derived protocol (for example, `TBinaryProtocol`) provides a more efficient implementation of each `skip*()` method.

For example, `TBinaryProtocol` skips a field by incrementing internal offset into the transport buffer.

### Partial Thrift Deserializer

Source files:
- TDeserializer.java

This component, traverses a serialized blob sequentially one field at a time. At the beginning of each field, it consults the informations stored in the compiled `ThriftMetadata` to see if that field needs to be deserialized. If yes, then the field is deserialized into a value as would normally take place during regular deserialization process. If that field is not in the target subset then the deserializer efficiently skips over that field.

### Field Value Processor

Source files:
- ThriftFieldValueProcessor.java
- ThriftStructProcessor.java

One very important benefit of partial deserialization is that the output of the deserialization process is not limited to a `TBase` derived object. It can deserialize a serialized blob into any type by using an appropriate `ThriftFieldValueProcessor`.

When the partial Thrift deserializer deserializes a field, it passes its value to a `ThriftFieldValueProcessor`. The processor gets to decide whether the value is stored as-is or is stored in some intermediate form. The default implementation of this interface is `ThriftStructProcessor`. This implementation outputs a `TBase` derived object. There are other implementations that exist (not included in this drop at present). For example, one implementation enables deserializing a Thrift blob directly into an `InternalRow` used by `Spark`. That has yielded orders of magnitude performance improvement over a `Spark` engine that consumes `Thrift` data using its default deserializer.

### Miscellanious Helpers

Files:
- TFieldData.java : Holds the type and id members of a TField into a single int. This encoding scheme obviates the need to instantiate TField during the partial deserialization process.
- EnumCache.java : Provides a memoized way to lookup an enum by its value.
- PartialThriftComparer.java : Enables comparison of two TBase instances such that the comparison is limited to the subset of fields defined by the supplied metadata.
