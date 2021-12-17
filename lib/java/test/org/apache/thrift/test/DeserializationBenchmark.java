package org.apache.thrift.test;

import org.apache.thrift.Fixtures;
import org.apache.thrift.TBase;
import org.apache.thrift.TDeserializer;
import org.apache.thrift.TException;
import org.apache.thrift.TSerializer;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TCompactProtocol;
import org.apache.thrift.protocol.TJSONProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.protocol.TSimpleJSONProtocol;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.infra.Blackhole;
import org.openjdk.jmh.profile.GCProfiler;
import org.openjdk.jmh.profile.StackProfiler;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import org.openjdk.jmh.runner.options.TimeValue;
import thrift.test.CompactProtoTestStruct;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Benchmark that demonstrates performance characteristics of partial deserialization
 * compared to full deserialization for each supported protocol.
 * <p>
 * We use {@code CompactProtoTestStruct} because it is one of the most complex to process.
 * We have chosen to use an arbitrarily selected subset of fields to deserialize.
 */
public class DeserializationBenchmark {

  /**
   * Number of deserialize operations per benchmark run.
   */
  public static final int NUM_ITERATIONS = 1000;

  /**
   * Deserialization mode.
   */
  public enum DeserializationMode {
    Full,
    Partial
  }

  /**
   * Deserialization protocol.
   */
  public enum Protocol {
    Binary,
    Compact,
    Json,
    SimpleJson,
  }

  @State(Scope.Thread)
  public static class Data {
    // A populated instance that is serialized once and then
    // repeatedly deserialized NUM_ITERATIONS times.
    private final  TBase object = Fixtures.compactProtoTestStruct;

    // Class of the object defined above.
    private final Class<? extends TBase> objectClass = CompactProtoTestStruct.class;

    // The list of fields deserialized during partial deserialization.
    private final List<String> fieldNames = new ArrayList<>(Arrays.asList(
        "a_byte",
        "i16_list",
        "i32_set",
        "i64_byte_map"
    ));

    // Note that we have excluded SimpleJson from the set below because that
    // protocol cannot handle CompactProtoTestStruct. We get the following exception:
    // TSimpleJSONProtocol$CollectionMapKeyException: Cannot serialize a map with keys
    // that are of type list
    @Param({"Binary", "Compact", "Json"})
    public Protocol protocol;

    @Param({"Full", "Partial"})
    public DeserializationMode mode;

    // Holds serialized form of 'object' defined earlier.
    public byte[] serialized;

    // Deserializer use for full deserialization.
    public TDeserializer deserializer;

    // Deserializer use for partial deserialization.
    public TDeserializer partialDeserializer;

    @Setup
    public void initialize() throws TException {
      TProtocolFactory protocolFactory = this.getProtocolFactory(this.protocol);
      TSerializer serializer = new TSerializer(protocolFactory);
      this.serialized = serializer.serialize(this.object);

      this.deserializer = new TDeserializer(protocolFactory);
      this.partialDeserializer =
          new TDeserializer(this.objectClass, this.fieldNames, protocolFactory);
    }

    private TProtocolFactory getProtocolFactory(Protocol prot) {
      switch (prot) {
        case Binary:
          return new TBinaryProtocol.Factory();

        case Compact:
          return new TCompactProtocol.Factory();

        case Json:
          return new TJSONProtocol.Factory();

        case SimpleJson:
          return new TSimpleJSONProtocol.Factory();

        default:
          throw new UnsupportedOperationException("Unsupported protocol: " + protocol);
      }
    }
  }

  @Benchmark
  public static void deserialize(Data data, Blackhole blackhole)
      throws TException, InstantiationException, IllegalAccessException {

    TBase instance;
    for (int i = 0; i < NUM_ITERATIONS; i++) {
      if (data.mode.equals(DeserializationMode.Full)) {
        instance = data.objectClass.newInstance();
        data.deserializer.deserialize(instance, data.serialized);
      } else if (data.mode.equals(DeserializationMode.Partial)) {
        instance = (TBase) data.partialDeserializer.partialDeserializeObject(data.serialized);
      } else {
        throw new UnsupportedOperationException("Unsupported deserialization mode : " + data.mode);
      }
      blackhole.consume(instance);
    }
  }

  public static void main(String[] args) throws Exception {
    Options opt = new OptionsBuilder()
        .include(DeserializationBenchmark.class.getSimpleName())
        .addProfiler(GCProfiler.class)
        .addProfiler(StackProfiler.class)
        .warmupIterations(2)
        .warmupTime(TimeValue.seconds(5))
        .measurementIterations(5)
        .measurementTime(TimeValue.seconds(5))
        .mode(Mode.Throughput)
        .shouldDoGC(true)
        .forks(3)
        .build();

    new Runner(opt).run();
  }
}
