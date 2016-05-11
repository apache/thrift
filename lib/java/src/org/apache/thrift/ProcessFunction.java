/**
 *
 */
package org.apache.thrift;

import com.rbkmoney.woody.api.event.CallType;
import com.rbkmoney.woody.api.trace.ContextUtils;
import com.rbkmoney.woody.api.trace.Metadata;
import com.rbkmoney.woody.api.trace.MetadataProperties;
import com.rbkmoney.woody.api.trace.TraceData;
import com.rbkmoney.woody.api.trace.context.TraceContext;
import org.apache.thrift.protocol.TMessage;
import org.apache.thrift.protocol.TMessageType;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class ProcessFunction<I, T extends TBase> {
  private final String methodName;

  private static final Logger LOGGER = LoggerFactory.getLogger(ProcessFunction.class.getName());

  public ProcessFunction(String methodName) {
    this.methodName = methodName;
  }

  public final void process(int seqid, TProtocol iprot, TProtocol oprot, I iface) throws TException {
    TraceData traceData = TraceContext.getCurrentTraceData();
    Metadata metadata = traceData.getServiceSpan().getMetadata();
    metadata.putValue(MetadataProperties.CALL_TYPE, isOneway() ? CallType.CAST : CallType.CALL);
    metadata.putValue(MetadataProperties.CALL_NAME, getMethodName());
    T args = getEmptyArgsInstance();
    try {
      args.read(iprot);
    } catch (TProtocolException e) {
      iprot.readMessageEnd();
      TApplicationException x = new TApplicationException(TApplicationException.PROTOCOL_ERROR, e.getMessage());
      ContextUtils.setCallError(traceData.getServiceSpan(), x);
      oprot.writeMessageBegin(new TMessage(getMethodName(), TMessageType.EXCEPTION, seqid));
      x.write(oprot);
      oprot.writeMessageEnd();
      oprot.getTransport().flush();
      return;
    }
    iprot.readMessageEnd();
    TBase result = null;

    try {
      result = getResult(iface, args);
    } catch(TException tex) {
      LOGGER.error("Internal error processing " + getMethodName(), tex);
      if (!isOneway()) {
        TApplicationException x = new TApplicationException(TApplicationException.INTERNAL_ERROR,
          "Internal error processing " + getMethodName());
        ContextUtils.setCallError(traceData.getServiceSpan(), tex);
        oprot.writeMessageBegin(new TMessage(getMethodName(), TMessageType.EXCEPTION, seqid));
        x.write(oprot);
        oprot.writeMessageEnd();
        oprot.getTransport().flush();
      }
      return;
    }

    if(!isOneway()) {
      oprot.writeMessageBegin(new TMessage(getMethodName(), TMessageType.REPLY, seqid));
      result.write(oprot);
      oprot.writeMessageEnd();
      oprot.getTransport().flush();
    }
  }

  protected abstract boolean isOneway();

  public abstract TBase getResult(I iface, T args) throws TException;

  public abstract T getEmptyArgsInstance();

  public String getMethodName() {
    return methodName;
  }
}

