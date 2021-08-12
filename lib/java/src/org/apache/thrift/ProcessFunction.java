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
import org.apache.thrift.transport.TTransportException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static com.rbkmoney.woody.api.trace.context.TraceContext.getCurrentTraceData;

public abstract class ProcessFunction<I, T extends TBase> {
  private final String methodName;

  private static final Logger LOGGER = LoggerFactory.getLogger(ProcessFunction.class.getName());

  public ProcessFunction(String methodName) {
    this.methodName = methodName;
  }

  public final void process(int seqid, TProtocol iprot, TProtocol oprot, I iface) throws TException {
    getCurrentTraceData().getServiceSpan().getMetadata().putValue(MetadataProperties.CALL_TYPE, isOneway() ? CallType.CAST : CallType.CALL);
    T args = getEmptyArgsInstance();

    args.read(iprot);
    iprot.readMessageEnd();
    TSerializable result = null;
    byte msgType = TMessageType.REPLY;

    try {
      result = getResult(iface, args);
    } catch (TTransportException ex) {
      LOGGER.error("Transport error while processing " + getMethodName(), ex);
      throw ex;
    } catch (TApplicationException ex) {
      LOGGER.error("Internal application error processing " + getMethodName(), ex);
      result = ex;
      msgType = TMessageType.EXCEPTION;
    } catch (Exception ex) {
      LOGGER.error("Internal error processing " + getMethodName(), ex);
      if(rethrowUnhandledExceptions()) throw new RuntimeException(ex.getMessage(), ex);
      if(!isOneway()) {
        result = new TApplicationException(TApplicationException.INTERNAL_ERROR,
            "Internal error processing " + getMethodName());
        msgType = TMessageType.EXCEPTION;
      }
    }

    if(!isOneway()) {
      oprot.writeMessageBegin(new TMessage(getMethodName(), msgType, seqid));
      result.write(oprot);
      oprot.writeMessageEnd();
      oprot.getTransport().flush();
    }
  }

  private void handleException(int seqid, TProtocol oprot) throws TException {
    if (!isOneway()) {
      TApplicationException x = new TApplicationException(TApplicationException.INTERNAL_ERROR,
        "Internal error processing " + getMethodName());
      oprot.writeMessageBegin(new TMessage(getMethodName(), TMessageType.EXCEPTION, seqid));
      x.write(oprot);
      oprot.writeMessageEnd();
      oprot.getTransport().flush();
    }
  }

  protected boolean rethrowUnhandledExceptions(){
    return false;
  }

  protected abstract boolean isOneway();

  public abstract TBase getResult(I iface, T args) throws TException;

  public abstract T getEmptyArgsInstance();

  public String getMethodName() {
    return methodName;
  }
}
