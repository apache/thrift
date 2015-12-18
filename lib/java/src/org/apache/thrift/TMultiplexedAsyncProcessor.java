
package org.apache.thrift;

import org.apache.thrift.TMultiplexedProcessor.StoredMessageProtocol;
import org.apache.thrift.protocol.TMessage;
import org.apache.thrift.protocol.TMessageType;
import org.apache.thrift.protocol.TMultiplexedProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.server.AbstractNonblockingServer.AsyncFrameBuffer;

import java.util.HashMap;
import java.util.Map;

/**
 * <code>TMultiplexedAsyncProcessor</code> is an <code>TAsyncProcessor</code> allowing
 * a single <code>TServer</code> to provide multiple services.
 *
 * <p>To do so, you instantiate the processor and then register additional
 * processors with it, as shown in the following example:</p>
 *
 * <blockquote><code>
 *     final TMultiplexedAsyncProcessor processor = new TMultiplexedAsyncProcessor();
 *
 *     processor.registerProcessor(
 *         "Calculator",
 *         new Calculator.AsyncProcessor(new CalculatorHandler()));
 *
 *     processor.registerProcessor(
 *         "WeatherReport",
 *         new WeatherReport.AsyncProcessor(new WeatherReportHandler()));
 *
 *     TServerTransport t = new TServerSocket(9090);
 *     TSimpleServer server = new TSimpleServer(processor, t);
 *
 *     server.serve();
 * </code></blockquote>
 */
public final class TMultiplexedAsyncProcessor implements TAsyncProcessor, TProcessor {
    private final Map<String,TBaseAsyncProcessor> SERVICE_PROCESSOR_MAP = new HashMap<String, TBaseAsyncProcessor>();

    /**
     * 'Register' a service with this <code>TMultiplexedAsyncProcessor</code>.  This
     * allows us to broker requests to individual services by using the service
     * name to select them at request time.
     *
     * @param serviceName Name of a service, has to be identical to the name
     * declared in the Thrift IDL, e.g. "WeatherReport".
     * @param processor Implementation of a service, usually referred to
     * as "handlers", e.g. WeatherReportHandler implementing WeatherReport.AsyncIface.
     */
    public void registerProcessor(String serviceName, TBaseAsyncProcessor processor) {
        SERVICE_PROCESSOR_MAP.put(serviceName, processor);
    }

    /**
     * This implementation of <code>process</code> performs the following steps:
     *
     * <ol>
     *     <li>Read the beginning of the message.</li>
     *     <li>Extract the service name from the message.</li>
     *     <li>Using the service name to locate the appropriate processor.</li>
     *     <li>Construct message without service name prefix.</li>
     *     <li>Locate process function.</li>
     *     <li>Read arguments.</li>
     *     <li>Start processing function.</li>
     *     <li>Set response ready for oneway calls.</li>
     * </ol>
     *
     * @param fb comes from layer below
     * @throws TException If the message type is not CALL or ONEWAY, if
     * the service name was not found in the message, or if the service
     * name was not found in the service map.  You called {@link #registerProcessor(String, TBaseAsyncProcessor) registerProcessor}
     * during initialization, right? :)
     */
    public boolean process(AsyncFrameBuffer fb) throws TException {
        final TMessage messageBegin = fb.getInputProtocol().readMessageBegin();

        final String serviceName = extractServiceName(messageBegin);

        final StoredMessageProtocol iprot = decorateProtocol(fb.getInputProtocol(), messageBegin, serviceName);

        return findActualProcessor(serviceName).process(fb, iprot, fb.getOutputProtocol());
    }

    /** expect {@link org.apache.thrift.protocol.TMultiplexedProtocol}-formatted message name */
    private static String extractServiceName(TMessage messageBegin) throws TException {
        if (messageBegin.type != TMessageType.CALL && messageBegin.type != TMessageType.ONEWAY) {
            // TODO Apache Guys - Can the server ever get an EXCEPTION or REPLY?
            // TODO Should we check for this here?
            throw new TException("Bad message type, this should not have happened!?");
        }

        // Extract the service name
        final int index = messageBegin.name.indexOf(TMultiplexedProtocol.SEPARATOR);
        if (index < 0) {
            throw new TException("Service name not found in message name: " + messageBegin.name + ".  Did you " +
                "forget to use a TMultiplexProtocol in your client?");
        }

        return messageBegin.name.substring(0, index);
    }

    /** Create a new "message begin" TMessage, removing the service name */
    private static StoredMessageProtocol decorateProtocol(TProtocol iprot, TMessage message, String serviceName) {
        final TMessage standardMessage = new TMessage(
            message.name.substring(serviceName.length() + TMultiplexedProtocol.SEPARATOR.length()),
            message.type,
            message.seqid
        );

        return new StoredMessageProtocol(iprot, standardMessage);
    }

    private TBaseAsyncProcessor findActualProcessor(String serviceName) throws TException {
        final TBaseAsyncProcessor actualProcessor = SERVICE_PROCESSOR_MAP.get(serviceName);

        if (actualProcessor == null) {
            throw new TException("Service name not found: " + serviceName + ".  Did you forget " +
                "to call registerProcessor()?");
        }

        return actualProcessor;
    }

    public boolean process(TProtocol in, TProtocol out) {
        return false;
    }
}
