package org.apache.thrift.server;

import com.rbkmoney.woody.api.interceptor.CommonInterceptor;
import com.rbkmoney.woody.api.interceptor.EmptyCommonInterceptor;
import com.rbkmoney.woody.api.trace.ContextUtils;
import com.rbkmoney.woody.api.trace.TraceData;
import com.rbkmoney.woody.api.trace.context.TraceContext;
import org.apache.thrift.TProcessor;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TIOStreamTransport;
import org.apache.thrift.transport.TTransport;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

/**
 * Servlet implementation class ThriftServer
 */
public class TServlet extends HttpServlet {

  private final TProcessor processor;

  private final TProtocolFactory inProtocolFactory;

  private final TProtocolFactory outProtocolFactory;

  private final Collection<Map.Entry<String, String>> customHeaders;

  private final CommonInterceptor defaultInterceptor = new EmptyCommonInterceptor() {
    @Override
    public boolean interceptResponse(TraceData traceData, Object providerContext, Object... contextParams) {
      Throwable t = ContextUtils.getCallError(traceData.getServiceSpan());
      if (t != null) {
        ContextUtils.setInterceptionError(traceData.getServiceSpan(), t);
        return false;
      }
      return true;
    }
  };

  private  CommonInterceptor interceptor;

  public TServlet(TProcessor processor, TProtocolFactory inProtocolFactory,
                   TProtocolFactory outProtocolFactory, CommonInterceptor interceptor) {
    super();
    this.processor = processor;
    this.inProtocolFactory = inProtocolFactory;
    this.outProtocolFactory = outProtocolFactory;
    this.customHeaders = new ArrayList<>();
    this.interceptor = interceptor == null ? defaultInterceptor : interceptor;
  }

  /**
   * @see HttpServlet#HttpServlet()
   */
  public TServlet(TProcessor processor, TProtocolFactory inProtocolFactory,
                   TProtocolFactory outProtocolFactory) {
    this(processor, inProtocolFactory, outProtocolFactory, null);
  }

  /**
   * @see HttpServlet#HttpServlet()
   */
  public TServlet(TProcessor processor, TProtocolFactory protocolFactory) {
    this(processor, protocolFactory, protocolFactory);
  }

  public TServlet(TProcessor processor, TProtocolFactory protocolFactory, CommonInterceptor interceptor) {
    this(processor, protocolFactory, protocolFactory, interceptor);
  }

  @Override
  protected void service(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
    doPost(req, resp);
  }

  /**
   * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse
   *      response)
   */
  @Override
  protected void doPost(HttpServletRequest request, HttpServletResponse response)
          throws ServletException, IOException {

    TraceData traceData = TraceContext.getCurrentTraceData();
    OutputStream out = null;
    try {
      InputStream in = request.getInputStream();
      out = response.getOutputStream();
        response.setContentType("application/x-thrift");
        if (!interceptor.interceptRequest(traceData, request, response)) {
          ContextUtils.tryThrowInterceptionError(traceData.getServiceSpan());
        }

        if (null != this.customHeaders) {
          for (Map.Entry<String, String> header : this.customHeaders) {
            response.addHeader(header.getKey(), header.getValue());
          }
        }

        TTransport transport = new TIOStreamTransport(in, out);

        TProtocol inProtocol = inProtocolFactory.getProtocol(transport);
        TProtocol outProtocol = outProtocolFactory.getProtocol(transport);

        processor.process(inProtocol, outProtocol);
    } catch (Throwable te) {
      ContextUtils.setCallError(traceData.getServiceSpan(), te);
    } finally {
        if (!interceptor.interceptResponse(traceData, response)) {
            Throwable t = ContextUtils.getInterceptionError(traceData.getServiceSpan());
            if (t != null) {
                throw new ServletException(t);
            }
        }
        if (out != null) {
            out.flush();
        }
    }
  }

  /**
   * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
   *      response)
   */
  protected void doGet(HttpServletRequest request, HttpServletResponse response)
          throws ServletException, IOException {
    doPost(request, response);
  }

  public void addCustomHeader(final String key, final String value) {
    this.customHeaders.add(new AbstractMap.SimpleImmutableEntry(key, value));
  }

  public void setCustomHeaders(Collection<Map.Entry<String, String>> headers) {
    this.customHeaders.clear();
    this.customHeaders.addAll(headers);
  }
}
