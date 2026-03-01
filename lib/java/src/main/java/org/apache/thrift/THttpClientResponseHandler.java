package org.apache.thrift;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import org.apache.hc.core5.http.ClassicHttpResponse;
import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.HttpException;
import org.apache.hc.core5.http.HttpStatus;
import org.apache.hc.core5.http.io.HttpClientResponseHandler;

public class THttpClientResponseHandler implements HttpClientResponseHandler<InputStream> {
  @Override
  public InputStream handleResponse(ClassicHttpResponse response)
      throws HttpException, IOException {
    try (InputStream is = response.getEntity().getContent()) {
      int responseCode = response.getCode();
      if (responseCode != HttpStatus.SC_OK) {
        throw new IOException("HTTP Response code: " + responseCode);
      }
      byte[] readByteArray = readIntoByteArray(is);
      try {
        // Indicate we're done with the content.
        consume(response.getEntity());
      } catch (IOException ioe) {
        // We ignore this exception, it might only mean the server has no
        // keep-alive capability.
      }
      return new ByteArrayInputStream(readByteArray);
    } catch (IOException ioe) {
      throw ioe;
    }
  }

  /**
   * Read the responses into a byte array so we can release the connection early. This implies that
   * the whole content will have to be read in memory, and that momentarily we might use up twice
   * the memory (while the thrift struct is being read up the chain). Proceeding differently might
   * lead to exhaustion of connections and thus to app failure.
   *
   * @param is input stream
   * @return read bytes
   * @throws IOException when exception during read
   */
  private static byte[] readIntoByteArray(InputStream is) throws IOException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    byte[] buf = new byte[1024];
    int len;
    do {
      len = is.read(buf);
      if (len > 0) {
        baos.write(buf, 0, len);
      }
    } while (-1 != len);
    return baos.toByteArray();
  }

  /**
   * copy from org.apache.http.util.EntityUtils#consume. Android has it's own httpcore that doesn't
   * have a consume.
   */
  private static void consume(final HttpEntity entity) throws IOException {
    if (entity == null) {
      return;
    }
    if (entity.isStreaming()) {
      InputStream instream = entity.getContent();
      if (instream != null) {
        instream.close();
      }
    }
  }
}
