package org.apache.thrift.async;

import org.apache.thrift.transport.TNonblockingSSLTransport;

public interface TAsyncSSLClientFactory<T extends TAsyncSSLClient> {
	public T getAsyncSSLClient(TNonblockingSSLTransport transport);
}
