/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.thrift.async;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.concurrent.atomic.AtomicLong;

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TFramedTransport;
import org.apache.thrift.transport.TMemoryBuffer;
import org.apache.thrift.transport.TNonblockingSSLTransport;
import org.apache.thrift.transport.TTransportException;


/**
 * Encapsulates an async method call Need to generate: - private void
 * write_args(TProtocol protocol) - public T getResult() throws <Exception_1>,
 * <Exception_2>, ...
 *
 * @param <T>
 */
public abstract class TAsyncSSLMethodCall<T> {

	private static final int INITIAL_MEMORY_BUFFER_SIZE = 128;
	private static AtomicLong sequenceIdCounter = new AtomicLong(0);

	public static enum State {
		WRITING_REQUEST_SIZE, WRITING_REQUEST_BODY, READING_RESPONSE, RESPONSE_READ, ERROR;
	}

	/**
	 * Next step in the call, initialized by start()
	 */
	private State state = null;

	protected final TNonblockingSSLTransport transport;
	private final TProtocolFactory protocolFactory;
	protected final TAsyncSSLClient client;
	private final AsyncMethodCallback<T> callback;
	private final boolean isOneway;
	private long sequenceId;
	private final long timeout;

	private ByteBuffer sizeBuffer;
	private final byte[] sizeBufferArray = new byte[4];
	private ByteBuffer frameBuffer;

	private long startTime = System.currentTimeMillis();

	protected TAsyncSSLMethodCall(TAsyncSSLClient client, TProtocolFactory protocolFactory,
			TNonblockingSSLTransport transport, AsyncMethodCallback<T> callback, boolean isOneway) {
		this.transport = transport;
		this.callback = callback;
		this.protocolFactory = protocolFactory;
		this.client = client;
		this.isOneway = isOneway;
		this.sequenceId = TAsyncSSLMethodCall.sequenceIdCounter.getAndIncrement();
		this.timeout = client.getTimeout();
	}

	protected State getState() {
		return state;
	}

	protected boolean isFinished() {
		return state == State.RESPONSE_READ;
	}

	protected long getStartTime() {
		return startTime;
	}

	protected long getSequenceId() {
		return sequenceId;
	}

	public TAsyncSSLClient getClient() {
		return client;
	}

	public boolean hasTimeout() {
		return timeout > 0;
	}

	public long getTimeoutTimestamp() {
		return timeout + startTime;
	}

	protected abstract void write_args(TProtocol protocol) throws TException;

	/**
	 * Initialize buffers.
	 *
	 * @throws TException
	 *             if buffer initialization fails
	 * @throws IOException
	 */
	protected void prepareMethodCall() throws TException {
		TMemoryBuffer memoryBuffer = new TMemoryBuffer(INITIAL_MEMORY_BUFFER_SIZE);
		TProtocol protocol = protocolFactory.getProtocol(memoryBuffer);
		write_args(protocol);

		int length = memoryBuffer.length();
		frameBuffer = ByteBuffer.wrap(memoryBuffer.getArray(), 0, length);

		TFramedTransport.encodeFrameSize(length, sizeBufferArray);
		sizeBuffer = ByteBuffer.wrap(sizeBufferArray);
	}

	/**
	 * Register with selector and start first state, which could be either
	 * connecting or writing.
	 *
	 * @throws IOException
	 *             if register or starting fails
	 */
	void start(Selector sel) throws IOException {
		SelectionKey key;
		if (transport.isOpen()) {
			state = State.WRITING_REQUEST_SIZE;
			key = transport.registerSelector(sel, SelectionKey.OP_WRITE);
		} else {
		    transport.startConnection();
		    if (!transport.isOpen())
		        throw new IOException("Unable to establish connection.");
		    state = State.WRITING_REQUEST_SIZE;
            key = transport.registerSelector(sel, SelectionKey.OP_WRITE);
		}

		key.attach(this);
	}

	void registerForFirstWrite(SelectionKey key) throws IOException {
		state = State.WRITING_REQUEST_SIZE;
		key.interestOps(SelectionKey.OP_WRITE);
	}

	protected ByteBuffer getFrameBuffer() {
		return frameBuffer;
	}

	/**
	 * Transition to next state, doing whatever work is required. Since this
	 * method is only called by the selector thread, we can make changes to our
	 * select interests without worrying about concurrency.
	 *
	 * @param key
	 */
	protected void transition(SelectionKey key) {
		// Ensure key is valid
		if (!key.isValid()) {
			key.cancel();
			Exception e = new TTransportException("Selection key not valid!");
			onError(e);
			return;
		}

		// Transition function
		try {
			switch (state) {
			case WRITING_REQUEST_SIZE:
				doWritingRequestSize();
				break;
			case WRITING_REQUEST_BODY:
				doWritingRequestBody(key);
				break;
			case READING_RESPONSE:
				doReadingResponseSize();
				doReadingResponseBody(key);
				break;
			default: // RESPONSE_READ, ERROR, or bug
				throw new IllegalStateException("Method call in state " + state
						+ " but selector called transition method. Seems like a bug...");
			}
		} catch (Exception e) {
			key.cancel();
			key.attach(null);
			onError(e);
		}
	}

	protected void onError(Exception e) {
		client.onError(e);
		callback.onError(e);
		state = State.ERROR;
	}

	private void doReadingResponseBody(SelectionKey key) throws IOException {
		int length = frameBuffer.limit();
		frameBuffer = transport.read(length);
		if (frameBuffer ==  null) {
			throw new IOException("Read call frame failed");
		}
		if (frameBuffer.remaining() == 0) {
			cleanUpAndFireCallback(key);
		}
	}

	private void cleanUpAndFireCallback(SelectionKey key) {
		state = State.RESPONSE_READ;
		key.interestOps(0);
		// this ensures that the TAsyncSSLMethod instance doesn't hang around
		key.attach(null);
		client.onComplete();
		callback.onComplete((T) this);
	}

	private void doReadingResponseSize() throws IOException {
		sizeBuffer = transport.read(4);
		if (sizeBuffer == null) {
			throw new IOException("Read call frame size failed");
		}
		if (sizeBuffer.remaining() == 0) {
			frameBuffer = ByteBuffer.allocate(TFramedTransport.decodeFrameSize(sizeBuffer.array()));
		}
	}

	private void doWritingRequestBody(SelectionKey key) throws IOException {
		if (transport.write(frameBuffer) < 0) {
			throw new IOException("Write call frame failed");
		}
		if (frameBuffer.remaining() == 0) {
			if (isOneway) {
				cleanUpAndFireCallback(key);
			} else {
				state = State.READING_RESPONSE;
				sizeBuffer.rewind(); // Prepare to read incoming frame size
				key.interestOps(SelectionKey.OP_READ);
			}
		}
	}

	private void doWritingRequestSize() throws IOException {
		if (transport.write(sizeBuffer) < 0) {
			throw new IOException("Write call frame size failed");
		}
		if (sizeBuffer.remaining() == 0) {
			state = State.WRITING_REQUEST_BODY;
		}
	}
}
