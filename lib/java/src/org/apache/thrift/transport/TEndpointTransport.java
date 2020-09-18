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
package org.apache.thrift.transport;

import org.apache.thrift.TConfiguration;

import java.util.Objects;

public abstract class TEndpointTransport extends TTransport{

    protected long getMaxMessageSize() { return getConfiguration().getMaxMessageSize(); }

    protected long knownMessageSize;
    protected long remainingMessageSize;

    private TConfiguration _configuration;
    public TConfiguration getConfiguration() {
        return _configuration;
    }

    public TEndpointTransport( TConfiguration config) throws TTransportException {
        _configuration = Objects.isNull(config) ? new TConfiguration() : config;

        resetConsumedMessageSize(-1);
    }

    /**
     * Resets RemainingMessageSize to the configured maximum
     * @param newSize
     */
    protected void resetConsumedMessageSize(long newSize) throws TTransportException {
        // full reset
        if (newSize < 0)
        {
            knownMessageSize = getMaxMessageSize();
            remainingMessageSize = getMaxMessageSize();
            return;
        }

        // update only: message size can shrink, but not grow
        if (newSize > knownMessageSize)
            throw new TTransportException(TTransportException.END_OF_FILE, "MaxMessageSize reached");

        knownMessageSize = newSize;
        remainingMessageSize = newSize;
    }

    /**
     * Updates RemainingMessageSize to reflect then known real message size (e.g. framed transport).
     * Will throw if we already consumed too many bytes or if the new size is larger than allowed.
     * @param size
     */
    public void updateKnownMessageSize(long size) throws TTransportException {
        long consumed = knownMessageSize - remainingMessageSize;
        resetConsumedMessageSize(size == 0 ? -1 : size);
        countConsumedMessageBytes(consumed);
    }

    /**
     * Throws if there are not enough bytes in the input stream to satisfy a read of numBytes bytes of data
     * @param numBytes
     */
    public void checkReadBytesAvailable(long numBytes) throws TTransportException {
        if (remainingMessageSize < numBytes)
            throw new TTransportException(TTransportException.END_OF_FILE, "MaxMessageSize reached");
    }

    /**
     * Consumes numBytes from the RemainingMessageSize.
     * @param numBytes
     */
    protected void countConsumedMessageBytes(long numBytes) throws TTransportException {
        if (remainingMessageSize >= numBytes)
        {
            remainingMessageSize -= numBytes;
        }
        else
        {
            remainingMessageSize = 0;
            throw new TTransportException(TTransportException.END_OF_FILE, "MaxMessageSize reached");
        }
    }

}
