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
package org.apache.thrift;


public class TConfiguration {
    public static final int DEFAULT_MAX_MESSAGE_SIZE = 100 * 1024 * 1024;
    public static final int DEFAULT_MAX_FRAME_SIZE = 16384000;      // this value is used consistently across all Thrift libraries
    public static final int DEFAULT_RECURSION_DEPTH = 64;

    private int maxMessageSize;
    private int maxFrameSize;
    private int recursionLimit;

    public TConfiguration() {
        this(DEFAULT_MAX_MESSAGE_SIZE, DEFAULT_MAX_FRAME_SIZE, DEFAULT_RECURSION_DEPTH);
    }
    public TConfiguration(int maxMessageSize, int maxFrameSize, int recursionLimit) {
        this.maxFrameSize = maxFrameSize;
        this.maxMessageSize = maxMessageSize;
        this.recursionLimit = recursionLimit;
    }

    public int getMaxMessageSize() {
        return maxMessageSize;
    }

    public int getMaxFrameSize() {
        return maxFrameSize;
    }

    public int getRecursionLimit() {
        return recursionLimit;
    }

    public void setMaxMessageSize(int maxMessageSize) {
        this.maxMessageSize = maxMessageSize;
    }

    public void setMaxFrameSize(int maxFrameSize) {
        this.maxFrameSize = maxFrameSize;
    }

    public void setRecursionLimit(int recursionLimit) {
        this.recursionLimit = recursionLimit;
    }

    public static final TConfiguration DEFAULT = new Builder().build();

    public static TConfiguration.Builder custom() {
        return new Builder();
    }

    public static class Builder {
        private int maxMessageSize ;
        private int maxFrameSize;
        private int recursionLimit ;

        Builder() {
            super();
            this.maxFrameSize = DEFAULT_MAX_FRAME_SIZE;
            this.maxMessageSize = DEFAULT_MAX_MESSAGE_SIZE;
            this.recursionLimit = DEFAULT_RECURSION_DEPTH;
        }

        public Builder setMaxMessageSize(int maxMessageSize) {
            this.maxMessageSize = maxMessageSize;
            return this;
        }

        public Builder setMaxFrameSize(int maxFrameSize) {
            this.maxFrameSize = maxFrameSize;
            return this;
        }

        public Builder setRecursionLimit(int recursionLimit) {
            this.recursionLimit = recursionLimit;
            return this;
        }

        public TConfiguration build() {
            return new TConfiguration(maxMessageSize, maxFrameSize, recursionLimit);
        }
    }
}
