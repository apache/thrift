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

package org.apache.thrift.utils;

import java.util.Arrays;
import java.util.concurrent.TimeUnit;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import okhttp3.OkHttpClient;
import okhttp3.Protocol;

public class OkHttp3Utils {
    private static OkHttpClient client;

    private OkHttp3Util() {
    }

    public static OkHttpClient getClient(int connectTimeout, int readTimeout,
                                         SSLSocketFactory sslFactory,
                                         TrustManager trustManager) {
        if (client == null) {
            synchronized (OkHttp3Util.class) {
                if (client == null) {
                    // Create OkHttpClient builder
                    OkHttpClient.Builder clientBuilder = new OkHttpClient.Builder()
                            .connectTimeout(connectTimeout, TimeUnit.MILLISECONDS)
                            .writeTimeout(readTimeout, TimeUnit.MILLISECONDS)
                            .readTimeout(readTimeout, TimeUnit.MILLISECONDS);
                    if (sslFactory != null) {
                        clientBuilder.sslSocketFactory(sslFactory, (X509TrustManager) trustManager);
                        clientBuilder.protocols(Arrays.asList(Protocol.HTTP_2, Protocol.HTTP_1_1));
                    } else {
                        // config the http2 prior knowledge
                        clientBuilder.protocols(Arrays.asList(Protocol.H2_PRIOR_KNOWLEDGE));
                    }
                    client = clientBuilder.build();
                }
            }
        }
        return client;
    }

    public static void close() {
        if (client != null) {
            client.connectionPool().evictAll();
            client.dispatcher().executorService().shutdown();
            client = null;
        }
    }
}
