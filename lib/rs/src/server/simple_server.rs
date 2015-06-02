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

use transport::{Transport, TransportFactory};
use transport::server::ServerTransport;
use protocol::{Protocol, ProtocolFactory};
use processor::Processor;

pub struct SimpleServer<Proc, TF, PF, T, ST, P> {
    processor: Proc,
    protocol_factory: PF,
    transport_factory: TF,
    server_transport: ST,
    protocol: Option<P>,
    transport: Option<T>
}

impl<Proc: Processor<P, P>, TF: TransportFactory<T>, PF: ProtocolFactory<P>, T: Transport, ST: ServerTransport, P: Protocol> SimpleServer<Proc, TF, PF, T, ST, P> {
    pub fn new(processor: Proc, server_transport: ST, tf: TF, pf: PF) -> Self {
        SimpleServer { processor: processor, protocol_factory: pf, transport_factory: tf,
                       protocol: None, transport: None, server_transport: server_transport }
    }

    pub fn serve(&mut self) {
        loop {
            let mut transport = self.server_transport.accept().unwrap();
            let mut prot1 = self.protocol_factory.new_protocol();
            let mut prot2 = self.protocol_factory.new_protocol();
            loop {
                if self.processor.process(&mut prot1, &mut prot2, &mut transport).is_err() {
                    break;
                }
            }
        }
    }
}
