// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

use log::warn;

use std::net::{TcpListener, ToSocketAddrs};
use std::sync::Arc;
use threadpool::ThreadPool;

#[cfg(unix)]
use std::os::unix::net::UnixListener;
#[cfg(unix)]
use std::path::Path;

use crate::protocol::{
    TInputProtocol, TInputProtocolFactory, TOutputProtocol, TOutputProtocolFactory,
};
use crate::transport::{TIoChannel, TReadTransportFactory, TTcpChannel, TWriteTransportFactory};
use crate::{ApplicationError, ApplicationErrorKind};

use super::TProcessor;
use crate::TransportErrorKind;

/// Fixed-size thread-pool blocking Thrift server.
///
/// A `TServer` listens on a given address and submits accepted connections
/// to an **unbounded** queue. Connections from this queue are serviced by
/// the first available worker thread from a **fixed-size** thread pool. Each
/// accepted connection is handled by that worker thread, and communication
/// over this thread occurs sequentially and synchronously (i.e. calls block).
/// Accepted connections have an input half and an output half, each of which
/// uses a `TTransport` and `TInputProtocol`/`TOutputProtocol` to translate
/// messages to and from byes. Any combination of `TInputProtocol`, `TOutputProtocol`
/// and `TTransport` may be used.
///
/// # Examples
///
/// Creating and running a `TServer` using Thrift-compiler-generated
/// service code.
///
/// ```no_run
/// use thrift::protocol::{TInputProtocolFactory, TOutputProtocolFactory};
/// use thrift::protocol::{TBinaryInputProtocolFactory, TBinaryOutputProtocolFactory};
/// use thrift::protocol::{TInputProtocol, TOutputProtocol};
/// use thrift::transport::{TBufferedReadTransportFactory, TBufferedWriteTransportFactory,
///                         TReadTransportFactory, TWriteTransportFactory};
/// use thrift::server::{TProcessor, TServer};
///
/// //
/// // auto-generated
/// //
///
/// // processor for `SimpleService`
/// struct SimpleServiceSyncProcessor;
/// impl SimpleServiceSyncProcessor {
///     fn new<H: SimpleServiceSyncHandler>(processor: H) -> SimpleServiceSyncProcessor {
///         unimplemented!();
///     }
/// }
///
/// // `TProcessor` implementation for `SimpleService`
/// impl TProcessor for SimpleServiceSyncProcessor {
///     fn process(&self, i: &mut dyn TInputProtocol, o: &mut dyn TOutputProtocol) -> thrift::Result<()> {
///         unimplemented!();
///     }
/// }
///
/// // service functions for SimpleService
/// trait SimpleServiceSyncHandler {
///     fn service_call(&self) -> thrift::Result<()>;
/// }
///
/// //
/// // user-code follows
/// //
///
/// // define a handler that will be invoked when `service_call` is received
/// struct SimpleServiceHandlerImpl;
/// impl SimpleServiceSyncHandler for SimpleServiceHandlerImpl {
///     fn service_call(&self) -> thrift::Result<()> {
///         unimplemented!();
///     }
/// }
///
/// // instantiate the processor
/// let processor = SimpleServiceSyncProcessor::new(SimpleServiceHandlerImpl {});
///
/// // instantiate the server
/// let i_tr_fact: Box<dyn TReadTransportFactory> = Box::new(TBufferedReadTransportFactory::new());
/// let i_pr_fact: Box<dyn TInputProtocolFactory> = Box::new(TBinaryInputProtocolFactory::new());
/// let o_tr_fact: Box<dyn TWriteTransportFactory> = Box::new(TBufferedWriteTransportFactory::new());
/// let o_pr_fact: Box<dyn TOutputProtocolFactory> = Box::new(TBinaryOutputProtocolFactory::new());
///
/// let mut server = TServer::new(
///     i_tr_fact,
///     i_pr_fact,
///     o_tr_fact,
///     o_pr_fact,
///     processor,
///     10
/// );
///
/// // start listening for incoming connections
/// match server.listen("127.0.0.1:8080") {
///   Ok(_)  => println!("listen completed"),
///   Err(e) => println!("listen failed with error {:?}", e),
/// }
/// ```
#[derive(Debug)]
pub struct TServer<PRC, RTF, IPF, WTF, OPF>
where
    PRC: TProcessor + Send + Sync + 'static,
    RTF: TReadTransportFactory + 'static,
    IPF: TInputProtocolFactory + 'static,
    WTF: TWriteTransportFactory + 'static,
    OPF: TOutputProtocolFactory + 'static,
{
    r_trans_factory: RTF,
    i_proto_factory: IPF,
    w_trans_factory: WTF,
    o_proto_factory: OPF,
    processor: Arc<PRC>,
    worker_pool: ThreadPool,
}

impl<PRC, RTF, IPF, WTF, OPF> TServer<PRC, RTF, IPF, WTF, OPF>
where
    PRC: TProcessor + Send + Sync + 'static,
    RTF: TReadTransportFactory + 'static,
    IPF: TInputProtocolFactory + 'static,
    WTF: TWriteTransportFactory + 'static,
    OPF: TOutputProtocolFactory + 'static,
{
    /// Create a `TServer`.
    ///
    /// Each accepted connection has an input and output half, each of which
    /// requires a `TTransport` and `TProtocol`. `TServer` uses
    /// `read_transport_factory` and `input_protocol_factory` to create
    /// implementations for the input, and `write_transport_factory` and
    /// `output_protocol_factory` to create implementations for the output.
    pub fn new(
        read_transport_factory: RTF,
        input_protocol_factory: IPF,
        write_transport_factory: WTF,
        output_protocol_factory: OPF,
        processor: PRC,
        num_workers: usize,
    ) -> TServer<PRC, RTF, IPF, WTF, OPF> {
        TServer {
            r_trans_factory: read_transport_factory,
            i_proto_factory: input_protocol_factory,
            w_trans_factory: write_transport_factory,
            o_proto_factory: output_protocol_factory,
            processor: Arc::new(processor),
            worker_pool: ThreadPool::with_name("Thrift service processor".to_owned(), num_workers),
        }
    }

    /// Listen for incoming connections on `listen_address`.
    ///
    /// `listen_address` should implement `ToSocketAddrs` trait.
    ///
    /// Return `()` if successful.
    ///
    /// Return `Err` when the server cannot bind to `listen_address` or there
    /// is an unrecoverable error.
    pub fn listen<A: ToSocketAddrs>(&mut self, listen_address: A) -> crate::Result<()> {
        let listener = TcpListener::bind(listen_address)?;
        for stream in listener.incoming() {
            match stream {
                Ok(s) => {
                    s.set_nodelay(true).ok();
                    let channel = TTcpChannel::with_stream(s);
                    self.handle_stream(channel)?;
                }
                Err(e) => {
                    warn!("failed to accept remote connection with error {:?}", e);
                }
            }
        }

        Err(crate::Error::Application(ApplicationError {
            kind: ApplicationErrorKind::Unknown,
            message: "aborted listen loop".into(),
        }))
    }

    /// Listen for incoming connections on `listen_path`.
    ///
    /// `listen_path` should implement `AsRef<Path>` trait.
    ///
    /// Return `()` if successful.
    ///
    /// Return `Err` when the server cannot bind to `listen_path` or there
    /// is an unrecoverable error.
    #[cfg(unix)]
    pub fn listen_uds<P: AsRef<Path>>(&mut self, listen_path: P) -> crate::Result<()> {
        let listener = UnixListener::bind(listen_path)?;
        for stream in listener.incoming() {
            match stream {
                Ok(s) => {
                    self.handle_stream(s)?;
                }
                Err(e) => {
                    warn!(
                        "failed to accept connection via unix domain socket with error {:?}",
                        e
                    );
                }
            }
        }

        Err(crate::Error::Application(ApplicationError {
            kind: ApplicationErrorKind::Unknown,
            message: "aborted listen loop".into(),
        }))
    }

    fn handle_stream<S: TIoChannel + Send + 'static>(&mut self, stream: S) -> crate::Result<()> {
        let (i_prot, o_prot) = self.new_protocols_for_connection(stream)?;
        let processor = self.processor.clone();
        self.worker_pool
            .execute(move || handle_incoming_connection(processor, i_prot, o_prot));
        Ok(())
    }

    fn new_protocols_for_connection<S: TIoChannel + Send + 'static>(
        &mut self,
        stream: S,
    ) -> crate::Result<(
        Box<dyn TInputProtocol + Send>,
        Box<dyn TOutputProtocol + Send>,
    )> {
        // split it into two - one to be owned by the
        // input tran/proto and the other by the output
        let (r_chan, w_chan) = stream.split()?;

        // input protocol and transport
        let r_tran = self.r_trans_factory.create(Box::new(r_chan));
        let i_prot = self.i_proto_factory.create(r_tran);

        // output protocol and transport
        let w_tran = self.w_trans_factory.create(Box::new(w_chan));
        let o_prot = self.o_proto_factory.create(w_tran);

        Ok((i_prot, o_prot))
    }
}

fn handle_incoming_connection<PRC>(
    processor: Arc<PRC>,
    i_prot: Box<dyn TInputProtocol>,
    o_prot: Box<dyn TOutputProtocol>,
) where
    PRC: TProcessor,
{
    let mut i_prot = i_prot;
    let mut o_prot = o_prot;
    loop {
        match processor.process(&mut *i_prot, &mut *o_prot) {
            Ok(()) => {}
            Err(err) => {
                match err {
                    crate::Error::Transport(ref transport_err)
                        if transport_err.kind == TransportErrorKind::EndOfFile => {}
                    other => warn!("processor completed with error: {:?}", other),
                }
                break;
            }
        }
    }
}
