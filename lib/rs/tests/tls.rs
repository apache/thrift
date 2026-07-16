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

#![cfg(feature = "rustls")]

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::{mpsc, Arc};
use std::thread;
use std::time::Duration;

use rustls::pki_types::ServerName;
use rustls::server::ResolvesServerCertUsingSni;
use rustls::{ClientConfig, RootCertStore, ServerConfig};
use thrift::transport::{TTlsClientChannel, TTlsServerChannel};

fn server_config() -> Arc<ServerConfig> {
    Arc::new(
        ServerConfig::builder_with_provider(Arc::new(rustls::crypto::ring::default_provider()))
            .with_safe_default_protocol_versions()
            .unwrap()
            .with_no_client_auth()
            .with_cert_resolver(Arc::new(ResolvesServerCertUsingSni::new())),
    )
}

fn client_config() -> Arc<ClientConfig> {
    Arc::new(
        ClientConfig::builder_with_provider(Arc::new(rustls::crypto::ring::default_provider()))
            .with_safe_default_protocol_versions()
            .unwrap()
            .with_root_certificates(RootCertStore::empty())
            .with_no_client_auth(),
    )
}

#[test]
fn client_with_nonblocking_stream_rejects_an_incomplete_handshake() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = listener.local_addr().unwrap();
    let (accepted_tx, accepted_rx) = mpsc::channel();
    let (release_tx, release_rx) = mpsc::channel();
    let server = thread::spawn(move || {
        let (mut stream, _) = listener.accept().unwrap();
        stream.write_all(&[0x16]).unwrap();
        stream.flush().unwrap();
        accepted_tx.send(()).unwrap();
        release_rx.recv_timeout(Duration::from_secs(5)).unwrap();
    });

    let stream = TcpStream::connect(address).unwrap();
    accepted_rx.recv_timeout(Duration::from_secs(5)).unwrap();
    let mut prefix = [0];
    assert_eq!(stream.peek(&mut prefix).unwrap(), 1);
    assert_eq!(prefix, [0x16]);
    stream.set_nonblocking(true).unwrap();
    let result = TTlsClientChannel::with_stream(
        stream,
        ServerName::try_from("localhost").unwrap(),
        client_config(),
    );

    release_tx.send(()).unwrap();
    server.join().unwrap();
    assert_incomplete_handshake_error(result, "client");
}

#[test]
fn client_reports_an_incomplete_nonblocking_handshake_without_progress() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = listener.local_addr().unwrap();
    let (accepted_tx, accepted_rx) = mpsc::channel();
    let (release_tx, release_rx) = mpsc::channel();
    let server = thread::spawn(move || {
        let (_stream, _) = listener.accept().unwrap();
        accepted_tx.send(()).unwrap();
        release_rx.recv_timeout(Duration::from_secs(5)).unwrap();
    });

    let stream = TcpStream::connect(address).unwrap();
    accepted_rx.recv_timeout(Duration::from_secs(5)).unwrap();
    stream.set_nonblocking(true).unwrap();
    let result = TTlsClientChannel::with_stream(
        stream,
        ServerName::try_from("localhost").unwrap(),
        client_config(),
    );

    release_tx.send(()).unwrap();
    server.join().unwrap();
    assert_incomplete_handshake_error(result, "client");
}

#[test]
fn server_handshake_rejects_an_incomplete_nonblocking_handshake() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = listener.local_addr().unwrap();
    let mut client = TcpStream::connect(address).unwrap();
    let (stream, _) = listener.accept().unwrap();
    client.write_all(&[0x16]).unwrap();
    client.flush().unwrap();
    let mut prefix = [0];
    assert_eq!(stream.peek(&mut prefix).unwrap(), 1);
    assert_eq!(prefix, [0x16]);
    stream.set_nonblocking(true).unwrap();

    let mut channel = TTlsServerChannel::with_stream(stream, server_config()).unwrap();
    assert_incomplete_handshake_error(channel.handshake(), "server");
}

#[test]
fn server_reports_an_incomplete_nonblocking_handshake_without_progress() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = listener.local_addr().unwrap();
    let _client = TcpStream::connect(address).unwrap();
    let (stream, _) = listener.accept().unwrap();
    stream.set_nonblocking(true).unwrap();

    let mut channel = TTlsServerChannel::with_stream(stream, server_config()).unwrap();
    assert_incomplete_handshake_error(channel.handshake(), "server");
}

#[test]
fn server_wraps_stream_without_waiting_for_a_handshake() {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = listener.local_addr().unwrap();
    let (wrapped_tx, wrapped_rx) = mpsc::channel();
    let server = thread::spawn(move || {
        let (stream, _) = listener.accept().unwrap();
        let mut channel = TTlsServerChannel::with_stream(stream, server_config()).unwrap();
        wrapped_tx.send(()).unwrap();
        let mut byte = [0];
        assert!(channel.read(&mut byte).is_err());
    });

    let probe = TcpStream::connect(address).unwrap();
    wrapped_rx.recv_timeout(Duration::from_secs(5)).unwrap();
    drop(probe);
    server.join().unwrap();
}

fn assert_incomplete_handshake_error<T>(result: thrift::Result<T>, role: &str) {
    let error = match result {
        Ok(_) => panic!("incomplete TLS handshake succeeded"),
        Err(error) => error,
    };
    match error {
        thrift::Error::Transport(error) => {
            assert_eq!(
                error.message,
                format!("TLS {role} handshake did not complete")
            );
        }
        error => panic!("unexpected error: {error:?}"),
    }
}
