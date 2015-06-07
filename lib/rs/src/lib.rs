#![feature(buf_stream)]

extern crate podio;

pub use protocol::Protocol;
pub use protocol::Error;
pub use transport::Transport;

pub mod protocol;
pub mod transport;
pub mod server;
pub mod processor;

#[derive(Debug)]
pub enum ThriftErr {
    TransportError(std::io::Error),
    ProtocolError1(protocol::Error),
    // Received a user-defined exception from the far end
    Exception,
}

impl std::convert::From<std::io::Error> for ThriftErr {
    fn from(err: std::io::Error) -> ThriftErr {
        ThriftErr::TransportError(err)
    }
}

impl std::convert::From<protocol::Error> for ThriftErr {
    fn from(err: protocol::Error) -> ThriftErr {
        ThriftErr::ProtocolError1(err)
    }
}

pub type TResult<T> = Result<T, ThriftErr>;
