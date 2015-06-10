
extern crate podio;
extern crate bufstream;

use std::io;
use std::convert::From;

pub use protocol::Protocol;
pub use protocol::Error;
pub use transport::Transport;

pub mod protocol;
pub mod transport;
pub mod server;
pub mod processor;

#[derive(Debug)]
pub enum ThriftErr {
    /// An error occurred when reading from/writing to the underlying transport
    TransportError(io::Error),

    /// An error occurred when encoding/decoding the data
    /// (this usually indicates a bug in the library)
    ProtocolError(protocol::Error),

    /// The server code threw a user-defined exception
    UserException,
}

impl From<io::Error> for ThriftErr {
    fn from(err: io::Error) -> ThriftErr {
        ThriftErr::TransportError(err)
    }
}

impl From<protocol::Error> for ThriftErr {
    fn from(err: protocol::Error) -> ThriftErr {
        ThriftErr::ProtocolError(err)
    }
}

pub type TResult<T> = Result<T, ThriftErr>;
