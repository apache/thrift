#![feature(core)]
#![feature(io)]
#![feature(slicing_syntax)]

pub use protocol::Protocol;
pub use transport::Transport;

pub mod protocol;
pub mod transport;

#[derive(Eq, PartialEq, Debug)]
pub enum ThriftErr {
    TransportError(std::old_io::IoError),
    UnknownProtocol,
    InvalidData,
    NegativeSize,
    SizeLimit,
    BadVersion,
    NotImplemented,
    DepthLimit,
    InvalidUtf8(std::str::Utf8Error),
    Exception,
    ProtocolError,
}

impl std::error::FromError<std::old_io::IoError> for ThriftErr {
	fn from_error(err: std::old_io::IoError) -> ThriftErr {
		ThriftErr::TransportError(err)
	}
}

pub type TResult<T> = Result<T, ThriftErr>;
