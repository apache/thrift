use std::net::TcpStream;

use protocol::Protocol;
use TResult;

pub trait Processor<P1: Protocol, P2: Protocol> {
    fn process(&mut self, in_prot: &mut P1, out_prot: &mut P2,
               transport: &mut TcpStream) -> TResult<()>;
}
