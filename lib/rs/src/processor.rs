use protocol::Protocol;
use transport::Transport;
use TResult;

pub trait Processor<P: Protocol, T: Transport> {
    fn process(&mut self, prot: &mut P, transport: &mut T) -> TResult<()>;
}
