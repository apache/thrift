use protocol::Protocol;
use transport::Transport;
use TResult;

pub trait Processor<P1: Protocol, P2: Protocol, T: Transport> {
    fn process(&mut self, in_prot: &mut P1, out_prot: &mut P2, transport: &mut T) -> TResult<()>;
}
