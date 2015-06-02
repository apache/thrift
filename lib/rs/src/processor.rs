use protocol::Protocol;

pub trait Processor<P1: Protocol, P2: Protocol> {
    fn process(in_prot: P1, out_prot: P2);
}
