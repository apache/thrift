use std::io::{ Writer, Reader };

pub mod tcp_transport;

pub trait Transport : Writer + Reader { }

#[cfg(test)]
pub mod test;
