use std::io::{ IoResult, MemReader, MemWriter, Reader, Writer };
use transport::Transport;

pub struct FakeTransport {
  reader: MemReader,
  writer: MemWriter,
}

impl FakeTransport {
  pub fn new(buf: Vec<u8>) -> FakeTransport {
    FakeTransport {
      reader: MemReader::new(buf),
      writer: MemWriter::new(),
    }
  }
}

impl Writer for FakeTransport {
  fn write(&mut self, buf: &[u8]) -> IoResult<()> {
    self.writer.write(buf)
  }

  fn flush(&mut self) -> IoResult<()> {
    self.writer.flush()
  }
}

impl Reader for FakeTransport {
  fn read(&mut self, buf: &mut [u8]) -> IoResult<usize> {
    self.reader.read(buf)
  }
}

impl Transport for FakeTransport { }
