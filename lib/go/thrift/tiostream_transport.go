/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package thrift

import (
  "bufio"
  "io"
  "os"
)

/**
 * This is the most commonly used base transport. It takes an InputStream
 * and an OutputStream and uses those to perform all transport operations.
 * This allows for compatibility with all the nice constructs Java already
 * has to provide a variety of types of streams.
 *
 */
type TIOStreamTransport struct {
  Reader       io.Reader
  Writer       io.Writer
  IsReadWriter bool
}

type TIOStreamTransportFactory struct {
  Reader       io.Reader
  Writer       io.Writer
  IsReadWriter bool
}

func (p *TIOStreamTransportFactory) GetTransport(trans TTransport) TTransport {
  if trans != nil {
    t, ok := trans.(*TIOStreamTransport)
    if ok {
      if t.IsReadWriter {
        return NewTIOStreamTransportRW(t.Reader.(io.ReadWriter))
      }
      if t.Reader != nil && t.Writer != nil {
        return NewTIOStreamTransportRAndW(t.Reader, t.Writer)
      }
      if t.Reader != nil && t.Writer == nil {
        return NewTIOStreamTransportR(t.Reader)
      }
      if t.Reader == nil && t.Writer != nil {
        return NewTIOStreamTransportW(t.Writer)
      }
      return NewTIOStreamTransportDefault()
    }
  }
  if p.IsReadWriter {
    return NewTIOStreamTransportRW(p.Reader.(io.ReadWriter))
  }
  if p.Reader != nil && p.Writer != nil {
    return NewTIOStreamTransportRAndW(p.Reader, p.Writer)
  }
  if p.Reader != nil && p.Writer == nil {
    return NewTIOStreamTransportR(p.Reader)
  }
  if p.Reader == nil && p.Writer != nil {
    return NewTIOStreamTransportW(p.Writer)
  }
  return NewTIOStreamTransportDefault()
}

func NewTIOStreamTransportFactory(reader io.Reader, writer io.Writer, isReadWriter bool) *TIOStreamTransportFactory {
  return &TIOStreamTransportFactory{Reader: reader, Writer: writer, IsReadWriter: isReadWriter}
}


/**
 * Subclasses can invoke the default constructor and then assign the input
 * streams in the open method.
 */
func NewTIOStreamTransportDefault() *TIOStreamTransport {
  return &TIOStreamTransport{}
}

/**
 * Input stream constructor.
 *
 * @param is Input stream to read from
 */
func NewTIOStreamTransportR(r io.Reader) *TIOStreamTransport {
  return &TIOStreamTransport{Reader: bufio.NewReader(r)}
}

/**
 * Output stream constructor.
 *
 * @param os Output stream to read from
 */
func NewTIOStreamTransportW(w io.Writer) *TIOStreamTransport {
  return &TIOStreamTransport{Writer: bufio.NewWriter(w)}
}

/**
 * Two-way stream constructor.
 *
 * @param is Input stream to read from
 * @param os Output stream to read from
 */
func NewTIOStreamTransportRAndW(r io.Reader, w io.Writer) *TIOStreamTransport {
  return &TIOStreamTransport{Reader: bufio.NewReader(r), Writer: bufio.NewWriter(w)}
}

/**
 * Two-way stream constructor.
 *
 * @param is Input stream to read from
 * @param os Output stream to read from
 */
func NewTIOStreamTransportRW(rw io.ReadWriter) *TIOStreamTransport {
  // bufio has a bug where once a Reader hits EOF, a new Write never brings the reader out of EOF
  // even if reader and writer use the same underlier
  //bufrw := bufio.NewReadWriter(bufio.NewReader(rw), bufio.NewWriter(rw));
  return &TIOStreamTransport{Reader: rw, Writer: rw, IsReadWriter: true}
}

/**
 * The streams must already be open at construction time, so this should
 * always return true.
 *
 * @return true
 */
func (p *TIOStreamTransport) IsOpen() bool {
  return true
}

/**
 * The streams must already be open. This method does nothing.
 */
func (p *TIOStreamTransport) Open() os.Error {
  return nil
}

func (p *TIOStreamTransport) Peek() bool {
  return p.IsOpen()
}

/**
 * Closes both the input and output streams.
 */
func (p *TIOStreamTransport) Close() os.Error {
  closedReader := false
  if p.Reader != nil {
    c, ok := p.Reader.(io.Closer)
    if ok {
      e := c.Close()
      closedReader = true
      if e != nil {
        LOGGER.Print("Error closing input stream.", e)
      }
    }
    p.Reader = nil
  }
  if p.Writer != nil && (!closedReader || !p.IsReadWriter) {
    c, ok := p.Writer.(io.Closer)
    if ok {
      e := c.Close()
      if e != nil {
        LOGGER.Print("Error closing output stream.", e)
      }
    }
    p.Writer = nil
  }
  return nil
}

/**
 * Reads from the underlying input stream if not null.
 */
func (p *TIOStreamTransport) Read(buf []byte) (int, os.Error) {
  if p.Reader == nil {
    return 0, NewTTransportException(NOT_OPEN, "Cannot read from null inputStream")
  }
  n, err := p.Reader.Read(buf)
  return n, NewTTransportExceptionFromOsError(err)
}

func (p *TIOStreamTransport) ReadAll(buf []byte) (int, os.Error) {
  return ReadAllTransport(p, buf)
}


/**
 * Writes to the underlying output stream if not null.
 */
func (p *TIOStreamTransport) Write(buf []byte) (int, os.Error) {
  if p.Writer == nil {
    LOGGER.Print("Could not write to iostream as Writer is null\n")
    return 0, NewTTransportException(NOT_OPEN, "Cannot write to null outputStream")
  }
  n, err := p.Writer.Write(buf)
  if n == 0 || err != nil {
    LOGGER.Print("Error writing to iostream, only wrote ", n, " bytes: ", err.String(), "\n")
  }
  return n, NewTTransportExceptionFromOsError(err)
}

/**
 * Flushes the underlying output stream if not null.
 */
func (p *TIOStreamTransport) Flush() os.Error {
  if p.Writer == nil {
    return NewTTransportException(NOT_OPEN, "Cannot flush null outputStream")
  }
  f, ok := p.Writer.(Flusher)
  if ok {
    err := f.Flush()
    if err != nil {
      return NewTTransportExceptionFromOsError(err)
    }
  }
  return nil
}
