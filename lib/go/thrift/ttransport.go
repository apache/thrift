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
  "os"
  "log"
  "strconv"
)

type Flusher interface {
  Flush() (err os.Error)
}

/**
 * Generic class that encapsulates the I/O layer. This is basically a thin
 * wrapper around the combined functionality of Java input/output streams.
 *
 */
type TTransport interface {
  /**
   * Queries whether the transport is open.
   *
   * @return True if the transport is open.
   */
  IsOpen() bool

  /**
   * Opens the transport for reading/writing.
   *
   * @returns TTransportException if the transport could not be opened
   */
  Open() (err os.Error)

  /**
   * Closes the transport.
   */
  Close() (err os.Error)

  /**
   * Reads up to len bytes into buffer buf, starting att offset off.
   *
   * @param buf Array to read into
   * @param off Index to start reading at
   * @param len Maximum number of bytes to read
   * @return The number of bytes actually read
   * @return TTransportException if there was an error reading data
   */
  Read(buf []byte) (n int, err os.Error)

  /**
   * Guarantees that all of len bytes are actually read off the transport.
   *
   * @param buf Array to read into
   * @param off Index to start reading at
   * @param len Maximum number of bytes to read
   * @return The number of bytes actually read, which must be equal to len
   * @return TTransportException if there was an error reading data
   */
  ReadAll(buf []byte) (n int, err os.Error)

  /**
   * Writes the buffer to the output
   *
   * @param buf The output data buffer
   * @return Number of bytes written
   * @return TTransportException if an error occurs writing data
   */
  Write(buf []byte) (n int, err os.Error)

  /**
   * Flush any pending data out of a transport buffer.
   *
   * @return TTransportException if there was an error writing out data.
   */
  Flush() (err os.Error)

  /**
   * Is there more data to be read?
   *
   * @return True if the remote side is still alive and feeding us
   */
  Peek() bool
}
/*
type TTransportBase struct {
}

func (p* TTransportBase) IsOpen() bool {
  return false;
};

func (p* TTransportBase) Peek() bool {
  return p.IsOpen();
}

func (p* TTransportBase) Open() os.Error {
  return NewTTransportException(UNKNOWN, "Subclasses must implement TTransportBase.Open()");
}

func (p* TTransportBase) Close() os.Error {
  return NewTTransportException(UNKNOWN, "Subclasses must implement TTransportBase.Close()");
}

func (p* TTransportBase) Read(buf []byte) (int, os.Error) {
  return 0, NewTTransportExceptionDefaultString("Subclasses must implement TTransportBase.Read()");
}

func (p* TTransportBase) ReadAll(buf []byte) (n int, err os.Error){
  ret := 0;
  size := len(buf);
  for (n < size) {
    ret, err = p.Read(buf[n:]);
    if ret <= 0 {
      if err != nil {
        err = NewTTransportExceptionDefaultString("Cannot read. Remote side has closed. Tried to read " + string(size) + " bytes, but only got " + string(n) + " bytes.");
      }
      return ret, err;
    }
    n += ret;
  }
  return n, err;
}

func (p* TTransportBase) Write(buf []byte) (int, os.Error) {
  return 0, NewTTransportExceptionDefaultString("Subclasses must implement TTransportBase.Write()");
}

func (p* TTransportBase) Flush() os.Error {
  return nil;
}
*/
/**
 * Guarantees that all of len bytes are actually read off the transport.
 *
 * @param buf Array to read into
 * @param off Index to start reading at
 * @param len Maximum number of bytes to read
 * @return The number of bytes actually read, which must be equal to len
 * @return TTransportException if there was an error reading data
 */
func ReadAllTransport(p TTransport, buf []byte) (n int, err os.Error) {
  ret := 0
  size := len(buf)
  for n < size {
    ret, err = p.Read(buf[n:])
    if ret <= 0 {
      if err != nil {
        err = NewTTransportExceptionDefaultString("Cannot read. Remote side has closed. Tried to read " + strconv.Itoa(size) + " bytes, but only got " + strconv.Itoa(n) + " bytes.")
      }
      return ret, err
    }
    n += ret
  }
  return n, err
}


var (
  LOGGER *log.Logger
)

func init() {
  LOGGER = log.New(os.Stderr, "", log.Ldate|log.Ltime|log.Lshortfile)
}
