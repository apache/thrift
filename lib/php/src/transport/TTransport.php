<?php

/**
 * Base interface for a transport agent.
 *
 * @package thrift.transport
 * @author Mark Slee <mcslee@facebook.com>
 */
abstract class TTransport {

  /**
   * Whether this transport is open.
   *
   * @return boolean true if open
   */
  public abstract function isOpen();

  /**
   * Open the transport for reading/writing
   *
   * @throws TTransportException if cannot open
   */
  public abstract function open();

  /**
   * Close the transport.
   */
  public abstract function close();

  /**
   * Read some data into the array.
   *
   * @param int    $len How much to read
   * @return string The data that has been read
   * @throws TTransportException if cannot read any more data
   */
  public abstract function read($len);

  /**
   * Guarantees that the full amount of data is read.
   *
   * @return string The data, of exact length
   * @throws TTransportException if cannot read data
   */
  public function readAll($len) {
    // return $this->read($len);

    $data = '';
    $got = 0;
    while (($got = strlen($data)) < $len) {
      $data .= $this->read($len - $got);
    }
    return $data;
  }

  /**
   * Writes the given data out.
   *
   * @param string $buf  The data to write
   * @throws TTransportException if writing fails
   */
  public abstract function write($buf);

  /**
   * Flushes any pending data out of a buffer
   *
   * @throws TTransportException if a writing error occurs
   */
  public function flush() {}
}

?>
