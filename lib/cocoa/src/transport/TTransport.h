@protocol TTransport <NSObject>

  /**
   * Guarantees that all of len bytes are read
   *
   * @param buf Buffer to read into
   * @param off Index in buffer to start storing bytes at
   * @param len Maximum number of bytes to read
   * @return The number of bytes actually read, which must be equal to len
   * @throws TTransportException if there was an error reading data
   */
- (int) readAll: (uint8_t *) buf offset: (int) off length: (int) len;

- (void) write: (const uint8_t *) data offset: (unsigned int) offset length: (unsigned int) length;

- (void) flush;
@end
