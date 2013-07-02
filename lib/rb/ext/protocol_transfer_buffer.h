#ifndef _PROTOCOL_TRANSFER_BUFFER_H_
#define _PROTOCOL_TRANSFER_BUFFER_H_

#include "protocol_transfer.h"

#define BUFFER_LEN 256

void buffer_transfer_initialize(protocol_transfer* pt, VALUE transport, VALUE wbuf, VALUE rbuf);
protocol_transfer* buffer_transfer_create(VALUE transport, VALUE wbuf, VALUE rbuf);

#endif