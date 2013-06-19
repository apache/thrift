#ifndef _PROTOCOL_TRANSFER_BUFFER_H_
#define _PROTOCOL_TRANSFER_BUFFER_H_

#include "protocol_transfer.h"


void buffer_transfer_initialize(protocol_transfer* pt, VALUE transport);
protocol_transfer* buffer_transfer_create(VALUE transport);

#endif