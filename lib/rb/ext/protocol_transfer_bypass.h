#ifndef _PROTOCOL_TRANSFER_BYPASS_H_
#define _PROTOCOL_TRANSFER_BYPASS_H_

#include "protocol_transfer.h"


void bypass_transfer_initialize(protocol_transfer* pt, VALUE transport);
protocol_transfer* bypass_transfer_create(VALUE transport);

#endif