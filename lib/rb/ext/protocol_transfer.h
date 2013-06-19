#ifndef __PROTOCOL_TRANSFER_H_
#define __PROTOCOL_TRANSFER_H_

#include <ruby.h>

struct _protocol_transfer;
typedef struct _protocol_transfer protocol_transfer;

struct _protocol_transfer
{
	int initialized;
	void* data;

	void (*write)(protocol_transfer* self, char* data, int length);
	int (*read)(protocol_transfer* self, char* buffer, int length);
	void (*flush)(protocol_transfer* self);
	
	void (*free)(protocol_transfer* self);
};

protocol_transfer* protocol_transfer_allocate();
void protocol_transfer_free(protocol_transfer* pt);

#endif