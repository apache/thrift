#include <ruby.h>
#include "protocol_transfer.h"

void protocol_transfer_free(protocol_transfer* pt)
{
	pt->free(pt);
}

void protocol_free(protocol_transfer* pt)
{
	free(pt);
}

protocol_transfer* protocol_transfer_allocate()
{
	protocol_transfer *pt = malloc(sizeof(protocol_transfer));

	pt->initialized = 0;
	pt->free = protocol_free;

	return pt; 
}

