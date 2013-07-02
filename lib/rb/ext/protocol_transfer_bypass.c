
#include <ruby.h>
#include "protocol_transfer.h"

#define DEBUG 0

#if DEBUG
  #define DEBUG_FUNCTION_ENTRY() printf("bypassed %s\n", __FUNCTION__);
#else
  #define DEBUG_FUNCTION_ENTRY()
#endif

struct _bypass_data;
typedef struct _bypass_data bypass_data;

struct _bypass_data
{
	ID read_all_method_id;
	ID read_byte_method_id;
	ID write_method_id;

	VALUE transport;
	VALUE buffer;
};



static void protocol_free(protocol_transfer* pt)
{
	if (pt == NULL) return;
	free(pt->data);
	free(pt);
}

static int protocol_read(protocol_transfer* pt, char* buffer, int length)
{
	DEBUG_FUNCTION_ENTRY();
	bypass_data* data = (bypass_data*)pt->data;

	if (length == 0) return 0;

	if (length > 1)
	{
		VALUE buf = rb_funcall(data->transport, data->read_all_method_id, 1, INT2FIX(length));
		memcpy(buffer, RSTRING_PTR(buf), RSTRING_LEN(buf));

		return RSTRING_LEN(buf);
	}
	else
	{
		VALUE b = rb_funcall(data->transport, data->read_byte_method_id, 0);
  		*buffer = FIX2INT(b);

  		return 1;
	}
}

static void protocol_write(protocol_transfer* pt, char* buf, int length)
{
	bypass_data* data = (bypass_data*)pt->data;
	      
	rb_funcall(data->transport, data->write_method_id, 1, rb_str_new(buf, length));
}

static void protocol_flush(protocol_transfer* self)
{
	return;
}


void bypass_transfer_initialize(protocol_transfer* pt, VALUE transport)
{
	bypass_data* data = (bypass_data*)malloc(sizeof(bypass_data));


	pt->data = data;
	data->transport = transport;
	data->read_all_method_id = rb_intern("read_all");;
	data->write_method_id = rb_intern("write");
	data->read_byte_method_id =  rb_intern("read_byte");

	pt->free = protocol_free;
	pt->read = protocol_read;
	pt->write = protocol_write;
	pt->flush = protocol_flush;

	pt->initialized = 1;
}

protocol_transfer* bypass_transfer_create(VALUE transport)
{
	protocol_transfer* pt = malloc(sizeof(protocol_transfer));
	bypass_transfer_initialize(pt, transport);
	return pt;
}
