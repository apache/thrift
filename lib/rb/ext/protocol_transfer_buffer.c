
#include <ruby.h>
#include "protocol_transfer.h"
#include "protocol_transfer_buffer.h"

#define DEBUG 0

#if DEBUG
  #define DEBUG_FUNCTION_ENTRY() printf("buffered %s\n", __FUNCTION__);
  #define DEBUG_FUNCTION_PROGRESS() printf("%s, %s:%d\n", __FILE__, __FUNCTION__, __LINE__);
#else
  #define DEBUG_FUNCTION_ENTRY()
  #define DEBUG_FUNCTION_PROGRESS() 
#endif

//#define LOG_FUNC()

struct _buffer_data;
typedef struct _buffer_data buffer_data;

static	ID read_into_buffer_method_id;
static	ID read_all_method_id;
static 	ID read_byte_method_id;
static 	ID write_method_id;
static 	ID available_method_id;



struct _buffer_data
{
	int write_bufer_idx;
	int read_buffer_idx;
	int read_buffer_sz;

	VALUE transport;
	VALUE rbuf;
	VALUE wbuf;
};

static int protocol_read(protocol_transfer* pt, char* buffer, int length);
static void protocol_write(protocol_transfer* pt, char* buf, int length);
static void protocol_flush(protocol_transfer* pt);




static void protocol_free(protocol_transfer* pt)
{
	if (pt != NULL) 
	{
		free(pt->data);
	}
	free(pt);
}

#define MIN(a,b) ((a)<(b)?(a):(b))

static int protocol_read(protocol_transfer* pt, char* buffer, int length)
{
	DEBUG_FUNCTION_ENTRY();
	buffer_data* data = (buffer_data*)pt->data;

	if (length == 0) return 0;

	//Do we have data in the read buffer?
	if (data->read_buffer_idx == data->read_buffer_sz)
	{
		//No, let's fetch some
		int available = NUM2INT(rb_funcall(data->transport, available_method_id, 0));
		int read_sz = (available < BUFFER_LEN) ? available : BUFFER_LEN;

		rb_funcall(data->transport, read_into_buffer_method_id, 2, data->rbuf, INT2FIX(read_sz));

		data->read_buffer_idx = 0;
		data->read_buffer_sz = read_sz;
	}

	int rsz = MIN(length, data->read_buffer_sz - data->read_buffer_idx);
	memcpy(buffer, RSTRING_PTR(data->rbuf) + data->read_buffer_idx, rsz);
	data->read_buffer_idx+= rsz;

	//Make sure we read everything that is requested, even if the buffer holds *some* data, but not enough.
	return rsz + protocol_read(pt, buffer + rsz, length - rsz);
}

static void protocol_write(protocol_transfer* pt, char* buf, int length)
{
	buffer_data* data = (buffer_data*)pt->data;

	if (length == 0) return;

	int tsize = MIN(length, BUFFER_LEN - data->write_bufer_idx);

	memcpy(RSTRING_PTR(data->wbuf) + data->write_bufer_idx, buf, tsize);
	data->write_bufer_idx += tsize;

	protocol_write(pt, buf + tsize, length - tsize);
}

static void protocol_flush(protocol_transfer* pt)
{
	buffer_data* data = (buffer_data*)pt->data;

	if (data->write_bufer_idx == RSTRING_LEN(data->wbuf))
		rb_funcall(data->transport, write_method_id, 1, data->wbuf);
	else
		rb_funcall(data->transport, write_method_id, 1, rb_str_new(RSTRING_PTR(data->wbuf), data->write_bufer_idx));

	data->write_bufer_idx = 0;
}


void buffer_transfer_initialize(protocol_transfer* pt, VALUE transport, VALUE rbuf, VALUE wbuf)
{
	buffer_data* data = (buffer_data*)malloc(sizeof(buffer_data));


	pt->data = data;
	data->transport = transport;
	read_all_method_id = rb_intern("read_all");;
	write_method_id = rb_intern("write");
	read_byte_method_id = rb_intern("read_byte");
	available_method_id = rb_intern("available");
	read_into_buffer_method_id = rb_intern("read_into_buffer");

	data->write_bufer_idx = 0;
	data->read_buffer_idx = 0;
	data->read_buffer_sz = 0;

	data->rbuf = rbuf;
	data->wbuf = wbuf;

	pt->free = protocol_free;
	pt->read = protocol_read;
	pt->write = protocol_write;
	pt->flush = protocol_flush;

	pt->initialized = 1;
}

protocol_transfer* buffer_transfer_create(VALUE transport, VALUE wbuf, VALUE rbuf)
{
	protocol_transfer* pt = malloc(sizeof(protocol_transfer));
	buffer_transfer_initialize(pt, transport, wbuf, rbuf);
	return pt;
}
