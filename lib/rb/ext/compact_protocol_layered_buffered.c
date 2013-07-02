#include <ruby.h>
#include <stdbool.h>
#include <stdint.h>
#include <constants.h>
#include <struct.h>
#include <bytes.h>

#include "compact_protocol_layered.h"

#include "protocol_transfer.h"
#include "protocol_transfer_buffer.h"
#include "compact_protocol_layered.h"



static VALUE rb_initialize(VALUE self, VALUE transport)
{
  rb_call_super(1, &transport);

  char buf[BUFFER_LEN];
  VALUE rbuf = rb_str_new(buf, BUFFER_LEN);
  VALUE wbuf = rb_str_new(buf, BUFFER_LEN);
  rb_ivar_set(self, rb_intern("@rbuf"), rbuf); //Avoid garbage collect
  rb_ivar_set(self, rb_intern("@wbuf"), wbuf); //Avoid garbage collect

  get_cdata(self)->pt = buffer_transfer_create(transport, wbuf, rbuf);
  return self;
}

void Init_compact_protocol_layered_buffered() {

  VALUE thrift_layered_compact_protocol_class = rb_const_get(thrift_module, rb_intern("LayeredCompactProtocol"));

  VALUE bpa_class = rb_define_class_under(thrift_module, "BufferedLayeredCompactProtocol", thrift_layered_compact_protocol_class);

  rb_define_method(bpa_class, "initialize", rb_initialize, 1);
}