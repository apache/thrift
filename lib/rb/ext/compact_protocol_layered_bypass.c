#include <ruby.h>
#include <stdbool.h>
#include <stdint.h>
#include <constants.h>
#include <struct.h>
#include <bytes.h>

#include "compact_protocol_layered.h"


#include "protocol_transfer.h"
#include "protocol_transfer_bypass.h"
#include "compact_protocol_layered.h"



static VALUE rb_initialize(VALUE self, VALUE transport)
{
  rb_call_super(1, &transport);
  get_cdata(self)->pt = bypass_transfer_create(transport);
  return self;
}

void Init_compact_protocol_layered_bypass() {

  VALUE thrift_layered_compact_protocol_class = rb_const_get(thrift_module, rb_intern("LayeredCompactProtocol"));

  VALUE bpa_class = rb_define_class_under(thrift_module, "BypassLayeredCompactProtocol", thrift_layered_compact_protocol_class);

  rb_define_method(bpa_class, "initialize", rb_initialize, 1);
}