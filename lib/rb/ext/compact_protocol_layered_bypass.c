#include <ruby.h>
#include <stdbool.h>
#include <stdint.h>
#include <constants.h>
#include <struct.h>
#include <bytes.h>

//#define LOG_FUNC() printf("Layered %s\n", __FUNCTION__);
#define LOG_FUNC()

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

void Init_layered_bypass_compact_protocol() {

  VALUE thrift_layered_compact_protocol_class = rb_const_get(thrift_module, rb_intern("LayeredCompactProtocol"));

  VALUE bpa_class = rb_define_class_under(thrift_module, "BypassLayeredCompactProtocol", thrift_layered_compact_protocol_class);

  rb_define_method(bpa_class, "initialize", rb_initialize, 1);
}