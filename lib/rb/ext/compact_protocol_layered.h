#ifndef COMPACT_PROTOCOL_LAYERED_H_
#define COMPACT_PROTOCOL_LAYERED_H_

#include <ruby.h>
#include <stdbool.h>
#include <stdint.h>
#include <constants.h>
#include <struct.h>
#include <bytes.h>

#include "protocol_transfer.h"

#define MAX_STRUCT_DEPTH 64

struct _compact_data;
 typedef struct _compact_data compact_data;

 struct _compact_data {
 	int last_field_id[MAX_STRUCT_DEPTH];
 	int last_field_index;


 	protocol_transfer* pt;

 	VALUE transport;

 	VALUE current_field_id;
 	VALUE current_field_type;
	VALUE current_field_value;

 	int current_field_active;
 };

 void Init_layered_compact_protocol();

compact_data* get_cdata(VALUE self);
void set_cdata(VALUE self, compact_data* cdata);


#endif