#include "t_type.h"
#include "t_typedef.h"

#include "md5.h"
#include "main.h"

void t_type::generate_fingerprint() {
  if (! has_fingerprint()) {
    pdebug("generating fingerprint for %s", get_name().c_str());
    std::string material = get_fingerprint_material();
    md5_state_t ctx;
    md5_init(&ctx);
    md5_append(&ctx, (md5_byte_t*)(material.data()), (int)material.size());
    md5_finish(&ctx, (md5_byte_t*)fingerprint_);
  }
}

t_type* t_type::get_true_type() {
  t_type* type = this;
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }
  return type;
}
