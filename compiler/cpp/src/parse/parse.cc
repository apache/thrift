#include "t_type.h"

#include "md5.h"

void t_type::generate_fingerprint() {
  std::string material = get_fingerprint_material();
  md5_state_t ctx;
  md5_init(&ctx);
  md5_append(&ctx, (md5_byte_t*)(material.data()), (int)material.size());
  md5_finish(&ctx, (md5_byte_t*)fingerprint_);
}
