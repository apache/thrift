// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_STRUCT_H
#define T_STRUCT_H

#include <vector>
#include <string>
#include <cstring>

#include "t_type.h"
#include "t_field.h"

// What's worse?  This, or making a src/parse/non_inlined.cc?
#include "md5.h"

// Forward declare that puppy
class t_program;

/**
 * A struct is a container for a set of member fields that has a name. Structs
 * are also used to implement exception types.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_struct : public t_type {
 public:
  t_struct(t_program* program) :
    t_type(program),
    is_xception_(false),
    xsd_all_(false)
  {
    memset(fingerprint_, 0, sizeof(fingerprint_));
  }

  t_struct(t_program* program, const std::string& name) :
    t_type(program, name),
    is_xception_(false),
    xsd_all_(false)
  {
    memset(fingerprint_, 0, sizeof(fingerprint_));
  }

  void set_name(const std::string& name) {
    name_ = name;
  }

  void set_xception(bool is_xception) {
    is_xception_ = is_xception;
  }

  void set_xsd_all(bool xsd_all) {
    xsd_all_ = xsd_all;
  }

  bool get_xsd_all() const {
    return xsd_all_;
  }

  void append(t_field* elem) {
    members_.push_back(elem);
  }

  const std::vector<t_field*>& get_members() {
    return members_;
  }

  bool is_struct() const {
    return !is_xception_;
  }

  bool is_xception() const {
    return is_xception_;
  }

  virtual std::string get_fingerprint_material() const {
    std::string rv = "{";
    std::vector<t_field*>::const_iterator m_iter;
    for (m_iter = members_.begin(); m_iter != members_.end(); ++m_iter) {
      rv += (**m_iter).get_fingerprint_material();
      rv += ";";
    }
    rv += "}";
    return rv;
  }

  // Fingerprint should change whenever (and only when)
  // the encoding via TDenseProtocol changes.
  static const int fingerprint_len = 16;

  // Call this before trying get_*_fingerprint().
  void generate_fingerprint() {
    std::string material = get_fingerprint_material();
    MD5_CTX ctx;
    MD5Init(&ctx);
    MD5Update(&ctx, (unsigned char*)(material.data()), material.size());
    MD5Final(fingerprint_, &ctx);
    //std::cout << get_name() << std::endl;
    //std::cout << material << std::endl;
    //std::cout << get_ascii_fingerprint() << std::endl << std::endl;
  }

  bool has_fingerprint() const {
    for (int i = 0; i < fingerprint_len; i++) {
      if (fingerprint_[i] != 0) {
        return true;
      }
    }
    return false;
  }

  const uint8_t* get_binary_fingerprint() const {
    return fingerprint_;
  }

  std::string get_ascii_fingerprint() const {
    std::string rv;
    const uint8_t* fp = get_binary_fingerprint();
    for (int i = 0; i < fingerprint_len; i++) {
      rv += byte_to_hex(fp[i]);
    }
    return rv;
  }

  // This function will break (maybe badly) unless 0 <= num <= 16.
  static char nybble_to_xdigit(int num) {
    if (num < 10) {
      return '0' + num;
    } else {
      return 'A' + num - 10;
    }
  }

  static std::string byte_to_hex(uint8_t byte) {
    std::string rv;
    rv += nybble_to_xdigit(byte >> 4);
    rv += nybble_to_xdigit(byte & 0x0f);
    return rv;
  }


 private:

  std::vector<t_field*> members_;
  bool is_xception_;

  bool xsd_all_;

  uint8_t fingerprint_[fingerprint_len];
};

#endif
