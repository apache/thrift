// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_DOC_H
#define T_DOC_H

/**
 * Documentation stubs
 *
 */
class t_doc {

 public:
  t_doc() : has_doc_(false) {}

  void set_doc(const std::string& doc) {
    doc_ = doc;
    has_doc_ = true;
  }

  const std::string& get_doc() const {
    return doc_;
  }

  bool has_doc() {
    return has_doc_;
  }

 private:
  std::string doc_;
  bool has_doc_;

};

#endif
