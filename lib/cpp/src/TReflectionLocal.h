// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _THRIFT_TREFLECTIONLOCAL_H_
#define _THRIFT_TREFLECTIONLOCAL_H_ 1

#include <stdint.h>
#include <protocol/TProtocol.h>

namespace facebook { namespace thrift { namespace reflection { namespace local {

using facebook::thrift::protocol::TType;

/**
 * A local reflection is a representation of a Thrift structure.
 * (It is called local because it cannot be serialized by Thrift).
 *
 * @author David Reiss <dreiss@facebook.com>
 */

struct TypeSpec {
  // Use an anonymous union here so we can fit two TypeSpecs in one cache line.
  union {
    struct {
      // Use parallel arrays here for denser packing (of the arrays).
      int16_t*   ftags;
      TypeSpec** specs;
      int        n_fields;
    } tstruct;
    struct {
      TypeSpec *subtype1;
      TypeSpec *subtype2;
    } tcontainer;
  };

  // Put this at the end so the 32-bit enum can be crammed up next to the
  // 32-bit int (n_fields).
  TType ttype;


  // Static initialization of unions isn't really possible,
  // so take the plunge and use constructors.
  // Hopefully they'll be evaluated at compile time.

  TypeSpec(TType ttype) : ttype(ttype) {}

  TypeSpec(TType ttype, int n_fields, int16_t* ftags, TypeSpec** specs) :
    ttype(ttype)
  {
    tstruct.n_fields = n_fields;
    tstruct.ftags = ftags;
    tstruct.specs = specs;
  }

  TypeSpec(TType ttype, TypeSpec* subtype1, TypeSpec* subtype2) :
    ttype(ttype)
  {
    tcontainer.subtype1 = subtype1;
    tcontainer.subtype2 = subtype2;
  }

};

}}}} // facebook::thrift::reflection::local

#endif // #ifndef _THRIFT_TREFLECTIONLOCAL_H_
