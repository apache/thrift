#ifndef T_LIST_H
#define T_LIST_H

#include "t_field.h"
#include <vector>

/**
 * List of elements.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_list {
 public:
  t_list() {}
  ~t_list() {}

  /** Add a new field to the list */
  void append(t_field* elem) { list_.push_back(elem); }

  /** Retrieve the list contents */
  const std::vector<t_field*>& elems() const { return list_; }

 private:
  std::vector<t_field*> list_;
};

#endif
