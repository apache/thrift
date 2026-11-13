/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <set>
#include <string>
#include <vector>

#include "thrift/parse/t_program.h"
#include "thrift/parse/t_type.h"
#include "thrift/parse/t_typedef.h"

#include "thrift/main.h"

using std::string;
using std::vector;

t_type* t_type::get_true_type() {
  return const_cast<t_type*>(const_cast<const t_type*>(this)->get_true_type());
}

const t_type* t_type::get_true_type() const {
  const t_type* type = this;
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }
  return type;
}

namespace {

/**
 * Follows typedefs and container elements one step at a time. path holds the
 * types on the way here, so that a typedef reached again through itself is
 * reported instead: a plain cycle would send get_true_type() round forever,
 * and one through a container would send a generator recursing into the
 * element type until the stack overflows.
 * done holds the types already walked in full.
 */
void resolve_type(const t_type* type,
                  std::set<const t_type*>& path,
                  std::set<const t_type*>& done) {
  if (done.count(type) != 0) {
    return;
  }
  if (!path.insert(type).second) {
    throw std::string("Type \"") + type->get_name() + "\" refers to itself";
  }
  if (type->is_typedef()) {
    resolve_type(((const t_typedef*)type)->get_type(), path, done);
  } else if (type->is_list()) {
    resolve_type(((const t_list*)type)->get_elem_type(), path, done);
  } else if (type->is_set()) {
    resolve_type(((const t_set*)type)->get_elem_type(), path, done);
  } else if (type->is_map()) {
    resolve_type(((const t_map*)type)->get_key_type(), path, done);
    resolve_type(((const t_map*)type)->get_val_type(), path, done);
  }
  path.erase(type);
  done.insert(type);
}

void resolve_type(const t_struct* tstruct,
                  std::set<const t_type*>& path,
                  std::set<const t_type*>& done) {
  for (const t_field* field : tstruct->get_members()) {
    resolve_type(field->get_type(), path, done);
    // The fields of an xsd_attrs block belong to an anonymous struct that is
    // not one of the program's objects.
    if (field->get_xsd_attrs() != nullptr) {
      resolve_type(field->get_xsd_attrs(), path, done);
    }
  }
}

} // namespace

void t_program::resolve_types() const {
  std::set<const t_type*> path;
  std::set<const t_type*> done;
  for (const t_typedef* td : typedefs_) {
    resolve_type(td, path, done);
  }
  for (const t_const* c : consts_) {
    resolve_type(c->get_type(), path, done);
  }
  for (const t_struct* ts : objects_) {
    resolve_type(ts, path, done);
  }
  for (const t_service* service : services_) {
    for (const t_function* function : service->get_functions()) {
      resolve_type(function->get_returntype(), path, done);
      resolve_type(function->get_arglist(), path, done);
      resolve_type(function->get_xceptions(), path, done);
    }
  }
}

/**
 * Cleans up text commonly found in doxygen-like comments
 *
 * Warning: if you mix tabs and spaces in a non-uniform way,
 * you will get what you deserve.
 */
char* clean_up_doctext(char* doctext) {
  // Convert to C++ string, and remove Windows's carriage returns.
  string docstring = doctext;
  docstring.erase(remove(docstring.begin(), docstring.end(), '\r'), docstring.end());

  // Separate into lines.
  vector<string> lines;
  string::size_type pos = string::npos;
  string::size_type last;
  while (true) {
    last = (pos == string::npos) ? 0 : pos + 1;
    pos = docstring.find('\n', last);
    if (pos == string::npos) {
      // First bit of cleaning.  If the last line is only whitespace, drop it.
      string::size_type nonwhite = docstring.find_first_not_of(" \t", last);
      if (nonwhite != string::npos) {
        lines.push_back(docstring.substr(last));
      }
      break;
    }
    lines.push_back(docstring.substr(last, pos - last));
  }

  // A very profound docstring.
  if (lines.empty()) {
    return nullptr;
  }

  // Clear leading whitespace from the first line.
  pos = lines.front().find_first_not_of(" \t");
  lines.front().erase(0, pos);

  // If every nonblank line after the first has the same number of spaces/tabs,
  // then a star, remove them.
  bool have_prefix = true;
  bool found_prefix = false;
  string::size_type prefix_len = 0;
  vector<string>::iterator l_iter;
  for (l_iter = lines.begin() + 1; l_iter != lines.end(); ++l_iter) {
    if (l_iter->empty()) {
      continue;
    }

    pos = l_iter->find_first_not_of(" \t");
    if (!found_prefix) {
      if (pos != string::npos) {
        if (l_iter->at(pos) == '*') {
          found_prefix = true;
          prefix_len = pos;
        } else {
          have_prefix = false;
          break;
        }
      } else {
        // Whitespace-only line.  Truncate it.
        l_iter->clear();
      }
    } else if (l_iter->size() > pos && l_iter->at(pos) == '*' && pos == prefix_len) {
      // Business as usual.
    } else if (pos == string::npos) {
      // Whitespace-only line.  Let's truncate it for them.
      l_iter->clear();
    } else {
      // The pattern has been broken.
      have_prefix = false;
      break;
    }
  }

  // If our prefix survived, delete it from every line.
  if (have_prefix) {
    // Get the star too.
    prefix_len++;
    for (l_iter = lines.begin() + 1; l_iter != lines.end(); ++l_iter) {
      l_iter->erase(0, prefix_len);
    }
  }

  // Now delete the minimum amount of leading whitespace from each line.
  prefix_len = string::npos;
  for (l_iter = lines.begin() + 1; l_iter != lines.end(); ++l_iter) {
    if (l_iter->empty()) {
      continue;
    }
    pos = l_iter->find_first_not_of(" \t");
    if (pos != string::npos && (prefix_len == string::npos || pos < prefix_len)) {
      prefix_len = pos;
    }
  }

  // If our prefix survived, delete it from every line.
  if (prefix_len != string::npos) {
    for (l_iter = lines.begin() + 1; l_iter != lines.end(); ++l_iter) {
      l_iter->erase(0, prefix_len);
    }
  }

  // Remove trailing whitespace from every line.
  for (l_iter = lines.begin(); l_iter != lines.end(); ++l_iter) {
    pos = l_iter->find_last_not_of(" \t");
    if (pos != string::npos && pos != l_iter->length() - 1) {
      l_iter->erase(pos + 1);
    }
  }

  // If the first line is empty, remove it.
  // Don't do this earlier because a lot of steps skip the first line.
  if (lines.front().empty()) {
    lines.erase(lines.begin());
  }

  // Now rejoin the lines and copy them back into doctext.
  docstring.clear();
  for (l_iter = lines.begin(); l_iter != lines.end(); ++l_iter) {
    docstring += *l_iter;
    docstring += '\n';
  }

  // assert(docstring.length() <= strlen(doctext));  may happen, see THRIFT-1755
  if (docstring.length() <= strlen(doctext)) {
    strcpy(doctext, docstring.c_str());
  } else {
    free(doctext); // too short
    doctext = strdup(docstring.c_str());
  }
  return doctext;
}
