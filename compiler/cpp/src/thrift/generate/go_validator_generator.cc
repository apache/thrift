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

/*
 * This file is programmatically sanitized for style:
 * astyle --style=1tbs -f -p -H -j -U go_validator_generator.cc
 *
 * The output of astyle should not be taken unquestioningly, but it is a good
 * guide for ensuring uniformity and readability.
 */

#include <fstream>
#include <iostream>
#include <limits>
#include <string>
#include <unordered_map>
#include <vector>

#include "thrift/generate/go_validator_generator.h"
#include "thrift/generate/validator_parser.h"
#include "thrift/platform.h"
#include "thrift/version.h"
#include <algorithm>
#include <clocale>
#include <sstream>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

std::string go_validator_generator::get_field_reference_name(t_field* field) {
  t_type* type(field->get_type());
  std::string tgt;
  t_const_value* def_value;
  go_generator->get_publicized_name_and_def_value(field, &tgt, &def_value);
  tgt = "p." + tgt;
  if (go_generator->is_pointer_field(field)
      && (type->is_base_type() || type->is_enum() || type->is_container())) {
    tgt = "*" + tgt;
  }
  return tgt;
}

void go_validator_generator::generate_struct_validator(std::ostream& out, t_struct* tstruct) {
  std::vector<t_field*> members = tstruct->get_members();
  validation_parser parser(tstruct);
  for (auto it = members.begin(); it != members.end(); it++) {
    t_field* field(*it);
    const std::vector<validation_rule*>& rules
        = parser.parse_field(field->get_type(), field->annotations_);
    if (rules.size() == 0) {
      continue;
    }
    bool opt = field->get_req() == t_field::T_OPTIONAL;
    t_type* type = field->get_type();
    std::string tgt = get_field_reference_name(field);
    std::string field_symbol = tstruct->get_name() + "." + field->get_name();
    generate_field_validator(out, generator_context{field_symbol, "", tgt, opt, type, rules});
  }
}

void go_validator_generator::generate_field_validator(std::ostream& out,
                                                      const generator_context& context) {
  t_type* type = context.type;
  if (type->is_typedef()) {
    type = type->get_true_type();
  }
  if (type->is_enum()) {
    if (context.tgt[0] == '*') {
      out << indent() << "if " << context.tgt.substr(1) << " != nil {" << '\n';
      indent_up();
    }
    generate_enum_field_validator(out, context);
    if (context.tgt[0] == '*') {
      indent_down();
      out << indent() << "}" << '\n';
    }
    return;
  } else if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    if (context.tgt[0] == '*') {
      out << indent() << "if " << context.tgt.substr(1) << " != nil {" << '\n';
      indent_up();
    }
    switch (tbase) {
    case t_base_type::TYPE_UUID:
    case t_base_type::TYPE_VOID:
      break;
    case t_base_type::TYPE_I8:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
      generate_integer_field_validator(out, context);
      break;
    case t_base_type::TYPE_DOUBLE:
      generate_double_field_validator(out, context);
      break;
    case t_base_type::TYPE_STRING:
      generate_string_field_validator(out, context);
      break;
    case t_base_type::TYPE_BOOL:
      generate_bool_field_validator(out, context);
      break;
    }
    if (context.tgt[0] == '*') {
      indent_down();
      out << indent() << "}" << '\n';
    }
    return;
  } else if (type->is_list()) {
    return generate_list_field_validator(out, context);
  } else if (type->is_set()) {
    return generate_set_field_validator(out, context);
  } else if (type->is_map()) {
    return generate_map_field_validator(out, context);
  } else if (type->is_struct() || type->is_xception()) {
    return generate_struct_field_validator(out, context);
  }
  throw "validator error: unsupported type: " + type->get_name();
}

void go_validator_generator::generate_enum_field_validator(std::ostream& out,
                                                           const generator_context& context) {
  for (auto it = context.rules.begin(); it != context.rules.end(); it++) {
    const std::vector<validation_value*>& values = (*it)->get_values();
    if (values.size() == 0) {
      continue;
    }
    std::string key = (*it)->get_name();

    if (key == "vt.in") {
      if (values.size() > 1) {
        std::string exist = GenID("_exist");
        out << indent() << "var " << exist << " bool" << '\n';

        std::string src = GenID("_src");
        out << indent() << src << " := []int64{";
        for (auto it = values.begin(); it != values.end(); it++) {
          if (it != values.begin()) {
            out << ", ";
          }
          out << "int64(";
          if ((*it)->is_field_reference()) {
            out << get_field_reference_name((*it)->get_field_reference());
          } else {
            out << (*it)->get_enum()->get_value();
          }
          out << ")";
        }
        out << "}" << '\n';

        out << indent() << "for _, src := range " << src << " {" << '\n';
        indent_up();
        out << indent() << "if int64(" << context.tgt << ") == src {" << '\n';
        indent_up();
        out << indent() << exist << " = true" << '\n';
        out << indent() << "break" << '\n';
        indent_down();
        out << indent() << "}" << '\n';
        indent_down();
        out << indent() << "}" << '\n';
        out << indent() << "if " << exist << " == false {" << '\n';
      } else {
        out << indent() << "if int64(" << context.tgt << ") != int64(";
        if (values[0]->is_field_reference()) {
          out << get_field_reference_name(values[0]->get_field_reference());
        } else {
          out << values[0]->get_enum()->get_value();
        }
        out << ") {" << '\n';
      }
      indent_up();
      out << indent()
          << "return thrift.NewValidationException(thrift.VALIDATION_FAILED, \"vt.in\", \""
          << context.field_symbol << "\", \"" << context.field_symbol
          << " not valid, rule vt.in check failed\")" << '\n';
      indent_down();
      out << indent() << "}" << '\n';
      if (values.size() > 1) {
        indent_down();
        out << indent() << "}" << '\n';
      }
    } else if (key == "vt.not_in") {
      if (values.size() > 1) {
        std::string src = GenID("_src");
        out << indent() << src << " := []int64{";
        for (auto it = values.begin(); it != values.end(); it++) {
          if (it != values.begin()) {
            out << ", ";
          }
          out << "int64(";
          if ((*it)->is_field_reference()) {
            out << get_field_reference_name((*it)->get_field_reference());
          } else {
            out << (*it)->get_enum()->get_value();
          }
          out << ")";
        }
        out << "}" << '\n';

        out << indent() << "for _, src := range " << src << " {" << '\n';
        indent_up();
        out << indent() << "if int64(" << context.tgt << ") == src {" << '\n';
      } else {
        out << indent() << "if int64(" << context.tgt << ") == ";
        out << "int64(";
        if (values[0]->is_field_reference()) {
          out << get_field_reference_name(values[0]->get_field_reference());
        } else {
          out << values[0]->get_enum()->get_value();
        }
        out << ") {" << '\n';
      }
      indent_up();
      out << indent()
          << "return thrift.NewValidationException(thrift.VALIDATION_FAILED, \"vt.not_in\", \""
          << context.field_symbol << "\", \"" << context.field_symbol
          << " not valid, rule vt.not_in check failed\")" << '\n';
      indent_down();
      out << indent() << "}" << '\n';
      if (values.size() > 1) {
        indent_down();
        out << indent() << "}" << '\n';
      }
    } else if (key == "vt.defined_only") {
      if (values[0]->get_bool()) {
        out << indent() << "if (" << context.tgt << ").String() == \"<UNSET>\" ";
      } else {
        continue;
      }
      out << "{" << '\n';
      indent_up();
      out << indent()
          << "return thrift.NewValidationException(thrift.VALIDATION_FAILED, \"" + key + "\", \""
          << context.field_symbol << "\", \"" << context.field_symbol << " not valid, rule " << key
          << " check failed\")" << '\n';
      indent_down();
      out << indent() << "}" << '\n';
    }
  }
}

void go_validator_generator::generate_bool_field_validator(std::ostream& out,
                                                           const generator_context& context) {
  for (auto it = context.rules.begin(); it != context.rules.end(); it++) {
    const std::vector<validation_value*>& values = (*it)->get_values();
    if (values.size() == 0) {
      continue;
    }
    std::string key = (*it)->get_name();

    if (key == "vt.const") {
      out << indent() << "if " << context.tgt << " != ";
      if (values[0]->is_field_reference()) {
        out << get_field_reference_name(values[0]->get_field_reference());
      } else {
        if (values[0]->get_bool()) {
          out << "true";
        } else {
          out << "false";
        }
      }
    }
    out << "{" << '\n';
    indent_up();
    out << indent()
        << "return thrift.NewValidationException(thrift.VALIDATION_FAILED, \"" + key + "\", \""
        << context.field_symbol << "\", \"" << context.field_symbol << " not valid, rule " << key
        << " check failed\")" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
  }
}

void go_validator_generator::generate_double_field_validator(std::ostream& out,
                                                             const generator_context& context) {
  for (auto it = context.rules.begin(); it != context.rules.end(); it++) {
    const std::vector<validation_value*>& values = (*it)->get_values();
    if (values.size() == 0) {
      continue;
    }

    std::map<std::string, std::string> signs{{"vt.lt", ">="},
                                             {"vt.le", ">"},
                                             {"vt.gt", "<="},
                                             {"vt.ge", "<"}};
    std::string key = (*it)->get_name();
    auto key_it = signs.find(key);
    if (key_it != signs.end()) {
      out << indent() << "if " << context.tgt << " " << key_it->second << " ";
      if (values[0]->is_field_reference()) {
        out << get_field_reference_name(values[0]->get_field_reference());
      } else {
        out << values[0]->get_double();
      }
      out << "{" << '\n';
      indent_up();
      out << indent()
          << "return thrift.NewValidationException(thrift.VALIDATION_FAILED, \"" + key + "\", \""
          << context.field_symbol << "\", \"" << context.field_symbol << " not valid, rule " << key
          << " check failed\")" << '\n';
      indent_down();
      out << indent() << "}" << '\n';
      continue;
    } else if (key == "vt.in") {
      if (values.size() > 1) {
        std::string exist = GenID("_exist");
        out << indent() << "var " << exist << " bool" << '\n';

        std::string src = GenID("_src");
        out << indent() << src << " := []float64{";
        for (auto it = values.begin(); it != values.end(); it++) {
          if (it != values.begin()) {
            out << ", ";
          }
          if ((*it)->is_field_reference()) {
            out << get_field_reference_name((*it)->get_field_reference());
          } else {
            out << (*it)->get_double();
          }
        }
        out << "}" << '\n';

        out << indent() << "for _, src := range " << src << " {" << '\n';
        indent_up();
        out << indent() << "if " << context.tgt << " == src {" << '\n';
        indent_up();
        out << indent() << exist << " = true" << '\n';
        out << indent() << "break" << '\n';
        indent_down();
        out << indent() << "}" << '\n';
        indent_down();
        out << indent() << "}" << '\n';
        out << indent() << "if " << exist << " == false {" << '\n';
      } else {
        out << indent() << "if " << context.tgt << " != ";
        if (values[0]->is_field_reference()) {
          out << get_field_reference_name(values[0]->get_field_reference());
        } else {
          out << values[0]->get_double();
        }
        out << "{" << '\n';
      }

      indent_up();
      out << indent()
          << "return thrift.NewValidationException(thrift.VALIDATION_FAILED, \"vt.in\", \""
          << context.field_symbol << "\", \"" << context.field_symbol
          << " not valid, rule vt.in check failed\")" << '\n';
      indent_down();
      out << indent() << "}" << '\n';
    } else if (key == "vt.not_in") {
      if (values.size() > 1) {
        std::string src = GenID("_src");
        out << indent() << src << " := []float64{";
        for (auto it = values.begin(); it != values.end(); it++) {
          if (it != values.begin()) {
            out << ", ";
          }
          if ((*it)->is_field_reference()) {
            out << get_field_reference_name((*it)->get_field_reference());
          } else {
            out << (*it)->get_double();
          }
        }
        out << "}" << '\n';

        out << indent() << "for _, src := range " << src << " {" << '\n';
        indent_up();
        out << indent() << "if " << context.tgt << " == src {" << '\n';
      } else {
        out << indent() << "if " << context.tgt << " == ";
        if (values[0]->is_field_reference()) {
          out << get_field_reference_name(values[0]->get_field_reference());
        } else {
          out << values[0]->get_double();
        }
        out << "{" << '\n';
      }
      indent_up();
      out << indent()
          << "return thrift.NewValidationException(thrift.VALIDATION_FAILED, \"vt.not_in\", \""
          << context.field_symbol << "\", \"" << context.field_symbol
          << " not valid, rule vt.not_in check failed\")" << '\n';
      indent_down();
      out << indent() << "}" << '\n';
      if (values.size() > 1) {
        indent_down();
        out << indent() << "}" << '\n';
      }
    }
  }
}

void go_validator_generator::generate_integer_field_validator(std::ostream& out,
                                                              const generator_context& context) {
  auto generate_current_type = [](std::ostream& out, t_type* type) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_I8:
      out << "int8";
      break;
    case t_base_type::TYPE_I16:
      out << "int16";
      break;
    case t_base_type::TYPE_I32:
      out << "int32";
      break;
    case t_base_type::TYPE_I64:
      out << "int64";
      break;
    default:
      throw "validator error: unsupported integer type: " + type->get_name();
    }
  };

  for (auto it = context.rules.begin(); it != context.rules.end(); it++) {
    const std::vector<validation_value*>& values = (*it)->get_values();
    if (values.size() == 0) {
      continue;
    }

    std::map<std::string, std::string> signs{{"vt.lt", ">="},
                                             {"vt.le", ">"},
                                             {"vt.gt", "<="},
                                             {"vt.ge", "<"}};
    std::string key = (*it)->get_name();
    auto key_it = signs.find(key);
    if (key_it != signs.end()) {
      out << indent() << "if " << context.tgt << " " << key_it->second << " ";
      if (values[0]->is_field_reference()) {
        out << get_field_reference_name(values[0]->get_field_reference());
      } else if (values[0]->is_validation_function()) {
        generate_current_type(out, context.type);
        out << "(";
        validation_value::validation_function* func = values[0]->get_function();
        if (func->name == "len") {
          out << "len(";
          if (func->arguments[0]->is_field_reference()) {
            out << get_field_reference_name(func->arguments[0]->get_field_reference());
          }
          out << ")";
        }
        out << ")";
      } else {
        out << values[0]->get_int();
      }
      out << "{" << '\n';
      indent_up();
      out << indent()
          << "return thrift.NewValidationException(thrift.VALIDATION_FAILED, \"" + key + "\", \""
          << context.field_symbol << "\", \"" << context.field_symbol << " not valid, rule " << key
          << " check failed\")" << '\n';
      indent_down();
      out << indent() << "}" << '\n';
    } else if (key == "vt.in") {
      if (values.size() > 1) {
        std::string exist = GenID("_exist");
        out << indent() << "var " << exist << " bool" << '\n';

        std::string src = GenID("_src");
        out << indent() << src << " := []";
        generate_current_type(out, context.type);
        out << "{";
        for (auto it = values.begin(); it != values.end(); it++) {
          if (it != values.begin()) {
            out << ", ";
          }
          if ((*it)->is_field_reference()) {
            out << get_field_reference_name((*it)->get_field_reference());
          } else if ((*it)->is_validation_function()) {
            generate_current_type(out, context.type);
            out << "(";
            validation_value::validation_function* func = (*it)->get_function();
            if (func->name == "len") {
              out << "len(";
              if (func->arguments[0]->is_field_reference()) {
                out << get_field_reference_name(func->arguments[0]->get_field_reference());
              }
              out << ")";
            }
            out << ")";
          } else {
            out << (*it)->get_int();
          }
        }
        out << "}" << '\n';

        out << indent() << "for _, src := range " << src << " {" << '\n';
        indent_up();
        out << indent() << "if " << context.tgt << " == src {" << '\n';
        indent_up();
        out << indent() << exist << " = true" << '\n';
        out << indent() << "break" << '\n';
        indent_down();
        out << indent() << "}" << '\n';
        indent_down();
        out << indent() << "}" << '\n';
        out << indent() << "if " << exist << " == false {" << '\n';
      } else {
        out << indent() << "if " << context.tgt << " != ";
        if (values[0]->is_field_reference()) {
          out << get_field_reference_name(values[0]->get_field_reference());
        } else if (values[0]->is_validation_function()) {
          generate_current_type(out, context.type);
          out << "(";
          validation_value::validation_function* func = values[0]->get_function();
          if (func->name == "len") {
            out << "len(";
            if (func->arguments[0]->is_field_reference()) {
              out << get_field_reference_name(func->arguments[0]->get_field_reference());
            }
            out << ")";
          }
          out << ")";
        } else {
          out << values[0]->get_int();
        }
        out << "{" << '\n';
      }
      indent_up();
      out << indent()
          << "return thrift.NewValidationException(thrift.VALIDATION_FAILED, \"vt.in\", \""
          << context.field_symbol << "\", \"" << context.field_symbol
          << " not valid, rule vt.in check failed\")" << '\n';
      indent_down();
      out << indent() << "}" << '\n';
    } else if (key == "vt.not_in") {
      if (values.size() > 1) {
        std::string src = GenID("_src");
        out << indent() << src << " := []";
        t_base_type::t_base tbase = ((t_base_type*)context.type)->get_base();
        switch (tbase) {
        case t_base_type::TYPE_I8:
          out << "int8";
          break;
        case t_base_type::TYPE_I16:
          out << "int16";
          break;
        case t_base_type::TYPE_I32:
          out << "int32";
          break;
        case t_base_type::TYPE_I64:
          out << "int64";
          break;
        default:
          throw "validator error: unsupported integer type: " + context.type->get_name();
        }
        out << "{";
        for (auto it = values.begin(); it != values.end(); it++) {
          if (it != values.begin()) {
            out << ", ";
          }
          if ((*it)->is_field_reference()) {
            out << get_field_reference_name((*it)->get_field_reference());
          } else if ((*it)->is_validation_function()) {
            generate_current_type(out, context.type);
            out << "(";
            validation_value::validation_function* func = (*it)->get_function();
            if (func->name == "len") {
              out << "len(";
              if (func->arguments[0]->is_field_reference()) {
                out << get_field_reference_name(func->arguments[0]->get_field_reference());
              }
              out << ")";
            }
            out << ")";
          } else {
            out << (*it)->get_int();
          }
        }
        out << "}" << '\n';

        out << indent() << "for _, src := range " << src << " {" << '\n';
        indent_up();
        out << indent() << "if " << context.tgt << " == src {" << '\n';
      } else {
        out << indent() << "if " << context.tgt << " == ";
        if (values[0]->is_field_reference()) {
          out << get_field_reference_name(values[0]->get_field_reference());
        } else if (values[0]->is_validation_function()) {
          generate_current_type(out, context.type);
          out << "(";
          validation_value::validation_function* func = values[0]->get_function();
          if (func->name == "len") {
            out << "len(";
            if (func->arguments[0]->is_field_reference()) {
              out << get_field_reference_name(func->arguments[0]->get_field_reference());
            }
            out << ")";
          }
          out << ")";
        } else {
          out << values[0]->get_int();
        }
        out << "{" << '\n';
      }
      indent_up();
      out << indent()
          << "return thrift.NewValidationException(thrift.VALIDATION_FAILED, \"vt.not_in\", \""
          << context.field_symbol << "\", \"" << context.field_symbol
          << " not valid, rule vt.not_in check failed\")" << '\n';
      indent_down();
      out << indent() << "}" << '\n';
      if (values.size() > 1) {
        indent_down();
        out << indent() << "}" << '\n';
      }
    }
  }
}

void go_validator_generator::generate_string_field_validator(std::ostream& out,
                                                             const generator_context& context) {
  std::string target = context.tgt;
  t_type* type = context.type;
  if (type->is_typedef()) {
    type = type->get_true_type();
  }
  if (type->is_binary()) {
    target = GenID("_tgt");
    out << indent() << target << " := "
        << "string(" << context.tgt << ")" << '\n';
  }
  for (auto it = context.rules.begin(); it != context.rules.end(); it++) {
    const std::vector<validation_value*>& values = (*it)->get_values();
    if (values.size() == 0) {
      continue;
    }
    std::string key = (*it)->get_name();

    if (key == "vt.const") {
      out << indent() << "if " << target << " != ";
      if (values[0]->is_field_reference()) {
        out << "string(";
        out << get_field_reference_name(values[0]->get_field_reference());
        out << ")";
      } else {
        out << "\"" << values[0]->get_string() << "\"";
      }
    } else if (key == "vt.min_size" || key == "vt.max_size") {
      out << indent() << "if len(" << target << ") ";
      if (key == "vt.min_size") {
        out << "<";
      } else {
        out << ">";
      }
      out << " int(";
      if (values[0]->is_field_reference()) {
        out << get_field_reference_name(values[0]->get_field_reference());
      } else if (values[0]->is_validation_function()) {
        validation_value::validation_function* func = values[0]->get_function();
        if (func->name == "len") {
          out << "len(";
          if (func->arguments[0]->is_field_reference()) {
            out << "string(";
            out << get_field_reference_name(values[0]->get_field_reference());
            out << ")";
          }
          out << ")";
        }
      } else {
        out << values[0]->get_int();
      }
      out << ")";
    } else if (key == "vt.pattern") {
      out << indent() << "if ok, _ := regexp.MatchString(" << target << ",";
      if (values[0]->is_field_reference()) {
        out << "string(";
        out << get_field_reference_name(values[0]->get_field_reference());
        out << ")";
      } else {
        out << "\"" << values[0]->get_string() << "\"";
      }
      out << "); ok ";
    } else if (key == "vt.prefix") {
      out << indent() << "if !strings.HasPrefix(" << target << ",";
      if (values[0]->is_field_reference()) {
        out << "string(";
        out << get_field_reference_name(values[0]->get_field_reference());
        out << ")";
      } else {
        out << "\"" << values[0]->get_string() << "\"";
      }
      out << ")";
    } else if (key == "vt.suffix") {
      out << indent() << "if !strings.HasSuffix(" << target << ",";
      if (values[0]->is_field_reference()) {
        out << "string(";
        out << get_field_reference_name(values[0]->get_field_reference());
        out << ")";
      } else {
        out << "\"" << values[0]->get_string() << "\"";
      }
      out << ")";
    } else if (key == "vt.contains") {
      out << indent() << "if !strings.Contains(" << target << ",";
      if (values[0]->is_field_reference()) {
        out << "string(";
        out << get_field_reference_name(values[0]->get_field_reference());
        out << ")";
      } else {
        out << "\"" << values[0]->get_string() << "\"";
      }
      out << ")";
    } else if (key == "vt.not_contains") {
      out << indent() << "if strings.Contains(" << target << ",";
      if (values[0]->is_field_reference()) {
        out << "string(";
        out << get_field_reference_name(values[0]->get_field_reference());
        out << ")";
      } else {
        out << "\"" << values[0]->get_string() << "\"";
      }
      out << ")";
    }
    out << "{" << '\n';
    indent_up();
    out << indent()
        << "return thrift.NewValidationException(thrift.VALIDATION_FAILED, \"" + key + "\", \""
        << context.field_symbol << "\", \"" << context.field_symbol << " not valid, rule " << key
        << " check failed\")" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
  }
}

void go_validator_generator::generate_set_field_validator(std::ostream& out,
                                                          const generator_context& context) {
  return generate_list_field_validator(out, context);
}

void go_validator_generator::generate_list_field_validator(std::ostream& out,
                                                           const generator_context& context) {
  for (auto it = context.rules.begin(); it != context.rules.end(); it++) {
    const std::vector<validation_value*>& values = (*it)->get_values();
    std::string key = (*it)->get_name();
    if (key == "vt.min_size" || key == "vt.max_size") {
      out << indent() << "if len(" << context.tgt << ")";
      if (key == "vt.min_size") {
        out << " < ";
      } else {
        out << " > ";
      }
      if (values[0]->is_field_reference()) {
        out << "int(";
        out << get_field_reference_name(values[0]->get_field_reference());
        out << ")";
      } else {
        out << values[0]->get_int();
      }
      out << "{" << '\n';
      indent_up();
      out << indent()
          << "return thrift.NewValidationException(thrift.VALIDATION_FAILED, \"" + key + "\", \""
          << context.field_symbol << "\", \"" << context.field_symbol << " not valid, rule " << key
          << " check failed\")" << '\n';
      indent_down();
      out << indent() << "}" << '\n';
    } else if (key == "vt.elem") {
      out << indent() << "for i := 0; i < len(" << context.tgt << ");i++ {" << '\n';
      indent_up();
      std::string src = GenID("_elem");
      out << indent() << src << " := " << context.tgt << "[i]" << '\n';
      t_type* elem_type;
      if (context.type->is_list()) {
        elem_type = ((t_list*)context.type)->get_elem_type();
      } else {
        elem_type = ((t_set*)context.type)->get_elem_type();
      }
      generator_context ctx{context.field_symbol + ".elem",
                            "",
                            src,
                            false,
                            elem_type,
                            std::vector<validation_rule*>{(*it)->get_inner()}};
      generate_field_validator(out, ctx);
      indent_down();
      out << indent() << "}" << '\n';
    }
  }
}

void go_validator_generator::generate_map_field_validator(std::ostream& out,
                                                          const generator_context& context) {
  for (auto it = context.rules.begin(); it != context.rules.end(); it++) {
    const std::vector<validation_value*>& values = (*it)->get_values();
    std::string key = (*it)->get_name();
    if (key == "vt.min_size" || key == "vt.max_size") {
      out << indent() << "if len(" << context.tgt << ")";
      if (key == "vt.min_size") {
        out << " < ";
      } else {
        out << " > ";
      }
      if (values[0]->is_field_reference()) {
        out << "int(";
        out << get_field_reference_name(values[0]->get_field_reference());
        out << ")";
      } else {
        out << values[0]->get_int();
      }
      out << "{" << '\n';
      indent_up();
      out << indent()
          << "return thrift.NewValidationException(thrift.VALIDATION_FAILED, \"" + key + "\", \""
          << context.field_symbol << "\", \"" << context.field_symbol << " not valid, rule " << key
          << " check failed\")" << '\n';
      indent_down();
      out << indent() << "}" << '\n';
    } else if (key == "vt.key") {
      std::string src = GenID("_key");
      out << indent() << "for " << src << " := range " << context.tgt << " {" << '\n';
      indent_up();
      generator_context ctx{context.field_symbol + ".key",
                            "",
                            src,
                            false,
                            ((t_map*)context.type)->get_key_type(),
                            std::vector<validation_rule*>{(*it)->get_inner()}};
      generate_field_validator(out, ctx);
      indent_down();
      out << indent() << "}" << '\n';
    } else if (key == "vt.value") {
      std::string src = GenID("_value");
      out << indent() << "for _, " << src << " := range " << context.tgt << " {" << '\n';
      indent_up();
      generator_context ctx{context.field_symbol + ".value",
                            "",
                            src,
                            false,
                            ((t_map*)context.type)->get_val_type(),
                            std::vector<validation_rule*>{(*it)->get_inner()}};
      generate_field_validator(out, ctx);
      indent_down();
      out << indent() << "}" << '\n';
    }
  }
}

void go_validator_generator::generate_struct_field_validator(std::ostream& out,
                                                             const generator_context& context) {
  bool generate_valid = true;
  validation_rule* last_valid_rule = nullptr;
  for (auto it = context.rules.begin(); it != context.rules.end(); it++) {
    const std::vector<validation_value*>& values = (*it)->get_values();
    if (values.size() == 0) {
      continue;
    }
    std::string key = (*it)->get_name();

    if (key == "vt.skip") {
      if (values[0]->is_field_reference() || !values[0]->get_bool()) {
        generate_valid = true;
      } else if (values[0]->get_bool()) {
        generate_valid = false;
      }
      last_valid_rule = *it;
    }
  }
  if (generate_valid) {
    if (last_valid_rule == nullptr) {
      out << indent() << "if err := " << context.tgt << ".Validate(); err != nil {" << '\n';
      indent_up();
      out << indent() << "return err" << '\n';
      indent_down();
      out << indent() << "}" << '\n';
    } else {
      const std::vector<validation_value*>& values = last_valid_rule->get_values();
      if (!values[0]->get_bool()) {
        out << indent() << "if err := " << context.tgt << ".Validate(); err != nil {" << '\n';
        indent_up();
        out << indent() << "return err" << '\n';
        indent_down();
        out << indent() << "}" << '\n';
      } else if (values[0]->is_field_reference()) {
        out << indent() << "if !";
        out << get_field_reference_name(values[0]->get_field_reference());
        out << "{" << '\n';
        indent_up();
        out << indent() << "if err := " << context.tgt << ".Validate(); err != nil {" << '\n';
        indent_up();
        out << indent() << "return err" << '\n';
        indent_down();
        out << indent() << "}" << '\n';
        indent_down();
        out << indent() << "}" << '\n';
      }
    }
  }
}
