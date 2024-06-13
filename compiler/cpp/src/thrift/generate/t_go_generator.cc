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
 * astyle --style=1tbs -f -p -H -j -U t_go_generator.cc
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

#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sstream>
#include <algorithm>
#include <clocale>
#include "thrift/platform.h"
#include "thrift/version.h"
#include "thrift/generate/t_generator.h"
#include "thrift/generate/t_go_generator.h"
#include "thrift/generate/go_validator_generator.h"

using std::map;
using std::ostream;
using std::ostringstream;
using std::string;
using std::stringstream;
using std::vector;

/**
 * A helper for automatically formatting the emitted Go code from the Thrift
 * IDL per the Go style guide.
 *
 * Returns:
 *  - true, if the formatting process succeeded.
 *  - false, if the formatting process failed, which means the basic output was
 *           still generated.
 */
bool format_go_output(const string& file_path);

// returns true if field initialization can be omitted since it has corresponding go type zero value
// or default value is not set
bool t_go_generator::omit_initialization(t_field* tfield) {
  t_const_value* value = tfield->get_value();
  if (!value) {
    return true;
  }
  t_type* type = tfield->get_type()->get_true_type();
  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();

    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "";

    case t_base_type::TYPE_STRING:
      if (type->is_binary()) {
        //[]byte are always inline
        return false;
      }
      // strings are pointers if has no default
      return value->get_string().empty();

    case t_base_type::TYPE_BOOL:
    case t_base_type::TYPE_I8:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
      return value->get_integer() == 0;
    case t_base_type::TYPE_DOUBLE:
      if (value->get_type() == t_const_value::CV_INTEGER) {
        return value->get_integer() == 0;
      } else {
        return value->get_double() == 0.;
      }

    case t_base_type::TYPE_UUID:
      // it's hard to detect all zero uuid here, so just always inline it.
      return false;

    default:
      throw "compiler error: unhandled type";
    }
  }
  return false;
}

// Returns true if the type need a reference if used as optional without default
static bool type_need_reference(t_type* type) {
  type = type->get_true_type();
  if (type->is_map() || type->is_set() || type->is_list() || type->is_struct()
      || type->is_xception() || type->is_binary()) {
    return false;
  }
  return true;
}

// returns false if field could not use comparison to default value as !IsSet*
bool t_go_generator::is_pointer_field(t_field* tfield, bool in_container_value) {
  (void)in_container_value;
  if (tfield->annotations_.count("cpp.ref") != 0) {
    return true;
  }
  t_type* type = tfield->get_type()->get_true_type();
  // Structs in containers are pointers
  if (type->is_struct() || type->is_xception()) {
    return true;
  }
  if (!(tfield->get_req() == t_field::T_OPTIONAL)) {
    return false;
  }
  bool has_default = tfield->get_value();
  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();

    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "";

    case t_base_type::TYPE_STRING:
      if (type->is_binary()) {
        //[]byte are always inline
        return false;
      }
      // strings are pointers if has no default
      return !has_default;

    case t_base_type::TYPE_BOOL:
    case t_base_type::TYPE_I8:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
    case t_base_type::TYPE_DOUBLE:
    case t_base_type::TYPE_UUID:
      return !has_default;

    default:
      break;
    }
  } else if (type->is_enum()) {
    return !has_default;
  } else if (type->is_struct() || type->is_xception()) {
    return true;
  } else if (type->is_map()) {
    return has_default;
  } else if (type->is_set()) {
    return has_default;
  } else if (type->is_list()) {
    return has_default;
  } else if (type->is_typedef()) {
    return has_default;
  }

  throw "INVALID TYPE IN type_to_go_type: " + type->get_name();
}

std::string t_go_generator::camelcase(const std::string& value) const {
  std::string value2(value);
  std::setlocale(LC_ALL, "C"); // set locale to classic

  // Fix common initialism in first word
  fix_common_initialism(value2, 0);

  // as long as we are changing things, let's change _ followed by lowercase to
  // capital and fix common initialisms
  for (std::string::size_type i = 1; i < value2.size() - 1; ++i) {
    if (value2[i] == '_') {
      if (islower(value2[i + 1])) {
        value2.replace(i, 2, 1, toupper(value2[i + 1]));
      }

      if (i > static_cast<std::string::size_type>(std::numeric_limits<int>().max())) {
        throw "integer overflow in t_go_generator::camelcase, value = " + value;
      }
      fix_common_initialism(value2, static_cast<int>(i));
    }
  }

  return value2;
}

// Checks to see if the word starting at i in value contains a common initialism
// and if so replaces it with the upper case version of the word.
void t_go_generator::fix_common_initialism(std::string& value, int i) const {
  if (!ignore_initialisms_) {
    size_t wordLen = value.find('_', i);
    if (wordLen != std::string::npos) {
      wordLen -= i;
    }
    std::string word = value.substr(i, wordLen);
    std::transform(word.begin(), word.end(), word.begin(), ::toupper);
    if (commonInitialisms.find(word) != commonInitialisms.end()) {
      value.replace(i, word.length(), word);
    }
  }
}

std::string t_go_generator::publicize(const std::string& value, bool is_args_or_result, const std::string& service_name) const {
  if (value.size() <= 0) {
    return value;
  }

  std::string value2(value), prefix;

  string::size_type dot_pos = value.rfind('.');
  if (dot_pos != string::npos) {
    prefix = value.substr(0, dot_pos + 1) + prefix;
    value2 = value.substr(dot_pos + 1);
  }

  if (!isupper(value2[0])) {
    value2[0] = toupper(value2[0]);
  }

  value2 = camelcase(value2);

  // final length before further checks, the string may become longer
  size_t len_before = value2.length();

  // IDL identifiers may start with "New" which interferes with the CTOR pattern
  // Adding an extra underscore to all those identifiers solves this
  if ((len_before >= 3) && (value2.substr(0, 3) == "New")) {
    value2 += '_';
  }

  // IDL identifiers may end with "Args"/"Result" which interferes with the implicit service
  // function structs
  // Adding another extra underscore to all those identifiers solves this
  // Suppress this check for the actual helper struct names
  if (!is_args_or_result) {
    bool ends_with_args = (len_before >= 4) && (value2.substr(len_before - 4, 4) == "Args");
    bool ends_with_rslt = (len_before >= 6) && (value2.substr(len_before - 6, 6) == "Result");
    if (ends_with_args || ends_with_rslt) {
      value2 += '_';
    }
  }

  // Avoid naming collisions with other services
  if (is_args_or_result) {
    prefix += publicize(service_name);
  }

  return prefix + value2;
}

std::string t_go_generator::publicize(const std::string& value, bool is_args_or_result) const {
  return publicize(value, is_args_or_result, service_name_);
}

std::string t_go_generator::new_prefix(const std::string& value) const {
  if (value.size() <= 0) {
    return value;
  }

  string::size_type dot_pos = value.rfind('.');
  if (dot_pos != string::npos) {
    return value.substr(0, dot_pos + 1) + "New" + publicize(value.substr(dot_pos + 1));
  }
  return "New" + publicize(value);
}

std::string t_go_generator::privatize(const std::string& value) const {
  if (value.size() <= 0) {
    return value;
  }

  std::string value2(value);

  if (!islower(value2[0])) {
    value2[0] = tolower(value2[0]);
  }

  value2 = camelcase(value2);

  return value2;
}

std::string t_go_generator::variable_name_to_go_name(const std::string& value) {
  if (value.size() <= 0) {
    return value;
  }

  std::string value2(value);
  std::transform(value2.begin(), value2.end(), value2.begin(), ::tolower);

  switch (value[0]) {
  case 'b':
  case 'B':
    if (value2 != "break") {
      return value;
    }

    break;

  case 'c':
  case 'C':
    if (value2 != "case" && value2 != "chan" && value2 != "const" && value2 != "continue") {
      return value;
    }

    break;

  case 'd':
  case 'D':
    if (value2 != "default" && value2 != "defer") {
      return value;
    }

    break;

  case 'e':
  case 'E':
    if (value2 != "else" && value2 != "error") {
      return value;
    }

    break;

  case 'f':
  case 'F':
    if (value2 != "fallthrough" && value2 != "for" && value2 != "func") {
      return value;
    }

    break;

  case 'g':
  case 'G':
    if (value2 != "go" && value2 != "goto") {
      return value;
    }

    break;

  case 'i':
  case 'I':
    if (value2 != "if" && value2 != "import" && value2 != "interface") {
      return value;
    }

    break;

  case 'm':
  case 'M':
    if (value2 != "map") {
      return value;
    }

    break;

  case 'p':
  case 'P':
    if (value2 != "package") {
      return value;
    }

    break;

  case 'r':
  case 'R':
    if (value2 != "range" && value2 != "return") {
      return value;
    }

    break;

  case 's':
  case 'S':
    if (value2 != "select" && value2 != "struct" && value2 != "switch") {
      return value;
    }

    break;

  case 't':
  case 'T':
    if (value2 != "type") {
      return value;
    }

    break;

  case 'v':
  case 'V':
    if (value2 != "var") {
      return value;
    }

    break;

  default:
    return value;
  }

  return value2 + "_a1";
}

/**
 * Prepares for file generation by opening up the necessary file output
 * streams.
 *
 * @param tprogram The program to generate
 */
void t_go_generator::init_generator() {
  // Make output directory
  string module = get_real_go_module(program_);
  string target = module;
  package_dir_ = get_out_dir();

  // This set is taken from https://github.com/golang/lint/blob/master/lint.go#L692
  commonInitialisms.insert("API");
  commonInitialisms.insert("ASCII");
  commonInitialisms.insert("CPU");
  commonInitialisms.insert("CSS");
  commonInitialisms.insert("DNS");
  commonInitialisms.insert("EOF");
  commonInitialisms.insert("GUID");
  commonInitialisms.insert("HTML");
  commonInitialisms.insert("HTTP");
  commonInitialisms.insert("HTTPS");
  commonInitialisms.insert("ID");
  commonInitialisms.insert("IP");
  commonInitialisms.insert("JSON");
  commonInitialisms.insert("LHS");
  commonInitialisms.insert("QPS");
  commonInitialisms.insert("RAM");
  commonInitialisms.insert("RHS");
  commonInitialisms.insert("RPC");
  commonInitialisms.insert("SLA");
  commonInitialisms.insert("SMTP");
  commonInitialisms.insert("SSH");
  commonInitialisms.insert("TCP");
  commonInitialisms.insert("TLS");
  commonInitialisms.insert("TTL");
  commonInitialisms.insert("UDP");
  commonInitialisms.insert("UI");
  commonInitialisms.insert("UID");
  commonInitialisms.insert("UUID");
  commonInitialisms.insert("URI");
  commonInitialisms.insert("URL");
  commonInitialisms.insert("UTF8");
  commonInitialisms.insert("VM");
  commonInitialisms.insert("XML");
  commonInitialisms.insert("XSRF");
  commonInitialisms.insert("XSS");

  // names of read and write methods
  if (read_write_private_) {
    read_method_name_ = "read";
    write_method_name_ = "write";
  } else {
    read_method_name_ = "Read";
    write_method_name_ = "Write";
  }
  equals_method_name_ = "Equals";

  while (true) {
    // TODO: Do better error checking here.
    MKDIR(package_dir_.c_str());

    if (module.empty()) {
      break;
    }

    string::size_type pos = module.find('.');

    if (pos == string::npos) {
      package_dir_ += "/";
      package_dir_ += module;
      package_name_ = module;
      module.clear();
    } else {
      package_dir_ += "/";
      package_dir_ += module.substr(0, pos);
      module.erase(0, pos + 1);
    }
  }

  string::size_type loc;

  while ((loc = target.find(".")) != string::npos) {
    target.replace(loc, 1, 1, '/');
  }

  // Make output files
  f_types_name_ = package_dir_ + "/" + program_name_ + ".go";
  f_types_.open(f_types_name_);

  f_consts_name_ = package_dir_ + "/" + program_name_ + "-consts.go";
  f_consts_.open(f_consts_name_);

  // Print header
  f_types_ << go_autogen_comment() << go_package() << render_includes(false);

  f_consts_ << go_autogen_comment() << go_package() << render_includes(true);

  f_const_values_ << '\n' << "func init() {" << '\n';

  // Create file for the GoUnusedProtection__ variable
  string f_unused_prot_name_ = package_dir_ + "/" + "GoUnusedProtection__.go";
  ofstream_with_content_based_conditional_update f_unused_prot_;
  f_unused_prot_.open(f_unused_prot_name_.c_str());
  f_unused_prot_ << go_autogen_comment() << go_package() << render_import_protection();
  f_unused_prot_.close();
}

string t_go_generator::render_included_programs(string& unused_prot) {
  const vector<t_program*>& includes = program_->get_includes();
  string result = "";
  string local_namespace = get_real_go_module(program_);
  std::set<std::string> included;
  for (auto include : includes) {
    std::string includeModule = get_real_go_module(include);
    if (!local_namespace.empty() && local_namespace == includeModule) {
      continue;
    }

    if (!included.insert(includeModule).second) {
        continue;
    }

    result += render_program_import(include, unused_prot);
  }
  return result;
}

string t_go_generator::render_program_import(const t_program* program, string& unused_protection) {
  string result = "";

  string go_module = get_real_go_module(program);
  string go_path = go_module;
  size_t found = 0;
  for (size_t j = 0; j < go_module.size(); j++) {
    // Import statement uses slashes ('/') in namespace
    if (go_module[j] == '.') {
      go_path[j] = '/';
      found = j + 1;
    }
  }

  auto it = package_identifiers_.find(go_module);
  auto last_component = go_module.substr(found);
  if (it == package_identifiers_.end()) {
    auto value = last_component;
    // This final path component has already been used, let's construct a more unique alias
    if (package_identifiers_set_.find(value) != package_identifiers_set_.end()) {
      // TODO: This would produce more readable code if it appended trailing go_module
      // path components to generate a more readable name unique identifier (e.g. use
      // packageacommon as the alias for packagea/common instead of common=). But just
      // appending an integer is much simpler code
      value = tmp(value);
    }
    package_identifiers_set_.insert(value);
    it = package_identifiers_.emplace(go_module, std::move(value)).first;
  }
  auto const& package_identifier = it->second;
  result += "\t";
  // if the package_identifier is different than final path component we need an alias
  if (last_component.compare(package_identifier) != 0) {
    result += package_identifier + " ";
  }
  string s;

  for (auto const& e : package_identifiers_set_)
  {
      s += e;
      s += ',';
  }

  s.pop_back();

  result += "\"" + gen_package_prefix_ + go_path + "\"\n";
  unused_protection += "var _ = " + package_identifier + ".GoUnusedProtection__\n";
  return result;
}

/**
 * Render import lines for the system packages.
 *
 * The arg system_packages supports the following two options for import auto
 * rename in case duplications happens:
 *
 * 1. The full import path without double quotation marks, with part after the
 *    last "/" as the import identifier. e.g.:
 *    - "context" (context)
 *    - "database/sql/driver" (driver)
 * 2. A rename import with double quotation marks around the full import path,
 *    with the part before the first space as the import identifier. e.g.:
 *    - "thrift \"github.com/apache/thrift/lib/go/thrift\"" (thrift)
 *
 * If a system package's package name is different from the last part of its
 * full import path, please always rename import it for dedup to work correctly,
 * e.g. "package \"github.com/org/go-package\"".
 *
 * @param system_packages
 */
string t_go_generator::render_system_packages(std::vector<string>& system_packages) {
  string result = "";

  for (vector<string>::iterator iter = system_packages.begin(); iter != system_packages.end(); ++iter) {
    string package = *iter;
    string identifier = package;
    auto space_pos = package.find(" ");
    if (space_pos != string::npos) {
      // This is a rename import line, no need to wrap double quotation marks.
      result += "\t"+ package +"\n";
      // The part before the first space is the import identifier.
      identifier = package.substr(0, space_pos);
    } else {
      result += "\t\""+ package +"\"\n";
      // The part after the last / is the import identifier.
      auto slash_pos = package.rfind("/");
      if (slash_pos != string::npos) {
        identifier = package.substr(slash_pos+1);
      }
    }

    // Reserve these package names in case the collide with imported Thrift packages
    package_identifiers_set_.insert(identifier);
    package_identifiers_.emplace(package, identifier);
  }
  return result;
}

/**
 * Renders all the imports necessary for including another Thrift program.
 * If consts include the additional imports.
 */
string t_go_generator::render_includes(bool consts) {
  const vector<t_program*>& includes = program_->get_includes();
  string result = "";
  string unused_prot = "";
  result += go_imports_begin(consts);
  result += render_included_programs(unused_prot);

  if (includes.size() > 0) {
    result += "\n";
  }

  return result + go_imports_end() + unused_prot;
}

string t_go_generator::render_import_protection() {
  return string("var GoUnusedProtection__ int;\n\n");
}

/**
 * Renders all the imports necessary to use the accelerated TBinaryProtocol
 */
string t_go_generator::render_fastbinary_includes() {
  return "";
}

/**
 * Autogen'd comment. The different text is necessary due to
 * https://github.com/golang/go/issues/13560#issuecomment-288457920
 */
string t_go_generator::go_autogen_comment() {
  return
        std::string() +
        "// Code generated by Thrift Compiler (" + THRIFT_VERSION + "). DO NOT EDIT.\n\n";
}

/**
 * Prints standard thrift package
 */
string t_go_generator::go_package() {
  return string("package ") + package_name_ + "\n\n";
}

/**
 * Render the beginning of the import statement.
 * If consts include the additional imports.
 */
string t_go_generator::go_imports_begin(bool consts) {
  std::vector<string> system_packages;
  system_packages.push_back("bytes");
  system_packages.push_back("context");
  // If not writing constants, and there are enums, need extra imports.
  if (!consts && get_program()->get_enums().size() > 0) {
    system_packages.push_back("database/sql/driver");
  }
  system_packages.push_back("errors");
  system_packages.push_back("fmt");
  system_packages.push_back("log/slog");
  system_packages.push_back("time");
  // For the thrift import, always do rename import to make sure it's called thrift.
  system_packages.push_back("thrift \"" + gen_thrift_import_ + "\"");

  // validator import
  system_packages.push_back("strings");
  system_packages.push_back("regexp");
  return "import (\n" + render_system_packages(system_packages);
}

/**
 * End the import statement, include undscore-assignments
 *
 * These "_ =" prevent the go compiler complaining about unused imports.
 * This will have to do in lieu of more intelligent import statement construction
 */
string t_go_generator::go_imports_end() {
  string import_end = string(
      ")\n\n"
      "// (needed to ensure safety because of naive import list construction.)\n"
      "var _ = bytes.Equal\n"
      "var _ = context.Background\n"
      "var _ = errors.New\n"
      "var _ = fmt.Printf\n"
      "var _ = slog.Log\n"
      "var _ = time.Now\n"
      "var _ = thrift.ZERO\n"
      "// (needed by validator.)\n"
      "var _ = strings.Contains\n"
      "var _ = regexp.MatchString\n\n");
  return import_end;
}

/**
 * Closes the type files
 */
void t_go_generator::close_generator() {
  f_const_values_ << "}" << '\n' << '\n';
  f_consts_ << f_const_values_.str();

  // Close types and constants files
  f_consts_.close();
  f_types_.close();
  format_go_output(f_types_name_);
  format_go_output(f_consts_name_);
}

/**
 * Generates a typedef.
 *
 * @param ttypedef The type definition
 */
void t_go_generator::generate_typedef(t_typedef* ttypedef) {
  generate_go_docstring(f_types_, ttypedef);
  string new_type_name(publicize(ttypedef->get_symbolic()));
  string base_type(type_to_go_type(ttypedef->get_type()));

  if (base_type == new_type_name) {
    return;
  }

  generate_deprecation_comment(f_types_, ttypedef->annotations_);
  f_types_ << "type " << new_type_name << " " << base_type << '\n' << '\n';
  // Generate a convenience function that converts an instance of a type
  // (which may be a constant) into a pointer to an instance of a type.
  generate_deprecation_comment(f_types_, ttypedef->annotations_);
  f_types_ << "func " << new_type_name << "Ptr(v " << new_type_name << ") *" << new_type_name
           << " { return &v }" << '\n' << '\n';
}

/**
 * Generates code for an enumerated type. Done using a class to scope
 * the values.
 *
 * @param tenum The enumeration
 */
void t_go_generator::generate_enum(t_enum* tenum) {
  std::ostringstream to_string_mapping, from_string_mapping;
  std::string tenum_name(publicize(tenum->get_name()));
  generate_go_docstring(f_types_, tenum);
  generate_deprecation_comment(f_types_, tenum->annotations_);
  f_types_ << "type " << tenum_name << " int64" << '\n' << "const (" << '\n';

  to_string_mapping << indent() << "func (p " << tenum_name << ") String() string {" << '\n';
  indent_up();
  to_string_mapping << indent() << "switch p {" << '\n';
  indent_down();

  generate_deprecation_comment(from_string_mapping, tenum->annotations_);
  from_string_mapping << indent() << "func " << tenum_name << "FromString(s string) (" << tenum_name
                      << ", error) {" << '\n';
  indent_up();
  from_string_mapping << indent() << "switch s {" << '\n';
  indent_down();

  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator c_iter;
  int value = -1;

  indent_up();
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    value = (*c_iter)->get_value();

    string iter_std_name(escape_string((*c_iter)->get_name()));
    string iter_name((*c_iter)->get_name());
    generate_deprecation_comment(f_types_, (*c_iter)->annotations_);
    f_types_ << indent() << tenum_name << "_" << iter_name << ' ' << tenum_name << " = "
             << value << '\n';
    // Dictionaries to/from string names of enums
    to_string_mapping << indent() << "case " << tenum_name << "_" << iter_name << ": return \""
                      << iter_std_name << "\"" << '\n';

    if (iter_std_name != escape_string(iter_name)) {
      from_string_mapping << indent() << "case \"" << iter_std_name << "\", \""
                          << escape_string(iter_name) << "\": return " << tenum_name << "_"
                          << iter_name << ", nil " << '\n';
    } else {
      from_string_mapping << indent() << "case \"" << iter_std_name << "\": return " << tenum_name
                          << "_" << iter_name << ", nil " << '\n';
    }
  }

  to_string_mapping << indent() << "}" << '\n';
  to_string_mapping << indent() << "return \"<UNSET>\"" << '\n';
  indent_down();
  to_string_mapping << indent() << "}" << '\n';
  indent_up();
  from_string_mapping << indent() << "}" << '\n';
  from_string_mapping << indent() << "return " << tenum_name << "(0),"
                      << " fmt.Errorf(\"not a valid " << tenum_name << " string\")" << '\n';
  indent_down();
  from_string_mapping << indent() << "}" << '\n';

  f_types_ << ")" << '\n' << '\n' << to_string_mapping.str() << '\n' << from_string_mapping.str()
           << '\n' << '\n';

  // Generate a convenience function that converts an instance of an enum
  // (which may be a constant) into a pointer to an instance of that enum
  // type.
  generate_deprecation_comment(f_types_, tenum->annotations_);
  f_types_ << "func " << tenum_name << "Ptr(v " << tenum_name << ") *" << tenum_name
           << " { return &v }" << '\n' << '\n';

  // Generate MarshalText
  f_types_ << "func (p " << tenum_name << ") MarshalText() ([]byte, error) {" << '\n';
  indent_up();
  f_types_ << indent() << "return []byte(p.String()), nil" << '\n';
  indent_down();
  f_types_ << "}" << '\n' << '\n';

  // Generate UnmarshalText
  f_types_ << "func (p *" << tenum_name << ") UnmarshalText(text []byte) error {" << '\n';
  indent_up();
  f_types_ << indent() << "q, err := " << tenum_name << "FromString(string(text))" << '\n';
  f_types_ << indent() << "if err != nil {" << '\n';
  indent_up();
  f_types_ << indent() << "return err" << '\n';
  indent_down();
  f_types_ << indent() << "}" << '\n';
  f_types_ << indent() << "*p = q" << '\n';
  f_types_ << indent() << "return nil" << '\n';
  indent_down();
  f_types_ << "}" << '\n' << '\n';

  // Generate Scan for sql.Scanner interface
  f_types_ << "func (p *" << tenum_name << ") Scan(value interface{}) error {" << '\n';
  indent_up();
  f_types_ << indent() << "v, ok := value.(int64)" << '\n';
  f_types_ << indent() << "if !ok {" << '\n';
  indent_up();
  f_types_ << indent() << "return errors.New(\"Scan value is not int64\")" << '\n';
  indent_down();
  f_types_ << indent() << "}" << '\n';
  f_types_ << indent() << "*p = " << tenum_name << "(v)" << '\n';
  f_types_ << indent() << "return nil" << '\n';
  indent_down();
  f_types_ << "}" << '\n' << '\n';

  // Generate Value for driver.Valuer interface
  f_types_ << "func (p *" << tenum_name << ") Value() (driver.Value, error) {" << '\n';
  indent_up();
  f_types_ << indent() << "if p == nil {" << '\n';
  indent_up();
  f_types_ << indent() << "return nil, nil" << '\n';
  indent_down();
  f_types_ << indent() << "}" << '\n';
  f_types_ << indent() << "return int64(*p), nil" << '\n';
  indent_down();
  f_types_ << "}" << '\n' << '\n';

}

/**
 * Generate a constant value
 */
void t_go_generator::generate_const(t_const* tconst) {
  t_type* type = tconst->get_type();
  string name = publicize(tconst->get_name());
  t_const_value* value = tconst->get_value();
  if (type->is_enum() || (type->is_base_type() && ((t_base_type*)type)->get_base() != t_base_type::TYPE_UUID)) {
    indent(f_consts_) << "const " << name << " = " << render_const_value(type, value, name) << '\n';
  } else {
    f_const_values_ << indent() << name << " = " << render_const_value(type, value, name) << '\n'
                    << '\n';

    f_consts_ << indent() << "var " << name << " " << type_to_go_type(type) << '\n';
  }
}

/**
 * Prints the value of a constant with the given type. Note that type checking
 * is NOT performed in this function as it is always run beforehand using the
 * validate_types method in main.cc
 */
string t_go_generator::render_const_value(t_type* type, t_const_value* value, const string& name, bool opt) {
  string typedef_opt_ptr;
  string typedef_opt;
  if (type->is_typedef()) {
    typedef_opt = publicize(type_name(type));
    typedef_opt_ptr = typedef_opt + "Ptr";
  }
  type = get_true_type(type);
  std::ostringstream out;

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();

    if (opt) {
      switch (tbase) {
        case t_base_type::TYPE_BOOL:
          if (typedef_opt_ptr != "") {
            out << typedef_opt_ptr;
          } else {
            out << "thrift.BoolPtr";
          }
          out << "(";
          out << (value->get_integer() > 0 ? "true" : "false");
          break;

        case t_base_type::TYPE_I8:
          if (typedef_opt_ptr != "") {
            out << typedef_opt_ptr;
          } else {
            out << "thrift.Int8Ptr";
          }
          out << "(";
          out << value->get_integer();
          break;
        case t_base_type::TYPE_I16:
          if (typedef_opt_ptr != "") {
            out << typedef_opt_ptr;
          } else {
            out << "thrift.Int16Ptr";
          }
          out << "(";
          out << value->get_integer();
          break;
        case t_base_type::TYPE_I32:
          if (typedef_opt_ptr != "") {
            out << typedef_opt_ptr;
          } else {
            out << "thrift.Int32Ptr";
          }
          out << "(";
          out << value->get_integer();
          break;
        case t_base_type::TYPE_I64:
          if (typedef_opt_ptr != "") {
            out << typedef_opt_ptr;
          } else {
            out << "thrift.Int64Ptr";
          }
          out << "(";
          out << value->get_integer();
          break;

        case t_base_type::TYPE_DOUBLE:
          if (typedef_opt_ptr != "") {
            out << typedef_opt_ptr;
          } else {
            out << "thrift.Float64Ptr";
          }
          out << "(";
          if (value->get_type() == t_const_value::CV_INTEGER) {
            out << value->get_integer();
          } else {
            out << value->get_double();
          }
          break;

        case t_base_type::TYPE_STRING:
          if (typedef_opt_ptr != "") {
            out << typedef_opt_ptr;
          } else {
            out << "thrift.StringPtr";
          }
          out << "(";
          out << '"' + get_escaped_string(value) + '"';
          break;

        case t_base_type::TYPE_UUID:
          if (typedef_opt_ptr != "") {
            out << typedef_opt_ptr << "(" << typedef_opt;
          } else {
            out << "thrift.TuuidPtr";
          }
          out << "(";
          out << "thrift.Must(thrift.ParseTuuid(\"" + get_escaped_string(value) + "\"))";
          if (typedef_opt_ptr != "") {
            out << ")";
          }
          break;

        default:
          throw "compiler error: no const of base type " + t_base_type::t_base_name(tbase);
      }
      out << ")";
    } else {
      switch (tbase) {
        case t_base_type::TYPE_STRING:
          if (type->is_binary()) {
            out << "[]byte(\"" << get_escaped_string(value) << "\")";
          } else {
            out << '"' << get_escaped_string(value) << '"';
          }

          break;

        case t_base_type::TYPE_BOOL:
          out << (value->get_integer() > 0 ? "true" : "false");
          break;

        case t_base_type::TYPE_I8:
        case t_base_type::TYPE_I16:
        case t_base_type::TYPE_I32:
        case t_base_type::TYPE_I64:
          if (opt) {
            out << "&(&struct{x int}{";
          }
          out << value->get_integer();
          if (opt) {
            out << "}).x";
          }
          break;

        case t_base_type::TYPE_DOUBLE:
          if (value->get_type() == t_const_value::CV_INTEGER) {
            out << value->get_integer();
          } else {
            out << value->get_double();
          }

          break;

        case t_base_type::TYPE_UUID:
          if (typedef_opt != "") {
            out << typedef_opt << "(";
          }
          out << "thrift.Must(thrift.ParseTuuid(\"" + get_escaped_string(value) + "\"))";
          if (typedef_opt != "") {
            out << ")";
          }

          break;

        default:
          throw "compiler error: no const of base type " + t_base_type::t_base_name(tbase);
      }
    }
  } else if (type->is_enum()) {
    if (opt) {
      if (typedef_opt_ptr != "") {
        out << typedef_opt_ptr << "(";
      } else {
        out << type_name(type) << "Ptr(";
      }
    }
    out << value->get_integer();
    if (opt) {
      out << ")";
    }
  } else if (type->is_struct() || type->is_xception()) {
    out << "&" << publicize(type_name(type)) << "{";
    indent_up();
    const vector<t_field*>& fields = ((t_struct*)type)->get_members();
    vector<t_field*>::const_iterator f_iter;
    const map<t_const_value*, t_const_value*, t_const_value::value_compare>& val = value->get_map();
    map<t_const_value*, t_const_value*, t_const_value::value_compare>::const_iterator v_iter;

    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      t_type* field_type = nullptr;
      bool is_optional = false;
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        if ((*f_iter)->get_name() == v_iter->first->get_string()) {
          field_type = (*f_iter)->get_type();
          is_optional = is_pointer_field(*f_iter);
        }
      }

      if (field_type == nullptr) {
        throw "type error: " + type->get_name() + " has no field " + v_iter->first->get_string();
      }
      out << '\n' << indent() << publicize(v_iter->first->get_string()) << ": "
          << render_const_value(field_type, v_iter->second, name, is_optional) << "," << '\n';
    }

    indent_down();
    out << "}";

  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();
    const map<t_const_value*, t_const_value*, t_const_value::value_compare>& val = value->get_map();
    out << "map[" << type_to_go_key_type(ktype) << "]" << type_to_go_type(vtype) << "{" << '\n';
    indent_up();
    map<t_const_value*, t_const_value*, t_const_value::value_compare>::const_iterator v_iter;

    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      out << indent() << render_const_value(ktype, v_iter->first, name) << ": "
          << render_const_value(vtype, v_iter->second, name) << "," << '\n';
    }

    indent_down();
    out << indent() << "}";
  } else if (type->is_list()) {
    t_type* etype = ((t_list*)type)->get_elem_type();
    const vector<t_const_value*>& val = value->get_list();
    out << "[]" << type_to_go_type(etype) << "{" << '\n';
    indent_up();
    vector<t_const_value*>::const_iterator v_iter;

    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      out << indent() << render_const_value(etype, *v_iter, name) << ", ";
    }

    indent_down();
    out << indent() << "}";
  } else if (type->is_set()) {
    t_type* etype = ((t_set*)type)->get_elem_type();
    const vector<t_const_value*>& val = value->get_list();
    out << "[]" << type_to_go_type(etype) << "{" << '\n';
    indent_up();
    vector<t_const_value*>::const_iterator v_iter;

    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      out << indent() << render_const_value(etype, *v_iter, name) << ", ";
    }

    indent_down();
    out << indent() << "}";
  } else {
    throw "CANNOT GENERATE CONSTANT FOR TYPE: " + type->get_name();
  }

  return out.str();
}

/**
 * Generates a go struct
 */
void t_go_generator::generate_struct(t_struct* tstruct) {
  generate_go_struct(tstruct, false);
}

/**
 * Generates a struct definition for a thrift exception. Basically the same
 * as a struct but extends the Exception class.
 *
 * @param txception The struct definition
 */
void t_go_generator::generate_xception(t_struct* txception) {
  generate_go_struct(txception, true);
}

/**
 * Generates a go struct
 */
void t_go_generator::generate_go_struct(t_struct* tstruct, bool is_exception) {
  generate_go_struct_definition(f_types_, tstruct, is_exception);
  // generate Validate function
  std::string tstruct_name(publicize(tstruct->get_name(), false));
  f_types_ << "func (p *" << tstruct_name << ") Validate() error {" << '\n';
  indent_up();
  go_validator_generator(this).generate_struct_validator(f_types_, tstruct);
  f_types_ << indent() << "return nil" << '\n';
  indent_down();
  f_types_ << "}" << '\n' << '\n';
}

void t_go_generator::get_publicized_name_and_def_value(t_field* tfield,
                                                       string* OUT_pub_name,
                                                       t_const_value** OUT_def_value) const {
  const string base_field_name = tfield->get_name();
  const string escaped_field_name = escape_string(base_field_name);
  *OUT_pub_name = publicize(escaped_field_name);
  *OUT_def_value = tfield->get_value();
}

void t_go_generator::generate_go_struct_initializer(ostream& out,
                                                    t_struct* tstruct,
                                                    bool is_args_or_result) {
  out << publicize(type_name(tstruct), is_args_or_result) << "{";
  indent_up();
  const vector<t_field*>& members = tstruct->get_members();
  bool empty = true;
  for (auto member : members) {
    bool pointer_field = is_pointer_field(member);
    string publicized_name;
    t_const_value* def_value;
    get_publicized_name_and_def_value(member, &publicized_name, &def_value);
    if (!pointer_field && def_value != nullptr && !omit_initialization(member)) {
      empty = false;
      out << '\n' << indent() << publicized_name << ": "
          << render_field_initial_value(member, member->get_name(), pointer_field) << ",";
    }
  }

  indent_down();
  if (!empty) {
    out << '\n' << indent();
  }
  out << "}" << '\n';
}

/**
 * Generates a struct definition for a thrift data type.
 *
 * @param tstruct The struct definition
 */
void t_go_generator::generate_go_struct_definition(ostream& out,
                                                   t_struct* tstruct,
                                                   bool is_exception,
                                                   bool is_result,
                                                   bool is_args) {
  const vector<t_field*>& members = tstruct->get_members();
  const vector<t_field*>& sorted_members = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator m_iter;

  std::string tstruct_name(publicize(tstruct->get_name(), is_args || is_result));
  generate_go_docstring(out, tstruct);
  generate_deprecation_comment(out, tstruct->annotations_);
  out << indent() << "type " << tstruct_name << " struct {" << '\n';
  /*
     Here we generate the structure specification for the fastbinary codec.
     These specifications have the following structure:
     thrift_spec -> tuple of item_spec
     item_spec -> nil | (tag, type_enum, name, spec_args, default)
     tag -> integer
     type_enum -> TType.I32 | TType.STRING | TType.STRUCT | ...
     name -> string_literal
     default -> nil  # Handled by __init__
     spec_args -> nil  # For simple types
                | (type_enum, spec_args)  # Value type for list/set
                | (type_enum, spec_args, type_enum, spec_args)
                  # Key and value for map
                | (class_name, spec_args_ptr) # For struct/exception
     class_name -> identifier  # Basically a pointer to the class
     spec_args_ptr -> expression  # just class_name.spec_args

     TODO(dreiss): Consider making this work for structs with negative tags.
  */
  // TODO(dreiss): Look into generating an empty tuple instead of nil
  // for structures with no members.
  // TODO(dreiss): Test encoding of structs where some inner structs
  // don't have thrift_spec.
  indent_up();

  int num_setable = 0;
  if (sorted_members.empty() || (sorted_members[0]->get_key() >= 0)) {
    int sorted_keys_pos = 0;

    for (m_iter = sorted_members.begin(); m_iter != sorted_members.end(); ++m_iter) {
      // Set field to optional if field is union, this is so we can get a
      // pointer to the field.
      if (tstruct->is_union())
        (*m_iter)->set_req(t_field::T_OPTIONAL);
      if (sorted_keys_pos != (*m_iter)->get_key()) {
        int first_unused = (std::max)(1, sorted_keys_pos++);
        while (sorted_keys_pos != (*m_iter)->get_key()) {
          ++sorted_keys_pos;
        }
        int last_unused = sorted_keys_pos - 1;
        if (first_unused < last_unused) {
          indent(out) << "// unused fields # " << first_unused << " to " << last_unused << '\n';
        } else if (first_unused == last_unused) {
          indent(out) << "// unused field # " << first_unused << '\n';
        }
      }

      t_type* fieldType = (*m_iter)->get_type();
      string goType = type_to_go_type_with_opt(fieldType, is_pointer_field(*m_iter));

      map<string,string>tags;
      tags["db"]=escape_string((*m_iter)->get_name());

      // Only add the `omitempty` tag if this field is optional and has no default value.
      // Otherwise a proper value like `false` for a bool field will be ommitted from
      // the JSON output since Go Marshal won't output `zero values`.
      bool has_default = (*m_iter)->get_value();
      bool is_optional = (*m_iter)->get_req() == t_field::T_OPTIONAL;
      if (is_optional && !has_default) {
        tags["json"]=escape_string((*m_iter)->get_name())+",omitempty";
      } else {
        tags["json"]=escape_string((*m_iter)->get_name());
      }

      // Check for user defined tags and them if there are any. User defined tags
      // can override the above db and json tags.
      std::map<string, std::vector<string>>::iterator it = (*m_iter)->annotations_.find("go.tag");
      if (it != (*m_iter)->annotations_.end()) {
        parse_go_tags(&tags, it->second.back());
      }

      string gotag;
      for (auto it = tags.begin(); it != tags.end(); ++it) {
        gotag += it->first + ":\"" + it->second + "\" ";
      }
      // Trailing whitespace
      gotag.resize(gotag.size()-1);

      generate_deprecation_comment(out, (*m_iter)->annotations_);
      indent(out) << publicize((*m_iter)->get_name()) << " " << goType << " `thrift:\""
                  << escape_string((*m_iter)->get_name()) << "," << sorted_keys_pos;
      if ((*m_iter)->get_req() == t_field::T_REQUIRED) {
        out << ",required";
      }

      out << "\" " << gotag << "`" << '\n';
      sorted_keys_pos++;
    }
  } else {
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      generate_deprecation_comment(out, (*m_iter)->annotations_);
      // This fills in default values, as opposed to nulls
      out << indent() << publicize((*m_iter)->get_name()) << " "
          << type_to_go_type((*m_iter)->get_type()) << '\n';
    }
  }

  indent_down();
  out << indent() << "}" << '\n' << '\n';
  generate_deprecation_comment(out, tstruct->annotations_);
  out << indent() << "func New" << tstruct_name << "() *" << tstruct_name << " {" << '\n';
  indent_up();
  out << indent() << "return &";
  generate_go_struct_initializer(out, tstruct, is_result || is_args);
  indent_down();
  out << indent() << "}" << '\n' << '\n';
  // Default values for optional fields
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    string publicized_name;
    t_const_value* def_value;
    get_publicized_name_and_def_value(*m_iter, &publicized_name, &def_value);
    t_type* fieldType = (*m_iter)->get_type();
    string goType = type_to_go_type_with_opt(fieldType, false);
    string def_var_name = tstruct_name + "_" + publicized_name + "_DEFAULT";
    if ((*m_iter)->get_req() == t_field::T_OPTIONAL || is_pointer_field(*m_iter)) {
      generate_deprecation_comment(out, (*m_iter)->annotations_);
      out << indent() << "var " << def_var_name << " " << goType;
      if (def_value != nullptr) {
        out << " = " << render_const_value(fieldType, def_value, (*m_iter)->get_name());
      }
      out << '\n';
    }
    out << '\n';

    // num_setable is used for deciding if Count* methods will be generated for union fields.
    // This applies to all nullable fields including slices (used for set, list and binary) and maps, not just pointers.
    t_type* type = fieldType->get_true_type();
    if (is_pointer_field(*m_iter)|| type->is_map() || type->is_set() || type->is_list() || type->is_binary()) {
      num_setable += 1;
    }

    if (is_pointer_field(*m_iter)) {
      string goOptType = type_to_go_type_with_opt(fieldType, true);
      string maybepointer = goOptType != goType ? "*" : "";
      generate_deprecation_comment(out, (*m_iter)->annotations_);
      out << indent() << "func (p *" << tstruct_name << ") Get" << publicized_name << "() "
          << goType << " {" << '\n';
      indent_up();
      out << indent() << "if !p.IsSet" << publicized_name << "() {" << '\n';
      indent_up();
      out << indent() << "return " << def_var_name << '\n';
      indent_down();
      out << indent() << "}" << '\n';
      out << indent() << "return " << maybepointer << "p." << publicized_name << '\n';
      indent_down();
      out << indent() << "}" << '\n' << '\n';
    } else {
      out << '\n';
      generate_deprecation_comment(out, (*m_iter)->annotations_);
      out << indent() << "func (p *" << tstruct_name << ") Get" << publicized_name << "() "
          << goType << " {" << '\n';
      indent_up();
      out << indent() << "return p." << publicized_name << '\n';
      indent_down();
      out << indent() << "}" << '\n' << '\n';
    }
  }

  if (tstruct->is_union() && num_setable > 0) {
    generate_countsetfields_helper(out, tstruct, tstruct_name, is_result);
  }

  generate_isset_helpers(out, tstruct, tstruct_name, is_result);
  generate_go_struct_reader(out, tstruct, tstruct_name, is_result);
  generate_go_struct_writer(out, tstruct, tstruct_name, is_result, num_setable > 0);
  if (!is_result && !is_args) {
    generate_go_struct_equals(out, tstruct, tstruct_name);
  }

  out << indent() << "func (p *" << tstruct_name << ") String() string {" << '\n';
  indent_up();
  out << indent() << "if p == nil {" << '\n';
  indent_up();
  out << indent() << "return \"<nil>\"" << '\n';
  indent_down();
  out << indent() << "}" << '\n';
  out << indent() << "return fmt.Sprintf(\"" << escape_string(tstruct_name) << "(%+v)\", *p)"
      << '\n';
  indent_down();
  out << indent() << "}" << '\n' << '\n';

  if (is_exception) {
    out << indent() << "func (p *" << tstruct_name << ") Error() string {" << '\n';
    indent_up();
    out << indent() << "return p.String()" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';

    out << indent() << "func (" << tstruct_name << ") TExceptionType() thrift.TExceptionType {" << '\n';
    indent_up();
    out << indent() << "return thrift.TExceptionTypeCompiled" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';

    out << indent() << "var _ thrift.TException = (*" << tstruct_name << ")(nil)"
        << '\n' << '\n';
  }

  if (!read_write_private_) {
    // Generate the implementation of slog.LogValuer,
    // see: https://issues.apache.org/jira/browse/THRIFT-5745
    out << indent() << "func (p *" << tstruct_name << ") LogValue() slog.Value {" << '\n';
    indent_up();
    out << indent() << "if p == nil {" << '\n';
    indent_up();
    out << indent() << "return slog.AnyValue(nil)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    out << indent() << "v := thrift.SlogTStructWrapper{" << '\n';
    indent_up();
    out << indent() << "Type: \"*" << package_name_ << "." << tstruct_name << "\"," << '\n';
    out << indent() << "Value: p," << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    out << indent() << "return slog.AnyValue(v)" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';

    out << indent() << "var _ slog.LogValuer = (*" << tstruct_name << ")(nil)"
        << '\n' << '\n';
  }
}

/**
 * Generates the IsSet helper methods for a struct
 */
void t_go_generator::generate_isset_helpers(ostream& out,
                                            t_struct* tstruct,
                                            const string& tstruct_name,
                                            bool is_result) {
  (void)is_result;
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  const string escaped_tstruct_name(escape_string(tstruct->get_name()));

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    const string field_name(publicize(escape_string((*f_iter)->get_name())));
    if ((*f_iter)->get_req() == t_field::T_OPTIONAL || is_pointer_field(*f_iter)) {
      generate_deprecation_comment(out, (*f_iter)->annotations_);
      out << indent() << "func (p *" << tstruct_name << ") IsSet" << field_name << "() bool {"
          << '\n';
      indent_up();
      t_type* ttype = (*f_iter)->get_type()->get_true_type();
      bool is_byteslice = ttype->is_binary();
      bool compare_to_nil_only = ttype->is_set() || ttype->is_list() || ttype->is_map()
                                 || (is_byteslice && !(*f_iter)->get_value());
      if (is_pointer_field(*f_iter) || compare_to_nil_only) {
        out << indent() << "return p." << field_name << " != nil" << '\n';
      } else {
        string def_var_name = tstruct_name + "_" + field_name + "_DEFAULT";
        if (is_byteslice) {
          out << indent() << "return !bytes.Equal(p." << field_name << ", " << def_var_name << ")"
              << '\n';
        } else {
          out << indent() << "return p." << field_name << " != " << def_var_name << '\n';
        }
      }
      indent_down();
      out << indent() << "}" << '\n' << '\n';
    }
  }
}

/**
 * Generates the CountSetFields helper method for a struct
 */
void t_go_generator::generate_countsetfields_helper(ostream& out,
                                                    t_struct* tstruct,
                                                    const string& tstruct_name,
                                                    bool is_result) {
  (void)is_result;
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  const string escaped_tstruct_name(escape_string(tstruct->get_name()));

  out << indent() << "func (p *" << tstruct_name << ") CountSetFields" << tstruct_name << "() int {"
      << '\n';
  indent_up();
  out << indent() << "count := 0" << '\n';
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_REQUIRED)
      continue;

    t_type* type = (*f_iter)->get_type()->get_true_type();

    if (!(is_pointer_field(*f_iter) || type->is_map() || type->is_set() || type->is_list() || type->is_binary()))
      continue;

    const string field_name(publicize(escape_string((*f_iter)->get_name())));

    out << indent() << "if (p.IsSet" << field_name << "()) {" << '\n';
    indent_up();
    out << indent() << "count++" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
  }

  out << indent() << "return count" << '\n' << '\n';
  indent_down();
  out << indent() << "}" << '\n' << '\n';
}

/**
 * Generates the read method for a struct
 */
void t_go_generator::generate_go_struct_reader(ostream& out,
                                               t_struct* tstruct,
                                               const string& tstruct_name,
                                               bool is_result) {
  (void)is_result;
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  string escaped_tstruct_name(escape_string(tstruct->get_name()));
  out << indent() << "func (p *" << tstruct_name << ") " << read_method_name_ << "(ctx context.Context, iprot thrift.TProtocol) error {"
      << '\n';
  indent_up();
  out << indent() << "if _, err := iprot.ReadStructBegin(ctx); err != nil {" << '\n';
  indent_up();
  out << indent() << "return thrift.PrependError(fmt.Sprintf(\"%T read error: \", p), err)"
      << '\n';
  indent_down();
  out << indent() << "}" << '\n' << '\n';

  // Required variables does not have IsSet functions, so we need tmp vars to check them.
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      const string field_name(publicize(escape_string((*f_iter)->get_name())));
      indent(out) << "var isset" << field_name << " bool = false;" << '\n';
    }
  }
  out << '\n';

  // Loop over reading in fields
  indent(out) << "for {" << '\n';
  indent_up();
  // Read beginning field marker
  out << indent() << "_, fieldTypeId, fieldId, err := iprot.ReadFieldBegin(ctx)" << '\n';
  out << indent() << "if err != nil {" << '\n';
  indent_up();
  out << indent() << "return thrift.PrependError(fmt.Sprintf("
                     "\"%T field %d read error: \", p, fieldId), err)" << '\n';
  indent_down();
  out << indent() << "}" << '\n';
  // Check for field STOP marker and break
  out << indent() << "if fieldTypeId == thrift.STOP {" << '\n';
  indent_up();
  out << indent() << "break" << '\n';
  indent_down();
  out << indent() << "}" << '\n';

  string thriftFieldTypeId;
  // Generate deserialization code for known cases
  int32_t field_id = -1;

  // Switch statement on the field we are reading, false if no fields present
  bool have_switch = !fields.empty();
  if (have_switch) {
    indent(out) << "switch fieldId {" << '\n';
  }

  // All the fields we know
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    field_id = (*f_iter)->get_key();

    // if negative id, ensure we generate a valid method name
    string field_method_prefix("ReadField");
    int32_t field_method_suffix = field_id;

    if (field_method_suffix < 0) {
      field_method_prefix += "_";
      field_method_suffix *= -1;
    }

    out << indent() << "case " << field_id << ":" << '\n';
    indent_up();
    thriftFieldTypeId = type_to_enum((*f_iter)->get_type());

    if (thriftFieldTypeId == "thrift.BINARY") {
      thriftFieldTypeId = "thrift.STRING";
    }

    out << indent() << "if fieldTypeId == " << thriftFieldTypeId << " {" << '\n';
    indent_up();
    out << indent() << "if err := p." << field_method_prefix << field_method_suffix << "(ctx, iprot); err != nil {"
        << '\n';
    indent_up();
    out << indent() << "return err" << '\n';
    indent_down();
    out << indent() << "}" << '\n';

    // Mark required field as read
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      const string field_name(publicize(escape_string((*f_iter)->get_name())));
      out << indent() << "isset" << field_name << " = true" << '\n';
    }

    indent_down();
    out << indent() << "} else {" << '\n';
    indent_up();
    out << indent() << "if err := iprot.Skip(ctx, fieldTypeId); err != nil {" << '\n';
    indent_up();
    out << indent() << "return err" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    indent_down();
    out << indent() << "}" << '\n';


    indent_down();
  }

  // Begin switch default case
  if (have_switch) {
    out << indent() << "default:" << '\n';
    indent_up();
  }

  // Skip unknown fields in either case
  out << indent() << "if err := iprot.Skip(ctx, fieldTypeId); err != nil {" << '\n';
  indent_up();
  out << indent() << "return err" << '\n';
  indent_down();
  out << indent() << "}" << '\n';

  // End switch default case
  if (have_switch) {
    indent_down();
    out << indent() << "}" << '\n';
  }

  // Read field end marker
  out << indent() << "if err := iprot.ReadFieldEnd(ctx); err != nil {" << '\n';
  indent_up();
  out << indent() << "return err" << '\n';
  indent_down();
  out << indent() << "}" << '\n';
  indent_down();
  out << indent() << "}" << '\n';
  out << indent() << "if err := iprot.ReadStructEnd(ctx); err != nil {" << '\n';
  indent_up();
  out << indent() << "return thrift.PrependError(fmt.Sprintf("
                     "\"%T read struct end error: \", p), err)" << '\n';
  indent_down();
  out << indent() << "}" << '\n';

  // Return error if any required fields are missing.
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      const string field_name(publicize(escape_string((*f_iter)->get_name())));
      out << indent() << "if !isset" << field_name << "{" << '\n';
      indent_up();
      out << indent() << "return thrift.NewTProtocolExceptionWithType(thrift.INVALID_DATA, "
                         "fmt.Errorf(\"Required field " << field_name << " is not set\"));" << '\n';
      indent_down();
      out << indent() << "}" << '\n';
    }
  }

  out << indent() << "return nil" << '\n';
  indent_down();
  out << indent() << "}" << '\n' << '\n';

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    string field_type_name(publicize((*f_iter)->get_type()->get_name()));
    string field_name(publicize((*f_iter)->get_name()));
    string field_method_prefix("ReadField");
    int32_t field_id = (*f_iter)->get_key();
    int32_t field_method_suffix = field_id;

    if (field_method_suffix < 0) {
      field_method_prefix += "_";
      field_method_suffix *= -1;
    }

    out << indent() << "func (p *" << tstruct_name << ") " << field_method_prefix << field_method_suffix
        << "(ctx context.Context, iprot thrift.TProtocol) error {" << '\n';
    indent_up();
    generate_deserialize_field(out, *f_iter, false, "p.");
    out << indent() << "return nil" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';
  }
}

void t_go_generator::generate_go_struct_writer(ostream& out,
                                               t_struct* tstruct,
                                               const string& tstruct_name,
                                               bool is_result,
                                               bool uses_countsetfields) {
  (void)is_result;
  string name(tstruct->get_name());
  const vector<t_field*>& fields = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator f_iter;
  indent(out) << "func (p *" << tstruct_name << ") " << write_method_name_ << "(ctx context.Context, oprot thrift.TProtocol) error {" << '\n';
  indent_up();
  if (tstruct->is_union() && uses_countsetfields) {
    std::string tstruct_name(publicize(tstruct->get_name()));
    out << indent() << "if c := p.CountSetFields" << tstruct_name << "(); c != 1 {" << '\n';
    indent_up();
    out << indent() << "return fmt.Errorf(\"%T write union: exactly one field must be set (%d set)\", p, c)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
  }
  out << indent() << "if err := oprot.WriteStructBegin(ctx, \"" << name << "\"); err != nil {" << '\n';
  indent_up();
  out << indent() << "return thrift.PrependError(fmt.Sprintf("
                     "\"%T write struct begin error: \", p), err)" << '\n';
  indent_down();
  out << indent() << "}" << '\n';

  string field_name;
  string escape_field_name;
  // t_const_value* field_default_value;
  t_field::e_req field_required;
  int32_t field_id = -1;

  out << indent() << "if p != nil {" << '\n';
  indent_up();

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    string field_method_prefix("writeField");
    field_name = (*f_iter)->get_name();
    escape_field_name = escape_string(field_name);
    field_id = (*f_iter)->get_key();
    int32_t field_method_suffix = field_id;

    if (field_method_suffix < 0) {
      field_method_prefix += "_";
      field_method_suffix *= -1;
    }

    out << indent() << "if err := p." << field_method_prefix << field_method_suffix
        << "(ctx, oprot); err != nil { return err }" << '\n';
  }

  indent_down();
  out << indent() << "}" << '\n';

  // Write the struct map
  out << indent() << "if err := oprot.WriteFieldStop(ctx); err != nil {" << '\n';
  indent_up();
  out << indent() << "return thrift.PrependError(\"write field stop error: \", err)" << '\n';
  indent_down();
  out << indent() << "}" << '\n';
  out << indent() << "if err := oprot.WriteStructEnd(ctx); err != nil {" << '\n';
  indent_up();
  out << indent() << "return thrift.PrependError(\"write struct stop error: \", err)" << '\n';
  indent_down();
  out << indent() << "}" << '\n';
  out << indent() << "return nil" << '\n';
  indent_down();
  out << indent() << "}" << '\n' << '\n';

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    string field_method_prefix("writeField");
    field_id = (*f_iter)->get_key();
    field_name = (*f_iter)->get_name();
    escape_field_name = escape_string(field_name);
    // field_default_value = (*f_iter)->get_value();
    field_required = (*f_iter)->get_req();
    int32_t field_method_suffix = field_id;

    if (field_method_suffix < 0) {
      field_method_prefix += "_";
      field_method_suffix *= -1;
    }

    out << indent() << "func (p *" << tstruct_name << ") " << field_method_prefix << field_method_suffix
        << "(ctx context.Context, oprot thrift.TProtocol) (err error) {" << '\n';
    indent_up();

    if (field_required == t_field::T_OPTIONAL) {
      out << indent() << "if p.IsSet" << publicize(field_name) << "() {" << '\n';
      indent_up();
    }

    out << indent() << "if err := oprot.WriteFieldBegin(ctx, \"" << escape_field_name << "\", "
        << type_to_enum((*f_iter)->get_type()) << ", " << field_id << "); err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(fmt.Sprintf(\"%T write field begin error "
        << field_id << ":" << escape_field_name << ": \", p), err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';

    // Write field contents
    generate_serialize_field(out, *f_iter, "p.");

    // Write field closer
    out << indent() << "if err := oprot.WriteFieldEnd(ctx); err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(fmt.Sprintf(\"%T write field end error "
        << field_id << ":" << escape_field_name << ": \", p), err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';

    if (field_required == t_field::T_OPTIONAL) {
      indent_down();
      out << indent() << "}" << '\n';
    }

    out << indent() << "return err" << '\n';
    indent_down();
    out << indent() << "}" << '\n' << '\n';
  }
}

void t_go_generator::generate_go_struct_equals(ostream& out,
                                               t_struct* tstruct,
                                               const string& tstruct_name) {
  string name(tstruct->get_name());
  const vector<t_field*>& fields = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator f_iter;
  indent(out) << "func (p *" << tstruct_name << ") " << equals_method_name_ << "(other *"
              << tstruct_name << ") bool {" << '\n';
  indent_up();

  string field_name;
  string publicize_field_name;
  out << indent() << "if p == other {" << '\n';
  indent_up();
  out << indent() << "return true" << '\n';
  indent_down();
  out << indent() << "} else if p == nil || other == nil {" << '\n';
  indent_up();
  out << indent() << "return false" << '\n';
  indent_down();
  out << indent() << "}" << '\n';

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    field_name = (*f_iter)->get_name();
    t_type* field_type = (*f_iter)->get_type();
    publicize_field_name = publicize(field_name);
    string goType = type_to_go_type_with_opt(field_type, is_pointer_field(*f_iter));

    string tgt = "p." + publicize_field_name;
    string src = "other." + publicize_field_name;
    t_type* ttype = field_type->get_true_type();
    // Compare field contents
    if (is_pointer_field(*f_iter)
        && (ttype->is_base_type() || ttype->is_enum() || ttype->is_container())) {
      string tgtv = "(*" + tgt + ")";
      string srcv = "(*" + src + ")";
      out << indent() << "if " << tgt << " != " << src << " {" << '\n';
      indent_up();
      out << indent() << "if " << tgt << " == nil || " << src << " == nil {" << '\n';
      indent_up();
      out << indent() << "return false" << '\n';
      indent_down();
      out << indent() << "}" << '\n';
      generate_go_equals(out, field_type, tgtv, srcv);
      indent_down();
      out << indent() << "}" << '\n';
    } else {
      generate_go_equals(out, field_type, tgt, src);
    }
  }
  out << indent() << "return true" << '\n';
  indent_down();
  out << indent() << "}" << '\n' << '\n';
}

/**
 * Generates a thrift service.
 *
 * @param tservice The service definition
 */
void t_go_generator::generate_service(t_service* tservice) {
  string test_suffix("_test");
  string filename = lowercase(service_name_);
  string f_service_name;

  generate_service_interface(tservice);
  generate_service_client(tservice);
  generate_service_server(tservice);
  generate_service_helpers(tservice);
  if(!skip_remote_) {
    generate_service_remote(tservice);
  }
  f_types_ << '\n';
}

/**
 * Generates helper functions for a service.
 *
 * @param tservice The service to generate a header definition for
 */
void t_go_generator::generate_service_helpers(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  f_types_ << "// HELPER FUNCTIONS AND STRUCTURES" << '\n' << '\n';

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* ts = (*f_iter)->get_arglist();
    generate_go_struct_definition(f_types_, ts, false, false, true);
    generate_go_function_helpers(*f_iter);
  }
}

/**
 * Generates a struct and helpers for a function.
 *
 * @param tfunction The function
 */
void t_go_generator::generate_go_function_helpers(t_function* tfunction) {
  if (!tfunction->is_oneway()) {
    t_struct result(program_, tfunction->get_name() + "_result");
    t_field success(tfunction->get_returntype(), "success", 0);
    success.set_req(t_field::T_OPTIONAL);

    if (!tfunction->get_returntype()->is_void()) {
      result.append(&success);
    }

    t_struct* xs = tfunction->get_xceptions();
    const vector<t_field*>& fields = xs->get_members();
    vector<t_field*>::const_iterator f_iter;

    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
      t_field* f = *f_iter;
      f->set_req(t_field::T_OPTIONAL);
      result.append(f);
    }

    generate_go_struct_definition(f_types_, &result, false, true);
  }
}

/**
 * Generates a service interface definition.
 *
 * @param tservice The service to generate a header definition for
 */
void t_go_generator::generate_service_interface(t_service* tservice) {
  string extends = "";
  string extends_if = "";
  string serviceName(publicize(tservice->get_name()));
  string interfaceName = serviceName;

  if (tservice->get_extends() != nullptr) {
    extends = type_name(tservice->get_extends());
    size_t index = extends.rfind(".");

    indent_up();
    if (index != string::npos) {
      extends_if = "\n" + indent() + extends.substr(0, index + 1)
                   + publicize(extends.substr(index + 1)) + "\n";
    } else {
      extends_if = "\n" + indent() + publicize(extends) + "\n";
    }
    indent_down();
  }

  generate_deprecation_comment(f_types_, tservice->annotations_);
  f_types_ << indent() << "type " << interfaceName << " interface {" << extends_if;
  indent_up();
  generate_go_docstring(f_types_, tservice);
  vector<t_function*> functions = tservice->get_functions();

  if (!functions.empty()) {
    f_types_ << '\n';
    vector<t_function*>::iterator f_iter;

    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
      generate_go_docstring(f_types_, (*f_iter));
      generate_deprecation_comment(f_types_, (*f_iter)->annotations_);
      f_types_ << indent() << function_signature_if(*f_iter, "", true) << '\n';
    }
  }

  indent_down();
  f_types_ << indent() << "}" << '\n' << '\n';
}

/**
 * Generates a service client definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_go_generator::generate_service_client(t_service* tservice) {
  string extends = "";
  string extends_field = "";
  string extends_client = "";
  string extends_client_new = "";
  string serviceName(publicize(tservice->get_name()));

  if (tservice->get_extends() != nullptr) {
    extends = type_name(tservice->get_extends());
    size_t index = extends.rfind(".");

    if (index != string::npos) {
      extends_client = extends.substr(0, index + 1) + publicize(extends.substr(index + 1))
                       + "Client";
      extends_client_new = extends.substr(0, index + 1) + "New"
                           + publicize(extends.substr(index + 1)) + "Client";
    } else {
      extends_client = publicize(extends) + "Client";
      extends_client_new = "New" + extends_client;
    }
  }

  extends_field = extends_client.substr(extends_client.find(".") + 1);

  generate_go_docstring(f_types_, tservice);
  generate_deprecation_comment(f_types_, tservice->annotations_);
  f_types_ << indent() << "type " << serviceName << "Client struct {" << '\n';
  indent_up();

  if (!extends_client.empty()) {
    f_types_ << indent() << "*" << extends_client << '\n';
  } else {
    f_types_ << indent() << "c thrift.TClient" << '\n';
    f_types_ << indent() << "meta thrift.ResponseMeta" << '\n';
  }

  indent_down();
  f_types_ << indent() << "}" << '\n' << '\n';

  // Legacy constructor function
  generate_deprecation_comment(f_types_, tservice->annotations_);
  f_types_ << indent() << "func New" << serviceName
             << "ClientFactory(t thrift.TTransport, f thrift.TProtocolFactory) *" << serviceName
             << "Client {" << '\n';
  indent_up();
  f_types_ << indent() << "return &" << serviceName << "Client";

  if (!extends.empty()) {
    f_types_ << "{" << extends_field << ": " << extends_client_new << "Factory(t, f)}";
  } else {
    indent_up();
    f_types_ << "{" << '\n';
    f_types_ << indent() << "c: thrift.NewTStandardClient(f.GetProtocol(t), f.GetProtocol(t))," << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';
  }

  indent_down();
  f_types_ << indent() << "}" << '\n' << '\n';
  // Legacy constructor function with custom input & output protocols
  generate_deprecation_comment(f_types_, tservice->annotations_);
  f_types_
      << indent() << "func New" << serviceName
      << "ClientProtocol(t thrift.TTransport, iprot thrift.TProtocol, oprot thrift.TProtocol) *"
      << serviceName << "Client {" << '\n';
  indent_up();
  f_types_ << indent() << "return &" << serviceName << "Client";

  if (!extends.empty()) {
    f_types_ << "{" << extends_field << ": " << extends_client_new << "Protocol(t, iprot, oprot)}"
               << '\n';
  } else {
    indent_up();
    f_types_ << "{" << '\n';
    f_types_ << indent() << "c: thrift.NewTStandardClient(iprot, oprot)," << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';
  }

  indent_down();
  f_types_ << indent() << "}" << '\n' << '\n';

  // Constructor function
  generate_deprecation_comment(f_types_, tservice->annotations_);
  f_types_ << indent() << "func New" << serviceName
    << "Client(c thrift.TClient) *" << serviceName << "Client {" << '\n';
  indent_up();
  f_types_ << indent() << "return &" << serviceName << "Client{" << '\n';

  indent_up();
  if (!extends.empty()) {
    f_types_ << indent() << extends_field << ": " << extends_client_new << "(c)," << '\n';
  } else {
    f_types_ << indent() << "c: c," << '\n';
  }
  indent_down();
  f_types_ << indent() << "}" << '\n';

  indent_down();
  f_types_ << indent() << "}" << '\n' << '\n';

  if (extends.empty()) {
    f_types_ << indent() << "func (p *" << serviceName << "Client) Client_() thrift.TClient {" << '\n';
    indent_up();
    f_types_ << indent() << "return p.c" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n' << '\n';

    f_types_ << indent() << "func (p *" << serviceName << "Client) LastResponseMeta_() thrift.ResponseMeta {" << '\n';
    indent_up();
    f_types_ << indent() << "return p.meta" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n' << '\n';

    f_types_ << indent() << "func (p *" << serviceName << "Client) SetLastResponseMeta_(meta thrift.ResponseMeta) {" << '\n';
    indent_up();
    f_types_ << indent() << "p.meta = meta" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n' << '\n';
  }

  // Generate client method implementations
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* arg_struct = (*f_iter)->get_arglist();
    const vector<t_field*>& fields = arg_struct->get_members();
    vector<t_field*>::const_iterator fld_iter;
    string funname = publicize((*f_iter)->get_name());
    // Open function
    generate_go_docstring(f_types_, (*f_iter));
    generate_deprecation_comment(f_types_, (*f_iter)->annotations_);
    f_types_ << indent() << "func (p *" << serviceName << "Client) "
               << function_signature_if(*f_iter, "", true) << " {" << '\n';
    indent_up();

    std::string method = (*f_iter)->get_name();
    std::string argsType = publicize(method + "_args", true);
    std::string argsName = tmp("_args");
    f_types_ << indent() << "var " << argsName << " " << argsType << '\n';

    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      f_types_ << indent() << argsName << "." << publicize((*fld_iter)->get_name())
               << " = " << variable_name_to_go_name((*fld_iter)->get_name()) << '\n';
    }

    if (!(*f_iter)->is_oneway()) {
      std::string metaName = tmp("_meta");
      std::string resultName = tmp("_result");
      std::string resultType = publicize(method + "_result", true);
      f_types_ << indent() << "var " << resultName << " " << resultType << '\n';
      f_types_ << indent() << "var " << metaName << " thrift.ResponseMeta" << '\n';
      f_types_ << indent() << metaName << ", _err = p.Client_().Call(ctx, \""
        << method << "\", &" << argsName << ", &" << resultName << ")" << '\n';
      f_types_ << indent() << "p.SetLastResponseMeta_(" << metaName << ")" << '\n';
      f_types_ << indent() << "if _err != nil {" << '\n';

      indent_up();
      f_types_ << indent() << "return" << '\n';
      indent_down();
      f_types_ << indent() << "}" << '\n';

      t_struct* xs = (*f_iter)->get_xceptions();
      const std::vector<t_field*>& xceptions = xs->get_members();
      vector<t_field*>::const_iterator x_iter;

      if (!xceptions.empty()) {
        f_types_ << indent() << "switch {" << '\n';

        for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
          const std::string pubname = publicize((*x_iter)->get_name());
          const std::string field = resultName + "." + pubname;

          f_types_ << indent() << "case " << field << "!= nil:" << '\n';
          indent_up();

          if (!(*f_iter)->get_returntype()->is_void()) {
            f_types_ << indent() << "return _r, " << field << '\n';
          } else {
            f_types_ << indent() << "return "<< field << '\n';
          }

          indent_down();
        }

        f_types_ << indent() << "}" << '\n' << '\n';
      }

      if ((*f_iter)->get_returntype()->is_struct()) {
        // Check if the result is nil, which likely means we have a new
        // exception added but unknown to the client yet
        // (e.g. client hasn't updated the thrift file).
        // Sadly this check can only be reliable done when the return type is a
        // struct in go.
        std::string retName = tmp("_ret");
        f_types_ << indent() << "if " << retName << " := " << resultName
                 << ".GetSuccess(); " << retName << " != nil {" << '\n';
        indent_up();
        f_types_ << indent() << "return " << retName << ", nil" << '\n';
        indent_down();
        f_types_ << indent() << "}" << '\n';
        f_types_ << indent() << "return nil, "
                 << "thrift.NewTApplicationException(thrift.MISSING_RESULT, \""
                 << method << " failed: unknown result\")" << '\n';
      } else if (!(*f_iter)->get_returntype()->is_void()) {
        f_types_ << indent() << "return " << resultName << ".GetSuccess(), nil" << '\n';
      } else {
        f_types_ << indent() << "return nil" << '\n';
      }
    } else {
      // Since we don't have response meta for oneway calls, overwrite it with
      // an empty one to avoid users getting the meta from last call and
      // mistaken it as from the oneway call.
      f_types_ << indent() << "p.SetLastResponseMeta_(thrift.ResponseMeta{})" << '\n';
      // TODO: would be nice to not to duplicate the call generation
      f_types_ << indent() << "if _, err := p.Client_().Call(ctx, \""
        << method << "\", &" << argsName << ", nil); err != nil {" << '\n';

      indent_up();
      f_types_ << indent() << "return err" << '\n';
      indent_down();
      f_types_ << indent() << "}" << '\n';
      f_types_ << indent() << "return nil" << '\n';
    }

    indent_down();
    f_types_ << "}" << '\n' << '\n';
  }
}

/**
 * Generates a command line tool for making remote requests
 *
 * @param tservice The service to generate a remote for.
 */
void t_go_generator::generate_service_remote(t_service* tservice) {
  vector<t_function*> functions;
  std::unordered_map<std::string, std::string> func_to_service;

  // collect all functions including inherited functions
  t_service* parent = tservice;
  while (parent != nullptr) {
    vector<t_function*> p_functions = parent->get_functions();
    functions.insert(functions.end(), p_functions.begin(), p_functions.end());

    // We need to maintain a map of functions names to the name of their parent.
    // This is because functions may come from a parent service, and if we need
    // to create the arguments struct (e.g. `NewParentServiceNameFuncNameArgs()`)
    // we need to make sure to specify the correct service name.
    for (vector<t_function*>::iterator f_iter = p_functions.begin(); f_iter != p_functions.end(); ++f_iter) {
      auto it = func_to_service.find((*f_iter)->get_name());
      if (it == func_to_service.end()) {
        func_to_service.emplace((*f_iter)->get_name(), parent->get_name());
      }
    }

    parent = parent->get_extends();
  }

  // This file is not useful if there are no functions; don't generate it
  if (functions.size() == 0) {
    return;
  }

  string f_remote_dir = package_dir_ + "/" + underscore(service_name_) + "-remote";
  MKDIR(f_remote_dir.c_str());

  vector<t_function*>::iterator f_iter;
  string f_remote_name = f_remote_dir + "/"
                         + underscore(service_name_) + "-remote.go";
  ofstream_with_content_based_conditional_update f_remote;
  f_remote.open(f_remote_name.c_str());

  string unused_protection;

  std::vector<string> system_packages;
  system_packages.push_back("context");
  system_packages.push_back("flag");
  system_packages.push_back("fmt");
  system_packages.push_back("math");
  system_packages.push_back("net");
  system_packages.push_back("net/url");
  system_packages.push_back("os");
  system_packages.push_back("strconv");
  system_packages.push_back("strings");
  // For the thrift import, always do rename import to make sure it's called thrift.
  system_packages.push_back("thrift \"" + gen_thrift_import_ + "\"");

  f_remote << go_autogen_comment();
  f_remote << indent() << "package main" << '\n' << '\n';
  f_remote << indent() << "import (" << '\n';
  f_remote << render_system_packages(system_packages);
  f_remote << indent() << render_included_programs(unused_protection);
  f_remote << render_program_import(program_, unused_protection);
  f_remote << indent() << ")" << '\n';
  f_remote << indent() << '\n';
  f_remote << indent() << unused_protection; // filled in render_included_programs()
  f_remote << indent() << '\n';
  f_remote << indent() << "func Usage() {" << '\n';
  indent_up();
  f_remote << indent() << "fmt.Fprintln(os.Stderr, \"Usage of \", os.Args[0], \" "
                          "[-h host:port] [-u url] [-f[ramed]] function [arg1 [arg2...]]:\")"
           << '\n';
  f_remote << indent() << "flag.PrintDefaults()" << '\n';
  f_remote << indent() << "fmt.Fprintln(os.Stderr, \"\\nFunctions:\")" << '\n';

  string package_name_aliased = package_identifiers_[get_real_go_module(program_)];

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    f_remote << indent() << "fmt.Fprintln(os.Stderr, \"  " << (*f_iter)->get_returntype()->get_name() << " "
             << (*f_iter)->get_name() << "(";
    t_struct* arg_struct = (*f_iter)->get_arglist();
    const std::vector<t_field*>& args = arg_struct->get_members();
    std::vector<t_field*>::size_type num_args = args.size();
    bool first = true;

    for (std::vector<t_field*>::size_type i = 0; i < num_args; ++i) {
      if (first) {
        first = false;
      } else {
        f_remote << ", ";
      }

      f_remote << args[i]->get_type()->get_name() << " " << args[i]->get_name();
    }

    f_remote << ")\")" << '\n';
  }

  f_remote << indent() << "fmt.Fprintln(os.Stderr)" << '\n';
  f_remote << indent() << "os.Exit(0)" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  f_remote << indent() << '\n';

  f_remote << indent() << "type httpHeaders map[string]string" << '\n';
  f_remote << indent() << '\n';
  f_remote << indent() << "func (h httpHeaders) String() string {" << '\n';
  indent_up();
  f_remote << indent() << "var m map[string]string = h" << '\n';
  f_remote << indent() << "return fmt.Sprintf(\"%s\", m)" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  f_remote << indent() << '\n';
  f_remote << indent() << "func (h httpHeaders) Set(value string) error {" << '\n';
  indent_up();
  f_remote << indent() << "parts := strings.Split(value, \": \")" << '\n';
  f_remote << indent() << "if len(parts) != 2 {" << '\n';
  indent_up();
  f_remote << indent() << "return fmt.Errorf(\"header should be of format 'Key: Value'\")" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  f_remote << indent() << "h[parts[0]] = parts[1]" << '\n';
  f_remote << indent() << "return nil" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  f_remote << indent() << '\n';

  f_remote << indent() << "func main() {" << '\n';
  indent_up();
  f_remote << indent() << "flag.Usage = Usage" << '\n';
  f_remote << indent() << "var host string" << '\n';
  f_remote << indent() << "var port int" << '\n';
  f_remote << indent() << "var protocol string" << '\n';
  f_remote << indent() << "var urlString string" << '\n';
  f_remote << indent() << "var framed bool" << '\n';
  f_remote << indent() << "var useHttp bool" << '\n';
  f_remote << indent() << "headers := make(httpHeaders)" << '\n';
  f_remote << indent() << "var parsedUrl *url.URL" << '\n';
  f_remote << indent() << "var trans thrift.TTransport" << '\n';
  f_remote << indent() << "_ = strconv.Atoi" << '\n';
  f_remote << indent() << "_ = math.Abs" << '\n';
  f_remote << indent() << "flag.Usage = Usage" << '\n';
  f_remote << indent() << "flag.StringVar(&host, \"h\", \"localhost\", \"Specify host and port\")"
           << '\n';
  f_remote << indent() << "flag.IntVar(&port, \"p\", 9090, \"Specify port\")" << '\n';
  f_remote << indent() << "flag.StringVar(&protocol, \"P\", \"binary\", \""
                          "Specify the protocol (binary, compact, simplejson, json)\")" << '\n';
  f_remote << indent() << "flag.StringVar(&urlString, \"u\", \"\", \"Specify the url\")" << '\n';
  f_remote << indent() << "flag.BoolVar(&framed, \"framed\", false, \"Use framed transport\")"
           << '\n';
  f_remote << indent() << "flag.BoolVar(&useHttp, \"http\", false, \"Use http\")" << '\n';
  f_remote << indent() << "flag.Var(headers, \"H\", \"Headers to set on the http(s) request (e.g. -H \\\"Key: Value\\\")\")" << '\n';
  f_remote << indent() << "flag.Parse()" << '\n';
  f_remote << indent() << '\n';
  f_remote << indent() << "if len(urlString) > 0 {" << '\n';
  indent_up();
  f_remote << indent() << "var err error" << '\n';
  f_remote << indent() << "parsedUrl, err = url.Parse(urlString)" << '\n';
  f_remote << indent() << "if err != nil {" << '\n';
  indent_up();
  f_remote << indent() << "fmt.Fprintln(os.Stderr, \"Error parsing URL: \", err)" << '\n';
  f_remote << indent() << "flag.Usage()" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  f_remote << indent() << "host = parsedUrl.Host" << '\n';
  f_remote << indent() << "useHttp = len(parsedUrl.Scheme) <= 0 || parsedUrl.Scheme == \"http\" || parsedUrl.Scheme == \"https\""
           << '\n';
  indent_down();
  f_remote << indent() << "} else if useHttp {" << '\n';
  indent_up();
  f_remote << indent() << "_, err := url.Parse(fmt.Sprint(\"http://\", host, \":\", port))"
           << '\n';
  f_remote << indent() << "if err != nil {" << '\n';
  indent_up();
  f_remote << indent() << "fmt.Fprintln(os.Stderr, \"Error parsing URL: \", err)" << '\n';
  f_remote << indent() << "flag.Usage()" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  f_remote << indent() << '\n';
  f_remote << indent() << "cmd := flag.Arg(0)" << '\n';
  f_remote << indent() << "var err error" << '\n';
  f_remote << indent() << "var cfg *thrift.TConfiguration = nil" << '\n';
  f_remote << indent() << "if useHttp {" << '\n';
  indent_up();
  f_remote << indent() << "trans, err = thrift.NewTHttpClient(parsedUrl.String())" << '\n';
  f_remote << indent() << "if len(headers) > 0 {" << '\n';
  indent_up();
  f_remote << indent() << "httptrans := trans.(*thrift.THttpClient)" << '\n';
  f_remote << indent() << "for key, value := range headers {" << '\n';
  indent_up();
  f_remote << indent() << "httptrans.SetHeader(key, value)" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  indent_down();
  f_remote << indent() << "} else {" << '\n';
  indent_up();
  f_remote << indent() << "portStr := fmt.Sprint(port)" << '\n';
  f_remote << indent() << "if strings.Contains(host, \":\") {" << '\n';
  indent_up();
  f_remote << indent() << "host, portStr, err = net.SplitHostPort(host)" << '\n';
  f_remote << indent() << "if err != nil {" << '\n';
  indent_up();
  f_remote << indent() << "fmt.Fprintln(os.Stderr, \"error with host:\", err)"
           << '\n';
  f_remote << indent() << "os.Exit(1)" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  f_remote << indent() << "trans = thrift.NewTSocketConf(net.JoinHostPort(host, portStr), cfg)" << '\n';
  f_remote << indent() << "if err != nil {" << '\n';
  indent_up();
  f_remote << indent() << "fmt.Fprintln(os.Stderr, \"error resolving address:\", err)" << '\n';
  f_remote << indent() << "os.Exit(1)" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  f_remote << indent() << "if framed {" << '\n';
  indent_up();
  f_remote << indent() << "trans = thrift.NewTFramedTransportConf(trans, cfg)" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  f_remote << indent() << "if err != nil {" << '\n';
  indent_up();
  f_remote << indent() << "fmt.Fprintln(os.Stderr, \"Error creating transport\", err)" << '\n';
  f_remote << indent() << "os.Exit(1)" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  f_remote << indent() << "defer trans.Close()" << '\n';
  f_remote << indent() << "var protocolFactory thrift.TProtocolFactory" << '\n';
  f_remote << indent() << "switch protocol {" << '\n';
  f_remote << indent() << "case \"compact\":" << '\n';
  indent_up();
  f_remote << indent() << "protocolFactory = thrift.NewTCompactProtocolFactoryConf(cfg)" << '\n';
  indent_down();
  f_remote << indent() << "case \"simplejson\":" << '\n';
  indent_up();
  f_remote << indent() << "protocolFactory = thrift.NewTSimpleJSONProtocolFactoryConf(cfg)" << '\n';
  indent_down();
  f_remote << indent() << "case \"json\":" << '\n';
  indent_up();
  f_remote << indent() << "protocolFactory = thrift.NewTJSONProtocolFactory()" << '\n';
  indent_down();
  f_remote << indent() << "case \"binary\", \"\":" << '\n';
  indent_up();
  f_remote << indent() << "protocolFactory = thrift.NewTBinaryProtocolFactoryConf(cfg)" << '\n';
  indent_down();
  f_remote << indent() << "default:" << '\n';
  indent_up();
  f_remote << indent() << "fmt.Fprintln(os.Stderr, \"Invalid protocol specified: \", protocol)"
           << '\n';
  f_remote << indent() << "Usage()" << '\n';
  f_remote << indent() << "os.Exit(1)" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  f_remote << indent() << "iprot := protocolFactory.GetProtocol(trans)" << '\n';
  f_remote << indent() << "oprot := protocolFactory.GetProtocol(trans)" << '\n';
  f_remote << indent() << "client := " << package_name_aliased << ".New" << publicize(service_name_)
           << "Client(thrift.NewTStandardClient(iprot, oprot))" << '\n';
  f_remote << indent() << "if err := trans.Open(); err != nil {" << '\n';
  indent_up();
  f_remote << indent() << "fmt.Fprintln(os.Stderr, \"Error opening socket to \", "
                          "host, \":\", port, \" \", err)" << '\n';
  f_remote << indent() << "os.Exit(1)" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  f_remote << indent() << '\n';
  f_remote << indent() << "switch cmd {" << '\n';

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* arg_struct = (*f_iter)->get_arglist();
    const std::vector<t_field*>& args = arg_struct->get_members();
    std::vector<t_field*>::size_type num_args = args.size();
    string funcName((*f_iter)->get_name());
    string pubName(publicize(funcName));
    string argumentsName(publicize(funcName + "_args", true, func_to_service[funcName]));
    f_remote << indent() << "case \"" << escape_string(funcName) << "\":" << '\n';
    indent_up();
    f_remote << indent() << "if flag.NArg() - 1 != " << num_args << " {" << '\n';
    indent_up();
    f_remote << indent() << "fmt.Fprintln(os.Stderr, \"" << escape_string(pubName) << " requires "
             << num_args << " args\")" << '\n';
    f_remote << indent() << "flag.Usage()" << '\n';
    indent_down();
    f_remote << indent() << "}" << '\n';

    for (std::vector<t_field*>::size_type i = 0; i < num_args; ++i) {
      std::vector<t_field*>::size_type flagArg = i + 1;
      t_type* the_type(args[i]->get_type());
      t_type* the_type2(get_true_type(the_type));

      if (the_type2->is_enum()) {
        f_remote << indent() << "tmp" << i << ", err := (strconv.Atoi(flag.Arg(" << flagArg << ")))"
                 << '\n';
        f_remote << indent() << "if err != nil {" << '\n';
        indent_up();
        f_remote << indent() << "Usage()" << '\n';
        f_remote << indent() << "return" << '\n';
        indent_down();
        f_remote << indent() << "}" << '\n';
        f_remote << indent() << "argvalue" << i << " := " << package_name_aliased << "."
                 << publicize(the_type->get_name()) << "(tmp" << i << ")" << '\n';
      } else if (the_type2->is_base_type()) {
        t_base_type::t_base e = ((t_base_type*)the_type2)->get_base();
        string err(tmp("err"));

        switch (e) {
        case t_base_type::TYPE_VOID:
          break;

        case t_base_type::TYPE_STRING:
          if (the_type2->is_binary()) {
            f_remote << indent() << "argvalue" << i << " := []byte(flag.Arg(" << flagArg << "))"
                     << '\n';
          } else {
            f_remote << indent() << "argvalue" << i << " := flag.Arg(" << flagArg << ")" << '\n';
          }
          break;

        case t_base_type::TYPE_BOOL:
          f_remote << indent() << "argvalue" << i << " := flag.Arg(" << flagArg << ") == \"true\""
                   << '\n';
          break;

        case t_base_type::TYPE_I8:
          f_remote << indent() << "tmp" << i << ", " << err << " := (strconv.Atoi(flag.Arg("
                   << flagArg << ")))" << '\n';
          f_remote << indent() << "if " << err << " != nil {" << '\n';
          indent_up();
          f_remote << indent() << "Usage()" << '\n';
          f_remote << indent() << "return" << '\n';
          indent_down();
          f_remote << indent() << "}" << '\n';
          f_remote << indent() << "argvalue" << i << " := int8(tmp" << i << ")" << '\n';
          break;

        case t_base_type::TYPE_I16:
          f_remote << indent() << "tmp" << i << ", " << err << " := (strconv.Atoi(flag.Arg("
                   << flagArg << ")))" << '\n';
          f_remote << indent() << "if " << err << " != nil {" << '\n';
          indent_up();
          f_remote << indent() << "Usage()" << '\n';
          f_remote << indent() << "return" << '\n';
          indent_down();
          f_remote << indent() << "}" << '\n';
          f_remote << indent() << "argvalue" << i << " := int16(tmp" << i << ")" << '\n';
          break;

        case t_base_type::TYPE_I32:
          f_remote << indent() << "tmp" << i << ", " << err << " := (strconv.Atoi(flag.Arg("
                   << flagArg << ")))" << '\n';
          f_remote << indent() << "if " << err << " != nil {" << '\n';
          indent_up();
          f_remote << indent() << "Usage()" << '\n';
          f_remote << indent() << "return" << '\n';
          indent_down();
          f_remote << indent() << "}" << '\n';
          f_remote << indent() << "argvalue" << i << " := int32(tmp" << i << ")" << '\n';
          break;

        case t_base_type::TYPE_I64:
          f_remote << indent() << "argvalue" << i << ", " << err
                   << " := (strconv.ParseInt(flag.Arg(" << flagArg << "), 10, 64))" << '\n';
          f_remote << indent() << "if " << err << " != nil {" << '\n';
          indent_up();
          f_remote << indent() << "Usage()" << '\n';
          f_remote << indent() << "return" << '\n';
          indent_down();
          f_remote << indent() << "}" << '\n';
          break;

        case t_base_type::TYPE_DOUBLE:
          f_remote << indent() << "argvalue" << i << ", " << err
                   << " := (strconv.ParseFloat(flag.Arg(" << flagArg << "), 64))" << '\n';
          f_remote << indent() << "if " << err << " != nil {" << '\n';
          indent_up();
          f_remote << indent() << "Usage()" << '\n';
          f_remote << indent() << "return" << '\n';
          indent_down();
          f_remote << indent() << "}" << '\n';
          break;

        case t_base_type::TYPE_UUID:
          f_remote << indent() << "argvalue" << i << ", " << err
                   << " := (thrift.ParseTuuid(flag.Arg(" << flagArg << ")))" << '\n';
          f_remote << indent() << "if " << err << " != nil {" << '\n';
          indent_up();
          f_remote << indent() << "Usage()" << '\n';
          f_remote << indent() << "return" << '\n';
          indent_down();
          f_remote << indent() << "}" << '\n';
          break;

        default:
          throw("Invalid base type in generate_service_remote");
        }

        // f_remote << publicize(args[i]->get_name()) << "(strconv.Atoi(flag.Arg(" << flagArg <<
        // ")))";
      } else if (the_type2->is_struct()) {
        string arg(tmp("arg"));
        string mbTrans(tmp("mbTrans"));
        string err1(tmp("err"));
        string factory(tmp("factory"));
        string jsProt(tmp("jsProt"));
        string err2(tmp("err"));
        std::string tstruct_name(publicize(the_type->get_name()));
        std::string tstruct_module( module_name(the_type));
        if(tstruct_module.empty()) {
          tstruct_module = package_name_aliased;
        }

        f_remote << indent() << arg << " := flag.Arg(" << flagArg << ")" << '\n';
        f_remote << indent() << mbTrans << " := thrift.NewTMemoryBufferLen(len(" << arg << "))"
                 << '\n';
        f_remote << indent() << "defer " << mbTrans << ".Close()" << '\n';
        f_remote << indent() << "_, " << err1 << " := " << mbTrans << ".WriteString(" << arg << ")"
                 << '\n';
        f_remote << indent() << "if " << err1 << " != nil {" << '\n';
        indent_up();
        f_remote << indent() << "Usage()" << '\n';
        f_remote << indent() << "return" << '\n';
        indent_down();
        f_remote << indent() << "}" << '\n';
        f_remote << indent() << factory << " := thrift.NewTJSONProtocolFactory()" << '\n';
        f_remote << indent() << jsProt << " := " << factory << ".GetProtocol(" << mbTrans << ")"
                 << '\n';
        f_remote << indent() << "argvalue" << i << " := " << tstruct_module << ".New" << tstruct_name
                 << "()" << '\n';
        f_remote << indent() << err2 << " := argvalue" << i << "." << read_method_name_ << "(context.Background(), " << jsProt << ")" << '\n';
        f_remote << indent() << "if " << err2 << " != nil {" << '\n';
        indent_up();
        f_remote << indent() << "Usage()" << '\n';
        f_remote << indent() << "return" << '\n';
        indent_down();
        f_remote << indent() << "}" << '\n';
      } else if (the_type2->is_container() || the_type2->is_xception()) {
        string arg(tmp("arg"));
        string mbTrans(tmp("mbTrans"));
        string err1(tmp("err"));
        string factory(tmp("factory"));
        string jsProt(tmp("jsProt"));
        string err2(tmp("err"));
        std::string argName(publicize(args[i]->get_name()));
        f_remote << indent() << arg << " := flag.Arg(" << flagArg << ")" << '\n';
        f_remote << indent() << mbTrans << " := thrift.NewTMemoryBufferLen(len(" << arg << "))"
                 << '\n';
        f_remote << indent() << "defer " << mbTrans << ".Close()" << '\n';
        f_remote << indent() << "_, " << err1 << " := " << mbTrans << ".WriteString(" << arg << ")"
                 << '\n';
        f_remote << indent() << "if " << err1 << " != nil { " << '\n';
        indent_up();
        f_remote << indent() << "Usage()" << '\n';
        f_remote << indent() << "return" << '\n';
        indent_down();
        f_remote << indent() << "}" << '\n';
        f_remote << indent() << factory << " := thrift.NewTJSONProtocolFactory()" << '\n';
        f_remote << indent() << jsProt << " := " << factory << ".GetProtocol(" << mbTrans << ")"
                 << '\n';
        f_remote << indent() << "containerStruct" << i << " := " << package_name_aliased << ".New"
                 << argumentsName << "()" << '\n';
        f_remote << indent() << err2 << " := containerStruct" << i << ".ReadField" << (i + 1) << "(context.Background(), "
                 << jsProt << ")" << '\n';
        f_remote << indent() << "if " << err2 << " != nil {" << '\n';
        indent_up();
        f_remote << indent() << "Usage()" << '\n';
        f_remote << indent() << "return" << '\n';
        indent_down();
        f_remote << indent() << "}" << '\n';
        f_remote << indent() << "argvalue" << i << " := containerStruct" << i << "." << argName
                 << '\n';
      } else {
        throw("Invalid argument type in generate_service_remote");
      }

      if (the_type->is_typedef()) {
        std::string typedef_module(module_name(the_type));
        if(typedef_module.empty()) {
          typedef_module = package_name_aliased;
        }
        f_remote << indent() << "value" << i << " := " << typedef_module << "."
                 << publicize(the_type->get_name()) << "(argvalue" << i << ")" << '\n';
      } else {
        f_remote << indent() << "value" << i << " := argvalue" << i << '\n';
      }
    }

    f_remote << indent() << "fmt.Print(client." << pubName << "(";
    bool argFirst = true;

    f_remote << "context.Background()";
    for (std::vector<t_field*>::size_type i = 0; i < num_args; ++i) {
      if (argFirst) {
        argFirst = false;
        f_remote << ", ";
      } else {
        f_remote << ", ";
      }

      if (args[i]->get_type()->is_enum()) {
        f_remote << "value" << i;
      } else if (args[i]->get_type()->is_base_type()) {
        t_base_type::t_base e = ((t_base_type*)(args[i]->get_type()))->get_base();

        switch (e) {
        case t_base_type::TYPE_VOID:
          break;

        case t_base_type::TYPE_STRING:
        case t_base_type::TYPE_BOOL:
        case t_base_type::TYPE_I8:
        case t_base_type::TYPE_I16:
        case t_base_type::TYPE_I32:
        case t_base_type::TYPE_I64:
        case t_base_type::TYPE_DOUBLE:
        case t_base_type::TYPE_UUID:
          f_remote << "value" << i;
          break;

        default:
          throw("Invalid base type in generate_service_remote");
        }

        // f_remote << publicize(args[i]->get_name()) << "(strconv.Atoi(flag.Arg(" << flagArg <<
        // ")))";
      } else {
        f_remote << "value" << i;
      }
    }

    f_remote << "))" << '\n';
    f_remote << indent() << "fmt.Print(\"\\n\")" << '\n';
    f_remote << indent() << "break" << '\n';
    indent_down();
  }

  f_remote << indent() << "case \"\":" << '\n';
  indent_up();
  f_remote << indent() << "Usage()" << '\n';
  indent_down();
  f_remote << indent() << "default:" << '\n';
  indent_up();
  f_remote << indent() << "fmt.Fprintln(os.Stderr, \"Invalid function \", cmd)" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  indent_down();
  f_remote << indent() << "}" << '\n';
  // Close service file
  f_remote.close();
  format_go_output(f_remote_name);
#ifndef _MSC_VER
  // Make file executable, love that bitwise OR action
  chmod(f_remote_name.c_str(),
        S_IRUSR | S_IWUSR | S_IXUSR
#ifndef _WIN32
        | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH
#endif
        );
#endif
}

/**
 * Generates a service server definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_go_generator::generate_service_server(t_service* tservice) {
  // Generate the dispatch methods
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  string extends = "";
  string extends_processor = "";
  string extends_processor_new = "";
  string serviceName(publicize(tservice->get_name()));

  if (tservice->get_extends() != nullptr) {
    extends = type_name(tservice->get_extends());
    size_t index = extends.rfind(".");

    if (index != string::npos) {
      extends_processor = extends.substr(0, index + 1) + publicize(extends.substr(index + 1))
                          + "Processor";
      extends_processor_new = extends.substr(0, index + 1) + "New"
                              + publicize(extends.substr(index + 1)) + "Processor";
    } else {
      extends_processor = publicize(extends) + "Processor";
      extends_processor_new = "New" + extends_processor;
    }
  }

  string pServiceName(privatize(tservice->get_name()));
  // Generate the header portion
  string self(tmp("self"));

  if (extends_processor.empty()) {
    generate_deprecation_comment(f_types_, tservice->annotations_);
    f_types_ << indent() << "type " << serviceName << "Processor struct {" << '\n';
    indent_up();
    f_types_ << indent() << "processorMap map[string]thrift.TProcessorFunction" << '\n';
    f_types_ << indent() << "handler " << serviceName << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n' << '\n';
    f_types_ << indent() << "func (p *" << serviceName
               << "Processor) AddToProcessorMap(key string, processor thrift.TProcessorFunction) {"
               << '\n';
    indent_up();
    f_types_ << indent() << "p.processorMap[key] = processor" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n' << '\n';
    f_types_ << indent() << "func (p *" << serviceName
               << "Processor) GetProcessorFunction(key string) "
                  "(processor thrift.TProcessorFunction, ok bool) {" << '\n';
    indent_up();
    f_types_ << indent() << "processor, ok = p.processorMap[key]" << '\n';
    f_types_ << indent() << "return processor, ok" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n' << '\n';
    f_types_ << indent() << "func (p *" << serviceName
               << "Processor) ProcessorMap() map[string]thrift.TProcessorFunction {" << '\n';
    indent_up();
    f_types_ << indent() << "return p.processorMap" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n' << '\n';
    generate_deprecation_comment(f_types_, tservice->annotations_);
    f_types_ << indent() << "func New" << serviceName << "Processor(handler " << serviceName
               << ") *" << serviceName << "Processor {" << '\n' << '\n';
    indent_up();
    f_types_
        << indent() << self << " := &" << serviceName
        << "Processor{handler:handler, processorMap:make(map[string]thrift.TProcessorFunction)}"
        << '\n';

    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
      string escapedFuncName(escape_string((*f_iter)->get_name()));
      f_types_ << indent() << self << ".processorMap[\"" << escapedFuncName << "\"] = &"
                 << pServiceName << "Processor" << publicize((*f_iter)->get_name())
                 << "{handler:handler}" << '\n';
    }

    string x(tmp("x"));
    f_types_ << indent() << "return " << self << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n' << '\n';
    f_types_ << indent() << "func (p *" << serviceName
               << "Processor) Process(ctx context.Context, iprot, oprot thrift.TProtocol) (success bool, err "
                  "thrift.TException) {" << '\n';
    indent_up();
    f_types_ << indent() << "name, _, seqId, err2 := iprot.ReadMessageBegin(ctx)" << '\n';
    f_types_ << indent() << "if err2 != nil { return false, thrift.WrapTException(err2) }" << '\n';
    f_types_ << indent() << "if processor, ok := p.GetProcessorFunction(name); ok {" << '\n';
    indent_up();
    f_types_ << indent() << "return processor.Process(ctx, seqId, iprot, oprot)" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';
    f_types_ << indent() << "iprot.Skip(ctx, thrift.STRUCT)" << '\n';
    f_types_ << indent() << "iprot.ReadMessageEnd(ctx)" << '\n';
    f_types_ << indent() << "" << x
               << " := thrift.NewTApplicationException(thrift.UNKNOWN_METHOD, \"Unknown function "
                  "\" + name)" << '\n';
    f_types_ << indent() << "oprot.WriteMessageBegin(ctx, name, thrift.EXCEPTION, seqId)" << '\n';
    f_types_ << indent() << "" << x << ".Write(ctx, oprot)" << '\n';
    f_types_ << indent() << "oprot.WriteMessageEnd(ctx)" << '\n';
    f_types_ << indent() << "oprot.Flush(ctx)" << '\n';
    f_types_ << indent() << "return false, " << x << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n' << '\n';
  } else {
    f_types_ << indent() << "type " << serviceName << "Processor struct {" << '\n';
    indent_up();
    f_types_ << indent() << "*" << extends_processor << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n' << '\n';
    f_types_ << indent() << "func New" << serviceName << "Processor(handler " << serviceName
               << ") *" << serviceName << "Processor {" << '\n';
    indent_up();
    f_types_ << indent() << self << " := &" << serviceName << "Processor{"
               << extends_processor_new << "(handler)}" << '\n';

    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
      string escapedFuncName(escape_string((*f_iter)->get_name()));
      f_types_ << indent() << self << ".AddToProcessorMap(\"" << escapedFuncName
                 << "\", &" << pServiceName << "Processor" << publicize((*f_iter)->get_name())
                 << "{handler:handler})" << '\n';
    }

    f_types_ << indent() << "return " << self << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n' << '\n';
  }

  // Generate the process subfunctions
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_process_function(tservice, *f_iter);
  }

  f_types_ << '\n';
}

/**
 * Generates a process function definition.
 *
 * @param tfunction The function to write a dispatcher for
 */
void t_go_generator::generate_process_function(t_service* tservice, t_function* tfunction) {
  // Open function
  string processorName = privatize(tservice->get_name()) + "Processor"
                         + publicize(tfunction->get_name());
  string argsname = publicize(tfunction->get_name() + "_args", true);
  string resultname = publicize(tfunction->get_name() + "_result", true);

  // t_struct* xs = tfunction->get_xceptions();
  // const std::vector<t_field*>& xceptions = xs->get_members();
  f_types_ << indent() << "type " << processorName << " struct {" << '\n';
  indent_up();
  f_types_ << indent() << "handler " << publicize(tservice->get_name()) << '\n';
  indent_down();
  f_types_ << indent() << "}" << '\n' << '\n';
  f_types_ << indent() << "func (p *" << processorName
             << ") Process(ctx context.Context, seqId int32, iprot, oprot thrift.TProtocol) (success bool, err "
                "thrift.TException) {" << '\n';
  indent_up();
  string write_err;
  if (!tfunction->is_oneway()) {
    write_err = tmp("_write_err");
    f_types_ << indent() << "var " << write_err << " error" << '\n';
  }
  f_types_ << indent() << "args := " << argsname << "{}" << '\n';
  f_types_ << indent() << "if err2 := args." << read_method_name_ << "(ctx, iprot); err2 != nil {" << '\n';
  indent_up();
  f_types_ << indent() << "iprot.ReadMessageEnd(ctx)" << '\n';
  if (!tfunction->is_oneway()) {
    f_types_ << indent()
               << "x := thrift.NewTApplicationException(thrift.PROTOCOL_ERROR, err2.Error())"
               << '\n';
    f_types_ << indent() << "oprot.WriteMessageBegin(ctx, \"" << escape_string(tfunction->get_name())
               << "\", thrift.EXCEPTION, seqId)" << '\n';
    f_types_ << indent() << "x.Write(ctx, oprot)" << '\n';
    f_types_ << indent() << "oprot.WriteMessageEnd(ctx)" << '\n';
    f_types_ << indent() << "oprot.Flush(ctx)" << '\n';
  }
  f_types_ << indent() << "return false, thrift.WrapTException(err2)" << '\n';
  indent_down();
  f_types_ << indent() << "}" << '\n';
  f_types_ << indent() << "iprot.ReadMessageEnd(ctx)" << '\n' << '\n';

  // Even though we never create the goroutine in oneway handlers,
  // always have (nop) tickerCancel defined makes the writing part of code
  // generating easier and less error-prone.
  f_types_ << indent() << "tickerCancel := func() {}" << '\n';
  // Only create the goroutine for non-oneways.
  if (!tfunction->is_oneway()) {
    f_types_ << indent() << "// Start a goroutine to do server side connectivity check." << '\n';
    f_types_ << indent() << "if thrift.ServerConnectivityCheckInterval > 0 {" << '\n';

    indent_up();
    f_types_ << indent() << "var cancel context.CancelCauseFunc" << '\n';
    f_types_ << indent() << "ctx, cancel = context.WithCancelCause(ctx)" << '\n';
    f_types_ << indent() << "defer cancel(nil)" << '\n';
    f_types_ << indent() << "var tickerCtx context.Context" << '\n';
    f_types_ << indent() << "tickerCtx, tickerCancel = context.WithCancel(context.Background())" << '\n';
    f_types_ << indent() << "defer tickerCancel()" << '\n';
    f_types_ << indent() << "go func(ctx context.Context, cancel context.CancelCauseFunc) {" << '\n';

    indent_up();
    f_types_ << indent() << "ticker := time.NewTicker(thrift.ServerConnectivityCheckInterval)" << '\n';
    f_types_ << indent() << "defer ticker.Stop()" << '\n';
    f_types_ << indent() << "for {" << '\n';

    indent_up();
    f_types_ << indent() << "select {" << '\n';
    f_types_ << indent() << "case <-ctx.Done():" << '\n';
    indent_up();
    f_types_ << indent() << "return" << '\n';
    indent_down();
    f_types_ << indent() << "case <-ticker.C:" << '\n';

    indent_up();
    f_types_ << indent() << "if !iprot.Transport().IsOpen() {" << '\n';
    indent_up();
    f_types_ << indent() << "cancel(thrift.ErrAbandonRequest)" << '\n';
    f_types_ << indent() << "return" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';
    indent_down();
    f_types_ << indent() << "}(tickerCtx, cancel)" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n' << '\n';
  } else {
    // Make sure we don't get the defined but unused compiling error.
    f_types_ << indent() << "_ = tickerCancel" << '\n' << '\n';
  }

  if (!tfunction->is_oneway()) {
    f_types_ << indent() << "result := " << resultname << "{}" << '\n';
  }
  bool need_reference = type_need_reference(tfunction->get_returntype());

  f_types_ << indent() << "if ";

  if (!tfunction->is_oneway()) {
    if (!tfunction->get_returntype()->is_void()) {
      f_types_ << "retval, ";
    }
  }

  // Generate the function call
  t_struct* arg_struct = tfunction->get_arglist();
  const std::vector<t_field*>& fields = arg_struct->get_members();
  vector<t_field*>::const_iterator f_iter;
  f_types_ << "err2 := p.handler." << publicize(tfunction->get_name()) << "(";
  bool first = true;

  f_types_ << "ctx";
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
      f_types_ << ", ";
    } else {
      f_types_ << ", ";
    }

    f_types_ << "args." << publicize((*f_iter)->get_name());
  }

  f_types_ << "); err2 != nil {" << '\n';
  indent_up();
  f_types_ << indent() << "tickerCancel()" << '\n';
  f_types_ << indent() << "err = thrift.WrapTException(err2)" << '\n';

  t_struct* exceptions = tfunction->get_xceptions();
  const vector<t_field*>& x_fields = exceptions->get_members();
  if (!x_fields.empty()) {
    f_types_ << indent() << "switch v := err2.(type) {" << '\n';

    vector<t_field*>::const_iterator xf_iter;

    for (xf_iter = x_fields.begin(); xf_iter != x_fields.end(); ++xf_iter) {
      f_types_ << indent() << "case " << type_to_go_type(((*xf_iter)->get_type())) << ":"
                 << '\n';
      indent_up();
      f_types_ << indent() << "result." << publicize((*xf_iter)->get_name()) << " = v" << '\n';
      indent_down();
    }

    f_types_ << indent() << "default:" << '\n';
    indent_up();
  }

  if (!tfunction->is_oneway()) {
    // Avoid writing the error to the wire if it's ErrAbandonRequest
    f_types_ << indent() << "if errors.Is(err2, thrift.ErrAbandonRequest) {" << '\n';
    indent_up();
    f_types_ << indent() << "return false, thrift.WrapTException(err2)" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';
    f_types_ << indent() << "if errors.Is(err2, context.Canceled) {" << '\n';
    indent_up();
    f_types_ << indent() << "if err := context.Cause(ctx); errors.Is(err, thrift.ErrAbandonRequest) {" << '\n';
    indent_up();
    f_types_ << indent() << "return false, thrift.WrapTException(err)" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';

    string exc(tmp("_exc"));
    f_types_ << indent() << exc << " := thrift.NewTApplicationException(thrift.INTERNAL_ERROR, "
                              "\"Internal error processing " << escape_string(tfunction->get_name())
               << ": \" + err2.Error())" << '\n';

    f_types_ << indent() << "if err2 := oprot.WriteMessageBegin(ctx, \"" << escape_string(tfunction->get_name())
               << "\", thrift.EXCEPTION, seqId); err2 != nil {" << '\n';
    indent_up();
    f_types_ << indent() << write_err << " = thrift.WrapTException(err2)" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';

    f_types_ << indent() << "if err2 := " << exc << ".Write(ctx, oprot); "
               << write_err << " == nil && err2 != nil {" << '\n';
    indent_up();
    f_types_ << indent() << write_err << " = thrift.WrapTException(err2)" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';

    f_types_ << indent() << "if err2 := oprot.WriteMessageEnd(ctx); "
               << write_err << " == nil && err2 != nil {" << '\n';
    indent_up();
    f_types_ << indent() << write_err << " = thrift.WrapTException(err2)" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';

    f_types_ << indent() << "if err2 := oprot.Flush(ctx); "
               << write_err << " == nil && err2 != nil {" << '\n';
    indent_up();
    f_types_ << indent() << write_err << " = thrift.WrapTException(err2)" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';

    f_types_ << indent() << "if " << write_err << " != nil {" << '\n';
    indent_up();
    f_types_ << indent() << "return false, thrift.WrapTException(" << write_err << ")" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';

    // return success=true as long as writing to the wire was successful.
    f_types_ << indent() << "return true, err" << '\n';
  }

  if (!x_fields.empty()) {
    indent_down();
    f_types_ << indent() << "}" << '\n'; // closes switch
  }

  indent_down();
  f_types_ << indent() << "}"; // closes err2 != nil

  if (!tfunction->is_oneway()) {
    if (!tfunction->get_returntype()->is_void()) {
      f_types_ << " else {" << '\n'; // make sure we set Success retval only on success
      indent_up();
      f_types_ << indent() << "result.Success = ";
      if (need_reference) {
        f_types_ << "&";
      }
      f_types_ << "retval" << '\n';
      indent_down();
      f_types_ << indent() << "}" << '\n';
    } else {
      f_types_ << '\n';
    }
    f_types_ << indent() << "tickerCancel()" << '\n';

    f_types_ << indent() << "if err2 := oprot.WriteMessageBegin(ctx, \""
               << escape_string(tfunction->get_name()) << "\", thrift.REPLY, seqId); err2 != nil {"
               << '\n';
    indent_up();
    f_types_ << indent() << write_err << " = thrift.WrapTException(err2)" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';

    f_types_ << indent() << "if err2 := result." << write_method_name_ << "(ctx, oprot); "
               << write_err << " == nil && err2 != nil {" << '\n';
    indent_up();
    f_types_ << indent() << write_err << " = thrift.WrapTException(err2)" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';

    f_types_ << indent() << "if err2 := oprot.WriteMessageEnd(ctx); "
               << write_err << " == nil && err2 != nil {" << '\n';
    indent_up();
    f_types_ << indent() << write_err << " = thrift.WrapTException(err2)" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';

    f_types_ << indent() << "if err2 := oprot.Flush(ctx); " << write_err << " == nil && err2 != nil {" << '\n';
    indent_up();
    f_types_ << indent() << write_err << " = thrift.WrapTException(err2)" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';

    f_types_ << indent() << "if " << write_err << " != nil {" << '\n';
    indent_up();
    f_types_ << indent() << "return false, thrift.WrapTException(" << write_err << ")" << '\n';
    indent_down();
    f_types_ << indent() << "}" << '\n';

    // return success=true as long as writing to the wire was successful.
    f_types_ << indent() << "return true, err" << '\n';
  } else {
    f_types_ << '\n';
    f_types_ << indent() << "tickerCancel()" << '\n';
    f_types_ << indent() << "return true, err" << '\n';
  }
  indent_down();
  f_types_ << indent() << "}" << '\n' << '\n';
}

/**
 * Deserializes a field of any type.
 */
void t_go_generator::generate_deserialize_field(ostream& out,
                                                t_field* tfield,
                                                bool declare,
                                                string prefix,
                                                bool inclass,
                                                bool coerceData,
                                                bool inkey,
                                                bool in_container_value) {
  (void)inclass;
  (void)coerceData;
  t_type* orig_type = tfield->get_type();
  t_type* type = get_true_type(orig_type);
  string name(prefix + publicize(tfield->get_name()));

  if (type->is_void()) {
    throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE: " + name;
  }

  if (type->is_struct() || type->is_xception()) {
    generate_deserialize_struct(out,
                                (t_struct*)type,
                                is_pointer_field(tfield, in_container_value),
                                declare,
                                name);
  } else if (type->is_container()) {
    generate_deserialize_container(out, orig_type, is_pointer_field(tfield), declare, name);
  } else if (type->is_base_type() || type->is_enum()) {

    if (declare) {
      string type_name = inkey ? type_to_go_key_type(tfield->get_type())
                               : type_to_go_type(tfield->get_type());

      out << indent() << "var " << tfield->get_name() << " " << type_name << '\n';
    }

    indent(out) << "if v, err := iprot.";

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();

      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " + name;
        break;

      case t_base_type::TYPE_STRING:
        if (type->is_binary() && !inkey) {
          out << "ReadBinary(ctx)";
        } else {
          out << "ReadString(ctx)";
        }

        break;

      case t_base_type::TYPE_BOOL:
        out << "ReadBool(ctx)";
        break;

      case t_base_type::TYPE_I8:
        out << "ReadByte(ctx)";
        break;

      case t_base_type::TYPE_I16:
        out << "ReadI16(ctx)";
        break;

      case t_base_type::TYPE_I32:
        out << "ReadI32(ctx)";
        break;

      case t_base_type::TYPE_I64:
        out << "ReadI64(ctx)";
        break;

      case t_base_type::TYPE_DOUBLE:
        out << "ReadDouble(ctx)";
        break;

      case t_base_type::TYPE_UUID:
        out << "ReadUUID(ctx)";
        break;

      default:
        throw "compiler error: no Go name for base type " + t_base_type::t_base_name(tbase);
      }
    } else if (type->is_enum()) {
      out << "ReadI32(ctx)";
    }

    out << "; err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(\"error reading field " << tfield->get_key()
        << ": \", err)" << '\n';

    indent_down();
    out << indent() << "} else {" << '\n';
    indent_up();
    string wrap;

    if (type->is_enum() || orig_type->is_typedef()) {
      wrap = publicize(type_name(orig_type));
    } else if (((t_base_type*)type)->get_base() == t_base_type::TYPE_I8) {
      wrap = "int8";
    }

    string maybe_address = (is_pointer_field(tfield) ? "&" : "");
    if (wrap == "") {
      indent(out) << name << " = " << maybe_address << "v" << '\n';
    } else {
      indent(out) << "temp := " << wrap << "(v)" << '\n';
      indent(out) << name << " = " << maybe_address << "temp" << '\n';
    }

    indent_down();
    out << indent() << "}" << '\n';
  } else {
    throw "INVALID TYPE IN generate_deserialize_field '" + type->get_name() + "' for field '"
        + tfield->get_name() + "'";
  }
}

/**
 * Generates an unserializer for a struct, calling read()
 */
void t_go_generator::generate_deserialize_struct(ostream& out,
                                                 t_struct* tstruct,
                                                 bool pointer_field,
                                                 bool declare,
                                                 string prefix) {
  string eq(declare ? " := " : " = ");

  out << indent() << prefix << eq << (pointer_field ? "&" : "");
  generate_go_struct_initializer(out, tstruct);
  out << indent() << "if err := " << prefix << "." << read_method_name_ << "(ctx, iprot); err != nil {" << '\n';
  indent_up();
  out << indent() << "return thrift.PrependError(fmt.Sprintf(\"%T error reading struct: \", "
      << prefix << "), err)" << '\n';
  indent_down();
  out << indent() << "}" << '\n';
}

/**
 * Serialize a container by writing out the header followed by
 * data and then a footer.
 */
void t_go_generator::generate_deserialize_container(ostream& out,
                                                    t_type* orig_type,
                                                    bool pointer_field,
                                                    bool declare,
                                                    string prefix) {
  t_type* ttype = get_true_type(orig_type);
  string eq(" = ");

  if (declare) {
    eq = " := ";
  }

  // Declare variables, read header
  if (ttype->is_map()) {
    out << indent() << "_, _, size, err := iprot.ReadMapBegin(ctx)" << '\n';
    out << indent() << "if err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(\"error reading map begin: \", err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    out << indent() << "tMap := make(" << type_to_go_type(orig_type) << ", size)" << '\n';
    out << indent() << prefix << eq << (pointer_field ? "&" : "") << "tMap" << '\n';
  } else if (ttype->is_set()) {
    out << indent() << "_, size, err := iprot.ReadSetBegin(ctx)" << '\n';
    out << indent() << "if err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(\"error reading set begin: \", err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    out << indent() << "tSet := make(" << type_to_go_type(orig_type) << ", 0, size)" << '\n';
    out << indent() << prefix << eq << (pointer_field ? "&" : "") << "tSet" << '\n';
  } else if (ttype->is_list()) {
    out << indent() << "_, size, err := iprot.ReadListBegin(ctx)" << '\n';
    out << indent() << "if err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(\"error reading list begin: \", err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    out << indent() << "tSlice := make(" << type_to_go_type(orig_type) << ", 0, size)" << '\n';
    out << indent() << prefix << eq << (pointer_field ? "&" : "") << "tSlice" << '\n';
  } else {
    throw "INVALID TYPE IN generate_deserialize_container '" + ttype->get_name() + "' for prefix '"
        + prefix + "'";
  }

  // For loop iterates over elements
  out << indent() << "for i := 0; i < size; i++ {" << '\n';
  indent_up();

  if (pointer_field) {
    prefix = "(*" + prefix + ")";
  }
  if (ttype->is_map()) {
    generate_deserialize_map_element(out, (t_map*)ttype, declare, prefix);
  } else if (ttype->is_set()) {
    generate_deserialize_set_element(out, (t_set*)ttype, declare, prefix);
  } else if (ttype->is_list()) {
    generate_deserialize_list_element(out, (t_list*)ttype, declare, prefix);
  }

  indent_down();
  out << indent() << "}" << '\n';

  // Read container end
  if (ttype->is_map()) {
    out << indent() << "if err := iprot.ReadMapEnd(ctx); err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(\"error reading map end: \", err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
  } else if (ttype->is_set()) {
    out << indent() << "if err := iprot.ReadSetEnd(ctx); err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(\"error reading set end: \", err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
  } else if (ttype->is_list()) {
    out << indent() << "if err := iprot.ReadListEnd(ctx); err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(\"error reading list end: \", err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
  }
}

/**
 * Generates code to deserialize a map
 */
void t_go_generator::generate_deserialize_map_element(ostream& out,
                                                      t_map* tmap,
                                                      bool declare,
                                                      string prefix) {
  (void)declare;
  string key = tmp("_key");
  string val = tmp("_val");
  t_field fkey(tmap->get_key_type(), key);
  t_field fval(tmap->get_val_type(), val);
  fkey.set_req(t_field::T_OPT_IN_REQ_OUT);
  fval.set_req(t_field::T_OPT_IN_REQ_OUT);
  generate_deserialize_field(out, &fkey, true, "", false, false, true);
  generate_deserialize_field(out, &fval, true, "", false, false, false, true);
  indent(out) << prefix << "[" << key << "] = " << val << '\n';
}

/**
 * Write a set element
 */
void t_go_generator::generate_deserialize_set_element(ostream& out,
                                                      t_set* tset,
                                                      bool declare,
                                                      string prefix) {
  (void)declare;
  string elem = tmp("_elem");
  t_field felem(tset->get_elem_type(), elem);
  felem.set_req(t_field::T_OPT_IN_REQ_OUT);
  generate_deserialize_field(out, &felem, true, "", false, false, false, true);
  indent(out) << prefix << " = append(" << prefix << ", " << elem << ")" << '\n';
}

/**
 * Write a list element
 */
void t_go_generator::generate_deserialize_list_element(ostream& out,
                                                       t_list* tlist,
                                                       bool declare,
                                                       string prefix) {
  (void)declare;
  string elem = tmp("_elem");
  t_field felem(((t_list*)tlist)->get_elem_type(), elem);
  felem.set_req(t_field::T_OPT_IN_REQ_OUT);
  generate_deserialize_field(out, &felem, true, "", false, false, false, true);
  indent(out) << prefix << " = append(" << prefix << ", " << elem << ")" << '\n';
}

/**
 * Serializes a field of any type.
 *
 * @param tfield The field to serialize
 * @param prefix Name to prepend to field name
 */
void t_go_generator::generate_serialize_field(ostream& out,
                                              t_field* tfield,
                                              string prefix,
                                              bool inkey) {
  t_type* type = get_true_type(tfield->get_type());
  string name(prefix + publicize(tfield->get_name()));

  // Do nothing for void types
  if (type->is_void()) {
    throw "compiler error: cannot generate serialize for void type: " + name;
  }

  if (type->is_struct() || type->is_xception()) {
    generate_serialize_struct(out, (t_struct*)type, name);
  } else if (type->is_container()) {
    generate_serialize_container(out, type, is_pointer_field(tfield), name);
  } else if (type->is_base_type() || type->is_enum()) {
    indent(out) << "if err := oprot.";

    if (is_pointer_field(tfield)) {
      name = "*" + name;
    }

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();

      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " + name;
        break;

      case t_base_type::TYPE_STRING:
        if (type->is_binary() && !inkey) {
          out << "WriteBinary(ctx, " << name << ")";
        } else {
          out << "WriteString(ctx, string(" << name << "))";
        }

        break;

      case t_base_type::TYPE_BOOL:
        out << "WriteBool(ctx, bool(" << name << "))";
        break;

      case t_base_type::TYPE_I8:
        out << "WriteByte(ctx, int8(" << name << "))";
        break;

      case t_base_type::TYPE_I16:
        out << "WriteI16(ctx, int16(" << name << "))";
        break;

      case t_base_type::TYPE_I32:
        out << "WriteI32(ctx, int32(" << name << "))";
        break;

      case t_base_type::TYPE_I64:
        out << "WriteI64(ctx, int64(" << name << "))";
        break;

      case t_base_type::TYPE_DOUBLE:
        out << "WriteDouble(ctx, float64(" << name << "))";
        break;

      case t_base_type::TYPE_UUID:
        out << "WriteUUID(ctx, thrift.Tuuid(" << name << "))";
        break;

      default:
        throw "compiler error: no Go name for base type " + t_base_type::t_base_name(tbase);
      }
    } else if (type->is_enum()) {
      out << "WriteI32(ctx, int32(" << name << "))";
    }

    out << "; err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(fmt.Sprintf(\"%T."
        << escape_string(tfield->get_name()) << " (" << tfield->get_key()
        << ") field write error: \", p), err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
  } else {
    throw "compiler error: Invalid type in generate_serialize_field '" + type->get_name()
        + "' for field '" + name + "'";
  }
}

/**
 * Serializes all the members of a struct.
 *
 * @param tstruct The struct to serialize
 * @param prefix  String prefix to attach to all fields
 */
void t_go_generator::generate_serialize_struct(ostream& out, t_struct* tstruct, string prefix) {
  (void)tstruct;
  out << indent() << "if err := " << prefix << "." << write_method_name_ << "(ctx, oprot); err != nil {" << '\n';
  indent_up();
  out << indent() << "return thrift.PrependError(fmt.Sprintf(\"%T error writing struct: \", "
      << prefix << "), err)" << '\n';
  indent_down();
  out << indent() << "}" << '\n';
}

void t_go_generator::generate_serialize_container(ostream& out,
                                                  t_type* ttype,
                                                  bool pointer_field,
                                                  string prefix) {
  if (pointer_field) {
    prefix = "*" + prefix;
  }
  if (ttype->is_map()) {
    out << indent() << "if err := oprot.WriteMapBegin(ctx, "
        << type_to_enum(((t_map*)ttype)->get_key_type()) << ", "
        << type_to_enum(((t_map*)ttype)->get_val_type()) << ", "
        << "len(" << prefix << ")); err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(\"error writing map begin: \", err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
  } else if (ttype->is_set()) {
    out << indent() << "if err := oprot.WriteSetBegin(ctx, "
        << type_to_enum(((t_set*)ttype)->get_elem_type()) << ", "
        << "len(" << prefix << ")); err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(\"error writing set begin: \", err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
  } else if (ttype->is_list()) {
    out << indent() << "if err := oprot.WriteListBegin(ctx, "
        << type_to_enum(((t_list*)ttype)->get_elem_type()) << ", "
        << "len(" << prefix << ")); err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(\"error writing list begin: \", err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
  } else {
    throw "compiler error: Invalid type in generate_serialize_container '" + ttype->get_name()
        + "' for prefix '" + prefix + "'";
  }

  if (ttype->is_map()) {
    t_map* tmap = (t_map*)ttype;
    out << indent() << "for k, v := range " << prefix << " {" << '\n';
    indent_up();
    generate_serialize_map_element(out, tmap, "k", "v");
    indent_down();
    indent(out) << "}" << '\n';
  } else if (ttype->is_set()) {
    t_set* tset = (t_set*)ttype;
    out << indent() << "for i := 0; i<len(" << prefix << "); i++ {" << '\n';
    indent_up();
    out << indent() << "for j := i+1; j<len(" << prefix << "); j++ {" << '\n';
    indent_up();
    string wrapped_prefix = prefix;
    if (pointer_field) {
      wrapped_prefix = "(" + prefix + ")";
    }
    string goType = type_to_go_type(tset->get_elem_type());
    out << indent() << "if func(tgt, src " << goType << ") bool {" << '\n';
    indent_up();
    generate_go_equals(out, tset->get_elem_type(), "tgt", "src");
    out << indent() << "return true" << '\n';
    indent_down();
    out << indent() << "}(" << wrapped_prefix << "[i], " << wrapped_prefix << "[j]) {" << '\n';
    indent_up();
    out << indent()
        << "return thrift.PrependError(\"\", fmt.Errorf(\"%T error writing set field: slice is not "
           "unique\", "
        << wrapped_prefix << "))" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
    out << indent() << "for _, v := range " << prefix << " {" << '\n';
    indent_up();
    generate_serialize_set_element(out, tset, "v");
    indent_down();
    indent(out) << "}" << '\n';
  } else if (ttype->is_list()) {
    t_list* tlist = (t_list*)ttype;
    out << indent() << "for _, v := range " << prefix << " {" << '\n';

    indent_up();
    generate_serialize_list_element(out, tlist, "v");
    indent_down();
    indent(out) << "}" << '\n';
  }

  if (ttype->is_map()) {
    out << indent() << "if err := oprot.WriteMapEnd(ctx); err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(\"error writing map end: \", err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
  } else if (ttype->is_set()) {
    out << indent() << "if err := oprot.WriteSetEnd(ctx); err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(\"error writing set end: \", err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
  } else if (ttype->is_list()) {
    out << indent() << "if err := oprot.WriteListEnd(ctx); err != nil {" << '\n';
    indent_up();
    out << indent() << "return thrift.PrependError(\"error writing list end: \", err)" << '\n';
    indent_down();
    out << indent() << "}" << '\n';
  }
}

/**
 * Serializes the members of a map.
 *
 */
void t_go_generator::generate_serialize_map_element(ostream& out,
                                                    t_map* tmap,
                                                    string kiter,
                                                    string viter) {
  t_field kfield(tmap->get_key_type(), "");
  t_field vfield(tmap->get_val_type(), "");
  kfield.set_req(t_field::T_OPT_IN_REQ_OUT);
  vfield.set_req(t_field::T_OPT_IN_REQ_OUT);
  generate_serialize_field(out, &kfield, kiter, true);
  generate_serialize_field(out, &vfield, viter);
}

/**
 * Serializes the members of a set.
 */
void t_go_generator::generate_serialize_set_element(ostream& out, t_set* tset, string prefix) {
  t_field efield(tset->get_elem_type(), "");
  efield.set_req(t_field::T_OPT_IN_REQ_OUT);
  generate_serialize_field(out, &efield, prefix);
}

/**
 * Serializes the members of a list.
 */
void t_go_generator::generate_serialize_list_element(ostream& out, t_list* tlist, string prefix) {
  t_field efield(tlist->get_elem_type(), "");
  efield.set_req(t_field::T_OPT_IN_REQ_OUT);
  generate_serialize_field(out, &efield, prefix);
}

/**
 * Compares any type
 */
void t_go_generator::generate_go_equals(ostream& out, t_type* ori_type, string tgt, string src) {

  t_type* ttype = get_true_type(ori_type);
  // Do nothing for void types
  if (ttype->is_void()) {
    throw "compiler error: cannot generate equals for void type: " + tgt;
  }

  if (ttype->is_struct() || ttype->is_xception()) {
    generate_go_equals_struct(out, ttype, tgt, src);
  } else if (ttype->is_container()) {
    generate_go_equals_container(out, ttype, tgt, src);
  } else if (ttype->is_base_type() || ttype->is_enum()) {
    out << indent() << "if ";
    if (ttype->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)ttype)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot equals void: " + tgt;
        break;

      case t_base_type::TYPE_STRING:
        if (ttype->is_binary()) {
          out << "bytes.Compare(" << tgt << ", " << src << ") != 0";
        } else {
          out << tgt << " != " << src;
        }
        break;

      case t_base_type::TYPE_BOOL:
      case t_base_type::TYPE_I8:
      case t_base_type::TYPE_I16:
      case t_base_type::TYPE_I32:
      case t_base_type::TYPE_I64:
      case t_base_type::TYPE_DOUBLE:
      case t_base_type::TYPE_UUID:
        out << tgt << " != " << src;
        break;

      default:
        throw "compiler error: no Go name for base type " + t_base_type::t_base_name(tbase);
      }
    } else if (ttype->is_enum()) {
      out << tgt << " != " << src;
    }

    out << " { return false }" << '\n';
  } else {
    throw "compiler error: Invalid type in generate_go_equals '" + ttype->get_name() + "' for '"
        + tgt + "'";
  }
}

/**
 * Compares the members of a struct
 */
void t_go_generator::generate_go_equals_struct(ostream& out,
                                               t_type* ttype,
                                               string tgt,
                                               string src) {
  (void)ttype;
  out << indent() << "if !" << tgt << "." << equals_method_name_ << "(" << src
      << ") { return false }" << '\n';
}

/**
 * Compares any container type
 */
void t_go_generator::generate_go_equals_container(ostream& out,
                                                  t_type* ttype,
                                                  string tgt,
                                                  string src) {
  out << indent() << "if len(" << tgt << ") != len(" << src << ") { return false }" << '\n';
  if (ttype->is_map()) {
    t_map* tmap = (t_map*)ttype;
    out << indent() << "for k, _tgt := range " << tgt << " {" << '\n';
    indent_up();
    string element_source = tmp("_src");
    out << indent() << element_source << " := " << src << "[k]" << '\n';
    generate_go_equals(out, tmap->get_val_type(), "_tgt", element_source);
    indent_down();
    indent(out) << "}" << '\n';
  } else if (ttype->is_list() || ttype->is_set()) {
    t_type* elem;
    if (ttype->is_list()) {
      t_list* temp = (t_list*)ttype;
      elem = temp->get_elem_type();
    } else {
      t_set* temp = (t_set*)ttype;
      elem = temp->get_elem_type();
    }
    out << indent() << "for i, _tgt := range " << tgt << " {" << '\n';
    indent_up();
    string element_source = tmp("_src");
    out << indent() << element_source << " := " << src << "[i]" << '\n';
    generate_go_equals(out, elem, "_tgt", element_source);
    indent_down();
    indent(out) << "}" << '\n';
  } else {
    throw "INVALID TYPE IN generate_go_equals_container '" + ttype->get_name();
  }
}

/**
 * Generates the docstring for a given struct.
 */
void t_go_generator::generate_go_docstring(ostream& out, t_struct* tstruct) {
  generate_go_docstring(out, tstruct, tstruct, "Attributes");
}

/**
 * Generates the docstring for a given function.
 */
void t_go_generator::generate_go_docstring(ostream& out, t_function* tfunction) {
  generate_go_docstring(out, tfunction, tfunction->get_arglist(), "Parameters");
}

/**
 * Generates the docstring for a struct or function.
 */
void t_go_generator::generate_go_docstring(ostream& out,
                                           t_doc* tdoc,
                                           t_struct* tstruct,
                                           const char* subheader) {
  bool has_doc = false;
  stringstream ss;

  if (tdoc->has_doc()) {
    has_doc = true;
    ss << tdoc->get_doc();
  }

  const vector<t_field*>& fields = tstruct->get_members();

  if (fields.size() > 0) {
    if (has_doc) {
      ss << '\n';
    }

    has_doc = true;
    ss << subheader << ":\n";
    vector<t_field*>::const_iterator p_iter;

    for (p_iter = fields.begin(); p_iter != fields.end(); ++p_iter) {
      t_field* p = *p_iter;
      ss << " - " << publicize(p->get_name());

      if (p->has_doc()) {
        ss << ": " << p->get_doc();
      } else {
        ss << '\n';
      }
    }
    ss << '\n';
  }

  if (has_doc) {
    generate_docstring_comment(out, "", "// ", ss.str(), "");
  }
}

/**
 * Generates the docstring for a generic object.
 */
void t_go_generator::generate_go_docstring(ostream& out, t_doc* tdoc) {
  if (tdoc->has_doc()) {
    generate_docstring_comment(out, "", "//", tdoc->get_doc(), "");
  }
}

/**
 * Declares an argument, which may include initialization as necessary.
 *
 * @param tfield The field
 */
string t_go_generator::declare_argument(t_field* tfield) {
  std::ostringstream result;
  result << publicize(tfield->get_name()) << "=";

  if (tfield->get_value() != nullptr) {
    result << "thrift_spec[" << tfield->get_key() << "][4]";
  } else {
    result << "nil";
  }

  return result.str();
}

/**
 * Renders a struct field initial value.
 *
 * @param tfield The field, which must have `tfield->get_value() != nullptr`
 */
string t_go_generator::render_field_initial_value(t_field* tfield,
                                                  const string& name,
                                                  bool optional_field) {
  t_type* type = get_true_type(tfield->get_type());

  if (optional_field) {
    // The caller will make a second pass for optional fields,
    // assigning the result of render_const_value to "*field_name". It
    // is maddening that Go syntax does not allow for a type-agnostic
    // way to initialize a pointer to a const value, but so it goes.
    // The alternative would be to write type specific functions that
    // convert from const values to pointer types, but given the lack
    // of overloading it would be messy.
    return "new(" + type_to_go_type(tfield->get_type()) + ")";
  } else {
    return render_const_value(type, tfield->get_value(), name);
  }
}

/**
 * Renders a function signature of the form 'type name(args)'
 *
 * @param tfunction Function definition
 * @return String of rendered function definition
 */
string t_go_generator::function_signature(t_function* tfunction, string prefix) {
  // TODO(mcslee): Nitpicky, no ',' if argument_list is empty
  return publicize(prefix + tfunction->get_name()) + "(" + argument_list(tfunction->get_arglist())
         + ")";
}

/**
 * Renders an interface function signature of the form 'type name(args)'
 *
 * @param tfunction Function definition
 * @return String of rendered function definition
 */
string t_go_generator::function_signature_if(t_function* tfunction, string prefix, bool addError) {
  string signature = publicize(prefix + tfunction->get_name()) + "(";
  signature += "ctx context.Context";
  if (!tfunction->get_arglist()->get_members().empty()) {
    signature += ", " + argument_list(tfunction->get_arglist());
  }
  signature += ") (";

  t_type* ret = tfunction->get_returntype();
  t_struct* exceptions = tfunction->get_xceptions();
  string errs = argument_list(exceptions);

  if (!ret->is_void()) {
    signature += "_r " + type_to_go_type(ret);

    if (addError || errs.size() == 0) {
      signature += ", ";
    }
  }

  if (addError) {
    signature += "_err error";
  }

  signature += ")";
  return signature;
}

/**
 * Renders a field list
 */
string t_go_generator::argument_list(t_struct* tstruct) {
  string result = "";
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  bool first = true;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      result += ", ";
    }

    result += variable_name_to_go_name((*f_iter)->get_name()) + " "
              + type_to_go_type((*f_iter)->get_type());
  }

  return result;
}

string t_go_generator::type_name(t_type* ttype) {
  string module( module_name(ttype));
  if( ! module.empty()) {
    return module + "." + ttype->get_name();
  }

  return ttype->get_name();
}

string t_go_generator::module_name(t_type* ttype) {
  t_program* program = ttype->get_program();

  if (program != nullptr && program != program_) {
    if (program->get_namespace("go").empty() ||
        program_->get_namespace("go").empty() ||
        program->get_namespace("go") != program_->get_namespace("go")) {
      string module(get_real_go_module(program));
      return package_identifiers_[module];
    }
  }

  return "";
}

/**
 * Converts the parse type to a go tyoe
 */
string t_go_generator::type_to_enum(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();

    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "NO T_VOID CONSTRUCT";

    case t_base_type::TYPE_STRING:
      /* this is wrong, binary is still a string type internally
      if (type->is_binary()) {
          return "thrift.BINARY";
      }
      */
      return "thrift.STRING";

    case t_base_type::TYPE_BOOL:
      return "thrift.BOOL";

    case t_base_type::TYPE_I8:
      return "thrift.BYTE";

    case t_base_type::TYPE_I16:
      return "thrift.I16";

    case t_base_type::TYPE_I32:
      return "thrift.I32";

    case t_base_type::TYPE_I64:
      return "thrift.I64";

    case t_base_type::TYPE_DOUBLE:
      return "thrift.DOUBLE";

    case t_base_type::TYPE_UUID:
      return "thrift.UUID";

    default:
      break;
    }
  } else if (type->is_enum()) {
    return "thrift.I32";
  } else if (type->is_struct() || type->is_xception()) {
    return "thrift.STRUCT";
  } else if (type->is_map()) {
    return "thrift.MAP";
  } else if (type->is_set()) {
    return "thrift.SET";
  } else if (type->is_list()) {
    return "thrift.LIST";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}

/**
 * Converts the parse type to a go map type, will throw an exception if it will
 * not produce a valid go map type.
 */
string t_go_generator::type_to_go_key_type(t_type* type) {
  t_type* resolved_type = type;

  while (resolved_type->is_typedef()) {
    resolved_type = ((t_typedef*)resolved_type)->get_type()->get_true_type();
  }

  if (resolved_type->is_map() || resolved_type->is_list() || resolved_type->is_set()) {
    throw "Cannot produce a valid type for a Go map key: " + type_to_go_type(type) + " - aborting.";
  }

  if (resolved_type->is_binary())
    return "string";

  return type_to_go_type(type);
}

/**
 * Converts the parse type to a go type
 */
string t_go_generator::type_to_go_type(t_type* type) {
  return type_to_go_type_with_opt(type, false);
}

/**
 * Converts the parse type to a go type, taking into account whether the field
 * associated with the type is T_OPTIONAL.
 */
string t_go_generator::type_to_go_type_with_opt(t_type* type,
                                                bool optional_field) {
  string maybe_pointer(optional_field ? "*" : "");

  if (type->is_typedef() && ((t_typedef*)type)->is_forward_typedef()) {
    type = ((t_typedef*)type)->get_true_type();
  }

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();

    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "";

    case t_base_type::TYPE_STRING:
      if (type->is_binary()) {
        return maybe_pointer + "[]byte";
      }

      return maybe_pointer + "string";

    case t_base_type::TYPE_BOOL:
      return maybe_pointer + "bool";

    case t_base_type::TYPE_I8:
      return maybe_pointer + "int8";

    case t_base_type::TYPE_I16:
      return maybe_pointer + "int16";

    case t_base_type::TYPE_I32:
      return maybe_pointer + "int32";

    case t_base_type::TYPE_I64:
      return maybe_pointer + "int64";

    case t_base_type::TYPE_DOUBLE:
      return maybe_pointer + "float64";

    case t_base_type::TYPE_UUID:
      return maybe_pointer + "thrift.Tuuid";

    default:
      break;
    }
  } else if (type->is_enum()) {
    return maybe_pointer + publicize(type_name(type));
  } else if (type->is_struct() || type->is_xception()) {
    return "*" + publicize(type_name(type));
  } else if (type->is_map()) {
    t_map* t = (t_map*)type;
    string keyType = type_to_go_key_type(t->get_key_type());
    string valueType = type_to_go_type(t->get_val_type());
    return maybe_pointer + string("map[") + keyType + "]" + valueType;
  } else if (type->is_set()) {
    t_set* t = (t_set*)type;
    string elemType = type_to_go_type(t->get_elem_type());
    return maybe_pointer + string("[]") + elemType;
  } else if (type->is_list()) {
    t_list* t = (t_list*)type;
    string elemType = type_to_go_type(t->get_elem_type());
    return maybe_pointer + string("[]") + elemType;
  } else if (type->is_typedef()) {
    return maybe_pointer + publicize(type_name(type));
  }

  throw "INVALID TYPE IN type_to_go_type: " + type->get_name();
}

/** See the comment inside generate_go_struct_definition for what this is. */
string t_go_generator::type_to_spec_args(t_type* ttype) {
  while (ttype->is_typedef()) {
    ttype = ((t_typedef*)ttype)->get_type();
  }

  if (ttype->is_base_type() || ttype->is_enum()) {
    return "nil";
  } else if (ttype->is_struct() || ttype->is_xception()) {
    return "(" + type_name(ttype) + ", " + type_name(ttype) + ".thrift_spec)";
  } else if (ttype->is_map()) {
    return "(" + type_to_enum(((t_map*)ttype)->get_key_type()) + ","
           + type_to_spec_args(((t_map*)ttype)->get_key_type()) + ","
           + type_to_enum(((t_map*)ttype)->get_val_type()) + ","
           + type_to_spec_args(((t_map*)ttype)->get_val_type()) + ")";
  } else if (ttype->is_set()) {
    return "(" + type_to_enum(((t_set*)ttype)->get_elem_type()) + ","
           + type_to_spec_args(((t_set*)ttype)->get_elem_type()) + ")";
  } else if (ttype->is_list()) {
    return "(" + type_to_enum(((t_list*)ttype)->get_elem_type()) + ","
           + type_to_spec_args(((t_list*)ttype)->get_elem_type()) + ")";
  }

  throw "INVALID TYPE IN type_to_spec_args: " + ttype->get_name();
}

// parses a string of struct tags into key/value pairs and writes them to the given map
void t_go_generator::parse_go_tags(map<string,string>* tags, const string in) {
  string key;
  string value;

  size_t mode=0; // 0/1/2 for key/value/whitespace
  size_t index=0;
  for (auto it=in.begin(); it<in.end(); ++it, ++index) {
      // Normally we start in key mode because the IDL is expected to be in
      // (go.tag="key:\"value\"") format, but if there is leading whitespace
      // we need to start in whitespace mode.
      if (index==0 && mode==0 && in[index]==' ') {
        mode=2;
      }

      if (mode==2) {
          if (in[index]==' ') {
              continue;
          }
          mode=0;
      }

      if (mode==0) {
          if (in[index]==':') {
              mode=1;
              index++;
              it++;
              continue;
          }
          key+=in[index];
      } else if (mode==1) {
          if (in[index]=='"') {
              (*tags)[key]=value;
              key=value="";
              mode=2;
              continue;
          }
          value+=in[index];
      }
  }
}

void t_go_generator::generate_deprecation_comment(ostream& out, const map<string, vector<string>>& annotations) {
  auto iter = annotations.find("deprecated");
  if (annotations.end() != iter) {
    out << indent() << "// Deprecated: ";

    bool first = true;
    for (auto str_iter = iter->second.begin(); str_iter != iter->second.end(); ++str_iter) {
      if (*str_iter == "1") {
        // Empty annotations will generate "1", skip those.
        continue;
      }
      if (first) {
        first = false;
      } else {
        out << "; ";
      }
      out << *str_iter;
    }
    if (first) {
      // All deprecation annotations are empty, put a generic reason here
      out << "(no reason given)";
    }
    out << '\n';
  }
}

bool format_go_output(const string& file_path) {

  // formatting via gofmt deactivated due to THRIFT-3893
  // Please look at the ticket and make sure you fully understand all the implications
  // before submitting a patch that enables this feature again. Thank you.
  (void) file_path;
  return false;

  /*
  const string command = "gofmt -w " + file_path;

  if (system(command.c_str()) == 0) {
    return true;
  }

  fprintf(stderr, "WARNING - Running '%s' failed.\n", command.c_str());
  return false;
  */
 }

std::string t_go_generator::display_name() const {
  return "Go";
}


THRIFT_REGISTER_GENERATOR(go, "Go",
                          "    package_prefix=  Package prefix for generated files.\n" \
                          "    thrift_import=   Override thrift package import path (default:" + DEFAULT_THRIFT_IMPORT + ")\n" \
                          "    package=         Package name (default: inferred from thrift file name)\n" \
                          "    ignore_initialisms\n"
                          "                     Disable automatic spelling correction of initialisms (e.g. \"URL\")\n" \
                          "    read_write_private\n"
                          "                     Make read/write methods private, default is public Read/Write\n"
                          "    skip_remote\n"
                          "                     Skip the generating of -remote folders for the client binaries for services\n")
