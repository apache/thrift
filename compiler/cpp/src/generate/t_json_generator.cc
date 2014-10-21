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
 *
 * Contains some contributions under the Thrift Software License.
 * Please see doc/old-thrift-license.txt in the Thrift distribution for
 * details.
 */

#include <fstream>
#include <iostream>
#include <sstream>

#include <stdlib.h>
#include <sys/stat.h>
#include <sstream>
#include "t_generator.h"
#include "platform.h"

using std::map;
using std::ofstream;
using std::ostream;
using std::ostringstream;
using std::string;
using std::stringstream;
using std::vector;
using std::stack;

static const string endl = "\n";
static const string quot = "\"";

class t_json_generator : public t_generator {
public:
  t_json_generator(
    t_program* program,
    const std::map<std::string, std::string>& parsed_options,
    const std::string& option_string)
    : t_generator(program)
  {
    (void) parsed_options;
    (void) option_string;
    out_dir_base_ = "gen-json";
  }

  virtual ~t_json_generator() {}

  /**
  * Init and close methods
  */

  void init_generator();
  void close_generator();

  void generate_typedef(t_typedef* ttypedef);
  void generate_enum(t_enum* tenum);
  void generate_program();
  void generate_consts(vector<t_const*>);
  void generate_function(t_function * tfunc);
  void generate_field(t_field * field);

  void generate_service(t_service* tservice);
  void generate_struct(t_struct* tstruct);

private:
  std::ofstream f_json_;
  std::stack<bool> _commaNeeded;
  string get_type_name(t_type* type);
  string get_const_value(t_const_value* val);

  void start_object();
  void start_array();
  void write_key(string key, string val);
  void write_key_int(string key, int val);
  void end_object(bool newLine);
  void end_array(bool newLine);
  void write_comma_if_needed();
  void indicate_comma_needed();
  string escapeJsonString(const string& input);

  void merge_includes(t_program*);
};

void t_json_generator::init_generator() {
  MKDIR(get_out_dir().c_str());

  string f_json_name = get_out_dir() + program_->get_name() + ".json";
  f_json_.open(f_json_name.c_str());

  //Merge all included programs into this one so we can output one big file.
  merge_includes(program_);
}

string t_json_generator::escapeJsonString(const string& input) {
  std::ostringstream ss;
  for (std::string::const_iterator iter = input.begin(); iter != input.end(); iter++) {
    switch (*iter) {
    case '\\': ss << "\\\\"; break;
    case '"': ss << "\\\""; break;
    case '/': ss << "\\/"; break;
    case '\b': ss << "\\b"; break;
    case '\f': ss << "\\f"; break;
    case '\n': ss << "\\n"; break;
    case '\r': ss << "\\r"; break;
    case '\t': ss << "\\t"; break;
    default: ss << *iter; break;
    }
  }
  return ss.str();
}

void t_json_generator::start_object(){
  f_json_ << "{";
  _commaNeeded.push(false);
}

void t_json_generator::start_array(){
  f_json_ << "[";
  _commaNeeded.push(false);
}

void t_json_generator::write_comma_if_needed(){
  if (_commaNeeded.top()) f_json_ << ",";
}

void t_json_generator::indicate_comma_needed(){
  _commaNeeded.pop();
  _commaNeeded.push(true);
}

void t_json_generator::write_key(string key, string val){
  write_comma_if_needed();
  f_json_ << quot << key << quot << ":" << quot << escapeJsonString(val) << quot;
  indicate_comma_needed();
}

void t_json_generator::write_key_int(string key, int val){
  write_comma_if_needed();
  f_json_ << quot << key << quot << ":" << quot << val << quot;
  indicate_comma_needed();
}

void t_json_generator::end_object(bool newLine){
  f_json_ << "}";
  if (newLine) f_json_ << endl;
  _commaNeeded.pop();
}

void t_json_generator::end_array(bool newLine){
  f_json_ << "]";
  if (newLine) f_json_ << endl;
  _commaNeeded.pop();
}

void t_json_generator::close_generator() {
  f_json_.close();
}

void t_json_generator::merge_includes(t_program * program) {
  vector<t_program*> includes = program->get_includes();
  vector<t_program*>::iterator inc_iter;
  for (inc_iter = includes.begin(); inc_iter != includes.end(); ++inc_iter){
    t_program* include = *inc_iter;
    //recurse in case we get crazy
    merge_includes(include);
    //merge enums
    vector<t_enum*> enums = include->get_enums();
    vector<t_enum*>::iterator en_iter;
    for (en_iter = enums.begin(); en_iter != enums.end(); ++en_iter) {
      program->add_enum(*en_iter);
    }
    //merge typedefs
    vector<t_typedef*> typedefs = include->get_typedefs();
    vector<t_typedef*>::iterator td_iter;
    for (td_iter = typedefs.begin(); td_iter != typedefs.end(); ++td_iter) {
      program->add_typedef(*td_iter);
    }
    //merge structs
    vector<t_struct*> objects = include->get_objects();
    vector<t_struct*>::iterator o_iter;
    for (o_iter = objects.begin(); o_iter != objects.end(); ++o_iter) {
      program->add_struct(*o_iter);
    }
    //merge constants
    vector<t_const*> consts = include->get_consts();
    vector<t_const*>::iterator c_iter;
    for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter){
      program->add_const(*c_iter);
    }

    // Generate services
    vector<t_service*> services = include->get_services();
    vector<t_service*>::iterator sv_iter;
    for (sv_iter = services.begin(); sv_iter != services.end(); ++sv_iter) {
      program->add_service(*sv_iter);
    }
  }
}

void t_json_generator::generate_program() {
  // Initialize the generator
  init_generator();
  start_object();

  write_key("name", program_->get_name());
  if (program_->has_doc()) write_key("doc", program_->get_doc());

  // Generate enums
  vector<t_enum*> enums = program_->get_enums();
  vector<t_enum*>::iterator en_iter;
  f_json_ << ",\"enums\":";
  start_array();
  f_json_ << endl;
  for (en_iter = enums.begin(); en_iter != enums.end(); ++en_iter) {
    write_comma_if_needed();
    generate_enum(*en_iter);
    indicate_comma_needed();
  }
  end_array(true);

  // Generate typedefs
  vector<t_typedef*> typedefs = program_->get_typedefs();
  vector<t_typedef*>::iterator td_iter;
  f_json_ << ",\"typedefs\":";
  start_array();
  f_json_ << endl;
  for (td_iter = typedefs.begin(); td_iter != typedefs.end(); ++td_iter) {
    write_comma_if_needed();
    generate_typedef(*td_iter);
    indicate_comma_needed();
  }
  end_array(true);

  // Generate structs, exceptions, and unions in declared order
  vector<t_struct*> objects = program_->get_objects();
  vector<t_struct*>::iterator o_iter;
  f_json_ << ",\"structs\":";
  start_array();
  f_json_ << endl;
  for (o_iter = objects.begin(); o_iter != objects.end(); ++o_iter) {
    write_comma_if_needed();
    if ((*o_iter)->is_xception()) {
      generate_xception(*o_iter);
    }
    else {
      generate_struct(*o_iter);
    }
    indicate_comma_needed();
  }
  end_array(true);

  // Generate constants
  vector<t_const*> consts = program_->get_consts();
  generate_consts(consts);

  // Generate services
  vector<t_service*> services = program_->get_services();
  vector<t_service*>::iterator sv_iter;
  f_json_ << ",\"services\":";
  start_array();
  f_json_ << endl;
  for (sv_iter = services.begin(); sv_iter != services.end(); ++sv_iter) {
    write_comma_if_needed();
    service_name_ = get_service_name(*sv_iter);
    generate_service(*sv_iter);
    indicate_comma_needed();
  }
  end_array(false);
  end_object(true);
  // Close the generator
  close_generator();
}

void t_json_generator::generate_typedef(t_typedef* ttypedef){
  start_object();
  write_key("name", ttypedef->get_name());
  write_key("type", get_type_name(ttypedef->get_true_type()));
  if (ttypedef->has_doc()) write_key("doc", ttypedef->get_doc());
  end_object(true);
}

void t_json_generator::generate_consts(vector<t_const*> consts){
  vector<t_const*>::iterator c_iter;
  f_json_ << ",\"constants\":";
  start_array();
  f_json_ << endl;
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    write_comma_if_needed();
    indicate_comma_needed();
    start_object();
    t_const* con = (*c_iter);
    write_key("name", con->get_name());
    write_key("type", get_type_name(con->get_type()));
    if (con->has_doc()) write_key("doc", con->get_doc());
    write_key("value", get_const_value(con->get_value()));
    end_object(true);
  }
  end_array(true);
}

void t_json_generator::generate_enum(t_enum* tenum) {
  start_object();
  write_key("name", tenum->get_name());
  if (tenum->has_doc()) write_key("doc", tenum->get_doc());
  f_json_ << ",\"members\":";
  start_array();
  vector<t_enum_value*> values = tenum->get_constants();
  vector<t_enum_value*>::iterator val_iter;
  for (val_iter = values.begin(); val_iter != values.end(); ++val_iter) {
    t_enum_value* val = (*val_iter);
    write_comma_if_needed();
    start_object();
    write_key("name", val->get_name());
    write_key_int("value", val->get_value());
    if (val->has_doc()) write_key("doc", val->get_doc());
    end_object(false);
    indicate_comma_needed();
  }
  end_array(false);
  end_object(true);
}

void t_json_generator::generate_struct(t_struct* tstruct){
  start_object();
  write_key("name", tstruct->get_name());
  if (tstruct->has_doc()) write_key("doc", tstruct->get_doc());
  if (tstruct->is_xception()) write_key("isException", "true");
  vector<t_field*> members = tstruct->get_members();
  vector<t_field*>::iterator mem_iter = members.begin();
  f_json_ << ",\"fields\":";
  start_array();
  for (; mem_iter != members.end(); mem_iter++) {
    generate_field((*mem_iter));
  }
  end_array(false);
  end_object(true);
}

void t_json_generator::generate_service(t_service* tservice){
  start_object();
  write_key("name", tservice->get_name());
  if (tservice->get_extends()) write_key("extendsType", tservice->get_extends()->get_name());
  if (tservice->has_doc()) write_key("doc", tservice->get_doc());
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator fn_iter = functions.begin();
  f_json_ << ",\"functions\":";
  start_array();
  for (; fn_iter != functions.end(); fn_iter++) {
    t_function* func = (*fn_iter);
    write_comma_if_needed();
    indicate_comma_needed();
    generate_function(func);
  }
  end_array(false);
  end_object(true);
}

void t_json_generator::generate_function(t_function* tfunc){
  start_object();
  write_key("name", tfunc->get_name());
  write_key("returnType", get_type_name(tfunc->get_returntype()));
  if (tfunc->is_oneway()) write_key("oneWay", "true");
  if (tfunc->has_doc()) write_key("doc", tfunc->get_doc());
  vector<t_field*> members = tfunc->get_arglist()->get_members();
  vector<t_field*>::iterator mem_iter = members.begin();
  f_json_ << ",\"arguments\":";
  start_array();
  for (; mem_iter != members.end(); mem_iter++) {
    generate_field((*mem_iter));
  }
  end_array(false);

  vector<t_field*> excepts = tfunc->get_xceptions()->get_members();
  vector<t_field*>::iterator ex_iter = excepts.begin();
  f_json_ << ",\"exceptions\":";
  start_array();
  for (; ex_iter != excepts.end(); ex_iter++) {
    generate_field((*ex_iter));
  }
  end_array(false);
  end_object(false);
}

void t_json_generator::generate_field(t_field * field){
  write_comma_if_needed();
  start_object();
  write_key_int("index", field->get_key());
  write_key("name", field->get_name());
  write_key("type", get_type_name(field->get_type()));
  if (field->has_doc()) write_key("doc", field->get_doc());
  switch (field->get_req()) {
        case t_field::T_REQUIRED:
            write_key("required", "true");
            break;
        default:
            write_key("required", "false");
            break;
  }
  if (field->get_value())
    write_key("default", get_const_value(field->get_value()));

  end_object(false);
  indicate_comma_needed();
}
string t_json_generator::get_const_value(t_const_value* tvalue){

  switch (tvalue->get_type()) {
  case t_const_value::CV_INTEGER:
    return tvalue->get_string();
  case t_const_value::CV_DOUBLE:
    return tvalue->get_string();
  case t_const_value::CV_STRING:
    return tvalue->get_string();
  case t_const_value::CV_LIST:
  {
    string list = "[";
    vector<t_const_value*> list_elems = tvalue->get_list();;
    vector<t_const_value*>::iterator list_iter;
    bool first = true;
    for (list_iter = list_elems.begin(); list_iter != list_elems.end(); list_iter++) {
      if (!first)list += ",";
      first = false;
      list += get_const_value(*list_iter);
    }
    return list + "]";
  }
  case t_const_value::CV_IDENTIFIER:
    return tvalue->get_identifier_name();
  case t_const_value::CV_MAP:
    map<t_const_value*, t_const_value*> map_elems = tvalue->get_map();
    map<t_const_value*, t_const_value*>::iterator map_iter;
    string map = "[";
    bool first = true;
    for (map_iter = map_elems.begin(); map_iter != map_elems.end(); map_iter++) {
      if (!first) map += ",";
      first = false;
      map += get_const_value(map_iter->first) + ":";
      map += get_const_value(map_iter->second);
    }
    return map + "]";
  }
  return "UNKNOWN";
}
string t_json_generator::get_type_name(t_type* ttype){
  if (ttype->is_container()) {
    if (ttype->is_list()) {
      return "list<" + get_type_name(((t_list*)ttype)->get_elem_type()) + ">";
    }
    else if (ttype->is_set()) {
      return "set<" + get_type_name(((t_set*)ttype)->get_elem_type()) + ">";
    }
    else if (ttype->is_map()) {
      return "map<" + get_type_name(((t_map*)ttype)->get_key_type()) +
        +"," + get_type_name(((t_map*)ttype)->get_val_type()) + ">";
    }
  }
  else if (ttype->is_base_type()) {
    return (((t_base_type*)ttype)->is_binary() ? "binary" : ttype->get_name());
  }
  return ttype->get_name();
}

THRIFT_REGISTER_GENERATOR(json, "JSON", "")
