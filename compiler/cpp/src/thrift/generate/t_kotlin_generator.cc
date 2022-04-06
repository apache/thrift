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

#include <cassert>
#include <ctime>

#include <cctype>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <sstream>
#include <string>
#include <vector>

#include <stdexcept>
#include <sys/stat.h>

#include "thrift/generate/t_oop_generator.h"
#include "thrift/platform.h"

using std::map;
using std::ostream;
using std::ostringstream;
using std::set;
using std::setfill;
using std::setw;
using std::string;
using std::stringstream;
using std::vector;

static const string endl = "\n"; // avoid ostream << std::endl flushes

static const string KOTLIN_RESERVED_WORDS[] = {
    "as",      "as?",      "break",    "class",   "continue",  "do",          "else",
    "false",   "for",      "fun",      "if",      "in",        "!in",         "interface",
    "is",      "!is",      "null",     "object",  "package",   "return",      "super",
    "this",    "throw",    "true",     "try",     "typealias", "typeof",      "val",
    "var",     "when",     "while",    "by",      "catch",     "constructor", "delegate",
    "dynamic", "field",    "file",     "finally", "get",       "import",      "init",
    "param",   "property", "receiver", "set",     "setparam",  "value",       "where",
};

const set<string> KOTLIN_RESERVED_WORDS_SET(KOTLIN_RESERVED_WORDS,
                                            KOTLIN_RESERVED_WORDS
                                                + sizeof(KOTLIN_RESERVED_WORDS)
                                                      / sizeof(KOTLIN_RESERVED_WORDS[0]));

/**
 * Kotlin code generator.
 */
class t_kotlin_generator : public t_oop_generator {
public:
  t_kotlin_generator(t_program* program,
                     const std::map<std::string, std::string>& /*parsed_options*/,
                     const std::string& /*option_string*/)
    : t_oop_generator(program) {}

  /**
   * Init and close methods
   */
  void init_generator() override;
  void close_generator() override;

  void generate_consts(std::vector<t_const*> consts) override;

  /**
   * Program-level generation functions
   */

  void generate_typedef(t_typedef* ttypedef) override;
  void generate_enum(t_enum* tenum) override;
  void generate_struct(t_struct* tstruct) override;
  // void generate_union(t_struct* tunion);
  void generate_xception(t_struct* txception) override;
  void generate_service(t_service* tservice) override;

private:
  std::string package_name_;
  std::string package_dir_;
  ofstream_with_content_based_conditional_update f_extensions_;
  ofstream_with_content_based_conditional_update f_types_;

  std::string kotlin_package();
  std::string warning_surpressions();
  std::string constant_name(std::string name);
  std::string type_to_enum(t_type* type);
  std::string inner_enum_type_name(t_type* ttype);
  bool is_enum_set(t_type* ttype);
  bool is_enum_map(t_type* ttype);
  std::string type_name(t_type* ttype,
                        bool in_init = false,
                        bool skip_generic = false,
                        bool force_namespace = false);
  std::string base_type_name(t_base_type* tbase);
  std::string function_signature(t_function* tfunction, std::string prefix = "");

  std::string base_type_write_expression(t_base_type* tbase, std::string it = "it");
  std::string base_type_read_expression(t_base_type* tbase);

  bool is_reserved(const string& name);

  string kotlin_safe_name(const string& name);

  void generate_kdoc_comment(std::ostream& out, t_doc* tdoc);

  void generate_kotlin_struct(t_struct* tstruct, bool is_exception);

  void generate_kotlin_ext_definition(std::ostream& out, std::string type_name);
  void generate_kotlin_ext_field_definition(std::ostream& out);
  void generate_kotlin_ext_map_definition(std::ostream& out);
  void generate_kotlin_ext_container_definition(std::ostream& out, std::string type_name);

  void generate_service_interface(t_service* tservice);
  void generate_service_client(t_service* tservice);
  void generate_client_call(std::ostream& out, t_service* tservice, t_function* tfunc);
  void generate_service_processor(t_service* tservice);
  void generate_service_process_function(ostream& out, t_service* tservice, t_function* tfunc);

  void generate_service_args_helpers(t_service* tservice);
  void generate_service_result_helpers(t_service* tservice);

  void generate_union_definition(std::ostream& out,
                                 t_struct* tunion,
                                 std::string additional_interface = "");
  void generate_union_standard_scheme(std::ostream& out, t_struct* tunion);
  void generate_union_tuple_scheme(std::ostream& out, t_struct* tunion);
  void generate_union_standard_scheme_read(std::ostream& out, t_struct* tunion);
  void generate_union_standard_scheme_write(std::ostream& out, t_struct* tunion);
  void generate_union_methods_definitions(std::ostream& out, t_struct* tunion);
  void generate_union_method_check_type(std::ostream& out, t_struct* tunion);

  void generate_struct_definition(std::ostream& out,
                                  t_struct* tstruct,
                                  bool is_xception = false,
                                  std::string additional_interface = "");
  void generate_struct_field_name_constants(std::ostream& out, t_struct* tstruct);
  void generate_struct_companion_object(std::ostream& out, t_struct* tstruct);
  void generate_struct_standard_scheme(std::ostream& out, t_struct* tstruct);
  void generate_struct_standard_scheme_read(std::ostream& out, t_struct* tstruct);
  void generate_struct_standard_scheme_write(std::ostream& out, t_struct* tstruct);
  void generate_struct_method_deep_copy(std::ostream& out, t_struct* tstruct);
  void generate_struct_method_compare_to(std::ostream& out, t_struct* tstruct);
  void generate_struct_method_field_for_id(std::ostream& out, t_struct* tstruct);
  void generate_struct_method_set_field_value(std::ostream& out, t_struct* tstruct);
  void generate_struct_method_get_field_value(std::ostream& out, t_struct* tstruct);
  void generate_struct_method_is_set(std::ostream& out, t_struct* tstruct);
  void generate_struct_method_clear(std::ostream& out, t_struct* tstruct);
  void generate_struct_method_validate(std::ostream& out, t_struct* tstruct);
  void generate_struct_method_read(std::ostream& out, t_struct* tstruct);
  void generate_struct_method_write(std::ostream& out, t_struct* tstruct);

  void generate_serialize_value(ostream& out, t_type* ttype, std::string it = "it");
  void generate_serialize_field(ostream& out, t_field* tfield);
  void generate_serialize_container(ostream& out, t_type* ttype, std::string it = "it");

  void generate_deserialize_value(ostream& out, t_type* ttype);
  void generate_deserialize_field(ostream& out, t_field* tfield, string prefix);
  void generate_deserialize_container(ostream& out, t_type* ttype);

  void generate_kotlin_union(t_struct* tstruct);
};

/**
 * Prepares for file generation by opening up the necessary file output
 * streams.
 *
 * @param tprogram The program to generate
 */
void t_kotlin_generator::init_generator() {
  // Make output directory
  MKDIR(get_out_dir().c_str());
  package_name_ = program_->get_namespace("java");
  string dir = package_name_;
  string subdir = get_out_dir();
  string::size_type loc;
  while ((loc = dir.find(".")) != string::npos) {
    subdir = subdir + "/" + dir.substr(0, loc);
    MKDIR(subdir.c_str());
    dir = dir.substr(loc + 1);
  }
  if (dir.size() > 0) {
    subdir = subdir + "/" + dir;
    MKDIR(subdir.c_str());
  }

  package_dir_ = subdir;

  string f_types_name = package_dir_ + "/" + program_->get_name() + "Constants.kt";
  f_types_.open(f_types_name);
  f_types_ << autogen_comment() << kotlin_package();

  string f_extensions_name = package_dir_ + "/TProtocolExt.kt";
  f_extensions_.open(f_extensions_name);
  f_extensions_ << autogen_comment() << kotlin_package();

  generate_kotlin_ext_definition(f_extensions_, "Message");
  generate_kotlin_ext_definition(f_extensions_, "Struct");
  generate_kotlin_ext_field_definition(f_extensions_);
  generate_kotlin_ext_map_definition(f_extensions_);
  generate_kotlin_ext_container_definition(f_extensions_, "Set");
  generate_kotlin_ext_container_definition(f_extensions_, "List");
}

/**
 * Nothing in Kotlin generator
 */
void t_kotlin_generator::close_generator() {
  f_types_ << endl;
  f_types_.close();
  f_extensions_ << endl;
  f_extensions_.close();
}

void t_kotlin_generator::generate_kotlin_ext_definition(std::ostream& out, std::string type_name) {
  out << "internal inline fun org.apache.thrift.protocol.TProtocol.write" << type_name
      << "(marker: "
         "org.apache.thrift.protocol.T"
      << type_name << ", action: () -> Unit) {" << endl;
  indent_up();
  indent(out) << "write" << type_name << "Begin(marker)" << endl;
  indent(out) << "try { action() }" << endl;
  indent(out) << "finally { write" << type_name << "End() }" << endl;
  scope_down(out);
  out << endl;

  out << "internal inline fun <R> org.apache.thrift.protocol.TProtocol.read" << type_name
      << "(action: org.apache.thrift.protocol.T" << type_name << ".() -> R): R {" << endl;
  indent_up();
  indent(out) << "val marker = read" << type_name << "Begin()" << endl;
  indent(out) << "try { return action(marker) }" << endl;
  indent(out) << "finally { read" << type_name << "End() }" << endl;
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_kotlin_ext_field_definition(std::ostream& out) {
  out << "internal inline fun org.apache.thrift.protocol.TProtocol.writeField(marker: "
         "org.apache.thrift.protocol.TField, action: () -> Unit) {"
      << endl;
  indent_up();
  indent(out) << "writeFieldBegin(marker)" << endl;
  indent(out) << "try { action() }" << endl;
  indent(out) << "finally { writeFieldEnd() }" << endl;
  scope_down(out);
  out << endl;

  out << "internal inline fun org.apache.thrift.protocol.TProtocol.readField(action: "
         "org.apache.thrift.protocol.TField.() -> kotlin.Unit): kotlin.Boolean {"
      << endl;
  indent_up();
  indent(out) << "val marker = readFieldBegin()" << endl;
  indent(out) << "if (marker.type == org.apache.thrift.protocol.TType.STOP) { return true }"
              << endl;
  indent(out) << "try {" << endl;
  indent_up();
  indent(out) << "action(marker)" << endl;
  indent(out) << "return false" << endl;
  indent_down();
  indent(out) << "} finally { readFieldEnd() }" << endl;
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_kotlin_ext_map_definition(std::ostream& out) {
  out << "internal inline fun <K, V> org.apache.thrift.protocol.TProtocol.writeMap(keyType: "
         "kotlin.Byte, valueType: kotlin.Byte, map: Map<K, V>, action: (Map.Entry<K, V>) -> "
         "Unit) {"
      << endl;
  indent_up();
  indent(out) << "writeMapBegin(org.apache.thrift.protocol.TMap(keyType, valueType, map.size))"
              << endl;
  indent(out) << "map.forEach { action(it) }" << endl;
  indent(out) << "writeMapEnd()" << endl;
  scope_down(out);
  out << endl;
  out << "internal inline fun <R> org.apache.thrift.protocol.TProtocol.readMap(action: "
         "org.apache.thrift.protocol.TMap.() -> R): R {"
      << endl;
  indent_up();
  indent(out) << "val marker = readMapBegin()" << endl;
  indent(out) << "val r = action(marker)" << endl;
  indent(out) << "readMapEnd()" << endl;
  indent(out) << "return r" << endl;
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_kotlin_ext_container_definition(std::ostream& out,
                                                                  std::string type_name) {
  out << "internal inline fun <T> org.apache.thrift.protocol.TProtocol.write" << type_name
      << "(elemType: kotlin.Byte, container: " << type_name << "<T>, action: (T) -> Unit) {"
      << endl;
  indent_up();
  indent(out) << "write" << type_name << "Begin(org.apache.thrift.protocol.T" << type_name
              << "(elemType, container.size))" << endl;
  indent(out) << "container.forEach { action(it) }" << endl;
  indent(out) << "write" << type_name << "End()" << endl;
  scope_down(out);
  out << endl;
  out << "internal inline fun <R> org.apache.thrift.protocol.TProtocol.read" << type_name
      << "(action: org.apache.thrift.protocol.T" << type_name << ".() -> R): R {" << endl;
  indent_up();
  indent(out) << "val marker = read" << type_name << "Begin()" << endl;
  indent(out) << "val r = action(marker)" << endl;
  indent(out) << "read" << type_name << "End()" << endl;
  indent(out) << "return r" << endl;
  scope_down(out);
  out << endl;
}

/**
 * Generates a typedef. This is not done in Java, since it does
 * not support arbitrary name replacements, and it'd be a wacky waste
 * of overhead to make wrapper classes.
 *
 * @param ttypedef The type definition
 */
void t_kotlin_generator::generate_typedef(t_typedef* ttypedef) {
  f_types_ << "typealias " << ttypedef->get_symbolic() << " = "
           << type_name(ttypedef->get_type(), true) << endl;
}

void t_kotlin_generator::generate_enum(t_enum* tenum) {
  // Make output file
  string f_enum_name = package_dir_ + "/" + (tenum->get_name()) + ".kt";
  ofstream_with_content_based_conditional_update f_enum;
  f_enum.open(f_enum_name.c_str());

  f_enum << autogen_comment() << kotlin_package();

  indent(f_enum) << "enum class " << kotlin_safe_name(tenum->get_name())
                 << "(private val value: kotlin.Int) : org.apache.thrift.TEnum {";
  indent_up();
  indent(f_enum);

  auto first = true;
  auto enum_values = tenum->get_constants();
  for (auto& enum_value : enum_values) {
    f_enum << (first ? "" : ",") << endl;
    first = false;
    indent(f_enum) << enum_value->get_name() << "(" << enum_value->get_value() << ")";
  }
  if (first) {
    indent(f_enum);
  }
  f_enum << ";" << endl << endl;
  indent(f_enum) << "override fun getValue() = value" << endl << endl;
  {
    indent(f_enum) << "companion object {" << endl;
    indent_up();
    {
      indent(f_enum) << "@kotlin.jvm.JvmStatic" << endl;
      indent(f_enum) << "fun findByValue(i: kotlin.Int): " << kotlin_safe_name(tenum->get_name())
                     << "? {" << endl;
      indent_up();
      {
        indent(f_enum) << "return when (i) {" << endl;
        indent_up();
        {
          auto enum_values = tenum->get_constants();
          for (auto& enum_value : enum_values) {
            indent(f_enum) << enum_value->get_value() << " -> " << enum_value->get_name() << endl;
          }
          indent(f_enum) << "else -> null" << endl;
        }
        scope_down(f_enum);
      }
      scope_down(f_enum);
    }
    scope_down(f_enum);
  }
  scope_down(f_enum);
  f_enum.close();
}

void t_kotlin_generator::generate_consts(std::vector<t_const*> consts) {
  for (auto const_value : consts) {
    auto const_type = const_value->get_type();
    if (const_type->is_base_type()) {
      f_types_ << "const ";
    }
    f_types_ << "val " << kotlin_safe_name(const_value->get_name()) << ": " << type_name(const_type)
             << " = ";

    auto value = const_value->get_value();
    if (const_type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)const_type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_STRING:
        f_types_ << "\"" << value->get_string() << "\"";
        break;
      case t_base_type::TYPE_BOOL:
        f_types_ << ((value->get_integer() > 0) ? "true" : "false");
        break;
      case t_base_type::TYPE_I8:
      case t_base_type::TYPE_I16:
      case t_base_type::TYPE_I32:
        f_types_ << value->get_integer();
        break;
      case t_base_type::TYPE_DOUBLE:
        if (value->get_type() == t_const_value::CV_INTEGER) {
          f_types_ << value->get_integer() << ".";
        } else {
          f_types_ << emit_double_as_string(value->get_double());
        }
        break;
      default:
        f_types_ << value->get_integer();
        break;
      }
    } else if (const_type->is_enum()) {
      auto namespace_prefix = const_type->get_program()->get_namespace("java");
      if (namespace_prefix.length() > 0) {
        namespace_prefix += ".";
      }
      f_types_ << namespace_prefix + value->get_identifier_with_parent();
    } else {
      // TODO
    }
    f_types_ << endl;
  }
}

string t_kotlin_generator::base_type_name(t_base_type* type) {
  t_base_type::t_base tbase = type->get_base();

  switch (tbase) {
  case t_base_type::TYPE_VOID:
    return "kotlin.Unit";
  case t_base_type::TYPE_STRING:
    if (type->is_binary()) {
      return "kotlin.ByteArray";
    } else {
      return "kotlin.String";
    }
  case t_base_type::TYPE_BOOL:
    return "kotlin.Boolean";
  case t_base_type::TYPE_I8:
    return "kotlin.Byte";
  case t_base_type::TYPE_I16:
    return "kotlin.Short";
  case t_base_type::TYPE_I32:
    return "kotlin.Int";
  case t_base_type::TYPE_I64:
    return "kotlin.Long";
  case t_base_type::TYPE_DOUBLE:
    return "kotlin.Double";
  default:
    throw "compiler error: no Kotlin name for base type " + t_base_type::t_base_name(tbase);
  }
}

string t_kotlin_generator::type_name(t_type* ttype,
                                     bool in_init,
                                     bool skip_generic,
                                     bool force_namespace) {
  ttype = get_true_type(ttype);
  string prefix;
  if (ttype->is_base_type()) {
    return base_type_name((t_base_type*)ttype);
  } else if (ttype->is_map()) {
    t_map* tmap = (t_map*)ttype;
    if (in_init) {
      prefix = "kotlin.collections.Map";
    } else {
      prefix = "kotlin.collections.Map";
    }
    return prefix
           + (skip_generic ? ""
                           : "<" + type_name(tmap->get_key_type(), true) + ", "
                                 + type_name(tmap->get_val_type(), true) + ">");
  } else if (ttype->is_set()) {
    t_set* tset = (t_set*)ttype;
    if (in_init) {
      prefix = "kotlin.collections.Set";
    } else {
      prefix = "kotlin.collections.Set";
    }
    return prefix + (skip_generic ? "" : "<" + type_name(tset->get_elem_type(), true) + ">");
  } else if (ttype->is_list()) {
    t_list* tlist = (t_list*)ttype;
    if (in_init) {
      prefix = "kotlin.collections.List";
    } else {
      prefix = "kotlin.collections.List";
    }
    return prefix + (skip_generic ? "" : "<" + type_name(tlist->get_elem_type(), true) + ">");
  }

  // Check for namespacing
  t_program* program = ttype->get_program();
  if ((program != nullptr) && ((program != program_) || force_namespace)) {
    string package = program->get_namespace("java");
    if (!package.empty()) {
      return package + "." + kotlin_safe_name(ttype->get_name());
    }
  }

  return kotlin_safe_name(ttype->get_name());
}

/**
 * Generates a struct definition for a thrift data type. This will be a org.apache.thrift.TBase
 * implementor.
 *
 * @param tstruct The struct definition
 */
void t_kotlin_generator::generate_struct(t_struct* tstruct) {
  if (tstruct->is_union()) {
    generate_kotlin_union(tstruct);
  } else {
    generate_kotlin_struct(tstruct, false);
  }
}

void t_kotlin_generator::generate_kotlin_union(t_struct* tunion) {
  string f_union_name = package_dir_ + "/" + (tunion->get_name()) + ".kt";
  ofstream_with_content_based_conditional_update f_union;
  f_union.open(f_union_name.c_str());
  f_union << autogen_comment() << warning_surpressions() << kotlin_package();
  generate_union_definition(f_union, tunion);
  f_union.close();
}

void t_kotlin_generator::generate_kotlin_struct(t_struct* tstruct, bool is_exception) {
  string f_struct_name = package_dir_ + "/" + (tstruct->get_name()) + ".kt";
  ofstream_with_content_based_conditional_update f_struct;
  f_struct.open(f_struct_name.c_str());
  f_struct << autogen_comment() << warning_surpressions() << kotlin_package();
  generate_struct_definition(f_struct, tstruct, is_exception);
  f_struct.close();
}

void t_kotlin_generator::generate_struct_field_name_constants(std::ostream& out,
                                                              t_struct* tstruct) {
  indent(out) << "enum class _Fields(private val thriftFieldId: kotlin.Short, private val "
                 "fieldName: kotlin.String) : org.apache.thrift.TFieldIdEnum {"
              << endl;
  indent_up();
  {
    // fields
    {
      bool first = true;
      for (auto& field : tstruct->get_members()) {
        if (!first) {
          out << "," << endl;
        }
        first = false;
        indent(out) << constant_name(field->get_name()) << "(" << field->get_key() << ", \""
                    << field->get_name() << "\")";
      }
      if (first) {
        indent(out);
      }
      out << ";" << endl << endl;
    }

    // methods
    indent(out) << "override fun getThriftFieldId() = thriftFieldId" << endl << endl;
    indent(out) << "override fun getFieldName() = fieldName" << endl << endl;

    // companion object
    indent(out) << "companion object {" << endl;
    indent_up();
    {
      indent(out) << "@kotlin.jvm.JvmStatic" << endl;
      indent(out) << "fun findByValue(value: kotlin.Int): _Fields? {" << endl;
      indent_up();
      {
        indent(out) << "return when (value) {" << endl;
        indent_up();
        {
          for (auto& field : tstruct->get_members()) {
            indent(out) << field->get_key() << " -> " << constant_name(field->get_name()) << endl;
          }
          indent(out) << "else -> null" << endl;
        }
        scope_down(out);
      }
      scope_down(out);
    }

    out << endl;

    {
      indent(out) << "@kotlin.jvm.JvmStatic" << endl;
      indent(out) << "fun findByName(name: kotlin.String): _Fields? {" << endl;
      indent_up();
      {
        indent(out) << "return when (name) {" << endl;
        indent_up();
        {
          for (auto& field : tstruct->get_members()) {
            indent(out) << "\"" << field->get_name() << "\""
                        << " -> " << constant_name(field->get_name()) << endl;
          }
          indent(out) << "else -> null" << endl;
        }
        scope_down(out);
      }
      scope_down(out);
    }

    scope_down(out);
  }
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_struct_companion_object(std::ostream& out, t_struct* tstruct) {
  indent(out) << "companion object {" << endl;
  indent_up();
  {
    indent(out) << "private val STRUCT_DESC: org.apache.thrift.protocol.TStruct = "
                   "org.apache.thrift.protocol.TStruct(\""
                << tstruct->get_name() << "\")" << endl;
    {
      for (auto& field : tstruct->get_members()) {
        indent(out) << "private val " << constant_name(field->get_name())
                    << "_FIELD_DESC: org.apache.thrift.protocol.TField = "
                       "org.apache.thrift.protocol.TField(\""
                    << field->get_name() << "\", " << type_to_enum(field->get_type()) << ", "
                    << field->get_key() << ")" << endl;
      }
    }
  }
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_struct_method_deep_copy(std::ostream& out, t_struct* tstruct) {
  indent(out) << "override fun deepCopy(): " << tstruct->get_name() << " {" << endl;
  indent_up();
  {
    indent(out) << "return " << tstruct->get_name() << " (" << endl;
    indent_up();
    {
      for (auto& field : tstruct->get_members()) {
        indent(out) << kotlin_safe_name(field->get_name()) << "," << endl;
      }
    }
    indent_down();
    indent(out) << ")" << endl;
  }
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_struct_method_compare_to(std::ostream& out, t_struct* tstruct) {
  indent(out) << "override fun compareTo(other: " << tstruct->get_name() << "?): kotlin.Int {"
              << endl;
  indent_up();
  {
    indent(out) << "val comparator = compareBy<" << tstruct->get_name()
                << "> { it::class.java.name }" << endl;
    indent_up();
    for (auto& field : tstruct->get_members()) {
      indent(out) << ".thenBy";
      auto field_type = field->get_type();
      if (field_type->is_list() || field_type->is_set() || field_type->is_map()
          || field_type->is_binary()) {
        out << "(org.apache.thrift.TBaseHelper::compareTo)";
      }
      out << " { it." << kotlin_safe_name(field->get_name()) << " } " << endl;
    }
    indent_down();
    indent(out) << "return nullsFirst(comparator).compare(this, other)" << endl;
  }
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_struct_method_field_for_id(std::ostream& out,
                                                             t_struct* /*tstruct*/) {
  indent(out) << "override fun fieldForId(fieldId: kotlin.Int): _Fields {" << endl;
  indent_up();
  {
    indent(out) << "return _Fields.findByValue(fieldId) ?: throw "
                   "kotlin.IllegalArgumentException(\"invalid fieldId $fieldId\")"
                << endl;
  }
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_struct_method_is_set(std::ostream& out, t_struct* tstruct) {
  indent(out) << "override fun isSet(field: _Fields): kotlin.Boolean {" << endl;
  indent_up();
  {
    indent(out) << "return when (field) {" << endl;
    indent_up();
    {
      auto members = tstruct->get_members();
      if (members.size() > 0) {
        for (auto& field : members) {
          indent(out) << "_Fields." << constant_name(field->get_name()) << " -> ";
          if (field->get_req() == t_field::T_REQUIRED) {
            out << "this._" << field->get_name() << " != null";
          } else {
            out << "this." << kotlin_safe_name(field->get_name()) << " != null";
          }
          out << endl;
        }
      } else {
        indent(out) << "else -> false" << endl;
      }
    }
    scope_down(out);
  }
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_struct_method_clear(std::ostream& out, t_struct* tstruct) {
  indent(out) << "override fun clear(): kotlin.Unit {" << endl;
  indent_up();
  {
    for (auto& field : tstruct->get_members()) {
      auto is_required = field->get_req() == t_field::T_REQUIRED;
      indent(out) << (is_required ? "_" + field->get_name() : kotlin_safe_name(field->get_name()))
                  << " = null" << endl;
    }
  }
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_struct_method_validate(std::ostream& out, t_struct* tstruct) {
  indent(out) << "@kotlin.jvm.Throws(org.apache.thrift.TException::class)" << endl;
  indent(out) << "fun validate(): kotlin.Unit {" << endl;
  indent_up();
  {
    for (auto& field : tstruct->get_members()) {
      bool is_required = field->get_req() == t_field::T_REQUIRED;
      if (is_required) {
        indent(out) << "if (_" << field->get_name() << " == null) {" << endl;
        indent_up();
        {
          indent(out) << "throw org.apache.thrift.TException(\"Required field `"
                      << field->get_name()
                      << "' is null, "
                         "struct is: $this\")"
                      << endl;
        }
        scope_down(out);
      }
    }
  }
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_struct_method_set_field_value(std::ostream& out,
                                                                t_struct* tstruct) {
  indent(out) << "@Suppress(\"UNCHECKED_CAST\")" << endl;
  indent(out) << "override fun setFieldValue(field: _Fields, value: kotlin.Any?): kotlin.Unit {"
              << endl;
  indent_up();
  {
    const vector<t_field*>& members = tstruct->get_members();
    if (members.size() > 0) {
      indent(out) << "when (field) {" << endl;
      indent_up();
      {
        for (auto& field : tstruct->get_members()) {
          auto is_required = field->get_req() == t_field::T_REQUIRED;
          indent(out) << "_Fields." << constant_name(field->get_name()) << " -> this."
                      << (is_required ? "_" + field->get_name()
                                      : kotlin_safe_name(field->get_name()))
                      << " = value as " << type_name(field->get_type()) << "?" << endl;
        }
      }
      scope_down(out);
    } else {
      indent(out) << "return" << endl;
    }
  }
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_struct_method_get_field_value(std::ostream& out,
                                                                t_struct* tstruct) {
  indent(out) << "override fun getFieldValue(field: _Fields): kotlin.Any? {" << endl;
  indent_up();
  {
    auto members = tstruct->get_members();
    if (members.size() > 0) {
      indent(out) << "return when (field) {" << endl;
      indent_up();
      {
        for (auto& field : tstruct->get_members()) {
          indent(out) << "_Fields." << constant_name(field->get_name()) << " -> this."
                      << kotlin_safe_name(field->get_name()) << endl;
        }
      }
      scope_down(out);
    } else {
      indent(out) << "return null" << endl;
    }
  }
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_struct_method_read(std::ostream& out, t_struct* tstruct) {
  indent(out) << "override fun read(iproto: org.apache.thrift.protocol.TProtocol): kotlin.Unit {"
              << endl;
  indent_up();
  {
    indent(out)
        << "require(org.apache.thrift.scheme.StandardScheme::class.java == iproto.scheme) { "
           "\"only standard scheme is "
           "supported for now\" }"
        << endl;
    indent(out) << tstruct->get_name() << "StandardScheme.read(iproto, this)" << endl;
  }
  scope_down(out);
  out << endl;
}
void t_kotlin_generator::generate_struct_method_write(std::ostream& out, t_struct* tstruct) {
  indent(out) << "override fun write(oproto: org.apache.thrift.protocol.TProtocol): kotlin.Unit {"
              << endl;
  indent_up();
  {
    indent(out)
        << "require(org.apache.thrift.scheme.StandardScheme::class.java == oproto.scheme) { "
           "\"only standard scheme is "
           "supported for now\" }"
        << endl;
    indent(out) << tstruct->get_name() << "StandardScheme.write(oproto, this)" << endl;
  }
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_struct_standard_scheme_read(std::ostream& out,
                                                              t_struct* tstruct) {
  indent(out) << "override fun read(iproto: org.apache.thrift.protocol.TProtocol, struct: "
              << tstruct->get_name() << ") {" << endl;
  indent_up();
  {
    indent(out) << "iproto.apply {" << endl;
    indent_up();
    {
      indent(out) << "readStruct {" << endl;
      indent_up();
      {
        indent(out) << "var stopped = false" << endl;
        indent(out) << "while (!stopped) {" << endl;
        indent_up();
        {
          indent(out) << "stopped = readField {" << endl;
          indent_up();
          {
            indent(out) << "val skipNext = { "
                           "org.apache.thrift.protocol.TProtocolUtil.skip(iproto, type) }"
                        << endl;

            indent(out) << "when (id.toInt()) {" << endl;
            indent_up();
            {
              for (auto& field : tstruct->get_members()) {
                indent(out) << field->get_key() << " -> {" << endl;
                indent_up();
                {
                  indent(out) << "if (type == " << type_to_enum(field->get_type()) << ") {" << endl;
                  indent_up();
                  generate_deserialize_field(out, field, "struct.");
                  indent_down();
                  indent(out) << "} else {" << endl;
                  indent_up();
                  indent(out) << "skipNext()" << endl;
                  indent_down();
                  indent(out) << "}" << endl;
                }
                scope_down(out);
              }
              indent(out) << "else -> skipNext()" << endl;
            }
            scope_down(out);
          }
          scope_down(out);
        }
        scope_down(out);
        indent(out) << "struct.validate()" << endl;
      }
      scope_down(out);
    }
    scope_down(out);
  }
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_struct_standard_scheme_write(std::ostream& out,
                                                               t_struct* tstruct) {
  indent(out) << "override fun write(oproto: org.apache.thrift.protocol.TProtocol, struct: "
              << tstruct->get_name() << ") {" << endl;
  indent_up();
  {
    indent(out) << "struct.validate()" << endl;
    indent(out) << "oproto.apply {" << endl;
    indent_up();
    {
      indent(out) << "writeStruct(STRUCT_DESC) {" << endl;
      indent_up();
      {
        for (auto& field : tstruct->get_members()) {
          auto is_required = field->get_req() == t_field::T_REQUIRED;
          indent(out) << "struct." << kotlin_safe_name(field->get_name())
                      << (is_required ? "" : "?") << ".let {" << endl;
          indent_up();
          {
            indent(out) << "writeField(" << constant_name(field->get_name()) << "_FIELD_DESC) {"
                        << endl;
            indent_up();
            generate_serialize_field(out, field);
            scope_down(out);
          }
          scope_down(out);
        }
      }
      indent(out) << "writeFieldStop()" << endl;
      scope_down(out);
    }
    scope_down(out);
  }
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_struct_standard_scheme(std::ostream& out, t_struct* tstruct) {
  indent(out) << "private object " << tstruct->get_name()
              << "StandardScheme : org.apache.thrift.scheme.StandardScheme<" << tstruct->get_name()
              << ">() {" << endl;
  indent_up();
  generate_struct_standard_scheme_read(out, tstruct);
  generate_struct_standard_scheme_write(out, tstruct);
  scope_down(out);
  out << endl;
}

void t_kotlin_generator::generate_union_tuple_scheme(std::ostream& out, t_struct* /*tunion*/) {
  indent(out) << "override fun tupleSchemeReadValue(iproto: org.apache.thrift.protocol.TProtocol, "
                 "fieldID: kotlin.Short) = throw kotlin.UnsupportedOperationException(\"only "
                 "standard scheme is supported for now\")"
              << endl;
  indent(out)
      << "override fun tupleSchemeWriteValue(oproto: org.apache.thrift.protocol.TProtocol) = "
         "throw kotlin.UnsupportedOperationException(\"only standard scheme is supported for "
         "now\")"
      << endl;
}

void t_kotlin_generator::generate_union_standard_scheme(std::ostream& out, t_struct* tunion) {
  generate_union_standard_scheme_read(out, tunion);
  generate_union_standard_scheme_write(out, tunion);
}

void t_kotlin_generator::generate_union_standard_scheme_read(std::ostream& out, t_struct* tunion) {
  indent(out)
      << "override fun standardSchemeReadValue(iproto: org.apache.thrift.protocol.TProtocol, "
         "field: org.apache.thrift.protocol.TField): Any? ="
      << endl;
  indent_up();
  indent(out) << "when (_Fields.findByValue(field.id.toInt())) {" << endl;
  indent_up();
  for (auto& member : tunion->get_members()) {
    auto expect_type = type_name(member->get_type());
    indent(out) << "_Fields." << constant_name(member->get_name()) << " -> {" << endl;
    indent_up();
    {
      indent(out) << "if (field.type == " << constant_name(member->get_name())
                  << "_FIELD_DESC.type) {" << endl;
      indent_up();
      indent(out) << "iproto.run {" << endl;
      indent_up();
      indent(out);
      generate_deserialize_value(out, member->get_type());
      out << endl;
      scope_down(out);
      indent_down();
      indent(out) << "} else {" << endl;
      indent_up();
      indent(out) << "org.apache.thrift.protocol.TProtocolUtil.skip(iproto, field.type)" << endl;
      indent(out) << "null" << endl;
      scope_down(out);
    }
    scope_down(out);
  }
  indent(out) << "null -> {" << endl;
  indent_up();
  indent(out) << "org.apache.thrift.protocol.TProtocolUtil.skip(iproto, field.type)" << endl;
  indent(out) << "null" << endl;
  scope_down(out);
  scope_down(out);
  indent_down();
}

void t_kotlin_generator::generate_union_standard_scheme_write(std::ostream& out, t_struct* tunion) {
  indent(out) << "@Suppress(\"UNCHECKED_CAST\")" << endl;
  indent(out)
      << "override fun standardSchemeWriteValue(oproto: org.apache.thrift.protocol.TProtocol) {"
      << endl;
  indent_up();
  indent(out) << "when (setField_) {" << endl;
  indent_up();
  for (auto& member : tunion->get_members()) {
    indent(out) << "_Fields." << constant_name(member->get_name()) << " -> {" << endl;
    indent_up();
    {
      indent(out) << "val it = value_ as " << type_name(member->get_type()) << endl;
      indent(out) << "oproto.apply {" << endl;
      indent_up();
      {
        indent(out);
        generate_serialize_value(out, member->get_type());
        out << endl;
      }
      scope_down(out);
    }
    scope_down(out);
  }
  indent(out) << "null -> throw kotlin.IllegalStateException(\"Cannot write union with unknown "
                 "field $setField_\")"
              << endl;
  scope_down(out);
  scope_down(out);
}
void t_kotlin_generator::generate_union_methods_definitions(std::ostream& out, t_struct* tunion) {
  {
    // this is a hack to reuse code
    t_struct union_fields(program_, tunion->get_name());
    t_enum enum_type(program_);
    enum_type.set_name("setField_");
    t_field set_field(&enum_type, "setField_", 0);
    t_base_type value_type("value_", t_base_type::TYPE_STRING);
    value_type.set_binary(true);
    t_field value(&value_type, "value_", 1);
    union_fields.append(&set_field);
    union_fields.append(&value);
    generate_struct_method_compare_to(out, &union_fields);
  }

  auto union_class_name = kotlin_safe_name(tunion->get_name());
  { indent(out) << "override fun deepCopy() = " << union_class_name << "(this)" << endl; }
  { indent(out) << "override fun enumForId(id: kotlin.Short) = fieldForId(id.toInt())" << endl; }
  { indent(out) << "override fun getStructDesc() = STRUCT_DESC" << endl; }
  {
    indent(out) << "override fun getFieldDesc(setField: _Fields) = when (setField) {" << endl;
    indent_up();
    for (auto& member : tunion->get_members()) {
      indent(out) << "_Fields." << constant_name(member->get_name()) << " -> "
                  << constant_name(member->get_name()) << "_FIELD_DESC" << endl;
    }
    scope_down(out);
  }
}

void t_kotlin_generator::generate_union_method_check_type(std::ostream& out, t_struct* tunion) {
  indent(out) << "@Suppress(\"UNCHECKED_CAST\")" << endl;
  indent(out) << "override fun checkType(setField: _Fields, value: kotlin.Any?) {" << endl;
  indent_up();
  indent(out) << "when (setField) {" << endl;
  indent_up();
  for (auto& member : tunion->get_members()) {
    auto expect_type = type_name(member->get_type());
    indent(out) << "_Fields." << constant_name(member->get_name()) << " -> value as? "
                << expect_type
                << " ?: throw kotlin.ClassCastException(\"Was expecting value of type `"
                << expect_type << "' for field `" << member->get_name()
                << "', but got ${value?.javaClass}\")" << endl;
  }
  scope_down(out);
  scope_down(out);
}

void t_kotlin_generator::generate_union_definition(std::ostream& out,
                                                   t_struct* tunion,
                                                   string /*additional interface*/) {
  auto union_class_name = kotlin_safe_name(tunion->get_name());
  indent(out) << "class " << union_class_name << " : org.apache.thrift.TUnion<" << union_class_name
              << ", " << union_class_name << "._Fields> {" << endl;
  indent_up();
  indent(out) << "constructor(setField: _Fields, value: kotlin.Any) : super(setField, value)"
              << endl;
  indent(out) << "constructor(other: " << union_class_name << ") : super(other)" << endl;
  indent(out) << "constructor() : super()" << endl;

  generate_struct_field_name_constants(out, tunion);
  generate_struct_companion_object(out, tunion);
  generate_struct_method_field_for_id(out, tunion);
  generate_union_methods_definitions(out, tunion);
  generate_union_method_check_type(out, tunion);
  generate_union_standard_scheme(out, tunion);
  generate_union_tuple_scheme(out, tunion);
  indent_down();
  indent(out) << "}" << endl;
}

void t_kotlin_generator::generate_struct_definition(std::ostream& out,
                                                    t_struct* tstruct,
                                                    bool is_exception,
                                                    string additional_interface) {
  generate_kdoc_comment(out, tstruct);
  auto members = tstruct->get_members();
  if (members.size() > 0) {
    indent(out) << "data class ";
  } else {
    indent(out) << "class ";
  }
  out << kotlin_safe_name(tstruct->get_name()) << "(";

  indent_up();
  auto sep = "";
  for (auto field : members) {
    out << sep << endl;
    sep = ",";
    generate_kdoc_comment(out, field);
    auto is_required = field->get_req() == t_field::T_REQUIRED;
    if (is_required) {
      indent(out) << "private var _" << field->get_name();
    } else if (is_exception && field->get_name() == "message") {
      // special handling for exception when field name is message - needs override
      if (!field->get_type()->is_string()) {
        throw "type error: for `message' field in an exception struct, it must be a string";
      }
      indent(out) << "override var message";
    } else {
      indent(out) << "var " << kotlin_safe_name(field->get_name());
    }
    out << ": " << type_name(field->get_type()) << "? = null";
  }
  indent_down();
  out << endl;
  indent(out) << ") : ";
  if (is_exception) {
    out << "org.apache.thrift.TException(), ";
  }
  if (additional_interface != "") {
    additional_interface = ", " + additional_interface;
  }
  out << "org.apache.thrift.TBase<" << tstruct->get_name() << ", " << tstruct->get_name()
      << "._Fields>" << additional_interface << " {" << endl;

  indent_up();

  for (auto field : members) {
    if (field->get_req() == t_field::T_REQUIRED) {
      indent(out);
      // special handling for exception when field name is message - needs override
      if (is_exception && field->get_name() == "message") {
        out << "override ";
      }
      out << "val " << kotlin_safe_name(field->get_name()) << ": " << type_name(field->get_type())
          << " get() = _" + kotlin_safe_name(field->get_name()) << "!!" << endl;
    }
  }

  generate_struct_field_name_constants(out, tstruct);
  generate_struct_companion_object(out, tstruct);
  generate_struct_standard_scheme(out, tstruct);
  generate_struct_method_compare_to(out, tstruct);
  generate_struct_method_field_for_id(out, tstruct);
  generate_struct_method_get_field_value(out, tstruct);
  generate_struct_method_set_field_value(out, tstruct);
  generate_struct_method_is_set(out, tstruct);
  generate_struct_method_deep_copy(out, tstruct);
  generate_struct_method_clear(out, tstruct);
  generate_struct_method_validate(out, tstruct);
  generate_struct_method_read(out, tstruct);
  generate_struct_method_write(out, tstruct);

  indent_down();
  indent(out) << "}" << endl;
}

string t_kotlin_generator::base_type_write_expression(t_base_type* tbase, string it) {
  switch (tbase->get_base()) {
  case t_base_type::TYPE_VOID:
    throw "compiler error: no void in base types";
  case t_base_type::TYPE_STRING:
    if (tbase->is_binary()) {
      return "writeBinary(java.nio.ByteBuffer.wrap(" + it + "))";
    } else {
      return "writeString(" + it + ")";
    }
  case t_base_type::TYPE_BOOL:
    return "writeBool(" + it + ")";
  case t_base_type::TYPE_I8:
    return "writeByte(" + it + ")";
  case t_base_type::TYPE_I16:
    return "writeI16(" + it + ")";
  case t_base_type::TYPE_I32:
    return "writeI32(" + it + ")";
  case t_base_type::TYPE_I64:
    return "writeI64(" + it + ")";
  case t_base_type::TYPE_DOUBLE:
    return "writeDouble(" + it + ")";
  default:
    throw "compiler error: no Kotlin name for base type "
        + t_base_type::t_base_name(tbase->get_base());
  }
}

string t_kotlin_generator::base_type_read_expression(t_base_type* tbase) {
  switch (tbase->get_base()) {
  case t_base_type::TYPE_VOID:
    throw "compiler error: no void in base types";
  case t_base_type::TYPE_STRING:
    if (tbase->is_binary()) {
      return "org.apache.thrift.TBaseHelper.byteBufferToByteArray(readBinary())";
    } else {
      return "readString()";
    }
  case t_base_type::TYPE_BOOL:
    return "readBool()";
  case t_base_type::TYPE_I8:
    return "readByte()";
  case t_base_type::TYPE_I16:
    return "readI16()";
  case t_base_type::TYPE_I32:
    return "readI32()";
  case t_base_type::TYPE_I64:
    return "readI64()";
  case t_base_type::TYPE_DOUBLE:
    return "readDouble()";
  default:
    throw "compiler error: no Kotlin name for base type "
        + t_base_type::t_base_name(tbase->get_base());
  }
}

void t_kotlin_generator::generate_serialize_value(ostream& out, t_type* type, string it) {
  t_type* ttype = get_true_type(type);
  if (ttype->is_struct() || ttype->is_xception()) {
    out << it << ".write(this)";
  } else if (ttype->is_container()) {
    generate_serialize_container(out, ttype, it);
  } else if (ttype->is_base_type()) {
    out << base_type_write_expression((t_base_type*)ttype, it);
  } else if (ttype->is_enum()) {
    out << "writeI32(" << it << ".value)";
  } else {
    printf("cannot deserialize type '%s'\n", type_name(ttype).c_str());
  }
}

void t_kotlin_generator::generate_deserialize_value(ostream& out, t_type* type) {
  t_type* ttype = get_true_type(type);
  if (ttype->is_struct() || ttype->is_xception()) {
    out << type_name(ttype) << "().apply { read(iproto) }";
  } else if (ttype->is_container()) {
    generate_deserialize_container(out, ttype);
  } else if (ttype->is_base_type()) {
    out << base_type_read_expression((t_base_type*)ttype);
  } else if (ttype->is_enum()) {
    out << "requireNotNull(" << type_name(ttype, false, false, true) + ".findByValue(readI32()))";
  } else {
    printf("cannot deserialize type '%s'\n", type_name(ttype).c_str());
  }
}

/**
 * Serializes a field of any type.
 *
 * @param tfield The field
 */
void t_kotlin_generator::generate_serialize_field(ostream& out, t_field* tfield) {
  t_type* type = get_true_type(tfield->get_type());
  if (type->is_void()) {
    throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE: " + tfield->get_name();
  }
  indent(out);
  generate_serialize_value(out, type);
  out << endl;
}

/**
 * Deserializes a field of any type.
 *
 * @param tfield The field
 * @param prefix The variable name or container for this field
 */
void t_kotlin_generator::generate_deserialize_field(ostream& out, t_field* tfield, string prefix) {
  t_type* type = get_true_type(tfield->get_type());
  if (type->is_void()) {
    throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE: " + prefix + tfield->get_name();
  }
  auto is_required = tfield->get_req() == t_field::T_REQUIRED;
  string name
      = prefix + (is_required ? "_" + tfield->get_name() : kotlin_safe_name(tfield->get_name()));
  indent(out) << name << " = ";
  generate_deserialize_value(out, type);
  out << endl;
}

/**
 * Serializes a container by writing its size and then iterating
 */
void t_kotlin_generator::generate_serialize_container(ostream& out, t_type* ttype, string it) {
  if (ttype->is_map()) {
    out << "writeMap(" << type_to_enum(((t_map*)ttype)->get_key_type()) << ", "
        << type_to_enum(((t_map*)ttype)->get_val_type()) << ", " << it << ") { (key, value) ->"
        << endl;
    indent_up();
    {
      generate_serialize_value(indent(out), ((t_map*)ttype)->get_key_type(), "key");
      out << endl;
      generate_serialize_value(indent(out), ((t_map*)ttype)->get_val_type(), "value");
      out << endl;
      indent_down();
    }
    indent(out) << "}";
  } else if (ttype->is_set()) {
    out << "writeSet(" << type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " << it << ") {"
        << endl;
    indent_up();
    {
      generate_serialize_value(indent(out), ((t_set*)ttype)->get_elem_type());
      out << endl;
      indent_down();
    }
    indent(out) << "}";
  } else if (ttype->is_list()) {
    out << "writeList(" << type_to_enum(((t_list*)ttype)->get_elem_type()) << ", " << it << ") {"
        << endl;
    {
      indent_up();
      generate_serialize_value(indent(out), ((t_list*)ttype)->get_elem_type());
      out << endl;
      indent_down();
    }
    indent(out) << "}";
  } else {
    throw "not a container type: " + ttype->get_name();
  }
}

/**
 * Deserializes a container by reading its size and then iterating
 */
void t_kotlin_generator::generate_deserialize_container(ostream& out, t_type* ttype) {
  if (ttype->is_map()) {
    out << "readMap {" << endl;
    indent_up();
    indent(out) << "kotlin.collections.List(size) {" << endl;
    indent_up();
    indent(out);
    generate_deserialize_value(out, ((t_map*)ttype)->get_key_type());
    out << " to ";
    generate_deserialize_value(out, ((t_map*)ttype)->get_val_type());
    out << endl;
    indent_down();
    indent(out) << "}.associate { it }" << endl;
    indent_down();
    indent(out) << "}";
  } else if (ttype->is_set()) {
    out << "readSet {" << endl;
    indent_up();
    indent(out) << "kotlin.collections.List(size) {" << endl;
    indent_up();
    indent(out);
    generate_deserialize_value(out, ((t_set*)ttype)->get_elem_type());
    out << endl;
    indent_down();
    indent(out) << "}.toSet()" << endl;
    indent_down();
    indent(out) << "}";
  } else if (ttype->is_list()) {
    out << "readList {" << endl;
    indent_up();
    indent(out) << "kotlin.collections.List(size) {" << endl;
    indent_up();
    indent(out);
    generate_deserialize_value(out, ((t_list*)ttype)->get_elem_type());
    out << endl;
    indent_down();
    indent(out) << "}" << endl;
    indent_down();
    indent(out) << "}";
  } else {
    throw "not a container type: " + ttype->get_name();
  }
}

string t_kotlin_generator::function_signature(t_function* tfunction, string prefix) {
  auto result = "suspend fun " + prefix + tfunction->get_name() + "(";
  auto arguments = tfunction->get_arglist();
  bool first = true;
  for (t_field* tfield : arguments->get_members()) {
    if (first) {
      first = false;
    } else {
      result += ", ";
    }
    result += tfield->get_name() + ": " + type_name(tfield->get_type());
  }
  result += "): ";
  result += type_name(tfunction->get_returntype());
  return result;
}

void t_kotlin_generator::generate_service_interface(t_service* tservice) {
  string f_service_name = package_dir_ + "/" + tservice->get_name() + ".kt";
  ofstream_with_content_based_conditional_update out;
  out.open(f_service_name.c_str());
  out << autogen_comment() << kotlin_package();
  out << "interface " << tservice->get_name() << " {" << endl;
  indent_up();
  for (auto tfunc : tservice->get_functions()) {
    generate_kdoc_comment(out, tfunc);
    indent(out) << function_signature(tfunc) << endl;
  }
  scope_down(out);
  out << endl << endl;
  out.close();
}

void t_kotlin_generator::generate_service_client(t_service* tservice) {
  string f_service_name = package_dir_ + "/" + tservice->get_name() + "Client.kt";
  ofstream_with_content_based_conditional_update out;
  out.open(f_service_name.c_str());
  out << autogen_comment() << warning_surpressions() << kotlin_package();
  generate_docstring_comment(out, "/**\n", " * ",
                             "client implementation for [" + tservice->get_name() + "]", " */\n");
  indent(out) << "class " << tservice->get_name() << "Client(" << endl;
  indent_up();
  indent(out) << "protocolFactory: org.apache.thrift.protocol.TProtocolFactory," << endl;
  indent(out) << "clientManager: org.apache.thrift.async.TAsyncClientManager," << endl;
  indent(out) << "transport: org.apache.thrift.transport.TNonblockingTransport" << endl;
  indent_down();
  out << "): org.apache.thrift.async.TAsyncClient(protocolFactory, clientManager, transport), "
      << tservice->get_name() << " {" << endl
      << endl;

  indent_up();
  {
    indent(out) << "private val seqId = java.util.concurrent.atomic.AtomicInteger()" << endl
                << endl;
    for (auto tfunc : tservice->get_functions()) {
      indent(out) << "override " << function_signature(tfunc) << " {" << endl;
      indent_up();
      {
        string args_name = tservice->get_name() + "FunctionArgs." + tfunc->get_name() + "_args";
        indent(out) << "val args = " << args_name << "(";
        auto first = true;
        for (auto tfield : tfunc->get_arglist()->get_members()) {
          if (!first) {
            out << ", ";
          }
          first = false;
          out << tfield->get_name();
        }
        out << ")" << endl;
        indent(out) << "return transformCallback {" << endl;
        indent_up();
        {
          indent(out) << "checkReady()" << endl;
          indent(out)
              << "___currentMethod = ProcessCall." << tfunc->get_name()
              << "Call(args, seqId.getAndIncrement(), this, ___protocolFactory, ___transport, it)"
              << endl;
          indent(out) << "___manager.call(___currentMethod)" << endl;
        }
        scope_down(out);
      }
      scope_down(out);
    }

    indent(out) << "private suspend fun <R> "
                   "org.apache.thrift.async.TAsyncClient.transformCallback(action: "
                   "(org.apache.thrift.async.AsyncMethodCallback<R>) -> Unit): R {"
                << endl;
    indent_up();
    indent(out) << "val deferred = kotlinx.coroutines.CompletableDeferred<R>()" << endl;
    indent(out) << "val callback = object : org.apache.thrift.async.AsyncMethodCallback<R> {"
                << endl;
    indent_up();
    indent(out) << "override fun onComplete(response: R) { deferred.complete(response) }" << endl;
    indent(out) << "override fun onError(exception: java.lang.Exception) { "
                   "deferred.completeExceptionally(exception) }"
                << endl;
    scope_down(out);
    indent(out) << "action(callback)" << endl;
    indent(out) << "return deferred.await()" << endl;
    scope_down(out);

    indent(out) << "sealed interface ProcessCall {" << endl;
    indent_up();
    for (auto tfunc : tservice->get_functions()) {
      generate_client_call(out, tservice, tfunc);
    }
    scope_down(out);
  }
  scope_down(out);
  out << endl << endl;
  out.close();
}

void t_kotlin_generator::generate_client_call(std::ostream& out,
                                              t_service* tservice,
                                              t_function* tfunc) {
  string funname = tfunc->get_name();
  string funclassname = funname + "Call";
  string rtype = type_name(tfunc->get_returntype(), true);

  indent(out) << "class " + funclassname + "(" << endl;
  indent_up();
  string args_name = tservice->get_name() + "FunctionArgs." + tfunc->get_name() + "_args";
  indent(out) << "val args: " << args_name << "," << endl;
  indent(out) << "val seqId: kotlin.Int," << endl;
  indent(out) << "client: org.apache.thrift.async.TAsyncClient," << endl;
  indent(out) << "protocolFactory: org.apache.thrift.protocol.TProtocolFactory," << endl;
  indent(out) << "transport: org.apache.thrift.transport.TNonblockingTransport," << endl;
  indent(out) << "resultHandler: org.apache.thrift.async.AsyncMethodCallback<" << rtype << ">,"
              << endl;
  indent_down();
  indent(out) << ") : org.apache.thrift.async.TAsyncMethodCall<" << rtype
              << ">(client, protocolFactory, transport, resultHandler, "
              << (tfunc->is_oneway() ? "true" : "false") << "), ProcessCall {" << endl;

  indent_up();
  indent(out) << "override fun write_args(protocol: org.apache.thrift.protocol.TProtocol) {"
              << endl;
  indent_up();
  indent(out) << "val marker = org.apache.thrift.protocol.TMessage(\"" << tfunc->get_name()
              << "\", org.apache.thrift.protocol.TMessageType.CALL, seqId)" << endl;
  indent(out) << "protocol.writeMessage(marker) { args.write(protocol) }" << endl;
  scope_down(out);

  indent(out) << "override fun getResult(): " << rtype << " {" << endl;
  indent_up();
  indent(out) << "check(state == org.apache.thrift.async.TAsyncMethodCall.State.RESPONSE_READ) { "
                 "\"Method call not finished!\" }"
              << endl;
  indent(out) << "val memoryTransport = "
                 "org.apache.thrift.transport.TMemoryInputTransport(frameBuffer.array())"
              << endl;
  indent(out) << "val protocol = client.protocolFactory.getProtocol(memoryTransport)" << endl;

  if (tfunc->is_oneway()) {
    indent(out) << "// one way function, nothing to read" << endl;
  } else {
    indent(out) << "return protocol.readMessage {" << endl;
    indent_up();
    {
      indent(out) << "if (type == org.apache.thrift.protocol.TMessageType.EXCEPTION) {" << endl;
      indent_up();
      indent(out) << "val ex = org.apache.thrift.TApplicationException().apply { read(protocol) }"
                  << endl;
      indent(out) << "throw ex" << endl;
      scope_down(out);
      indent(out) << "if (seqid != seqId) {" << endl;
      indent_up();
      indent(out) << "throw org.apache.thrift.TApplicationException(" << endl;
      indent_up();
      indent(out) << "org.apache.thrift.TApplicationException.BAD_SEQUENCE_ID," << endl;
      indent(out) << "\"" << funname
                  << " failed: out of sequence response: expected $seqId but got ${seqid}\""
                  << endl;
      indent_down();
      indent(out) << ")" << endl;
      scope_down(out);
      string result_name = tservice->get_name() + "FunctionResult." + tfunc->get_name() + "_result";
      indent(out) << "val result = " << result_name << "().apply { read(protocol) }" << endl;
      for (auto xception : tfunc->get_xceptions()->get_members()) {
        indent(out) << "result." << xception->get_name() << "?.let { throw it }" << endl;
      }
      if (!tfunc->get_returntype()->is_void()) {
        indent(out)
            << "result.success ?: throw "
               "org.apache.thrift.TApplicationException(org.apache.thrift.TApplicationException."
               "MISSING_RESULT, \"returnString failed: unknown result\")"
            << endl;
      }
    }
    scope_down(out);
  }
  scope_down(out);
  scope_down(out);
}

void t_kotlin_generator::generate_service_processor(t_service* tservice) {
  string f_service_name = package_dir_ + "/" + tservice->get_name() + "Processor.kt";
  ofstream_with_content_based_conditional_update out;
  out.open(f_service_name.c_str());
  out << autogen_comment() << warning_surpressions() << kotlin_package();
  auto service_imports = {"import kotlinx.coroutines.future.future"};
  for (auto service_import : service_imports) {
    out << service_import << endl;
  }
  out << endl;

  generate_docstring_comment(out, "/**\n", " * ",
                             "server implementation for [" + tservice->get_name() + "]", " */\n");
  indent(out) << "class " << tservice->get_name() << "Processor(" << endl;
  indent_up();
  indent(out) << "handler: " << tservice->get_name() << "," << endl;
  indent(out) << "private val scope: kotlinx.coroutines.CoroutineScope," << endl;
  indent(out) << "private val processMap: kotlin.collections.Map<kotlin.String, "
                 "org.apache.thrift.AsyncProcessFunction<"
              << tservice->get_name()
              << ", out org.apache.thrift.TBase<*, "
                 "*>, out kotlin.Any>> = mapOf("
              << endl;
  indent_up();
  {
    for (auto tfunc : tservice->get_functions()) {
      indent(out) << '"' << tfunc->get_name() << '"' << " to ProcessFunction." << tfunc->get_name()
                  << "(scope)," << endl;
    }
  }
  indent_down();
  indent(out) << ")" << endl;
  indent_down();
  out << "): org.apache.thrift.TBaseAsyncProcessor<" << tservice->get_name()
      << ">(handler, processMap) {" << endl;
  indent_up();
  indent(out) << "companion object {" << endl;
  indent_up();
  indent(out) << "internal val logger: org.slf4j.Logger = "
                 "org.slf4j.LoggerFactory.getLogger("
              << tservice->get_name() << "Processor::class.java)" << endl;
  scope_down(out);

  indent(out) << "sealed interface ProcessFunction {" << endl;
  indent_up();

  {
    for (auto tfunc : tservice->get_functions()) {
      generate_service_process_function(out, tservice, tfunc);
    }
  }
  scope_down(out);
  scope_down(out);
  out << endl << endl;
  out.close();
}

void t_kotlin_generator::generate_service_process_function(ostream& out,
                                                           t_service* tservice,
                                                           t_function* tfunc) {
  string args_name = tservice->get_name() + "FunctionArgs." + tfunc->get_name() + "_args";
  string rtype = type_name(tfunc->get_returntype(), true);

  indent(out) << "class " << tfunc->get_name() << "<I : " << tservice->get_name()
              << ">(private val scope: kotlinx.coroutines.CoroutineScope) : "
                 "org.apache.thrift.AsyncProcessFunction<I, "
              << args_name << ", " << rtype << ">(\"" << tfunc->get_name()
              << "\"), ProcessFunction {" << endl;
  indent_up();
  {
    indent(out) << "override fun isOneway() = " << (tfunc->is_oneway() ? "true" : "false") << endl;
    indent(out) << "override fun getEmptyArgsInstance() = " << args_name << "()" << endl;
    indent(out) << "override fun start(iface: I, args: " << args_name
                << ", resultHandler: org.apache.thrift.async.AsyncMethodCallback<" << rtype
                << ">) {" << endl;
    indent_up();
    indent(out) << "scope.future {" << endl;
    indent_up();
    indent(out) << "iface." << tfunc->get_name() << "(";
    {
      auto arguments = tfunc->get_arglist();
      bool first = true;
      for (t_field* tfield : arguments->get_members()) {
        if (first) {
          first = false;
        } else {
          out << ", ";
        }
        out << "args." << tfield->get_name() << "!!";
      }
    }
    out << ")" << endl;
    indent_down();
    indent(out) << "}.whenComplete { r, t ->" << endl;
    {
      indent_up();
      indent(out) << "if (t != null) {" << endl;
      indent_up();
      indent(out) << "resultHandler.onError(t as java.lang.Exception)" << endl;
      indent_down();
      indent(out) << "} else {" << endl;
      indent_up();
      indent(out) << "resultHandler.onComplete(r)" << endl;
    }
    scope_down(out);
    scope_down(out);
    scope_down(out);

    indent(out) << "override fun getResultHandler(fb: "
                   "org.apache.thrift.server.AbstractNonblockingServer.AsyncFrameBuffer, seqid: "
                   "Int) ="
                << endl;
    indent_up();
    {
      indent(out) << "object : org.apache.thrift.async.AsyncMethodCallback<" << rtype << ">{"
                  << endl;
      indent_up();
      {
        indent(out) << "override fun onComplete(response: " << rtype << ") {" << endl;
        indent_up();
        if (tfunc->is_oneway()) {
          indent(out) << "// one way function, no result handling" << endl;
        } else {
          string result_name
              = tservice->get_name() + "FunctionResult." + tfunc->get_name() + "_result";
          indent(out) << "val result = " << result_name << "()" << endl;
          if (!tfunc->get_returntype()->is_void()) {
            indent(out) << "result.success = response" << endl;
          }
          indent(out) << "try {" << endl;
          indent_up();
          indent(out)
              << "sendResponse(fb, result, org.apache.thrift.protocol.TMessageType.REPLY, seqid)"
              << endl;
          indent_down();
          indent(out) << "} catch (e: org.apache.thrift.transport.TTransportException) {" << endl;
          indent_up();
          indent(out) << "logger.error(\"TTransportException writing to internal frame buffer\", e)"
                      << endl;
          indent(out) << "fb.close()" << endl;
          indent_down();
          indent(out) << "} catch (e: Exception) {" << endl;
          indent_up();
          indent(out) << "logger.error(\"Exception writing to internal frame buffer\", e)" << endl;
          indent(out) << "onError(e)" << endl;
          scope_down(out);
        }
        scope_down(out);
      }
      {
        indent(out) << "override fun onError(exception: kotlin.Exception) {" << endl;
        indent_up();
        if (tfunc->is_oneway()) {
          indent(out) << "if (exception is org.apache.thrift.transport.TTransportException) {"
                      << endl;
          indent_up();
          indent(out) << "logger.error(\"TTransportException inside handler\", exception)" << endl;
          indent(out) << "fb.close()" << endl;
          indent_down();
          indent(out) << "} else {" << endl;
          indent_up();
          indent(out) << "logger.error(\"Exception inside oneway handler\", exception)" << endl;
          scope_down(out);
        } else {
          indent(out) << "val (msgType, msg) = when (exception) {" << endl;
          indent_up();

          auto xceptions = tfunc->get_xceptions()->get_members();
          for (auto xception : xceptions) {
            indent(out) << "is " << type_name(xception->get_type()) << " -> {" << endl;
            indent_up();
            string result_name
                = tservice->get_name() + "FunctionResult." + tfunc->get_name() + "_result";
            indent(out) << "val result = " << result_name << "()" << endl;
            indent(out) << "result." << xception->get_name() << " = exception" << endl;
            indent(out) << "org.apache.thrift.protocol.TMessageType.REPLY to result" << endl;
            scope_down(out);
          }

          indent(out) << "is org.apache.thrift.transport.TTransportException -> {" << endl;
          indent_up();
          indent(out) << "logger.error(\"TTransportException inside handler\", exception)" << endl;
          indent(out) << "fb.close()" << endl;
          indent(out) << "return" << endl;
          scope_down(out);

          indent(out) << "is org.apache.thrift.TApplicationException -> {" << endl;
          indent_up();
          indent(out) << "logger.error(\"TApplicationException inside handler\", exception)"
                      << endl;
          indent(out) << "org.apache.thrift.protocol.TMessageType.EXCEPTION to exception" << endl;
          scope_down(out);

          indent(out) << "else -> {" << endl;
          indent_up();
          indent(out) << "logger.error(\"Exception inside handler\", exception)" << endl;
          indent(out) << "org.apache.thrift.protocol.TMessageType.EXCEPTION to "
                         "org.apache.thrift.TApplicationException(org.apache.thrift."
                         "TApplicationException.INTERNAL_ERROR, exception.message)"
                      << endl;
          scope_down(out);
          scope_down(out);

          indent(out) << "try {" << endl;
          indent_up();
          indent(out) << "sendResponse(fb, msg, msgType, seqid)" << endl;
          indent_down();
          indent(out) << "} catch (ex: java.lang.Exception) {" << endl;
          indent_up();
          indent(out) << "logger.error(\"Exception writing to internal frame buffer\", ex)" << endl;
          indent(out) << "fb.close()" << endl;
          scope_down(out);
        }

        scope_down(out);
      }
      scope_down(out);
    }
    indent_down();
  }
  scope_down(out);
}

void t_kotlin_generator::generate_service_result_helpers(t_service* tservice) {
  string f_service_result_name = package_dir_ + "/" + tservice->get_name() + "FunctionResult.kt";
  ofstream_with_content_based_conditional_update out;
  out.open(f_service_result_name.c_str());
  out << autogen_comment() << warning_surpressions() << kotlin_package();

  generate_docstring_comment(out, "/**\n", " * ",
                             "function result for [" + tservice->get_name() + "]", " */\n");
  indent(out) << "sealed interface " << tservice->get_name() << "FunctionResult {" << endl;
  indent_up();
  for (auto func : tservice->get_functions()) {
    if (func->is_oneway()) {
      continue;
    }
    t_struct result(program_, func->get_name() + "_result");
    t_field success(func->get_returntype(), "success", 0);
    if (!func->get_returntype()->is_void()) {
      result.append(&success);
    }
    for (auto& member : func->get_xceptions()->get_members()) {
      result.append(member);
    }
    generate_struct_definition(out, &result, false, tservice->get_name() + "FunctionResult");
  }
  scope_down(out);
  out.close();
}

void t_kotlin_generator::generate_service_args_helpers(t_service* tservice) {
  string f_service_args_name = package_dir_ + "/" + tservice->get_name() + "FunctionArgs.kt";
  ofstream_with_content_based_conditional_update out;
  out.open(f_service_args_name.c_str());
  out << autogen_comment() << warning_surpressions() << kotlin_package();
  generate_docstring_comment(out, "/**\n", " * ",
                             "function arguments for [" + tservice->get_name() + "]", " */\n");
  indent(out) << "sealed interface " << tservice->get_name() << "FunctionArgs {" << endl;
  indent_up();
  for (auto func : tservice->get_functions()) {
    t_struct* ts = func->get_arglist();
    generate_struct_definition(out, ts, false, tservice->get_name() + "FunctionArgs");
    out << endl;
  }
  scope_down(out);
  out.close();
}

void t_kotlin_generator::generate_service(t_service* tservice) {
  generate_service_interface(tservice);
  generate_service_client(tservice);
  generate_service_processor(tservice);
  generate_service_args_helpers(tservice);
  generate_service_result_helpers(tservice);
}

void t_kotlin_generator::generate_xception(t_struct* txception) {
  generate_kotlin_struct(txception, true);
}

/**
 * Converts the parse type to a Java enum string for the given type.
 */
string t_kotlin_generator::type_to_enum(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "NO T_VOID CONSTRUCT";
    case t_base_type::TYPE_STRING:
      return "org.apache.thrift.protocol.TType.STRING";
    case t_base_type::TYPE_BOOL:
      return "org.apache.thrift.protocol.TType.BOOL";
    case t_base_type::TYPE_I8:
      return "org.apache.thrift.protocol.TType.BYTE";
    case t_base_type::TYPE_I16:
      return "org.apache.thrift.protocol.TType.I16";
    case t_base_type::TYPE_I32:
      return "org.apache.thrift.protocol.TType.I32";
    case t_base_type::TYPE_I64:
      return "org.apache.thrift.protocol.TType.I64";
    case t_base_type::TYPE_DOUBLE:
      return "org.apache.thrift.protocol.TType.DOUBLE";
    }
  } else if (type->is_enum()) {
    return "org.apache.thrift.protocol.TType.I32";
  } else if (type->is_struct() || type->is_xception()) {
    return "org.apache.thrift.protocol.TType.STRUCT";
  } else if (type->is_map()) {
    return "org.apache.thrift.protocol.TType.MAP";
  } else if (type->is_set()) {
    return "org.apache.thrift.protocol.TType.SET";
  } else if (type->is_list()) {
    return "org.apache.thrift.protocol.TType.LIST";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}

string t_kotlin_generator::inner_enum_type_name(t_type* ttype) {
  ttype = get_true_type(ttype);
  if (ttype->is_map()) {
    t_map* tmap = (t_map*)ttype;
    t_type* key_type = get_true_type(tmap->get_key_type());
    return type_name(key_type, true) + ".class";
  } else if (ttype->is_set()) {
    t_set* tset = (t_set*)ttype;
    t_type* elem_type = get_true_type(tset->get_elem_type());
    return type_name(elem_type, true) + ".class";
  }
  return "";
}

bool t_kotlin_generator::is_enum_set(t_type* ttype) {
  ttype = get_true_type(ttype);
  if (ttype->is_set()) {
    t_set* tset = (t_set*)ttype;
    t_type* elem_type = get_true_type(tset->get_elem_type());
    return elem_type->is_enum();
  }
  return false;
}

bool t_kotlin_generator::is_enum_map(t_type* ttype) {
  ttype = get_true_type(ttype);
  if (ttype->is_map()) {
    t_map* tmap = (t_map*)ttype;
    t_type* key_type = get_true_type(tmap->get_key_type());
    return key_type->is_enum();
  }
  return false;
}

/**
 * Packages the generated file
 *
 * @return String of the package, i.e. "package org.apache.thriftdemo"
 */
string t_kotlin_generator::kotlin_package() {
  if (!package_name_.empty()) {
    return string("package ") + package_name_ + endl + endl;
  }
  return "";
}

string t_kotlin_generator::warning_surpressions() {
  return "@file:Suppress(\"ClassName\", \"PropertyName\", \"RedundantUnitReturnType\", "
         "\"NestedLambdaShadowedImplicitParameter\", "
         "\"RemoveRedundantQualifierName\")"
         + endl;
}

string t_kotlin_generator::constant_name(string name) {
  string constant_name;
  bool is_first = true;
  bool was_previous_char_upper = false;
  for (char character : name) {
    bool is_upper = isupper(character);
    if (is_upper && !is_first && !was_previous_char_upper) {
      constant_name += '_';
    }
    constant_name += toupper(character);
    is_first = false;
    was_previous_char_upper = is_upper;
  }
  return constant_name;
}

bool t_kotlin_generator::is_reserved(const string& name) {
  return KOTLIN_RESERVED_WORDS_SET.find(name) != KOTLIN_RESERVED_WORDS_SET.end();
}

string t_kotlin_generator::kotlin_safe_name(const string& name) {
  if (is_reserved(name)) {
    return "`" + name + "`";
  } else {
    return name;
  }
}

void t_kotlin_generator::generate_kdoc_comment(ostream& out, t_doc* tdoc) {
  if (tdoc->has_doc()) {
    generate_docstring_comment(out, "/**\n", " * ", tdoc->get_doc(), " */\n");
  }
}

THRIFT_REGISTER_GENERATOR(kotlin, "Kotlin", "")
