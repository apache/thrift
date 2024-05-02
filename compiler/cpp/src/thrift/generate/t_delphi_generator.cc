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

#include <cassert>

#include <string>
#include <fstream>
#include <iostream>
#include <vector>
#include <list>

#include <stdlib.h>
#include <sys/stat.h>
#include <sstream>
#include <cctype>

#include "thrift/platform.h"
#include "thrift/generate/t_oop_generator.h"

#ifdef _WIN32
#include <locale>
#include <codecvt>
#include <combaseapi.h>
#include <guiddef.h>
#endif

using std::map;
using std::set;
using std::ofstream;
using std::ostream;
using std::ostringstream;
using std::string;
using std::stringstream;
using std::vector;

class t_delphi_generator : public t_oop_generator {
public:
  t_delphi_generator(t_program* program,
                     const std::map<std::string, std::string>& parsed_options,
                     const std::string& option_string)
    : t_oop_generator(program) {
    (void)option_string;
    indent_impl_ = 0;
    has_forward = false;
    has_enum = false;
    has_const = false;
    std::map<std::string, std::string>::const_iterator iter;

    register_types_ = false;
    constprefix_ = false;
    old_names_ = false;
    events_ = false;
    xmldoc_ = false;
    async_ = false;
    com_types_ = false;
    rtti_ = false;
    for( iter = parsed_options.begin(); iter != parsed_options.end(); ++iter) {
      if( iter->first.compare("register_types") == 0) {
        register_types_ = true;
      } else if( iter->first.compare("old_names") == 0) {
        old_names_ = true;
      } else if( iter->first.compare("constprefix") == 0) {
        constprefix_ = true;
      } else if( iter->first.compare("events") == 0) {
        events_ = true;
      } else if( iter->first.compare("xmldoc") == 0) {
        xmldoc_ = true;
      } else if( iter->first.compare("async") == 0) {
        async_ = true;
      } else if( iter->first.compare("com_types") == 0) {
        com_types_ = true;
      } else if( iter->first.compare("rtti") == 0) {
        rtti_ = true;
      } else {
        throw "unknown option delphi:" + iter->first;
      }
    }

    out_dir_base_ = "gen-delphi";
    escape_.clear();
    escape_['\''] = "''";
  }

  void init_generator() override;
  void close_generator() override;
  std::string display_name() const override;

  void generate_consts(std::vector<t_const*> consts) override;

  void generate_typedef(t_typedef* ttypedef) override;
  void generate_enum(t_enum* tenum) override;
  void generate_forward_declaration(t_struct* tstruct) override;
  void generate_struct(t_struct* tstruct) override;
  void generate_xception(t_struct* txception) override;
  void generate_service(t_service* tservice) override;
  void generate_property(ostream& out, t_field* tfield, bool isPublic, bool is_xception);
  void generate_property_writer_(ostream& out, t_field* tfield, bool isPublic);

  void generate_delphi_property(ostream& out,
                                bool struct_is_exception,
                                t_field* tfield,
                                bool isPublic,
                                std::string fieldPrefix = "");
  void generate_delphi_isset_reader_writer_definition(ostream& out, t_field* tfield, bool is_xception);
  void generate_delphi_property_reader_definition(ostream& out,
                                                  t_field* tfield,
                                                  bool is_xception_class);
  void generate_delphi_property_writer_definition(ostream& out,
                                                  t_field* tfield,
                                                  bool is_xception_class);
  void generate_delphi_property_reader_impl(ostream& out,
                                            std::string cls_prefix,
                                            std::string name,
                                            t_type* type,
                                            t_field* tfield,
                                            std::string fieldPrefix,
                                            bool is_xception_class);
  void generate_delphi_property_writer_impl(ostream& out,
                                            std::string cls_prefix,
                                            std::string name,
                                            t_type* type,
                                            t_field* tfield,
                                            std::string fieldPrefix,
                                            bool is_xception_class,
                                            bool is_union,
                                            bool is_xception_factory,
                                            std::string xception_factory_name);
  void generate_delphi_clear_union_value(ostream& out,
                                         std::string cls_prefix,
                                         std::string name,
                                         t_type* type,
                                         t_field* tfield,
                                         std::string fieldPrefix,
                                         bool is_xception_class,
                                         bool is_union,
                                         bool is_xception_factory,
                                         std::string xception_factory_name);
  void generate_delphi_isset_reader_writer_impl(ostream& out,
                                         std::string cls_prefix,
                                         std::string name,
                                         t_type* type,
                                         t_field* tfield,
                                         std::string fieldPrefix,
                                         bool is_xception);
  void generate_delphi_struct_writer_impl(ostream& out,
                                          std::string cls_prefix,
                                          t_struct* tstruct,
                                          bool is_exception,
                                          bool is_x_factory);
  void generate_delphi_struct_result_writer_impl(ostream& out,
                                                 std::string cls_prefix,
                                                 t_struct* tstruct,
                                                 bool is_exception,
                                                 bool is_x_factory);

  void generate_delphi_struct_tostring_impl(ostream& out,
                                            std::string cls_prefix,
                                            t_struct* tstruct,
                                            bool is_exception,
                                            bool is_x_factory);

  void add_delphi_uses_list(string unitname);

  void generate_delphi_struct_reader_impl(ostream& out,
                                          std::string cls_prefix,
                                          t_struct* tstruct,
                                          bool is_exception,
                                          bool is_x_factory);
  void generate_delphi_create_exception_impl(ostream& out,
                                             string cls_prefix,
                                             t_struct* tstruct,
                                             bool is_exception);

  bool is_deprecated(std::map<std::string, std::vector<std::string>>& annotations);
  std::string render_deprecation_attribute(std::map<std::string, std::vector<std::string>>& annotations, std::string prefix, std::string postfix);

  bool const_needs_var(t_type* type);
  void print_const_prop(std::ostream& out, string name, t_type* type, t_const_value* value);
  void print_private_field(std::ostream& out, string name, t_type* type, t_const_value* value);
  void print_const_value(std::ostream& vars,
                         std::ostream& out,
                         std::string name,
                         t_type* type,
                         t_const_value* value);
  void initialize_field(std::ostream& vars,
                        std::ostream& out,
                        std::string name,
                        t_type* type,
                        t_const_value* value);
  void finalize_field(std::ostream& out,
                      std::string name,
                      t_type* type,
                      t_const_value* value,
                      std::string cls_nm = "");
  std::string render_const_value(std::ostream& local_vars,
                                 std::ostream& out,
                                 std::string name,
                                 t_type* type,
                                 t_const_value* value);
  void print_const_def_value(std::ostream& vars,
                             std::ostream& out,
                             std::string name,
                             t_type* type,
                             t_const_value* value,
                             std::string cls_nm = "");
  std::string make_constants_classname();

  void generate_delphi_struct(t_struct* tstruct, bool is_exception);
  void generate_delphi_struct_impl(ostream& out,
                                   std::string cls_prefix,
                                   t_struct* tstruct,
                                   bool is_exception,
                                   bool is_result = false,
                                   bool is_x_factory = false);
  void print_delphi_struct_type_factory_func(ostream& out, t_struct* tstruct);
  void generate_delphi_struct_type_factory(ostream& out,
                                           std::string cls_prefix,
                                           t_struct* tstruct,
                                           bool is_exception,
                                           bool is_result = false,
                                           bool is_x_factory = false);
  void generate_delphi_struct_type_factory_registration(ostream& out,
                                                        std::string cls_prefix,
                                                        t_struct* tstruct,
                                                        bool is_exception,
                                                        bool is_result = false,
                                                        bool is_x_factory = false);
  void generate_delphi_struct_definition(std::ostream& out,
                                         t_struct* tstruct,
                                         bool is_xception = false,
                                         bool in_class = false,
                                         bool is_result = false,
                                         bool is_x_factory = false);
  void generate_delphi_struct_reader(std::ostream& out, t_struct* tstruct);
  void generate_delphi_struct_result_writer(std::ostream& out, t_struct* tstruct);
  void generate_delphi_struct_writer(std::ostream& out, t_struct* tstruct);
  void generate_delphi_struct_tostring(std::ostream& out, t_struct* tstruct);

  void generate_function_helpers(t_function* tfunction);
  void generate_service_interface(t_service* tservice);
  void generate_service_interface(t_service* tservice, bool for_async);
  void generate_guid(std::ostream& out);
  void generate_service_helpers(t_service* tservice);
  void generate_service_client(t_service* tservice);
  void generate_service_server(t_service* tservice);
  void generate_process_function(t_service* tservice, t_function* function);

  void generate_deserialize_field(std::ostream& out,
                                  bool is_xception,
                                  t_field* tfield,
                                  std::string prefix,
                                  std::ostream& local_vars);
  void generate_deserialize_struct(std::ostream& out,
                                   t_struct* tstruct,
                                   std::string name,
                                   std::string prefix);
  void generate_deserialize_container(ostream& out,
                                      bool is_xception,
                                      t_type* ttype,
                                      string name,
                                      std::ostream& local_vars);

  void generate_deserialize_set_element(std::ostream& out,
                                        bool is_xception,
                                        t_set* tset,
                                        std::string prefix,
                                        std::ostream& local_vars);
  void generate_deserialize_map_element(std::ostream& out,
                                        bool is_xception,
                                        t_map* tmap,
                                        std::string prefix,
                                        std::ostream& local_vars);
  void generate_deserialize_list_element(std::ostream& out,
                                         bool is_xception,
                                         t_list* list,
                                         std::string prefix,
                                         std::ostream& local_vars);

  void generate_serialize_field(std::ostream& out,
                                bool is_xception,
                                t_field* tfield,
                                std::string prefix,
                                std::ostream& local_vars);
  void generate_serialize_struct(std::ostream& out,
                                 t_struct* tstruct,
                                 std::string prefix,
                                 std::ostream& local_vars);
  void generate_serialize_container(std::ostream& out,
                                    bool is_xception,
                                    t_type* ttype,
                                    std::string prefix,
                                    std::ostream& local_vars);
  void generate_serialize_map_element(std::ostream& out,
                                      bool is_xception,
                                      t_map* tmap,
                                      std::string iter,
                                      std::string map,
                                      std::ostream& local_vars);
  void generate_serialize_set_element(std::ostream& out,
                                      bool is_xception,
                                      t_set* tmap,
                                      std::string iter,
                                      std::ostream& local_vars);
  void generate_serialize_list_element(std::ostream& out,
                                       bool is_xception,
                                       t_list* tlist,
                                       std::string iter,
                                       std::ostream& local_vars);

  void delphi_type_usings(std::ostream& out);
  std::string delphi_thrift_usings();

  std::string type_name(t_type* ttype,
                        bool b_cls = false,
                        bool b_no_postfix = false,
                        bool b_exception_factory = false,
                        bool b_full_exception_factory = false);
  std::string normalize_clsnm(std::string name,
                              std::string prefix,
                              bool b_no_check_keyword = false);
  std::string make_valid_delphi_identifier(std::string const& fromName);
  std::string make_pascal_string_literal( std::string value);
  std::string input_arg_prefix(t_type* ttype);

  std::string base_type_name(t_base_type* tbase);
  std::string declare_field(t_field* tfield,
                            bool init = false,
                            std::string prefix = "",
                            bool is_xception_class = false);
  std::string function_signature(t_function* tfunction,
                                 bool for_async,
                                 std::string full_cls = "",
                                 bool is_xception = false);
  std::string argument_list(t_struct* tstruct);
  std::string constructor_argument_list(t_struct* tstruct, std::string current_indent);
  std::string type_to_enum(t_type* ttype);
  std::string prop_name(t_field* tfield, bool is_xception = false, std::string prefix = "");
  std::string prop_name(std::string name, bool is_xception = false, std::string prefix = "");
  std::string constructor_param_name(string name);

  void write_enum(std::string line);
  void write_forward_decr(std::string line);
  void write_const(std::string line);
  void write_struct(std::string line);
  void write_service(std::string line);

  std::string autogen_comment() override {
    return std::string("(**\n") + " * Autogenerated by Thrift Compiler (" + THRIFT_VERSION + ")\n"
           + " *\n" + " * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING\n"
           + " *)\n";
  }

  string replace_all(string contents, string search, string replace);
  string xml_encode(string contents);
  string xmldoc_encode(string contents);
  string xmlattrib_encode(string contents);
  void generate_delphi_doc(std::ostream& out, t_field* field);
  void generate_delphi_doc(std::ostream& out, t_doc* tdoc);
  void generate_delphi_doc(std::ostream& out, t_function* tdoc);
  void generate_delphi_docstring_comment(std::ostream& out, string contents);

  bool type_can_be_null(t_type* ttype) {
    while (ttype->is_typedef()) {
      ttype = ((t_typedef*)ttype)->get_type();
    }

    return ttype->is_container() || ttype->is_struct() || ttype->is_xception();
  }

private:
  std::string namespace_name_;
  std::ostringstream s_forward_decr;
  std::ostringstream s_enum;
  std::ostringstream s_const;
  std::ostringstream s_struct;
  std::ostringstream s_service;
  std::ostringstream s_const_impl;
  std::ostringstream s_struct_impl;
  std::ostringstream s_service_impl;
  std::ostringstream s_type_factory_registration;
  std::ostringstream s_type_factory_funcs;
  bool has_forward;
  bool has_enum;
  bool has_const;
  std::string namespace_dir_;
  std::set<std::string> types_known;
  std::list<t_typedef*> typedefs_pending;
  std::vector<std::string> uses_list;
  std::string empty_value(t_type* type);

  const std::string DELPHI_KEYWORDS[81] = {
    // keywords
    "and", "array", "as", "asm", "at", "automated", "begin", "case", "class", "const", "constructor",
    "destructor", "dispinterface", "div", "do", "downto", "else", "end", "except", "exports", "file",
    "finalization", "finally", "for", "function", "goto", "if", "implementation", "in", "inherited",
    "initialization", "inline", "interface", "is", "label", "library", "mod", "nil", "not", "object",
    "of", "on", "or", "out", "packed", "private", "procedure", "program", "property", "protected",
    "public", "published", "raise", "record", "repeat", "resourcestring", "set", "shl", "shr", "string",
    "then", "threadvar", "to", "try", "type", "unit", "until", "uses", "var", "while", "with", "xor",
    // predefined types (lowercase!)
    "ansistring", "boolean", "double", "int64", "integer", "shortint", "smallint", "string", "unicodestring"
  };

  // reserved variables and types (lowercase!)
  const std::string DELPHI_RESERVED_NAMES[8] = {
    "result", "system", "sysutils", "tbytes", "tclass", "thrift", "tinterfacedobject", "tobject"
  };

  // reserved method names (lowercase!)
  const std::string DELPHI_RESERVED_METHOD[31] = {
    "afterconstruction", "beforedestruction", "classinfo", "classname", "classnameis", "classparent",
    "classtype", "cleanupinstance", "create", "defaulthandler", "destroy", "dispatch", "equals",
    "fieldaddress", "free", "freeinstance", "gethashcode", "getinterface", "getinterfaceentry",
    "getinterfacetable", "inheritsfrom", "initinstance", "instancesize", "methodaddress", "methodname",
    "newinstance", "read", "safecallexception", "tostring", "unitname", "write"
  };

  // reserved exception class method names (lowercase!)
  const std::string DELPHI_RESERVED_METHOD_EXCEPTION[23] = {
    "setinnerexception", "setstackinfo", "getstacktrace", "raisingexception", "createfmt", "createres",
    "createresfmt", "createhelp", "createfmthelp", "createreshelp", "createresfmthelp", "getbaseexception",
    "baseexception", "helpcontext", "innerexception", "message", "stacktrace", "stackinfo",
    "getexceptionstackinfoproc", "getstackinfostringproc", "cleanupstackinfoproc", "raiseouterexception",
    "throwouterexception"
  };

  // TODO: put all into one map, value=flags tells what it is
  std::set<std::string> delphi_keywords                  = std::set<string>(DELPHI_KEYWORDS,                  DELPHI_KEYWORDS                  + sizeof(DELPHI_KEYWORDS)                  / sizeof(DELPHI_KEYWORDS[0]));
  std::set<std::string> delphi_reserved_names            = std::set<string>(DELPHI_RESERVED_NAMES,            DELPHI_RESERVED_NAMES            + sizeof(DELPHI_RESERVED_NAMES)            / sizeof(DELPHI_RESERVED_NAMES[0]));
  std::set<std::string> delphi_reserved_method           = std::set<string>(DELPHI_RESERVED_METHOD,           DELPHI_RESERVED_METHOD           + sizeof(DELPHI_RESERVED_METHOD)           / sizeof(DELPHI_RESERVED_METHOD[0]));
  std::set<std::string> delphi_reserved_method_exception = std::set<string>(DELPHI_RESERVED_METHOD_EXCEPTION, DELPHI_RESERVED_METHOD_EXCEPTION + sizeof(DELPHI_RESERVED_METHOD_EXCEPTION) / sizeof(DELPHI_RESERVED_METHOD_EXCEPTION[0]));

  bool find_keyword(std::set<std::string>& keywords, std::string name);
  std::string normalize_name(std::string name,
                             bool b_method = false,
                             bool b_exception_method = false,
                             bool b_force_underscore = false);

  bool is_fully_defined_type(t_type* ttype);
  void add_defined_type(t_type* ttype);
  void init_known_types_list();
  bool is_void(t_type* type);
  int indent_impl_;
  bool register_types_;
  bool constprefix_;
  bool old_names_;
  bool events_;
  bool xmldoc_;
  bool async_;
  bool com_types_;
  bool rtti_;
  void indent_up_impl() { ++indent_impl_; };
  void indent_down_impl() { --indent_impl_; };
  std::string indent_impl() {
    std::string ind = "";
    int i;
    for (i = 0; i < indent_impl_; ++i) {
      ind += "  ";
    }
    return ind;
  };
  std::ostream& indent_impl(std::ostream& os) { return os << indent_impl(); };
};

string t_delphi_generator::replace_all(string contents, string search, string repl) {
  string str(contents);

  size_t slen = search.length();
  size_t rlen = repl.length();
  size_t incr = (rlen > 0) ? rlen : 1;

  if (slen > 0) {
    size_t found = str.find(search);
    while ((found != string::npos) && (found < str.length())) {
      str.replace(found, slen, repl);
      found = str.find(search, found + incr);
    }
  }

  return str;
}

// XML encoding
string t_delphi_generator::xml_encode(string contents) {
  string str(contents);

  // escape the escape
  str = replace_all(str, "&", "&amp;");

  // other standard XML entities
  str = replace_all(str, "<", "&lt;");
  str = replace_all(str, ">", "&gt;");

  return str;
}

// XML attribute encoding
string t_delphi_generator::xmlattrib_encode(string contents) {
  string str(xml_encode(contents));

  // our attribs are enclosed in "
  str = replace_all(str, "\"", "\\\"");

  return str;
}

// XML encoding for doc comments
string t_delphi_generator::xmldoc_encode(string contents) {
  string str(xml_encode(contents));

  // XMLDoc specific: convert linebreaks into <para>graphs</para>
  str = replace_all(str, "\r\n", "\r");
  str = replace_all(str, "\n", "\r");
  str = replace_all(str, "\r", "</para>\n<para>");

  return str;
}

void t_delphi_generator::generate_delphi_docstring_comment(ostream& out, string contents) {
  if (xmldoc_) {
    generate_docstring_comment(out,
                               "{$REGION 'XMLDoc'}/// <summary>\n",
                               "/// ",
                               "<para>" + contents + "</para>",
                               "/// </summary>\n{$ENDREGION}\n");
  }
}

void t_delphi_generator::generate_delphi_doc(ostream& out, t_field* field) {
  if (xmldoc_) {
    if (field->get_type()->is_enum()) {
      string combined_message = xmldoc_encode(field->get_doc()) + "\n<seealso cref=\""
                                + xmldoc_encode(type_name(field->get_type())) + "\"/>";
      generate_delphi_docstring_comment(out, combined_message);
    } else {
      generate_delphi_doc(out, (t_doc*)field);
    }
  }
}

void t_delphi_generator::generate_delphi_doc(ostream& out, t_doc* tdoc) {
  if (tdoc->has_doc() && xmldoc_) {
    generate_delphi_docstring_comment(out, xmldoc_encode(tdoc->get_doc()));
  }
}

void t_delphi_generator::generate_delphi_doc(ostream& out, t_function* tfunction) {
  if (tfunction->has_doc() && xmldoc_) {
    stringstream ps;
    const vector<t_field*>& fields = tfunction->get_arglist()->get_members();
    vector<t_field*>::const_iterator p_iter;
    for (p_iter = fields.begin(); p_iter != fields.end(); ++p_iter) {
      t_field* p = *p_iter;
      ps << "\n<param name=\"" << xmlattrib_encode(p->get_name()) << "\">";
      if (p->has_doc()) {
        std::string str = p->get_doc();
        str.erase(std::remove(str.begin(), str.end(), '\n'),
                  str.end()); // remove the newlines that appear from the parser
        ps << xmldoc_encode(str);
      }
      ps << "</param>";
    }
    generate_docstring_comment(out,
                               "{$REGION 'XMLDoc'}",
                               "/// ",
                               "<summary><para>" + xmldoc_encode(tfunction->get_doc())
                               + "</para></summary>" + ps.str(),
                               "{$ENDREGION}\n");
  }
}

bool t_delphi_generator::find_keyword(std::set<std::string>& keywords, std::string name) {
  std::string::size_type len = name.length();

  if (len <= 0) {
    return false;
  }

  std::string::size_type nlast = name.find_last_of('_');

  if (nlast >= 1) {
    if (nlast == (len - 1)) {
      string new_name(name, 0, nlast);
      return find_keyword(keywords, new_name);
    }
  }

  return (keywords.find(name) != keywords.end());
}

std::string t_delphi_generator::normalize_name(std::string name,
                                               bool b_method,
                                               bool b_exception_method,
                                               bool b_force_underscore) {
  string tmp(name);
  std::transform(tmp.begin(), tmp.end(), tmp.begin(), static_cast<int (*)(int)>(std::tolower));

  bool b_reserved = false;
  bool b_keyword = false;

  if (find_keyword(delphi_keywords, tmp)) {
    b_keyword = true;
  } else if (find_keyword(delphi_reserved_names, tmp)) {
    b_reserved = true;
  } else if (b_method && find_keyword(delphi_reserved_method, tmp)) {
    b_reserved = true;
  } else if (b_exception_method && find_keyword(delphi_reserved_method_exception, tmp)) {
    b_reserved = true;
  }

  // neither reserved nor keyword?
  if (!(b_reserved || b_keyword)) {
    return name;
  }

  // apply the rule: old style '_' postfix or more modern '&' prefix?
  // underscore always on non-keywords or when explicitly asked via arg
  if( (!b_keyword) || old_names_ || b_force_underscore) {
    return name + "_";
  } else {
    return "&" + name;
  }
}

void t_delphi_generator::add_delphi_uses_list(string unitname) {
  vector<std::string>::const_iterator s_iter;
  bool found = false;
  for (s_iter = uses_list.begin(); s_iter != uses_list.end(); ++s_iter) {
    if ((*s_iter) == unitname) {
      found = true;
      break;
    }
  }
  if (!found) {
    uses_list.push_back(unitname);
  }
}

void t_delphi_generator::init_generator() {
  indent_impl_ = 0;
  namespace_name_ = program_->get_namespace("delphi");
  has_forward = false;
  has_enum = false;
  has_const = false;

  add_delphi_uses_list("Classes");
  add_delphi_uses_list("SysUtils");
  add_delphi_uses_list("Generics.Collections");
  if(async_) {
    add_delphi_uses_list("System.Threading");
  }

  add_delphi_uses_list("Thrift");
  add_delphi_uses_list("Thrift.Utils");
  add_delphi_uses_list("Thrift.Collections");
  add_delphi_uses_list("Thrift.Protocol");
  add_delphi_uses_list("Thrift.Transport");
  if (register_types_) {
    add_delphi_uses_list("Thrift.TypeRegistry");
  }

  init_known_types_list();

  string unitname, nsname;
  const vector<t_program*>& includes = program_->get_includes();
  for (auto include : includes) {
    unitname = include->get_name();
    nsname = include->get_namespace("delphi");
    if ("" != nsname) {
      unitname = normalize_name(nsname,false,false,true/*force underscore*/);
    }
    add_delphi_uses_list(unitname);
  }

  MKDIR(get_out_dir().c_str());
}

void t_delphi_generator::close_generator() {
  std::string unitname = program_name_;
  if ("" != namespace_name_) {
    unitname = namespace_name_;
  }

  for (int i = 0; i < (int)unitname.size(); i++) {
    if (unitname[i] == ' ') {
      unitname.replace(i, 1, "_");
    }
  }

  unitname = normalize_name(unitname,false,false,true/*force underscore*/);

  std::string f_name = get_out_dir() + "/" + unitname + ".pas";
  ofstream_with_content_based_conditional_update f_all;

  f_all.open(f_name);

  f_all << autogen_comment() << '\n';
  generate_delphi_doc(f_all, program_);
  f_all << "unit " << unitname << ";" << '\n' << '\n';
  f_all << "{$WARN SYMBOL_DEPRECATED OFF}" << '\n';
  if(com_types_) {
    f_all << "{$MINENUMSIZE 4}" << '\n';
  }
  if(rtti_) {
    f_all << "{$IFOPT M+} {$DEFINE TYPEINFO_WAS_ON} {$ELSE} {$UNDEF TYPEINFO_WAS_ON} {$ENDIF}" << '\n';
  }
  f_all << '\n';
  f_all << "interface" << '\n' << '\n';
  f_all << "uses" << '\n';

  indent_up();

  vector<std::string>::const_iterator s_iter;
  for (s_iter = uses_list.begin(); s_iter != uses_list.end(); ++s_iter) {
    if (s_iter != uses_list.begin()) {
      f_all << ",";
      f_all << '\n';
    }
    indent(f_all) << *s_iter;
  }

  f_all << ";" << '\n' << '\n';

  indent_down();

  string tmp_unit(unitname);
  for (int i = 0; i < (int)tmp_unit.size(); i++) {
    if (tmp_unit[i] == '.') {
      tmp_unit.replace(i, 1, "_");
    }
  }

  f_all << "const" << '\n';
  indent_up();
  indent(f_all) << "c" << tmp_unit << "_Option_Register_Types = " << (register_types_ ? "True" : "False") << ";" << '\n';
  indent(f_all) << "c" << tmp_unit << "_Option_ConstPrefix    = " << (constprefix_ ? "True" : "False") << ";" << '\n';
  indent(f_all) << "c" << tmp_unit << "_Option_Events         = " << (events_ ? "True" : "False") << ";" << '\n';
  indent(f_all) << "c" << tmp_unit << "_Option_XmlDoc         = " << (xmldoc_ ? "True" : "False") << ";" << '\n';
  indent(f_all) << "c" << tmp_unit << "_Option_Async          = " << (async_ ? "True" : "False") << ";" << '\n';
  indent(f_all) << "c" << tmp_unit << "_Option_COM_types      = " << (com_types_ ? "True" : "False") << ";" << '\n';
  indent(f_all) << "c" << tmp_unit << "_Option_Old_Names      = " << (old_names_ ? "True" : "False") << ";" << '\n';
  indent(f_all) << "c" << tmp_unit << "_Option_RTTI           = " << (rtti_ ? "True" : "False") << ";" << '\n';
  indent_down();

  f_all << '\n';
  f_all << "type" << '\n';
  if (has_forward) {
    f_all << s_forward_decr.str() << '\n';
  }
  if (has_enum) {
    indent(f_all) << '\n';
    indent(f_all) << "{$SCOPEDENUMS ON}" << '\n' << '\n';
    f_all << s_enum.str();
    indent(f_all) << "{$SCOPEDENUMS OFF}" << '\n' << '\n';
  }
  f_all << s_struct.str();
  f_all << s_service.str();
  f_all << s_const.str();
  f_all << "implementation" << '\n' << '\n';
  f_all << s_struct_impl.str();
  f_all << s_service_impl.str();
  f_all << s_const_impl.str();

  if (register_types_) {
    f_all << '\n';
    f_all << "// Type factory methods and registration" << '\n';
    f_all << s_type_factory_funcs.str();
    f_all << "procedure RegisterTypeFactories;" << '\n';
    f_all << "begin" << '\n';
    f_all << s_type_factory_registration.str();
    f_all << "end;" << '\n';
  }
  f_all << '\n';

  string constants_class = make_constants_classname();

  f_all << "initialization" << '\n';
  if (has_const) {
    f_all << "{$IF CompilerVersion < 21.0}  // D2010" << '\n';
    f_all << "  " << constants_class.c_str() << "_Initialize;" << '\n';
    f_all << "{$IFEND}" << '\n';
  }
  if (register_types_) {
    f_all << "  RegisterTypeFactories;" << '\n';
  }
  f_all << '\n';

  f_all << "finalization" << '\n';
  if (has_const) {
    f_all << "{$IF CompilerVersion < 21.0}  // D2010" << '\n';
    f_all << "  " << constants_class.c_str() << "_Finalize;" << '\n';
    f_all << "{$IFEND}" << '\n';
  }
  f_all << '\n' << '\n';

  f_all << "end." << '\n';
  f_all.close();

  if (!typedefs_pending.empty()) {
    pwarning(0, "%d typedefs with unresolved type references left:\n", typedefs_pending.size());
    for (std::list<t_typedef*>::iterator iter = typedefs_pending.begin();
         typedefs_pending.end() != iter;
         ++iter) {
      pwarning(0, "- %s\n", (*iter)->get_symbolic().c_str());
    }
  }
}

void t_delphi_generator::delphi_type_usings(ostream& out) {
  indent_up();
  indent(out) << "Classes, SysUtils, Generics.Collections, Thrift.Collections, Thrift.Protocol,"
              << '\n';
  indent(out) << "Thrift.Transport;" << '\n' << '\n';
  indent_down();
}

void t_delphi_generator::generate_forward_declaration(t_struct* tstruct) {
  // Forward declare struct def
  has_forward = true;
  pdebug("forward declaration of %s\n", type_name(tstruct).c_str());

  string what = tstruct->is_xception() ? "class" : "interface";

  indent_up();
  indent(s_forward_decr) << type_name(tstruct, tstruct->is_xception(), true) << " = " << what << ";"
                         << '\n';
  indent_down();

  add_defined_type(tstruct);
}

void t_delphi_generator::generate_typedef(t_typedef* ttypedef) {
  t_type* type = ttypedef->get_type();

  // write now or save for later?
  if (!is_fully_defined_type(type)) {
    pverbose("typedef %s: unresolved dependencies found\n", type_name(ttypedef).c_str());
    typedefs_pending.push_back(ttypedef);
    return;
  }

  indent_up();
  generate_delphi_doc(s_struct, ttypedef);
  indent(s_struct) << type_name(ttypedef) << " = ";

  // commented out: the benefit is not big enough to risk breaking existing code
  // bool container = type->is_list() || type->is_map() || type->is_set();
  // if( ! container)
  //  s_struct << "type ";  //the "type A = type B" syntax leads to E2574 with generics

  s_struct << type_name(ttypedef->get_type()) << ";" << '\n' << '\n';
  indent_down();

  add_defined_type(ttypedef);
}

bool t_delphi_generator::is_fully_defined_type(t_type* ttype) {
  if ((nullptr != ttype->get_program()) && (ttype->get_program() != program_)) {
    t_scope* scope = ttype->get_program()->scope();
    if (nullptr != scope->get_type(ttype->get_name())) {
      // printf("type %s found in included scope %s\n", ttype->get_name().c_str(),
      // ttype->get_program()->get_name().c_str());
      return true;
    }
  }

  if (ttype->is_typedef()) {
    return (types_known.find(type_name(ttype)) != types_known.end());
  }

  if (ttype->is_base_type()) {
    return (types_known.find(base_type_name((t_base_type*)ttype)) != types_known.end());
  } else if (ttype->is_enum()) {
    return true; // enums are written first, before all other types
  } else if (ttype->is_map()) {
    t_map* tmap = (t_map*)ttype;
    return is_fully_defined_type(tmap->get_key_type())
           && is_fully_defined_type(tmap->get_val_type());
  } else if (ttype->is_set()) {
    t_set* tset = (t_set*)ttype;
    return is_fully_defined_type(tset->get_elem_type());
  } else if (ttype->is_list()) {
    t_list* tlist = (t_list*)ttype;
    return is_fully_defined_type(tlist->get_elem_type());
  }

  return (types_known.find(type_name(ttype)) != types_known.end());
}

void t_delphi_generator::add_defined_type(t_type* ttype) {
  // mark as known type
  types_known.insert(type_name(ttype));

  // check all pending typedefs
  std::list<t_typedef*>::iterator iter;
  bool more = true;
  while (more && (!typedefs_pending.empty())) {
    more = false;

    for (iter = typedefs_pending.begin(); typedefs_pending.end() != iter; ++iter) {
      t_typedef* ttypedef = (*iter);
      if (is_fully_defined_type(ttypedef->get_type())) {
        pverbose("typedef %s: all pending references are now resolved\n",
                 type_name(ttypedef).c_str());
        typedefs_pending.erase(iter);
        generate_typedef(ttypedef);
        more = true;
        break;
      }
    }
  }
}

void t_delphi_generator::init_known_types_list() {
  // known base types
  types_known.insert( type_name(g_type_string));
  types_known.insert( type_name(g_type_binary));
  types_known.insert( type_name(g_type_uuid));
  types_known.insert( type_name(g_type_bool));
  types_known.insert( type_name(g_type_i8));
  types_known.insert( type_name(g_type_i16));
  types_known.insert( type_name(g_type_i32));
  types_known.insert( type_name(g_type_i64));
  types_known.insert( type_name(g_type_double));
}

void t_delphi_generator::generate_enum(t_enum* tenum) {
  has_enum = true;
  indent_up();
  generate_delphi_doc(s_enum, tenum);
  indent(s_enum) << type_name(tenum, true, true) << " = "
                 << "(" << '\n';
  indent_up();
  vector<t_enum_value*> constants = tenum->get_constants();
  if (constants.empty()) {
    indent(s_enum) << "dummy = 0  // empty enums are not allowed";
  } else {
    vector<t_enum_value*>::iterator c_iter;
    for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
      int value = (*c_iter)->get_value();
      if (c_iter != constants.begin()) {
        s_enum << ",";
        s_enum << '\n';
      }
      generate_delphi_doc(s_enum, *c_iter);
      indent(s_enum) << normalize_name((*c_iter)->get_name()) << " = " << value;
      s_enum << render_deprecation_attribute((*c_iter)->annotations_, " {", "}");
    }
  }
  s_enum << '\n';
  indent_down();
  indent(s_enum) << ")" << render_deprecation_attribute(tenum->annotations_, " ", "") << ";" << '\n' << '\n';
  indent_down();
}

std::string t_delphi_generator::make_pascal_string_literal(std::string value) {
  std::stringstream result;

  if (value.length() == 0) {
    return "";
  }

  result << "'";
  for (signed char const c: value) {
    if( (c >= 0) && (c < 32)) {  // convert ctrl chars, but leave UTF-8 alone
      result << "#" << (int)c;
    } else if (c == '\'') {
      result << "''";   // duplicate any single quotes we find
    } else {
      result << c;   // anything else "as is"
    }
  }
  result << "'";

  return result.str();
}

std::string t_delphi_generator::make_valid_delphi_identifier(std::string const& fromName) {
  std::string str = fromName;
  if (str.empty()) {
    return str;
  }

  // tests rely on this
  assert(('A' < 'Z') && ('a' < 'z') && ('0' < '9'));

  // if the first letter is a number, we add an additional underscore in front of it
  char c = str.at(0);
  if (('0' <= c) && (c <= '9')) {
    str = "_" + str;
  }

  // following chars: letter, number or underscore
  for (size_t i = 0; i < str.size(); ++i) {
    c = str.at(i);
    if ((('A' > c) || (c > 'Z')) && (('a' > c) || (c > 'z')) && (('0' > c) || (c > '9'))
        && ('_' != c)) {
      str.replace(i, 1, "_");
    }
  }

  return str;
}

std::string t_delphi_generator::make_constants_classname() {
  if (constprefix_) {
    return make_valid_delphi_identifier("T" + program_name_ + "Constants");
  } else {
    return "TConstants"; // compatibility
  }
}

void t_delphi_generator::generate_consts(std::vector<t_const*> consts) {
  if (consts.empty()) {
    return;
  }

  has_const = true;
  string constants_class = make_constants_classname();

  indent_up();
  indent(s_const) << constants_class.c_str() << " = class" << '\n';
  indent(s_const) << "private" << '\n';
  indent_up();
  vector<t_const*>::iterator c_iter;
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    if (const_needs_var((*c_iter)->get_type())) {
      print_private_field(s_const,
                          normalize_name((*c_iter)->get_name()),
                          (*c_iter)->get_type(),
                          (*c_iter)->get_value());
    }
  }
  indent_down();
  indent(s_const) << "public" << '\n';
  indent_up();
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    generate_delphi_doc(s_const, *c_iter);
    print_const_prop(s_const,
                     normalize_name((*c_iter)->get_name()),
                     (*c_iter)->get_type(),
                     (*c_iter)->get_value());
  }
  indent(s_const) << "{$IF CompilerVersion >= 21.0}" << '\n';
  indent(s_const) << "class constructor Create;" << '\n';
  indent(s_const) << "class destructor Destroy;" << '\n';
  indent(s_const) << "{$IFEND}" << '\n';
  indent_down();
  indent(s_const) << "end;" << '\n' << '\n';
  indent_down();

  std::ostringstream vars, code;

  indent_up_impl();
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    initialize_field(vars,
                     code,
                     prop_name((*c_iter)->get_name(), false, "F"),
                     (*c_iter)->get_type(),
                     (*c_iter)->get_value());
  }
  indent_down_impl();

  indent_impl(s_const_impl) << "{$IF CompilerVersion >= 21.0}" << '\n';
  indent_impl(s_const_impl) << "class constructor " << constants_class.c_str() << ".Create;"
                            << '\n';

  if (!vars.str().empty()) {
    indent_impl(s_const_impl) << "var" << '\n';
    s_const_impl << vars.str();
  }
  indent_impl(s_const_impl) << "begin" << '\n';
  if (!code.str().empty()) {
    s_const_impl << code.str();
  }
  indent_impl(s_const_impl) << "end;" << '\n' << '\n';
  indent_impl(s_const_impl) << "class destructor " << constants_class.c_str() << ".Destroy;"
                            << '\n';
  indent_impl(s_const_impl) << "begin" << '\n';
  indent_up_impl();
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    if (const_needs_var((*c_iter)->get_type())) {
      finalize_field(s_const_impl,
                     normalize_name((*c_iter)->get_name()),
                     (*c_iter)->get_type(),
                     (*c_iter)->get_value());
    }
  }
  indent_impl(s_const_impl) << "inherited;" << '\n';
  indent_down_impl();
  indent_impl(s_const_impl) << "end;" << '\n';
  indent_impl(s_const_impl) << "{$ELSE}" << '\n';

  vars.str("");
  code.str("");

  indent_up_impl();
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    if (const_needs_var((*c_iter)->get_type())) {
      initialize_field(vars,
                       code,
                       constants_class + "." + prop_name((*c_iter)->get_name(), false, "F"),
                       (*c_iter)->get_type(),
                       (*c_iter)->get_value());
    }
  }
  indent_down_impl();

  indent_impl(s_const_impl) << "procedure " << constants_class.c_str() << "_Initialize;" << '\n';
  if (!vars.str().empty()) {
    indent_impl(s_const_impl) << "var" << '\n';
    s_const_impl << vars.str();
  }
  indent_impl(s_const_impl) << "begin" << '\n';
  if (!code.str().empty()) {
    s_const_impl << code.str();
  }
  indent_impl(s_const_impl) << "end;" << '\n' << '\n';

  indent_impl(s_const_impl) << "procedure " << constants_class.c_str() << "_Finalize;" << '\n';
  indent_impl(s_const_impl) << "begin" << '\n';
  indent_up_impl();
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    finalize_field(s_const_impl,
                   normalize_name((*c_iter)->get_name()),
                   (*c_iter)->get_type(),
                   (*c_iter)->get_value(),
                   constants_class);
  }
  indent_down_impl();
  indent_impl(s_const_impl) << "end;" << '\n';
  indent_impl(s_const_impl) << "{$IFEND}" << '\n' << '\n';
}

void t_delphi_generator::print_const_def_value(std::ostream& vars,
                                               std::ostream& out,
                                               string name,
                                               t_type* type,
                                               t_const_value* value,
                                               string cls_nm) {

  string cls_prefix;

  if (cls_nm == "") {
    cls_prefix = "";
  } else {
    cls_prefix = cls_nm + ".";
  }

  if (type->is_struct() || type->is_xception()) {
    const vector<t_field*>& fields = ((t_struct*)type)->get_members();
    vector<t_field*>::const_iterator f_iter;
    const map<t_const_value*, t_const_value*, t_const_value::value_compare>& val = value->get_map();
    map<t_const_value*, t_const_value*, t_const_value::value_compare>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      t_type* field_type = nullptr;
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        if ((*f_iter)->get_name() == v_iter->first->get_string()) {
          field_type = (*f_iter)->get_type();
        }
      }
      if (field_type == nullptr) {
        throw "type error: " + type->get_name() + " has no field " + v_iter->first->get_string();
      }
      string val = render_const_value(vars, out, name, field_type, v_iter->second);
      indent_impl(out) << cls_prefix << normalize_name(name) << "."
                       << prop_name(v_iter->first->get_string(), type->is_xception())
                       << " := " << val << ";" << '\n';
    }
  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();
    const map<t_const_value*, t_const_value*, t_const_value::value_compare>& val = value->get_map();
    map<t_const_value*, t_const_value*, t_const_value::value_compare>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string key = render_const_value(vars, out, name, ktype, v_iter->first);
      string val = render_const_value(vars, out, name, vtype, v_iter->second);
      indent_impl(out) << cls_prefix << normalize_name(name) << "[" << key << "]"
                       << " := " << val << ";" << '\n';
    }
  } else if (type->is_list() || type->is_set()) {
    t_type* etype;
    if (type->is_list()) {
      etype = ((t_list*)type)->get_elem_type();
    } else {
      etype = ((t_set*)type)->get_elem_type();
    }

    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string val = render_const_value(vars, out, name, etype, *v_iter);
      indent_impl(out) << cls_prefix << normalize_name(name) << ".Add(" << val << ");" << '\n';
    }
  }
}

void t_delphi_generator::print_private_field(std::ostream& out,
                                             string name,
                                             t_type* type,
                                             t_const_value* value) {
  (void)value;
  indent(out) << "class var F" << name << ": " << type_name(type) << ";" << '\n';
}

bool t_delphi_generator::const_needs_var(t_type* type) {
  t_type* truetype = type;
  while (truetype->is_typedef()) {
    truetype = ((t_typedef*)truetype)->get_type();
  }
  return (!truetype->is_base_type());
}

void t_delphi_generator::print_const_prop(std::ostream& out,
                                          string name,
                                          t_type* type,
                                          t_const_value* value) {
  (void)value;
  if (const_needs_var(type)) {
    indent(out) << "class property " << name << ": " << type_name(type) << " read F" << name << ";"
                << '\n';
  } else {
    std::ostringstream vars; // dummy
    string v2 = render_const_value(vars, out, name, type, value);
    indent(out) << "const " << name << " = " << v2 << ";" << '\n';
  }
}

void t_delphi_generator::print_const_value(std::ostream& vars,
                                           std::ostream& out,
                                           string name,
                                           t_type* type,
                                           t_const_value* value) {
  t_type* truetype = type;
  while (truetype->is_typedef()) {
    truetype = ((t_typedef*)truetype)->get_type();
  }

  if (truetype->is_base_type()) {
    // already done
    // string v2 = render_const_value( vars, out, name, type, value);
    // indent_impl(out) << name << " := " << v2 << ";" << '\n';
  } else if (truetype->is_enum()) {
    indent_impl(out) << name << " := " << type_name(type) << "." << value->get_identifier_name()
                     << ";" << '\n';
  } else {
    string typname;
    typname = type_name(truetype, true, false, type->is_xception(), type->is_xception());
    indent_impl(out) << name << " := " << typname << ".Create;" << '\n';
    print_const_def_value(vars, out, name, truetype, value);
  }
}

void t_delphi_generator::initialize_field(std::ostream& vars,
                                          std::ostream& out,
                                          string name,
                                          t_type* type,
                                          t_const_value* value) {
  print_const_value(vars, out, name, type, value);
}

void t_delphi_generator::finalize_field(std::ostream& out,
                                        string name,
                                        t_type* type,
                                        t_const_value* value,
                                        string cls_nm) {
  (void)out;
  (void)name;
  (void)type;
  (void)value;
  (void)cls_nm;
}

string t_delphi_generator::render_const_value(ostream& vars,
                                              ostream& out,
                                              string name,
                                              t_type* type,
                                              t_const_value* value) {
  (void)name;

  t_type* truetype = type;
  while (truetype->is_typedef()) {
    truetype = ((t_typedef*)truetype)->get_type();
  }

  std::ostringstream render;

  if (truetype->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)truetype)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_STRING:
      render << "'" << get_escaped_string(value) << "'";
      break;
    case t_base_type::TYPE_UUID:
      render << "['{" << value->get_uuid() << "}']";
      break;
    case t_base_type::TYPE_BOOL:
      render << ((value->get_integer() > 0) ? "True" : "False");
      break;
    case t_base_type::TYPE_I8:
      render << "ShortInt( " << value->get_integer() << ")";
      break;
    case t_base_type::TYPE_I16:
      render << "SmallInt( " << value->get_integer() << ")";
      break;
    case t_base_type::TYPE_I32:
      render << "LongInt( " << value->get_integer() << ")";
      break;
    case t_base_type::TYPE_I64:
      render << "Int64( " << value->get_integer() << ")";
      break;
    case t_base_type::TYPE_DOUBLE:
      if (value->get_type() == t_const_value::CV_INTEGER) {
        render << value->get_integer() << ".0"; // make it a double constant by adding ".0"
      } else {
        render << value->get_double();
      }
      break;
    default:
      throw "compiler error: no const of base type " + t_base_type::t_base_name(tbase);
    }
  } else if (truetype->is_enum()) {
    render << type_name(type, false) << "." << value->get_identifier_name();
  } else {
    string t = tmp("tmp");
    vars << "  " << t << " : " << type_name(type) << ";" << '\n';
    print_const_value(vars, out, t, type, value);
    render << t;
  }

  return render.str();
}

void t_delphi_generator::generate_struct(t_struct* tstruct) {
  generate_delphi_struct(tstruct, false);
}

void t_delphi_generator::generate_xception(t_struct* txception) {
  generate_delphi_struct(txception, true);
}

void t_delphi_generator::generate_delphi_struct(t_struct* tstruct, bool is_exception) {
  indent_up();
  generate_delphi_struct_definition(s_struct, tstruct, is_exception);
  indent_down();

  add_defined_type(tstruct);

  generate_delphi_struct_impl(s_struct_impl, "", tstruct, is_exception);
  if (register_types_) {
    generate_delphi_struct_type_factory(s_type_factory_funcs, "", tstruct, is_exception);
    generate_delphi_struct_type_factory_registration(s_type_factory_registration,
                                                     "",
                                                     tstruct,
                                                     is_exception);
  }
}

void t_delphi_generator::generate_delphi_struct_impl(ostream& out,
                                                     string cls_prefix,
                                                     t_struct* tstruct,
                                                     bool is_exception,
                                                     bool is_result,
                                                     bool is_x_factory) {

  if (is_exception && (!is_x_factory)) {
    generate_delphi_struct_impl(out, cls_prefix, tstruct, is_exception, is_result, true);
  }

  string cls_nm;

  string exception_factory_name;

  if (is_exception) {
    exception_factory_name = normalize_clsnm(tstruct->get_name(), "", true) + "Factory";
  }

  if (is_exception) {
    cls_nm = type_name(tstruct, true, (!is_x_factory), is_x_factory, true);
  } else {
    cls_nm = type_name(tstruct, true, false);
  }

  std::ostringstream vars, code;

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  indent_up_impl();
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* t = (*m_iter)->get_type();
    while (t->is_typedef()) {
      t = ((t_typedef*)t)->get_type();
    }
    if ((*m_iter)->get_value() != nullptr) {
      initialize_field(vars,
                       code,
                       prop_name((*m_iter)->get_name(), is_exception, ""),
                       t,
                       (*m_iter)->get_value());
      if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
        indent_impl(code) << prop_name((*m_iter), is_exception, "F__isset_") << " := True;"
                          << '\n';
      }
    }
  }
  indent_down_impl();

  indent_impl(out) << "constructor " << cls_prefix << cls_nm << "."
                   << "Create;" << '\n';

  if (!vars.str().empty()) {
    out << "var" << '\n';
    out << vars.str();
  }

  indent_impl(out) << "begin" << '\n';
  indent_up_impl();
  if (is_exception && (!is_x_factory)) {
    indent_impl(out) << "inherited Create('');" << '\n';
  } else {
    indent_impl(out) << "inherited;" << '\n';
  }

  if (!code.str().empty()) {
    out << code.str();
  }

  indent_down_impl();
  indent_impl(out) << "end;" << '\n' << '\n';

  if ((members.size() > 0) && is_exception && (!is_x_factory)) {
    indent_impl(out) << "constructor " << cls_prefix << cls_nm << "."
                     << "Create(" << constructor_argument_list(tstruct, indent_impl()) << ");"
                     << '\n';
    indent_impl(out) << "begin" << '\n';
    indent_up_impl();
    indent_impl(out) << "Create;" << '\n';
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      string propname = prop_name((*m_iter)->get_name(), is_exception);
      string param_name = constructor_param_name((*m_iter)->get_name());
      indent_impl(out) << propname << " := " << param_name << ";" << '\n';
    }
    indent_impl(out) << "UpdateMessageProperty;" << '\n';
    indent_down_impl();
    indent_impl(out) << "end;" << '\n' << '\n';
  }

  indent_impl(out) << "destructor " << cls_prefix << cls_nm << "."
                   << "Destroy;" << '\n';
  indent_impl(out) << "begin" << '\n';
  indent_up_impl();

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* t = (*m_iter)->get_type();
    while (t->is_typedef()) {
      t = ((t_typedef*)t)->get_type();
    }
    finalize_field(out, prop_name(*m_iter, is_exception), t, (*m_iter)->get_value());
  }

  indent_impl(out) << "inherited;" << '\n';
  indent_down_impl();
  indent_impl(out) << "end;" << '\n' << '\n';

  if (is_exception && (!is_x_factory)) {
    indent_impl(out) << "function " << cls_prefix << cls_nm << "." << exception_factory_name
                     << ": I" << exception_factory_name << ";" << '\n';
    indent_impl(out) << "begin" << '\n';
    indent_up_impl();
    indent_impl(out) << "if F" << exception_factory_name << " = nil" << '\n';
    indent_impl(out) << "then F" << exception_factory_name << " := T" << exception_factory_name << "Impl.Create;" << '\n' << '\n';
    indent_impl(out) << "result := F" << exception_factory_name << ";" << '\n';
    indent_down_impl();
    indent_impl(out) << "end;" << '\n' << '\n';
    indent_impl(out) << "function " << cls_prefix << cls_nm << ".QueryInterface(const IID: TGUID; out Obj): HRESULT;" << '\n';
    indent_impl(out) << "begin" << '\n';
    indent_up_impl();
    indent_impl(out) << "if GetInterface(IID, Obj)" << '\n';
    indent_impl(out) << "then result := S_OK" << '\n';
    indent_impl(out) << "else result := E_NOINTERFACE;" << '\n';
    indent_down_impl();
    indent_impl(out) << "end;" << '\n' << '\n';
    indent_impl(out) << "function " << cls_prefix << cls_nm << "._AddRef: Integer;" << '\n';
    indent_impl(out) << "begin" << '\n';
    indent_up_impl();
    indent_impl(out) << "result := -1;    // not refcounted" << '\n';
    indent_down_impl();
    indent_impl(out) << "end;" << '\n' << '\n';
    indent_impl(out) << "function " << cls_prefix << cls_nm << "._Release: Integer;" << '\n';
    indent_impl(out) << "begin" << '\n';
    indent_up_impl();
    indent_impl(out) << "result := -1;    // not refcounted" << '\n';
    indent_down_impl();
    indent_impl(out) << "end;" << '\n' << '\n';
  }

  if (tstruct->is_union()) {
    indent_impl(out) << "procedure " << cls_prefix << cls_nm << "."
                     << "ClearUnionValues;" << '\n';
    indent_impl(out) << "begin" << '\n';
    indent_up_impl();
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      t_type* t = (*m_iter)->get_type();
      while (t->is_typedef()) {
        t = ((t_typedef*)t)->get_type();
      }

      generate_delphi_clear_union_value(out,
                                        cls_prefix,
                                        cls_nm,
                                        t,
                                        *m_iter,
                                        "F",
                                        is_exception,
                                        tstruct->is_union(),
                                        is_x_factory,
                                        exception_factory_name);
    }
    indent_down_impl();
    indent_impl(out) << "end;" << '\n' << '\n';
  }

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* t = (*m_iter)->get_type();
    while (t->is_typedef()) {
      t = ((t_typedef*)t)->get_type();
    }
    generate_delphi_property_reader_impl(out, cls_prefix, cls_nm, t, *m_iter, "F", is_exception);
    generate_delphi_property_writer_impl(out,
                                         cls_prefix,
                                         cls_nm,
                                         t,
                                         *m_iter,
                                         "F",
                                         is_exception,
                                         tstruct->is_union(),
                                         is_x_factory,
                                         exception_factory_name);
    if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
      generate_delphi_isset_reader_writer_impl(out, cls_prefix, cls_nm, t, *m_iter, "F", is_exception);
    }
  }

  generate_delphi_struct_reader_impl(out, cls_prefix, tstruct, is_exception, is_x_factory);
  if (is_result) {
    generate_delphi_struct_result_writer_impl(out, cls_prefix, tstruct, is_exception, is_x_factory);
  } else {
    generate_delphi_struct_writer_impl(out, cls_prefix, tstruct, is_exception, is_x_factory);
  }
  generate_delphi_struct_tostring_impl(out, cls_prefix, tstruct, is_exception, is_x_factory);

  if (is_exception && is_x_factory) {
    generate_delphi_create_exception_impl(out, cls_prefix, tstruct, is_exception);
  }
}

void t_delphi_generator::print_delphi_struct_type_factory_func(ostream& out, t_struct* tstruct) {
  string struct_intf_name = type_name(tstruct);
  out << "Create_";
  out << struct_intf_name;
  out << "_Impl";
}

void t_delphi_generator::generate_delphi_struct_type_factory(ostream& out,
                                                             string cls_prefix,
                                                             t_struct* tstruct,
                                                             bool is_exception,
                                                             bool is_result,
                                                             bool is_x_factory) {
  (void)cls_prefix;
  if (is_exception)
    return;
  if (is_result)
    return;
  if (is_x_factory)
    return;

  string struct_intf_name = type_name(tstruct);
  string cls_nm = type_name(tstruct, true, false);

  out << "function ";
  print_delphi_struct_type_factory_func(out, tstruct);
  out << ": ";
  out << struct_intf_name;
  out << ";" << '\n';
  out << "begin" << '\n';
  indent_up();
  indent(out) << "Result := " << cls_nm << ".Create;" << '\n';
  indent_down();
  out << "end;" << '\n' << '\n';
}

void t_delphi_generator::generate_delphi_struct_type_factory_registration(ostream& out,
                                                                          string cls_prefix,
                                                                          t_struct* tstruct,
                                                                          bool is_exception,
                                                                          bool is_result,
                                                                          bool is_x_factory) {
  (void)cls_prefix;
  if (is_exception)
    return;
  if (is_result)
    return;
  if (is_x_factory)
    return;

  string struct_intf_name = type_name(tstruct);

  indent(out) << "  TypeRegistry.RegisterTypeFactory<" << struct_intf_name << ">(";
  print_delphi_struct_type_factory_func(out, tstruct);
  out << ");";
  out << '\n';
}

void t_delphi_generator::generate_delphi_struct_definition(ostream& out,
                                                           t_struct* tstruct,
                                                           bool is_exception,
                                                           bool in_class,
                                                           bool is_result,
                                                           bool is_x_factory) {
  bool is_final = (tstruct->annotations_.find("final") != tstruct->annotations_.end());
  string struct_intf_name;
  string struct_name;
  string isset_name;
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  string exception_factory_name = normalize_clsnm(tstruct->get_name(), "", true) + "Factory";

  if (is_exception) {
    struct_intf_name = type_name(tstruct, false, false, true);
  } else {
    struct_intf_name = type_name(tstruct);
  }

  if (is_exception) {
    struct_name = type_name(tstruct, true, (!is_x_factory), is_x_factory);
  } else {
    struct_name = type_name(tstruct, true);
  }

  if ((!is_exception) || is_x_factory) {

    generate_delphi_doc(out, tstruct);
    if(rtti_) {
      indent(out) << "{$TYPEINFO ON}" << '\n';
      indent(out) << "{$RTTI EXPLICIT METHODS([vcPublic, vcPublished]) PROPERTIES([vcPublic, vcPublished])}" << '\n';
      indent(out) << struct_intf_name << " = interface(IBaseWithTypeInfo)" << '\n';
    } else {
      indent(out) << struct_intf_name << " = interface(IBase)" << '\n';
    }
    indent_up();

    generate_guid(out);

    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      generate_delphi_property_reader_definition(out, *m_iter, is_exception);
      generate_delphi_property_writer_definition(out, *m_iter, is_exception);
    }

    if (is_x_factory) {
      out << '\n';
      indent(out) << "// Create Exception Object" << '\n';
      indent(out) << "function CreateException: " << type_name(tstruct, true, true) << ";" << '\n';
    }

    if (members.size() > 0) {
      out << '\n';
      for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        generate_property(out, *m_iter, true, is_exception);
      }
    }

    if (members.size() > 0) {
      out << '\n';
      for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
          generate_delphi_isset_reader_writer_definition(out, *m_iter, is_exception);
        }
      }
    }

    if (members.size() > 0) {
      out << '\n';
      for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
          isset_name = prop_name(*m_iter, is_exception, "__isset_");
          indent(out) << "property " << isset_name << ": System.Boolean read Get" << isset_name << " write Set" << isset_name << ";"
                      << '\n';
        }
      }
    }

    indent_down();
    indent(out) << "end;"
                << render_deprecation_attribute(tstruct->annotations_, " {", "}")
                << '\n';
    if(rtti_) {
      indent(out) << "{$IFNDEF TYPEINFO_WAS_ON} {$TYPEINFO OFF} {$ENDIF}" << '\n';
    }
    indent(out) << '\n';
  }

  generate_delphi_doc(out, tstruct);
  indent(out) << struct_name << " = ";
  if (is_final) {
    out << "sealed ";
  }
  out << "class(";
  if (is_exception && (!is_x_factory)) {
    out << "TException, IInterface, IBase, ISupportsToString";
  } else {
    out << "TInterfacedObject, IBase, ISupportsToString, " << struct_intf_name;
  }
  out << ")" << '\n';

  if (is_exception && (!is_x_factory)) {
    indent(out) << "public" << '\n';
    indent_up();
    indent(out) << "type" << '\n';
    indent_up();
    generate_delphi_struct_definition(out, tstruct, is_exception, in_class, is_result, true);
    indent_down();
    indent_down();
  }

  indent(out) << "private" << '\n';
  indent_up();

  if (is_exception && (!is_x_factory)) {
    indent(out) << "F" << exception_factory_name << " :" << struct_intf_name << ";" << '\n' << '\n';
  }

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    indent(out) << declare_field(*m_iter, false, "F", is_exception) << '\n';
  }

  if (members.size() > 0) {
    indent(out) << '\n';
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
        isset_name = prop_name(*m_iter, is_exception, "F__isset_");
        indent(out) << isset_name << ": System.Boolean;" << '\n';
      }
    }
  }

  indent(out) << '\n';

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    generate_delphi_property_reader_definition(out, *m_iter, is_exception);
    generate_delphi_property_writer_definition(out, *m_iter, is_exception);
  }

  if (tstruct->is_union()) {
    out << '\n';
    indent(out) << "// Clear values(for union's property setter)" << '\n';
    indent(out) << "procedure ClearUnionValues;" << '\n';
  }

  if (members.size() > 0) {
    out << '\n';
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
        isset_name = prop_name(*m_iter, is_exception, "__isset_");
        indent(out) << "function Get" << isset_name << ": System.Boolean;" << '\n';
        indent(out) << "procedure Set" << isset_name << "( const value : System.Boolean);" << '\n';
      }
    }
  }

  if (is_exception && (!is_x_factory)) {
    out << '\n';
    indent_down();
    indent(out) << "strict protected" << '\n';
    indent_up();
    indent(out) << "function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;" << '\n';
    indent(out) << "function _AddRef: Integer; stdcall;" << '\n';
    indent(out) << "function _Release: Integer; stdcall;" << '\n';
    out << '\n';
  }

  indent_down();
  indent(out) << "public" << '\n';
  indent_up();

  if ((members.size() > 0) && is_exception && (!is_x_factory)) {
    indent(out) << "constructor Create; overload;" << render_deprecation_attribute(tstruct->annotations_," ",";") << '\n';
    indent(out) << "constructor Create(" << constructor_argument_list(tstruct, indent())
                << "); overload;" << render_deprecation_attribute(tstruct->annotations_," ",";") << '\n';
  } else {
    indent(out) << "constructor Create;" << render_deprecation_attribute(tstruct->annotations_," ",";") << '\n';
  }

  indent(out) << "destructor Destroy; override;" << '\n';

  out << '\n';
  indent(out) << "function ToString: string; override;" << '\n';

  if (is_exception && (!is_x_factory)) {
    out << '\n';
    indent(out) << "// Exception Factory" << '\n';
    indent(out) << "function " << exception_factory_name << ": " << struct_intf_name << ";" << '\n';
  }

  out << '\n';
  indent(out) << "// IBase" << '\n';
  indent(out) << "procedure Read( const iprot: IProtocol);" << '\n';
  indent(out) << "procedure Write( const oprot: IProtocol);" << '\n';

  if (is_exception && is_x_factory) {
    out << '\n';
    indent(out) << "// Create Exception Object" << '\n';
    indent(out) << "function CreateException: " << type_name(tstruct, true, true) << ";" << '\n';
  }

  if (members.size() > 0) {
    out << '\n';
    indent(out) << "// Properties" << '\n';
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      generate_property(out, *m_iter, true, is_exception);
    }
  }

  if (members.size() > 0) {
    out << '\n';
    indent(out) << "// isset" << '\n';
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
        isset_name = prop_name(*m_iter, is_exception, "__isset_");
        indent(out) << "property " << isset_name << ": System.Boolean read Get" << isset_name << " write Set" << isset_name << ";"
                    << '\n';
      }
    }
  }

  indent_down();
  indent(out) << "end;" << '\n' << '\n';
}

void t_delphi_generator::generate_service(t_service* tservice) {
  indent_up();
  generate_delphi_doc(s_service, tservice);
  indent(s_service) << normalize_clsnm(service_name_, "T") << " = class" << '\n';
  indent(s_service) << "public" << '\n';
  indent_up();
  indent(s_service) << "type" << '\n';
  generate_service_interface(tservice);
  generate_service_client(tservice);
  generate_service_server(tservice);
  generate_service_helpers(tservice);
  indent_down();
  indent_down();
  indent(s_service) << "end;" << '\n';
  indent(s_service) << '\n';
  indent_down();
}

void t_delphi_generator::generate_service_interface(t_service* tservice) {
  generate_service_interface(tservice,false);
  if(async_) {
    generate_service_interface(tservice,true);
  }
}


void t_delphi_generator::generate_service_interface(t_service* tservice, bool for_async) {
  string extends = "";
  string extends_iface = "";
  string iface_name = for_async ? "IAsync" : "Iface";

  indent_up();

  generate_delphi_doc(s_service, tservice);
  if (tservice->get_extends() != nullptr) {
    extends = type_name(tservice->get_extends(), true, true);
    extends_iface = extends + "." + iface_name;
    generate_delphi_doc(s_service, tservice);
    indent(s_service) << iface_name << " = interface(" << extends_iface << ")" << '\n';
  } else {
    indent(s_service) << iface_name << " = interface" << '\n';
  }

  indent_up();
  generate_guid(s_service);
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_delphi_doc(s_service, *f_iter);
    indent(s_service) << function_signature(*f_iter, for_async) << '\n';
  }
  indent_down();
  indent(s_service) << "end;" 
                    << render_deprecation_attribute( tservice->annotations_, " {", "}")
                    << '\n' << '\n';

  indent_down();
}

void t_delphi_generator::generate_guid(std::ostream& out) {
#ifdef _WIN32   // TODO: add support for non-windows platforms if needed
  GUID guid;
  if (SUCCEEDED(CoCreateGuid(&guid))) {
    OLECHAR guid_chars[40]{};
    if (StringFromGUID2(guid, &guid_chars[0], sizeof(guid_chars) / sizeof(guid_chars[0])) > 0) {
      std::wstring guid_wstr(guid_chars);
      std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> convert;
      std::string guid_str = convert.to_bytes(guid_wstr);
      indent(out) << "['" << guid_str << "']" << '\n';
    }
  }
#else
  (void)out;  // prevent unused warning on other platforms
#endif
}

void t_delphi_generator::generate_service_helpers(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* ts = (*f_iter)->get_arglist();
    generate_delphi_struct_definition(s_service, ts, false, true);
    generate_delphi_struct_impl(s_service_impl,
                                normalize_clsnm(service_name_, "T") + ".",
                                ts,
                                false);
    generate_function_helpers(*f_iter);
  }
}

void t_delphi_generator::generate_service_client(t_service* tservice) {
  indent_up();
  string extends = "";
  string extends_client = "TInterfacedObject";
  string implements = async_ ? "Iface, IAsync" : "Iface";

  generate_delphi_doc(s_service, tservice);
  if (tservice->get_extends() != nullptr) {
    extends = type_name(tservice->get_extends(), true, true);
    extends_client = extends + ".TClient";
  }
  indent(s_service) << "TClient = class( " << extends_client << ", " << implements << ")" << '\n';

  indent(s_service) << "public" << '\n';
  indent_up();

  indent(s_service) << "constructor Create( prot: IProtocol); overload;" << '\n';

  indent_impl(s_service_impl) << "constructor " << normalize_clsnm(service_name_, "T")
                              << ".TClient.Create( prot: IProtocol);" << '\n';
  indent_impl(s_service_impl) << "begin" << '\n';
  indent_up_impl();
  indent_impl(s_service_impl) << "Create( prot, prot );" << '\n';
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << '\n' << '\n';

  indent(s_service)
      << "constructor Create( const iprot: IProtocol; const oprot: IProtocol); overload;" << '\n';

  indent_impl(s_service_impl) << "constructor " << normalize_clsnm(service_name_, "T")
                              << ".TClient.Create( const iprot: IProtocol; const oprot: IProtocol);"
                              << '\n';
  indent_impl(s_service_impl) << "begin" << '\n';
  indent_up_impl();
  indent_impl(s_service_impl) << "inherited Create;" << '\n';
  indent_impl(s_service_impl) << "iprot_ := iprot;" << '\n';
  indent_impl(s_service_impl) << "oprot_ := oprot;" << '\n';
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << '\n' << '\n';

  indent_down();

  if (extends.empty()) {
    indent(s_service) << "protected" << '\n';
    indent_up();
    indent(s_service) << "iprot_: IProtocol;" << '\n';
    indent(s_service) << "oprot_: IProtocol;" << '\n';
    indent(s_service) << "seqid_: System.Integer;" << '\n';
    indent_down();

    indent(s_service) << "public" << '\n';
    indent_up();
    indent(s_service) << "property InputProtocol: IProtocol read iprot_;" << '\n';
    indent(s_service) << "property OutputProtocol: IProtocol read oprot_;" << '\n';
    indent_down();
  }

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;

  indent(s_service) << "protected" << '\n';
  indent_up();

  indent(s_service) << "// Iface" << '\n';
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string funname = (*f_iter)->get_name();
    generate_delphi_doc(s_service, *f_iter);
    indent(s_service) << function_signature(*f_iter, false) << '\n';
  }

  if( async_) {
    indent(s_service) << '\n';
    indent(s_service) << "// IAsync" << '\n';
    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
      string funname = (*f_iter)->get_name();
      generate_delphi_doc(s_service, *f_iter);
      indent(s_service) << function_signature(*f_iter, true) << '\n';
    }
  }

  indent_down();

  indent(s_service) << "public" << '\n';
  indent_up();

  string full_cls = normalize_clsnm(service_name_, "T") + ".TClient";

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string funname = (*f_iter)->get_name();

    vector<t_field*>::const_iterator fld_iter;
    t_struct* arg_struct = (*f_iter)->get_arglist();
    const vector<t_field*>& fields = arg_struct->get_members();

    // one for sync only, two for async+sync
    int mode = async_ ? 1 : 0;
    while( mode >= 0) {
      bool for_async = (mode != 0);
      mode--;

      indent_impl(s_service_impl) << function_signature(*f_iter, for_async, full_cls) << '\n';
      indent_impl(s_service_impl) << "begin" << '\n';
      indent_up_impl();

      t_type* ttype = (*f_iter)->get_returntype();
      if( for_async) {
        if (is_void(ttype)) {
           // Delphi forces us to specify a type with IFuture<T>, so we use Integer=0 for void methods
          indent_impl(s_service_impl) << "result := TTask.Future<System.Integer>(function: System.Integer" << '\n';
        } else {
          string rettype = type_name(ttype, false, true, false, true);
          indent_impl(s_service_impl) << "result := TTask.Future<" << rettype << ">(function: " << rettype << '\n';
        }
        indent_impl(s_service_impl) << "begin" << '\n';
        indent_up_impl();
      }

      indent_impl(s_service_impl) << "send_" << funname << "(";

      bool first = true;
      for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
        if (first) {
          first = false;
        } else {
          s_service_impl << ", ";
        }
        s_service_impl << normalize_name((*fld_iter)->get_name());
      }
      s_service_impl << ");" << '\n';

      if (!(*f_iter)->is_oneway()) {
        s_service_impl << indent_impl();
        if (!(*f_iter)->get_returntype()->is_void()) {
          s_service_impl << "Result := ";
        }
        s_service_impl << "recv_" << funname << "();" << '\n';
      }

      if( for_async) {
        if (is_void(ttype)) {
          indent_impl(s_service_impl) << "Result := 0;" << '\n';  // no IFuture<void> in Delphi
        }
        indent_down_impl();
        indent_impl(s_service_impl) << "end);" << '\n';
      }

      indent_down_impl();
      indent_impl(s_service_impl) << "end;" << '\n' << '\n';
    }

    t_function send_function(g_type_void,
                             string("send_") + (*f_iter)->get_name(),
                             (*f_iter)->get_arglist());

    string argsname = (*f_iter)->get_name() + "_args";
    string args_clsnm = normalize_clsnm(argsname, "T");
    string args_intfnm = normalize_clsnm(argsname, "I");

    string argsvar = tmp("_args");
    string msgvar = tmp("_msg");

    indent(s_service) << function_signature(&send_function, false) << '\n';
    indent_impl(s_service_impl) << function_signature(&send_function, false, full_cls) << '\n';
    indent_impl(s_service_impl) << "var" << '\n';
    indent_up_impl();
    indent_impl(s_service_impl) << argsvar << " : " << args_intfnm << ";" << '\n';
    indent_impl(s_service_impl) << msgvar << " : Thrift.Protocol.TThriftMessage;" << '\n';
    indent_down_impl();
    indent_impl(s_service_impl) << "begin" << '\n';
    indent_up_impl();

    indent_impl(s_service_impl) << "seqid_ := seqid_ + 1;" << '\n';
    indent_impl(s_service_impl) << "Thrift.Protocol.Init( " << msgvar << ", '" << funname
                                << "', " << ((*f_iter)->is_oneway() ? "TMessageType.Oneway"
                                                                    : "TMessageType.Call")
                                << ", seqid_);" << '\n';

    indent_impl(s_service_impl) << "oprot_.WriteMessageBegin( " << msgvar << " );" << '\n';
    indent_impl(s_service_impl) << argsvar << " := " << args_clsnm << "Impl.Create();" << '\n';

    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      indent_impl(s_service_impl) << argsvar << "." << prop_name(*fld_iter)
                                  << " := " << normalize_name((*fld_iter)->get_name()) << ";"
                                  << '\n';
    }
    indent_impl(s_service_impl) << argsvar << ".Write(oprot_);" << '\n';
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      indent_impl(s_service_impl) << argsvar << "." << prop_name(*fld_iter)
                                  << " := " << empty_value((*fld_iter)->get_type()) << ";" << '\n';
    }

    indent_impl(s_service_impl) << "oprot_.WriteMessageEnd();" << '\n';
    indent_impl(s_service_impl) << "oprot_.Transport.Flush();" << '\n';

    indent_down_impl();
    indent_impl(s_service_impl) << "end;" << '\n' << '\n';

    if (!(*f_iter)->is_oneway()) {
      string org_resultname = (*f_iter)->get_name() + "_result";
      string result_clsnm = normalize_clsnm(org_resultname, "T");
      string result_intfnm = normalize_clsnm(org_resultname, "I");

      t_struct noargs(program_);
      t_function recv_function((*f_iter)->get_returntype(),
                               string("recv_") + (*f_iter)->get_name(),
                               &noargs,
                               (*f_iter)->get_xceptions());

      t_struct* xs = (*f_iter)->get_xceptions();
      const std::vector<t_field*>& xceptions = xs->get_members();

      string exceptvar = tmp("_ex");
      string appexvar = tmp("_ax");
      string retvar = tmp("_ret");

      indent(s_service) << function_signature(&recv_function, false) << '\n';
      indent_impl(s_service_impl) << function_signature(&recv_function, false, full_cls) << '\n';
      indent_impl(s_service_impl) << "var" << '\n';
      indent_up_impl();
      indent_impl(s_service_impl) << msgvar << " : Thrift.Protocol.TThriftMessage;" << '\n';
      if (xceptions.size() > 0) {
        indent_impl(s_service_impl) << exceptvar << " : Exception;" << '\n';
      }
      indent_impl(s_service_impl) << appexvar << " : TApplicationException;" << '\n';
      indent_impl(s_service_impl) << retvar << " : " << result_intfnm << ";" << '\n';

      indent_down_impl();
      indent_impl(s_service_impl) << "begin" << '\n';
      indent_up_impl();
      indent_impl(s_service_impl) << msgvar << " := iprot_.ReadMessageBegin();" << '\n';
      indent_impl(s_service_impl) << "if (" << msgvar << ".Type_ = TMessageType.Exception) then begin" << '\n';
      indent_up_impl();
      indent_impl(s_service_impl) << appexvar << " := TApplicationException.Read(iprot_);" << '\n';
      indent_impl(s_service_impl) << "iprot_.ReadMessageEnd();" << '\n';
      indent_impl(s_service_impl) << "raise " << appexvar << ";" << '\n';
      indent_down_impl();
      indent_impl(s_service_impl) << "end;" << '\n';

      indent_impl(s_service_impl) << retvar << " := " << result_clsnm << "Impl.Create();" << '\n';
      indent_impl(s_service_impl) << retvar << ".Read(iprot_);" << '\n';
      indent_impl(s_service_impl) << "iprot_.ReadMessageEnd();" << '\n';

      if (!(*f_iter)->get_returntype()->is_void()) {
        indent_impl(s_service_impl) << "if (" << retvar << ".__isset_success) then begin" << '\n';
        indent_up_impl();
        indent_impl(s_service_impl) << "Result := " << retvar << ".Success;" << '\n';
        t_type* type = (*f_iter)->get_returntype();
        if (type->is_struct() || type->is_xception() || type->is_map() || type->is_list()
            || type->is_set()) {
          indent_impl(s_service_impl) << retvar << ".Success := nil;" << '\n';
        }
        indent_impl(s_service_impl) << "Exit;" << '\n';
        indent_down_impl();
        indent_impl(s_service_impl) << "end;" << '\n';
      }

      vector<t_field*>::const_iterator x_iter;
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        indent_impl(s_service_impl) << "if (" << retvar << "." << prop_name(*x_iter, false, "__isset_")
                                    << ") then begin" << '\n';
        indent_up_impl();
        indent_impl(s_service_impl) << exceptvar << " := " << retvar << "." << prop_name(*x_iter)
                                    << ".CreateException;" << '\n';
        indent_impl(s_service_impl) << "raise " << exceptvar << ";" << '\n';
        indent_down_impl();
        indent_impl(s_service_impl) << "end;" << '\n';
      }

      if (!(*f_iter)->get_returntype()->is_void()) {
        indent_impl(s_service_impl)
            << "raise TApplicationExceptionMissingResult.Create('"
            << (*f_iter)->get_name() << " failed: unknown result');" << '\n';
      }

      indent_down_impl();
      indent_impl(s_service_impl) << "end;" << '\n' << '\n';
    }
  }

  indent_down();
  indent(s_service) << "end;"
                    << render_deprecation_attribute( tservice->annotations_, " {", "}")
                    << '\n' << '\n';
}

void t_delphi_generator::generate_service_server(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  string extends = "";
  string extends_processor = "";

  string full_cls = normalize_clsnm(service_name_, "T") + ".TProcessorImpl";

  if (tservice->get_extends() != nullptr) {
    extends = type_name(tservice->get_extends(), true, true);
    extends_processor = extends + ".TProcessorImpl";
    indent(s_service) << "TProcessorImpl = class(" << extends_processor << ", IProcessor)" << '\n';
  } else {
    indent(s_service) << "TProcessorImpl = class( TInterfacedObject, IProcessor)" << '\n';
  }

  indent(s_service) << "public" << '\n';
  indent_up();
  indent(s_service) << "constructor Create( iface_: Iface );" << '\n';
  indent(s_service) << "destructor Destroy; override;" << '\n';
  indent_down();

  indent_impl(s_service_impl) << "constructor " << full_cls << ".Create( iface_: Iface );" << '\n';
  indent_impl(s_service_impl) << "begin" << '\n';
  indent_up_impl();
  if (tservice->get_extends() != nullptr) {
    indent_impl(s_service_impl) << "inherited Create( iface_);" << '\n';
  } else {
    indent_impl(s_service_impl) << "inherited Create;" << '\n';
  }
  indent_impl(s_service_impl) << "Self.iface_ := iface_;" << '\n';
  if (tservice->get_extends() != nullptr) {
    indent_impl(s_service_impl) << "ASSERT( processMap_ <> nil);  // inherited" << '\n';
  } else {
    indent_impl(s_service_impl)
        << "processMap_ := TThriftDictionaryImpl<string, TProcessFunction>.Create;" << '\n';
  }

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    indent_impl(s_service_impl) << "processMap_.AddOrSetValue( '" << (*f_iter)->get_name() << "', "
                                << (*f_iter)->get_name() << "_Process);" << '\n';
  }
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << '\n' << '\n';

  indent_impl(s_service_impl) << "destructor " << full_cls << ".Destroy;" << '\n';
  indent_impl(s_service_impl) << "begin" << '\n';
  indent_up_impl();
  indent_impl(s_service_impl) << "inherited;" << '\n';
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << '\n' << '\n';

  indent(s_service) << "private" << '\n';
  indent_up();
  indent(s_service) << "iface_: Iface;" << '\n';
  indent_down();

  if (tservice->get_extends() == nullptr) {
    indent(s_service) << "protected" << '\n';
    indent_up();
    indent(s_service) << "type" << '\n';
    indent_up();
    indent(s_service) << "TProcessFunction = reference to procedure( seqid: System.Integer; const iprot: "
                         "IProtocol; const oprot: IProtocol"
                      << (events_ ? "; const events : IRequestEvents" : "") << ");" << '\n';
    indent_down();
    indent_down();
    indent(s_service) << "protected" << '\n';
    indent_up();
    indent(s_service) << "processMap_: IThriftDictionary<string, TProcessFunction>;" << '\n';
    indent_down();
  }

  indent(s_service) << "public" << '\n';
  indent_up();
  if (extends.empty()) {
    indent(s_service) << "function Process( const iprot: IProtocol; const oprot: IProtocol; const "
                         "events : IProcessorEvents): System.Boolean;" << '\n';
  } else {
    indent(s_service) << "function Process( const iprot: IProtocol; const oprot: IProtocol; const "
                         "events : IProcessorEvents): System.Boolean; reintroduce;" << '\n';
  }

  indent_impl(s_service_impl) << "function " << full_cls << ".Process( const iprot: IProtocol; "
                                                            "const oprot: IProtocol; const events "
                                                            ": IProcessorEvents): System.Boolean;" << '\n';
  ;
  indent_impl(s_service_impl) << "var" << '\n';
  indent_up_impl();
  indent_impl(s_service_impl) << "msg : Thrift.Protocol.TThriftMessage;" << '\n';
  indent_impl(s_service_impl) << "fn : TProcessFunction;" << '\n';
  indent_impl(s_service_impl) << "x : TApplicationException;" << '\n';
  if (events_) {
    indent_impl(s_service_impl) << "context : IRequestEvents;" << '\n';
  }
  indent_down_impl();
  indent_impl(s_service_impl) << "begin" << '\n';
  indent_up_impl();
  indent_impl(s_service_impl) << "try" << '\n';
  indent_up_impl();
  indent_impl(s_service_impl) << "msg := iprot.ReadMessageBegin();" << '\n';
  indent_impl(s_service_impl) << "fn := nil;" << '\n';
  indent_impl(s_service_impl) << "if not processMap_.TryGetValue(msg.Name, fn)" << '\n';
  indent_impl(s_service_impl) << "or not Assigned(fn) then begin" << '\n';
  indent_up_impl();
  indent_impl(s_service_impl) << "TProtocolUtil.Skip(iprot, TType.Struct);" << '\n';
  indent_impl(s_service_impl) << "iprot.ReadMessageEnd();" << '\n';
  indent_impl(s_service_impl) << "x := "
                                 "TApplicationExceptionUnknownMethod.Create("
                                 "'Invalid method name: ''' + msg.Name + '''');" << '\n';
  indent_impl(s_service_impl)
      << "Thrift.Protocol.Init( msg, msg.Name, TMessageType.Exception, msg.SeqID);"
      << '\n';
  indent_impl(s_service_impl) << "oprot.WriteMessageBegin( msg);" << '\n';
  indent_impl(s_service_impl) << "x.Write(oprot);" << '\n';
  indent_impl(s_service_impl) << "oprot.WriteMessageEnd();" << '\n';
  indent_impl(s_service_impl) << "oprot.Transport.Flush();" << '\n';
  indent_impl(s_service_impl) << "Result := True;" << '\n';
  indent_impl(s_service_impl) << "Exit;" << '\n';
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << '\n';
  if (events_) {
    indent_impl(s_service_impl) << "if events <> nil" << '\n';
    indent_impl(s_service_impl) << "then context := events.CreateRequestContext(msg.Name)" << '\n';
    indent_impl(s_service_impl) << "else context := nil;" << '\n';
    indent_impl(s_service_impl) << "try" << '\n';
    indent_up_impl();
    indent_impl(s_service_impl) << "fn(msg.SeqID, iprot, oprot, context);" << '\n';
    indent_down_impl();
    indent_impl(s_service_impl) << "finally" << '\n';
    indent_up_impl();
    indent_impl(s_service_impl) << "if context <> nil then begin" << '\n';
    indent_up_impl();
    indent_impl(s_service_impl) << "context.CleanupContext;" << '\n';
    indent_impl(s_service_impl) << "context := nil;" << '\n';
    indent_down_impl();
    indent_impl(s_service_impl) << "end;" << '\n';
    indent_down_impl();
    indent_impl(s_service_impl) << "end;" << '\n';
  } else {
    indent_impl(s_service_impl) << "fn(msg.SeqID, iprot, oprot);" << '\n';
  }
  indent_down_impl();
  indent_impl(s_service_impl) << "except" << '\n';
  indent_up_impl();
  indent_impl(s_service_impl) << "on TTransportExceptionTimedOut do begin" << '\n';
  indent_up_impl();
  indent_impl(s_service_impl) << "Result := True;" << '\n';
  indent_impl(s_service_impl) << "Exit;" << '\n';
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << '\n';
  indent_impl(s_service_impl) << "else begin" << '\n';
  indent_up_impl();
  indent_impl(s_service_impl) << "Result := False;" << '\n';
  indent_impl(s_service_impl) << "Exit;" << '\n';
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << '\n';
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << '\n';
  indent_impl(s_service_impl) << "Result := True;" << '\n';
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << '\n' << '\n';

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_process_function(tservice, *f_iter);
  }

  indent_down();
  indent(s_service) << "end;" << '\n' << '\n';
}

void t_delphi_generator::generate_function_helpers(t_function* tfunction) {
  if (tfunction->is_oneway()) {
    return;
  }

  t_struct result(program_, tfunction->get_name() + "_result");
  t_field success(tfunction->get_returntype(), "Success", 0);
  if (!tfunction->get_returntype()->is_void()) {
    result.append(&success);
  }

  t_struct* xs = tfunction->get_xceptions();
  const vector<t_field*>& fields = xs->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    result.append(*f_iter);
  }

  generate_delphi_struct_definition(s_service, &result, false, true, true);
  generate_delphi_struct_impl(s_service_impl,
                              normalize_clsnm(service_name_, "T") + ".",
                              &result,
                              false);
}

void t_delphi_generator::generate_process_function(t_service* tservice, t_function* tfunction) {
  (void)tservice;
  string funcname = tfunction->get_name();
  string full_cls = normalize_clsnm(service_name_, "T") + ".TProcessorImpl";

  string org_argsname = funcname + "_args";
  string args_clsnm = normalize_clsnm(org_argsname, "T");
  string args_intfnm = normalize_clsnm(org_argsname, "I");

  string org_resultname = funcname + "_result";
  string result_clsnm = normalize_clsnm(org_resultname, "T");
  string result_intfnm = normalize_clsnm(org_resultname, "I");

  indent(s_service) << "procedure " << funcname
                    << "_Process( seqid: System.Integer; const iprot: IProtocol; const oprot: IProtocol"
                    << (events_ ? "; const events : IRequestEvents" : "") << ");" << '\n';

  if (tfunction->is_oneway()) {
    indent_impl(s_service_impl) << "// one way processor" << '\n';
  } else {
    indent_impl(s_service_impl) << "// both way processor" << '\n';
  }

  indent_impl(s_service_impl)
      << "procedure " << full_cls << "." << funcname
      << "_Process( seqid: System.Integer; const iprot: IProtocol; const oprot: IProtocol"
      << (events_ ? "; const events : IRequestEvents" : "") << ");" << '\n';
  indent_impl(s_service_impl) << "var" << '\n';
  indent_up_impl();
  indent_impl(s_service_impl) << "args: " << args_intfnm << ";" << '\n';
  if (!tfunction->is_oneway()) {
    indent_impl(s_service_impl) << "msg: Thrift.Protocol.TThriftMessage;" << '\n';
    indent_impl(s_service_impl) << "ret: " << result_intfnm << ";" << '\n';
    indent_impl(s_service_impl) << "appx : TApplicationException;" << '\n';
  }

  indent_down_impl();
  indent_impl(s_service_impl) << "begin" << '\n';
  indent_up_impl();

  if (!tfunction->is_oneway()) {
    indent_impl(s_service_impl) << "ret := " << result_clsnm << "Impl.Create;" << '\n';
  }

  indent_impl(s_service_impl) << "try" << '\n';
  indent_up_impl();

  if (events_) {
    indent_impl(s_service_impl) << "if events <> nil then events.PreRead;" << '\n';
  }
  indent_impl(s_service_impl) << "args := " << args_clsnm << "Impl.Create;" << '\n';
  indent_impl(s_service_impl) << "args.Read(iprot);" << '\n';
  indent_impl(s_service_impl) << "iprot.ReadMessageEnd();" << '\n';
  if (events_) {
    indent_impl(s_service_impl) << "if events <> nil then events.PostRead;" << '\n';
  }

  t_struct* xs = tfunction->get_xceptions();
  const std::vector<t_field*>& xceptions = xs->get_members();
  vector<t_field*>::const_iterator x_iter;

  t_struct* arg_struct = tfunction->get_arglist();
  const std::vector<t_field*>& fields = arg_struct->get_members();
  vector<t_field*>::const_iterator f_iter;

  s_service_impl << indent_impl();
  if (!tfunction->is_oneway() && !tfunction->get_returntype()->is_void()) {
    s_service_impl << "ret.Success := ";
  }
  s_service_impl << "iface_." << normalize_name(tfunction->get_name(), true) << "(";
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      s_service_impl << ", ";
    }
    s_service_impl << "args." << prop_name(*f_iter);
  }
  s_service_impl << ");" << '\n';

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    indent_impl(s_service_impl) << "args." << prop_name(*f_iter)
                                << " := " << empty_value((*f_iter)->get_type()) << ";" << '\n';
  }

  indent_down_impl();
  indent_impl(s_service_impl) << "except" << '\n';
  indent_up_impl();

  for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
    indent_impl(s_service_impl) << "on E: " << type_name((*x_iter)->get_type(), true, true)
                                << " do begin" << '\n';
    indent_up_impl();
    if (!tfunction->is_oneway()) {
      string factory_name = normalize_clsnm((*x_iter)->get_type()->get_name(), "", true)
                            + "Factory";
      indent_impl(s_service_impl) << "ret." << prop_name(*x_iter) << " := E." << factory_name << ";"
                                  << '\n';
    }
    indent_down_impl();
    indent_impl(s_service_impl) << "end;" << '\n';
  }

  indent_impl(s_service_impl) << "on E: Exception do begin" << '\n';
  indent_up_impl();
  if(events_) {
    indent_impl(s_service_impl) << "if events <> nil then events.UnhandledError(E);" << '\n';
  }
  if (!tfunction->is_oneway()) {
    indent_impl(s_service_impl) << "appx := TApplicationExceptionInternalError.Create(E.Message);"
                                << '\n';
    indent_impl(s_service_impl) << "try" << '\n';
    indent_up_impl();
    if(events_) {
      indent_impl(s_service_impl) << "if events <> nil then events.PreWrite;" << '\n';
    }
    indent_impl(s_service_impl) << "Thrift.Protocol.Init( msg, '"
                                << tfunction->get_name() << "', TMessageType.Exception, seqid);"
                                << '\n';
    indent_impl(s_service_impl) << "oprot.WriteMessageBegin( msg);" << '\n';
    indent_impl(s_service_impl) << "appx.Write(oprot);" << '\n';
    indent_impl(s_service_impl) << "oprot.WriteMessageEnd();" << '\n';
    indent_impl(s_service_impl) << "oprot.Transport.Flush();" << '\n';
    if(events_) {
      indent_impl(s_service_impl) << "if events <> nil then events.PostWrite;" << '\n';
    }
    indent_impl(s_service_impl) << "Exit;" << '\n';
    indent_down_impl();
    indent_impl(s_service_impl) << "finally" << '\n';
    indent_up_impl();
    indent_impl(s_service_impl) << "appx.Free;" << '\n';
    indent_down_impl();
    indent_impl(s_service_impl) << "end;" << '\n';
  }
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << '\n';

  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << '\n';

  if (!tfunction->is_oneway()) {
    if (events_) {
      indent_impl(s_service_impl) << "if events <> nil then events.PreWrite;" << '\n';
    }
    indent_impl(s_service_impl) << "Thrift.Protocol.Init( msg, '"
                                << tfunction->get_name() << "', TMessageType.Reply, seqid); "
                                << '\n';
    indent_impl(s_service_impl) << "oprot.WriteMessageBegin( msg); " << '\n';
    indent_impl(s_service_impl) << "ret.Write(oprot);" << '\n';
    indent_impl(s_service_impl) << "oprot.WriteMessageEnd();" << '\n';
    indent_impl(s_service_impl) << "oprot.Transport.Flush();" << '\n';
    if (events_) {
      indent_impl(s_service_impl) << "if events <> nil then events.PostWrite;" << '\n';
    }
  } else if (events_) {
    indent_impl(s_service_impl) << "if events <> nil then events.OnewayComplete;" << '\n';
  }

  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << '\n' << '\n';
}

void t_delphi_generator::generate_deserialize_field(ostream& out,
                                                    bool is_xception,
                                                    t_field* tfield,
                                                    string prefix,
                                                    ostream& local_vars) {
  t_type* type = tfield->get_type();
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }

  if (type->is_void()) {
    throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE: " + prefix + tfield->get_name();
  }

  string name = prefix + prop_name(tfield, is_xception);

  if (type->is_struct() || type->is_xception()) {
    generate_deserialize_struct(out, (t_struct*)type, name, "");
  } else if (type->is_container()) {
    generate_deserialize_container(out, is_xception, type, name, local_vars);
  } else if (type->is_base_type() || type->is_enum()) {
    indent_impl(out) << name << " := ";

    if (type->is_enum()) {
      out << type_name(type, false) << "(";
    }

    out << "iprot.";

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " + name;
        break;
      case t_base_type::TYPE_STRING:
        if (type->is_binary()) {
          out << (com_types_ ? "ReadBinaryCOM();" :  "ReadBinary();");
        } else {
          out << "ReadString();";
        }
        break;
      case t_base_type::TYPE_UUID:
        out << "ReadUuid();";
        break;
      case t_base_type::TYPE_BOOL:
        out << "ReadBool();";
        break;
      case t_base_type::TYPE_I8:
        out << "ReadByte();";
        break;
      case t_base_type::TYPE_I16:
        out << "ReadI16();";
        break;
      case t_base_type::TYPE_I32:
        out << "ReadI32();";
        break;
      case t_base_type::TYPE_I64:
        out << "ReadI64();";
        break;
      case t_base_type::TYPE_DOUBLE:
        out << "ReadDouble();";
        break;
      default:
        throw "compiler error: no Delphi name for base type " + t_base_type::t_base_name(tbase);
      }
    } else if (type->is_enum()) {
      out << "ReadI32()";
      out << ");";
    }
    out << '\n';
  } else {
    printf("DO NOT KNOW HOW TO DESERIALIZE FIELD '%s' TYPE '%s'\n",
           tfield->get_name().c_str(),
           type_name(type).c_str());
  }
}

void t_delphi_generator::generate_deserialize_struct(ostream& out,
                                                     t_struct* tstruct,
                                                     string name,
                                                     string prefix) {
  string typ_name;

  if (tstruct->is_xception()) {
    typ_name = type_name(tstruct, true, false, true, true);
  } else {
    typ_name = type_name(tstruct, true, false);
  }

  indent_impl(out) << prefix << name << " := " << typ_name << ".Create;" << '\n';
  indent_impl(out) << prefix << name << ".Read(iprot);" << '\n';
}

void t_delphi_generator::generate_deserialize_container(ostream& out,
                                                        bool is_xception,
                                                        t_type* ttype,
                                                        string name,
                                                        std::ostream& local_vars) {

  string obj;
  string counter;
  string local_var;

  if (ttype->is_map()) {
    obj = tmp("_map");
  } else if (ttype->is_set()) {
    obj = tmp("_set");
  } else if (ttype->is_list()) {
    obj = tmp("_list");
  }

  if (ttype->is_map()) {
    local_var = obj + ": TThriftMap;";
  } else if (ttype->is_set()) {
    local_var = obj + ": TThriftSet;";
  } else if (ttype->is_list()) {
    local_var = obj + ": TThriftList;";
  }
  local_vars << "  " << local_var << '\n';
  counter = tmp("_i");
  local_var = counter + ": System.Integer;";
  local_vars << "  " << local_var << '\n';

  indent_impl(out) << name << " := " << type_name(ttype, true) << ".Create;" << '\n';

  if (ttype->is_map()) {
    indent_impl(out) << obj << " := iprot.ReadMapBegin();" << '\n';
  } else if (ttype->is_set()) {
    indent_impl(out) << obj << " := iprot.ReadSetBegin();" << '\n';
  } else if (ttype->is_list()) {
    indent_impl(out) << obj << " := iprot.ReadListBegin();" << '\n';
  }

  indent_impl(out) << "for " << counter << " := 0 to " << obj << ".Count - 1 do begin" << '\n';
  indent_up_impl();
  if (ttype->is_map()) {
    generate_deserialize_map_element(out, is_xception, (t_map*)ttype, name, local_vars);
  } else if (ttype->is_set()) {
    generate_deserialize_set_element(out, is_xception, (t_set*)ttype, name, local_vars);
  } else if (ttype->is_list()) {
    generate_deserialize_list_element(out, is_xception, (t_list*)ttype, name, local_vars);
  }
  indent_down_impl();
  indent_impl(out) << "end;" << '\n';

  if (ttype->is_map()) {
    indent_impl(out) << "iprot.ReadMapEnd();" << '\n';
  } else if (ttype->is_set()) {
    indent_impl(out) << "iprot.ReadSetEnd();" << '\n';
  } else if (ttype->is_list()) {
    indent_impl(out) << "iprot.ReadListEnd();" << '\n';
  }
}

void t_delphi_generator::generate_deserialize_map_element(ostream& out,
                                                          bool is_xception,
                                                          t_map* tmap,
                                                          string prefix,
                                                          ostream& local_vars) {

  string key = tmp("_key");
  string val = tmp("_val");
  string local_var;

  t_field fkey(tmap->get_key_type(), key);
  t_field fval(tmap->get_val_type(), val);

  local_vars << "  " << declare_field(&fkey) << '\n';
  local_vars << "  " << declare_field(&fval) << '\n';

  generate_deserialize_field(out, is_xception, &fkey, "", local_vars);
  generate_deserialize_field(out, is_xception, &fval, "", local_vars);

  indent_impl(out) << prefix << ".AddOrSetValue( " << key << ", " << val << ");" << '\n';
}

void t_delphi_generator::generate_deserialize_set_element(ostream& out,
                                                          bool is_xception,
                                                          t_set* tset,
                                                          string prefix,
                                                          ostream& local_vars) {
  string elem = tmp("_elem");
  t_field felem(tset->get_elem_type(), elem);
  local_vars << "  " << declare_field(&felem) << '\n';
  generate_deserialize_field(out, is_xception, &felem, "", local_vars);
  indent_impl(out) << prefix << ".Add(" << elem << ");" << '\n';
}

void t_delphi_generator::generate_deserialize_list_element(ostream& out,
                                                           bool is_xception,
                                                           t_list* tlist,
                                                           string prefix,
                                                           ostream& local_vars) {
  string elem = tmp("_elem");
  t_field felem(tlist->get_elem_type(), elem);
  local_vars << "  " << declare_field(&felem) << '\n';
  generate_deserialize_field(out, is_xception, &felem, "", local_vars);
  indent_impl(out) << prefix << ".Add(" << elem << ");" << '\n';
}

void t_delphi_generator::generate_serialize_field(ostream& out,
                                                  bool is_xception,
                                                  t_field* tfield,
                                                  string prefix,
                                                  ostream& local_vars) {
  (void)local_vars;

  t_type* type = tfield->get_type();
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }

  string name = prefix + prop_name(tfield, is_xception);

  if (type->is_void()) {
    throw "CANNOT GENERATE SERIALIZE CODE FOR void TYPE: " + name;
  }

  if (type->is_struct() || type->is_xception()) {
    generate_serialize_struct(out, (t_struct*)type, name, local_vars);
  } else if (type->is_container()) {
    generate_serialize_container(out, is_xception, type, name, local_vars);
  } else if (type->is_base_type() || type->is_enum()) {

    indent_impl(out) << "oprot.";

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();

      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " + name;
        break;
      case t_base_type::TYPE_STRING:
        if (type->is_binary()) {
          out << "WriteBinary(";
        } else {
          out << "WriteString(";
        }
        out << name << ");";
        break;
      case t_base_type::TYPE_UUID:
        out << "WriteUuid(" << name << ");";
        break;
      case t_base_type::TYPE_BOOL:
        out << "WriteBool(" << name << ");";
        break;
      case t_base_type::TYPE_I8:
        out << "WriteByte(" << name << ");";
        break;
      case t_base_type::TYPE_I16:
        out << "WriteI16(" << name << ");";
        break;
      case t_base_type::TYPE_I32:
        out << "WriteI32(" << name << ");";
        break;
      case t_base_type::TYPE_I64:
        out << "WriteI64(" << name << ");";
        break;
      case t_base_type::TYPE_DOUBLE:
        out << "WriteDouble(" << name << ");";
        break;
      default:
        throw "compiler error: no Delphi name for base type " + t_base_type::t_base_name(tbase);
      }
    } else if (type->is_enum()) {
      out << "WriteI32(System.Integer(" << name << "));";
    }
    out << '\n';
  } else {
    printf("DO NOT KNOW HOW TO SERIALIZE '%s%s' TYPE '%s'\n",
           prefix.c_str(),
           tfield->get_name().c_str(),
           type_name(type).c_str());
  }
}

void t_delphi_generator::generate_serialize_struct(ostream& out,
                                                   t_struct* tstruct,
                                                   string prefix,
                                                   ostream& local_vars) {
  (void)local_vars;
  (void)tstruct;
  out << indent_impl() << prefix << ".Write(oprot);" << '\n';
}

void t_delphi_generator::generate_serialize_container(ostream& out,
                                                      bool is_xception,
                                                      t_type* ttype,
                                                      string prefix,
                                                      ostream& local_vars) {
  string obj;
  if (ttype->is_map()) {
    obj = tmp("map");
    local_vars << "  " << obj << " : TThriftMap;" << '\n';
    indent_impl(out) << "Thrift.Protocol.Init( " << obj << ", "
                     << type_to_enum(((t_map*)ttype)->get_key_type()) << ", "
                     << type_to_enum(((t_map*)ttype)->get_val_type()) << ", " << prefix
                     << ".Count);" << '\n';
    indent_impl(out) << "oprot.WriteMapBegin( " << obj << ");" << '\n';
  } else if (ttype->is_set()) {
    obj = tmp("set_");
    local_vars << "  " << obj << " : TThriftSet;" << '\n';
    indent_impl(out) << "Thrift.Protocol.Init( " << obj << ", "
                     << type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " << prefix
                     << ".Count);" << '\n';
    indent_impl(out) << "oprot.WriteSetBegin( " << obj << ");" << '\n';
  } else if (ttype->is_list()) {
    obj = tmp("list_");
    local_vars << "  " << obj << " : TThriftList;" << '\n';
    indent_impl(out) << "Thrift.Protocol.Init( " << obj << ", "
                     << type_to_enum(((t_list*)ttype)->get_elem_type()) << ", " << prefix
                     << ".Count);" << '\n';
    indent_impl(out) << "oprot.WriteListBegin( " << obj << ");" << '\n';
  }

  string iter = tmp("_iter");
  if (ttype->is_map()) {
    local_vars << "  " << iter << ": " << type_name(((t_map*)ttype)->get_key_type()) << ";" << '\n';
    indent_impl(out) << "for " << iter << " in " << prefix << ".Keys do begin" << '\n';
    indent_up_impl();
  } else if (ttype->is_set()) {
    local_vars << "  " << iter << ": " << type_name(((t_set*)ttype)->get_elem_type()) << ";"
               << '\n';
    indent_impl(out) << "for " << iter << " in " << prefix << " do begin" << '\n';
    indent_up_impl();
  } else if (ttype->is_list()) {
    local_vars << "  " << iter << ": " << type_name(((t_list*)ttype)->get_elem_type()) << ";"
               << '\n';
    indent_impl(out) << "for " << iter << " in " << prefix << " do begin" << '\n';
    indent_up_impl();
  }

  if (ttype->is_map()) {
    generate_serialize_map_element(out, is_xception, (t_map*)ttype, iter, prefix, local_vars);
  } else if (ttype->is_set()) {
    generate_serialize_set_element(out, is_xception, (t_set*)ttype, iter, local_vars);
  } else if (ttype->is_list()) {
    generate_serialize_list_element(out, is_xception, (t_list*)ttype, iter, local_vars);
  }

  indent_down_impl();
  indent_impl(out) << "end;" << '\n';

  if (ttype->is_map()) {
    indent_impl(out) << "oprot.WriteMapEnd();" << '\n';
  } else if (ttype->is_set()) {
    indent_impl(out) << "oprot.WriteSetEnd();" << '\n';
  } else if (ttype->is_list()) {
    indent_impl(out) << "oprot.WriteListEnd();" << '\n';
  }
}

void t_delphi_generator::generate_serialize_map_element(ostream& out,
                                                        bool is_xception,
                                                        t_map* tmap,
                                                        string iter,
                                                        string map,
                                                        ostream& local_vars) {
  t_field kfield(tmap->get_key_type(), iter);
  generate_serialize_field(out, is_xception, &kfield, "", local_vars);
  t_field vfield(tmap->get_val_type(), map + "[" + iter + "]");
  generate_serialize_field(out, is_xception, &vfield, "", local_vars);
}

void t_delphi_generator::generate_serialize_set_element(ostream& out,
                                                        bool is_xception,
                                                        t_set* tset,
                                                        string iter,
                                                        ostream& local_vars) {
  t_field efield(tset->get_elem_type(), iter);
  generate_serialize_field(out, is_xception, &efield, "", local_vars);
}

void t_delphi_generator::generate_serialize_list_element(ostream& out,
                                                         bool is_xception,
                                                         t_list* tlist,
                                                         string iter,
                                                         ostream& local_vars) {
  t_field efield(tlist->get_elem_type(), iter);
  generate_serialize_field(out, is_xception, &efield, "", local_vars);
}

void t_delphi_generator::generate_property(ostream& out,
                                           t_field* tfield,
                                           bool isPublic,
                                           bool is_xception) {
  generate_delphi_property(out, is_xception, tfield, isPublic, "Get");
}

void t_delphi_generator::generate_delphi_property(ostream& out,
                                                  bool struct_is_xception,
                                                  t_field* tfield,
                                                  bool isPublic,
                                                  std::string fieldPrefix) {
  (void)isPublic;

  t_type* ftype = tfield->get_type();
  bool is_xception = ftype->is_xception();
  generate_delphi_doc(out, tfield);
  indent(out) << "property " << prop_name(tfield, struct_is_xception) << ": "
              << type_name(ftype, false, true, is_xception, true)
              << " read " << prop_name(tfield, struct_is_xception, fieldPrefix)
              << " write " << prop_name(tfield, struct_is_xception, "Set")
              << ";"
              << render_deprecation_attribute(tfield->annotations_, " {", "}")
              << '\n';
}

std::string t_delphi_generator::prop_name(t_field* tfield, bool is_xception, std::string prefix) {
  return prop_name(tfield->get_name(), is_xception, prefix);
}

std::string t_delphi_generator::prop_name(string name, bool is_xception, std::string prefix) {
  string ret = name;
  ret[0] = toupper(ret[0]);
  return normalize_name(prefix + ret, true, is_xception);
}

std::string t_delphi_generator::constructor_param_name(string name) {
  string ret = name;
  ret[0] = toupper(ret[0]);
  return normalize_name("a" + ret, false, false);
}

string t_delphi_generator::normalize_clsnm(string clsnm, string prefix, bool b_no_check_keyword) {
  if (clsnm.size() > 0) {
    clsnm[0] = toupper(clsnm[0]);
  }
  if (b_no_check_keyword) {
    return prefix + clsnm;
  } else {
    return normalize_name(prefix + clsnm);
  }
}

string t_delphi_generator::type_name(t_type* ttype,
                                     bool b_cls,
                                     bool b_no_postfix,
                                     bool b_exception_factory,
                                     bool b_full_exception_factory) {

  if (ttype->is_typedef()) {
    t_typedef* tdef = (t_typedef*)ttype;
    if (tdef->is_forward_typedef()) { // forward types according to THRIFT-2421
      if (tdef->get_type() != nullptr) {
        return type_name(tdef->get_type(),
                         b_cls,
                         b_no_postfix,
                         b_exception_factory,
                         b_full_exception_factory);
      } else {
        throw "unresolved forward declaration: " + tdef->get_symbolic();
      }
    } else {
      return normalize_name("T" + tdef->get_symbolic());
    }
  }

  string typ_nm;

  string s_factory;

  if (ttype->is_base_type()) {
    return base_type_name((t_base_type*)ttype);
  } else if (ttype->is_enum()) {
    b_cls = true;
    b_no_postfix = true;
  } else if (ttype->is_map()) {
    t_map* tmap = (t_map*)ttype;
    if (b_cls) {
      typ_nm = "TThriftDictionaryImpl";
    } else {
      typ_nm = "IThriftDictionary";
    }
    return typ_nm + "<" + type_name(tmap->get_key_type()) + ", " + type_name(tmap->get_val_type())
           + ">";
  } else if (ttype->is_set()) {
    t_set* tset = (t_set*)ttype;
    if (b_cls) {
      typ_nm = "TThriftHashSetImpl";
    } else {
      typ_nm = "IThriftHashSet";
    }
    return typ_nm + "<" + type_name(tset->get_elem_type()) + ">";
  } else if (ttype->is_list()) {
    t_list* tlist = (t_list*)ttype;
    if (b_cls) {
      typ_nm = "TThriftListImpl";
    } else {
      typ_nm = "IThriftList";
    }
    return typ_nm + "<" + type_name(tlist->get_elem_type()) + ">";
  }

  string type_prefix;

  if (b_cls) {
    type_prefix = "T";
  } else {
    type_prefix = "I";
  }

  string nm = normalize_clsnm(ttype->get_name(), type_prefix);

  if (b_exception_factory) {
    nm = nm + "Factory";
  }

  if (b_cls) {
    if (!b_no_postfix) {
      nm = nm + "Impl";
    }
  }

  if (b_exception_factory && b_full_exception_factory) {
    return type_name(ttype, true, true, false, false) + "." + nm;
  }

  return nm;
}

// returns "const " for some argument types
string t_delphi_generator::input_arg_prefix(t_type* ttype) {

  // base types
  if (ttype->is_base_type()) {
    switch (((t_base_type*)ttype)->get_base()) {

    // these should be const'ed for optimal performamce
    case t_base_type::TYPE_STRING: // refcounted pointer
    case t_base_type::TYPE_UUID:   // refcounted pointer
    case t_base_type::TYPE_I64:    // larger than 32 bit
    case t_base_type::TYPE_DOUBLE: // larger than 32 bit
      return "const ";

    // all others don't need to be
    case t_base_type::TYPE_I8:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_BOOL:
    case t_base_type::TYPE_VOID:
      return "";

    // we better always report any unknown types
    default:
      throw "compiler error: no input_arg_prefix() for base type "
          + t_base_type::t_base_name(((t_base_type*)ttype)->get_base());
    }

    // enums
  } else if (ttype->is_enum()) {
    return ""; // usually <= 32 bit

    // containers
  } else if (ttype->is_map()) {
    return "const "; // refcounted pointer

  } else if (ttype->is_set()) {
    return "const "; // refcounted pointer

  } else if (ttype->is_list()) {
    return "const "; // refcounted pointer
  }

  // any other type, either TSomething or ISomething
  return "const "; // possibly refcounted pointer
}

string t_delphi_generator::base_type_name(t_base_type* tbase) {
  switch (tbase->get_base()) {
  case t_base_type::TYPE_VOID:
    // no "void" in Delphi language
    return "";
  case t_base_type::TYPE_STRING:
    if (tbase->is_binary()) {
      if( com_types_)
        return "IThriftBytes";
      if( rtti_)
        return "Thrift.Protocol.TThriftBytes";  // has TypeInfo
      return  "SysUtils.TBytes";
    } else {
      return com_types_ ? "System.WideString" : "System.UnicodeString";
    }
  case t_base_type::TYPE_UUID:
    return "System.TGuid";
  case t_base_type::TYPE_BOOL:
    return "System.Boolean";
  case t_base_type::TYPE_I8:
    return "System.ShortInt";
  case t_base_type::TYPE_I16:
    return "System.SmallInt";
  case t_base_type::TYPE_I32:
    return "System.Integer";
  case t_base_type::TYPE_I64:
    return "System.Int64";
  case t_base_type::TYPE_DOUBLE:
    return "System.Double";
  default:
    throw "compiler error: no Delphi name for base type "
        + t_base_type::t_base_name(tbase->get_base());
  }
}

string t_delphi_generator::declare_field(t_field* tfield,
                                         bool init,
                                         std::string prefix,
                                         bool is_xception_class) {
  (void)init;

  t_type* ftype = tfield->get_type();
  bool is_xception = ftype->is_xception();

  string result = prop_name(tfield, is_xception_class, prefix) + ": "
                  + type_name(ftype, false, true, is_xception, true) + ";";
  return result;
}

string t_delphi_generator::function_signature(t_function* tfunction,
                                              bool for_async,
                                              std::string full_cls,
                                              bool is_xception) {
  t_type* ttype = tfunction->get_returntype();
  string prefix;
  if (full_cls == "") {
    prefix = "";
  } else {
    prefix = full_cls + ".";
  }

  string signature = "";

  if( for_async) {
    if (is_void(ttype)) {
      signature = "function " + prefix + normalize_name(tfunction->get_name(), true, is_xception) + "Async("
                + argument_list(tfunction->get_arglist()) + "): IFuture<Integer>;";  // no IFuture<void> in Delphi
    } else {
      signature = "function " + prefix + normalize_name(tfunction->get_name(), true, is_xception) + "Async("
                + argument_list(tfunction->get_arglist()) + "): IFuture<"
                + type_name(ttype, false, true, is_xception, true) + ">;";
    }
  } else {
    if (is_void(ttype)) {
      signature = "procedure " + prefix + normalize_name(tfunction->get_name(), true, is_xception) + "("
                + argument_list(tfunction->get_arglist()) + ");";
    } else {
      signature = "function " + prefix + normalize_name(tfunction->get_name(), true, is_xception) + "("
                + argument_list(tfunction->get_arglist()) + "): "
                + type_name(ttype, false, true, is_xception, true) + ";";
    }
  }

  // deprecated method? only at intf decl!
  if( full_cls == "") {
    signature += render_deprecation_attribute(tfunction->annotations_, " ", ";");
  }

  return signature;
}

string t_delphi_generator::argument_list(t_struct* tstruct) {
  string result = "";
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  bool first = true;
  t_type* tt;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      result += "; ";
    }

    tt = (*f_iter)->get_type();
    result += input_arg_prefix(tt); // const?
    result += normalize_name((*f_iter)->get_name()) + ": "
              + type_name(tt, false, true, tt->is_xception(), true);
  }
  return result;
}

string t_delphi_generator::constructor_argument_list(t_struct* tstruct, string current_indent) {
  ostringstream result;
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  bool first = true;
  t_type* tt;
  string line = "";
  string newline_indent = current_indent + "  ";

  bool firstline = true;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      line += ";";
    }

    if (line.size() > 80) {
      if (firstline) {
        result << '\n' << newline_indent;
        firstline = false;
      }
      result << line << '\n';
      line = newline_indent;
    } else if (line.size() > 0) {
      line += " ";
    }

    tt = (*f_iter)->get_type();
    line += input_arg_prefix(tt); // const?
    line += constructor_param_name((*f_iter)->get_name()) + ": "
            + type_name(tt, false, true, tt->is_xception(), true);
  }

  if (line.size() > 0) {
    result << line;
  }

  string result_str;

  if (firstline) {
    result_str = " " + result.str();
  } else {
    result_str = result.str();
  }

  return result_str;
}

string t_delphi_generator::type_to_enum(t_type* type) {
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "NO T_VOID CONSTRUCT";
    case t_base_type::TYPE_STRING:
      return "TType.String_";
    case t_base_type::TYPE_UUID:
      return "TType.Uuid";
    case t_base_type::TYPE_BOOL:
      return "TType.Bool_";
    case t_base_type::TYPE_I8:
      return "TType.Byte_";
    case t_base_type::TYPE_I16:
      return "TType.I16";
    case t_base_type::TYPE_I32:
      return "TType.I32";
    case t_base_type::TYPE_I64:
      return "TType.I64";
    case t_base_type::TYPE_DOUBLE:
      return "TType.Double_";
    }
  } else if (type->is_enum()) {
    return "TType.I32";
  } else if (type->is_struct() || type->is_xception()) {
    return "TType.Struct";
  } else if (type->is_map()) {
    return "TType.Map";
  } else if (type->is_set()) {
    return "TType.Set_";
  } else if (type->is_list()) {
    return "TType.List";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}

string t_delphi_generator::empty_value(t_type* type) {
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      return "0";
    case t_base_type::TYPE_STRING:
      if (type->is_binary()) {
        return "nil";
      } else {
        return "''";
      }
    case t_base_type::TYPE_UUID:
      return "System.TGuid.Empty";
    case t_base_type::TYPE_BOOL:
      return "False";
    case t_base_type::TYPE_I8:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
      return "0";
    case t_base_type::TYPE_DOUBLE:
      return "0.0";
    }
  } else if (type->is_enum()) {
    return "T" + type->get_name() + "(0)";
  } else if (type->is_struct() || type->is_xception()) {
    return "nil";
  } else if (type->is_map()) {
    return "nil";
  } else if (type->is_set()) {
    return "nil";
  } else if (type->is_list()) {
    return "nil";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}

void t_delphi_generator::generate_delphi_property_writer_definition(ostream& out,
                                                                    t_field* tfield,
                                                                    bool is_xception_class) {
  t_type* ftype = tfield->get_type();
  bool is_xception = ftype->is_xception();

  indent(out) << "procedure " << prop_name(tfield, is_xception_class, "Set")
              << "( const Value: " << type_name(ftype, false, true, is_xception, true) << ");"
              << render_deprecation_attribute(tfield->annotations_, " ", ";")
              << '\n';
}

void t_delphi_generator::generate_delphi_property_reader_definition(ostream& out,
                                                                    t_field* tfield,
                                                                    bool is_xception_class) {
  t_type* ftype = tfield->get_type();
  bool is_xception = ftype->is_xception();

  indent(out) << "function " << prop_name(tfield, is_xception_class, "Get") << ": "
              << type_name(ftype, false, true, is_xception, true) << ";" 
              << render_deprecation_attribute(tfield->annotations_, " ", ";")
              << '\n';
}

void t_delphi_generator::generate_delphi_isset_reader_writer_definition(ostream& out,
                                                                 t_field* tfield,
                                                                 bool is_xception) {
  indent(out) << "function " << prop_name(tfield, is_xception,"Get__isset_") << ": System.Boolean;" << '\n';
  indent(out) << "procedure " << prop_name(tfield, is_xception, "Set__isset_") << "( const value : System.Boolean);" << '\n';
}

void t_delphi_generator::generate_delphi_clear_union_value(ostream& out,
                                                           std::string cls_prefix,
                                                           std::string name,
                                                           t_type* type,
                                                           t_field* tfield,
                                                           std::string fieldPrefix,
                                                           bool is_xception_class,
                                                           bool is_union,
                                                           bool is_xception_factory,
                                                           std::string xception_factory_name) {
  (void)cls_prefix;
  (void)name;
  (void)type;
  (void)is_union;
  (void)is_xception_factory;
  (void)xception_factory_name;

  t_type* ftype = tfield->get_type();
  bool is_xception = ftype->is_xception();

  indent_impl(out) << "if " << prop_name(tfield, is_xception_class,"F__isset_") << " then begin"
                   << '\n';
  indent_up_impl();
  indent_impl(out) << prop_name(tfield, is_xception_class,"F__isset_") << " := False;" << '\n';
  indent_impl(out) << prop_name(tfield, is_xception_class,fieldPrefix) << " := "
                   << "Default( " << type_name(ftype, false, true, is_xception, true) << ");"
                   << '\n';
  indent_down_impl();
  indent_impl(out) << "end;" << '\n';
}

void t_delphi_generator::generate_delphi_property_writer_impl(ostream& out,
                                                              std::string cls_prefix,
                                                              std::string name,
                                                              t_type* type,
                                                              t_field* tfield,
                                                              std::string fieldPrefix,
                                                              bool is_xception_class,
                                                              bool is_union,
                                                              bool is_xception_factory,
                                                              std::string xception_factory_name) {
  (void)type;

  t_type* ftype = tfield->get_type();
  bool is_xception = ftype->is_xception();

  indent_impl(out) << "procedure " << cls_prefix << name << "."
                   << prop_name(tfield, is_xception_class,"Set")
                   << "( const Value: " << type_name(ftype, false, true, is_xception, true) << ");"
                   << '\n';
  indent_impl(out) << "begin" << '\n';
  indent_up_impl();
  if (is_union) {
    indent_impl(out) << "ClearUnionValues;" << '\n';
  }
  if (tfield->get_req() != t_field::T_REQUIRED) {
    indent_impl(out) << prop_name(tfield, is_xception_class,"F__isset_") << " := True;" << '\n';
  }
  indent_impl(out) << prop_name(tfield, is_xception_class,fieldPrefix) << " := Value;" << '\n';

  if (is_xception_class && (!is_xception_factory)) {
    indent_impl(out) << xception_factory_name << "." << prop_name(tfield, is_xception_class)
                     << " := Value;" << '\n';
  }

  indent_down_impl();
  indent_impl(out) << "end;" << '\n' << '\n';
}

void t_delphi_generator::generate_delphi_property_reader_impl(ostream& out,
                                                              std::string cls_prefix,
                                                              std::string name,
                                                              t_type* type,
                                                              t_field* tfield,
                                                              std::string fieldPrefix,
                                                              bool is_xception_class) {
  (void)type;

  t_type* ftype = tfield->get_type();
  bool is_xception = ftype->is_xception();

  indent_impl(out) << "function " << cls_prefix << name << "."
                   << prop_name(tfield, is_xception_class,"Get") << ": "
                   << type_name(ftype, false, true, is_xception, true) << ";" << '\n';
  indent_impl(out) << "begin" << '\n';
  indent_up_impl();
  indent_impl(out) << "Result := " << prop_name(tfield, is_xception_class,fieldPrefix) << ";"
                   << '\n';
  indent_down_impl();
  indent_impl(out) << "end;" << '\n' << '\n';
}

void t_delphi_generator::generate_delphi_isset_reader_writer_impl(ostream& out,
                                                           std::string cls_prefix,
                                                           std::string name,
                                                           t_type* type,
                                                           t_field* tfield,
                                                           std::string fieldPrefix,
                                                           bool is_xception) {
  (void)type;

  string isset_name = prop_name(tfield, is_xception, "__isset_");

  indent_impl(out) << "function " << cls_prefix << name << "."
                   << "Get" << isset_name << ": System.Boolean;" << '\n';
  indent_impl(out) << "begin" << '\n';
  indent_up_impl();
  indent_impl(out) << "Result := " << fieldPrefix << isset_name << ";" << '\n';
  indent_down_impl();
  indent_impl(out) << "end;" << '\n' << '\n';

  indent_impl(out) << "procedure " << cls_prefix << name << "."
                   << "Set" << isset_name << "( const value: System.Boolean);" << '\n';
  indent_impl(out) << "begin" << '\n';
  indent_up_impl();
  indent_impl(out) << fieldPrefix << isset_name << " := value;" << '\n';
  indent_down_impl();
  indent_impl(out) << "end;" << '\n' << '\n';
}

void t_delphi_generator::generate_delphi_create_exception_impl(ostream& out,
                                                               string cls_prefix,
                                                               t_struct* tstruct,
                                                               bool is_exception) {
  (void)cls_prefix;

  string exception_cls_nm = type_name(tstruct, true, true);
  string cls_nm = type_name(tstruct, true, false, is_exception, is_exception);

  indent_impl(out) << "function " << cls_nm << ".CreateException: " << exception_cls_nm << ";"
                   << '\n';

  indent_impl(out) << "begin" << '\n';
  indent_up_impl();

  indent_impl(out) << "Result := " << exception_cls_nm << ".Create;" << '\n';
  string factory_name = normalize_clsnm(tstruct->get_name(), "", true) + "Factory";
  indent_impl(out) << "Result.F" << factory_name << " := Self;" << '\n';

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  string propname;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    propname = prop_name(*f_iter, is_exception);
    if ((*f_iter)->get_req() != t_field::T_REQUIRED) {
      indent_impl(out) << "if " << prop_name(*f_iter, is_exception,"__isset_") << " then begin" << '\n';
      indent_up_impl();
    }
    indent_impl(out) << "Result." << propname << " := " << propname << ";" << '\n';
    if ((*f_iter)->get_req() != t_field::T_REQUIRED) {
      indent_down_impl();
      indent_impl(out) << "end;" << '\n';
    }
  }

  indent_impl(out) << "Result.UpdateMessageProperty;" << '\n';

  indent_down_impl();
  indent_impl(out) << "end;" << '\n' << '\n';
}

void t_delphi_generator::generate_delphi_struct_reader_impl(ostream& out,
                                                            string cls_prefix,
                                                            t_struct* tstruct,
                                                            bool is_exception,
                                                            bool is_x_factory) {

  ostringstream local_vars;
  ostringstream code_block;

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  indent_impl(code_block) << "begin" << '\n';
  indent_up_impl();

  indent_impl(local_vars) << "tracker : IProtocolRecursionTracker;" << '\n';
  indent_impl(code_block) << "tracker := iprot.NextRecursionLevel;" << '\n';

  // local bools for required fields
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      indent_impl(local_vars) << prop_name(*f_iter, is_exception,"_req_isset_") << " : System.Boolean;"
                              << '\n';
      indent_impl(code_block) << prop_name(*f_iter, is_exception,"_req_isset_") << " := FALSE;"
                              << '\n';
    }
  }

  indent_impl(code_block) << "struc := iprot.ReadStructBegin;" << '\n';

  indent_impl(code_block) << "try" << '\n';
  indent_up_impl();

  indent_impl(code_block) << "while (true) do begin" << '\n';
  indent_up_impl();

  indent_impl(code_block) << "field_ := iprot.ReadFieldBegin();" << '\n';

  indent_impl(code_block) << "if (field_.Type_ = TType.Stop) then Break;" << '\n';

  bool first = true;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {

    if (first) {
      code_block << '\n';
      indent_impl(code_block) << "case field_.ID of" << '\n';
      indent_up_impl();
    }

    first = false;
    if (f_iter != fields.begin()) {
      code_block << '\n';
    }

    indent_impl(code_block) << (*f_iter)->get_key() << ": begin" << '\n';
    indent_up_impl();
    indent_impl(code_block) << "if (field_.Type_ = " << type_to_enum((*f_iter)->get_type())
                            << ") then begin" << '\n';
    indent_up_impl();

    generate_deserialize_field(code_block, is_exception, *f_iter, "Self.", local_vars);

    // required field?
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      indent_impl(code_block) << prop_name(*f_iter, is_exception,"_req_isset_") << " := TRUE;"
                              << '\n';
    }

    indent_down_impl();

    indent_impl(code_block) << "end else begin" << '\n';
    indent_up_impl();
    indent_impl(code_block) << "TProtocolUtil.Skip(iprot, field_.Type_);" << '\n';
    indent_down_impl();
    indent_impl(code_block) << "end;" << '\n';
    indent_down_impl();
    indent_impl(code_block) << "end;";
  }

  if (!first) {
    code_block << '\n';
    indent_down_impl();
    indent_impl(code_block) << "else" << '\n';
    indent_up_impl();
  }

  indent_impl(code_block) << "TProtocolUtil.Skip(iprot, field_.Type_);" << '\n';

  if (!first) {
    indent_down_impl();
    indent_impl(code_block) << "end;" << '\n';
  }

  indent_impl(code_block) << "iprot.ReadFieldEnd;" << '\n';

  indent_down_impl();

  indent_impl(code_block) << "end;" << '\n';
  indent_down_impl();

  indent_impl(code_block) << "finally" << '\n';
  indent_up_impl();
  indent_impl(code_block) << "iprot.ReadStructEnd;" << '\n';
  indent_down_impl();
  indent_impl(code_block) << "end;" << '\n';

  // all required fields have been read?
  first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      if(first) {
        code_block << '\n';
        first = false;
      }
      indent_impl(code_block) << "if not " << prop_name(*f_iter, is_exception,"_req_isset_") << '\n';
      indent_impl(code_block)
          << "then raise TProtocolExceptionInvalidData.Create("
          << "'required field " << prop_name(*f_iter, is_exception) << " not set');"
          << '\n';
    }
  }

  if( is_exception && (!is_x_factory)) {
    code_block << '\n';
    indent_impl(code_block) << "UpdateMessageProperty;" << '\n';
  }
  indent_down_impl();
  indent_impl(code_block) << "end;" << '\n' << '\n';

  string cls_nm;

  cls_nm = type_name(tstruct, true, is_exception && (!is_x_factory), is_x_factory, is_x_factory);

  indent_impl(out) << "procedure " << cls_prefix << cls_nm << ".Read( const iprot: IProtocol);"
                   << '\n';
  indent_impl(out) << "var" << '\n';
  indent_up_impl();
  indent_impl(out) << "field_ : TThriftField;" << '\n';
  indent_impl(out) << "struc : TThriftStruct;" << '\n';
  indent_down_impl();
  out << local_vars.str() << '\n';
  out << code_block.str();
}

void t_delphi_generator::generate_delphi_struct_result_writer_impl(ostream& out,
                                                                   string cls_prefix,
                                                                   t_struct* tstruct,
                                                                   bool is_exception,
                                                                   bool is_x_factory) {

  ostringstream local_vars;
  ostringstream code_block;

  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator f_iter;

  indent_impl(code_block) << "begin" << '\n';
  indent_up_impl();

  indent_impl(local_vars) << "tracker : IProtocolRecursionTracker;" << '\n';
  indent_impl(code_block) << "tracker := oprot.NextRecursionLevel;" << '\n';

  indent_impl(code_block) << "Thrift.Protocol.Init( struc, '" << name << "');" << '\n';
  indent_impl(code_block) << "oprot.WriteStructBegin(struc);" << '\n';

  if (fields.size() > 0) {
    indent_impl(code_block) << "Thrift.Protocol.Init( field_);" << '\n';
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
      indent_impl(code_block) << "if (" << prop_name(*f_iter, is_exception,"__isset_") << ") then"
                              << '\n';
      indent_impl(code_block) << "begin" << '\n';
      indent_up_impl();
      indent_impl(code_block) << "field_.Name := '" << (*f_iter)->get_name() << "';" << '\n';
      indent_impl(code_block) << "field_.Type_  := " << type_to_enum((*f_iter)->get_type()) << ";"
                              << '\n';
      indent_impl(code_block) << "field_.ID := " << (*f_iter)->get_key() << ";" << '\n';
      indent_impl(code_block) << "oprot.WriteFieldBegin(field_);" << '\n';
      generate_serialize_field(code_block, is_exception, *f_iter, "Self.", local_vars);
      indent_impl(code_block) << "oprot.WriteFieldEnd();" << '\n';
      indent_down_impl();
    }
  }

  indent_impl(code_block) << "oprot.WriteFieldStop();" << '\n';
  indent_impl(code_block) << "oprot.WriteStructEnd();" << '\n';

  indent_down_impl();
  indent_impl(code_block) << "end;" << '\n' << '\n';

  string cls_nm;

  cls_nm = type_name(tstruct, true, is_exception && (!is_x_factory), is_x_factory, is_x_factory);

  indent_impl(out) << "procedure " << cls_prefix << cls_nm << ".Write( const oprot: IProtocol);"
                   << '\n';
  indent_impl(out) << "var" << '\n';
  indent_up_impl();
  indent_impl(out) << "struc : TThriftStruct;" << '\n';

  if (fields.size() > 0) {
    indent_impl(out) << "field_ : TThriftField;" << '\n';
  }

  out << local_vars.str();
  indent_down_impl();
  out << code_block.str();
}

void t_delphi_generator::generate_delphi_struct_writer_impl(ostream& out,
                                                            string cls_prefix,
                                                            t_struct* tstruct,
                                                            bool is_exception,
                                                            bool is_x_factory) {

  ostringstream local_vars;
  ostringstream code_block;

  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator f_iter;

  indent_impl(code_block) << "begin" << '\n';
  indent_up_impl();

  indent_impl(local_vars) << "tracker : IProtocolRecursionTracker;" << '\n';
  indent_impl(code_block) << "tracker := oprot.NextRecursionLevel;" << '\n';

  indent_impl(code_block) << "Thrift.Protocol.Init( struc, '" << name << "');" << '\n';
  indent_impl(code_block) << "oprot.WriteStructBegin(struc);" << '\n';

  if (fields.size() > 0) {
    indent_impl(code_block) << "Thrift.Protocol.Init( field_);" << '\n';
  }

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    string fieldname = prop_name((*f_iter), is_exception);
    string isset_name = prop_name((*f_iter), is_exception, "__isset_");
    bool null_allowed = type_can_be_null((*f_iter)->get_type());
    bool is_required = ((*f_iter)->get_req() == t_field::T_REQUIRED);
    bool has_isset = (!is_required);
    if (is_required && null_allowed) {
      null_allowed = false;
      indent_impl(code_block) << "if (Self." << fieldname << " = nil)" << '\n';
      indent_impl(code_block) << "then raise TProtocolExceptionInvalidData.Create("
                              << "'required field " << fieldname << " not set');"
                              << '\n';
    }
    if (null_allowed) {
      indent_impl(code_block) << "if (Self." << fieldname << " <> nil)";
      if (has_isset) {
        code_block << " and " << isset_name;
      }
      code_block << " then begin" << '\n';
      indent_up_impl();
    } else {
      if (has_isset) {
        indent_impl(code_block) << "if (" << isset_name << ") then begin" << '\n';
        indent_up_impl();
      }
    }
    indent_impl(code_block) << "field_.Name := '" << (*f_iter)->get_name() << "';" << '\n';
    indent_impl(code_block) << "field_.Type_  := " << type_to_enum((*f_iter)->get_type()) << ";"
                            << '\n';
    indent_impl(code_block) << "field_.ID := " << (*f_iter)->get_key() << ";" << '\n';
    indent_impl(code_block) << "oprot.WriteFieldBegin(field_);" << '\n';
    generate_serialize_field(code_block, is_exception, *f_iter, "Self.", local_vars);
    indent_impl(code_block) << "oprot.WriteFieldEnd();" << '\n';
    if (null_allowed || has_isset) {
      indent_down_impl();
      indent_impl(code_block) << "end;" << '\n';
    }
  }

  indent_impl(code_block) << "oprot.WriteFieldStop();" << '\n';
  indent_impl(code_block) << "oprot.WriteStructEnd();" << '\n';

  indent_down_impl();
  indent_impl(code_block) << "end;" << '\n' << '\n';

  string cls_nm;

  cls_nm = type_name(tstruct, true, is_exception && (!is_x_factory), is_x_factory, is_x_factory);

  indent_impl(out) << "procedure " << cls_prefix << cls_nm << ".Write( const oprot: IProtocol);"
                   << '\n';
  indent_impl(out) << "var" << '\n';
  indent_up_impl();
  indent_impl(out) << "struc : TThriftStruct;" << '\n';
  if (fields.size() > 0) {
    indent_impl(out) << "field_ : TThriftField;" << '\n';
  }
  out << local_vars.str();
  indent_down_impl();
  out << code_block.str();
}

void t_delphi_generator::generate_delphi_struct_tostring_impl(ostream& out,
                                                              string cls_prefix,
                                                              t_struct* tstruct,
                                                              bool is_exception,
                                                              bool is_x_factory) {

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  string cls_nm;

  if (is_exception) {
    cls_nm = type_name(tstruct, true, (!is_x_factory), is_x_factory, true);
  } else {
    cls_nm = type_name(tstruct, true, false);
  }

  string tmp_sb = tmp("_sb");
  string tmp_first = tmp("_first");
  bool useFirstFlag = false;

  indent_impl(out) << "function " << cls_prefix << cls_nm << ".ToString: string;" << '\n';
  indent_impl(out) << "var" << '\n';
  indent_up_impl();
  indent_impl(out) << tmp_sb << " : TThriftStringBuilder;" << '\n';
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    bool is_optional = ((*f_iter)->get_req() != t_field::T_REQUIRED);
    if (is_optional) {
      indent_impl(out) << tmp_first << " : System.Boolean;" << '\n';
      useFirstFlag = true;
    }
    break;
  }
  indent_down_impl();
  indent_impl(out) << "begin" << '\n';
  indent_up_impl();

  indent_impl(out) << tmp_sb << " := TThriftStringBuilder.Create('(');" << '\n';
  indent_impl(out) << "try" << '\n';
  indent_up_impl();

  if (useFirstFlag) {
    indent_impl(out) << tmp_first << " := TRUE;" << '\n';
  }

  bool had_required = false; // set to true after first required field has been processed

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    bool null_allowed = type_can_be_null((*f_iter)->get_type());
    bool is_optional = ((*f_iter)->get_req() != t_field::T_REQUIRED);
    if (null_allowed) {
      indent_impl(out) << "if (Self." << prop_name((*f_iter), is_exception) << " <> nil)";
      if (is_optional) {
        out << " and " << prop_name(*f_iter, is_exception,"__isset_");
      }
      out << " then begin" << '\n';
      indent_up_impl();
    } else {
      if (is_optional) {
        indent_impl(out) << "if (" << prop_name(*f_iter, is_exception, "__isset_") << ") then begin"
                         << '\n';
        indent_up_impl();
      }
    }

    if (useFirstFlag && (!had_required)) {
      indent_impl(out) << "if not " << tmp_first << " then " << tmp_sb << ".Append(',');" << '\n';
      if (is_optional) {
        indent_impl(out) << tmp_first << " := FALSE;" << '\n';
      }
      indent_impl(out) << tmp_sb << ".Append('" << prop_name((*f_iter), is_exception) << ": ');"
                       << '\n';
    } else {
      indent_impl(out) << tmp_sb << ".Append(', " << prop_name((*f_iter), is_exception) << ": ');"
                       << '\n';
    }

    t_type* ttype = (*f_iter)->get_type();
    while (ttype->is_typedef()) {
      ttype = ((t_typedef*)ttype)->get_type();
    }

    if (ttype->is_xception() || ttype->is_struct()) {
      indent_impl(out) << "if (Self." << prop_name((*f_iter), is_exception) << " = nil) then " << tmp_sb
                       << ".Append('<null>') else " << tmp_sb << ".Append( Self."
                       << prop_name((*f_iter), is_exception) << ".ToString());" << '\n';
    } else if (ttype->is_enum()) {
      indent_impl(out) << tmp_sb << ".Append(EnumUtils<"
                       << type_name(ttype, false, true, false, false)
                       << ">.ToString( System.Ord( Self."
                       << prop_name((*f_iter), is_exception) << ")));" << '\n';
    } else if (ttype->is_uuid()) {
      indent_impl(out) << tmp_sb << ".Append( GUIDToString(Self." << prop_name((*f_iter), is_exception) << "));"
                       << '\n';
    } else {
      indent_impl(out) << tmp_sb << ".Append( Self." << prop_name((*f_iter), is_exception) << ");"
                       << '\n';
    }

    if (null_allowed || is_optional) {
      indent_down_impl();
      indent_impl(out) << "end;" << '\n';
    }

    if (!is_optional) {
      had_required = true; // now __first must be false, so we don't need to check it anymore
    }
  }

  indent_impl(out) << tmp_sb << ".Append(')');" << '\n';
  indent_impl(out) << "Result := " << tmp_sb << ".ToString;" << '\n';
  if (useFirstFlag) {
    indent_impl(out) << "if " << tmp_first << " then {prevent warning};" << '\n';
  }

  indent_down_impl();
  indent_impl(out) << "finally" << '\n';
  indent_up_impl();
  indent_impl(out) << tmp_sb << ".Free;" << '\n';
  indent_down_impl();
  indent_impl(out) << "end;" << '\n';

  indent_down_impl();
  indent_impl(out) << "end;" << '\n' << '\n';
}

bool t_delphi_generator::is_void(t_type* type) {
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    if (tbase == t_base_type::TYPE_VOID) {
      return true;
    }
  }
  return false;
}

std::string t_delphi_generator::display_name() const {
  return "Delphi";
}


bool t_delphi_generator::is_deprecated(std::map<std::string, std::vector<std::string>>& annotations)
{
  auto iter = annotations.find("deprecated");
  return (annotations.end() != iter);
}

std::string t_delphi_generator::render_deprecation_attribute(std::map<std::string, std::vector<std::string>>& annotations, std::string prefix, std::string postfix)
{
  std::string result = "";
  auto iter = annotations.find("deprecated");
  if( annotations.end() != iter) {
    result += prefix;
    result += "deprecated";

    // empty annotation values end up with "1" somewhere, ignore these as well
    if ((iter->second.back().length() > 0) && (iter->second.back() != "1")) {
      result += " " + make_pascal_string_literal(iter->second.back());
    }

    result += postfix;
  }
  return result;
}




THRIFT_REGISTER_GENERATOR(
    delphi,
    "Delphi",
    "    register_types:  Enable TypeRegistry, allows for creation of struct, union\n"
    "                     and container instances by interface or TypeInfo()\n"
    "    constprefix:     Name TConstants classes after IDL to reduce ambiguities\n"
    "    events:          Enable and use processing events in the generated code.\n"
    "    xmldoc:          Enable XMLDoc comments for Help Insight etc.\n"
    "    async:           Generate IAsync interface to use Parallel Programming Library (XE7+ only).\n"
    "    com_types:       Use COM-compatible data types (e.g. WideString).\n"
    "    old_names:       Compatibility: generate \"reserved\" identifiers with '_' postfix instead of '&' prefix.\n"
    "    rtti:            Activate {$TYPEINFO} and {$RTTI} at the generated API interfaces.\n")
