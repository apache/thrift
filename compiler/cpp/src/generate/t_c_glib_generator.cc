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
#include <string>
#include <vector>

#include <ctype.h>

#include "platform.h"
#include "t_oop_generator.h"

using namespace std;

/* forward declarations */
string initial_caps_to_underscores(string name);
string to_upper_case(string name);
string to_lower_case(string name);

/**
 * C code generator, using glib for C typing.
 */
class t_c_glib_generator : public t_oop_generator {
 public:

  /* constructor */
  t_c_glib_generator(t_program *program,
                     const map<string, string> &parsed_options,
                     const string &option_string) : t_oop_generator(program)
  {
    (void) parsed_options;
    (void) option_string;
    /* set the output directory */
    this->out_dir_base_ = "gen-c_glib";

    /* set the namespace */
    this->nspace = program_->get_namespace("c_glib");

    if (this->nspace.empty()) {
      this->nspace = "";
      this->nspace_u = "";
      this->nspace_uc = "";
      this->nspace_lc = "";
    } else {
      /* replace dots with underscores */
      char *tmp = strdup(this->nspace.c_str());
      for (unsigned int i = 0; i < strlen(tmp); i++) {
        if (tmp[i] == '.') {
          tmp[i] = '_';
        }
      }
      this->nspace = string(tmp, strlen(tmp));
      free(tmp);

      /* clean up the namespace for C.
       * An input of 'namespace foo' should result in:
       *  - nspace = foo       - for thrift objects and typedefs
       *  - nspace_u = Foo     - for internal GObject prefixes
       *  - nspace_uc = FOO_   - for macro prefixes
       *  - nspace_lc = foo_   - for filename and method prefixes
       * The underscores are there since uc and lc strings are used as file and
       * variable prefixes.
       */
      this->nspace_u = initial_caps_to_underscores(this->nspace);
      this->nspace_uc = to_upper_case(this->nspace_u) + "_";
      this->nspace_lc = to_lower_case(this->nspace_u) + "_";
    }
  }

  /* initialization and destruction */
  void init_generator();
  void close_generator();

  /* generation functions */
  void generate_typedef(t_typedef *ttypedef);
  void generate_enum(t_enum *tenum);
  void generate_consts(vector<t_const *> consts);
  void generate_struct(t_struct *tstruct);
  void generate_service(t_service *tservice);
  void generate_xception(t_struct *tstruct);

 private:

  /* file streams */
  ofstream f_types_;
  ofstream f_types_impl_;
  ofstream f_header_;
  ofstream f_service_;

  /* namespace variables */
  string nspace;
  string nspace_u;
  string nspace_uc;
  string nspace_lc;

  /* helper functions */
  bool is_complex_type(t_type *ttype);
  string type_name(t_type* ttype, bool in_typedef=false, bool is_const=false);
  string base_type_name(t_base_type *type);
  string type_to_enum(t_type *type);
  string constant_value(string name, t_type *type, t_const_value *value);
  string function_signature(t_function *tfunction);
  string argument_list(t_struct *tstruct);
  string xception_list(t_struct *tstruct);
  string declare_field(t_field *tfield, bool init=false, bool pointer=false, bool constant=false, bool reference=false);

  /* generation functions */
  void generate_const_initializer(string name, t_type *type, t_const_value *value);
  void generate_service_client(t_service *tservice);
  void generate_service_server(t_service *tservice);
  void generate_object(t_struct *tstruct);
  void generate_struct_writer(ofstream &out, t_struct *tstruct, string this_name, string this_get="", bool is_function=true);
  void generate_struct_reader(ofstream &out, t_struct *tstruct, string this_name, string this_get="", bool is_function=true);

  void generate_serialize_field(ofstream &out, t_field *tfield, string prefix, string suffix, int error_ret);
  void generate_serialize_struct(ofstream &out, t_struct *tstruct, string prefix, int error_ret);
  void generate_serialize_container(ofstream &out, t_type *ttype, string prefix, int error_ret);
  void generate_serialize_map_element(ofstream &out, t_map *tmap, string key, string value, int error_ret);
  void generate_serialize_set_element(ofstream &out, t_set *tset, string element, int error_ret);
  void generate_serialize_list_element(ofstream &out, t_list *tlist, string list, string index, int error_ret);

  void generate_deserialize_field(ofstream &out, t_field *tfield, string prefix, string suffix, int error_ret);
  void generate_deserialize_struct(ofstream &out, t_struct *tstruct, string prefix, int error_ret);
  void generate_deserialize_container(ofstream &out, t_type *ttype, string prefix, int error_ret);
  void generate_deserialize_map_element(ofstream &out, t_map *tmap, string prefix, int error_ret);
  void generate_deserialize_set_element(ofstream &out, t_set *tset, string prefix, int error_ret);
  void generate_deserialize_list_element(ofstream &out, t_list *tlist, string prefix, string index, int error_ret);

  string generate_new_hash_from_type(t_type * ttype);
  string generate_new_array_from_type(t_type * ttype); 
};

/**
 * Prepare for file generation by opening up the necessary file
 * output streams.
 */
void t_c_glib_generator::init_generator() {
  /* create output directory */
  MKDIR(get_out_dir().c_str());

  string program_name_u = initial_caps_to_underscores(program_name_);
  string program_name_uc = to_upper_case(program_name_u);
  string program_name_lc = to_lower_case(program_name_u);

  /* create output files */
  string f_types_name = get_out_dir() + this->nspace_lc
                        + program_name_lc + "_types.h";
  f_types_.open(f_types_name.c_str());
  string f_types_impl_name = get_out_dir() + this->nspace_lc
                             + program_name_lc + "_types.c";
  f_types_impl_.open(f_types_impl_name.c_str());

  /* add thrift boilerplate headers */
  f_types_ << autogen_comment();
  f_types_impl_ << autogen_comment();

  /* include inclusion guard */
  f_types_ << 
    "#ifndef " << this->nspace_uc << program_name_uc << "_TYPES_H" << endl <<
    "#define " << this->nspace_uc << program_name_uc << "_TYPES_H" << endl <<
    endl;

  /* include base types */
  f_types_ <<
    "/* base includes */" << endl <<
    "#include <glib-object.h>" << endl <<
    "#include <thrift_struct.h>" << endl <<
    "#include <protocol/thrift_protocol.h>" << endl;

  /* include other thrift includes */
  const vector<t_program *> &includes = program_->get_includes();
  for (size_t i = 0; i < includes.size(); ++i) {
    f_types_ <<
      "/* other thrift includes */" << endl <<
      "#include \"" << this->nspace_lc << includes[i]->get_name() <<
          "_types.h\"" << endl;
  }
  f_types_ << endl;

  /* include custom headers */
  const vector<string> &c_includes = program_->get_c_includes();
  f_types_ << "/* custom thrift includes */" << endl;
  for (size_t i = 0; i < c_includes.size(); ++i) {
    if (c_includes[i][0] == '<') {
      f_types_ <<
        "#include " << c_includes[i] << endl;
    } else {
      f_types_ <<
        "#include \"" << c_includes[i] << "\"" << endl;
    }
  }
  f_types_ << endl;

  // include the types file
  f_types_impl_ <<
    endl <<
    "#include \"" << this->nspace_lc << program_name_u << 
        "_types.h\"" << endl <<
    "#include <thrift.h>" << endl <<
    endl;

  f_types_ <<
    "/* begin types */" << endl << endl;
}

/**
 *  Finish up generation and close all file streams.
 */
void t_c_glib_generator::close_generator() {
  string program_name_uc = to_upper_case 
    (initial_caps_to_underscores(program_name_));

  /* end the header inclusion guard */
  f_types_ <<
    "#endif /* " << this->nspace_uc << program_name_uc << "_TYPES_H */" << endl;

  /* close output file */
  f_types_.close();
  f_types_impl_.close();
}

/**
 * Generates a Thrift typedef in C code.  For example:
 * 
 * Thrift: 
 * typedef map<i32,i32> SomeMap
 * 
 * C: 
 * typedef GHashTable * ThriftSomeMap;
 */
void t_c_glib_generator::generate_typedef(t_typedef* ttypedef) {
  f_types_ <<
    indent() << "typedef " << type_name(ttypedef->get_type(), true) <<
        " " << this->nspace << ttypedef->get_symbolic() << ";" << endl <<
    endl;
} 

/**
 * Generates a C enumeration.  For example:
 *
 * Thrift:
 * enum MyEnum {
 *   ONE = 1,
 *   TWO
 * }
 *
 * C:
 * enum _ThriftMyEnum {
 *   THRIFT_MY_ENUM_ONE = 1,
 *   THRIFT_MY_ENUM_TWO
 * };
 * typedef enum _ThriftMyEnum ThriftMyEnum;
 */
void t_c_glib_generator::generate_enum(t_enum *tenum) {
  string name = tenum->get_name();
  string name_uc = to_upper_case(initial_caps_to_underscores(name));

  f_types_ <<
    indent() << "enum _" << this->nspace << name << " {" << endl;

  indent_up();

  vector<t_enum_value *> constants = tenum->get_constants();
  vector<t_enum_value *>::iterator c_iter;
  bool first = true;

  /* output each of the enumeration elements */
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    if (first) {
      first = false;
    } else {
      f_types_ << "," << endl;
    }

    f_types_ <<
      indent() << this->nspace_uc << name_uc << "_" << (*c_iter)->get_name();
    if ((*c_iter)->has_value()) {
      f_types_ <<
        " = " << (*c_iter)->get_value();
    }
  }

  indent_down();
  f_types_ <<
    endl <<
    "};" << endl <<
    "typedef enum _" << this->nspace << name << " " << this->nspace << name << ";" << endl <<
    endl;
}

/**
 * Generates Thrift constants in C code.
 */
void t_c_glib_generator::generate_consts (vector<t_const *> consts) {
  f_types_ << "/* constants */" << endl;
  f_types_impl_ << "/* constants */" << endl;

  vector<t_const *>::iterator c_iter;
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    string name = (*c_iter)->get_name();
    string name_uc = to_upper_case(name);
    string name_lc = to_lower_case(name);
    t_type *type = (*c_iter)->get_type();
    t_const_value *value = (*c_iter)->get_value();

    f_types_ <<
      indent() << "#define " << this->nspace_uc << name_uc << " " <<
          constant_value (name_lc, type, value) << endl;

    generate_const_initializer (name_lc, type, value);
  }

  f_types_ << endl;
  f_types_impl_ << endl;
}

/**
 * Generate Thrift structs in C code, as GObjects.  Example:
 *
 * Thrift:
 * struct Bonk
 * {
 *   1: string message,
 *   2: i32 type
 * }
 *
 * C GObject instance header:
 * struct _ThriftBonk
 * {
 *   GObject parent;
 *
 *   gchar * message;
 *   gint32 type;
 * };
 * typedef struct _ThriftBonk ThriftBonk
 * // ... additional GObject boilerplate ...
 */
void t_c_glib_generator::generate_struct (t_struct *tstruct) {
  f_types_ << "/* struct " << tstruct->get_name() << " */" << endl;
  generate_object(tstruct);
}

/**
 * Generate C code to represent Thrift services.  Creates a new GObject
 * which can be used to access the service.
 */
void t_c_glib_generator::generate_service (t_service *tservice) {
  string svcname_u = initial_caps_to_underscores(tservice->get_name());
  string svcname_uc = this->nspace_uc + to_upper_case(svcname_u);
  string filename = this->nspace_lc + to_lower_case(svcname_u);

  // make output files
  string f_header_name = get_out_dir() + filename + ".h";
  f_header_.open(f_header_name.c_str());

  string program_name_u = initial_caps_to_underscores(program_name_);
  string program_name_lc = to_lower_case(program_name_u);

  // add header file boilerplate
  f_header_ <<
    autogen_comment();

  // add an inclusion guard
  f_header_ <<
    "#ifndef " << svcname_uc << "_H" << endl <<
    "#define " << svcname_uc << "_H" << endl <<
    endl;

  // add standard includes
  f_header_ <<
    "#include \"" << this->nspace_lc << program_name_lc << "_types.h\"" << endl;

  // if we are inheriting from another service, include its header
  t_service *extends_service = tservice->get_extends();
  if (extends_service != NULL) {
    f_header_ <<
      "#include \"" << this->nspace_lc << to_lower_case(initial_caps_to_underscores(extends_service->get_name())) << ".h\"" << endl;
  }
  f_header_ << endl;

  // create the service implementation
  string f_service_name = get_out_dir() + filename + ".c";
  f_service_.open(f_service_name.c_str());

  // add the boilerplace header
  f_service_ <<
    autogen_comment();

  // include the headers
  f_service_ <<
    "#include <string.h>" << endl <<
    "#include <thrift.h>" << endl <<
    "#include <thrift_application_exception.h>" << endl <<
    "#include \"" << filename << ".h\"" << endl <<
    endl;

  // generate the client objects
  generate_service_client (tservice);

  // generate the server objects
  generate_service_server (tservice);

  // end the header inclusion guard
  f_header_ <<
    "#endif /* " << svcname_uc << "_H */" << endl;

  // close the files
  f_service_.close();
  f_header_.close();
}

/**
 *
 */
void t_c_glib_generator::generate_xception (t_struct *tstruct) {
  string name = tstruct->get_name();
  string name_u = initial_caps_to_underscores(name);
  string name_lc = to_lower_case(name_u);
  string name_uc = to_upper_case(name_u);

  generate_object(tstruct);

  f_types_ << "/* exception */" << endl <<
    "typedef enum" << endl <<
    "{" << endl <<
    "  " << this->nspace_uc << name_uc << "_ERROR_CODE," << endl <<
    "} " << this->nspace << name << "Error;" << endl <<
    endl <<
    "GQuark " << this->nspace_lc << name_lc << "_error_quark (void);" << endl <<
    "#define " << this->nspace_uc << name_uc << "_ERROR (" <<
      this->nspace_lc << name_lc << "_error_quark())" << endl <<
    endl <<
    endl;

  f_types_impl_ <<
    "/* define the GError domain for exceptions */" << endl <<
    "#define " << this->nspace_uc << name_uc << "_ERROR_DOMAIN \"" <<
        this->nspace_lc << name_lc << "_error_quark\"" << endl <<
    "GQuark" << endl <<
    this->nspace_lc << name_lc << "_error_quark (void)" << endl <<
    "{" << endl <<
    "  return g_quark_from_static_string (" << this->nspace_uc << name_uc <<
        "_ERROR_DOMAIN);" << endl <<
    "}" << endl <<
    endl;
}

/********************
 * HELPER FUNCTIONS *
 ********************/

/**
 * Returns true if ttype is not a primitive.
 */
bool t_c_glib_generator::is_complex_type(t_type *ttype) {
  ttype = get_true_type (ttype);

  return ttype->is_container()
         || ttype->is_struct()
         || ttype->is_xception()
         || (ttype->is_base_type()
             && (((t_base_type *) ttype)->get_base()
                  == t_base_type::TYPE_STRING));
}


/**
 * Maps a Thrift t_type to a C type.
 */
string t_c_glib_generator::type_name (t_type* ttype, bool in_typedef, bool is_const) {
  (void) in_typedef;
  if (ttype->is_base_type()) {
    string bname = base_type_name ((t_base_type *) ttype);

    if (is_const) {
      return "const " + bname;
    } else {
      return bname;
    }
  }

  if (ttype->is_container()) {
    string cname;

    t_container *tcontainer = (t_container *) ttype;
    if (tcontainer->has_cpp_name()) {
      cname = tcontainer->get_cpp_name();
    } else if (ttype->is_map()) {
      cname = "GHashTable *";
    } else if (ttype->is_set()) {
      // since a set requires unique elements, use a GHashTable, and
      // populate the keys and values with the same data, using keys for
      // the actual writes and reads.
      // TODO: discuss whether or not to implement TSet, THashSet or GHashSet
      cname = "GHashTable *";
    } else if (ttype->is_list()) {
      // TODO: investigate other implementations besides GPtrArray
      cname = "GPtrArray *";
      t_type *etype = ((t_list *) ttype)->get_elem_type();
      if (etype->is_base_type()) {
        t_base_type::t_base tbase = ((t_base_type *) etype)->get_base();
        switch (tbase) {
          case t_base_type::TYPE_VOID:
            throw "compiler error: cannot determine array type";
          case t_base_type::TYPE_BOOL:
          case t_base_type::TYPE_BYTE:
          case t_base_type::TYPE_I16:
          case t_base_type::TYPE_I32:
          case t_base_type::TYPE_I64:
          case t_base_type::TYPE_DOUBLE:
            cname = "GArray *";
            break;
          case t_base_type::TYPE_STRING:
            break;
          default:
            throw "compiler error: no array info for type";
        }
      }
    }

    if (is_const) {
      return "const " + cname;
    } else {
      return cname;
    }
  }

  // check for a namespace
  string pname = this->nspace + ttype->get_name();

  if (is_complex_type (ttype)) {
    pname += " *";
  }

  if (is_const) {
    return "const " + pname;
  } else {
    return pname;
  }
}

/**
 * Maps a Thrift primitive to a C primitive.
 */
string t_c_glib_generator::base_type_name(t_base_type *type) {
  t_base_type::t_base tbase = type->get_base();

  switch (tbase) {
    case t_base_type::TYPE_VOID:
      return "void";
    case t_base_type::TYPE_STRING:
      if (type->is_binary()) {
        return "GByteArray *";
      } else {
        return "gchar *";
      }
    case t_base_type::TYPE_BOOL:
      return "gboolean";
    case t_base_type::TYPE_BYTE:
      return "gint8";
    case t_base_type::TYPE_I16:
      return "gint16";
    case t_base_type::TYPE_I32:
      return "gint32";
    case t_base_type::TYPE_I64:
      return "gint64";
    case t_base_type::TYPE_DOUBLE:
      return "gdouble";
    default:
      throw "compiler error: no C base type name for base type "
            + t_base_type::t_base_name (tbase);
  }
}

/**
 * Returns a member of the ThriftType C enumeration in thrift_protocol.h
 * for a Thrift type.
 */
string t_c_glib_generator::type_to_enum (t_type *type) {
  type = get_true_type (type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type *) type)->get_base();

    switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "NO T_VOID CONSTRUCT";
      case t_base_type::TYPE_STRING:
        return "T_STRING";
      case t_base_type::TYPE_BOOL:
        return "T_BOOL";
      case t_base_type::TYPE_BYTE:
        return "T_BYTE";
      case t_base_type::TYPE_I16:
        return "T_I16";
      case t_base_type::TYPE_I32:
        return "T_I32";
      case t_base_type::TYPE_I64:
        return "T_I64";
      case t_base_type::TYPE_DOUBLE:
        return "T_DOUBLE";
    }
  } else if (type->is_enum()) {
    return "T_I32";
  } else if (type->is_struct()) {
    return "T_STRUCT";
  } else if (type->is_xception()) {
    return "T_STRUCT";
  } else if (type->is_map()) {
    return "T_MAP";
  } else if (type->is_set()) {
    return "T_SET";
  } else if (type->is_list()) {
    return "T_LIST";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}


/**
 * Returns C code that represents a Thrift constant.
 */
string t_c_glib_generator::constant_value(string name, t_type *type, t_const_value *value) {
  ostringstream render;

  if (type->is_base_type()) {
    /* primitives */
    t_base_type::t_base tbase = ((t_base_type *) type)->get_base();
    switch (tbase) {
      case t_base_type::TYPE_STRING:
        render << "\"" + value->get_string() + "\"";
        break;
      case t_base_type::TYPE_BOOL:
        render << ((value->get_integer() != 0) ? 1 : 0);
        break;
      case t_base_type::TYPE_BYTE:
      case t_base_type::TYPE_I16:
      case t_base_type::TYPE_I32:
      case t_base_type::TYPE_I64:
        render << value->get_integer();
        break;
      case t_base_type::TYPE_DOUBLE:
        if (value->get_type() == t_const_value::CV_INTEGER) {
          render << value->get_integer();
        } else {
          render << value->get_double();
        }
        break;
      default:
        throw "compiler error: no const of base type "
              + t_base_type::t_base_name (tbase);
    }
  } else if (type->is_enum()) {
    render << "(" << type_name (type) << ")" << value->get_integer();
  } else if (type->is_struct() || type->is_xception() || type->is_list()
             || type->is_set() || type->is_map()) {
    render << "(" << this->nspace_lc <<
      to_lower_case(name) << "_constant())";
  } else {
    render << "NULL /* not supported */";
  }

  return render.str();
}

/**
 * Renders a function signature of the form 'type name(args)'
 *
 * @param tfunction Function definition
 * @return String of rendered function definition
 */
string t_c_glib_generator::function_signature(t_function* tfunction) {
  t_type* ttype = tfunction->get_returntype();
  t_struct* arglist = tfunction->get_arglist();
  t_struct* xlist = tfunction->get_xceptions();
  string fname = initial_caps_to_underscores(tfunction->get_name());

  bool has_return = !ttype->is_void();
  bool has_args = arglist->get_members().size() == 0;
  bool has_xceptions = xlist->get_members().size() == 0;
  return
    "gboolean " + this->nspace_lc + fname + " (" + this->nspace
    + service_name_ + "If * iface"
    + (has_return ? ", " + type_name(ttype) + "* _return" : "")
    + (has_args ? "" : (", " + argument_list (arglist))) 
    + (has_xceptions ? "" : (", " + xception_list (xlist)))
    + ", GError ** error)";
}

/**
 * Renders a field list
 *
 * @param tstruct The struct definition
 * @return Comma sepearated list of all field names in that struct
 */
string t_c_glib_generator::argument_list (t_struct* tstruct) {
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
    result += type_name((*f_iter)->get_type(), false, true) + " " +
              (*f_iter)->get_name();
  }
  return result;
}

/**
 * Renders mutable exception lists
 *
 * @param tstruct The struct definition
 * @return Comma sepearated list of all field names in that struct
 */
string t_c_glib_generator::xception_list (t_struct* tstruct) {
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
    result += type_name((*f_iter)->get_type(), false, false) + "* " +
              (*f_iter)->get_name();
  }
  return result;
}


/**
 * Declares a field, including any necessary initialization.
 */
string t_c_glib_generator::declare_field(t_field *tfield,
                                         bool init,
                                         bool pointer,
                                         bool constant,
                                         bool reference) {
  string result = "";
  if (constant) {
    result += "const ";
  }
  result += type_name(tfield->get_type());
  if (pointer) {
    result += "*";
  }
  if (reference) {
    result += "*";
  }
  result += " " + tfield->get_name();
  if (init) {
    t_type* type = get_true_type(tfield->get_type());

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type *) type)->get_base();
      switch (tbase) {
        case t_base_type::TYPE_VOID:
          break;
        case t_base_type::TYPE_BOOL:
        case t_base_type::TYPE_BYTE:
        case t_base_type::TYPE_I16:
        case t_base_type::TYPE_I32:
        case t_base_type::TYPE_I64:
          result += " = 0";
          break;
        case t_base_type::TYPE_DOUBLE:
          result += " = (gdouble) 0";
          break;
        case t_base_type::TYPE_STRING:
          result += " = NULL";
          break;
        default:
          throw "compiler error: no C intializer for base type "
                + t_base_type::t_base_name (tbase);
      }
    } else if (type->is_enum()) {
      result += " = (" + type_name (type) + ") 0";
    }
  }

  if (!reference) {
    result += ";";
  }

  return result;
}

/**
 * Generates C code that initializes complex constants.
 */
void t_c_glib_generator::generate_const_initializer(string name, t_type *type, t_const_value *value) {
  string name_u = initial_caps_to_underscores(name);
  string name_lc = to_lower_case(name_u);
  string type_u = initial_caps_to_underscores(type->get_name());
  string type_uc = to_upper_case(type_u);

  if (type->is_struct() || type->is_xception()) {
    const vector<t_field *> &fields = ((t_struct *) type)->get_members();
    vector<t_field *>::const_iterator f_iter;
    const map<t_const_value *, t_const_value *> &val = value->get_map();
    map<t_const_value *, t_const_value *>::const_iterator v_iter;
    ostringstream initializers;

    // initialize any constants that may be referenced by this initializer
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      t_type *field_type = NULL;
      string field_name = "";

      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        if ((*f_iter)->get_name() == v_iter->first->get_string()) {
          field_type = (*f_iter)->get_type();
          field_name = (*f_iter)->get_name();
        }
      }
      if (field_type == NULL) {
        throw "type error: " + type->get_name() + " has no field "
              + v_iter->first->get_string();
      }
      field_name = tmp (field_name);

      generate_const_initializer (name + "_constant_" + field_name,
                                  field_type, v_iter->second);
      initializers <<
        "    constant->" << v_iter->first->get_string() << " = " <<
        constant_value (name + "_constant_" + field_name,
                        field_type, v_iter->second) << ";" << endl <<
        "    constant->__isset_" << v_iter->first->get_string() <<
        " = TRUE;" << endl;
    }

    // implement the initializer
    f_types_impl_ <<
      "static " << this->nspace << type->get_name() << " *" << endl <<
      this->nspace_lc << name_lc << "_constant (void)" << endl <<
      "{" << endl <<
      "  static " << this->nspace << type->get_name() <<
          " *constant = NULL;" << endl <<
      "  if (constant == NULL)" << endl <<
      "  {" << endl <<
      "    constant = g_object_new (" << this->nspace_uc << "TYPE_" <<
          type_uc << ", NULL);" << endl <<
      initializers.str() << endl <<
      "  }" << endl <<
      "  return constant;" << endl <<
      "}" << endl <<
      endl;
  } else if (type->is_list()) {
    string list_type = "GPtrArray *";
    string list_initializer = "g_ptr_array_new()";
    string list_appender = "g_ptr_array_add";
    bool list_variable = false;

    t_type* etype = ((t_list*)type)->get_elem_type();
    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    ostringstream initializers;

    if (etype->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type *) etype)->get_base();
      switch (tbase) {
        case t_base_type::TYPE_VOID:
          throw "compiler error: cannot determine array type";
        case t_base_type::TYPE_BOOL:
        case t_base_type::TYPE_BYTE:
        case t_base_type::TYPE_I16:
        case t_base_type::TYPE_I32:
        case t_base_type::TYPE_I64:
        case t_base_type::TYPE_DOUBLE:
          list_type = "GArray *";
          list_initializer = generate_new_array_from_type (etype);
          list_appender = "g_array_append_val";
          list_variable = true;
          break;
        case t_base_type::TYPE_STRING:
          break;
        default:
          throw "compiler error: no array info for type";
      }
    }

    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string fname = tmp (name);

      generate_const_initializer (fname, etype, (*v_iter));
      if (list_variable) {
        initializers <<
          "    " << type_name (etype) << " " << fname << " = " <<
            constant_value (fname, (t_type *) etype, (*v_iter)) << ";" <<
                endl <<
          "    " << list_appender << "(constant, " << fname << ");" << endl;
      } else {
        initializers <<
          "    " << list_appender << "(constant, " <<
          constant_value (fname, (t_type *) etype, (*v_iter)) << ");" << endl;
      }
    }

    f_types_impl_ <<
      "static " << list_type << endl <<
      this->nspace_lc << name_lc << "_constant (void)" << endl <<
      "{" << endl <<
      "  static " << list_type << " constant = NULL;" << endl <<
      "  if (constant == NULL)" << endl <<
      "  {" << endl <<
      "    constant = " << list_initializer << ";" << endl <<
      initializers.str() << endl <<
      "  }" << endl <<
      "  return constant;" << endl <<
      "}" << endl <<
    endl;
  } else if (type->is_set()) {
    t_type *etype = ((t_set *) type)->get_elem_type();
    const vector<t_const_value *>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    ostringstream initializers;

    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string fname = tmp (name);
      generate_const_initializer (fname, etype, (*v_iter));
      initializers <<
        "    " << type_name (etype) << " " << fname << " = " <<
          constant_value (fname, (t_type *) etype, (*v_iter)) << ";" << endl <<
        "    g_hash_table_insert (constant, &" << fname << ", &" << fname <<
        ");" << endl;
    }

    f_types_impl_ <<
      "static GHashTable *" << endl <<
      this->nspace_lc << name_lc << "_constant (void)" << endl <<
      "{" << endl <<
      "  static GHashTable *constant = NULL;" << endl <<
      "  if (constant == NULL)" << endl <<
      "  {" << endl <<
      "    constant = g_hash_table_new (NULL, NULL);" << endl <<
      initializers.str() << endl <<
      "  }" << endl <<
      "  return constant;" << endl <<
      "}" << endl <<
    endl; 
  } else if (type->is_map()) {
    t_type *ktype = ((t_map *) type)->get_key_type();
    t_type *vtype = ((t_map *) type)->get_val_type();
    const vector<t_const_value *>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    ostringstream initializers;

    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string fname = tmp (name);
      string kname = fname + "key";
      string vname = fname + "val";
      generate_const_initializer (kname, ktype, (*v_iter));
      generate_const_initializer (vname, vtype, (*v_iter));

      initializers <<
        "    " << type_name (ktype) << " " << kname << " = " <<
          constant_value (kname, (t_type *) ktype, (*v_iter)) << ";" << endl <<
        "    " << type_name (vtype) << " " << vname << " = " <<
          constant_value (vname, (t_type *) vtype, (*v_iter)) << ";" << endl <<
        "    g_hash_table_insert (constant, &" << fname << ", &" << fname <<
        ");" << endl;
    }

    f_types_impl_ <<
      "static GHashTable *" << endl <<
      this->nspace_lc << name_lc << "_constant (void)" << endl <<
      "{" << endl <<
      "  static GHashTable *constant = NULL;" << endl <<
      "  if (constant == NULL)" << endl <<
      "  {" << endl <<
      "    constant = g_hash_table_new (NULL, NULL);" << endl <<
      initializers.str() << endl <<
      "  }" << endl <<
      "  return constant;" << endl <<
      "}" << endl <<
    endl;
  }
}

/**
 * Generates C code that represents a Thrift service client.
 */
void t_c_glib_generator::generate_service_client(t_service *tservice) {
  /* get some C friendly service names */
  string service_name_lc = to_lower_case(initial_caps_to_underscores(service_name_));
  string service_name_uc = to_upper_case(service_name_lc);

  // Generate the client interface dummy object in the header.
  f_header_ <<
    "/* " << service_name_ << " service interface */" << endl <<
    "typedef struct _" << this->nspace << service_name_ << "If " <<
        this->nspace << service_name_ << "If; " <<
        " /* dummy object */" << endl <<
    endl;

  // Generate the client interface object in the header.
  f_header_ <<
    "struct _" << this->nspace << service_name_ << "IfInterface" << endl <<
    "{" << endl <<
    "  GTypeInterface parent;" << endl <<
  endl;

  /* write out the functions for this interface */
  indent_up();
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    /* make the function name C friendly */
    string funname = initial_caps_to_underscores((*f_iter)->get_name());
    t_type *ttype = (*f_iter)->get_returntype();
    t_struct *arglist = (*f_iter)->get_arglist();
    t_struct *xlist = (*f_iter)->get_xceptions();
    bool has_return = !ttype->is_void();
    bool has_args = arglist->get_members().size() == 0;
    bool has_xceptions = xlist->get_members().size() == 0;

    string params = "(" + this->nspace + service_name_ + "If *iface"
                    + (has_return ? ", " + type_name (ttype) + "* _return" : "")
                    + (has_args ? "" : (", " + argument_list (arglist)))
                    + (has_xceptions ? "" : (", " + xception_list (xlist)))
                    + ", GError **error)";
                    
    indent(f_header_) << "gboolean (*" << funname << ") " << params << ";" <<
                          endl;
  }
  indent_down();

  f_header_ <<
    "};" << endl <<
    "typedef struct _" << this->nspace << service_name_ << "IfInterface " <<
        this->nspace << service_name_ << "IfInterface;" << endl <<
    endl;

  // generate all the interface boilerplate
  f_header_ <<
    "GType " << this->nspace_lc << service_name_lc <<
        "_if_get_type (void);" << endl <<
    "#define " << this->nspace_uc << "TYPE_" << service_name_uc << "_IF " <<
        "(" << this->nspace_lc << service_name_lc << "_if_get_type())" <<
        endl <<
    "#define " << this->nspace_uc << service_name_uc << "_IF(obj) " <<
        "(G_TYPE_CHECK_INSTANCE_CAST ((obj), " <<
        this->nspace_uc << "TYPE_" << service_name_uc << "_IF, " <<
        this->nspace << service_name_ << "If))" << endl <<
    "#define " << this->nspace_uc << "IS_" << service_name_uc << "_IF(obj) " <<
        "(G_TYPE_CHECK_INSTANCE_TYPE ((obj, " <<
        this->nspace_uc << "TYPE_" << service_name_uc << "_IF))" << endl <<
    "#define " << this->nspace_uc << service_name_uc <<
        "_IF_GET_INTERFACE(inst) (G_TYPE_INSTANCE_GET_INTERFACE ((inst), " <<
        this->nspace_uc << "TYPE_" << service_name_uc << "_IF, " <<
        this->nspace << service_name_ << "IfInterface))" << endl <<
    endl;

  // write out all the interface function prototypes
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    /* make the function name C friendly */
    string funname = initial_caps_to_underscores((*f_iter)->get_name());
    t_type *ttype = (*f_iter)->get_returntype();
    t_struct *arglist = (*f_iter)->get_arglist();
    t_struct *xlist = (*f_iter)->get_xceptions();
    bool has_return = !ttype->is_void();
    bool has_args = arglist->get_members().size() == 0;
    bool has_xceptions = xlist->get_members().size() == 0;

    string params = "(" + this->nspace + service_name_ + "If *iface"
                    + (has_return ? ", " + type_name (ttype) + "* _return" : "")
                    + (has_args ? "" : (", " + argument_list (arglist)))
                    + (has_xceptions ? "" : (", " + xception_list (xlist)))
                    + ", GError **error)";

    f_header_ << "gboolean " << this->nspace_lc << service_name_lc <<
                 "_if_" << funname << " " << params << ";" << endl;
  }
  f_header_ << endl;

  // Generate the client object instance definition in the header.
  f_header_ <<
    "/* " << service_name_ << " service client */" << endl <<
    "struct _" << this->nspace << service_name_ << "Client" << endl <<
    "{" << endl <<
    "  GObject parent;" << endl <<
    endl <<
    "  ThriftProtocol *input_protocol;" << endl <<
    "  ThriftProtocol *output_protocol;" << endl <<
    "};" << endl <<
    "typedef struct _" << this->nspace << service_name_ << "Client " <<
      this->nspace << service_name_ << "Client;" << endl <<
    endl;

  // Generate the class definition in the header.
  f_header_ <<
    "struct _" << this->nspace << service_name_ << "ClientClass" << endl <<
    "{" << endl <<
    "  GObjectClass parent;" << endl <<
    "};" << endl <<
    "typedef struct _" << this->nspace << service_name_ << "ClientClass " <<
      this->nspace << service_name_ << "ClientClass;" << endl <<
    endl;

  // Create all the GObject boilerplate
  f_header_ <<
    "GType " << this->nspace_lc << service_name_lc << 
        "_client_get_type (void);" << endl <<
    "#define " << this->nspace_uc << "TYPE_" << service_name_uc << "_CLIENT " <<
        "(" << this->nspace_lc << service_name_lc << "_client_get_type())" <<
        endl <<
    "#define " << this->nspace_uc << service_name_uc << "_CLIENT(obj) " <<
        "(G_TYPE_CHECK_INSTANCE_CAST ((obj), " << 
        this->nspace_uc << "TYPE_" << service_name_uc << "_CLIENT, " << 
        this->nspace << service_name_ << "Client))" << endl <<
    "#define " << this->nspace_uc << service_name_uc << "_CLIENT_CLASS(c) " << 
        "(G_TYPE_CHECK_CLASS_CAST ((c), " << 
        this->nspace_uc << "TYPE_" << service_name_uc << "_CLIENT, " << 
        this->nspace << service_name_ << "ClientClass))" << endl <<
    "#define " << this->nspace_uc << service_name_uc << "_IS_CLIENT(obj) " <<
        "(G_TYPE_CHECK_INSTANCE_TYPE ((obj), " << 
        this->nspace_uc << "TYPE_" << service_name_uc << "_CLIENT))" << endl << 
    "#define " << this->nspace_uc << service_name_uc <<
        "_IS_CLIENT_CLASS(c) " << "(G_TYPE_CHECK_CLASS_TYPE ((c), " << 
        this->nspace_uc << "TYPE_" << service_name_uc << "_CLIENT))" << endl <<
    "#define " << this->nspace_uc << service_name_uc <<
        "_CLIENT_GET_CLASS(obj) " << "(G_TYPE_INSTANCE_GET_CLASS ((obj), " << 
        this->nspace_uc << "TYPE_" << service_name_uc << "_CLIENT, " <<
        this->nspace << service_name_ << "ClientClass))" << endl << 
    endl;

  /* write out the function prototypes */
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    /* make the function name C friendly */
    string funname = to_lower_case(initial_caps_to_underscores((*f_iter)->get_name()));

    t_function service_function ((*f_iter)->get_returntype(),
                                 service_name_lc + string ("_client_")
                                 + funname,
                                 (*f_iter)->get_arglist(),
                                 (*f_iter)->get_xceptions());
    indent(f_header_) << function_signature (&service_function) << ";" << endl;

    t_function send_function (g_type_void,
                              service_name_lc + string ("_client_send_")
                              + funname,
                              (*f_iter)->get_arglist());
    indent(f_header_) << function_signature (&send_function) << ";" << endl;

    // implement recv if not a oneway service
    if (!(*f_iter)->is_oneway()) {
      t_struct noargs (program_);
      t_function recv_function ((*f_iter)->get_returntype(),
                                service_name_lc + string ("_client_recv_") 
                                + funname,
                                &noargs,
                                (*f_iter)->get_xceptions());
      indent(f_header_) << function_signature (&recv_function) << ";" << endl;
    }
  }

  f_header_ << endl;
  // end of header code

  // Generate interface method implementations
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    /* make the function name C friendly */
    string funname = initial_caps_to_underscores((*f_iter)->get_name());
    t_type *ttype = (*f_iter)->get_returntype();
    t_struct *arglist = (*f_iter)->get_arglist();
    t_struct *xlist = (*f_iter)->get_xceptions();
    bool has_return = !ttype->is_void();
    bool has_args = arglist->get_members().size() == 0;
    bool has_xceptions = xlist->get_members().size() == 0;

    string params = "(" + this->nspace + service_name_ + "If *iface"
                    + (has_return ? ", " + type_name (ttype) + "* _return" : "")
                    + (has_args ? "" : (", " + argument_list (arglist)))
                    + (has_xceptions ? "" : (", " + xception_list (xlist)))
                    + ", GError **error)";

    string params_without_type = string("iface, ")
                                 + (has_return ? "_return, " : "");

    const vector<t_field *>& fields = arglist->get_members();
    vector<t_field *>::const_iterator f_iter_field;
    for (f_iter_field = fields.begin(); f_iter_field != fields.end(); ++f_iter_field) {
      params_without_type += (*f_iter_field)->get_name(); 
      params_without_type += ", ";
    }

    const vector<t_field *>& xceptions = xlist->get_members();
    vector<t_field *>::const_iterator x_iter;
    for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
      params_without_type += (*x_iter)->get_name();
      params_without_type += ", ";
    }

    f_service_ <<
      "gboolean" << endl <<
      this->nspace_lc << service_name_lc << "_if_" << funname << " " <<
          params << endl <<
      "{" << endl << 
      "  return " << this->nspace_uc << service_name_uc <<
          "_IF_GET_INTERFACE (iface)->" << funname << " (" <<
          params_without_type << "error);" << endl <<
      "}" << endl <<
      endl;
  }

  // Generate interface boilerplate
  f_service_ <<
    "GType" << endl <<
    this->nspace_lc << service_name_lc << "_if_get_type (void)" << endl <<
    "{" << endl <<
    "  static GType type = 0;" << endl <<
    "  if (type == 0)" << endl <<
    "  {" << endl <<
    "    static const GTypeInfo type_info =" << endl <<
    "    {" << endl <<
    "      sizeof (" << this->nspace << service_name_ << "IfInterface)," <<
        endl <<
    "      NULL,  /* base_init */" << endl <<
    "      NULL  /* base_finalize */" << endl <<
    "    };" << endl <<
    "    type = g_type_register_static (G_TYPE_INTERFACE," << endl <<
    "                                   \"" << this->nspace << service_name_ <<
        "If\"," << endl <<
    "                                   &type_info, 0);" << endl <<
    "  }" << endl <<
    "  return type;" << endl <<
    "}" << endl <<
    endl;

  // Generate client boilerplate
  f_service_ <<
    "static void " << endl <<
    this->nspace_lc << service_name_lc <<
        "_if_interface_init (" << this->nspace << service_name_ <<
        "IfInterface *iface);" << endl <<
    endl <<
    "G_DEFINE_TYPE_WITH_CODE (" << this->nspace << service_name_ <<
      "Client, " << this->nspace_lc << service_name_lc << "_client," << endl <<
      "                       G_TYPE_OBJECT, " << endl <<
    "                         G_IMPLEMENT_INTERFACE (" <<
        this->nspace_uc << "TYPE_" << service_name_uc << "_IF," << endl <<
    "                                                " <<
        this->nspace_lc << service_name_lc << "_if_interface_init));" << endl <<
    endl;

  // Generate client properties
  f_service_ <<
    "enum _" << this->nspace << service_name_ << "ClientProperties" << endl <<
    "{" << endl <<
    "  PROP_0," << endl <<
    "  PROP_" << this->nspace_uc << service_name_uc <<
        "_CLIENT_INPUT_PROTOCOL," <<
        endl <<
    "  PROP_" << this->nspace_uc << service_name_uc <<
        "_CLIENT_OUTPUT_PROTOCOL" <<
        endl <<
    "};" << endl <<
  endl;

  // generate property setter
  f_service_ <<
    "void" << endl <<
    this->nspace_lc << service_name_lc << "_client_set_property (" <<
        "GObject *object, guint property_id, const GValue *value, " <<
        "GParamSpec *pspec)" << endl <<
    "{" << endl <<
    "  " << this->nspace << service_name_ << "Client *client = " <<
        this->nspace_uc << service_name_uc << "_CLIENT (object);" << endl <<
    endl <<
    "  THRIFT_UNUSED_VAR (pspec);" << endl <<
    endl <<
    "  switch (property_id)" << endl <<
    "  {" << endl <<
    "    case PROP_" << this->nspace_uc << service_name_uc <<
        "_CLIENT_INPUT_PROTOCOL:" << endl <<
    "      client->input_protocol = g_value_get_object (value);" << endl <<
    "      break;" << endl <<
    "    case PROP_" << this->nspace_uc << service_name_uc <<
        "_CLIENT_OUTPUT_PROTOCOL:" << endl <<
    "      client->output_protocol = g_value_get_object (value);" << endl <<
    "      break;" << endl <<
    "  }" << endl <<
    "}" << endl <<
  endl;

  // generate property getter
  f_service_ <<
    "void" << endl <<
    this->nspace_lc << service_name_lc << "_client_get_property (" <<
        "GObject *object, guint property_id, GValue *value, " <<
        "GParamSpec *pspec)" << endl <<
    "{" << endl <<
    "  " << this->nspace << service_name_ << "Client *client = " <<
        this->nspace_uc << service_name_uc << "_CLIENT (object);" << endl <<
    endl <<
    "  THRIFT_UNUSED_VAR (pspec);" << endl <<
    endl <<
    "  switch (property_id)" << endl <<
    "  {" << endl <<
    "    case PROP_" << this->nspace_uc << service_name_uc <<
        "_CLIENT_INPUT_PROTOCOL:" << endl <<
    "      g_value_set_object (value, client->input_protocol);" << endl <<
    "      break;" << endl <<
    "    case PROP_" << this->nspace_uc << service_name_uc <<
        "_CLIENT_OUTPUT_PROTOCOL:" << endl <<
    "      g_value_set_object (value, client->output_protocol);" << endl <<
    "      break;" << endl <<
    "  }" << endl <<
    "}" << endl <<
  endl;


  // Generate client method implementations
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string name = (*f_iter)->get_name();
    string funname = initial_caps_to_underscores(name);

    // Get the struct of function call params and exceptions
    t_struct* arg_struct = (*f_iter)->get_arglist();

    // Function for sending
    t_function send_function (g_type_void,
                              service_name_lc + string ("_client_send_")
                              + funname,
                              (*f_iter)->get_arglist());

    // Open the send function
    indent(f_service_) <<
      function_signature (&send_function) << endl;
    scope_up(f_service_);

    // Serialize the request
    f_service_ <<
      indent() << "gint32 cseqid = 0;" << endl <<
      indent() << "ThriftProtocol * protocol = " << 
        this->nspace_uc << service_name_uc << 
        "_CLIENT (iface)->output_protocol;" << endl <<
      endl <<
      indent() << "if (thrift_protocol_write_message_begin (protocol, \"" <<
        name << "\", T_CALL, cseqid, error) < 0)" << endl <<
      indent() << "  return FALSE;" << endl <<
      endl;

    generate_struct_writer (f_service_, arg_struct, "", "", false);

    f_service_ <<
      indent() <<
        "if (thrift_protocol_write_message_end (protocol, error) < 0)" <<
        endl <<
      indent() <<
        "  return FALSE;" << endl <<
      indent() <<
        "if (!thrift_transport_flush (protocol->transport, error))" << endl <<
      indent() <<
        "  return FALSE;" << endl <<
      indent() <<
        "if (!thrift_transport_write_end (protocol->transport, error))" <<
        endl <<
      indent() <<
        "  return FALSE;" << endl <<
      endl <<
      indent() << 
        "return TRUE;" << endl;

    scope_down(f_service_);
    f_service_ << endl;

    // Generate recv function only if not an async function
    if (!(*f_iter)->is_oneway()) {
      t_struct noargs (program_);
      t_function recv_function ((*f_iter)->get_returntype(),
                                service_name_lc
                                + string ("_client_recv_") + funname, &noargs,
                                (*f_iter)->get_xceptions());
      // Open function
      indent(f_service_) <<
        function_signature (&recv_function) << endl;
      scope_up(f_service_);

      f_service_ << endl <<
        indent() << "gint32 rseqid;" << endl <<
        indent() << "gchar * fname;" << endl <<
        indent() << "ThriftMessageType mtype;" << endl <<
        indent() << "ThriftProtocol * protocol = " << 
                      this->nspace_uc << service_name_uc <<
                      "_CLIENT (iface)->input_protocol;" << endl <<
        endl <<
        indent() << "if (thrift_protocol_read_message_begin " << 
                      "(protocol, &fname, &mtype, &rseqid, error) < 0)" <<
                      endl <<
        indent() << "{" << endl <<
        indent() << "  if (fname) g_free (fname);" << endl <<
        indent() << "  return FALSE;" << endl <<
        indent() << "}" << endl <<
        endl <<
        indent() << "if (mtype == T_EXCEPTION) {" << endl <<
        indent() << "  if (fname) g_free (fname);" << endl <<
        indent() << "  ThriftApplicationException *xception = g_object_new (THRIFT_TYPE_APPLICATION_EXCEPTION, NULL);" << endl <<

        indent() << "  thrift_struct_read (THRIFT_STRUCT (xception), protocol, NULL);" << endl <<
        indent() << "  thrift_protocol_read_message_end (protocol, NULL);" << endl <<
        indent() << "  thrift_transport_read_end (protocol->transport, NULL);" << endl <<
        indent() << "  g_set_error (error, THRIFT_APPLICATION_EXCEPTION_ERROR, xception->type, \"application error: %s\", xception->message);" << endl <<
        indent() << "  g_object_unref (xception);" << endl <<
        indent() << "  return FALSE;" << endl <<
        indent() << "} else if (mtype != T_REPLY) {" << endl <<
        indent() << "  if (fname) g_free (fname);" << endl <<
        indent() << "  thrift_protocol_skip (protocol, T_STRUCT, NULL);" << endl <<
        indent() << "  thrift_protocol_read_message_end (protocol, NULL);" << endl <<
        indent() << "  thrift_transport_read_end (protocol->transport, NULL);" << endl << 
        indent() << "  g_set_error (error, THRIFT_APPLICATION_EXCEPTION_ERROR, THRIFT_APPLICATION_EXCEPTION_ERROR_INVALID_MESSAGE_TYPE, \"invalid message type %d, expected T_REPLY\", mtype);" << endl <<
        indent() << "  return FALSE;" << endl <<
        indent() << "} else if (strncmp (fname, \"" << name << "\", " << name.length() << ") != 0) {" << endl <<
        indent() << "  thrift_protocol_skip (protocol, T_STRUCT, NULL);" << endl <<
        indent() << "  thrift_protocol_read_message_end (protocol, error);" << endl <<
        indent() << "  thrift_transport_read_end (protocol->transport, error);" << endl <<
        indent() << "  g_set_error (error, THRIFT_APPLICATION_EXCEPTION_ERROR, THRIFT_APPLICATION_EXCEPTION_ERROR_WRONG_METHOD_NAME, \"wrong method name %s, expected " << name << "\", fname);" << endl <<
        indent() << "  if (fname) g_free (fname);" << endl <<
        indent() << "  return FALSE;" << endl <<
        indent() << "}" << endl <<
        indent() << "if (fname) g_free (fname);" << endl << 
        endl;

      t_struct* xs = (*f_iter)->get_xceptions();
      const std::vector<t_field*>& xceptions = xs->get_members();
      vector<t_field*>::const_iterator x_iter;

      {
        t_struct result(program_, tservice->get_name() + "_" + 
                        (*f_iter)->get_name() + "_result");
        t_field success((*f_iter)->get_returntype(), "*_return", 0);
        if (!(*f_iter)->get_returntype()->is_void()) {
          result.append(&success);
        }

        // add readers for exceptions, dereferencing the pointer.
        for (x_iter = xceptions.begin(); x_iter != xceptions.end(); x_iter++) {
          t_field *xception = new t_field((*x_iter)->get_type(),
                                          "*" + (*x_iter)->get_name(),
                                          (*x_iter)->get_key());
          result.append (xception);
        }

        generate_struct_reader (f_service_, &result, "", "", false);
      }

      f_service_ <<
        indent() << "if (thrift_protocol_read_message_end (protocol, error) < 0)" << endl <<
        indent() << "  return FALSE;" << endl <<
        endl <<
        indent() << "if (!thrift_transport_read_end (protocol->transport, error))" << endl <<
        indent() << "  return FALSE;" << endl <<
        endl;

      // copy over any throw exceptions and return failure
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); x_iter++) {
        f_service_ << 
          indent() << "if (*" << (*x_iter)->get_name() << " != NULL)" << endl <<
          indent() << "{" << endl <<
          indent() << "    g_set_error (error, " << this->nspace_uc <<
              to_upper_case(initial_caps_to_underscores(
                                 (*x_iter)->get_type()->get_name())) <<
              "_ERROR, " <<
              this->nspace_uc <<
              to_upper_case(initial_caps_to_underscores(
                                 (*x_iter)->get_type()->get_name())) <<
              "_ERROR_CODE, \"" << (*x_iter)->get_type()->get_name() << 
              "\");" << endl <<
          indent() << "    return FALSE;" << endl <<
          indent() << "}" << endl;
      }
      // Close function
      indent(f_service_) << "return TRUE;" << endl;
      scope_down(f_service_);
      f_service_ << endl;
    }

    // Open function
    t_function service_function((*f_iter)->get_returntype(),
                                 service_name_lc
                                 + string ("_client_") + funname,
                                 (*f_iter)->get_arglist(),
                                 (*f_iter)->get_xceptions());
    indent(f_service_) <<
      function_signature (&service_function) << endl;
    scope_up(f_service_);

    // wrap each function
    f_service_ <<
      indent() << "if (!" << this->nspace_lc << service_name_lc <<
                   "_client_send_" << funname <<
      " (iface";

    // Declare the function arguments
    const vector<t_field *> &fields = arg_struct->get_members();
    vector<t_field *>::const_iterator fld_iter;
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      f_service_ << ", " << (*fld_iter)->get_name();
    }
    f_service_ << ", error))" << endl <<
      indent() << "  return FALSE;" << endl;

    // if not oneway, implement recv
    if (!(*f_iter)->is_oneway()) {
      string ret = (*f_iter)->get_returntype()->is_void() ? "" : "_return, ";

      const vector<t_field *>& xceptions =
          (*f_iter)->get_xceptions()->get_members();
      vector<t_field *>::const_iterator x_iter;
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        ret += (*x_iter)->get_name();
        ret += ", ";
      }

      f_service_ <<
        indent() << "if (!" << this->nspace_lc << service_name_lc <<
          "_client_recv_" << funname <<
          " (iface, " << ret << "error))" << endl <<
        indent() << "  return FALSE;" << endl;
    }

    // return TRUE which means all functions were called OK
    indent(f_service_) << "return TRUE;" << endl;
    scope_down(f_service_);
    f_service_ << endl;
  }

  // create the interface initializer
  f_service_ <<
    "static void" << endl <<
    this->nspace_lc << service_name_lc << "_if_interface_init (" <<
        this->nspace << service_name_ << "IfInterface *iface)" << endl <<
    "{" << endl;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    /* make the function name C friendly */
    string funname = initial_caps_to_underscores((*f_iter)->get_name());

    f_service_ <<
      "  iface->" << funname << " = " << this->nspace_lc << service_name_lc <<
        "_client_" << funname << ";" << endl;
  }
  f_service_ <<
    "}" << endl <<
    endl;

  // create the client instance initializer
  f_service_ <<
    "static void" << endl <<
    this->nspace_lc << service_name_lc << "_client_init (" <<
        this->nspace << service_name_ << "Client *client)" << endl <<
    "{" << endl <<
    "  client->input_protocol = NULL;" << endl <<
    "  client->output_protocol = NULL;" << endl <<
    "}" << endl <<
    endl;

  // create the client class initializer
  f_service_ <<
    "static void" << endl <<
    this->nspace_lc << service_name_lc << "_client_class_init (" <<
        this->nspace << service_name_ << "ClientClass *cls)" << endl <<
    "{" << endl <<
    "  GObjectClass *gobject_class = G_OBJECT_CLASS (cls);" << endl <<
    "  GParamSpec *param_spec;" << endl <<
    endl <<
    "  gobject_class->set_property = " << this->nspace_lc <<
        service_name_lc << "_client_set_property;" << endl <<
    "  gobject_class->get_property = " << this->nspace_lc <<
        service_name_lc << "_client_get_property;" << endl <<
    endl <<
    "  param_spec = g_param_spec_object (\"input_protocol\"," << endl <<
    "                                    \"input protocol (construct)\"," <<
        endl <<
    "                                    \"Set the client input protocol\"," <<
        endl <<
    "                                    THRIFT_TYPE_PROTOCOL," << endl <<
    "                                    G_PARAM_READWRITE);" << endl <<
    "  g_object_class_install_property (gobject_class," << endl <<
    "                                   PROP_" << this->nspace_uc <<
        service_name_uc << "_CLIENT_INPUT_PROTOCOL, param_spec);" << endl <<
    endl <<
    "  param_spec = g_param_spec_object (\"output_protocol\"," << endl <<
    "                                    \"output protocol (construct)\"," <<
        endl <<
    "                                    \"Set the client output protocol\"," <<
        endl <<
    "                                    THRIFT_TYPE_PROTOCOL," << endl <<
    "                                    G_PARAM_READWRITE);" << endl <<
    "  g_object_class_install_property (gobject_class," << endl <<
    "                                   PROP_" << this->nspace_uc <<
        service_name_uc << "_CLIENT_OUTPUT_PROTOCOL, param_spec);" << endl <<
    "}" << endl <<
    endl;
}

/**
 * Generates C code that represents a Thrift service server.
 */
void t_c_glib_generator::generate_service_server (t_service *tservice) {
  (void) tservice;
  /* get some C friendly service names */
  string service_name_u = initial_caps_to_underscores(service_name_);
  string service_name_uc = to_upper_case(service_name_u);

  // write the server object instance definition in the header.
  // TODO: implement after implement TServer and TProcessor
}

/**
 * Generates C code to represent a THrift structure as a GObject.
 */
void t_c_glib_generator::generate_object(t_struct *tstruct) {
  string name = tstruct->get_name();
  string name_u = initial_caps_to_underscores(name);
  string name_uc = to_upper_case(name_u);

  // write the instance definition
  f_types_ <<
    "struct _" << this->nspace << name << endl <<
    "{ " << endl <<
    "  ThriftStruct parent; " << endl <<
    endl <<
    "  /* public */" << endl;

  // for each field, add a member variable
  bool has_nonrequired_fields = false;
  vector<t_field *>::const_iterator m_iter;
  const vector<t_field *> &members = tstruct->get_members();
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type *t = get_true_type ((*m_iter)->get_type());
    f_types_ <<
      "  " << type_name (t) << " " << (*m_iter)->get_name() << ";" << endl;
    if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
      has_nonrequired_fields = true;
      f_types_ <<
        "  gboolean __isset_" << (*m_iter)->get_name() << ";" << endl;
    }
  }

  // close the structure definition and create a typedef
  f_types_ <<
    "};" << endl <<
    "typedef struct _" << this->nspace << name << " " << 
        this->nspace << name << ";" << endl <<
      endl;

  // write the class definition
  f_types_ <<
    "struct _" << this->nspace << name << "Class" << endl <<
    "{" << endl <<
    "  ThriftStructClass parent;" << endl <<
    "};" << endl <<
    "typedef struct _" << this->nspace << name << "Class " << this->nspace << name << "Class;" << endl <<
    endl;

  // write the standard GObject boilerplate
  f_types_ <<
    "GType " << this->nspace_lc << name_u << "_get_type (void);" << endl <<
    "#define " << this->nspace_uc << "TYPE_" << name_uc << " (" << this->nspace_lc << name_u << "_get_type())" << endl <<
    "#define " << this->nspace_uc << name_uc << "(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), " << this->nspace_uc << "TYPE_" << name_uc << ", " << this->nspace << name << "))" << endl <<
    "#define " << this->nspace_uc << name_uc << "_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c), " << this->nspace_uc << "_TYPE_" << name_uc << ", " << this->nspace << name << "Class))" << endl <<
    "#define " << this->nspace_uc << "IS_" << name_uc << "(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), " << this->nspace_uc << "TYPE_" << name_uc << "))" << endl <<
    "#define " << this->nspace_uc << "IS_" << name_uc << "_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), " << this->nspace_uc << "TYPE_" << name_uc << "))" << endl <<
    "#define " << this->nspace_uc << name_uc << "_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), " << this->nspace_uc << "TYPE_" << name_uc << ", " << this->nspace << name << "Class))" << endl <<
    endl;

  // start writing the object implementation .c file
  // generate struct I/O methods
  string this_get = this->nspace + name + " * this_object = " 
                    + this->nspace_uc + name_uc + "(object);";
  generate_struct_reader (f_types_impl_, tstruct, "this_object->", this_get);
  generate_struct_writer (f_types_impl_, tstruct, "this_object->", this_get);

  // generate the instance init function
  f_types_impl_ <<
    "void " << endl <<
    this->nspace_lc << name_u << "_instance_init (" << this->nspace << name << " * object)" << endl <<
    "{" << endl;

  // satisfy compilers with -Wall turned on
  indent_up();
  indent(f_types_impl_) << "/* satisfy -Wall */" << endl <<
               indent() << "THRIFT_UNUSED_VAR (object);" << endl;

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* t = get_true_type ((*m_iter)->get_type());
    if (t->is_base_type()) {
      // only have init's for base types
      string dval = " = ";
      if (t->is_enum()) {
        dval += "(" + type_name (t) + ")";
      }
      t_const_value* cv = (*m_iter)->get_value();
      if (cv != NULL) {
        dval += constant_value ("", t, cv);
      } else {
        dval += t->is_string() ? "NULL" : "0";
      }
      indent(f_types_impl_) << "object->" << (*m_iter)->get_name() << dval << ";" << endl;
    } else if (t->is_struct()) {
      string name = (*m_iter)->get_name();
      string type_name_uc = to_upper_case
        (initial_caps_to_underscores((*m_iter)->get_type()->get_name()));
      indent(f_types_impl_) << "object->" << name << " = g_object_new (" << this->nspace_uc << "TYPE_" << type_name_uc << ", NULL);" << endl;
    } else if (t->is_xception()) {
      string name = (*m_iter)->get_name();
      indent(f_types_impl_) << "object->" << name << " = NULL;" << endl;
    } else if (t->is_container()) {
      string name = (*m_iter)->get_name();

      if (t->is_map() || t->is_set()) {
        indent(f_types_impl_) << "object->" << name << " = g_hash_table_new (NULL, NULL);" << endl;
      } else if (t->is_list()) {
        t_type *etype = ((t_list *) t)->get_elem_type();
        string init_function = "g_ptr_array_new()";

        if (etype->is_base_type()) {
          t_base_type::t_base tbase = ((t_base_type *) etype)->get_base();
          switch (tbase) {
            case t_base_type::TYPE_VOID:
              throw "compiler error: cannot determine array type";
            case t_base_type::TYPE_BOOL:
            case t_base_type::TYPE_BYTE:
            case t_base_type::TYPE_I16:
            case t_base_type::TYPE_I32:
            case t_base_type::TYPE_I64:
            case t_base_type::TYPE_DOUBLE:
              init_function = generate_new_array_from_type (etype);
              break;
            case t_base_type::TYPE_STRING:
              break;
            default:
              throw "compiler error: no array info for type";
          }
        }

        indent(f_types_impl_) << "object->" << name << " = " <<
                                  init_function << ";" << endl;
      }

    }

    /* if not required, initialize the __isset variable */
    if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
      indent(f_types_impl_) << "object->__isset_" << (*m_iter)->get_name() << " = FALSE;" << endl;
    }
  }

  indent_down();
  f_types_impl_ << "}" << endl <<
  endl;

  /* create the destructor */
  f_types_impl_ <<
    "void " << endl <<
    this->nspace_lc << name_u << "_finalize (GObject *object)" << endl <<
    "{" << endl;
  indent_up();

  f_types_impl_ <<
    indent() <<
    this->nspace << name << " *tobject = " << this->nspace_uc <<
    name_uc << " (object);" << endl << endl;

  f_types_impl_ <<
    indent() << "/* satisfy -Wall in case we don't use tobject */" << endl <<
    indent() << "THRIFT_UNUSED_VAR (tobject);" << endl;

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* t = get_true_type ((*m_iter)->get_type());
    if (t->is_container()) { 
      string name = (*m_iter)->get_name();
      if (t->is_map() || t->is_set()) {
        f_types_impl_ <<
          indent() << "g_hash_table_unref (tobject->" << name << ");" << endl;
      } else if (t->is_list()) {
        t_type *etype = ((t_list *) t)->get_elem_type();
        string destructor_function = "g_ptr_array_free";

        if (etype->is_base_type()) {
          t_base_type::t_base tbase = ((t_base_type *) etype)->get_base();
          switch (tbase) {
            case t_base_type::TYPE_VOID:
              throw "compiler error: cannot determine array type";
            case t_base_type::TYPE_BOOL:
            case t_base_type::TYPE_BYTE:
            case t_base_type::TYPE_I16:
            case t_base_type::TYPE_I32:           
            case t_base_type::TYPE_I64:
            case t_base_type::TYPE_DOUBLE:
              destructor_function = "g_array_free";
              break;
            case t_base_type::TYPE_STRING:
              break;
            default:
              throw "compiler error: no array info for type";
          }
        }

        f_types_impl_ <<
          indent() << destructor_function << " (tobject->" << name <<
                       ", FALSE);" << endl;
      }
    }
  }

  indent_down();
  f_types_impl_ <<
    "}" << endl <<
    endl;


  f_types_impl_ <<
    "void " << endl <<
    this->nspace_lc << name_u << "_class_init (ThriftStructClass * cls)" << endl <<
    "{" << endl;
  indent_up();

  f_types_impl_ <<   
    indent() << "GObjectClass *gobject_class = G_OBJECT_CLASS (cls);" << endl <<
    endl <<
    indent() << "gobject_class->finalize = " << this->nspace_lc << name_u << "_finalize;" << endl <<
    indent() << "cls->read = " << this->nspace_lc << name_u << "_read;" << endl <<
    indent() << "cls->write = " << this->nspace_lc << name_u << "_write;" << endl;

  indent_down();
  f_types_impl_ <<
    "}" << endl <<
    endl;


  f_types_impl_ <<
    "GType" << endl <<
    this->nspace_lc << name_u << "_get_type (void)" << endl <<
    "{" << endl <<
    "  static GType type = 0;" << endl <<
    endl <<
    "  if (type == 0) " << endl <<
    "  {" << endl <<
    "    static const GTypeInfo type_info = " << endl <<
    "    {" << endl <<
    "      sizeof (" << this->nspace << name << "Class)," << endl <<
    "      NULL, /* base_init */" << endl <<
    "      NULL, /* base_finalize */" << endl <<
    "      (GClassInitFunc) " << this->nspace_lc << name_u << "_class_init," << endl <<
    "      NULL, /* class_finalize */" << endl <<
    "      NULL, /* class_data */" << endl <<
    "      sizeof (" << this->nspace << name << ")," << endl <<
    "      0, /* n_preallocs */" << endl <<
    "      (GInstanceInitFunc) " << this->nspace_lc << name_u << "_instance_init," << endl <<
    "      NULL, /* value_table */" << endl <<
    "    };" << endl <<
    endl <<
    "    type = g_type_register_static (THRIFT_TYPE_STRUCT, " << endl <<
    "                                   \"" << this->nspace << name << "Type\"," << endl <<
    "                                   &type_info, 0);" << endl <<
    "  }" << endl <<
    endl <<
    "  return type;" << endl <<
    "}" << endl <<
    endl;
}

/**
 * Generates functions to write Thrift structures to a stream.
 */
void t_c_glib_generator::generate_struct_writer (ofstream &out,
                                                 t_struct *tstruct,
                                                 string this_name,
                                                 string this_get,
                                                 bool is_function) {
  string name = tstruct->get_name();
  string name_u = initial_caps_to_underscores(name);
  string name_uc = to_upper_case(name_u);

  const vector<t_field *> &fields = tstruct->get_members();
  vector <t_field *>::const_iterator f_iter;
  int error_ret = 0;

  if (is_function) {
    error_ret = -1;
    indent(out) <<
      "gint32" << endl <<
      this->nspace_lc << name_u <<
      "_write (ThriftStruct *object, ThriftProtocol *protocol, GError **error)" << endl;
  }
  indent(out) << "{" << endl;
  indent_up();

  out <<
    indent() << "gint32 ret;" << endl <<
    indent() << "gint32 xfer = 0;" << endl <<
    endl;

  indent(out) << this_get << endl;
  // satisfy -Wall in the case of an empty struct
  if (!this_get.empty()) {
    indent(out) << "THRIFT_UNUSED_VAR (this_object);" << endl;
  }

  out <<
    indent() << "if ((ret = thrift_protocol_write_struct_begin (protocol, \"" << name << "\", error)) < 0)" << endl <<
    indent() << "  return " << error_ret << ";" << endl <<
    indent() << "xfer += ret;" << endl;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_OPTIONAL) {
      indent(out) << "if (this_object->__isset_" << (*f_iter)->get_name() << " == TRUE) {" << endl;
      indent_up();
    } 

    out <<
     indent() << "if ((ret = thrift_protocol_write_field_begin (protocol, " <<
     "\"" << (*f_iter)->get_name() << "\", " <<
     type_to_enum ((*f_iter)->get_type()) << ", " <<
     (*f_iter)->get_key() << ", error)) < 0)" << endl <<
     indent() << "  return " << error_ret << ";" << endl <<
     indent() << "xfer += ret;" << endl;
    generate_serialize_field (out, *f_iter, this_name, "", error_ret);
    out <<
      indent() << "if ((ret = thrift_protocol_write_field_end (protocol, error)) < 0)" << endl <<
      indent() << "  return " << error_ret << ";" << endl <<
      indent() << "xfer += ret;" << endl;

    if ((*f_iter)->get_req() == t_field::T_OPTIONAL) {
      indent_down();
      indent(out) << "}" << endl;
    }
  }

  // write the struct map
  out <<
    indent() << "if ((ret = thrift_protocol_write_field_stop (protocol, error)) < 0)" << endl <<
    indent() << "  return " << error_ret << ";" << endl <<
    indent() << "xfer += ret;" << endl <<
    indent() << "if ((ret = thrift_protocol_write_struct_end (protocol, error)) < 0)" << endl <<
    indent() << "  return " << error_ret << ";" << endl <<
    indent() << "xfer += ret;" << endl <<
    endl;

  if (is_function) {
    indent(out) << "return xfer;" << endl;
  }

  indent_down();
  indent(out) <<
    "}" << endl <<
    endl;
}

/**
 * Generates code to read Thrift structures from a stream.
 */
void t_c_glib_generator::generate_struct_reader(ofstream &out,
                                                t_struct *tstruct,
                                                string this_name,
                                                string this_get,
                                                bool is_function) {
  string name = tstruct->get_name();
  string name_u = initial_caps_to_underscores(name);
  string name_uc = to_upper_case(name_u);
  int error_ret = 0;
  const vector<t_field *> &fields = tstruct->get_members();
  vector <t_field *>::const_iterator f_iter;

  if (is_function) {
    error_ret = -1;
    indent(out) <<
      "/* reads a " << name_u << " object */" << endl <<
      "gint32" << endl <<
      this->nspace_lc << name_u <<
          "_read (ThriftStruct *object, ThriftProtocol *protocol, GError **error)" << endl;
  }

  indent(out) << "{" << endl;
  indent_up();

  // declare stack temp variables
  out <<
    indent() << "gint32 ret;" << endl <<
    indent() << "gint32 xfer = 0;" << endl <<
    indent() << "gchar *name;" << endl <<
    indent() << "ThriftType ftype;" << endl <<
    indent() << "gint16 fid;" << endl <<
    indent() << "guint32 len = 0;" << endl <<
    indent() << "gpointer data = NULL;" << endl <<
    indent() << this_get << endl;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      indent(out) << "gboolean isset_" << (*f_iter)->get_name() << " = FALSE;" << endl;
    }
  }

  out << endl;

  // satisfy -Wall in case we don't use some variables
  out <<
    indent() << "/* satisfy -Wall in case these aren't used */" << endl <<
    indent() << "THRIFT_UNUSED_VAR (len);" << endl <<
    indent() << "THRIFT_UNUSED_VAR (data);" << endl;

  if (!this_get.empty()) {
    out << indent() << "THRIFT_UNUSED_VAR (this_object);" << endl;
  }
  out << endl;

  // read the beginning of the structure marker
  out <<
    indent() << "/* read the struct begin marker */" << endl <<
    indent() << "if ((ret = thrift_protocol_read_struct_begin (protocol, &name, error)) < 0)" << endl <<
    indent() << "{" << endl <<
    indent() << "  if (name) g_free (name);" << endl <<
    indent() << "  return " << error_ret << ";" << endl <<
    indent() << "}" << endl <<
    indent() << "xfer += ret;" << endl <<
    indent() << "if (name) g_free (name);" << endl <<
    endl;

  // read the struct fields
  out <<
    indent() << "/* read the struct fields */" << endl <<
    indent() << "while (1)" << endl;
  scope_up(out);

  // read beginning field marker
  out <<
    indent() << "/* read the beginning of a field */" << endl <<
    indent() << "if ((ret = thrift_protocol_read_field_begin (protocol, &name, &ftype, &fid, error)) < 0)" << endl <<
    indent() << "{" << endl <<
    indent() << "  if (name) g_free (name);" << endl <<
    indent() << "  return " << error_ret << ";" << endl <<
    indent() << "}" << endl <<
    indent() << "xfer += ret;" << endl <<
    indent() << "if (name) g_free (name);" << endl <<
    endl;

  // check for field STOP marker
  out <<
    indent() << "/* break if we get a STOP field */" << endl <<
    indent() << "if (ftype == T_STOP)" << endl <<
    indent() << "{" << endl <<
    indent() << "  break;" << endl <<
    indent() << "}" << endl <<
    endl;

  // switch depending on the field type
  indent(out) <<
    "switch (fid)" << endl;

  // start switch
  scope_up(out);

  // generate deserialization code for known types
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    indent(out) <<
      "case " << (*f_iter)->get_key() << ":" << endl;
    indent_up();
    indent(out) <<
      "if (ftype == " << type_to_enum ((*f_iter)->get_type()) << ")" << endl;
    indent(out) <<
      "{" << endl;


    indent_up();
    // generate deserialize field
    generate_deserialize_field (out, *f_iter, this_name, "", error_ret);
    indent_down();

    out <<
      indent() << "} else {" << endl <<
      indent() << "  if ((ret = thrift_protocol_skip (protocol, ftype, error)) < 0)" << endl <<
      indent() << "    return " << error_ret << ";" << endl <<
      indent() << "  xfer += ret;" << endl <<
      indent() << "}" << endl <<
      indent() << "break;" << endl;
    indent_down();
  }

  // create the default case
  out <<
    indent() << "default:" << endl <<
    indent() << "  if ((ret = thrift_protocol_skip (protocol, ftype, error)) < 0)" << endl <<
    indent() << "    return " << error_ret << ";" << endl <<
    indent() << "  xfer += ret;" << endl <<
    indent() << "  break;" << endl;

  // end switch
  scope_down(out);

  // read field end marker
  out <<
    indent() << "if ((ret = thrift_protocol_read_field_end (protocol, error)) < 0)" << endl <<
    indent() << "  return " << error_ret << ";" << endl <<
    indent() << "xfer += ret;" << endl;

  // end while loop
  scope_down(out);
  out << endl;

  // read the end of the structure
  out <<
    indent() << "if ((ret = thrift_protocol_read_struct_end (protocol, error)) < 0)" << endl <<
    indent() << "  return " << error_ret << ";" << endl <<
    indent() << "xfer += ret;" << endl <<
    endl;

  // if a required field is missing, throw an error
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      out <<
        indent() << "if (!isset_" << (*f_iter)->get_name() << ")" << endl <<
        indent() << "{" << endl <<
        indent() << "  g_set_error (error, THRIFT_PROTOCOL_ERROR," << endl <<
        indent() << "               THRIFT_PROTOCOL_ERROR_INVALID_DATA," << endl <<
        indent() << "               \"missing field\");" << endl <<
        indent() << "  return -1;" << endl <<
        indent() << "}" << endl <<
        endl;
    }
  }

  if (is_function) {
    indent(out) <<
      "return xfer;" << endl;
  }

  // end the function/structure
  indent_down();
  indent(out) <<
    "}" << endl <<
    endl;
}

void t_c_glib_generator::generate_serialize_field(ofstream &out,
                                                  t_field *tfield,
                                                  string prefix,
                                                  string suffix,
                                                  int error_ret) {
  t_type *type = get_true_type (tfield->get_type());
  string name = prefix + tfield->get_name() + suffix;

  if (type->is_void()) {
    throw "CANNOT GENERATE SERIALIZE CODE FOR void TYPE: " + name;
  }

  if (type->is_struct() || type->is_xception()) {
    generate_serialize_struct (out, (t_struct *) type, name, error_ret);
  } else if (type->is_container()) {
    generate_serialize_container (out, type, name, error_ret);
  } else if (type->is_base_type() || type->is_enum()) {
    indent(out) <<
      "if ((ret = thrift_protocol_write_";

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type *) type)->get_base();
      switch (tbase) {
        case t_base_type::TYPE_VOID:
          throw "compiler error: cannot serialize void field in a struct: "
                + name;
          break;
        case t_base_type::TYPE_BOOL:
          out << "bool (protocol, " << name;
          break;
        case t_base_type::TYPE_BYTE:
          out << "byte (protocol, " << name;
          break;
        case t_base_type::TYPE_I16:
          out << "i16 (protocol, " << name;
          break;
        case t_base_type::TYPE_I32:
          out << "i32 (protocol, " << name;
          break;
        case t_base_type::TYPE_I64:
          out << "i64 (protocol, " << name;
          break;
        case t_base_type::TYPE_DOUBLE:
          out << "double (protocol, " << name;
          break;
        case t_base_type::TYPE_STRING:
          if (((t_base_type *) type)->is_binary()) {
            out << "binary (protocol, ((GByteArray *) " << name <<
                   ")->data, ((GByteArray *) " << name <<
                   ")->len";
          } else {
            out << "string (protocol, " << name;
          }
          break;
        default:
          throw "compiler error: no C writer for base type "
                + t_base_type::t_base_name (tbase) + name;
      }
    } else if (type->is_enum()) {
      out << "i32 (protocol, (gint32) " << name;
    }
    out << ", error)) < 0)" << endl <<
      indent() << "  return " << error_ret << ";" << endl;
  } else {
    printf ("DO NOT KNOW HOW TO SERIALIZE FIELD '%s' TYPE '%s'\n",
            name.c_str(), type_name (type).c_str());
  }
}

void t_c_glib_generator::generate_serialize_struct(ofstream &out,
                                                   t_struct *tstruct,
                                                   string prefix,
                                                   int error_ret) {
  (void) tstruct;
  out <<
    indent() << "if ((ret = thrift_struct_write (THRIFT_STRUCT (" << prefix << "), protocol, error)) < 0)" << endl <<
    indent() << "  return " << error_ret << ";" << endl <<
    indent() << "xfer += ret;" << endl <<
    endl;
}

void t_c_glib_generator::generate_serialize_container(ofstream &out,
                                                      t_type *ttype,
                                                      string prefix,
                                                      int error_ret) {
  scope_up(out);

  if (ttype->is_map()) {
    string length = "g_hash_table_size ((GHashTable *) " + prefix + ")";
    t_type *tkey = ((t_map *) ttype)->get_key_type();
    t_type *tval = ((t_map *) ttype)->get_val_type();
    string tkey_name = type_name (tkey);
    string tval_name = type_name (tval);
    string tkey_ptr = tkey->is_string() || !tkey->is_base_type() ? "" : "*";
    string tval_ptr = tval->is_string() || !tval->is_base_type() ? "" : "*";

    /*
     * Some ugliness here.  To maximize backwards compatibility, we
     * avoid using GHashTableIter and instead get a GList of all keys,
     * then copy it into a array on the stack, and free it.
     * This is because we may exit early before we get a chance to free the
     * GList.
     */
    out <<
      indent() << "if ((ret = thrift_protocol_write_map_begin (protocol, " <<
                   type_to_enum (tkey) << ", " << type_to_enum (tval) <<
                   ", (gint32) " << length << ", error)) < 0)" << endl <<
      indent() << "  return " << error_ret << ";" << endl <<
      indent() << "xfer += ret;" << endl <<
      endl <<
      indent() << "GList *key_list = NULL, *iter = NULL;" << endl <<
      indent() << tkey_name << tkey_ptr << " key;" << endl <<
      indent() << tval_name << tval_ptr << " value;" << endl <<
      indent() << "g_hash_table_foreach ((GHashTable *) " << prefix << 
                   ", thrift_hash_table_get_keys, &key_list);" << endl <<
      indent() << tkey_name << tkey_ptr <<
                   " keys[g_list_length (key_list)];" << endl <<
      indent() << "int i=0, key_count = g_list_length (key_list);" << endl <<
      indent() <<
        "for (iter = g_list_first (key_list); iter; iter = iter->next)" <<
        endl <<
      indent() << "{" << endl <<
      indent() << "  keys[i++] = (" << tkey_name << tkey_ptr <<
                   ") iter->data;" << endl <<
      indent() << "}" << endl <<
      indent() << "g_list_free (key_list);" << endl <<
      endl <<
      indent() << "for (i = 0; i < key_count; ++i)" << endl;

    scope_up(out);
    out <<
      indent() << "key = keys[i];" << endl <<
      indent() << "value = (" << tval_name << tval_ptr <<
                   ") g_hash_table_lookup (((GHashTable *) " << prefix <<
                   "), (gpointer) key);" << endl <<
      endl;
    generate_serialize_map_element (out, (t_map *) ttype, tkey_ptr + " key",
                                    tval_ptr + " value", error_ret);
    scope_down(out);

    out <<
      indent() << "if ((ret = thrift_protocol_write_map_end (protocol, error)) < 0)" << endl <<
      indent() << "  return " << error_ret << ";" << endl <<
      indent() << "xfer += ret;" << endl;
  } else if (ttype->is_set()) {
    string length = "g_hash_table_size ((GHashTable *) " + prefix + ")";
    t_type *telem = ((t_set *) ttype)->get_elem_type();
    string telem_name = type_name (telem);
    string telem_ptr = telem->is_string() || !telem->is_base_type() ? "" : "*";
    out <<
      indent() << "if ((ret = thrift_protocol_write_set_begin (protocol, " <<
                   type_to_enum (telem) << ", (gint32) " << length <<
                   ", error)) < 0)" << endl <<
      indent() << "  return " << error_ret << ";" << endl <<
      indent() << "xfer += ret;" << endl <<
      indent() << "GList *key_list = NULL, *iter = NULL;" << endl <<
      indent() << telem_name << telem_ptr << " elem;" << endl <<
      indent() << "gpointer value;" << endl <<
      indent() << "g_hash_table_foreach ((GHashTable *) " << prefix <<
                   ", thrift_hash_table_get_keys, &key_list);" << endl <<
      indent() << telem_name << telem_ptr << "keys[g_list_length (key_list)];" << endl <<
      indent() << "int i=0, key_count = g_list_length (key_list);" << endl <<
      indent() << "for (iter = g_list_first (key_list); iter; iter = iter->next)" << endl <<
      indent() << "{" << endl <<
      indent() << "  keys[i++] = (" << telem_name << telem_ptr << ") iter->data;" << endl <<
      indent() << "}" << endl <<
      indent() << "g_list_free (key_list);" << endl <<
      endl <<
      indent() << "for (i=0; i<key_count; ++i)" << endl;

    scope_up(out);
    out <<
      indent() << "elem = keys[i];" << endl <<
      indent() << "value = (gpointer) g_hash_table_lookup (((GHashTable *) " <<
                   prefix << "), (gpointer) elem);" << endl <<
      endl;
    generate_serialize_set_element (out, (t_set *) ttype, telem_ptr + "elem",
                                    error_ret);
    scope_down(out);

    out <<
      indent() << "if ((ret = thrift_protocol_write_set_end (protocol, error)) < 0)" << endl <<
      indent() << "  return " << error_ret << ";" << endl <<
      indent() << "xfer += ret;" << endl;
  } else if (ttype->is_list()) {
    string length = prefix + "->len";
    out <<
      indent() << "if ((ret = thrift_protocol_write_list_begin (protocol, " <<
                   type_to_enum (((t_list *) ttype)->get_elem_type()) <<
                   ", (gint32) " << length << ", error)) < 0)" << endl <<
      indent() << "  return " << error_ret << ";" << endl <<
      indent() << "xfer += ret;" << endl <<
      indent() << "guint i;" << endl <<
      indent() << "for (i=0; i<" << length << "; i++)" << endl;

    scope_up(out);
    generate_serialize_list_element (out, (t_list *) ttype, prefix, "i", error_ret);
    scope_down(out);

    out <<
      indent() << "if ((ret = thrift_protocol_write_list_end (protocol, error)) < 0)" << endl <<
      indent() << "  return " << error_ret << ";" << endl <<
      indent() << "xfer += ret;" << endl;
  }

  scope_down(out);
}

void t_c_glib_generator::generate_serialize_map_element(ofstream &out,
                                                        t_map *tmap,
                                                        string key,
                                                        string value,
                                                        int error_ret) {
  t_field kfield (tmap->get_key_type(), key);
  generate_serialize_field (out, &kfield, "", "", error_ret);

  t_field vfield (tmap->get_val_type(), value);
  generate_serialize_field (out, &vfield, "", "", error_ret);
}

void t_c_glib_generator::generate_serialize_set_element(ofstream &out,
                                                        t_set *tset,
                                                        string element,
                                                        int error_ret) {
  t_field efield (tset->get_elem_type(), element);
  generate_serialize_field (out, &efield, "", "", error_ret);
}

void t_c_glib_generator::generate_serialize_list_element(ofstream &out,
                                                         t_list *tlist,
                                                         string list,
                                                         string index,
                                                         int error_ret) {
  t_type *ttype = tlist->get_elem_type();

  // cast to non-const
  string name = "g_ptr_array_index ((GPtrArray *) " + list + ", "
                + index + ")";

  if (ttype->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type *) ttype)->get_base(); 
    switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot determine array type";
        break;
      case t_base_type::TYPE_BOOL:
        name = "g_array_index (" + list + ", gboolean, " + index + ")";
        break;
      case t_base_type::TYPE_BYTE:
        name = "g_array_index (" + list + ", gint8, " + index + ")";
        break;
      case t_base_type::TYPE_I16:
        name = "g_array_index (" + list + ", gint16, " + index + ")";
        break;
      case t_base_type::TYPE_I32:
        name = "g_array_index (" + list + ", gint32, " + index + ")";
        break;
      case t_base_type::TYPE_I64:
        name = "g_array_index (" + list + ", gint64, " + index + ")";
        break;
      case t_base_type::TYPE_DOUBLE:
        name = "g_array_index (" + list + ", gdouble, " + index + ")";
        break;
      case t_base_type::TYPE_STRING:
        break;
      default:
        throw "compiler error: no array info for type";
    }
  }

  t_field efield (ttype, name);
  generate_serialize_field (out, &efield, "", "", error_ret);
}

/* deserializes a field of any type. */
void t_c_glib_generator::generate_deserialize_field(ofstream &out,
                                                    t_field *tfield,
                                                    string prefix,
                                                    string suffix,
                                                    int error_ret) {
  t_type *type = get_true_type (tfield->get_type());

  if (type->is_void()) {
    throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE: " +
      prefix + tfield->get_name();
  }

  string name = prefix + tfield->get_name() + suffix;

  if (type->is_struct() || type->is_xception()) {
    generate_deserialize_struct (out, (t_struct *) type, name, error_ret);
  } else if (type->is_container()) {
    generate_deserialize_container (out, type, name, error_ret);
  } else if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type *) type)->get_base();

    indent(out) << "if ((ret = thrift_protocol_read_";

    switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " + name;
        break;
      case t_base_type::TYPE_STRING:
        if (((t_base_type *) type)->is_binary()) {
          out << "binary (protocol, &data, &len";
        } else {
          out << "string (protocol, &" << name;
        }
        break;
      case t_base_type::TYPE_BOOL:
        out << "bool (protocol, &" << name;
        break;
      case t_base_type::TYPE_BYTE:
        out << "byte (protocol, &" << name;
        break;
      case t_base_type::TYPE_I16:
        out << "i16 (protocol, &" << name;
        break;
      case t_base_type::TYPE_I32:
        out << "i32 (protocol, &" << name;
        break;
      case t_base_type::TYPE_I64:
        out << "i64 (protocol, &" << name;
        break;
      case t_base_type::TYPE_DOUBLE:
        out << "double (protocol, &" << name;
        break;
      default:
        throw "compiler error: no C reader for base type "
          + t_base_type::t_base_name (tbase) + name;
    }
    out << ", error)) < 0)" << endl;
    out << indent() << "  return " << error_ret << ";" << endl <<
           indent() << "xfer += ret;" << endl;

    // load the byte array with the data
    if (tbase == t_base_type::TYPE_STRING
        && ((t_base_type *) type)->is_binary()) {
      indent(out) << name << " = g_byte_array_new();" << endl;
      indent(out) << "g_byte_array_append (" << name << ", (guint8 *) data, (guint) len);" << endl;
      indent(out) << "g_free (data);" << endl;
    }
  } else if (type->is_enum()) {
    string t = tmp ("ecast");
    out <<
      indent() << "gint32 " << t << ";" << endl <<
      indent() << "if ((ret = thrift_protocol_read_i32 (protocol, &" << t << ", error)) < 0)" << endl <<
      indent() << "  return " << error_ret << ";" << endl <<
      indent() << "xfer += ret;" << endl <<
      indent() << name << " = (" << type_name (type) << ")" << t << ";" << endl;
  } else {
    printf ("DO NOT KNOW HOW TO DESERIALIZE FIELD '%s' TYPE '%s'\n",
            tfield->get_name().c_str(), type_name (type).c_str());
  }

  // if the type is not required and this is a thrift struct (no prefix),
  // set the isset variable.  if the type is required, then set the
  // local variable indicating the value was set, so that we can do    // validation later.
  if (tfield->get_req() != t_field::T_REQUIRED && prefix != "") {
    indent(out) << prefix << "__isset_" << tfield->get_name() << suffix << " = TRUE;" << endl;
  } else if (tfield->get_req() == t_field::T_REQUIRED && prefix != "") {
    indent(out) << "isset_" << tfield->get_name() << " = TRUE;" << endl;
  }
}

void t_c_glib_generator::generate_deserialize_struct(ofstream &out,
                                                     t_struct *tstruct,
                                                     string prefix,
                                                     int error_ret) {
  string name_uc = to_upper_case(initial_caps_to_underscores(tstruct->get_name()));
  out <<
    indent() << prefix << " = g_object_new (" << this->nspace_uc << "TYPE_" << name_uc << ", NULL);" << endl <<
    indent() << "if ((ret = thrift_struct_read (THRIFT_STRUCT (" << prefix << "), protocol, error)) < 0)" << endl <<
    indent() << "  return " << error_ret << ";" << endl <<
    indent() << "xfer += ret;" << endl;
}

void t_c_glib_generator::generate_deserialize_container (ofstream &out, t_type *ttype,
                                               string prefix, int error_ret) {
  scope_up(out);

  if (ttype->is_map()) {
    out <<
      indent() << "guint32 size;" << endl <<
      indent() << "ThriftType key_type;" << endl <<
      indent() << "ThriftType value_type;" << endl <<
      endl <<
      indent() << "/* read the map begin marker */" << endl <<
      indent() << "if ((ret = thrift_protocol_read_map_begin (protocol, &key_type, &value_type, &size, error)) < 0)" << endl <<
      indent() << "  return " << error_ret << ";" << endl <<
      indent() << "xfer += ret;" << endl <<
      endl;

    // iterate over map elements
    out <<
      indent() << "/* iterate through each of the map's fields */" << endl <<
      indent() << "guint32 i;" << endl <<
      indent() << "for (i = 0; i < size; i++)" << endl;
    scope_up(out);
    generate_deserialize_map_element (out, (t_map *) ttype, prefix, error_ret);
    scope_down(out);
    out << endl;
  
    // read map end
    out <<
      indent() << "/* read the map end marker */" << endl <<
      indent() << "if ((ret = thrift_protocol_read_map_end (protocol, error)) < 0)" << endl <<
      indent() << "  return " << error_ret << ";" << endl <<
      indent() << "xfer += ret;" << endl;
  } else if (ttype->is_set()) {
    out <<
      indent() << "guint32 size;" << endl <<
      indent() << "ThriftType element_type;" << endl <<
      indent() << "if ((ret = thrift_protocol_read_set_begin (protocol, &element_type, &size, error)) < 0)" << endl <<
      indent() << "  return " << error_ret << ";" << endl <<
      indent() << "xfer += ret;" << endl <<
      endl;

    // iterate over the elements
    out <<
      indent() << "/* iterate through the set elements */" << endl <<
      indent() << "guint32 i;" << endl <<
      indent() << "for (i = 0; i < size; ++i)" << endl;

    scope_up(out);
    generate_deserialize_set_element (out, (t_set *) ttype, prefix, error_ret);
    scope_down(out);

    // read set end
    out <<
      indent() << "if ((ret = thrift_protocol_read_set_end (protocol, error)) < 0)" << endl <<
      indent() << "  return " << error_ret << ";" << endl <<
      indent() << "xfer += ret;" << endl <<
      endl;
  } else if (ttype->is_list()) {
    out <<
      indent() << "guint32 size;" << endl <<
      indent() << "ThriftType element_type;" << endl <<
      indent() << "if ((ret = thrift_protocol_read_list_begin (protocol, &element_type, &size, error)) < 0)" << endl <<
      indent() << "  return " << error_ret << ";" << endl <<
      indent() << "xfer += ret;" << endl <<
      endl;

    out <<
      indent() << "/* iterate through list elements */" << endl <<
      indent() << "guint32 i;" << endl <<
      indent() << "for (i = 0; i < size; i++)" << endl;

    scope_up(out);
    generate_deserialize_list_element (out, (t_list *) ttype, prefix, "i",
                                       error_ret);
    scope_down(out);

    out <<
      indent() << "if ((ret = thrift_protocol_read_list_end (protocol, error)) < 0)" << endl << 
      indent() << "  return " << error_ret << ";" << endl <<
      indent() << "xfer += ret;" << endl <<
      endl;
  }

  scope_down(out);
}

void t_c_glib_generator::generate_deserialize_map_element(ofstream &out,
                                                          t_map *tmap,
                                                          string prefix,
                                                          int error_ret) {
  t_type *tkey = tmap->get_key_type();
  t_type *tval = tmap->get_val_type();
  string tkey_name = type_name (tkey);
  string tval_name = type_name (tval);
  string tkey_ptr = tkey->is_string() || !tkey->is_base_type() ? "" : "*";
  string tval_ptr = tval->is_string() || !tval->is_base_type() ? "" : "*";

  string keyname = tmp("key");
  string valname = tmp("val");

  if (tkey->is_map()) {
    out <<
      indent() << tkey_name << tkey_ptr << " " << keyname << " = g_hash_table_new (NULL, NULL);" << endl;
  } else {
    out <<
      indent() << tkey_name << tkey_ptr << " " << keyname << (tkey_ptr != "" ? " = g_new (" + tkey_name + ", 1)" : "") << ";" << endl;
  }

  if (tval->is_map()) {
    out <<
      indent() << tval_name << tval_ptr << " " << valname << " = g_hash_table_new (NULL, NULL);" << endl;
  } else {
    out <<
      indent() << tval_name << tval_ptr << " " << valname << (tval_ptr != "" ? " = g_new (" + tval_name + ", 1)" : "") << ";" << endl;
  }

  // deserialize the fields of the map element
  t_field fkey (tkey, tkey_ptr + keyname);
  generate_deserialize_field (out, &fkey, "", "", error_ret);
  t_field fval (tval, tval_ptr + valname);
  generate_deserialize_field (out, &fval, "", "", error_ret);

  // insert into the hashtable.  if the field is not a pointer, then use
  // the address of the object.
  indent(out) <<
    "g_hash_table_insert ((GHashTable *)" << prefix << ", (gpointer) " << (tkey_ptr != "" ? "" : "&") << keyname << ", (gpointer) " << (tval_ptr != "" ? "" : "&") << valname << ");" << endl;
}

void t_c_glib_generator::generate_deserialize_set_element(ofstream &out,
                                                          t_set *tset,
                                                          string prefix,
                                                          int error_ret) {
  t_type *telem = tset->get_elem_type();
  string telem_name = type_name (telem);
  string telem_ptr = telem->is_string() || !telem->is_base_type() ? "" : "*";

  if (telem->is_map()) {
    out <<
      indent() << telem_name << telem_ptr << " elem = g_hash_table_new (NULL, NULL);" << endl;
  } else {
    out <<
      indent() << telem_name << telem_ptr << " elem" << (telem_ptr != "" ? " = g_new (" + telem_name + ", 1)" : "") << ";" << endl;
  }

  t_field felem (telem, telem_ptr + "elem");
  generate_deserialize_field (out, &felem, "", "", error_ret);

  indent(out) <<
    "g_hash_table_insert ((GHashTable *) " << prefix << ", (gpointer) elem, (gpointer) 1);" << endl;
}

void t_c_glib_generator::generate_deserialize_list_element(ofstream &out,
                                                           t_list *tlist,
                                                           string prefix,
                                                           string index,
                                                           int error_ret) {
  (void) index;
  string elem = tmp ("_elem");
  t_field felem (tlist->get_elem_type(), elem);

  indent(out) << declare_field (&felem, true) << endl;
  generate_deserialize_field (out, &felem, "", "", error_ret);

  indent(out);

  t_type *ttype = tlist->get_elem_type();
  if (ttype->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type *) ttype)->get_base();
    switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot determine array type";
      case t_base_type::TYPE_STRING:
        out << "g_ptr_array_add (" << prefix << ", " << elem << ");" << endl;
        return;
      case t_base_type::TYPE_BOOL:
      case t_base_type::TYPE_BYTE:
      case t_base_type::TYPE_I16:
      case t_base_type::TYPE_I32:
      case t_base_type::TYPE_I64:
      case t_base_type::TYPE_DOUBLE:
        out << "g_array_append_val (" << prefix << ", " << elem << ");" << endl;
        return;
      default:
        throw "compiler error: no array info for type";
    }
  }
  out << "g_ptr_array_add (" << prefix << ", " << elem << ");" << endl;
}

string t_c_glib_generator::generate_new_hash_from_type (t_type * ttype) {
  if (ttype->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type *) ttype)->get_base();
    switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot determine hash type";
        break;
      case t_base_type::TYPE_BOOL:
        return "g_hash_table_new (thrift_gboolean_hash, thrift_gboolean_equal);";
      case t_base_type::TYPE_BYTE:
        return "g_hash_table_new (thrift_gint8_hash, thrift_gint8_equal);";
      case t_base_type::TYPE_I16:
        return "g_hash_table_new (thrift_gint16_hash, thrift_gint16_equal);";
      case t_base_type::TYPE_I32:
        return "g_hash_table_new (thrift_gint32_hash, thrift_gint32_equal);";
      case t_base_type::TYPE_I64:
        return "g_hash_table_new (thrift_gint64_hash, thrift_gint64_equal);";
      case t_base_type::TYPE_DOUBLE:
        return "g_hash_table_new (thrift_gdouble_hash, thrift_gdouble_equal);";
      case t_base_type::TYPE_STRING:
        return "g_hash_table_new (g_str_hash, g_str_equal);";
      default:
        throw "compiler error: no hash table info for type";
    }
  }
  return "g_hash_table_new (NULL, NULL);";
}

string t_c_glib_generator::generate_new_array_from_type(t_type * ttype) {
  if (ttype->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type *) ttype)->get_base();
    switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot determine array type";
        break;
      case t_base_type::TYPE_BOOL:
        return "g_array_new (0, 1, sizeof (gboolean));";
      case t_base_type::TYPE_BYTE:
        return "g_array_new (0, 1, sizeof (gint8));";
      case t_base_type::TYPE_I16:
        return "g_array_new (0, 1, sizeof (gint16));";
      case t_base_type::TYPE_I32:
        return "g_array_new (0, 1, sizeof (gint32));";
      case t_base_type::TYPE_I64:
        return "g_array_new (0, 1, sizeof (gint64));";
      case t_base_type::TYPE_DOUBLE:
        return "g_array_new (0, 1, sizeof (gdouble));";
      case t_base_type::TYPE_STRING:
        return "g_ptr_array_new();";
      default:
        throw "compiler error: no array info for type";
    }
  }
  return "g_ptr_array_new();";
}


/***************************************
 * UTILITY FUNCTIONS                   *
 ***************************************/

/**
 * Upper case a string.  Wraps boost's string utility.
 */
string to_upper_case(string name) {
  string s (name);
  std::transform (s.begin(), s.end(), s.begin(), ::toupper);
  return s;
//  return boost::to_upper_copy (name);
}

/**
 * Lower case a string.  Wraps boost's string utility.
 */
string to_lower_case(string name) {
  string s (name);
  std::transform (s.begin(), s.end(), s.begin(), ::tolower);
  return s;
//  return boost::to_lower_copy (name);
}

/**
 * Makes a string friendly to C code standards by lowercasing and adding
 * underscores, with the exception of the first character.  For example:
 *
 * Input: "ZomgCamelCase"
 * Output: "zomg_camel_case"
 */
string initial_caps_to_underscores(string name) {
  string ret;
  const char *tmp = name.c_str();
  int pos = 0;

  /* the first character isn't underscored if uppercase, just lowercased */
  ret += tolower (tmp[pos]);
  pos++;
  for (unsigned int i = pos; i < name.length(); i++) {
    char lc = tolower (tmp[i]); 
    if (lc != tmp[i]) {
      ret += '_';
    }
    ret += lc;
  }

  return ret;
}

/* register this generator with the main program */
THRIFT_REGISTER_GENERATOR(c_glib, "C, using GLib", "")
