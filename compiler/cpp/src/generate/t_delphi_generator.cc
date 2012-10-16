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

#include <string>
#include <fstream>
#include <iostream>
#include <vector>
#include <list>

#include <stdlib.h>
#include <sys/stat.h>
#include <sstream>
#include <cctype>

#include "platform.h"
#include "t_oop_generator.h"

using namespace std;


class t_delphi_generator : public t_oop_generator
{
  public:
    t_delphi_generator(
        t_program* program,
        const std::map<std::string, std::string>& parsed_options,
        const std::string& option_string)
      : t_oop_generator(program)
    {
      (void) option_string;

      std::map<std::string, std::string>::const_iterator iter;

      iter = parsed_options.find("ansistr_binary");
      ansistr_binary_ = (iter != parsed_options.end());

      out_dir_base_ = "gen-delphi";
      escape_.clear();
      escape_['\''] = "''";
    }


    void init_generator();
    void close_generator();

    void generate_consts(std::vector<t_const*> consts);

    void generate_typedef (t_typedef* ttypedef);
    void generate_enum (t_enum* tenum);
    void generate_struct (t_struct* tstruct);
    void generate_xception (t_struct* txception);
    void generate_service (t_service* tservice);
    void generate_property(ostream& out, t_field* tfield, bool isPublic,  bool is_xception);
    void generate_property_writer_(ostream& out, t_field* tfield, bool isPublic);

    void generate_delphi_property(ostream& out, bool struct_is_exception, t_field* tfield, bool isPublic, std::string fieldPrefix = "");
    void generate_delphi_isset_reader_definition(ostream& out, t_field* tfield, bool is_xception);
    void generate_delphi_property_reader_definition(ostream& out, t_field* tfield, bool is_xception_class);
    void generate_delphi_property_writer_definition(ostream& out, t_field* tfield, bool is_xception_class);
    void generate_delphi_property_reader_impl(ostream& out, std::string cls_prefix, std::string name, t_type* type, t_field* tfield, std::string fieldPrefix, bool is_xception_class);
    void generate_delphi_property_writer_impl(ostream& out, std::string cls_prefix, std::string name, t_type* type, t_field* tfield, std::string fieldPrefix, bool is_xception_class, bool is_xception_factory, std::string xception_factroy_name);
    void generate_delphi_isset_reader_impl(ostream& out, std::string cls_prefix, std::string name, t_type* type, t_field* tfield, std::string fieldPrefix, bool is_xception);
    void generate_delphi_struct_writer_impl(ostream& out, std::string cls_prefix, t_struct* tstruct, bool is_exception);
    void generate_delphi_struct_result_writer_impl(ostream& out, std::string cls_prefix, t_struct* tstruct, bool is_exception);

    void generate_delphi_struct_tostring_impl(ostream& out, std::string cls_prefix, t_struct* tstruct, bool is_exception, bool is_x_factory);

    void add_delphi_uses_list( string unitname);

    void generate_delphi_struct_reader_impl(ostream& out, std::string cls_prefix, t_struct* tstruct, bool is_exception);
    void generate_delphi_create_exception_impl(ostream& out, string cls_prefix, t_struct* tstruct, bool is_exception);

    void print_const_prop(std::ostream& out, string name, t_type* type, t_const_value* value);
    void print_private_field(std::ostream& out, string name, t_type* type, t_const_value* value);
    void print_const_value ( std::ostream& vars, std::ostream& out, std::string name, t_type* type, t_const_value* value);
    void initialize_field(std::ostream& vars, std::ostream& out, std::string name, t_type* type, t_const_value* value);
    void finalize_field(std::ostream& out, std::string name, t_type* type, t_const_value* value, std::string cls_nm = "");
    std::string render_const_value( std::ostream& local_vars, std::ostream& out, std::string name, t_type* type, t_const_value* value);
    void print_const_def_value( std::ostream& vars, std::ostream& out, std::string name, t_type* type, t_const_value* value, std::string cls_nm = "");

    void generate_delphi_struct(t_struct* tstruct, bool is_exception);
    void generate_delphi_struct_impl( ostream& out, std::string cls_prefix, t_struct* tstruct, bool is_exception, bool is_result = false, bool is_x_factory = false);
    void generate_delphi_struct_definition(std::ostream& out, t_struct* tstruct, bool is_xception=false, bool in_class=false, bool is_result=false, bool is_x_factory = false);
    void generate_delphi_struct_reader(std::ostream& out, t_struct* tstruct);
    void generate_delphi_struct_result_writer(std::ostream& out, t_struct* tstruct);
    void generate_delphi_struct_writer(std::ostream& out, t_struct* tstruct);
    void generate_delphi_struct_tostring(std::ostream& out, t_struct* tstruct);

    void generate_function_helpers(t_function* tfunction);
    void generate_service_interface (t_service* tservice);
    void generate_service_helpers (t_service* tservice);
    void generate_service_client (t_service* tservice);
    void generate_service_server (t_service* tservice);
    void generate_process_function (t_service* tservice, t_function* function);

    void generate_deserialize_field (std::ostream& out, bool is_xception, t_field* tfield, std::string prefix, std::ostream& local_vars);
    void generate_deserialize_struct (std::ostream& out, t_struct* tstruct, std::string name, std::string prefix);
    void generate_deserialize_container(ostream& out, bool is_xception, t_type* ttype, string name, std::ostream& local_vars);

    void generate_deserialize_set_element (std::ostream& out, bool is_xception, t_set* tset, std::string prefix, std::ostream& local_vars);
    void generate_deserialize_map_element (std::ostream& out, bool is_xception, t_map* tmap, std::string prefix, std::ostream& local_vars);
    void generate_deserialize_list_element (std::ostream& out, bool is_xception, t_list* list, std::string prefix, std::ostream& local_vars);

    void generate_serialize_field (std::ostream& out, bool is_xception, t_field* tfield, std::string prefix, std::ostream& local_vars);
    void generate_serialize_struct (std::ostream& out, t_struct* tstruct, std::string prefix, std::ostream& local_vars);
    void generate_serialize_container (std::ostream& out, bool is_xception, t_type* ttype, std::string prefix, std::ostream& local_vars);
    void generate_serialize_map_element (std::ostream& out, bool is_xception, t_map* tmap, std::string iter, std::string map, std::ostream& local_vars);
    void generate_serialize_set_element (std::ostream& out, bool is_xception, t_set* tmap, std::string iter, std::ostream& local_vars);
    void generate_serialize_list_element (std::ostream& out, bool is_xception, t_list* tlist, std::string iter, std::ostream& local_vars);

    void delphi_type_usings(std::ostream& out);
    std::string delphi_thrift_usings();

    std::string type_name( t_type* ttype, bool b_cls=false, bool b_no_postfix=false, bool b_exception_factory=false, bool b_full_exception_factory = false);
    std::string normalize_clsnm(std::string name, std::string prefix, bool b_no_check_keyword = false);
    std::string input_arg_prefix( t_type* ttype);
  
    std::string base_type_name(t_base_type* tbase);
    std::string declare_field(t_field* tfield, bool init=false, std::string prefix="", bool is_xception_class = false);
    std::string function_signature(t_function* tfunction, std::string full_cls="", bool is_xception = false);  
    std::string argument_list(t_struct* tstruct);
    std::string constructor_argument_list(t_struct* tstruct, std::string current_indent);
    std::string type_to_enum(t_type* ttype);
    std::string prop_name(t_field* tfield, bool is_xception = false);
    std::string prop_name(std::string name, bool is_xception = false);
    std::string constructor_param_name(string name);

    void write_enum(std::string line);
    void write_forward_decr(std::string line);
    void write_const(std::string line);
    void write_struct(std::string line);
    void write_service(std::string line);

    virtual std::string autogen_comment() {
      return
        std::string("(**\n") +
        " * Autogenerated by Thrift Compiler (" + THRIFT_VERSION + ")\n" +
        " *\n" +
        " * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING\n" +
        " *)\n";
    }

    bool type_can_be_null(t_type* ttype) {
      while (ttype->is_typedef()) {
        ttype = ((t_typedef*)ttype)->get_type();
      }

      return ttype->is_container() ||
        ttype->is_struct() ||
        ttype->is_xception();
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
    bool has_enum;
    bool has_const;
    std::string namespace_dir_;
    std::map<std::string, int> delphi_keywords;
    std::map<std::string, int> delphi_reserved_method;
    std::map<std::string, int> delphi_reserved_method_exception;
    std::map<std::string, int> types_known;
    std::list<t_typedef*> typedefs_pending;
    std::vector<std::string> uses_list;
    void create_keywords();
    bool find_keyword( std::map<std::string, int>& keyword_map, std::string name);
    std::string normalize_name( std::string name, bool b_method = false, bool b_exception_method = false);
    std::string empty_value(t_type* type);
    bool is_fully_defined_type( t_type* ttype);
    void add_defined_type( t_type* ttype);
    void init_known_types_list();
    bool is_void( t_type* type );
    int indent_impl_;
    bool ansistr_binary_;
    void indent_up_impl(){
      ++indent_impl_;
    };
    void indent_down_impl() {
      --indent_impl_;
    };
    std::string indent_impl() {
      std::string ind = "";
      int i;
      for (i = 0; i < indent_impl_; ++i) {
        ind += "  ";
      }
      return ind;
    };
    std::ostream& indent_impl(std::ostream &os) {
      return os << indent_impl();
    };
};

bool t_delphi_generator::find_keyword( std::map<std::string, int>& keyword_map, std::string name) {
  int len = name.length();

  if ( len <= 0 ) {
    return false;
  }

  int nlast = name.find_last_of('_');

  if ( nlast >= 1) {
    if (nlast == (len - 1)) {
      string new_name( name, 0, nlast);
      return find_keyword( keyword_map, new_name);
    }
  }
  return (keyword_map[name] == 1);
}

std::string t_delphi_generator::normalize_name( std::string name, bool b_method, bool b_exception_method) {
  string tmp( name );
  std::transform(tmp.begin(), tmp.end(), tmp.begin(), static_cast<int (*)(int)>(std::tolower));

  bool b_found = false;

  if ( find_keyword( delphi_keywords, tmp) ) {
    b_found = true;
  } else if ( b_method && find_keyword( delphi_reserved_method, tmp)) {
    b_found = true;
  } else if ( b_exception_method && find_keyword( delphi_reserved_method_exception, tmp)) {
    b_found = true;
  }

  if (b_found) {
    return name + "_";
  } else {
    return name;
  }
}

void t_delphi_generator::create_keywords() {
  delphi_keywords["and"] = 1;
  delphi_keywords["end"] = 1;
  delphi_keywords["interface"] = 1;
  delphi_keywords["raise"] = 1;
  delphi_keywords["uses"] = 1;
  delphi_keywords["array"] = 1;
  delphi_keywords["except"] = 1;
  delphi_keywords["is"] = 1;
  delphi_keywords["record"] = 1;
  delphi_keywords["var"] = 1;
  delphi_keywords["as"] = 1;
  delphi_keywords["exports"] = 1;
  delphi_keywords["label"] = 1;
  delphi_keywords["repeat"] = 1;
  delphi_keywords["while"] = 1;
  delphi_keywords["asm"] = 1;
  delphi_keywords["file"] = 1;
  delphi_keywords["library"] = 1;
  delphi_keywords["resourcestring"] = 1;
  delphi_keywords["with"] = 1;
  delphi_keywords["begin"] = 1;
  delphi_keywords["finalization"] = 1;
  delphi_keywords["mod"] = 1;
  delphi_keywords["set"] = 1;
  delphi_keywords["xor"] = 1;
  delphi_keywords["case"] = 1;
  delphi_keywords["finally"] = 1;
  delphi_keywords["nil"] = 1;
  delphi_keywords["shl"] = 1;
  delphi_keywords["class"] = 1;
  delphi_keywords["for"] = 1;
  delphi_keywords["not"] = 1;
  delphi_keywords["shr"] = 1;
  delphi_keywords["const"] = 1;
  delphi_keywords["function"] = 1;
  delphi_keywords["object"] = 1;
  delphi_keywords["string"] = 1;
  delphi_keywords["constructor"] = 1;
  delphi_keywords["goto"] = 1;
  delphi_keywords["of"] = 1;
  delphi_keywords["then"] = 1;
  delphi_keywords["destructor"] = 1;
  delphi_keywords["if"] = 1;
  delphi_keywords["or"] = 1;
  delphi_keywords["threadvar"] = 1;
  delphi_keywords["dispinterface"] = 1;
  delphi_keywords["implementation"] = 1;
  delphi_keywords["out"] = 1;
  delphi_keywords["to"] = 1;
  delphi_keywords["div"] = 1;
  delphi_keywords["in"] = 1;
  delphi_keywords["packed"] = 1;
  delphi_keywords["try"] = 1;
  delphi_keywords["do"] = 1;
  delphi_keywords["inherited"] = 1;
  delphi_keywords["procedure"] = 1;
  delphi_keywords["type"] = 1;
  delphi_keywords["downto"] = 1;
  delphi_keywords["initialization"] = 1;
  delphi_keywords["program"] = 1;
  delphi_keywords["unit"] = 1;
  delphi_keywords["else"] = 1;
  delphi_keywords["inline"] = 1;
  delphi_keywords["property"] = 1;
  delphi_keywords["until"] = 1;
  delphi_keywords["private"] = 1;
  delphi_keywords["protected"] = 1;
  delphi_keywords["public"] = 1;
  delphi_keywords["published"] = 1;
  delphi_keywords["automated"] = 1;
  delphi_keywords["at"] = 1;
  delphi_keywords["on"] = 1;

  delphi_reserved_method["create"] = 1;
  delphi_reserved_method["free"] = 1;
  delphi_reserved_method["initinstance"] = 1;
  delphi_reserved_method["cleanupinstance"] = 1;
  delphi_reserved_method["classtype"] = 1;
  delphi_reserved_method["classname"] = 1;
  delphi_reserved_method["classnameis"] = 1;
  delphi_reserved_method["classparent"] = 1;
  delphi_reserved_method["classinfo"] = 1;
  delphi_reserved_method["instancesize"] = 1;
  delphi_reserved_method["inheritsfrom"] = 1;
  delphi_reserved_method["methodaddress"] = 1;
  delphi_reserved_method["methodaddress"] = 1;
  delphi_reserved_method["methodname"] = 1;
  delphi_reserved_method["fieldaddress"] = 1;
  delphi_reserved_method["fieldaddress"] = 1;
  delphi_reserved_method["getinterface"] = 1;
  delphi_reserved_method["getinterfaceentry"] = 1;
  delphi_reserved_method["getinterfacetable"] = 1;
  delphi_reserved_method["unitname"] = 1;
  delphi_reserved_method["equals"] = 1;
  delphi_reserved_method["gethashcode"] = 1;
  delphi_reserved_method["tostring"] = 1;
  delphi_reserved_method["safecallexception"] = 1;
  delphi_reserved_method["afterconstruction"] = 1;
  delphi_reserved_method["beforedestruction"] = 1;
  delphi_reserved_method["dispatch"] = 1;
  delphi_reserved_method["defaulthandler"] = 1;
  delphi_reserved_method["newinstance"] = 1;
  delphi_reserved_method["freeinstance"] = 1;
  delphi_reserved_method["destroy"] = 1;
  delphi_reserved_method["read"] = 1;
  delphi_reserved_method["write"] = 1;

  delphi_reserved_method_exception["setinnerexception"] = 1;
  delphi_reserved_method_exception["setstackinfo"] = 1;
  delphi_reserved_method_exception["getstacktrace"] = 1;
  delphi_reserved_method_exception["raisingexception"] = 1;
  delphi_reserved_method_exception["createfmt"] = 1;
  delphi_reserved_method_exception["createres"] = 1;
  delphi_reserved_method_exception["createresfmt"] = 1;
  delphi_reserved_method_exception["createhelp"] = 1;
  delphi_reserved_method_exception["createfmthelp"] = 1;
  delphi_reserved_method_exception["createreshelp"] = 1;
  delphi_reserved_method_exception["createresfmthelp"] = 1;
  delphi_reserved_method_exception["getbaseexception"] = 1;
  delphi_reserved_method_exception["baseexception"] = 1;
  delphi_reserved_method_exception["helpcontext"] = 1;
  delphi_reserved_method_exception["innerexception"] = 1;
  delphi_reserved_method_exception["message"] = 1;
  delphi_reserved_method_exception["stacktrace"] = 1;
  delphi_reserved_method_exception["stackinfo"] = 1;
  delphi_reserved_method_exception["getexceptionstackinfoproc"] = 1;
  delphi_reserved_method_exception["getstackinfostringproc"] = 1;
  delphi_reserved_method_exception["cleanupstackinfoproc"] = 1;
  delphi_reserved_method_exception["raiseouterexception"] = 1;
  delphi_reserved_method_exception["throwouterexception"] = 1;
}

void t_delphi_generator::add_delphi_uses_list( string unitname){
  vector<std::string>::const_iterator s_iter;
  bool found = false;
  for (s_iter = uses_list.begin(); s_iter != uses_list.end(); ++s_iter) {
    if ((*s_iter) == unitname ) {
      found = true;
      break;
    }
  }
  if (! found) {
    uses_list.push_back( unitname );
  }
}

void t_delphi_generator::init_generator() {
  indent_impl_ = 0;
  namespace_name_ = program_->get_namespace("delphi");
  has_enum = false;
  has_const = false;
  create_keywords();
  add_delphi_uses_list("Classes");
  add_delphi_uses_list("SysUtils");
  add_delphi_uses_list("Generics.Collections");
  add_delphi_uses_list("Thrift");
  add_delphi_uses_list("Thrift.Utils");
  add_delphi_uses_list("Thrift.Collections");
  add_delphi_uses_list("Thrift.Protocol");
  add_delphi_uses_list("Thrift.Transport");

  init_known_types_list();
  
  string unitname, nsname;
  const vector<t_program*>& includes = program_->get_includes();
  for (size_t i = 0; i < includes.size(); ++i) {
    unitname = includes[i]->get_name();
    nsname = includes[i]->get_namespace("delphi");
    if ( "" != nsname) {
      unitname = nsname;
    }
    add_delphi_uses_list(unitname);
  }


  MKDIR(get_out_dir().c_str());
}

void t_delphi_generator::close_generator() { 
  std::string unitname = program_name_;
  if( "" != namespace_name_) {
    unitname = namespace_name_;
  }

  for ( int i = 0; i < (int)unitname.size(); i++) {
    if ( unitname[i] == ' ' ) {
      unitname.replace( i, 1, "_" );
    }
  }

  std::string f_name = get_out_dir() + "/" + unitname + ".pas";
  std::ofstream f_all;

  f_all.open( f_name.c_str() );

  f_all << autogen_comment() << endl;
  f_all << "unit " << unitname << ";" << endl << endl;
  f_all << "interface" << endl << endl;
  f_all  << "uses"  << endl;

  indent_up();

  vector<std::string>::const_iterator s_iter;
  for (s_iter = uses_list.begin(); s_iter != uses_list.end(); ++s_iter) {
    if (s_iter != uses_list.begin()) {
      f_all << ",";
      f_all << endl;
    }
    indent(f_all) << *s_iter;
  }

  f_all << ";" << endl << endl;

  indent_down();

  string tmp_unit( unitname );
  for ( int i = 0; i < (int)tmp_unit.size(); i++) {
    if ( tmp_unit[i] == '.' ) {
      tmp_unit.replace( i, 1, "_" );
    }
  }

  f_all  << "const"  << endl;
  indent_up();
  indent(f_all)  << "c" << tmp_unit << "_Option_AnsiStr_Binary = " << ( ansistr_binary_ ? "True" : "False") << ";" << endl;
  indent_down();

  f_all  << "type"  << endl;
  f_all  << s_forward_decr.str();
  if (has_enum) {
    indent(f_all) << endl;
    indent(f_all) << "{$SCOPEDENUMS ON}" << endl << endl;
    f_all  << s_enum.str();
    indent(f_all) << "{$SCOPEDENUMS OFF}" << endl << endl;
  }
  f_all  << s_struct.str();
  f_all  << s_service.str();
  f_all  << s_const.str();
  f_all  << "implementation"  << endl << endl;
  f_all  << s_struct_impl.str();
  f_all  << s_service_impl.str();
  f_all  << s_const_impl.str();

  if ( has_const ) {
    f_all  << "{$IF CompilerVersion < 21.0}" << endl;
    f_all  << "initialization" << endl;
    f_all  << "begin" << endl;
    f_all  << "  TConstants_Initialize;" << endl;
    f_all  << "end;" << endl << endl;

    f_all  << "finalization" << endl;
    f_all  << "begin" << endl;
    f_all  << "  TConstants_Finalize;" << endl;
    f_all  << "end;" << endl;
    f_all  << "{$IFEND}" << endl << endl;
  }
  f_all  << "end." << endl;
  f_all.close();
  
  if( ! typedefs_pending.empty())
    printf("pending typedefs with unresolved references are left\n");
}

void t_delphi_generator::delphi_type_usings( ostream& out) {
  indent_up();
  indent(out) << "Classes, SysUtils, Generics.Collections, Thrift.Collections, Thrift.Protocol," << endl;
  indent(out) << "Thrift.Transport;" << endl << endl;
  indent_down();
}

void t_delphi_generator::generate_typedef(t_typedef* ttypedef) {
  t_type* type = ttypedef->get_type();

  // write now or save for later?  
  if( ! is_fully_defined_type( type)) {
    typedefs_pending.push_back( ttypedef); 
    return;
  }
  
  indent_up();
  indent(s_struct) << 
    type_name(ttypedef) << " = ";

  bool container = type->is_list() || type->is_map() || type->is_set();

  // commented out: the benefit is not big enough to risk breaking existing code
  //if( ! container)
  //  s_struct << "type ";  //the "type A = type B" syntax leads to E2574 with generics

  s_struct << type_name(ttypedef->get_type(), ! container) << ";" << endl <<
    endl;
  indent_down();
  
  add_defined_type( ttypedef);
}

bool t_delphi_generator::is_fully_defined_type( t_type* ttype) {
  if (ttype->is_typedef()) {
    return (1 == types_known[ type_name(ttype)]);
  }
  
  if (ttype->is_base_type()) {
    return (1 == types_known[ base_type_name((t_base_type*)ttype)]);
  } else if (ttype->is_enum()) {
    return true;  // enums are written first, before all other types
  } else if (ttype->is_map()) {
    t_map *tmap = (t_map*) ttype;
    return is_fully_defined_type( tmap->get_key_type()) &&
           is_fully_defined_type( tmap->get_val_type());
  } else if (ttype->is_set()) {
    t_set* tset = (t_set*) ttype;
    return is_fully_defined_type( tset->get_elem_type());
  } else if (ttype->is_list()) {
    t_list* tlist = (t_list*) ttype;
    return is_fully_defined_type( tlist->get_elem_type());
  }

  return (1 == types_known[ type_name(ttype)]);
}

void t_delphi_generator::add_defined_type( t_type* ttype) {
  // mark as known type
  types_known[ type_name(ttype)] = 1;
  
  // check all pending typedefs
  std::list<t_typedef*>::iterator  iter;
  bool more = true;
  while( more && (! typedefs_pending.empty()))
  {
    more = false;
    
    for( iter = typedefs_pending.begin(); typedefs_pending.end() != iter; ++iter)
    {
      t_typedef* ttypedef = (*iter);
      if( is_fully_defined_type( ttypedef->get_type()))
      {
        typedefs_pending.erase( iter);
        generate_typedef( ttypedef);
        more = true;
        break;
      }
    }
  }
}

void t_delphi_generator::init_known_types_list() {
  // known base types
  types_known[ type_name( g_type_string)] = 1;
  types_known[ type_name( g_type_binary)] = 1;
  types_known[ type_name( g_type_bool)] = 1;
  types_known[ type_name( g_type_byte)] = 1;
  types_known[ type_name( g_type_i16)] = 1;
  types_known[ type_name( g_type_i32)] = 1;
  types_known[ type_name( g_type_i64)] = 1;
  types_known[ type_name( g_type_double)] = 1;
}

void t_delphi_generator::generate_enum(t_enum* tenum) {
  has_enum = true;
  indent_up();
  indent(s_enum) <<
    type_name(tenum,true,true) << " = " <<  "("  << endl;
  indent_up();
  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator c_iter;
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    int value = (*c_iter)->get_value();
    if (c_iter != constants.begin()) {
      s_enum << ",";
      s_enum << endl;
    }
    indent(s_enum) << normalize_name((*c_iter)->get_name()) << " = " << value;
  }
  s_enum << endl;
  indent_down();
  indent(s_enum) << ");" << endl << endl;
  indent_down();
}

void t_delphi_generator::generate_consts(std::vector<t_const*> consts) {
  if (consts.empty()){
    return;
  }

  has_const = true;

  indent_up();
  indent(s_const) <<
    "TConstants = class" << endl;
  indent(s_const) << "private" << endl;
  indent_up();
  vector<t_const*>::iterator c_iter;
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    print_private_field(s_const, normalize_name((*c_iter)->get_name()),
      (*c_iter)->get_type(), (*c_iter)->get_value());
  }
  indent_down();
  indent(s_const) << "public" << endl;
  indent_up();
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    print_const_prop(s_const, normalize_name((*c_iter)->get_name()), 
      (*c_iter)->get_type(), (*c_iter)->get_value());
  }
  indent(s_const) << "{$IF CompilerVersion >= 21.0}" << endl;
  indent(s_const) << "class constructor Create;" << endl;
  indent(s_const) << "class destructor Destroy;" << endl;
  indent(s_const) << "{$IFEND}" << endl;
  indent_down();
  indent(s_const) << "end;" << endl << endl;
  indent_down();

  std::ostringstream  vars, code;

  indent_up_impl();
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    initialize_field(vars, code, "F" + prop_name( (*c_iter)->get_name()),
      (*c_iter)->get_type(), (*c_iter)->get_value());
  }
  indent_down_impl();

  indent_impl(s_const_impl) << "{$IF CompilerVersion >= 21.0}" << endl;
  indent_impl(s_const_impl) << "class constructor TConstants.Create;" << endl;

  if ( ! vars.str().empty() ) {
    indent_impl(s_const_impl) << "var" << endl;
    s_const_impl << vars.str();
  }
  indent_impl(s_const_impl) << "begin" << endl;
  if ( ! code.str().empty() ) {
    s_const_impl << code.str();
  }
  indent_impl(s_const_impl) << "end;" << endl << endl;
  indent_impl(s_const_impl) << "class destructor TConstants.Destroy;" << endl;
  indent_impl(s_const_impl) << "begin" << endl;
  indent_up_impl();
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    finalize_field(s_const_impl, normalize_name( (*c_iter)->get_name()),
      (*c_iter)->get_type(), (*c_iter)->get_value());
  }
  indent_impl(s_const_impl) << "inherited;" << endl;
  indent_down_impl();
  indent_impl(s_const_impl) << "end;" << endl;
  indent_impl(s_const_impl) << "{$ELSE}" << endl;

  vars.str("");
  code.str("");

  indent_up_impl();
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    initialize_field( vars, code, "TConstants.F" + prop_name( (*c_iter)->get_name()),
      (*c_iter)->get_type(), (*c_iter)->get_value());
  }
  indent_down_impl();

  indent_impl(s_const_impl) << "procedure TConstants_Initialize;" << endl;
  if ( ! vars.str().empty() ) {
    indent_impl(s_const_impl) << "var" << endl;
    s_const_impl << vars.str();
  }
  indent_impl(s_const_impl) << "begin" << endl;
  if ( ! code.str().empty() ) {
    s_const_impl << code.str();
  }
  indent_impl(s_const_impl) << "end;" << endl << endl;

  indent_impl(s_const_impl) << "procedure TConstants_Finalize;" << endl;
  indent_impl(s_const_impl) << "begin" << endl;
  indent_up_impl();
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    finalize_field(s_const_impl, normalize_name( (*c_iter)->get_name()),
      (*c_iter)->get_type(), (*c_iter)->get_value(), "TConstants" );
  }
  indent_down_impl();
  indent_impl(s_const_impl) << "end;" << endl;
  indent_impl(s_const_impl) << "{$IFEND}" << endl << endl;
}

void t_delphi_generator::print_const_def_value(std::ostream& vars, std::ostream& out, string name, t_type* type, t_const_value* value, string cls_nm)
{

  string cls_prefix;

  if (cls_nm == "") {
    cls_prefix = "";
  } else {
    cls_prefix = cls_nm + ".";
  }

  if (type->is_struct() || type->is_xception()) {
    const vector<t_field*>& fields = ((t_struct*)type)->get_members();
    vector<t_field*>::const_iterator f_iter;
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      t_type* field_type = NULL;
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        if ((*f_iter)->get_name() == v_iter->first->get_string()) {
          field_type = (*f_iter)->get_type();
        }
      }
      if (field_type == NULL) {
        throw "type error: " + type->get_name() + " has no field " + v_iter->first->get_string();
      }
      string val = render_const_value( vars, out, name, field_type, v_iter->second);
      indent_impl(out) << cls_prefix << normalize_name(name) << "." << prop_name( v_iter->first->get_string(), type->is_xception()) << " := " << val << ";" << endl;
    }
  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string key = render_const_value( vars, out, name, ktype, v_iter->first);
      string val = render_const_value( vars, out, name, vtype, v_iter->second);
      indent_impl(out) << cls_prefix << normalize_name(name) << "[" << key << "]" << " := " << val << ";" << endl;
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
      string val = render_const_value( vars, out, name, etype, *v_iter);
      indent_impl(out) << cls_prefix << normalize_name(name) << ".Add(" << val << ");" << endl;
    }
  }
}

void t_delphi_generator::print_private_field(std::ostream& out, string name, t_type* type, t_const_value* value) {
  (void) value;
  indent(out) << "class var F" << name << ": " << type_name(type) << ";" << endl;
}

void t_delphi_generator::print_const_prop(std::ostream& out, string name, t_type* type, t_const_value* value) {
  (void) value;
  indent(out) << "class property " << name << ": " << type_name(type) << " read F" << name << ";" << endl;
}

void t_delphi_generator::print_const_value( std::ostream& vars, std::ostream& out, string name, t_type* type, t_const_value* value) {
  t_type* truetype = type;
  while (truetype->is_typedef()) {
    truetype = ((t_typedef*)truetype)->get_type();
  }

  if (truetype->is_base_type()) {
    string v2 = render_const_value( vars, out, name, type, value);
    indent_impl(out) << name << " := " << v2 << ";" << endl;
  } else if (truetype->is_enum()) {
    indent_impl(out) << name << " := " << type_name(type) << "(" << value->get_integer() << ");" << endl;
  } else {
    string typname;
    typname = type_name( type, true, false, type->is_xception(), type->is_xception());
    indent_impl(out) << name << " := " << typname << ".Create;" << endl;
    print_const_def_value( vars, out, name, type, value);
  }
}

void t_delphi_generator::initialize_field(std::ostream& vars, std::ostream& out, string name, t_type* type, t_const_value* value) {
  print_const_value( vars, out, name, type, value );
}

void t_delphi_generator::finalize_field(std::ostream& out, string name, t_type* type, t_const_value* value , string cls_nm) {
  (void) out;
  (void) name;
  (void) type;
  (void) value;
  (void) cls_nm;
}

string t_delphi_generator::render_const_value(ostream& vars, ostream& out, string name, t_type* type, t_const_value* value) {
  (void) name;

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
      case t_base_type::TYPE_BOOL:
        render << ((value->get_integer() > 0) ? "True" : "False");
        break;
      case t_base_type::TYPE_BYTE:
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
          render << value->get_integer();
        } else {
          render << value->get_double();
        }
        break;
      default:
          render << "";
    }
  } else if (truetype->is_enum()) {
    render << type_name( type, false) << "(" << value->get_integer() << ")";
  } else {
    string t = tmp("tmp");
    vars <<  "  " << t << " : " << type_name(type) << ";" << endl;
    print_const_value( vars, out, t, type, value);
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
  
  add_defined_type( tstruct);

  generate_delphi_struct_impl(s_struct_impl, "", tstruct, is_exception);
}

void t_delphi_generator::generate_delphi_struct_impl( ostream& out, string cls_prefix, t_struct* tstruct, bool is_exception, bool is_result, bool is_x_factory) {

  if (is_exception &&  (! is_x_factory)) {
    generate_delphi_struct_impl( out,  cls_prefix, tstruct, is_exception, is_result, true);
  }

  string cls_nm;

  string exception_factory_name;

  if (is_exception) {
    exception_factory_name = normalize_clsnm( tstruct->get_name(), "", true ) + "Factory";
  }

  if (is_exception) {
    cls_nm = type_name(tstruct,true,(! is_x_factory),is_x_factory,true);
  }
  else {
    cls_nm = type_name(tstruct,true,false);
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
    if ((*m_iter)->get_value() != NULL) {
      initialize_field( vars, code, "F" + prop_name( (*m_iter)->get_name(), is_exception), t, (*m_iter)->get_value());
    }
  }
  indent_down_impl();

  indent_impl(out) << "constructor " << cls_prefix << cls_nm <<  "." << "Create;" << endl;

  if ( ! vars.str().empty()) {
    out << "var" << endl;
    out << vars.str();
  }

  indent_impl(out) << "begin" << endl;
  indent_up_impl();
  if (is_exception && (! is_x_factory)) {
    indent_impl(out) << "inherited Create('');" << endl;
    indent_impl(out) << "F" << exception_factory_name << " := T" << exception_factory_name << "Impl.Create;" << endl;
  } else {
    indent_impl(out) << "inherited;" << endl;
  }

  if ( ! code.str().empty()) {
    out << code.str();
  }

  indent_down_impl();
  indent_impl(out) << "end;" << endl << endl;

  if ((members.size() > 0) && is_exception && (!is_x_factory)) {
    indent_impl(out) << "constructor " << cls_prefix << cls_nm <<  "." << "Create(" << constructor_argument_list( tstruct, indent_impl()) << ");" << endl;
    indent_impl(out) << "begin" << endl;
    indent_up_impl();
    indent_impl(out) << "Create;" << endl;
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      string propname = prop_name((*m_iter)->get_name(), is_exception);
      string param_name = constructor_param_name( (*m_iter)->get_name());
      indent_impl(out) << propname << " := " << param_name << ";" << endl;
    }
    indent_impl(out) << "UpdateMessageProperty;" << endl;
    indent_down_impl();
    indent_impl(out) << "end;" << endl << endl;
  }

  indent_impl(out) << "destructor "  << cls_prefix << cls_nm <<  "." << "Destroy;" << endl;
  indent_impl(out) << "begin" << endl;
  indent_up_impl();

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* t = (*m_iter)->get_type();
    while (t->is_typedef()) {
      t = ((t_typedef*)t)->get_type();
    }
    finalize_field( out, prop_name(*m_iter, is_exception), t, (*m_iter)->get_value());
  }

  indent_impl(out) << "inherited;" << endl;
  indent_down_impl();
  indent_impl(out) << "end;" << endl << endl;

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* t = (*m_iter)->get_type();
    while (t->is_typedef()) {
      t = ((t_typedef*)t)->get_type();
    }
    generate_delphi_property_reader_impl( out, cls_prefix, cls_nm, t, *m_iter, "F", is_exception);
    generate_delphi_property_writer_impl( out, cls_prefix, cls_nm, t, *m_iter, "F", is_exception , is_x_factory, exception_factory_name);
    if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
      generate_delphi_isset_reader_impl( out, cls_prefix, cls_nm, t, *m_iter, "F", is_exception);
    }
  }

  if ((! is_exception) || is_x_factory) {
    generate_delphi_struct_reader_impl( out, cls_prefix, tstruct, is_exception);
    if ( is_result ) {
      generate_delphi_struct_result_writer_impl( out, cls_prefix, tstruct, is_exception);
    } else {
      generate_delphi_struct_writer_impl( out, cls_prefix, tstruct, is_exception);
    }
  }
  generate_delphi_struct_tostring_impl( out, cls_prefix, tstruct, is_exception, is_x_factory);

  if (is_exception && is_x_factory) {
    generate_delphi_create_exception_impl( out, cls_prefix, tstruct, is_exception);
  }
}

void t_delphi_generator::generate_delphi_struct_definition(ostream &out, t_struct* tstruct, bool is_exception, bool in_class, bool is_result, bool is_x_factory) {
  bool is_final = (tstruct->annotations_.find("final") != tstruct->annotations_.end());
  string struct_intf_name;
  string struct_name;
  string isset_name; 
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  string exception_factory_name = normalize_clsnm( tstruct->get_name(), "", true ) + "Factory";

  if (is_exception) {
    struct_intf_name = type_name(tstruct,false,false,true);
  }
  else {
    struct_intf_name = type_name(tstruct);
  }


  if (is_exception) {
    struct_name = type_name(tstruct, true, (! is_x_factory), is_x_factory);
  }
  else {
    struct_name = type_name(tstruct,true);
  }

  if ((! is_exception) || is_x_factory) {

    indent(out) << struct_intf_name << " = interface(IBase)" << endl;
    indent_up();

    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      generate_delphi_property_reader_definition( out, *m_iter, is_exception);
      generate_delphi_property_writer_definition( out, *m_iter, is_exception);
    }

    if (is_x_factory) {
      out << endl;
      indent(out) << "// Create Exception Object" << endl;
      indent(out) << "function CreateException: " << type_name(tstruct,true,true) << ";" << endl;
    }

    if (members.size() > 0) {
      out << endl;
      for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        generate_property(out, *m_iter, true, is_exception);
      }
    }

    if (members.size() > 0) {
      out << endl;
      for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
          generate_delphi_isset_reader_definition( out, *m_iter, is_exception);
        }
      }
    }

    if (members.size() > 0) {
      out << endl;
      for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
          isset_name = "__isset_" + prop_name(*m_iter, is_exception);
          indent(out) << "property " << isset_name << ": Boolean read Get" << isset_name << ";" << endl;
        }
      }
    }

    indent_down();
    indent(out) << "end;" << endl << endl;
  }

  indent(out) << struct_name << " = ";
  if (is_final) {
    out << "sealed ";
  }
  out << "class(";
  if ( is_exception && (! is_x_factory)) {
    out << "TException";
  } else {
    out << "TInterfacedObject, IBase, " << struct_intf_name;
  }
  out << ")" << endl;

  if (is_exception && (! is_x_factory)) {
    indent(out) << "public" << endl;
    indent_up();
    indent(out) << "type" << endl;
    indent_up();
    generate_delphi_struct_definition( out, tstruct, is_exception, in_class, is_result, true);
    indent_down();
    indent_down();
  }

  indent(out) << "private" << endl;
  indent_up();

  if (is_exception && (! is_x_factory)) {
    indent(out) << "F" << exception_factory_name << " :" << struct_intf_name << ";" << endl << endl;
  }

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    indent(out) << declare_field(*m_iter, false, "F", is_exception) << endl;
  }

  if (members.size() > 0) {
    indent(out) << endl;
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
        isset_name = "F__isset_" + prop_name(*m_iter, is_exception);
        indent(out) << isset_name << ": Boolean;" << endl;
      }
    }
  }

  indent(out) << endl;

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    generate_delphi_property_reader_definition( out, *m_iter, is_exception);
    generate_delphi_property_writer_definition( out, *m_iter, is_exception);
  }

  if (members.size() > 0) {
    out << endl;
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
        isset_name = "__isset_" + prop_name(*m_iter, is_exception);
        indent(out) << "function Get" << isset_name << ": Boolean;" << endl;
      }
    }
  }

  indent_down();

  indent(out) << "public" << endl;
  indent_up();

  if ((members.size() > 0) && is_exception && (! is_x_factory)) {
    indent(out) << "constructor Create; overload;" << endl;
    indent(out) << "constructor Create(" << constructor_argument_list( tstruct, indent()) << "); overload;" << endl;
  } else {
    indent(out) << "constructor Create;" << endl;
  }

  indent(out) << "destructor Destroy; override;" << endl;

  out  << endl;
  indent(out) << "function ToString: string; override;" << endl;

  if (is_exception && (! is_x_factory)) {
    out  << endl;
    indent(out) << "// Exception Factory" << endl;
    indent(out) << "property " << exception_factory_name << ": " << struct_intf_name << " read F" << exception_factory_name << " write F" << exception_factory_name << ";" << endl;
  }

  if ((! is_exception) || is_x_factory) {
    out  << endl;
    indent(out) << "// IBase" << endl;
    indent(out) << "procedure Read( const iprot: IProtocol);" << endl;
    indent(out) << "procedure Write( const oprot: IProtocol);" << endl;
  }

  if (is_exception && is_x_factory) {
    out  << endl;
    indent(out) << "// Create Exception Object" << endl;
    indent(out) << "function CreateException: " << type_name(tstruct,true,true) << ";" << endl;
  }

  if (members.size() > 0) {
    out << endl;
    indent(out) << "// Properties" << endl;
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      generate_property(out, *m_iter, true, is_exception);
    }
  }

  if (members.size() > 0) {
    out << endl;
    indent(out) << "// isset" << endl;
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
        isset_name = "__isset_" + prop_name(*m_iter, is_exception);
        indent(out) << "property " << isset_name << ": Boolean read Get" << isset_name << ";" << endl;
      }
    }
  }

  indent_down();
  indent(out) << "end;" << endl << endl;
}

void t_delphi_generator::generate_service(t_service* tservice) {
  indent_up();
  indent(s_service) << normalize_clsnm(service_name_, "T") << " = class" << endl;
  indent(s_service) << "public" << endl;
  indent_up();
  indent(s_service) << "type" << endl;
  generate_service_interface(tservice);
  generate_service_client(tservice);
  generate_service_server(tservice);
  generate_service_helpers(tservice);
  indent_down();
  indent_down();
  indent(s_service) << "end;" << endl;
  indent(s_service) << endl;
  indent_down();
}

void t_delphi_generator::generate_service_interface(t_service* tservice) {
  string extends = "";
  string extends_iface = "";

  indent_up();

  if (tservice->get_extends() != NULL) {
    extends = type_name(tservice->get_extends(), true, true);
    extends_iface = extends + ".Iface";
    indent(s_service) <<
      "Iface = interface(" << extends_iface << ")" << endl;
  } else {
    indent(s_service) <<
      "Iface = interface" << endl;
  }

  indent_up();
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter)
  {
    indent(s_service) <<
      function_signature(*f_iter) << endl;
  }
  indent_down();
  indent(s_service) << "end;" << endl << endl;

  indent_down();
}

void t_delphi_generator::generate_service_helpers(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* ts = (*f_iter)->get_arglist();
    generate_delphi_struct_definition(s_service, ts, false, true);
    generate_delphi_struct_impl(s_service_impl, normalize_clsnm( service_name_, "T") + ".", ts, false);
    generate_function_helpers(*f_iter);
  }
}

void t_delphi_generator::generate_service_client(t_service* tservice) {
  indent_up();
  string extends = "";
  string extends_client = "";
  if (tservice->get_extends() != NULL) {
    extends = type_name(tservice->get_extends());
    extends_client = extends + ".Client, ";
  }

  if (tservice->get_extends() != NULL) {
    extends = type_name(tservice->get_extends(), true, true);
    extends_client = extends + ".TClient";
    indent(s_service) <<
      "TClient = class(" << extends_client << ", Iface)" << endl;
  } else {
    indent(s_service) <<
      "TClient = class( TInterfacedObject, Iface)" << endl;
  }

  indent(s_service) << "public" << endl;
  indent_up();

  indent(s_service) << "constructor Create( prot: IProtocol); overload;" << endl;

  indent_impl(s_service_impl) <<  "constructor " << normalize_clsnm( service_name_, "T") << ".TClient.Create( prot: IProtocol);" << endl;
  indent_impl(s_service_impl) <<  "begin" << endl;
  indent_up_impl();
  indent_impl(s_service_impl) << "Create( prot, prot );" << endl;    
  indent_down_impl();
  indent_impl(s_service_impl) <<  "end;" << endl << endl;

  indent(s_service) << "constructor Create( const iprot: IProtocol; const oprot: IProtocol); overload;" << endl;

  indent_impl(s_service_impl) <<  
    "constructor " << normalize_clsnm( service_name_, "T") << 
    ".TClient.Create( const iprot: IProtocol; const oprot: IProtocol);" << endl;
  indent_impl(s_service_impl) <<  "begin" << endl;
  indent_up_impl();
  indent_impl(s_service_impl) << "iprot_ := iprot;" << endl;    
  indent_impl(s_service_impl) << "oprot_ := oprot;" << endl;    
  indent_down_impl();
  indent_impl(s_service_impl) <<  "end;" << endl << endl;

  indent_down();

  if (extends.empty()) {
    indent(s_service) << "protected" << endl;
    indent_up();
    indent(s_service) << "iprot_: IProtocol;" << endl;
    indent(s_service) << "oprot_: IProtocol;" << endl;
    indent(s_service) << "seqid_: Integer;" << endl;
    indent_down();

    indent(s_service) << "public" << endl;
    indent_up();
    indent(s_service) << "property InputProtocol: IProtocol read iprot_;" << endl;
    indent(s_service) << "property OutputProtocol: IProtocol read oprot_;" << endl;
    indent_down();
  }

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;

  indent(s_service) << "protected" << endl;
  indent_up();
  indent(s_service) << "// Iface" << endl;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string funname = (*f_iter)->get_name();
    indent(s_service) << function_signature(*f_iter) << endl;
  }
  indent_down();

  indent(s_service) << "public" << endl;
  indent_up();

  string full_cls = normalize_clsnm(service_name_,"T") + ".TClient";

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string funname = (*f_iter)->get_name();

    indent_impl(s_service_impl) << function_signature(*f_iter, full_cls) << endl;
    indent_impl(s_service_impl) << "begin" << endl;
    indent_up_impl();
    indent_impl(s_service_impl) << "send_" << funname << "(";

    t_struct* arg_struct = (*f_iter)->get_arglist();

    const vector<t_field*>& fields = arg_struct->get_members();
    vector<t_field*>::const_iterator fld_iter;
    bool first = true;
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      if (first) {
        first = false;
      } else {
        s_service_impl << ", ";
      }
      s_service_impl << normalize_name( (*fld_iter)->get_name());
    }
    s_service_impl << ");" << endl;

    if (!(*f_iter)->is_oneway()) {
      s_service_impl << indent_impl();
      if (!(*f_iter)->get_returntype()->is_void()) {
        s_service_impl << "Result := ";
      }
      s_service_impl <<
        "recv_" << funname << "();" << endl;
    }

    indent_down_impl();
    indent_impl(s_service_impl) << "end;" << endl << endl;

    t_function send_function(g_type_void,
        string("send_") + (*f_iter)->get_name(),
        (*f_iter)->get_arglist());

    string argsname =  (*f_iter)->get_name() + "_args";
    string args_clsnm = normalize_clsnm( argsname, "T");
    string args_intfnm= normalize_clsnm( argsname, "I");

    indent(s_service) << function_signature(&send_function) << endl;
    indent_impl(s_service_impl) << function_signature(&send_function, full_cls) << endl;
    indent_impl(s_service_impl) << "var" << endl;
    indent_up_impl();
    indent_impl(s_service_impl) << "args : " << args_intfnm << ";" << endl;
    indent_impl(s_service_impl) << "msg : IMessage;" << endl;
    indent_down_impl();
    indent_impl(s_service_impl) << "begin" << endl;
    indent_up_impl();

    indent_impl(s_service_impl) <<
      "seqid_ := seqid_ + 1;" << endl;
    indent_impl(s_service_impl) <<
      "msg := TMessageImpl.Create('" << funname << "', TMessageType.Call, seqid_);" << endl;

    indent_impl(s_service_impl) <<
      "oprot_.WriteMessageBegin( msg );" << endl;
    indent_impl(s_service_impl) <<
      "args := " << args_clsnm << "Impl.Create();" << endl;

    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      indent_impl(s_service_impl) <<
        "args." << prop_name(*fld_iter) << " := " << normalize_name( (*fld_iter)->get_name()) << ";" << endl;
    }
    indent_impl(s_service_impl) << "args.Write(oprot_);" << endl;
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      indent_impl(s_service_impl) <<
        "args." << prop_name(*fld_iter) << " := " << empty_value((*fld_iter)->get_type()) << ";" << endl;
    }

    indent_impl(s_service_impl) << "oprot_.WriteMessageEnd();" << endl;
    indent_impl(s_service_impl) << "oprot_.Transport.Flush();" << endl;

    indent_down_impl();
    indent_impl(s_service_impl) << "end;" << endl << endl;

    if (!(*f_iter)->is_oneway()) {
      string org_resultname = (*f_iter)->get_name() + "_result" ;
      string result_clsnm = normalize_clsnm( org_resultname, "T");
      string result_intfnm = normalize_clsnm( org_resultname, "I");

      t_struct noargs(program_);
      t_function recv_function((*f_iter)->get_returntype(),
          string("recv_") + (*f_iter)->get_name(),
          &noargs,
          (*f_iter)->get_xceptions());

      t_struct *xs = (*f_iter)->get_xceptions();
      const std::vector<t_field*>& xceptions = xs->get_members();

      indent(s_service) << function_signature(&recv_function) << endl;
      indent_impl(s_service_impl) << function_signature(&recv_function, full_cls) << endl;
      indent_impl(s_service_impl) << "var" << endl;
      indent_up_impl();
      indent_impl(s_service_impl) << "msg : IMessage;" << endl;
      if ( xceptions.size() > 0) {
        indent_impl(s_service_impl) << "ex : Exception;" << endl;
      }
      indent_impl(s_service_impl) << "x : TApplicationException;" << endl;
      indent_impl(s_service_impl) << "ret : " << result_intfnm << ";" << endl;

      indent_down_impl();
      indent_impl(s_service_impl) << "begin" << endl;
      indent_up_impl();
      indent_impl(s_service_impl) << "msg := iprot_.ReadMessageBegin();" << endl;
      indent_impl(s_service_impl) << "if (msg.Type_ = TMessageType.Exception) then" << endl;
      indent_impl(s_service_impl) << "begin" << endl;
      indent_up_impl();
      indent_impl(s_service_impl) << "x := TApplicationException.Read(iprot_);" << endl;
      indent_impl(s_service_impl) << "iprot_.ReadMessageEnd();" << endl;
      indent_impl(s_service_impl) << "raise x;" << endl;
      indent_down_impl();
      indent_impl(s_service_impl) << "end;" << endl;

      indent_impl(s_service_impl) << "ret := " << result_clsnm << "Impl.Create();" << endl;
      indent_impl(s_service_impl) << "ret.Read(iprot_);" << endl;
      indent_impl(s_service_impl) << "iprot_.ReadMessageEnd();" << endl;

      if (!(*f_iter)->get_returntype()->is_void()) {
        indent_impl(s_service_impl) << "if (ret.__isset_success) then" << endl;
        indent_impl(s_service_impl) << "begin" << endl;
        indent_up_impl();
        indent_impl(s_service_impl) << "Result := ret.Success;" << endl;
        t_type *type = (*f_iter)->get_returntype();
        if (type->is_struct() || type->is_xception() || type->is_map() || type->is_list() || type->is_set()) {
          indent_impl(s_service_impl) << "ret.Success := nil;" << endl;
        }
        indent_impl(s_service_impl) << "Exit;" << endl;
        indent_down_impl();
        indent_impl(s_service_impl) << "end;" << endl;
      }

      vector<t_field*>::const_iterator x_iter;
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        indent_impl(s_service_impl) << "if (ret.__isset_" << prop_name(*x_iter) << ") then" << endl;
        indent_impl(s_service_impl) << "begin" << endl;
        indent_up_impl();
        indent_impl(s_service_impl) << "ex := ret." << prop_name(*x_iter) << ".CreateException;" << endl;
        indent_impl(s_service_impl) << "raise ex;" << endl;
        indent_down_impl();
        indent_impl(s_service_impl) << "end;" << endl;
      }

      if (!(*f_iter)->get_returntype()->is_void()) {
        indent_impl(s_service_impl) << 
          "raise TApplicationException.Create(TApplicationException.TExceptionType.MissingResult, '" << (*f_iter)->get_name() << " failed: unknown result');" << endl;
      }

      indent_down_impl();
      indent_impl(s_service_impl) << "end;" << endl << endl;
    }
  }

  indent_down();
  indent(s_service) << "end;" << endl << endl;
}

void t_delphi_generator::generate_service_server(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  string extends = "";
  string extends_processor = "";

  string full_cls = normalize_clsnm( service_name_, "T") + ".TProcessorImpl";

  if (tservice->get_extends() != NULL) {
    extends = type_name(tservice->get_extends(), true, true);
    extends_processor = extends + ".TProcessorImpl";
    indent(s_service) <<
      "TProcessorImpl = class(" << extends_processor << ", IProcessor)" << endl;
  } else {
    indent(s_service) <<
      "TProcessorImpl = class( TInterfacedObject, IProcessor)" << endl;
  }

  indent(s_service) << "public" << endl;
  indent_up();
  indent(s_service) << "constructor Create( iface_: Iface );" << endl;
  indent(s_service) << "destructor Destroy; override;" << endl;
  indent_down();

  indent_impl(s_service_impl) << "constructor " << full_cls << ".Create( iface_: Iface );" << endl;
  indent_impl(s_service_impl) << "begin" << endl;
  indent_up_impl();
  if (tservice->get_extends() != NULL)
  {
    indent_impl(s_service_impl) << "inherited Create( iface_);" << endl;
  } else {
    indent_impl(s_service_impl) << "inherited Create;" << endl;
  }
  indent_impl(s_service_impl) << "Self.iface_ := iface_;" << endl;
  if (tservice->get_extends() != NULL)
  {
    indent_impl(s_service_impl) << "ASSERT( processMap_ <> nil);  // inherited" << endl;
  } else {
    indent_impl(s_service_impl) << "processMap_ := TThriftDictionaryImpl<string, TProcessFunction>.Create;" << endl;
  }

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    indent_impl(s_service_impl) << 
      "processMap_.AddOrSetValue( '" << (*f_iter)->get_name() << "', " << (*f_iter)->get_name() << "_Process);" << endl;
  }
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << endl << endl;

  indent_impl(s_service_impl) << "destructor " << full_cls << ".Destroy;" << endl;
  indent_impl(s_service_impl) << "begin" << endl;
  indent_up_impl();
  indent_impl(s_service_impl) << "inherited;" << endl;
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << endl << endl;

  indent(s_service) << "private" << endl;
  indent_up();
  indent(s_service) << "iface_: Iface;" << endl;
  indent_down();

  if (tservice->get_extends() == NULL)
  {
    indent(s_service) << "protected" << endl;
    indent_up();
    indent(s_service) << "type" << endl;
    indent_up();
    indent(s_service) << "TProcessFunction = reference to procedure( seqid: Integer; const iprot: IProtocol; const oprot: IProtocol);" << endl;
    indent_down();
    indent_down();
    indent(s_service) << "protected" << endl;
    indent_up();
    indent(s_service) << "processMap_: IThriftDictionary<string, TProcessFunction>;" << endl;
    indent_down();
  }

  indent(s_service) << "public" << endl;
  indent_up();
  if (extends.empty()) {
    indent(s_service) << "function Process( const iprot: IProtocol; const oprot: IProtocol): Boolean;" << endl;
  } else {
    indent(s_service) << "function Process( const iprot: IProtocol; const oprot: IProtocol): Boolean; reintroduce;" << endl;
  }

  indent_impl(s_service_impl) << "function " << full_cls << ".Process( const iprot: IProtocol; const oprot: IProtocol): Boolean;" << endl;;
  indent_impl(s_service_impl) << "var" << endl;
  indent_up_impl();
  indent_impl(s_service_impl) << "msg : IMessage;" << endl;
  indent_impl(s_service_impl) << "fn : TProcessFunction;" << endl;
  indent_impl(s_service_impl) << "x : TApplicationException;" << endl;
  indent_down_impl();
  indent_impl(s_service_impl) << "begin" << endl;
  indent_up_impl();
  indent_impl(s_service_impl) << "try" << endl;
  indent_up_impl();
  indent_impl(s_service_impl) << "msg := iprot.ReadMessageBegin();" << endl;
  indent_impl(s_service_impl) << "fn := nil;" << endl;
  indent_impl(s_service_impl) << "if not processMap_.TryGetValue(msg.Name, fn)" << endl;
  indent_impl(s_service_impl) << "or not Assigned(fn) then" << endl;
  indent_impl(s_service_impl) << "begin" << endl;
  indent_up_impl();
  indent_impl(s_service_impl) << "TProtocolUtil.Skip(iprot, TType.Struct);" << endl;
  indent_impl(s_service_impl) << "iprot.ReadMessageEnd();" << endl;
  indent_impl(s_service_impl) << "x := TApplicationException.Create(TApplicationException.TExceptionType.UnknownMethod, 'Invalid method name: ''' + msg.Name + '''');" << endl;
  indent_impl(s_service_impl) << "msg := TMessageImpl.Create(msg.Name, TMessageType.Exception, msg.SeqID);" << endl;
  indent_impl(s_service_impl) << "oprot.WriteMessageBegin( msg);" << endl;
  indent_impl(s_service_impl) << "x.Write(oprot);" << endl;
  indent_impl(s_service_impl) << "oprot.WriteMessageEnd();" << endl;
  indent_impl(s_service_impl) << "oprot.Transport.Flush();" << endl;
  indent_impl(s_service_impl) << "Result := True;" << endl;
  indent_impl(s_service_impl) << "Exit;" << endl;
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << endl;
  indent_impl(s_service_impl) << "fn(msg.SeqID, iprot, oprot);" << endl;
  indent_down_impl();
  indent_impl(s_service_impl) << "except" << endl;
  indent_up_impl();
  indent_impl(s_service_impl) << "Result := False;" << endl;
  indent_impl(s_service_impl) << "Exit;" << endl;
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << endl;
  indent_impl(s_service_impl) << "Result := True;" << endl;
  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << endl << endl;

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter)
  {
    generate_process_function(tservice, *f_iter);
  }

  indent_down();
  indent(s_service) << "end;" << endl << endl;

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

  t_struct *xs = tfunction->get_xceptions();
  const vector<t_field*>& fields = xs->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    result.append(*f_iter);
  }

  generate_delphi_struct_definition(s_service, &result, false, true, true);
  generate_delphi_struct_impl(s_service_impl, normalize_clsnm( service_name_, "T") + ".", &result, false);
}

void t_delphi_generator::generate_process_function(t_service* tservice, t_function* tfunction) {
  (void) tservice;
  string funcname = tfunction->get_name();
  string full_cls = normalize_clsnm( service_name_, "T") + ".TProcessorImpl";

  string org_argsname  = funcname + "_args";
  string args_clsnm = normalize_clsnm(org_argsname, "T");
  string args_intfnm = normalize_clsnm(org_argsname, "I");

  string org_resultname = funcname + "_result";
  string result_clsnm = normalize_clsnm(org_resultname, "T");
  string result_intfnm = normalize_clsnm(org_resultname, "I");

  indent(s_service) <<
    "procedure " << funcname << "_Process( seqid: Integer; const iprot: IProtocol; const oprot: IProtocol);" << endl;

  if (tfunction->is_oneway()) {
    indent_impl(s_service_impl) << "// one way processor" << endl;
  } else {
    indent_impl(s_service_impl) << "// both way processor" << endl;
  }

  indent_impl(s_service_impl) <<
    "procedure " << full_cls << "." << funcname << "_Process( seqid: Integer; const iprot: IProtocol; const oprot: IProtocol);" << endl;
  indent_impl(s_service_impl) << "var" << endl;
  indent_up_impl();
  indent_impl(s_service_impl) << "args: " << args_intfnm << ";" << endl;
  if (!tfunction->is_oneway()) {
    indent_impl(s_service_impl) << "msg: IMessage;" << endl;
    indent_impl(s_service_impl) << "ret: " << result_intfnm << ";" << endl;
  }

  indent_down_impl();
  indent_impl(s_service_impl) << "begin" << endl;
  indent_up_impl();
  indent_impl(s_service_impl) << "args := " << args_clsnm << "Impl.Create;" << endl;
  indent_impl(s_service_impl) << "args.Read(iprot);" << endl;
  indent_impl(s_service_impl) << "iprot.ReadMessageEnd();" << endl;

  t_struct* xs = tfunction->get_xceptions();
  const std::vector<t_field*>& xceptions = xs->get_members();
  vector<t_field*>::const_iterator x_iter;

  if (!tfunction->is_oneway()) {
    indent_impl(s_service_impl) << "ret := " << result_clsnm << "Impl.Create;" << endl;
  }

  if (!tfunction->is_oneway() && xceptions.size() > 0) {
    indent_impl(s_service_impl) << "try" << endl;
    indent_up_impl();
  }

  t_struct* arg_struct = tfunction->get_arglist();
  const std::vector<t_field*>& fields = arg_struct->get_members();
  vector<t_field*>::const_iterator f_iter;

  s_service_impl << indent_impl();
  if (!tfunction->is_oneway() && !tfunction->get_returntype()->is_void()) {
    s_service_impl << "ret.Success := ";
  }
  s_service_impl <<  "iface_." << normalize_name( tfunction->get_name(), true) << "(";
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      s_service_impl << ", ";
    }
    s_service_impl << "args." << prop_name(*f_iter);
  }
  s_service_impl << ");" << endl;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    indent_impl(s_service_impl) <<
      "args." << prop_name(*f_iter) << " := " << empty_value((*f_iter)->get_type()) << ";" << endl;
  }

  if (!tfunction->is_oneway() && xceptions.size() > 0) {
    indent_down_impl();
    indent_impl(s_service_impl) << "except" << endl;
    indent_up_impl();
    for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
      indent_impl(s_service_impl) << "on E: " << type_name((*x_iter)->get_type(),true,true) << " do" << endl;
      indent_impl(s_service_impl) << "begin" << endl;
      indent_up_impl();
      if (!tfunction->is_oneway()) {
        string factory_name = normalize_clsnm((*x_iter)->get_type()->get_name(),"",true) + "Factory";
        indent_impl(s_service_impl) << 
          "ret." << prop_name(*x_iter) << " := E." << factory_name << ";" << endl;
      }
      indent_down_impl();
      indent_impl(s_service_impl) << "end;" << endl;
    }
    indent_down_impl();
    indent_impl(s_service_impl) << "end;" << endl;
  }

  if (! tfunction->is_oneway()) {
    indent_impl(s_service_impl) << "msg := TMessageImpl.Create('" << tfunction->get_name() << "', TMessageType.Reply, seqid); " << endl;
    indent_impl(s_service_impl) << "oprot.WriteMessageBegin( msg); " << endl;
    indent_impl(s_service_impl) << "ret.Write(oprot);" << endl;
    indent_impl(s_service_impl) << "oprot.WriteMessageEnd();" << endl;
    indent_impl(s_service_impl) << "oprot.Transport.Flush();" << endl;
  }

  indent_down_impl();
  indent_impl(s_service_impl) << "end;" << endl << endl;
}

void t_delphi_generator::generate_deserialize_field(ostream& out, bool is_xception, t_field* tfield, string prefix, ostream& local_vars) {
  t_type* type = tfield->get_type();
  while(type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }

  if (type->is_void()) {
    throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE: " + prefix + tfield->get_name();
  }

  string name = prefix + prop_name(tfield,is_xception);

  if (type->is_struct() || type->is_xception()) {
    generate_deserialize_struct(out, (t_struct*)type, name, "");
  } else if (type->is_container()) {
    generate_deserialize_container(out, is_xception, type, name, local_vars);
  } else if (type->is_base_type() || type->is_enum()) {
    indent_impl(out) <<
      name << " := ";

    if (type->is_enum())
    {
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
          if (((t_base_type*)type)->is_binary()) {
             if (ansistr_binary_) {
               out << "ReadAnsiString();";
             } else {
               out << "ReadBinary();";
             }
          } else {
            out << "ReadString();";
          }
          break;
        case t_base_type::TYPE_BOOL:
          out << "ReadBool();";
          break;
        case t_base_type::TYPE_BYTE:
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
          throw "compiler error: no C# name for base type " + tbase;
      }
    } else if (type->is_enum()) {
      out << "ReadI32()";
      out << ");";
    }
    out << endl;
  } else {
    printf("DO NOT KNOW HOW TO DESERIALIZE FIELD '%s' TYPE '%s'\n", tfield->get_name().c_str(), type_name(type).c_str());
  }
}

void t_delphi_generator::generate_deserialize_struct(ostream& out, t_struct* tstruct, string name, string prefix) {
  string typ_name;

  if (tstruct->is_xception()) {
    typ_name = type_name(tstruct,true,false,true,true);
  } else {
    typ_name = type_name(tstruct,true,false);
  }

  indent_impl(out) << prefix << name << " := " << typ_name << ".Create;" << endl;
  indent_impl(out) << prefix << name << ".Read(iprot);" << endl;
}

void t_delphi_generator::generate_deserialize_container(ostream& out, bool is_xception, t_type* ttype, string name, std::ostream& local_vars) {

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
    local_var = obj + ": IMap;";
  } else if (ttype->is_set()) {
    local_var = obj + ": ISet;";
  } else if (ttype->is_list()) {
    local_var = obj + ": IList;";
  }
  local_vars << "  " << local_var << endl;
  counter = tmp("_i");
  local_var = counter + ": Integer;";
  local_vars << "  " << local_var << endl;

  indent_impl(out) << name << " := " << type_name(ttype, true) << ".Create;" << endl;

  if (ttype->is_map()) {
    indent_impl(out) << obj << " := iprot.ReadMapBegin();" << endl;
  } else if (ttype->is_set()) {
    indent_impl(out) << obj << " := iprot.ReadSetBegin();" << endl;
  } else if (ttype->is_list()) {
    indent_impl(out) << obj << " := iprot.ReadListBegin();" << endl;
  }

  indent_impl(out) << 
    "for " << counter << " := 0 to " << obj << ".Count - 1 do" << endl;
  indent_impl(out) <<  "begin" << endl;
  indent_up_impl();
  if (ttype->is_map()) {
    generate_deserialize_map_element(out, is_xception, (t_map*)ttype, name, local_vars);
  } else if (ttype->is_set()) {
    generate_deserialize_set_element(out, is_xception, (t_set*)ttype, name, local_vars);
  } else if (ttype->is_list()) {
    generate_deserialize_list_element(out, is_xception, (t_list*)ttype, name, local_vars);
  }
  indent_down_impl();
  indent_impl(out) << "end;" << endl;

  if (ttype->is_map()) {
    indent_impl(out) << "iprot.ReadMapEnd();" << endl;
  } else if (ttype->is_set()) {
    indent_impl(out) << "iprot.ReadSetEnd();" << endl;
  } else if (ttype->is_list()) {
    indent_impl(out) << "iprot.ReadListEnd();" << endl;
  }
}

void t_delphi_generator::generate_deserialize_map_element(ostream& out, bool is_xception, t_map* tmap, string prefix, ostream& local_vars) {

  string key = tmp("_key");
  string val = tmp("_val");
  string local_var;

  t_field fkey(tmap->get_key_type(), key);
  t_field fval(tmap->get_val_type(), val);

  local_vars << "  " << declare_field(&fkey) << endl;
  local_vars << "  " << declare_field(&fval) << endl;

  generate_deserialize_field(out, is_xception, &fkey, "", local_vars);
  generate_deserialize_field(out, is_xception, &fval, "", local_vars);

  indent_impl(out) <<
    prefix << ".AddOrSetValue( " << key << ", " << val << ");" << endl;

}

void t_delphi_generator::generate_deserialize_set_element(ostream& out, bool is_xception, t_set* tset, string prefix, ostream& local_vars) {
  string elem = tmp("_elem");
  t_field felem(tset->get_elem_type(), elem);
  local_vars << "  " << declare_field(&felem) << endl;
  generate_deserialize_field(out, is_xception, &felem, "", local_vars);
  indent_impl(out) <<
    prefix << ".Add(" << elem << ");" << endl;
}

void t_delphi_generator::generate_deserialize_list_element(ostream& out, bool is_xception, t_list* tlist, string prefix, ostream& local_vars) {
  string elem = tmp("_elem");
  t_field felem(tlist->get_elem_type(), elem);
  local_vars << "  " << declare_field(&felem) << endl;
  generate_deserialize_field(out, is_xception, &felem, "", local_vars);
  indent_impl(out) <<
    prefix << ".Add(" << elem << ");" << endl;
}

void t_delphi_generator::generate_serialize_field(ostream& out, bool is_xception, t_field* tfield, string prefix, ostream& local_vars) {
  (void) local_vars;

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
    
    indent_impl(out) <<
      "oprot.";

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();

      switch(tbase) {
        case t_base_type::TYPE_VOID:
          throw "compiler error: cannot serialize void field in a struct: " + name;
          break;
        case t_base_type::TYPE_STRING:
          if (((t_base_type*)type)->is_binary()) {
            if (ansistr_binary_) {
              out << "WriteAnsiString(";
            } else {
              out << "WriteBinary(";
            }
          } else {
            out << "WriteString(";
          }
          out << name << ");";
          break;
        case t_base_type::TYPE_BOOL:
          out << "WriteBool(" << name << ");";
          break;
        case t_base_type::TYPE_BYTE:
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
          throw "compiler error: no Delphi name for base type " + tbase;
      }
    } else if (type->is_enum()) {
      out << "WriteI32(Integer(" << name << "));";
    }
    out << endl;
  } else {
    printf("DO NOT KNOW HOW TO SERIALIZE '%s%s' TYPE '%s'\n",
        prefix.c_str(),
        tfield->get_name().c_str(),
        type_name(type).c_str());
  }
}

void t_delphi_generator::generate_serialize_struct(ostream& out, t_struct* tstruct, string prefix, ostream& local_vars) {
  (void) local_vars;
  (void) tstruct;
  out <<
    indent_impl() << prefix << ".Write(oprot);" << endl;
}

void t_delphi_generator::generate_serialize_container(ostream& out, bool is_xception, t_type* ttype, string prefix, ostream& local_vars) {
  string obj;
  if (ttype->is_map()) {
    obj = tmp("map");
    local_vars << "  " << obj << " : IMap;" << endl;
    indent_impl(out) << obj << " := TMapImpl.Create( " <<
      type_to_enum(((t_map*)ttype)->get_key_type()) << ", " <<
      type_to_enum(((t_map*)ttype)->get_val_type()) << ", " <<
      prefix << ".Count);" << endl;
    indent_impl(out) << "oprot.WriteMapBegin( " << obj << ");" << endl;
  } else if (ttype->is_set()) {
    obj = tmp("set_");
    local_vars << "  " << obj << " : ISet;" << endl;
    indent_impl(out) << obj << " := TSetImpl.Create("  << 
      type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " <<
      prefix << ".Count);" << endl;
    indent_impl(out) <<
      "oprot.WriteSetBegin( " << obj << ");" << endl;
  } else if (ttype->is_list()) {
    obj = tmp("list_");
    local_vars << "  " << obj << " : IList;" << endl;
    indent_impl(out) << obj << " := TListImpl.Create("  << 
      type_to_enum(((t_list*)ttype)->get_elem_type()) << ", " <<
      prefix << ".Count);" << endl;
    indent_impl(out) <<
      "oprot.WriteListBegin( " << obj << ");" << endl;
  }

  string iter = tmp("_iter");
  if (ttype->is_map()) {
    local_vars << "  " << iter << ": " << type_name(((t_map*)ttype)->get_key_type()) << ";" << endl;
    indent_impl(out) << "for " << iter << " in " << prefix << ".Keys do" << endl;
    indent_impl(out) << "begin" << endl;
    indent_up_impl();
  } else if (ttype->is_set()) {
    local_vars << "  " << iter << ": " << type_name(((t_set*)ttype)->get_elem_type()) << ";" << endl;
    indent_impl(out) << "for " << iter << " in " << prefix << " do" << endl;
    indent_impl(out) << "begin" << endl;
    indent_up_impl();
  } else if (ttype->is_list()) {
    local_vars << "  " << iter << ": " << type_name(((t_list*)ttype)->get_elem_type()) << ";" << endl;
    indent_impl(out) << "for " << iter << " in " << prefix << " do" << endl;
    indent_impl(out) << "begin" << endl;
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
  indent_impl(out) << "end;" << endl;

  if (ttype->is_map()) {
    indent_impl(out) << "oprot.WriteMapEnd();" << endl;
  } else if (ttype->is_set()) {
    indent_impl(out) << "oprot.WriteSetEnd();" << endl;
  } else if (ttype->is_list()) {
    indent_impl(out) << "oprot.WriteListEnd();" << endl;
  }
}

void t_delphi_generator::generate_serialize_map_element(ostream& out, bool is_xception, t_map* tmap, string iter, string map, ostream& local_vars) {
  t_field kfield(tmap->get_key_type(), iter);
  generate_serialize_field(out, is_xception, &kfield, "", local_vars);
  t_field vfield(tmap->get_val_type(), map + "[" + iter + "]");
  generate_serialize_field(out, is_xception, &vfield, "", local_vars);
}

void t_delphi_generator::generate_serialize_set_element(ostream& out, bool is_xception, t_set* tset, string iter, ostream& local_vars) {
  t_field efield(tset->get_elem_type(), iter);
  generate_serialize_field(out, is_xception, &efield, "", local_vars);
}

void t_delphi_generator::generate_serialize_list_element(ostream& out, bool is_xception, t_list* tlist, string iter, ostream& local_vars) {
  t_field efield(tlist->get_elem_type(), iter);
  generate_serialize_field(out, is_xception, &efield, "", local_vars);
}

void t_delphi_generator::generate_property(ostream& out, t_field* tfield, bool isPublic, bool is_xception) {
  generate_delphi_property(out, is_xception, tfield, isPublic, "Get");
}

void t_delphi_generator::generate_delphi_property(ostream& out, bool struct_is_xception, t_field* tfield, bool isPublic, std::string fieldPrefix) {
  (void) isPublic;

  t_type* ftype = tfield->get_type();
  bool is_xception = ftype->is_xception();
  indent(out) << "property " << prop_name(tfield, struct_is_xception) << ": " << type_name(ftype, false, true, is_xception, true) << " read " << fieldPrefix + prop_name(tfield, struct_is_xception)
    << " write Set" << prop_name(tfield, struct_is_xception) << ";" << endl;
}

std::string t_delphi_generator::prop_name(t_field* tfield, bool is_xception) {
  return  prop_name(tfield->get_name(), is_xception);
}

std::string t_delphi_generator::prop_name(string name, bool is_xception) {
  string ret = name;
  ret[0] = toupper(ret[0]);
  return normalize_name( ret, true, is_xception);
}

std::string t_delphi_generator::constructor_param_name(string name) {
  string ret = name;
  ret[0] = toupper(ret[0]);
  ret = "A" + ret;
  return normalize_name( ret, false, false);
}

string t_delphi_generator::normalize_clsnm(string clsnm, string prefix, bool b_no_check_keyword) {
  if (clsnm.size() > 0) {
    clsnm[0] = toupper(clsnm[0]);
  }
  if (b_no_check_keyword) {
    return prefix + clsnm;
  } else {
    return normalize_name( prefix + clsnm);
  }
}

string t_delphi_generator::type_name( t_type* ttype, bool b_cls, bool b_no_postfix, bool b_exception_factory, bool b_full_exception_factory) {
  
  if (ttype->is_typedef()) {
    return normalize_name( "T"+((t_typedef*)ttype)->get_symbolic());
  }
  
  string typ_nm;

  string s_factory;

  if (ttype->is_base_type()) {
    return base_type_name((t_base_type*)ttype);
  } else if (ttype->is_enum()) {
    b_cls = true;
    b_no_postfix = true;
  } else if (ttype->is_map()) {
    t_map *tmap = (t_map*) ttype;
    if (b_cls) {
      typ_nm = "TThriftDictionaryImpl";
    } else {
      typ_nm = "IThriftDictionary";
    }
    return typ_nm + "<" + type_name(tmap->get_key_type()) +
      ", " + type_name(tmap->get_val_type()) + ">";
  } else if (ttype->is_set()) {
    t_set* tset = (t_set*) ttype;
    if (b_cls) {
      typ_nm = "THashSetImpl";
    } else {
      typ_nm = "IHashSet";
    }
    return typ_nm + "<" + type_name(tset->get_elem_type()) + ">";
  } else if (ttype->is_list()) {
    t_list* tlist = (t_list*) ttype;
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

  string nm = normalize_clsnm( ttype->get_name(), type_prefix);

  if (b_exception_factory) {
    nm = nm + "Factory";
  }

  if (b_cls) {
    if (! b_no_postfix) {
      nm = nm + "Impl";
    }
  }

  if ( b_exception_factory && b_full_exception_factory) {
    return type_name( ttype, true, true, false, false ) + "." + nm;
  }

  return nm;
}

// returns "const " for some argument types
string t_delphi_generator::input_arg_prefix( t_type* ttype) {

  // base types
  if (ttype->is_base_type()) {
    switch (((t_base_type*)ttype)->get_base()) {
  
      // these should be const'ed for optimal performamce
      case t_base_type::TYPE_STRING:  // refcounted pointer
      case t_base_type::TYPE_I64:   // larger than 32 bit
      case t_base_type::TYPE_DOUBLE:   // larger than 32 bit
        return "const ";
  
      // all others don't need to be
      case t_base_type::TYPE_BYTE:
      case t_base_type::TYPE_I16:
      case t_base_type::TYPE_I32:
      case t_base_type::TYPE_BOOL:
      case t_base_type::TYPE_VOID:
        return "";
      
      // we better always report any unknown types
      default:
        throw "compiler error: no input_arg_prefix() for base type " + (((t_base_type*)ttype)->get_base());
    }
  
  // enums
  } else if (ttype->is_enum()) {
    return "";   // usually <= 32 bit
  
  // containers
  } else if (ttype->is_map()) {
    return "const ";  // refcounted pointer

  } else if (ttype->is_set()) {
    return "const ";  // refcounted pointer

  } else if (ttype->is_list()) {
    return "const ";  // refcounted pointer

  }
  
  // any other type, either TSomething or ISomething
  return "const ";  // possibly refcounted pointer
}

string t_delphi_generator::base_type_name(t_base_type* tbase) {
  switch (tbase->get_base()) {
    case t_base_type::TYPE_VOID:
      // no "void" in Delphi language
      return "";
    case t_base_type::TYPE_STRING:
      if (tbase->is_binary()) {
        if ( ansistr_binary_) {
          return "AnsiString";
        } else {
          return "TBytes";
        }
      } else {
        return "string";
      }
    case t_base_type::TYPE_BOOL:
      return "Boolean";
    case t_base_type::TYPE_BYTE:
      return "ShortInt";
    case t_base_type::TYPE_I16:
      return "SmallInt";
    case t_base_type::TYPE_I32:
      return "Integer";
    case t_base_type::TYPE_I64:
      return "Int64";
    case t_base_type::TYPE_DOUBLE:
      return "Double";
    default:
      throw "compiler error: no Delphi name for base type " + tbase->get_base();
  }
}

string t_delphi_generator::declare_field(t_field* tfield, bool init, std::string prefix, bool is_xception_class) {
  (void) init;

  t_type * ftype = tfield->get_type();
  bool is_xception = ftype->is_xception();

  string result = prefix + prop_name(tfield, is_xception_class) + ": " + type_name(ftype,false,true,is_xception,true) + ";";
  return result;
}

string t_delphi_generator::function_signature(t_function* tfunction, std::string full_cls, bool is_xception) {
  t_type* ttype = tfunction->get_returntype();
  string prefix;
  if (full_cls == "") {
    prefix = "";
  } else {
    prefix = full_cls + ".";
  }
  if (is_void(ttype)) {
    return "procedure " + prefix + normalize_name(tfunction->get_name(), true, is_xception)  + "(" + argument_list(tfunction->get_arglist()) + ");";
  } else {
    return "function " + prefix + normalize_name(tfunction->get_name(), true, is_xception)  + "(" + argument_list(tfunction->get_arglist()) + "): " + type_name(ttype, false, true, is_xception, true) + ";";
  }
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
  result += input_arg_prefix(tt);  // const?
  result += normalize_name((*f_iter)->get_name()) + ": " + type_name( tt, false, true, tt->is_xception(), true);
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
      if ( firstline ) {
        result << endl << newline_indent;
        firstline = false;
      }
      result << line << endl;
      line = newline_indent;
    } else if ( line.size() > 0) {
      line += " ";
    }

    tt = (*f_iter)->get_type();
    line += input_arg_prefix(tt);  // const?
    line += constructor_param_name((*f_iter)->get_name()) + ": " + type_name( tt, false, true, tt->is_xception(), true);
  }

  if ( line.size() > 0) {
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
      case t_base_type::TYPE_BOOL:
        return "TType.Bool_";
      case t_base_type::TYPE_BYTE:
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
        if (((t_base_type*)type)->is_binary()) {
          if (ansistr_binary_) {
            return "''";
          } else {
            return "nil";
          }
        } else {
          return "''";
        }
      case t_base_type::TYPE_BOOL:
        return "False";
      case t_base_type::TYPE_BYTE:
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

void t_delphi_generator::generate_delphi_property_writer_definition(ostream& out, t_field* tfield, bool is_xception_class) {
  t_type * ftype = tfield->get_type();
  bool is_xception = ftype->is_xception();

  indent(out) << "procedure Set" << prop_name(tfield, is_xception_class) << "( const Value: " << type_name(ftype,false,true,is_xception,true) << ");" << endl;
}

void t_delphi_generator::generate_delphi_property_reader_definition(ostream& out, t_field* tfield, bool is_xception_class) {
  t_type * ftype = tfield->get_type();
  bool is_xception = ftype->is_xception();

  indent(out) << "function Get" << prop_name(tfield, is_xception_class) << ": " << type_name(ftype,false,true,is_xception,true) << ";" << endl;
}

void t_delphi_generator::generate_delphi_isset_reader_definition(ostream& out, t_field* tfield, bool is_xception) {
  indent(out) << "function Get__isset_" << prop_name( tfield, is_xception) << ": Boolean;" << endl;
}

void t_delphi_generator::generate_delphi_property_writer_impl(ostream& out, std::string cls_prefix, std::string name, t_type* type, t_field* tfield, std::string fieldPrefix, bool is_xception_class, bool is_xception_factory, std::string xception_factroy_name) {
  (void) type;

  t_type * ftype = tfield->get_type();
  bool is_xception = ftype->is_xception();

  indent_impl(out) << "procedure " << cls_prefix << name << "." << "Set" << prop_name(tfield, is_xception_class) << "( const Value: " << type_name(ftype,false,true,is_xception,true) << ");" << endl;
  indent_impl(out) << "begin" << endl;
  indent_up_impl();
  if (tfield->get_req() != t_field::T_REQUIRED) {
    indent_impl(out) << "F__isset_" << prop_name(tfield, is_xception_class) << " := True;" << endl;
  }
  indent_impl(out) << fieldPrefix << prop_name(tfield, is_xception_class) << " := Value;" << endl;

  if (is_xception_class && (! is_xception_factory) ) {
    indent_impl(out) << "F" << xception_factroy_name << "." << prop_name(tfield, is_xception_class) << " := Value;" << endl;
  }

  indent_down_impl();
  indent_impl(out) << "end;" << endl << endl;
}

void t_delphi_generator::generate_delphi_property_reader_impl(ostream& out, std::string cls_prefix, std::string name, t_type* type, t_field* tfield, std::string fieldPrefix, bool is_xception_class) {
  (void) type;

  t_type * ftype = tfield->get_type();
  bool is_xception = ftype->is_xception();

  indent_impl(out) << "function " << cls_prefix << name << "." << "Get" << prop_name( tfield, is_xception_class) << ": " << type_name(ftype,false,true,is_xception,true) << ";" << endl;
  indent_impl(out) << "begin" << endl;
  indent_up_impl();
  indent_impl(out) << "Result := " << fieldPrefix << prop_name(tfield, is_xception_class) << ";" << endl;
  indent_down_impl();
  indent_impl(out) << "end;" << endl << endl;
}

void t_delphi_generator::generate_delphi_isset_reader_impl(ostream& out, std::string cls_prefix, std::string name, t_type* type, t_field* tfield, std::string fieldPrefix, bool is_xception) {
  (void) type;

  string isset_name = "__isset_" + prop_name( tfield, is_xception);
  indent_impl(out) << "function " << cls_prefix << name << "." << "Get" << isset_name << ": Boolean;" << endl;
  indent_impl(out) << "begin" << endl;
  indent_up_impl();
  indent_impl(out) << "Result := " << fieldPrefix << isset_name << ";" << endl;
  indent_down_impl();
  indent_impl(out) << "end;" << endl << endl;
}

void t_delphi_generator::generate_delphi_create_exception_impl(ostream& out, string cls_prefix, t_struct* tstruct, bool is_exception) {
  (void) cls_prefix;

  string exception_cls_nm = type_name(tstruct,true,true);
  string cls_nm = type_name(tstruct,true,false,is_exception,is_exception);

  indent_impl(out) << "function " << cls_nm << ".CreateException: " << exception_cls_nm << ";" << endl;

  indent_impl(out) << "begin" << endl;
  indent_up_impl();


  indent_impl(out) << "Result := " << exception_cls_nm << ".Create;" << endl;
  string factory_name = normalize_clsnm(tstruct->get_name(),"",true) + "Factory";
  indent_impl(out) << "Result." << factory_name << " := Self;" << endl;

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  string propname;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    propname = prop_name(*f_iter, is_exception);
    if ((*f_iter)->get_req() != t_field::T_REQUIRED) {
      indent_impl(out) << "if __isset_" << propname << " then" << endl;
      indent_impl(out) << "begin" << endl;
      indent_up_impl();
    }
    indent_impl(out) << "Result." << propname <<  " := " << propname << ";" << endl;
    if ((*f_iter)->get_req() != t_field::T_REQUIRED) {
      indent_down_impl();
      indent_impl(out) << "end;" << endl;
    }
  }

  indent_impl(out) << "Result.UpdateMessageProperty;" << endl;
 
  indent_down_impl();
  indent_impl(out) << "end;" << endl << endl;
}

void t_delphi_generator::generate_delphi_struct_reader_impl(ostream& out, string cls_prefix, t_struct* tstruct, bool is_exception) {

  ostringstream local_vars;
  ostringstream code_block;

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;


  indent_impl(code_block) << "begin" << endl;
  indent_up_impl();

  // local bools for required fields
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      indent_impl(local_vars) << 
        "_req_isset_" << prop_name(*f_iter, is_exception) << " : Boolean;" << endl;
      indent_impl(code_block) << 
        "_req_isset_" << prop_name(*f_iter, is_exception) << " := FALSE;" << endl;
    }
  }
  
  indent_impl(code_block) << "struc := iprot.ReadStructBegin;" << endl;

  indent_impl(code_block) << "try" << endl;
  indent_up_impl();

  indent_impl(code_block) << "while (true) do" << endl;
  indent_impl(code_block) << "begin" << endl;
  indent_up_impl();

  indent_impl(code_block) << "field_ := iprot.ReadFieldBegin();" << endl;

  indent_impl(code_block) << "if (field_.Type_ = TType.Stop) then" << endl;
  indent_impl(code_block) << "begin" << endl;
  indent_up_impl();
  indent_impl(code_block) << "Break;" << endl;
  indent_down_impl();
  indent_impl(code_block) << "end;" << endl;


  bool first = true;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {

    if (first) {
      indent_impl(code_block) << "case field_.ID of" << endl;
      indent_up_impl();
    }

    first = false;
    if (f_iter != fields.begin()) {
      code_block << ";" << endl;
    }
    indent_impl(code_block) << (*f_iter)->get_key() << ": begin" << endl;
    indent_up_impl();
    indent_impl(code_block) << "if (field_.Type_ = " << type_to_enum((*f_iter)->get_type()) << ") then" << endl;
    indent_impl(code_block) << "begin" << endl;
    indent_up_impl();

    generate_deserialize_field(code_block, is_exception,  *f_iter, "", local_vars);

    // required field?
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      indent_impl(code_block) << 
        "_req_isset_" << prop_name(*f_iter, is_exception) << " := TRUE;" << endl;
    }

    indent_down_impl();

    indent_impl(code_block) << "end else" << endl;
    indent_impl(code_block) << "begin" << endl;
    indent_up_impl();
    indent_impl(code_block) << "TProtocolUtil.Skip(iprot, field_.Type_);" << endl;
    indent_down_impl();
    indent_impl(code_block) << "end;" << endl;
    indent_down_impl();
    indent_impl(code_block) << "end";

  }

  if (! first) {
    code_block << endl;
    indent_impl(code_block) << "else begin" << endl;
    indent_up_impl();
  }

  indent_impl(code_block) << "TProtocolUtil.Skip(iprot, field_.Type_);" << endl;

  if (! first) {
    indent_down_impl();
    indent_impl(code_block) << "end;" << endl;
    indent_down_impl();
    indent_impl(code_block) << "end;" << endl;
  }

  indent_impl(code_block) << "iprot.ReadFieldEnd;" << endl;

  indent_down_impl();

  indent_impl(code_block) << "end;" << endl;
  indent_down_impl();

  indent_impl(code_block) << "finally" << endl;
  indent_up_impl();
  indent_impl(code_block) << "iprot.ReadStructEnd;" << endl;
  indent_down_impl();
  indent_impl(code_block) << "end;" << endl;

  // all required fields have been read?  
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      indent_impl(code_block) << 
        "if not _req_isset_" << prop_name(*f_iter, is_exception) << endl;
      indent_impl(code_block) << 
        "then raise TProtocolException.Create( TProtocolException.INVALID_DATA, '" << 
        prop_name(*f_iter, is_exception) << "');" << endl;
    }
  }
  
  indent_down_impl();
  indent_impl(code_block) << "end;" << endl << endl;

  string cls_nm;

  cls_nm = type_name(tstruct,true,false,is_exception,is_exception);

  indent_impl(out) << "procedure " << cls_prefix << cls_nm << ".Read( const iprot: IProtocol);" << endl;
  indent_impl(out) << "var" << endl;
  indent_up_impl();
  indent_impl(out) << "field_ : IField;" << endl;
  indent_impl(out) << "struc : IStruct;" << endl;
  indent_down_impl();
  out << local_vars.str() << endl;
  out << code_block.str();
}

void t_delphi_generator::generate_delphi_struct_result_writer_impl(ostream& out, string cls_prefix, t_struct* tstruct, bool is_exception) {

  ostringstream local_vars;
  ostringstream code_block;

  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator f_iter;


  indent_impl(code_block) << "begin" << endl;
  indent_up_impl();

  indent_impl(code_block) << "struc := TStructImpl.Create('" << name << "');" << endl;

  indent_impl(code_block) << "oprot.WriteStructBegin(struc);" << endl;

  if (fields.size() > 0) {
    indent_impl(code_block) << "field_ := TFieldImpl.Create;" << endl;
    bool first = true;
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
      if (! first) {
        indent_impl(code_block) << "end else" << endl;
      }

      indent_impl(code_block) << "if (__isset_" << prop_name(*f_iter,is_exception) << ") then" << endl;
      indent_impl(code_block) << "begin" << endl;
      indent_up_impl();
      indent_impl(code_block) <<
        "field_.Name := '" << (*f_iter)->get_name() << "';" << endl;
      indent_impl(code_block) <<
        "field_.Type_  := " << type_to_enum((*f_iter)->get_type()) << ";" << endl;
      indent_impl(code_block) <<
        "field_.ID := " << (*f_iter)->get_key() << ";" << endl;
      indent_impl(code_block) <<
        "oprot.WriteFieldBegin(field_);" << endl;
      generate_serialize_field(code_block, is_exception, *f_iter, "", local_vars);
      indent_impl(code_block) << "oprot.WriteFieldEnd();" << endl;
      indent_down_impl();
    }

    if (! first) {
        indent_impl(code_block) << "end;" << endl;
    }

  }


  indent_impl(code_block) << "oprot.WriteFieldStop();" << endl;
  indent_impl(code_block) << "oprot.WriteStructEnd();" << endl;

  indent_down_impl();
  indent_impl(code_block) << "end;" << endl << endl;

  string cls_nm;

  cls_nm = type_name(tstruct,true,false,is_exception,is_exception);

  indent_impl(out) << "procedure " << cls_prefix << cls_nm << ".Write( const oprot: IProtocol);" << endl;
  indent_impl(out) << "var" << endl;
  indent_up_impl();
  indent_impl(out) << "struc : IStruct;" << endl;

  if (fields.size() > 0) {
    indent_impl(out) << "field_ : IField;" << endl;
  }

  out << local_vars.str();
  indent_down_impl();
  out << code_block.str();

}

void t_delphi_generator::generate_delphi_struct_writer_impl(ostream& out, string cls_prefix, t_struct* tstruct, bool is_exception) {

  ostringstream local_vars;
  ostringstream code_block;

  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator f_iter;


  indent_impl(code_block) << "begin" << endl;
  indent_up_impl();

  indent_impl(code_block) << "struc := TStructImpl.Create('" << name << "');" << endl;

  indent_impl(code_block) << "oprot.WriteStructBegin(struc);" << endl;

  if (fields.size() > 0) {
    indent_impl(code_block) << "field_ := TFieldImpl.Create;" << endl;
  }

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    bool null_allowed = type_can_be_null((*f_iter)->get_type());
    bool is_optional  = ((*f_iter)->get_req() != t_field::T_REQUIRED);
    if (null_allowed) {
      indent_impl(code_block) <<
        "if (" << prop_name((*f_iter), is_exception) << " <> nil)";
      if (is_optional) {
        code_block << 
          " and __isset_" << prop_name(*f_iter,is_exception);
      }
      code_block << 
        " then" << endl;
      indent_impl(code_block) << "begin" << endl;
      indent_up_impl();
    } else {
      if (is_optional) {
        indent_impl(code_block) << "if (__isset_" << prop_name(*f_iter,is_exception) << ") then" << endl;
        indent_impl(code_block) << "begin" << endl;
        indent_up_impl();
      } else {
        indent_impl(code_block) << "// required field" << endl;
      }
    }
    indent_impl(code_block) <<
      "field_.Name := '" << (*f_iter)->get_name() << "';" << endl;
    indent_impl(code_block) <<
      "field_.Type_  := " << type_to_enum((*f_iter)->get_type()) << ";" << endl;
    indent_impl(code_block) <<
      "field_.ID := " << (*f_iter)->get_key() << ";" << endl;
    indent_impl(code_block) <<
      "oprot.WriteFieldBegin(field_);" << endl;
    generate_serialize_field(code_block, is_exception, *f_iter, "", local_vars);
    indent_impl(code_block) << "oprot.WriteFieldEnd();" << endl;
    if (null_allowed || is_optional) {
      indent_down_impl();
      indent_impl(code_block) << "end;" << endl;
    }
  }

  indent_impl(code_block) << "oprot.WriteFieldStop();" << endl;
  indent_impl(code_block) << "oprot.WriteStructEnd();" << endl;

  indent_down_impl();
  indent_impl(code_block) << "end;" << endl << endl;

  string cls_nm;

  cls_nm = type_name(tstruct,true,false,is_exception,is_exception);

  indent_impl(out) << "procedure " << cls_prefix << cls_nm << ".Write( const oprot: IProtocol);" << endl;
  indent_impl(out) << "var" << endl;
  indent_up_impl();
  indent_impl(out) << "struc : IStruct;" << endl;
  if (fields.size() > 0) {
    indent_impl(out) << "field_ : IField;" << endl;
  }
  out << local_vars.str();
  indent_down_impl();
  out << code_block.str();

}

void t_delphi_generator::generate_delphi_struct_tostring_impl(ostream& out, string cls_prefix, t_struct* tstruct, bool is_exception, bool is_x_factory) {

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  string cls_nm;

  if (is_exception) {
    cls_nm = type_name(tstruct,true,(! is_x_factory),is_x_factory,true);
  } else {
    cls_nm = type_name(tstruct,true,false);
  }

  string tmp_sb = "sb";

  indent_impl(out) << "function " << cls_prefix << cls_nm << ".ToString: string;" << endl;
  indent_impl(out) << "var" << endl;
  indent_up_impl();
  indent_impl(out) << tmp_sb << " : TThriftStringBuilder;" << endl;
  indent_down_impl();
  indent_impl(out) << "begin" << endl;
  indent_up_impl();

  indent_impl(out) << tmp_sb << " := TThriftStringBuilder.Create('(');" << endl;
  indent_impl(out) << "try" << endl;
  indent_up_impl();

  bool first = true;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
      indent_impl(out) <<
        tmp_sb << ".Append('" << prop_name((*f_iter), is_exception) << ": ');" << endl;
    } else {
      indent_impl(out) <<
        tmp_sb << ".Append('," << prop_name((*f_iter), is_exception) << ": ');" << endl;
    }
    t_type* ttype = (*f_iter)->get_type();
    if (ttype->is_xception() || ttype->is_struct()) {
      indent_impl(out) <<
        "if (" << prop_name((*f_iter), is_exception) << " = nil) then " << tmp_sb <<  ".Append('<null>') else " << tmp_sb <<  ".Append("<< prop_name((*f_iter), is_exception)  << ".ToString());" << endl;
    } else if (ttype->is_enum()) {
      indent_impl(out) <<
        tmp_sb << ".Append(Integer(" << prop_name((*f_iter), is_exception) << "));" << endl;
    } else {
      indent_impl(out) <<
        tmp_sb << ".Append(" << prop_name((*f_iter), is_exception)  << ");" << endl;
    }
  }

  indent_impl(out) <<
    tmp_sb << ".Append(')');" << endl;
  indent_impl(out) <<
    "Result := " << tmp_sb <<  ".ToString;" << endl;

  indent_down_impl();
  indent_impl(out) << "finally" << endl;
  indent_up_impl();
  indent_impl(out) << tmp_sb << ".Free;" << endl;
  indent_down_impl();
  indent_impl(out) << "end;" << endl;

  indent_down_impl();
  indent_impl(out) << "end;" << endl << endl;
}

bool t_delphi_generator::is_void( t_type* type ) {
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

THRIFT_REGISTER_GENERATOR(delphi, "delphi",
"    ansistr_binary:  Use AnsiString as binary properties.\n")

