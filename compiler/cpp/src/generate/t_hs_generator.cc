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

#include <string>
#include <fstream>
#include <iostream>
#include <vector>

#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sstream>
#include "t_oop_generator.h"
#include "platform.h"
using namespace std;


/**
 * Haskell code generator.
 *
 */
class t_hs_generator : public t_oop_generator {
 public:
  t_hs_generator(
      t_program* program,
      const std::map<std::string, std::string>& parsed_options,
      const std::string& option_string)
    : t_oop_generator(program)
  {
    out_dir_base_ = "gen-hs";
  }

  /**
   * Init and close methods
   */

  void init_generator();
  void close_generator();

  /**
   * Program-level generation functions
   */
  void generate_typedef  (t_typedef*  ttypedef);
  void generate_enum     (t_enum*     tenum);
  void generate_const    (t_const*    tconst);
  void generate_struct   (t_struct*   tstruct);
  void generate_xception (t_struct*   txception);
  void generate_service  (t_service*  tservice);

  std::string render_const_value(t_type* type, t_const_value* value);

  /**
   * Struct generation code
   */

  void generate_hs_struct(t_struct* tstruct, bool is_exception);
  void generate_hs_struct_definition(std::ofstream &out,t_struct* tstruct, bool is_xception=false,bool helper=false);
  void generate_hs_struct_reader(std::ofstream& out, t_struct* tstruct);
  void generate_hs_struct_writer(std::ofstream& out, t_struct* tstruct);
  void generate_hs_function_helpers(t_function* tfunction);

  /**
   * Service-level generation functions
   */

  void generate_service_helpers   (t_service*  tservice);
  void generate_service_interface (t_service* tservice);
  void generate_service_client    (t_service* tservice);
  void generate_service_server    (t_service* tservice);
  void generate_process_function  (t_service* tservice, t_function* tfunction);

  /**
   * Serialization constructs
   */

  void generate_deserialize_field        (std::ofstream &out,
                                          t_field*    tfield,
                                          std::string prefix);

  void generate_deserialize_struct       (std::ofstream &out,
                                          t_struct*   tstruct);

  void generate_deserialize_container    (std::ofstream &out,
                                          t_type*     ttype);

  void generate_deserialize_set_element  (std::ofstream &out,
                                          t_set*      tset);


  void generate_deserialize_list_element (std::ofstream &out,
                                          t_list*     tlist,
                                          std::string prefix="");
  void generate_deserialize_type          (std::ofstream &out,
                                           t_type* type);

  void generate_serialize_field          (std::ofstream &out,
                                          t_field*    tfield,
                                          std::string name= "");

  void generate_serialize_struct         (std::ofstream &out,
                                          t_struct*   tstruct,
                                          std::string prefix="");

  void generate_serialize_container      (std::ofstream &out,
                                          t_type*     ttype,
                                          std::string prefix="");

  void generate_serialize_map_element    (std::ofstream &out,
                                          t_map*      tmap,
                                          std::string kiter,
                                          std::string viter);

  void generate_serialize_set_element    (std::ofstream &out,
                                          t_set*      tmap,
                                          std::string iter);

  void generate_serialize_list_element   (std::ofstream &out,
                                          t_list*     tlist,
                                          std::string iter);

  /**
   * Helper rendering functions
   */

  std::string hs_autogen_comment();
  std::string hs_imports();
  std::string type_name(t_type* ttype);
  std::string function_type(t_function* tfunc, bool options = false, bool io = false, bool method = false);
  std::string type_to_enum(t_type* ttype);
  std::string render_hs_type(t_type* type, bool needs_parens = true);


 private:

  /**
   * File streams
   */

  std::ofstream f_types_;
  std::ofstream f_consts_;
  std::ofstream f_service_;
  std::ofstream f_iface_;
  std::ofstream f_client_;

};


/**
 * Prepares for file generation by opening up the necessary file output
 * streams.
 *
 * @param tprogram The program to generate
 */
void t_hs_generator::init_generator() {
  // Make output directory
  MKDIR(get_out_dir().c_str());

  // Make output file

  string pname = capitalize(program_name_);
  string f_types_name = get_out_dir()+pname+"_Types.hs";
  f_types_.open(f_types_name.c_str());

  string f_consts_name = get_out_dir()+pname+"_Consts.hs";
  f_consts_.open(f_consts_name.c_str());

  // Print header
  f_types_ <<
    hs_autogen_comment() << endl <<
    "module " << pname <<"_Types where" << endl <<
    hs_imports() << endl;

  f_consts_ <<
    hs_autogen_comment() << endl <<
    "module " << pname <<"_Consts where" << endl <<
    hs_imports() << endl <<
    "import " << pname<<"_Types"<< endl;

}


/**
 * Autogen'd comment
 */
string t_hs_generator::hs_autogen_comment() {
  return
    std::string("-----------------------------------------------------------------\n") +
    "-- Autogenerated by Thrift                                     --\n" +
    "--                                                             --\n" +
    "-- DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING --\n" +
    "-----------------------------------------------------------------\n";
}

/**
 * Prints standard thrift imports
 */
string t_hs_generator::hs_imports() {
  return "import Thrift\nimport Data.Typeable ( Typeable )\nimport Control.Exception\nimport qualified Data.Map as Map\nimport qualified Data.Set as Set\nimport Data.Int";
}

/**
 * Closes the type files
 */
void t_hs_generator::close_generator() {
  // Close types file
  f_types_.close();
  f_consts_.close();
}

/**
 * Generates a typedef. Ez.
 *
 * @param ttypedef The type definition
 */
void t_hs_generator::generate_typedef(t_typedef* ttypedef) {
  f_types_ <<
    indent() << "type "<< capitalize(ttypedef->get_symbolic()) << " = " << render_hs_type(ttypedef->get_type(), false) << endl << endl;
}

/**
 * Generates code for an enumerated type.
 * the values.
 *
 * @param tenum The enumeration
 */
void t_hs_generator::generate_enum(t_enum* tenum) {
  indent(f_types_) << "data "<<capitalize(tenum->get_name())<<" = ";
  indent_up();
  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator c_iter;
  bool first = true;
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    string name = capitalize((*c_iter)->get_name());
    if(first)
      first=false;
    else
      f_types_ << "|";
    f_types_ << name;
  }
  indent(f_types_) << "deriving (Show,Eq, Typeable, Ord)" << endl;
  indent_down();

  int value = -1;
  indent(f_types_) << "instance Enum " << capitalize(tenum->get_name()) << " where" << endl;
  indent_up();
  indent(f_types_) << "fromEnum t = case t of" << endl;
  indent_up();
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    if ((*c_iter)->has_value()) {
      value = (*c_iter)->get_value();
    } else {
      ++value;
    }
    string name = capitalize((*c_iter)->get_name());

    f_types_ <<
      indent() << name << " -> " << value << endl;
  }
  indent_down();

  indent(f_types_) << "toEnum t = case t of" << endl;
  indent_up();
  for(c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    if ((*c_iter)->has_value()) {
      value = (*c_iter)->get_value();
    } else {
      ++value;
    }
    string name = capitalize((*c_iter)->get_name());

    f_types_ <<
      indent() << value << " -> " << name << endl;
  }
  indent(f_types_) << "_ -> throw ThriftException" << endl;
  indent_down();
  indent_down();
}

/**
 * Generate a constant value
 */
void t_hs_generator::generate_const(t_const* tconst) {
  t_type* type = tconst->get_type();
  string name = decapitalize(tconst->get_name());
  t_const_value* value = tconst->get_value();

  indent(f_consts_) << name << " :: " << render_hs_type(type, false) << endl;
  indent(f_consts_) << name << " = " << render_const_value(type, value) << endl << endl;
}

/**
 * Prints the value of a constant with the given type. Note that type checking
 * is NOT performed in this function as it is always run beforehand using the
 * validate_types method in main.cc
 */
string t_hs_generator::render_const_value(t_type* type, t_const_value* value) {
  type = get_true_type(type);
  std::ostringstream out;
  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_STRING:
      out << '"' << get_escaped_string(value) << '"';
      break;
    case t_base_type::TYPE_BOOL:
      out << (value->get_integer() > 0 ? "True" : "False");
      break;
    case t_base_type::TYPE_BYTE:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
      out << value->get_integer();
      break;
    case t_base_type::TYPE_DOUBLE:
      if (value->get_type() == t_const_value::CV_INTEGER) {
        out << value->get_integer();
      } else {
        out << value->get_double();
      }
      break;
    default:
      throw "compiler error: no const of base type " + t_base_type::t_base_name(tbase);
    }
  } else if (type->is_enum()) {
    t_enum* tenum = (t_enum*)type;
    vector<t_enum_value*> constants = tenum->get_constants();
    vector<t_enum_value*>::iterator c_iter;
    int val = -1;
    for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
      if ((*c_iter)->has_value()) {
        val = (*c_iter)->get_value();
      } else {
        ++val;
      }
      if(val == value->get_integer()){
        indent(out) << capitalize((*c_iter)->get_name());
        break;
      }
    }
  } else if (type->is_struct() || type->is_xception()) {
    string cname = type_name(type);
    indent(out) << cname << "{";
    const vector<t_field*>& fields = ((t_struct*)type)->get_members();
    vector<t_field*>::const_iterator f_iter;
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    bool first = true;
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
      string fname = v_iter->first->get_string();
      if(first)
        first=false;
      else
        out << ",";
      out << "f_" << cname << "_" << fname << " = Just (" << render_const_value(field_type, v_iter->second) << ")";

    }
    indent(out) << "}";
  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    out << "(Map.fromList [";
    bool first=true;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string key = render_const_value(ktype, v_iter->first);
      string val = render_const_value(vtype, v_iter->second);
      if(first)
        first=false;
      else
        out << ",";
      out << "(" << key << ","<< val << ")";
    }
    out << "])";
  } else if (type->is_list() || type->is_set()) {
    t_type* etype;

    if (type->is_list()) {
        etype = ((t_list*) type)->get_elem_type();
    } else  {
        etype = ((t_set*) type)->get_elem_type();
    }

    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    bool first = true;

    if (type->is_set())
        out << "(Set.fromList ";

    out << "[";

    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      if(first)
        first=false;
      else
        out << ",";
      out << render_const_value(etype, *v_iter);
    }

    out << "]";
    if (type->is_set())
        out << ")";
  } else {
    throw "CANNOT GENERATE CONSTANT FOR TYPE: " + type->get_name();
  }
  return out.str();
}

/**
 * Generates a "struct"
 */
void t_hs_generator::generate_struct(t_struct* tstruct) {
  generate_hs_struct(tstruct, false);
}

/**
 * Generates a struct definition for a thrift exception. Basically the same
 * as a struct, but also has an exception declaration.
 *
 * @param txception The struct definition
 */
void t_hs_generator::generate_xception(t_struct* txception) {
  generate_hs_struct(txception, true);
}

/**
 * Generates a Haskell struct
 */
void t_hs_generator::generate_hs_struct(t_struct* tstruct,
                                              bool is_exception) {
  generate_hs_struct_definition(f_types_,tstruct, is_exception,false);
}

/**
 * Generates a struct definition for a thrift data type.
 *
 * @param tstruct The struct definition
 */
void t_hs_generator::generate_hs_struct_definition(ofstream& out,
                                                   t_struct* tstruct,
                                                   bool is_exception,
                                                   bool helper) {
  string tname = type_name(tstruct);
  string name = tstruct->get_name();
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  indent(out) << "data "<<tname<<" = "<<tname;
  if (members.size() > 0) {
    out << "{";
    bool first=true;
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      if(first)
        first=false;
      else
        out << ",";
      string mname = (*m_iter)->get_name();
      out << "f_" << tname << "_" << mname << " :: Maybe " << render_hs_type((*m_iter)->get_type());
    }
    out << "}";
  }

  out << " deriving (Show,Eq,Ord,Typeable)" << endl;
  if (is_exception) out << "instance Exception " << tname << endl;
  generate_hs_struct_writer(out, tstruct);

  generate_hs_struct_reader(out, tstruct);
  //f_struct_.close();
}



/**
 * Generates the read method for a struct
 */
void t_hs_generator::generate_hs_struct_reader(ofstream& out, t_struct* tstruct) {
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  string sname = type_name(tstruct);
  string str = tmp("_str");
  string t = tmp("_t");
  string id = tmp("_id");

  indent(out) << "read_" << sname << "_fields iprot rec = do" << endl;
  indent_up(); // do

  // Read beginning field marker
  indent(out) << "(_," << t <<","<<id<<") <- readFieldBegin iprot" << endl;
  // Check for field STOP marker and break
  indent(out) <<
    "if " << t <<" == T_STOP then return rec else" << endl;
  indent_up(); // if
  indent(out) << "case " << id<<" of " << endl;
  indent_up(); // case
  // Generate deserialization code for known cases
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    indent(out) << (*f_iter)->get_key() << " -> ";
    out << "if " << t <<" == " << type_to_enum((*f_iter)->get_type()) << " then do" << endl;
    indent_up(); // if
    indent(out) << "s <- ";
    generate_deserialize_field(out, *f_iter,str);
    out << endl;
    indent(out) << "read_"<<sname<<"_fields iprot rec{f_"<<sname<<"_"<< decapitalize((*f_iter)->get_name()) <<"=Just s}" << endl;
    out <<
      indent() << "else do" << endl;
    indent_up();
    indent(out) << "skip iprot "<< t << endl;
    indent(out) << "read_"<<sname<<"_fields iprot rec" << endl;
    indent_down(); // -do
    indent_down(); // -if
  }


  // In the default case we skip the field
  out <<
    indent() << "_ -> do" << endl;
  indent_up();
  indent(out) << "skip iprot "<<t<< endl;
  indent(out) << "readFieldEnd iprot" << endl;
  indent(out) << "read_"<<sname<<"_fields iprot rec" << endl;
  indent_down(); // -case
  indent_down(); // -if
  indent_down(); // -do
  indent_down();

  // read
  indent(out) << "read_"<<sname<<" iprot = do" << endl;
  indent_up();
  indent(out) << "readStructBegin iprot" << endl;
  indent(out) << "rec <- read_"<<sname<<"_fields iprot ("<<sname<<"{";
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if(first)
      first=false;
    else
      out << ",";
    out << "f_" << sname << "_" << decapitalize((*f_iter)->get_name()) << "=Nothing";
  }
  out << "})" << endl;
  indent(out) << "readStructEnd iprot" << endl;
  indent(out) << "return rec" << endl;
  indent_down();
}

void t_hs_generator::generate_hs_struct_writer(ofstream& out,
                                               t_struct* tstruct) {
  string name = type_name(tstruct);
  const vector<t_field*>& fields = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator f_iter;
  string str = tmp("_str");
  string f = tmp("_f");

  indent(out) <<
    "write_"<<name<<" oprot rec = do" << endl;
  indent_up();
  indent(out) <<
    "writeStructBegin oprot \""<<name<<"\"" << endl;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    // Write field header
    string mname = (*f_iter)->get_name();
    indent(out) <<
      "case f_" << name << "_" << mname << " rec of {Nothing -> return (); Just _v -> do" << endl;
    indent_up();
    indent(out) << "writeFieldBegin oprot (\""<< (*f_iter)->get_name()<<"\","
                <<type_to_enum((*f_iter)->get_type())<<","
                <<(*f_iter)->get_key()<<")" << endl;

    // Write field contents
    out << indent();
    generate_serialize_field(out, *f_iter, "_v");
    out << endl;
    // Write field closer
    indent(out) << "writeFieldEnd oprot}" << endl;
    indent_down();
  }

  // Write the struct map
  out <<
    indent() << "writeFieldStop oprot" << endl <<
    indent() << "writeStructEnd oprot" << endl;

  indent_down();
}

/**
 * Generates a thrift service.
 *
 * @param tservice The service definition
 */
void t_hs_generator::generate_service(t_service* tservice) {
  string f_service_name = get_out_dir()+capitalize(service_name_)+".hs";
  f_service_.open(f_service_name.c_str());

  f_service_ <<
    hs_autogen_comment() << endl <<
    "module " << capitalize(service_name_) << " where" << endl <<
    hs_imports() << endl;


  if(tservice->get_extends()){
    f_service_ <<
      "import qualified " << capitalize(tservice->get_extends()->get_name()) << endl;
  }


  f_service_ <<
     "import " << capitalize(program_name_) << "_Types" << endl <<
    "import qualified " << capitalize(service_name_) << "_Iface as Iface" << endl;


  // Generate the three main parts of the service
  generate_service_helpers(tservice);
  generate_service_interface(tservice);
  generate_service_client(tservice);
  generate_service_server(tservice);


  // Close service file
  f_service_.close();
}

/**
 * Generates helper functions for a service.
 *
 * @param tservice The service to generate a header definition for
 */
void t_hs_generator::generate_service_helpers(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  indent(f_service_) <<
    "-- HELPER FUNCTIONS AND STRUCTURES --" << endl << endl;

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* ts = (*f_iter)->get_arglist();
    generate_hs_struct_definition(f_service_,ts, false);
    generate_hs_function_helpers(*f_iter);
  }
}

/**
 * Generates a struct and helpers for a function.
 *
 * @param tfunction The function
 */
void t_hs_generator::generate_hs_function_helpers(t_function* tfunction) {
  t_struct result(program_, decapitalize(tfunction->get_name()) + "_result");
  t_field success(tfunction->get_returntype(), "success", 0);
  if (!tfunction->get_returntype()->is_void()) {
    result.append(&success);
  }

  t_struct* xs = tfunction->get_xceptions();
  const vector<t_field*>& fields = xs->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    result.append(*f_iter);
  }
  generate_hs_struct_definition(f_service_,&result, false);
}

/**
 * Generates a service interface definition.
 *
 * @param tservice The service to generate a header definition for
 */
void t_hs_generator::generate_service_interface(t_service* tservice) {
  string f_iface_name = get_out_dir()+capitalize(service_name_)+"_Iface.hs";
  f_iface_.open(f_iface_name.c_str());
  indent(f_iface_) << "module " << capitalize(service_name_) << "_Iface where" << endl;

  indent(f_iface_) <<
    hs_imports() << endl <<
    "import " << capitalize(program_name_) << "_Types" << endl <<
    endl;

  if (tservice->get_extends() != NULL) {
    string extends = type_name(tservice->get_extends());
    indent(f_iface_) << "import " << extends <<"_Iface" << endl;
    indent(f_iface_) << "class "<< extends << "_Iface a => " << capitalize(service_name_) << "_Iface a where" << endl;
  } else {
    f_iface_ << indent() << "class " << capitalize(service_name_) << "_Iface a where" << endl;
  }
  indent_up();

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string ft = function_type(*f_iter,true,true,true);
    f_iface_ <<
      indent() << decapitalize((*f_iter)->get_name()) << " :: a -> " << ft  << endl;
  }
  indent_down();
  f_iface_.close();

}

/**
 * Generates a service client definition. Note that in Haskell, the client doesn't implement iface. This is because
 * The client does not (and should not have to) deal with arguments being Nothing.
 *
 * @param tservice The service to generate a server for.
 */
void t_hs_generator::generate_service_client(t_service* tservice) {
  string f_client_name = get_out_dir()+capitalize(service_name_)+"_Client.hs";
  f_client_.open(f_client_name.c_str());

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;

  string extends = "";
  string exports="";
  bool first = true;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    if(first)
      first=false;
    else
      exports+=",";
    string funname = (*f_iter)->get_name();
    exports+=funname;
  }
  indent(f_client_) << "module " << capitalize(service_name_) << "_Client("<<exports<<") where" << endl;

  if (tservice->get_extends() != NULL) {
    extends = type_name(tservice->get_extends());
    indent(f_client_) << "import " << extends << "_Client" << endl;
  }
  indent(f_client_) << "import Data.IORef" << endl;
  indent(f_client_) << hs_imports() << endl;
  indent(f_client_) << "import " << capitalize(program_name_) << "_Types" << endl;
  indent(f_client_) << "import " << capitalize(service_name_) << endl;
  // DATS RITE A GLOBAL VAR
  indent(f_client_) << "seqid = newIORef 0" << endl;


  // Generate client method implementations

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* arg_struct = (*f_iter)->get_arglist();
    const vector<t_field*>& fields = arg_struct->get_members();
    vector<t_field*>::const_iterator fld_iter;
    string funname = (*f_iter)->get_name();

    string fargs = "";
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      fargs+= " arg_" + decapitalize((*fld_iter)->get_name());
    }

    // Open function
    indent(f_client_) << funname << " (ip,op)" <<  fargs << " = do" << endl;
    indent_up();
    indent(f_client_) <<  "send_" << funname << " op" << fargs;

    f_client_ << endl;

    if (!(*f_iter)->is_oneway()) {
      f_client_ << indent();
      f_client_ <<
        "recv_" << funname << " ip" << endl;
    }
    indent_down();

    indent(f_client_) <<
      "send_" << funname << " op" << fargs << " = do" << endl;
    indent_up();
    indent(f_client_) << "seq <- seqid" << endl;
    indent(f_client_) << "seqn <- readIORef seq" << endl;
    std::string argsname = capitalize((*f_iter)->get_name() + "_args");

    // Serialize the request header
    f_client_ <<
      indent() << "writeMessageBegin op (\"" << (*f_iter)->get_name() << "\", M_CALL, seqn)" << endl;
    f_client_ << indent() << "write_" << argsname << " op ("<<argsname<<"{";
    bool first = true;
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      if(first)
        first=false;
      else
        f_client_ << ",";
      f_client_ << "f_" << argsname <<"_" << (*fld_iter)->get_name() << "=Just arg_" << (*fld_iter)->get_name();
    }
    f_client_ << "})" << endl;

    // Write to the stream
    f_client_ <<
      indent() << "writeMessageEnd op" << endl <<
      indent() << "tFlush (getTransport op)" << endl;

    indent_down();

    if (!(*f_iter)->is_oneway()) {
      std::string resultname = capitalize((*f_iter)->get_name() + "_result");
      t_struct noargs(program_);

      std::string funname = string("recv_") + (*f_iter)->get_name();

      t_function recv_function((*f_iter)->get_returntype(),
                               funname,
                               &noargs);
      // Open function
      f_client_ <<
        indent() << funname << " ip = do" << endl;
      indent_up(); // fun

      // TODO(mcslee): Validate message reply here, seq ids etc.

      f_client_ <<
        indent() << "(fname, mtype, rseqid) <- readMessageBegin ip" << endl;
      f_client_ <<
        indent() << "if mtype == M_EXCEPTION then do" << endl <<
        indent() << "  x <- readAppExn ip" << endl <<
        indent() << "  readMessageEnd ip" << endl;
      f_client_ <<
        indent() << "  throw x" << endl;
      f_client_ <<
        indent() << "  else return ()" << endl;

      t_struct* xs = (*f_iter)->get_xceptions();
      const std::vector<t_field*>& xceptions = xs->get_members();

      f_client_ <<
        indent() << "res <- read_" << resultname << " ip" << endl;
      f_client_ <<
        indent() << "readMessageEnd ip" << endl;

      // Careful, only return _result if not a void function
      if (!(*f_iter)->get_returntype()->is_void()) {
        f_client_ <<
          indent() << "case f_" << resultname << "_success res of" << endl;
        indent_up(); // case
        indent(f_client_) << "Just v -> return v" << endl;
        indent(f_client_) << "Nothing -> do" << endl;
        indent_up(); // none
      }


      vector<t_field*>::const_iterator x_iter;
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        f_client_ <<
          indent() << "case f_"<< resultname << "_" << (*x_iter)->get_name() << " res of" << endl;
        indent_up(); //case
        indent(f_client_) << "Nothing -> return ()" << endl;
        indent(f_client_) << "Just _v -> throw _v" << endl;
        indent_down(); //-case
      }

      // Careful, only return _result if not a void function
      if ((*f_iter)->get_returntype()->is_void()) {
        indent(f_client_) <<
          "return ()" << endl;
      } else {
        f_client_ <<
          indent() << "throw (AppExn AE_MISSING_RESULT \"" << (*f_iter)->get_name() << " failed: unknown result\")" << endl;
        indent_down(); //-none
        indent_down(); //-case
      }

      // Close function
      indent_down(); //-fun
    }
  }
  f_client_.close();


}

/**
 * Generates a service server definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_hs_generator::generate_service_server(t_service* tservice) {
  // Generate the dispatch methods
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  // Generate the process subfunctions
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_process_function(tservice, *f_iter);
  }


  indent(f_service_) << "proc handler (iprot,oprot) (name,typ,seqid) = case name of" << endl;
  indent_up();
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string fname = (*f_iter)->get_name();
    indent(f_service_) << "\""<<fname<<"\" -> process_" << decapitalize(fname) << " (seqid,iprot,oprot,handler)" << endl;
  }
  indent(f_service_) << "_ -> ";
  if(tservice->get_extends() != NULL){
    f_service_ << type_name(tservice->get_extends()) << ".proc handler (iprot,oprot) (name,typ,seqid)" << endl;
  } else {
    f_service_ << "do" << endl;
    indent_up();
    indent(f_service_) << "skip iprot T_STRUCT" << endl;
    indent(f_service_) << "readMessageEnd iprot" << endl;
    indent(f_service_) << "writeMessageBegin oprot (name,M_EXCEPTION,seqid)" << endl;
    indent(f_service_) << "writeAppExn oprot (AppExn AE_UNKNOWN_METHOD (\"Unknown function \" ++ name))" << endl;
    indent(f_service_) << "writeMessageEnd oprot" << endl;
    indent(f_service_) << "tFlush (getTransport oprot)" << endl;
    indent_down();
  }
  indent_down();

  // Generate the server implementation
  indent(f_service_) <<
    "process handler (iprot, oprot) = do" << endl;
  indent_up();

  f_service_ <<
    indent() << "(name, typ, seqid) <- readMessageBegin iprot" << endl;
  f_service_ << indent() << "proc handler (iprot,oprot) (name,typ,seqid)" << endl;
  indent(f_service_) << "return True" << endl;
  indent_down();

}

/**
 * Generates a process function definition.
 *
 * @param tfunction The function to write a dispatcher for
 */
void t_hs_generator::generate_process_function(t_service* tservice,
                                               t_function* tfunction) {
  // Open function
  indent(f_service_) <<
    "process_" << tfunction->get_name() << " (seqid, iprot, oprot, handler) = do" << endl;
  indent_up();

  string argsname = capitalize(tfunction->get_name()) + "_args";
  string resultname = capitalize(tfunction->get_name()) + "_result";

  // Generate the function call
  t_struct* arg_struct = tfunction->get_arglist();
  const std::vector<t_field*>& fields = arg_struct->get_members();
  vector<t_field*>::const_iterator f_iter;


  f_service_ <<
    indent() << "args <- read_" << argsname << " iprot" << endl;
  f_service_ <<
    indent() << "readMessageEnd iprot" << endl;

  t_struct* xs = tfunction->get_xceptions();
  const std::vector<t_field*>& xceptions = xs->get_members();
  vector<t_field*>::const_iterator x_iter;
  int n = xceptions.size();
  if (!tfunction->is_oneway()){
    if(!tfunction->get_returntype()->is_void()){
      n++;
    }
    indent(f_service_) << "rs <- return (" << resultname;

    for(int i=0; i<n;i++){
      f_service_ << " Nothing";
    }
    f_service_ << ")" << endl;
  }

  indent(f_service_) << "res <- ";
  // Try block for a function with exceptions
  if (xceptions.size() > 0) {
    for(unsigned int i=0;i<xceptions.size();i++){
      f_service_ << "(Control.Exception.catch" << endl;
      indent_up();
      f_service_ << indent();
    }
  }

  f_service_ << "(do" << endl;
  indent_up();
  f_service_ << indent();
  if (!tfunction->is_oneway() && !tfunction->get_returntype()->is_void()){
    f_service_ << "res <- ";
  }
  f_service_ << "Iface." << tfunction->get_name() << " handler";
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    f_service_ <<  " (f_" << argsname <<  "_" << (*f_iter)->get_name() << " args)";
  }


  if (!tfunction->is_oneway() && !tfunction->get_returntype()->is_void()){
    f_service_ << endl;
    indent(f_service_) << "return rs{f_"<<resultname<<"_success= Just res}";
  } else if (!tfunction->is_oneway()){
    f_service_ << endl;
    indent(f_service_) << "return rs";
  }
  f_service_ << ")" << endl;
  indent_down();

  if (xceptions.size() > 0 && !tfunction->is_oneway()) {
    for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
      indent(f_service_) << "(\\e  -> " <<endl;
      indent_up();
      if(!tfunction->is_oneway()){
        f_service_ <<
          indent() << "return rs{f_"<<resultname<<"_" << (*x_iter)->get_name() << " =Just e}";
      } else {
        indent(f_service_) << "return ()";
      }
      f_service_ << "))" << endl;
      indent_down();
      indent_down();
    }
  }



  // Shortcut out here for oneway functions
  if (tfunction->is_oneway()) {
    f_service_ <<
      indent() << "return ()" << endl;
    indent_down();
    return;
  }

  f_service_ <<
    indent() << "writeMessageBegin oprot (\"" << tfunction->get_name() << "\", M_REPLY, seqid);" << endl <<
    indent() << "write_"<<resultname<<" oprot res" << endl <<
    indent() << "writeMessageEnd oprot" << endl <<
    indent() << "tFlush (getTransport oprot)" << endl;

  // Close function
  indent_down();
}

/**
 * Deserializes a field of any type.
 */
void t_hs_generator::generate_deserialize_field(ofstream &out,
                                                   t_field* tfield,
                                                   string prefix){
  t_type* type = tfield->get_type();
  generate_deserialize_type(out,type);
}


/**
 * Deserializes a field of any type.
 */
void t_hs_generator::generate_deserialize_type(ofstream &out,
                                                   t_type* type){
  type = get_true_type(type);

  if (type->is_void()) {
    throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE";
  }


  if (type->is_struct() || type->is_xception()) {
    generate_deserialize_struct(out,
                                (t_struct*)type);
  } else if (type->is_container()) {
    generate_deserialize_container(out, type);
  } else if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "compiler error: cannot serialize void field in a struct";
      break;
    case t_base_type::TYPE_STRING:
      out << "readString";
      break;
    case t_base_type::TYPE_BOOL:
      out << "readBool";
      break;
    case t_base_type::TYPE_BYTE:
      out << "readByte";
      break;
    case t_base_type::TYPE_I16:
      out << "readI16";
      break;
    case t_base_type::TYPE_I32:
      out << "readI32";
      break;
    case t_base_type::TYPE_I64:
      out << "readI64";
      break;
    case t_base_type::TYPE_DOUBLE:
      out << "readDouble";
      break;
    default:
      throw "compiler error: no PHP name for base type " + t_base_type::t_base_name(tbase);
    }
    out << " iprot";
  } else if (type->is_enum()) {
    string ename = capitalize(type->get_name());
    out << "(do {i <- readI32 iprot; return (toEnum i :: " << ename << ")})";
  } else {
    printf("DO NOT KNOW HOW TO DESERIALIZE TYPE '%s'\n",
           type->get_name().c_str());
  }
}


/**
 * Generates an unserializer for a struct, calling read()
 */
void t_hs_generator::generate_deserialize_struct(ofstream &out,
                                                  t_struct* tstruct) {
  string name = capitalize(tstruct->get_name());
  out << "(read_" << name << " iprot)";

}

/**
 * Serialize a container by writing out the header followed by
 * data and then a footer.
 */
void t_hs_generator::generate_deserialize_container(ofstream &out,
                                                    t_type* ttype) {
  string size = tmp("_size");
  string ktype = tmp("_ktype");
  string vtype = tmp("_vtype");
  string etype = tmp("_etype");
  string con = tmp("_con");

  t_field fsize(g_type_i32, size);
  t_field fktype(g_type_byte, ktype);
  t_field fvtype(g_type_byte, vtype);
  t_field fetype(g_type_byte, etype);

  // Declare variables, read header
  if (ttype->is_map()) {
    out << "(let {f 0 = return []; f n = do {k <- ";
    generate_deserialize_type(out,((t_map*)ttype)->get_key_type());
    out << "; v <- ";
    generate_deserialize_type(out,((t_map*)ttype)->get_val_type());
    out << ";r <- f (n-1); return $ (k,v):r}} in do {("<<ktype<<","<<vtype<<","<<size<<") <- readMapBegin iprot; l <- f " << size << "; return $ Map.fromList l})";
  } else if (ttype->is_set()) {
    out << "(let {f 0 = return []; f n = do {v <- ";
    generate_deserialize_type(out,((t_map*)ttype)->get_key_type());
    out << ";r <- f (n-1); return $ v:r}} in do {("<<etype<<","<<size<<") <- readSetBegin iprot; l <- f " << size << "; return $ Set.fromList l})";
  } else if (ttype->is_list()) {
    out << "(let {f 0 = return []; f n = do {v <- ";
    generate_deserialize_type(out,((t_map*)ttype)->get_key_type());
    out << ";r <- f (n-1); return $ v:r}} in do {("<<etype<<","<<size<<") <- readListBegin iprot; f " << size << "})";
  }
}


/**
 * Serializes a field of any type.
 *
 * @param tfield The field to serialize
 * @param prefix Name to prepend to field name
 */
void t_hs_generator::generate_serialize_field(ofstream &out,
                                                 t_field* tfield,
                                                 string name) {
  t_type* type = get_true_type(tfield->get_type());

  // Do nothing for void types
  if (type->is_void()) {
    throw "CANNOT GENERATE SERIALIZE CODE FOR void TYPE: " +
      tfield->get_name();
  }

  if(name.length() == 0){
    name = decapitalize(tfield->get_name());
  }

  if (type->is_struct() || type->is_xception()) {
    generate_serialize_struct(out,
                              (t_struct*)type,
                              name);
  } else if (type->is_container()) {
    generate_serialize_container(out,
                                 type,
                                 name);
  } else if (type->is_base_type() || type->is_enum()) {
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw
          "compiler error: cannot serialize void field in a struct: " + name;
        break;
      case t_base_type::TYPE_STRING:
        out << "writeString oprot " << name;
        break;
      case t_base_type::TYPE_BOOL:
        out << "writeBool oprot " << name;
       break;
      case t_base_type::TYPE_BYTE:
        out << "writeByte oprot " << name;
        break;
      case t_base_type::TYPE_I16:
        out << "writeI16 oprot " << name;
        break;
      case t_base_type::TYPE_I32:
        out << "writeI32 oprot " << name;
        break;
      case t_base_type::TYPE_I64:
        out << "writeI64 oprot " << name;
        break;
      case t_base_type::TYPE_DOUBLE:
        out << "writeDouble oprot " << name;
        break;
      default:
        throw "compiler error: no hs name for base type " + t_base_type::t_base_name(tbase);
      }

    } else if (type->is_enum()) {
      string ename = capitalize(type->get_name());
      out << "writeI32 oprot (fromEnum "<< name << ")";
    }

  } else {
    printf("DO NOT KNOW HOW TO SERIALIZE FIELD '%s' TYPE '%s'\n",
           tfield->get_name().c_str(),
           type->get_name().c_str());
  }
}

/**
 * Serializes all the members of a struct.
 *
 * @param tstruct The struct to serialize
 * @param prefix  String prefix to attach to all fields
 */
void t_hs_generator::generate_serialize_struct(ofstream &out,
                                               t_struct* tstruct,
                                               string prefix) {
  out << "write_" << type_name(tstruct) << " oprot " << prefix;
}

void t_hs_generator::generate_serialize_container(ofstream &out,
                                                  t_type* ttype,
                                                  string prefix) {
  if (ttype->is_map()) {
    string k = tmp("_kiter");
    string v = tmp("_viter");
    out << "(let {f [] = return (); f (("<<k<<","<<v<<"):t) = do {";
    generate_serialize_map_element(out, (t_map*)ttype, k, v);
    out << ";f t}} in do {writeMapBegin oprot ("<< type_to_enum(((t_map*)ttype)->get_key_type())<<","<< type_to_enum(((t_map*)ttype)->get_val_type())<<",Map.size " << prefix << "); f (Map.toList " << prefix << ");writeMapEnd oprot})";
  } else if (ttype->is_set()) {
    string v = tmp("_viter");
    out << "(let {f [] = return (); f ("<<v<<":t) = do {";
    generate_serialize_set_element(out, (t_set*)ttype, v);
    out << ";f t}} in do {writeSetBegin oprot ("<< type_to_enum(((t_set*)ttype)->get_elem_type())<<",Set.size " << prefix << "); f (Set.toList " << prefix << ");writeSetEnd oprot})";
  } else if (ttype->is_list()) {
    string v = tmp("_viter");
    out << "(let {f [] = return (); f ("<<v<<":t) = do {";
    generate_serialize_list_element(out, (t_list*)ttype, v);
    out << ";f t}} in do {writeListBegin oprot ("<< type_to_enum(((t_list*)ttype)->get_elem_type())<<",length " << prefix << "); f " << prefix << ";writeListEnd oprot})";
  }

}

/**
 * Serializes the members of a map.
 *
 */
void t_hs_generator::generate_serialize_map_element(ofstream &out,
                                                     t_map* tmap,
                                                     string kiter,
                                                     string viter) {
  t_field kfield(tmap->get_key_type(), kiter);
  out << "do {";
  generate_serialize_field(out, &kfield);
  out << ";";
  t_field vfield(tmap->get_val_type(), viter);
  generate_serialize_field(out, &vfield);
  out << "}";
}

/**
 * Serializes the members of a set.
 */
void t_hs_generator::generate_serialize_set_element(ofstream &out,
                                                     t_set* tset,
                                                     string iter) {
  t_field efield(tset->get_elem_type(), iter);
  generate_serialize_field(out, &efield);
}

/**
 * Serializes the members of a list.
 */
void t_hs_generator::generate_serialize_list_element(ofstream &out,
                                                      t_list* tlist,
                                                      string iter) {
  t_field efield(tlist->get_elem_type(), iter);
  generate_serialize_field(out, &efield);
}


string t_hs_generator::function_type(t_function* tfunc, bool options, bool io, bool method){
  string result="";

  const vector<t_field*>& fields = tfunc->get_arglist()->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if(options) result += "Maybe ";
    result += render_hs_type((*f_iter)->get_type(), options);
    result += " -> ";
  }
  if(fields.empty() && !method){
    result += "() -> ";
  }
  if(io) result += "IO ";
  result += render_hs_type(tfunc->get_returntype(), io);
  return result;
}


string t_hs_generator::type_name(t_type* ttype) {
  string prefix = "";
  t_program* program = ttype->get_program();
  if (program != NULL && program != program_) {
    if (!ttype->is_service()) {
      prefix = capitalize(program->get_name()) + "_Types.";
    }
  }

  string name = ttype->get_name();
  if(ttype->is_service()){
    name = capitalize(name);
  } else {
    name = capitalize(name);
  }
  return prefix + name;
}

/**
 * Converts the parse type to a Protocol.t_type enum
 */
string t_hs_generator::type_to_enum(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      return "T_VOID";
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
  } else if (type->is_struct() || type->is_xception()) {
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
 * Converts the parse type to an haskell type
 */
string t_hs_generator::render_hs_type(t_type* type, bool needs_parens) {
  type = get_true_type(type);
  string type_repr;

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      return "()";
    case t_base_type::TYPE_STRING:
      return "String";
    case t_base_type::TYPE_BOOL:
      return "Bool";
    case t_base_type::TYPE_BYTE:
      return "Int";
    case t_base_type::TYPE_I16:
      return "Int";
    case t_base_type::TYPE_I32:
      return "Int";
    case t_base_type::TYPE_I64:
      return "Int64";
    case t_base_type::TYPE_DOUBLE:
      return "Double";
    }
  } else if (type->is_enum()) {
    return capitalize(((t_enum*)type)->get_name());
  } else if (type->is_struct() || type->is_xception()) {
    return type_name((t_struct*)type);
  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();

    type_repr = "Map.Map " + render_hs_type(ktype, true) + " " + render_hs_type(vtype, true);
  } else if (type->is_set()) {
    t_type* etype = ((t_set*)type)->get_elem_type();

    type_repr = "Set.Set " + render_hs_type(etype, true) ;
  } else if (type->is_list()) {
    t_type* etype = ((t_list*)type)->get_elem_type();
    return "[" + render_hs_type(etype, false) + "]";
  } else {
    throw "INVALID TYPE IN type_to_enum: " + type->get_name();
  }

  return needs_parens ? "(" + type_repr + ")" : type_repr;
}


THRIFT_REGISTER_GENERATOR(hs, "Haskell", "");
