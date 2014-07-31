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
#include "version.h"

using std::map;
using std::ofstream;
using std::ostringstream;
using std::string;
using std::stringstream;
using std::vector;

static const string endl = "\n";  // avoid ostream << std::endl flushes

/**
 * Haskell code generator.
 *
 */
class t_hs_generator : public t_oop_generator {
 public:
  t_hs_generator(t_program* program,
                 const map<string, string>& parsed_options,
                 const string& option_string)
    : t_oop_generator(program)
  {
    (void) parsed_options;
    (void) option_string;
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

  string render_const_value(t_type* type, t_const_value* value);

  /**
   * Struct generation code
   */

  void generate_hs_struct            (t_struct* tstruct,
                                      bool is_exception);

  void generate_hs_struct_definition (ofstream &out,
                                      t_struct* tstruct,
                                      bool is_xception = false,
                                      bool helper = false);

  void generate_hs_struct_reader     (ofstream& out,
                                      t_struct* tstruct);

  void generate_hs_struct_writer     (ofstream& out,
                                      t_struct* tstruct);

  void generate_hs_struct_arbitrary  (ofstream& out,
                                      t_struct* tstruct);

  void generate_hs_function_helpers  (t_function* tfunction);

  void generate_hs_typemap           (ofstream& out,
                                      t_struct* tstruct);

  void generate_hs_default           (ofstream& out,
                                      t_struct* tstruct);


  /**
   * Service-level generation functions
   */

  void generate_service_helpers   (t_service* tservice);
  void generate_service_interface (t_service* tservice);
  void generate_service_client    (t_service* tservice);
  void generate_service_server    (t_service* tservice);
  void generate_process_function  (t_service* tservice,
                                   t_function* tfunction);

  /**
   * Serialization constructs
   */

  void generate_deserialize_field        (ofstream &out,
                                          t_field* tfield,
                                          string prefix);

  void generate_deserialize_struct       (ofstream &out,
                                          t_struct* tstruct,
                                          string name = "");

  void generate_deserialize_container    (ofstream &out,
                                          t_type* ttype,
                                          string arg = "");

  void generate_deserialize_set_element  (ofstream &out,
                                          t_set* tset);


  void generate_deserialize_list_element (ofstream &out,
                                          t_list* tlist,
                                          string prefix = "");

  void generate_deserialize_type          (ofstream &out,
                                           t_type* type,
                                           string arg = "");

  void generate_serialize_type           (ofstream &out,
                                          t_type* type,
                                          string name = "");

  void generate_serialize_struct         (ofstream &out,
                                          t_struct* tstruct,
                                          string prefix = "");

  void generate_serialize_container      (ofstream &out,
                                          t_type* ttype,
                                          string prefix = "");

  void generate_serialize_map_element    (ofstream &out,
                                          t_map* tmap,
                                          string kiter,
                                          string viter);

  void generate_serialize_set_element    (ofstream &out,
                                          t_set* tmap,
                                          string iter);

  void generate_serialize_list_element   (ofstream &out,
                                          t_list* tlist,
                                          string iter);

  /**
   * Helper rendering functions
   */

  string hs_autogen_comment();
  string hs_language_pragma();
  string hs_imports();

  string type_name(t_type* ttype,
                   string function_prefix = "");

  string field_name(string tname, string fname);

  string function_type(t_function* tfunc,
                       bool options = false,
                       bool io = false,
                       bool method = false);

  string type_to_enum(t_type* ttype);

  string type_to_default(t_type* ttype);

  string render_hs_type(t_type* type,
                        bool needs_parens);

  string type_to_constructor(t_type* ttype);

  string render_hs_type_for_function_name(t_type *type);

 private:

  ofstream f_types_;
  ofstream f_consts_;
  ofstream f_service_;
  ofstream f_iface_;
  ofstream f_client_;
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
  string f_types_name = get_out_dir() + pname + "_Types.hs";
  f_types_.open(f_types_name.c_str());

  string f_consts_name = get_out_dir() + pname + "_Consts.hs";
  f_consts_.open(f_consts_name.c_str());

  // Print header
  f_types_ << hs_language_pragma() << endl;
  f_types_ << hs_autogen_comment() << endl;
  f_types_ << "module " << pname << "_Types where" << endl;
  f_types_ << hs_imports() << endl;

  f_consts_ << hs_language_pragma() << endl;
  f_consts_ << hs_autogen_comment() << endl;
  f_consts_ << "module " << pname << "_Consts where" << endl;
  f_consts_ << hs_imports() << endl;
  f_consts_ << "import " << pname << "_Types" << endl;
}

string t_hs_generator::hs_language_pragma() {
  return string("{-# LANGUAGE DeriveDataTypeable #-}\n"
                "{-# LANGUAGE OverloadedStrings #-}\n"
                "{-# OPTIONS_GHC -fno-warn-missing-fields #-}\n"
                "{-# OPTIONS_GHC -fno-warn-missing-signatures #-}\n"
                "{-# OPTIONS_GHC -fno-warn-name-shadowing #-}\n"
                "{-# OPTIONS_GHC -fno-warn-unused-imports #-}\n"
                "{-# OPTIONS_GHC -fno-warn-unused-matches #-}\n");
}

/**
 * Autogen'd comment
 */
string t_hs_generator::hs_autogen_comment() {
  return string("-----------------------------------------------------------------\n") +
                "-- Autogenerated by Thrift Compiler (" + THRIFT_VERSION + ")                      --\n" +
                "--                                                             --\n" +
                "-- DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING --\n" +
                "-----------------------------------------------------------------\n";
}

/**
 * Prints standard thrift imports
 */
string t_hs_generator::hs_imports() {
  const vector<t_program*>& includes = program_->get_includes();
  string result = string(
      "import Prelude ( Bool(..), Enum, Float, IO, Double, String, Maybe(..),\n"
      "                 Eq, Show, Ord,\n"
      "                 concat, error, fromIntegral, fromEnum, length, map,\n"
      "                  maybe, not, null, otherwise, return, show, toEnum,\n"
      "                 enumFromTo, Bounded, minBound, maxBound,\n"
      "                 (.), (&&), (||), (==), (++), ($), (-), (>>=))\n"
      "\n"
      "import Control.Exception\n"
      "import Control.Monad ( liftM, ap, when )\n"
      "import Data.ByteString.Lazy (ByteString)\n"
      "import Data.Functor ( (<$>) )\n"
      "import Data.Hashable\n"
      "import Data.Int\n"
      "import Data.Maybe (catMaybes)\n"
      "import Data.Text.Lazy ( Text )\n"
      "import Data.Text.Lazy.Encoding ( decodeUtf8, encodeUtf8 )\n"
      "import qualified Data.Text.Lazy as T\n"
      "import Data.Typeable ( Typeable )\n"
      "import qualified Data.HashMap.Strict as Map\n"
      "import qualified Data.HashSet as Set\n"
      "import qualified Data.Vector as Vector\n"
      "import Test.QuickCheck.Arbitrary ( Arbitrary(..) )\n"
      "import Test.QuickCheck ( elements )\n"
      "\n"
      "import Thrift\n"
      "import Thrift.Types\n"
      "import Thrift.Arbitraries\n"
      "\n");

  for (size_t i = 0; i < includes.size(); ++i)
    result += "import qualified " + capitalize(includes[i]->get_name()) + "_Types\n";

  if (includes.size() > 0)
    result += "\n";

  return result;
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
  string tname = capitalize(ttypedef->get_symbolic());
  string tdef = render_hs_type(ttypedef->get_type(), false);
  indent(f_types_) << "type " << tname << " = " << tdef << endl;
  f_types_ << endl;
}

/**
 * Generates code for an enumerated type.
 * the values.
 *
 * @param tenum The enumeration
 */
void t_hs_generator::generate_enum(t_enum* tenum) {
  indent(f_types_) << "data " << capitalize(tenum->get_name()) << " = ";
  indent_up();
  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator c_iter;

  bool first = true;
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    string name = capitalize((*c_iter)->get_name());
    f_types_ << (first ? "" : "|");
    f_types_ << name;
    first = false;
  }
  indent(f_types_) << "deriving (Show,Eq, Typeable, Ord, Bounded)" << endl;
  indent_down();

  string ename = capitalize(tenum->get_name());

  indent(f_types_) << "instance Enum " << ename << " where" << endl;
  indent_up();
  indent(f_types_) << "fromEnum t = case t of" << endl;
  indent_up();
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    int value = (*c_iter)->get_value();
    string name = capitalize((*c_iter)->get_name());
    indent(f_types_) << name << " -> " << value << endl;
  }
  indent_down();
  indent(f_types_) << "toEnum t = case t of" << endl;
  indent_up();
  for(c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    int value = (*c_iter)->get_value();
    string name = capitalize((*c_iter)->get_name());
    indent(f_types_) << value << " -> " << name << endl;
  }
  indent(f_types_) << "_ -> throw ThriftException" << endl;
  indent_down();
  indent_down();

  indent(f_types_) << "instance Hashable " << ename << " where" << endl;
  indent_up();
  indent(f_types_) << "hashWithSalt salt = hashWithSalt salt . fromEnum" << endl;
  indent_down();

  indent(f_types_) << "instance Arbitrary " << ename << " where" << endl;
  indent_up();
  indent(f_types_) << "arbitrary = elements (enumFromTo minBound maxBound)" << endl;
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
  indent(f_consts_) << name << " = " << render_const_value(type, value) << endl;
  f_consts_ << endl;
}

/**
 * Prints the value of a constant with the given type. Note that type checking
 * is NOT performed in this function as it is always run beforehand using the
 * validate_types method in main.cc
 */
string t_hs_generator::render_const_value(t_type* type, t_const_value* value) {
  if (value == nullptr)
    return type_to_default(type);

  type = get_true_type(type);
  ostringstream out;

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
    for (auto& c_iter : constants) {
      int val = c_iter->get_value();
      if (val == value->get_integer()) {
        t_program* prog = type->get_program();
        if (prog != nullptr && prog != program_)
          out << capitalize(prog->get_name()) << "_Types.";
        out << capitalize(c_iter->get_name());
        break;
      }
    }

  } else if (type->is_struct() || type->is_xception()) {
    string cname = type_name(type);
    out << "default_" << cname << "{";

    const vector<t_field*>& fields = ((t_struct*)type)->get_members();
    vector<t_field*>::const_iterator f_iter;

    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;

    bool first = true;
    for (auto& v_iter : val) {
      t_field* field = nullptr;

      for (auto& f_iter : fields)
        if (f_iter->get_name() == v_iter.first->get_string())
          field = f_iter;

      if (field == nullptr)
        throw "type error: " + cname + " has no field " + v_iter.first->get_string();

      string fname = v_iter.first->get_string();
      string const_value = render_const_value(field->get_type(), v_iter.second);

      out << (first ? "" : ", ");
      out << field_name(cname, fname) << " = ";
      if (field->get_req() == t_field::T_OPTIONAL ||
	  ((t_type*)field->get_type())->is_xception()) {
        out << "Just ";
      }
      out << const_value;
      first = false;
    }

    out << "}";

  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();

    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;

    out << "(Map.fromList [";

    bool first = true;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string key = render_const_value(ktype, v_iter->first);
      string val = render_const_value(vtype, v_iter->second);
      out << (first ? "" : ",");
      out << "(" << key << "," << val << ")";
      first = false;
    }
    out << "])";

  } else if (type->is_list() || type->is_set()) {
    t_type* etype = type->is_list()
        ? ((t_list*) type)->get_elem_type()
        : ((t_set*) type)->get_elem_type();

    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;

    if (type->is_set())
      out << "(Set.fromList [";
    else
      out << "(Vector.fromList [";

    bool first = true;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      out << (first ? "" : ",");
      out << render_const_value(etype, *v_iter);
      first = false;
    }

    out << "])";

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
  (void) helper;
  string tname = type_name(tstruct);
  string name = tstruct->get_name();

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  indent(out) << "data " << tname << " = " << tname;
  if (members.size() > 0) {
    indent_up();
    bool first = true;
    for (auto* m_iter : members) {
      if (first) {
        indent(out) << "{ ";
        first = false;
      } else {
        indent(out) << ", ";
      }
      string mname = m_iter->get_name();
      out << field_name(tname, mname) << " :: ";
      if (m_iter->get_req() == t_field::T_OPTIONAL ||
	  ((t_type*)m_iter->get_type())->is_xception()) {
        out << "Maybe ";
      }
      out << render_hs_type(m_iter->get_type(), true) << endl;
    }
    indent(out) << "}";
    indent_down();
  }

  out << " deriving (Show,Eq,Typeable)" << endl;

  if (is_exception)
    out << "instance Exception " << tname << endl;

  indent(out) << "instance Hashable " << tname << " where" << endl;
  indent_up();
  indent(out) << "hashWithSalt salt record = salt";
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    string mname = (*m_iter)->get_name();
    indent(out) << " `hashWithSalt` " << field_name(tname, mname) << " record";
  }
  indent(out) << endl;
  indent_down();

  generate_hs_struct_arbitrary(out, tstruct);
  generate_hs_struct_writer(out, tstruct);
  generate_hs_struct_reader(out, tstruct);
  generate_hs_typemap(out, tstruct);
  generate_hs_default(out, tstruct);
}

void t_hs_generator::generate_hs_struct_arbitrary(ofstream& out, t_struct* tstruct) {
  string tname = type_name(tstruct);
  string name = tstruct->get_name();
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  indent(out) << "instance Arbitrary " << tname << " where "<< endl;
  indent_up();
  if (members.size() > 0) {
    indent(out) << "arbitrary = liftM " << tname;
    indent_up(); indent_up(); indent_up(); indent_up();
    bool first=true;
    for (auto* m_iter : members) {
      if(first) {
        first=false;
        out << " ";
      }
      else {
        indent(out) << "`ap`";
      }
      out << "(";
      if (m_iter->get_req() == t_field::T_OPTIONAL ||
	  ((t_type*)m_iter->get_type())->is_xception()) {
        out << "liftM Just ";
      }
      out << "arbitrary)" << endl;
    }
    indent_down(); indent_down(); indent_down(); indent_down();

    // Shrink
    indent(out) << "shrink obj | obj == default_" << tname << " = []" << endl;
    indent(out) << "           | otherwise = catMaybes" << endl;
    indent_up();
    first = true;
    for (auto& m_iter : members) {
      if (first) {
        first = false;
        indent(out) << "[ ";
      } else {
        indent(out) << ", ";
      }
      string fname = field_name(tname, m_iter->get_name());
      out << "if obj == default_" << tname;
      out << "{" << fname << " = " << fname << " obj} ";
      out << "then Nothing ";
      out << "else Just $ default_" << tname;
      out << "{" << fname << " = " << fname << " obj}" << endl;
    }
    indent(out) << "]" << endl;
    indent_down();
  } else { /* 0 == members.size() */
     indent(out) << "arbitrary = elements [" <<tname<< "]" << endl;
  }
  indent_down();
}

/**
 * Generates the read method for a struct
 */
void t_hs_generator::generate_hs_struct_reader(ofstream& out, t_struct* tstruct) {
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  string sname = type_name(tstruct);
  string id = tmp("_id");
  string val = tmp("_val");

  indent(out) << "to_" << sname << " :: ThriftVal -> " << sname << endl;
  indent(out) << "to_" << sname << " (TStruct fields) = " << sname << "{" << endl;
  indent_up();

  bool first = true;

  // Generate deserialization code for known cases
  for (auto* f_iter : fields) {
    int32_t key = f_iter->get_key();
    string etype = type_to_enum(f_iter->get_type());
    string fname = decapitalize(f_iter->get_name());

    if (first) {
      first = false;
    } else {
      out << "," << endl;
    }

    // Fill in Field
    indent(out) << field_name(sname, fname) << " = ";

    out << "maybe (";
    if (f_iter->get_req() == t_field::T_REQUIRED) {
      out << "error \"Missing required field: " << fname << "\"";
    } else {
      if ((f_iter->get_req() == t_field::T_OPTIONAL ||
	   ((t_type*)f_iter->get_type())->is_xception()) &&
	  f_iter->get_value() == nullptr) {
	out << "Nothing";
      } else {
        out << field_name(sname, fname) << " default_" << sname;
      }
    }
    out << ") ";

    out << "(\\(_," << val << ") -> ";
    if (f_iter->get_req() == t_field::T_OPTIONAL ||
	((t_type*)f_iter->get_type())->is_xception())
      out << "Just ";
    generate_deserialize_field(out, f_iter, val);
    out << ")";
    out << " (Map.lookup (" << key << ") fields)";

  }

  out << endl;
  indent(out) << "}" << endl;
  indent_down();

  // read
  string tmap = type_name(tstruct, "typemap_");
  indent(out) << "to_" << sname << " _ = error \"not a struct\"" << endl;

  indent(out) << "read_" << sname <<
    " :: (Transport t, Protocol p) => p t -> IO " << sname << endl;
  indent(out) << "read_" << sname << " iprot = to_" << sname;
  out << " <$> readVal iprot (T_STRUCT " << tmap << ")" << endl;

  indent(out) << "decode_" << sname <<
    " :: (Protocol p, Transport t) => p t -> ByteString -> " << sname << endl;
  indent(out) << "decode_" << sname << " iprot bs = to_" << sname << " $ ";
  out << "deserializeVal iprot (T_STRUCT " << tmap << ") bs" << endl;
}

void t_hs_generator::generate_hs_struct_writer(ofstream& out,
                                               t_struct* tstruct) {
  string name = type_name(tstruct);
  const vector<t_field*>& fields = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator f_iter;
  string str = tmp("_str");
  string f = tmp("_f");
  string v = tmp("_v");

  indent(out) << "from_" << name << " :: " << name << " -> ThriftVal" << endl;
  indent(out) << "from_" << name << " record = TStruct $ Map.fromList ";
  indent_up();

  // Get Exceptions
  bool hasExn = false;
  for (auto* f_iter : fields) {
    if (((t_type*)f_iter->get_type())->is_xception()) {
      hasExn = true;
      break;
    }
  }

  bool isfirst = true;
  if (hasExn) {
    out << endl;
    indent(out) << "(let exns = catMaybes ";
    indent_up();
    for (auto* f_iter : fields) {
      if (((t_type*)f_iter->get_type())->is_xception()) {
	if (isfirst) {
	  out << "[ ";
	  isfirst = false;
	} else {
	  out << ", ";
	}
	string mname = f_iter->get_name();
	int32_t key = f_iter->get_key();
	out << "(\\" << v << " -> (" << key << ", (\"" << mname << "\",";
	generate_serialize_type(out, f_iter->get_type(), v);
	out << "))) <$> " << field_name(name, mname) << " record";
      }
    }
    if (!isfirst) {
      out << "]" << endl;
    }
    indent_down();
    indent(out) << "in if not (null exns) then exns else ";
    indent_up();
  } else {
    out << "$ ";
  }

  out << "catMaybes" << endl;
  // Get the Rest
  isfirst = true;
  for (auto* f_iter : fields) {
    // Write field header
    if (isfirst) {
      indent(out) << "[ ";
      isfirst = false;
    } else {
      indent(out) << ", ";
    }
    string mname = f_iter->get_name();
    int32_t key = f_iter->get_key();
    out << "(\\";
    out << v << " -> ";
    if (f_iter->get_req() != t_field::T_OPTIONAL &&
	!((t_type*)f_iter->get_type())->is_xception()) {
      out << "Just ";
    }
    out << "(" << key << ", (\"" << mname << "\",";
    generate_serialize_type(out, f_iter->get_type(), v);
    out << "))) ";
    if (f_iter->get_req() != t_field::T_OPTIONAL &&
	!((t_type*)f_iter->get_type())->is_xception()) {
      out << "$";
    } else {
      out << "<$>";
    }
    out << " " << field_name(name, mname) << " record" << endl;
  }

  // Write the struct map
  if (isfirst) {
    indent(out) << "[]" << endl;
  } else {
    indent(out) << "]" << endl;
  }
  if (hasExn) {
    indent(out) << ")" << endl;
    indent_down();
  }
  indent_down();

  // write
  indent(out) << "write_" << name << " :: (Protocol p, Transport t) => p t -> "
              << name << " -> IO ()" << endl;
  indent(out) << "write_" << name << " oprot record = writeVal oprot $ from_";
  out << name << " record" << endl;

  // encode
  indent(out) << "encode_" << name << " :: (Protocol p, Transport t) => p t -> "
              << name << " -> ByteString" << endl;
  indent(out) << "encode_" << name << " oprot record = serializeVal oprot $ ";
  out << "from_" << name << " record" << endl;
}

/**
 * Generates a thrift service.
 *
 * @param tservice The service definition
 */
void t_hs_generator::generate_service(t_service* tservice) {
  string f_service_name = get_out_dir() + capitalize(service_name_) + ".hs";
  f_service_.open(f_service_name.c_str());

  f_service_ << hs_language_pragma() << endl;
  f_service_ << hs_autogen_comment() << endl;
  f_service_ << "module " << capitalize(service_name_) << " where" << endl;
  f_service_ << hs_imports() << endl;

  if (tservice->get_extends()) {
    f_service_ << "import qualified " << capitalize(tservice->get_extends()->get_name()) << endl;
  }

  f_service_ << "import " << capitalize(program_name_) << "_Types" << endl;
  f_service_ << "import qualified " << capitalize(service_name_) << "_Iface as Iface" << endl;

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

  indent(f_service_) << "-- HELPER FUNCTIONS AND STRUCTURES --" << endl;
  indent(f_service_) << endl;

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
  t_struct result(program_, field_name(tfunction->get_name(), "result"));
  t_field success(tfunction->get_returntype(), "success", 0);

  if (!tfunction->get_returntype()->is_void())
    result.append(&success);

  t_struct* xs = tfunction->get_xceptions();
  const vector<t_field*>& fields = xs->get_members();

  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    result.append(*f_iter);

  generate_hs_struct_definition(f_service_,&result, false);
}

/**
 * Generate the map from field names to (type, id)
 * @param tstruct the Struct
 */
void t_hs_generator::generate_hs_typemap(ofstream& out,
                                         t_struct* tstruct) {
  string name = type_name(tstruct);
  const auto& fields = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator f_iter;

  indent(out) << "typemap_" << name << " :: TypeMap" << endl;
  indent(out) << "typemap_" << name << " = Map.fromList [";
  bool first = true;
  for (const auto& f_iter : fields) {
    string mname = f_iter->get_name();
    if (!first) {
      out << ",";
    }

    t_type* type = get_true_type(f_iter->get_type());
    int32_t key = f_iter->get_key();
    out << "(\"" << mname << "\",(" << key << "," << type_to_enum(type) << "))";
    first = false;
  }
  out << "]" << endl;
}

/**
 * generate the struct with default values filled in
 * @param tstruct the Struct
 */
void t_hs_generator::generate_hs_default(ofstream& out,
                                         t_struct* tstruct) {
  string name = type_name(tstruct);
  string fname = type_name(tstruct, "default_");
  const auto& fields = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator f_iter;

  indent(out) << fname << " :: " << name << endl;
  indent(out) << fname << " = " << name << "{" << endl;
  indent_up();
  bool first = true;
  for (const auto& f_iter : fields) {
    string mname = f_iter->get_name();
    if (first) {
      first = false;
    } else {
      out << "," << endl;
    }

    t_type* type = get_true_type(f_iter->get_type());
    t_const_value* value = f_iter->get_value();
    indent(out) << field_name(name, mname) << " = ";
    if (f_iter->get_req() == t_field::T_OPTIONAL ||
	((t_type*)f_iter->get_type())->is_xception()) {
      if (value == nullptr) {
        out << "Nothing";
      } else {
        out << "Just " << render_const_value(type, value);
      }
    } else {
      out << render_const_value(type, value);
    }
  }
  out << "}" << endl;
  indent_down();
}

/**
 * Generates a service interface definition.
 *
 * @param tservice The service to generate a header definition for
 */
void t_hs_generator::generate_service_interface(t_service* tservice) {
  string f_iface_name = get_out_dir() + capitalize(service_name_) + "_Iface.hs";
  f_iface_.open(f_iface_name.c_str());

  f_iface_ << hs_language_pragma() << endl;
  f_iface_ << hs_autogen_comment() << endl;

  f_iface_ << "module " << capitalize(service_name_) << "_Iface where" << endl;

  f_iface_ << hs_imports() << endl;
  f_iface_ << "import " << capitalize(program_name_) << "_Types" << endl;
  f_iface_ << endl;

  string sname = capitalize(service_name_);
  if (tservice->get_extends() != NULL) {
    string extends = type_name(tservice->get_extends());

    indent(f_iface_) << "import " << extends << "_Iface" << endl;
    indent(f_iface_) << "class " << extends << "_Iface a => " << sname << "_Iface a where" << endl;

  } else {
    indent(f_iface_) << "class " << sname << "_Iface a where" << endl;
  }

  indent_up();

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string ft = function_type(*f_iter, true, true, true);
    indent(f_iface_) << decapitalize((*f_iter)->get_name()) << " :: a -> " << ft  << endl;
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
  string f_client_name = get_out_dir() + capitalize(service_name_) + "_Client.hs";
  f_client_.open(f_client_name.c_str());
  f_client_ << hs_language_pragma() << endl;
  f_client_ << hs_autogen_comment() << endl;

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;

  string extends = "";
  string exports = "";

  bool first = true;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    exports += (first ? "" : ",");
    string funname = (*f_iter)->get_name();
    exports += decapitalize(funname);
    first = false;
  }

  string sname = capitalize(service_name_);
  indent(f_client_) << "module " << sname << "_Client(" << exports << ") where" << endl;

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
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter)
      fargs += " arg_" + decapitalize((*fld_iter)->get_name());

    // Open function
    indent(f_client_) << decapitalize(funname) << " (ip,op)" <<  fargs << " = do" << endl;
    indent_up();
    indent(f_client_) <<  "send_" << funname << " op" << fargs;

    f_client_ << endl;

    if (!(*f_iter)->is_oneway())
      indent(f_client_) << "recv_" << funname << " ip" << endl;


    indent_down();

    indent(f_client_) << "send_" << funname << " op" << fargs << " = do" << endl;
    indent_up();

    indent(f_client_) << "seq <- seqid" << endl;
    indent(f_client_) << "seqn <- readIORef seq" << endl;
    string argsname = capitalize((*f_iter)->get_name() + "_args");

    // Serialize the request header
    string fname = (*f_iter)->get_name();
    indent(f_client_) << "writeMessage op (\"" << fname << "\", M_CALL, seqn)" << endl;
    indent(f_client_) << "write_" << argsname << " op (" << argsname << "{";

    bool first = true;
    for (auto& fld_iter : fields) {
      string fieldname = fld_iter->get_name();
      f_client_ << (first ? "" : ",");
      f_client_ << field_name(argsname, fieldname) << "=";
      if (fld_iter->get_req() == t_field::T_OPTIONAL ||
	  ((t_type*)fld_iter->get_type())->is_xception())
        f_client_ << "Just ";
      f_client_ << "arg_" << fieldname;
      first = false;
    }
    f_client_ << "})" << endl;

    // Write to the stream
    indent(f_client_) << "tFlush (getTransport op)" << endl;
    indent_down();

    if (!(*f_iter)->is_oneway()) {
      string resultname = capitalize((*f_iter)->get_name() + "_result");
      t_struct noargs(program_);

      string funname = string("recv_") + (*f_iter)->get_name();
      t_function recv_function((*f_iter)->get_returntype(), funname, &noargs);


      // Open function
      indent(f_client_) << funname << " ip = do" << endl;
      indent_up();

      indent(f_client_) << "(fname, mtype, rseqid) <- readMessage ip" << endl;
      indent(f_client_) << "when (mtype == M_EXCEPTION) $ readAppExn ip >>= throw" << endl;

      indent(f_client_) << "res <- read_" << resultname << " ip" << endl;

      t_struct* xs = (*f_iter)->get_xceptions();
      const vector<t_field*>& xceptions = xs->get_members();

      for (auto x_iter : xceptions) {
	indent(f_client_) << "maybe (return ()) throw ("
			  << field_name(resultname, x_iter->get_name())
			  << " res)" << endl;
      }

      if (!(*f_iter)->get_returntype()->is_void())
        indent(f_client_) << "return $ " << field_name(resultname, "success") << " res" << endl;
      else
	indent(f_client_) << "return ()" << endl;
 
      // Close function
      indent_down();
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
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter)
    generate_process_function(tservice, *f_iter);

  indent(f_service_) << "proc_ handler (iprot,oprot) (name,typ,seqid) = case name of" << endl;
  indent_up();

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string fname = (*f_iter)->get_name();
    indent(f_service_) << "\"" << fname << "\" -> process_" << decapitalize(fname) << " (seqid,iprot,oprot,handler)" << endl;
  }

  indent(f_service_) << "_ -> ";
  if (tservice->get_extends() != NULL) {
    f_service_ << type_name(tservice->get_extends()) << ".proc_ handler (iprot,oprot) (name,typ,seqid)" << endl;

  } else {
    f_service_ << "do" << endl;
    indent_up();
    indent(f_service_) << "_ <- readVal iprot (T_STRUCT Map.empty)" << endl;
    indent(f_service_) << "writeMessage oprot (name,M_EXCEPTION,seqid)" << endl;
    indent(f_service_) << "writeAppExn oprot (AppExn AE_UNKNOWN_METHOD (\"Unknown function \" ++ T.unpack name))" << endl;
    indent(f_service_) << "tFlush (getTransport oprot)" << endl;
    indent_down();
  }

  indent_down();

  // Generate the server implementation
  indent(f_service_) << "process handler (iprot, oprot) = do" << endl;
  indent_up();

  indent(f_service_) << "(name, typ, seqid) <- readMessage iprot" << endl;
  indent(f_service_) << "proc_ handler (iprot,oprot) (name,typ,seqid)" << endl;
  indent(f_service_) << "return True" << endl;
  indent_down();
}

bool hasNoArguments(t_function* func) {
    return (func->get_arglist()->get_members().empty());
}

string t_hs_generator::render_hs_type_for_function_name(t_type* type) {
    string type_str = render_hs_type(type, false);
    int found = -1;

    while (true) {
        found = type_str.find_first_of("[]. ", found + 1);
        if (string::npos == size_t(found)) {
            break;
        }

        if (type_str[found] == '.')
            type_str[found] = '_';
        else
            type_str[found] = 'Z';
    }
    return type_str;
}

/**
 * Generates a process function definition.
 *
 * @param tfunction The function to write a dispatcher for
 */
void t_hs_generator::generate_process_function(t_service* tservice,
                                               t_function* tfunction) {
  (void) tservice;
  // Open function
  string funname = decapitalize(tfunction->get_name());
  indent(f_service_) << "process_" << funname << " (seqid, iprot, oprot, handler) = do" << endl;
  indent_up();

  string argsname = capitalize(tfunction->get_name()) + "_args";
  string resultname = capitalize(tfunction->get_name()) + "_result";

  // Generate the function call
  t_struct* arg_struct = tfunction->get_arglist();
  const vector<t_field*>& fields = arg_struct->get_members();
  vector<t_field*>::const_iterator f_iter;

  indent(f_service_) << "args <- read_" << argsname << " iprot" << endl;

  t_struct* xs = tfunction->get_xceptions();
  const vector<t_field*>& xceptions = xs->get_members();
  vector<t_field*>::const_iterator x_iter;

  size_t n = xceptions.size() + 1;
  // Try block for a function with exceptions
  if (n > 0) {
    for(size_t i = 0; i < n; i++) {
      indent(f_service_) << "(Control.Exception.catch" << endl;
      indent_up();
    }
  }

  if (n > 0) {
    indent(f_service_) << "(do" << endl;
    indent_up();
  }
  indent(f_service_);

  if (!tfunction->is_oneway() && !tfunction->get_returntype()->is_void())
    f_service_ << "val <- ";

  f_service_ << "Iface." << decapitalize(tfunction->get_name()) << " handler";
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter)
    f_service_ << " (" <<
      field_name(argsname, (*f_iter)->get_name()) << " args)";

  if (!tfunction->is_oneway() && !tfunction->get_returntype()->is_void()) {
    f_service_ << endl;
    indent(f_service_) << "let res = default_" << resultname << "{" <<
      field_name(resultname, "success") << " = val}";

  } else if (!tfunction->is_oneway()) {
    f_service_ << endl;
    indent(f_service_) << "let res = default_" << resultname;
  }
  f_service_ << endl;

  // Shortcut out here for oneway functions
  if (tfunction->is_oneway()) {
    indent(f_service_) << "return ()";
  } else {
    indent(f_service_ ) << "writeMessage oprot (\"" << tfunction->get_name() << "\", M_REPLY, seqid)" << endl;
    indent(f_service_ ) << "write_" << resultname << " oprot res" << endl;
    indent(f_service_ ) << "tFlush (getTransport oprot)";
  }
  if (n > 0) {
    f_service_ << ")";
    indent_down();
  }
  f_service_ << endl;

  if (n > 0) {
    for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
      indent(f_service_) << "(\\e  -> do" << endl;
      indent_up();

      if (!tfunction->is_oneway()) {
	indent(f_service_) << "let res = default_" << resultname << "{"
			   << field_name(resultname, (*x_iter)->get_name()) << " = Just e}" << endl;
	indent(f_service_) << "writeMessage oprot (\"" << tfunction->get_name() << "\", M_REPLY, seqid)" << endl;
	indent(f_service_ ) << "write_" << resultname << " oprot res" << endl;
	indent(f_service_ ) << "tFlush (getTransport oprot)";
      } else {
        indent(f_service_) << "return ()";
      }

      f_service_ << "))" << endl;
      indent_down();
      indent_down();
    }
    indent(f_service_) << "((\\_ -> do" << endl;
    indent_up();

    if (!tfunction->is_oneway()) {
      indent(f_service_) << "writeMessage oprot (\"" << tfunction->get_name() << "\", M_EXCEPTION, seqid)" << endl;
      indent(f_service_ ) << "writeAppExn oprot (AppExn AE_UNKNOWN \"\")" << endl;
      indent(f_service_ ) << "tFlush (getTransport oprot)";
    } else {
      indent(f_service_) << "return ()";
    }

    f_service_ << ") :: SomeException -> IO ()))" << endl;
    indent_down();
    indent_down();
      
  }
  // Close function
  indent_down();

}

/**
 * Deserializes a field of any type.
 */
void t_hs_generator::generate_deserialize_field(ofstream &out,
                                                t_field* tfield,
                                                string prefix) {
  (void) prefix;
  t_type* type = tfield->get_type();
  generate_deserialize_type(out,type, prefix);
}

/**
 * Deserializes a field of any type.
 */
void t_hs_generator::generate_deserialize_type(ofstream &out,
                                               t_type* type,
                                               string arg) {
  type = get_true_type(type);
  string val = tmp("_val");
  out << "(case " << arg << " of {" << type_to_constructor(type) << " " << val << " -> ";

  if (type->is_void())
    throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE";

  if (type->is_struct() || type->is_xception()) {
    generate_deserialize_struct(out, (t_struct*)type, val);

  } else if (type->is_container()) {
    generate_deserialize_container(out, type, val);

  } else if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    if (tbase == t_base_type::TYPE_STRING && !((t_base_type*)type)->is_binary()) {
      out << "decodeUtf8 ";
    }
    out << val;
  } else if (type->is_enum()) {
    out << "toEnum $ fromIntegral " << val;

  } else {
    throw "DO NOT KNOW HOW TO DESERIALIZE TYPE " + type->get_name();
  }
  out << "; _ -> error \"wrong type\"})";
}


/**
 * Generates an unserializer for a struct, calling read()
 */
void t_hs_generator::generate_deserialize_struct(ofstream &out,
                                                 t_struct* tstruct,
                                                 string name) {

  out << "(" << type_name(tstruct, "to_") << " (TStruct " << name << "))";
}

/**
 * Serialize a container by writing out the header followed by
 * data and then a footer.
 */
void t_hs_generator::generate_deserialize_container(ofstream &out,
                                                    t_type* ttype,
                                                    string arg) {

  string val = tmp("_v");
  // Declare variables, read header
  if (ttype->is_map()) {
    string key = tmp("_k");
    out << "(Map.fromList $ map (\\(" << key << "," << val << ") -> (";
    generate_deserialize_type(out,((t_map*)ttype)->get_key_type(),key);

    out << ",";
    generate_deserialize_type(out,((t_map*)ttype)->get_val_type(),val);

    out << ")) " << arg << ")";

  } else if (ttype->is_set()) {
    out << "(Set.fromList $ map (\\" << val << " -> ";
    generate_deserialize_type(out,((t_map*)ttype)->get_key_type(),val);
    out << ") " << arg << ")";

  } else if (ttype->is_list()) {
    out << "(Vector.fromList $ map (\\" << val << " -> ";
    generate_deserialize_type(out,((t_map*)ttype)->get_key_type(),val);
    out << ") " << arg << ")";
  }
}

/**
 * Serializes a field of any type.
 *
 * @param tfield The field to serialize
 * @param prefix Name to prepend to field name
 */
void t_hs_generator::generate_serialize_type(ofstream &out,
                                              t_type* type,
                                              string name) {

  type = get_true_type(type);
  // Do nothing for void types
  if (type->is_void())
    throw "CANNOT GENERATE SERIALIZE CODE FOR void TYPE";

  if (type->is_struct() || type->is_xception()) {
    generate_serialize_struct(out, (t_struct*)type, name);

  } else if (type->is_container()) {
    generate_serialize_container(out, type, name);

  } else if (type->is_base_type() || type->is_enum()) {
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      out << type_to_constructor(type) << " ";
      if (tbase == t_base_type::TYPE_STRING && !((t_base_type*)type)->is_binary()) {
        out << "$ encodeUtf8 ";
      }
      out << name;

    } else if (type->is_enum()) {
      string ename = capitalize(type->get_name());
      out << "TI32 $ fromIntegral $ fromEnum " << name;
    }

  } else {
    throw "DO NOT KNOW HOW TO SERIALIZE FIELD OF TYPE " + type->get_name();
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
  out << type_name(tstruct, "from_") << " " << prefix;
}

void t_hs_generator::generate_serialize_container(ofstream &out,
                                                  t_type* ttype,
                                                  string prefix) {
  string k = tmp("_k");
  string v = tmp("_v");

  if (ttype->is_map()) {
    t_type* ktype = ((t_map*)ttype)->get_key_type();
    t_type* vtype = ((t_map*)ttype)->get_val_type();
    out << "TMap " << type_to_enum(ktype) << " " << type_to_enum(vtype);
    out << " $ map (\\(" << k << "," << v << ") -> (";
    generate_serialize_type(out, ktype, k);
    out << ", ";
    generate_serialize_type(out, vtype, v);
    out << ")) $ Map.toList " << prefix;

  } else if (ttype->is_set()) {
    out << "TSet " << type_to_enum(((t_list*)ttype)->get_elem_type());
    out <<" $ map (\\" << v << " -> ";
    generate_serialize_type(out, ((t_list*)ttype)->get_elem_type(), v);
    out << ") $ Set.toList " << prefix;

  } else if (ttype->is_list()) {
    out << "TList " << type_to_enum(((t_list*)ttype)->get_elem_type());
    out <<" $ map (\\" << v << " -> ";
    generate_serialize_type(out, ((t_list*)ttype)->get_elem_type(), v);
    out << ") $ Vector.toList " << prefix;
  }

}

string t_hs_generator::function_type(t_function* tfunc, bool options, bool io, bool method) {
  string result = "";

  const vector<t_field*>& fields = tfunc->get_arglist()->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (auto& f_iter : fields) {
    if (f_iter->get_req() == t_field::T_OPTIONAL ||
	((t_type*)f_iter->get_type())->is_xception())
      result += "Maybe ";
    result += render_hs_type(f_iter->get_type(), options);
    result += " -> ";
  }

  if (fields.empty() && !method)
    result += "() -> ";

  if (io)
    result += "IO ";

  result += render_hs_type(tfunc->get_returntype(), io);
  return result;
}


string t_hs_generator::type_name(t_type* ttype, string function_prefix) {
  string prefix = "";
  t_program* program = ttype->get_program();

  if (program != NULL && program != program_)
    if (!ttype->is_service())
      prefix = capitalize(program->get_name()) + "_Types.";

  return prefix + function_prefix + capitalize(ttype->get_name());
}

string t_hs_generator::field_name(string tname, string fname) {
  return decapitalize(tname) + "_" + fname;
}

/**
 * Converts the parse type to a Protocol.t_type enum
 */
string t_hs_generator::type_to_enum(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:   return "T_VOID";
    case t_base_type::TYPE_STRING: return "T_STRING";
    case t_base_type::TYPE_BOOL:   return "T_BOOL";
    case t_base_type::TYPE_BYTE:   return "T_BYTE";
    case t_base_type::TYPE_I16:    return "T_I16";
    case t_base_type::TYPE_I32:    return "T_I32";
    case t_base_type::TYPE_I64:    return "T_I64";
    case t_base_type::TYPE_DOUBLE: return "T_DOUBLE";
    }

  } else if (type->is_enum()) {
    return "T_I32";

  } else if (type->is_struct() || type->is_xception()) {
    return "(T_STRUCT " + type_name((t_struct*)type, "typemap_") + ")";

  } else if (type->is_map()) {
    string ktype = type_to_enum(((t_map*)type)->get_key_type());
    string vtype = type_to_enum(((t_map*)type)->get_val_type());
    return "(T_MAP " + ktype + " " + vtype + ")";

  } else if (type->is_set()) {
    return "(T_SET " + type_to_enum(((t_list*)type)->get_elem_type()) + ")";

  } else if (type->is_list()) {
    return "(T_LIST " + type_to_enum(((t_list*)type)->get_elem_type()) + ")";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}

/**
 * Converts the parse type to a default value
 */
string t_hs_generator::type_to_default(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:   return "error \"No default value for type T_VOID\"";
    case t_base_type::TYPE_STRING: return "\"\"";
    case t_base_type::TYPE_BOOL:   return "False";
    case t_base_type::TYPE_BYTE:   return "0";
    case t_base_type::TYPE_I16:    return "0";
    case t_base_type::TYPE_I32:    return "0";
    case t_base_type::TYPE_I64:    return "0";
    case t_base_type::TYPE_DOUBLE: return "0";
    }

  } else if (type->is_enum()) {
    return "(toEnum 0)";

  } else if (type->is_struct() || type->is_xception()) {
    return type_name((t_struct*)type, "default_");

  } else if (type->is_map()) {
    return "Map.empty";

  } else if (type->is_set()) {
    return "Set.empty";

  } else if (type->is_list()) {
    return "Vector.empty";
  }

  throw "INVALID TYPE IN type_to_default: " + type->get_name();
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
    case t_base_type::TYPE_VOID:   return "()";
    case t_base_type::TYPE_STRING: return (((t_base_type*)type)->is_binary() ? "ByteString" : "T.Text");
    case t_base_type::TYPE_BOOL:   return "Bool";
    case t_base_type::TYPE_BYTE:   return "Int8";
    case t_base_type::TYPE_I16:    return "Int16";
    case t_base_type::TYPE_I32:    return "Int32";
    case t_base_type::TYPE_I64:    return "Int64";
    case t_base_type::TYPE_DOUBLE: return "Double";
    }

  } else if (type->is_enum()) {
    return type_name((t_enum*)type);

  } else if (type->is_struct() || type->is_xception()) {
    return type_name((t_struct*)type);

  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();
    type_repr = "Map.HashMap " + render_hs_type(ktype, true) + " " + render_hs_type(vtype, true);

  } else if (type->is_set()) {
    t_type* etype = ((t_set*)type)->get_elem_type();
    type_repr = "Set.HashSet " + render_hs_type(etype, true) ;

  } else if (type->is_list()) {
    t_type* etype = ((t_list*)type)->get_elem_type();
    type_repr = "Vector.Vector " + render_hs_type(etype, true);

  } else {
    throw "INVALID TYPE IN type_to_enum: " + type->get_name();
  }

  return needs_parens ? "(" + type_repr + ")" : type_repr;
}

/**
 * Converts the parse type to a haskell constructor
 */
string t_hs_generator::type_to_constructor(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:   throw "invalid type: T_VOID";
    case t_base_type::TYPE_STRING: return "TString";
    case t_base_type::TYPE_BOOL:   return "TBool";
    case t_base_type::TYPE_BYTE:   return "TByte";
    case t_base_type::TYPE_I16:    return "TI16";
    case t_base_type::TYPE_I32:    return "TI32";
    case t_base_type::TYPE_I64:    return "TI64";
    case t_base_type::TYPE_DOUBLE: return "TDouble";
    }

  } else if (type->is_enum()) {
    return "TI32";

  } else if (type->is_struct() || type->is_xception()) {
    return "TStruct";

  } else if (type->is_map()) {
    return "TMap _ _";

  } else if (type->is_set()) {
    return "TSet _";

  } else if (type->is_list()) {
    return "TList _";
  }
  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}


THRIFT_REGISTER_GENERATOR(hs, "Haskell", "")
