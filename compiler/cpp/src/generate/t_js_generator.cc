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
#include <list>

#include <stdlib.h>
#include <sys/stat.h>
#include <sstream>
#include "platform.h"
using namespace std;


#include "t_oop_generator.h"

/**
 * JS code generator.
 */
class t_js_generator : public t_oop_generator {
 public:
  t_js_generator(t_program* program,
                const std::map<std::string, std::string>& parsed_options,
                const std::string& option_string) :
     t_oop_generator(program) {

     out_dir_base_ = "gen-js";
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
   * Structs!
   */
  void generate_js_struct(t_struct* tstruct, bool is_exception);
  void generate_js_struct_definition(std::ofstream& out, t_struct* tstruct, bool is_xception=false);
  void generate_js_struct_reader(std::ofstream& out, t_struct* tstruct);
  void generate_js_struct_writer(std::ofstream& out, t_struct* tstruct);
  void generate_js_function_helpers(t_function* tfunction);

  /**
   * Service-level generation functions
   */
  void generate_service_helpers   (t_service* tservice);
  void generate_service_interface (t_service* tservice);
  void generate_service_rest      (t_service* tservice);
  void generate_service_client    (t_service* tservice);
  void generate_service_processor (t_service* tservice);
  void generate_process_function  (t_service* tservice, t_function* tfunction);

  /**
   * Serialization constructs
   */

  void generate_deserialize_field        (std::ofstream &out,
                                          t_field*    tfield,
                                          std::string prefix="",
                                          bool inclass=false);

  void generate_deserialize_struct       (std::ofstream &out,
                                          t_struct*   tstruct,
                                          std::string prefix="");

  void generate_deserialize_container    (std::ofstream &out,
                                          t_type*     ttype,
                                          std::string prefix="");

  void generate_deserialize_set_element  (std::ofstream &out,
                                          t_set*      tset,
                                          std::string prefix="");

  void generate_deserialize_map_element  (std::ofstream &out,
                                          t_map*      tmap,
                                          std::string prefix="");

  void generate_deserialize_list_element (std::ofstream &out,
                                          t_list*     tlist,
                                          std::string prefix="");

  void generate_serialize_field          (std::ofstream &out,
                                          t_field*    tfield,
                                          std::string prefix="");

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

  std::string js_includes();
  std::string declare_field(t_field* tfield, bool init=false, bool obj=false);
  std::string function_signature(t_function* tfunction, std::string prefix="");
  std::string argument_list(t_struct* tstruct);
  std::string type_to_enum(t_type* ttype);

  std::string autogen_comment() {
    return
      std::string("//\n") +
      "// Autogenerated by Thrift\n" +
      "//\n" +
      "// DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING\n" +
      "//\n";
  }

  std::vector<std::string> js_namespace_pieces(t_program* p) {
      std::string ns = p->get_namespace("js");

      std::string::size_type   loc;
      std::vector<std::string> pieces;

      if (ns.size() > 0) {
          while ((loc = ns.find(".")) != std::string::npos) {
              pieces.push_back(ns.substr(0, loc));
              ns = ns.substr(loc+1);
          }
      }

      if (ns.size() > 0) {
          pieces.push_back(ns);
      }

      return pieces;
  }

  std::string js_namespace(t_program* p) {
      std::string ns = p->get_namespace("js");
      if (ns.size() > 0) {
          ns += ".";
      }


      return ns;
  }

 private:

  /**
   * File streams
   */
  std::ofstream f_types_;
  std::ofstream f_service_;
};


/**
 * Prepares for file generation by opening up the necessary file output
 * streams.
 *
 * @param tprogram The program to generate
 */
void t_js_generator::init_generator() {
  // Make output directory
  MKDIR(get_out_dir().c_str());

  string outdir = get_out_dir();

  // Make output file
  string f_types_name = outdir+program_->get_name()+"_types.js";
  f_types_.open(f_types_name.c_str());

  // Print header
  f_types_ <<
    autogen_comment() <<
    js_includes();


  string pns;

  //setup the namespace
  vector<string> ns_pieces = js_namespace_pieces( program_ );
  if( ns_pieces.size() > 0){
      f_types_ << "var " << ns_pieces[0] << " = {}"<<endl;

      pns = ns_pieces[0];

      for(size_t i=1; i<ns_pieces.size(); i++){
          pns += "." + ns_pieces[i];

          f_types_ << pns << " = {}"<<endl;
      }
  }

}

/**
 * Prints standard js imports
 */
string t_js_generator::js_includes() {
  string inc;

  return inc;
}

/**
 * Close up (or down) some filez.
 */
void t_js_generator::close_generator() {
  // Close types file

  f_types_.close();
}

/**
 * Generates a typedef. This is not done in JS, types are all implicit.
 *
 * @param ttypedef The type definition
 */
void t_js_generator::generate_typedef(t_typedef* ttypedef) {}

/**
 * Generates code for an enumerated type. Since define is expensive to lookup
 * in JS, we use a global array for this.
 *
 * @param tenum The enumeration
 */
void t_js_generator::generate_enum(t_enum* tenum) {

  f_types_ << js_namespace(tenum->get_program())<<tenum->get_name()<<" = { "<<endl;

  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator c_iter;
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    int value = (*c_iter)->get_value();
    if (c_iter != constants.begin())
        f_types_ << ",";

    f_types_ << "'" << (*c_iter)->get_name() << "' : " << value << endl;
  }

  f_types_ << "}"<<endl;
}

/**
 * Generate a constant value
 */
void t_js_generator::generate_const(t_const* tconst) {
  t_type* type = tconst->get_type();
  string name = tconst->get_name();
  t_const_value* value = tconst->get_value();

  f_types_ << js_namespace(program_)  << name << " = ";
  f_types_ << render_const_value(type, value) << endl;
}

/**
 * Prints the value of a constant with the given type. Note that type checking
 * is NOT performed in this function as it is always run beforehand using the
 * validate_types method in main.cc
 */
string t_js_generator::render_const_value(t_type* type, t_const_value* value) {
  std::ostringstream out;

  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_STRING:
      out << "'" << value->get_string() << "'";
      break;
    case t_base_type::TYPE_BOOL:
      out << (value->get_integer() > 0 ? "true" : "false");
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
    out << value->get_integer();
  } else if (type->is_struct() || type->is_xception()) {
    out << "new " << js_namespace(type->get_program()) << type->get_name() << "({" << endl;
    indent_up();
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
      if (v_iter != val.begin())
        out << ",";
      out << render_const_value(g_type_string, v_iter->first);
      out << " : ";
      out << render_const_value(field_type, v_iter->second);
    }

    out << "})";
  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    bool    key_is_string = false;
    if (ktype->is_base_type() && ((t_base_type*)ktype)->get_base() == t_base_type::TYPE_STRING){
        key_is_string = true;
    }

    t_type* vtype = ((t_map*)type)->get_val_type();
    out << "{";

    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
        if (v_iter != val.begin())
          out << "," << endl;

        out << render_const_value(ktype, v_iter->first);

        out << " : ";
        out << render_const_value(vtype, v_iter->second);
    }

    out << endl << "}";
  } else if (type->is_list() || type->is_set()) {
    t_type* etype;
    if (type->is_list()) {
      etype = ((t_list*)type)->get_elem_type();
    } else {
      etype = ((t_set*)type)->get_elem_type();
    }
    out << "[";
    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      if (v_iter != val.begin())
        out << ",";
      out << render_const_value(etype, *v_iter);
    }
    out << "]";
  }
  return out.str();
}

/**
 * Make a struct
 */
void t_js_generator::generate_struct(t_struct* tstruct) {
  generate_js_struct(tstruct, false);
}

/**
 * Generates a struct definition for a thrift exception. Basically the same
 * as a struct but extends the Exception class.
 *
 * @param txception The struct definition
 */
void t_js_generator::generate_xception(t_struct* txception) {
  generate_js_struct(txception, true);
}

/**
 * Structs can be normal or exceptions.
 */
void t_js_generator::generate_js_struct(t_struct* tstruct,
                                            bool is_exception) {
  generate_js_struct_definition(f_types_, tstruct, is_exception);
}

/**
 * Generates a struct definition for a thrift data type. This is nothing in JS
 * where the objects are all just associative arrays (unless of course we
 * decide to start using objects for them...)
 *
 * @param tstruct The struct definition
 */
void t_js_generator::generate_js_struct_definition(ofstream& out,
                                                       t_struct* tstruct,
                                                       bool is_exception) {
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  out <<  js_namespace(tstruct->get_program()) << tstruct->get_name() <<" = function(args){\n";


  //members with arguments
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    string dval = declare_field(*m_iter,true,true);
    t_type* t = get_true_type((*m_iter)->get_type());
    if ((*m_iter)->get_value() != NULL && !(t->is_struct() || t->is_xception())) {
        dval = render_const_value((*m_iter)->get_type(), (*m_iter)->get_value());
        out << indent() << "this." << (*m_iter)->get_name() << " = " << dval << endl;
    } else {
        out << indent() <<  dval << endl;
    }

  }

  // Generate constructor from array
  if (members.size() > 0) {

    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      t_type* t = get_true_type((*m_iter)->get_type());
      if ((*m_iter)->get_value() != NULL && (t->is_struct() || t->is_xception())) {
        indent(out) << "this." << (*m_iter)->get_name() << " = " << render_const_value(t, (*m_iter)->get_value())  << endl;
      }
    }

    out << "if( args != null ){";

    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        out << indent() << "if (null != args." << (*m_iter)->get_name() << ")" <<endl
            << indent() << "this." << (*m_iter)->get_name() << " = args." << (*m_iter)->get_name()  << endl ;

    }

    out << "}";

  }

  indent_down();
  out << "}\n";

  if (is_exception) {
      out << "for (var property in Thrift.Exception)"<<endl<<
          js_namespace(tstruct->get_program())<<tstruct->get_name()<<"[property] = Thrift.Exception[property]"<<endl;
  }

  //init prototype
  out << js_namespace(tstruct->get_program())<<tstruct->get_name() <<".prototype = {}\n";


  generate_js_struct_reader(out, tstruct);
  generate_js_struct_writer(out, tstruct);

}

/**
 * Generates the read() method for a struct
 */
void t_js_generator::generate_js_struct_reader(ofstream& out,
                                                   t_struct* tstruct) {
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  out <<  js_namespace(tstruct->get_program())<<tstruct->get_name() << ".prototype.read = function(input){ "<<endl;

  indent_up();

  indent(out) << "var ret = input.readStructBegin()" << endl;


  // Loop over reading in fields
  indent(out) << "while (1) " << endl;

  scope_up(out);

  indent(out) << "var ret = input.readFieldBegin()" << endl;
  indent(out) << "var fname = ret.fname" <<endl;
  indent(out) << "var ftype = ret.ftype" <<endl;
  indent(out) << "var fid   = ret.fid"   <<endl;


  // Check for field STOP marker and break
  indent(out) << "if (ftype == Thrift.Type.STOP) " << endl;
  indent_up();
  indent(out) << "break" << endl;
  indent_down();

  // Switch statement on the field we are reading
  indent(out) << "switch(fid)" << endl;

  scope_up(out);

  // Generate deserialization code for known cases
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {

    indent(out) << "case " << (*f_iter)->get_key() << ":";
    indent(out) << "if (ftype == " << type_to_enum((*f_iter)->get_type()) << ") {" << endl;

    indent_up();
    generate_deserialize_field(out, *f_iter, "this.");
    indent_down();

    indent(out) << "} else {" << endl;

    indent(out) <<  "  input.skip(ftype)" << endl;

    out <<
      indent() << "}" << endl <<
      indent() << "break" << endl;

  }
  // In the default case we skip the field
  indent(out) <<  "default:" << endl;
  indent(out) <<  "  input.skip(ftype)" << endl;

  scope_down(out);

  indent(out) << "input.readFieldEnd()" << endl;

  scope_down(out);

  indent(out) << "input.readStructEnd()" << endl;

  indent(out) << "return" << endl;

  indent_down();
  out << indent() << "}" << endl << endl;
}

/**
 * Generates the write() method for a struct
 */
void t_js_generator::generate_js_struct_writer(ofstream& out,
                                                   t_struct* tstruct) {
  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  out << js_namespace(tstruct->get_program())<< tstruct->get_name() << ".prototype.write = function(output){ "<<endl;

  indent_up();

  indent(out) << "output.writeStructBegin('" << name << "')" << endl;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    out << indent() << "if (null != this." << (*f_iter)->get_name() << ") {" << endl;
    indent_up();

    indent(out) <<
      "output.writeFieldBegin(" <<
      "'" << (*f_iter)->get_name() << "', " <<
      type_to_enum((*f_iter)->get_type()) << ", " <<
      (*f_iter)->get_key() << ")" << endl;


    // Write field contents
    generate_serialize_field(out, *f_iter, "this.");

    indent(out) <<
        "output.writeFieldEnd()" << endl;

    indent_down();
    indent(out) << "}" << endl;
  }


  out <<
    indent() << "output.writeFieldStop()" << endl <<
    indent() << "output.writeStructEnd()" << endl;

  out <<indent() << "return" << endl;

  indent_down();
  out <<
    indent() << "}" << endl <<
    endl;
}

/**
 * Generates a thrift service.
 *
 * @param tservice The service definition
 */
void t_js_generator::generate_service(t_service* tservice) {
    string f_service_name = get_out_dir()+service_name_+".js";
    f_service_.open(f_service_name.c_str());

    generate_service_helpers(tservice);
    generate_service_interface(tservice);
    generate_service_client(tservice);

    f_service_.close();
}

/**
 * Generates a service server definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_js_generator::generate_service_processor(t_service* tservice) {

}

/**
 * Generates a process function definition.
 *
 * @param tfunction The function to write a dispatcher for
 */
void t_js_generator::generate_process_function(t_service* tservice,
                                                 t_function* tfunction) {

}

/**
 * Generates helper functions for a service.
 *
 * @param tservice The service to generate a header definition for
 */
void t_js_generator::generate_service_helpers(t_service* tservice) {
    vector<t_function*> functions = tservice->get_functions();
    vector<t_function*>::iterator f_iter;

    f_service_ <<
        "//HELPER FUNCTIONS AND STRUCTURES" << endl << endl;

    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
        t_struct* ts = (*f_iter)->get_arglist();
        string name = ts->get_name();
        ts->set_name(service_name_ + "_" + name);
        generate_js_struct_definition(f_service_, ts, false);
        generate_js_function_helpers(*f_iter);
        ts->set_name(name);
    }
}

/**
 * Generates a struct and helpers for a function.
 *
 * @param tfunction The function
 */
void t_js_generator::generate_js_function_helpers(t_function* tfunction) {
    t_struct result(program_, service_name_ + "_" + tfunction->get_name() + "_result");
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

    generate_js_struct_definition(f_service_, &result, false);
}

/**
 * Generates a service interface definition.
 *
 * @param tservice The service to generate a header definition for
 */
void t_js_generator::generate_service_interface(t_service* tservice) {

}

/**
 * Generates a REST interface
 */
void t_js_generator::generate_service_rest(t_service* tservice) {

}

/**
 * Generates a service client definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_js_generator::generate_service_client(t_service* tservice) {
  string extends = "";

  f_service_ <<
      js_namespace(tservice->get_program()) << service_name_ << "Client = function(input, output) {"<<endl;

  indent_up();


  f_service_ <<
      indent() << "  this.input  = input" << endl <<
      indent() << "  this.output = null == output ? input : output" << endl <<
      indent() << "  this.seqid  = 0" << endl;

  indent_down();

  f_service_ <<
      indent() << "}" << endl;


  if (tservice->get_extends() != NULL) {
      extends = tservice->get_extends()->get_name();

      f_service_ << "for (var property in "<<extends<<"Client)"<<endl<<
          js_namespace(tservice->get_program()) << service_name_<<"Client[property] = "<<extends<<"Client[property]"<<endl;

  }

  //init prototype
  f_service_ <<  js_namespace(tservice->get_program())<<service_name_ << "Client.prototype = {}"<<endl;


  // Generate client method implementations
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* arg_struct = (*f_iter)->get_arglist();
    const vector<t_field*>& fields = arg_struct->get_members();
    vector<t_field*>::const_iterator fld_iter;
    string funname = (*f_iter)->get_name();

    // Open function
    f_service_ <<  js_namespace(tservice->get_program())<<service_name_<<"Client.prototype." << function_signature(*f_iter) << "{" << endl;

    indent_up();

    indent(f_service_) << indent() <<
      "this.send_" << funname << "(";

    bool first = true;
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      if (first) {
        first = false;
      } else {
        f_service_ << ", ";
      }
      f_service_ << (*fld_iter)->get_name();
    }
    f_service_ << ")" << endl;

    if (!(*f_iter)->is_oneway()) {
      f_service_ << indent();
      if (!(*f_iter)->get_returntype()->is_void()) {
        f_service_ << "return ";
      }
      f_service_ <<
        "this.recv_" << funname << "()" << endl;
    }

    indent_down();

    f_service_ << "}" << endl << endl;

    f_service_ <<  js_namespace(tservice->get_program())<<service_name_ <<
        "Client.prototype.send_" << function_signature(*f_iter) << "{" <<endl;

    indent_up();

    std::string argsname =  js_namespace(program_)+ service_name_ + "_" + (*f_iter)->get_name() + "_args";

    // Serialize the request header
    f_service_ <<
      indent() << "this.output.writeMessageBegin('" << (*f_iter)->get_name() << "', Thrift.MessageType.CALL, this.seqid)" << endl;

    f_service_ <<
      indent() << "var args = new " << argsname << "()" << endl;

    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      f_service_ <<
        indent() << "args." << (*fld_iter)->get_name() << " = " << (*fld_iter)->get_name() << endl;
    }

    // Write to the stream
    f_service_ <<
      indent() << "args.write(this.output)" << endl <<
      indent() << "this.output.writeMessageEnd()" << endl <<
      indent() << "return this.output.getTransport().flush()" << endl;


    indent_down();

    f_service_ << "}" << endl;


    if (!(*f_iter)->is_oneway()) {
      std::string resultname = js_namespace(tservice->get_program()) + service_name_ + "_" + (*f_iter)->get_name() + "_result";
      t_struct noargs(program_);

      t_function recv_function((*f_iter)->get_returntype(),
                               string("recv_") + (*f_iter)->get_name(),
                               &noargs);
      // Open function
      f_service_ <<
          endl <<  js_namespace(tservice->get_program())<<service_name_ <<
          "Client.prototype." << function_signature(&recv_function) << "{" << endl;

      indent_up();

      f_service_ <<
          indent() << "var ret = this.input.readMessageBegin()" << endl <<
          indent() << "var fname = ret.fname" << endl <<
          indent() << "var mtype = ret.mtype" << endl <<
          indent() << "var rseqid= ret.rseqid" <<endl <<
          indent() << "if (mtype == Thrift.MessageType.EXCEPTION) {" << endl <<
          indent() << "  var x = new Thrift.ApplicationException()" << endl <<
          indent() << "  x.read(this.input)" << endl <<
          indent() << "  this.input.readMessageEnd()" << endl <<
          indent() << "  throw x" << endl <<
          indent() << "}" << endl;


      f_service_ <<
        indent() << "var result = new " << resultname << "()" << endl <<
        indent() << "result.read(this.input)" << endl;


      f_service_ <<
        indent() << "this.input.readMessageEnd()" << endl <<
        endl;


      // Careful, only return result if not a void function
      if (!(*f_iter)->get_returntype()->is_void()) {
        f_service_ <<
          indent() << "if (null != result.success ) {" << endl <<
          indent() << "  return result.success" << endl <<
          indent() << "}" << endl;
      }

      t_struct* xs = (*f_iter)->get_xceptions();
      const std::vector<t_field*>& xceptions = xs->get_members();
      vector<t_field*>::const_iterator x_iter;
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        f_service_ <<
          indent() << "if (null != result." << (*x_iter)->get_name() << ") {" << endl <<
          indent() << "  throw result." << (*x_iter)->get_name() << endl <<
          indent() << "}" << endl;
      }

      // Careful, only return _result if not a void function
      if ((*f_iter)->get_returntype()->is_void()) {
        indent(f_service_) <<
          "return" << endl;
      } else {
        f_service_ <<
          indent() << "throw \"" << (*f_iter)->get_name() << " failed: unknown result\"" << endl;
      }

      // Close function
      indent_down();
      f_service_ << "}"<<endl;

    }
  }

}

/**
 * Deserializes a field of any type.
 */
void t_js_generator::generate_deserialize_field(ofstream &out,
                                                  t_field* tfield,
                                                  string prefix,
                                                  bool inclass) {
  t_type* type = get_true_type(tfield->get_type());

  if (type->is_void()) {
    throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE: " +
      prefix + tfield->get_name();
  }

  string name = prefix+tfield->get_name();

  if (type->is_struct() || type->is_xception()) {
    generate_deserialize_struct(out,
                                (t_struct*)type,
                                 name);
  } else if (type->is_container()) {
    generate_deserialize_container(out, type, name);
  } else if (type->is_base_type() || type->is_enum()) {
    indent(out) << "var rtmp = input.";

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " +
          name;
        break;
      case t_base_type::TYPE_STRING:
        out << "readString()";
        break;
      case t_base_type::TYPE_BOOL:
        out << "readBool()";
        break;
      case t_base_type::TYPE_BYTE:
        out << "readByte()";
        break;
      case t_base_type::TYPE_I16:
        out << "readI16()";
        break;
      case t_base_type::TYPE_I32:
        out << "readI32()";
        break;
      case t_base_type::TYPE_I64:
        out << "readI64()";
        break;
      case t_base_type::TYPE_DOUBLE:
        out << "readDouble()";
        break;
      default:
        throw "compiler error: no JS name for base type " + t_base_type::t_base_name(tbase);
      }
    } else if (type->is_enum()) {
      out << "readI32()";
    }
    out << endl;

    out <<name << " = rtmp.value"<<endl;


  } else {
    printf("DO NOT KNOW HOW TO DESERIALIZE FIELD '%s' TYPE '%s'\n",
           tfield->get_name().c_str(), type->get_name().c_str());
  }
}

/**
 * Generates an unserializer for a variable. This makes two key assumptions,
 * first that there is a const char* variable named data that points to the
 * buffer for deserialization, and that there is a variable protocol which
 * is a reference to a TProtocol serialization object.
 */
void t_js_generator::generate_deserialize_struct(ofstream &out,
                                                   t_struct* tstruct,
                                                   string prefix) {
    out <<
        indent() << prefix << " = new " <<  js_namespace(tstruct->get_program())<<tstruct->get_name() << "()" << endl <<
        indent() << prefix << ".read(input)" << endl;

}

void t_js_generator::generate_deserialize_container(ofstream &out,
                                                      t_type* ttype,
                                                      string prefix) {
  scope_up(out);

  string size = tmp("_size");
  string ktype = tmp("_ktype");
  string vtype = tmp("_vtype");
  string etype = tmp("_etype");

  t_field fsize(g_type_i32, size);
  t_field fktype(g_type_byte, ktype);
  t_field fvtype(g_type_byte, vtype);
  t_field fetype(g_type_byte, etype);

  out << indent() << "var " << size << " = 0" << endl;
  out << indent() << "var rtmp3" << endl;


  // Declare variables, read header
  if (ttype->is_map()) {
      out <<
          indent() << prefix << " = {}" << endl <<
          indent() << "var " << ktype << " = 0" << endl <<
          indent() << "var " << vtype << " = 0" << endl;

      out << indent() << "rtmp3 = input.readMapBegin()" << endl;
      out << indent() << ktype << "= rtmp3.ktype" << endl;
      out << indent() << vtype << "= rtmp3.vtype" << endl;
      out << indent() << size  << "= rtmp3.size" << endl;


  } else if (ttype->is_set()) {

    out <<
        indent() << prefix << " = []" << endl <<
        indent() << "var " << etype << " = 0" << endl <<
        indent() << "rtmp3 = input.readSetBegin()" << endl <<
        indent() << etype << "= rtmp3.etype"<<endl<<
        indent() << size << " = rtmp3.size"<<endl;

  } else if (ttype->is_list()) {

    out <<
        indent() << prefix << " = []" << endl <<
        indent() << "var " << etype << " = 0" << endl <<
        indent() << "rtmp3 = input.readListBegin()" << endl <<
        indent() << etype << " = rtmp3.etype"<<endl<<
        indent() << size << " = rtmp3.size"<<endl;
  }

  // For loop iterates over elements
  string i = tmp("_i");
  indent(out) <<
    "for (var " <<
    i << " = 0; " << i << " < " << size << "; ++" << i << ")" << endl;

  scope_up(out);

  if (ttype->is_map()) {
    generate_deserialize_map_element(out, (t_map*)ttype, prefix);
  } else if (ttype->is_set()) {
    generate_deserialize_set_element(out, (t_set*)ttype, prefix);
  } else if (ttype->is_list()) {
    generate_deserialize_list_element(out, (t_list*)ttype, prefix);
  }

  scope_down(out);


  // Read container end
  if (ttype->is_map()) {
    indent(out) << "input.readMapEnd()" << endl;
  } else if (ttype->is_set()) {
    indent(out) << "input.readSetEnd()" << endl;
  } else if (ttype->is_list()) {
    indent(out) << "input.readListEnd()" << endl;
  }

  scope_down(out);
}


/**
 * Generates code to deserialize a map
 */
void t_js_generator::generate_deserialize_map_element(ofstream &out,
                                                        t_map* tmap,
                                                        string prefix) {
  string key = tmp("key");
  string val = tmp("val");
  t_field fkey(tmap->get_key_type(), key);
  t_field fval(tmap->get_val_type(), val);

  indent(out) <<
    declare_field(&fkey, true, false) << endl;
  indent(out) <<
    declare_field(&fval, true, false) << endl;

  generate_deserialize_field(out, &fkey);
  generate_deserialize_field(out, &fval);

  indent(out) <<
      prefix << "[" << key << "] = " << val << endl;
}

void t_js_generator::generate_deserialize_set_element(ofstream &out,
                                                        t_set* tset,
                                                        string prefix) {
  string elem = tmp("elem");
  t_field felem(tset->get_elem_type(), elem);

  indent(out) <<
    "var " << elem << " = null" << endl;

  generate_deserialize_field(out, &felem);

  indent(out) <<
      prefix << ".push(" << elem << ")" << endl;
}

void t_js_generator::generate_deserialize_list_element(ofstream &out,
                                                         t_list* tlist,
                                                         string prefix) {
  string elem = tmp("elem");
  t_field felem(tlist->get_elem_type(), elem);

  indent(out) <<
    "var " << elem << " = null" << endl;

  generate_deserialize_field(out, &felem);

  indent(out) <<
      prefix << ".push(" << elem << ")" << endl;
}


/**
 * Serializes a field of any type.
 *
 * @param tfield The field to serialize
 * @param prefix Name to prepend to field name
 */
void t_js_generator::generate_serialize_field(ofstream &out,
                                                t_field* tfield,
                                                string prefix) {
  t_type* type = get_true_type(tfield->get_type());

  // Do nothing for void types
  if (type->is_void()) {
    throw "CANNOT GENERATE SERIALIZE CODE FOR void TYPE: " +
      prefix + tfield->get_name();
  }

  if (type->is_struct() || type->is_xception()) {
    generate_serialize_struct(out,
                              (t_struct*)type,
                               prefix +tfield->get_name() );
  } else if (type->is_container()) {
    generate_serialize_container(out,
                                 type,
                                 prefix + tfield->get_name());
  } else if (type->is_base_type() || type->is_enum()) {

    string name = tfield->get_name();

    //Hack for when prefix is defined (always a hash ref)
    if(!prefix.empty())
      name = prefix + tfield->get_name();

    indent(out) << "output.";

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw
          "compiler error: cannot serialize void field in a struct: " + name;
        break;
      case t_base_type::TYPE_STRING:
        out << "writeString(" << name << ")";
        break;
      case t_base_type::TYPE_BOOL:
        out << "writeBool(" << name << ")";
        break;
      case t_base_type::TYPE_BYTE:
        out << "writeByte(" << name << ")";
        break;
      case t_base_type::TYPE_I16:
        out << "writeI16(" << name << ")";
        break;
      case t_base_type::TYPE_I32:
        out << "writeI32(" << name << ")";
        break;
      case t_base_type::TYPE_I64:
        out << "writeI64(" << name << ")";
        break;
      case t_base_type::TYPE_DOUBLE:
        out << "writeDouble(" << name << ")";
        break;
      default:
        throw "compiler error: no JS name for base type " + t_base_type::t_base_name(tbase);
      }
    } else if (type->is_enum()) {
      out << "writeI32(" << name << ")";
    }
    out << endl;

  } else {
    printf("DO NOT KNOW HOW TO SERIALIZE FIELD '%s%s' TYPE '%s'\n",
           prefix.c_str(),
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
void t_js_generator::generate_serialize_struct(ofstream &out,
                                                 t_struct* tstruct,
                                                 string prefix) {
    indent(out) << prefix << ".write(output)" << endl;
}

/**
 * Writes out a container
 */
void t_js_generator::generate_serialize_container(ofstream &out,
                                                    t_type* ttype,
                                                    string prefix) {
  scope_up(out);

  if (ttype->is_map()) {
    indent(out) <<
        "output.writeMapBegin(" <<
        type_to_enum(((t_map*)ttype)->get_key_type()) << ", " <<
        type_to_enum(((t_map*)ttype)->get_val_type()) << ", " <<
        prefix << ".length)" << endl;
  } else if (ttype->is_set()) {
    indent(out) <<
      "output.writeSetBegin(" <<
      type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " <<
        prefix << ".length)" << endl;

  } else if (ttype->is_list()) {

    indent(out) <<
        "output.writeListBegin(" <<
        type_to_enum(((t_list*)ttype)->get_elem_type()) << ", " <<
        prefix << ".length)" << endl;

  }

  scope_up(out);

  if (ttype->is_map()) {
    string kiter = tmp("kiter");
    string viter = tmp("viter");
    indent(out) << "for(var "<<kiter<<" in "<<prefix<<")";
    scope_up(out);
    indent(out) << "var "<<viter<<" = "<<prefix<<"["<<kiter<<"]"<<endl;
    generate_serialize_map_element(out, (t_map*)ttype, kiter, viter);
    scope_down(out);


  } else if (ttype->is_set()) {
    string iter = tmp("iter");
    indent(out) <<
      "for(var "<<iter<<" in " << prefix << ")" << endl;
    scope_up(out);
    indent(out) << iter << "=" << prefix << "[" << iter << "]"<< endl;
    generate_serialize_set_element(out, (t_set*)ttype, iter);
    scope_down(out);


  } else if (ttype->is_list()) {
    string iter = tmp("iter");
    indent(out) <<
        "for(var "<<iter<<" in "<< prefix << ")" << endl;
    scope_up(out);
    indent(out) << iter << "=" << prefix << "[" << iter << "]"<< endl;
    generate_serialize_list_element(out, (t_list*)ttype, iter);
    scope_down(out);
  }

  scope_down(out);

  if (ttype->is_map()) {
    indent(out) <<
      "output.writeMapEnd()" << endl;
  } else if (ttype->is_set()) {
    indent(out) <<
      "output.writeSetEnd()" << endl;
  } else if (ttype->is_list()) {
    indent(out) <<
      "output.writeListEnd()" << endl;
  }

  scope_down(out);
}

/**
 * Serializes the members of a map.
 *
 */
void t_js_generator::generate_serialize_map_element(ofstream &out,
                                                      t_map* tmap,
                                                      string kiter,
                                                      string viter) {
  t_field kfield(tmap->get_key_type(), kiter);
  generate_serialize_field(out, &kfield);

  t_field vfield(tmap->get_val_type(), viter);
  generate_serialize_field(out, &vfield);
}

/**
 * Serializes the members of a set.
 */
void t_js_generator::generate_serialize_set_element(ofstream &out,
                                                      t_set* tset,
                                                      string iter) {
  t_field efield(tset->get_elem_type(), iter);
  generate_serialize_field(out, &efield);
}

/**
 * Serializes the members of a list.
 */
void t_js_generator::generate_serialize_list_element(ofstream &out,
                                                       t_list* tlist,
                                                       string iter) {
  t_field efield(tlist->get_elem_type(), iter);
  generate_serialize_field(out, &efield);
}

/**
 * Declares a field, which may include initialization as necessary.
 *
 * @param ttype The type
 */
string t_js_generator::declare_field(t_field* tfield, bool init, bool obj) {
  string result = "this." + tfield->get_name();

  if(!obj){
      result = tfield->get_name();
  }

  if (init) {
    t_type* type = get_true_type(tfield->get_type());
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        break;
      case t_base_type::TYPE_STRING:
        result += " = ''";
        break;
      case t_base_type::TYPE_BOOL:
        result += " = false";
        break;
      case t_base_type::TYPE_BYTE:
      case t_base_type::TYPE_I16:
      case t_base_type::TYPE_I32:
      case t_base_type::TYPE_I64:
        result += " = 0";
        break;
      case t_base_type::TYPE_DOUBLE:
        result += " = 0.0";
        break;
      default:
        throw "compiler error: no JS initializer for base type " + t_base_type::t_base_name(tbase);
      }
    } else if (type->is_enum()) {
      result += " = 0";
    } else if (type->is_map()){
      result += " = {}";
    } else if (type->is_container()) {
      result += " = []";
    } else if (type->is_struct() || type->is_xception()) {
      if (obj) {
          result += " = new " +js_namespace(type->get_program()) + type->get_name() + "()";
      } else {
        result += " = null";
      }
    }
  }
  return result;
}

/**
 * Renders a function signature of the form 'type name(args)'
 *
 * @param tfunction Function definition
 * @return String of rendered function definition
 */
string t_js_generator::function_signature(t_function* tfunction,
                                            string prefix) {

  string str;

  str  = prefix + tfunction->get_name() + " = function(";


  //Need to create js function arg inputs
  const vector<t_field*> &fields = tfunction->get_arglist()->get_members();
  vector<t_field*>::const_iterator f_iter;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {

      if(f_iter != fields.begin())
          str += ",";

      str += (*f_iter)->get_name();
  }


  str += ")";
  return str;
}

/**
 * Renders a field list
 */
string t_js_generator::argument_list(t_struct* tstruct) {
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
    result += (*f_iter)->get_name();
  }
  return result;
}

/**
 * Converts the parse type to a C++ enum string for the given type.
 */
string t_js_generator ::type_to_enum(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "NO T_VOID CONSTRUCT";
    case t_base_type::TYPE_STRING:
      return "Thrift.Type.STRING";
    case t_base_type::TYPE_BOOL:
      return "Thrift.Type.BOOL";
    case t_base_type::TYPE_BYTE:
      return "Thrift.Type.BYTE";
    case t_base_type::TYPE_I16:
      return "Thrift.Type.I16";
    case t_base_type::TYPE_I32:
      return "Thrift.Type.I32";
    case t_base_type::TYPE_I64:
      return "Thrift.Type.I64";
    case t_base_type::TYPE_DOUBLE:
      return "Thrift.Type.DOUBLE";
    }
  } else if (type->is_enum()) {
    return "Thrift.Type.I32";
  } else if (type->is_struct() || type->is_xception()) {
    return "Thrift.Type.STRUCT";
  } else if (type->is_map()) {
    return "Thrift.Type.MAP";
  } else if (type->is_set()) {
    return "Thrift.Type.SET";
  } else if (type->is_list()) {
    return "Thrift.Type.LIST";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}


THRIFT_REGISTER_GENERATOR(js, "Javascript", "");
